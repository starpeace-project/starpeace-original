unit CacheObjectSpool;

interface

  { $DEFINE CACHESERVER} //>>

  {$IFDEF CACHESERVER}
  uses
    SysUtils, Collection, Windows, CacheObjects, CacheManagerRDO;
  {$ELSE}
  uses
    SysUtils, Collection, Windows, CacheObjects, CacheManagerRDO; //CacheServerRDO;
  {$ENDif}

  const
    MaxTries = 20;

  type
    {
    TCacheObjectInfo =
      class
        public
          constructor Create(anObj : TCachedObject);//; aPath : string);
          destructor  Destroy; override;
        private
          //fPath     : string;
          fObject   : TCachedObject;
          fRefCount : integer;
        public
          //property Path : string read fPath;
        private
          function AddRef : integer;
          function Release : integer;
      end;
    }

    TWorldCacheSpool =
      class
        public
          constructor Create(world : string);
          destructor  Destroy; override;
        private
          fWorldName : string;
          //fObjects   : TLockableCollection;
          fProxy     : IWorldProxy;
        public
          procedure Lock;
          procedure Unlock;
        private
          function  ExpiredTTL(TTL, LastMod : string) : boolean;
          function  RequestCache(Obj : TCachedObject; chkHist : boolean) : string;
          function  OpenObject(path : string; recache : boolean) : TCachedObject;
          //function  IndexOf(path : string) : integer;
          function  OpenLink(path : string) : string;
          function  GetPath(x, y : integer) : string;
        public
          function  DoGetObjectByPath(path : string; recache, chkHist : boolean) : TCachedObject;
          function  UpdateCache(path : string; Obj : TCachedObject; chkHist : boolean) : boolean;
          function  GetObjectByCoord(x, y : integer; recache : boolean) : TCachedObject;
          function  GetObjectByPath(path : string; recache : boolean) : TCachedObject;
          function  GetObjectByAbsPath(path : string; recache : boolean) : TCachedObject;
          function  GetClass(path : string) : TCachedObject;
          procedure DeleteObject(Obj : TCachedObject);
        private
          function  GetProxy : IWorldProxy;
        public
          property  Proxy : IWorldProxy read GetProxy;
      end;

    TWorldCacheSpools =
      class
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fObjects : TLockableCollection;
          fClasses : TWorldCacheSpool;
        public
          function GetWorldSpool(name : string) : TWorldCacheSpool;
        public
          property Classes : TWorldCacheSpool read fClasses;
      end;

  var
    CacheSpools : TWorldCacheSpools = nil;

implementation

  uses
    Logs, CacheCommon, SpecialChars, CacheNameUtils;

  function CvtToInt(str : string) : integer;
    begin
      if str <> ''
        then
          try
            result := StrToInt(str);
          except
            result := 0;
          end
        else result := 0;
    end;


  // TCacheObjectInfo

  {
  constructor TCacheObjectInfo.Create(anObj : TCachedObject);// aPath : string);
    begin
      inherited Create;
      fObject := anObj;
      //fPath := lowercase(aPath);
    end;

  destructor TCacheObjectInfo.Destroy;
    begin
      try
        fObject.Free;
      except
        on e : Exception do
          Logs.Log('Survival', DateTimeToStr(Now) + ' Error Destroying the Cached Object MSG = ' + e.Message);
      end;
      inherited;
    end;

  function TCacheObjectInfo.AddRef : integer;
    begin
      inc(fRefCount);
      result := fRefCount;
    end;

  function TCacheObjectInfo.Release : integer;
    begin
      dec(fRefCount);
      result := fRefCount;
    end;
  }

  // TWorldCacheSpool

  constructor TWorldCacheSpool.Create(world : string);
    begin
      inherited Create;
      fWorldName := world;
      //fObjects   := TLockableCollection.Create(0, rkBelonguer);
    end;

  destructor TWorldCacheSpool.Destroy;
    begin
      //fObjects.Free;
      inherited;
    end;

  procedure TWorldCacheSpool.Lock;
    begin
      //fObjects.Lock;
    end;

  procedure TWorldCacheSpool.Unlock;
    begin
      //fObjects.Unlock;
    end;

  function TWorldCacheSpool.ExpiredTTL(TTL, LastMod : string) : boolean;
    begin
      if TTL = NULLTTL
        then result := true
        else result := Now - StrToDateTime(LastMod) > TTLToDateTime(TTL);
    end;

  function TWorldCacheSpool.RequestCache(Obj : TCachedObject; chkHist : boolean) : string;
    var
      kind  : integer;
      info  : integer;
      ver   : string;
      agnt  : string;
      objId : string;
      Prx   : IWorldProxy;
    begin
      Prx := Proxy;
      if Prx <> nil
        then
          begin
            ver := Obj[ppVersion];
            if ver = IntToStr(Prx.MSVersion)
              then
                begin
                  objId := Obj[ppObjId];
                  kind  := StrToInt(Obj[ppKind]);
                  info  := StrToInt(Obj[ppInfo]);
                  try
                    result := Prx.GetCache(StrToInt(objId), kind, info, chkHist);
                  except
                    on e : Exception do
                      Logs.Log('Survival', DateTimeToStr(Now) + ' Error calling Prx.GetCache MSG = ' + e.Message);
                  end;
                end
              else
                begin
                  // Obj.DeleteLinks;
                  agnt  := Obj[ppRnAgent];
                  objId := Obj[ppObjLoc];
                  if (agnt <> '') and (objId <> '')
                    then
                      try
                        result := Prx.RenewCache(agnt, objId, chkHist);
                      except
                        on e : Exception do
                          Logs.Log('Survival', DateTimeToStr(Now) + ' Error calling Proxy.RenewCache MSG = ' + e.Message);
                      end
                    else result := resError;
                end;
          end
        else result := '';
    end;

  function TWorldCacheSpool.OpenObject(path : string; recache : boolean) : TCachedObject;
    var
      TTL     : string;
      LastMod : string;
      tries   : integer;
      updated : boolean;
      lstTTL  : string;
    begin
      result := TCachedObject.Open(path, false);
      if not (ioFileDoesntExist in result.IOResult) and not (ioTimeOutRead in result.IOResult)
        then
          begin
            TTL     := result[ppTTL];
            LastMod := result[ppLastMod];
            try
              if recache and (TTL <> TTLNever) and (LastMod <> '') and ExpiredTTL(TTL, LastMod)
                then
                  begin
                    lstTTL := result[ppTTL];
                    tries  := 1;
                    repeat
                      updated := UpdateCache(path, result, false); //tries = 1); // >> The recontrapeluza!!!
                      lstTTL  := result[ppTTL];
                      inc(tries);
                    until updated or (tries > 1) or (lstTTL <> NULLTTL);
                    if not updated
                      then result.IOResult := result.IOResult + [ioExpiredTTL];
                  end;
            except
              on e : Exception do
                Logs.Log('Survival', DateTimeToStr(Now) + ' Error opening in OpenObject: ' + path + ' MSG = ' + e.Message);
            end;
          end
    end;

  function TWorldCacheSpool.DoGetObjectByPath(path : string; recache, chkHist : boolean) : TCachedObject;
    var
      index : integer;
      //Obj   : TCacheObjectInfo;
      trap  : integer;
    begin
      trap := 0;
      try
        Lock;
        try
          trap := 1;
          if path <> ''
            then
              begin
                {
                if chkHist
                  then index := IndexOf(path)
                  else index := noIndex;
                }
                index := noIndex;
                trap := 2;
                if index = noIndex
                  then
                    begin
                      result := OpenObject(path, recache);
                      trap := 3;
                      //Obj := TCacheObjectInfo.Create(result);//, path);
                      trap := 4;
                      //Obj.AddRef;
                      trap := 5;
                      //fObjects.Insert(Obj);
                      trap := 6;
                    end
                  else
                    try
                      trap := 7;
                      //Obj := TCacheObjectInfo(fObjects[index]);
                      trap := 8;
                      //Obj.AddRef;
                      trap := 9;
                      //result := Obj.fObject;
                      result := nil; // >>
                      trap := 10;
                    except
                      on e : Exception do
                        begin
                          Logs.Log('Survival', DateTimeToStr(Now) + ' Error Adding Refs in DoGetObjectByPath (' + path + ') World: ' + fWorldName + ' MSG = ' + e.Message);
                          result := nil;
                          raise;
                        end;
                    end;
              end
            else result := nil;
        finally
          Unlock;
        end;
      except
        on e : Exception do
          begin
            Logs.Log('Survival', DateTimeToStr(Now) + ' TRAP=' + IntToStr(trap) + ' Error in DoGetObjectByPath (' + path + ') World: ' + fWorldName + ' MSG = ' + e.Message);
            result := nil;
          end;
      end;
    end;

  function TWorldCacheSpool.UpdateCache(path : string; Obj : TCachedObject; chkHist : boolean) : boolean;
    begin
      if Proxy <> nil
        then
          begin
            if RequestCache(Obj, chkHist) = resOk // Keep here...
              then
                begin
                  Obj.SetPath(path);
                  result := true;
                end
              else result := false
          end
        else result := false;
    end;

  function TWorldCacheSpool.GetObjectByCoord(x, y : integer; recache : boolean) : TCachedObject;
    var
      path  : string;
      canrw : boolean;
      Prx   : IWorldProxy;
      trap  : integer;
    begin
      trap := 0;
      Prx := Proxy;
      if Prx <> nil
        then
          try
            Lock;
            try
              trap := 1;
              path := GetPath(x, y);
              trap := 2;
              if path <> ''
                then result := DoGetObjectByPath('worlds\' + fWorldName + '\' + path, recache, true)
                else
                  begin
                    trap := 3;
                    try
                      canrw := Prx.RenewCache('Facility', IntToStr(x) + ',' +  IntToStr(y), false) = resOk;
                      trap := 4;
                    except
                      on e : Exception do
                        begin
                          Logs.Log('Survival', DateTimeToStr(Now) + 'TRAP= ' + IntToStr(trap) + ' Error Renwing object in GetObjectByCoord ' + IntToStr(x) + ', ' + IntToStr(y) + ') World: ' + fWorldName + ' MSG = ' + e.Message);
                          canrw := false;
                        end;
                    end;
                    trap := 5;
                    if canrw
                      then
                        begin
                          path := GetPath(x, y);
                          trap := 6;
                          if path <> ''
                            then result := DoGetObjectByPath('worlds\' + fWorldName + '\' + path, false, false)
                            else result := nil;
                          trap := 7;
                        end
                      else result := nil;
                  end;
              // In case there were problems reading the cached file, here we go again...
              if (result <> nil) and (ioFileDoesntExist in result.IOResult)
                then
                  begin
                    trap := 8;
                    try
                      canrw := Prx.RenewCache('Facility', IntToStr(x) + ',' +  IntToStr(y), false) = resOk;
                      trap := 9;
                    except
                      on e : Exception do
                        begin
                          Logs.Log('Survival', DateTimeToStr(Now) + 'TRAP= ' + IntToStr(trap) + ' Error Renwing object in GetObjectByCoord ' + IntToStr(x) + ', ' + IntToStr(y) + ') World: ' + fWorldName + ' MSG = ' + e.Message);
                          canrw := false;
                        end;
                    end;
                    trap := 10;
                    if canrw
                      then
                        begin
                          path := GetPath(x, y);
                          trap := 11;
                          if path <> ''
                            then result := DoGetObjectByPath('worlds\' + fWorldName + '\' + path, false, false)
                            else result := nil;
                          trap := 12;
                        end
                      else result := nil;
                  end;
            finally
              Unlock;
            end;
          except
            on e : Exception do
              begin
                result := nil;
                Logs.Log('Survival', DateTimeToStr(Now) + 'TRAP= ' + IntToStr(trap) + ' Error in GetObjectByCoord(' + IntToStr(x) + ', ' + IntToStr(y) + ') World: ' + fWorldName + ' MSG = ' + e.Message);
              end;
          end
        else result := nil;
    end;

  function TWorldCacheSpool.GetObjectByPath(path : string; recache : boolean) : TCachedObject;
    begin
      try
        Lock;
        try
          result := DoGetObjectByPath('worlds\' + fWorldName + '\' + path, recache, true);
        finally
          Unlock;
        end;
      except
        on e : Exception do
          begin
            result := nil;
            Logs.Log('Survival', DateTimeToStr(Now) + ' Error in GetObjectByPath: ' + path + ' MSG = ' + e.Message);
          end;
      end;
    end;

  function TWorldCacheSpool.GetObjectByAbsPath(path : string; recache : boolean) : TCachedObject;
    begin
      try
        Lock;
        try
          result := DoGetObjectByPath(path, recache, true);
        finally
          Unlock;
        end;
      except
        on e : Exception do
          begin
            result := nil;
            Logs.Log('Survival', DateTimeToStr(Now) + ' Error in GetObjectByAbsPath: ' + path + ' MSG = ' + e.Message);
          end;
      end;
    end;

  function TWorldCacheSpool.GetClass(path : string) : TCachedObject;
    begin
      try
        Lock;
        try
          result := DoGetObjectByPath('classes\' + path, false, true);
        finally
          Unlock;
        end;
      except
        on e : Exception do
          begin
            result := nil;
            Logs.Log('Survival', DateTimeToStr(Now) + ' Error in TWorldCacheSpool.GetClass: ' + path + ' MSG = ' + e.Message);
          end;
      end;
    end;

  procedure TWorldCacheSpool.DeleteObject(Obj : TCachedObject);
    //var
      //i : integer;
    begin
      {
      try
        Lock;
        try
          i := pred(fObjects.Count);
          while (i >= 0) and (TCacheObjectInfo(fObjects[i]).fObject <> Obj) do
            dec(i);
          if (i >= 0) and (TCacheObjectInfo(fObjects[i]).Release = 0)
            then fObjects.AtDelete(i);
        finally
          Unlock;
        end;
      except
        on e : Exception do
          Logs.Log('Survival', DateTimeToStr(Now) + ' Error in TWorldCacheSpool.DeleteObject. MSG = ' + e.Message);
      end;
      }
    end;

  function TWorldCacheSpool.GetProxy : IWorldProxy;
    begin
      if (fProxy = nil) or not fProxy.IsConnected
        then
          if fWorldName <> ''
            then fProxy := WorldProxies[fWorldName]
            else fProxy := nil;
      result := fProxy;
    end;

  {
  function TWorldCacheSpool.IndexOf(path : string) : integer;
    begin
      path   := lowercase(path);
      result := pred(fObjects.Count);
      while (result >= 0) and (TCacheObjectInfo(fObjects[result]).Path <> path) do
        dec(result);
    end;
  }

  function TWorldCacheSpool.OpenLink(path : string) : string;
    var
      TxtFile : TextFile;
      succeed : boolean;
      tries   : integer;
    begin
      result  := '';
      tries   := 0;
      succeed := false;
      AssignFile(TxtFile, path);
      while (tries < MaxTries) and not succeed do
        begin
          try
            Reset(TxtFile);
            try
              ReadLn(TxtFile, result);
            finally
              CloseFile(TxtFile);
            end;
            succeed := true;
          except
            succeed := true;
            sleep(10);
          end;
          inc(tries);
        end;
    end;

  function TWorldCacheSpool.GetPath(x, y : integer) : string;
    var
      SearchRec : TSearchRec;
      wc        : string;
      tries     : integer;
      succeed   : boolean;
      trap      : integer;
      path      : string;
    begin
      trap := 0;
      try
        if fWorldName <> ''
          then
            begin
              trap := 1;
              path := GetObjectFolder(fWorldName, X, Y);
              trap := 2;
              wc := GetCoordWildCards(X, Y);
              trap := 3;
              tries := 0;
              succeed := false;
              while (tries < MaxTries) and not succeed do
                try
                  trap := 4;
                  succeed := SysUtils.FindFirst(path + wc, faArchive, SearchRec) = 0;
                  trap := 5;
                  if succeed
                    then
                      begin
                        //result := Copy(SearchRec.Name, length(wc), length(SearchRec.Name) - length(wc) + 1);
                        result := OpenLink(path + SearchRec.Name);
                        trap := 6;
                        if result <> ''
                          then TranslateChars(result, BackSlashChar, '\')
                          else succeed := false;
                        trap := 7;
                      end
                    else
                      begin
                        trap := 8;
                        result := '';
                        sleep(30);
                        trap := 9;
                      end;
                  inc(tries);
                finally
                  SysUtils.FindClose(SearchRec);
                  trap := 10;
                end;
            end
          else result := '';
      except
        on e : Exception do
          begin
            Logs.Log('Survival', DateTimeToStr(Now) + 'TRAP=' + IntToStr(trap) + ' Error GetPath(' + IntToStr(x) + ', ' + IntToStr(y) + ') World: ' + fWorldName + ' MSG = ' + e.Message);
            result := '';
          end;
      end;
    end;


  // TWorldCacheSpools

  constructor TWorldCacheSpools.Create;
    begin
      inherited Create;
      fObjects := TLockableCollection.Create(0, rkBelonguer);
      fClasses := TWorldCacheSpool.Create('');
    end;

  destructor TWorldCacheSpools.Destroy;
    begin
      fObjects.Free;
      fClasses.Free;
      inherited;
    end;

  function TWorldCacheSpools.GetWorldSpool(name : string) : TWorldCacheSpool;
    var
      i : integer;
    begin
      fObjects.Lock;
      try
        name := uppercase(name);
        i    := pred(fObjects.Count);
        while (i >= 0) and (TWorldCacheSpool(fObjects[i]).fWorldName <> name) do
          dec(i);
        if i >= 0
          then result := TWorldCacheSpool(fObjects[i])
          else
            begin
              result := TWorldCacheSpool.Create(name);
              fObjects.Insert(result);
            end
      finally
        fObjects.Unlock;
      end;
    end;

initialization

  CacheSpools := TWorldCacheSpools.Create;

finalization

  {$IFDEF CACHESERVER}
  if CacheSpools <> nil
    then
      begin
        CacheSpools.Free;
        CacheSpools := nil;
      end;
  {$ENDIF}

end.
