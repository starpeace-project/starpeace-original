unit ModelServerCache;

interface

  uses
    SysUtils, SyncObjs, CacheAgent, CacheCommon, RDOInterfaces, RDORootServer,
    Collection;

  procedure RegisterCacher(const ClassName : string; Agent : CCacheAgent);
  function  GetObjectPath(Obj : TObject; kind, info : integer) : string;
  function  CacheObject(Obj : TObject; kind, info : integer) : boolean;
  function  CacheColl(Coll : TCollection) : boolean;
  function  CacheMetaObject(Obj : TObject; kind, info : integer) : boolean;
  function  UncacheObject(Obj : TObject; kind, info : integer) : boolean;
  function  UncacheColl(Coll : TCollection) : boolean;
  function  UncacheMetaObject(Obj : TObject; kind, info : integer) : boolean;
  function  UpdateObjectCache(Obj : TObject; kind, info : integer) : boolean;
  function  UpdateObjectCacheEx(Obj : TObject; kind, info : integer; update : boolean) : TObjectCache;
  function  SaveImage(Obj : TObjectCache) : boolean;

  procedure InitModelServerCache;
  procedure CreateCacheServer(ServerConn : IRDOConnectionsServer; MaxQueryThreads : integer; theWorldLock : TCriticalSection);
  procedure DoneModelServerCache;

  function  RegisterWorld(const Name, RemoteAddress : WideString; LocalPort, RemotePort : integer; var WorldURL : string) : boolean;
  procedure SetWorldName(aName : string);
  function  GetWorldName : string;

  function  GetGlobalPath(const path : string) : string;
  function  CreateMapLink(X, Y : integer; const path : string) : string;
  function  CreateOutputLink(X, Y, K, P, C : integer; const Output, Town, Company, Utility, Circuits : string; Role : TFacilityRole) : string;
  function  CreateInputLink(X, Y, Capacity, SupLevel : integer; const Input, Town, Company, Utility, Circuits  : string; Role : TFacilityRole) : string;
  procedure SetMSVersion(ver : integer);

  procedure SetBackupSection(sect : TCriticalSection);
  procedure InformBackup(inBackup : boolean);

  procedure SetLogPath(aPath : string);
  procedure ClearCacheLog;
  procedure SetLogVer(ver : integer);
  procedure WarpLog(ver : integer);
  //function  CreateFacLink(aPath, info : string; MaxTries : integer) : boolean;

  procedure BackgroundCache(Obj : TObject; rcLinks : boolean);
  procedure BackgroundUncache(Obj : TObject);
  procedure BackgroundInvalidateCache(Obj : TObject);

  function  InvalidateCache(Obj : TObject; MetaObject : boolean) : boolean;
  function  InvalidateObjectByPath(path : string) : boolean;

  procedure CacheObjectLinks(path, links : string);

  type
    EMSCacheRDOError = class(Exception);

    TOnRenewCache = function(Agent, ObjId : string) : TObjectCache of object;

    TMSObjectCacher =
      class(TRDORootServer)
        private
          fOnRenewCache : TOnRenewCache;
        public
          property OnRenewCache : TOnRenewCache read fOnRenewCache write fOnRenewCache;
        private
          procedure LockCache;
          procedure UnlockCache;
          procedure LockMS;
          procedure UnlockMS;
        private
          fCLCounter : integer;
          fMLCounter : integer;
          fCaching   : boolean;
        private
          function GetReport : string;
        published
          function GetCache  (Id, kind, info : integer) : OleVariant;
          function RenewCache(Agent, ObjId : WideString) : OleVariant;
          property Report : string read GetReport;
      end;

  var
    EnabledLogs   : boolean          = false;
    MSVersion     : integer          = 0;
    MSCacher      : TMSObjectCacher  = nil;
    CacheLock     : TCriticalSection = nil;
    BackingUp     : boolean          = false;
    BackupSection : TCriticalSection = nil;


implementation

  uses
    WinSockRDOConnection, RDOObjectProxy, Logs;

  const
    MSCacheDLL = 'ModelServerCache.dll';

  const
    RDOCacheServerTimeout = 10000;

  procedure RegisterCacher( const ClassName : string; Agent : CCacheAgent ); external MSCacheDLL;
  function  dllCacheObject(Obj : TObject; kind, info : integer) : boolean; external MSCacheDLL name 'CacheObject';

  function CacheObject(Obj : TObject; kind, info : integer) : boolean;
    begin
      if EnabledLogs
        then
          begin
            {$IFDEF USELogs}
            //Logs.Log('Cache', TimeToStr(Now) + ' Cache(' + Obj.ClassName + ')');
            {$ENDIF}
            try
              result := dllCacheObject(Obj, kind, info);
            {$IFDEF USELogs}
              //Logs.Log('Cache', TimeToStr(Now) + ' OK!');
            {$ENDIF}
            except
            {$IFDEF USELogs}
             //Logs.Log('Survival', DateTimeToStr(Now) + ' MS Error caching object!');
            {$ENDIF}
              result := false;
            end;
          end
        else
          try
            result := dllCacheObject(Obj, kind, info);
          except
            result := false;
          end;
    end;

  function CacheColl(Coll : TCollection) : boolean;
    var
      i : integer;
    begin
      result := true;
      for i := 0 to pred(Coll.Count) do
        if not CacheObject( Coll[i], noKind, noInfo )
          then result := false;
    end;

  function GetObjectPath( Obj : TObject; kind, info : integer ) : string; external MSCacheDLL;

  (*
  function dllGetObjectPath( Obj : TObject ) : string; external MSCacheDLL name 'GetObjectPath';
  function GetObjectPath( Obj : TObject ) : string;
    begin
      if EnabledLogs
        then
          begin
            {$IFDEF USELogs}
            Logs.Log('Cache', TimeToStr(Now) + ' Getting Object Path: ' + Obj.ClassName);
            {$ENDIF}
            try
              result := dllGetObjectPath(Obj);
            {$IFDEF USELogs}
              Logs.Log('Cache', TimeToStr(Now) + ' Getting Object Path succeed.');
            {$ENDIF}
            except
            {$IFDEF USELogs}
              Logs.Log('Cache', TimeToStr(Now) + ' Error Getting Object Path.');
            {$ENDIF}
              result := '';
            end;
          end
        else
          try
            result := dllGetObjectPath(Obj);
          except
            result := '';
          end;
    end;
  *)

  function  CacheMetaObject( Obj : TObject; kind, info : integer ) : boolean;  external MSCacheDLL;

  function  dllUncacheObject( Obj : TObject; kind, info : integer ) : boolean; external MSCacheDLL name 'UncacheObject';

  function UncacheObject( Obj : TObject; kind, info : integer ) : boolean;
    begin
      if EnabledLogs
        then
          begin
            {$IFDEF USELogs}
            //Logs.Log('Cache', TimeToStr(Now) + ' Uncache(' + Obj.ClassName + ')');
            {$ENDIF}
            try
              result := dllUnCacheObject(Obj, kind, info);
            {$IFDEF USELogs}
              //Logs.Log('Cache', TimeToStr(Now) + ' OK!');
            {$ENDIF}
            except
            {$IFDEF USELogs}
              //Logs.Log('Cache', TimeToStr(Now) + ' Error!');
            {$ENDIF}
              result := false;
            end;
          end
        else
          try
            result := dllUnCacheObject(Obj, kind, info);
          except
            result := false;
          end;
    end;

  function UncacheColl(Coll : TCollection) : boolean;
    var
      i : integer;
    begin
      result := true;
      for i := 0 to pred(Coll.Count) do
        if not UncacheObject( Coll[i], noKind, noInfo )
          then result := false;
    end;

  function  UncacheMetaObject(Obj : TObject; kind, info : integer) : boolean; external MSCacheDLL;
  function  UpdateObjectCache(Obj : TObject; kind, info : integer) : boolean;  external MSCacheDLL;
  function  UpdateObjectCacheEx(Obj : TObject; kind, info : integer; update : boolean) : TObjectCache; external MSCacheDLL;
  function  SaveImage(Obj : TObjectCache) : boolean; external MSCacheDLL;

  procedure InitCachers; external MSCacheDLL;
  procedure DoneCachers; external MSCacheDLL;

  function  GetGlobalPath(const path : string) : string; external MSCacheDLL;
  function  CreateMapLink(X, Y : integer; const path : string) : string; external MSCacheDLL;
  function  CreateOutputLink(X, Y, K, P, C : integer; const Output, Town, Company, Utility, Circuits : string; Role : TFacilityRole) : string; external MSCacheDLL;
  function  CreateInputLink(X, Y, Capacity, SupLevel : integer; const Input, Town, Company, Utility, Circuits  : string; Role : TFacilityRole) : string; external MSCacheDLL;
  procedure SetMSVersion(ver : integer); external MSCacheDLL;

  function  GetWorldName : string; external MSCacheDLL;
  procedure SetWorldName(aName : string); external MSCacheDLL;

  function  GetCacheRootPath : string; external MSCacheDLL;
  procedure SetCacheRootPath(const aPath : string); external MSCacheDLL;

  procedure SetLogPath(aPath : string); external MSCacheDLL;
  procedure ClearCacheLog; external MSCacheDLL;
  procedure SetLogVer(ver : integer); external MSCacheDLL;
  procedure WarpLog(ver : integer); external MSCacheDLL;
  //function  CreateFacLink(aPath, info : string; MaxTries : integer) : boolean; external MSCacheDLL;

  procedure BackgroundCache(Obj : TObject; rcLinks : boolean); external MSCacheDLL;
  procedure BackgroundUncache(Obj : TObject); external MSCacheDLL;
  procedure BackgroundInvalidateCache(Obj : TObject); external MSCacheDLL;
  function  InvalidateCache(Obj : TObject; MetaObject : boolean) : boolean; external MSCacheDLL;
  function  InvalidateObjectByPath(path : string) : boolean; external MSCacheDLL;
  procedure CacheObjectLinks(path, links : string); external MSCacheDLL;

  procedure SetBackupSection(sect : TCriticalSection);
    begin
      BackupSection := sect;
    end;

  procedure InformBackup(inBackup : boolean);
    begin
      CacheLock.Enter;
      try
        BackingUp := inBackup;
      finally
        CacheLock.Leave;
      end;
    end;


  // TMSObjectCacher: is the class registered to allow remote modules access
  // cache using RDO.

  procedure TMSObjectCacher.LockCache;
    begin
      inc(fCLCounter);
      if CacheLock <> nil
        then CacheLock.Enter;
    end;

  procedure TMSObjectCacher.UnlockCache;
    begin
      if CacheLock <> nil
        then CacheLock.Leave;
      dec(fCLCounter);
    end;

  procedure TMSObjectCacher.LockMS;
    begin
      inc(fMLCounter);
      if BackupSection <> nil
        then BackupSection.Enter;
    end;

  procedure TMSObjectCacher.UnlockMS;
    begin
      if BackupSection <> nil
        then BackupSection.Leave;
      dec(fMLCounter);
    end;

  function TMSObjectCacher.GetCache(Id, kind, info : integer) : OleVariant;
    var
      Obj : TObject;
      Img : TObjectCache;
    begin
      result := resError;
      LockCache;
      try
        Obj := TObject(Id);
        if Obj <> nil
          then
            try
              {$IFDEF USELogs}
               //Logs.Log('Cache', TimeToStr(Now) + ' GenImage Start: ' + Obj.ClassName + '(' + IntToStr(Id) + ')');
              {$ENDIF}
              if not BackingUp
                then
                  begin
                    LockMS;  // >>> SALVAJADA in Maceo street
                    fCaching := true;
                    try
                      Img := UpdateObjectCacheEx(Obj, kind, info, false);
                    finally
                      fCaching := false;
                      UnlockMS; // >>> SALVAJADA in Maceo street
                    end;
                  end
                else
                  begin
                    result := resOK;
                    Img := nil;
                  end;
            except
              Img := nil;
              result := resError;
            end
          else
            begin
              result := resError;
              Img := nil;
            end;
        {$IFDEF USELogs}
         {if Img <> nil
           then Logs.Log('Cache', TimeToStr(Now) + ' GenImage end.')
           else Logs.Log('Cache', DateTimeToStr(Now) + ' Error in GenImage.');}
        {$ENDIF}
      finally
        UnlockCache;
      end;

      // Save Image
      if Img <> nil
        then
          try
            try
              ModelServerCache.SaveImage(Img);
              result := resOk;
            finally
              Img.Free;
            end;
          except
            result := resError;
          end;
    end;

  function TMSObjectCacher.RenewCache(Agent, ObjId : WideString) : OleVariant;
    var
      Img : TObjectCache;
    begin
      Img    := nil;
      result := resError;
      LockCache;
      try
        {$IFDEF USELogs}
        //Logs.Log('Cache', TimeToStr(Now) + ' RDO renewing cache Agent = ' + Agent + '; ObjId = ' + ObjId);
        {$ENDIF}
        try
          if not BackingUp
            then
              begin
                LockMS;
                fCaching := true;
                try
                  if Assigned(fOnRenewCache)
                    then Img := fOnRenewCache(Agent, ObjId);
                finally
                  fCaching := false;
                  UnlockMS;
                end;
              end
            else
              begin
                result := resOK;
                Img := nil;
              end;
        except
          Img := nil;
        end;
        {$IFDEF USELogs}
         {if Img <> nil
           then Logs.Log('Cache', TimeToStr(Now) + ' Renew GenImage end.')
           else Logs.Log('Cache', DateTimeToStr(Now) + ' Error in Renew GenImage.');}
        {$ENDIF}
      finally
        UnlockCache;
      end;

      // Save Image
      if Img <> nil
        then
          try
            try
              ModelServerCache.SaveImage(Img);
              result := resOk;
            finally
              Img.Free;
            end;
          except
            result := resError;
          end;
    end;

  function TMSObjectCacher.GetReport : string;
    begin
      result := 'CL: ' + IntToStr(fCLCounter) + ' MSL: ' + IntToStr(fMLCounter);
      if fCaching
        then result := result + ' and caching...'
    end;

  procedure InitModelServerCache;
    begin
      InitCachers;
      CacheLock := TCriticalSection.Create;
    end;

  procedure CreateCacheServer(ServerConn : IRDOConnectionsServer; MaxQueryThreads : integer; theWorldLock : TCriticalSection);
    begin
      MSCacher := TMSObjectCacher.Create(ServerConn, MaxQueryThreads, nil, MSObjectCacherName);
      //MSCacher.RDOServer.SetCriticalSection(WorldLock);
      SetBackupSection(theWorldLock);
    end;

  procedure DoneModelServerCache;
    begin
      MSCacher.Free;
      DoneCachers;
      CacheLock.Free;
      CacheLock := nil;
    end;

  function RegisterWorld(const Name, RemoteAddress : WideString; LocalPort, RemotePort : integer; var WorldURL : string) : boolean;
    var
      ClientConn : IRDOConnectionInit;
      Proxy      : OleVariant;
      Addr       : string;
    begin
      SetWorldName(Name);
      ClientConn := TWinSockRDOConnection.Create('ClientConn');
      ClientConn.Server := RemoteAddress;
      ClientConn.Port   := RemotePort;
      if ClientConn.Connect(RDOCacheServerTimeout)
        then
          begin
            try
              Proxy := TRDOObjectProxy.Create as IDispatch;
              Proxy.SetConnection(ClientConn);
              if Proxy.BindTo(WSObjectCacherName)
                then
                  begin
                    SetCacheRootPath(Proxy.CachePath);
                    WorldURL := Proxy.WorldURL;
                    Addr     := (ClientConn as IRDOConnection).LocalAddress;
                    result   := Proxy.RegisterWorld(Name, Addr, LocalPort, MSVersion);
                    SetMSVersion(MSVersion);
                  end
                else result := false;
            except
              result := false;
            end;
          end
        else result := false;
    end;

end.


