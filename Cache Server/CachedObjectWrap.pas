unit CachedObjectWrap;

interface

  uses
    CacheObjects, CacheManagerRDO, CacheObjectSpool;

  const
    tidName        = 'Name';
    tidSubObjCount = 'SubObjs';

  type
    TCachedObjectWrap =
      class
        public
          constructor Create(Cacher : ICacher);
          destructor  Destroy; override;
        published
          function  ContainsFolder(const Name : WideString) : OleVariant;
          function  SetClass(const Name : WideString) : OleVariant;
          function  SetObject(X, Y : Integer) : OleVariant;
          function  SetObjectOfWorld(X, Y : Integer; const World : WideString) : OleVariant;
          function  GetPath(useless : integer) : OleVariant;
          function  SetPath(const aPath : WideString) : OleVariant;
          function  SetAbsPath(const aPath : WideString) : OleVariant;
          function  SetWorld(const Name : WideString) : OleVariant;
          function  Properties(const Name : WideString) : OleVariant;
          function  GetPropertyList(const Names : WideString) : OleVariant;
          function  EnableReCache(value : WordBool) : OleVariant;
          function  GetIterator(const Folder : WideString) : OleVariant;
          function  GetInputNames(useless : integer; lang : widestring) : OleVariant;
          function  GetOutputNames(useless : integer; lang : widestring) : OleVariant;
          function  OpenGate(const Folder, Gate : WideString) : OleVariant;
          function  GetPropArray(const Mask : WideString) : OleVariant;
          procedure RDODestroy;
          procedure KeepAlive;
          procedure Refresh;
          function  GetSubObject(index : integer) : OleVariant;
          function  GetSubObjectProps(index : integer; names : WideString) : OleVariant;
        private
          procedure ReleaseObject;
          function  OpenSubObject(index : integer) : TCachedObjectWrap;
          function  GetSubObjCount : integer;
        published
          property  SubObjCount : integer read GetSubObjCount;
        private
          function  GetGateNames(const Gate, Lang : string) : string;
        private
          fCacheSpool   : TWorldCacheSpool;
          fCachedObject : TCachedObject;
          fWorldName    : string;
          fRecache      : WordBool;
          fLastUpdate   : TDateTime;
          fParent       : TCachedObjectWrap;
          fCacher       : ICacher;
        private
          function GetObjPath : string;
        protected
          property Parent : TCachedObjectWrap read fParent write fParent;
          property Path   : string read GetObjPath;
        public
          property LastUpdate : TDateTime read fLastUpdate;
        private
          //function  ExpiredTTL(TTL, LastMod : string) : boolean;
        public
          //function  SetToObject(const aPath : string) : WordBool;
        private
          //function  RequestCache(Id : string) : string;
          //function  UpdateCache (Id : string) : boolean;
          //function  GetRelativePath : string;
        public
          property CachedObject : CacheObjects.TCachedObject read fCachedObject;
        published
          property Recache      : WordBool read fRecache write fRecache;
      end;

implementation

  uses
    Classes, SysUtils, ComServ, CacheAgent, CacheCommon, CacheRegistryData, SpecialChars,
    CacheNameUtils, FolderIteratorWrap, CacheServerReportForm, CompStringsParser,
    Logs;

  const
    LineBreak = #13#10;

  // TCachedObjectWrap

  constructor TCachedObjectWrap.Create;
    begin
      inherited Create;
      inc(CacheServerReportForm.RDOCacheObjCount);
      fRecache    := true;
      fLastUpdate := Now;
      fCacher     := Cacher;
    end;

  destructor TCachedObjectWrap.Destroy;
    begin
      dec(CacheServerReportForm.RDOCacheObjCount);
      ReleaseObject;
      inherited;
    end;

  function TCachedObjectWrap.ContainsFolder(const Name : WideString) : OleVariant;
    begin
      try
        result := (fCachedObject <> nil) and fCachedObject.ContainsFolder(Name);
      except
        result := false;
      end;
    end;

  function TCachedObjectWrap.SetClass(const Name : WideString) : OleVariant;
    begin
      KeepAlive;
      try
        ReleaseObject;
        if fCacheSpool <> nil
          then fCachedObject := fCacheSpool.GetClass(Name)
          else fCachedObject := nil;
      except
        result := false;
      end;
    end;

  function TCachedObjectWrap.SetObject(X, Y : Integer) : OleVariant;
    begin
      KeepAlive;
      try
        ReleaseObject;
        if fCacheSpool <> nil
          then fCachedObject := fCacheSpool.GetObjectByCoord(X, Y, fRecache)
          else fCachedObject := nil;
        result := fCachedObject <> nil;
      except
        result := false;
      end;
    end;

  function TCachedObjectWrap.SetObjectOfWorld(X, Y : Integer; const World : WideString) : OleVariant;
    begin
      KeepAlive;
      try
        result := SetWorld(World) and SetObject(X, Y);
      except
        result := false;
      end;
    end;

  function TCachedObjectWrap.GetPath(useless : integer) : OleVariant;
    begin
      result := Path;
    end;

  function TCachedObjectWrap.SetPath(const aPath : WideString) : OleVariant;
    begin
      KeepAlive;
      try
        ReleaseObject;
        if fCacheSpool <> nil
          then fCachedObject := fCacheSpool.GetObjectByPath(aPath, fRecache)
          else fCachedObject := nil;
        result := (fCachedObject <> nil) and (fCachedObject.IOResult = []);
      except
        result := false;
      end;
    end;

  function TCachedObjectWrap.SetAbsPath(const aPath : WideString) : OleVariant;
    begin
      KeepAlive;
      try
        ReleaseObject;
        if fCacheSpool <> nil
          then fCachedObject := fCacheSpool.GetObjectByAbsPath(aPath, fRecache)
          else fCachedObject := nil;
        result := (fCachedObject <> nil) and (fCachedObject.IOResult = []);
      except
        result := false;
      end;
    end;

  function TCachedObjectWrap.SetWorld(const Name : WideString) : OleVariant;
    begin
      KeepAlive;
      try
        ReleaseObject;
        fWorldName  := Name;
        fCacheSpool := CacheSpools.GetWorldSpool(Name);
        result      := true;
      except
        result := false;
      end;
    end;

  function TCachedObjectWrap.Properties(const Name : WideString) : OleVariant;
    begin
      KeepAlive;
      try
        if (fCachedObject <> nil) and (lowercase(Name) <> 'password') // Patch!!!
          then result := fCachedObject.Properties[Name]
          else result := '';
      except
        result := '';
      end;
    end;

  function TCachedObjectWrap.GetPropertyList(const Names : WideString) : OleVariant;
    var
      resStr  : string;
      ansiStr : string;
      aux     : string;
      p       : integer;
    begin
      KeepAlive;
      try
        if fCachedObject <> nil
          then
            begin
              ansiStr := Names;
              resStr  := '';
              p       := 1;
              aux     := GetNextStringUpTo(ansiStr, p, #9);
              while aux <> '' do
                begin
                  inc(p);
                  resStr := resStr + fCachedObject.Properties[aux] + #9;
                  aux := GetNextStringUpTo(ansiStr, p, #9);
                end;
              result := widestring(resStr);
            end
          else result := '';
      except
        result := '';
      end;
    end;

  function TCachedObjectWrap.EnableReCache(value : WordBool) : OleVariant;
    begin
      KeepAlive;
      fReCache := value;
      result   := true;
    end;

  function TCachedObjectWrap.GetIterator(const Folder : WideString) : OleVariant;
    var
      Itr : TFolderIteratorWrap;
    begin
      if (fCachedObject <> nil) and fCachedObject.IsFolder
        then Itr := TFolderIteratorWrap.Create(fCachedObject.RelPath + Folder + '\', onBoth)
        else Itr := nil;
      result := integer(Itr);
    end;

  function TCachedObjectWrap.GetSubObjCount : integer;
    var
      aux : string;
    begin
      if fCachedObject <> nil
        then aux := fCachedObject.Properties[tidSubObjCount]
        else aux := '';
      if aux <> ''
        then
          try
            result := StrToInt(aux);
          except
            result := 0;
          end
        else result := 0;
    end;

  function TCachedObjectWrap.GetGateNames(const Gate, Lang : string) : string;
    var
      count : integer;
      i     : integer;
      aux   : string;
      //gtMap : string;
    begin
      KeepAlive;
      result := '';
      try
        if fCachedObject <> nil
          then
            begin
              //gtMap := fCachedObject.Properties['GateMap'];
              count := StrToInt(fCachedObject.Properties[Gate + 'Count']);
              for i := 0 to pred(count) do
                begin
                  aux := fCachedObject.Properties[Gate + 'Path' + IntToStr(i)];
                  if aux <> ''// and ((gtMap = '') or (gtMap[i] = '1'))
                    then
                      begin
                        aux := aux + '::' + fCachedObject.Properties[Gate + IntToStr(i) + '.' + Lang];
                        if result = ''
                          then result := aux
                          else result := result + ^M^J + aux;
                      end;
                end;
            end;
      except
      end;
    end;

  function TCachedObjectWrap.GetObjPath : string;
    begin
      if fCachedObject <> nil
        then result := fCachedObject.RelPath
        else result := '';
    end;

  function TCachedObjectWrap.GetInputNames(useless : integer; lang : widestring) : OleVariant;
    begin
      KeepAlive;
      result := GetGateNames('Input', lang);
    end;

  function TCachedObjectWrap.GetOutputNames(useless : integer; lang : widestring) : OleVariant;
    begin
      KeepAlive;
      result := GetGateNames('Output', lang);
    end;

  function TCachedObjectWrap.OpenGate(const Folder, Gate : WideString) : OleVariant;
    var
      Obj : TCachedObjectWrap;
    begin
      KeepAlive;
      try
        Obj := TCachedObjectWrap.Create(fCacher);
        Obj.fWorldName := fWorldName;
        if Obj.SetPath(Path + Folder + '\' + Gate)
          then result := integer(Obj)
          else
            begin
              result := integer(0);
              Obj.Free;
            end;
      except
        result := integer(0);
      end;
    end;

  function TCachedObjectWrap.GetPropArray(const Mask : WideString) : OleVariant;
    var
      i   : integer;
      aux : string;
      prp : string;
      msk : string;
    begin
      KeepAlive;
      try
        if fCachedObject <> nil
          then
            begin
              aux := '';
              msk := Mask;
              for i := 0 to pred(fCachedObject.Values.Count) do
                begin
                  prp := fCachedObject.Values[i];
                  if pos(msk, prp) = 1
                    then aux := aux + prp + LineBreak;
                end;
              result := WideString(aux);
            end
          else result := '';
      except
        result := '';
      end;
    end;

  procedure TCachedObjectWrap.RDODestroy;
    begin
      // >> Free;
    end;

  procedure TCachedObjectWrap.KeepAlive;
    begin
      fLastUpdate := Now;
    end;

  procedure TCachedObjectWrap.Refresh;
    begin
      try
        if (fCacheSpool <> nil) and (fCachedObject <> nil)
          then fCacheSpool.UpdateCache(fCachedObject.RelPath, fCachedObject, false); // >> Suspicious
      except
      end;
    end;

  procedure TCachedObjectWrap.ReleaseObject;
    begin
      {
      if (fCacheSpool <> nil) and (fCachedObject <> nil)
        then fCacheSpool.DeleteObject(fCachedObject); // add this line fCachedObject := nil;
      }
      try
        fCachedObject.Free;
        fCachedObject := nil;
      except
        Logs.Log('Survival', DateTimeToStr(Now) + ' Error destroying Cached Object..');
      end;
      fCachedObject := nil;
    end;

  function TCachedObjectWrap.OpenSubObject(index : integer) : TCachedObjectWrap;
    var
      tmpObj  : TCachedObjectWrap;
      thePath : string;
      theName : string;
      objId   : string;
      Proxy   : IWorldProxy;
    begin
      try
        if (fCacheSpool <> nil) and (fCachedObject <> nil) and (fWorldName <> '')
          then
            begin
              tmpObj  := TCachedObjectWrap.Create(fCacher);
              theName := IntToStr(index div SubObjForFile) + FiveExt;
              thePath := ExtractFilePath(fCachedObject.RelPath);
              tmpObj.SetWorld(fWorldName);
              if tmpObj.SetAbsPath(thePath + theName)
                then result := tmpObj
                else
                  begin
                    objId := fCachedObject[ppObjId];
                    Proxy := fCacheSpool.Proxy;
                    if (objId <> '') and (Proxy <> nil) and (Proxy.GetCache(StrToInt(objId), okSubObj, index, false) = resOk)
                      then
                        if tmpObj.SetAbsPath(thePath + theName)
                          then result := tmpObj
                          else
                            begin
                              result := nil;
                              tmpObj.Free;
                            end
                      else
                        begin
                          result := nil;
                          tmpObj.Free;
                        end;
                  end;
            end
          else result := nil;
      except
        result := nil;
      end;
    end;

  function TCachedObjectWrap.GetSubObject(index : integer) : OleVariant;
    begin
      try
        result := integer(OpenSubObject(index));
      except
        result := 0;
      end;
    end;

  function TCachedObjectWrap.GetSubObjectProps(index : integer; names : WideString) : OleVariant;
    var
      tmpObj : TCachedObjectWrap;
    begin
      try
        if fCachedObject <> nil
          then
            begin
              tmpObj := OpenSubObject(index);
              try
                if tmpObj <> nil
                  then result := tmpObj.GetPropertyList(names)
                  else result := '';
              finally
                tmpObj.Free;
              end;
            end
          else result := '';
      except
        result := '';
      end;
    end;

end.
