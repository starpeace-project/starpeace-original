unit CachedObjectWrap;

interface

  uses
    CacheObjects, CacheManagerRDO;

  const
    tidName = 'Name';

  type
    TCachedObjectWrap =
      class
        public
          constructor Create;
          destructor  Destroy; override;
        published
          function  ContainsFolder(const Name : WideString) : OleVariant;
          function  SetClass(const Name : WideString) : OleVariant;
          function  SetObject(X, Y : Integer) : OleVariant;
          function  SetObjectOfWorld(X, Y : Integer; const World : WideString) : OleVariant;
          function  GetPath(useless : integer) : OleVariant;
          function  SetPath(const aPath : WideString) : OleVariant;
          function  SetWorld(const Name : WideString) : OleVariant;
          function  Properties(const Name : WideString) : OleVariant;
          function  EnableReCache(value : WordBool) : OleVariant;
          function  GetIterator(const Folder : WideString) : OleVariant;
          function  GetInputNames(useless : integer) : OleVariant;
          function  GetOutputNames(useless : integer) : OleVariant;
          function  OpenGate(const Folder, Gate : WideString) : OleVariant;
          function  GetPropArray(const Mask : WideString) : OleVariant;
          procedure RDODestroy;
          procedure KeepAlive;
          procedure Refresh;
        private
          function  GetGateNames(const Gate : string) : string;
        private
          fCachedObject : CacheObjects.TCachedObject;
          fWorldName    : string;
          fProxy        : IWorldProxy;
          fPath         : string;
          fRecache      : WordBool;
          fLastUpdate   : TDateTime;
        public
          property LastUpdate : TDateTime read fLastUpdate;
        private
          function  SetToObject(const aPath : string) : WordBool; safecall;
          function  RequestCache(Id : string) : string;
          function  UpdateCache (Id : string) : boolean;
          function  GetRelativePath : string;
        end;

implementation

  uses
    Classes, SysUtils, ComServ, CacheCommon, CacheRegistryData, SpecialChars,
    CacheNameUtils, FolderIteratorWrap, CacheServerReportForm;

  const
    LineBreak = #13#10;

  // TCachedObjectWrap

  constructor TCachedObjectWrap.Create;
    begin
      inherited Create;
      fRecache := true;
      inc(CacheServerReportForm.RDOCacheObjCount);
      fLastUpdate := Now;
    end;

  destructor TCachedObjectWrap.Destroy;
    begin
      dec(CacheServerReportForm.RDOCacheObjCount);
      fCachedObject.Free;
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
      try
        result := SetToObject('Classes\' + Name);
      except
        result := false;
      end;
    end;

  function TCachedObjectWrap.SetObject(X, Y : Integer) : OleVariant;
    var
      aPath     : string;
      WildCards : string;
      SearchRec : TSearchRec;
    begin
      KeepAlive;
      try
        if fWorldName <> ''
          then
            begin
              aPath := GetObjectFolder(fWorldName, X, Y);
              WildCards := GetCoordWildCards(X, Y);
              try
                if FindFirst(aPath + WildCards, faArchive, SearchRec) = 0
                  then
                    begin
                      aPath := Copy(SearchRec.Name, length(WildCards), length(SearchRec.Name) - length(WildCards) + 1);
                      TranslateChars(aPath, BackSlashChar, '\');
                      result := SetPath(aPath);
                    end
                  else result := false;
              finally
                FindClose(SearchRec);
              end;
            end
          else result := false;
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
      result := fPath;
    end;

  function TCachedObjectWrap.SetPath(const aPath : WideString) : OleVariant;
    begin
      KeepAlive;
      try
        fPath := aPath;
        result := SetToObject(GetRelativePath);
      except
        result := false;
      end;
    end;

  function TCachedObjectWrap.SetWorld(const Name : WideString) : OleVariant;
    begin
      KeepAlive;
      try
        fWorldName := Name;
        fCachedObject.Free;
        fCachedObject := nil;
        result := true;
      except
        result := false;
      end;
    end;

  function TCachedObjectWrap.Properties(const Name : WideString) : OleVariant;
    begin
      KeepAlive;
      try
        if fCachedObject <> nil
          then result := fCachedObject.Properties[Name]
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

  function TCachedObjectWrap.GetGateNames(const Gate : string) : string;
    var
      count : integer;
      i     : integer;
      aux   : string;
    begin
      KeepAlive;
      result := '';
      try
        if fCachedObject <> nil
          then
            begin
              count := StrToInt(fCachedObject.Properties[Gate + 'Count']);
              for i := 0 to pred(count) do
                begin
                  aux := fCachedObject.Properties[Gate + 'Path' + IntToStr(i)];
                  if aux <> ''
                    then
                      begin
                        aux := aux + '::' + fCachedObject.Properties[Gate + IntToStr(i)];
                        if result = ''
                          then result := aux
                          else result := result + ^M^J + aux;
                      end;
                end;
            end;
      except
      end;
    end;

  function TCachedObjectWrap.GetInputNames(useless : integer) : OleVariant;
    begin
      KeepAlive;
      result := GetGateNames('Input');
    end;

  function TCachedObjectWrap.GetOutputNames(useless : integer) : OleVariant;
    begin
      KeepAlive;
      result := GetGateNames('Output');
    end;

  function TCachedObjectWrap.OpenGate(const Folder, Gate : WideString) : OleVariant;
    var
      Obj : TCachedObjectWrap;
    begin
      KeepAlive;
      try
        Obj := TCachedObjectWrap.Create;
        Obj.fWorldName := fWorldName;
        if Obj.SetPath(fPath + Folder + '\' + Gate)
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
      Free;
    end;

  procedure TCachedObjectWrap.KeepAlive;
    begin
      fLastUpdate := Now;
    end;

  procedure TCachedObjectWrap.Refresh;
    begin
      try
        fCachedObject.SetPath(GetRelativePath);
      except
      end;
    end;

  function TCachedObjectWrap.SetToObject(const aPath : string) : WordBool;
    var
      TTL     : string;
      LastMod : string;
    begin
      if fCachedObject = nil
        then fCachedObject := CacheObjects.TCachedObject.Open(aPath, false)
        else fCachedObject.SetPath(aPath);
      try
        if not (ioFileDoesntExist in fCachedObject.IOResult) and not (ioTimeOutRead in fCachedObject.IOResult)
          then
            begin
              TTL     := fCachedObject[ppTTL];
              LastMod := fCachedObject[ppLastMod];
              try
                if fRecache and (TTL <> TTLNever) and (LastMod <> '') and (Now - StrToDateTime(LastMod) > TTLToDateTime(TTL))
                  then
                    if UpdateCache(fCachedObject[ppObjId])
                      then result := true
                      else
                        begin
                          result := false;
                          fCachedObject.ClearProperties;
                        end
                  else result := true;
              except
                result := false;
                fCachedObject.ClearProperties;
              end;
            end
          else
            begin
              result := false;
              fCachedObject.ClearProperties;
            end;
      finally
        fCachedObject.Unlock;
      end;
    end;

  function TCachedObjectWrap.RequestCache(Id : string) : string;
    begin
      if fProxy <> nil
        then result := fProxy.GetCache(StrToInt(Id))
        else result := '';
    end;

  function TCachedObjectWrap.UpdateCache(Id : string) : boolean;
    var
      res : string;
    begin
      if (fProxy = nil) and (fWorldName <> '')
        then fProxy := WorldProxies[fWorldName];
      if fProxy <> nil
        then
          begin
            res := RequestCache(Id);
            if res = resOk
              then
                begin
                  fCachedObject.SetPath(GetRelativePath);
                  result := true;
                end
              else result := false
          end
        else result := false;
    end;

  function TCachedObjectWrap.GetRelativePath : string;
    begin
      result := 'Worlds\' + fWorldName + '\' + fPath;
    end;

end.
