unit WSObjectCacher;

interface

  uses
    Windows, Classes, SysUtils, CacheCommon, RDOInterfaces, CacheRegistryKeys,
    CacheObjects, RDORootServer, Collection, SyncObjs;

  // WEB Server's Cache RDO Server for Model Servers

  type
    TCacheQuery     = class;
    TWSObjectCacher = class;
    TCacheThread    = class;

    TCacheQuery =
      class
        public
          constructor Create(const Query : string);
        private
          fQuery : string;
      end;

    TWSObjectCacher =
      class(TRDORootServer)
        public
          constructor Create(aServerConn : IRDOConnectionsServer; const RootName : string);
          destructor  Destroy; override;
        private
          fCacheQueue  : TLockableCollection;
          fCacheThread : TThread;
          fCacheEvent  : TEvent;
        private
          procedure CacheObject(const ReqText : string);
        published
          procedure RegisterWorld(const WorldName, ServerName : WideString; ServerPort : integer);
          procedure InitWorld(const World : WideString; xSize, ySize : integer);
        private
          property CacheQueue : TLockableCollection read fCacheQueue;
          property CacheEvent : TEvent read fCacheEvent;
      end;

    TCacheThread =
      class(TThread)
        public
          constructor Create(Owner : TWSObjectCacher);
        private
          fOwner : TWSObjectCacher;
        protected
          procedure Execute; override;
        private
          procedure ExecuteQuery(const Query : string);
          procedure CreateObject(const Path : string; List : TStringList);
          procedure DeleteObject(const Path : string);
          procedure CreateLinks(CachedObj : TCachedObject; const Links : string);
          procedure DeleteLinks(CachedObj : TCachedObject; const Links : string);
      end;

    EWSCacheRDOError = class(Exception);

  // Call InitWebServerCache to initialize the server

  procedure InitWebServerCache(ServerConn : IRDOConnectionsServer; Port : integer);

  // Call DoneWebServerCache to unitialize the server

  procedure DoneWebServerCache;

implementation

  uses
    CompStringsParser, RDOServer, ComObj, Registry, SpecialChars,
    FileCtrl, DACacherServer;

  // TCacheQuery

  constructor TCacheQuery.Create(const Query : string);
    begin
      fQuery := Query;
    end;

  // TWSObjectCacher

  constructor TWSObjectCacher.Create(aServerConn : IRDOConnectionsServer; const RootName : string);
    begin
      inherited;
      fCacheQueue  := TLockableCollection.Create(0, rkBelonguer);
      fCacheThread := TCacheThread.Create(Self);
      fCacheEvent  := TEvent.Create(nil, true, false, '');
    end;

  destructor TWSObjectCacher.Destroy;
    begin
      fCacheQueue.Free;
    end;

  procedure TWSObjectCacher.CacheObject(const ReqText : string);
    begin
      fCacheQueue.Lock;
      try
        fCacheQueue.Insert(TCacheQuery.Create(ReqText));
        fCacheEvent.SetEvent;
      finally
        fCacheQueue.Unlock;
      end;
    end;

  procedure TWSObjectCacher.RegisterWorld(const WorldName, ServerName : WideString; ServerPort : integer);
    var
      Reg : TRegistry;
      aux : string;
    begin
      try
        Reg := TRegistry.Create;
        try
          aux := WorldsKey + WorldName;
          Reg.RootKey := HKEY_LOCAL_MACHINE;
          Reg.CreateKey(aux);
          if Reg.OpenKey(aux, false)
            then
              begin
                Reg.WriteString('Server', ServerName);
                Reg.WriteInteger('Port', ServerPort);
                Reg.RegistryConnect('');
              end;
        finally
          Reg.Free;
        end;
      except
      end;
    end;

  procedure TWSObjectCacher.InitWorld(const World : WideString; xSize, ySize : integer);
    var
      MapPth : string;
    begin
      MapPth := GetCacheRootPath + 'Worlds\' + World + '\Map\';
      ForceDirectories(MapPth);
    end;

  // TCacheThread

    constructor TCacheThread.Create(Owner : TWSObjectCacher);
      begin
        inherited Create(true);
        Priority := tpHigher;
        fOwner := Owner;
        Resume;
      end;

    procedure TCacheThread.Execute;
      var
        CacheQuery : TCacheQuery;
      begin
        while not Terminated do
          begin
            fOwner.CacheQueue.Lock;
            if fOwner.CacheQueue.Count > 0
              then
                try
                  CacheQuery := TCacheQuery(fOwner.CacheQueue.AtExtract(0));
                  ExecuteQuery(CacheQuery.fQuery);
                  CacheQuery.Free;
                finally
                  fOwner.CacheQueue.Unlock;
                end
              else
                begin
                  fOwner.CacheQueue.Unlock;
                  fOwner.CacheEvent.ResetEvent;
                  fOwner.CacheEvent.WaitFor(INFINITE);
                end;
          end;
      end;

    procedure TCacheThread.ExecuteQuery(const Query : string);
      var
        ActStr : string;
        Action : integer;
        Path   : string;
        List   : TStringList;
      begin
        try
          List := TStringList.Create;
          try
            List.Text := Query;
            ActStr    := List.Values[ActionName];
            if ActStr <> ''
              then
                begin
                  List.Delete(0); // Delete Action from properties
                  Action := StrToInt(ActStr);
                  Path   := List.Values[PathName];
                  if Path <> ''
                    then
                      begin
                        List.Delete(0); // Delete path from properties
                        case Action of
                          caCreate :
                            begin
                              CreateObject(Path, List);
                              List := nil;
                            end;
                          caDelete :
                            DeleteObject(Path);
                          else raise EWSCacheRDOError.Create('Error in format...');
                        end;
                      end;
                end
              else raise EWSCacheRDOError.Create('Error in format...');
          finally
            List.Free;
          end;
        except
        end;
      end;

  procedure TCacheThread.CreateObject(const Path : string; List : TStringList);
    var
      CachedObj : TCachedObject;
      Links     : string;
    begin
      // This make paths relative to "Cache\World\".
      CachedObj := TCachedObject.Create(Path, List);
      try
        Links := CachedObj[LinkName];
        if Links <> ''
          then CreateLinks(CachedObj, Links);
        CachedObj.Flush;
      finally
        CachedObj.Free;
      end;
    end;

  procedure TCacheThread.DeleteObject(const Path : string);
    var
      CachedObj : TCachedObject;
      Links     : string;
    begin
      // This make paths relative to "Cache\World\".
      CachedObj := TCachedObject.Open(Path, false);
      try
        Links := CachedObj[LinkName];
        if Links <> ''
          then DeleteLinks(CachedObj, Links);
      finally
        CachedObj.Delete;
      end;
    end;

  procedure TCacheThread.CreateLinks(CachedObj : TCachedObject; const Links : string);
    var
      p : integer;
      s : string;
    begin
      p := 1;
      s := GetNextStringUpTo(Links, p, LinkSep);
      while s <> '' do
        begin
          CachedObj.CreateLink(s);
          s := GetNextStringUpTo(Links, p, LinkSep);
        end;
    end;

  procedure TCacheThread.DeleteLinks(CachedObj : TCachedObject; const Links : string);
    var
      p : integer;
      s : string;
    begin
      p := 1;
      s := GetNextStringUpTo(Links, p, LinkSep);
      while s <> '' do
        begin
          CachedObj.DeleteLink(s);
          s := GetNextStringUpTo(Links, p, LinkSep);
        end;
    end;

  var
    WSCacher : TWSObjectCacher = nil;
    DAServer : TDACacheServer  = nil;

  procedure InitWebServerCache(ServerConn : IRDOConnectionsServer; Port : integer);
    begin
      try
        WSCacher := TWSObjectCacher.Create(ServerConn, WSObjectCacherName);
        DAServer := TDACacheServer.Create(Port, WSCacher.CacheObject);
      except
        raise EWSCacheRDOError.Create('Cannot initialize Web Server Cache');
      end;
    end;

  procedure DoneWebServerCache;
    begin
      try
        WSCacher.Free;
        DAServer.Free;
      except
      end;
    end;

end.
