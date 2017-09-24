unit CacheServerReportForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, SyncObjs, SocketComp, ImageClient;

const
  MaxTicks          = 30;
  PictureServerPort = 6010;

type
  TCacheServerReport = class(TForm)
    Panel1: TPanel;
    Image1: TImage;
    PageControl1: TPageControl;
    General: TTabSheet;
    Label1: TLabel;
    GlobalPath: TEdit;
    Browse: TButton;
    Initialize: TButton;
    LocalPath: TEdit;
    Button1: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Port: TEdit;
    Label4: TLabel;
    WebSite: TEdit;
    TabSheet1: TTabSheet;
    Label5: TLabel;
    Label6: TLabel;
    lbCacheObjCount: TLabel;
    lbCacheItrCount: TLabel;
    LogTimer: TTimer;
    procedure InitializeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LogTimerTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure StarPictureServer;
    procedure OnClientConnect(Sender : TObject; Socket : TCustomWinSocket);
    procedure OnClientRead(Sender : TObject; Socket : TCustomWinSocket);
    procedure OnClientClose(Sender : TObject; Socket : TCustomWinSocket);
    procedure OnClientError(Sender : TObject; Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer);
    function  SaveImage(Client : TImageClient) : boolean;
  private
    fTickCount    : integer;
    fMaxTTL       : TDateTime;
    fServerSocket : TServerSocket;
  end;

var
  CacheServerReport : TCacheServerReport;
  DeleteCache       : boolean;
  RDOCacheObjCount  : integer;
  RDOCacheItrCount  : integer;

implementation

{$IFDEF LOGS}
  uses
    Registry, CacheRegistryKeys, FileCtrl, ShellAPI, RDOInterfaces,
    WinSockRDOConnectionsServer, RDORootServer, CacheCommon, CachedObjectWrap,
    FolderIteratorWrap, Collection, LogFile;
{$ELSE}
  uses
    Registry, CacheRegistryKeys, FileCtrl, ShellAPI, RDOInterfaces,
    WinSockRDOConnectionsServer, RDORootServer, CacheCommon, CachedObjectWrap,
    FolderIteratorWrap, Collection;
{$ENDIF}

  {$R *.DFM}

  const
    MaxPictBufferSize = 1024;

  type
    TWorldRegistyServer =
      class(TRDORootServer)
        public
          constructor Create(ConnectionsServer : IRDOConnectionsServer; MaxQueryThreads : integer; QueryCritSection : TCriticalSection; const RootName : string);
          destructor  Destroy; override;
        private
          fCachePath  : WideString;
          fWorldURL   : WideString;
          fLivingObjs : TLockableCollection;
        published
          function  RegisterWorld(const WorldName, ServerName : WideString; ServerPort : integer) : variant;
          function  CreateObject(const WorldName : WideString) : OleVariant;
          function  CreateIterator(const Path : WideString; Options : integer) : OleVariant;
          procedure CloseObject(Obj : integer);
        published
          property  CachePath : WideString read fCachePath;
          property  WorldURL  : WideString read fWorldURL;
        public
          procedure CheckObject(MaxTTL : TDateTime);
        public
          property LivingObjs : TLockableCollection read fLivingObjs;
      end;

  // TWorldRegistyServer

  constructor TWorldRegistyServer.Create(ConnectionsServer : IRDOConnectionsServer; MaxQueryThreads : integer; QueryCritSection : TCriticalSection; const RootName : string);
    begin
      inherited;
      fLivingObjs := TLockableCollection.Create(0, rkBelonguer);
    end;

  destructor TWorldRegistyServer.Destroy;
    begin
      fLivingObjs.Free;
      inherited;
    end;

  function TWorldRegistyServer.RegisterWorld(const WorldName, ServerName : WideString; ServerPort : integer) : variant;
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
                result := true;
              end
            else result := false;
        finally
          Reg.Free;
        end;
      except
        result := false;
      end;
    end;

  function TWorldRegistyServer.CreateObject(const WorldName : WideString) : OleVariant;
    var
      CacheObj : TCachedObjectWrap;
    begin
      try
        CacheObj := TCachedObjectWrap.Create;
        if CacheObj.SetWorld(WorldName)
          then
            begin
              result := integer(CacheObj);
              fLivingObjs.Insert(CacheObj);
            end
          else
            begin
              result := integer(0);
              CacheObj.Free;
            end;
      except
        result := integer(0);
      end;
    end;

  function TWorldRegistyServer.CreateIterator(const Path : WideString; Options : integer) : OleVariant;
    var
      Itr : TFolderIteratorWrap;
    begin
      try
        Itr := TFolderIteratorWrap.Create(Path, Options);
        result := integer(Itr);
      except
        result := 0;
      end;
    end;

  procedure TWorldRegistyServer.CloseObject(Obj : integer);
    begin
      try
        fLivingObjs.Delete(TObject(Obj));
      except
      end;
    end;

  procedure TWorldRegistyServer.CheckObject(MaxTTL : TDateTime);
    var
      CurDate : TDateTime;
      Obj     : TCachedObjectWrap;
      i       : integer;
    begin
      CurDate := Now;
      fLivingObjs.Lock;
      try
        i := 0;
        while i < fLivingObjs.Count do
          begin
            Obj := TCachedObjectWrap(fLivingObjs[i]);
            if CurDate - Obj.LastUpdate > MaxTTL
              then fLivingObjs.Delete(Obj)
              else inc(i);
          end;
      finally
        fLivingObjs.Unlock;
      end;
    end;

  // RemoveFullPath

  function RemoveFullPath(const Path : string) : boolean;
    var
      FileOp : TSHFileOpStruct;
      tmp    : array[0..MAX_PATH] of char;
    begin
      fillchar(tmp, sizeof(tmp), 0);
      strpcopy(tmp, Path);
      // If Path is a folder the last '\' must be removed.
      if Path[length(Path)] = '\'
        then tmp[length(Path)-1] := #0;
      with FileOp do
        begin
          wFunc  := FO_DELETE;
          Wnd    := 0;
          pFrom  := tmp;
          pTo    := nil;
          fFlags := FOF_NOCONFIRMATION or FOF_SILENT;
          hNameMappings := nil;
        end;
      result := SHFileOperation(FileOp) = 0;
    end;

  var
    CachePath   : string = '';
    CacheServer : TWorldRegistyServer = nil;

  // TCacheServerReport

  procedure TCacheServerReport.InitializeClick(Sender: TObject);
    var
      Reg        : TRegistry;
      ServerConn : IRDOConnectionsServer;
    begin
      try
        fMaxTTL     := EncodeTime(0, 5, 0, 0);
        ServerConn  := TWinSockRDOConnectionsServer.Create(StrToInt(Port.Text));
        CacheServer := TWorldRegistyServer.Create(ServerConn, 1, nil, WSObjectCacherName);
        CacheServer.fCachePath := GlobalPath.Text;
        CacheServer.fWorldURL  := WebSite.Text;
        LogTimer.Enabled := true;

        if (System.pos('cache', lowercase(LocalPath.Text)) <> 0) and DirectoryExists(LocalPath.Text) and DeleteCache
          then RemoveFullPath(LocalPath.Text);

        ForceDirectories(LocalPath.Text);

        StarPictureServer;

        Initialize.Enabled := false;

        {$IFDEF LOGS}
        SetLogFile(ExtractFilePath(Application.ExeName) + 'CacheServer.log');
        {$ENDIF}
        // Save the path in the registry
        Reg := TRegistry.Create;
        try
          Reg.RootKey := HKEY_LOCAL_MACHINE;
          if Reg.OpenKey(CacheKey, true)
            then
              begin
                Reg.WriteString('GlobalPath', GlobalPath.Text);
                Reg.WriteString('RootPath', LocalPath.Text);
                Reg.WriteString('WebSite', WebSite.Text);
                Reg.RegistryConnect('');
              end
        finally
          Reg.Free;
        end;
      except
        Application.MessageBox('Cannot initialize the server', 'Model Server', MB_ICONERROR or MB_OK);
      end;
      Application.Minimize;
    end;

  procedure TCacheServerReport.FormCreate(Sender: TObject);
    var
      Reg : TRegistry;
      str : string;
    begin
      try
        Reg := TRegistry.Create;
        try
          Reg.RootKey := HKEY_LOCAL_MACHINE;
          if Reg.OpenKey(CacheKey, false)
            then
              begin
                LocalPath.Text  := Reg.ReadString('RootPath');
                GlobalPath.Text := Reg.ReadString('GlobalPath');
                str := Reg.ReadString('WebSite');
                if str <> ''
                  then WebSite.Text := str;
              end;
        finally
          Reg.Free;
        end;
      except
      end;
    end;

procedure TCacheServerReport.LogTimerTimer(Sender: TObject);
  begin
    lbCacheObjCount.Caption := IntToStr(CacheServer.LivingObjs.Count);
    lbCacheItrCount.Caption := IntToStr(RDOCacheItrCount);
    if fTickCount > MaxTicks
      then
        begin
          CacheServer.CheckObject(fMaxTTL);
          fTickCount := 0;
        end
      else inc(fTickCount);
  end;

procedure TCacheServerReport.OnClientConnect(Sender : TObject; Socket : TCustomWinSocket);
  begin
    Socket.Data := TImageClient.Create;
  end;

procedure TCacheServerReport.OnClientError(Sender : TObject; Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer);
  begin
    ErrorCode := 0;
  end;

procedure TCacheServerReport.OnClientRead(Sender : TObject; Socket : TCustomWinSocket);
  var
    Client  : TImageClient;
    logData : TStringList;
    buffer  : array[0..MaxPictBufferSize-1] of byte;
    size    : integer;
  begin
    Client := TImageClient(Socket.Data);
    if Client <> nil
      then
        if not Client.Loaded
          then
            begin
              logData := TStringList.Create;
              logData.Text := Socket.ReceiveText;
              Client.Name := logData.Values['User'];
              Client.World := logData.Values['World'];
              Client.Loaded := true;
              try
                Client.Size := StrToInt(logData.Values['Size']);
                Socket.SendText('SEND');
              except
                Socket.SendText('ERROR');
                Socket.Data := nil;
                Client.Free;
              end;
            end
          else
            begin
              size := Socket.ReceiveBuf(buffer, sizeof(buffer));
              if size > 0
                then
                  if Client.Recieve(buffer, size)
                    then
                      begin
                        if SaveImage(Client)
                          then Socket.SendText('OK')
                          else Socket.SendText('ERROR');
                        Client.Free;
                        Socket.Data := nil;
                      end;
            end;
  end;

procedure TCacheServerReport.OnClientClose(Sender : TObject; Socket : TCustomWinSocket);
  begin
    if Socket.Data <> nil
      then TImageClient(Socket.Data).Free;
  end;

function TCacheServerReport.SaveImage(Client : TImageClient) : boolean;
  var
    path   : string;
    Stream : TStream;
  begin
    path := CacheServer.CachePath;
    if (path <> '') and (path[length(path)] = '\')
      then path := ExtractFilePath(copy(path, 1, pred(length(path))))
      else path := ExtractFilePath(path);
    path := path + 'UserInfo\' + Client.World + '\' + Client.Name;
    ForceDirectories(path);
    if DirectoryExists(path)
      then
        try
          Stream := TFileStream.Create(path + '\largephoto.jpg', fmCreate);
          try
            Client.Stream.Seek(0, 0);
            Stream.CopyFrom(Client.Stream, Client.Size);
            result := true;
          finally
            Stream.Free;
          end;
        except
          result := false;
        end
      else result := false;
  end;

procedure TCacheServerReport.StarPictureServer;
  begin
    fServerSocket := TServerSocket.Create(self);
    fServerSocket.Port := PictureServerPort;
    fServerSocket.OnClientConnect := OnClientConnect;
    fServerSocket.OnClientRead := OnClientRead;
    fServerSocket.OnClientDisconnect := OnClientClose;
    fServerSocket.OnClientError := OnClientError;
    fServerSocket.Open;
  end;

procedure TCacheServerReport.FormDestroy(Sender: TObject);
  begin
    if fServerSocket <> nil
      then fServerSocket.Close;
    fServerSocket.Free;
  end;

end.
