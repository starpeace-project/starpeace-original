unit CacheServerReportForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, SyncObjs, SocketComp, CacheManagerRDO,
  ImageClient;

const
  MaxTicks          = 20;
  PictureServerPort = 6010;
  MaxServerThreads  = 16;
  MaxAllowedSearch  = 150;

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
    TabSheet2: TTabSheet;
    Bevel1: TBevel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    UseDS: TCheckBox;
    DSAddress: TEdit;
    DSPort: TEdit;
    DSArea: TEdit;
    DSCluster: TEdit;
    StoreDSData: TButton;
    procedure InitializeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LogTimerTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StoreDSDataClick(Sender: TObject);
  private
    procedure StarPictureServer;
    procedure OnClientConnect(Sender : TObject; Socket : TCustomWinSocket);
    procedure OnClientRead(Sender : TObject; Socket : TCustomWinSocket);
    procedure OnClientClose(Sender : TObject; Socket : TCustomWinSocket);
    procedure OnClientError(Sender : TObject; Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer);
    function  SaveImage(Client : TImageClient) : boolean;
  private
    fTickCount     : integer;
    fMaxTTL        : TDateTime;
    fServerSocket  : TServerSocket;
  end;

var
  CacheServerReport : TCacheServerReport;
  KeepCache         : boolean;
  AutoRunCache      : boolean;
  RDOCacheObjCount  : integer;
  RDOCacheItrCount  : integer;

implementation

  uses
    Registry, CacheRegistryKeys, FileCtrl, ShellAPI, RDOInterfaces, HostNames,
    WinSockRDOConnectionsServer, RDORootServer, CacheCommon, CachedObjectWrap,
    FolderIteratorWrap, Collection, Logs, WinSockRDOConnection, RDOServer,
    RDOObjectProxy, InputSearchWrap, OutputSearchWrap, MathUtils;

  {$R *.DFM}

  const
    MaxPictBufferSize = 1024;

  type
    TCacheServer =
      class(TRDORootServer, ICacher)
        public
          constructor Create(ConnectionsServer : IRDOConnectionsServer; MaxQueryThreads : integer; QueryCritSection : TCriticalSection; const RootName : string);
          destructor  Destroy; override;
        private
          fCachePath  : WideString;
          fWorldURL   : WideString;
          fLivingObjs : TLockableCollection;
        published
          function  RegisterWorld(const WorldName, ServerName : WideString; ServerPort, Version : integer) : OleVariant;
          function  CreateObject(const WorldName : WideString) : OleVariant;
          function  CreateIterator(const Path : WideString; Options : integer) : OleVariant;
          procedure CloseObject(Obj : integer);
          //function  IPC_GetCache(World : widestring; Id, kind, info : integer) : OleVariant;
          //function  IPC_RenewCache(World, Agent, ObjId  : widestring) : OleVariant;
        published
          function FindSuppliers(const Output, World, Town, Name : widestring; Count, X, Y, SortMode, Role : integer) : OleVariant;
          function FindClients  (const Output, World, Town, Name : widestring; Count, X, Y, SortMode, Role : integer) : OleVariant;
        published
          property  CachePath : WideString read fCachePath;
          property  WorldURL  : WideString read fWorldURL;
        public
          procedure CheckObject(MaxTTL : TDateTime);
        public
          property LivingObjs : TLockableCollection read fLivingObjs;
        published
          function SimTimeOut(mmSec : integer) : OleVariant;
        private
          function  QueryInterface(const IID: TGUID; out Obj) : hresult; stdcall;
          function  _AddRef  : integer; stdcall;
          function  _Release : integer; stdcall;
          function  GetWorldProxy(Name : string) : IWorldProxy;
      end;

  // TCacheServer

  constructor TCacheServer.Create(ConnectionsServer : IRDOConnectionsServer; MaxQueryThreads : integer; QueryCritSection : TCriticalSection; const RootName : string);
    begin
      inherited;
      fLivingObjs := TLockableCollection.Create(0, rkBelonguer);
    end;

  destructor TCacheServer.Destroy;
    begin
      fLivingObjs.Free;
      inherited;
    end;

  function TCacheServer.RegisterWorld(const WorldName, ServerName : WideString; ServerPort, Version : integer) : OleVariant;
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
                Reg.WriteInteger('Version', Version);
                Reg.RegistryConnect('');
                result := true;
              end
            else result := false;
        finally
          Reg.Free;
        end;
      except
        result := false;
        Logs.Log( 'Survival', DateTimeToStr(Now) + ' Error registering Server.' );
      end;
    end;

  function TCacheServer.CreateObject(const WorldName : WideString) : OleVariant;
    var
      CacheObj : TCachedObjectWrap;
      trap     : integer;
    begin
      trap := 1;
      try
        CacheObj := TCachedObjectWrap.Create(self);
        trap := 2;
        if CacheObj.SetWorld(WorldName)
          then
            begin
              trap := 3;
              result := integer(CacheObj);
              trap := 4;
              fLivingObjs.Insert(CacheObj);
              trap := 5;
            end
          else
            begin
              trap := 6;
              result := integer(0);
              CacheObj.Free;
              trap := 7;
            end;
      except
        on E : Exception do
          begin
            result := integer(0);
            Logs.Log( 'Survival', DateTimeToStr(Now) + ' (2)- Error creating cache object. TRAP=' + IntToStr(trap) + ' ' + E.Message); // 2
          end;
      end;
    end;

  function TCacheServer.CreateIterator(const Path : WideString; Options : integer) : OleVariant;
    var
      Itr : TFolderIteratorWrap;
    begin
      try
        Itr := TFolderIteratorWrap.Create(Path, Options);
        result := integer(Itr);
      except
        result := 0;
        Logs.Log( 'Survival', DateTimeToStr(Now) + ' (3)- Error creating iterator.' ); // 3
      end;
    end;

  function TCacheServer.FindSuppliers(const Output, World, Town, Name : widestring; Count, X, Y, SortMode, Role : integer) : OleVariant;
    begin
      try
        result := OutputSearchWrap.FindConnections(Output, World, Town, Name, min(MaxAllowedSearch, Count), X, Y, SortMode, Role)
      except
        result := '';
      end;
    end;

  function TCacheServer.FindClients(const Output, World, Town, Name : widestring; Count, X, Y, SortMode, Role : integer) : OleVariant;
    begin
      try
        result := InputSearchWrap.FindConnections(Output, World, Town, Name, min(MaxAllowedSearch, Count), X, Y, SortMode, Role)
      except
        result := '';
      end;
    end;

  procedure TCacheServer.CloseObject(Obj : integer);
    begin
      try
        fLivingObjs.Delete(TObject(Obj));
      except
        Logs.Log( 'Survival', DateTimeToStr(Now) + ' (4)- Error removing object.' ); // 4
      end;
    end;

{
  function TCacheServer.IPC_GetCache(World : widestring; Id, kind, info : integer) : OleVariant;
    var
      tmpProxy : IWorldProxy;
      wideRes  : widestring;
    begin
      tmpProxy := WorldProxies.GetWorldProxy(World);
      if tmpProxy <> nil
        then wideRes := tmpProxy.GetCache(Id, kind, info)
        else wideRes := resError;
      result := wideRes;
    end;

  function TCacheServer.IPC_RenewCache(World, Agent, ObjId  : widestring) : OleVariant;
    var
      tmpProxy : IWorldProxy;
      wideRes  : widestring;
    begin
      tmpProxy := WorldProxies.GetWorldProxy(World);
      if tmpProxy <> nil
        then wideRes := tmpProxy.RenewCache(Agent, ObjId)
        else wideRes := resError;
      result := wideRes;
    end;
}

  procedure TCacheServer.CheckObject(MaxTTL : TDateTime);
    var
      CurDate : TDateTime;
      Obj     : TCachedObjectWrap;
      i       : integer;
    begin
      try
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
      except
        Logs.Log( 'Survival', DateTimeToStr(Now) + '(6) Error Checking Objects TTLs.' );
      end;
    end;

  function TCacheServer.SimTimeOut(mmSec : integer) : OleVariant;
    begin
      Sleep(mmSec);
      result := true;
    end;

  function TCacheServer.QueryInterface(const IID: TGUID; out Obj): hresult;
    begin
      pointer(Obj) := nil;
      result := E_FAIL;
    end;

  function TCacheServer._AddRef: integer;
    begin
      result := 1;
    end;

  function TCacheServer._Release: integer;
    begin
      result := 1;
    end;

  function TCacheServer.GetWorldProxy(Name : string) : IWorldProxy;
    begin
      result := WorldProxies.GetWorldProxy(Name);
    end;


  // RemoveFullPath

  function RemoveFullPath(const Path : string) : boolean;
    var
      FileOp : TSHFileOpStruct;
      tmp    : array[0..MAX_PATH] of char;
    begin
      try
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
            fFlags := 0;
            hNameMappings := nil;
          end;
        result := SHFileOperation(FileOp) = 0;
      except
        result := false;
        Logs.Log( 'Survival', DateTimeToStr(Now) + ' (5)- Error removing path: ' + Path); // 5
      end;
    end;

  var
    CachePath   : string = '';
    CacheServer : TCacheServer = nil;

  // TCacheServerReport

  procedure TCacheServerReport.InitializeClick(Sender: TObject);
    var
      Reg        : TRegistry;
      ServerConn : IRDOConnectionsServer;
    begin
      Logs.Log( 'Survival', DateTimeToStr(Now) + ' Starting Cache Server..');
      try
        fMaxTTL     := EncodeTime(0, 1, 0, 0);
        ServerConn  := TWinSockRDOConnectionsServer.Create(StrToInt(Port.Text));
        //ServerConn.AuthenticationMode := amSecure;
        CacheServer := TCacheServer.Create(ServerConn, MaxServerThreads, nil, WSObjectCacherName);
        CacheServer.fCachePath := GlobalPath.Text;
        CacheServer.fWorldURL  := WebSite.Text;
        LogTimer.Enabled := true;

        //if (System.pos('cache', lowercase(LocalPath.Text)) <> 0) and DirectoryExists(LocalPath.Text) and not KeepCache
          //then RemoveFullPath(LocalPath.Text);

        ForceDirectories(LocalPath.Text);

        StarPictureServer;

        Initialize.Enabled := false;

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
                Reg.WriteString('Port', Port.Text );
                Reg.RegistryConnect('');
              end
        finally
          Reg.Free;
        end;
      except
        Application.MessageBox('Cannot initialize the server', 'Model Server', MB_ICONERROR or MB_OK);
      end;
      if Sender <> self
        then Application.Minimize;
    end;

  procedure TCacheServerReport.FormCreate(Sender: TObject);

    procedure ReadDataFromDirSever( Address : string; Port : integer );
      var
        DSCnx   : IRDOConnectionInit;
        WSDSCnx : TWinSockRDOConnection;
        DSProxy : OleVariant;
        session : integer;
      begin
        WSDSCnx      := TWinSockRDOConnection.Create('WSDSCnx');
        DSCnx        := WSDSCnx;
        DSCnx.Server := Address;
        DSCnx.Port   := Port;
        DSProxy      := TRDOObjectProxy.Create as IDispatch;
        if DSCnx.Connect( 20000 )
          then
            begin
              DSProxy.SetConnection( DSCnx );
              DSProxy.BindTo( 'DirectoryServer' );
              DSProxy.TimeOut := 20000;
              DSProxy.WaitForAnswer := true;
              session         := DSProxy.RDOOpenSession;
              if session <> 0
                then
                  begin
                    DSProxy.BindTo( session );
                    DSProxy.RDOCurrentKey := 'Root/Areas/' + DSArea.Text + '/Clusters/' + DSCluster.Text + '/Cache';
                    LocalPath.Text  := DSProxy.RDOReadString( 'RootPath' );
                    GlobalPath.Text := DSProxy.RDOReadString( 'GlobalPath' );
                    WebSite.Text    := DSProxy.RDOReadString( 'WebSite' );
                    self.Port.Text  := DSProxy.RDOReadString( 'Port' );
                    DSProxy.RDOWriteString( 'IP', GetLocalAddress );
                  end
                else raise Exception.Create( 'Cannot create session!' );
            end
          else raise Exception.Create( 'Cannot connect to Directory Server!' );
      end;

    var
      Reg : TRegistry;
    begin
      try
        Reg := TRegistry.Create;
        try
          Reg.RootKey := HKEY_LOCAL_MACHINE;
          if Reg.OpenKey( CacheKey, false )
            then
              begin
                try
                  UseDS.Checked  := Reg.ReadBool( 'UseDirServer' );
                  DSAddress.Text := Reg.ReadString( 'DSAddr' );
                  DSPort.Text    := Reg.ReadString( 'DSPort' );
                  DSArea.Text    := Reg.ReadString( 'Area' );
                  DSCluster.Text := Reg.ReadString( 'Cluster' );
                except
                  UseDS.Checked := false;
                end;
                if UseDS.Checked
                  then ReadDataFromDirSever( DSAddress.Text, StrToInt(DSPort.Text) )
                  else
                    begin
                      LocalPath.Text  := Reg.ReadString( 'RootPath' );
                      GlobalPath.Text := Reg.ReadString( 'GlobalPath' );
                      WebSite.Text    := Reg.ReadString( 'WebSite' );
                      Port.Text       := Reg.ReadString( 'Port' );
                    end
              end;
        finally
          Reg.Free;
        end;
      except
      end
    end;

procedure TCacheServerReport.LogTimerTimer(Sender: TObject);
  begin
    try
      if fTickCount > MaxTicks
        then
          begin
            lbCacheObjCount.Caption := IntToStr(CacheServer.LivingObjs.Count);
            lbCacheItrCount.Caption := IntToStr(RDOCacheItrCount);
            CacheServer.CheckObject(fMaxTTL);
            try
              WorldProxies.CheckHistories;
            except
              Logs.Log( 'Survival', DateTimeToStr(Now) + ' (6.1)- Error in LogTimerTimer checking object''s history.' );
            end;
            fTickCount := 0;
          end
        else inc(fTickCount);
    except
      Logs.Log( 'Survival', DateTimeToStr(Now) + ' (6)- Error in LogTimerTimer.' );
    end;
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
    try
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
    except
      Logs.Log( 'Survival', DateTimeToStr(Now) + ' (7)- Error in Image Client OnClientRead.' ); // 6
    end;
  end;

procedure TCacheServerReport.OnClientClose(Sender : TObject; Socket : TCustomWinSocket);
  begin
    try
      if Socket.Data <> nil
        then
          begin
            TImageClient(Socket.Data).Free;
            Socket.Data := nil;
          end;
    except
      Logs.Log( 'Survival', DateTimeToStr(Now) + ' (8)- Error in Image Client OnClientClose.' ); // 8
    end;
  end;

function TCacheServerReport.SaveImage(Client : TImageClient) : boolean;
  var
    path   : string;
    Stream : TStream;
  begin
    try
      path := CacheServer.CachePath;
      if (path <> '') and (path[length(path)] = '\')
        then path := ExtractFilePath(copy(path, 1, pred(length(path))))
        else path := ExtractFilePath(path);
      path := path + 'Web\UserInfo\' + Client.World + '\' + Client.Name;
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
    except
      Logs.Log( 'Survival', DateTimeToStr(Now) + ' (9)- Error in Image Client SaveImage.' ); // 9
      result := false;
    end;
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
    CacheServer.Free;
    if fServerSocket <> nil
      then fServerSocket.Close;
    fServerSocket.Free;
  end;

procedure TCacheServerReport.FormShow(Sender: TObject);
  begin
    if AutoRunCache and Initialize.Enabled
      then InitializeClick(self);
  end;


procedure TCacheServerReport.StoreDSDataClick(Sender: TObject);
  var
    DSCnx   : IRDOConnectionInit;
    WSDSCnx : TWinSockRDOConnection;
    DSProxy : OleVariant;
    session : integer;
    key     : string;
    Reg     : TRegistry;
    //useless : WordBool;
  begin
    WSDSCnx      := TWinSockRDOConnection.Create('WSDSCnx');
    DSCnx        := WSDSCnx;
    DSCnx.Server := DSAddress.Text;
    DSCnx.Port   := StrToInt(DSPort.Text);
    DSProxy      := TRDOObjectProxy.Create as IDispatch;
    if DSCnx.Connect( 20000 )
      then
        begin
          DSProxy.SetConnection( DSCnx );
          DSProxy.BindTo( 'DirectoryServer' );
          DSProxy.TimeOut := 20000;
          session := DSProxy.RDOOpenSession;
          if session <> 0
            then
              begin
                DSProxy.BindTo( session );
                DSProxy.WaitForAnswer := true;
                key := 'Root/Areas/' + DSArea.Text + '/Clusters/' + DSCluster.Text + '/Cache';
                if DSProxy.RDOCreateFullPathKey( key, true )
                  then
                    begin
                      //useless := DSProxy.RDOSetSecurityLevel(wordbool(false));
                      DSProxy.RDOCurrentKey := key;
                      DSProxy.RDOWriteString( 'GlobalPath', GlobalPath.Text);
                      DSProxy.RDOWriteString( 'RootPath', LocalPath.Text);
                      DSProxy.RDOWriteString( 'WebSite', WebSite.Text);
                      //useless := DSProxy.RDOSetSecurityLevel(wordbool(true));
                      DSProxy.RDOWriteString( 'Port', Port.Text );
                      Reg := TRegistry.Create;
                      try
                        Reg.RootKey := HKEY_LOCAL_MACHINE;
                        if Reg.OpenKey( CacheKey, false )
                          then
                            begin
                              Reg.WriteBool( 'UseDirServer', UseDS.Checked );
                              Reg.WriteString( 'DSAddr', DSAddress.Text );
                              Reg.WriteString( 'DSPort', DSPort.Text );
                              Reg.WriteString( 'Area', DSArea.Text );
                              Reg.WriteString( 'Cluster', DSCluster.Text );
                            end;
                      finally
                        Reg.Free;
                      end;
                    end;
              end
            else raise Exception.Create( 'Cannot create session!' );
        end
      else raise Exception.Create( 'Cannot connect to Directory Server!' );
  end;

initialization

  KeepCache    := false;
  AutoRunCache := false;

end.
