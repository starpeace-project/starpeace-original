unit InterfaceServerReportForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, InterfaceServer, SocketComp;

type
  TInterfaceServerReport = class(TForm)
    Panel1: TPanel;
    Image1: TImage;
    PageControl1: TPageControl;
    TabSheet2: TTabSheet;
    Label11: TLabel;
    daPort: TEdit;
    Label5: TLabel;
    daHostAddress: TEdit;
    Bevel1: TBevel;
    Label1: TLabel;
    CallbackPort: TEdit;
    TabSheet1: TTabSheet;
    Mail: TTabSheet;
    Label7: TLabel;
    Label8: TLabel;
    MailServer: TEdit;
    MailPort: TEdit;
    StartServer: TButton;
    ClientsPort: TEdit;
    Label2: TLabel;
    eFixedIp: TEdit;
    Label3: TLabel;
    TabSheet3: TTabSheet;
    Bevel2: TBevel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    UseDS: TCheckBox;
    DSAddress: TEdit;
    DSPort: TEdit;
    DSArea: TEdit;
    DSCluster: TEdit;
    StoreDSData: TButton;
    Label4: TLabel;
    WorldName: TEdit;
    TabSheet4: TTabSheet;
    Label6: TLabel;
    Label9: TLabel;
    edGMAddr: TEdit;
    edGMPort: TEdit;
    Label10: TLabel;
    edGMId: TEdit;
    Reconnect: TButton;
    btnBlackBox: TButton;
    tReconnect: TTimer;
    procedure StartServerClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StoreDSDataClick(Sender: TObject);
    procedure ReconnectClick(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
    procedure btnBlackBoxClick(Sender: TObject);
    procedure tReconnectTimer(Sender: TObject);
  private
    fInterfaceServer : TInterfaceServer;
  private
    //procedure SaveRegistryData;
    //procedure LoadRegistryData;
    procedure OnBBConnect(Sender : TObject; Socket : TCustomWinSocket);
    procedure OnBBError(Sender : TObject; Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer);
    procedure OnBBDisconnect(Sender : TObject; Socket : TCustomWinSocket);
  private
    fSilent   : boolean;
  end;

var
  InterfaceServerReport: TInterfaceServerReport;

implementation

{$R *.DFM}

  uses
    LogFile, Registry, HostNames, WinSockRDOConnection, RDOServer, RDOInterfaces,
    RDOObjectProxy, ISMLS, Logs, WinSock;

  const
    ISKey  = '\Software\Wow6432Node\Starpeace\Five\InterfaceServer\';
    WorldNameS : string = '';
{
  procedure TInterfaceServerReport.SaveRegistryData;
    var
      Reg : TRegistry;
    begin
      Reg := TRegistry.Create;
      try
        Reg.RootKey := HKEY_LOCAL_MACHINE;
        if Reg.OpenKey(ISKey, true)
          then
            begin
              Reg.WriteString('MSAddr', daHostAddress.Text);
              Reg.WriteString('MSPort', daPort.Text);
              Reg.WriteString(ExtractFileName(Application.ExeName) + '.CallBack', CallbackPort.Text);
              Reg.WriteString('ClientsPort', ClientsPort.Text);
              Reg.WriteString('MailServer', MailServer.Text);
              Reg.WriteString('MailPort', MailPort.Text);
              Reg.WriteString('ProxyIP', eFixedIp.Text);
              Reg.WriteString('GMId', edGMId.Text);
              Reg.WriteString('GMAddr', edGMAddr.Text);
              Reg.WriteString('GMPort', edGMPort.Text);
            end;
      finally
        Reg.Free;
      end;
    end;
}

{
  procedure TInterfaceServerReport.LoadRegistryData;
    var
      Reg : TRegistry;
      aux : string;
    begin
      Reg := TRegistry.Create;
      try
        Reg.RootKey := HKEY_LOCAL_MACHINE;
        if Reg.OpenKey(ISKey, false)
          then
            begin
              daHostAddress.Text := Reg.ReadString('MSAddr');
              daPort.Text        := Reg.ReadString('MSPort');
              aux                := Reg.ReadString(ExtractFileName(Application.ExeName) + '.CallBack');
              if aux = ''
                then aux := IntToStr(7005 + random(100));
              CallbackPort.Text := aux;
              ClientsPort.Text   := Reg.ReadString('ClientsPort');
              MailServer.Text    := Reg.ReadString('MailServer');
              MailPort.Text      := Reg.ReadString('MailPort');
              eFixedIp.Text      := Reg.ReadString('ProxyIP');
              try
                edGMId.Text := Reg.ReadString('GMId');
              except
                edGMId.Text := 'GMS1';
              end;
              try
                edGMAddr.Text := Reg.ReadString('GMAddr');
              except
                edGMAddr.Text := 'www.starpeace.net';
              end;
              try
                edGMPort.Text := Reg.ReadString('GMPort');
              except
                edGMPort.Text := '1200';
              end;
            end;
      finally
        Reg.Free;
      end;
    end;
}

  procedure TInterfaceServerReport.StartServerClick(Sender: TObject);
    begin
      try
        try
          IsMLS.LoadMLS;
        except
          // >>
        end;
        fInterfaceServer :=
          TInterfaceServer.Create(
            daHostAddress.Text,
            MailServer.Text,
            DSAddress.Text,
            edGMAddr.Text,
            DSArea.Text,
            StrToInt(daPort.Text),
            StrToInt(CallbackPort.Text),
            StrToInt(MailPort.Text),
            StrToInt(DSPort.Text),
            StrToInt(edGMPort.Text),
            StrToInt(ClientsPort.Text),
            eFixedIp.Text,
            ExtractFilePath(ParamStr(0)) + 'BannedPlayers.txt' );
        try
          fInterfaceServer.InitMailServer(MailServer.Text, StrToInt(MailPort.Text));
          StartServer.Enabled := false;
          Caption := fInterfaceServer.WorldName + ' - ' + Caption;
          Application.Title := Application.Title + ' - ' + fInterfaceServer.WorldName;
        except
          fInterfaceServer.Free;
          Application.MessageBox( 'Cannot connect to the Mail Server!', 'Error', MB_ICONERROR or MB_OK );
        end;
        tReconnect.Enabled := true;
      except
        Application.MessageBox( 'Cannot initialize Interface Server!', 'Error', MB_ICONERROR or MB_OK );
      end;
    end;

  procedure TInterfaceServerReport.FormDestroy(Sender: TObject);
    begin
      fInterfaceServer.Free;
    end;

  procedure TInterfaceServerReport.FormCreate(Sender: TObject);

    procedure ReadDataFromDirSever( Address : string; Port : integer );
      var
        DSCnx   : IRDOConnectionInit;
        WSDSCnx : TWinSockRDOConnection;
        DSProxy : OleVariant;
        session : integer;
        useless : integer;
        //key     : string;
        //aux     : string;
      begin
        WSDSCnx      := TWinSockRDOConnection.Create( 'DS' );
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
              session  := DSProxy.RDOOpenSession;
              if session <> 0
                then
                  begin
                    DSProxy.BindTo( session );
                    try
                      if DSProxy.RDOSetCurrentKey('Root/Areas/' + DSArea.Text + '/Worlds/' + WorldName.Text + '/Interface')
                        then
                          begin
                            edGMId.Text := DSProxy.RDOReadString( 'GMId' );
                            ClientsPort.Text := IntToStr(DSProxy.RDOReadInteger( 'Port' ));

                            useless := DSProxy.RDOSetSecurityLevel(wordbool(false));
                            try
                              eFixedIp.Text := DSProxy.RDOReadString( 'ProxyIP' );
                              if eFixedIp.Text = ''
                                then DSProxy.RDOWriteString( 'IP', GetLocalAddress )
                                else DSProxy.RDOWriteString( 'IP', eFixedIp.Text );
                            finally
                              useless := DSProxy.RDOSetSecurityLevel(wordbool(true));
                            end;

                            if DSProxy.RDOSetCurrentKey('Root/Areas/' + DSArea.Text + '/Clusters/' + DSCluster.Text + '/Mail')
                              then
                                begin
                                  MailServer.Text := DSProxy.RDOReadString( 'IP' );
                                  MailPort.Text := IntToStr(DSProxy.RDOReadInteger( 'Port' ));
                                end;

                            if DSProxy.RDOSetCurrentKey('Root/Areas/' + DSArea.Text + '/Worlds/' + WorldName.Text + '/Model')
                              then
                                begin
                                  daHostAddress.Text := DSProxy.RDOReadString( 'IP' );
                                  daPort.Text := IntToStr(DSProxy.RDOReadInteger( 'Port' ));
                                end;

                            if DSProxy.RDOSetCurrentKey('Root/GM/' + edGMId.Text)
                              then
                                begin
                                  edGMAddr.Text := DSProxy.RDOReadString( 'Address' );
                                  edGMPort.Text := DSProxy.RDOReadString('Port');
                                end;
                          end
                    finally
                      DSProxy.RDOEndSession;
                    end;
                  end
                else raise Exception.Create( 'Cannot create session!' );
            end
          else raise Exception.Create( 'Cannot connect to Directory Server!' );
      end;

    var
      Reg : TRegistry;
      aux : string;
      FindKey : boolean;

    begin
      try
        if ParamStr(2) <> ''
          then
            WorldNameS := ParamStr(2)+'\'
          else
            begin
              aux := ExtractFileName( ParamStr(0) );
              if pos( uppercase('FIVEInterfaceServer'), uppercase(aux) ) <> 0
                then
                  WorldNameS := ''
                else
                  begin
                    aux := copy( aux, 4, 255 ); //IS-Worldname.exe
                    WorldNameS := copy( aux, 1, pos( '.', aux )-1 )+'\';
                  end;
            end;
        Reg := TRegistry.Create;
        try
          Reg.RootKey := HKEY_LOCAL_MACHINE;
          FindKey := Reg.OpenKey( ISKey + WorldNameS , false );
          if not FindKey
            then
              FindKey := Reg.OpenKey( ISKey, false );
          if FindKey//Reg.OpenKey( ISKey + WorldNameS , false )
            then
              begin
                if paramstr(2) = ''
                  then WorldName.Text := Reg.ReadString( 'WorldName' )
                  else WorldName.Text := paramstr(2);
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
                      daHostAddress.Text := Reg.ReadString('MSAddr');
                      daPort.Text        := Reg.ReadString('MSPort');
                      ClientsPort.Text   := Reg.ReadString('ClientsPort');
                      MailServer.Text    := Reg.ReadString('MailServer');
                      MailPort.Text      := Reg.ReadString('MailPort');
                      eFixedIp.Text      := Reg.ReadString('ProxyIP');
                    end;
                aux := IntToStr(StrToInt(ClientsPort.Text) + 100);
                CallbackPort.Text := aux;
              end;
        finally
          Reg.Free;
        end;
      except
      end
      {$IFDEF LogsEnabled}
      SetLogFile( ExtractFilePath(Application.ExeName) + 'InterfaceServer.log' );
      {$ENDIF}
    end;

  procedure TInterfaceServerReport.FormShow(Sender: TObject);
    begin
      if uppercase(paramstr(1)) = 'AUTORUN'
        then StartServerClick( self );
      if (uppercase(paramstr(1)) = 'BOXED') or (uppercase(paramstr(2)) = 'BOXED') or (uppercase(paramstr(3)) = 'BOXED')
        then
          begin
            fSilent := true;
            btnBlackBoxClick(Sender);
          end;
    end;

  procedure TInterfaceServerReport.StoreDSDataClick(Sender: TObject);
    var
      DSCnx   : IRDOConnectionInit;
      WSDSCnx : TWinSockRDOConnection;
      DSProxy : OleVariant;
      session : integer;
      key     : string;
      Reg     : TRegistry;
      useless : integer;
    begin
      WSDSCnx      := TWinSockRDOConnection.Create( 'DS' );
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
                try
                  DSProxy.WaitForAnswer := true;
                  DSProxy.BindTo( session );
                  try
                    useless := DSProxy.RDOSetSecurityLevel(wordbool(false));
                  except
                  end;
                  key := 'Root/Areas/' + DSArea.Text + '/Worlds/' + WorldName.Text + '/Interface';
                  if DSProxy.RDOCreateFullPathKey( key, true )
                    then
                      begin
                        DSProxy.RDOCurrentKey := key;
                        DSProxy.RDOWriteString( 'Cluster', DSCluster.Text );
                        DSProxy.RDOWriteString( 'GMId', edGMId.Text );
                        DSProxy.RDOWriteString( 'Cluster', DSCluster.Text );
                        if daPort.Text = ''
                          then daPort.Text := '8000';
                        DSProxy.RDOWriteInteger( 'Port', StrToInt(ClientsPort.Text) );
                        DSProxy.RDOWriteInteger( 'CallBackPort', StrToInt(CallbackPort.Text) );
                        if eFixedIp.Text = ''
                          then DSProxy.RDOWriteString('IP', GetLocalAddress)
                          else
                            begin
                              DSProxy.RDOWriteString('IP', eFixedIp.Text);
                              DSProxy.RDOWriteString('ProxyIP', eFixedIp.Text);
                            end;
                        Reg := TRegistry.Create;
                        try
                          Reg.RootKey := HKEY_LOCAL_MACHINE;
                          if Reg.OpenKey( ISKey + WorldNameS, true )
                            then
                              begin
                                Reg.WriteBool( 'UseDirServer', UseDS.Checked );
                                Reg.WriteString( 'DSAddr', DSAddress.Text );
                                Reg.WriteString( 'DSPort', DSPort.Text );
                                Reg.WriteString( 'CallBackPort', CallbackPort.Text );
                                Reg.WriteString( 'Area', DSArea.Text );
                                Reg.WriteString( 'Cluster', DSCluster.Text );
                                if paramstr(2) = ''
                                  then Reg.WriteString( 'WorldName', WorldName.Text );
                              end;
                        finally
                          Reg.Free;
                        end;
                      end;
                finally
                  DSProxy.RDOEndSession;
                end
              else raise Exception.Create( 'Cannot create session!' );
          end
        else raise Exception.Create( 'Cannot connect to Directory Server!' );
    end;

  procedure TInterfaceServerReport.ReconnectClick(Sender: TObject);
    begin
      with fInterfaceServer do
        try
          InitMailServer( MailAddr, MailPort );
        except
        end;
      with fInterfaceServer do
        try
          InitGMServer( GMAddr, GMPort );
        except
        end;
      with fInterfaceServer do
        try
          InitDSServer( DSAddr, DSPort, DSArea );
        except
        end;
    end;

  procedure TInterfaceServerReport.Image1DblClick(Sender: TObject);
    begin
      ISMLS.SaveMLS;
    end;

  procedure TInterfaceServerReport.btnBlackBoxClick(Sender: TObject);
    {var
      BBForm : TBBForm;
      ip     : integer;
      LogRec : PLogSocketRec;}
    begin
      {BBForm := TBBForm.Create(self);
      try
        if fSilent or ((BBForm.ShowModal = mrOk) and (BBForm.cbLogId.Text <> ''))
          then
            begin
              new(LogRec);
              LogRec.Socket := TClientSocket.Create(self);
              if fSilent
                then LogRec.id := 'survival'
                else LogRec.id := BBForm.cbLogId.Text;
              ip := inet_addr(pchar(BBForm.eServer.Text));
              if ip = u_long(INADDR_NONE)
                then LogRec.Socket.Host := BBForm.eServer.Text
                else LogRec.Socket.Address := BBForm.eServer.Text;
              try
                LogRec.Socket.Port := StrToInt(BBForm.ePort.Text);
              except
                LogRec.Socket.Port := 9911;
              end;
              LogRec.Socket.Socket.Data := LogRec;
              LogRec.Socket.OnConnect := OnBBConnect;
              LogRec.Socket.OnError := OnBBError;
              LogRec.Socket.OnDisconnect := OnBBDisconnect;
              LogRec.Socket.Active := true;

              InitLogProxy(BBForm.eServer.Text, LogRec.Socket.Port + 1);
            end;
      finally
        BBForm.Free;
      end;}
    end;

  procedure TInterfaceServerReport.OnBBConnect(Sender : TObject; Socket : TCustomWinSocket);
    //var
      //LogRec : PLogSocketRec;
    begin
      {LogRec := PLogSocketRec(Socket.Data);
      if LogRec <> nil
        then
          begin
            Logs.InitSocket(LogRec.id, LogRec.Socket);
            LogRec.Socket.Socket.SendText(ExtractFileName(Application.ExeName) + '.' + LogRec.id + ^M^J);
            if not fSilent
              then ShowMessage(LogRec.id + ' is Connected to Black-Box Server');
          end;}
    end;

  procedure TInterfaceServerReport.OnBBError(Sender : TObject; Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer);
    begin
      ErrorCode := 0;
    end;

  procedure TInterfaceServerReport.OnBBDisconnect(Sender : TObject; Socket : TCustomWinSocket);
    //var
      //LogRec : PLogSocketRec;
    begin
      {LogRec := PLogSocketRec(Socket.Data);
      if LogRec <> nil
        then
          begin
            Logs.InitSocket(LogRec.id, nil);
            Dispose(LogRec);
            Socket.Data := nil;
          end;}
    end;

  procedure TInterfaceServerReport.tReconnectTimer(Sender: TObject);
    begin
      if fInterfaceServer <> nil
        then
          begin
            if fInterfaceServer.MaintDue
              then Halt; //Close;
            try
              with fInterfaceServer do
                if not DSOK
                  then
                    begin
                      Logs.Log('Survival', 'Start reconnecting DS..');
                      CreateDSConnection;
                      Logs.Log('Survival', 'End reconnecting DS..');
                    end;
            except
              Logs.Log('Survival', 'Error reconnecting DS..');
            end;
            try
              with fInterfaceServer do
                if MailConn = nil
                  then
                    begin
                      Logs.Log('Survival', 'Start reconnecting Mail..');
                      InitMailServer(MailAddr, MailPort);
                      Logs.Log('Survival', 'End reconnecting Mail..');
                    end;
            except
              Logs.Log('Survival', 'Error reconnecting Mail..');
            end;
            {try
              with fInterfaceServer do
                if GMClientConn = nil
                  then
                    begin
                      Logs.Log('Survival', 'Start reconnecting GM..');
                      InitGMServer(GMAddr, GMPort);
                      Logs.Log('Survival', 'End reconnecting GM..');
                    end;
            except
              Logs.Log('Survival', 'Error reconnecting GM..');
            end;}
          end;

    end;

end.


