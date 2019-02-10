unit ModelServerReportForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, Buttons, Spin, SocketComp, MemoryManager,
  ShareMem;

const
  ModelServerKey = '\Software\Oceanus\Five\ModelServer\'; 

type
  TModelServerReport = class(TForm)
    Panel1: TPanel;
    Image1: TImage;
    PageControl1: TPageControl;
    General: TTabSheet;
    CreateWorld: TButton;
    TabSheet2: TTabSheet;
    Label8: TLabel;
    csHostAddress: TEdit;
    Label9: TLabel;
    csServerPort: TEdit;
    Label10: TLabel;
    csCallbackPort: TEdit;
    TabSheet3: TTabSheet;
    mLog: TListBox;
    TabSheet4: TTabSheet;
    Label11: TLabel;
    daPort: TEdit;
    TabSheet5: TTabSheet;
    RegisteredExtensions: TListBox;
    Label12: TLabel;
    AddExtension: TButton;
    OpenExtension: TOpenDialog;
    Label1: TLabel;
    BaseDir: TEdit;
    Label2: TLabel;
    StatsTimer: TTimer;
    TabSheet6: TTabSheet;
    BPS: TLabel;
    LoadProgress: TProgressBar;
    MailSheet: TTabSheet;
    Label7: TLabel;
    Label14: TLabel;
    msHostAddress: TEdit;
    MailServerPort: TEdit;
    WorldName: TEdit;
    NewsServerPort: TEdit;
    Label13: TLabel;
    ReconnectNews: TButton;
    ReconnectMail: TButton;
    Comment: TLabel;
    cbCompanies: TCheckBox;
    cbFacilities: TCheckBox;
    cbTycoons: TCheckBox;
    cbTowns: TCheckBox;
    Label5: TLabel;
    Label6: TLabel;
    SatellitePath: TEdit;
    TabSheet1: TTabSheet;
    Bevel1: TBevel;
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
    ReconnectDS: TButton;
    Label19: TLabel;
    Label20: TLabel;
    seBackupRate: TSpinEdit;
    Panel2: TPanel;
    btnBlackbox: TButton;
    tReconnect: TTimer;
    Label21: TLabel;
    seDT: TSpinEdit;
    spPurge: TSpinEdit;
    Label22: TLabel;
    cbLinks: TCheckBox;
    CheckBox1: TCheckBox;
    lbMemUsage: TLabel;
    MemTimer: TTimer;
    Label23: TLabel;
    lbMailMessages: TLabel;
    btSendMail: TButton;
    Label24: TLabel;
    lbMessagesSent: TLabel;
    btTimeWarp: TButton;
    cbAutoRestart: TCheckBox;
    eMaintDate: TEdit;
    Label3: TLabel;
    BackupTimer: TTimer;
    procedure CreateWorldClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure isConnectClick(Sender: TObject);
    procedure StatsTimerTimer(Sender: TObject);
    procedure DASpeedChange(Sender: TObject);
    procedure ReconnectNewsClick(Sender: TObject);
    procedure ReconnectMailClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StoreDSDataClick(Sender: TObject);
    procedure ReconnectDSClick(Sender: TObject);
    procedure seBackupRateChange(Sender: TObject);
    procedure btnBlackboxClick(Sender: TObject);
    procedure tReconnectTimer(Sender: TObject);
    procedure seDTChange(Sender: TObject);
    procedure spPurgeChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure cbFacilitiesClick(Sender: TObject);
    procedure cbLinksClick(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure MemTimerTimer(Sender: TObject);
    procedure btSendMailClick(Sender: TObject);
    procedure btTimeWarpClick(Sender: TObject);
    procedure eMaintDateChange(Sender: TObject);
    procedure cbAutoRestartClick(Sender: TObject);
    procedure BackupTimerTimer(Sender: TObject);
  private
    fProgessBarPos : byte;
    fRunning       : boolean;
    fVersion       : integer;
    fBackupError   : boolean;
    fSimReport     : TStringList;
    fLastRestarted : TDateTime;
    fMaxInvestors  : integer;
  private
    procedure UpdateProgress(Comment : string; aPos : byte);
    //procedure OnBBConnect(Sender : TObject; Socket : TCustomWinSocket);
    //procedure OnBBError(Sender : TObject; Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer);
    //procedure OnBBDisconnect(Sender : TObject; Socket : TCustomWinSocket);
  private
    fBkCount : integer;
  private
    function CopyFile(const src, dest : string) : boolean;
  end;

var
  ModelServerReport: TModelServerReport;

implementation

  {$R *.DFM}

  uses
    ModelServer, LogFile, Logs, Registry, ModelServerCache, World, HostNames,
    BBLogsFrm, WinSockRDOConnection, RDOServer, RDOInterfaces, RDOObjectProxy,
    DirectoryServerProtocol, Winsock, ShellAPI, CompStringsParser;

  procedure TModelServerReport.CreateWorldClick(Sender: TObject);

    procedure InsertExtensionNames;
      var
        i : integer;
      begin
        RegisteredExtensions.Items.Clear;
        for i := 0 to pred(TheModelServer.ExtensionList.Count) do
          RegisteredExtensions.Items.Add( ExtractFileName(TheModelServer.ExtensionList[i]) );
      end;

    function GetCacheOptions : TCacheOptions;
      begin
        result := [];
        if cbFacilities.Checked
          then Include(result, coFacilities)
          else
            if cbLinks.Checked
              then Include(result, coLinks);
        if cbTycoons.Checked
          then Include(result, coTycoons);
        if cbCompanies.Checked
          then Include(result, coCompanies);
        if cbTowns.Checked
          then Include(result, coTowns);
      end;

    var
      Reg : TRegistry;

    begin
      if fRunning
        then TheModelServer.ForceBackup := true
        else
          begin
            Logs.Log( 'Survival', 'Server launched!' );
            try
              Logs.Log( 'Survival', 'Storing config to registry...' );
              Reg := TRegistry.Create;
              try
                Reg.RootKey := HKEY_LOCAL_MACHINE;
                if Reg.OpenKey( ModelServerKey, true )
                  then
                    begin
                      Reg.WriteString('BaseDir', BaseDir.Text);
                      Reg.WriteString('WorldName', WorldName.Text);
                      Reg.WriteString('CacheServer', csHostAddress.Text);
                      //Reg.WriteString('InterfaceServer', isHostAddress.Text);
                      Reg.WriteString('MailServer', msHostAddress.Text);
                      Reg.WriteString('SatellitePath', SatellitePath.Text );
                      Reg.WriteString('Port', daPort.Text );
                      Reg.WriteString('Version', IntToStr(fVersion));
                      Reg.WriteBool('AutoRestart', cbAutoRestart.Checked);
                      Reg.WriteInteger('PurgeCount', spPurge.Value);
                    end;
               Logs.Log( 'Survival', 'Storing config to registry OK.' );
              finally
                Reg.Free;
              end;
            except
              Logs.Log( 'Survival', 'Error reading config from registry.' );
            end;
            try
              TheModelServer.CacheOptions := GetCacheOptions;
              ModelServerCache.MSVersion  := fVersion;
              TheModelServer.LastDateUp   := fLastRestarted;
              CreateWorld.Enabled         := false;
              Logs.Log( 'Survival', 'Proceding to start world...' );
              TheModelServer.InitWorld(
                WorldName.Text,
                UpdateProgress,
                BaseDir.Text,
                csHostAddress.Text,
                SatellitePath.Text,
                StrToInt(daPort.Text),
                StrToInt(csCallbackPort.Text),
                StrToInt(csServerPort.Text),
                //TThreadPriority(SimSpeed.Position),
                msHostAddress.Text,
                StrToInt(MailServerPort.Text),
                StrToInt(NewsServerPort.Text) );
              TheModelServer.InitDirServer( DSArea.Text, DSAddress.Text, StrToInt(DSPort.Text) );

              // Force Max Investors
              if fMaxInvestors > 0
                then TheModelServer.MaxInvestors := fMaxInvestors;

              TheModelServer.SetPurgeTyccons(spPurge.Value);

              AddExtension.Enabled := true;
              InsertExtensionNames;
              //CreateWorld.Enabled := false;
              CreateWorld.Enabled := true;
              CreateWorld.Caption := '&Save World';
              fRunning            := true;
              //SimSpeed.Enabled    := false;
              //DASpeed.Enabled     := false;
              tReconnect.Enabled  := true;
              btSendMail.Enabled := true;
              btTimeWarp.Enabled := true;
              // Restart Data
              TheModelServer.AutoRestart := cbAutoRestart.Checked;
              if TheModelServer.AutoRestart and (eMaintDate.Text <> '')
                then
                  try
                    TheModelServer.RestartDate := StrToDateTime(eMaintDate.Text);
                  except
                    TheModelServer.RestartDate := Now + 1000;
                  end
                else TheModelServer.RestartDate := Now + 1000;
              BackupTimer.Enabled := true;
              fBkCount := 1;
            except
              on E : Exception do
                begin
                  Logs.Log( 'Survival', 'Error calling InitWorld: ' + E.Message );
                  Application.MessageBox( pchar('Cannot create world!'#13#10 + E.Message), 'Model Server', MB_ICONERROR or MB_OK );
                end;
            end;
          end;
    end;

  procedure TModelServerReport.FormDestroy(Sender: TObject);
    begin
      btTimeWarpClick(Sender);
      try
        TheModelServer.Free;
      except
      end;
    end;

  procedure TModelServerReport.FormCreate(Sender: TObject);

    procedure ReadDataFromDirSever( Address : string; Port : integer );
      var
        DSCnx     : IRDOConnectionInit;
        WSDSCnx   : TWinSockRDOConnection;
        DSProxy   : OleVariant;
        session   : integer;
        key       : string;
        satPath   : string;
        dataPath  : string;
      begin
        WSDSCnx      := TWinSockRDOConnection.Create( 'DSCnx' );
        DSCnx        := WSDSCnx;
        DSCnx.Server := Address;
        DSCnx.Port   := Port;
        DSProxy      := TRDOObjectProxy.Create as IDispatch;
        if DSCnx.Connect( 20000 )
          then
            begin
              DSProxy.SetConnection( DSCnx );
              if DSProxy.BindTo( 'DirectoryServer' )
                then
                  begin
                    DSProxy.WaitForAnswer := true;
                    DSProxy.TimeOut := 60000;
                    session         := DSProxy.RDOOpenSession;
                  end
                else session := 0;
              if session <> 0
                then
                  begin
                    DSProxy.BindTo( session );
                    try
                      key := 'Root/Areas/' + DSArea.Text + '/Worlds/' + WorldName.Text + '/Model';

                      if DSProxy.RDOSetCurrentKey('Root/Areas/' + DSArea.Text + '/Clusters/' + DSCluster.Text)
                        then
                          begin
                            satPath  := DSProxy.RDOReadString('satellite');
                            dataPath := DSProxy.RDOReadString('datapath');
                            if dataPath <> ''
                              then SatellitePath.Text :=  dataPath + '\web\satellite';
                          end;

                      if DSProxy.RDOSetCurrentKey(key)
                        then
                          begin
                            BaseDir.Text  := DSProxy.RDOReadString('BaseDir');
                            daPort.Text   := IntToStr(DSProxy.RDOReadInteger('Port'));
                            fMaxInvestors := DSProxy.RDOReadInteger('MaxInvestors');
                            DSProxy.RDOWriteString( 'IP', GetLocalAddress );
                            // Old compatibility
                            if satPath = ''
                              then SatellitePath.Text := DSProxy.RDOReadString( 'SatellitePath' );
                          end;

                      if DSProxy.RDOSetCurrentKey('Root/Areas/' + DSArea.Text + '/Clusters/' + DSCluster.Text + '/Cache')
                        then csHostAddress.Text := DSProxy.RDOReadString( 'IP' );

                      if DSProxy.RDOSetCurrentKey('Root/Areas/' + DSArea.Text + '/Clusters/' + DSCluster.Text + '/Mail')
                        then
                          begin
                            msHostAddress.Text  := DSProxy.RDOReadString( 'IP' );
                            MailServerPort.Text := DSProxy.RDOReadString( 'Port' );
                          end;

                      if DSProxy.RDOSetCurrentKey('Root/Areas/' + DSArea.Text + '/Clusters/' + DSCluster.Text + '/News')
                        then NewsServerPort.Text := DSProxy.RDOReadString( 'Port' );

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
      ver : string;
      dt  : string;
    begin
      fSimReport := TStringList.Create;
      Logs.Log( 'Survival', 'Creating ModelServer instance... ' );
      TheModelServer := TModelServer.Create;
      Logs.Log( 'Survival', 'Creating ModelServer instance: OK. ' );
      SetLogFile( ExtractFilePath(Application.ExeName) + 'ModelServer.log' );
      Logs.Log( 'Survival', 'Getting config from registry... ' );
      try
        Reg := TRegistry.Create;
        try
          Reg.RootKey := HKEY_LOCAL_MACHINE;
          if Reg.OpenKey( ModelServerKey, false )
            then
              begin
                WorldName.Text := Reg.ReadString( 'WorldName' );
                ver := Reg.ReadString('Version');
                if ver <> ''
                  then fVersion := StrToInt(ver) + 1
                  else fVersion := 1;
                dt := Reg.ReadString('Restarted');
                if dt <> ''
                  then fLastRestarted := StrToDateTime(dt)
                  else fLastRestarted := 0;
                try
                  UseDS.Checked  := Reg.ReadBool( 'UseDirServer' );
                  DSAddress.Text := Reg.ReadString( 'DSAddr' );
                  DSPort.Text    := Reg.ReadString( 'DSPort' );
                  DSArea.Text    := Reg.ReadString( 'Area' );
                  DSCluster.Text := Reg.ReadString( 'Cluster' );
                  try
                    cbAutoRestart.Checked := Reg.ReadBool('AutoRestart');
                  except
                    // First time may fail..
                  end;
                  try
                    spPurge.Value := Reg.ReadInteger('PurgeCount');
                  except
                    spPurge.Value := 0;
                  end;
                except
                  UseDS.Checked := false;
                end;
                if UseDS.Checked
                  then ReadDataFromDirSever( DSAddress.Text, StrToInt(DSPort.Text) )
                  else
                    begin
                      BaseDir.Text       := Reg.ReadString( 'BaseDir' );
                      csHostAddress.Text := Reg.ReadString( 'CacheServer' );
                      msHostAddress.Text := Reg.ReadString( 'MailServer' );
                      SatellitePath.Text := Reg.ReadString( 'SatellitePath' );
                      daPort.Text        := Reg.ReadString( 'Port' );
                      if (daPort.Text = '') or (daPort.Text = '0')
                        then daPort.Text := '7000';
                    end;
                Logs.Log( 'Survival', 'BaseDir: ' + BaseDir.Text );
                Logs.Log( 'Survival', 'csHostAddress: ' + csHostAddress.Text );
                Logs.Log( 'Survival', 'msHostAddress: ' + msHostAddress.Text );
                Logs.Log( 'Survival', 'SatellitePath: ' + SatellitePath.Text );
                Logs.Log( 'Survival', 'daPort: ' + daPort.Text );
              end;
        finally
          Reg.Free;
        end;
        eMaintDate.Text := DateTimeToStr(Now + 7); // a week..
      except
        Logs.Log( 'Survival', 'Error reading config from registry... ' );
      end
    end;

  procedure TModelServerReport.isConnectClick(Sender: TObject);
    begin
      try
        // TheModelServer.InitInterfaceServerEvents( isHostAddress.Text, StrToInt(isServerPort.Text) );
        //isConnect.Enabled := false;
      except
        //Application.MessageBox( pchar('Cannot register ' + OpenExtension.FileName), 'Model Server', MB_ICONERROR or MB_OK );
      end
    end;

  procedure TModelServerReport.StatsTimerTimer(Sender: TObject);
    begin
      if TheModelServer <> nil
        then BPS.Caption := IntToStr(TheModelServer.BPS) + ' fac/sec, dt: ' + Format('%.3n', [TheModelServer.CurDt]) + ', simulating ' + IntToStr(TheModelServer.HoursADay) + ' hours/day.';
    end;

  procedure TModelServerReport.DASpeedChange(Sender: TObject);
    begin
      {
      if Sender = DASpeed
        then SimSpeed.Position := SimSpeed.Max - DASpeed.Position
        else DASpeed.Position  := DASpeed.Max - SimSpeed.Position
      }
    end;

  procedure TModelServerReport.UpdateProgress(Comment : string; aPos : byte);
    begin
      if self.Comment.Caption <> Comment
        then
          begin
            self.Comment.Caption := Comment;
            self.Comment.Repaint;
          end;
      if fProgessBarPos <> aPos          
        then
          begin
            fProgessBarPos := aPos;
            LoadProgress.Position := aPos;      
            LoadProgress.Repaint;
          end;
      Application.ProcessMessages;
    end;

  procedure TModelServerReport.ReconnectNewsClick(Sender: TObject);
    begin
      try                                                                         
        TheModelServer.InitNewsServer( msHostAddress.Text, StrToInt(NewsServerPort.Text), TheModelServer.TheWorld.Name );
      except
      end;
    end;

  procedure TModelServerReport.ReconnectMailClick(Sender: TObject);
    begin
      try
        TheModelServer.InitMailServer( msHostAddress.Text, StrToInt(MailServerPort.Text), TheModelServer.TheWorld.Name );
      except
      end;                            
   end;

  procedure TModelServerReport.FormShow(Sender: TObject);
    var
      options : string;
    begin
      if (paramcount >= 1) and (uppercase(paramstr(1)) = 'AUTORUN')
        then
          begin
            if paramcount >= 2
              then options := UpperCase(paramstr(2))
              else options := '';

            cbFacilities.Checked := pos('F+', options) <> 0;
            cbCompanies.Checked  := pos('C+', options) <> 0;
            cbTycoons.Checked    := pos('P+', options) <> 0;
            cbTowns.Checked      := pos('T+', options) <> 0;

            CreateWorldClick( self );
          end;
    end;

  procedure TModelServerReport.StoreDSDataClick(Sender: TObject);
    var
      DSCnx     : IRDOConnectionInit;
      WSDSCnx   : TWinSockRDOConnection;
      DSProxy   : OleVariant;
      session   : integer;
      key       : string;
      Reg       : TRegistry;
    begin
      WSDSCnx      := TWinSockRDOConnection.Create( 'DSCnx' );
      DSCnx        := WSDSCnx;
      DSCnx.Server := DSAddress.Text;
      DSCnx.Port   := StrToInt(DSPort.Text);
      DSProxy      := TRDOObjectProxy.Create as IDispatch;
      if DSCnx.Connect( 20000 )
        then
          begin
            DSProxy.SetConnection( DSCnx );
            if DSProxy.BindTo( 'DirectoryServer' )
              then
                begin
                  DSProxy.TimeOut := 20000;
                  DSProxy.WaitForAnswer := true;
                  session := DSProxy.RDOOpenSession;
                end
              else session := 0;
            if session <> 0
              then
                begin
                  DSProxy.BindTo( session );
                  key := 'Root/Areas/' + DSArea.Text + '/Worlds/' + WorldName.Text + '/Model';
                  if DSProxy.RDOCreateFullPathKey( key, true )
                    then
                      begin
                        if DSProxy.RDOSetCurrentKey(key)
                          then
                            begin
                              DSProxy.RDOWriteString( 'BaseDir', BaseDir.Text );
                              DSProxy.RDOWriteString( 'SatellitePath', SatellitePath.Text );
                              DSProxy.RDOWriteString( 'Cluster', DSCluster.Text );
                              if daPort.Text = ''
                                then daPort.Text := '7000';
                              DSProxy.RDOWriteInteger( 'Port', StrToInt(daPort.Text) );
                            end;
                        Reg := TRegistry.Create;
                        try
                          Reg.RootKey := HKEY_LOCAL_MACHINE;
                          if Reg.OpenKey( ModelServerKey, false )
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


  procedure TModelServerReport.ReconnectDSClick(Sender: TObject);
    begin
      TheModelServer.InitDirServer( DSArea.Text, DSAddress.Text, StrToInt(DSPort.Text) );
    end;


  procedure TModelServerReport.seBackupRateChange(Sender: TObject);
    begin
      ModelServer.BackupRate := seBackupRate.Value;
    end;

  procedure TModelServerReport.btnBlackboxClick(Sender: TObject);
    {var
      BBForm : TBBForm;
      ip     : u_long;
      LogRec : PLogSocketRec;}
    begin
      {BBForm := TBBForm.Create(self);
      try
        if (BBForm.ShowModal = mrOk) and (BBForm.cbLogId.Text <> '')
          then
            begin
              new(LogRec);
              LogRec.Socket := TClientSocket.Create(self);
              LogRec.id := BBForm.cbLogId.Text;
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
            end;
      finally
        BBForm.Free;
      end;}
    end;

  {procedure TModelServerReport.OnBBConnect(Sender : TObject; Socket : TCustomWinSocket);
    begin
    end;

  procedure TModelServerReport.OnBBError(Sender : TObject; Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer);
    begin
      ErrorCode := 0;
    end;

  procedure TModelServerReport.OnBBDisconnect(Sender : TObject; Socket : TCustomWinSocket);
    begin
    end;}

  procedure TModelServerReport.tReconnectTimer(Sender: TObject);
    begin
      if (TheModelServer <> nil) and not TheModelServer.BackupError
        then
          begin
            if TheModelServer.TheWorld <> nil
              then
                begin
                  mLog.Items.Clear;
                  mLog.Items.Add('Investors: ' + IntToStr(TheModelServer.TheWorld.InvestorCount));
                  mLog.Items.Add('Visitors: ' + IntToStr(TheModelServer.TheWorld.VisitorCount));
                  fSimReport.Text := TheModelServer.TheWorld.SimReport;
                  mLog.Items.AddStrings(fSimReport);
                end;
            try
              if TheModelServer.MailConn = nil
                then
                  begin
                    Logs.Log('Survival', 'Start reconnect mail..');
                    ReconnectMailClick(Sender);
                    Logs.Log('Survival', 'End reconnecting mail..');
                  end;
            except
              Logs.Log('Survival', 'Error reconnecting mail..');
            end;
            try
              if TheModelServer.NewsConn = nil
                then
                  begin
                    Logs.Log('Survival', 'Start reconnect news..');
                    ReconnectNewsClick(Sender);
                    Logs.Log('Survival', 'End reconnecting news..');
                  end;
            except
              Logs.Log('Survival', 'Error reconnecting news..');
            end;
            try
              if TheModelServer.DirConn = nil
                then
                  begin
                    Logs.Log('Survival', 'Start reconnect directory..');
                    ReconnectDSClick(Sender);
                    Logs.Log('Survival', 'End reconnecting dirtectory..');
                  end;
            except
              Logs.Log('Survival', 'Error reconnecting directory..');
            end;
            fBackupError := false;
          end
        else
          begin
            fBackupError := true;
            Close;
          end;
    end;

  procedure TModelServerReport.seDTChange(Sender: TObject);
    begin
      ModelServer.WorldDt := seDT.Value;
    end;

  procedure TModelServerReport.spPurgeChange(Sender: TObject);
    var
      Reg : TRegistry;
    begin
      try
        if TheModelServer <> nil
          then TheModelServer.SetPurgeTyccons(spPurge.Value);
        Reg := TRegistry.Create;
        try
          Reg.RootKey := HKEY_LOCAL_MACHINE;
          if Reg.OpenKey(ModelServerKey, true)
            then Reg.WriteInteger('PurgeCount', spPurge.Value);
        finally
          Reg.Free;
        end;
      except
      end;
    end;

  procedure TModelServerReport.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    begin
      CanClose := fBackupError or (TheModelServer = nil) or (TheModelServer.TheWorld = nil) or TheModelServer.MaintDue or (Application.MessageBox('Are you sure you want to close this world?', 'Notice', MB_YESNO) = IDYES);
    end;

  procedure TModelServerReport.cbFacilitiesClick(Sender: TObject);
    begin
      cbLinks.Checked := not cbFacilities.Checked;
    end;

  procedure TModelServerReport.cbLinksClick(Sender: TObject);
    begin
      cbFacilities.Checked := not cbLinks.Checked;
    end;

  procedure TModelServerReport.CheckBox1Click(Sender: TObject);
    begin
      if TheModelServer <> nil
        then TheModelServer.Integration := CheckBox1.Checked;
    end;

  procedure TModelServerReport.MemTimerTimer(Sender: TObject);
    var
      date : string;
      Reg  : TRegistry;
    begin
      lbMemUsage.Caption := Format('Mem Used: %.0n bytes', [int(GetHeapStatus.TotalAllocated)]);
      if (TheModelServer <> nil) and (TheModelServer.TheWorld <> nil)
        then
          begin
            if TheModelServer.TheWorld.ZoningMessages <> nil
              then
                begin
                  lbMailMessages.Caption := IntToStr(TheModelServer.TheWorld.ZoningMessages.Count);
                  lbMessagesSent.Caption := IntToStr(TheModelServer.TheWorld.ZoneMessSent);
                end;
            date := DateToStr(TheModelServer.TheWorld.VirtualTime);
            Caption := 'FIVE Model Server (' + date + ')';
            if TheModelServer.MaintDue
              then
                begin
                  Reg := TRegistry.Create;
                  try
                    Reg.RootKey := HKEY_LOCAL_MACHINE;
                    if Reg.OpenKey(ModelServerKey, true)
                      then Reg.WriteString('Restarted', DateTimeToStr(Now));
                  finally
                    Reg.Free;
                  end;
                  Close;
                end;
          end;
    end;

  procedure TModelServerReport.btSendMailClick(Sender: TObject);
    begin
      try
        TheModelServer.TheWorld.SendZoningMessages(0);
      except
      end;
    end;

  procedure TModelServerReport.btTimeWarpClick(Sender: TObject);
    begin
      try
        if (TheModelServer <> nil) and (TheModelServer.TheWorld <> nil)
          then TheModelServer.TheWorld.TimeWarp;
      except
      end;
    end;

  procedure TModelServerReport.eMaintDateChange(Sender: TObject);
    begin
      try
        if TheModelServer <> nil
          then TheModelServer.RestartDate := StrToDateTime(eMaintDate.Text);
      except
      end;
    end;

  procedure TModelServerReport.cbAutoRestartClick(Sender: TObject);
    var
      Reg : TRegistry;
    begin
      try
        if TheModelServer <> nil
          then TheModelServer.AutoRestart := cbAutoRestart.Checked;
        eMaintDate.Enabled := cbAutoRestart.Checked;
        Reg := TRegistry.Create;
        try
          Reg.RootKey := HKEY_LOCAL_MACHINE;
          if Reg.OpenKey(ModelServerKey, true)
            then Reg.WriteBool('AutoRestart', cbAutoRestart.Checked);
        finally
          Reg.Free;
        end;
      except
      end;
    end;

  function TModelServerReport.CopyFile(const src, dest : string) : boolean;
    var
      FileOp : TSHFileOpStruct;
      lpstr1 : array[0..MAX_PATH] of char;
      lpstr2 : array[0..MAX_PATH] of char;
    begin
      fillchar(lpstr1, sizeof(lpstr1), 0);
      fillchar(lpstr2, sizeof(lpstr2), 0);
      strpcopy(lpstr1, src);
      strpcopy(lpstr2, dest);
      with FileOp do
        begin
          wFunc  := FO_COPY;
          Wnd    := 0;
          pFrom  := lpstr1;
          pTo    := lpstr2;
          fFlags := FOF_NOCONFIRMATION or FOF_SILENT;
          hNameMappings := nil;
        end;
      result := SHFileOperation(FileOp) = 0;
    end;

  procedure TModelServerReport.BackupTimerTimer(Sender: TObject);
    var
      Reg     : TRegistry;
      bckName : string;
      p       : integer;
      bckNo   : integer;
      split   : boolean;
      src, dest : string;
    begin
      try
        Reg := TRegistry.Create;
        try
          Reg.RootKey := HKEY_LOCAL_MACHINE;
          if Reg.OpenKey(ModelServerKey, true)
            then
              begin
                bckName := Reg.ReadString('LastGoodBackup');
                split   := system.pos('.back', LowerCase(bckName)) <> 0;
                if split
                  then
                    begin
                      p  := pos('.', bckName);
                      bckNo := StrToInt(CompStringsParser.GetNextStringUpTo(bckName, p, '.'));
                      if bckNo = 1
                        then bckNo := 20
                        else bckNo := bckNo - 1;
                      src  := BaseDir.Text + '\' + WorldName.Text + '.' + IntToStr(bckNo) + '.back';
                      dest := BaseDir.Text + '\Save' + IntToStr(fBkCount) + '_' + WorldName.Text + '.' + IntToStr(bckNo) + '.back';
                      if CopyFile(src, dest)
                        then
                          begin
                            src  := src  + '.facs';
                            dest := dest + '.facs';
                            if CopyFile(src, dest)
                              then fBkCount := 1 + fBkCount mod 20;
                          end;
                    end
                  else
                    begin
                      src  := BaseDir.Text + '\' + bckName;
                      dest := BaseDir.Text + '\Save' + IntToStr(fBkCount) + '_' + bckName;
                      if CopyFile(src, dest)
                        then fBkCount := 1 + fBkCount mod 20;
                    end;
              end;
        finally
          Reg.Free;
        end;
      except
        on e : Exception do
          Logs.Log( 'Survival', TimeToStr(Now) + 'Error creating copy of backup ' + e.Message);
      end;
    end;

end.
