unit MailServerReportForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, SyncObjs;

const
  CheckMessageTicks = 10;

type
  TMailServerReport = class(TForm)
    Panel1: TPanel;
    Image1: TImage;
    PageControl1: TPageControl;
    General: TTabSheet;
    Label1: TLabel;
    MailRoot: TEdit;
    Browse: TButton;
    Initialize: TButton;
    Label3: TLabel;
    Port: TEdit;
    CheckTimer: TTimer;
    Label2: TLabel;
    lMsgCount: TLabel;
    TabSheet1: TTabSheet;
    Bevel1: TBevel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    UseDS: TCheckBox;
    DSAddress: TEdit;
    DSPort: TEdit;
    DSArea: TEdit;
    DSCluster: TEdit;
    StoreDSData: TButton;
    Label7: TLabel;
    procedure InitializeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CheckTimerTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StoreDSDataClick(Sender: TObject);
  private
    fTicks : integer;
  end;

var
  MailServerReport : TMailServerReport;
  AutoRun          : boolean;

implementation

  uses
    Registry, MailServer, MailData, RDOInterfaces, WinSockRDOConnectionsServer, HostNames,
    RDORootServer, LogFile, WinSockRDOConnection, RDOServer, RDOObjectProxy;

  {$R *.DFM}

  var
    CourierServer : TMailServer = nil;

  // TCacheServerReport

  procedure TMailServerReport.InitializeClick(Sender: TObject);
    var
      ServerConn : IRDOConnectionsServer;
    begin
      try
        ServerConn    := TWinSockRDOConnectionsServer.Create(StrToInt(Port.Text) );
        CourierServer := TMailServer.Create(ServerConn, MaxMailQueryThreads, 'MailServer' );
        Initialize.Enabled := false;
        SaveMailRoot(MailRoot.Text);
        //CheckTimer.Enabled := true;
        {$IFDEF LOGS}
        LogFile.SetLogFile(ExtractFilePath(ParamStr(0)) + 'MailServer.log');
        {$ENDIF}
      except
        Application.MessageBox( 'Cannot initialize the server', 'Mail Server', MB_ICONERROR or MB_OK );
      end;
      if Sender <> self
        then Application.Minimize;
    end;

  procedure TMailServerReport.FormCreate(Sender: TObject);

    procedure ReadDataFromDirSever( Address : string; Port : integer );
      var
        DSCnx   : IRDOConnectionInit;
        WSDSCnx : TWinSockRDOConnection;
        DSProxy : OleVariant;
        session : integer;
      begin
        WSDSCnx      := TWinSockRDOConnection.Create('DS1');
        DSCnx        := WSDSCnx;
        DSCnx.Server := Address;
        DSCnx.Port   := Port;
        DSProxy      := TRDOObjectProxy.Create as IDispatch;
        if DSCnx.Connect( 20000 )
          then
            begin
              DSProxy.SetConnection( DSCnx );
              DSProxy.BindTo( 'DirectoryServer' );
              DSProxy.WaitForAnswer := true;
              DSProxy.TimeOut := 20000;
              session         := DSProxy.RDOOpenSession;
              if session <> 0
                then
                  begin
                    DSProxy.BindTo( session );
                    DSProxy.RDOCurrentKey := 'Root/Areas/' + DSArea.Text + '/Clusters/' + DSCluster.Text + '/Mail';
                    MailRoot.Text  := DSProxy.RDOReadString( 'Root' );
                    self.Port.Text := IntToStr(DSProxy.RDOReadInteger( 'Port' ));
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
          if Reg.OpenKey( MailKey, false )
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
                  else MailRoot.Text := GetMailRoot
              end;
        finally
          Reg.Free;
        end;
      except
      end
      //
    end;

  procedure TMailServerReport.FormDestroy(Sender: TObject);
    begin
      CourierServer.Free;
    end;

procedure TMailServerReport.CheckTimerTimer(Sender: TObject);
  begin
    if CourierServer <> nil
      then
        begin
          lMsgCount.Caption := IntToStr(CourierServer.Messages.Count);
          if fTicks = CheckMessageTicks
            then
              begin
              end
            else inc(fTicks);
        end;
  end;

  procedure TMailServerReport.FormShow(Sender: TObject);
    begin
      if AutoRun and Initialize.Enabled
        then InitializeClick(Sender);
    end;

  procedure TMailServerReport.StoreDSDataClick(Sender: TObject);
    var
      DSCnx   : IRDOConnectionInit;
      WSDSCnx : TWinSockRDOConnection;
      DSProxy : OleVariant;
      session : integer;
      key     : string;
      Reg     : TRegistry;
    begin
      WSDSCnx      := TWinSockRDOConnection.Create('DS2');
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
                  key := 'Root/Areas/' + DSArea.Text + '/Clusters/' + DSCluster.Text + '/Mail';
                  if DSProxy.RDOCreateFullPathKey( key, true )
                    then
                      begin
                        DSProxy.RDOCurrentKey := key;
                        DSProxy.RDOWriteString( 'Root', MailRoot.Text );
                        DSProxy.RDOWriteInteger( 'Port', StrToInt(Port.Text) );
                        Reg := TRegistry.Create;
                        try
                          Reg.RootKey := HKEY_LOCAL_MACHINE;
                          if Reg.OpenKey( MailKey, false )
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

  AutoRun := false;

end.
