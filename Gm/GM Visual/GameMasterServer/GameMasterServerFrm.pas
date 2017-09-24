unit GameMasterServerFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, GMKernel, ExtCtrls;

type
  TMainForm =
    class(TForm)
        PageControl1: TPageControl;
        General: TTabSheet;
    btnStart: TButton;
        Stop: TButton;
        GMPortEdit: TEdit;
        Label1: TLabel;
        TabSheet1: TTabSheet;
        DSPort: TEdit;
        DSAddress: TEdit;
        UseDS: TCheckBox;
        Label2: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        GMSNameEdit: TEdit;
        TabSheet2: TTabSheet;
        Label5: TLabel;
        Label6: TLabel;
        lvGameMasters: TListView;
        lvIntServers: TListView;
        btnReconnect: TButton;
    Panel1: TPanel;
    Image1: TImage;
        procedure Button1Click(Sender: TObject);
        procedure ButtonXClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnReconnectClick(Sender: TObject);
      private
        procedure LoadRegistryData;
        procedure SaveRegistryData;
        procedure SaveConfigToDS;
      private
        procedure OnRegisterInterfaceServer( IsId : TIServerId; ClientAddress : string );
        procedure OnDeleteInterfaceServer( IsId : TIServerId );
        procedure OnRegisterGameMaster( ClientAddress : widestring; GMName : widestring );
        procedure OnDeleteGameMaster( GMName : widestring );
    end;

var
  MainForm: TMainForm;

implementation

  uses
    GMServerRDOMger, Registry, HostNames, Threads, Logs, GMServer,
    WinSockRDOConnection, RDOServer, RDOInterfaces, RDOObjectProxy;

{$R *.DFM}

  procedure TMainForm.Button1Click(Sender: TObject);
    var
      RDOPort : integer;
    begin
      if (GMPortEdit.Text <> '') and (GMSNameEdit.Text <> '')
        then
          begin
            Logs.Log(getLogId, '');
            Logs.Log(getLogId, {DateTimeToStr(Now) +} ' Starting GM Server..');
            Logs.Log(getLogId, '');

            RDOPort := StrToInt( GMPortEdit.Text );

            TheRD0Mger.SetupRDO( RDOPort );

            if TheRD0Mger.GameMasterServer <> nil
              then
                if TheRD0Mger.GameMasterServer.ConnectToDS( DSAddress.Text, StrToInt(DSPort.Text))
                  then
                    begin
                      if UseDS.Checked
                        then SaveConfigToDS;
                      with TheRD0Mger.GameMasterServer do
                        begin
                          OnRegisterInterfaceServer := self.OnRegisterInterfaceServer;
                          OnDeleteInterfaceServer   := self.OnDeleteInterfaceServer;
                          OnRegisterGameMaster      := self.OnRegisterGameMaster;
                          OnDeleteGameMaster        := self.OnDeleteGameMaster;
                        end;
                      btnStart.Enabled := false;
                    end
                  else
                    begin
                      //ShowMessage( 'Cannot Connect to DirectoryServer' );
                      Logs.Log(getLogId, '');
                      Logs.Log(getLogId, {DateTimeToStr(Now) +} ' Cannot Initialize GM Server..');
                      Logs.Log(getLogId, '');
                      Close;
                    end;
          end
        else ShowMessage( 'Fill up the fields' );
    end;

  procedure TMainForm.ButtonXClick(Sender: TObject);
    begin
      TheRD0Mger.DoneRDO;
    end;

  procedure TMainForm.FormCreate(Sender: TObject);
    var
      par : string;
    begin
      BeginThreads;
      LoadRegistryData;
      InitRD0Mger;
      par := ParamStr(1);
      if CompareText( par, 'autorun' ) = 0
        then
          begin
            Button1Click( self );
            btnStart.Enabled := false;
          end;
    end;

  const
    ISKey  = '\Software\Oceanus\Five\GMServer\';

  procedure TMainForm.SaveRegistryData;
    var
      Reg : TRegistry;
    begin
      Reg := TRegistry.Create;
      try
        Reg.RootKey := HKEY_LOCAL_MACHINE;
        if Reg.OpenKey(ISKey, true)
          then
            begin
              Reg.WriteBool('UseDS', UseDS.Checked);
              Reg.WriteString('DSAddr', DSAddress.Text);
              Reg.WriteString('DSPort', DSPort.Text);
              Reg.WriteString('GMSName', GMSNameEdit.Text);
              Reg.WriteString('GMSPort', GMPortEdit.Text);
            end;
      finally
        Reg.Free;
      end;
    end;

  procedure TMainForm.LoadRegistryData;
    var
      Reg : TRegistry;
    begin
      Reg := TRegistry.Create;
      try
        Reg.RootKey := HKEY_LOCAL_MACHINE;
        if Reg.OpenKey(ISKey, false)
          then
            begin
              UseDS.Checked    := Reg.ReadBool( 'UseDS');
              DSAddress.Text   := Reg.ReadString( 'DSAddr' );
              DSPort.Text      := Reg.ReadString( 'DSPort' );
              GMSNameEdit.Text := Reg.ReadString( 'GMSName' );
              GMPortEdit.Text  := Reg.ReadString( 'GMSPort' );
            end;
      finally
        Reg.Free;
      end;
    end;

  procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
    begin
      SaveRegistryData;
      EndThreads;
    end;

  procedure TMainForm.SaveConfigToDS;
    begin
      if (TheRD0Mger.GameMasterServer <> nil)
        then TheRD0Mger.GameMasterServer.SaveConfigToDS( GMSNameEdit.Text, StrToInt(GMPortEdit.Text) );
    end;

  procedure TMainForm.OnRegisterInterfaceServer( IsId : TIServerId; ClientAddress : string );
    begin
      with lvIntServers.Items.Add do
        begin
          Caption := ClientAddress;
          data    := pointer(IsId);
        end;
    end;

  procedure TMainForm.OnDeleteInterfaceServer( IsId : TIServerId );
    var
      i : integer;
      f : boolean;
    begin
      i := 0;
      f := false;
      while (i < lvIntServers.Items.Count) and not f do
        begin
          f := integer(lvIntServers.Items[i].Data) = IsId;
          inc( i );
        end;
      if f
        then lvIntServers.Items.Delete( pred(i) );
    end;

  procedure TMainForm.OnRegisterGameMaster( ClientAddress : widestring; GMName : widestring );
    begin
      with lvGameMasters.Items.Add do
        begin
          Caption := GMName;
        end;
    end;

  procedure TMainForm.OnDeleteGameMaster( GMName : widestring );
    var
      i : integer;
      f : boolean;
    begin
      i := 0;
      f := false;
      while (i < lvGameMasters.Items.Count) and not f do
        begin
          f := lvGameMasters.Items[i].Caption = GMName;
          inc( i );
        end;
      if f
        then lvGameMasters.Items.Delete( pred(i) );
    end;

  procedure TMainForm.btnReconnectClick(Sender: TObject);
    begin
      if (TheRD0Mger.GameMasterServer = nil) or not TheRD0Mger.GameMasterServer.ConnectToDS( DSAddress.Text, StrToInt(DSPort.Text))
        then ShowMessage( 'cannot reconnect!!!' );
    end;

end.
