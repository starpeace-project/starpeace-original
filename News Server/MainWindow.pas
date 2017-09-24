unit MainWindow;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, NewsServer, News;

type
  TNewsWin = class(TForm, ILog)
    Panel1: TPanel;
    PageControl1: TPageControl;
    General: TTabSheet;
    CreateNewsCenter: TButton;
    Image1: TImage;
    NewsPath: TEdit;
    Label3: TLabel;
    Port: TEdit;
    Label1: TLabel;
    TabSheet1: TTabSheet;
    Log: TMemo;
    Generate: TButton;
    Reload: TButton;
    TabSheet2: TTabSheet;
    Label2: TLabel;
    UseDS: TCheckBox;
    Label4: TLabel;
    DSAddress: TEdit;
    Label5: TLabel;
    DSPort: TEdit;
    Bevel1: TBevel;
    DSArea: TEdit;
    Label6: TLabel;
    DSCluster: TEdit;
    StoreDSData: TButton;
    cbAllow: TCheckBox;
    procedure CreateNewsCenterClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure GenerateClick(Sender: TObject);
    procedure ReloadClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StoreDSDataClick(Sender: TObject);
    procedure UseDSClick(Sender: TObject);
    procedure cbAllowClick(Sender: TObject);
  private
    fNewsServer : TNewsServer;
  // ILog
  private
    fLogStr : string;
    procedure LogThis( str : string );
    procedure syncLogThis;
  // IUknown
  private
    function QueryInterface(const IID: TGUID; out Obj): hresult; stdcall;
    function _AddRef  : integer; stdcall;
    function _Release : integer; stdcall;
  end;

var
  NewsWin: TNewsWin;

implementation

  {$R *.DFM}

  uses
    StdReporters, GenericReporter, NewsRegistry, Registry, SmartThreads, HostNames,
    GenerateDialog, WinSockRDOConnection, RDOServer, RDOInterfaces, RDOObjectProxy;


  procedure TNewsWin.CreateNewsCenterClick(Sender: TObject);
    begin
      randomize;
      InitNews;
      StdReporters.RegisterReporters;
      GenericReporter.RegisterReporters;
      fNewsServer := TNewsServer.Create( StrToInt(Port.Text), NewsPath.Text, self );
      fNewsServer.AllowGenerate := false;
      CreateNewsCenter.Enabled := false;
      Generate.Enabled := true;
      Reload.Enabled := true;
      if Sender <> self
        then Application.Minimize;
    end;

  procedure TNewsWin.FormCreate(Sender: TObject);

    procedure ReadDataFromDirSever( Address : string; Port : integer );
      var
        DSCnx   : IRDOConnectionInit;
        WSDSCnx : TWinSockRDOConnection;
        DSProxy : OleVariant;
        session : integer;
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
              session := DSProxy.RDOOpenSession;
              if session <> 0
                then
                  begin
                    DSProxy.BindTo( session );
                    try
                      DSProxy.RDOCurrentKey := 'Root/Areas/' + DSArea.Text + '/Clusters/' + DSCluster.Text + '/News';
                      NewsPath.Text  := DSProxy.RDOReadString( 'Root' );
                      self.Port.Text := IntToStr(DSProxy.RDOReadInteger( 'Port' ));
                      DSProxy.RDOWriteString( 'IP', GetLocalAddress );
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
    begin
      try
        Reg := TRegistry.Create;
        try
          Reg.RootKey := HKEY_LOCAL_MACHINE;
          if Reg.OpenKey( tidRegKey_News, false )
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
                  else NewsPath.Text := Reg.ReadString( 'Path' );
              end;
        finally
          Reg.Free;
        end;
      except
      end
    end;

  procedure TNewsWin.Button1Click(Sender: TObject);
    begin
      {
      fNewsServer.RDOCreateNewsCenter( 'Test' );
      fNewsServer.RDOCreateNewspaper( 'Test', 'Vanguardia', 'PGI', '' );
      fNewsServer.RDOGenerateNewspapers( 'Test', EncodeDate( 2000, 1, 15 ) );
      }
    end;

  function TNewsWin.QueryInterface(const IID: TGUID; out Obj): hresult; stdcall;
    begin
      pointer(Obj) := nil;
      result := E_FAIL;
    end;

  procedure TNewsWin.LogThis( str : string );
    begin
      fLogStr := str;
      CallSync( syncLogThis );
    end;

  procedure TNewsWin.syncLogThis;
    begin
      Log.Lines.Add( fLogStr );
      while (Log.Lines.Count > 100) do
        Log.Lines.Delete( 0 );
    end;

  function TNewsWin._AddRef  : integer; stdcall;
    begin
      result := 1;
    end;

  function TNewsWin._Release : integer; stdcall;
    begin
      result := 1;
    end;

  procedure TNewsWin.GenerateClick(Sender: TObject);
    var
      i : integer;
    begin
      for i := 0 to pred(fNewsServer.NewsCenters.Count) do
        GenerateDlg.Worlds.Items.Add( TNewsCenter(fNewsServer.NewsCenters[i]).Name );
      if (GenerateDlg.ShowModal = mrOk) and (GenerateDlg.Worlds.ItemIndex <> -1)
        then fNewsServer.RDOGenerateNewspapers( TNewsCenter(fNewsServer.NewsCenters[GenerateDlg.Worlds.ItemIndex]).Name, EncodeDate( 2000, 1, 1 ), '', 0, '' );
    end;

  procedure TNewsWin.ReloadClick(Sender: TObject);
    var
      i : integer;
    begin
      for i := 0 to pred(fNewsServer.NewsCenters.Count) do
        TNewsCenter(fNewsServer.NewsCenters[i]).ReloadReporters;
    end;

  procedure TNewsWin.FormShow(Sender: TObject);
    begin
      if uppercase(paramstr(1)) = 'AUTORUN'
        then CreateNewsCenterClick( self );
    end;

  procedure TNewsWin.StoreDSDataClick(Sender: TObject);
    var
      DSCnx   : IRDOConnectionInit;
      WSDSCnx : TWinSockRDOConnection;
      DSProxy : OleVariant;
      session : integer;
      key     : string;
      Reg     : TRegistry;
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
            DSProxy.WaitForAnswer := true;
            DSProxy.TimeOut := 20000;
            session := DSProxy.RDOOpenSession;
            if session <> 0
              then
                begin
                  DSProxy.BindTo( session );
                  try
                    key := 'Root/Areas/' + DSArea.Text + '/Clusters/' + DSCluster.Text + '/News';
                    if DSProxy.RDOCreateFullPathKey( key, true )
                      then
                        begin
                          DSProxy.RDOCurrentKey := key;
                          DSProxy.RDOWriteString( 'Root', NewsPath.Text );
                          DSProxy.RDOWriteInteger( 'Port', StrToInt(Port.Text) );
                          Reg := TRegistry.Create;
                          try
                            Reg.RootKey := HKEY_LOCAL_MACHINE;
                            if Reg.OpenKey( tidRegKey_News, false )
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
                  finally
                    DSProxy.RDOEndSession;
                  end
                end
              else raise Exception.Create( 'Cannot create session!' );
          end
        else raise Exception.Create( 'Cannot connect to Directory Server!' );
    end;

  procedure TNewsWin.UseDSClick(Sender: TObject);
    var
      Reg : TRegistry;
    begin
      Reg := TRegistry.Create;
      try
        Reg.RootKey := HKEY_LOCAL_MACHINE;
        if Reg.OpenKey( tidRegKey_News, false )
          then Reg.WriteBool( 'UseDirServer', UseDS.Checked );
      finally
        Reg.Free;
      end;
    end;

  procedure TNewsWin.cbAllowClick(Sender: TObject);
    begin
      if fNewsServer <> nil
        then fNewsServer.AllowGenerate := cbAllow.Checked;
    end;

end.



