unit Configuration;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TConfigurationFrm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    IPconfig1: TEdit;
    Edit21: TEdit;
    cboServers: TComboBox;
    cboPorts: TComboBox;
    FullControl: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    { Private declarations }
    { Public declarations }
  end;

var
  ConfigurationFrm: TConfigurationFrm;

implementation

uses
   Registry, DirectoryRegistry, IniFiles;


{$R *.DFM}
const
 defaultserver = 'dir.starpeace.net' ;
 defaultport   = '2222' ;

procedure TConfigurationFrm.FormCreate(Sender: TObject);
  var
    //Reg : TRegistry;
    //aux : string;
    i : integer ;
    IniFile : TIniFile ;
    ServersStrings : TStringList ;
    PortsStrings    : TStringList ;
    AppPath : string ;
    LServer, LPort : string ;
  begin
  { TODO 5 -oorelbigm -cadd : Add the processing of the HistoryConect.ini file }
    ServersStrings := TStringList.Create ;
    PortsStrings := TStringList.Create ;
    AppPath := ParamStr(0) ;
    AppPath := ExtractFilePath ( AppPath ) ;
    //try
      IniFile := TIniFile.Create (AppPath+'HistoryConect.ini') ;
    try
      IniFile.ReadSection ( 'Servers History', ServersStrings ) ;
      IniFile.ReadSection ( 'Ports History', PortsStrings ) ;
      LServer := IniFile.ReadString ( 'Last ServerPort', 'Server', defaultserver ) ;
      LPort := IniFile.ReadString ( 'Last ServerPort', 'Port', defaultport );
      for i := 0 to ServersStrings.Count-1 do
        begin
          cboServers.Items.Add ( ServersStrings[i] ) ;
        end ;
      for i := 0 to PortsStrings.Count - 1 do
        begin
          cboPorts.Items.Add ( PortsStrings[i] ) ;
        end ;
    finally
      IniFile.Free ;
      if cboServers.Items.Count = 0
        then
          begin
            cboServers.Text := defaultserver ;
            cboServers.Items.Add ( cboServers.Text ) ;
          end
        else cboServers.Text := LServer ;//cboServers.Items[0] ;
      if cboPorts.Items.Count = 0
        then
          begin
            cboPorts.Text := defaultport;
            cboPorts.Items.Add ( cboPorts.Text ) ;
          end
        else cboPorts.Text := LPort; //cboPorts.Items[0];
      ServersStrings.Free ;
      PortsStrings.Free ;
    end ;
{    try
      Reg := TRegistry.Create;
      try
        Reg.RootKey := HKEY_LOCAL_MACHINE;
        if Reg.OpenKey( tidRegKey_Directory, false )
          then
            begin
              aux := Reg.ReadString( 'IPAddr' );
              if aux <> ''
                then IPconfig.Text := aux;
            end;
      finally
        Reg.Free;
      end;
    except
    end   }
  end;

procedure TConfigurationFrm.Button1Click(Sender: TObject);
  //var
  //  Reg : TRegistry;
  begin
    cboServers.Items.Add ( cboServers.Text ) ;
    cboPorts.Items.Add ( cboPorts.Text ) ;
{    try
      Reg := TRegistry.Create;
      try
        Reg.RootKey := HKEY_LOCAL_MACHINE;
        if Reg.OpenKey(tidRegKey_Directory, true)
          then
            begin
              Reg.WriteString('IPAddr', cboServers.Text); //IPConfig.Text
            end;
      finally
        Reg.Free;
      end;
    except
    end;}
  end;

procedure TConfigurationFrm.FormDestroy(Sender: TObject);
var
 IniFile : TIniFile ;
 AppPath : string ;
 i : integer ;
begin
//
 AppPath := ParamStr(0) ;
 AppPath := ExtractFilePath ( AppPath ) ;
 try
  IniFile := TIniFile.Create ( AppPath+'HistoryConect.ini' ) ;
  try
    for i := 0 to cboServers.Items.Count - 1 do
     begin
       IniFile.WriteString (  'Servers History' , cboServers.Items[i], '' ) ;
     end ;
    for i := 0 to cboPorts.Items.Count - 1 do
     begin
       IniFile.WriteString (  'Ports History' , cboPorts.Items[i], '' ) ;
     end ;
    IniFile.WriteString ( 'Last ServerPort', 'Server', cboServers.Text) ;
    IniFile.WriteString ( 'Last ServerPort', 'Port', cboPorts.Text) ;
  finally
   IniFile.Free ;
  end ;
 except
 end ;
end;

end.
