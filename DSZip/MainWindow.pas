unit MainWindow;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, DirectoryServer, Db, ADODB;

type
  TDirectoryWin = class(TForm)
    Image1: TImage;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Label3: TLabel;
    DBName: TEdit;
    Label1: TLabel;
    SecurePort: TEdit;
    Start: TButton;
    UnsecuredPort: TEdit;
    Label2: TLabel;
    SessionTimer: TTimer;
    Label4: TLabel;
    lbSections: TLabel;
    Label5: TLabel;
    IPAddress: TEdit;
    TabSheet2: TTabSheet;
    Label6: TLabel;
    eUser: TEdit;
    Label7: TLabel;
    ePassword: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure StartClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SessionTimerTimer(Sender: TObject);
    procedure eUserChange(Sender: TObject);
    procedure ePasswordChange(Sender: TObject);
  private
    fSecureDirServer    : TDirectoryServer;
    fUnsecuredDirServer : TDirectoryServer;
    fSessionTTL         : TDateTime;
  end;

var
  DirectoryWin: TDirectoryWin;


implementation

  uses
    Registry, DirectoryRegistry, DirectoryManager;

  {$R *.DFM}

  function scramble(const str : string) : string;
    var
      b : byte;
      i : integer;
    begin
      setlength(result, length(str));
      for i := 1 to length(str) do
        begin
          b := ord(str[i]);
          if b mod 2 = 0
            then inc(b)
            else dec(b);
          result[i] := char(b);
        end;
    end;

  function unscramble(const str : string) : string;
    var
      b : byte;
      i : integer;
    begin
      setlength(result, length(str));
      for i := 1 to length(str) do
        begin
          b := ord(str[i]);
          if b mod 2 = 0
            then inc(b)
            else dec(b);
          result[i] := char(b);
        end;
    end;

  procedure TDirectoryWin.FormCreate(Sender: TObject);
    var
      Reg : TRegistry;
      aux : string;
    begin
      fSessionTTL := EncodeTime(0, 5, 0, 0); // 5 Minutes to Live
      try
        Reg := TRegistry.Create;
        try
          Reg.RootKey := HKEY_LOCAL_MACHINE;
          if Reg.OpenKey( tidRegKey_Directory, false )
            then
              begin
                aux := Reg.ReadString( 'FullName' );
                if aux <> ''
                  then DBName.Text := aux;
                aux := Reg.ReadString('DBUser');
                if aux <> ''
                  then
                    begin
                      dbUser     := aux;
                      eUser.text := aux;
                    end;
                aux := unscramble(Reg.ReadString('DBPassword'));
                if aux <> ''
                  then
                    begin
                      dbPassword := aux;
                      ePassword.Text := aux;
                    end;
                aux := Reg.ReadString('SecurePort');
                if aux <> ''
                  then SecurePort.Text := aux;
                aux := Reg.ReadString('UnsecuredPort');
                if aux <> ''
                  then UnsecuredPort.Text := aux;
                aux := Reg.ReadString('IPAddress');
                if aux <> ''
                  then IPAddress.Text := aux;
              end;
        finally
          Reg.Free;
        end;
      except
      end
    end;

  procedure TDirectoryWin.StartClick( Sender : TObject );
    var
      Reg : TRegistry;
    begin
      dbUser := eUser.Text;
      dbPassword := ePassword.Text;
      fSecureDirServer    := TDirectoryServer.Create( StrToInt( SecurePort.Text )   , DBName.Text, true );
      fUnsecuredDirServer := TDirectoryServer.Create( StrToInt( UnsecuredPort.Text ), DBName.Text, false );
      Start.Enabled := false;
      if Sender <> self
        then Application.Minimize;
      SessionTimer.Enabled := true;
      try
        Reg := TRegistry.Create;
        try
          Reg.RootKey := HKEY_LOCAL_MACHINE;
          if Reg.OpenKey(tidRegKey_Directory, true)
            then
              begin
                Reg.WriteString('FullName', DBName.Text);
                Reg.WriteString('DBUser', DirectoryManager.dbUser);
                Reg.WriteString('DBPassword', scramble(DirectoryManager.dbPassword));
                Reg.WriteString('SecurePort', SecurePort.Text);
                Reg.WriteString('UnsecuredPort', UnsecuredPort.Text);
                Reg.WriteString('IPAddress', IPAddress.Text);
              end;
        finally
          Reg.Free;
        end;
      except
      end;
    end;

  procedure TDirectoryWin.FormShow( Sender : TObject );
    begin
      if uppercase(paramstr(1)) = 'AUTORUN'
        then StartClick( self );
    end;

  procedure TDirectoryWin.SessionTimerTimer(Sender: TObject);
    begin
      try
        lbSections.Caption := IntToStr(fSecureDirServer.SessionCount);
        fSecureDirServer.CheckSessions(fSessionTTL);
      except
      end;
      try
        fUnsecuredDirServer.CheckSessions(fSessionTTL);
      except
      end;
    end;

  procedure TDirectoryWin.eUserChange(Sender: TObject);
    begin
      DirectoryManager.dbUser := eUser.Text;
    end;

  procedure TDirectoryWin.ePasswordChange(Sender: TObject);
    begin
      DirectoryManager.dbPassword := ePassword.Text;
    end;

end.
