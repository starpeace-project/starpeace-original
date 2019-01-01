unit MainWindow;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, DirectoryServer;

type
  TDirectoryWin = class(TForm)
    Image1: TImage;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Label3: TLabel;
    DirectoryFullName: TEdit;
    Label1: TLabel;
    SecurePort: TEdit;
    Start: TButton;
    UnsecuredPort: TEdit;
    Label2: TLabel;
    SessionTimer: TTimer;
    Label4: TLabel;
    lbSections: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure StartClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SessionTimerTimer(Sender: TObject);
  private
    fSecureDirServer    : TDirectoryServer;
    fUnsecuredDirServer : TDirectoryServer;
    fSessionTTL         : TDateTime;
  end;

var
  DirectoryWin: TDirectoryWin;


implementation

  uses
    Registry, DirectoryRegistry;

  {$R *.DFM}

  procedure TDirectoryWin.FormCreate(Sender: TObject);
    var
      Reg : TRegistry;
    begin
      fSessionTTL := EncodeTime(0, 5, 0, 0); // 5 Minutes to Live
      try
        Reg := TRegistry.Create;
        try
          Reg.RootKey := HKEY_LOCAL_MACHINE;
          if Reg.OpenKey( tidRegKey_Directory, false )
            then DirectoryFullName.Text := Reg.ReadString( 'FullName' );
        finally
          Reg.Free;
        end;
      except
      end
    end;

  procedure TDirectoryWin.StartClick( Sender : TObject );
    begin
      fSecureDirServer    := TDirectoryServer.Create( StrToInt( SecurePort.Text ), DirectoryFullName.Text, true );
      fUnsecuredDirServer := TDirectoryServer.Create( StrToInt( UnsecuredPort.Text ), DirectoryFullName.Text, false );
      Start.Enabled := false;
      if Sender <> self
        then Application.Minimize;
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

end.
