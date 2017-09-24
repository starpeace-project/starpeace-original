unit AuthtestFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TAuthtestForm = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Label3: TLabel;
    lbResult: TLabel;
    eDomain: TEdit;
    Label4: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Edit2KeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

const
  DIR_SEGA_NOERROR                 = 0;
  DIR_SEGA_ERROR_UserNotFound      = 1;
  DIR_SEGA_ERROR_BadUserIDFormat   = 2;
  DIR_SEGA_ERROR_UnknownDomain     = 3;
  DIR_SEGA_ERROR_InternalError     = 4;
  DIR_SEGA_ERROR_UserNotAuthorized = 5;
  DIR_SEGA_ERROR_SystemDown        = 6;
  DIR_SEGA_ERROR_NotInitialized    = 7;
  DIR_SEGA_ERROR_TimeOut           = 8;

var
  AuthtestForm: TAuthtestForm;

  function GetErrorText(code : integer) : string;

  function authUser( usr, psw : pchar ) : integer; cdecl; external 'sega_snap.dll';
  function initAuthFunc : integer;                 cdecl; external 'sega_snap.dll'


implementation

{$R *.DFM}


function GetErrorText(code : integer) : string;
  begin
    result := 'UNKNOWN';
    case code of
      DIR_SEGA_NOERROR : result := 'OK';
      DIR_SEGA_ERROR_UserNotFound : result := 'Invalid User/Password combination';
      DIR_SEGA_ERROR_BadUserIDFormat : result := 'Bad UserID Format';
      DIR_SEGA_ERROR_UnknownDomain : result := 'Unknown Domain';
      DIR_SEGA_ERROR_InternalError : result := 'Internal Error';
      DIR_SEGA_ERROR_UserNotAuthorized : result := 'User Not Authorized';
      DIR_SEGA_ERROR_SystemDown : result := 'System Down';
      DIR_SEGA_ERROR_NotInitialized : result := 'Not Initialized';
      DIR_SEGA_ERROR_TimeOut : result := 'Time Out';
    end;
  end;

procedure TAuthtestForm.Button1Click(Sender: TObject);
  var
    u, n : string;
  begin
    u := Edit1.Text + eDomain.Text;
    n := Edit2.Text;
    lbResult.Caption := GetErrorText(authUser(pchar(u), pchar(n)));
  end;

procedure TAuthtestForm.Edit2KeyPress(Sender: TObject; var Key: Char);
  begin
    if Key = #13
      then Button1Click(Sender);
  end;

end.
