unit GameMasterLogon;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, GMChat, RDOInterfaces, ComCtrls, ExtCtrls;

type
  TLogon =
    class(TForm)
        Image1: TImage;
        Label4: TLabel;
        GMUsername: TEdit;
        GMPassword: TEdit;
        Label1: TLabel;
        Button1: TButton;
        Button2: TButton;
        procedure Button2Click(Sender: TObject);
        procedure Button1Click(Sender: TObject);
    end;

var
  Logon: TLogon;

implementation

{$R *.DFM}

procedure TLogon.Button2Click(Sender: TObject);
begin
  if GMUsername.Text <> ''
    then
      begin
        GMView.User     := GMUsername.Text;
        GMView.password := GMPassword.Text;
        ModalResult     := mrOK;
      end;
end;

procedure TLogon.Button1Click(Sender: TObject);
begin
  ModalResult     := mrCancel;
end;

end.
