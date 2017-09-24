unit NewKeyFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TNewKeyForm = class(TForm)
    Label1: TLabel;
    edCurrKey: TEdit;
    Label2: TLabel;
    edNewKey: TEdit;
    Label3: TLabel;
    cbSecurity: TComboBox;
    Button1: TButton;
    Button2: TButton;
    procedure Button1KeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  NewKeyForm: TNewKeyForm;

implementation

{$R *.DFM}

procedure TNewKeyForm.Button1KeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13
    then
end;

end.
