unit AccForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TAccountForm = class(TForm)
    Account: TEdit;
    Domain: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    OK: TButton;
    Button2: TButton;
    FwdAddr: TEdit;
    Label3: TLabel;
    FwdKeep: TCheckBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AccountForm: TAccountForm;

implementation

{$R *.DFM}

end.
