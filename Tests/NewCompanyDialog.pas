unit NewCompanyDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TNewCompanyDlg = class(TForm)
    Label1: TLabel;
    CompanyName: TEdit;
    Button1: TButton;
    Button2: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  NewCompanyDlg: TNewCompanyDlg;

implementation

{$R *.DFM}

end.
