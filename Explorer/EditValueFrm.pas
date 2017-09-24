unit EditValueFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TeditValueForm = class(TForm)
    Label4: TLabel;
    edEntryName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    cbKind: TComboBox;
    Label3: TLabel;
    edValue: TEdit;
    Button1: TButton;
    Button2: TButton;
    cbSecurity: TComboBox;
    Label5: TLabel;
    edCurrKey: TEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  editValueForm: TeditValueForm;

implementation

{$R *.DFM}


end.
