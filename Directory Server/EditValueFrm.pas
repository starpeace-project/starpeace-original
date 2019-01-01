unit EditValueFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TeditValueForm = class(TForm)
    Label4: TLabel;
    edCurrKey: TEdit;
    edEntryName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    cbKind: TComboBox;
    Label3: TLabel;
    edValue: TEdit;
    Button1: TButton;
    Button2: TButton;
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
