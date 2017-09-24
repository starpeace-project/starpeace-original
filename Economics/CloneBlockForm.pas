unit CloneBlockForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin;

type
  TCloneForm = class(TForm)
    Label1: TLabel;
    Cluster: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    Label2: TLabel;
    GrowRatio: TEdit;
    Label3: TLabel;
    InputRatio: TEdit;
    Label4: TLabel;
    OutputRatio: TEdit;
    Small: TCheckBox;
    Label5: TLabel;
    ROI: TSpinEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CloneForm: TCloneForm;

implementation

{$R *.DFM}



end.
