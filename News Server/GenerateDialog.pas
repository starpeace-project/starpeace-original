unit GenerateDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TGenerateDlg = class(TForm)
    Worlds: TListBox;
    Label1: TLabel;
    Generate: TButton;
    Cancel: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  GenerateDlg: TGenerateDlg;

implementation

{$R *.DFM}


end.
