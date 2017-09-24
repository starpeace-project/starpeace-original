unit BlockUnBlock;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFBlockUnblock = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    block: TRadioButton;
    unblock: TRadioButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FBlockUnblock: TFBlockUnblock;

implementation

{$R *.DFM}

end.
