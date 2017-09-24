unit MianFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

function XReverse(str : string) : string; external 'mmlib.dll';

procedure TForm1.Button1Click(Sender: TObject);
  var
    str : string;
  begin
    str := XReverse('La puchica');
    ShowMessage(str);
  end;

end.
