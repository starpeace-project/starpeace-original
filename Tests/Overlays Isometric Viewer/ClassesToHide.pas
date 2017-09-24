unit ClassesToHide;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TReadClassesToHide = class(TForm)
    Button1: TButton;
    ClassesToHide: TMemo;
    Button2: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ReadClassesToHide: TReadClassesToHide;

implementation

{$R *.DFM}

end.
