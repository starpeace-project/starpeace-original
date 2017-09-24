unit PlotterTest;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TForm1 = class(TForm)
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  PlotterGrid;

procedure TForm1.FormShow(Sender: TObject);
  var
    P : TPlotter;
  begin
    P := TPlotter.Create( self );
    P.Parent := self;
    P.Top := 10;
    P.Left := 10;
    P.Chart( '12,-100,200,30,180,255,150,0,10,150,100,20,10,5,50,', 2039, true, true );
  end;

end.
