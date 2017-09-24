program FilterBitmap;

uses
  Forms,
  FilterTestWindow in 'FilterTestWindow.pas' {Form1},
  FilterImage in 'FilterImage.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
