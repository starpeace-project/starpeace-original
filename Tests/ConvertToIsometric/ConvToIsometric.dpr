program ConvToIsometric;

uses
  Forms,
  ConvToIsometricTestWindow in 'ConvToIsometricTestWindow.pas' {Form1},
  ConvertToIsometric in 'ConvertToIsometric.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
