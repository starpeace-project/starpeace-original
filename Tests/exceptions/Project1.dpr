program Project1;

uses
  Forms,
  exceptfrm in 'exceptfrm.pas' {Form1},
  FiveErrorHandler in '..\..\Utils\VCL\FiveErrorHandler.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
