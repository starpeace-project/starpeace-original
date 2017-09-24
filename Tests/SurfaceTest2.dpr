program SurfaceTest2;

uses
  Forms,
  sufacetstwin in 'sufacetstwin.pas' {Form1},
  Surfaces in '..\Surfaces\Surfaces.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
