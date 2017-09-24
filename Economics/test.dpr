program test;

uses
  Forms,
  sources in 'C:\five\sources.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
