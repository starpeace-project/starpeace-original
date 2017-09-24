program Test;

uses
  SMCheck in 'SMCheck.pas',
  Forms,
  SMC in 'SMC.pas' {Form6};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm6, Form6);
  Application.Run;
end.
