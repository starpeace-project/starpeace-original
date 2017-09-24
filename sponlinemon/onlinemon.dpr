program onlinemon;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {MainForm},
  Monitors in 'Monitors.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
