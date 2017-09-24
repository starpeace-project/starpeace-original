program OPM;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {MainForm},
  Monitors in 'Monitors.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Online Players Monitor';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
