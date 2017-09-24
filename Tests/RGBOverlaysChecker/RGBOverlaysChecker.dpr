program RGBOverlaysChecker;

uses
  Forms,
  MainWindow in 'MainWindow.pas' {MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
