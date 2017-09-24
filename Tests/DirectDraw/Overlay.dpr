program Overlay;

uses
  Forms,
  OverlayMainWindow in 'OverlayMainWindow.pas' {OverlayMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TOverlayMainForm, OverlayMainForm);
  Application.Run;
end.
