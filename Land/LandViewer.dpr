program LandViewer;

uses
  Forms,
  MapViewerWindow in 'MapViewerWindow.pas' {LandViewerWin},
  Land in 'Land.pas',
  LandGenerator in 'LandGenerator.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TLandViewerWin, LandViewerWin);
  Application.Run;
end.
