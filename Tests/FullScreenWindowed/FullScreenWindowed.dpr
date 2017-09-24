program FullScreenWindowed;

uses
  Forms,
  FullScreenWindow in 'FullScreenWindow.pas' {FullScreenForm},
  SubWindow in 'SubWindow.pas' {SubForm},
  SubWindow2 in 'SubWindow2.pas' {SubForm2},
  SpriteToSurface in 'SpriteToSurface.pas',
  Animations in 'Animations.pas',
  FullScreenWindowMgr in 'FullScreenWindowMgr.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFullScreenForm, FullScreenForm);
  Application.CreateForm(TSubForm, SubForm);
  Application.CreateForm(TSubForm2, SubForm2);
  Application.Run;
end.
