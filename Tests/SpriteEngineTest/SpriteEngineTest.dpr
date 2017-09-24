program SpriteEngineTest;

uses
  Forms,
  ImageForm in 'ImageForm.pas' {ImageTestWindow},
  ImageLoader in '..\..\Voyager\Components\MapIsoView\ImageLoader.pas',
  GameTypes in '..\..\Voyager\Components\MapIsoView\GameTypes.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TImageTestWindow, ImageTestWindow);
  Application.Run;
end.
