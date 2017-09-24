program Iso3DViewerTest;

uses
  Forms,
  SoundTypes in '..\..\Voyager\Components\MapIso3DView\SoundTypes.pas',
  Aircraft in '..\..\Voyager\Components\MapIso3DView\Aircraft.pas',
  BuildClasses in '..\..\Voyager\Components\MapIso3DView\BuildClasses.pas',
  CacheBackup in '..\..\Voyager\Components\MapIso3DView\CacheBackup.pas',
  Car in '..\..\Voyager\Components\MapIso3DView\Car.pas',
  Circuits in '..\..\Voyager\Components\MapIso3DView\Circuits.pas',
  CircuitsHandler in '..\..\Voyager\Components\MapIso3DView\CircuitsHandler.pas',
  Concrete in '..\..\Voyager\Components\MapIso3DView\Concrete.pas',
  FiveControl in '..\..\Voyager\Components\MapIso3DView\FiveControl.pas',
  FiveTypes in '..\..\Voyager\Components\MapIso3DView\FiveTypes.pas',
  GameControl in '..\..\Voyager\Components\MapIso3DView\GameControl.pas',
  GameTypes in '..\..\Voyager\Components\MapIso3DView\GameTypes.pas',
  GlassedBuffers in '..\..\Voyager\Components\MapIso3DView\GlassedBuffers.pas',
  ImageCache in '..\..\Voyager\Components\MapIso3DView\ImageCache.pas',
  ImageLoader in '..\..\Voyager\Components\MapIso3DView\ImageLoader.pas',
  Lander in '..\..\Voyager\Components\MapIso3DView\Lander.pas',
  LanderTypes in '..\..\Voyager\Components\MapIso3DView\LanderTypes.pas',
  LocalCacheManager in '..\..\Voyager\Components\MapIso3DView\LocalCacheManager.pas',
  LocalCacheTypes in '..\..\Voyager\Components\MapIso3DView\LocalCacheTypes.pas',
  Map in '..\..\Voyager\Components\MapIso3DView\Map.pas',
  MapSprites in '..\..\Voyager\Components\MapIso3DView\MapSprites.pas',
  MapTypes in '..\..\Voyager\Components\MapIso3DView\MapTypes.pas',
  SoundCache in '..\..\Voyager\Components\MapIso3DView\SoundCache.pas',
  SoundMixer in '..\..\Voyager\Components\MapIso3DView\SoundMixer.pas',
  Sounds in '..\..\Voyager\Components\MapIso3DView\Sounds.pas',
  Animations in '..\..\Utils\CodeLib\Animations.pas',
  AxlDebug in '..\..\Utils\CodeLib\AxlDebug.pas',
  CoreTypes in '..\..\Utils\CodeLib\CoreTypes.pas',
  ScrollRegions in '..\..\Utils\CodeLib\ScrollRegions.pas',
  ShutDown in '..\..\Utils\CodeLib\ShutDown.pas',
  Threads in '..\..\Utils\CodeLib\Threads.pas',
  ThreadTimer in '..\..\Utils\CodeLib\ThreadTimer.pas',
  Warnings in '..\..\Utils\CodeLib\Warnings.pas',
  ServerCnxEvents in 'ServerCnxEvents.pas',
  ClientView in 'ClientView.pas',
  FiveMain in 'FiveMain.pas' {FiveMainForm},
  CaptureCoords in 'CaptureCoords.pas' {CaptureCoordinates},
  VisualClassManager in '..\..\Class Packer\VisualClassManager.pas',
  Roads in '..\..\Voyager\Components\MapIso3DView\Roads.pas',
  Railroads in '..\..\Voyager\Components\MapIso3DView\Railroads.pas',
  LogFile in '..\..\Rdo\Common\LogFile.pas',
  FiveIsometricMap in '..\..\Voyager\Components\IsometricMap\FiveIsometricMap.pas',
  IsometricMap in '..\..\Voyager\Components\IsometricMap\IsometricMap.pas',
  IsometricMapTypes in '..\..\Voyager\Components\IsometricMap\IsometricMapTypes.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFiveMainForm, FiveMainForm);
  Application.CreateForm(TCaptureCoordinates, CaptureCoordinates);
  Application.Run;
end.

