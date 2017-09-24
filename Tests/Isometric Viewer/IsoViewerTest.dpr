program IsoViewerTest;

uses
  Forms,
  SoundTypes in '..\..\Voyager\Components\MapIsoView\SoundTypes.pas',
  Aircraft in '..\..\Voyager\Components\MapIsoView\Aircraft.pas',
  BuildClasses in '..\..\Voyager\Components\MapIsoView\BuildClasses.pas',
  CacheBackup in '..\..\Voyager\Components\MapIsoView\CacheBackup.pas',
  Car in '..\..\Voyager\Components\MapIsoView\Car.pas',
  Circuits in '..\..\Voyager\Components\MapIsoView\Circuits.pas',
  CircuitsHandler in '..\..\Voyager\Components\MapIsoView\CircuitsHandler.pas',
  Concrete in '..\..\Voyager\Components\MapIsoView\Concrete.pas',
  FiveControl in '..\..\Voyager\Components\MapIsoView\FiveControl.pas',
  FiveTypes in '..\..\Voyager\Components\MapIsoView\FiveTypes.pas',
  GameControl in '..\..\Voyager\Components\MapIsoView\GameControl.pas',
  GameTypes in '..\..\Voyager\Components\MapIsoView\GameTypes.pas',
  GlassedBuffers in '..\..\Voyager\Components\MapIsoView\GlassedBuffers.pas',
  ImageCache in '..\..\Voyager\Components\MapIsoView\ImageCache.pas',
  ImageLoader in '..\..\Voyager\Components\MapIsoView\ImageLoader.pas',
  Lander in '..\..\Voyager\Components\MapIsoView\Lander.pas',
  LanderTypes in '..\..\Voyager\Components\MapIsoView\LanderTypes.pas',
  LocalCacheManager in '..\..\Voyager\Components\MapIsoView\LocalCacheManager.pas',
  LocalCacheTypes in '..\..\Voyager\Components\MapIsoView\LocalCacheTypes.pas',
  Map in '..\..\Voyager\Components\MapIsoView\Map.pas',
  MapSprites in '..\..\Voyager\Components\MapIsoView\MapSprites.pas',
  MapTypes in '..\..\Voyager\Components\MapIsoView\MapTypes.pas',
  SoundCache in '..\..\Voyager\Components\MapIsoView\SoundCache.pas',
  SoundMixer in '..\..\Voyager\Components\MapIsoView\SoundMixer.pas',
  Sounds in '..\..\Voyager\Components\MapIsoView\Sounds.pas',
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
  Roads in '..\..\Voyager\Components\MapIsoView\Roads.pas',
  Railroads in '..\..\Voyager\Components\MapIsoView\Railroads.pas',
  TraincarSprite in '..\..\Voyager\Components\MapIsoView\TraincarSprite.pas',
  ActorPool in '..\..\Actors\ActorPool.pas',
  DistributedStates in '..\..\Actors\DistributedStates.pas',
  StateEngine in '..\..\Actors\StateEngine.pas',
  ActorTypes in '..\..\Actors\ActorTypes.pas',
  LogFile in '..\..\Rdo\Common\LogFile.pas',
  IsometricMap in '..\..\Voyager\Components\IsometricMap\IsometricMap.pas',
  IsometricMapTypes in '..\..\Voyager\Components\IsometricMap\IsometricMapTypes.pas',
  FiveIsometricMap in '..\..\Voyager\Components\IsometricMap\FiveIsometricMap.pas',
  TimerTicker in '..\..\Utils\CodeLib\TimerTicker.pas',
  Options in 'Options.pas' {OptionsForm},
  FocusTypes in '..\..\Voyager\Components\MapIsoView\FocusTypes.pas',
  Pedestrian in '..\..\Voyager\Components\MapIsoView\Pedestrian.pas',
  IsoProfile in '..\..\Voyager\Components\MapIsoView\IsoProfile.pas',
  Profiler in '..\..\Kernel\Profiler.pas',
  GMKernel in '..\..\Gm\GMKernel.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFiveMainForm, FiveMainForm);
  Application.CreateForm(TCaptureCoordinates, CaptureCoordinates);
  Application.CreateForm(TOptionsForm, OptionsForm);
  Application.Run;
end.

