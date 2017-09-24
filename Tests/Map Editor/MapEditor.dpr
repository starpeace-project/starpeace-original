program MapEditor;

uses
  Forms,
  SoundTypes in 'MapIsoView\SoundTypes.pas',
  BuildClasses in 'MapIsoView\BuildClasses.pas',
  Circuits in 'MapIsoView\Circuits.pas',
  CircuitsHandler in 'MapIsoView\CircuitsHandler.pas',
  Concrete in 'MapIsoView\Concrete.pas',
  MapControl in 'MapIsoView\MapControl.pas',
  FiveTypes in 'MapIsoView\FiveTypes.pas',
  GameControl in 'MapIsoView\GameControl.pas',
  GameTypes in 'MapIsoView\GameTypes.pas',
  ImageCache in 'MapIsoView\ImageCache.pas',
  ImageLoader in 'MapIsoView\ImageLoader.pas',
  Lander in 'MapIsoView\Lander.pas',
  LanderTypes in 'MapIsoView\LanderTypes.pas',
  LocalCacheManager in 'MapIsoView\LocalCacheManager.pas',
  LocalCacheTypes in 'MapIsoView\LocalCacheTypes.pas',
  Map in 'MapIsoView\Map.pas',
  MapTypes in 'MapIsoView\MapTypes.pas',
  SoundCache in 'MapIsoView\SoundCache.pas',
  SoundMixer in 'MapIsoView\SoundMixer.pas',
  Sounds in 'MapIsoView\Sounds.pas',
  Animations in '..\..\Utils\CodeLib\Animations.pas',
  AxlDebug in '..\..\Utils\CodeLib\AxlDebug.pas',
  CoreTypes in '..\..\Utils\CodeLib\CoreTypes.pas',
  ScrollRegions in '..\..\Utils\CodeLib\ScrollRegions.pas',
  ShutDown in '..\..\Utils\CodeLib\ShutDown.pas',
  Threads in '..\..\Utils\CodeLib\Threads.pas',
  ThreadTimer in '..\..\Utils\CodeLib\ThreadTimer.pas',
  Warnings in '..\..\Utils\CodeLib\Warnings.pas',
  VisualClassManager in '..\..\Class Packer\VisualClassManager.pas',
  Roads in 'MapIsoView\Roads.pas',
  Railroads in 'MapIsoView\Railroads.pas',
  IsometricMap in 'IsometricMap\IsometricMap.pas',
  IsometricMapTypes in 'IsometricMap\IsometricMapTypes.pas',
  FiveIsometricMap in 'IsometricMap\FiveIsometricMap.pas',
  TimerTicker in '..\..\Utils\CodeLib\TimerTicker.pas',
  FocusTypes in 'MapIsoView\FocusTypes.pas',
  MapEditorMain in 'MapEditorMain.pas' {MapEditorMainForm},
  MasterURLHandler in 'MasterURLHandler.pas',
  VoyagerUIEvents in '..\..\Voyager\VoyagerUIEvents.pas',
  AccidentImageViewerControl in 'AccidentImageViewerControl.pas',
  GMKernel in '..\..\Gm\GMKernel.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMapEditorMainForm, MapEditorMainForm);
  Application.Run;
end.

