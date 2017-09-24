program VoxelRenderer;

uses
  Forms,
  VoxelRendererWindow in 'VoxelRendererWindow.pas' {VoxelRendererForm},
  FiveControl in 'FiveControl.pas',
  GameControl in 'GameControl.pas',
  GameTypes in 'GameTypes.pas',
  ImageCache in 'ImageCache.pas',
  ImageLoader in 'ImageLoader.pas',
  Lander in 'Lander.pas',
  LanderTypes in 'LanderTypes.pas',
  Map in 'Map.pas',
  MapTypes in 'MapTypes.pas',
  LocalCacheManager in 'LocalCacheManager.pas',
  MathUtils in '..\..\Utils\Misc\MathUtils.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TVoxelRendererForm, VoxelRendererForm);
  Application.Run;
end.
