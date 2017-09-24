program PDClient;

uses
  Forms,
  PDMainForm in 'PDMainForm.pas' {Form1},
  Engine3D in '..\..\3D Engine\Engine3D.pas',
  Events in '..\..\3D Engine\Events.pas',
  Notifications in '..\..\3D Engine\Notifications.pas',
  OutputEngine in '..\..\3D Engine\OutputEngine.pas',
  AmbientEffects in '..\..\3D Engine\RM Engine\AmbientEffects.pas',
  AmbientEffectsInt in '..\..\3D Engine\RM Engine\AmbientEffectsInt.pas',
  ClassLibrary in '..\..\3D Engine\RM Engine\ClassLibrary.pas',
  D3DRMTypes in '..\..\3D Engine\RM Engine\D3DRMTypes.pas',
  DDEngine in '..\..\3D Engine\RM Engine\DDEngine.pas',
  FrameExplorer in '..\..\3D Engine\RM Engine\FrameExplorer.pas',
  LODs in '..\..\3D Engine\RM Engine\LODs.pas',
  RMEngine in '..\..\3D Engine\RM Engine\RMEngine.pas',
  RMEngineInt in '..\..\3D Engine\RM Engine\RMEngineInt.pas',
  D3D in '..\..\DirectX Sources\D3d.pas',
  D3DCaps in '..\..\DirectX Sources\D3DCaps.pas',
  D3DRM in '..\..\DirectX Sources\D3DRM.pas',
  D3DRMDef in '..\..\DirectX Sources\D3DRMDef.pas',
  D3DRMObj in '..\..\DirectX Sources\D3drmobj.pas',
  D3DRMWin in '..\..\DirectX Sources\D3DRMWin.pas',
  D3DTypes in '..\..\DirectX Sources\D3DTypes.pas',
  DDraw in '..\..\DirectX Sources\Ddraw.pas',
  DInput in '..\..\DirectX Sources\DInput.pas',
  DPlay in '..\..\DirectX Sources\DPlay.pas',
  DPLobby in '..\..\DirectX Sources\DPLobby.pas',
  DSetup in '..\..\DirectX Sources\DSetup.pas',
  DSound in '..\..\DirectX Sources\DSound.pas',
  DVP in '..\..\DirectX Sources\DVP.pas',
  DXFile in '..\..\DirectX Sources\DXFile.pas',
  DXTools in '..\..\DirectX Sources\DXTools.pas',
  MultiMon in '..\..\DirectX Sources\Multimon.pas',
  RMXFGUID in '..\..\DirectX Sources\RMXFGUID.pas',
  RMXFTmpl in '..\..\DirectX Sources\RMXFTmpl.pas',
  Dresser in '..\..\Fake Voyager3D\Components\Map3DView\Dresser.pas',
  RMDresser in '..\..\Fake Voyager3D\Components\Map3DView\RMDresser.pas',
  VisualClasses in '..\..\Fake Voyager3D\Components\Map3DView\VisualClasses.pas',
  Collection in '..\..\Kernel\Collection.pas',
  InterfaceCollection in '..\..\Kernel\InterfaceCollection.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
