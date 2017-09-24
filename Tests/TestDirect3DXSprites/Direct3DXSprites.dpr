program Direct3DXSprites;

uses
  Forms,
  MainWindow in 'MainWindow.pas' {MainForm},
  d3dx in '..\..\DirectX7\d3dx.pas',
  d3dxcore in '..\..\DirectX7\d3dxcore.pas',
  d3dxsprite in '..\..\DirectX7\d3dxsprite.pas',
  d3dxerr in '..\..\DirectX7\d3dxerr.pas',
  Dibs in '..\..\Utils\Graphics\Dibs.pas',
  ImageLoader in '..\..\Voyager\Components\MapIsoView\ImageLoader.pas',
  GameTypes in '..\..\Voyager\Components\MapIsoView\GameTypes.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
