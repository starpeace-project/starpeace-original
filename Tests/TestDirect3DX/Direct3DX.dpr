program Direct3DX;

uses
  Forms,
  MainWindow in 'MainWindow.pas' {MainForm},
  d3dx in '..\..\DirectX7\d3dx.pas',
  d3dxcore in '..\..\DirectX7\d3dxcore.pas',
  d3dxsprite in '..\..\DirectX7\d3dxsprite.pas',
  d3dxerr in '..\..\DirectX7\d3dxerr.pas',
  DirectDrawUtils in '..\..\Utils\GameAPI\DirectDrawUtils.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
