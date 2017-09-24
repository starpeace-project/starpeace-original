{$APPTYPE CONSOLE}
program surfacetest;

uses
  Math,
  Classes,
  ClassStorage in '..\Kernel\ClassStorage.pas',
  Surfaces in '..\Surfaces\Surfaces.pas',
  PyramidalModifier in '..\Surfaces\PyramidalModifier.pas';

procedure DoStuff;
  var
    SP     : TSurfacePool;
    IP     : TIntegratorPool;
    I      : TSurfaceIntegrator;
    M1, M2 : TSurfaceModifier;
    x, y   : integer;
  begin
    SP := TSurfacePool.Create;
    IP := TIntegratorPool.Create;
    InitTheClassStorage;
    TheClassStorage.RegisterClass( tidClassFamily_SurfacePools, tidSurfacePool_Surfaces, SP );
    TheClassStorage.RegisterClass( tidClassFamily_SurfacePools, tidSurfacePool_Integrators, IP );
    SP.AddSurface( TSurface.Create( 'TestSurface', 'Test Surface' ));
    M1 := TPyramidalModifier.Create( 'TestSurface', Point(50, 50), 400, 0.1 );
    M2 := TPyramidalModifier.Create( 'TestSurface', Point(45, 45), -400, 0.1 );
    I := TSurfaceIntegrator.Create( 'TestSurface', Rect( 0, 0, 100, 100 ) );
    IP.IntegrateAll;
    for y := 40 to 60 do
      begin
        for x := 40 to 60 do
          write( ' ', (M1[x, y] + M2[x, y]):2:0 );
        writeln;
      end;
    writeln( I.Value:2:2 );
    writeln( M2[45, 45] );
    readln;
  end;

begin
  DoStuff;
end.
