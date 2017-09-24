unit sufacetstwin;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Surfaces, ExtCtrls;

type
  TForm1 = class(TForm)
    map: TImage;
    Label1: TLabel;
    x: TEdit;
    Label2: TLabel;
    y: TEdit;
    Label3: TLabel;
    value: TEdit;
    Label4: TLabel;
    strength: TEdit;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
  private
    procedure PaintMap;
  private
    Surface : TSurface;
  end;

var
  Form1: TForm1;

implementation

  uses
    ClassStorage, PyramidalModifier;

{$R *.DFM}

 {
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
 }

  procedure TForm1.FormCreate(Sender: TObject);
    begin
      InitTheClassStorage;
      TheClassStorage.RegisterClass( tidClassFamily_SurfacePools, tidSurfacePool_Surfaces, TSurfacePool.Create );
      Surface := TSurface.Create( 'TestSurface', 'Test Surface' );
    end;

  procedure TForm1.PaintMap;
    var
      x, y : integer;
    begin
      for y := 0 to pred(map.Height) do
        for x := 0 to pred(map.Width) do
          
    end;

    
end.
