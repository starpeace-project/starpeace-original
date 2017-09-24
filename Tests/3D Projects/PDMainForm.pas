unit PDMainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, 
  OutputEngine, RMEngineInt, Engine3D, Dresser, ClassLibrary, VisualClasses,
  Notifications;

const
  GridSizeX = 100;
  GridSizeZ = 100;

type
  TForm1 =
    class(TForm, IHook, IModelLoader)
        procedure FormCreate(Sender: TObject);
        procedure FormActivate(Sender: TObject);
        procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
      private
        fInitialized    : boolean;
        fActive         : boolean;
        f3DEngine       : I3DEngine;
        fDDOutputEngine : IDDrawOutputEngine;
        fDresser        : IMeshDresser;
        fClassRenderer  : IVisualClassesRenderer;
        fClassLibrary   : TClassLibrary;
        fClassContainer : TClassContainer;
        fAnimated       : I3DAnimated;
        fCamera         : I3DCamera;
        procedure InitNotifications;
        procedure InitClasses;
        procedure InitAmbientEffects;
        procedure InitMaterials;
        procedure CreateScene;
        procedure OnIdle(Sender: TObject; var Done: Boolean);
        function  InsertObject( x, y, z : T3DValue; Parent : I3DObject; ClassId : cardinal ) : I3DMesh;
        procedure Notify( Event : TEventClass; const Info );
      private
        procedure LoadPartition( x, y, z : integer; Parent : I3DObject );
        procedure UnLoadPartition( x, y, z : integer; Parent : I3DObject );
    end;

var
  Form1: TForm1;

implementation

  uses
    DDEngine, RMEngine, RMDresser, AmbientEffectsInt, AmbientEffects, D3DRMDef, Events;

  const
    CLASSID_GUY = 100;

{$R *.DFM}

  procedure TForm1.FormCreate(Sender: TObject);
    begin
      fInitialized       := false;
      fActive            := false;
      InitNotifications;
      fDDOutputEngine    := TDDOutputEngine.Create( Handle, DMMODE_WINDOWED );
      f3DEngine          := TD3DRMEngine.Create( Width, Height );
      InitClasses;
      InitMaterials;
      Application.OnIdle := OnIdle;
    end;

  procedure TForm1.FormActivate(Sender: TObject);
    var
      VideoModeId : TVideoModeId;
    begin
      fActive := Application.Active;
      if fActive
        then
          begin
            if not fInitialized
              then
                begin
                  if SUCCEEDED( f3DEngine.Initialize( fDDOutputEngine as IOutputDevice ))
                    then
                      begin
                        fInitialized := true;
                        fInitialized := fDDOutputEngine.FindVideoMode( 640, 480, 16, VideoModeId ) and
                                        SUCCEEDED(fDDOutputEngine.setVideoMode( VideoModeId ));
                        CreateScene;
                      end;
                end;
          end;
    end;

  type
    TDrawArea =
      class( TInterfacedObject, IDrawArea )
        public
        x, y, width, height : DWORD;
        function getX : DWORD;
        function getY : DWORD;
        function getWidth : DWORD;
        function getHeight : DWORD;
      end;

  // TDrawArea

  function TDrawArea.getX : DWORD;
    begin
      result := x;
    end;

  function TDrawArea.getY : DWORD;
    begin
      result := y;
    end;

  function TDrawArea.getWidth : DWORD;
    begin
      result := width;
    end;

  function TDrawArea.getHeight : DWORD;
    begin
      result := height;
    end;

  function CreateDrawArea( x, y, width, height : DWORD ) : IDrawArea;
    var
      DA : TDrawArea;
    begin
      DA := TDrawArea.Create;
      DA.x := x;
      DA.y := y;
      DA.width := width;
      DA.height := height;
      result := DA;
    end;

  procedure TForm1.InitNotifications;
    begin
      InitNotificationEngine;
      RegisterEventClass( evAnimationFinishes, 0 );
      RegisterEventClass( evCameraMove, 0 );
      RegisterEventClass( evCameraCreated, 0 );
      RegisterEventClass( evCameraRemoved, 0 );
    end;

  procedure TForm1.InitClasses;
    var
      i : integer;
    begin
      fClassContainer    := TClassContainer.Create;
      fClassContainer.AddSearchPath( '\work\pd client\release\classes' );
      fClassContainer.RegisterClasses;

      fClassLibrary      := TClassLibrary.Create;
      fDresser           := TRetainedModeDresser.Create( fClassLibrary, f3DEngine as ID3DRMEngine );
      fClassRenderer     := fDresser as IVisualClassesRenderer;

      for i := 0 to pred(fClassContainer.Count) do
        fClassRenderer.RenderVisualClass( fClassContainer.Classes[i] );
    end;

  procedure TForm1.InitAmbientEffects;
    var
      FogParams     : TFogParams;
      AmbientEffect : IAmbientEffect;
    begin
      AmbientEffect := TFogAmbientEffect.Create( AMBIENT_FOG, f3DEngine as ID3DRMEngine );
      with FogParams do
        begin
          PByteArray(@Color)[0] := 100;
          PByteArray(@Color)[1] := 100;
          PByteArray(@Color)[2] := 100;
          PByteArray(@Color)[3] := 0;

          FogMode    := D3DRMFOG_LINEAR;
          FogStart   := 10;
          FogEnd     := 200;
          FogDensity := 0;
        end;
      AmbientEffect.SetParams( FogParams );
      f3DEngine.RegisterAmbientEffect( AmbientEffect );
    end;

  procedure TForm1.InitMaterials;
    var
      MaterialClass : IMaterialClass;
    begin
      MaterialClass := f3DEngine.getMaterialLibrary.CreateMaterialClass;
      MaterialClass.SetMaterialId( pchar('pepe') );
      MaterialClass.CreateMapFromFile( 0, pchar('c:\work\media\tex3.ppm') );
    end;

  procedure TForm1.CreateScene;
    var
      XCamera     : I3DCamera;
      Viewport1   : I3DViewport;
      Viewport2   : I3DViewport;
      Light       : I3DLight;
      Mesh        : I3DMesh;
      SubMesh     : I3DMesh;
      Animated    : I3DAnimated;
      Animation   : IAnimation;
      Pos         : T3DVector;
      Material    : IMaterial;
    begin
      f3DEngine.AddEventHook( evAnimationFinishes, self );

      // LM Support!!!
      f3DEngine.setLMGridInfo( GridSizeX, 10, GridSizeZ );
      f3DEngine.EnableLMSupport( self );


      f3DEngine.CreateCamera( 0, nil, fCamera );
      f3DEngine.CreateViewport( CreateDrawArea( 0, 0, 200, 200 ), fCamera, Viewport1 );
      fCamera.setVisualRange( 1, 50000 );

      f3DEngine.CreateLight( LIGHT_DIRECTIONAL, nil, Light );
      Light.setPosition( 0, 0, 500 );
      Light.SetOrientation(0, 0, -1, 0, 1, 0);
      Light.setRGBColor( 1, 1, 1 );

      f3DEngine.CreateLight( LIGHT_AMBIENT, nil, Light );
      Light.setRGBColor( 0.6, 0.6, 0.6 );

      {
      Material := f3DEngine.getMaterialLibrary.CreateMaterial( 'pepe' );
      Mesh     := InsertObject( 0, 0, 150, CLASSID_GUY ) as I3DMesh;
      Mesh.SetOrientation(0, 0, 1, 0, 1, 0);


      f3DEngine.CreateCamera( 0, Mesh, XCamera );
      XCamera.SetPosition( 0, 0, 300 );
      XCamera.SetOrientation(0, 0, -1, 0, 1, 0);
      XCamera.setVisualRange(1, 600 );
      f3DEngine.CreateViewport( CreateDrawArea( 0, 200, 200, 200 ), XCamera, Viewport2 );

      //Mesh.SetMaterial( Material );

      SubMesh   := Mesh.getNamedSubObject( pchar('x3ds_Chest') );
      SubMesh.SetMaterial( Material );
      Animated  := SubMesh as I3DAnimated;
      Animation := Animated.CreateAnimation( 20, 12 );
      Animation.SetLooped( false );

      SubMesh.GetPosition( Pos.x, Pos.y, Pos.z );
      Animation.AddKeyAtFrame( 1, KEYTYPE_POSITION, Pos );

      Pos.x := -10;
      Pos.y := 0;
      Pos.z := 0;
      Animation.AddKeyAtFrame( 10, KEYTYPE_POSITION, Pos );

      SubMesh.GetPosition( Pos.x, Pos.y, Pos.z );
      Animation.AddKeyAtFrame( 19, KEYTYPE_POSITION, Pos );
      Animated.Animate( Animation.GetAnimationId );
      fAnimated := Mesh as I3DAnimated;
      }
    end;

  procedure TForm1.OnIdle(Sender: TObject; var Done: Boolean);
    begin
      try
        if (fInitialized and fActive)
          then
            begin
              f3DEngine.Move( 1 );
              f3DEngine.Render;
              Done := false;
            end;
        except
        end;
    end;

  function TForm1.InsertObject( x, y, z : T3DValue; Parent : I3DObject; ClassId : cardinal ) : I3DMesh;
    var
      Obj  : I3DObject;
      Mesh : I3DMesh;
    begin
      if SUCCEEDED(f3DEngine.CreateObject( OBJTYPE_MESH, Parent, 0, Obj ))
        then
          begin
            Mesh := Obj as I3DMesh;
            fDresser.DressMesh( Mesh, ClassId );
            Mesh.SetPosition( x, y, z );
            result := Mesh;
          end;
    end;

  procedure TForm1.Notify( Event : TEventClass; const Info );
    begin
      fAnimated.Animate( 100 + random(6) );
    end;

  procedure TForm1.LoadPartition( x, y, z : integer; Parent : I3DObject );
    var
      Pos : T3DVector;
    begin
      Pos.x := x*GridSizeX;
      Pos.y := 5;
      Pos.z := z*GridSizeZ;
      InsertObject( Pos.x, Pos.y, Pos.z, Parent, CLASSID_GUY );
    end;

  procedure TForm1.UnLoadPartition( x, y, z : integer; Parent : I3DObject );
    begin
      f3DEngine.RemoveObject( Parent );
    end;

  procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    var
      Dir, Up : T3DVector;
      Pos     : T3DVector;
    begin
      case Key of
        VK_UP :
          begin
            fCamera.getOrientation( Dir.x, Dir.y, Dir.z, Up.x, Up.y, Up.z );
            fCamera.getRelativePosition( nil, Pos.x, Pos.y, Pos.z );
            fCamera.SetRelativePosition( nil, Pos.x + Dir.x*20, Pos.y + Dir.y*20, Pos.z + Dir.z*20 );
          end;
        VK_DOWN :
          begin
            fCamera.getOrientation( Dir.x, Dir.y, Dir.z, Up.x, Up.y, Up.z );
            fCamera.getRelativePosition( nil, Pos.x, Pos.y, Pos.z );
            fCamera.SetRelativePosition( nil, Pos.x - Dir.x*20, Pos.y - Dir.y*20, Pos.z - Dir.z*20 );
          end;
        VK_LEFT :
          fCamera.SetAngVelocity( 0, 1, 0, -0.1 );
        VK_RIGHT :
          fCamera.SetAngVelocity( 0, 1, 0, 0.1 );
      end;
    end;

  procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    begin
      case Key of
        VK_UP, VK_DOWN :
          fCamera.SetVelocity( 0 );
        VK_LEFT, VK_RIGHT :
          fCamera.SetAngVelocity( 0, 1, 0, 0 );
      end;
    end;

end.
