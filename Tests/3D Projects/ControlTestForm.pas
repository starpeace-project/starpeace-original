unit ControlTestForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Panel3D,
  OutputEngine, RMEngineInt, Engine3D, Dresser, ClassLibrary, VisualClasses, Notifications,
  ExtCtrls;

type
  TfrmControlTest =
    class(TForm, IHook)
        procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
      private
        fInitialized    : boolean;
        fActive         : boolean;
        fDresser        : IMeshDresser;
        fClassRenderer  : IVisualClassesRenderer;
        fClassLibrary   : TClassLibrary;
        fClassContainer : TClassContainer;
        fAnimated       : I3DAnimated;
        fCamera         : I3DCamera;
        fPanel3D        : T3DPanel;
        procedure InitNotifications;
        procedure InitClasses;
        procedure CreateScene;
        procedure OnIdle(Sender: TObject; var Done: Boolean);
        function  InsertObject( x, y, z : T3DValue; Parent : I3DObject; ClassId : cardinal ) : I3DMesh;
        procedure Notify( Event : TEventClass; const Info );
    end;

var
  frmControlTest: TfrmControlTest;

implementation

  uses
    RMDresser, AmbientEffectsInt, AmbientEffects, D3DRMDef, Events;

  const
    CLASSID_GUY = 100;

  {$R *.DFM}

  procedure TfrmControlTest.FormCreate(Sender: TObject);
    begin
      InitNotifications;
      fPanel3D        :=  T3DPanel.Create( self );
      fPanel3D.Parent := self;
      fPanel3D.Top    := 100;
      fPanel3D.Left   := 100;
      fPanel3D.Width  := 200;
      fPanel3D.Height := 200;
      fInitialized    := false;
      fActive         := false;
      Application.OnIdle := OnIdle;
    end;

  procedure TfrmControlTest.FormActivate(Sender: TObject);
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
                  if fPanel3D.Initialize3D( DMMODE_WINDOWED )
                    then
                      begin
                        fInitialized := true;
                        fInitialized := fPanel3D.DDOutputEngine.FindVideoMode( 640, 480, 16, VideoModeId ) and
                                        SUCCEEDED(fPanel3D.DDOutputEngine.setVideoMode( VideoModeId ));
                        InitClasses;
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

  procedure TfrmControlTest.InitNotifications;
    begin
      InitNotificationEngine;
      RegisterEventClass( evAnimationFinishes, 0 );
      RegisterEventClass( evCameraMove, 0 );
      RegisterEventClass( evCameraCreated, 0 );
      RegisterEventClass( evCameraRemoved, 0 );
    end;

  procedure TfrmControlTest.InitClasses;
    var
      i : integer;
    begin
      fClassContainer    := TClassContainer.Create;
      fClassContainer.AddSearchPath( '\work\pd client\release\classes' );
      fClassContainer.RegisterClasses;

      fClassLibrary      := TClassLibrary.Create;
      fDresser           := TRetainedModeDresser.Create( fClassLibrary, fPanel3D.Engine3D as ID3DRMEngine );
      fClassRenderer     := fDresser as IVisualClassesRenderer;

      for i := 0 to pred(fClassContainer.Count) do
        fClassRenderer.RenderVisualClass( fClassContainer.Classes[i] );
    end;

  procedure TfrmControlTest.CreateScene;
    var
      XCamera     : I3DCamera;
      Viewport1   : I3DViewport;
      Viewport2   : I3DViewport;
      Light       : I3DLight;
      Mesh        : I3DMesh;
    begin
      fPanel3D.Engine3D.AddEventHook( evAnimationFinishes, self );

      fPanel3D.Engine3D.CreateCamera( 0, nil, fCamera );
      fPanel3D.Engine3D.CreateViewport( CreateDrawArea( 0, 0, 200, 200 ), fCamera, Viewport1 );
      fCamera.setVisualRange( 1, 50000 );

      fPanel3D.Engine3D.CreateLight( LIGHT_DIRECTIONAL, nil, Light );
      Light.setPosition( 0, 0, 500 );
      Light.SetOrientation(0, 0, -1, 0, 1, 0);
      Light.setRGBColor( 1, 1, 1 );

      fPanel3D.Engine3D.CreateLight( LIGHT_AMBIENT, nil, Light );
      Light.setRGBColor( 0.6, 0.6, 0.6 );

      Mesh := InsertObject( 0, 0, 150, nil, CLASSID_GUY ) as I3DMesh;
      Mesh.SetOrientation(0, 0, 1, 0, 1, 0);
      fAnimated := Mesh as I3DAnimated;
      fAnimated.Animate( 105 );

      fPanel3D.Engine3D.CreateCamera( 0, Mesh, XCamera );
      XCamera.SetPosition( 0, 0, 300 );
      XCamera.SetOrientation(0, 0, -1, 0, 1, 0);
      XCamera.setVisualRange(1, 600 );
      fPanel3D.Engine3D.CreateViewport( CreateDrawArea( 0, 200, 200, 200 ), XCamera, Viewport2 );
    end;

  procedure TfrmControlTest.OnIdle(Sender: TObject; var Done: Boolean);
    begin
      try
        if (fInitialized and fActive)
          then
            begin
              fPanel3D.Engine3D.Move( 1 );
              fPanel3D.Engine3D.Render;
              Done := false;
            end;
        except
        end;
    end;

  function TfrmControlTest.InsertObject( x, y, z : T3DValue; Parent : I3DObject; ClassId : cardinal ) : I3DMesh;
    var
      Obj  : I3DObject;
      Mesh : I3DMesh;
    begin
      if SUCCEEDED(fPanel3D.Engine3D.CreateObject( OBJTYPE_MESH, Parent, 0, Obj ))
        then
          begin
            Mesh := Obj as I3DMesh;
            fDresser.DressMesh( Mesh, ClassId );
            Mesh.SetPosition( x, y, z );
            result := Mesh;
          end;
    end;

  procedure TfrmControlTest.Notify( Event : TEventClass; const Info );
    begin
      fAnimated.Animate( 100 + random(6) );
    end;

end.
