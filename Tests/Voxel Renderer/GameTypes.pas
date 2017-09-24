unit GameTypes;

interface

uses
  Windows, Classes, Warnings, SpriteImages, SpeedBmp;

type
  index = 0..0;

const                     // Focus kinds  (0..9 ~ Reserved)
  fkSelection =  0;

type
  TGameImage   = TFrameImage;
  TCanvasImage = TSpeedBitmap;

type
  TZoomLevel = integer;
  TRotation  = (drNorth, drEast, drSouth, drWest);

type
  IGameUpdater =
    interface
      function  Lock : integer;
      function  Unlock : integer;
      function  LockCount : integer;
      procedure QueryUpdate(Defer : boolean);
    end;

type
  IGameView = interface;

  IGameFocus =
    interface(IGameUpdater)
      procedure MouseMove(x, y : integer);
      procedure MouseClick;
      procedure KeyPressed(which : word; const Shift : TShiftState);
      procedure Refresh;
      function  GetText(kind : integer) : string;
      function  GetObject : TObject;
      procedure Dispatch(var msg);
      function  GetRect : TRect;
      procedure SetRect(const R : TRect);
    end;

  IGameDocument = interface;

  TOnRegionsUpdateNotification     = procedure (const view : IGameView; const Regions : array of TRect) of object;
  TOnRegionsUpdateDoneNotification = procedure (const view : IGameView) of object;

  IGameView =
    interface(IGameUpdater)
      // private
      function  GetOrigin : TPoint;
      procedure SetOrigin(const which : TPoint);
      function  GetDocument : IGameDocument;
      procedure SetDocument(const which : IGameDocument);
      function  GetZoomLevel : TZoomLevel;
      procedure SetZoomLevel(which : TZoomLevel);
      function  GetRotation : TRotation;
      procedure SetRotation(which : TRotation);
      procedure SetOnRegionsUpdate(OnRegionsUpdate : TOnRegionsUpdateNotification);
      procedure SetOnRegionsUpdateDone(OnRegionsUpdateDone : TOnRegionsUpdateDoneNotification);
      // public
      procedure UpdateRegions(const which : array of TRect);
      function  GetFocus            : IGameFocus;
      function  GetSize             : TPoint;
      property  Origin              : TPoint        read GetOrigin     write SetOrigin;
      property  Size                : TPoint        read GetSize;
      property  Document            : IGameDocument read GetDocument   write SetDocument;
      property  ZoomLevel           : TZoomLevel    read GetZoomLevel  write SetZoomLevel;
      property  Rotation            : TRotation     read GetRotation   write SetRotation;
      property  OnRegionsUpdate     : TOnRegionsUpdateNotification     write SetOnRegionsUpdate;
      property  OnRegionsUpdateDone : TOnRegionsUpdateDoneNotification write SetOnRegionsUpdateDone;
    end;

  IGameDocument =
    interface
      procedure RenderSnapshot(const view : IGameView; const ClipRect : TRect; target : TCanvasImage);
      function  ClipMovement(const view : IGameView; var dx, dy : integer) : boolean;
      function  CreateFocus(const view : IGameView) : IGameFocus;
      procedure ViewChanged(const view : IGameView);
    end;


implementation

end.
