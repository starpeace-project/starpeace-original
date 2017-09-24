unit GameTypes;

interface

  uses
    Windows, Classes, Graphics, SoundTypes, Warnings, SpriteImages, SpeedBmp;

  type
    index = 0..0;

  const                     // Focus kinds  (0..9 ~ Reserved)
    fkSelection =  0;

  type
    TGameImage   = TFrameImage;
    TCanvasImage = TSpeedBitmap;

  type
    PRectArray = ^TRectArray;
    TRectArray = array[0..0] of TRect;

  type
    TZoomLevel = integer;
    TZoomRes   = (zr4x8, zr8x16, zr16x32, zr32x64);  // ZoomLevel ~ ord(ZoomRes)
    TRotation  = (drNorth, drEast, drSouth, drWest);

  type
    TAngle = (agN, agNNE, agNE, agENE, agE, agESE, agSE, agSSE, agS, agSSW, agSW, agWSW, agW, agWNW, agNW, agNNW);

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
    IImager   = interface;

    IGameFocus =
      interface(IGameUpdater)
        procedure MouseMove(x, y : integer);
        procedure MouseClick;
        procedure KeyPressed(which : word; const Shift : TShiftState);
        procedure Refresh;
        function  GetText(kind : integer) : string;
        function  GetObject : TObject;
        procedure Dispatch(var msg);
        function  GetView : IGameView;
        function  GetImager : IImager;
        function  GetSoundManager : ISoundManager;
        function  GetInformant : IWarningInformant;
        function  GetZoomFactor : single;
        function  GetRect : TRect;
        procedure SetRect(const R : TRect);
        function  IsLandVisible : boolean;
        function  GetSmallTextData(out x, y : integer; out text : string; out color : TColor) : boolean;
        function  HasValidSurface : boolean;
      end;

    IGameDocument = interface;

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
        function  GetImageSuit : integer;
        // public
        procedure UpdateRegions(const which : array of TRect);
        function  GetFocus : IGameFocus;
        function  GetSize : TPoint;
        function  ViewPtToScPt(const which : TPoint) : TPoint;
        function  ScPtToViewPt(const which : TPoint) : TPoint;
        function  GetTextDimensions(const text : string; out width, height : integer) : boolean;
        property  Origin              : TPoint        read GetOrigin     write SetOrigin;
        property  Size                : TPoint        read GetSize;
        property  Document            : IGameDocument read GetDocument   write SetDocument;
        property  ZoomLevel           : TZoomLevel    read GetZoomLevel  write SetZoomLevel;
        property  Rotation            : TRotation     read GetRotation   write SetRotation;
      end;

    IGameDocument =
      interface
        procedure RenderSnapshot(const view : IGameView; const ClipRect : TRect; target : TCanvasImage);
        function  ClipMovement(const view : IGameView; var dx, dy : integer) : boolean;
        function  CreateFocus(const view : IGameView) : IGameFocus;
        procedure SetImageSuit( ImageSuit : integer );
      end;

    IImager =
      interface
        function  GetObjectImage(id : integer; angle : TAngle) : TGameImage;
        function  GetSpareImage : TGameImage;
        function  GetDownloadImage : TGameImage;
        function  GetShadeImage : TGameImage;
        function  GetRedShadeImage : TGameImage;
        function  GetBlackShadeImage : TGameImage;
        {$IFDEF SHOWCNXS}
        function  GetCnxSourceDownImage : TGameImage;
        function  GetCnxSourceTopImage : TGameImage;
        function  GetCnxDestDownImage : TGameImage;
        function  GetCnxDestTopImage : TGameImage;
        {$ENDIF}
        procedure SetImageSuit( ImageSuit : integer );
        function  GetImageSuit : integer;
        procedure AllowDownloadNotifies;
        procedure ForbidDownloadNotifies;
      end;

implementation

end.
