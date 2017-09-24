unit GameTypes;

interface

  uses
    Windows, Classes, Graphics, SoundTypes, Warnings, SpriteImages, SpeedBmp, Controls;

  type
    index = 0..0;

  const                     // Focus kinds  (0..9 ~ Reserved)
    fkSelection =  0;

  const
    cMaxBuildingHeight = 34;// 26;  
    cMaxLandHeight     = 3;

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
        procedure QueryViewUpdate(Defer : boolean);
      end;

  type
    IGameView = interface;
    IImager   = interface;

    IGameFocus =
      interface(IGameUpdater)
        procedure MouseMove(x, y : integer);
        procedure MouseLeave;
        procedure MouseClick(const Button : TMouseButton);
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
        procedure DestroyAll;
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
        function  GetClientRect: TRect;
        function  ViewPtToScPt(const which : TPoint) : TPoint;
        function  ScPtToViewPt(const which : TPoint) : TPoint;
        function  GetTextDimensions(const text : string; out width, height : integer) : boolean;
        procedure UpdateZone;
        property  Origin              : TPoint        read GetOrigin     write SetOrigin;
        property  Size                : TPoint        read GetSize;
        property  ClientRect          : TRect         read GetClientRect;
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
        procedure DestroyAll;
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

  procedure incrow(const view : IGameView; i, j : integer; out ri, rj : integer);
  procedure decrow(const view : IGameView; const  i, j : integer; out ri, rj : integer);
  procedure inccol(const view : IGameView; i, j : integer; out ri, rj : integer);
  procedure deccol(const view : IGameView; i, j : integer; out ri, rj : integer);
  procedure offsetcoords(const view : IGameView; i, j : integer; out ri, rj : integer; iofs, jofs : integer);
  function calcrow(const view : IGameView; i, j, rows, cols : integer) : integer;
  function calccol(const view : IGameView; i, j, rows, cols : integer) : integer;


implementation

  procedure incrow(const view : IGameView; i, j : integer; out ri, rj : integer);
    begin
      case view.Rotation of
        drNorth:
          begin
            ri := i + 1;
            rj := j;
          end;
        drEast:
          begin
            ri := i;
            rj := j + 1;
          end;
        drSouth:
          begin
            ri := i - 1;
            rj := j;
          end;
        drWest:
          begin
            ri := i;
            rj := j - 1;
          end;
      end;
    end;

  procedure decrow(const view : IGameView; const  i, j : integer; out ri, rj : integer);
    begin
      case view.Rotation of
        drNorth:
          begin
            ri := i - 1;
            rj := j;
          end;
        drEast:
          begin
            ri := i;
            rj := j - 1;
          end;
        drSouth:
          begin
            ri := i + 1;
            rj := j;
          end;
        drWest:
          begin
            ri := i;
            rj := j + 1;
          end;
      end;
    end;

  procedure inccol(const view : IGameView; i, j : integer; out ri, rj : integer);
    begin
      case view.Rotation of
        drNorth:
          begin
            ri := i;
            rj := j + 1;
          end;
        drEast:
          begin
            ri := i - 1;
            rj := j;
          end;
        drSouth:
          begin
            ri := i;
            rj := j - 1;
          end;
        drWest:
          begin
            ri := i + 1;
            rj := j;
          end;
      end;
    end;

  procedure deccol(const view : IGameView; i, j : integer; out ri, rj : integer);
    begin
      case view.Rotation of
        drNorth:
          begin
            ri := i;
            rj := j - 1;
          end;
        drEast:
          begin
            ri := i + 1;
            rj := j;
          end;
        drSouth:
          begin
            ri := i;
            rj := j + 1;
          end;
        drWest:
          begin
            ri := i - 1;
            rj := j;
          end;
      end;
    end;

  procedure offsetcoords(const view : IGameView; i, j : integer; out ri, rj : integer; iofs, jofs : integer);
    begin
      case view.Rotation of
        drNorth:
          begin
            ri := i + iofs;
            rj := j + jofs;
          end;
        drEast:
          begin
            ri := i - jofs;
            rj := j + iofs;
          end;
        drSouth:
          begin
            ri := i - iofs;
            rj := j - jofs;
          end;
        drWest:
          begin
            ri := i + jofs;
            rj := j - iofs;
          end;
      end;
    end;

  function calcrow(const view : IGameView; i, j, rows, cols : integer) : integer;
    begin
      case view.Rotation of
        drNorth:
          Result := i;
        drEast:
          Result := j;
        drSouth:
          Result := pred(rows) - i;
        else // drWest
          Result := pred(cols) - j;
      end;
    end;

  function calccol(const view : IGameView; i, j, rows, cols : integer) : integer;
    begin
      case view.Rotation of
        drNorth:
          Result := j;
        drEast:
          Result := pred(rows) - i;
        drSouth:
          Result := pred(cols) - j;
        else // drWest
          Result := i;
      end;
    end;

end.
