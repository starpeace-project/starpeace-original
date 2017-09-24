unit Lander;

interface

  uses
    Windows, Classes, Graphics, GameTypes, LanderTypes, Filters;

  type
    PTextRenderingInfo = ^TTextRenderingInfo;
    TTextRenderingInfo =
      record
        x, y : integer;
        text : string;
      end;

  type
    PTextRenderingInfoArray = ^TTextRenderingInfoArray;
    TTextRenderingInfoArray = array [0..0] of TTextRenderingInfo;

  type
    TLander =
      class(TInterfacedObject, IGameDocument, ICoordinateConverter)
        public
          constructor Create(const Map : IWorldMap);
          destructor  Destroy;   override;
        private // IGameDocument
          procedure RenderSnapshot(const view : IGameView; const ClipRect : TRect; snap : TCanvasImage);
          function  ClipMovement(const view : IGameView; var dx, dy : integer) : boolean;
          function  CreateFocus(const view : IGameView) : IGameFocus;
          procedure SetImageSuit( ImageSuit : integer );
        private // ICoordinateConverter
          function ObjectToMap(const view : IGameView; x, y : integer; out i, j : integer; checkbase : boolean) : boolean;
          function ScreenToMap(const view : IGameView; x, y : integer; out i, j : integer) : boolean;
          function MapToScreen(const view : IGameView; i, j : integer; out x, y : integer) : boolean;
        private
          fMap : IWorldMap;
          procedure CalculateFirstGrid(const view : IGameView; var x, y : integer; out i, j : integer);
        private
          fLastZoom           : TZoomRes;
          fLastRotation       : TRotation;
          fSpareCache         : array [TZoomRes, TRotation] of TGameImage;
          fDownloadSign       : array [TZoomRes, TRotation] of TGameImage;
          fShadeCache         : array [TZoomRes, TRotation] of TGameImage;
          fRedShadeCache      : array [TZoomRes, TRotation] of TGameImage;
          fBlackShadeCache    : array [TZoomRes, TRotation] of TGameImage;
          fCnxSourceDownCache : array [TZoomRes, TRotation] of TGameImage;
          fCnxSourceTopCache  : array [TZoomRes, TRotation] of TGameImage;
          fCnxDestDownCache   : array [TZoomRes, TRotation] of TGameImage;
          fCnxDestTopCache    : array [TZoomRes, TRotation] of TGameImage;
        private
          fTextsToRenderCount : integer;
          fTextsToRenderAlloc : integer;
          fTextsToRender      : PTextRenderingInfoArray;
          procedure AddTextToRender(const texttorender : TTextRenderingInfo);
        private
          function CheckPoint(const view : IGameView; x, y, i, j : integer) : boolean;
          function CheckBuildingBase(const view : IGameView; x, y : integer; out i, j : integer) : boolean;
      end;

implementation

  uses
    SysUtils, AxlDebug, MathUtils, SpriteImages, ColorTableMgr, MapTypes,
    CanvasBmp {$IFDEF PROFILES}, Profiler, IsoProfile{$ENDIF};

  const
    cMaxBuildingHeight = 22;
    cMaxLandHeight     = 3;

  const
    cAlpha = 1;

  // Utils  >>

  function ClipInteger(var i : integer; min, max : integer) : boolean;
    begin
      Result := true;
      if i < min
        then i := min
        else
          if i > max
            then i := max
            else Result := false;
    end;

  function max(x, y : integer) : integer;
    begin
      if x >= y
        then Result := x
        else Result := y;
    end;

  function min(x, y : integer) : integer;
    begin
      if x <= y
        then Result := x
        else Result := y;
    end;

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

  procedure decrow(const view : IGameView; i, j : integer; out ri, rj : integer);
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

  procedure RotateItem(var item : TItemInfo; rotation : TRotation);
    var
      tmp : integer;
    begin
      case rotation of
        drNorth: ;
        drEast:
          begin
            tmp := item.r;
            item.r := item.c;
            item.c := pred(item.size) - tmp;
          end;
        drSouth:
          begin
            item.r := pred(item.size) - item.r;
            item.c := pred(item.size) - item.c;
          end;
        else // drWest
          begin
            tmp := item.r;
            item.r := pred(item.size) - item.c;
            item.c := tmp;
          end;
      end;
    end;

  // Cohen-Sutherland's algorithm

  function ClipLine(var srcx, srcy, destx, desty : integer; const ClipRect : TRect) : boolean;
    type
      TEdge  = (edTop, edBottom, edLeft, edRight);

    type
      TPointCode = set of TEdge;

    procedure CodePoint(x, y : integer; out Code : TPointCode);
      begin
        Code := [];
        if x < ClipRect.Left
          then Code := [edLeft]
          else
            if x > ClipRect.Right
              then Code := [edRight];
        if y < ClipRect.Top
          then Code := Code + [edTop]
          else
            if y > ClipRect.Bottom
              then Code := Code + [edBottom]
      end;

    var
      srccode, destcode : TPointCode;
      outcode           : TPointCode;
      x, y              : single;
    begin
      CodePoint(srcx, srcy, srccode);
      CodePoint(destx, desty, destcode);
      while (srccode*destcode = []) and ((srccode <> []) or (destcode <> [])) do
        begin
          if srccode <> []
            then outcode := srccode
            else outcode := destcode;
          if edTop in outcode
            then
              begin
                x := srcx + (destx - srcx)*(ClipRect.Top - srcy)/(desty - srcy);
                y := ClipRect.Top;
              end
            else
              if edBottom in outcode
                then
                  begin
                    x := srcx + (destx - srcx)*(ClipRect.Bottom - srcy)/(desty - srcy);
                    y := ClipRect.Bottom;
                  end
                else
                  if edLeft in outcode
                    then
                      begin
                        x := ClipRect.Left;
                        y := srcy + (desty - srcy)*(ClipRect.Left - srcx)/(destx - srcx);
                      end
                    else
                      begin
                        x := ClipRect.Right;
                        y := srcy + (desty - srcy)*(ClipRect.Right - srcx)/(destx - srcx);
                      end;
          if outcode = srccode
            then
              begin
                srcx := round(x);
                srcy := round(y);
                CodePoint(srcx, srcy, srccode)
              end
            else
              begin
                destx := round(x);
                desty := round(y);
                CodePoint(destx, desty, destcode)
              end;
        end;
      if (srccode = []) and (destcode = [])
        then Result := true
        else Result := false;
    end;

  function ClipDrawLine(const Canvas : TBufferCanvas; x1, y1, x2, y2 : integer; const ClipRect : TRect) : boolean;
    var
      srcx, srcy   : integer;
      destx, desty : integer;
    begin
      srcx := x1;
      srcy := y1;
      destx := x2;
      desty := y2;
      {
      if ClipLine(srcx, srcy, destx, desty, ClipRect)
        then
          begin
      }
            Canvas.MoveTo(srcx, srcy);
            Canvas.LineTo(destx, desty);
            Result := true;
      {
          end
        else Result := false;
      }
    end;

  // TLander

  constructor TLander.Create(const Map : IWorldMap);
    begin
      inherited Create;
      fMap := Map;
    end;

  destructor TLander.Destroy;
    begin
      assert(RefCount = 0);
      freemem(fTextsToRender);
      inherited;
    end;

  procedure TLander.RenderSnapshot(const view : IGameView; const ClipRect : TRect; snap : TCanvasImage);
    var
      x, y      : integer;
      i, j      : integer;
      y1, y2    : integer;
      u         : integer;
      focus     : IGameFocus;
      Imager    : IImager;
      cols      : integer;
      rows      : integer;
      zoom      : TZoomRes;
      rotation  : TRotation;

    function FilterObjectPalette(img : TGameImage; const obj : TObjInfo) : TPaletteInfo;
      begin
        with obj do
          if loReddened in Options
            then Result := img.GetFilteredPalette(pfReddenPalette, [0])
            else
              if loGrayed in Options
                then Result := img.GetFilteredPalette(pfColorToGray, [0])
                else
                  if loRedded in Options
                    then Result := img.GetFilteredPalette(pfRedFilter, [0])
                    else
                      if loGreened in Options
                        then Result := img.GetFilteredPalette(pfGreenFilter, [0])
                        else
                          if loDarkened in Options
                            then Result := img.GetFilteredPalette(pfDarkenPalette, [0])
                            else
                              if loColorTinted in Options
                                then Result := img.GetFilteredPalette(pfTintPalette, [shadeid and $FF, shadeid shr 8 and $FF, shadeid shr 16 and $FF, 0.5])
                                else Result := nil;
      end;

    procedure DrawSpareImage(x, y, Size : integer; const obj : TObjInfo);
      var
        i, j        : integer;
        xx, xi      : integer;
        yy, yi      : integer;
        PaletteInfo : TPaletteInfo;
      begin
        xi := x;
        yi := y;
        if fSpareCache[zoom, rotation] = nil
          then fSpareCache[zoom, rotation] := Imager.GetSpareImage;
        if fDownloadSign[zoom, rotation] = nil
          then fDownloadSign[zoom, rotation] := Imager.GetDownloadImage;
        PaletteInfo := FilterObjectPalette(fSpareCache[zoom, rotation], obj);
        for i := 0 to pred(Size) do
          begin
            xx := x;
            yy := y;
            for j := 0 to pred(Size) do
              begin
                if loGlassed in obj.Options
                  then fSpareCache[zoom, rotation].Draw(xx, yy + u - fSpareCache[zoom, rotation].Height, cAlpha, 0, ClipRect, snap, PaletteInfo)
                  else fSpareCache[zoom, rotation].Draw(xx, yy + u - fSpareCache[zoom, rotation].Height, 0, 0, ClipRect, snap, PaletteInfo);
                inc(xx, 2*u);
                dec(yy, u);
              end;
            inc(x, 2*u);
            inc(y, u);
          end;
        PaletteInfo := FilterObjectPalette(fDownloadSign[zoom, rotation], obj);
        if loGlassed in obj.Options
          then fDownloadSign[zoom, rotation].Draw(xi + (4*u*Size - fDownloadSign[zoom, rotation].Width) div 2, yi - fDownloadSign[zoom, rotation].Height + u, cAlpha, 0, ClipRect, snap, PaletteInfo)
          else fDownloadSign[zoom, rotation].Draw(xi + (4*u*Size - fDownloadSign[zoom, rotation].Width) div 2, yi - fDownloadSign[zoom, rotation].Height + u, 0, 0, ClipRect, snap, PaletteInfo);
      end;

    procedure DrawObject(x, y, Size : integer; const obj : TObjInfo);
      const
        corSize = 4;
      var
        img          : TGameImage;
        delta        : integer;
        dh           : integer;
        textrendinfo : TTextRenderingInfo;
        PaletteInfo  : TPaletteInfo;
        shadeimg     : TGameImage;
      begin
        with obj do
          begin
            img := Imager.GetObjectImage(id, obj.angle);
            delta := u*Size;
            if img <> nil
              then dh := img.Height - delta
              else dh := 2*u*Size - delta;
            if loGrated in Options
              then
                with snap do
                  begin
                    //Pen.Color := $0088BBBB;
                    Canvas.Pen.Color := clLime;
                    Canvas.Pen.Width := 2;

                    // corner A
                    ClipDrawLine(Canvas, x - 1, y - 2, x - 1 + 2*delta div corSize, y - delta div corSize - 2, ClipRect);
                    ClipDrawLine(Canvas, x - 1, y - 2, x - 1, y - 1 - dh div corSize - 2, ClipRect);
                    // corner B
                    ClipDrawLine(Canvas, x - 1, y - 1 - dh - 2, x - 1 + 2*delta div corSize, y - 1 - dh - delta div corSize - 2, ClipRect);
                    ClipDrawLine(Canvas, x - 1, y - 1 - dh - 2, x - 1, y - 1 - dh + dh div corSize - 2, ClipRect);
                    // corner G
                    ClipDrawLine(Canvas, x - 1 + 2*delta, y - delta - 2, x - 1 + 2*delta - 2*delta div corSize, y - delta + delta div corSize - 2, ClipRect);
                    ClipDrawLine(Canvas, x - 1 + 2*delta, y - delta - 2, x - 1 + 2*delta + 2*delta div corSize, y - delta + delta div corSize - 2, ClipRect);
                    ClipDrawLine(Canvas, x - 1 + 2*delta, y - delta - 2, x - 1 + 2*delta, y - delta - dh div corSize - 2, ClipRect);
                    // corner H
                    ClipDrawLine(Canvas, x - 1 + 2*delta, y - delta - dh - 2, x - 1 + 2*delta - 2*delta div corSize, y - delta + delta div corSize - dh - 2, ClipRect);
                    ClipDrawLine(Canvas, x - 1 + 2*delta, y - delta - dh - 2, x - 1 + 2*delta + 2*delta div corSize, y - delta + delta div corSize - dh - 2, ClipRect);
                    ClipDrawLine(Canvas, x - 1 + 2*delta, y - delta - dh - 2, x - 1 + 2*delta, y - delta - dh + dh div corSize - 2, ClipRect);
                    // corner E
                    ClipDrawLine(Canvas, x - 1 + 4*delta, y - 2, x - 1 + 4*delta - 2*delta div corSize, y - delta div corSize - 2, ClipRect);
                    ClipDrawLine(Canvas, x - 1 + 4*delta, y - 2, x - 1 + 4*delta, y - dh div corSize - 2, ClipRect);
                    // corner F
                    ClipDrawLine(Canvas, x - 1 + 4*delta, y - dh - 2, x - 1 + 4*delta - 2*delta div corSize, y - dh - delta div corSize - 2, ClipRect);
                    ClipDrawLine(Canvas, x - 1 + 4*delta, y - dh - 2, x - 1 + 4*delta, y - dh + dh div corSize - 2, ClipRect);

                    {
                    Pen.Color := clLime;
                    MoveTo(x - 1, y);
                    LineTo(x - 1 + 2*delta, y - 1 - delta);
                    LineTo(x - 1 + 4*delta, y - 1);
                    MoveTo(x - 1 + 2*delta, y - 1 - delta);
                    LineTo(x - 1 + 2*delta, y - 1 - delta - dh);
                    LineTo(x - 1 + 4*delta, y - 1 - dh);
                    MoveTo(x - 1 + 2*delta, y - 1 - delta - dh);
                    LineTo(x - 1, y - 1 - dh);
                    }
                  end;
            if img <> nil
              then
                begin
                  PaletteInfo := FilterObjectPalette(img, obj);
                  if loGlassed in Options
                    then img.Draw(x, y + u*Size - img.Height, cAlpha, Frame, ClipRect, snap, PaletteInfo)
                    else img.Draw(x, y + u*Size - img.Height, 0, Frame, ClipRect, snap, PaletteInfo);
                end
              else DrawSpareImage(x, y, size, obj);
            if loShaded in Options
              then
                begin
                  if fShadeCache[zoom, rotation] = nil
                    then fShadeCache[zoom, rotation] := Imager.GetShadeImage;
                  fShadeCache[zoom, rotation].Draw(x, y + u*Size - fShadeCache[zoom, rotation].Height, cAlpha, 0, ClipRect, snap, nil);
                end;
            if loRedShaded in Options
              then
                begin
                  if fRedShadeCache[zoom, rotation] = nil
                    then fRedShadeCache[zoom, rotation] := Imager.GetRedShadeImage;
                  fRedShadeCache[zoom, rotation].Draw(x, y + u*Size - fRedShadeCache[zoom, rotation].Height, cAlpha, 0, ClipRect, snap, nil);
                end;
            if loBlackShaded in Options
              then
                begin
                  if fBlackShadeCache[zoom, rotation] = nil
                    then fBlackShadeCache[zoom, rotation] := Imager.GetBlackShadeImage;
                  fBlackShadeCache[zoom, rotation].Draw(x, y + u*Size - fBlackShadeCache[zoom, rotation].Height, cAlpha, 0, ClipRect, snap, nil);
                end;
            if loColorShaded in Options
              then
                begin
                  shadeimg := Imager.GetObjectImage(shadeid, agN);
                  if shadeimg <> nil
                    then shadeimg.Draw(x, y + u*Size - shadeimg.Height, cAlpha, 0, ClipRect, snap, nil);
                end;
            if loGrated in Options
              then
                with snap do
                  begin
                    // corner A
                    ClipDrawLine(Canvas, x - 1, y - 2, x - 1 + 2*delta div corSize, y + delta div corSize - 2, ClipRect);
                    // corner B
                    ClipDrawLine(Canvas, x - 1, y - 1 - dh - 2, x - 1 + 2*delta div corSize, y - 1 - dh + delta div corSize - 2, ClipRect);
                    {
                    // corner C
                    MoveTo(x - 1 + 2*delta, y + delta - dh - 2);
                    LineTo(x - 1 + 2*delta - 2*delta div corSize, y - dh + delta - delta div corSize - 2);
                    MoveTo(x - 1 + 2*delta, y + delta - dh - 2);
                    LineTo(x - 1 + 2*delta + 2*delta div corSize, y - dh + delta - delta div corSize - 2);
                    MoveTo(x - 1 + 2*delta, y + delta - dh - 2);
                    LineTo(x - 1 + 2*delta, y + delta - dh + dh div corSize - 2);
                    }
                    // corner D
                    ClipDrawLine(Canvas, x - 1 + 2*delta, y + delta - 2, x - 1 + 2*delta - 2*delta div corSize, y + delta - delta div corSize - 2, ClipRect);
                    ClipDrawLine(Canvas, x - 1 + 2*delta, y + delta - 2, x - 1 + 2*delta + 2*delta div corSize, y + delta - delta div corSize - 2, ClipRect);
                    ClipDrawLine(Canvas, x - 1 + 2*delta, y + delta - 2, x - 1 + 2*delta, y + delta - dh div corSize - 2, ClipRect);
                    // corner E
                    ClipDrawLine(Canvas, x - 1 + 4*delta, y - 2, x - 1 + 4*delta - 2*delta div corSize, y + delta div corSize - 2, ClipRect);
                    // corner F
                    ClipDrawLine(Canvas, x - 1 + 4*delta, y - dh - 2, x - 1 + 4*delta - 2*delta div corSize, y - dh + delta div corSize - 2, ClipRect);
                    {
                    MoveTo(x - 1, y - 1);
                    LineTo(x - 1 + 2*delta, y - 1 + delta);
                    LineTo(x - 1 + 4*delta, y - 1);
                    LineTo(x - 1 + 4*delta, y - 1 - dh);
                    LineTo(x - 1 + 2*delta, y - 1 + delta - dh);
                    LineTo(x - 1 + 2*delta, y - 1 + delta);
                    MoveTo(x - 1, y - 1);
                    LineTo(x - 1, y - 1 - dh);
                    LineTo(x - 1 + 2*delta, y - 1 + delta - dh);
                    }
                  end;
            if (Caption <> '') or (loGrated in Options) and (id and idMask = idBuildingMask)
              then
                begin
                  inc(x, 2*u*Size);
                  if img <> nil
                    then dec(y, img.Height - delta)
                    else dec(y, u*Size - delta);
                  textrendinfo.x := x;
                  textrendinfo.y := y;
                  textrendinfo.text := Caption;
                  AddTextToRender(textrendinfo);
                end;
          end;
      end;

    procedure DrawSimpleObject(x, y : integer; const obj : TObjInfo);
      var
        img         : TGameImage;
        PaletteInfo : TPaletteInfo;
      begin
        with obj do
          begin
            img := Imager.GetObjectImage(id, obj.angle);
            if img <> nil
              then
                begin
                  PaletteInfo := FilterObjectPalette(img, obj);
                  if loGlassed in Options
                    then img.Draw(x, y, cAlpha, Frame, ClipRect, snap, PaletteInfo)
                    else img.Draw(x, y, 0, Frame, ClipRect, snap, PaletteInfo);
                  if loGrated in Options
                    then
                      with snap.Canvas do
                        begin
                          Brush.Style := bsClear;
                          Pen.Color := $0088BBBB;
                          Rectangle(x, y, x + img.Width, y + img.Height);
                        end;
                end;
          end;
      end;

    function InsideRightEdge(i, j : integer) : boolean;
      begin
        case rotation of
          drNorth:
            Result := (j < cols) and (i >= 0);
          drEast:
            Result := (j >= 0) and (i >= 0);
          drSouth:
            Result := (j >= 0) and (i < rows);
          else // drWest
            Result := (j < cols) and (i < rows);
        end;
      end;

    function InsideBottomEdge(i, j : integer) : boolean;
      begin
        case rotation of
          drNorth:
            Result := i >= 0;
          drEast:
            Result := j >= 0;
          drSouth:
            Result := i < rows;
          else // drWest
            Result := j < cols;
        end;
      end;

    function OnLeftEdge(i, j : integer) : boolean;
      begin
        case rotation of
          drNorth:
            Result := j = 0;
          drEast:
            Result := i = pred(rows);
          drSouth:
            Result := j = pred(cols);
          else // drWest
            Result := i = 0;
        end;
      end;

    procedure DrawLandLayers(x, y, i, j : integer);

      procedure DrawRank(x, i, j : integer);
        var
          OnLeft       : boolean;
          OnTop        : boolean;
          item         : TItemInfo;
          k            : integer;
          drawi, drawj : integer;

        procedure RotateDrawPoint(var i, j : integer; size : integer);
          begin
            case rotation of
              drNorth: ;
              drEast:
                dec(j, pred(size));
              drSouth:
                begin
                  inc(i, pred(size));
                  dec(j, pred(size));
                end;
              else // drWest
                inc(i, pred(size));
            end;
          end;

        procedure DrawItemBase(const item : TItemInfo;x, y, i, j, Size : integer);
          var
            r, c    : integer;
            obj     : TObjInfo;
            overlay : TItemInfo;
          begin
            inc(x, 2*pred(Size)*u);
            dec(y, pred(Size)*u);
            RotateDrawPoint(i, j, Size);
            for c := j + Size - 1 downto j do
              begin
                for r := i downto i - Size + 1 do
                  begin
                    if (lsOverlayedGround in item.States) and fMap.GetGroundOverlayInfo(r, c, focus, overlay)
                      then
                        begin
                          if lsGrounded in overlay.States
                            then
                              if fMap.GetGroundInfo(r, c, focus, obj)
                                then DrawObject(x, y, 1, obj);
                          DrawObject(x, y, 1, overlay);
                        end
                      else
                        if fMap.GetGroundInfo(r, c, focus, obj)
                          then DrawObject(x, y, 1, obj);
                    inc(x, 2*u);
                    inc(y, u);
                  end;
                dec(x, 2*succ(Size)*u);
                dec(y, pred(Size)*u);
              end;
          end;

        procedure DrawOverlays(x, y, i, j, Size : integer);
          var
            r, c        : integer;
            overlayinfo : TItemOverlayInfo;
            k           : integer;
          begin
            inc(x, 2*pred(Size)*u);
            dec(y, pred(Size)*u);
            RotateDrawPoint(i, j, Size);
            for c := j + Size - 1 downto j do
              begin
                for r := i downto i - Size + 1 do
                  begin
                    if fMap.GetItemOverlayInfo(r, c, focus, overlayinfo)
                      then
                        with overlayinfo do
                          for k := 1 to count do
                            DrawSimpleObject(x + objects[k].x, y + objects[k].y, objects[k]);
                    inc(x, 2*u);
                    inc(y, u);
                  end;
                dec(x, 2*succ(Size)*u);
                dec(y, pred(Size)*u);
              end;
          end;

        procedure ClipDrawOverlays(x, y, i, j : integer; xmin, xmax, ymin, ymax : integer);
          var
            overlayinfo : TItemOverlayInfo;
            k           : integer;
            img         : TGameImage;
            ImgClipRect : TRect;
            PaletteInfo : TPaletteInfo;
          begin
            if fMap.GetItemOverlayInfo(i, j, focus, overlayinfo)
              then
                with overlayinfo do
                  for k := 1 to count do
                    begin
                      img := Imager.GetObjectImage(objects[k].id, objects[k].angle);
                      if img <> nil
                        then
                          begin
                            if loNoClip in objects[k].Options
                              then ImgClipRect := ClipRect
                              else
                                begin
                                  ImgClipRect := Rect(max(xmin, x + objects[k].x), max(ymin, y + objects[k].y), min(xmax, x + objects[k].x + img.Width), min(ymax, y + objects[k].y + img.Height));
                                  IntersectRect(ImgClipRect, ImgClipRect, ClipRect);
                                end;
                            if not IsRectEmpty(ImgClipRect)
                              then
                                begin
                                  PaletteInfo := FilterObjectPalette(img, objects[k]);
                                  if loGlassed in objects[k].Options
                                    then img.Draw(x + objects[k].x, y + objects[k].y, cAlpha, objects[k].Frame, ImgClipRect, snap, PaletteInfo)
                                    else img.Draw(x + objects[k].x, y + objects[k].y, 0, objects[k].Frame, ImgClipRect, snap, PaletteInfo);
                                end;
                          end;
                    end;
          end;

        procedure DrawItemOverlays(x, y : integer; const item : TItemInfo);
          var
            k           : integer;
            itemimg     : TGameImage;
            overlimg    : TGameImage;
            PaletteInfo : TPaletteInfo;
          begin
            itemimg := Imager.GetObjectImage(item.id, item.angle);
            if itemimg <> nil
              then
                with item, Overlays do
                  for k := 1 to Count do
                    begin
                      overlimg := Imager.GetObjectImage(Objects[k].id, Objects[k].angle);
                      if overlimg <> nil
                        then
                          begin
                            PaletteInfo := FilterObjectPalette(overlimg, objects[k]);
                            if loGlassed in Objects[k].Options
                              then overlimg.Draw(x + Objects[k].x, y + u*Size - itemimg.Height + Objects[k].y, cAlpha, Objects[k].Frame, ClipRect, snap, PaletteInfo)
                              else overlimg.Draw(x + Objects[k].x, y + u*Size - itemimg.Height + Objects[k].y, 0, Objects[k].Frame, ClipRect, snap, PaletteInfo);
                          end;
                    end;
          end;

        procedure DrawItem(x, y, i, j : integer);
          var
            upitem    : TItemInfo;
            ritem     : TItemInfo;
            upi, upj  : integer;
            ri, rj    : integer;
            clipdelta : integer;
          begin
            if lsHighLand in item.States
              then clipdelta := 10
              else clipdelta := 0;
            if loUpProjected in item.Options
              then
                begin
                  incrow(view, i, j, upi, upj);
                  if fMap.GetItemInfo(upi, upj, focus, upitem) and (lsOverlayed in upitem.States)
                    then ClipDrawOverlays(x - 2*u, y - u, upi, upj, x, x + 4*u, y - u - clipdelta, y + u);
                  inccol(view, i, j, upi, upj);
                  if fMap.GetItemInfo(upi, upj, focus, upitem) and (lsOverlayed in upitem.States)
                    then ClipDrawOverlays(x + 2*u, y - u, upi, upj, x, x + 4*u, y - u - clipdelta, y + u);
                  if lsGrounded in item.States
                    then DrawItemBase(item, x, y, i, j, item.Size);
                  DrawObject(x, y, item.Size, item);
                  DrawItemOverlays(x, y, item);
                  if lsOverlayed in item.States
                    then DrawOverlays(x, y, i, j, item.Size);
                end
              else
                begin
                  if lsGrounded in item.States
                    then DrawItemBase(item, x, y, i, j, item.Size);
                  DrawObject(x, y, item.Size, item);
                  DrawItemOverlays(x, y, item);
                  incrow(view, i, j, upi, upj);
                  fMap.GetItemInfo(upi, upj, focus, upitem);
                  inccol(view, i, j, ri, rj);
                  fMap.GetItemInfo(ri, rj, focus, ritem);
                  if (lsDoubleOverlayed in item.States) or (lsDoubleOverlayed in upitem.States) or (lsDoubleOverlayed in ritem.States)
                    then DrawOverlays(x, y, i, j, item.Size);
                  if lsOverlayed in upitem.States
                    then ClipDrawOverlays(x - 2*u, y - u, upi, upj, x, x + 4*u, y - u - clipdelta, y + u);
                  if lsOverlayed in ritem.States
                    then ClipDrawOverlays(x + 2*u, y - u, ri, rj, x, x + 4*u, y - u - clipdelta, y + u);
                  if not (lsDoubleOverlayed in item.States) and not (lsDoubleOverlayed in upitem.States) and not (lsDoubleOverlayed in ritem.States)
                    then DrawOverlays(x, y, i, j, item.Size);
                end;
          end;

        begin
          OnTop  := (y <= ClipRect.Top);
          OnLeft := true;
          while (x < ClipRect.Right) and InsideRightEdge(i, j) do
            begin
              if fMap.GetItemInfo(i, j, focus, item)
                then
                  with item do
                    begin
                      RotateItem(item, rotation);
                      if (c = 0) and (r = pred(Size))
                        then DrawItem(x, y, i, j)
                        else
                          if (OnLeft and (r + c = pred(Size))) or (OnTop and (c = 0)) or (OnTop and OnLeft)
                            then
                              begin
                                offsetcoords(view, i, j, drawi, drawj, pred(Size - r), -c);
                                DrawItem(x - 2*u*(Size - r + c - 1), y - u*(Size - r - c - 1), drawi, drawj); // >> ojo, la (i,j) no est'an bien aqu'i para DrawLandBase
                              end;
                      if r < pred(Size - c)
                        then k := r
                        else k := pred(Size - c);
                    end
                else k := 0;
              while k >= 0 do
                begin
                  inccol(view, i, j, i, j);
                  decrow(view, i, j, i, j);
                  inc(x, 4*u);
                  dec(k);
                end;
              OnLeft := false;
            end; // while
        end;

      begin
        while (y < y2) and InsideBottomEdge(i, j) do
          begin
            DrawRank(x, i, j);
            if (x + 4*u < ClipRect.Left) or OnLeftEdge(i, j)
              then
                begin
                  decrow(view, i, j, i, j);
                  inc(x, 2*u);
                end
              else
                begin
                  deccol(view, i, j, i, j);
                  dec(x, 2*u);
                end;
            inc(y, u);
          end;
      end;

    procedure DrawTexts;

      procedure DoDrawText(x, y : integer; which : string);
        const
          ScanningText = 'Scanning...';
        const
          ExtraWidth = 100;
        var
          Rect    : TRect;
          MidRect : TRect;
          i, j    : integer;
          org     : TPoint;
        begin
          if which = ''
            then which := ScanningText;
          snap.Canvas.Font.Name := 'Verdana';
          snap.Canvas.Font.Style := [fsBold];
          snap.Canvas.Font.Size := 7;
          snap.Canvas.Font.Color  := clBlack;
          snap.Canvas.Brush.Style := bsClear;
          Rect := Classes.Rect(0, 0, 200, 1000);
          DrawText(snap.Canvas.Handle, pchar(which), length(which), Rect, DT_CALCRECT or DT_CENTER or DT_WORDBREAK or DT_EXPANDTABS);
          Rect := Classes.Rect(x - ExtraWidth, y - Rect.Bottom, x + ExtraWidth, y); // ***
          MidRect := Rect;
          for i := -1 to 1 do
            for j := -1 to 1 do
              if ((i = j) or (i = -j)) and (i <> 0)
                then
                  begin
                    Rect := MidRect;
                    OffsetRect(Rect, i, j);
                    DrawText(snap.Canvas.Handle, pchar(which), length(which), Rect, DT_CENTER or DT_WORDBREAK or DT_EXPANDTABS);
                  end;
          snap.Canvas.Font.Color := clYellow;
          DrawText(snap.Canvas.Handle, pchar(which), length(which), MidRect, DT_CENTER or DT_WORDBREAK or DT_EXPANDTABS);
          if which = focus.GetText(fkSelection)
            then
              begin
                org := view.Origin;
                OffsetRect(Rect, org.x, org.y);
                focus.SetRect(Rect);
              end;
        end;

      var
        k            : integer;
        textrendinfo : TTextRenderingInfo;
      begin
        for k := 0 to pred(fTextsToRenderCount) do
          begin
            textrendinfo := fTextsToRender[k];
            DoDrawText(textrendinfo.x, textrendinfo.y, textrendinfo.text);
          end;
        fTextsToRenderCount := 0;
      end;

    procedure DrawFocusObject;
      var
        obj : TOffsetedObjInfo;
      begin
        if fMap.GetFocusObjectInfo(focus, ClipRect, obj)
          then DrawSimpleObject(obj.x, obj.y, obj);
      end;

    procedure DrawGrid;
      var
        x, y : integer;
        i, j : integer;
        y1   : integer;
        u    : integer;

      procedure DrawRank(x, i, j : integer);
        begin
          while (x < ClipRect.Right) and InsideRightEdge(i, j) do
            begin
              with snap.Canvas do
                begin
                  MoveTo(x, y);
                  LineTo(x + 2*u, y + u);
                  LineTo(x + 4*u, y);
                  LineTo(x + 2*u, y - u);
                  LineTo(x, y);
                end;
              inccol(view, i, j, i, j);
              decrow(view, i, j, i, j);
              inc(x, 4*u);
            end; // while
        end;

      begin
        u := 2 shl view.ZoomLevel;
        x := ClipRect.Left;
        y := ClipRect.Top;
        CalculateFirstGrid(view, x, y, i, j);
        y1 := ClipRect.Bottom + u;
        snap.Canvas.Pen.Color := clNavy;
        while (y < y1) and InsideBottomEdge(i, j) do
          begin
            DrawRank(x, i, j);
            if (x + 2*u <= ClipRect.Left) or OnLeftEdge(i, j)
              then
                begin
                  decrow(view, i, j, i, j);
                  inc(x, 2*u);
                end
              else
                begin
                  deccol(view, i, j, i, j);
                  dec(x, 2*u);
                end;
            inc(y, u);
          end;
      end;

    begin
      focus  := view.GetFocus;
      cols := fMap.GetColumns;
      rows := fMap.GetRows;
      Imager := fMap.GetImager(focus);
      zoom := TZoomRes(view.ZoomLevel);
      rotation := view.Rotation;
      u := 2 shl view.ZoomLevel;
      x := ClipRect.Left - 2*u;
      y := ClipRect.Top;
      CalculateFirstGrid(view, x, y, i, j);
      y1 := ClipRect.Bottom + cMaxLandHeight*u;
      y2 := y1 + cMaxBuildingHeight*u;
      snap.Canvas.Brush.Color := clBlack;
      snap.Canvas.Brush.Style := bsSolid;
      snap.Canvas.FillRect(ClipRect);
      DrawLandLayers(x, y, i, j);
      DrawFocusObject;
      DrawTexts;
      if false // >>>> this comment the next statement out
        then DrawGrid;
      // >>>> Draw mouse hint
      if (fLastZoom <> zoom) or (fLastRotation <> rotation)
        then
          begin
            fSpareCache[fLastZoom, fLastRotation] := nil;
            fDownloadSign[fLastZoom, fLastRotation] := nil;
            fShadeCache[fLastZoom, fLastRotation] := nil;
            fRedShadeCache[fLastZoom, fLastRotation] := nil;
            fBlackShadeCache[fLastZoom, fLastRotation] := nil;
            fCnxSourceDownCache[fLastZoom, fLastRotation] := nil;
            fCnxSourceTopCache[fLastZoom, fLastRotation] := nil;
            fCnxDestDownCache[fLastZoom, fLastRotation] := nil;
            fCnxDestTopCache[fLastZoom, fLastRotation] := nil;
            fLastZoom := zoom;
            fLastRotation := rotation;
          end;
    end;

  function TLander.ClipMovement(const view : IGameView; var dx, dy : integer) : boolean;
    const
      cMargin = 14;
    var
      i, j : integer;
      x, y : integer;
      rows : integer;
      cols : integer;

    function ClipRow : boolean;
      begin
        case view.Rotation of
          drNorth:
            Result := ClipInteger(i, cMargin, rows + cMargin);
          drEast:
            Result := ClipInteger(j, cMargin, cols + cMargin);
          drSouth:
            Result := ClipInteger(i, -cMargin, rows - cMargin);
          else // drWest
            Result := ClipInteger(j, -cMargin, cols - cMargin);
        end;
      end;

    function ClipCol : boolean;
      begin
        case view.Rotation of
          drNorth:
            Result := ClipInteger(j, 0, cols);
          drEast:
            Result := ClipInteger(i, 0, rows);
          drSouth:
            Result := ClipInteger(j, 0, cols);
          else // drWest
            Result := ClipInteger(i, 0, rows);
        end;
      end;

    begin
      rows := fMap.GetRows;
      cols := fMap.GetColumns;
      x := -dx;
      y := -dy;
      ScreenToMap(view, x, y, i, j);
      {$B+} Result := ClipRow or ClipCol; {$B-}
      if Result
        then
          begin
            MapToScreen(view, i, j, x, y);
            dx := -x;
            dy := -y;
          end;
    end;

  function TLander.CreateFocus(const view : IGameView) : IGameFocus;
    begin
      Result := fMap.CreateFocus(view);
    end;

  procedure TLander.SetImageSuit( ImageSuit : integer );
    begin
      fMap.SetImageSuit( ImageSuit );
    end;

  function TLander.ObjectToMap(const view : IGameView; x, y : integer; out i, j : integer; checkbase : boolean) : boolean;
    var
      xx, yy : integer;
      u      : integer;
      rows   : integer;
      cols   : integer;
      rinc   : integer;
      cinc   : integer;

    function OnLeftEdge : boolean;
      begin
        case view.rotation of
          drNorth:
            Result := j = 0;
          drEast:
            Result := i = pred(rows);
          drSouth:
            Result := j = pred(cols);
          else // drWest
            Result := i = 0;
        end;
      end;

    function InsideMap : boolean;
      begin
        case view.rotation of
          drNorth:
            Result := (i < rows) and (j < cols);
          drEast:
            Result := (i >= 0) and (j < cols);
          drSouth:
            Result := (i >= 0) and (j >= 0);
          else // drWest
            Result := (i < rows) and (j >= 0);
        end;
      end;

    function BelowLeft(out by : integer) : boolean;
      begin
        case view.Rotation of
          drNorth:
            by := -j;
          drEast:
            by := i - pred(rows);
          drSouth:
            by := j - pred(cols);
          else // drWest
            by := -i;
        end;
        Result := by > 0;
      end;

    function BelowBottom(out by : integer) : boolean;
      begin
        case view.Rotation of
          drNorth:
            by := -i;
          drEast:
            by := -j;
          drSouth:
            by := i - pred(rows);
          else // drWest
            by := j - pred(cols);
        end;
        Result := by > 0;
      end;

    procedure RowLowerBound(var i, j : integer);
      begin
        case view.Rotation of
          drNorth:
            i := 0;
          drEast:
            j := 0;
          drSouth:
            i := pred(rows);
          else // drWest
            j := pred(cols);
        end;
      end;

    procedure ColLowerBound(var i, j : integer);
      begin
        case view.Rotation of
          drNorth:
            j := 0;
          drEast:
            i := pred(rows);
          drSouth:
            j := pred(cols);
          else // drWest
            i := 0;
        end;
      end;

    begin
      rows := fMap.GetRows;
      cols := fMap.GetColumns;
      u := 2 shl view.ZoomLevel;
      ScreenToMap(view, x, y + cMaxBuildingHeight*u, i, j);
      MapToScreen(view, i, j, xx, yy);
      if BelowBottom(cinc)
        then
          begin
            offsetcoords(view, i, j, i, j, 0, cinc);
            dec(yy, 2*u*cinc);
            RowLowerBound(i, j);
          end;
      if BelowLeft(rinc)
        then
          begin
            offsetcoords(view, i, j, i, j, rinc, 0);
            dec(yy, 2*u*rinc);
            ColLowerBound(i, j);
          end;
      while InsideMap and not CheckPoint(view, x, y, i, j) do
        begin
          if (xx + 2*u < x) or OnLeftEdge
            then
              begin
                inc(xx, 2*u);
                inccol(view, i, j, i, j);
              end
            else
              begin
                dec(xx, 2*u);
                incrow(view, i, j, i, j);
              end;
          dec(yy, u);
        end;
      if Debugging
        then LogThis(Format('i= %d, j= %d', [i, j]));
      Result := InsideMap;
      if not Result and checkbase
        then Result := CheckBuildingBase(view, x, y, i, j);
    end;

  function TLander.ScreenToMap(const view : IGameView; x, y : integer; out i, j : integer) : boolean;
    var
      org  : TPoint;
      aux  : integer;
      u, h : integer;
      tu   : integer;
      rows : integer;
      cols : integer;
    begin
      rows := fMap.GetRows;
      cols := fMap.GetColumns;
      if (view.Rotation = drEast) or (view.Rotation = drWest)
        then
          begin
            aux := rows;
            rows := cols;
            cols := aux;
          end;
      org := view.Origin;
      inc(x, org.x);
      inc(y, org.y);
      u   := 2 shl view.ZoomLevel;
      tu  := u + u;
      inc(tu, tu);
      aux := 2*(u*cols - y);
      h   := aux + tu*succ(rows) - x;
      if h >= 0
        then i := h div tu
        else i := (h - tu) div tu;
      h := aux + x;
      if h >= 0
        then j := h div tu
        else j := (h - tu) div tu;
      case view.Rotation of
        drNorth: ;
        drEast:
          begin
            aux := i;
            i := pred(cols - j);
            j := aux;
          end;
        drSouth:
          begin
            i := pred(rows - i);
            j := pred(cols - j);
          end;
        drWest:
          begin
            aux := i;
            i := j;
            j := pred(rows - aux);
          end;
      end;
      Result := (i >= 0) and (i < rows) and (j >= 0) and (j < cols);
    end;

  function TLander.MapToScreen(const view : IGameView; i, j : integer; out x, y : integer) : boolean;
    var
      org  : TPoint;
      u    : integer;
      rows : integer;
      cols : integer;
      aux  : integer;
    begin                                                    
      rows := fMap.GetRows;
      cols := fMap.GetColumns;
      case view.Rotation of
        drNorth: ;
        drEast:
          begin
            aux := rows;
            rows := cols;
            cols := aux;
            aux := i;
            i := j;
            j := pred(cols - aux);
          end;
        drSouth:
          begin
            i := pred(rows - i);
            j := pred(cols - j);
          end;
        drWest:
          begin
            aux := rows;
            rows := cols;
            cols := aux;
            aux := i;
            i := pred(rows - j);
            j := aux;
          end;
      end;
      org  := view.Origin;
      u    := 2 shl view.ZoomLevel;
      x    := 2*u*(rows - i + j);
      y    := u*((rows - i) + (cols - j));
      dec(x, org.x);
      dec(y, org.y);
      Result := (i >= 0) and (i < rows) and (j >= 0) and (j < cols);
    end;

  procedure TLander.CalculateFirstGrid(const view : IGameView; var x, y : integer; out i, j : integer);
    var
      xb, yb : integer;
      u      : integer;
      rows   : integer;
      cols   : integer;
      rinc   : integer;
      cinc   : integer;

    function AboveTop(out by : integer) : boolean;
      begin
        case view.Rotation of
          drNorth:
            by := i - pred(rows);
          drEast:
            by := j - pred(cols);
          drSouth:
            by := -i;
          else // drWest
            by := -j;
        end;
        Result := by > 0;
      end;

    function BelowLeft(out by : integer) : boolean;
      begin
        case view.Rotation of
          drNorth:
            by := -j;
          drEast:
            by := i - pred(rows);
          drSouth:
            by := j - pred(cols);
          else // drWest
            by := -i;
        end;
        Result := by > 0;
      end;

    procedure RowUpperBound(var i, j : integer);
      begin
        case view.Rotation of
          drNorth:
            i := pred(rows);
          drEast:
            j := pred(cols);
          drSouth:
            i := 0;
          else // drWest
            j := 0;
        end;
      end;

    procedure ColLowerBound(var i, j : integer);
      begin
        case view.Rotation of
          drNorth:
            j := 0;
          drEast:
            i := pred(rows);
          drSouth:
            j := pred(cols);
          else // drWest
            i := 0;
        end;
      end;

    begin
      rows := fMap.GetRows;
      cols := fMap.GetColumns;
      u  := 2 shl view.ZoomLevel;
      xb := x;
      yb := y;
      ScreenToMap(view, x, y, i, j);
      MapToScreen(view, i, j, x, y);
      while (y > yb) or (x > xb) do
        begin
          dec(x, 2*u);
          dec(y, u);
          incrow(view, i, j, i, j);
        end;
      if AboveTop(cinc)
        then
          begin
            offsetcoords(view, i, j, i, j, 0, cinc);
            inc(x, 4*u*cinc);
            RowUpperBound(i, j);
          end
        else
          if BelowLeft(rinc)
            then
              begin
                offsetcoords(view, i, j, i, j, -rinc, 0);
                inc(x, 4*u*rinc);
                ColLowerBound(i, j);
              end;
    end;

  procedure TLander.AddTextToRender(const texttorender : TTextRenderingInfo);
    const
      cTextsToRenderDelta = 1;
    begin
      if fTextsToRenderCount = fTextsToRenderAlloc
        then
          begin
            inc(fTextsToRenderAlloc, cTextsToRenderDelta);
            reallocmem(fTextsToRender, fTextsToRenderAlloc*sizeof(fTextsToRender[0]));
            initialize(fTextsToRender[fTextsToRenderCount], fTextsToRenderAlloc - fTextsToRenderCount);
          end;
      fTextsToRender[fTextsToRenderCount] := texttorender;
      inc(fTextsToRenderCount);
    end;

  function TLander.CheckPoint(const view : IGameView; x, y, i, j : integer) : boolean;
    var
      xx, yy : integer;
      item   : TItemInfo;
      Imager : IImager;
      img    : TGameImage;
      u      : integer;
      focus  : IGameFocus;  // >>>> nil
    begin
      if fMap.GetItemInfo(i, j, focus, item) and (lsFocusable in item.States)
        then
          begin
            RotateItem(item, view.Rotation);
            with item do
              begin
                Imager := fMap.GetImager(view.GetFocus);
                img := Imager.GetObjectImage(id, angle);
                if img <> nil
                  then
                    begin
                      u := 2 shl view.ZoomLevel;
                      offsetcoords(view, i, j, i, j, pred(Size), 0);
                      offsetcoords(view, i, j, i, j, -r, -c);
                      MapToScreen(view, i, j, xx, yy);
                      dec(x, xx);
                      dec(y, yy + succ(u*Size - img.Height));
                      Result := (x >= 0) and (y >= 0) and (x < img.Width) and (y < img.Height) and (img.Pixel[x, y, item.Frame] <> img.TranspIndx);
                    end
                  else Result := false;
              end
          end
        else Result := false;
    end;

  function TLander.CheckBuildingBase(const view : IGameView; x, y : integer; out i, j : integer) : boolean;
    var
      item : TItemInfo;
    begin
      Result := ScreenToMap(view, x, y, i, j) and fMap.GetItemInfo(i, j, view.GetFocus, item) and (lsFocusable in item.States);
    end;

end.
