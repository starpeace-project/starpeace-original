unit Lander;

interface

uses
  Windows, Classes, Graphics, GameTypes, LanderTypes;


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
        procedure ViewChanged(const view : IGameView);
      private // ICoordinateConverter
        function ObjectToMap(const view : IGameView; x, y : integer; out i, j : integer) : boolean;
        function ScreenToMap(const view : IGameView; x, y : integer; out i, j : integer) : boolean;
        function MapToScreen(const view : IGameView; i, j : integer; out x, y : integer) : boolean;
      private
        fMap : IWorldMap;
        procedure CalculateFirstGrid(const view : IGameView; var x, y : integer; out i, j : integer);
      private
        fSpareCache      : TGameImage;
        fShadeCache      : TGameImage;
        fRedShadeCache   : TGameImage;
        fBlackShadeCache : TGameImage;
        function  CheckPoint(const view : IGameView; x, y, i, j : integer) : boolean;
        procedure DrawGrid(const view : IGameView; const ClipRect : TRect; snap : TCanvasImage);
    end;


implementation


uses
  SysUtils, AxlDebug, MathUtils, GDI, ColorTableMgr, NumUtils;

const
  cMaxBuildingHeight = 16;
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

// TLander

constructor TLander.Create(const Map : IWorldMap);
  begin
    inherited Create;
    fMap := Map;
  end;

destructor TLander.Destroy;
  begin
    assert(RefCount = 0);
    inherited;
  end;

const
  lightx = -1;
  lighty = -1;
  lightz = 1;

procedure TLander.RenderSnapshot(const view : IGameView; const ClipRect : TRect; snap : TCanvasImage);
  var
    u      : integer;
    focus  : IGameFocus;
    Imager : IImager;

  procedure DrawLand;
    var
      obj        : TObjInfo;
      img        : TGameImage;
      i, j       : integer;
      ox, oy     : integer;
      pixel      : integer;
      pixheight  : integer;
      pixrepcnt  : integer;
      x, y       : integer;
      scy        : integer;
      lumN, lumD : integer;
      rgb        : word;
      rgbQuad    : TRGBQuad;

    function CalcPixelHeight(i, j, ox, oy : integer; out lumN, lumD : integer) : integer;
      var
        neighbobj     : TObjInfo;
        x1, x2, x3    : integer;
        y1, y2, y3    : integer;
        z1, z2, z3    : integer;
        nx, ny, nz    : integer; // normal vector
        v1x, v1y, v1z : integer; // triangle-side vectors
        v2x, v2y, v2z : integer;
        d             : integer;  // intersect
        nmod          : single;
      begin
        x1 := 2*u;
        y1 := 0;
        if fMap.GetGroundInfo(i + 1, j, focus, neighbobj)
          then z1 := neighbobj.Height
          else z1 := 0;
        x2 := 2*u;
        y2 := 2*u;
        if fMap.GetGroundInfo(i, j - 1, focus, neighbobj)
          then z2 := neighbobj.Height
          else z2 := 0;
        if ox < 2*u
          then
            begin
              x3 := 0;
              y3 := u;
              if fMap.GetGroundInfo(i + 1, j - 1, focus, neighbobj)
                then z3 := neighbobj.Height
                else z3 := 0;
            end
          else
            begin
              x3 := 4*u;
              y3 := u;
              z3 := obj.Height;
            end;
        v1x := x1 - x3;
        v1y := y1 - y3;
        v1z := z1 - z3;
        v2x := x2 - x3;
        v2y := y2 - y3;
        v2z := z2 - z3;
        nx := v1y*v2z - v1z*v2y;
        ny := v1z*v2x - v1x*v2z;
        nz := v1x*v2y - v1y*v2x;
        d := -nx*x1 - ny*y1 - nz*z1;
        Result := (-d - nx*ox - ny*oy) div nz;
        lumN := abs(lightx*nx + lighty*ny + lightz*nz);
        lumD := abs(nx) + abs(ny) + abs(nz);
      end;

    function PackRGBQuad(const RGBQuad : TRGBQuad) : word;
      const
        rMask = $7C00;
        gMask = $03E0;
        bMask = $001F;
      var
        rRight : byte;
        gRight : byte;
        bRight : byte;
        rLeft  : byte;
        gLeft  : byte;
        bLeft  : byte;
      begin
        rRight := RightShiftCount( rMask );
        gRight := RightShiftCount( gMask );
        bRight := RightShiftCount( bMask );

        rLeft := LeftShiftCount( rMask );
        gLeft := LeftShiftCount( gMask );
        bLeft := LeftShiftCount( bMask );
        with RGBQuad do
          Result := ( (rgbRed   shr rLeft) shl rRight ) or
                    ( (rgbGreen shr gLeft) shl gRight ) or
                    ( (rgbBlue  shr bLeft) shl bRight );
      end;

    begin
      x := ClipRect.Left;
      y := ClipRect.Bottom + cMaxLandHeight*u;
      scy := y;
      while x <= ClipRect.Right do
        begin
          while scy >= ClipRect.Top do
            begin
              ScreenToMap(view, x, y, i, j);
              MapToScreen(view, i, j, ox, oy);
              ox := x - ox;
              oy := y - (oy - u);
              if (oy >= 0) and (ox >= 0) and fMap.GetGroundInfo(i, j, focus, Obj)
                then
                  begin
                    img := Imager.GetLandImage(obj.id);
                    if img <> nil
                      then
                        begin
//                          img.PaletteInfo.RequiredState([tsHiColorTableValid]);
                          repeat
                            pixel := img.Pixel[ox, oy, obj.frame];
                            pixheight := CalcPixelHeight(i, j, ox, oy , lumN, lumD);
                            rgbQuad := img.PaletteInfo.RGBPalette[pixel];
                            rgbQuad.rgbRed := (lumN*rgbQuad.rgbRed) div lumD;
                            rgbQuad.rgbGreen := (lumN*rgbQuad.rgbGreen) div lumD;
                            rgbQuad.rgbBlue := (lumN*rgbQuad.rgbBlue) div lumD;
                            rgb := PackRGBQuad(rgbQuad);
                            pixrepcnt := pixheight - (y - scy);
                            if pixrepcnt > 0
                              then
                                if (pixel <> img.TransparentIndx) and (x < snap.Width) and (scy < snap.Height)
                                  then
                                    begin
                                      repeat
                                        if scy >= 0
                                          then pword(snap.PixelAddr[x, scy])^ := rgb{img.PaletteInfo.HiColorTable[pixel]};
                                        dec(scy);
                                        dec(pixrepcnt);
                                      until pixrepcnt = 0;
                                      dec(oy);
                                      dec(y);
                                    end
                                  else
                                    begin
                                      dec(scy);
                                      dec(oy);
                                      dec(y);
                                      dec(pixrepcnt);
                                    end
                              else
                                begin
                                  dec(oy);
                                  dec(y);
                                end;
                          until (Pixel = img.TransparentIndx) or (oy < 0) or (scy <= ClipRect.Top);
                        end;
                  end
                else
                  begin
                    dec(y);
                    dec(scy);
                  end;
            end;
          y := ClipRect.Bottom + cMaxLandHeight*u;
          scy := y;
          inc(x);
        end;
    end;

  begin
    focus := view.GetFocus;
    Imager := fMap.GetImager(focus);
    u := 2 shl view.ZoomLevel;
    snap.Canvas.Brush.Color := clBlack;
    snap.Canvas.Brush.Style := bsSolid;
    snap.Canvas.FillRect(ClipRect);
    DrawLand;
    if false // >>>> this comment the next statement out
      then DrawGrid(view, ClipRect, snap);
    // >>>> Draw mouse hint
  end;

function TLander.ClipMovement(const view : IGameView; var dx, dy : integer) : boolean;
  const
    cMargin = 14;
  var
    i, j : integer;
    x, y : integer;
    rows : integer;
    cols : integer;
  begin
    rows := fMap.GetRows;
    cols := fMap.GetColumns;
    x := -dx;
    y := -dy;
    ScreenToMap(view, x, y, i, j);
    {$B+} Result := ClipInteger(i, cMargin, rows + cMargin) or ClipInteger(j, 0, cols); {$B-}
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

procedure TLander.ViewChanged(const view : IGameView);
  begin
    fSpareCache := nil;
    fShadeCache := nil;
    fRedShadeCache := nil;
    fBlackShadeCache := nil;
  end;

function TLander.ObjectToMap(const view : IGameView; x, y : integer; out i, j : integer) : boolean;
  var
    xx, yy : integer;
    u      : integer;
    rows   : integer;
    cols   : integer;
  begin
    rows := fMap.GetRows;
    cols := fMap.GetColumns;
    u := 2 shl view.ZoomLevel;
    ScreenToMap(view, x, y + cMaxBuildingHeight*u, i, j);
    MapToScreen(view, i, j, xx, yy);
    if i < 0
      then
        begin
          inc(j, -i);
          dec(yy, -2*u*i);
          i := 0;
        end;
    if j < 0
      then
        begin
          inc(i, -j);
          dec(yy, -2*u*j);
          j := 0;
        end;
    while (i < rows) and (j < cols) and not CheckPoint(view, x, y, i, j) do
      begin
        if (xx + 2*u < x) or (j = 0)
          then
            begin
              inc(xx, 2*u);
              inc(j);
            end
          else
            begin
              dec(xx, 2*u);
              inc(i);
            end;
        dec(yy, u);
      end;
    if Debugging
      then LogThis(Format('i= %d, j= %d', [i, j]));
    Result := (i < rows) and (j < cols);
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
    org  := view.Origin;
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
    Result := (i >= 0) and (i < rows) and (j >= 0) and (j < cols);
  end;

function TLander.MapToScreen(const view : IGameView; i, j : integer; out x, y : integer) : boolean;
  var
    org  : TPoint;
    u    : integer;
    rows : integer;
    cols : integer;
  begin
    rows := fMap.GetRows;
    cols := fMap.GetColumns;
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
  begin
    rows := fMap.GetRows;
    u  := 2 shl view.ZoomLevel;
    xb := x;
    yb := y;
    ScreenToMap(view, x, y, i, j);
    MapToScreen(view, i, j, x, y);
    while (y > yb) or (x > xb) do
      begin
        dec(x, 2*u);
        dec(y, u);
        inc(i);
      end;
    if i >= rows
      then
        begin
          inc(j, succ(i - rows));
          inc(x, 4*u*succ(i - rows));
          i := pred(rows);
        end
      else
        if j < 0
          then
            begin
              inc(i, j);
              inc(x, -4*u*j);
              j := 0;
            end;
  end;

function TLander.CheckPoint(const view : IGameView; x, y, i, j : integer) : boolean;
  begin
    Result := false;
  end;

procedure TLander.DrawGrid(const view : IGameView; const ClipRect : TRect; snap : TCanvasImage);
  var
    x, y : integer;
    i, j : integer;
    y1   : integer;
    u    : integer;
    cols : integer;

  procedure DrawRank(x, i, j : integer);
    begin
      while (x < ClipRect.Right) and (j < cols) and (i >= 0) do
        begin
          with snap.Canvas do
            begin
              MoveTo(x, y);
              LineTo(x + 2*u, y + u);
              LineTo(x + 4*u, y);
              LineTo(x + 2*u, y - u);
              LineTo(x, y);
            end;
          inc(j);
          dec(i);
          inc(x, 4*u);
        end; // while
    end;

  begin
    cols := fMap.GetColumns;
    u := 2 shl view.ZoomLevel;
    x := ClipRect.Left;
    y := ClipRect.Top;
    CalculateFirstGrid(view, x, y, i, j);
    y1 := ClipRect.Bottom + u;
    snap.Canvas.Pen.Color := clNavy;
    while (y < y1) and (i >= 0) do
      begin
        DrawRank(x, i, j);
        if (x + 2*u <= ClipRect.Left) or (j = 0)
          then
            begin
              dec(i);
              inc(x, 2*u);
            end
          else
            begin
              dec(j);
              dec(x, 2*u);
            end;
        inc(y, u);
      end;
  end;

end.
