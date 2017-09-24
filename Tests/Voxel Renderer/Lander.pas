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
  SysUtils, AxlDebug, MathUtils, GDI, ColorTableMgr, NumUtils, ColorSpaces;

const
  cMaxBuildingHeight = 16;
  cMaxLandHeight     = 3;

const
  cAlpha = 1;

const
  cMaxScreenHeight = 1200;

type
  TLandRenderInfo =
    record
      hiclrpix : word;
      repcnt   : integer;
    end;

type
  TScanLineRenderInfo = array [0 .. pred(cMaxScreenHeight)] of TLandRenderInfo;

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

procedure TLander.RenderSnapshot(const view : IGameView; const ClipRect : TRect; snap : TCanvasImage);
  var
    u      : integer;
    focus  : IGameFocus;
    Imager : IImager;

  procedure DrawLand;
    var
      ground           : TObjInfo;
      img              : TGameImage;
      i, j             : integer;
      ox, oy           : integer;
      pixel            : integer;
      pixheight        : integer;
      pixrepcnt        : integer;
      x, y             : integer;
      scy              : integer;
      lum              : single;
      rgb              : word;
      rgbQuad          : TRGBQuad;
      egroundinfo      : TExtraLandInfo;
      scanlinerendinfo : TScanLineRenderInfo;

    function CalcPixelHeight(ox, oy : integer) : integer;
      begin
        with egroundinfo.heightinfo do
          if ox < 2*u
            then Result := (-ld - lnx*ox - lny*oy) div lnz
            else Result := (-rd - rnx*ox - rny*oy) div rnz;
      end;

    function CalcPixelLuminance(ox, oy : integer) : single;
      begin
        with egroundinfo.luminfo do
          if ox < 2*u
            then Result := abs((-llumd - lluma*ox - llumb*oy)/llumc)
            else Result := abs((-rlumd - rluma*ox - rlumb*oy)/rlumc);
      end;

    function PackRGBQuad(const RGBQuad : TRGBQuad) : word;
      const
        rRight = 10;
        gRight = 5;
        rLeft  = 3;
        gLeft  = 3;
        bLeft  = 3;
      begin
        with RGBQuad do
          Result := ( (rgbRed   shr rLeft) shl rRight ) or
                    ( (rgbGreen shr gLeft) shl gRight ) or
                    ( rgbBlue shr bLeft );
      end;

    begin
      x := ClipRect.Left;
      y := ClipRect.Bottom + cMaxBuildingHeight*u;
      scy := y;
      fillchar(scanlinerendinfo, sizeof(scanlinerendinfo), 0);
      while x <= ClipRect.Right do
        begin
          while scy >= ClipRect.Top do
            begin
              ScreenToMap(view, x, y, i, j);
              MapToScreen(view, i, j, ox, oy);
              ox := x - ox;
              oy := y - (oy - u);
              if (oy >= 0) and (ox >= 0) and fMap.GetGroundInfo(i, j, focus, ground)
                then // now ground information should be collected
                  begin
                    fMap.GetExtraLandInfo(i, j, egroundinfo);
                    img := Imager.GetLandImage(ground.id);
                    if img <> nil
                      then
                        repeat
                          pixel := img.Pixel[ox, oy, ground.frame];
                          pixheight := CalcPixelHeight(ox, oy);
                          pixrepcnt := pixheight - (y - scy);
                          scanlinerendinfo[y].repcnt := pixrepcnt;
                          if pixrepcnt > 0
                            then
                              if pixel <> img.TransparentIndx[0]
                                then
                                  begin
                                    lum := CalcPixelLuminance(ox, oy);
                                    rgbQuad := img.PaletteInfo.RGBPalette[pixel];
                                    //ChangeLightOf(rgbQuad, round((0.25 + lum/2)*200));
                                    ScaleLightOf(rgbQuad, lum);
                                    rgb := PackRGBQuad(rgbQuad);
                                    if y >= 0
                                      then scanlinerendinfo[y].hiclrpix := rgb;
                                    dec(scy, pixrepcnt);
                                    dec(oy);
                                    dec(y);
                                  end
                                else
                                  begin
                                    scanlinerendinfo[y].hiclrpix := rgb;
                                    dec(scy, pixrepcnt);
                                    dec(oy);
                                    dec(y);
                                  end
                            else
                              begin
                                dec(oy);
                                dec(y);
                              end;
                        until (Pixel = img.TransparentIndx[0]) or (oy < 0) or (scy <= ClipRect.Top);
                  end
                else dec(y);
            end;
          inc(y);
          while scy <= ClipRect.Bottom do
            begin
              pixrepcnt := scanlinerendinfo[y].repcnt;
              if pixrepcnt > 0
                then
                  repeat
                    if (x < snap.Width) and (scy >= 0) and (scy < snap.Height)
                      then pword(snap.PixelAddr[x, scy])^ := scanlinerendinfo[y].hiclrpix;
                    dec(pixrepcnt);
                    inc(scy);
                  until pixrepcnt = 0;
              inc(y);
            end;
          y := ClipRect.Bottom + cMaxBuildingHeight*u;
          scy := y;
          inc(x);
        end;
    end;

  begin
    focus := view.GetFocus;
    Imager := fMap.GetImager(focus);
    u := 2 shl view.ZoomLevel;
    snap.Canvas.Brush.Color := clGray;
    snap.Canvas.Brush.Style := bsSolid;
    snap.Canvas.FillRect(ClipRect);
    DrawLand;
    if false // >>>> this comment the next statement out
      then DrawGrid(view, ClipRect, snap);
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

(*

  procedure DrawLand;
    var
      ground           : TObjInfo;
      img              : TGameImage;
      i, j             : integer;
      ox, oy           : integer;
      pixel            : integer;
      pixheight        : integer;
      pixrepcnt        : integer;
      x, y             : integer;
      scy              : integer;
      lum              : single;
      rgb              : word;
      rgbQuad          : TRGBQuad;
      egroundinfo      : TExtraLandInfo;

    function CalcPixelHeight(ox, oy : integer) : integer;
      begin
        with egroundinfo.heightinfo do
          if ox < 2*u
            then Result := (-ld - lnx*ox - lny*oy) div lnz
            else Result := (-rd - rnx*ox - rny*oy) div rnz;
      end;

    function CalcPixelLuminance(ox, oy : integer) : single;
      begin
        with egroundinfo.luminfo do
          if ox < 2*u
            then Result := abs((-llumd - lluma*ox - llumb*oy)/llumc)
            else Result := abs((-rlumd - rluma*ox - rlumb*oy)/rlumc);
      end;

    function PackRGBQuad(const RGBQuad : TRGBQuad) : word;
      const
        rRight = 10;
        gRight = 5;
        rLeft  = 3;
        gLeft  = 3;
        bLeft  = 3;
      begin
        with RGBQuad do
          Result := ( (rgbRed   shr rLeft) shl rRight ) or
                    ( (rgbGreen shr gLeft) shl gRight ) or
                    ( rgbBlue shr bLeft );
      end;

    begin
      x := ClipRect.Left;
      y := ClipRect.Bottom + cMaxBuildingHeight*u;
      scy := y;
      while x <= ClipRect.Right do
        begin
          while scy >= ClipRect.Top do
            begin
              ScreenToMap(view, x, y, i, j);
              MapToScreen(view, i, j, ox, oy);
              ox := x - ox;
              oy := y - (oy - u);
              if (oy >= 0) and (ox >= 0) and fMap.GetGroundInfo(i, j, focus, ground)
                then
                  begin
                    fMap.GetExtraLandInfo(i, j, egroundinfo);
                    img := Imager.GetLandImage(ground.id);
                    if img <> nil
                      then
                        repeat
                          pixel := img.Pixel[ox, oy, ground.frame];
                          pixheight := CalcPixelHeight(ox, oy);
                          {
                          if lum < 0.5
                            then
                              begin
                                rgbQuad.rgbRed := round((lum)*rgbQuad.rgbRed);
                                rgbQuad.rgbGreen := round((lum)*rgbQuad.rgbGreen);
                                rgbQuad.rgbBlue := round((lum)*rgbQuad.rgbBlue);
                              end
                            else
                              if lum > 0.5
                                then
                                  begin
                                    rgbQuad.rgbRed := round(2*($FF - rgbQuad.rgbRed)*lum + 2*rgbQuad.rgbRed - $FF);
                                    rgbQuad.rgbGreen := round(2*($FF - rgbQuad.rgbGreen)*lum + 2*rgbQuad.rgbGreen - $FF);
                                    rgbQuad.rgbBlue := round(2*($FF - rgbQuad.rgbBlue)*lum + 2*rgbQuad.rgbBlue - $FF);
                                  end;
                                }
                          //ChangeLightOf(rgbQuad, round((0.25 + lum/2)*200));
                          pixrepcnt := pixheight - (y - scy);
                          if pixrepcnt > 0
                            then
                              if (pixel <> img.TransparentIndx) and (x < snap.Width) and (scy < snap.Height)
                                then
                                  begin
                                    lum := CalcPixelLuminance(ox, oy);
                                    rgbQuad := img.PaletteInfo.RGBPalette[pixel];
                                    ScaleLightOf(rgbQuad, lum);
                                    rgb := PackRGBQuad(rgbQuad);
                                    repeat
                                      if scy >= 0
                                        then pword(snap.PixelAddr[x, scy])^ := rgb;
                                      dec(scy);
                                      dec(pixrepcnt);
                                    until pixrepcnt = 0;
                                    dec(oy);
                                    dec(y);
                                  end
                                else
                                  begin
                                    repeat
                                      if (scy >= 0) and (x < snap.Width) and (scy < snap.Height)
                                        then pword(snap.PixelAddr[x, scy])^ := rgb;
                                      dec(scy);
                                      dec(pixrepcnt);
                                    until pixrepcnt = 0;
                                    dec(oy);
                                    dec(y);
                                  end
                            else
                              begin
                                dec(oy);
                                dec(y);
                              end;
                        until (Pixel = img.TransparentIndx) or (oy < 0) or (scy <= ClipRect.Top);
                  end
                else dec(y);
            end;
          y := ClipRect.Bottom + cMaxBuildingHeight*u;
          scy := y;
          inc(x);
        end;
    end;

*)
