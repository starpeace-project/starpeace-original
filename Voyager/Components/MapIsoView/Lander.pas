unit Lander;

interface

  uses
    Windows, Classes, Graphics, GameTypes, LanderTypes, Filters, VCLUtils;

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

  {$IFDEF SHOWCNXS}
  type
    TCnxRenderingData =
      record
        drawimg : boolean;
        x, y    : integer;
      end;

  type
    PCnxRenderingDataArray = ^TCnxRenderingDataArray;
    TCnxRenderingDataArray = array [0..0] of TCnxRenderingData;

  type
    TCnxsRenderingData =
      record
        drawimg    : boolean;
        x, y       : integer;
        count      : integer;
        renderdata : PCnxRenderingDataArray;
      end;
  {$ENDIF}

  type
    TLander =
      class(TInterfacedObject, IGameDocument, ICoordinateConverter)
        public
          constructor Create(const Map : ILanderMap);
          destructor  Destroy;   override;
          procedure   DestroyAll;
        private // IGameDocument
          procedure RenderSnapshot(const view : IGameView; const ClipRect : TRect; snap : TCanvasImage);
          function  ClipMovement(const view : IGameView; var dx, dy : integer) : boolean;
          function  CreateFocus(const view : IGameView) : IGameFocus;
          procedure SetImageSuit( ImageSuit : integer );
        private // ICoordinateConverter
          function  ObjectToMap(const view : IGameView; x, y : integer; out i, j : integer; checkbase : boolean) : boolean;
          function  ScreenToMap(const view : IGameView; x, y : integer; out i, j : integer) : boolean;
          function  MapToScreen(const view : IGameView; i, j : integer; out x, y : integer) : boolean;
          procedure GetViewRegion(const view : IGameView; out imin, jmin, imax, jmax : integer);
        private
          fMap : ILanderMap;
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
        {$IFDEF SHOWCNXS}
        private
          fCnxsRenderingData  : TCnxsRenderingData;
        {$ENDIF}
        private
          function CheckPoint(const view : IGameView; x, y, i, j : integer) : boolean;
          function CheckBuildingBase(const view : IGameView; x, y : integer; out i, j : integer) : boolean;
      end;

implementation

  uses
    SysUtils, AxlDebug, MathUtils, GlassedBuffers, SpriteImages, ColorTableMgr, MapTypes,
    CanvasBmp, Literals {$IFDEF PROFILES}, Profiler, IsoProfile{$ENDIF};


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

  constructor TLander.Create(const Map : ILanderMap);
    begin
      inherited Create;
      fMap := Map;
    end;

  destructor TLander.Destroy;
    begin
      assert(RefCount = 0);
      {$IFDEF SHOWCNXS}
      FreeBuffer;
      {$ENDIF}
      if fTextsToRender<>nil  //.rag
        then freemem(fTextsToRender);
      {$IFDEF SHOWCNXS}
      if fCnxsRenderingData.count <> 0
        then freemem(fCnxsRenderingData.renderdata);
      {$ENDIF}
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
      smalltext : string;

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
        _2Delta      : integer;
        dh           : integer;
        textrendinfo : TTextRenderingInfo;
        PaletteInfo  : TPaletteInfo;
        shadeimg     : TGameImage;
        Temp1, Temp2, Temp3 : integer;
      begin
        with obj do
          begin
            img := Imager.GetObjectImage(id, angle);
            delta := u*Size;
            _2Delta := 2*delta;
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
                    Temp1 := pred(x);
                    ClipDrawLine(Canvas, Temp1, y - 2, Temp1 + _2Delta div corSize, y - delta div corSize - 2, ClipRect);
                    ClipDrawLine(Canvas, Temp1, y - 2, Temp1, y - 1 - dh div corSize - 2, ClipRect);
                    // corner B
                    ClipDrawLine(Canvas, Temp1, y - 1 - dh - 2, Temp1 + _2Delta div corSize, y - 1 - dh - delta div corSize - 2, ClipRect);
                    ClipDrawLine(Canvas, Temp1, y - 1 - dh - 2, Temp1, y - 1 - dh + dh div corSize - 2, ClipRect);
                    // corner G
                    Temp2 := Temp1 + _2Delta;
                    ClipDrawLine(Canvas, Temp2, y - delta - 2, Temp1 + _2Delta - _2Delta div corSize, y - delta + delta div corSize - 2, ClipRect);
                    ClipDrawLine(Canvas, Temp2, y - delta - 2, Temp1 + _2Delta + _2Delta div corSize, y - delta + delta div corSize - 2, ClipRect);
                    ClipDrawLine(Canvas, Temp2, y - delta - 2, Temp1 + _2Delta, y - delta - dh div corSize - 2, ClipRect);
                    // corner H
                    ClipDrawLine(Canvas, Temp2, y - delta - dh - 2, Temp2 - _2Delta div corSize, y - delta + delta div corSize - dh - 2, ClipRect);
                    ClipDrawLine(Canvas, Temp2, y - delta - dh - 2, Temp2 + _2Delta div corSize, y - delta + delta div corSize - dh - 2, ClipRect);
                    ClipDrawLine(Canvas, Temp2, y - delta - dh - 2, Temp2, y - delta - dh + dh div corSize - 2, ClipRect);
                    // corner E
                    Temp3 := Temp1 + 4*delta;
                    ClipDrawLine(Canvas, Temp3, y - 2, Temp3 - _2Delta div corSize, y - delta div corSize - 2, ClipRect);
                    ClipDrawLine(Canvas, Temp3, y - 2, Temp3, y - dh div corSize - 2, ClipRect);
                    // corner F
                    ClipDrawLine(Canvas, Temp3, y - dh - 2, Temp3 - _2Delta div corSize, y - dh - delta div corSize - 2, ClipRect);
                    ClipDrawLine(Canvas, Temp3, y - dh - 2, Temp3, y - dh + dh div corSize - 2, ClipRect);

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
                  fShadeCache[zoom, rotation].Draw(x + sxshift, y + syshift + u*Size - fShadeCache[zoom, rotation].Height, cAlpha, 0, ClipRect, snap, nil);
                end;
            if loRedShaded in Options
              then
                begin
                  if fRedShadeCache[zoom, rotation] = nil
                    then fRedShadeCache[zoom, rotation] := Imager.GetRedShadeImage;
                  fRedShadeCache[zoom, rotation].Draw(x + sxshift, y + syshift + u*Size - fRedShadeCache[zoom, rotation].Height, cAlpha, 0, ClipRect, snap, nil);
                end;
            if loBlackShaded in Options
              then
                begin
                  if fBlackShadeCache[zoom, rotation] = nil
                    then fBlackShadeCache[zoom, rotation] := Imager.GetBlackShadeImage;
                  fBlackShadeCache[zoom, rotation].Draw(x + sxshift, y + syshift + u*Size - fBlackShadeCache[zoom, rotation].Height, cAlpha, 0, ClipRect, snap, nil);
                end;
            if loColorShaded in Options
              then
                begin
                  shadeimg := Imager.GetObjectImage(shadeid, agN);
                  if shadeimg <> nil
                    then shadeimg.Draw(x + sxshift, y + syshift + u*Size - shadeimg.Height, cAlpha, 0, ClipRect, snap, nil);
                end;
            if loGrated in Options
              then
                with snap do
                  begin         // Aqui se dibuja el canjon de selecion
                    // corner A
                    ClipDrawLine(Canvas, x - 1, y - 2, x - 1 + _2Delta div corSize, y + delta div corSize - 2, ClipRect);
                    // corner B
                    ClipDrawLine(Canvas, x - 1, y - 1 - dh - 2, x - 1 + _2Delta div corSize, y - 1 - dh + delta div corSize - 2, ClipRect);
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
                    ClipDrawLine(Canvas, x - 1 + _2Delta, y + delta - 2, x - 1 + _2Delta - _2Delta div corSize, y + delta - delta div corSize - 2, ClipRect);
                    ClipDrawLine(Canvas, x - 1 + _2Delta, y + delta - 2, x - 1 + _2Delta + _2Delta div corSize, y + delta - delta div corSize - 2, ClipRect);
                    ClipDrawLine(Canvas, x - 1 + _2Delta, y + delta - 2, x - 1 + _2Delta, y + delta - dh div corSize - 2, ClipRect);
                    // corner E
                    ClipDrawLine(Canvas, x - 1 + 4*delta, y - 2, x - 1 + 4*delta - 2*delta div corSize, y + delta div corSize - 2, ClipRect);
                    // corner F
                    ClipDrawLine(Canvas, x - 1 + 4*delta, y - dh - 2, x - 1 + 4*delta - _2Delta div corSize, y - dh + delta div corSize - 2, ClipRect);
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

    procedure DrawSimpleObject(const x, y : integer; const obj : TObjInfo);
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

    function InsideRightEdge(const i, j : integer) : boolean;
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

    function InsideBottomEdge(const i, j : integer) : boolean;
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

    function OnLeftEdge(const i, j : integer) : boolean;
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

        procedure CalcIterationValues(const i, j, Size : integer; out sc, ec, deltac, sr, er, deltar : integer; out colinouterloop : boolean);
          begin
            case rotation of
              drNorth:
                begin
                  sc := j + Size - 1;
                  ec := j - 1;
                  deltac := -1;
                  sr := i;
                  er := i - Size;
                  deltar := -1;
                  colinouterloop := true;
                end;
              drEast:
                begin
                  sc := j;
                  ec := j - Size;
                  deltac := -1;
                  sr := i - Size + 1;
                  er := i + 1;
                  deltar := 1;
                  colinouterloop := false;
                end;
              drSouth:
                begin
                  sc := j - Size + 1;
                  ec := j + 1;
                  deltac := 1;
                  sr := i;
                  er := i + Size;
                  deltar := 1;
                  colinouterloop := true;
                end;
              else // drWest
                begin
                  sc := j;
                  ec := j + Size;
                  deltac := 1;
                  sr := i + Size - 1;
                  er := i - 1;
                  deltar := -1;
                  colinouterloop := false;
                end;
            end;
          end;

        procedure StartOuterLoop(const colinouterloop : boolean; const sc, sr : integer; out c, r : integer);
          begin
            if colinouterloop
              then c := sc
              else r := sr;
          end;

        function CheckOuterLoopCond(const colinouterloop : boolean;const  c, r, ec, er : integer) : boolean;
          begin
            if colinouterloop
              then Result := c <> ec
              else Result := r <> er;
          end;

        procedure DoOuterLoopStep(const colinouterloop : boolean; var c, r : integer; const deltac, deltar : integer);
          begin
            if colinouterloop
              then inc(c, deltac)
              else inc(r, deltar);
          end;

        procedure StartInnerLoop(const colinouterloop : boolean; const sc, sr : integer; out c, r : integer);
          begin
            if not colinouterloop
              then c := sc
              else r := sr;
          end;

        function CheckInnerLoopCond(const colinouterloop : boolean; const c, r, ec, er : integer) : boolean;
          begin
            if not colinouterloop
              then Result := c <> ec
              else Result := r <> er;
          end;

        procedure DoInnerLoopStep(const colinouterloop : boolean; var c, r : integer; const deltac, deltar : integer);
          begin
            if not colinouterloop
              then inc(c, deltac)
              else inc(r, deltar);
          end;

        procedure DrawItemBase(const item : TItemInfo;x, y, i, j, Size : integer);
          var
            r, c           : integer;
            obj            : TObjInfo;
            overlay        : TItemInfo;
            sc, ec         : integer;
            sr, er         : integer;
            deltac         : integer;
            deltar         : integer;
            colinouterloop : boolean;
          begin
            inc(x, 2*pred(Size)*u);
            dec(y, pred(Size)*u);
            CalcIterationValues(i, j, Size, sc, ec, deltac, sr, er, deltar, colinouterloop);
            StartOuterLoop(colinouterloop, sc, sr, c, r);
            while CheckOuterLoopCond(colinouterloop, c, r, ec, er) do
              begin
                StartInnerLoop(colinouterloop, sc, sr, c, r);
                while CheckInnerLoopCond(colinouterloop, c, r, ec, er) do
                  begin
                    if (lsOverlayedGround in item.States) and fMap.GetGroundOverlayInfo(r, c, focus, overlay)
                      then
                        begin
                          if lsGrounded in overlay.States
                            then
                              if fMap.GetGroundInfo(r, c, focus, obj)
                                then DrawObject(x, y, 1, obj);
                          with overlay do
                            DrawObject(x + xshift, y + yshift, 1, overlay);
                        end
                      else
                        if fMap.GetGroundInfo(r, c, focus, obj)
                          then DrawObject(x, y, 1, obj);
                    inc(x, 2*u);
                    inc(y, u);
                    DoInnerLoopStep(colinouterloop, c, r, deltac, deltar);
                  end;
                dec(x, 2*succ(Size)*u);
                dec(y, pred(Size)*u);
                DoOuterLoopStep(colinouterloop, c, r, deltac, deltar);
              end;
          end;

        procedure DrawOverlays(x, y, i, j, Size : integer);
          var
            r, c           : integer;
            overlayinfo    : TItemOverlayInfo;
            k              : integer;
            sc, ec         : integer;
            sr, er         : integer;
            deltac         : integer;
            deltar         : integer;
            colinouterloop : boolean;
          begin
            inc(x, 2*pred(Size)*u);
            dec(y, pred(Size)*u);
            CalcIterationValues(i, j, Size, sc, ec, deltac, sr, er, deltar, colinouterloop);
            StartOuterLoop(colinouterloop, sc, sr, c, r);
            while CheckOuterLoopCond(colinouterloop, c, r, ec, er) do
              begin
                StartInnerLoop(colinouterloop, sc, sr, c, r);
                while CheckInnerLoopCond(colinouterloop, c, r, ec, er) do
                  begin
                    if fMap.GetItemOverlayInfo(r, c, focus, overlayinfo)
                      then
                        with overlayinfo do
                          for k := 1 to count do
                            with objects[k] do
                              DrawSimpleObject(x + xshift, y + yshift, objects[k]);
                    inc(x, 2*u);
                    inc(y, u);
                    DoInnerLoopStep(colinouterloop, c, r, deltac, deltar);
                  end;
                dec(x, 2*succ(Size)*u);
                dec(y, pred(Size)*u);
                DoOuterLoopStep(colinouterloop, c, r, deltac, deltar);
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
                    with objects[k] do
                      begin
                        img := Imager.GetObjectImage(id, angle);
                        if img <> nil
                          then
                            begin
                              if loNoClip in Options
                                then ImgClipRect := ClipRect
                                else
                                  begin
                                    ImgClipRect := Rect(max(xmin, x + xshift), max(ymin, y + yshift), min(xmax, x + xshift + img.Width), min(ymax, y + yshift + img.Height));
                                    IntersectRect(ImgClipRect, ImgClipRect, ClipRect);
                                  end;
                              if not IsRectEmpty(ImgClipRect)
                                then
                                  begin
                                    PaletteInfo := FilterObjectPalette(img, objects[k]);
                                    if loGlassed in Options
                                      then img.Draw(x + xshift, y + yshift, cAlpha, Frame, ImgClipRect, snap, PaletteInfo)
                                      else img.Draw(x + xshift, y + yshift, 0, Frame, ImgClipRect, snap, PaletteInfo);
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
            try
              itemimg := Imager.GetObjectImage(item.id, item.angle);
              if itemimg <> nil
                then
                  with item, Overlays do
                    for k := 1 to Count do
                      with Objects[k] do
                        begin
                          overlimg := Imager.GetObjectImage(id, angle);
                          if overlimg <> nil
                            then
                              begin
                                PaletteInfo := FilterObjectPalette(overlimg, objects[k]);
                                if loGlassed in Options
                                  then overlimg.Draw(x + xshift, y + u*Size - itemimg.Height + yshift, cAlpha, Frame, ClipRect, snap, PaletteInfo)
                                  else overlimg.Draw(x + xshift, y + u*Size - itemimg.Height + yshift, 0, Frame, ClipRect, snap, PaletteInfo);
                              end;
                        end;
            except
            end;
          end;

        procedure DrawItem(x, y, i, j : integer);
          var
            upitem    : TItemInfo;
            ritem     : TItemInfo;
            upi, upj  : integer;
            ri, rj    : integer;
            clipdelta : integer;
            Temp : boolean;
          begin
            if lsHighLand in item.States
              then clipdelta := 10
              else clipdelta := 0;
            if loUpProjected in item.Options
              then
                begin
                  incrow(view, i, j, upi, upj);
                  with item do
                    begin
                      if fMap.GetItemInfo(upi, upj, focus, upitem) and (lsOverlayed in upitem.States)
                        then ClipDrawOverlays(x - 2*u, y - u, upi, upj, x, x + 4*u, y - u - clipdelta, y + u);
                      inccol(view, i, j, upi, upj);
                      if fMap.GetItemInfo(upi, upj, focus, upitem) and (lsOverlayed in upitem.States)
                        then ClipDrawOverlays(x + 2*u, y - u, upi, upj, x, x + 4*u, y - u - clipdelta, y + u);
                      if lsGrounded in States
                        then DrawItemBase(item, x, y, i, j, Size);
                      DrawObject(x + xshift, y + yshift, Size, item);  // este es para dibujar los edificios
                      DrawItemOverlays(x, y, item);
                      if lsOverlayed in States
                        then DrawOverlays(x, y, i, j, Size);
                    end;
                end
              else
                begin
                  with item do
                    begin
                      if lsGrounded in States
                        then DrawItemBase(item, x, y, i, j, Size);
                      DrawObject(x + xshift, y + yshift, Size, item); // este dibuja las carreteras
                    end;
                  DrawItemOverlays(x, y, item);
                  incrow(view, i, j, upi, upj);
                  fMap.GetItemInfo(upi, upj, focus, upitem);
                  inccol(view, i, j, ri, rj);
                  fMap.GetItemInfo(ri, rj, focus, ritem);

                  Temp := (lsDoubleOverlayed in item.States) or
                     (lsDoubleOverlayed in upitem.States) or
                     (lsDoubleOverlayed in ritem.States);
                  if Temp
                    then DrawOverlays(x, y, i, j, item.Size);  // dibuja los carritos en los puentes
                  if lsOverlayed in upitem.States
                    then ClipDrawOverlays(x - 2*u, y - u, upi, upj, x, x + 4*u, y - u - clipdelta, y + u);

                  if lsOverlayed in ritem.States
                    then ClipDrawOverlays(x + 2*u, y - u, ri, rj, x, x + 4*u, y - u - clipdelta, y + u);  // este casi no funciona
                  if not Temp   //not (lsDoubleOverlayed in item.States) and
                                //not (lsDoubleOverlayed in upitem.States) and
                                //not (lsDoubleOverlayed in ritem.States)
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
          ScanningTextId = 'Literal420';
        const
          ExtraWidth = 100;
        var
          Rect    : TRect;
          MidRect : TRect;
          i, j    : integer;
          org     : TPoint;
        begin
          if which = ''
            then which := GetLiteral(ScanningTextId);
          with snap.Canvas.Font do
            begin
              Name := 'Verdana';
              Style := [fsBold];
              Size := 7;
              Color  := clBlack;
            END;
          snap.Canvas.Brush.Style := bsClear;
          Rect := Classes.Rect(0, 0, 200, 1000);
          with snap.Canvas do
            begin
              DrawText(Handle, pchar(which), length(which), Rect, DT_CALCRECT or DT_CENTER or DT_WORDBREAK or DT_EXPANDTABS);
              Rect := Classes.Rect(x - ExtraWidth, y - Rect.Bottom, x + ExtraWidth, y); // ***
              MidRect := Rect;
              for i := -1 to 1 do
                for j := -1 to 1 do
                  if ((i = j) or (i = -j)) and (i <> 0)
                    then
                      begin
                        Rect := MidRect;
                        OffsetRect(Rect, i, j);
                        DrawText(Handle, pchar(which), length(which), Rect, DT_CENTER or DT_WORDBREAK or DT_EXPANDTABS);
                      end;
              snap.Canvas.Font.Color := clYellow;
              DrawText(Handle, pchar(which), length(which), MidRect, DT_CENTER or DT_WORDBREAK or DT_EXPANDTABS);
            end;
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

    {$IFDEF SHOWCNXS}
    procedure DrawObjectConnections;
      var
        k          : integer;
        cnxsinfo   : TCnxsInfo;
        pivotinfo  : TCnxInfo;
        pivotdata  : TCnxRenderingData;
        srcx       : integer;
        srcy       : integer;
        destx      : integer;
        desty      : integer;
        //drawbuff   : TCanvasImage;
        //drawnlines : integer;

      procedure DoDrawCnxSrcDownImg(x, y : integer);
        begin
          if fCnxSourceDownCache[zoom, rotation] = nil
            then fCnxSourceDownCache[zoom, rotation] := Imager.GetCnxSourceDownImage;
          fCnxSourceDownCache[zoom, rotation].Draw(x - fCnxSourceDownCache[zoom, rotation].Width div 2, y - fCnxSourceDownCache[zoom, rotation].Height div 2, 0, 0, ClipRect, snap, nil);
        end;

      procedure DoDrawCnxSrcTopImg(x, y : integer);
        begin
          if fCnxSourceTopCache[zoom, rotation] = nil
            then fCnxSourceTopCache[zoom, rotation] := Imager.GetCnxSourceTopImage;
          fCnxSourceTopCache[zoom, rotation].Draw(x - fCnxSourceTopCache[zoom, rotation].Width div 2, y - fCnxSourceTopCache[zoom, rotation].Height div 2, 0, 0, ClipRect, snap, nil);
        end;

      procedure DoDrawCnxDestDownImg(x, y : integer);
        begin
          if fCnxDestDownCache[zoom, rotation] = nil
            then fCnxDestDownCache[zoom, rotation] := Imager.GetCnxDestDownImage;
          fCnxDestDownCache[zoom, rotation].Draw(x - fCnxDestDownCache[zoom, rotation].Width div 2, y - fCnxDestDownCache[zoom, rotation].Height div 2, 0, 0, ClipRect, snap, nil);
        end;

      procedure DoDrawCnxDestTopImg(x, y : integer);
        begin
          if fCnxDestTopCache[zoom, rotation] = nil
            then fCnxDestTopCache[zoom, rotation] := Imager.GetCnxDestTopImage;
          fCnxDestTopCache[zoom, rotation].Draw(x - fCnxDestTopCache[zoom, rotation].Width div 2, y - fCnxDestTopCache[zoom, rotation].Height div 2, 0, 0, ClipRect, snap, nil);
        end;

      function CalcCnxRenderingData(const cnx : TCnxInfo; cnxkind : TCnxKind; out cnxrenderdata : TCnxRenderingData) : boolean;
        var
          item    : TItemInfo;
          img     : TGameImage;
          imgRect : TRect;

        function CalcSrcImagesRect(x, y : integer) : TRect;
          var
            downRect : TRect;
            topRect  : TRect;
            whalf    : integer;
            hhalf    : integer;
          begin
            if fCnxSourceDownCache[zoom, rotation] = nil
              then fCnxSourceDownCache[zoom, rotation] := Imager.GetCnxSourceDownImage;
            if fCnxSourceTopCache[zoom, rotation] = nil
              then fCnxSourceTopCache[zoom, rotation] := Imager.GetCnxSourceTopImage;
            with cnxrenderdata do
              begin
                whalf := fCnxSourceDownCache[zoom, rotation].Width div 2;
                hhalf := fCnxSourceDownCache[zoom, rotation].Height div 2;
                downRect := Rect(x - whalf, y - hhalf, x + whalf, y + hhalf);
                whalf := fCnxSourceTopCache[zoom, rotation].Width div 2;
                hhalf := fCnxSourceTopCache[zoom, rotation].Height div 2;
                topRect := Rect(x - whalf, y - hhalf, x + whalf, y + hhalf);
              end;
            UnionRect(Result, downRect, topRect);
          end;

        function CalcDestImagesRect(x, y : integer) : TRect;
          var
            downRect : TRect;
            topRect  : TRect;
            whalf    : integer;
            hhalf    : integer;
          begin
            if fCnxDestDownCache[zoom, rotation] = nil
              then fCnxDestDownCache[zoom, rotation] := Imager.GetCnxDestDownImage;
            if fCnxDestTopCache[zoom, rotation] = nil
              then fCnxDestTopCache[zoom, rotation] := Imager.GetCnxDestTopImage;
            with cnxrenderdata do
              begin
                whalf := fCnxDestDownCache[zoom, rotation].Width div 2;
                hhalf := fCnxDestDownCache[zoom, rotation].Height div 2;
                downRect := Rect(x - whalf, y - hhalf, x + whalf, y + hhalf);
                whalf := fCnxDestTopCache[zoom, rotation].Width div 2;
                hhalf := fCnxDestTopCache[zoom, rotation].Height div 2;
                topRect := Rect(x - whalf, y - hhalf, x + whalf, y + hhalf);
              end;
            UnionRect(Result, downRect, topRect);
          end;

        begin
          Result := false;
          with cnxrenderdata do
            begin
              drawimg := false;
              if fMap.GetItemInfo(cnx.r, cnx.c, focus, item) and (lsGrounded in item.States) and MapToScreen(view, cnx.r, cnx.c, x, y)
                then
                  begin
                    img := Imager.GetObjectImage(item.id, item.angle);
                    if img <> nil
                      then
                        begin
                          x := x + 2*u;
                          y := y + u - img.Height;
                          if cnxkind = cnxkInputs
                            then imgRect := CalcSrcImagesRect(x, y)
                            else imgRect := CalcDestImagesRect(x, y);
                          drawimg := IntersectRect(imgRect, imgRect, ClipRect);
                          Result := true;
                        end;
                  end;
            end;
        end;

      begin
        if fMap.GetCnxsInfo(focus, cnxsinfo)
          then
            begin
              //drawbuff := GetBuffer(snap.Width, snap.Height);
              //drawnlines := 0;
              //SetupBufferBackground(ClipRect);
              with cnxsinfo, fCnxsRenderingData, (*drawbuff*)snap.Canvas do
                begin
                  if count <> cnxcount
                    then
                      begin
                        if count <> 0
                          then freemem(renderdata);
                        getmem(renderdata, sizeof(renderdata[0])*cnxcount);
                        count := cnxcount;
                      end;
                  if view.ZoomLevel >= 2
                    then Pen.Width := 2
                    else Pen.Width := 1;

                  pivotinfo.r := cnxsinfo.r;
                  pivotinfo.c := cnxsinfo.c;
                  pivotinfo.color := clNone;
                  if cnxkind = cnxkInputs
                    then CalcCnxRenderingData(pivotinfo, cnxkOutputs, pivotdata)
                    else CalcCnxRenderingData(pivotinfo, cnxkInputs, pivotdata);
                  for k := 0 to pred(cnxcount) do
                    if not CalcCnxRenderingData(cnxs[k], cnxkind, renderdata[k])
                      then
                        begin
                          renderdata[k].drawimg := false;
                          renderdata[k].x := -1;
                          renderdata[k].y := -1;
                        end;
                  if pivotdata.drawimg
                    then
                      case cnxkind of
                        cnxkInputs:
                          DoDrawCnxDestDownImg(pivotdata.x, pivotdata.y);
                        cnxkOutputs:
                          DoDrawCnxSrcDownImg(pivotdata.x, pivotdata.y);
                      end;
                  for k := 0 to pred(cnxcount) do
                    if renderdata[k].drawimg
                      then
                        case cnxkind of
                          cnxkInputs:
                            DoDrawCnxSrcDownImg(renderdata[k].x, renderdata[k].y);
                          cnxkOutputs:
                            DoDrawCnxDestDownImg(renderdata[k].x, renderdata[k].y);
                        end;
                  for k := 0 to pred(cnxcount) do
                    begin
                      srcx := pivotdata.x;
                      srcy := pivotdata.y;
                      destx := renderdata[k].x;
                      desty := renderdata[k].y;
                      if ClipLine(srcx, srcy, destx, desty, ClipRect)
                        then
                          begin
                            Pen.Color := cnxs[k].color;
                            MoveTo(srcx, srcy);
                            LineTo(destx, desty);
                            //inc(drawnlines);
                          end;
                    end;
                  Pen.Width := 1;
                  (*if drawnlines > 0
                    then RenderBuffer(snap, ClipRect);*)
                  if pivotdata.drawimg
                    then
                      case cnxkind of
                        cnxkInputs:
                          DoDrawCnxDestTopImg(pivotdata.x, pivotdata.y);
                        cnxkOutputs:
                          DoDrawCnxSrcTopImg(pivotdata.x, pivotdata.y);
                      end;
                  for k := 0 to pred(cnxcount) do
                    if renderdata[k].drawimg
                      then
                        case cnxkind of
                          cnxkInputs:
                            DoDrawCnxSrcTopImg(renderdata[k].x, renderdata[k].y);
                          cnxkOutputs:
                            DoDrawCnxDestTopImg(renderdata[k].x, renderdata[k].y);
                        end;
                end;
            end;
      end;
    {$ENDIF}

    procedure DrawFocusObject;
      var
        obj : TObjInfo;
      begin
        if fMap.GetFocusObjectInfo(focus, ClipRect, obj)
          then DrawSimpleObject(obj.xshift, obj.yshift, obj);
      end;

    procedure DrawSurfaceData(x, y, i, j : integer);

      procedure DrawRank(x, i, j : integer);
        var
          surfinfoobj : TObjInfo;
        begin
          while (x < ClipRect.Right) and InsideRightEdge(i, j) do
            begin
              if fMap.GetSurfaceInfo(i, j, focus, surfinfoobj)
                then DrawObject(x, y, 1, surfinfoobj);
              inccol(view, i, j, i, j);
              decrow(view, i, j, i, j);
              inc(x, 4*u);
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

    procedure DrawAirObjects;
      var
        airobjs : TAirObjsInfo;
        x, y    : integer;
        k       : integer;
      begin
        if fMap.GetAirInfo(focus, ClipRect, airobjs)
          then
            with airobjs do
              for k := 0 to pred(count) do
                begin
                  MapToScreen(view, objects[k].r, objects[k].c, x, y);
                  DrawSimpleObject(x + objects[k].xshift, y + objects[k].yshift, objects[k]);
                end;
      end;

    procedure DrawSmallText;
      var
        x, y  : integer;
        j, k  : integer;
        color : TColor;
      begin
        if focus.GetSmallTextData(x, y, smalltext, color)
          then
            with snap.Canvas do
              begin
                SetBkMode(Handle, TRANSPARENT);
                Font.Name := 'Verdana';
                Font.Size := 8;
                Font.Style := [fsBold];
                Font.Color := clBlack;
                for j := -1 to 1 do
                  for k := -1 to 1 do
                    if ((j = k) or (j = -k)) and (j <> 0)
                      then TextOut(x + j, y + k, smalltext);
                Font.Color := color;
                TextOut(x, y, smalltext);
              end;
      end;

    {$IFDEF DRAWGRID}
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
    {$ENDIF}

    begin
      //Profiler.ProcStarted(prfKind_Main, prfId_RenderingInit);
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
      //Profiler.ProcEnded(prfKind_Main, prfId_RenderingInit);
      //Profiler.ProcStarted(prfKind_Main, prfId_RegionFill);
      snap.Canvas.Brush.Color := clBlack;
      snap.Canvas.Brush.Style := bsSolid;
      snap.Canvas.FillRect(ClipRect);
      //Profiler.ProcEnded(prfKind_Main, prfId_RegionFill);
      if focus.IsLandVisible
        then
          try
            //Profiler.ProcStarted(prfKind_Main, prfId_LandRendering);
            DrawLandLayers(x, y, i, j);
            //Profiler.ProcEnded(prfKind_Main, prfId_LandRendering);
            DrawFocusObject;
          except
          end;
      //Profiler.ProcStarted(prfKind_Main, prfId_SurfaceRendering);
      if focus.HasValidSurface
        then DrawSurfaceData(x, y, i, j);
      //Profiler.ProcEnded(prfKind_Main, prfId_SurfaceRendering);
      if focus.IsLandVisible
        then
          begin
            //Profiler.ProcStarted(prfKind_Main, prfId_ConnectionsRendering);
            {$IFDEF SHOWCNXS}
            DrawObjectConnections;
            {$ENDIF}
            //Profiler.ProcEnded(prfKind_Main, prfId_ConnectionsRendering);
            //Profiler.ProcStarted(prfKind_Main, prfId_TextRendering);
            DrawTexts;
            //Profiler.ProcEnded(prfKind_Main, prfId_TextRendering);
            //Profiler.ProcStarted(prfKind_Main, prfId_AirplaneRendering);
            if focus.IsLandVisible
              then DrawAirObjects;
            //Profiler.ProcEnded(prfKind_Main, prfId_AirplaneRendering);
          end;
      {
      Profiler.ProcStarted(prfKind_Main, prfId_ChatTextRendering);
      Profiler.ProcEnded(prfKind_Main, prfId_ChatTextRendering);
      }
      DrawSmallText;
      {$IFDEF DRAWGRID}
      if true
        then DrawGrid;
      {$ENDIF}
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
      u  := 2 shl view.ZoomLevel;
      tu := u shl 2;
      //tu  := u + u;
      //inc(tu, tu);
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

  procedure TLander.GetViewRegion(const view : IGameView; out imin, jmin, imax, jmax : integer);
    var
      i, j : integer;
    begin
      with view.GetSize do
        begin
          ScreenToMap(view, x, y, i, j);
          case view.Rotation of
            drNorth:
              imin := max(i, 0);
            drEast:
              jmin := max(j, 0);
            drSouth:
              imax := min(i, pred(fMap.GetRows));
            drWest:
              jmax := min(j, pred(fMap.GetColumns));
          end;
          ScreenToMap(view, 0, y, i, j);
          case view.Rotation of
            drNorth:
              jmin := max(j, 0);
            drEast:
              imax := min(i, pred(fMap.GetRows));
            drSouth:
              jmax := min(j, pred(fMap.GetColumns));
            drWest:
              imin := max(i, 0);
          end;
          ScreenToMap(view, 0, 0, i, j);
          case view.Rotation of
            drNorth:
              imax := min(i, pred(fMap.GetRows));
            drEast:
              jmax := min(j, pred(fMap.GetColumns));
            drSouth:
              imin := max(i, 0);
            drWest:
              jmin := max(j, 0);
          end;
          ScreenToMap(view, x, 0, i, j);
          case view.Rotation of
            drNorth:
              jmax := min(j, pred(fMap.GetColumns));
            drEast:
              imin := max(i, 0);
            drSouth:
              jmin := max(j, 0);
            drWest:
              imax := min(i, pred(fMap.GetRows));
          end;
        end;
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

  procedure TLander.DestroyAll;
    begin
      fMap := nil;
    end;
    
end.
