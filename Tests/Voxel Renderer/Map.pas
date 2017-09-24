unit Map;

interface


uses
  Windows, Classes, Graphics, Threads, GameTypes, LanderTypes, MapTypes, ImageCache;

type
  TLandItem =
    record
      id     : idLand;
      height : integer;
    end;

type
  TLandRow   = array[word] of TLandItem;
  TLandItems = array[word] of ^TLandRow;

type
  TLandHeightInfo = THeightInfo;

type
  TLandLuminanceInfo = TLuminanceInfo;

type
  TExtraLandInfoItem =
    record
      heightinfo : TLandHeightInfo;
      luminfo    : TLandLuminanceInfo;
    end;

const
  cBlockBits = 5;
  cBlockSize = 1 shl cBlockBits;
  cBlockMask = pred(cBlockSize);

type
  PExtraLandInfoItems = ^TExtraLandInfoItems;
  TExtraLandInfoItems = array[0 .. pred(cBlockSize), 0 .. pred(cBlockSize)] of TExtraLandInfoItem;

type
  TExtraLandInfoBlock =
    record
      validlum : boolean;
      fItems   : PExtraLandInfoItems;
    end;

type
  TExtraLandInfoBlockRow   = array[word] of TExtraLandInfoBlock;
  TExtraLandInfoBlockItems = array[word] of ^TExtraLandInfoBlockRow;

type
  TWorldMap =
    class(TInterfacedObject, IWorldMap)
      public
        constructor Create(const Manager : ILocalCacheManager);
        destructor  Destroy;   override;
      private // IWorldMap
        fManager   : ILocalCacheManager;
        fConverter : ICoordinateConverter;
        procedure InitMap;
        function  GetRows : integer;
        function  GetColumns : integer;
        function  GetGroundInfo(i, j : integer; const focus : IGameFocus; out ground : TObjInfo) : boolean;
        function  GetExtraLandInfo(i, j : integer; out extrainfo : TExtraLandInfo) : boolean;
        function  CreateFocus(const view : IGameView) : IGameFocus;
        function  GetImager(const focus : IGameFocus) : IImager;
      public
        procedure SetCoordConverter(const which : ICoordinateConverter);
      private  // Land
        fColumns             : integer;
        fRows                : integer;
        fLands               : ^TLandItems;
        fExtraLandInfoBlocks : ^TExtraLandInfoBlockItems;
        fMapImg              : TMapImage;
        procedure CreateLands(Map : TMapImage);
        procedure DestroyLands;
        procedure CreateExtraLandInfoBlocks;
        procedure DestroyExtraLandInfoBlocks;
        procedure CreateExtraLandInfoItems(row, col : integer; createluminfo : boolean);
        procedure DestroyExtraLandInfoItems(row, col : integer);
        function  GetLandHeightInfo(i, j : integer; out heightinfo : TLandHeightInfo) : boolean;
      private
        procedure ReleaseMap;
      private // Images
        fImageCache : TImageCache;
      private  // misc
        fFocuses : TList;
        procedure InitViewOrigin(const view : IGameView);
    end;

implementation


uses
  Messages, AxlDebug, SysUtils, Land, MathUtils;

type
  TGameFocus =
    class(TInterfacedObject, IGameFocus)
      public
        constructor Create(Map : TWorldMap; const view : IGameView);
        destructor  Destroy;   override;
      private // IGameUpdater
        fLockCount     : integer;
        fUpdateDefered : boolean;
        function  Lock : integer;
        function  Unlock : integer;
        function  LockCount : integer;
        procedure QueryUpdate(Defer : boolean);
      private // IGameFocus ... Dispatch inherited from TObject
        procedure MouseMove(x, y : integer);
        procedure MouseClick;
        procedure KeyPressed(which : word; const Shift : TShiftState);
        procedure Refresh;
        function  GetText(kind : integer) : string;
        function  GetObject : TObject;
        function  GetRect : TRect;
        procedure SetRect(const R : TRect);
      private
        fMap        : TWorldMap;
        fView       : IGameView;
        fZoomFactor : single;
        fImager     : IImager;
        fMouseX     : integer;
        fMouseY     : integer;
        Selection   : TSelectionData;
        function GetImager : IImager;
      private
        procedure ViewZoomed(var msg : TViewZoomedMsg); message msgViewZoomed;
        procedure MoveTo(var msg : TMoveToMsg);         message msgMoveTo;
    end;

const
  cBasicU = 16;

// Utils

function min(i, j : integer) : integer;
  begin
    if i <= j
      then Result := i
      else Result := j;
  end;

function max(i, j : integer) : integer;
  begin
    if i >= j
      then Result := i
      else Result := j;
  end;

procedure SetRectOrigin(var which : TRect; x, y : integer);
  begin
    OffsetRect(which, x - which.Left, y - which.Top);
  end;

procedure FreeObject(var which);
  var
    aux : TObject;
  begin
    aux := TObject(which);
    TObject(which) := nil;
    aux.Free;
  end;

function RectFromMapBlock(const view : IGameView; const converter : ICoordinateConverter; i, j : integer) : TRect;
  var
    x, y   : integer;
    u      : integer;
    R      : TRect;
  begin
    converter.MapToScreen(view, i, j, x, y);
    u := 2 shl view.ZoomLevel;
    R.Left := x;
    R.Right := x + 4*u;
    R.Top := y - u;
    R.Bottom := y + u;
    Result := R;
  end;

// TWorldMap

constructor TWorldMap.Create(const Manager : ILocalCacheManager);
  begin
    inherited Create;
    fManager := Manager;
  end;

destructor TWorldMap.Destroy;
  begin
    assert(RefCount = 0);
    ReleaseMap;
    pointer(fConverter) := nil;  // Cross referenced
    inherited;
  end;

function TWorldMap.GetRows : integer;
  begin
    Result := fRows;
  end;

function TWorldMap.GetColumns : integer;
  begin
    Result := fColumns;
  end;

const
  cFocusOptions : array[boolean] of TLandOption = (loRedShaded, loShaded);

function TWorldMap.GetGroundInfo(i, j : integer; const focus : IGameFocus; out ground : TObjInfo) : boolean;
  begin
    Result := (i >= 0) and (i < fRows) and (j >= 0) and (j < fColumns);
    if Result
      then
        begin
          fillchar(ground, sizeof(ground), 0);
          ground.Options := [];
          ground.id := fLands[i, j].id;
          if LandTypeOf(fLands[i, j].id) = ldtSpecial
            then fLands[i, j].id := ord(LandClassOf(fLands[i, j].id)) shl lndClassShift;
          ground.Height := 1 + fLands[i, j].height;
        end;
  end;

function TWorldMap.GetExtraLandInfo(i, j : integer; out extrainfo : TExtraLandInfo) : boolean;
  var
    row, col : integer;
  begin
    Result := (i >= 0) and (i < fRows) and (j >= 0) and (j < fColumns);
    if Result
      then
        begin
          fillchar(extrainfo, sizeof(extrainfo), 0);
          row := i shr cBlockBits;
          col := j shr cBlockBits;
          if (fExtraLandInfoBlocks[row, col].fItems = nil) or not fExtraLandInfoBlocks[row, col].validlum
            then CreateExtraLandInfoItems(row, col, true);
          extrainfo.heightinfo := fExtraLandInfoBlocks[row, col].fItems[i and cBlockMask, j and cBlockMask].heightinfo;
          extrainfo.luminfo := fExtraLandInfoBlocks[row, col].fItems[i and cBlockMask, j and cBlockMask].luminfo;
        end;
  end;

function TWorldMap.CreateFocus(const view : IGameView) : IGameFocus;
  begin
    InitViewOrigin(view);
    Result := TGameFocus.Create(Self, view);
  end;

function TWorldMap.GetImager(const focus : IGameFocus) : IImager;
  begin
    Result := TGameFocus(focus.GetObject).GetImager;
  end;

procedure TWorldMap.InitMap;
  var
    Map : TMapImage;
    i   : integer;
  begin
    Map         := fManager.GetLandMap;
    fMapImg     := Map;
    fColumns    := Map.Width;
    fRows       := Map.Height;
    fFocuses    := TList.Create;
    fImageCache := TImageCache.Create(fManager);
    CreateLands(Map);
    CreateExtraLandInfoBlocks;
    for i := 0 to pred(fFocuses.Count) do
      InitViewOrigin(TGameFocus(fFocuses[i]).fView);
  end;

procedure TWorldMap.ReleaseMap;
  begin
    DestroyExtraLandInfoBlocks;
    DestroyLands;
    FreeObject(fImageCache);
    assert(fFocuses.Count = 0, Format('fFocuses.Free error, Count = %d', [fFocuses.Count]));
    FreeObject(fFocuses);  // <<>> Members of fFcocused
    fRows        := 0;
    fColumns     := 0;
    fMapImg      := nil;
  end;

procedure TWorldMap.SetCoordConverter(const which : ICoordinateConverter);
  begin
    fConverter := which;
    fConverter._Release; // cross referenced
  end;

procedure TWorldMap.CreateLands(Map : TMapImage);
  const
    MaxHeight = 40;
  var
    i, j : integer;
  begin
    getmem(fLands, fRows*sizeof(fLands[0]));
    for i := 0 to pred(fRows) do
      begin
        getmem(fLands[i], fColumns*sizeof(fLands[0, 0]));
        for j := 0 to pred(fColumns) do
          begin
            fLands[i, j].id := pbyte(Map.PixelAddr[j, i, 0])^;
            //fLands[i, j].height := 1 + random(3*MaxHeight);
            {
            if random(4) = 0
              then fLands[i, j].height := round(MaxHeight*sqrt(2 + sqr((cos(i*Pi/5))) + sqr(sin(j*Pi/5))))
              else fLands[i, j].height := round(MaxHeight*(2 + sqr((cos(i*Pi/5))) + sqr(sin(j*Pi/5))));
            }
            if LandClassOf(fLands[i, j].id) <> lncZoneD
              then fLands[i, j].height := round(MaxHeight*(2 + cos(i*Pi/5) + sin(j*Pi/5)))
              else fLands[i, j].height := 0;
            //fLands[i, j].height := 10*((j + i) mod 3);
            //fLands[i, j].height := 10*((i mod 3 + j mod 3))
            //fLands[i, j].height := 10;
          end;
      end;
  end;

procedure TWorldMap.DestroyLands;
  begin
    freemem(fLands);
  end;

procedure TWorldMap.CreateExtraLandInfoBlocks;
  var
    i    : integer;
    r, c : integer;
  begin
    r := succ(fRows shr cBlockBits);
    c := succ(fColumns shr cBlockBits);
    getmem(fExtraLandInfoBlocks, r*sizeof(fExtraLandInfoBlocks[0]));
    getmem(fExtraLandInfoBlocks[0], r*c*sizeof(fExtraLandInfoBlocks[0, 0]));
    fillchar(fExtraLandInfoBlocks[0]^, r*c*sizeof(fExtraLandInfoBlocks[0, 0]), 0);
    for i := 1 to pred(r) do
      fExtraLandInfoBlocks[i] := @fExtraLandInfoBlocks[i - 1, c];
  end;

procedure TWorldMap.DestroyExtraLandInfoBlocks;
  var
    i, j : integer;
    r, c : integer;
  begin
    r := succ(fRows shr cBlockBits);
    c := succ(fColumns shr cBlockBits);
    for i := 0 to pred(r) do
      for j := 0 to pred(c) do
        if fExtraLandInfoBlocks[i, j].fItems <> nil
          then DestroyExtraLandInfoItems(i, j);
    freemem(fExtraLandInfoBlocks[0]);
    freemem(fExtraLandInfoBlocks);
  end;

procedure TWorldMap.CreateExtraLandInfoItems(row, col : integer; createluminfo : boolean);
  var
    i, j   : integer;
    rr, cc : integer;

  procedure CalcLandHeightInfo(i, j : integer; out landheightinfo : TLandHeightInfo);
    var
      obj           : TObjInfo;
      x1, x2, x3    : integer;
      y1, y2, y3    : integer;
      z1, z2, z3    : integer;
      v1x, v1y, v1z : integer; // triangle-side vectors
      v2x, v2y, v2z : integer;
    begin // this is still unefficient, a lot of things can be simplified
      x1 := 2*cBasicU;
      y1 := 0;
      if GetGroundInfo(i + 1, j, nil, obj)
        then z1 := obj.Height
        else z1 := 0;
      x2 := 2*cBasicU;
      y2 := 2*cBasicU;
      if GetGroundInfo(i, j - 1, nil, obj)
        then z2 := obj.Height
        else z2 := 0;
      x3 := 0;
      y3 := cBasicU;
      if GetGroundInfo(i + 1, j - 1, nil, obj)
        then z3 := obj.Height
        else z3 := 0;
      v1x := x1 - x3;
      v1y := y1 - y3;
      v1z := z1 - z3;
      v2x := x2 - x3;
      v2y := y2 - y3;
      v2z := z2 - z3;
      with landheightinfo do
        begin
          lnx := v1y*v2z - v1z*v2y;
          lny := v1z*v2x - v1x*v2z;
          lnz := v1x*v2y - v1y*v2x;
          ld  := -lnx*x1 - lny*y1 - lnz*z1;
        end;
      x3 := 4*cBasicU;
      y3 := cBasicU;
      if GetGroundInfo(i, j, nil, obj)
        then z3 := obj.Height
        else z3 := 0;
      v1x := x1 - x3;
      v1y := y1 - y3;
      v1z := z1 - z3;
      v2x := x2 - x3;
      v2y := y2 - y3;
      v2z := z2 - z3;
      with landheightinfo do
        begin
          rnx := v2y*v1z - v2z*v1y;
          rny := v2z*v1x - v2x*v1z;
          rnz := v2x*v1y - v2y*v1x;
          rd  := -rnx*x1 - rny*y1 - rnz*z1;
        end;
    end;

  procedure CalcLandLumInfo(i, j : integer; out landluminfo : TLandLuminanceInfo);
    var
      lightx : single;
      lighty : single;
      lightz : single;
      lmod   : single;
    var
      heightinfo    : TLandHeightInfo;
      nmod          : single;
      nx, ny, nz    : integer;
      i1x, i1y, i1z : single;
      i2x, i2y, i2z : single;
      i3x, i3y, i3z : single;
      v1x, v1y, v1z : single; // intensity triangle-side vectors
      v2x, v2y, v2z : single;
    begin
      {
      lightx := -4;
      lighty := 3;
      lightz := 3;
      }
      lightx := -4;
      lighty := 3;
      lightz := 4;
      lmod := sqrt(lightx*lightx + lighty*lighty + lightz*lightz);
      lightx := lightx/lmod;
      lighty := lighty/lmod;
      lightz := lightz/lmod;
      if GetLandHeightInfo(i + 1, j + 1, heightinfo)
        then
          begin
            nx := heightinfo.lnx + heightInfo.rnx;
            ny := heightinfo.lny + heightInfo.rny;
            nz := heightinfo.lnz + heightInfo.rnz;
          end
        else
          begin
            nx := 0;
            ny := 0;
            nz := 8*cBasicU*cBasicU;
          end;
      if GetLandHeightInfo(i, j + 1, heightinfo)
        then
          begin
            inc(nx, heightinfo.lnx);
            inc(ny, heightinfo.lny);
            inc(nz, heightinfo.lnz);
          end
        else inc(nz, 4*cBasicU*cBasicU);
      if GetLandHeightInfo(i, j, heightinfo)
        then
          begin
            inc(nx, heightinfo.lnx + heightinfo.rnx);
            inc(ny, heightinfo.lny + heightinfo.rny);
            inc(nz, heightinfo.lnz + heightinfo.rnz);
          end
        else inc(nz, 8*cBasicU*cBasicU);
      if GetLandHeightInfo(i + 1, j, heightinfo)
        then
          begin
            inc(nx, heightinfo.rnx);
            inc(ny, heightinfo.rny);
            inc(nz, heightinfo.rnz);
          end
        else inc(nz, 4*cBasicU*cBasicU);
      i1x := nx/6;
      i1y := ny/6;
      i1z := nz/6;
      nmod := sqrt(i1x*i1x + i1y*i1y + i1z*i1z);
      if nmod <> 0
        then i1z := realmax(0, (lightx*i1x + lighty*i1y + lightz*i1z)/nmod)
        else i1z := 0;
      i1x := 2*cBasicU;
      i1y := 0;
      if GetLandHeightInfo(i, j, heightinfo)
        then
          begin
            nx := heightinfo.lnx + heightInfo.rnx;
            ny := heightinfo.lny + heightInfo.rny;
            nz := heightinfo.lnz + heightInfo.rnz;
          end
        else
          begin
            nx := 0;
            ny := 0;
            nz := 8*cBasicU*cBasicU;
          end;
      if GetLandHeightInfo(i - 1, j, heightinfo)
        then
          begin
            inc(nx, heightinfo.lnx);
            inc(ny, heightinfo.lny);
            inc(nz, heightinfo.lnz);
          end
        else inc(nz, 4*cBasicU*cBasicU);
      if GetLandHeightInfo(i - 1, j - 1, heightinfo)
        then
          begin
            inc(nx, heightinfo.lnx + heightinfo.rnx);
            inc(ny, heightinfo.lny + heightinfo.rny);
            inc(nz, heightinfo.lnz + heightinfo.rnz);
          end
        else inc(nz, 8*cBasicU*cBasicU);
      if GetLandHeightInfo(i, j - 1, heightinfo)
        then
          begin
            inc(nx, heightinfo.rnx);
            inc(ny, heightinfo.rny);
            inc(nz, heightinfo.rnz);
          end
        else inc(nz, 4*cBasicU*cBasicU);
      i2x := nx/6;
      i2y := ny/6;
      i2z := nz/6;
      nmod := sqrt(i2x*i2x + i2y*i2y + i2z*i2z);
      if nmod <> 0
        then i2z := realmax(0, (lightx*i2x + lighty*i2y + lightz*i2z)/nmod)
        else i2z := 0;
      i2x := 2*cBasicU;
      i2y := 2*cBasicU;
      if GetLandHeightInfo(i + 1, j, heightinfo)
        then
          begin
            nx := heightinfo.lnx + heightInfo.rnx;
            ny := heightinfo.lny + heightInfo.rny;
            nz := heightinfo.lnz + heightInfo.rnz;
          end
        else
          begin
            nx := 0;
            ny := 0;
            nz := 8*cBasicU*cBasicU;
          end;
      if GetLandHeightInfo(i, j, heightinfo)
        then
          begin
            inc(nx, heightinfo.lnx);
            inc(ny, heightinfo.lny);
            inc(nz, heightinfo.lnz);
          end
        else inc(nz, 4*cBasicU*cBasicU);
      if GetLandHeightInfo(i, j - 1, heightinfo)
        then
          begin
            inc(nx, heightinfo.lnx + heightinfo.rnx);
            inc(ny, heightinfo.lny + heightinfo.rny);
            inc(nz, heightinfo.lnz + heightinfo.rnz);
          end
        else inc(nz, 8*cBasicU*cBasicU);
      if GetLandHeightInfo(i + 1, j - 1, heightinfo)
        then
          begin
            inc(nx, heightinfo.rnx);
            inc(ny, heightinfo.rny);
            inc(nz, heightinfo.rnz);
          end
        else inc(nz, 4*cBasicU*cBasicU);
      i3x := nx/6;
      i3y := ny/6;
      i3z := nz/6;
      nmod := sqrt(i3x*i3x + i3y*i3y + i3z*i3z);
      if nmod <> 0
        then i3z := realmax(0, (lightx*i3x + lighty*i3y + lightz*i3z)/nmod)
        else i3z := 0;
      i3x := 0;
      i3y := cBasicU;
      v1x := i1x - i3x;
      v1y := i1y - i3y;
      v1z := i1z - i3z;
      v2x := i2x - i3x;
      v2y := i2y - i3y;
      v2z := i2z - i3z;
      with landluminfo do
        begin
          lluma := v1y*v2z - v1z*v2y;
          llumb := v1z*v2x - v1x*v2z;
          llumc := v1x*v2y - v1y*v2x;
          llumd := -lluma*i1x - llumb*i1y - llumc*i1z;
        end;
      if GetLandHeightInfo(i, j + 1, heightinfo)
        then
          begin
            nx := heightinfo.lnx + heightInfo.rnx;
            ny := heightinfo.lny + heightInfo.rny;
            nz := heightinfo.lnz + heightInfo.rnz;
          end
        else
          begin
            nx := 0;
            ny := 0;
            nz := -8*cBasicU*cBasicU;
          end;
      if GetLandHeightInfo(i - 1, j + 1, heightinfo)
        then
          begin
            inc(nx, heightinfo.lnx);
            inc(ny, heightinfo.lny);
            inc(nz, heightinfo.lnz);
          end
        else inc(nz, -4*cBasicU*cBasicU);
      if GetLandHeightInfo(i - 1, j, heightinfo)
        then
          begin
            inc(nx, heightinfo.lnx + heightinfo.rnx);
            inc(ny, heightinfo.lny + heightinfo.rny);
            inc(nz, heightinfo.lnz + heightinfo.rnz);
          end
        else inc(nz, -8*cBasicU*cBasicU);
      if GetLandHeightInfo(i, j, heightinfo)
        then
          begin
            inc(nx, heightinfo.rnx);
            inc(ny, heightinfo.rny);
            inc(nz, heightinfo.rnz);
          end
        else inc(nz, -4*cBasicU*cBasicU);
      i3x := nx/6;
      i3y := ny/6;
      i3z := nz/6;
      nmod := sqrt(i3x*i3x + i3y*i3y + i3z*i3z);
      if nmod <> 0
        then i3z := realmax(0, (lightx*i3x + lighty*i3y + lightz*i3z)/nmod)
        else i3z := 0;
      i3x := 4*cBasicU;
      i3y := cBasicU;
      v1x := i1x - i3x;
      v1y := i1y - i3y;
      v1z := i1z - i3z;
      v2x := i2x - i3x;
      v2y := i2y - i3y;
      v2z := i2z - i3z;
      with landluminfo do
        begin
          rluma := v1y*v2z - v1z*v2y;
          rlumb := v1z*v2x - v1x*v2z;
          rlumc := v1x*v2y - v1y*v2x;
          rlumd := -rluma*i1x - rlumb*i1y - rlumc*i1z;
        end;
    end;

  begin
    with fExtraLandInfoBlocks[row, col] do
      begin
        rr := row shl cBlockBits;
        cc := col shl cBlockBits;
        new(fItems);
        for i := rr to rr + pred(cBlockSize) do
          for j := cc to cc + pred(cBlockSize) do
            CalcLandHeightInfo(i, j, fItems[i - rr, j - cc].heightinfo);
        if createluminfo
          then
            for i := rr to rr + pred(cBlockSize) do
              for j := cc to cc + pred(cBlockSize) do
                CalcLandLumInfo(i, j, fItems[i - rr, j - cc].luminfo);
        validlum := createluminfo;
      end;
  end;

procedure TWorldMap.DestroyExtraLandInfoItems(row, col : integer);
  begin
    with fExtraLandInfoBlocks[row, col] do
      dispose(fItems);
  end;

function TWorldMap.GetLandHeightInfo(i, j : integer; out heightinfo : TLandHeightInfo) : boolean;
  var
    row, col : integer;
  begin
    Result := (i >= 0) and (i < fRows) and (j >= 0) and (j < fColumns);
    if Result
      then
        begin
          fillchar(heightinfo, sizeof(heightinfo), 0);
          row := i shr cBlockBits;
          col := j shr cBlockBits;
          if fExtraLandInfoBlocks[row, col].fItems = nil
            then CreateExtraLandInfoItems(row, col, false);
          heightinfo := fExtraLandInfoBlocks[row, col].fItems[i and cBlockMask, j and cBlockMask].heightinfo;
        end;
  end;

procedure TWorldMap.InitViewOrigin(const view : IGameView);
  var
    x, y : integer;
    size : TPoint;
  begin
    fConverter.MapToScreen(view, fRows div 2, fColumns div 2, x, y);
    size := view.GetSize;
    view.Origin := Point(x - size.x div 2, y - size.y div 2);
  end;

// TGameFocus

constructor TGameFocus.Create(Map : TWorldMap; const view : IGameView);
  begin
    inherited Create;
    fMap  := Map;
    fView := view;
    fImager := nil;
    fZoomFactor := 2 shl fView.ZoomLevel / cBasicU;
    Map.fFocuses.Add(Self);
  end;

destructor TGameFocus.Destroy;
  begin
    fMap.fFocuses.Remove(Self);
    inherited;
  end;

function TGameFocus.Lock : integer;
  begin
    inc(fLockCount);
    Result := fLockCount;
  end;

function TGameFocus.Unlock : integer;
  begin
    assert(fLockCount > 0);
    dec(fLockCount);
    if (fLockCount = 0) and fUpdateDefered
      then fView.QueryUpdate(false);
    Result := fLockCount;
  end;

function TGameFocus.LockCount : integer;
  begin
    Result := fLockCount;
  end;

procedure TGameFocus.QueryUpdate(Defer : boolean);
  begin
    if (fLockCount > 0) and Defer
      then fUpdateDefered := true
      else fView.QueryUpdate(false);
  end;

procedure TGameFocus.MouseMove(x, y : integer);
  begin
    fMouseX := x;
    fMouseY := y;
  end;

procedure TGameFocus.MouseClick;
  begin
  end;

procedure TGameFocus.KeyPressed(which : word; const Shift : TShiftState);
  begin
  end;

procedure TGameFocus.Refresh;
  begin
  end;

function TGameFocus.GetText(kind : integer) : string;
  begin
    assert(kind = fkSelection);
    Result := Selection.Text;
  end;

function TGameFocus.GetObject : TObject;
  begin
    Result := Self;
  end;

function TGameFocus.GetRect : TRect;
  var
    org : TPoint;
  begin
    org := fView.Origin;
    Result := Selection.TextRect;
    if not IsRectEmpty(Result)
      then OffsetRect(Result, -org.x, -org.y);
  end;

procedure TGameFocus.SetRect(const R : TRect);
  begin
    Selection.TextRect := R;
  end;

function TGameFocus.GetImager : IImager;
  begin
    if fImager = nil
      then fImager := fMap.fImageCache.GetImager(fView);
    Result := fImager;
  end;

procedure TGameFocus.ViewZoomed(var msg : TViewZoomedMsg);
  var
    ci, cj    : integer;
    movetomsg : TMoveToMsg;
  begin
    fImager := nil;
    fMap.fConverter.ScreenToMap(fView, fView.Size.x div 2, fView.Size.y div 2, ci, cj);
    fView.ZoomLevel := msg.Zoom;
    fZoomFactor := 2 shl fView.ZoomLevel/cBasicU;
    movetomsg.id := msgMoveTo;
    movetomsg.i := ci;
    movetomsg.j := cj;
    Dispatch(movetomsg);
  end;

procedure TGameFocus.MoveTo(var msg : TMoveToMsg);
  var
    x, y : integer;
    org  : TPoint;
    size : TPoint;
  begin
    with msg do
      begin
        fMap.fConverter.MapToScreen(fView, i, j, x, y);
        org  := fView.Origin;
        size := fView.GetSize;
        inc(org.x, x - size.x div 2);
        inc(org.y, y - size.y div 2);
        fView.Origin := org;
      end;
  end;

end.
