unit Car;

interface

  uses
    Windows, GameTypes, LanderTypes, SoundTypes, Circuits, Roads, MapSprites, MapTypes,
    Protocol, Railroads, VCLUtils;

  const
    carNone = high(TCarId);

  const
    cCarSpeed        = 0.5;  // blocks x second
    cCarFramesPerSec = 10;
    cCarsFrameDelay  = 1000 div cCarFramesPerSec;

  type
    TBlockRoutes = array [TRotation] of TSpriteRoute;

  type
    ICar =
      interface(IMapSprite)
        function GetCargo       : TCargoKind;
        function GetRoadSide    : TRoadSide;
        function GetOldRoadSide : TRoadSide;
        function GetInstanceIdx : integer;

        function GetNextDir : TCarDir;
        function PredictDir : TCarDir;
        property Cargo       : TCargoKind read GetCargo;
        property RoadSide    : TRoadSide  read GetRoadSide;
        property OldRoadSide : TRoadSide  read GetOldRoadSide;
        property InstanceIdx : integer    read GetInstanceIdx;
        procedure ClearInterfaced;
      end;

  type
    ICarManager =
      interface(IMapSpriteManager)
        procedure RegionUpdated(imin, jmin, imax, jmax : integer);
        function  GetNextDir(const Car : ICar; out nextdir : TCarDir) : boolean;
        function  GetRoadUnderCar(const Car : ICar) : TRoadBlockId;
        procedure GetCarRoutes(const Car : ICar; out routes : TBlockRoutes);
        function  IsFreeRoad(const Car : ICar; i, j : integer; RoadSide : TRoadSide) : boolean;
        function  CheckVerticalShift(i, j : integer) : boolean;
        procedure DestroyAll;
      end;

  type
    TCar =
      class(TInterfacedObject, ICar)
        private
          fId              : integer;
          fFrame           : integer;
          fMapPos          : TPoint;
          fOldMapPos       : TPoint;
          fBlocksToMove    : integer;
          fTurnsMade       : integer;
          fLastMoveTicks   : cardinal;
          fManager         : ICarManager;
          fCargo           : TCargoKind;
          fInstanceIdx     : integer;
          fRoadSide        : TRoadSide;
          fOldRoadSide     : TRoadSide;
          fBlockRoutes     : TBlockRoutes;
          fCurNode         : integer;    // este es el cursor dentro del bloque
          fOldBlockRoutes  : TBlockRoutes;
          fOldNode         : integer;
          fNextDir         : TCarDir;
          fNextDirValid    : boolean;
          fPredictedDir    : TCarDir;
          fPredDirValid    : boolean;
          fLastFrameUpdate : integer;
          fZoom            : TZoomRes;  // last zoom seen
          fRotation        : TRotation; // last rotation seen
          fImages          : array[TZoomRes, TRotation] of TSpriteImages;
        private
          procedure UpdateFrame;
          procedure Move(var FreeCar : boolean);
          function  GetVerticalShift : integer;
          function  GetOldVerticalShift : integer;
        public
          constructor Create(Id : integer; const IniMapPos : TPoint; BlocksToMove : integer; const Manager : ICarManager; CarClass : PCarClass);
          destructor  Destroy; override;
        private // IMapSprite
          function  GetId : integer;
          function  GetFrame : integer;
          function  GetAngle : TAngle;
          function  GetMapX : integer;
          function  GetMapY : integer;
          function  GetOldMapX : integer;
          function  GetOldMapY : integer;
          procedure AnimationTick;
          procedure NewView(const View : IGameView);
          function  GetBlockX(const View : IGameView) : integer;
          function  GetBlockY(const View : IGameView) : integer;
          function  GetOldBlockX(const View : IGameView) : integer;
          function  GetOldBlockY(const View : IGameView) : integer;
          function  GetWidth(const View : IGameView) : integer;
          function  GetHeight(const View : IGameView) : integer;
        private // ICar
          function  GetCargo       : TCargoKind;
          function  GetRoadSide    : TRoadSide;
          function  GetOldRoadSide : TRoadSide;
          function  GetInstanceIdx : integer;
          function  GetNextDir     : TCarDir;
          function  PredictDir     : TCarDir;
          procedure ClearInterfaced;
      end;

  type
    PCarInstances = ^TCarInstances;
    TCarInstances = array [0..pred(high(TCarId))] of pointer;

  type
    PCarClasses = ^TCarClasses;
    TCarClasses = array [0..0] of TCarClass;

  type
    TCarClassSoundInfo =
      record
        SoundTarget : ISoundTarget;
        sumr        : integer;
        sumc        : integer;
        carcnt      : integer;
      end;

  type
    PCarClassSoundInfoArray = ^TCarClassSoundInfoArray;
    TCarClassSoundInfoArray = array [0..0] of TCarClassSoundInfo;

  type
    TCarManager =
      class(TMapSpriteManager, ICarManager)
        public
          constructor Create(const Map : IWorldMap; const Converter : ICoordinateConverter; const Manager : ILocalCacheManager; MinAnimInterval : integer; OwnsSprites : boolean);
          destructor  Destroy; override;
          procedure   DestroyAll;
        protected // IMapSpriteManager
          function  GetSpriteCount : integer; override;
          function  GetSprite(i : integer) : IMapSprite; override;
          procedure SpriteMapPosChanged(const Sprite : IMapSprite); override;
          function  RegisterSprite(const Sprite : IMapSprite) : integer; override;
          procedure UnregisterSprite(const Sprite : IMapSprite); override;
          procedure RecacheSoundTargets(soundsenabled : boolean); override;
          procedure Disable; override;
        private // ICarsManager
          procedure RegionUpdated(imin, jmin, imax, jmax : integer);
          function  GetNextDir(const Car : ICar; out nextdir : TCarDir) : boolean;
          function  GetRoadUnderCar(const Car : ICar) : TRoadBlockId;
          procedure GetCarRoutes(const Car : ICar; out routes : TBlockRoutes);
          function  IsFreeRoad(const Car : ICar; i, j : integer; RoadSide : TRoadSide) : boolean;
          function  CheckVerticalShift(i, j : integer) : boolean;
        private
          fMaxCars      : integer;
          fAliveCars    : integer;
          fCarInstCount : integer;
          fCarInstAlloc : integer;
          fCarInstances : PCarInstances;
          fTicks        : integer;
        private // valid classes cache
          fClassCount   : integer;
          fClasses      : PCarClasses;
          fClassesIndex : array [idCar] of integer;
        private // sounds
          fSoundInfo     : PCarClassSoundInfoArray;
          fSoundsEnabled : boolean;
        protected
          procedure AnimationTick; override;
          function  GetFullSpriteId(const Sprite : IMapSprite) : integer; override;
          procedure RegulateCarFlow(imin, jmin, imax, jmax : integer);
          procedure UpdateCarSounds;
      end;

  const
    cCarsTimerInterval  = cCarsFrameDelay;

implementation

  uses
    Classes, Concrete, math; //AxlDebug, //.rag

  const
    cTrafficRegInterval = 10000 div cCarsTimerInterval;
    cCarsSoundInterval  = cTrafficRegInterval div 3;

  const
    cStoppedCarDeathDelay = 6000;

  // Utils
  procedure boundvalue(const min, max : single; var value : single);
    begin
      if value < min
        then value := min;
      if value > max
        then value := max;
    end;

  // TCar
  constructor TCar.Create(Id : integer; const IniMapPos : TPoint; BlocksToMove : integer; const Manager : ICarManager; CarClass : PCarClass);
    var
      roadblockid : TRoadBlockId;
    begin
      inherited Create;
      fId := Id;
      fMapPos := IniMapPos;
      fOldMapPos := fMapPos;
      fBlocksToMove := BlocksToMove;
      fManager := Manager;
      roadblockid := fManager.GetRoadUnderCar(Self);
      fCargo := CarClass.Cargo;
      if roadblockid in cNorthPointingBlocks
        then fRoadSide := rsEast
        else
          if roadblockid in cSouthPointingBlocks
            then fRoadSide := rsWest
            else
              if roadblockid in cEastPointingBlocks
                then fRoadSide := rsSouth
                else fRoadSide := rsNorth;
      fNextDirValid := fManager.GetNextDir(Self, fNextDir);
      fManager.GetCarRoutes(Self, fBlockRoutes);
      fInstanceIdx := fManager.RegisterSprite(ICar(Self));
      fManager.UpdateSpriteRect(ICar(Self));
    end;

  destructor TCar.Destroy;
    begin
      {if fManager <> nil
        then fManager.UnregisterSprite(ICar(Self));}
      //pointer(fManager) := nil; // >> cross referenced   //.rag
      inherited;
    end;

  function TCar.GetId : integer;
    begin
      Result := fId;
    end;

  function TCar.GetFrame : integer;
    begin
      Result := fFrame;
    end;

  function TCar.GetAngle : TAngle;
    begin
      if fBlockRoutes[fRotation].Nodes <> nil
        then Result := fBlockRoutes[fRotation].Nodes[fCurNode].angle
        else Result := low(fBlockRoutes[fRotation].Nodes[fCurNode].angle);
    end;

  function TCar.GetMapX : integer;
    begin
      Result := fMapPos.X;
    end;

  function  TCar.GetMapY : integer;
    begin
      Result := fMapPos.Y;
    end;

  function TCar.GetOldMapX : integer;
    begin
      Result := fOldMapPos.X;
    end;

  function TCar.GetOldMapY : integer;
    begin
      Result := fOldMapPos.Y;
    end;

  procedure TCar.AnimationTick;
    var
      FreeCar  : boolean;
      OldFrame : integer;
    begin
      fOldBlockRoutes := fBlockRoutes;
      fOldMapPos := fMapPos;
      fOldNode := fCurNode;
      FreeCar := false;
      if fCurNode < pred(pred(fBlockRoutes[fRotation].Count))
        then inc(fCurNode)
        else Move(FreeCar);
      OldFrame := fFrame;
      UpdateFrame;
      if ((fCurNode <> fOldNode) or (OldFrame <> fFrame) or FreeCar)and (fManager <> nil)
        then fManager.UpdateSpriteRect(ICar(Self));
      if FreeCar
        then 
          begin
            if fManager <> nil
              then fManager.UnregisterSprite(ICar(Self));
          end;
    end;

  procedure TCar.NewView(const View : IGameView);
    begin
      fFrame := 0;
      fillchar(fImages[fZoom, fRotation], sizeof(fImages[fZoom, fRotation]), 0);  //.rag (Que es esto)
      fZoom := TZoomRes(View.ZoomLevel);
      fRotation := View.Rotation;
      if fBlockRoutes[fRotation].Nodes <> nil
        then
          if fImages[fZoom, fRotation][fBlockRoutes[fRotation].Nodes[fCurNode].angle] = nil
            then fManager.GetSpriteImages(ICar(Self), View, fImages[fZoom, fRotation]);
    end;

  function TCar.GetBlockX(const View : IGameView) : integer;
    begin
      fZoom := TZoomRes(View.ZoomLevel);
      fRotation := View.Rotation;
      if fBlockRoutes[fRotation].Nodes <> nil
        then Result := fBlockRoutes[fRotation].Nodes[fCurNode].x*cZoomFactors[fZoom].m div cZoomFactors[fZoom].n - GetWidth(View) div 2
        else Result := 0;
    end;

  function TCar.GetBlockY(const View : IGameView) : integer;
    begin
      fZoom := TZoomRes(View.ZoomLevel);
      fRotation := View.Rotation;
      if fBlockRoutes[fRotation].Nodes <> nil
        then Result := (fBlockRoutes[fRotation].Nodes[fCurNode].y + GetVerticalShift)*cZoomFactors[fZoom].m div cZoomFactors[fZoom].n - GetHeight(View)
        else Result := 0;
    end;

  function TCar.GetOldBlockX(const View : IGameView) : integer;
    begin
      fZoom := TZoomRes(View.ZoomLevel);
      fRotation := View.Rotation;
      if fOldBlockRoutes[fRotation].Nodes <> nil
        then Result := fOldBlockRoutes[fRotation].Nodes[fOldNode].x*cZoomFactors[fZoom].m div cZoomFactors[fZoom].n - GetWidth(View) div 2
        else Result := 0;
    end;

  function TCar.GetOldBlockY(const View : IGameView) : integer;
    begin
      fZoom := TZoomRes(View.ZoomLevel);
      fRotation := View.Rotation;
      if fOldBlockRoutes[fRotation].Nodes <> nil
        then Result := (fOldBlockRoutes[fRotation].Nodes[fOldNode].y + GetOldVerticalShift)*cZoomFactors[fZoom].m div cZoomFactors[fZoom].n - GetHeight(View)
        else Result := 0;
    end;

  function TCar.GetWidth(const View : IGameView) : integer;
    begin
      fRotation := View.Rotation;
      if fBlockRoutes[fRotation].Nodes <> nil
        then
          begin
            fZoom := TZoomRes(View.ZoomLevel);
            fRotation := View.Rotation;
            if fImages[fZoom, fRotation][fBlockRoutes[fRotation].Nodes[fCurNode].angle] = nil
              then fManager.GetSpriteImages(ICar(Self), View, fImages[fZoom, fRotation]);
            if fImages[fZoom, fRotation][fBlockRoutes[fRotation].Nodes[fCurNode].angle] <> nil
              then Result := fImages[fZoom, fRotation][fBlockRoutes[fRotation].Nodes[fCurNode].angle].Width
              else Result := 0;
          end
        else Result := 0;
    end;

  function TCar.GetHeight(const View : IGameView) : integer;
    begin
      fRotation := View.Rotation;
      if fBlockRoutes[fRotation].Nodes <> nil
        then
          begin
            fZoom := TZoomRes(View.ZoomLevel);
            fRotation := View.Rotation;
            if fImages[fZoom, fRotation][fBlockRoutes[fRotation].Nodes[fCurNode].angle] = nil
              then fManager.GetSpriteImages(ICar(Self), View, fImages[fZoom, fRotation]);
            if fImages[fZoom, fRotation][fBlockRoutes[fRotation].Nodes[fCurNode].angle] <> nil
              then Result := fImages[fZoom, fRotation][fBlockRoutes[fRotation].Nodes[fCurNode].angle].Height
              else Result := 0;
          end
        else Result := 0;
    end;

  function TCar.GetCargo : TCargoKind;
    begin
      Result := fCargo;
    end;

  function TCar.GetRoadSide : TRoadSide;
    begin
      Result := fRoadSide;
    end;

  function TCar.GetOldRoadSide : TRoadSide;
    begin
      Result := fOldRoadSide;
    end;

  function TCar.GetInstanceIdx : integer;
    begin
      Result := fInstanceIdx;
    end;

  function TCar.GetNextDir : TCarDir;
    begin
      Result := fNextDir;
    end;

  function TCar.PredictDir : TCarDir;
    var
      OldMapPos   : TPoint;
      OldRoadSide : TRoadSide;
    begin
      OldMapPos := fMapPos;
      OldRoadSide := fRoadSide;
      case fNextDir of
        cdNorth:
          begin
            inc(fMapPos.Y);
            fRoadSide := rsEast;
          end;
        cdSouth:
          begin
            dec(fMapPos.Y);
            fRoadSide := rsWest;
          end;
        cdEast:
          begin
            inc(fMapPos.X);
            fRoadSide := rsSouth;
          end;
        cdWest:
          begin
            dec(fMapPos.X);
            fRoadSide := rsNorth;
          end;
      end;
      fPredDirValid := fManager.GetNextDir(ICar(Self), fPredictedDir);
      fMapPos := OldMapPos;
      fRoadSide := OldRoadSide;
      Result := fPredictedDir;
    end;

  procedure TCar.ClearInterfaced;
    begin
      fManager := nil;
   end;

  procedure TCar.UpdateFrame;
    var
      ElapsedTicks : integer;
      CurrentTick  : integer;
      Img          : TGameImage;
    begin
      if fBlockRoutes[fRotation].Nodes <> nil
        then
          begin
            Img := fImages[fZoom, fRotation][fBlockRoutes[fRotation].Nodes[fCurNode].angle];
            if (Img <> nil) and (Img.FrameCount > 1)
              then
                begin
                  CurrentTick := GetTickCount;
                  if fFrame >= Img.FrameCount
                    then
                      begin
                        fFrame := 0;
                        fLastFrameUpdate := CurrentTick;
                      end;
                  ElapsedTicks := CurrentTick - fLastFrameUpdate;
                  if ElapsedTicks >= Img.FrameDelay[fFrame]
                    then
                      begin
                        if fFrame < pred(Img.FrameCount)
                          then inc(fFrame)
                          else fFrame := 0;
                        fLastFrameUpdate := CurrentTick;
                      end;
                end;
          end;
    end;

  procedure TCar.Move(var FreeCar : boolean);
    const
      cBlocksToMoveInc = 5; // 32
      cMaxTurnsAllowed = 5;
    var
      PrevDir : TCarDir;
    begin
      if (fBlocksToMove = 0) and ((fTurnsMade < cMaxTurnsAllowed) or fManager.IsSpriteVisible(ICar(Self)))
        then inc(fBlocksToMove, cBlocksToMoveInc);
      if (fBlocksToMove > 0) and fNextDirValid
        then
          begin
            fOldRoadSide := fRoadSide;
            case fNextDir of
              cdNorth:
                if fManager.IsFreeRoad(Self, fMapPos.Y + 1, fMapPos.X, rsEast)
                  then
                    begin
                      inc(fMapPos.Y);
                      fRoadSide := rsEast;
                    end;
              cdSouth:
                if fManager.IsFreeRoad(Self, fMapPos.Y - 1, fMapPos.X, rsWest)
                  then
                    begin
                      dec(fMapPos.Y);
                      fRoadSide := rsWest;
                    end;
              cdEast:
                if fManager.IsFreeRoad(Self, fMapPos.Y, fMapPos.X + 1, rsSouth)
                  then
                    begin
                      inc(fMapPos.X);
                      fRoadSide := rsSouth;
                    end;
              cdWest:
                if fManager.IsFreeRoad(Self, fMapPos.Y, fMapPos.X - 1, rsNorth)
                  then
                    begin
                      dec(fMapPos.X);
                      fRoadSide := rsNorth;
                    end;
            end;
            dec(fBlocksToMove);
          end;
      if (fOldMapPos.X <> fMapPos.X) or (fOldMapPos.Y <> fMapPos.Y) or not fNextDirValid
        then
          begin
            if (fOldMapPos.X <> fMapPos.X) or (fOldMapPos.Y <> fMapPos.Y)
              then
                begin
                  fLastMoveTicks := GetTickCount;
                  fManager.SpriteMapPosChanged(ICar(Self));
                end;
            PrevDir := fNextDir;
            if fPredDirValid or fManager.GetNextDir(ICar(Self), fNextDir)
              then
                begin
                  if PrevDir <> fNextDir
                    then inc(fTurnsMade);
                  if fPredDirValid
                    then
                      begin
                        fNextDir := fPredictedDir;
                        fPredDirValid := false;
                      end;
                  fManager.GetCarRoutes(Self, fBlockRoutes);
                  fCurNode := 0;
                  fNextDirValid := true;
                end
              else // car reached a dead end
                begin
                  {
                  if not fManager.IsSpriteVisible(ICar(Self))
                    then FreeCar := true;
                  }
                  FreeCar := true;
                  fNextDirValid := false;
                end;
          end
        else
          if (GetTickCount - fLastMoveTicks > cStoppedCarDeathDelay) or not fManager.IsSpriteVisible(ICar(Self))
            then FreeCar := true;
    end;

  function TCar.GetVerticalShift : integer;
    begin
      if fManager.CheckVerticalShift(fMapPos.y, fMapPos.x)
        then Result := -cPlatformShift
        else Result := 0;
    end;

  function TCar.GetOldVerticalShift : integer;
    begin
      if fManager.CheckVerticalShift(fOldMapPos.y, fOldMapPos.x)
        then Result := -cPlatformShift
        else Result := 0;
    end;

  // TCarClassSoundTarget

  type
    TCarClassSoundTarget =
      class(TInterfacedObject, ISoundTarget)
        public
          constructor Create(const Owner : IGameFocus; CarClass : PCarClass; const Converter : ICoordinateConverter);
          destructor  Destroy; override;
        private
          fOwner         : IGameFocus;
          fCarClass      : PCarClass;
          fConverter     : ICoordinateConverter;
          fPan           : single;
          fVolume        : single;
          fLastPlayed    : dword;
          fClassCentroid : TMapPoint;
        private // ISoundTarget
          function  GetSoundName  : string;
          function  GetSoundKind  : integer;
          function  GetPriority   : integer;
          function  IsLooped      : boolean;
          function  GetVolume     : single;
          function  GetPan        : single;
          function  ShouldPlayNow : boolean;
          function  IsCacheable   : boolean;
          function  IsEqualTo(const SoundTarget : ISoundTarget) : boolean;
          function  GetObject : TObject;
          procedure UpdateSoundParameters;
      end;

  // TCarClassSoundTarget

  constructor TCarClassSoundTarget.Create(const Owner : IGameFocus; CarClass : PCarClass; const Converter : ICoordinateConverter);
    begin
      inherited Create;
      fOwner := Owner;
      // fOwner._Release; // >> cross referenced //.RAG
      fCarClass := CarClass;
      fConverter := Converter;
      // fConverter._Release; // >> cross referenced //.RAG
    end;

  destructor TCarClassSoundTarget.Destroy;
    begin
      // pointer(fConverter) := nil; // >> cross referenced //.RAG
      //  pointer(fOwner) := nil; // >> cross referenced    //.RAG
      fCarClass := nil; //.rag
      inherited;
    end;

  function TCarClassSoundTarget.GetSoundName : string;
    begin
      try
        if (fCarClass<>nil)
          then Result := fCarClass.SoundData.wavefile;
      except
        Result := '';
      end;
    end;

  function TCarClassSoundTarget.GetSoundKind : integer;
    begin
      try
         Result := fCarClass.id;
      except
        result := 0;
      end;
    end;

  function TCarClassSoundTarget.GetPriority : integer;
    begin
      Result := fCarClass.SoundData.priority;
    end;

  function TCarClassSoundTarget.IsLooped : boolean;
    begin
      Result := fCarClass.SoundData.looped;
    end;

  function TCarClassSoundTarget.GetVolume : single;
    begin
      Result := fVolume*fCarClass.SoundData.atenuation;
    end;

  function TCarClassSoundTarget.GetPan : single;
    begin
      Result := fPan;
    end;

  function TCarClassSoundTarget.ShouldPlayNow : boolean;
    var
      ElapsedTicks : integer;
    begin
      with fCarClass.SoundData do
        if (not looped and (period <> 0)) or (fLastPlayed = 0)
          then
            begin
              ElapsedTicks := GetTickCount - fLastPlayed;
              if ElapsedTicks >= period
                then
                  begin
                    if random < probability
                      then Result := true
                      else Result := false;
                    fLastPlayed := GetTickCount;
                  end
                else Result := false;
            end
          else Result := false;
    end;

  function TCarClassSoundTarget.IsCacheable : boolean;
    begin
      Result := false;
    end;

  function TCarClassSoundTarget.IsEqualTo(const SoundTarget : ISoundTarget) : boolean;
    var
      SndTargetObj : TObject;
    begin
      SndTargetObj := SoundTarget.GetObject;
      if SndTargetObj is TCarClassSoundTarget
        then Result := fCarClass = TCarClassSoundTarget(SndTargetObj).fCarClass
        else Result := false;
    end;

  function TCarClassSoundTarget.GetObject : TObject;
    begin
      Result := Self;
    end;

  procedure TCarClassSoundTarget.UpdateSoundParameters;
    var
      view       : IGameView;
      screensize : TPoint;
      tmppt      : TPoint;
      x, y       : integer;
      u          : integer;
      ci, cj     : integer;
      Dist       : single;
    begin
      with fOwner do
        begin
          view := GetView;
          fConverter.MapToScreen(view, fClassCentroid.r, fClassCentroid.c, x, y);
          tmppt := view.ViewPtToScPt(Point(x, y));
          x := tmppt.x;
          y := tmppt.y;
          u := 2 shl view.ZoomLevel;
          x := x + 2*u;
          screensize := Point(GetSystemMetrics(SM_CXSCREEN), GetSystemMetrics(SM_CYSCREEN));
          if abs(x - screensize.x/2) > cPanDeadZone
            then fPan := cLeftPan + (cRightPan - cLeftPan)*x/screensize.x
            else fPan := cCenterPan;
          boundvalue(cLeftPan, cRightPan, fPan);
          tmppt := view.ScPtToViewPt(Point(screensize.x div 2, screensize.y div 2));
          fConverter.ScreenToMap(view, tmppt.x, tmppt.y, ci, cj);
          Dist := sqrt(sqr(ci - fClassCentroid.r) + sqr(cj - fClassCentroid.c));
          if Dist < cMaxHearDist
            then fVolume := cMinVol + (1 - Dist/cMaxHearDist)*(cMaxVol - cMinVol)*(1 - abs(view.ZoomLevel - ord(cBasicZoomRes))*cZoomVolStep)
            else fVolume := cMinVol;
          boundvalue(0, cMaxVol, fVolume);
        end;
    end;

  // TCarManager

  constructor TCarManager.Create(const Map : IWorldMap; const Converter : ICoordinateConverter; const Manager : ILocalCacheManager; MinAnimInterval : integer; OwnsSprites : boolean);
    var
      i        : integer;
      id       : idcar;
      CarClass : PCarClass;
    begin
      inherited;
      fInterval := cCarsTimerInterval;
      fClassCount := fManager.GetCarClassCount;
      getmem(fClasses, fClassCount*sizeof(fClasses[0]));
      getmem(fSoundInfo, fClassCount*sizeof(fSoundInfo[0]));
      initialize(fSoundInfo^, fClassCount);
      fSoundsEnabled := true;
      zeromemory( fClasses, fClassCount*sizeof(fClasses[0]));
      fillchar(fClassesIndex, sizeof(fClassesIndex), $FF);
      i := 0;
      for id := low(id) to high(id) do
        begin
          CarClass := fManager.GetCarClass(id);
          if (CarClass <> nil) and CarClass.valid
            then
              try
                fClasses[i] := CarClass^;
                fClassesIndex[CarClass.id] := i;
                inc(i);
              except
              end;
        end;
    end;

  destructor TCarManager.Destroy;
    begin
      DestroyAll;
      inherited;
    end;

  procedure TCarManager.DestroyAll; //.rag
    var
      i : integer;
    begin
      if fCarInstances<>nil
        then
          begin
            for i:=0 to fCarInstCount-1 do
              IMapSprite(fCarInstances[i]) := nil;
            FreememAndNil(fCarInstances);
          end;
      FreememAndNil(fClasses);
      if fSoundInfo<>nil
        then
          begin
            for i := pred(fClassCount) downto 0 do
              fSoundInfo[i].SoundTarget := nil;
            FreememAndNil(fSoundInfo);
          end;
    end;
    
  function TCarManager.GetSpriteCount : integer;
    begin
      Result := fCarInstCount;
    end;

  function TCarManager.GetSprite(i : integer) : IMapSprite;
    begin
      if (fCarInstances <> nil) and (i < fCarInstCount) and (fCarInstances[i] <> nil)
        then Result := IMapSprite(fCarInstances[i])
        else Result := nil;
    end;

  procedure TCarManager.SpriteMapPosChanged(const Sprite : IMapSprite);
    begin
      fMap.SetCar(Sprite.OldMapY, Sprite.OldMapX, ICar(Sprite).OldRoadSide, carNone);
      fMap.SetCar(Sprite.MapY, Sprite.MapX, ICar(Sprite).RoadSide, ICar(Sprite).InstanceIdx);
    end;

  function TCarManager.RegisterSprite(const Sprite : IMapSprite) : integer;
    var
      i          : integer;
      carinstidx : tcarid;
    begin
      i := 0;
      carinstidx := carNone;
      while (i < fCarInstCount) and (carinstidx = carNone) do
        if fCarInstances[i] = nil
          then
            begin
              carinstidx := i;
              IMapSprite(fCarInstances[carinstidx]) := Sprite;
              // Sprite._AddRef; //.rag
            end
          else inc(i);
      if carinstidx = carNone
        then
          begin
            if fCarInstCount = fCarInstAlloc
              then
                begin
                  inc(fCarInstAlloc, 5);
                  reallocmem(fCarInstances, fCarInstAlloc*sizeof(fCarInstances[0]));
                  fillchar(fCarInstances[fCarInstCount], (fCarInstAlloc - fCarInstCount)*sizeof(fCarInstances[0]), 0);
                end;
            carinstidx := fCarInstCount;
            IMapSprite(fCarInstances[carinstidx]) := Sprite;
            // Sprite._AddRef; //.rag
            inc(fCarInstCount);
          end;
      inc(fAliveCars);
      Result := carinstidx;
    end;

  procedure TCarManager.UnregisterSprite(const Sprite : IMapSprite);
    begin
      if (fCarInstances <> nil) and (ICar(Sprite).InstanceIdx < fCarInstCount)
        then
          begin
            fMap.SetCar(Sprite.MapY, Sprite.MapX, ICar(Sprite).RoadSide, carNone);
            ICar(Sprite).ClearInterfaced;
            IMapSprite(fCarInstances[ICar(Sprite).InstanceIdx]) := nil;
            dec(fAliveCars);
          end;
    end;

  procedure TCarManager.RecacheSoundTargets(soundsenabled : boolean);
    var
      i : integer;
    begin
      fSoundsEnabled := soundsenabled;
      for i := 0 to pred(fClassCount) do
        with fSoundInfo[i] do
          SoundTarget := nil;
      if fSoundsEnabled
        then UpdateCarSounds;
    end;

  procedure TCarManager.Disable;
    begin
      inherited;
      UpdateCarSounds;
    end;

  procedure TCarManager.RegionUpdated(imin, jmin, imax, jmax : integer);
    begin
      if not fExternallyDisabled
        then
          begin
            RegulateCarFlow(imin, jmin, imax, jmax);
            if fSoundsEnabled
              then UpdateCarSounds;
          end;
    end;

  function TCarManager.GetNextDir(const Car : ICar; out nextdir : TCarDir) : boolean;
    const
      cOppositeDir : array[TRoadSide] of TCarDir = (cdEast, cdWest, cdSouth, cdNorth);
    const
      cValidDir : array[TCarDir] of TRoadBlockIdSet = (cNorthPointingBlocks, cSouthPointingBlocks, cEastPointingBlocks, cWestPointingBlocks);
    const
      cTrivialDirRoadBlocks : set of TRoadBlockId = [rbNSRoad, rbWERoad]; // >> with a little thinking this could be extended to corners and to end and start blocks
      cTrivialDirs          : array [TRoadSide] of TCarDir = (cdWest, cdEast, cdNorth, cdSouth);
    var
      roadblockid : TRoadBlockId;
      roadside    : TRoadSide;
      dir         : TCarDir;
      dirprobs    : array [TCarDir] of single;
      threshprob  : single;
      base        : single;

    function saferandom : single;
      begin
        repeat
          Result := random;
        until Result > 0;
      end;

    {$IFDEF USECARGODATA}
    procedure CalcDirProbs;
      var
        cargodata  : PCargoData;
        cdir       : TCarDir;
        minslope   : single;
        slopessum  : single;
        validprobs : boolean;
      begin
        cargodata := fMap.GetCargoData(Car.MapY, Car.MapX);
        if cargodata <> cargoNone
          then
            begin
              dirprobs[low(cdir)] := cargodata.Slopes[low(cdir), Car.Cargo];
              minslope := dirprobs[low(cdir)];
              for cdir := succ(low(cdir)) to high(cdir) do
                begin
                  dirprobs[cdir] := cargodata.Slopes[cdir, Car.Cargo];
                  if dirprobs[cdir] < minslope
                    then minslope := dirprobs[cdir];
                end;
              slopessum := 0;
              for cdir := low(cdir) to high(cdir) do
                begin
                  dirprobs[cdir] := dirprobs[cdir] - minslope;
                  if (roadblockid in cValidDir[cdir]) and (cdir <> cOppositeDir[roadside])
                    then slopessum := slopessum + dirprobs[cdir]
                    else dirprobs[cdir] := 0;
                end;
              if slopessum > 0
                then
                  begin
                    validprobs := true;
                    for cdir := low(cdir) to high(cdir) do
                      dirprobs[cdir] := dirprobs[cdir]/slopessum;
                  end
                else validprobs := false;
            end
          else validprobs := false;
        if not validprobs
          then
            begin
              slopessum := 0;
              for cdir := low(cdir) to high(cdir) do
                begin
                  if (roadblockid in cValidDir[cdir]) and (cdir <> cOppositeDir[roadside])
                    then dirprobs[cdir] := 1
                    else dirprobs[cdir] := 0;
                  slopessum := slopessum + dirprobs[cdir];
                end;
              if slopessum > 0
                then
                  for cdir := low(cdir) to high(cdir) do
                    dirprobs[cdir] := dirprobs[cdir]/slopessum;
            end;
      end;
    {$ELSE}
    procedure CalcDirProbs;
      var
        slopessum : single;
        cdir      : TCarDir;
      begin
        slopessum := 0;
        for cdir := low(cdir) to high(cdir) do
          begin
            if (roadblockid in cValidDir[cdir]) and (cdir <> cOppositeDir[roadside])
              then dirprobs[cdir] := 1
              else dirprobs[cdir] := 0;
            slopessum := slopessum + dirprobs[cdir];
          end;
        if slopessum > 0
          then
            for cdir := low(cdir) to high(cdir) do
              dirprobs[cdir] := dirprobs[cdir]/slopessum;
      end;
    {$ENDIF}

    begin
      roadblockid := GetRoadUnderCar(Car);
      roadside := Car.RoadSide;
      if roadblockid in cTrivialDirRoadBlocks
        then
          begin
            nextdir := cTrivialDirs[roadside];
            Result := true;
          end
        else
          begin
            CalcDirProbs;
            threshprob := saferandom;
            base := 0;
            dir := low(dir);
            repeat
              base := base + dirprobs[dir];
              if base < threshprob
                then inc(dir);
            until (base >= threshprob) or (dir = high(dir));
            if dirprobs[dir] > 0
              then
                begin
                  nextdir := dir;
                  Result := true;
                end
              else Result := false;
          end;
    end;

  function TCarManager.GetRoadUnderCar(const Car : ICar) : TRoadBlockId;
    var
      roadblock : TRoad;
    begin
      roadblock := fMap.GetRoad(Car.MapY, Car.MapX);
      if roadblock <> roadNone
        then Result := RoadIdOf(roadblock)
        else Result := rbNone;
    end;

  procedure TCarManager.GetCarRoutes(const Car : ICar; out routes : TBlockRoutes);
    var
      roadblock : TRoad;
      rot       : TRotation;

    function RotateRoadSide(roadside : TRoadSide; rotation : TRotation) : TRoadSide;
      const
        cRotatedRoadSides : array [TRotation, TRoadSide] of TRoadSide =
          (
            (rsNorth, rsSouth, rsEast, rsWest),
            (rsWest, rsEast, rsNorth, rsSouth),
            (rsSouth, rsNorth, rsWest, rsEast),
            (rsEast, rsWest, rsSouth, rsNorth)
          );
      begin
        Result := cRotatedRoadSides[rotation, roadside];
      end;

    function RotateCarDir(dir : TCarDir; rotation : TRotation) : TCarDir;
      const
        cRotatedCarDirs : array [TRotation, TCarDir] of TCarDir =
          (
            (cdNorth, cdSouth, cdEast, cdWest),
            (cdWest, cdEast, cdNorth, cdSouth),
            (cdSouth, cdNorth, cdWest, cdEast),
            (cdEast, cdWest, cdSouth, cdNorth)
          );
      begin
        Result := cRotatedCarDirs[rotation, dir];
      end;

    begin
      roadblock := fMap.GetRoad(Car.MapY, Car.MapX) and not cDummyRoadMask;
      for rot := low(rot) to high(rot) do
        routes[rot] := fManager.GetRoadBlockClass(RotateRoadBlockId(roadblock, rot)).CarRoutes[RotateRoadSide(Car.RoadSide, rot), RotateCarDir(Car.GetNextDir, rot)];
    end;

  function TCarManager.IsFreeRoad(const Car : ICar; i, j : integer; RoadSide : TRoadSide) : boolean;
    const
      MultiDirBlocks : set of TRoadBlockId =
        [
          rbLeftPlug,
          rbRightPlug,
          rbTopPlug,
          rbBottomPlug,
          rbCrossRoads
        ];
    var
      roadblockid : TRoadBlockId;
      NSideCar    : ICar;
      SSideCar    : ICar;
      ESideCar    : ICar;
      WSideCar    : ICar;
      roadblock   : TRoad;

    function GetCarInstance(i, j : integer; roadside : TRoadSide) : ICar;
      var
        caridx : tcarid;
      begin
        caridx := fMap.GetCar(i, j, roadside);
        if caridx <> carNone
          then Result := ICar(GetSprite(caridx))
          else Result := nil;
      end;

    {
    function CheckForTrains : boolean;
      begin
        if fMap.GetRailroad(i, j) <> railroadNone
          then
            if roadblockid = rbNSRoad
              then Result := (fMap.GetTraincar(i, j - 1, 0) <> traincarNone) or (fMap.GetTraincar(i, j - 1, 1) <> traincarNone)
                             or (fMap.GetTraincar(i, j, 0) <> traincarNone) or (fMap.GetTraincar(i, j, 1) <> traincarNone)
                             or (fMap.GetTraincar(i, j + 1, 0) <> traincarNone) or (fMap.GetTraincar(i, j + 1, 1) <> traincarNone)
              else
                if roadblockid = rbWERoad
                  then Result := (fMap.GetTraincar(i - 1, j, 0) <> traincarNone) or (fMap.GetTraincar(i - 1, j, 1) <> traincarNone)
                                 or (fMap.GetTraincar(i, j, 0) <> traincarNone) or (fMap.GetTraincar(i, j, 1) <> traincarNone)
                                 or (fMap.GetTraincar(i + 1, j, 0) <> traincarNone) or (fMap.GetTraincar(i + 1, j, 1) <> traincarNone)
                  else Result := false
          else Result := false;
      end;
    }

    begin
      roadblock := fMap.GetRoad(i, j);
      if roadblock <> roadNone
        then roadblockid := RoadIdOf(roadblock)
        else roadblockid := rbNone;
      if roadblockid in MultiDirBlocks
        then
          begin
            NSideCar := GetCarInstance(i, j, rsNorth);
            SSideCar := GetCarInstance(i, j, rsSouth);
            ESideCar := GetCarInstance(i, j, rsEast);
            WSideCar := GetCarInstance(i, j, rsWest);
            Result := (roadblock <> roadNone) and ((NSideCar = nil) or (RoadSide = rsSouth) and (Car.PredictDir <> NSideCar.PredictDir)) and ((SSideCar = nil) or (RoadSide = rsNorth) and (Car.PredictDir <> SSideCar.PredictDir)) and ((ESideCar = nil) or (RoadSide = rsWest) and (Car.PredictDir <> ESideCar.PredictDir)) and ((WSideCar = nil) or (RoadSide = rsEast) and (Car.PredictDir <> WSideCar.PredictDir)) {and not CheckForTrains};
          end
        else Result := (roadblock <> roadNone) and (fMap.GetCar(i, j, RoadSide) = carNone) {and not CheckForTrains};
    end;

  function TCarManager.CheckVerticalShift(i, j : integer) : boolean;
    begin
      Result := fMap.CheckForWater(i, j) and fMap.CheckForConcrete(i, j);
    end;

  procedure TCarManager.AnimationTick;
    var
      i      : integer;
      imin   : integer;
      jmin   : integer;
      imax   : integer;
      jmax   : integer;
    begin
      inherited;
      if (fTicks mod cCarsSoundInterval = 0) and fSoundsEnabled
        then UpdateCarSounds;
      if fTicks = pred(cTrafficRegInterval)
        then
          begin
            for i := 0 to pred(fFocuses.Count) do
              begin
                fConverter.GetViewRegion(IGameFocus(fFocuses[i]).GetView, imin, jmin, imax, jmax);
                RegulateCarFlow(imin, jmin, imax, jmax);
              end;
            fTicks := 0;
          end
        else inc(fTicks);
    end;

  function TCarManager.GetFullSpriteId(const Sprite : IMapSprite) : integer;
    begin
      Result := idCarMask or Sprite.Id;
    end;

  procedure TCarManager.RegulateCarFlow(imin, jmin, imax, jmax : integer);
    const
      cMaxViewCars = 40;

    const
      cCarsXMargin = 3;
      cCarsYMargin = 3;

    procedure CreateCarsInRegion(imin, jmin, imax, jmax : integer; carstocreate : integer);

      function GenerateCarId(cargo : TCargoKind; out carid : idCar) : boolean;
        var
          selectedids    : array [idCar] of idCar;
          selectedidscnt : word;
          threshold      : single;
          maxprob        : single;
          i              : integer;
          carclass       : TCarClass;
        begin
          threshold := random;
          selectedidscnt := 0;
          maxprob := 0;
          for i := 0 to pred(fClassCount) do
            begin
              carclass := fClasses[i];
              if carclass.Cargo = cargo
                then
                  if carclass.Prob > threshold
                    then
                      begin
                        selectedids[selectedidscnt] := carclass.id;
                        inc(selectedidscnt);
                      end
                    else
                      if carclass.Prob > maxprob
                        then maxprob := carclass.Prob;
            end;
          if selectedidscnt = 0
            then
              for i := 0 to pred(fClassCount) do
                begin
                  carclass := fClasses[i];
                  if (carclass.Cargo = cargo) and (carclass.Prob = maxprob)
                    then
                      begin
                        selectedids[selectedidscnt] := carclass.id;
                        inc(selectedidscnt);
                      end;
                end;
          if selectedidscnt > 0
            then
              begin
                carid := selectedids[random(selectedidscnt)];
                Result := true;
              end
            else Result := false;
        end;

      function ThereIsRoad(i, j : integer) : boolean;
        var
          roadblock : TRoad;
        begin
          roadblock := fMap.GetRoad(i, j);
          if roadblock <> roadNone
            then Result := (roadblock and cDummyRoadMask) = 0
            else Result := false;
        end;

      {$IFDEF USECARGODATA}
      const
        cBlocksToMove = 32;
      const
        cSignificantCargo = 5;
      var
        i, j        : integer;
        carid       : idCar;
        carclass    : PCarClass;
        cargodata   : PCargoData;
        selcargos   : array [0..2] of TCargoKind;
        selcargocnt : integer;
        ck          : TCargoKind;
        createdcars : integer;
      begin
        if carstocreate > 0
          then
            with fMap do
              begin
                createdcars := 0;
                i := max(imin - cCarsYMargin, 0);
                while (i <= min(imin + cCarsYMargin, pred(GetRows))) and (createdcars < carstocreate) do
                  begin
                    j := max(jmin - cCarsXMargin, 0);
                    while (j <= min(jmax + cCarsXMargin, pred(GetColumns))) and (createdcars < carstocreate) do
                      begin
                        if ThereIsRoad(i, j)
                          then
                            begin
                              cargodata := fMap.GetCargoData(i, j);
                              if (cargodata <> cargoNone) and (fMap.GetCar(i, j, rsNorth) = carNone) and (fMap.GetCar(i, j, rsSouth) = carNone) and (fMap.GetCar(i, j, rsEast) = carNone) and (fMap.GetCar(i, j, rsWest) = carNone)
                                then
                                  begin
                                    selcargocnt := 0;
                                    for ck := low(ck) to high(ck) do
                                      if abs(cargodata.Cargos[ck]) > cSignificantCargo
                                        then
                                          begin
                                            selcargos[selcargocnt] := ck;
                                            inc(selcargocnt);
                                          end;
                                    if selcargocnt > 0
                                      then
                                        if GenerateCarId(selcargos[random(selcargocnt)], carid)
                                          then
                                            begin
                                              carclass := fManager.GetCarClass(carid);
                                              TCar.Create(carid, Point(j, i), cBlocksToMove, Self, carclass);
                                              inc(createdcars);
                                            end;
                                  end;
                            end;
                        inc(j);
                      end;
                    inc(i);
                  end;
                i := min(imax + cCarsYMargin, pred(GetRows));
                while (i >= max(imax - cCarsYMargin, 0)) and (createdcars < carstocreate) do
                  begin
                    j := max(jmin - cCarsXMargin, 0);
                    while (j <= min(jmax + cCarsXMargin, pred(GetColumns))) and (createdcars < carstocreate) do
                      begin
                        if ThereIsRoad(i, j)
                          then
                            begin
                              cargodata := fMap.GetCargoData(i, j);
                              if (cargodata <> cargoNone) and (fMap.GetCar(i, j, rsNorth) = carNone) and (fMap.GetCar(i, j, rsSouth) = carNone) and (fMap.GetCar(i, j, rsEast) = carNone) and (fMap.GetCar(i, j, rsWest) = carNone)
                                then
                                  begin
                                    selcargocnt := 0;
                                    for ck := low(ck) to high(ck) do
                                      if abs(cargodata.Cargos[ck]) > cSignificantCargo
                                        then
                                          begin
                                            selcargos[selcargocnt] := ck;
                                            inc(selcargocnt);
                                          end;
                                    if selcargocnt > 0
                                      then
                                        if GenerateCarId(selcargos[random(selcargocnt)], carid)
                                          then
                                            begin
                                              carclass := fManager.GetCarClass(carid);
                                              TCar.Create(carid, Point(j, i), cBlocksToMove, Self, carclass);
                                              inc(createdcars);
                                            end;
                                  end;
                            end;
                        inc(j);
                      end;
                    dec(i);
                  end;
                j := max(jmin - cCarsXMargin, 0);
                while (j <= min(jmin + cCarsXMargin, pred(GetColumns))) and (createdcars < carstocreate) do
                  begin
                    i := max(imin - cCarsYMargin, 0);
                    while (i <= min(imax + cCarsYMargin, pred(GetRows))) and (createdcars < carstocreate) do
                      begin
                        if ThereIsRoad(i, j)
                          then
                            begin
                              cargodata := fMap.GetCargoData(i, j);
                              if (cargodata <> cargoNone) and (fMap.GetCar(i, j, rsNorth) = carNone) and (fMap.GetCar(i, j, rsSouth) = carNone) and (fMap.GetCar(i, j, rsEast) = carNone) and (fMap.GetCar(i, j, rsWest) = carNone)
                                then
                                  begin
                                    selcargocnt := 0;
                                    for ck := low(ck) to high(ck) do
                                      if abs(cargodata.Cargos[ck]) > cSignificantCargo
                                        then
                                          begin
                                            selcargos[selcargocnt] := ck;
                                            inc(selcargocnt);
                                          end;
                                    if selcargocnt > 0
                                      then
                                        if GenerateCarId(selcargos[random(selcargocnt)], carid)
                                          then
                                            begin
                                              carclass := fManager.GetCarClass(carid);
                                              TCar.Create(carid, Point(j, i), cBlocksToMove, Self, carclass);
                                              inc(createdcars);
                                            end;
                                  end;
                            end;
                        inc(i);
                      end;
                    inc(j);
                  end;
                j := min(jmax + cCarsXMargin, pred(GetColumns));
                while (j >= max(jmax - cCarsXMargin, 0)) and (createdcars < carstocreate) do
                  begin
                    i := max(imin - cCarsYMargin, 0);
                    while (i <= min(imax + cCarsYMargin, pred(GetRows))) and (createdcars < carstocreate) do
                      begin
                        if ThereIsRoad(i, j)
                          then
                            begin
                              cargodata := fMap.GetCargoData(i, j);
                              if (cargodata <> cargoNone) and (fMap.GetCar(i, j, rsNorth) = carNone) and (fMap.GetCar(i, j, rsSouth) = carNone) and (fMap.GetCar(i, j, rsEast) = carNone) and (fMap.GetCar(i, j, rsWest) = carNone)
                                then
                                  begin
                                    selcargocnt := 0;
                                    for ck := low(ck) to high(ck) do
                                      if abs(cargodata.Cargos[ck]) > cSignificantCargo
                                        then
                                          begin
                                            selcargos[selcargocnt] := ck;
                                            inc(selcargocnt);
                                          end;
                                    if selcargocnt > 0
                                      then
                                        if GenerateCarId(selcargos[random(selcargocnt)], carid)
                                          then
                                            begin
                                              carclass := fManager.GetCarClass(carid);
                                              TCar.Create(carid, Point(j, i), cBlocksToMove, Self, carclass);
                                              inc(createdcars);
                                            end;
                                  end;
                            end;
                        inc(i);
                      end;
                    dec(j);
                  end;
              end;
      end;
      {$ELSE}
      const
        cBlocksToMove = 32;
      const
        cargokinds : array [0..2] of TCargoKind = (carPeople, carLight, carHeavy);
      var
        i, j        : integer;
        ngcarcount  : integer;
        ngtocreate  : integer;
        sgcarcount  : integer;
        sgtocreate  : integer;
        egcarcount  : integer;
        egtocreate  : integer;
        wgcarcount  : integer;
        wgtocreate  : integer;
        rsttocreate : integer;
        carid       : idCar;
        carclass    : PCarClass;
      begin
        if carstocreate > 0
          then
            begin
              ngtocreate := carstocreate div 4;
              sgtocreate := ngtocreate;
              egtocreate := ngtocreate;
              wgtocreate := ngtocreate;
              rsttocreate := carstocreate mod 4;
              if rsttocreate > 0
                then
                  begin
                    inc(ngtocreate);
                    dec(rsttocreate);
                    if rsttocreate > 0
                      then
                        begin
                          inc(sgtocreate);
                          dec(rsttocreate);
                          if rsttocreate > 0
                            then inc(egtocreate);
                        end;
                  end;
              ngcarcount := 0;
              sgcarcount := 0;
              egcarcount := 0;
              wgcarcount := 0;
              with fMap do
                begin
                  i := max(imin - cCarsYMargin, 0);
                  while (i <= min(imin + cCarsYMargin, pred(GetRows))) and (ngcarcount < ngtocreate) do
                    begin
                      j := max(jmin - cCarsXMargin, 0);
                      while (j <= min(jmax + cCarsXMargin, pred(GetColumns))) and (ngcarcount < ngtocreate) do
                        begin
                          if ThereIsRoad(i, j) and (GetCar(i, j, rsNorth) = carNone) and (GetCar(i, j, rsSouth) = carNone) and (GetCar(i, j, rsEast) = carNone) and (GetCar(i, j, rsWest) = carNone)
                            then
                              if GenerateCarId(cargokinds[random(3)], carid)
                                then
                                  begin
                                    carclass := fManager.GetCarClass(carid);
                                    TCar.Create(carid, Point(j, i), cBlocksToMove, Self, carclass);
                                    inc(ngcarcount);
                                  end;
                          inc(j);
                        end;
                      inc(i);
                    end;
                  i := min(imax + cCarsYMargin, pred(GetRows));
                  while (i >= max(imax - cCarsYMargin, 0)) and (sgcarcount < sgtocreate) do
                    begin
                      j := max(jmin - cCarsXMargin, 0);
                      while (j <= min(jmax + cCarsXMargin, pred(GetColumns))) and (sgcarcount < sgtocreate) do
                        begin
                          if ThereIsRoad(i, j) and (GetCar(i, j, rsNorth) = carNone) and (GetCar(i, j, rsSouth) = carNone) and (GetCar(i, j, rsEast) = carNone) and (GetCar(i, j, rsWest) = carNone)
                            then
                              if GenerateCarId(cargokinds[random(3)], carid)
                                then
                                  begin
                                    carclass := fManager.GetCarClass(carid);
                                    TCar.Create(carid, Point(j, i), cBlocksToMove, Self, carclass);
                                    inc(sgcarcount);
                                  end;
                          inc(j);
                        end;
                      dec(i);
                    end;
                  j := max(jmin - cCarsXMargin, 0);
                  while (j <= min(jmin + cCarsXMargin, pred(GetColumns))) and (egcarcount < egtocreate) do
                    begin
                      i := max(imin - cCarsYMargin, 0);
                      while (i <= min(imax + cCarsYMargin, pred(GetRows))) and (egcarcount < egtocreate) do
                        begin
                          if ThereIsRoad(i, j) and (GetCar(i, j, rsNorth) = carNone) and (GetCar(i, j, rsSouth) = carNone) and (GetCar(i, j, rsEast) = carNone) and (GetCar(i, j, rsWest) = carNone)
                            then
                              if GenerateCarId(cargokinds[random(3)], carid)
                                then
                                  begin
                                    carclass := fManager.GetCarClass(carid);
                                    TCar.Create(carid, Point(j, i), cBlocksToMove, Self, carclass);
                                    inc(egcarcount);
                                  end;
                          inc(i);
                        end;
                      inc(j);
                    end;
                  j := min(jmax + cCarsXMargin, pred(GetColumns));
                  while (j >= max(jmax - cCarsXMargin, 0)) and (wgcarcount < wgtocreate) do
                    begin
                      i := max(imin - cCarsYMargin, 0);
                      while (i <= min(imax + cCarsYMargin, pred(GetRows))) and (wgcarcount < wgtocreate) do
                        begin
                          if ThereIsRoad(i, j) and (GetCar(i, j, rsNorth) = carNone) and (GetCar(i, j, rsSouth) = carNone) and (GetCar(i, j, rsEast) = carNone) and (fMap.GetCar(i, j, rsWest) = carNone)
                            then
                              if GenerateCarId(cargokinds[random(3)], carid)
                                then
                                  begin
                                    carclass := fManager.GetCarClass(carid);
                                    TCar.Create(carid, Point(j, i), cBlocksToMove, Self, carclass);
                                    inc(wgcarcount);
                                  end;
                          inc(i);
                        end;
                      dec(j);
                    end;
                end;
            end;
      end;
      {$ENDIF}

    function CarsInRegion(imin, jmin, imax, jmax : integer) : integer;
      var
        i   : integer;
        Car : ICar;
      begin
        Result := 0;
        for i := 0 to pred(fCarInstCount) do
          begin
            Car := ICar(fCarInstances[i]);
            if (Car <> nil) and (Car.MapY >= imin) and (Car.MapY <= imax) and (Car.MapX >= jmin) and (Car.MapX <= jmax)
              then inc(Result);
          end;
      end;

    var
      k : integer;
    begin
      if fFocuses.Count > 0
        then
          begin
            fMaxCars := 0;
            for k := 0 to pred(fFocuses.Count) do
              fMaxCars := fMaxCars + round(cMaxViewCars/IGameFocus(fFocuses[k]).GetZoomFactor);
            fMaxCars := fMaxCars div fFocuses.Count;
            CreateCarsInRegion(imin, jmin, imax, jmax, fMaxCars - CarsInRegion(imin, jmin, imax, jmax));
          end;
    end;

  procedure TCarManager.UpdateCarSounds;
    var
      i          : integer;
      Car        : ICar;
      CarClass   : PCarClass;
      SndTargObj : TCarClassSoundTarget;
      Focus      : IGameFocus;
      k          : integer;
    begin
      for i := 0 to pred(fClassCount) do
        with fSoundInfo[i] do
          begin
            sumr := 0;
            sumc := 0;
            carcnt := 0;
          end;
      for i := 0 to pred(fCarInstCount) do
        begin
          Car := ICar(fCarInstances[i]);
          if (Car <> nil) and IsSpriteVisible(Car) // >>
            then
              begin
                inc(fSoundInfo[fClassesIndex[Car.Id]].sumr, Car.MapY);
                inc(fSoundInfo[fClassesIndex[Car.Id]].sumc, Car.MapX);
                inc(fSoundInfo[fClassesIndex[Car.Id]].carcnt);
              end;
        end;
      for k := 0 to pred(fFocuses.Count) do
        begin
          Focus := IGameFocus(fFocuses[k]);
          for i := 0 to pred(fClassCount) do
            begin
              CarClass := @fClasses[i];
              if CarClass.SoundData.wavefile <> ''
                then
                  with fSoundInfo[i] do
                    if carcnt > 0
                      then
                        begin
                          if SoundTarget = nil
                            then
                              begin // >> should keep a sound target per focus
                                SoundTarget := TCarClassSoundTarget.Create(Focus, CarClass, fConverter);
                                Focus.GetSoundManager.AddTargets(SoundTarget);
                              end;
                          SndTargObj := TCarClassSoundTarget(SoundTarget.GetObject);
                          SndTargObj.fClassCentroid.r := round(sumr/carcnt);
                          SndTargObj.fClassCentroid.c := round(sumc/carcnt);
                          SoundTarget.UpdateSoundParameters;
                          Focus.GetSoundManager.UpdateTarget(SoundTarget);
                        end
                      else
                        if SoundTarget <> nil
                          then
                            begin
                              Focus.GetSoundManager.RemoveTarget(SoundTarget);
                              SoundTarget := nil;
                            end;
            end;
        end;
    end;

end.
