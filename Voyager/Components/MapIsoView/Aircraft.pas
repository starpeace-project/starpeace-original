unit Aircraft;

interface

  uses
    Windows, Classes, GameTypes, MapSprites, LanderTypes, MapTypes, SoundTypes;

  const
    cPlaneFramesPerSec = 16;
    cPlaneFrameDelay   = 1000 div cPlaneFramesPerSec;

  type
    IAircraft =
      interface(IMapSprite)
        function  GetSoundTarget : ISoundTarget;
        procedure SetSoundTarget(const SoundTarget : ISoundTarget);
        property SoundTarget : ISoundTarget read GetSoundTarget write SetSoundTarget;
        procedure ClearInterfaced;
      end;

    IAircraftManager =
      interface(IMapSpriteManager)
        procedure RegionUpdated(imin, jmin, imax, jmax : integer);
        procedure DestroyAll;
      end;

    TAircraft =
      class(TInterfacedObject, IAircraft)
        public
          constructor Create(Id : integer; const IniMapPos : TPoint; IniAngle : TAngle; BlocksToMove : integer; const Manager : IAircraftManager; PlaneClass : PPlaneClass);
          destructor  Destroy; override;
        private
          fId              : integer;
          fFrame           : integer;
          fAngle           : TAngle;
          fMapPos          : TPoint;
          fOldMapPos       : TPoint;
          fBlockX          : integer;
          fBlockY          : integer;
          fOldBlockX       : integer;
          fOldBlockY       : integer;
          fManager         : IAircraftManager;
          fSoundTarget     : ISoundTarget;
          fLastFrameUpdate : integer;
          fBlocksToMove    : integer;
          fFramesPerBlock  : integer;
          fLastMoveTicks   : dword;
          fAnimTicks       : integer;
          fDeltaX          : integer;
          fDeltaY          : integer;
          fDeltaXRest      : integer;
          fDeltaYRest      : integer;
          fSlope           : single;
          fZoom            : TZoomRes;  // last zoom seen
          fRotation        : TRotation; // last rotation seen
          fImages          : array[TZoomRes, TRotation] of TSpriteImages;
        private
          procedure ReprogramPlane;
          function  MoveInMap(var FreePlane : boolean) : boolean;
          procedure UpdateFrame;
        private
          function RotateBlockX(blockx, blocky : integer; rotation : TRotation) : integer;
          function RotateBlockY(blockx, blocky : integer; rotation : TRotation) : integer;
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
        private // IAircraft
          function  GetSoundTarget : ISoundTarget;
          procedure ClearInterfaced;
          procedure SetSoundTarget(const SoundTarget : ISoundTarget);
      end;

  type
    PPlaneClasses = ^TPlaneClasses;
    TPlaneClasses = array [0..0] of TPlaneClass;

  type
    TAircraftManager =
      class(TMapSpriteManager, IAircraftManager)
        public
          constructor Create(const Map : IWorldMap; const Converter : ICoordinateConverter; const Manager : ILocalCacheManager; MinAnimInterval : integer; OwnsSprites : boolean);
          destructor  Destroy; override;
          procedure   DestroyAll;
        public // IMapSpriteManager
          function  GetSpriteCount : integer; override;
          function  GetSprite(i : integer) : IMapSprite; override;
          procedure SpriteMapPosChanged(const Sprite : IMapSprite); override;
          function  RegisterSprite(const Sprite : IMapSprite) : integer; override;
          procedure UnregisterSprite(const Sprite : IMapSprite); override;
          procedure RecacheSoundTargets(soundsenabled : boolean); override;
          procedure Disable; override;
        public// IPlanesManager
          procedure RegionUpdated(imin, jmin, imax, jmax : integer);
        private
          fPlaneInstances : TInterfaceList;
          fTicks          : integer;
        private // valid classes cache
          fClassCount : integer;
          fClasses    : PPlaneClasses;
        private
          fSoundsEnabled : boolean;
        protected
          procedure AnimationTick; override;
          function  GetFullSpriteId(const Sprite : IMapSprite) : integer; override;
          procedure RegulatePlaneFlow(imin, jmin, imax, jmax : integer);
      end;

  const
    cPlanesTimerInterval = cPlaneFrameDelay;

implementation
  uses
    VCLUtils, sysutils;
    
  const
    cAirTrafficRegInterval = 20000 div cPlanesTimerInterval;

  const
    u = 16; // >>>

  const
    cStoppedPlaneDeathDelay = 300;

  procedure FreeObject(var which);
    var
      aux : TObject;
    begin
      aux := TObject(which);
      TObject(which) := nil;
      aux.Free;
    end;

  procedure boundvalue(min, max : single; var value : single);
    begin
      if value < min
        then value := min;
      if value > max
        then value := max;
    end;

  // TAircraft
  
  constructor TAircraft.Create(Id : integer; const IniMapPos : TPoint; IniAngle : TAngle; BlocksToMove : integer; const Manager : IAircraftManager; PlaneClass : PPlaneClass);
    begin
      inherited Create;
      fId := Id;
      fMapPos := IniMapPos;
      fAngle := IniAngle;
      fBlocksToMove := BlocksToMove;
      fOldMapPos := fMapPos;
      fManager := Manager;
      fFramesPerBlock := round(cPlaneFramesPerSec/PlaneClass.Speed);
      fManager.RegisterSprite(IAircraft(Self));
      fManager.UpdateSpriteRect(IAircraft(Self));
     // fManager._Release; // >> cross referenced
    end;

  destructor TAircraft.Destroy;
    begin
      {if fManager <> nil
        then fManager.UnregisterSprite(IAircraft(Self));}
      // pointer(fManager) := nil; // >> cross referenced
      inherited;
    end;

  procedure TAircraft.ReprogramPlane;
    const
      cBlocksInc = 32;
    begin
      if fBlocksToMove = 0
        then inc(fBlocksToMove, cBlocksInc);
    end;

  function TAircraft.MoveInMap(var FreePlane : boolean) : boolean;
    var
      DeltaX, DeltaY : integer;
    begin
      DeltaX := 0;
      DeltaY := 0;
      if fBlocksToMove > 0
        then
          begin
            dec(fBlocksToMove);
            case fAngle of
              agN:
                DeltaY := 1;
              agNE:
                begin
                  DeltaX := 1;
                  DeltaY := 1;
                end;
              agE:
                DeltaX := 1;
              agSE:
                begin
                  DeltaX := 1;
                  DeltaY := -1;
                end;
              agS:
                DeltaY := -1;
              agSW:
                begin
                  DeltaX := -1;
                  DeltaY := -1;
                end;
              agW:
                DeltaX := -1;
              agNW:
                begin
                  DeltaX := -1;
                  DeltaY := 1;
                end;
            end;
          end;
      if (DeltaX <> 0) or (DeltaY <> 0)
        then
          begin
            fBlockX := 0;
            fBlockY := 0;
            case DeltaX of
              -1:
                case DeltaY of
                  -1:
                    begin
                      fDeltaX := 0;
                      fDeltaXRest := 0;
                      fDeltaY := 2*u div pred(fFramesPerBlock);
                      fDeltaYRest := 2*u mod pred(fFramesPerBlock);
                      fSlope := 0;
                    end;
                  0:
                    begin
                      fDeltaX := -(2*u div pred(fFramesPerBlock));
                      fDeltaXRest := -(2*u mod pred(fFramesPerBlock));
                      fDeltaY := u div pred(fFramesPerBlock);
                      fDeltaYRest := u mod pred(fFramesPerBlock);
                      fSlope := -0.5;
                    end;
                  1:
                    begin
                      fDeltaX := -(4*u div pred(fFramesPerBlock));
                      fDeltaXRest := -(4*u mod pred(fFramesPerBlock));
                      fDeltaY := 0;
                      fDeltaYRest := 0;
                      fSlope := 0;
                    end;
                end;
              0:
                case DeltaY of
                  -1:
                    begin
                      fDeltaX := 2*u div pred(fFramesPerBlock);
                      fDeltaXRest := 2*u mod pred(fFramesPerBlock);
                      fDeltaY := u div pred(fFramesPerBlock);
                      fDeltaYRest := u mod pred(fFramesPerBlock);
                      fSlope := 0.5;
                    end;
                  1:
                    begin
                      fDeltaX := -(2*u div pred(fFramesPerBlock));
                      fDeltaXRest := -(2*u mod pred(fFramesPerBlock));
                      fDeltaY := -(u div pred(fFramesPerBlock));
                      fDeltaYRest := -(u mod pred(fFramesPerBlock));
                      fSlope := 0.5;
                    end;
                end;
              1:
                case DeltaY of
                  -1:
                    begin
                      fDeltaX := 4*u div pred(fFramesPerBlock);
                      fDeltaXRest := 4*u mod pred(fFramesPerBlock);
                      fDeltaY := 0;
                      fDeltaYRest := 0;
                      fSlope := 0;
                    end;
                  0:
                    begin
                      fDeltaX := 2*u div pred(fFramesPerBlock);
                      fDeltaXRest := 2*u mod pred(fFramesPerBlock);
                      fDeltaY := -(u div pred(fFramesPerBlock));
                      fDeltaYRest := -(u mod pred(fFramesPerBlock));
                      fSlope := -0.5;
                    end;
                  1:
                    begin
                      fDeltaX := 0;
                      fDeltaXRest := 0;
                      fDeltaY := -(2*u div pred(fFramesPerBlock));
                      fDeltaYRest := -(2*u mod pred(fFramesPerBlock));
                      fSlope := 0;
                    end;
                end;
            end;
            inc(fMapPos.X, DeltaX);
            inc(fMapPos.Y, DeltaY);
            fManager.SpriteMapPosChanged(IAircraft(Self));
            fLastMoveTicks := GetTickCount;
            Result := true;
          end
        else
          begin
            if (GetTickCount - fLastMoveTicks > cStoppedPlaneDeathDelay) and not fManager.IsSpriteVisible(IAircraft(Self))
              then FreePlane := true
              else
                if fManager.IsSpriteVisible(IAircraft(Self))
                  then ReprogramPlane;
            Result := false;
          end;
    end;

  procedure TAircraft.UpdateFrame;
    var
      ElapsedTicks : integer;
      CurrentTick  : integer;
      Img          : TGameImage;
    begin
      Img := fImages[fZoom, fRotation][fAngle];
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

  function TAircraft.RotateBlockX(blockx, blocky : integer; rotation : TRotation) : integer;
    begin
      case rotation of
        drNorth:
          Result := blockx;
        drEast:
          Result := 2*blocky;
        drSouth:
          Result := -blockx;
        else // drWest
          Result := -2*blocky;
      end;
    end;

  function TAircraft.RotateBlockY(blockx, blocky : integer; rotation : TRotation) : integer;
    begin
      case rotation of
        drNorth:
          Result := blocky;
        drEast:
          Result := round(-blockx/2);
        drSouth:
          Result := -blocky;
        else // drWest
          Result := round(blockx/2);
      end;
    end;

  function TAircraft.GetId : integer;
    begin
      Result := fId;
    end;

  function TAircraft.GetFrame : integer;
    begin
      Result := fFrame;
    end;

  function TAircraft.GetAngle : TAngle;
    begin
      Result := fAngle;
    end;

  function TAircraft.GetMapX : integer;
    begin
      Result := fMapPos.X;
    end;

  function TAircraft.GetMapY : integer;
    begin
      Result := fMapPos.Y;
    end;

  function TAircraft.GetOldMapX : integer;
    begin
      Result := fOldMapPos.x;
    end;

  function TAircraft.GetOldMapY : integer;
    begin
      Result := fOldMapPos.y;
    end;

  procedure TAircraft.AnimationTick;
    var
      OldFrame  : integer;
      FreePlane : boolean;
    begin
      fOldBlockX := fBlockX;
      fOldBlockY := fBlockY;
      fOldMapPos := fMapPos;
      FreePlane := false;
      if fAnimTicks < pred(pred(fFramesPerBlock))
        then
          begin
            inc(fAnimTicks);
            if fSlope <> 0
              then
                begin
                  inc(fBlockX, fDeltaX);
                  if fDeltaXRest > 0
                    then
                      begin
                        inc(fBlockX);
                        dec(fDeltaXRest);
                      end
                    else
                      if fDeltaXRest < 0
                        then
                          begin
                            dec(fBlockX);
                            inc(fDeltaXRest);
                          end;
                  fBlockY := round(fSlope*fBlockX);
                end
              else
                if fDeltaX <> 0
                  then
                    begin
                      if fDeltaXRest > 0
                        then
                          begin
                            inc(fBlockX);
                            dec(fDeltaXRest);
                          end
                        else
                          if fDeltaXRest < 0
                            then
                              begin
                                dec(fBlockX);
                                inc(fDeltaXRest);
                              end;
                      inc(fBlockX, fDeltaX);
                    end
                  else
                    if fDeltaY <> 0
                      then
                        begin
                          inc(fBlockY, fDeltaY);
                          if fDeltaYRest > 0
                            then
                              begin
                                inc(fBlockY);
                                dec(fDeltaYRest);
                              end
                            else
                              if fDeltaYRest < 0
                                then
                                  begin
                                    dec(fBlockY);
                                    inc(fDeltaYRest);
                                  end;
                        end;
          end
        else
          if MoveInMap(FreePlane) and not FreePlane
            then fAnimTicks := 0;
      OldFrame := fFrame;
      UpdateFrame;
      if ((fMapPos.x <> fOldMapPos.x) or (fMapPos.y <> fOldMapPos.y) or (fBlockX <> fOldBlockX) or (fBlockY <> fOldBlockY) or (OldFrame <> fFrame) or FreePlane) and (fManager <> nil)
        then fManager.UpdateSpriteRect(IAircraft(Self));
      if FreePlane
        then 
          begin
            // _Release; //.rag
            if fManager <> nil
              then fManager.UnregisterSprite(IAircraft(Self));
          end;
    end;

  procedure TAircraft.NewView(const View : IGameView);
    begin
      fFrame := 0;
      fillchar(fImages[fZoom, fRotation], sizeof(fImages[fZoom, fRotation]), 0);
      fZoom := TZoomRes(View.ZoomLevel);
      fRotation := View.Rotation;
      if fImages[fZoom, fRotation][fAngle] = nil
        then fManager.GetSpriteImages(IAircraft(Self), View, fImages[fZoom, fRotation]);
    end;

  function TAircraft.GetBlockX(const View : IGameView) : integer;
    begin
      Result := RotateBlockX(fBlockX, fBlockY, View.Rotation)*cZoomFactors[TZoomRes(View.ZoomLevel)].m div cZoomFactors[TZoomRes(View.ZoomLevel)].n - GetWidth(View) div 2;
    end;

  function TAircraft.GetBlockY(const View : IGameView) : integer;
    begin
      Result := RotateBlockY(fBlockX, fBlockY, View.Rotation)*cZoomFactors[TZoomRes(View.ZoomLevel)].m div cZoomFactors[TZoomRes(View.ZoomLevel)].n - GetHeight(View) div 2;
    end;

  function TAircraft.GetOldBlockX(const View : IGameView) : integer;
    begin
      Result := RotateBlockX(fOldBlockX, fOldBlockY, View.Rotation)*cZoomFactors[TZoomRes(View.ZoomLevel)].m div cZoomFactors[TZoomRes(View.ZoomLevel)].n - GetWidth(View) div 2;
    end;

  function TAircraft.GetOldBlockY(const View : IGameView) : integer;
    begin
      Result := RotateBlockY(fOldBlockX, fOldBlockY, View.Rotation)*cZoomFactors[TZoomRes(View.ZoomLevel)].m div cZoomFactors[TZoomRes(View.ZoomLevel)].n - GetHeight(View) div 2;
    end;

  function TAircraft.GetWidth(const View : IGameView) : integer;
    begin
      fZoom := TZoomRes(View.ZoomLevel);
      fRotation := View.Rotation;
      if fImages[fZoom, fRotation][fAngle] = nil
        then fManager.GetSpriteImages(IAircraft(Self), View, fImages[fZoom, fRotation]);
      if fImages[fZoom, fRotation][fAngle] <> nil
        then Result := fImages[fZoom, fRotation][fAngle].Width
        else Result := 0;
    end;

  function TAircraft.GetHeight(const View : IGameView) : integer;
    begin
      fZoom := TZoomRes(View.ZoomLevel);
      fRotation := View.Rotation;
      if fImages[fZoom, fRotation][fAngle] = nil
        then fManager.GetSpriteImages(IAircraft(Self), View, fImages[fZoom, fRotation]);
      if fImages[fZoom, fRotation][fAngle] <> nil
        then Result := fImages[fZoom, fRotation][fAngle].Height
        else Result := 0;
    end;

  function TAircraft.GetSoundTarget : ISoundTarget;
    begin
      Result := fSoundTarget;
    end;

  procedure TAircraft.SetSoundTarget(const SoundTarget : ISoundTarget);
    begin
      fSoundTarget := SoundTarget;
    end;

  // TAircraftSoundTarget
  
  type
    TAircraftSoundTarget =
      class(TInterfacedObject, ISoundTarget)
        public
          constructor Create(Owner : IGameFocus; const Aircraft : IAircraft; PlaneClass : PPlaneClass; const Converter : ICoordinateConverter);
          destructor  Destroy; override;
        private
          fOwner      : IGameFocus;
          fAircraft   : IAircraft;
          fConverter  : ICoordinateConverter;
          fSoundData  : TSoundData;
          fPan        : single;
          fVolume     : single;
          fLastPlayed : dword;
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

  constructor TAircraftSoundTarget.Create(Owner : IGameFocus; const Aircraft : IAircraft; PlaneClass : PPlaneClass; const Converter : ICoordinateConverter);
    begin
      inherited Create;
      fOwner := Owner;
      //fOwner._Release; // >> cross referenced
      fAircraft := Aircraft;
      //fAircraft._Release; // >> cross referenced
      fSoundData := PlaneClass.SoundData;
      fConverter := Converter;
      //fConverter._Release; // >> cross referenced
      UpdateSoundParameters;
    end;

  destructor TAircraftSoundTarget.Destroy;
    begin
      fConverter := nil;  // >> cross referenced
      fAircraft := nil; // >> cross referenced
      fOwner := nil; // >> cross referenced
      inherited;
    end;

  function TAircraftSoundTarget.GetSoundName : string;
    begin
      Result := fSoundData.wavefile;
    end;

  function TAircraftSoundTarget.GetSoundKind : integer;
    begin
      Result := integer(Self);
    end;

  function TAircraftSoundTarget.GetPriority : integer;
    begin
      Result := fSoundData.priority;
    end;

  function TAircraftSoundTarget.IsLooped : boolean;
    begin
      Result := fSoundData.looped;
    end;

  function TAircraftSoundTarget.GetVolume : single;
    begin
      Result := fVolume*fSoundData.atenuation;
    end;

  function TAircraftSoundTarget.GetPan : single;
    begin
      Result := fPan;
    end;

  function TAircraftSoundTarget.ShouldPlayNow : boolean;
    var
      ElapsedTicks : integer;
    begin
      with fSoundData do
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

  function TAircraftSoundTarget.IsCacheable : boolean;
    begin
      Result := false;
    end;

  function TAircraftSoundTarget.IsEqualTo(const SoundTarget : ISoundTarget) : boolean;
    var
      SndTargetObj : TObject;
    begin
      SndTargetObj := SoundTarget.GetObject;
      if SndTargetObj is TAircraftSoundTarget
        then Result := fAircraft = TAircraftSoundTarget(SndTargetObj).fAircraft
        else Result := false;
    end;

  function TAircraftSoundTarget.GetObject : TObject;
    begin
      Result := Self;
    end;

  procedure TAircraftSoundTarget.UpdateSoundParameters;
    var
      screensize : TPoint;
      tmppt      : TPoint;
      x, y       : integer;
      u          : integer;
      ci, cj     : integer;
      Dist       : single;
      view       : IGameView;
    begin
      with fOwner do
        begin
          view := GetView;
          fConverter.MapToScreen(view, fAircraft.MapY, fAircraft.MapX, x, y);
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
          Dist := sqrt(sqr(ci - fAircraft.MapY) + sqr(cj - fAircraft.MapX));
          if Dist < cMaxHearDist
            then fVolume := cMinVol + (1 - Dist/cMaxHearDist)*(cMaxVol - cMinVol)*(1 - abs(view.ZoomLevel - ord(cBasicZoomRes))*cZoomVolStep)
            else fVolume := cMinVol;
          boundvalue(cMinVol, cMaxVol, fVolume);
        end;
    end;

  procedure TAircraft.ClearInterfaced;
    begin
      fManager     := nil;
      fSoundTarget := nil;
    end;
    
  // TAircraftManager

  constructor TAircraftManager.Create(const Map : IWorldMap; const Converter : ICoordinateConverter; const Manager : ILocalCacheManager; MinAnimInterval : integer; OwnsSprites : boolean);
    var
      i          : integer;
      id         : idCar;
      PlaneClass : PPlaneClass;
    begin
      inherited Create(Map, Converter, Manager, MinAnimInterval, OwnsSprites);
      fPlaneInstances := TInterfaceList.Create;
      fInterval := cPlanesTimerInterval;
      with fMap do
        begin
          fClassCount := fManager.GetPlaneClassCount;
          getmem(fClasses, fClassCount*sizeof(fClasses[0]));
          zeromemory( fClasses, fClassCount*sizeof(fClasses[0]));
          i := 0;
          for id := low(id) to high(id) do
            begin
              PlaneClass := fManager.GetPlaneClass(id);
              if (PlaneClass <> nil) and PlaneClass.valid
                then
                  try
                    fClasses[i] := PlaneClass^;
                    inc(i);
                  except
                  end;
            end;
        end;
      fSoundsEnabled := true;
    end;

  destructor TAircraftManager.Destroy;
    begin
      DestroyAll;
      inherited;
    end;

  procedure TAircraftManager.DestroyAll;
    begin
      FreeAndNil(fPlaneInstances);
      FreememAndNil(fClasses);
    end;
    
  function TAircraftManager.GetSpriteCount : integer;
    begin
      Result := fPlaneInstances.Count;
    end;

  function TAircraftManager.GetSprite(i : integer) : IMapSprite;
    begin
      Result := IMapSprite(fPlaneInstances[i]);
    end;

  procedure TAircraftManager.SpriteMapPosChanged(const Sprite : IMapSprite);
    var
      SoundTarget : ISoundTarget;
      Focus       : IGameFocus;
      i           : integer;
    begin
      for i := 0 to pred(fFocuses.Count) do
        begin
          Focus := IGameFocus(fFocuses[i]);
          SoundTarget := IAircraft(Sprite).SoundTarget; // >> must ask for focus SoundTarget
          if SoundTarget <> nil
            then
              try
                SoundTarget.UpdateSoundParameters;
                Focus.GetSoundManager.UpdateTarget(SoundTarget);
              except
              end;
        end;
    end;

  function TAircraftManager.RegisterSprite(const Sprite : IMapSprite) : integer;
    var
      SoundTarget : ISoundTarget;
      Focus       : IGameFocus;
      i           : integer;
      PlaneClass  : PPlaneClass;
    begin
      Result := fPlaneInstances.Add(Sprite);
      // Sprite._AddRef;
      if fSoundsEnabled
        then
          begin
            PlaneClass := fManager.GetPlaneClass(Sprite.Id);
            for i := 0 to pred(fFocuses.Count) do
              begin
                Focus := IGameFocus(fFocuses[i]);
                if PlaneClass.SoundData.wavefile <> ''
                  then
                    begin
                      SoundTarget := TAircraftSoundTarget.Create(Focus, IAircraft(Sprite), PlaneClass, fConverter);
                      IAircraft(Sprite).SoundTarget := SoundTarget; // >> must keep a sound target for each focus
                      Focus.GetSoundManager.AddTargets(SoundTarget);
                    end;
              end;
          end;
    end;

  procedure TAircraftManager.UnregisterSprite(const Sprite : IMapSprite);
    var
      SoundTarget : ISoundTarget;
      Focus       : IGameFocus;
      i           : integer;
    begin
      fPlaneInstances.Remove(Sprite);
      // fPlaneInstances[fPlaneInstances.IndexOf(pointer(Sprite))] := nil;
      SoundTarget := IAircraft(Sprite).SoundTarget; // >>
      if SoundTarget <> nil
        then
          for i := 0 to pred(fFocuses.Count) do
            begin
              Focus := IGameFocus(fFocuses[i]);
              Focus.GetSoundManager.RemoveTarget(SoundTarget);
            end;
      IAircraft(Sprite).ClearInterfaced;
    end;

  procedure TAircraftManager.RecacheSoundTargets(soundsenabled : boolean);
    var
      i           : integer;
      j           : integer;
      Focus       : IGameFocus;
      Aircraft    : IAircraft;
      PlaneClass  : PPlaneClass;
      SoundTarget : ISoundTarget;
    begin
      fSoundsEnabled := soundsenabled;
      for i := 0 to pred(fPlaneInstances.Count) do
        begin
          Aircraft := IAircraft(fPlaneInstances[i]);
          if Aircraft <> nil
            then
              begin
                PlaneClass := fManager.GetPlaneClass(Aircraft.Id);
                for j := 0 to pred(fFocuses.Count) do
                  begin
                    Focus := IGameFocus(fFocuses[j]);
                    if PlaneClass.SoundData.wavefile <> ''
                      then
                        begin
                          Aircraft.SoundTarget := nil;
                          if fSoundsEnabled
                            then
                              begin
                                SoundTarget := TAircraftSoundTarget.Create(Focus, Aircraft, PlaneClass, fConverter);
                                Aircraft.SoundTarget := SoundTarget; // >> must keep a sound target for each focus
                                Focus.GetSoundManager.AddTargets(SoundTarget);
                              end;
                        end;
                  end;
              end;
        end;
    end;

  procedure TAircraftManager.Disable;
    begin
      inherited;
      // fPlaneInstances.Pack;
    end;

  procedure TAircraftManager.RegionUpdated(imin, jmin, imax, jmax : integer);
    begin
      if not fExternallyDisabled
        then RegulatePlaneFlow(imin, jmin, imax, jmax);
    end;

  procedure TAircraftManager.AnimationTick;
    var
      i        : integer;
      imin     : integer;
      jmin     : integer;
      imax     : integer;
      jmax     : integer;
    begin
      inherited;
//      fPlaneInstances.Pack;
      if fTicks = pred(cAirTrafficRegInterval)
        then
          begin
            for i := 0 to pred(fFocuses.Count) do
              begin
                fConverter.GetViewRegion(IGameFocus(fFocuses[i]).GetView, imin, jmin, imax, jmax);
                RegulatePlaneFlow(imin, jmin, imax, jmax);
              end;
            fTicks := 0;
          end
        else inc(fTicks);
    end;

  function TAircraftManager.GetFullSpriteId(const Sprite : IMapSprite) : integer;
    begin
      Result := idPlaneMask or Sprite.Id;
    end;

  procedure TAircraftManager.RegulatePlaneFlow(imin, jmin, imax, jmax: integer);

    function GetNorthAngle : TAngle;
      var
        randnum : integer;
      begin
        randnum := random(3);
        case randnum of
          0:
            Result := agNW;
          1:
            Result := agN;
          2:
            Result := agNE
          else
            Result := agN;
        end;
      end;

    function GetSouthAngle : TAngle;
      var
        randnum : integer;
      begin
        randnum := random(3);
        case randnum of
          0:
            Result := agSW;
          1:
            Result := agS;
          2:
            Result := agSE
          else
            Result := agS;
        end;
      end;

    function GetEastAngle : TAngle;
      var
        randnum : integer;
      begin
        randnum := random(3);
        case randnum of
          0:
            Result := agNE;
          1:
            Result := agE;
          2:
            Result := agSE
          else
            Result := agE;
        end;
      end;

    function GetWestAngle : TAngle;
      var
        randnum : integer;
      begin
        randnum := random(3);
        case randnum of
          0:
            Result := agNW;
          1:
            Result := agW;
          2:
            Result := agSW
          else
            Result := agW;
        end;
      end;

    function GeneratePlaneId(out planeid : idPlane) : boolean;
      var
        selectedids    : array [idPlane] of idPlane;
        selectedidscnt : word;
        threshold      : single;
        maxprob        : single;
        i              : integer;
        planeclass     : TPlaneClass;
      begin
        threshold := random;
        selectedidscnt := 0;
        maxprob := 0;
        for i := 0 to pred(fClassCount) do
          begin
            planeclass := fClasses[i];
            if planeclass.Prob > threshold
              then
                begin
                  selectedids[selectedidscnt] := planeclass.id;
                  inc(selectedidscnt);
                end
              else
                if planeclass.Prob > maxprob
                  then maxprob := planeclass.Prob;
          end;
        if selectedidscnt = 0
          then
            for i := 0 to pred(fClassCount) do
              begin
                planeclass := fClasses[i];
                if planeclass.Prob = maxprob
                  then
                    begin
                      selectedids[selectedidscnt] := planeclass.id;
                      inc(selectedidscnt);
                    end;
              end;
        if selectedidscnt > 0
          then
            begin
              planeid := selectedids[random(selectedidscnt)];
              Result := true;
            end
          else Result := false;
      end; // >> check this id selection method, bug here!!!

    const
      cMaxPlanes    = 5;
      cBlocksToMove = 32;
    var
      zone       : byte;
      planeid    : idPlane;
      planeclass : PPlaneClass;
    begin
      zone := random(4);
      if (fFocuses.Count > 0) and (fPlaneInstances.Count < cMaxPlanes)
        then
          if GeneratePlaneId(planeid)
            then
              begin
                planeclass := fManager.GetPlaneClass(planeid);
                case Zone of
                  0:
                    TAircraft.Create(planeid, Point(jmin + random(jmax - jmin), imin - 8), GetNorthAngle, cBlocksToMove, Self, planeclass);
                  1:
                    TAircraft.Create(planeid, Point(jmin + random(jmax - jmin), imax + 8), GetSouthAngle, cBlocksToMove, Self, planeclass);
                  2:
                    TAircraft.Create(planeid, Point(jmin - 8, imin + random(imax - imin)), GetEastAngle, cBlocksToMove, Self, planeclass);
                  3:
                    TAircraft.Create(planeid, Point(jmax + 8, imin + random(imax - imin)), GetWestAngle, cBlocksToMove, Self, planeclass);
                end;
              end;
    end;

end.
