unit TraincarSprite;

interface

  uses
    GameTypes, MapSprites, MapTypes, Vehicles, Sounds;

  type
    ITraincarSprite =
      interface(IMapSprite)
        function  GetGroupId : integer;
        function  GetSoundTarget : ISoundTarget;
        procedure SetSoundTarget(const SoundTarget : ISoundTarget);
        function  GetIdx : byte;
        procedure SetIdx(Idx : byte);
        property  GroupId     : integer      read GetGroupId;
        property  SoundTarget : ISoundTarget read GetSoundTarget write SetSoundTarget;
        property  Idx         : byte         read GetIdx         write SetIdx;
      end;

  type
    TTraincarSprite =
      class(TInterfacedObject, ITraincarSprite)
        public
          constructor Create(const Traincar : IVehicle; TraincarClass : PTraincarClass; const Manager : IMapSpriteManager);
          destructor  Destroy; override;
        private
          fTraincar        : IVehicle;
          fManager         : IMapSpriteManager;
          fFrame           : integer;
          fAngle           : TAngle;
          fMapX            : integer;
          fMapY            : integer;
          fOldMapX         : integer;
          fOldMapY         : integer;
          fBlockX          : integer;
          fBlockY          : integer;
          fOldBlockX       : integer;
          fOldBlockY       : integer;
          fSoundTarget     : ISoundTarget;
          fLastFrameUpdate : integer;
          fIdx             : byte;
          fZoom            : TZoomRes;  // last zoom seen
          fRotation        : TRotation; // last rotation seen
          fImages          : array[TZoomRes, TRotation] of TSpriteImages;
        private
          procedure CalcTraincarCoords(trainx, trainy : single; out mapx, mapy, blockx, blocky : integer);
          procedure CalcAngle(out angle : TAngle);
          procedure UpdateFrame;
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
        private // ITraincarSprite
          function  GetGroupId : integer;
          function  GetSoundTarget : ISoundTarget;
          procedure SetSoundTarget(const SoundTarget : ISoundTarget);
          function  GetIdx : byte;
          procedure SetIdx(Idx : byte);
      end;

  type
    TTraincarSpriteManager =
      class(TMapSpritesManager, IMapSpriteManager)
        public
          constructor Create(Map : TWorldMap; MinAnimInterval : integer; OwnsSprites : boolean; const TraincarsArray : IVehicleArray);
          destructor  Destroy; override;
        private // IMapSpriteManager
          function  GetSpriteCount : integer; override;
          function  GetSprite(i : integer) : IMapSprite; override;
          procedure SpriteMapPosChanged(const Sprite : IMapSprite); override;
          function  RegisterSprite(const Sprite : IMapSprite) : integer; override;
          procedure UnregisterSprite(const Sprite : IMapSprite); override;
          procedure RecacheSoundTargets; override;
        private
          fTraincarsArray : IVehicleArray;
          function  GetFullSpriteId(const Sprite : IMapSprite) : integer; override;
          procedure TraincarsArrayChanged(TraincarsArray : IVehicleArray; ArrayChange : TArrayChange; const Info);
      end;
      
implementation

  uses
    SysUtils, LogFile, Windows;

  const
    cTraincarSpritesTimerInterval = cNoTimerInterval;

  const
    cBasicU = 16;

  const
    cXHotSpot = 0;
    cYHotSpot = 10;

  const
    cChangeMapCoordThresh = 0.0001;

  constructor TTraincarSprite.Create(const Traincar : IVehicle; TraincarClass : PTraincarClass; const Manager : IMapSpriteManager);
    begin
      inherited Create;
      fTraincar := Traincar;
      fManager := Manager;
      CalcTraincarCoords(fTraincar.getX, fTraincar.getY, fMapX, fMapY, fBlockX, fBlockY);
      CalcAngle(fAngle);
      fOldMapX := fMapX;
      fOldMapY := fMapY;
      fOldBlockX := fBlockX;
      fOldBlockY := fBlockY;
      if fManager <> nil
        then
          begin
            fManager.RegisterSprite(ITraincarSprite(Self));
            fManager.UpdateSpriteRect(ITraincarSprite(Self));
          end
        else raise Exception.Create('Manager = nil in constructor');
    end;

  destructor TTraincarSprite.Destroy;
    begin
      {if fManager <> nil
        then fManager.UnregisterSprite(ITraincarSprite(Self))
        else raise Exception.Create('Manager = nil in constructor');}
      inherited;
    end;

  procedure TTraincarSprite.CalcTraincarCoords(trainx, trainy : single; out mapx, mapy, blockx, blocky : integer);
    var
      rndx, rndy   : integer;
      fracx, fracy : single;
      img          : TGameImage;
    begin
      rndx := round(trainx);
      rndy := round(trainy);
      if abs(trainx - rndx) > cChangeMapCoordThresh
        then
          begin
            mapx := trunc(trainx);
            fracx := frac(trainx);
          end
        else
          begin
            mapx := rndx;
            fracx := 0;
          end;
      if abs(trainy - rndy) > cChangeMapCoordThresh
        then
          begin
            mapy := trunc(trainy);
            fracy := frac(trainy);
          end
        else
          begin
            mapy := rndy;
            fracy := 0;
          end;
      blockx := round(2*cBasicU*(1 - fracy + fracx));
      blocky := round(cBasicU*((1 - fracy) + (1 - fracx)));
      img := fImages[fZoom, fRotation][fAngle];
      if img <> nil
        then
          begin
            dec(blockx, img.Width div 2 + cXHotSpot);
            dec(blocky, img.Height + cYHotSpot);
          end;
    end;

  procedure TTraincarSprite.CalcAngle(out angle : TAngle);
    var
      angdegrees : integer;
    begin
      angdegrees := round(fTraincar.getAngle*180/Pi);
      if angdegrees < 0
        then angdegrees := 360 + angdegrees;
      if (angdegrees < 0) or (angdegrees > 360)
        then raise Exception.Create('Invalid angle');
      case angdegrees of
        0..10:
          angle := agE;
        11..33:
          angle := agENE;
        34..55:
          angle := agNE;
        56..78:
          angle := agNNE;
        79..100:
          angle := agN;
        101..123:
          angle := agNNW;
        124..145:
          angle := agNW;
        146..168:
          angle := agWNW;
        169..190:
          angle := agW;
        191..213:
          angle := agWSW;
        214..235:
          angle := agSW;
        236..258:
          angle := agSSW;
        259..280:
          angle := agS;
        281..303:
          angle := agSSE;
        304..325:
          angle := agSE;
        326..348:
          angle := agESE;
        else
          angle := agE;
      end;
    end;

  procedure TTraincarSprite.UpdateFrame;
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

  function TTraincarSprite.GetId : integer;
    begin
      Result := fTraincar.getVisualClass;
    end;

  function TTraincarSprite.GetFrame : integer;
    begin
      Result := fFrame;
    end;

  function TTraincarSprite.GetAngle : TAngle;
    begin
      Result := fAngle;
    end;

  function TTraincarSprite.GetMapX : integer;
    begin
      Result := fMapX;
    end;

  function TTraincarSprite.GetMapY : integer;
    begin
      Result := fMapY;
    end;

  function TTraincarSprite.GetOldMapX : integer;
    begin
      Result := fOldMapX;
    end;

  function TTraincarSprite.GetOldMapY : integer;
    begin
      Result := fOldMapY;
    end;

  procedure TTraincarSprite.AnimationTick;
    var
      angle : TAngle;
    begin
      //LogThis('Id = ' + IntToStr(integer(fTraincar)) + ' X = ' + FloatToStr(fTraincar.getX) + ' Y = ' + FloatToStr(fTraincar.getY) + ' Angle = ' + IntToStr(round(fTraincar.getAngle*180/Pi)));
      fOldMapX := fMapX;
      fOldMapY := fMapY;
      fOldBlockX := fBlockX;
      fOldBlockY := fBlockY;
      CalcTraincarCoords(fTraincar.getX, fTraincar.getY, fMapX, fMapY, fBlockX, fBlockY);
      CalcAngle(angle);
      if angle <> fAngle
        then fAngle := angle;
      UpdateFrame;
      fManager.UpdateSpriteRect(ITraincarSprite(Self));
    end;

  procedure TTraincarSprite.NewView(const View : IGameView);
    begin
      fFrame := 0;
      fillchar(fImages[fZoom, fRotation], sizeof(fImages[fZoom, fRotation]), 0);
      fZoom := TZoomRes(View.ZoomLevel);
      fRotation := View.Rotation;
      if fImages[fZoom, fRotation][fAngle] = nil
        then fManager.GetSpriteImages(ITraincarSprite(Self), View, fImages[fZoom, fRotation]);
    end;

  function TTraincarSprite.GetBlockX(const View : IGameView) : integer;
    begin
      Result := fBlockX*cZoomFactors[TZoomRes(View.ZoomLevel)].m div cZoomFactors[TZoomRes(View.ZoomLevel)].n;
    end;

  function TTraincarSprite.GetBlockY(const View : IGameView) : integer;
    begin
      Result := fBlockY*cZoomFactors[TZoomRes(View.ZoomLevel)].m div cZoomFactors[TZoomRes(View.ZoomLevel)].n;
    end;

  function TTraincarSprite.GetOldBlockX(const View : IGameView) : integer;
    begin
      Result := fOldBlockX*cZoomFactors[TZoomRes(View.ZoomLevel)].m div cZoomFactors[TZoomRes(View.ZoomLevel)].n;
    end;

  function TTraincarSprite.GetOldBlockY(const View : IGameView) : integer;
    begin
      Result := fOldBlockY*cZoomFactors[TZoomRes(View.ZoomLevel)].m div cZoomFactors[TZoomRes(View.ZoomLevel)].n;
    end;

  function TTraincarSprite.GetWidth(const View : IGameView) : integer;
    begin
      fZoom := TZoomRes(View.ZoomLevel);
      fRotation := View.Rotation;
      if fImages[fZoom, fRotation][fAngle] = nil
        then fManager.GetSpriteImages(ITraincarSprite(Self), View, fImages[fZoom, fRotation]);
      if fImages[fZoom, fRotation][fAngle] <> nil
        then Result := fImages[fZoom, fRotation][fAngle].Width
        else Result := 0;
    end;

  function TTraincarSprite.GetHeight(const View : IGameView) : integer;
    begin
      fZoom := TZoomRes(View.ZoomLevel);
      fRotation := View.Rotation;
      if fImages[fZoom, fRotation][fAngle] = nil
        then fManager.GetSpriteImages(ITraincarSprite(Self), View, fImages[fZoom, fRotation]);
      if fImages[fZoom, fRotation][fAngle] <> nil
        then Result := fImages[fZoom, fRotation][fAngle].Height
        else Result := 0;
    end;

  function TTraincarSprite.GetGroupId : integer;
    begin
      Result := fTraincar.getGroupId;
    end;

  function TTraincarSprite.GetSoundTarget : ISoundTarget;
    begin
      Result := fSoundTarget;
    end;

  procedure TTraincarSprite.SetSoundTarget(const SoundTarget : ISoundTarget);
    begin
      fSoundTarget := SoundTarget;
    end;

  function TTraincarSprite.GetIdx : byte;
    begin
      Result := fIdx;
    end;

  procedure TTraincarSprite.SetIdx(Idx : byte);
    begin
      fIdx := Idx;
    end;

  // TTraincarSpriteSoundTarget

  type
    TTraincarSpriteSoundTarget =
      class(TInterfacedObject, ISoundTarget)
        public
          constructor Create(Owner : TGameFocus; const TraincarSprite : ITraincarSprite; TraincarClass : PTraincarClass);
        private
          fOwner          : TGameFocus;
          fTraincarSprite : ITraincarSprite;
          fSoundData      : TSoundData;
          fPan            : single;
          fVolume         : single;
          fLastPlayed     : dword;
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

  constructor TTraincarSpriteSoundTarget.Create(Owner : TGameFocus; const TraincarSprite : ITraincarSprite; TraincarClass : PTraincarClass);
    begin
      inherited Create;
      fOwner := Owner;
      fTraincarSprite := TraincarSprite;
      fSoundData := TraincarClass.SoundData;
      UpdateSoundParameters;
    end;

  function TTraincarSpriteSoundTarget.GetSoundName : string;
    begin
      Result := fSoundData.wavefile;
    end;

  function TTraincarSpriteSoundTarget.GetSoundKind : integer;
    begin
      Result := integer(Self);
    end;

  function TTraincarSpriteSoundTarget.GetPriority : integer;
    begin
      Result := fSoundData.priority;
    end;

  function TTraincarSpriteSoundTarget.IsLooped : boolean;
    begin
      Result := true; //fSoundData.looped;
    end;

  function TTraincarSpriteSoundTarget.GetVolume : single;
    begin
      Result := fVolume*fSoundData.atenuation;
    end;

  function TTraincarSpriteSoundTarget.GetPan : single;
    begin
      Result := fPan;
    end;

  function TTraincarSpriteSoundTarget.ShouldPlayNow : boolean;
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

  function TTraincarSpriteSoundTarget.IsCacheable : boolean;
    begin
      Result := false;
    end;

  function TTraincarSpriteSoundTarget.IsEqualTo(const SoundTarget : ISoundTarget) : boolean;
    var
      SndTargetObj : TObject;
    begin
      SndTargetObj := SoundTarget.GetObject;
      if SndTargetObj is TTraincarSpriteSoundTarget
        then Result := fTraincarSprite = TTraincarSpriteSoundTarget(SndTargetObj).fTraincarSprite
        else Result := false;
    end;

  function TTraincarSpriteSoundTarget.GetObject : TObject;
    begin
      Result := Self;
    end;

  procedure TTraincarSpriteSoundTarget.UpdateSoundParameters;
    var
      screensize : TPoint;
      tmppt      : TPoint;
      x, y       : integer;
      u          : integer;
      ci, cj     : integer;
      Dist       : single;
    begin
      with fOwner, fMap do
        begin
          fConverter.MapToScreen(fView, fTraincarSprite.MapY, fTraincarSprite.MapX, x, y);
          tmppt := fView.ViewPtToScPt(Point(x, y));
          x := tmppt.x;
          y := tmppt.y;
          u := 2 shl fView.ZoomLevel;
          x := x + 2*u;
          screensize := Point(GetSystemMetrics(SM_CXSCREEN), GetSystemMetrics(SM_CYSCREEN));
          if abs(x - screensize.x/2) > cPanDeadZone
            then fPan := cLeftPan + (cRightPan - cLeftPan)*x/screensize.x
            else fPan := cCenterPan;
          boundvalue(cLeftPan, cRightPan, fPan);
          tmppt := fView.ScPtToViewPt(Point(screensize.x div 2, screensize.y div 2));
          fConverter.ScreenToMap(fView, tmppt.x, tmppt.y, ci, cj);
          Dist := sqrt(sqr(ci - fTraincarSprite.MapY) + sqr(cj - fTraincarSprite.MapX));
          if Dist < cMaxHearDist
            then fVolume := cMinVol + (1 - Dist/cMaxHearDist)*(cMaxVol - cMinVol)*(1 - abs(fView.ZoomLevel - ord(cBasicZoomRes))*cZoomVolStep)
            else fVolume := cMinVol;
          boundvalue(cMinVol, cMaxVol, fVolume);
        end;
    end;

  // TTraincarSpriteManager

  constructor TTraincarSpriteManager.Create(Map: TWorldMap; MinAnimInterval : integer; OwnsSprites : boolean; const TraincarsArray : IVehicleArray);
    begin
      inherited Create(Map, MinAnimInterval, OwnsSprites);
      fTraincarsArray := TraincarsArray;
      fTraincarsArray.RegisterNotificationProc(TraincarsArrayChanged);
    end;

  destructor TTraincarSpriteManager.Destroy;
    begin
      fTraincarsArray.UnRegisterNotificationProc(TraincarsArrayChanged);
      inherited;
    end;

  function TTraincarSpriteManager.GetSpriteCount : integer;
    begin
      Result := fTraincarsArray.getVehicleCount;
    end;

  function TTraincarSpriteManager.GetSprite(i : integer) : IMapSprite;
    begin
      Result := IMapSprite(fTraincarsArray.getVehicle(i).getCustomData);
    end;

  procedure TTraincarSpriteManager.SpriteMapPosChanged(const Sprite : IMapSprite);
    var
      SoundTarget : ISoundTarget;
      Focus       : TGameFocus;
      i           : integer;
    begin
      for i := 0 to pred(fMap.fFocuses.Count) do
        begin
          Focus := TGameFocus(fMap.fFocuses[i]);
          SoundTarget := ITraincarSprite(Sprite).SoundTarget; // >>
          if SoundTarget <> nil
            then
              begin
                SoundTarget.UpdateSoundParameters;
                Focus.fSoundManager.UpdateTarget(SoundTarget);
              end;
        end;
    end;

  function TTraincarSpriteManager.RegisterSprite(const Sprite : IMapSprite) : integer;
    var
      SoundTarget   : ISoundTarget;
      Focus         : TGameFocus;
      i             : integer;
      TraincarClass : PTraincarClass;
      traincaridx   : TTraincarIdx;
    begin
      traincaridx := fMap.SetTraincar(Sprite.MapY, Sprite.MapX, pointer(Sprite));
      ITraincarSprite(Sprite).Idx := traincaridx;
      TraincarClass := fMap.fManager.GetTraincarClass(Sprite.Id);
      for i := 0 to pred(fMap.fFocuses.Count) do
        begin
          Focus := TGameFocus(fMap.fFocuses[i]);
          if TraincarClass.SoundData.wavefile <> ''
            then
              begin
                SoundTarget := TTraincarSpriteSoundTarget.Create(Focus, ITraincarSprite(Sprite), TraincarClass);
                ITraincarSprite(Sprite).SoundTarget := SoundTarget; // >>
                Focus.fSoundManager.AddTargets(SoundTarget);
              end;
        end;
      Result := 0; // result doesn't matter here
    end;

  procedure TTraincarSpriteManager.UnregisterSprite(const Sprite : IMapSprite);
    var
      SoundTarget : ISoundTarget;
      Focus       : TGameFocus;
      i           : integer;
    begin
      SoundTarget := ITraincarSprite(Sprite).SoundTarget;
      if SoundTarget <> nil
        then
          for i := 0 to pred(fMap.fFocuses.Count) do
            begin
              Focus := TGameFocus(fMap.fFocuses[i]);
              Focus.fSoundManager.RemoveTarget(SoundTarget);
            end;
      fMap.RemoveTraincar(Sprite.MapY, Sprite.MapX, ITraincarSprite(Sprite).Idx);
    end;

  procedure TTraincarSpriteManager.RecacheSoundTargets;
    var
      SoundTarget    : ISoundTarget;
      Focus          : TGameFocus;
      i, j           : integer;
      TraincarClass  : PTraincarClass;
      Traincar       : IVehicle;
      TraincarSprite : ITraincarSprite;
    begin
      for i := 0 to pred(fTraincarsArray.getVehicleCount) do
        begin
          Traincar := fTraincarsArray.getVehicle(i);
          TraincarSprite := ITraincarSprite(Traincar.getCustomData);
          if TraincarSprite <> nil
            then
              begin
                TraincarClass := fMap.fManager.GetTraincarClass(TraincarSprite.Id);
                for j := 0 to pred(fMap.fFocuses.Count) do
                  begin
                    Focus := TGameFocus(fMap.fFocuses[j]);
                    if TraincarClass.SoundData.wavefile <> ''
                      then
                        begin
                          TraincarSprite.SoundTarget := nil;
                          SoundTarget := TTraincarSpriteSoundTarget.Create(Focus, TraincarSprite, TraincarClass);
                          TraincarSprite.SoundTarget := SoundTarget; // >>
                          Focus.fSoundManager.AddTargets(SoundTarget);
                        end;
                  end;
              end;
        end;
    end;

  function TTraincarSpriteManager.GetFullSpriteId(const Sprite : IMapSprite) : integer;
    begin
      Result := idTraincarMask or Sprite.Id;
    end;

  procedure TTraincarSpriteManager.TraincarsArrayChanged(TraincarsArray : IVehicleArray; ArrayChange : TArrayChange; const Info);
    var
      i              : integer;
      Traincar       : IVehicle;
      TraincarSprite : ITraincarSprite;
      traincaridx    : TTraincarIdx;
      TraincarClass  : PTraincarClass;
      Vehicle        : IVehicle absolute Info;
    begin
      fTraincarsArray := TraincarsArray;
      for i := 0 to pred(fMap.fFocuses.Count) do
        TGameFocus(fMap.fFocuses[i]).StartCaching;
      try
        case ArrayChange of
          achUpdate:
            for i := 0 to pred(fTraincarsArray.getVehicleCount) do
              begin
                Traincar := fTraincarsArray.getVehicle(i);
                TraincarSprite := ITraincarSprite(Traincar.getCustomData);
                if TraincarSprite = nil
                  then
                    begin
                      TraincarClass := fMap.fManager.GetTraincarClass(Traincar.getVisualClass);
                      TraincarSprite := TTraincarSprite.Create(Traincar, TrainCarClass, Self);
                      TraincarSprite._AddRef;
                      Traincar.setCustomData(pointer(TraincarSprite), sizeof(TraincarSprite));
                    end;
                TraincarSprite.AnimationTick;
                fMap.RemoveTraincar(TraincarSprite.OldMapY, TraincarSprite.OldMapX, TraincarSprite.Idx);
                traincaridx := fMap.SetTraincar(TraincarSprite.MapY, TraincarSprite.MapX, pointer(TraincarSprite));
                TraincarSprite.Idx := traincaridx;
              end;
          achVehicleDeletion:
            begin
              TraincarSprite := ITraincarSprite(Vehicle.getCustomData);
              UnregisterSprite(TraincarSprite);
            end;
        end;
      finally
        for i := 0 to pred(fMap.fFocuses.Count) do
          TGameFocus(fMap.fFocuses[i]).StopCaching;
      end;
    end;

end.
