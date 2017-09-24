unit Pedestrian;

interface
 {$ifdef Pedestrian}
  uses Windows, GameTypes, LanderTypes, SoundTypes, Circuits, Roads, MapSprites, VCLUtils, MapTypes, Collection,
    classes, sysutils;

  type
    TPedestrianId = word;

  const
    carNone = high(TPedestrianId);

    cPedestrianSpeed        = 0.5;  // blocks x second
    cPedestrianFramesPerSec = 10;
    cPedestriansFrameDelay  = 1000 div cPedestrianFramesPerSec;
    cPedestriansTimerInterval  = cPedestriansFrameDelay;


  type
    PPedestrianClasses = ^TPedestrianClasses;
    TPedestrianClasses = array [0..0] of PPedestrianClass;

    TPedestrianManager = class;

    IPedestrian =
      interface(IMapSprite)
        function GetShiftX: integer;
        function GetShiftY: integer;
      end;

    IPedestrianManager =
      interface(IMapSpriteManager)
        procedure Enable;
        procedure Disable;
        procedure DestroyAll;
        procedure RegionUpdated(imin, jmin, imax, jmax : integer);
        function  GetImageInfo(const id: integer; const Angle: TAngle): PImageInfo;
      end;

    TPedestrian =
      class(TInterfacedObject, IPedestrian)
        public
          function  GetId : integer;
          function  GetFrame : integer;
          function  GetAngle : TAngle;
          function  GetMapX : integer;
          function  GetMapY : integer;
          function  GetOldMapX : integer;
          function  GetOldMapY : integer;
          function  GetLWidth: integer;
          function  GetLHeight: integer;
        public
          constructor Create(Id : integer; const IniMapPos : TPoint; const Manager : IPedestrianManager; const PedestrianClass : PPedestrianClass; const Converter: ICoordinateConverter);
        private // IMapSprite
          procedure AnimationTick;
          procedure NewView(const View : IGameView);
          function  GetBlockX(const View : IGameView) : integer;
          function  GetBlockY(const View : IGameView) : integer;
          function  GetOldBlockX(const View : IGameView) : integer;
          function  GetOldBlockY(const View : IGameView) : integer;
          function  GetWidth(const View : IGameView) : integer;
          function  GetHeight(const View : IGameView) : integer;
          function GetShiftX: integer;
          function GetShiftY: integer;
          property  Id      : integer read GetId;
          property  Frame   : integer read GetFrame;
          property  Angle   : TAngle  read GetAngle;
          property  MapX    : integer read GetMapX;
          property  MapY    : integer read GetMapY;
          property  OldMapX : integer read GetOldMapX;
          property  OldMapY : integer read GetOldMapY;
          property  Width   : integer read GetLWidth;
          property  Height  : integer read GetLHeight;
        private
          fMapPos          : TPoint;
          fOldMapPos       : TPoint;
          fId              : integer;
          fAngle           : TAngle;
          fFrame           : integer;
          fOldFrame        : integer;
          fImageInfo       : PImageInfo;
          fPedestrianManager  : IPedestrianManager;
          fPedestrianClass : PPedestrianClass;
          fLastCurrentTick : integer;
          fTickCount       : word;
          fConverter       : ICoordinateConverter;
          fBlockX          : integer;
          fBlockY          : integer;
        end;

    TPedestrianManager =
      class(TMapSpriteManager, IPedestrianManager)
          constructor Create(const Map : IWorldMap; const Converter : ICoordinateConverter; const Manager : ILocalCacheManager; MinAnimInterval : integer; OwnsSprites : boolean);
          destructor  Destroy; override;
          procedure   DestroyAll;
        protected // IMapSpriteManager
          procedure RegionUpdated(imin, jmin, imax, jmax : integer);
          function  GetSpriteCount : integer; override;
          function  GetSprite(i : integer) : IMapSprite; override;
          procedure SpriteMapPosChanged(const Sprite : IMapSprite); override;
          function  RegisterSprite(const Sprite : IMapSprite) : integer; override;
          procedure UnregisterSprite(const Sprite : IMapSprite); override;
          procedure RecacheSoundTargets(soundsenabled : boolean); override;
          procedure AnimationTick; override;
        protected // IPedestrianManager
          function  GetFullSpriteId(const Sprite : IMapSprite) : integer; override;
          function  GetImageInfo(const id: integer; const Angle: TAngle): PImageInfo;
        private // valid classes cache
          fClassCount   : integer;
          fClasses      : PPedestrianClasses;
          fClassesIndex : array [idPedestrian] of word;
          fPedestrianInstances : TInterfaceList;
          fConverter    : ICoordinateConverter;
      end;
{$endif}
implementation

{$ifdef Pedestrian}
uses
  Animations, Concrete;

{ TPedestrian }
constructor TPedestrian.Create(Id : integer; const IniMapPos : TPoint; const Manager : IPedestrianManager; const PedestrianClass : PPedestrianClass; const Converter: ICoordinateConverter);
  begin                                          
    inherited create;
    fMapPos := IniMapPos;
    fId     := Id;
    fAngle  := agN;
    fPedestrianClass := PedestrianClass;
    if PedestrianClass<>nil
      then
        begin
          fImageInfo := @PedestrianClass.ImageInfo[fAngle];
          fTickCount :=  PedestrianClass.DelayTick;
        end
      else fTickCount := 1;

    fPedestrianManager := Manager;
    with fPedestrianManager do
      begin
        RegisterSprite(IPedestrian(self));
        SpriteMapPosChanged(IPedestrian(Self));
      end;
  end;

procedure TPedestrian.AnimationTick;
  var
    CurrentTick : integer;
  begin
    if fPedestrianManager.IsSpriteVisible(IPedestrian(self))
      then
        begin
          CurrentTick := GetTickCount;
          if abs(fLastCurrentTick-CurrentTick)>fTickCount
            then
              begin
                if fImageInfo<>nil
                  then
                    begin
                      inc(fBlockX);
                      if fBlockX>fImageInfo.Width
                        then fBlockX := 0;
                      inc(fFrame);
                      if fFrame>=fImageInfo.FrameCount
                        then fFrame := 0;
                    end
                  else fFrame := 0;
//                if (fOldFrame <> fFrame) and (fPedestrianManager <> nil)
  //                then
                    begin
                       if (fConverter<>nil)
                         then
                           begin
                             //fConverter.GetViewRegion(
                             ;//fPedestrianManager.fConverter.
                           end;
                       fOldMapPos := fMapPos;
                       fPedestrianManager.UpdateSpriteRect(IPedestrian(Self));
                    end;
                fLastCurrentTick := CurrentTick;
              end;
        end;
  end;

function TPedestrian.GetAngle: TAngle;
  begin
    result := fAngle;
  end;

function TPedestrian.GetBlockX(const View: IGameView): integer;
  begin
    if fImageInfo<>nil
      then result := fBlockX
      else result := 0;
  end;

function TPedestrian.GetBlockY(const View: IGameView): integer;
  begin
    if fImageInfo<>nil
      then result := fBlockY
      else result := 0;
  end;

function TPedestrian.GetFrame: integer;
  begin
    result := fFrame;
  end;

function TPedestrian.GetHeight(const View: IGameView): integer;
  begin
    if fImageInfo<>nil
      then result := fImageInfo.Height
      else result := 0;
  end;

function TPedestrian.GetShiftX: integer;
  begin
  end;

function TPedestrian.GetShiftY: integer;
  begin
  end;

function TPedestrian.GetId: integer;
  begin
    result := fId;
  end;

function TPedestrian.GetMapX: integer;
  begin
    result := fMapPos.x;
  end;

function TPedestrian.GetMapY: integer;
  begin
    result := fMapPos.y;
  end;

function TPedestrian.GetOldBlockX(const View: IGameView): integer;
  begin
    if fImageInfo<>nil
      then result := fImageInfo.width
      else result := 0;
    result := 0;
  end;

function TPedestrian.GetOldBlockY(const View: IGameView): integer;
  begin
    if fImageInfo<>nil
      then result := fImageInfo.Height
      else result := 0;
    result := 0;
  end;

function TPedestrian.GetOldMapX: integer;
  begin
    result := fOldMapPos.x;
  end;

function TPedestrian.GetOldMapY: integer;
  begin
    result := fOldMapPos.y;
  end;

function  TPedestrian.GetLWidth: integer;
  begin
    if fImageInfo<>nil
      then result := fImageInfo.Width
      else result := 0;
  end;

function  TPedestrian.GetLHeight: integer;
  begin
    if fImageInfo<>nil
      then result := fImageInfo.Height
      else result := 0;
  end;

function TPedestrian.GetWidth(const View: IGameView): integer;
  begin
    if fImageInfo<>nil
      then result := fImageInfo.Width
      else result := 0;
  end;

procedure TPedestrian.NewView(const View: IGameView);
begin

end;

{ TPedestrianManager }
constructor TPedestrianManager.Create(const Map: IWorldMap; const Converter: ICoordinateConverter; const Manager: ILocalCacheManager; MinAnimInterval: integer; OwnsSprites: boolean);
  var
    i        : integer;
    id       : idcar;
    PedestrianClass : PPedestrianClass;
  begin
    inherited;
    fInterval := cPedestriansTimerInterval;
    fClassCount := fManager.GetPedestrianClassCount;
    getmem(fClasses, fClassCount*sizeof(fClasses[0]));
    fillchar(fClasses^, fClassCount*sizeof(fClasses[0]), 0);
    fillchar(fClassesIndex, sizeof(fClassesIndex), $FF);
    i := 0;
    for id := low(id) to high(id) do
      begin
        PedestrianClass := fManager.GetPedestrianClass(id);
        if (PedestrianClass <> nil) and PedestrianClass.valid
          then
            try
              fClasses[i] := PedestrianClass;
              fClassesIndex[PedestrianClass.id] := i;
              inc(i);
            except
            end;
      end;
    fPedestrianInstances := TInterfaceList.Create;
    fConverter := Converter;
  end;

destructor TPedestrianManager.Destroy;
  begin
    DestroyAll;
    inherited;
  end;

procedure TPedestrianManager.AnimationTick;
  var
    i         : integer;
    CurSprite : IMapSprite;
  begin
    for i := 0 to pred(fFocuses.Count) do
      (IGameFocus(fFocuses[i]) as IAnimationCache).StartCaching;
    try
      for i := pred(GetSpriteCount) downto 0 do
        begin
          CurSprite := GetSprite(i);
          try
            if (CurSprite <> nil) and IsSpriteVisible(CurSprite)
              then
                begin
                  CurSprite.AnimationTick;
                  CurSprite := nil;
                end;
          except
            // a sprite generating an exception shouldn't affect the others (blinking bug???)
          end;
        end;
    finally
      for i := pred(fFocuses.Count) downto 0 do
        (IGameFocus(fFocuses[i]) as IAnimationCache).StopCaching;
    end;
  end;


procedure TPedestrianManager.DestroyAll;
  begin
    if fClasses<>nil
      then
        begin
          freemem(fClasses);
          fClasses := nil;
        end;
    fPedestrianInstances.Free;
    fPedestrianInstances := nil;
    fConverter := nil;
  end;

function TPedestrianManager.GetFullSpriteId(const Sprite: IMapSprite): integer;
  begin
    result := idPedestrianMask or Sprite.Id;
  end;

function TPedestrianManager.GetImageInfo(const id: integer; const Angle: TAngle): PImageInfo;
  var
    p: PPedestrianClass;
  begin
     p := fManager.GetPedestrianClass(id);
     if (p<>nil) and p.valid
       then result := @p.ImageInfo[Angle]
       else result := nil;
  end;

function TPedestrianManager.GetSprite(i: integer): IMapSprite;
  begin
    if i<fPedestrianInstances.Count
      then result := IMapSprite(fPedestrianInstances[i])
      else result := nil;
  end;

function TPedestrianManager.GetSpriteCount: integer;
  begin
    result := fPedestrianInstances.Count;
  end;

procedure TPedestrianManager.RecacheSoundTargets(soundsenabled: boolean);
  begin
    inherited;
  end;

procedure TPedestrianManager.RegionUpdated(imin, jmin, imax, jmax : integer);
  var
    i, j : integer;
    k : integer;
    {$ifOpt d+}
    s : string;
    {$endif}
    id : integer;
    Concrete : TConcrete;
    pe : TPedestrians;
  begin
    if (not fExternallyDisabled) and (fClasses<>nil)
      then
        begin
          with fMap do
            begin
              for i:= iMin to iMax do
                for j:=jMin to jMax do
                  if (fMap.CheckForConcrete(i, j)) and
                     (not fMap.CheckForBuilding(i, j)) and
                     (fMap.Getroad(i,j)=RoadNone) and
                     (fMap.GetConcrete(i, j)=cFullConcrete) and
                     (fMap.GetPedestrian(i, j)=nil)
                    then
                      begin
                        //Concrete := GetConcrete(i, j);
                        //if (Concrete=cSpecialConcrete) or (Concrete=cFullConcrete)
                          //then
                          pe := fMap.GetPedestrian(j, i);
                          if pe=nil
                            then
                              begin
                                k := 1;
                                TPedestrian.Create(1, Point(j, i), Self, fClasses[k], fConverter);
                                //if (j+i mod 2) = 0
                                  //then TPedestrian.Create(0, Point(j, i), Self, fClasses[0], fConverter)
    //                              else TPedestrian.Create(1, Point(j, i), Self, fClasses[0], fConverter);
                              end;
                      end;
            end;
        end;
    {$ifOpt d+}
      s := inttostr(fPedestrianInstances.Count);
      Outputdebugstring(pchar(s));
    {$endif}
  end;

function TPedestrianManager.RegisterSprite(const Sprite: IMapSprite): integer;
  begin
    fPedestrianInstances.Add(Sprite);
  end;

procedure TPedestrianManager.SpriteMapPosChanged(const Sprite: IMapSprite);
  begin
    with fMap, Sprite do
      begin
        fMap.RemovePedestrian(OldMapY, OldMapX, Sprite);
        fMap.AddPedestrian(MapY, MapX, Sprite);
      end;
  end;

procedure TPedestrianManager.UnregisterSprite(const Sprite: IMapSprite);
  begin
    fPedestrianInstances.Remove(Sprite);
  end;
{$endif}
end.
