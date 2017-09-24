unit MapSprites;

interface

  uses
    Windows, Classes, GameTypes, ShutDown, TimerTypes, LanderTypes, MapTypes;

  type
    TSpriteImages = array [TAngle] of TGameImage;

  type
    IMapSprite =
      interface
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
        property  Id      : integer read GetId;
        property  Frame   : integer read GetFrame;
        property  Angle   : TAngle  read GetAngle;
        property  MapX    : integer read GetMapX;
        property  MapY    : integer read GetMapY;
        property  OldMapX : integer read GetOldMapX;
        property  OldMapY : integer read GetOldMapY;
      end;

  type
    IMapSpriteManager =
      interface
        function  GetSpriteCount : integer;
        function  GetSprite(i : integer) : IMapSprite;

        procedure UpdateSpriteRect(const Sprite : IMapSprite);
        procedure SpriteMapPosChanged(const Sprite : IMapSprite);
        function  RegisterSprite(const Sprite : IMapSprite) : integer;
        procedure UnregisterSprite(const Sprite : IMapSprite);
        function  IsSpriteVisible(const Sprite : IMapSprite) : boolean;
        function  GetSpriteImages(const Sprite : IMapSprite; const View : IGameView; out Images : TSpriteImages) : boolean;
        procedure AttachFocus(const focus : IGameFocus);
        procedure DetachFocus(const focus : IGameFocus);
        procedure NewView(const View : IGameView);
        procedure RecacheSoundTargets(soundsenabled : boolean);
        procedure Enable;
        procedure Disable;
        property  SpriteCount : integer read GetSpriteCount;
        property  Sprites[i : integer] : IMapSprite read GetSprite; default;
      end;

  type
    TMapSpriteManager =
      class(TInterfacedObject, IMapSpriteManager, IShutDownTarget, ITickeable)
        public
          constructor Create(const Map : IWorldMap; const Converter : ICoordinateConverter; const Manager : ILocalCacheManager; MinAnimInterval : integer; OwnsSprites : boolean);
          destructor  Destroy; override;
        protected // IMapSpriteManager
          function  GetSpriteCount : integer; virtual; abstract;
          function  GetSprite(i : integer) : IMapSprite; virtual; abstract;
          procedure UpdateSpriteRect(const Sprite : IMapSprite);
          procedure SpriteMapPosChanged(const Sprite : IMapSprite); virtual; abstract;
          function  RegisterSprite(const Sprite : IMapSprite) : integer; virtual; abstract;
          procedure UnregisterSprite(const Sprite : IMapSprite); virtual; abstract;
          function  IsSpriteVisible(const Sprite : IMapSprite) : boolean; virtual;
          function  GetSpriteImages(const Sprite : IMapSprite; const View : IGameView; out Images : TSpriteImages) : boolean; virtual;
          procedure AttachFocus(const focus : IGameFocus);
          procedure DetachFocus(const focus : IGameFocus);
          procedure NewView(const View : IGameView); virtual;
          procedure RecacheSoundTargets(soundsenabled : boolean); virtual; abstract;
          procedure Enable; virtual;
          procedure Disable; virtual;
        protected // IShutDownTarget
          function  GetPriority : integer;
          procedure OnSuspend;
          procedure OnResume;
          procedure OnShutDown;
        protected // ITickeable
          fInterval   : integer;
          fLastUpdate : integer;
          fEnabled    : boolean;
          function Enabled : boolean;
          function Tick : integer;
        protected
          fMap         : IWorldMap;
          fConverter   : ICoordinateConverter;
          fManager     : ILocalCacheManager;
          fOwnsSprites : boolean;
          procedure AnimationTick; virtual;
          function  GetFullSpriteId(const Sprite : IMapSprite) : integer; virtual; abstract;
        protected
          fExternallyDisabled : boolean;
        protected
          fFocuses : TList;
      end;

implementation

  uses
    TimerTicker, Animations;

  const
    cNoTimerInterval = -1; // this interval will cause not timer to be created at all

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

  // TMapSpriteManager

  constructor TMapSpriteManager.Create(const Map : IWorldMap; const Converter : ICoordinateConverter; const Manager : ILocalCacheManager; MinAnimInterval : integer; OwnsSprites : boolean);
    begin
      inherited Create;
      fMap := Map;
      fMap._Release; // >> cross referenced
      fConverter := Converter;
      fConverter._Release; // >> cross referenced
      fManager := Manager;
      fFocuses := TList.Create;
      if MinAnimInterval <> cNoTimerInterval
        then
          begin
            AttachTickeable(Self);
            fEnabled := true;
          end;
      ShutDown.AttachTarget(Self);
    end;

  destructor TMapSpriteManager.Destroy;
    begin
      ShutDown.DetachTarget(Self);
      DetachTickeable(Self);
      fFocuses.Free;
      pointer(fConverter) := nil; // >> cross referenced
      pointer(fMap) := nil; // >> cross referenced
      inherited;
    end;

  procedure TMapSpriteManager.UpdateSpriteRect(const Sprite : IMapSprite);
    var
      R          : TRect;
      OldArea    : TRect;
      oldx, oldy : integer;
      Area       : TRect;
      x, y       : integer;
      i          : integer;
      focus      : IGameFocus;
      view       : IGameView;
    begin
      with fConverter do
        for i := 0 to pred(fFocuses.Count) do
          begin
            focus := IGameFocus(fFocuses[i]);
            view := focus.GetView;
            MapToScreen(view, Sprite.OldMapY, Sprite.OldMapX, oldx, oldy);
            oldx := oldx + Sprite.GetOldBlockX(view);
            oldy := oldy + Sprite.GetOldBlockY(view);
            OldArea := Rect(max(oldx, 0), max(oldy, 0), oldx + Sprite.GetWidth(view), oldy + Sprite.GetHeight(view));
            MapToScreen(view, Sprite.MapY, Sprite.MapX, x, y);
            x := x + Sprite.GetBlockX(view);
            y := y + Sprite.GetBlockY(view);
            Area := Rect(max(x, 0), max(y, 0), x + Sprite.GetWidth(view), y + Sprite.GetHeight(view));
            UnionRect(R, Area, OldArea);
            (focus as IAnimationCache).AddAnimationRect(R);
          end;
    end;

  function TMapSpriteManager.IsSpriteVisible(const Sprite : IMapSprite) : boolean;
    var
      i          : integer;
      FocusCount : integer;
      imin, jmin : integer;
      imax, jmax : integer;
      Visible    : boolean;
    begin
      with fMap do
        begin
          i := 0;
          FocusCount := fFocuses.Count;
          if FocusCount > 0
            then
              repeat
                fConverter.GetViewRegion(IGameFocus(fFocuses[i]).GetView, imin, jmin, imax, jmax);
                Visible := (Sprite.MapX >= jmin) and (Sprite.MapY >= imin) and (Sprite.MapX <= jmax) and (Sprite.MapY <= imax);
                inc(i);
              until (i >= FocusCount) or Visible
            else Visible := false;
        end;
      Result := Visible;
    end;

  function TMapSpriteManager.GetSpriteImages(const Sprite : IMapSprite; const View : IGameView; out Images : TSpriteImages) : boolean;
    var
      Focus    : IGameFocus;
      Imager   : IImager;
      id       : integer;
      angle    : TAngle;
    begin
      Focus := View.GetFocus;
      if Focus <> nil
        then
          begin
            Imager := Focus.GetImager;
            if Imager <> nil
              then
                begin
                  id := GetFullSpriteId(Sprite);
                  for angle := low(angle) to high(angle) do
                    Images[angle] := Imager.GetObjectImage(id, angle);
                  Result := true;
                end
              else Result := false;
          end
        else Result := false;
    end;

  procedure TMapSpriteManager.AttachFocus(const focus : IGameFocus);
    begin
      fFocuses.Add(pointer(focus));
    end;

  procedure TMapSpriteManager.DetachFocus(const focus : IGameFocus);
    begin
      fFocuses.Remove(pointer(focus));
    end;

  procedure TMapSpriteManager.NewView(const View : IGameView);
    var
      Sprite : IMapSprite;
      i      : integer;
    begin
      for i := 0 to pred(GetSpriteCount) do
        begin
          Sprite := GetSprite(i);
          if Sprite <> nil
            then Sprite.NewView(View);
        end;
    end;

  procedure TMapSpriteManager.Enable;
    begin
      fExternallyDisabled := false;
      fEnabled := true;
    end;

  procedure TMapSpriteManager.Disable;
    var
      Sprite : IMapSprite;
      i      : integer;
    begin
      fExternallyDisabled := true;
      fEnabled := false;
      try
        for i := 0 to pred(GetSpriteCount) do
          try
            begin
              Sprite := GetSprite(i);
              if Sprite <> nil
                then Sprite._Release;
            end;
          except
          end;
      except
      end;
    end;

  function TMapSpriteManager.GetPriority : integer;
    begin
      Result := 100;
    end;

  procedure TMapSpriteManager.OnSuspend;
    begin
      fEnabled := false;
    end;

  procedure TMapSpriteManager.OnResume;
    begin
      if not fExternallyDisabled
        then fEnabled := true;
    end;

  procedure TMapSpriteManager.OnShutDown;
    var
      i         : integer;
      CurSprite : IMapSprite;
    begin
      fEnabled := false;
      for i := 0 to pred(GetSpriteCount) do
        begin
          CurSprite := GetSprite(i);
          if CurSprite <> nil
            then CurSprite._Release; // >> is this always valid?
        end;
    end;

  function TMapSpriteManager.Enabled : boolean;
    begin
      Result := fEnabled;
    end;

  function TMapSpriteManager.Tick;
    var
      CurrentTicks : integer;
      ElapsedTicks : integer;
      FrameDelay   : integer;
    begin
      CurrentTicks := GetTickCount;
      ElapsedTicks := CurrentTicks - fLastUpdate;
      FrameDelay   := fInterval;
      if ElapsedTicks >= FrameDelay
        then
          begin
            AnimationTick;
            fLastUpdate := CurrentTicks;
            Result := FrameDelay;
            {
            inc(fLastUpdate, FrameDelay); // we could do this
            Result := FrameDelay - (CurrentTicks - fLastUpdate);
            }
          end
        else Result := FrameDelay - ElapsedTicks;
    end;

  procedure TMapSpriteManager.AnimationTick;
    var
      i         : integer;
      CurSprite : IMapSprite;
    begin
      for i := 0 to pred(fFocuses.Count) do
        (IGameFocus(fFocuses[i]) as IAnimationCache).StartCaching;
      try
        for i := 0 to pred(GetSpriteCount) do
          begin
            CurSprite := GetSprite(i);
            try
              if CurSprite <> nil
                then CurSprite.AnimationTick;
            except
              // a sprite generating an exception shouldn't affect the others (blinking bug???)
            end;
          end;
      finally
        for i := 0 to pred(fFocuses.Count) do
          (IGameFocus(fFocuses[i]) as IAnimationCache).StopCaching;
      end;
    end;
    
end.
