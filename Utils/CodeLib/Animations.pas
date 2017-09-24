unit Animations;

interface

  uses
    Windows, Classes, TimerTypes, ShutDown, VCLUtils;

  type
    IAnimationTarget =
      interface
        function  IsCyclic : boolean;
        function  CurrentFrame : integer;
        function  FrameCount : integer;
        function  FrameDelay(frame : integer) : integer;
        procedure AnimationTick(frame : integer);
        function  IsEqualTo(const AnimTarget : IAnimationTarget) : boolean;
        function  GetObject : TObject;
      end;

  type
    IAnimationCache =
      interface ['{F0ECCE60-881C-11d4-9147-0048546B6CD9}']
        procedure StartCaching;
        procedure StopCaching;
        procedure AddAnimationRect(const which : TRect);
      end;

  type
    TAnimManagerNotification = procedure of object;

  type
    TTargetCheck = function (const Target : IAnimationTarget; info : array of const) : boolean;

  type
    TAnimationManager =
      class(TInterfacedObject, IShutDownTarget, ITickeable)
        public
          constructor Create;
          destructor  Destroy;   override;
          procedure DestroyAll;
        private
          fCache                 : IAnimationCache;
          fOnAnimationCycleStart : TAnimManagerNotification;
          fOnAnimationCycleEnd   : TAnimManagerNotification;
          fEnabled               : boolean;
        public
          property Cache : IAnimationCache read fCache write fCache;
        private  // IShutDownTarget
          procedure IShutDownTarget.OnSuspend = Suspend;
          procedure IShutDownTarget.OnResume  = Resume;
          function  GetPriority : integer;
          procedure OnShutDown;
        private
          function Enabled : boolean;
          function Tick : integer;
        public
          procedure AddTargets(const which : array of IAnimationTarget);
          procedure RemoveTargets(const which : array of IAnimationTarget);
          procedure CheckRemoveTargets(Check : TTargetCheck; info : array of const);
          procedure Clear;
          procedure DeleteAll(const _class : array of TClass);
          procedure Suspend;
          procedure Resume;
        public
          property OnAnimationCycleStart : TAnimManagerNotification write fOnAnimationCycleStart;
          property OnAnimationCycleEnd   : TAnimManagerNotification write fOnAnimationCycleEnd;
        private
          fTargets : TList;
          function SearchTarget(const which : IAnimationTarget) : pointer;  // TTargetInfo
      end;

implementation

  uses
    SysUtils, TimerTicker;

  type
    TTargetInfo =
      class
        public
          constructor Create(Owner : TAnimationManager; const target : IAnimationTarget; StartTick : integer);
        private
          fOwner        : TAnimationManager;
          fTarget       : IAnimationTarget;
          fHighFrame    : integer;
          fStartFrame   : integer;
          fCurrentFrame : integer;
          fLastUpdate   : integer;
          fEnabled      : boolean;
          //fVisible      : boolean; //***
          function Tick(CurrentTick : integer) : integer;
      end;

  // Utils

  procedure FreeObject(var which);
    var
      aux : TObject;
    begin
      aux := TObject(which);
      TObject(which) := nil;
      aux.Free;
    end;

  // TTargetInfo

  constructor TTargetInfo.Create(Owner : TAnimationManager; const target : IAnimationTarget; StartTick : integer);
    begin
      inherited Create;
      fOwner     := Owner;
      fTarget    := target;
      fHighFrame := pred(target.FrameCount);
      fStartFrame := target.CurrentFrame;
      if (fStartFrame < 0) or (fStartFrame > fHighFrame)
        then raise Exception.Create('(fStartFrame < 0) or (fStartFrame > fHighFrame)');
      fCurrentFrame := fStartFrame;
      fLastUpdate   := StartTick;
      fEnabled      := true;
    end;

  function TTargetInfo.Tick(CurrentTick : integer) : integer;
    var
      ElapsedTicks : integer;
      FrameDelay   : integer;
    begin
      ElapsedTicks := CurrentTick - fLastUpdate;
      FrameDelay   := fTarget.FrameDelay(fCurrentFrame);
      if ElapsedTicks >= FrameDelay
        then
          begin
            if fCurrentFrame < fHighFrame
              then inc(fCurrentFrame)
              else fCurrentFrame := 0;
            try
              fTarget.AnimationTick(fCurrentFrame);
            except
            end;
            fLastUpdate := CurrentTick;
            if (fCurrentFrame = fHighFrame) and not fTarget.IsCyclic
              then
                begin
                  fEnabled := false;
                  Result := high(Result);
                end
              else Result := fTarget.FrameDelay(fCurrentFrame);
          end
        else Result := FrameDelay - ElapsedTicks;
    end;

  // TAnimationManager

  const
    cAnimTimerInterval = 60;

  constructor TAnimationManager.Create;
    begin
      inherited Create;
      fTargets := TList.Create;
      AttachTickeable(Self);
      Shutdown.AttachTarget(Self);
    end;

  destructor TAnimationManager.Destroy;
    begin
      DestroyAll;
      inherited;
    end;

  procedure TAnimationManager.DestroyAll;
    begin
      if fTargets<>nil
        then
          begin
            Clear;
            FreeAndNil(fTargets);
            Shutdown.DetachTarget(Self);
            fCache := nil;
            DetachTickeable(Self);
          end;
    end;

  function TAnimationManager.GetPriority : integer;
    begin
      Result := 100;
    end;

  procedure TAnimationManager.OnShutDown;
    begin
      fEnabled := false;
    end;

  function TAnimationManager.Enabled : boolean;
    begin
      Result := fEnabled;
    end;

  function TAnimationManager.Tick : integer;
    var
      aux         : TTargetInfo;
      mininterval : integer;
      desinterval : integer;
      i           : integer;
    begin
      if assigned(fCache)
        then fCache.StartCaching;
      if assigned(fOnAnimationCycleStart)
        then fOnAnimationCycleStart;
      try
        mininterval := high(mininterval);
        for i := 0 to pred(fTargets.Count) do
          begin
            aux := TTargetInfo(fTargets[i]);
            if (aux <> nil) and aux.fEnabled
              then
                begin
                  try
                    desinterval := aux.Tick(GetTickCount);
                  except
                  end;
                  if desinterval < mininterval
                    then mininterval := desinterval;
                end;
          end;
        if (mininterval > 0) and (mininterval < 2*cTimerInterval)
          then Result := mininterval
          else Result := cAnimTimerInterval;
      finally
        fTargets.Pack;
        if Assigned(fOnAnimationCycleEnd)
          then fOnAnimationCycleEnd;
        if assigned(fCache)
          then fCache.StopCaching;
      end;
    end;

  procedure TAnimationManager.AddTargets(const which : array of IAnimationTarget);
    var
      i : integer;
    begin
      for i := low(which) to high(which) do
        fTargets.Add(TTargetInfo.Create(Self, which[i], GetTickCount));
      fEnabled := fTargets.Count > 0;
    end;

  procedure TAnimationManager.RemoveTargets(const which : array of IAnimationTarget);
    var
      i   : integer;
      aux : TTargetInfo;
    begin
      for i := low(which) to high(which) do
        begin
          aux := TTargetInfo(SearchTarget(which[i]));
          if aux <> nil
            then
              begin
                fTargets.Remove(aux);
                aux.Free;
              end;
        end;
    end;

  procedure TAnimationManager.CheckRemoveTargets(Check : TTargetCheck; info : array of const);
    var
      i   : integer;
      aux : TTargetInfo;
    begin
      for i := 0 to pred(fTargets.Count) do
        begin
          aux := TTargetInfo(fTargets[i]);
          if (aux <> nil) and Check(aux.fTarget, info)
            then aux.fEnabled := false;
        end;
    end;

  procedure TAnimationManager.Clear;
    var
      i   : integer;
      aux : TTargetInfo;
    begin
      fEnabled := false;
      for i := pred(fTargets.Count) downto 0 do
        begin
          aux := TTargetInfo(fTargets[i]);
          if aux <> nil
            then
              begin
                fTargets[i] := nil;
                aux.Free;
              end;
        end;
      fTargets.Pack;
    end;

  procedure TAnimationManager.DeleteAll(const _class : array of TClass);
    var
      i   : integer;
      aux : TTargetInfo;
      lclass : TClass;
      j : integer;
    begin
      fEnabled := false;
      try
        for i := pred(fTargets.Count) downto 0 do
          begin
            aux := TTargetInfo(fTargets[i]);
            if (aux <> nil)
              then
                begin
                  lclass := aux.fTarget.GetObject.ClassType;
                  j := low(_class);
                  while j<high(_class) do
                    begin
                      if _class[j]=lclass
                        then
                          begin
                            fTargets[i] := nil;
                            aux.Free;
                            j := high(_class);
                          end
                        else inc(j);
                    end;
                end;
          end;
      except
      end;
      fTargets.Pack;
    end;

  procedure TAnimationManager.Suspend;
    begin
      fEnabled := false;
    end;

  procedure TAnimationManager.Resume;
    begin
      fEnabled := fTargets.Count > 0;
    end;

  function TAnimationManager.SearchTarget(const which : IAnimationTarget) : pointer; // TTargetInfo
    var
      res : TTargetInfo absolute Result;
      i   : integer;
    begin
      assert(fTargets.Count > 0);
      i := 0;
      res := nil;
      while (i < fTargets.Count) and (res = nil) do
        begin
          res := TTargetInfo(fTargets[i]);
          if (res <> nil) and (res.fTarget <> which)
            then
              begin
                res := nil;
                inc(i);
              end;
        end;
    end;

end.
