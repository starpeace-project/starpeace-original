unit TimerTicker;

interface

  uses
    Classes, TimerTypes, ShutDown, TimerUtils, ThreadTimer;

  type
    TTickerTimer = TThreadTimer;

  const
    cTimerInterval = 40;

  type
    TTimerTicker =
      class(TInterfacedObject, ITicker, IShutDownTarget)
        private
          fTimer      : TTickerTimer;
          fTickeables : TList;
          procedure TimerTick;
        public
          constructor Create;
          destructor  Destroy; override;
        private // ITicker
          function  GetEnabled : boolean;
          procedure SetEnabled(which : boolean);
          function  Count : integer;
          function  GetTickeable(idx : integer) : ITickeable;
          procedure Attach(const which : ITickeable);
          procedure Detach(const which : ITickeable);
        private // IShutDownTarget
          function  GetPriority : integer;
          procedure OnSuspend;
          procedure OnResume;
          procedure OnShutDown;
      end;

  procedure AttachTickeable(const which : ITickeable);
  procedure DetachTickeable(const which : ITickeable);
  procedure EnableTicker;
  procedure DisableTicker;

implementation

  uses
    Windows;

  var
    theTicker : ITicker;

  // TTimerTicker

  constructor TTimerTicker.Create;
    begin
      inherited;
      fTickeables := TList.Create;
      fTimer := TTickerTimer.Create;
      fTimer.OnTimer := TimerTick;
      fTimer.Interval := cTimerInterval;
      fTimer.Enabled := true;
      AttachTarget(Self);
    end;

  destructor TTimerTicker.Destroy;
    begin
      DetachTarget(Self);
      fTimer.Free;
      fTickeables.Free;
      inherited;
    end;

  function TTimerTicker.GetEnabled : boolean;
    begin
      Result := fTimer.Enabled;
    end;

  procedure TTimerTicker.SetEnabled(which : boolean);
    begin
      fTimer.Enabled := which;
    end;

  function TTimerTicker.Count : integer;
    begin
      Result := fTickeables.Count;
    end;

  function TTimerTicker.GetTickeable(idx : integer) : ITickeable;
    begin
      Result := ITickeable(fTickeables[idx]);
    end;

  procedure TTimerTicker.Attach(const which : ITickeable);
    begin
      which._AddRef;
      fTickeables.Add(pointer(which));
    end;

  procedure TTimerTicker.Detach(const which : ITickeable);
    var
      idx : integer;
    begin
      idx := fTickeables.IndexOf(pointer(which));
      if idx <> -1
        then
          begin
            fTickeables.Delete(idx);
            which._Release;
          end;
    end;

  function TTimerTicker.GetPriority : integer;
    begin
      Result := 100;
    end;

  procedure TTimerTicker.OnSuspend;
    begin
      if fTimer <> nil
        then fTimer.Enabled := false;
    end;

  procedure TTimerTicker.OnResume;
    begin
      if fTimer <> nil
        then fTimer.Enabled := true;
    end;

  procedure TTimerTicker.OnShutDown;
    begin
      fTimer.Enabled := false;
    end;

  procedure TTimerTicker.TimerTick;
    var
      i            : integer;
      CurTickeable : ITickeable;
      mininterval  : integer;
      desinterval  : integer;
    begin
      if fTimer.Enabled
        then
          begin
            mininterval := high(mininterval);
            for i := 0 to pred(fTickeables.Count) do
              begin
                CurTickeable := ITickeable(fTickeables[i]);
                if CurTickeable.Enabled
                  then
                    begin
                      desinterval := ITickeable(fTickeables[i]).Tick;
                      if desinterval < mininterval
                        then mininterval := desinterval;
                    end;
              end;
            if (mininterval > 0) and (mininterval < 2*cTimerInterval)
              then fTimer.Interval := mininterval
              else fTimer.Interval := cTimerInterval;
          end;
    end;

  procedure AttachTickeable(const which : ITickeable);
    begin
      if theTicker = nil
        then theTicker := TTimerTicker.Create;
      theTicker.Attach(which);
    end;

  procedure DetachTickeable(const which : ITickeable);
    begin
      if theTicker <> nil
        then theTicker.Detach(which);
    end;

  procedure EnableTicker;
    begin
      if theTicker <> nil
        then theTicker.Enabled := true;
    end;

  procedure DisableTicker;
    begin
      if theTicker <> nil
        then theTicker.Enabled := false;
    end;

end.
