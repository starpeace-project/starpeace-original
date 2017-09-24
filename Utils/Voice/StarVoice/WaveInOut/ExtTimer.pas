unit ExtTimer;

interface

  uses
    SyncObjs;

  type
    TTimerProc = procedure of object;

    TExtTimer =
      class
        public
          constructor Create(aInterval : integer);
          destructor  Destroy;                                                  override;
        private
          fLock : TCriticalSection;
          procedure EnterSync;
          procedure LeaveSync;
        private
          fInterval   : integer;
          fResolution : integer;
          fId         : integer;
          fEnabled    : boolean;
          procedure SetEnabled(aEnabled : boolean);
          procedure SetInterval(aInterval : integer);
        public
          property Enabled : boolean    read fEnabled    write SetEnabled;
          property Resolution : integer read fResolution write fResolution;
          property Interval : integer   read fInterval   write SetInterval;
        public
          OnTimer : TTimerProc;
        private
          procedure Tick;
      end;

implementation

  uses
    Windows, mmSystem;

  procedure TimerProc(uTimerID, uMessage : UINT; dwUser, dw1, dw2 : DWORD); stdcall;
    var
      Timer : TExtTimer absolute dwUser;
    begin
      Timer.Tick;
    end;

  constructor TExtTimer.Create(aInterval : integer);
    begin
      inherited Create;
      Assert(aInterval > 0, 'TExtTimer.Create: Invalid timer interval');
      fLock := TCriticalSection.Create;
      fInterval := aInterval;
      fResolution := 10;
    end;

  destructor TExtTimer.Destroy;
    begin
      Enabled := false;
      fLock.Free;
      inherited;
    end;

  procedure TExtTimer.EnterSync;
    begin
      fLock.Enter;
    end;
    
  procedure TExtTimer.LeaveSync;
    begin
      fLock.Leave;
    end;
    
  procedure TExtTimer.SetEnabled(aEnabled : boolean);
    begin
      if aEnabled <> fEnabled
        then
          begin
            fEnabled := aEnabled;
            if fId <> 0
              then
                begin
                  EnterSync;
                  try
                    timeKillEvent(fId);
                    fId := 0;
                  finally
                    LeaveSync;
                  end;
                end;
            if fEnabled
              then fId := timeSetEvent(fInterval, fResolution, @TimerProc, integer(Self), TIME_PERIODIC);
          end;
    end;

  procedure TExtTimer.SetInterval(aInterval : integer);
    begin
      fInterval := aInterval;
      if Enabled
        then
          begin
            Enabled := false;
            Enabled := true;
          end;
    end;

  procedure TExtTimer.Tick;
    begin
      if Enabled and assigned(OnTimer)
        then
          try
            EnterSync;
            try
              OnTimer;
            finally
              LeaveSync;
            end;
          except
            Assert(false);
          end;
    end;

end.


