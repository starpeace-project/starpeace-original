unit ThreadTimer;

interface

uses
  Windows, Threads;

const
  cDefaultInterval = 1000;
  cDefaultLeadTime = 100; 
  
type
  TNotifyProc = procedure of object;

type
  TThreadTimer =
    class
      public
        constructor Create;
        destructor  Destroy;   override;
      private
        fInterval : cardinal;
        fLeadTime : cardinal;
        fEnabled  : boolean;
        fOnTimer  : TNotifyProc;
        procedure SetInterval(which : cardinal);
        procedure SetLeadTime(which : cardinal);
        procedure SetEnabled(which : boolean);
        procedure SetOnTimer(which : TNotifyProc);
      published
        property Interval : cardinal    read fInterval write SetInterval;
        property LeadTime : cardinal    read fLeadTime write SetLeadTime;
        property Enabled  : boolean     read fEnabled  write SetEnabled;
        property OnTimer  : TNotifyProc read fOnTimer  write SetOnTimer;
      private
        fThread : TAxlThread;
        fEvent  : thandle;
        procedure ControlManager(const which : array of const);
        procedure TimerTick(const which : array of const);
    end;


implementation


uses
  SysUtils;


// TThreadTimer

constructor TThreadTimer.Create;
  begin
    inherited;
    fInterval := cDefaultInterval;
    fLeadTime := cDefaultLeadTime;
    fEvent    := CreateEvent(nil, false, false, nil);
    fThread   := TExclusiveThread.Create(priNormal);
  end;

destructor TThreadTimer.Destroy;
  begin
    fOnTimer := nil;   // Go out if waiting
    fThread.Free;
    CloseHandle(fEvent);
    inherited;
  end;

procedure TThreadTimer.SetInterval(which : cardinal);
  begin
    assert(which > 0);
    fInterval := which;
  end;

procedure TThreadTimer.SetLeadTime(which : cardinal);
  begin
    assert(which > 0);
    fLeadTime := which;
  end;

procedure TThreadTimer.SetEnabled(which : boolean);
  begin
    if which <> fEnabled
      then
        begin
          fEnabled := which;
          if tsRunning in fThread.State
            then SetEvent(fEvent);
        end;
  end;

procedure TThreadTimer.SetOnTimer(which : TNotifyProc);
  var
    old : TNotifyProc;
  begin
    if @which <> @fOnTimer
      then
        begin
          old := fOnTimer;
          fOnTimer := which;
          if not assigned(old)
            then fThread.Defer(ControlManager, [0])
            else
              if not assigned(which)
                then SetEvent(fEvent);
        end;
  end;

procedure TThreadTimer.ControlManager(const which : array of const);
  var
    Delay    : dword;
    ticks    : cardinal;
    LeadTime : cardinal;
  begin
    if fLeadTime < fInterval
      then LeadTime := fLeadTime
      else LeadTime := 2*fInterval div 3;
    if fEnabled
      then Delay := fInterval
      else Delay := INFINITE;
    while assigned(fOnTimer) do
      if WaitForSingleObject(fEvent, Delay) = WAIT_TIMEOUT
        then
          if assigned(fOnTimer)
            then
              begin
                ticks := GetTickCount;
                try
                  Join(TimerTick, which);
                except
                  // *** protect against exceptions on timer tick, this would kill the thread
                  // and stop the timer forever
                end;
                if fEnabled
                  then
                    begin
                      ticks := GetTickCount - ticks;
                      if (fInterval > ticks + LeadTime)
                        then Delay := fInterval - ticks
                        else Delay := LeadTime;
                    end
                  else Delay := INFINITE;
              end
            else Delay := INFINITE
        else   // Even pulsed
          if fEnabled
            then Delay := fInterval
            else Delay := INFINITE;
  end;

procedure TThreadTimer.TimerTick(const which : array of const);
  begin
    if assigned(fOnTimer)
      then fOnTimer();
  end;


end.
