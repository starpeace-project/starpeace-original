unit TimerUtils;

// Copyright (c) 1996 Hitzel Cruz & Jorge Romero Gomez, Merchise.

interface

  {$LONGSTRINGS ON}
  {$BOOLEVAL OFF}

  uses
    Windows, MMSystem, Messages, SysUtils, Classes, Forms;

  // Timers --------------------------------------------------------------------------------------

  type
    ETimerError = class( Exception );

  type
    TNotifyProc = procedure of object;

  // TSimpleTimer

  type
    TTicker =
      class
        protected
          fInterval   : integer;
          fResolution : integer;
          fEnabled    : boolean;
          fOntimer    : TNotifyProc;
          fPostTicks  : boolean;

        public
          constructor Create;   // This is an abstract class!
          destructor  Destroy;                                                   override;

        public
          procedure Pause;
          procedure Resume;

        protected
          procedure Update;                                                      virtual; abstract;
          procedure Timer;                                                       virtual;

        protected
          procedure SetEnabled( aEnabled : boolean );                            virtual;
          procedure SetOnTimer( aProc : TNotifyProc );                           virtual;
          procedure SetPostTicks( TicksPosted : boolean );                       virtual;
          procedure SetInterval( aInterval : integer );                          virtual; abstract;
          procedure SetResolution( aInterval : integer );                        virtual; abstract;

        published
          property Interval : integer    read fInterval   write SetInterval;
          property Resolution : integer  read fResolution write SetResolution;
          property Enabled : boolean     read fEnabled    write SetEnabled;
          property OnTimer : TNotifyProc read fOnTimer    write SetOnTimer;
          property PostTicks : boolean   read fPostTicks  write SetPostTicks;
      end;

  type
    TSimpleTimer =
      class( TTicker )
        protected
          WndHandle : HWnd;
          TimerId   : integer;

        public
          constructor Create;
          destructor  Destroy;                                                   override;

        protected
          procedure Update;                                                      override;
          procedure SetInterval( aInterval : integer );                          override;
          procedure SetResolution( aInterval : integer );                        override;
          procedure InternalWndProc( var Msg : TMessage );
      end;

  // TEnhancedTimer

  type
    TEnhancedTimer =
      class( TTicker )
        protected
          WndHandle : HWnd;
          TimerId   : integer;

        public
          constructor Create;
          destructor  Destroy;                                                   override;

        protected
          procedure Update;                                                      override;
          procedure SetInterval( aInterval : integer );                          override;
          procedure SetResolution( aInterval : integer );                        override;
          procedure SetPostTicks( TicksPosted : boolean );                       override;
          procedure InternalWndProc( var Msg : TMessage );
      end;

  // TTrustedEnhTimer

  type
    TTrustedEnhTimer = class;

    TTimerThread =
      class( TThread )
        private
          fOwner  : TTrustedEnhTimer;
          TimerId : integer;
          Event   : THandle;
        public
          constructor Create( aOwner : TTrustedEnhTimer );
          destructor  Destroy; override;
        public
          procedure InitTicking;
          procedure ResetTicking;
        protected
          procedure Timer;
          procedure Execute; override;
      end;

    TTrustedEnhTimer =
      class( TTicker )
        protected
          fTimerThread : TTimerThread;
          fLostEvents  : integer;

        public
          constructor Create;
          destructor  Destroy;                                                   override;

        protected
          procedure Update;                                                      override;
          procedure Timer;                                                       override;
          procedure SetInterval( aInterval : integer );                          override;
          procedure SetResolution( aInterval : integer );                        override;

        public
          property LostEvents : integer read fLostEvents;
      end;

  // Miscellaneous -------------------------------------------------------------------------------

  function TimerMinResolution : integer;
  function TimerMaxResolution : integer;
  function EnhTimerMinResolution : integer;
  function EnhTimerMaxResolution : integer;

  function AdjustInterval( Interval, MinResolution, MaxResolution : integer ) : integer;

  function TicksToTime( Ticks : longint ) : string;
  
  // Stopwatches ---------------------------------------------------------------------------------

  type
    TStopWatch =
      class
        procedure Reset;
        procedure Start;
        procedure Stop;

        function ElapsedTicks : longint;
        function ElapsedTime : string;

        private
          fStartedAt  : longint;
          fLastPeriod : longint;
      end;

implementation

  uses
    NumUtils, mr_StrUtils;

  // Stopwatches ---------------------------------------------------------------------------------

  procedure TStopWatch.Reset;
    begin
      fStartedAt  := timeGetTime;
      fLastPeriod := 0;
    end;

  procedure TStopWatch.Start;
    begin
      fStartedAt  := longint(timeGetTime) - fLastPeriod;
      fLastPeriod := 0;
    end;

  procedure TStopWatch.Stop;
    begin
      fLastPeriod := longint(timeGetTime) - fStartedAt;
      fStartedAt  := 0;
    end;

  function TStopWatch.ElapsedTicks : longint;
    begin
      if fStartedAt = 0
        then Result := fLastPeriod
        else Result := longint(timeGetTime) - fStartedAt;
    end;

  function TStopWatch.ElapsedTime : string;
    begin
      Result := TicksToTime( ElapsedTicks );
    end;

  // Timers --------------------------------------------------------------------------------------

  const
    defEnabled  = false;
    defInterval = 1;

  constructor TTicker.Create;
    begin
      inherited;

      fInterval   := defInterval;
      fEnabled    := defEnabled;
      fResolution := TimerMinResolution;
    end;

  destructor TTicker.Destroy;
    begin
      Enabled := false;
      inherited;
    end;

  procedure TTicker.Pause;
    begin
      Enabled := false;
    end;

  procedure TTicker.Resume;
    begin
      Enabled := true;
    end;

  procedure TTicker.SetPostTicks( TicksPosted : boolean );
    begin
    end;

  procedure TTicker.SetOnTimer( aProc : TNotifyProc );
    begin
      fOnTimer := aProc;
      Update;
    end;

  procedure TTicker.SetEnabled( aEnabled : boolean );
    begin
      if fEnabled <> aEnabled
        then
          begin
            fEnabled := aEnabled;
            Update;
          end;
    end;

  procedure TTicker.Timer;
    begin
      if Assigned( fOnTimer )
        then fOnTimer;
    end;

  // TSimpleTimer

  constructor TSimpleTimer.Create;
    begin
      inherited;

      fPostTicks := true;
      WndHandle  := AllocateHwnd( InternalWndProc );
    end;

  destructor  TSimpleTimer.Destroy;
    begin
      DeallocateHwnd( WndHandle );
      inherited;
    end;

  procedure TSimpleTimer.InternalWndProc( var Msg : TMessage );
    begin
      with Msg do
        if Msg = WM_TIMER
          then
            try
              Timer;
            except
              Application.HandleException( Self );
            end
          else Result := DefWindowProc( WndHandle, Msg, wParam, lParam );
    end;

  procedure TSimpleTimer.Update;
    begin
      if TimerId <> 0
        then
          begin
            KillTimer( WndHandle, TimerId );
            TimerId := 0;
          end;
      if Enabled and Assigned( fOnTimer ) and ( fInterval <> 0 )
        then TimerId := SetTimer( WndHandle, 1, fInterval, nil );
    end;

  procedure TSimpleTimer.SetInterval( aInterval : integer );
    begin
      if fInterval <> aInterval
        then
          begin
            fInterval := AdjustInterval( aInterval, TimerMinResolution, TimerMaxResolution );
            Update;
          end;
    end;

  procedure TSimpleTimer.SetResolution( aInterval : integer );
    begin
    end;

  var
    tc         : TTimeCaps;
    etInterval : integer;

  // TEnhancedTimer

  constructor TEnhancedTimer.Create;
    begin
      inherited;

      fResolution := TimerMinResolution;
      fInterval   := etInterval;
      fEnabled    := defEnabled;
      timeBeginPeriod( etInterval );
      WndHandle := AllocateHwnd( InternalWndProc );
    end;

  destructor TEnhancedTimer.Destroy;
    begin
      Enabled := false;
      DeallocateHwnd( WndHandle );
      timeEndPeriod( etInterval );

      inherited;
    end;

  const
    WM_INTERNALTIMER = WM_USER + 56789;

  procedure TEnhancedTimer.InternalWndProc( var Msg : TMessage );
    begin
      with Msg do
        if Msg = WM_INTERNALTIMER
          then
            try
              Timer;
            except
              Application.HandleException( Self );
            end
          else Result := DefWindowProc( WndHandle, Msg, wParam, lParam );
    end;

  procedure TimerEvent( uTimerId, uMessage : UINT; dwUser, dw1, dw2 : DWORD ); stdcall;
    var
      Timer : TEnhancedTimer absolute dwUser;
    begin
      with Timer do
        if PostTicks
          then PostMessage( WndHandle, WM_INTERNALTIMER, 0, 0 )
          else SendMessage( WndHandle, WM_INTERNALTIMER, 0, 0 );
    end;

  procedure TEnhancedTimer.Update;
    var
      Msg : TMsg;
    begin
      if ( TimerId <> 0 ) and ( timeKillEvent( TimerId ) = TIMERR_NOERROR )
        then
          repeat
            // Empty message queue...
          until not PeekMessage( Msg, WndHandle, WM_INTERNALTIMER, WM_INTERNALTIMER, PM_REMOVE );
      if Enabled and Assigned( fOnTimer ) and ( fInterval <> 0 )
        then TimerId  := timeSetEvent( fInterval, fResolution, TimerEvent, Integer( Self ), TIME_PERIODIC )
        else TimerId := 0;
    end;

  procedure TEnhancedTimer.SetPostTicks( TicksPosted : boolean );
    begin
      fPostTicks := TicksPosted;
    end;

  procedure TEnhancedTimer.SetInterval( aInterval : integer );
    begin
      if fInterval <> aInterval
        then
          begin
            fInterval := Max( EnhTimerMinResolution, aInterval );
            Update;
          end;
    end;

  procedure TEnhancedTimer.SetResolution( aInterval : integer );
    begin
      if fResolution <> aInterval
        then
          begin
            if aInterval < 0
              then Resolution := 0;
            fResolution := aInterval;
            Update;
          end;
    end;

  // TTimerThread

  constructor TTimerThread.Create( aOwner : TTrustedEnhTimer );
    begin
      inherited Create( false );
      fOwner          := aOwner;
      FreeOnTerminate := false;
      Event           := CreateEvent( nil, false, false, 'TimerThreadEvent' );
    end;

  destructor TTimerThread.Destroy;
    begin
      Suspend;
      ResetTicking;
      ResetEvent( Event );
      CloseHandle( Event );
      inherited;
    end;

  procedure TrustedTimerEvent( uTimerId, uMessage : UINT; dwUser, dw1, dw2 : DWORD ); stdcall;
    var
      Timer : TTrustedEnhTimer absolute dwUser;
    begin
      with Timer do
        begin
          Inc( fLostEvents );
          SetEvent( fTimerThread.Event );
        end;
    end;

  procedure TTimerThread.InitTicking;
    begin
      if TimerId <> 0
        then ResetTicking;
      with fOwner do
        TimerId  := timeSetEvent( fInterval, fResolution, TrustedTimerEvent, integer( fOwner ), TIME_PERIODIC );
      if TimerId = 0
        then raise ETimerError.Create( 'Could not allocate timer event' );
    end;

  procedure TTimerThread.ResetTicking;
    begin
      if TimerId <> 0
        then
          begin
            timeKillEvent( TimerId );
            TimerId := 0;
          end;
    end;

  procedure TTimerThread.Execute;
    begin
      while not Terminated do
        begin
          if ( TimerId <> 0 ) and ( WaitForSingleObject( Event, INFINITE ) = WAIT_OBJECT_0 )
           then Synchronize( Timer );
        end;
    end;

  procedure TTimerThread.Timer;
    begin
      fOwner.Timer;
    end;

  // TTrustedEnhTimer

  constructor TTrustedEnhTimer.Create;
    begin
      inherited;
      fTimerThread := TTimerThread.Create( Self );
      fResolution  := TimerMinResolution;
      fInterval    := etInterval;
      fEnabled     := defEnabled;
      timeBeginPeriod( etInterval );
    end;

  destructor TTrustedEnhTimer.Destroy;
    begin
      Enabled := false;
      fTimerThread.Free;
      timeEndPeriod( etInterval );
      inherited;
    end;

  procedure TTrustedEnhTimer.Update;
    begin
      fTimerThread.ResetTicking;
      if Enabled and Assigned( fOnTimer ) and ( fInterval <> 0 )
        then fTimerThread.InitTicking;
    end;

  procedure TTrustedEnhTimer.Timer;
    begin
      Dec( fLostEvents );
      inherited;
      fLostEvents := 0;
    end;

  procedure TTrustedEnhTimer.SetInterval( aInterval : integer );
    begin
      if fInterval <> aInterval
        then
          begin
            fInterval := Max( EnhTimerMinResolution, aInterval );
            Update;
          end;
    end;

  procedure TTrustedEnhTimer.SetResolution( aInterval : integer );
    begin
      if fResolution <> aInterval
        then
          begin
            if aInterval < 0
              then Resolution := 0;
            fResolution := aInterval;
            Update;
          end;
    end;

  // Miscellaneous

  function TimerMinResolution : integer;
    begin
      Result := 55; //!!!!
    end;

  function TimerMaxResolution : integer;
    begin
      Result := MaxInt; //!!!!
    end;

  function EnhTimerMinResolution : integer;
    begin
      Result := tc.wPeriodMin;
    end;

  function EnhTimerMaxResolution : integer;
    begin
      Result := tc.wPeriodMax;
    end;

  function AdjustInterval( Interval, MinResolution, MaxResolution : integer ) : integer;
    begin
      Result := Max( Min( MaxResolution, NearestMult( Interval, MinResolution ) ), MinResolution );
    end;

  function TicksToTime( Ticks : longint ) : string;
    var
      Hundredths : integer;
      Seconds    : integer;
      Minutes    : integer;
      Hours      : integer;
    begin
      Hundredths := ( Ticks mod 1000 ) div 10;
      Ticks      := Ticks div 1000;
      Seconds    := Ticks mod 60;
      Ticks      := Ticks div 60;
      Minutes    := Ticks mod 60;
      Hours      := Ticks div 60;
      Result     := IntToStr( Hours ) + ':' + NumToStr( Minutes, 10, 2 ) + ':' + NumToStr( Seconds, 10, 2 ) + '.' + NumToStr( Hundredths, 10, 2 );
    end;

initialization
  if timeGetDevCaps( @tc, sizeof( TTimeCaps ) ) <> TIMERR_NOERROR
    then etInterval := 0
    else etInterval := Min( Max( tc.wPeriodMin, defInterval ), tc.wPeriodMax );

end.
