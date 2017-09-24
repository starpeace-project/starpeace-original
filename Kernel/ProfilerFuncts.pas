unit ProfilerFuncts;

interface

  type
    TProfileKind = 0..30;
    TProfileId   = 0..30;

  procedure RequestProfile( Kind : TProfileKind; Id : TProfileId; Name : string );
  procedure ProcStarted( Kind : TProfileKind; Id : TProfileId );
  procedure ProcEnded( Kind : TProfileKind; Id : TProfileId );

  procedure Reset( Kind : TProfileKind );
  procedure LogResults( Kind : TProfileKind; LogName : string );

implementation

  uses
    Windows, Logs, SysUtils;

  type
    TProfileRec =
      record
        Name        : string;
        Active      : boolean;
        TotalTicks  : comp;
        MarkerStart : comp;
        TimeElapsed : comp;
      end;

  var
    Profiles    : array[TProfileKind, TProfileId] of TProfileRec;
    MarkerStart : array[TProfileKind] of comp;

  procedure RequestProfile( Kind : TProfileKind; Id : TProfileId; Name : string );
    begin
      Profiles[Kind, Id].Name        := Name;
      Profiles[Kind, Id].Active      := true;
      Profiles[Kind, Id].TotalTicks  := 0;
      Profiles[Kind, Id].MarkerStart := 0;
    end;

  procedure ProcStarted( Kind : TProfileKind; Id : TProfileId );
    begin
      QueryPerformanceCounter( TLargeInteger(Profiles[Kind, Id].MarkerStart) );
    end;

  procedure ProcEnded( Kind : TProfileKind; Id : TProfileId );
    var
      counter : comp;
      delta   : comp;
    begin
      QueryPerformanceCounter( TLargeInteger(counter) );
      delta := counter - Profiles[Kind, Id].MarkerStart;
      Profiles[Kind, Id].TotalTicks := Profiles[Kind, Id].TotalTicks + delta;
    end;

  procedure Reset( Kind : TProfileKind );
    var
      id : TProfileId;
    begin
      // reset profiles
      for id := low(id) to high(id) do
        Profiles[Kind, id].TotalTicks := 0;
      // start total counting
      QueryPerformanceCounter( TLargeInteger(MarkerStart[Kind]) );
    end;

  procedure LogResults( Kind : TProfileKind; LogName : string );
    var
      freq  : comp;

    function TicksToMSecStr( tics : comp ) : string;
      begin
        try
          if freq > 0
            then result := IntToStr(round(1000*tics/freq)) + ' msecs'
            else result := 'UNKNOWN FREQUENCY';
        except
          result := 'NUMERIC ERROR';
        end;
      end;

    function RatioToStr( num, dem : comp ) : string;
      begin
        if dem > 0
          then result := IntToStr(round((100*num)/dem)) + '%'
          else result := '0%';
      end;

    var
      id      : TProfileId;
      absTime : comp;
      relTime : comp;
    begin
      // stop total counting
      QueryPerformanceCounter( TLargeInteger(absTime) );
      absTime := absTime - MarkerStart[Kind];
      // compute total
      relTime := 0;
      for id := low(id) to high(id) do
        if Profiles[Kind, id].Active
          then relTime := relTime + Profiles[Kind, id].TotalTicks;
      // produce report
      if (relTime > 0) and (absTime > 0)
        then
          begin
            QueryPerformanceFrequency( TLargeInteger(freq) );
            Logs.Log( LogName, '-------------------------------------' );
            Logs.Log( LogName, 'Abs Time sampled: ' + TicksToMSecStr(absTime) );
            Logs.Log( LogName, 'Rel Time sampled: ' + TicksToMSecStr(relTime) );
            for id := low(id) to high(id) do
              if Profiles[Kind, id].Active
                then
                  begin
                    Logs.Log( LogName, 'Profile: ' + Profiles[Kind, id].Name );
                    Logs.Log( LogName, #9'Time running:   ' + TicksToMSecStr(Profiles[Kind, id].TotalTicks) );
                    Logs.Log( LogName, #9'Relative usage: ' + RatioToStr(Profiles[Kind, id].TotalTicks, relTime) );
                    Logs.Log( LogName, #9'Absolute usage: ' + RatioToStr(Profiles[Kind, id].TotalTicks, absTime) );
                  end;
            Logs.Log( LogName, 'Other tasks: ' );
            Logs.Log( LogName, #9'Time running: ' + TicksToMSecStr(absTime - relTime) );
            Logs.Log( LogName, #9'usage:        ' + RatioToStr(absTime - relTime, absTime) );
          end;
    end;


end.

