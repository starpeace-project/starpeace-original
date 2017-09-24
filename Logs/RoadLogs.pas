unit RoadLogs;

interface

  procedure CreateCircuitSeg(CircuitId, TycoonId, x1, y1, x2, y2: integer);
  procedure BreakCircuitAt  (CircuitId, TycoonId, x, y : integer);
  procedure WipeCircuit     (CircuitId, TycoonId, x1, y1, x2, y2 : integer);

implementation

  uses
    SysUtils, Logs;

  procedure CreateCircuitSeg(CircuitId, TycoonId, x1, y1, x2, y2 : integer);
    begin
      logs.log('circuits', Format('NEW %d %d %d %d %d %d', [CircuitId, TycoonId, x1, y1, x2, y2]) + #9 + DateTimeToStr(Now));
    end;

  procedure BreakCircuitAt(CircuitId, TycoonId, x, y : integer);
    begin
      logs.log('circuits', Format('BREAK %d %d %d %d', [CircuitId, TycoonId, x, y]) + #9 + DateTimeToStr(Now));
    end;

  procedure WipeCircuit(CircuitId, TycoonId, x1, y1, x2, y2 : integer);
    begin
      logs.log('circuits', Format('WIPE %d %d %d %d %d %d', [CircuitId, TycoonId, x1, y1, x2, y2]) + #9 + DateTimeToStr(Now));
    end;

end.
