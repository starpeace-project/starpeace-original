unit Logs;

interface

  uses
  {$IFDEF USELogs}
    ShareMem,
    {$endif}
    SocketComp, SysUtils;

  procedure Log( LogId, Info : string );
  procedure InitSocket(id : string; aSocket : TClientSocket);
  procedure LogMemReport(LogId : string);

  var
    LogProxy : OleVariant;

implementation

  uses
    Variants;

  {$IFDEF USELogs}

  procedure Log( LogId, Info : string ); external 'SPLogs.dll';
  procedure InitSocket(id : string; aSocket : TClientSocket); external 'SPLogs.dll';

  procedure LogMemReport;
    begin
      {$IFDEF REPORTMEM}
      Log(LogId, Format('AllocMem: %d, AllocSize: %d', [ShareMem.GetAllocMemCount, ShareMem.GetAllocMemSize]));
      {$ENDIF}
    end;

  {$ELSE}

  procedure Log( LogId, Info : string );
    begin
    end;

  procedure InitSocket(id : string; aSocket : TClientSocket);
    begin
    end;

  procedure LogMemReport;
    begin
    end;

  {$ENDIF}

initialization

  LogProxy := Unassigned;
  
end.

