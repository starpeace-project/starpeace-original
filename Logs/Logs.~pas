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

{$IFDEF VER140}           { Delphi 6.0 } //.rag
  uses
    Variants;
{$ENDIF}

  {$IFDEF USELogs}

  procedure Log( LogId, Info : string ); external 'FIVELogs.dll';
  procedure InitSocket(id : string; aSocket : TClientSocket); external 'FIVELogs.dll';

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

