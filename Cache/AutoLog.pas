unit AutoLog;

interface

  procedure Log( LogId, Info : string );
  procedure InitLogs;

implementation

  uses
    SyncObjs, SysUtils;

  var
    LogLock : TCriticalSection = nil;

  procedure InitLogs;
    begin
      LogLock := TCriticalSection.Create;
    end;

  procedure DoneLogs;
    begin
      LogLock.Free;
    end;

  procedure Log( LogId, Info : string );
    var
      filename : string;
      LogFile  : Text;
    begin
      LogLock.Acquire;
      try
        filename := 'c:\' + LogId + '.log';
        AssignFile( LogFile, filename );
        try
          Append( LogFile );
        except
          Rewrite( LogFile );
        end;
        try
          writeln( LogFile, Info );
        finally
          CloseFile( LogFile );
        end;
      finally
        LogLock.Release
      end;
    end;

initialization

  //InitLogs;

finalization

  //DoneLogs;

end.

