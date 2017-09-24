unit LogFile;

interface

  procedure SetLogFile( LogFileName : string );
  procedure LogThis( MsgToLog : string );

implementation

  {$IFDEF Logs}
  uses
    SyncObjs;

  var
    LgFile      : TextFile;
    Initialized : boolean = false;
    Lock        : TCriticalSection;
  {$ENDIF}

  procedure SetLogFile( LogFileName : string );
    begin
      {$IFDEF Logs}
      Lock.Acquire;
      AssignFile( LgFile, LogFileName );
      try
        ReWrite( LgFile );
        CloseFile( LgFile );
        Initialized := true
      except
        Initialized := false
      end;
      Lock.Release
      {$ENDIF}
    end;

  procedure LogThis( MsgToLog : string );
    begin
      {$IFDEF Logs}
      Lock.Acquire;
      if Initialized
        then
          begin
            try
              Append( LgFile );
              try
                WriteLn( LgFile, MsgToLog )
              finally
                CloseFile( LgFile )
              end
            except
              Initialized := false
            end
          end;
      Lock.Release
      {$ENDIF}
    end;

{$IFDEF Logs}
initialization
  Lock := TCriticalSection.Create
finalization
  Lock.Free
{$ENDIF}
end.
