unit LogFile;

interface

  procedure SetLogFile( LogFileName : string );
  procedure LogThis( MsgToLog : string );

implementation

  {$IFDEF LogsEnabled}
  uses
    SyncObjs;

  var
    LgFile      : TextFile;
    Initialized : boolean = false;
    Lock        : TCriticalSection;
  {$ENDIF}

  procedure SetLogFile( LogFileName : string );
    begin
      {$IFDEF LogsEnabled}
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
      {$IFDEF LogsEnabled}
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

{$IFDEF LogsEnabled}
initialization
  Lock := TCriticalSection.Create
finalization
  Lock.Free
{$ENDIF}
end.
