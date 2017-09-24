unit Profiler;

{ $DEFINE UseProfiler}

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

  {$IFDEF UseProfiler}                              

  procedure RequestProfile( Kind : TProfileKind; Id : TProfileId; Name : string ); external 'FIVEProfiler.dll';
  procedure ProcStarted( Kind : TProfileKind; Id : TProfileId ); external 'FIVEProfiler.dll';
  procedure ProcEnded( Kind : TProfileKind; Id : TProfileId ); external 'FIVEProfiler.dll';

  procedure Reset( Kind : TProfileKind ); external 'FIVEProfiler.dll';
  procedure LogResults( Kind : TProfileKind; LogName : string ); external 'FIVEProfiler.dll';

  {$ELSE}

  procedure RequestProfile( Kind : TProfileKind; Id : TProfileId; Name : string );
    begin
    end;

  procedure ProcStarted( Kind : TProfileKind; Id : TProfileId );
    begin
    end;

  procedure ProcEnded( Kind : TProfileKind; Id : TProfileId );
    begin
    end;

  procedure Reset( Kind : TProfileKind ); 
    begin
    end;

  procedure LogResults( Kind : TProfileKind; LogName : string );
    begin
    end;

  {$ENDIF}

end.
