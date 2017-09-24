unit SmartThreads;

interface

  uses
    Windows, Classes, SysUtils;

  type
    TSmartThread =
      class( TThread )
        public
          constructor Create( CreateSuspended : boolean );
          destructor  Destroy; override;
        public
          procedure CallSync( ExtMethod : TThreadMethod );
      end;

  procedure CallSync( ExtMethod : TThreadMethod );

  type
    ESmartSyncFault = class( Exception );

  var
    MainThreadId : DWORD;

  procedure InitThreadRegistry;
  procedure DoneThreadRegistry;

{$IFNDEF LIBRARY}
  function FindThread( ThreadId : dword ) : TThread;
{$ENDIF}

implementation

{$IFDEF LIBRARY}

  uses
    SmartThreadRegistry;

  procedure InitThreadRegistry;
    begin
      SmartThreadRegistry.InitThreadRegistry;
    end;

  procedure DoneThreadRegistry;
    begin
      SmartThreadRegistry.DoneThreadRegistry;
    end;

{$ELSE}

  procedure RegisterThread( Thread : TThread ); external 'ThreadRegistry.dll';
  procedure UnregisterThread( Thread : TThread ); external 'ThreadRegistry.dll';
  function  FindThread( ThreadId : dword ) : TThread; external 'ThreadRegistry.dll';
  procedure InitThreadRegistry; external 'ThreadRegistry.dll';
  procedure DoneThreadRegistry; external 'ThreadRegistry.dll';

{$ENDIF}

  // TSmartThread

  constructor TSmartThread.Create( CreateSuspended : boolean );
    begin
      inherited Create( true );
      RegisterThread( self );
      if not CreateSuspended
        then Resume;
    end;

  destructor TSmartThread.Destroy;
    begin
      UnregisterThread( self );
      inherited;
    end;

  procedure TSmartThread.CallSync( ExtMethod : TThreadMethod );
    begin
      Synchronize( ExtMethod );
    end;

  // CallSync

  procedure CallSync( ExtMethod : TThreadMethod );
    var
      CurrentThreadId : DWORD;
      CurrentThread   : TThread;
    begin
      CurrentThreadId := GetCurrentThreadId;
      if CurrentThreadId <> MainThreadId
        then
          begin
            CurrentThread := FindThread( CurrentThreadId );
            if (CurrentThread <> nil) and (CurrentThread is TSmartThread)
              then TSmartThread(CurrentThread).CallSync( ExtMethod )
              else raise ESmartSyncFault.Create( 'Attemp to synchronize in a non smart thread' )
          end
        else ExtMethod;
    end;

initialization

  MainThreadId := GetCurrentThreadId;

end.
