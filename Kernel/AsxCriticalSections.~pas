unit AsxCriticalSections;

interface

  uses
    Windows, SyncObjs;

  type
    TAsymetrixCriticalSection =
      class
        public
          constructor Create;
          destructor  Destroy; override;
        public
          function  BeginRead ( timeout : dword ) : TWaitResult;
          function  BeginWrite( timeout : dword ) : TWaitResult;
          procedure EndRead;
          procedure EndWrite;
        private
          fCanRead        : TEvent;
          fCanWrite       : TEvent;
          fInnerLocking   : TCriticalSection;
          fInnerUnlocking : TCriticalSection;
          fReadCount      : integer;
          fWriteCount     : integer;
        private
          procedure LockWrite;
          procedure LockRead;
          procedure UnlockWrite;
          procedure UnlockRead;
      end;

implementation

  uses
    SysUtils, Logs;

  // TAsymetrixCriticalSection

  constructor TAsymetrixCriticalSection.Create;
    begin
      inherited Create;
      fCanRead  := TEvent.Create( nil, true, true, '' );
      fCanWrite := TEvent.Create( nil, true, true, '' );
      fInnerLocking := TCriticalSection.Create;
      fInnerUnlocking := TCriticalSection.Create;
    end;

  destructor TAsymetrixCriticalSection.Destroy;
    begin
      try
        Logs.Log( 'Demolition', TimeToStr(Now) + ' - ' + ClassName );
      except
      end;
      fCanRead.Free;
      fCanWrite.Free;
      fInnerLocking.Free;
      fInnerUnlocking.Free;
      inherited;
    end;

  function TAsymetrixCriticalSection.BeginRead( timeout : dword ) : TWaitResult;
    begin
      fInnerLocking.Enter;
      result := fCanRead.WaitFor( timeout );
      LockWrite;
      fInnerLocking.Leave;
    end;

  function TAsymetrixCriticalSection.BeginWrite( timeout : dword ) : TWaitResult;
    begin
      fInnerLocking.Enter;
      result := fCanRead.WaitFor( timeout );
      if result = wrSignaled
        then result := fCanWrite.WaitFor( timeout );
      LockWrite;
      LockRead;
      fInnerLocking.Leave;
    end;

  procedure TAsymetrixCriticalSection.EndRead;
    begin
      fInnerUnlocking.Enter;
      UnlockWrite;
      fInnerUnlocking.Leave;
    end;

  procedure TAsymetrixCriticalSection.EndWrite;
    begin
      fInnerUnlocking.Enter;
      UnlockWrite;
      UnlockRead;
      fInnerUnlocking.Leave;
    end;

  procedure TAsymetrixCriticalSection.LockWrite;
    begin
      if fWriteCount = 0
        then fCanWrite.ResetEvent;
      inc( fWriteCount );
    end;

  procedure TAsymetrixCriticalSection.LockRead;
    begin
      if fReadCount = 0
        then fCanRead.ResetEvent;
      inc( fReadCount );
    end;

  procedure TAsymetrixCriticalSection.UnlockWrite;
    begin
      dec( fWriteCount );
      if fWriteCount = 0
        then fCanWrite.SetEvent;
    end;

  procedure TAsymetrixCriticalSection.UnlockRead;
    begin
      dec( fReadCount );
      if fReadCount = 0
        then fCanRead.SetEvent;
    end;
    

end.



