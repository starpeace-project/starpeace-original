unit SimpleDaemon;

interface

  uses
    DirServerSession, Daemons;

  function CreateDaemon(const Session : IDirServerSession) : IDaemon;

implementation

  uses
    Windows, SyncObjs;

  type
    TSimpleDaemon =
      class(TInterfacedObject, IDaemon)
        public
          constructor Create(const Session : IDirServerSession);
          destructor  Destroy; override;
        private // IDaemon
          procedure SetSession(const Session : IDirServerSession);
          function  GetName : string;
          function  GetDescription : string;
          function  IsRunning : boolean;
          procedure Run;
          function  GetPeriod  : integer;
          procedure SetPeriod(period : integer);
          function  LastRun : integer;
          function  ShowPropertiesUI : boolean;
        private
          fSession : IDirServerSession;
          fLock    : TCriticalSection;
          fPeriod  : integer;
          fRunning : boolean;
          fLastRun : integer;
      end;

  function CreateDaemon(const Session : IDirServerSession) : IDaemon;
    begin
      Result := TSimpleDaemon.Create(Session);
    end;

  constructor TSimpleDaemon.Create(const Session : IDirServerSession);
    begin
      inherited Create;
      fSession := Session;
      fLock := TCriticalSection.Create;
      fPeriod := 5000;
    end;

  destructor TSimpleDaemon.Destroy;
    begin
      fLock.Free;
      inherited;
    end;

  procedure TSimpleDaemon.SetSession(const Session : IDirServerSession);
    begin
      fSession := Session;
    end;

  function TSimpleDaemon.GetName : string;
    begin
      Result := 'Simple daemon';
    end;

  function TSimpleDaemon.GetDescription : string;
    begin
      Result := 'This is a test daemon';
    end;

  function TSimpleDaemon.IsRunning : boolean;
    begin
      fLock.Enter;
      try
        Result := fRunning;
      finally
        fLock.Leave;
      end;
    end;

  procedure TSimpleDaemon.Run;
    begin
      fLock.Enter;
      try
        fRunning := true;
        fLastRun := GetTickCount;
      finally
        fLock.Leave;
      end;
      MessageBeep(0);
      fLock.Enter;
      try
        fRunning := false;
      finally
        fLock.Leave;
      end;
    end;

  function TSimpleDaemon.GetPeriod : integer;
    begin
      Result := fPeriod;
    end;

  procedure TSimpleDaemon.SetPeriod(period : integer);
    begin
      fPeriod := period;
    end;

  function TSimpleDaemon.LastRun : integer;
    begin
      fLock.Enter;
      try
        Result := fLastRun;
      finally
        fLock.Leave;
      end;
    end;

  function TSimpleDaemon.ShowPropertiesUI : boolean;
    begin
      Result := false;
    end;

end.
