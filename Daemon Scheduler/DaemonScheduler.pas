unit DaemonScheduler;

interface

  uses
    Windows, Classes, ExtCtrls, SyncObjs, RDOInterfaces, WinSockRDOConnection, RDOObjectProxy,
    DirServerSession, Daemons;

  type
    TDaemonScheduler =
      class
        public
          constructor Create(MaxDaemonThreads : integer);
          destructor  Destroy; override;
        public
          function  Start(const DSAddr : string; DSPort : integer) : boolean;
          procedure Stop;
          procedure Schedule(const Daemon : IDaemon);
        private
          fDaemons : TList;
          procedure LoadDaemons(const DSAddr : string; DSPort : integer);
          procedure FreeDaemons;
        private
          function GetDaemonCount : integer;
          function GetDaemon(i : integer) : IDaemon;
        public
          property DaemonCount          : integer read GetDaemonCount;
          property Daemons[i : integer] : IDaemon read GetDaemon;
        private
          fTimer            : TTimer;
          fReadyDaemons     : TList;
          fReadyDaemonsLock : TCriticalSection;
          fDaemonThreads    : TList;
          fDaemonReadyEvent : THandle;
          fDoneEvent        : THandle;
        private
          procedure SchedulerTimer(Sender : TObject);
          procedure OnIdle(Sender: TObject; var Done: Boolean);
      end;

  const
    cLogId = 'Daemon Scheduler';
    
implementation

  uses
    SysUtils, Logs, Clipbrd, Forms;

  type
    TDaemonThread =
      class(TThread)
        private
          constructor Create(Scheduler : TDaemonScheduler);
        private
          fScheduler : TDaemonScheduler;
        protected
          procedure Execute; override;
      end;

  // TDaemonThread

  constructor TDaemonThread.Create(Scheduler : TDaemonScheduler);
    begin
      inherited Create(true);
      fScheduler := Scheduler;
      Resume;
    end;

  procedure TDaemonThread.Execute;
    var
      DaemonThreadEvents : array [0..1] of THandle;
      Daemon             : IDaemon;
    begin
      with fScheduler do
        begin
          DaemonThreadEvents[0] := fDaemonReadyEvent;
          DaemonThreadEvents[1] := fDoneEvent;
          while not Terminated do
            begin
              fReadyDaemonsLock.Enter;
              try
                if fReadyDaemons.Count > 0
                  then
                    begin
                      Daemon := IDaemon(fReadyDaemons[0]);
                      fReadyDaemons.Delete(0);
                    end
                  else
                    begin
                      Daemon := nil;
                      ResetEvent(fDaemonReadyEvent);
                    end;
              finally
                fReadyDaemonsLock.Leave;
              end;
              if (Daemon <> nil) and not Daemon.IsRunning
                then Daemon.Run;
              WaitForMultipleObjects(2, @DaemonThreadEvents[0], false, INFINITE);
            end;
        end;
    end;

  // TDaemonScheduler

  constructor TDaemonScheduler.Create(MaxDaemonThreads : integer);
    var
      i : integer;
    begin
      inherited Create;
      fDaemons := TList.Create;
      fTimer := TTimer.Create(nil);
      fTimer.Enabled := false;
      fTimer.Interval := 10;
      fTimer.OnTimer := SchedulerTimer;
      Application.OnIdle := OnIdle;
      fReadyDaemons := TList.Create;
      fReadyDaemonsLock := TCriticalSection.Create;
      fDaemonThreads := TList.Create;
      fDaemonReadyEvent := CreateEvent(nil, true, false, nil);
      fDoneEvent := CreateEvent(nil, true, false, nil);
      for i := 0 to pred(MaxDaemonThreads) do
        fDaemonThreads.Add(TDaemonThread.Create(Self));
    end;

  destructor TDaemonScheduler.Destroy;
    var
      i : integer;
    begin
      Stop;
      SetEvent(fDoneEvent);
      for i := 0 to pred(fDaemonThreads.Count) do
        TDaemonThread(fDaemonThreads[i]).Free;
      CloseHandle(fDoneEvent);
      CloseHandle(fDaemonReadyEvent);
      fDaemonThreads.Free;
      fReadyDaemonsLock.Free;
      fReadyDaemons.Free; 
      fTimer.Free;
      FreeDaemons;
      fDaemons.Free;
      inherited;
    end;

  function TDaemonScheduler.Start(const DSAddr : string; DSPort : integer) : boolean;
    begin
      try
        if fDaemons.Count = 0
          then LoadDaemons(DSAddr, DSPort);
        if fDaemons.Count = 0
          then Log(cLogId, 'No daemons loaded');
        fTimer.Enabled := true;
        Result := true;
      except
        Result := false;
      end;
    end;

  procedure TDaemonScheduler.Stop;
    begin
      fTimer.Enabled := false;
    end;

  procedure TDaemonScheduler.Schedule(const Daemon : IDaemon);
    begin
      if not Daemon.IsRunning
        then
          begin
            Log(cLogId, 'Scheduling daemon "' + Daemon.GetName + '"');
            fReadyDaemonsLock.Enter;
            try
              fReadyDaemons.Add(pointer(Daemon));
              if fReadyDaemons.Count > 0//= 1
                then SetEvent(fDaemonReadyEvent);
            finally
              fReadyDaemonsLock.Leave;
            end;
          end;
    end;

  procedure TDaemonScheduler.LoadDaemons(const DSAddr : string; DSPort : integer);
    var
      info          : TSearchRec;
      ok            : boolean;
      dllhandle     : THandle;
      daemoncreator : TDaemonCreatorRoutine;
      Daemon        : IDaemon;
    begin
      ok := FindFirst('*.dll', faAnyFile, info) = 0;
      try
        while ok do
          begin
            dllhandle := LoadLibrary(pchar(info.Name));
            if dllhandle <> 0
              then
                begin
                  daemoncreator := GetProcAddress(dllhandle, cDaemonCreatorRoutineName);
                  if @daemoncreator <> nil
                    then
                      begin
                        Daemon := daemoncreator(DSAddr, DSPort);
                        Daemon._AddRef;
                        fDaemons.Add(pointer(Daemon));
                        Log(cLogId, Daemon.GetName + ' loaded');
                      end;
                end;
            ok := FindNext(info) = 0;
          end;
      finally
        FindClose(info);
      end;
    end;

  procedure TDaemonScheduler.FreeDaemons;
    var
      i : integer;
    begin
      for i := 0 to pred(fDaemons.Count) do
        begin
          IDaemon(fDaemons[i])._Release;
          fDaemons[i] := nil
        end;
      fDaemons.Pack;
    end;

  function TDaemonScheduler.GetDaemonCount;
    begin
      Result := fDaemons.Count;
    end;

  function TDaemonScheduler.GetDaemon(i : integer) : IDaemon;
    begin
      Result := IDaemon(fDaemons[i]);
    end;

  procedure TDaemonScheduler.SchedulerTimer(Sender : TObject);
    var
      i            : integer;
      elapsedticks : integer;
      Daemon       : IDaemon;
    begin
      if fDaemons <> nil
        then
          for i := 0 to pred(fDaemons.Count) do
            begin
              Daemon := IDaemon(fDaemons[i]);
              elapsedticks := longint(GetTickCount) - Daemon.LastRun;
              if elapsedticks >= Daemon.GetPeriod
                then Schedule(Daemon);
            end;
    end;

  procedure TDaemonScheduler.OnIdle(Sender: TObject; var Done: Boolean);
    var
      i      : integer;
      Daemon : IDaemon;
    begin
      for i := 0 to pred(fDaemons.Count) do
        begin
          Daemon := IDaemon(fDaemons[i]);
          if not Daemon.IsRunning
            then Daemon.SynchronizedRun;
        end;
      Done := true;
    end;

end.
