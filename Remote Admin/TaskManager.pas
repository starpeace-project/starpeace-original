unit TaskManager;

interface

  uses
    ComObj, RemoteAdmin_TLB, Classes;

  type
    TTaskManager =
      class(TAutoObject, ITaskManager)
        public
          destructor Destroy; override;
        protected
          function  Get_TaskCount : integer; safecall;
          procedure EnumTasks; safecall;
          procedure Set_TaskCount(Value : integer); safecall;
          function  GetTaskName(idx : integer) : OleVariant; safecall;
          procedure KillTask(id : integer); safecall;
          procedure StopTask(id : integer); safecall;
          function  Reboot : wordbool; safecall;
          function  GetTaskId(idx : integer): OleVariant; safecall;
          function  LaunchTask(const filename : widestring): wordbool; safecall;
        private
          fTasks : TStringList;
      end;

implementation

  uses
    ComServ, RemoteAdm, Windows, Logs;

  destructor TTaskManager.Destroy;
    begin
      fTasks.Free;
      inherited;
    end;

  function TTaskManager.Get_TaskCount : integer;
    begin
      if fTasks <> nil
        then result := fTasks.Count
        else result := 0;
    end;

  procedure TTaskManager.EnumTasks;
    begin
      if fTasks <> nil
        then fTasks.Free;
      fTasks := RemoteAdm.GetProcessList;
    end;

  procedure TTaskManager.Set_TaskCount(Value : integer);
    begin
    end;

  function TTaskManager.GetTaskName(idx : integer) : OleVariant;
    begin
      if (fTasks <> nil) and (idx < fTasks.Count)
        then result := fTasks[idx]
        else result := '';
    end;

  procedure TTaskManager.KillTask(id : integer);
    begin
      RemoteAdm.StopProgram( id, 10000 );
    end;

  procedure TTaskManager.StopTask(id : integer);
    begin
      RemoteAdm.StopProgram( id, 10000 );
    end;

  function TTaskManager.Reboot : wordbool;
    begin
      Result := RemoteAdm.Reboot;
    end;

  function TTaskManager.GetTaskId(idx : integer) : OleVariant;
    begin
      if (fTasks <> nil) and (idx < fTasks.Count)
        then result := integer(fTasks.Objects[idx])
        else result := 0;
    end;

  function TTaskManager.LaunchTask(const filename : widestring) : wordbool;
    var
      procid      : integer;
      programpath : string;

    function FilterPath(const path : string) : string;
      var
        p : integer;
      begin
        Result := path;
        p := pos('*', Result);
        while p > 0 do
          begin
            Result[p] := '+';
            p := pos('*', Result);
          end;
      end;

    begin
      programpath := FilterPath(filename);
      if RemoteAdm.StartProgram(programpath, procid) and (procid <> 0)
        then
          begin
            Log('TaskManager', programpath + ' succesfully run');
            Result := true;
          end
        else
          begin
            Log('TaskManager', 'Could not run ' + programpath);
            Result := false;
          end;
    end;

initialization
  TAutoObjectFactory.Create(ComServer, TTaskManager, Class_TaskManager, ciMultiInstance);
end.

