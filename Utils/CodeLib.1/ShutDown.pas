unit ShutDown;

interface

  type
    IShutDownTarget =
      interface
        function  GetPriority : integer;
        procedure OnSuspend;
        procedure OnResume;
        procedure OnShutDown;
      end;

  procedure AttachTarget(const which : IShutDownTarget);
  procedure DetachTarget(const which : IShutDownTarget);
  function  DoSuspend : integer;
  function  DoResume : integer;
  procedure DoShutDown;

implementation

  uses
    Classes;

  // Utils

  type
    TTargetList =
      class(TList)
        public
          destructor  Destroy;   override;
        public
          function Lock : integer;
          function Unlock : integer;
        protected
          function Add(const which : IShutDownTarget) : integer;
          function Remove(const which : IShutDownTarget) : integer;
        private
          fSorted : boolean;
          procedure Sort;
          procedure Suspend;
          procedure Resume;
          procedure Shutdown;
      end;

  var
    Targets : TTargetList = nil;

  function CompareTargets(Item1, Item2 : pointer) : integer;
    var
      Target1 : IShutDownTarget absolute Item1;
      Target2 : IShutDownTarget absolute Item2;
    begin
      Result := Target2.GetPriority - Target1.GetPriority;
    end;

  procedure FreeObject(var which);
    var
      aux : TObject;
    begin
      aux := TObject(which);
      TObject(which) := nil;
      aux.Free;
    end;

  // TTargetList

  destructor TTargetList.Destroy;
    begin
      ShutDown;
      inherited;
    end;

  function TTargetList.Lock : integer;
    begin
      Suspend;
      Result := 0;
    end;

  function TTargetList.Unlock : integer;
    begin
      Resume;
      Result := 0;
    end;

  function TTargetList.Add(const which : IShutDownTarget) : integer;
    begin
      which.OnSuspend;
      fSorted := (Count = 0);
      Result := inherited Add(pointer(which));
    end;

  function TTargetList.Remove(const which : IShutDownTarget) : integer;
    var
      idx : integer;
    begin
      idx := IndexOf(pointer(which));
      assert(idx <> -1);
      which.OnResume;
      Delete(idx);
      Result := idx;
    end;

  procedure TTargetList.Sort;
    begin
      if not fSorted
        then
          begin
            inherited Sort(CompareTargets);
            fSorted := true;
          end;
    end;

  procedure TTargetList.Suspend;
    var
      i : integer;
    begin
      Sort;
      for i := 0 to pred(Count) do
        try
          IShutDownTarget(List[i]).OnSuspend;
        except
        end;
    end;

  procedure TTargetList.Resume;
    var
      i : integer;
    begin
      Sort;
      for i := 0 to pred(Count) do
        try
          IShutDownTarget(List[i]).OnResume;
        except
        end;
    end;

  procedure TTargetList.Shutdown;
    var
      i : integer;
    begin
      Sort;
      for i := 0 to pred(Count) do
        try
          IShutDownTarget(List[i]).OnShutDown;
        except
        end;
      // <<>> finalize(IShutDownTarget(List[0]), Count);
      Clear;
    end;

  // Implementation

  procedure AttachTarget(const which : IShutDownTarget);
    begin
      if Targets = nil
        then Targets := TTargetList.Create;
      Targets.Add(which);
    end;

  procedure DetachTarget(const which : IShutDownTarget);
    begin
      if Targets <> nil  // post-shutdown state
        then
          begin
            Targets.Remove(which);
            if Targets.Count = 0
              then FreeObject(Targets);
          end;
    end;

  function DoSuspend : integer;
    begin
      if Targets <> nil
        then Result := Targets.Lock
        else Result := 0;
    end;

  function DoResume : integer;
    begin
      if Targets <> nil
        then Result := Targets.Unlock
        else Result := 0;
    end;

  procedure DoShutDown;
    begin
      FreeObject(Targets);
    end;

initialization
finalization
  DoShutDown;  // Only to be sure
end.
