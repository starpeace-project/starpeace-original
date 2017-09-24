unit RDOLogAgents;

interface

  uses
    RDOInterfaces;

  type
    TLogAgentRec =
      record
        Id    : string;
        Agent : IRDOLogAgent;
      end;

    PLogAgentArray = ^TLogAgentArray;
    TLogAgentArray = array[0..0] of TLogAgentRec;

  type
    TRDOLogAgentRegistry =
      class(TInterfacedObject, IRDOLogAgentRegistry)
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fAgents : PLogAgentArray;
          fCount  : integer;
        private
          function  GetLogAgent(aClassName : string) : IRDOLogAgent;
          procedure RegisterAgent(aClassName : string; anAgent : IRDOLogAgent);
      end;

implementation

  // TRDOLogAgentRegistry

  constructor TRDOLogAgentRegistry.Create;
    begin
      inherited;
    end;

  destructor TRDOLogAgentRegistry.Destroy;
    begin
      if fAgents <> nil
        then
          begin
            Finalize(fAgents[0], fCount);
            ReallocMem(fAgents, 0);
          end;
      inherited;
    end;

  function TRDOLogAgentRegistry.GetLogAgent(aClassName : string) : IRDOLogAgent;
    var
      i : integer;
    begin
      i := 0;
      while (i < fCount) and (fAgents[i].Id <> aClassName) do
        inc(i);
      if i < fCount
        then result := fAgents[i].Agent
        else result := nil;
    end;

  procedure TRDOLogAgentRegistry.RegisterAgent(aClassName : string; anAgent : IRDOLogAgent);
    begin
      inc(fCount);
      ReallocMem(fAgents, fCount*sizeof(fAgents[0]));
      with fAgents[fCount - 1] do
        begin
          Id    := aClassName;
          Agent := anAgent;
        end;
    end;


end.
