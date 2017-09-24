unit RDOLogAgents;

interface

  uses
    Classes, RDOInterfaces, Kernel, World;

  type
    TLogAgentRec =
      record
        ClassName : string;
        Agent     : ILogAgent;
      end;

    PLogAgentArray = ^TLogAgentArray;
    TLogAgentArray = array[0..0] of TLogAgentRec;

    TLogAgents =
      class(TInterfacedObject, ILogAgents)
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fAgents  : PLogAgentArray;
          fMethods : TStringList;
          fCount   : integer;
        private
          function  LogableMethod(aMethod : string) : boolean;
          function  GetLogAgentById(Id : string) : ILogAgent;
          function  GetLogAgentByClass(aClass : TClass) : ILogAgent;
          procedure RegisterMethods(Names : array of string);
          procedure RegisterAgent(aClassName : string; anAgent : ILogAgent);
      end;

    TLogAgent =
      class(TInterfacedObject, ILogAgent)
        public
          constructor Create(anId : string; aWorld : TWorld; aLogEvent : TLogQueryEvent);
        private
          fId    : string;
          fWorld : TWorld;
          fOnLog : TLogQueryEvent;
        private
          function  GetId : string;
          function  GetLogId(Obj : TObject) : string; virtual; abstract;
          function  GetObject(ObjId : string) : TObject; virtual; abstract;
          procedure LogQuery(Query : string);
      end;

   // TWorldLogAgent
   // TFacilityLogAgent
   // TBlockLogAgent
   // TInputLogAgent
   // TOutputLogAgent
   // .. ??

implementation

  // TLogAgents

  constructor TLogAgents.Create;
    begin
      inherited;
      fMethods := TStringList.Create;
      fMethods.Sorted := true;
      fMethods.Duplicates := dupIgnore;
    end;

  destructor TLogAgents.Destroy;
    begin
      if fAgents <> nil
        then
          begin
            Finalize(fAgents[0], fCount);
            ReallocMem(fAgents, 0);
          end;
      fMethods.Free;
      inherited;
    end;

  function TLogAgents.LogableMethod(aMethod : string) : boolean;
    begin
      result := fMethods.IndexOf(aMethod) <> -1;
    end;

  function TLogAgents.GetLogAgentById(Id : string) : ILogAgent;
    var
      i : integer;
    begin
      i := 0;
      while (i < fCount) and (fAgents[i].Agent.GetId <> Id) do
        inc(i);
      if i < fCount
        then result := fAgents[i].Agent
        else result := nil;
    end;

  function TLogAgents.GetLogAgentByClass(aClass : TClass) : ILogAgent;

    function FindAgent(name : string; Agent : ILogAgent) : boolean;
      var
        i : integer;
      begin
        i := 0;
        while (i < fCount) and (fAgents[i].ClassName <> name) do
          inc(i);
        result := i < fCount;
        if result
          then Agent := fAgents[i].Agent
          else Agent := nil;
      end;

    var
      Cls : TClass;
    begin
      if not FindAgent(aClass.ClassName, result)
        then
          begin
            Cls := aClass.ClassParent;
            while (Cls <> nil) and FindAgent(Cls.ClassName, result) do
              Cls := aClass.ClassParent;
            RegisterAgent(aClass.ClassName, result);
          end;
    end;

  procedure TLogAgents.RegisterMethods(Names : array of string);
    var
      i : integer;
    begin
      for i := low(Names) to high(Names) do
        fMethods.Add(Names[i]);
    end;

  procedure TLogAgents.RegisterAgent(aClassName : string; anAgent : ILogAgent);
    begin
      inc(fCount);
      ReallocMem(fAgents, fCount*sizeof(fAgents[0]));
      with fAgents[fCount - 1] do
        begin
          ClassName := aClassName;
          Agent     := anAgent;
        end;
    end;


  // TLogAgent

  constructor TLogAgent.Create(anId : string; aWorld : TWorld; aLogEvent : TLogQueryEvent);
    begin
      inherited Create;
      fId    := anId;
      fWorld := aworld;
      fOnLog := aLogEvent;
    end;

  function TLogAgent.GetId : string;
    begin
      result := fId;
    end;

  procedure TLogAgent.LogQuery(Query : string);
    begin
      if Assigned(fOnLog)
        then fOnlog(Query);
    end;

end.
