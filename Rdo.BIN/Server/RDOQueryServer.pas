unit RDOQueryServer;

interface

  uses
    RDOInterfaces, RDOObjectServer, RDOQueries;

  const
    tidConnRequestName = 'RDOCnntId';

  type
    TRDOQueryServer =
      class(TInterfacedObject, IRDOQueryServer, IRDOLog)
        private
          fObjServer : TRDOObjectServer;
          fLogAgents : ILogAgents;
          fBusy      : boolean;
        private
          function  GetCommand(Query : TRDOQuery; var ConnId : integer; var QueryStatus : integer) : TRDOQuery;
          function  SetCommand(Query : TRDOQuery; var QueryStatus : integer) : TRDOQuery;
          function  CallCommand(Query : TRDOQuery; var QueryStatus : integer) : TRDOQuery;
          function  IdOfCommand(Query : TRDOQuery) : TRDOQuery;
        public
          constructor Create(ObjectServer : TRDOObjectServer);
          function    ExecQuery(Query : TRDOQuery; ConnId : integer; var QueryStatus : integer) : TRDOQuery;
          procedure   RegisterAgents(Agents : ILogAgents);
          procedure   ExecLogQuery(Query : string);
          function    GetBusy : boolean;
          function    GetStatus : integer;
          procedure   SetBusy(value : boolean);
        public
          property    Status : integer read GetStatus;
        private
          //procedure   LogMethod(Obj : TObject; Query : string);
      end;

implementation

  uses
    Windows, SysUtils, RDOProtocol, RDOUtils, ErrorCodes, CompStringsParser;

  type
    TRDOCommand = (cmSel, cmGet, cmSet, cmCall, cmIdOf, cmUnkCmd);

  const
    RDOCommandNames : array [TRDOCommand] of string =
      (
        SelObjCmd, GetPropCmd, SetPropCmd, CallMethCmd, IdOfCmd, ''
      );

  function MapIdentToCommand(Ident : string) : TRDOCommand;
    var
      CurCmd       : TRDOCommand;
      LowCaseIdent : string;
    begin
      CurCmd := Low(CurCmd);
      LowCaseIdent := LowerCase(Ident);
      while (CurCmd <> High(CurCmd)) and (RDOCommandNames[CurCmd] <> LowCaseIdent) do
        inc(CurCmd);
      Result := CurCmd
    end;

  // TRDOQueryServer

  constructor TRDOQueryServer.Create(ObjectServer : TRDOObjectServer);
    begin
      inherited Create;
      fObjServer := ObjectServer
    end;

  function TRDOQueryServer.ExecQuery(Query : TRDOQuery; ConnId : integer; var QueryStatus : integer) : TRDOQuery;
    begin
      QueryStatus := 1; {1} // starting
      {
      ThreadPrio := Query.Priority;
      if ThreadPrio <> THREAD_PRIORITY_NORMAL
        then SetThreadPriority(GetCurrentThread, ThreadPrio)
        else SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_NORMAL);
      }
      QueryStatus := 2; {2}
      case Query.QKind of
        qkGeID :
          begin
            Result := IdOfCommand(Query)
          end;
        qkGetProp :
          begin
            QueryStatus := 3; {3}
            result := GetCommand(Query, ConnId, QueryStatus);
          end;
        qkSetProp :
          begin
            QueryStatus := 4; {4}
            result := SetCommand(Query, QueryStatus);
          end;
        qkCallRet, qkCallNoRet :
          begin
            QueryStatus := 5; {5}
            result := CallCommand(Query, QueryStatus);
          end;
        else result := nil;
      end;
      QueryStatus := 6; {6}
    end;

  procedure TRDOQueryServer.RegisterAgents(Agents : ILogAgents);
    begin
      fLogAgents := Agents;
    end;

  procedure TRDOQueryServer.ExecLogQuery(Query : string);
  {
    var
      len     : integer;
      p       : integer;
      AgentId : string;
      ObjId   : string;
      Agent   : ILogAgent;
      Obj     : TObject;
      aux     : integer;
    }
    begin
    {
      try
        if fLogAgents <> nil
          then
            begin
              len := length(Query);
              p   := 1;
              SkipSpaces(Query, p);
              AgentId := GetNextStringUpTo(Query, p, '#');
              inc(p);
              ObjId   := GetNextStringUpTo(Query, p, '#');
              inc(p);
              Agent   := fLogAgents.GetLogAgentById(AgentId);
              if Agent <> nil
                then
                  begin
                    Obj := Agent.GetObject(ObjId);
                    //if Obj <> nil
                      //then ExecQuery('0 sel ' + IntToStr(integer(Obj)) + copy(Query, p, len), 0, aux);
                  end;
            end;
      except
      end;
    }
    end;

  function TRDOQueryServer.GetBusy : boolean;
    begin
      result := fBusy;
    end;

  function TRDOQueryServer.GetStatus : integer;
    begin
      result := 0;
    end;

  procedure TRDOQueryServer.SetBusy(value : boolean);
    begin
      fBusy := value;
    end;

  {
  procedure TRDOQueryServer.LogMethod(Obj : TObject; Query : string);
    var
      aux   : string;
      Agent : ILogAgent;
    begin
      try
        Agent := fLogAgents.GetLogAgentByClass(Obj.ClassType);
        if Agent <> nil
          then
            begin
              aux := Agent.GetId + '#' + Agent.GetLogId(Obj) + '# ' + Query;
              Agent.LogQuery(aux);
            end;
      except
      end;
    end;
  }

  function TRDOQueryServer.GetCommand(Query : TRDOQuery; var ConnId : integer; var QueryStatus : integer)  : TRDOQuery;
    var
      PropValue : variant;
      ErrorCode : integer;
    begin
      QueryStatus := 31;
      if Query.Name = tidConnRequestName
        then PropValue := ConnId
        else PropValue := fObjServer.GetProperty(Query.ObjId, Query.Name, ErrorCode, QueryStatus);
      if ErrorCode = errNoError
        then
          begin
            result := TRDOQuery.Create(qkAnswer, Query.Id);
            result.PushParam(PropValue);
          end
        else
          begin
            result := TRDOQuery.Create(qkError, Query.Id);
            result.PushParam(ErrorCode);
          end;
      QueryStatus := 32;
    end;

  function TRDOQueryServer.SetCommand(Query : TRDOQuery; var QueryStatus : integer) : TRDOQuery;
    var
      PropValue : variant;
      ErrorCode : integer;
      idx       : integer;
    begin
      QueryStatus := 41;
      {
      if (fLogAgents <> nil) and fLogAgents.LogableMethod(PropName)
        then LogMethod(TObject(ObjectId), 'set ' + copy(QueryText, SavePos, QueryLen - SavePos + 1));
      }
      if Query.ParamCount > 0
        then
          begin
            idx := 0;
            PropValue := Query.PopParam(idx);
            try
              fObjServer.SetProperty(Query.ObjId, Query.Name, PropValue, ErrorCode, QueryStatus);
            except
              ErrorCode := errUnknownError
            end;
          end
        else ErrorCode := errIllegalPropValue;
      if ErrorCode = errNoError
        then
          begin
            result := TRDOQuery.Create(qkAnswer, Query.Id);
            result.PushParam(ErrorCode);
          end
        else
          begin
            result := TRDOQuery.Create(qkError, Query.Id);
            result.PushParam(ErrorCode);
          end;
      QueryStatus := 42;
    end;

  function TRDOQueryServer.CallCommand(Query : TRDOQuery; var QueryStatus : integer) : TRDOQuery;
    var
      MethodName : string;
      ErrorCode  : integer;
      ParamIdx   : integer;
      Params     : variant;
      Res        : variant;
      Tmp        : variant;
      ByRefParms : boolean;
      idx        : integer;
    begin
      QueryStatus := 51;
      MethodName := Query.Name;
      {
      if (fLogAgents <> nil) and fLogAgents.LogableMethod(MethodName)
        then LogMethod(TObject(ObjectId), 'call ' + copy(QueryText, SavePos, QueryLen - ScanPos + 1));
      }
      if Query.QKind = qkCallRet
        then TVarData(Res).VType := varVariant
        else Res := UnAssigned;
      if Query.ParamCount > 0
        then Params := VarArrayCreate([1, Query.ParamCount], varVariant)
        else Params := UnAssigned;
      ByRefParms := false;
      idx := 0;
      for ParamIdx := 1 to Query.ParamCount do
        begin
          Params[ParamIdx] := Query.PopParam(idx);
          if TVarData(Params[ParamIdx]).VType = varVariant
            then
              begin
                TVarData(Tmp).VType := varVariant;
                GetMem(TVarData(Tmp).VPointer, SizeOf(variant));
                Params[ParamIdx] := Tmp;
                ByRefParms := true;
              end;
        end;

      QueryStatus := 52;
      fObjServer.CallMethod(Query.ObjId, MethodName, Params, Res, ErrorCode, QueryStatus);
      QueryStatus := 53;

      if ErrorCode = errNoError
        then
          begin
            result := TRDOQuery.Create(qkAnswer, Query.Id);
            if Query.QKind = qkCallRet
              then
                try
                  result.PushParam(Res);
                except
                  ErrorCode := errMalformedResult;
                  result.QKind := qkError;
                  result.PushParam(errMalformedResult);
                end
              else
                if not ByRefParms
                  then result.PushParam(errNoError);
          end
        else
          begin
            result := TRDOQuery.Create(qkError, Query.Id);
            result.PushParam(ErrorCode);
          end;

      if (ErrorCode = errNoError) and ByRefParms
        then
          begin
            for ParamIdx := 1 to Query.ParamCount do
              begin
                if VarType(Params[ParamIdx]) and varTypeMask = varVariant
                  then
                    begin
                      try
                        if ErrorCode = errNoError
                          then result.PushParam(PVariant(TVarData(Params[ParamIdx]).VPointer)^);
                      except
                        ErrorCode := errMalformedResult;
                        result.Free;
                        result := TRDOQuery.Create(qkError, Query.Id);
                        result.PushParam(ErrorCode);
                      end;
                      FreeMem(TVarData(Params[ParamIdx]).VPointer);
                    end
              end;
          end;
    end;

  function TRDOQueryServer.IdOfCommand(Query : TRDOQuery) : TRDOQuery;
    var
      ObjectId    : integer;
      ErrorCode   : integer;
      QueryStatus : integer;
    begin
      ObjectId := fObjServer.GetIdOf(Query.Name, ErrorCode, QueryStatus);
      if ErrorCode = errNoError
        then
          begin
            result := TRDOQuery.Create(qkAnswer, Query.Id);
            result.PushParam(ObjectId);
          end
        else
          begin
            result := TRDOQuery.Create(qkError, Query.Id);
            result.PushParam(ErrorCode);
          end;
    end;

end.
