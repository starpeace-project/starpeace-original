unit RDOMarshalers;

interface

  uses
    ActiveX,
    {$IFDEF AutoServer}
    RDOClient_TLB,
    {$ENDIF}
    RDOInterfaces,
    RDOQueries;

{$IFDEF AutoServer}
  function  MarshalPropertyGet( ObjectId : integer; const PropName : string; RDOConnection : IRDOConnection; TimeOut, Priority : integer; out ErrorCode : integer ) : OleVariant;
{$ELSE}
  function  MarshalPropertyGet( ObjectId : integer; const PropName : string; RDOConnection : IRDOConnection; TimeOut, Priority : integer; out ErrorCode : integer ) : variant;
{$ENDIF}
  procedure MarshalPropertySet( ObjectId : integer; const PropName : string; const PropValue : variant; RDOConnection : IRDOConnection; TimeOut, Priority : integer; out ErrorCode : integer );
  procedure MarshalMethodCall( ObjectId : integer; const MethodName : string; const Params : PDispParams; var Res : variant; RDOConnection : IRDOConnection; TimeOut, Priority : integer; out ErrorCode : integer ); // procedure MarshalMethodCall( ObjectId : integer; const MethodName : string; var Params : variant; var Res : variant; RDOConnection : IRDOConnection; TimeOut, Priority : integer; out ErrorCode : integer );
  function  MarshalObjIdGet( ObjectName : string; RDOConnection : IRDOConnection; TimeOut, Priority : integer; out ErrorCode : integer ) : integer;

implementation

  uses
    Windows, SysUtils, RDOProtocol, RDOUtils, ErrorCodes, RDOVariantUtils;

{$IFDEF AutoServer}
  function  MarshalPropertyGet( ObjectId : integer; const PropName : string; RDOConnection : IRDOConnection; TimeOut, Priority : integer; out ErrorCode : integer ) : OleVariant;
{$ELSE}
  function  MarshalPropertyGet( ObjectId : integer; const PropName : string; RDOConnection : IRDOConnection; TimeOut, Priority : integer; out ErrorCode : integer ) : variant;
{$ENDIF}
    var
      Query       : TRDOQuery;
      QueryResult : TRDOQuery;
      idx         : integer;
    begin
      result := Unassigned;
      Query  := TRDOQuery.Create(qkGetProp, 0);
      try
        if Priority <> THREAD_PRIORITY_NORMAL
          then Query.Priority := ThreadPriorityToQueryPriority(Priority);
        Query.ObjId := ObjectId;
        Query.Name  := PropName;
        QueryResult := RDOConnection.SendReceive(Query, ErrorCode, TimeOut);
         if ErrorCode = errNoError
          then
            begin
              if QueryResult <> nil
                then
                  try
                    idx := 0;
                    if QueryResult.QKind = qkError
                      then
                        try
                          ErrorCode := QueryResult.PopParam(idx);
                        except
                          ErrorCode := errMalformedResult;
                        end
                      else
                        try
                          result := QueryResult.PopParam(idx);
                        except
                          ErrorCode := errMalformedResult;
                        end;
                  finally
                    QueryResult.Free;
                  end
                else ErrorCode := errMalformedResult;
            end;
      finally
        Query.Free;
      end;
    end;

  procedure MarshalPropertySet( ObjectId : integer; const PropName : string; const PropValue : variant; RDOConnection : IRDOConnection; TimeOut, Priority : integer; out ErrorCode : integer );
    var
      Query        : TRDOQuery;
      QueryResult  : TRDOQuery;
    begin
      Query := TRDOQuery.Create(qkSetProp, 0);
      try
        if Priority <> THREAD_PRIORITY_NORMAL
          then Query.Priority := ThreadPriorityToQueryPriority(Priority);
        Query.ObjId := ObjectId;
        Query.Name := PropName;
        try
          Query.PushParam(PropValue);
          ErrorCode := errNoError;
        except
          ErrorCode := errIllegalPropValue
        end;
        if ErrorCode = errNoError
          then
            if TimeOut <> 0
              then
                begin
                  try
                    QueryResult := RDOConnection.SendReceive( Query, ErrorCode, TimeOut );
                    QueryResult.Free;
                  finally
                    Query.Free;
                  end;
                end
              else
                begin
                  RDOConnection.Send(Query);
                  ErrorCode := errNoError;
                end;
      except
        ErrorCode := errUnknownError;
      end;
    end;

  procedure MarshalMethodCall( ObjectId : integer; const MethodName : string; const Params : PDispParams; var Res : variant; RDOConnection : IRDOConnection; TimeOut, Priority : integer; out ErrorCode : integer ); // procedure MarshalMethodCall( ObjectId : integer; const MethodName : string; var Params : variant; var Res : variant; RDOConnection : IRDOConnection; TimeOut, Priority : integer; out ErrorCode : integer );
    const
      MaxParams = 32;
    var
      Query       : TRDOQuery;
      QueryResult : TRDOQuery;
      ParamIdx    : integer;
      ParamCount  : integer;
      //TmpVariant  : variant;
      idx         : integer;
      ParmsByRef  : boolean;
      Aux         : PVariant;
      PtrRec      : TMarshalPtr;
      ParamInfo   : array[1..MaxParams] of boolean;
    begin
      ErrorCode := errNoError;

      // build the query
      if TVarData( Res ).VType <> varEmpty
        then Query := TRDOQuery.Create(qkCallRet, 0)
        else Query := TRDOQuery.Create(qkCallNoRet, 0);

      Query.ObjId := ObjectId;
      Query.Name  := MethodName;

      // set priority
      if Priority <> THREAD_PRIORITY_NORMAL
        then Query.Priority := ThreadPriorityToQueryPriority(Priority);

      // get param count
      {
      if TVarData( Params ).VType and varArray <> 0
        then ParamCount := VarArrayHighBound(Params, 1)
        else ParamCount := 0;
      }
      ParamCount := Params.cArgs;

      // push in/out params
      ParmsByRef := false;
      ParamIdx   := pred(ParamCount);
      while (ParamIdx >= 0) and (ErrorCode = errNoError) do
        begin
          if TVarData(Params.rgvarg[ ParamIdx ]).VType and varArray = 0
            then
              begin
                ParamInfo[ParamIdx] := false;
                try
                  if TVarData( Params.rgvarg[ ParamIdx ] ).VType and varTypeMask = varVariant
                    then
                      begin
                        Aux := PVariant(TVarData( Params.rgvarg[ ParamIdx ] ).vPointer);
                        if (Aux <> nil) and (TVarData(Aux^).vType = varPointer)
                          then
                            begin
                              PtrRec := GetMarshalPtr(Aux^);
                              Query.PushPtr(PtrRec.ptr, PtrRec.size);
                            end
                          else
                            begin
                              // check for marchaled types...
                              {
                              TVarData( TmpVariant ).VType := varVariant;
                              Params.rgvarg[ ParamIdx ] := TmpVariant;
                              }
                              Query.PushParamRef;
                              ParmsByRef := true;
                              ParamInfo[ParamIdx] := true;
                            end;
                      end
                    else Query.PushParam(variant(Params.rgvarg[ ParamIdx ]));
                except
                  ErrorCode := errIllegalParamList;
                end;
              end
            else ErrorCode := errIllegalParamList;
          dec( ParamIdx );
        end;

      if ErrorCode = errNoError
        then
          begin
            if (Query.QKind = qkCallRet) or ParmsByRef
              then
                try
                  // invoke method and wait...
                  QueryResult := RDOConnection.SendReceive( Query, ErrorCode, TimeOut );
                  try
                    if ErrorCode = errNoError
                      then
                        begin
                          idx := 0;
                          ErrorCode := GetQueryErrorCode( QueryResult );
                          if (ErrorCode = errNoError) and (TVarData( Res ).VType <> varEmpty )
                            then
                              try
                                if QueryResult.ParamCount > 0
                                  then Res := QueryResult.PopParam(idx)
                                  else Res := Unassigned;
                              except
                                Res := Unassigned;
                                ErrorCode := errMalformedResult;
                              end;
                          // Set out params
                          if (ErrorCode = errNoError) and (QueryResult <> nil) and ParmsByRef
                            then
                              begin
                                ParamIdx := pred(ParamCount);
                                while (ParamIdx >= 0) and (ErrorCode = errNoError) do
                                  begin
                                    if ParamInfo[ParamIdx]
                                      then
                                        try
                                          Params.rgvarg[ ParamIdx ].pvarVal^ := QueryResult.PopParam(idx);
                                        except
                                          ErrorCode := errMalformedResult;
                                        end;
                                    dec(ParamIdx);
                                  end;
                              end;
                        end;
                  finally
                    // release result
                    QueryResult.Free;
                  end;
                finally
                  // release query
                  Query.Free;
                end
              else RDOConnection.Send( Query ); // invoke method and don't wait...
          end
        else Query.Free;
    end;

  function MarshalObjIdGet( ObjectName : string; RDOConnection : IRDOConnection; TimeOut, Priority : integer; out ErrorCode : integer ) : integer;
    var
      Query       : TRDOQuery;
      QueryResult : TRDOQuery;
      idx         : integer;
    begin
      try
        Query := TRDOQuery.Create(qkGeID, 0);
        try
          if Priority <> THREAD_PRIORITY_NORMAL
            then Query.Priority := ThreadPriorityToQueryPriority(Priority);
          Query.Name := ObjectName;
          QueryResult := RDOConnection.SendReceive( Query, ErrorCode, TimeOut );
          try
            if ErrorCode = errNoError
              then
                begin
                  ErrorCode := GetQueryErrorCode( QueryResult );
                  if (ErrorCode = errNoError) and (QueryResult <> nil)
                    then
                      try
                        idx    := 0;
                        result := QueryResult.PopParam(idx);
                      except
                        result := 0;
                        ErrorCode := errMalformedQuery;
                      end
                    else result := 0;
                end
              else result := 0;
          finally
            QueryResult.Free;
          end;
        finally
          Query.Free;
        end;
      except
        result := 0;
        ErrorCode := errUnknownError;
      end;
    end;

end.
