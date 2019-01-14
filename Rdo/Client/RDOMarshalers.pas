unit RDOMarshalers;

interface

  uses
    {$IFDEF AutoServer}
    RDOClient_TLB,
    {$ENDIF}
    {$IFDEF VER140} // delphi 6
    Variants,
    {$ENDIF}
    {$IFDEF VER150} // delphi 7
    Variants,
    {$ENDIF}
    RDOInterfaces;

  function  MarshalPropertyGet( ObjectId : integer; const PropName : string; RDOConnection : IRDOConnection; TimeOut, Priority : integer; out ErrorCode : integer ) : variant;
  procedure MarshalPropertySet( ObjectId : integer; const PropName : string; const PropValue : variant; RDOConnection : IRDOConnection; TimeOut, Priority : integer; out ErrorCode : integer );
  procedure MarshalMethodCall( ObjectId : integer; const MethodName : string; var Params : variant; var Res : variant; RDOConnection : IRDOConnection; TimeOut, Priority : integer; out ErrorCode : integer );
  function  MarshalObjIdGet( ObjectName : string; RDOConnection : IRDOConnection; TimeOut, Priority : integer; out ErrorCode : integer ) : integer;

implementation

  uses
    Windows, SysUtils, RDOProtocol, RDOUtils
   {$IFDEF VER140}
    ,Variants
   {$ENDIF}
   ,ErrorCodes;

  function GetQueryErrorCode( QueryResText : string ) : integer;
    var
      ScanPos         : integer;
      QueryResLen     : integer;
      ErrorKeyWrdPos  : integer;
      ErrorCodeDigits : string;
    begin
      ErrorKeyWrdPos := KeyWordPos( ErrorKeyWord, QueryResText );
      if ErrorKeyWrdPos <> 0
        then
          begin
            ScanPos := ErrorKeyWrdPos + Length( ErrorKeyWord );
            QueryResLen := Length( QueryResText );
            while ( ScanPos <= QueryResLen ) and ( QueryResText[ ScanPos ] in WhiteSpace ) do
              inc( ScanPos );
            ErrorCodeDigits := '';
            if ScanPos <= QueryResLen
              then
                begin
                  while ( ScanPos <= QueryResLen ) and ( QueryResText[ ScanPos ] in [ '0' .. '9' ] ) do
                    begin
                      ErrorCodeDigits := ErrorCodeDigits + QueryResText[ ScanPos ];
                      inc( ScanPos )
                    end;
                  try
                    Result := StrToInt( ErrorCodeDigits )
                  except
                    Result := errMalformedResult
                  end
                end
              else
                Result := errMalformedResult
          end
        else
          Result := errNoError
    end;

  function GetVariableValue( QueryResText, VarName : string; out Error : boolean ) : string;
    var
      ScanPos         : integer;
      VarNamePos      : integer;
      QueryResLen     : integer;
      VarValueDelimit : char;
      EndOfLiteral    : boolean;
    begin
      Result := '';
      Error := false;
      VarNamePos := KeyWordPos( VarName, QueryResText );
      if VarNamePos <> 0
        then
          begin
            ScanPos := VarNamePos + Length( VarName );
            QueryResLen := Length( QueryResText );
            while ( ScanPos <= QueryResLen ) and ( QueryResText[ ScanPos ] in WhiteSpace ) do
              inc( ScanPos );
            if ( ScanPos <= QueryResLen ) and ( QueryResText[ ScanPos ] = NameValueSep )
              then
                begin
                  inc( ScanPos );
                  while ( ScanPos <= QueryResLen ) and ( QueryResText[ ScanPos ] in WhiteSpace ) do
                    inc( ScanPos );
                  if ScanPos <= QueryResLen
                    then
                      begin
                        VarValueDelimit := QueryResText[ ScanPos ];
                        if ( VarValueDelimit <> Quote ) and ( VarValueDelimit <> LiteralDelim )
                          then
                            VarValueDelimit := Blank;
                        inc( ScanPos );
                        repeat
                          while ( ScanPos <= QueryResLen ) and ( QueryResText[ ScanPos ] <> VarValueDelimit ) do
                            begin
                              Result := Result + QueryResText[ ScanPos ];
                              inc( ScanPos )
                            end;
                          inc( ScanPos );
                          if ( ScanPos <= QueryResLen ) and ( QueryResText[ ScanPos ] = VarValueDelimit )
                            then
                              begin
                                EndOfLiteral := false;
                                Result := Result + VarValueDelimit;
                                inc( ScanPos )
                              end
                            else
                              EndOfLiteral := true;
                        until EndOfLiteral
                      end
                    else
                      Error := true
                end
              else
                Error := true
          end
        else
          Error := true
    end;

  function MarshalPropertyGet( ObjectId : integer; const PropName : string; RDOConnection : IRDOConnection; TimeOut, Priority : integer; out ErrorCode : integer ) : variant;
    var
      QueryText   : string;
      QueryResult : string;
      VarValue    : string;
      Error       : boolean;
    begin
      Result := '';
      if Priority <> THREAD_PRIORITY_NORMAL
        then
          QueryText := PriorityToPriorityId( Priority ) + Blank
        else
          QueryText := '';
      QueryText := QueryText + SelObjCmd + Blank + IntToStr( ObjectId ) + Blank + GetPropCmd + Blank + PropName + QueryTerm;
      QueryResult := RDOConnection.SendReceive( QueryText, ErrorCode, TimeOut );
      if ErrorCode = errNoError
        then
          begin
            ErrorCode := GetQueryErrorCode( QueryResult );
            if ErrorCode = errNoError
              then
                begin
                  VarValue := GetVariableValue( QueryResult, PropName, Error );
                  if Error
                    then
                      ErrorCode := errMalformedResult
                    else
                      try
                        Result := GetVariantFromStr( VarValue )
                      except
                        ErrorCode := errMalformedResult;
                        Result := ''
                      end
                end
          end
    end;

  procedure MarshalPropertySet( ObjectId : integer; const PropName : string; const PropValue : variant; RDOConnection : IRDOConnection; TimeOut, Priority : integer; out ErrorCode : integer );
    var
      QueryText    : string;
      QueryResult  : string;
      PropValAsStr : string;
      IllegalVType : boolean;
    begin
      PropValAsStr := GetStrFromVariant( PropValue, IllegalVType );
      if not IllegalVType
        then
          begin
            if Priority <> THREAD_PRIORITY_NORMAL
              then
                QueryText := PriorityToPriorityId( Priority ) + Blank
              else
                QueryText := '';
            QueryText := QueryText + SelObjCmd + Blank + IntToStr( ObjectId ) + Blank + SetPropCmd + Blank + PropName + NameValueSep + LiteralDelim + PropValAsStr + LiteralDelim + QueryTerm;
            if TimeOut <> 0
              then
                begin
                  QueryResult := RDOConnection.SendReceive( QueryText, ErrorCode, TimeOut );
                  if ErrorCode = errNoError
                    then
                      ErrorCode := GetQueryErrorCode( QueryResult )
                end
              else
                begin
                  RDOConnection.Send( QueryText );
                  ErrorCode := errNoError
                end
          end
        else
          ErrorCode := errIllegalPropValue
    end;

  procedure MarshalMethodCall( ObjectId : integer; const MethodName : string; var Params : variant; var Res : variant; RDOConnection : IRDOConnection; TimeOut, Priority : integer; out ErrorCode : integer );
    var
      QueryText    : string;
      QueryResult  : string;
      ParamIdx     : integer;
      ParamCount   : integer;
      BRefParamIdx : integer;
      ParamAsStr   : string;
      IllegalVType : boolean;
      VarValue     : string;
      Error        : boolean;
      TmpVariant   : variant;
    begin
      ErrorCode := errNoError;
      if Priority <> THREAD_PRIORITY_NORMAL
        then
          QueryText := PriorityToPriorityId( Priority ) + Blank
        else
          QueryText := '';
      if TVarData( Res ).VType <> varEmpty
        then
          QueryText := QueryText + SelObjCmd + Blank + IntToStr( ObjectId ) + Blank + CallMethCmd + Blank + MethodName + Blank + LiteralDelim + VariantId + LiteralDelim + Blank
        else
          QueryText := QueryText + SelObjCmd + Blank + IntToStr( ObjectId ) + Blank + CallMethCmd + Blank + MethodName + Blank + LiteralDelim + VoidId + LiteralDelim + Blank;
      ParamCount := 0;
      if not IllegalVType
        then
          begin
            ParamIdx := 1;
            if TVarData( Params ).VType and varArray <> 0
              then
                ParamCount := VarArrayHighBound( Params, 1 );
            while ( ParamIdx <= ParamCount ) and ( ErrorCode = errNoError ) do
              begin
                if TVarData( Params[ ParamIdx ] ).VType and varArray = 0
                  then
                    begin
                      if TVarData( Params[ ParamIdx ] ).VType and varTypeMask = varVariant
                        then
                          begin
                            TVarData( TmpVariant ).VType := varVariant;
                            Params[ ParamIdx ] := TmpVariant;
                            ParamAsStr := GetTypeIdFromVariant( Params[ ParamIdx ], IllegalVType )
                          end
                        else
                          ParamAsStr := GetStrFromVariant( Params[ ParamIdx ], IllegalVType );
                      if not IllegalVType
                        then
                          begin
                            QueryText := QueryText + LiteralDelim + ParamAsStr + LiteralDelim;
                            if ParamIdx <> ParamCount
                              then
                                QueryText := QueryText + ParamDelim
                          end
                        else
                          ErrorCode := errIllegalParamList
                    end
                  else
                    ErrorCode := errIllegalParamList;
                inc( ParamIdx )
              end
          end
        else
          ErrorCode := errUnknownError;
      if ErrorCode = errNoError
        then
          begin
            QueryText := QueryText + QueryTerm;
            if ( TimeOut <> 0 ) or ( TVarData( Res ).VType <> varEmpty )
              then
                begin
                  QueryResult := RDOConnection.SendReceive( QueryText, ErrorCode, TimeOut );
                  if ErrorCode = errNoError
                    then
                      begin
                        ErrorCode := GetQueryErrorCode( QueryResult );
                        if ( ErrorCode = errNoError ) and ( TVarData( Res ).VType <> varEmpty )
                          then
                            begin
                              VarValue := GetVariableValue( QueryResult, ResultVarName, Error );
                              if Error
                                then
                                  begin
                                    ErrorCode := errMalformedResult;
                                    Res := ''
                                  end
                                else
                                  try
                                    Res := GetVariantFromStr( VarValue );
                                  except
                                    ErrorCode := errIllegalFunctionRes;
                                    Res := ''
                                  end
                            end;

                        if TVarData( Params ).VType <> varEmpty
                          then
                            begin
                              BRefParamIdx := 1;
                              ParamIdx := 1;
                              while ParamIdx <= ParamCount do
                                begin
                                  if TVarData( Params[ ParamIdx ] ).VType and varTypeMask = varVariant
                                    then
                                      begin
                                        VarValue := GetVariableValue( QueryResult, ByRefParName + IntToStr( BRefParamIdx ), Error );
                                        if Error
                                          then
                                            ErrorCode := errMalformedResult
                                          else
                                            try
                                              Params[ ParamIdx ] := GetVariantFromStr( VarValue )
                                            except
                                              ErrorCode := errIllegalFunctionRes
                                            end;
                                        inc( BRefParamIdx )
                                      end;
                                  inc( ParamIdx )
                                end
                            end
                      end
                end
              else
                RDOConnection.Send( QueryText )
          end
    end;

  function MarshalObjIdGet( ObjectName : string; RDOConnection : IRDOConnection; TimeOut, Priority : integer; out ErrorCode : integer ) : integer;
    var
      QueryText   : string;
      QueryResult : string;
      VarValue    : string;
      Error       : boolean;
    begin
      Result := 0;
      if Priority <> THREAD_PRIORITY_NORMAL
        then
          QueryText := PriorityToPriorityId( Priority ) + Blank
        else
          QueryText := '';
      QueryText := QueryText + IdOfCmd + Blank + LiteralDelim + ObjectName + LiteralDelim + QueryTerm;
      QueryResult := RDOConnection.SendReceive( QueryText, ErrorCode, TimeOut );
      if ErrorCode = errNoError
        then
          begin
            ErrorCode := GetQueryErrorCode( QueryResult );
            if ErrorCode = errNoError
              then
                begin
                  VarValue := GetVariableValue( QueryResult, ObjIdVarName, Error );
                  if not Error
                    then
                      try
                        Result := StrToInt( VarValue )
                      except
                        ErrorCode := errMalformedResult
                      end
                    else
                      ErrorCode := errMalformedResult
                end
          end
    end;

end.
