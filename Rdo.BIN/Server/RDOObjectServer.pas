unit RDOObjectServer;

interface

  uses
    SyncObjs, RDOObjectRegistry;

  type
    TRDOObjectServer =
      class
        private
          fObjectRegistry  : TRDOObjectsRegistry;
          fCriticalSection : TCriticalSection;
        public
          constructor Create( ObjectRegistry : TRDOObjectsRegistry; CriticalSection : TCriticalSection );
        public
          procedure SetCriticalSection( CriticalSection : TCriticalSection );
          procedure Lock;
          procedure UnLock;
        public
          function  GetProperty( ObjectId : integer; const PropName : string; out ErrorCode : integer; var QueryStatus : integer ) : variant; stdcall;
          procedure SetProperty( ObjectId : integer; const PropName : string; const PropValue : variant; out ErrorCode : integer; var QueryStatus : integer ); stdcall;
          procedure CallMethod( ObjectId : integer; const MethodName : string; var Params : variant; var Res : variant; out ErrorCode : integer; var QueryStatus : integer ); stdcall;
          function  GetIdOf( ObjectName : string; out ErrorCode : integer; var QueryStatus : integer ) : integer; stdcall;
      end;

implementation

  uses
    {$IFDEF USELogs}
    SysUtils, TypInfo, RDOInterfaces, ErrorCodes, Logs;
    {$ELSE}
    SysUtils, TypInfo, RDOInterfaces, ErrorCodes;
    {$ENDIF}

  constructor TRDOObjectServer.Create( ObjectRegistry : TRDOObjectsRegistry; CriticalSection : TCriticalSection );
    begin
      inherited Create;
      fObjectRegistry := ObjectRegistry;
      fCriticalSection := CriticalSection
    end;

  procedure TRDOObjectServer.Lock;
    begin
      if Assigned( fCriticalSection )
        then
          fCriticalSection.Acquire
    end;

  procedure TRDOObjectServer.UnLock;
    begin
      if Assigned( fCriticalSection )
        then
          fCriticalSection.Release
    end;

  procedure TRDOObjectServer.SetCriticalSection( CriticalSection : TCriticalSection );
    begin
      fCriticalSection := CriticalSection
    end;

  function TRDOObjectServer.GetProperty( ObjectId : integer; const PropName : string; out ErrorCode : integer; var QueryStatus : integer ) : variant;
    var
      theObject   : TObject;
      thePropInfo : PPropInfo;
      LockObjIntf : ILockObject;
      Dummy       : variant;
      TmpWideStr  : widestring;
    begin
      Result := '';
      LockObjIntf := nil;
      ErrorCode := errNoError;
      theObject := TObject( ObjectId );
      try
        Lock;
        try
          if (theObject <> nil) and (theObject.ClassInfo <> nil)
            then
              begin
                thePropInfo := GetPropInfo( theObject.ClassInfo, PropName );
                if thePropInfo <> nil
                  then
                    begin
                      theObject.GetInterface( ILockObject, LockObjIntf );
                      if LockObjIntf <> nil
                        then
                          LockObjIntf.Lock;
                      try
                        case thePropInfo.PropType^.Kind of
                          tkInteger, tkChar, tkEnumeration, tkSet, tkWChar:
                            Result := GetOrdProp( theObject, thePropInfo );
                          tkString, tkLString:
                            Result := GetStrProp( theObject, thePropInfo );
                          tkWString:
                            begin
                              GetWideStrProp( theObject, thePropInfo, TmpWideStr );
                              Result := TmpWideStr
                            end;
                          tkFloat:
                            Result := GetFloatProp( theObject, thePropInfo );
                          tkVariant:
                            Result := GetVariantProp( theObject, thePropInfo )
                          else
                            ErrorCode := errIllegalPropType
                        end
                      finally
                        if LockObjIntf <> nil
                          then
                            LockObjIntf.UnLock
                      end
                    end
                  else
                    begin
                      TVarData( Result ).VType := varVariant;
                      Dummy := UnAssigned;
                      CallMethod( ObjectId, PropName, Dummy, Result, ErrorCode, QueryStatus );
                    end
              end
            else
              ErrorCode := errUnexistentProperty
        finally
          UnLock
        end
      except
        ErrorCode := errIllegalObject
      end
    end;

  procedure TRDOObjectServer.SetProperty( ObjectId : integer; const PropName : string; const PropValue : variant; out ErrorCode : integer; var QueryStatus : integer );
    var
      theObject   : TObject;
      thePropInfo : PPropInfo;
      LockObjIntf : ILockObject;
    begin
      ErrorCode := errNoError;
      theObject := TObject( ObjectId );
      try
        Lock;
        try
          if (theObject <> nil) and (theObject.ClassInfo <> nil)
            then
              begin
                thePropInfo := GetPropInfo( theObject.ClassInfo, PropName );
                if thePropInfo <> nil
                  then
                    try
                      theObject.GetInterface( ILockObject, LockObjIntf );
                      if LockObjIntf <> nil
                        then
                          LockObjIntf.Lock;
                      try
                        case thePropInfo.PropType^.Kind of
                          tkInteger, tkChar, tkEnumeration, tkSet, tkWChar:
                            SetOrdProp( theObject, thePropInfo, PropValue );
                          tkString, tkLString:
                            SetStrProp( theObject, thePropInfo, string(PropValue) );
                          tkWString:
                            SetWideStrProp( theObject, thePropInfo, widestring(PropValue) );
                          tkFloat:
                            SetFloatProp( theObject, thePropInfo, PropValue );
                          tkVariant:
                            SetVariantProp( theObject, thePropInfo, PropValue );
                          else
                            ErrorCode := errIllegalPropType
                        end
                      finally
                        if LockObjIntf <> nil
                          then
                            LockObjIntf.UnLock
                      end
                    except
                      ErrorCode := errIllegalPropValue
                    end
                  else
                    ErrorCode := errUnexistentProperty
              end
            else
              ErrorCode := errUnexistentProperty
        finally
          UnLock
        end
      except
        ErrorCode := errIllegalObject
      end
    end;

  procedure TRDOObjectServer.CallMethod( ObjectId : integer; const MethodName : string; var Params : variant; var Res : variant; out ErrorCode : integer; var QueryStatus : integer );
    const
      MaxRegs = 3;
      JustEAX = 1;
    var
      theObject   : TObject;
      MethodAddr  : pointer;
      ParamCount  : integer;
      ParamIndex  : integer;
      ParamsPtr   : PVarData;
      RegsUsed    : integer;
      LockObjIntf : ILockObject;
      SecLock     : TCriticalSection;
    begin
      QueryStatus   := 521;
      theObject := TObject( ObjectId );
      if ObjectId = 0
        then ErrorCode := errIllegalObject
        else
          try
            MethodAddr := theObject.MethodAddress( MethodName );
            if MethodAddr <> nil
              then
                try
                  ParamIndex := 1;
                  if TVarData( Params ).VType and varArray <> 0
                    then
                      begin
                        ParamCount := VarArrayHighBound( Params, 1 );
                        ParamsPtr := VarArrayLock( Params )
                      end
                    else
                      ParamCount := 0;
                  RegsUsed := 1;
                  theObject.GetInterface( ILockObject, LockObjIntf );
                  if LockObjIntf <> nil
                    then
                      LockObjIntf.Lock;
                  Lock;
                  SecLock := fCriticalSection;
                  QueryStatus := 522;
                  try
                    asm
                      push eax
                      push esi
                      push edi
                      mov  eax, ParamCount
                      mov  esi, ParamsPtr

    @NextParam:       cmp  ParamIndex, eax
                      ja   @ResParam
                      cmp  word ptr TVarData( [esi] ).VType, varVariant
                      jnz  @ChechPointer
                      mov  edi, TVarData( [esi] ).VPointer
                      cmp  RegsUsed, MaxRegs
                      jnz  @UseRegister
                      push edi
                      jmp  @Iterate

    @ChechPointer:    cmp  word ptr TVarData( [esi] ).VType, varByRef
                      jnz  @CheckIfInteger
                      mov  edi, TVarData( [esi] ).vPointer
                      cmp  RegsUsed, MaxRegs
                      jnz  @UseRegister
                      push edi
                      jmp  @Iterate

    @CheckIfInteger:  cmp  word ptr TVarData( [esi] ).VType, varInteger
                      jnz  @CheckIfSingle
                      mov  edi, TVarData( [esi] ).VInteger
                      cmp  RegsUsed, MaxRegs
                      jnz  @UseRegister
                      push edi
                      jmp  @Iterate

    @CheckIfSingle:   cmp  word ptr TVarData( [esi] ).VType, varSingle
                      jnz  @CheckIfDouble
                      push TVarData( [esi] ).VSingle
                      jmp  @Iterate

    @CheckIfDouble:   cmp  word ptr TVarData( [esi] ).VType, varDouble
                      jnz  @String
                      mov  edi, dword ptr TVarData( [esi] ).VDouble + Type( dword )
                      push edi
                      mov  edi, dword ptr TVarData( [esi] ).VDouble
                      push edi
                      jmp  @Iterate

    @String:          mov  edi, TVarData( [esi] ).VOLEStr
                      cmp  RegsUsed, MaxRegs
                      jnz  @UseRegister
                      push edi
                      jmp  @Iterate

    @UseRegister:     cmp  RegsUsed, JustEAX
                      jnz  @UseECX
                      mov  edx, edi
                      inc  RegsUsed
                      jmp  @Iterate

    @UseECX:          mov  ecx, edi
                      inc  RegsUsed
    @Iterate:         inc  ParamIndex
                      add  esi, Type( TVarData )
                      jmp  @NextParam

    @ResParam:        mov  edi, Res
                      cmp  word ptr TVarData( [ edi ] ).VType, varEmpty
                      jz   @DoCall
                      cmp  RegsUsed, JustEAX
                      jnz  @TryWithECX
                      mov  edx, edi
                      jmp  @DoCall

    @TryWithECX:      cmp  RegsUsed, MaxRegs
                      jz   @PushResParam
                      mov  ecx, edi
                      jmp  @DoCall

    @PushResParam:    push edi
    @DoCall:          mov  eax, theObject
                      call MethodAddr
                      pop  edi
                      pop  esi
                      pop  eax
                    end;
                  finally
                    QueryStatus := 523;
                    UnLock;
                    if SecLock <> fCriticalSection
                      then
                        begin
                          fCriticalSection := SecLock;
                          {$IFDEF USELogs}
                          try
                            Logs.Log('RDO', 'Ignored result of function ' + MethodName);
                          except
                          end;
                          {$ENDIF}
                        end;
                    if LockObjIntf <> nil
                      then
                        LockObjIntf.UnLock;
                    if TVarData( Params ).VType and varArray <> 0
                      then
                        VarArrayUnlock( Params )
                  end
                except
                  QueryStatus := 524;
                  ErrorCode := errIllegalParamList
                end
              else
                ErrorCode := errUnexistentMethod
          except
            QueryStatus := 524;
            ErrorCode := errIllegalObject
          end;
    end;

  function TRDOObjectServer.GetIdOf( ObjectName : string; out ErrorCode : integer; var QueryStatus : integer ) : integer;
    var
      ObjectId : integer;
    begin
      ObjectId := fObjectRegistry.GetObjectId( ObjectName );
      if ObjectId = NoId
        then
          ErrorCode := errIllegalObject
        else
          ErrorCode := errNoError;
      Result := ObjectId
    end;

end.
