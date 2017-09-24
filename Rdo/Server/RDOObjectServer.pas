unit RDOObjectServer;

interface

  uses
    SyncObjs, RDOObjectRegistry;

  type
    TRDOObjectServer =
      class
        public
          constructor Create( ObjectRegistry : TRDOObjectsRegistry; CriticalSection : TCriticalSection );
        public
          procedure SetCriticalSection( CriticalSection : TCriticalSection );
          procedure Lock;
          procedure UnLock;
        public
          function  GetProperty( ObjectId : integer; const PropName : string; out ErrorCode : integer; var QueryStatus, RDOCallCnt : integer ) : variant; stdcall;
          procedure SetProperty( ObjectId : integer; const PropName : string; const PropValue : variant; out ErrorCode : integer; var QueryStatus, RDOCallCnt : integer ); stdcall;
          procedure CallMethod( ObjectId : integer; const MethodName : string; var Params : variant; var Res : variant; out ErrorCode : integer; var QueryStatus, RDOCallCnt : integer ); stdcall;
          function  GetIdOf( ObjectName : string; out ErrorCode : integer; var QueryStatus : integer ) : integer; stdcall;
        private
          fObjectRegistry  : TRDOObjectsRegistry;
          fCriticalSection : TCriticalSection;
      end;

implementation

  uses
    Windows, SysUtils, TypInfo, RDOInterfaces, ErrorCodes, 
    {$IFDEF VER140}
      Variants,
    {$ENDIF}
    Logs;

  var
    RDOCallCounter : integer = 0;

  // TRDOObjectServer

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

  function TRDOObjectServer.GetProperty( ObjectId : integer; const PropName : string; out ErrorCode : integer; var QueryStatus, RDOCallCnt : integer ) : variant;
    var
      theObject   : TObject;
      thePropInfo : PPropInfo;
      LockObjIntf : ILockObject;
      Dummy       : variant;
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
                          tkString, tkLString, tkWString:
                            Result := GetStrProp( theObject, thePropInfo );
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
                      CallMethod( ObjectId, PropName, Dummy, Result, ErrorCode, QueryStatus, RDOCallCnt );
                    end
              end
            else
              ErrorCode := errUnexistentProperty
        finally
          UnLock
        end
      except
        ErrorCode := errIllegalObject;
        Logs.Log('Survival', DateTimeToStr(Now) + 'Error at: TRDOObjectServer.GetProperty (126)');
      end
    end;

  procedure TRDOObjectServer.SetProperty( ObjectId : integer; const PropName : string; const PropValue : variant; out ErrorCode : integer; var QueryStatus, RDOCallCnt : integer );
    var
      theObject   : TObject;
      thePropInfo : PPropInfo;
      LockObjIntf : ILockObject;
    begin
      ErrorCode := errNoError;
      theObject := TObject( ObjectId );
      try
        QueryStatus := 411;
        Lock;
        QueryStatus := 412;
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
                          tkString, tkLString, tkWString:
                              SetStrProp( theObject, thePropInfo, string(PropValue) );
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
                      ErrorCode := errIllegalPropValue;
                      Logs.Log('Survival', DateTimeToStr(Now) + 'Error at: TRDOObjectServer.SetProperty (174)');
                    end
                  else
                    ErrorCode := errUnexistentProperty
              end
            else
              ErrorCode := errUnexistentProperty
        finally
          UnLock
        end;
        QueryStatus := 413;
      except
        ErrorCode := errIllegalObject;
        Logs.Log('Survival', DateTimeToStr(Now) + 'Error at: TRDOObjectServer.SetProperty (186)');
      end
    end;

  procedure TRDOObjectServer.CallMethod( ObjectId : integer; const MethodName : string; var Params : variant; var Res : variant; out ErrorCode : integer; var QueryStatus, RDOCallCnt : integer );
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
                  RDOCallCnt  := Windows.InterlockedIncrement(RDOCallCounter);
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
                      jnz  @CheckIfInteger
                      mov  edi, TVarData( [esi] ).VPointer
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
                            Logs.Log('Survival', DateTimeToStr(Now) + ' Ignored result of function ' + MethodName);
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
                  ErrorCode := errIllegalParamList;
                  Logs.Log('Survival', DateTimeToStr(Now) + ' Error at: TRDOObjectServer.CallMethod "' + MethodName + '" (319)');
                end
              else
                ErrorCode := errUnexistentMethod
          except
            QueryStatus := 524;
            ErrorCode := errIllegalObject;
            Logs.Log('Survival', DateTimeToStr(Now) + ' Error at: TRDOObjectServer.CallMethod "' + MethodName + '" (326)');
          end;
    end;

  function TRDOObjectServer.GetIdOf( ObjectName : string; out ErrorCode : integer; var QueryStatus : integer ) : integer;
    var
      ObjectId : integer;
    begin
      ObjectId := fObjectRegistry.GetObjectId( ObjectName );
      if ObjectId = NoId
        then ErrorCode := errIllegalObject
        else ErrorCode := errNoError;
      Result := ObjectId
    end;

end.
