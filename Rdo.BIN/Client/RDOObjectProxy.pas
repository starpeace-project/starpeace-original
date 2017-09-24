unit RDOObjectProxy;

interface

  uses
    Windows,
    ComObj,
    ActiveX,
    {$IFDEF AutoServer}
    RDOClient_TLB,
    {$ENDIF}
    RDOInterfaces;

  type
    TRDOObjectProxy =
      {$IFDEF AutoServer}
      class(TAutoObject, IRDOObjectProxy)
      {$ELSE}
      class(TInterfacedObject, IDispatch)
      {$ENDIF}
        private
          fObjectId      : integer;
          fRDOConnection : IRDOConnection;
          fTimeOut       : integer;
          fWaitForAnswer : boolean;
          fPriority      : integer;
          fErrorCode     : integer;
        protected
          function GetIDsOfNames(const IID : TGUID; Names : Pointer; NameCount, LocaleID : Integer; DispIDs : Pointer) : HResult; {$IFDEF AutoServer } override; {$ENDIF} stdcall;
          {$IFNDEF AutoServer}
          function GetTypeInfo(Index, LocaleID : Integer; out TypeInfo) : HResult; stdcall;
          function GetTypeInfoCount(out Count : Integer) : HResult; stdcall;
          {$ENDIF}
          function Invoke(DispID : Integer; const IID : TGUID; LocaleID : Integer; Flags : Word; var Params; VarResult, ExcepInfo, ArgErr : Pointer) : HResult; {$IFDEF AutoServer } override; {$ENDIF} stdcall;
          {$IFDEF AutoServer}
        protected
          procedure Initialize; override;
          {$ELSE}
        public
          constructor Create;
          {$ENDIF}
      end;

implementation

  uses
    {$IFDEF AutoServer}
    ComServ,
    {$ENDIF}
    SysUtils,
    ErrorCodes,
    RDOMarshalers,
    RDOVariantUtils;

  const
    BindToName        = 'bindto';
    SetConnectionName = 'setconnection';
    WaitForAnswerName = 'waitforanswer';
    TimeOutName       = 'timeout';
    PriorityName      = 'priority';
    ErrorCodeName     = 'errorcode';
    OnStartPageName   = 'onstartpage';   // Needed for ASP components only
    OnEndPageName     = 'onendpage';     // Needed for ASP components only
    RemoteObjectName  = 'remoteobjectid';

  const
    BindToDispId        = 1;
    SetConnectionDispId = 2;
    WaitForAnswerDispId = 3;
    TimeOutDispId       = 4;
    PriorityDispId      = 5;
    ErrorCodeDispId     = 6;
    OnStartPageDispId   = 7;
    OnEndPageDispId     = 8;
    RemoteObjectId      = 9;
    RemoteDispId        = 10;

  const
    DefTimeOut = 5000;

  const
    DefPriority = THREAD_PRIORITY_NORMAL;

  type
    PInteger = ^integer;

  var
    MembNameTLSIdx : dword = $FFFFFFFF; // >> Delphi 4

  function MapQueryErrorToHResult(ErrorCode : integer) : HResult;
    begin
      case ErrorCode of
        errNoError, errNoResult:
          result := S_OK;
        errMalformedQuery:
          result := E_UNEXPECTED;
        errIllegalObject:
          result := DISP_E_BADCALLEE;
        errUnexistentProperty:
          result := DISP_E_MEMBERNOTFOUND;
        errIllegalPropValue:
          result := DISP_E_TYPEMISMATCH;
        errUnexistentMethod:
          result := DISP_E_MEMBERNOTFOUND;
        errIllegalParamList:
          result := DISP_E_BADVARTYPE;
        errIllegalPropType:
          result := DISP_E_BADVARTYPE;
        else
          result := E_FAIL
      end
    end;

  // TObjectProxy

  {$IFDEF AutoServer}
  procedure TRDOObjectProxy.Initialize;
  {$ELSE}
  constructor TRDOObjectProxy.Create;
  {$ENDIF}
    begin
      inherited;
      fTimeOut := DefTimeOut;
      fPriority := DefPriority
    end;

  function TRDOObjectProxy.GetIDsOfNames(const IID : TGUID; Names : Pointer; NameCount, LocaleID : Integer; DispIDs : Pointer) : HResult;

    function MemberNameToDispId(MemberName : string) : integer;
      const
        LocalMembers : array [BindToDispId .. RemoteObjectId] of string =
          (
            BindToName,
            SetConnectionName,
            WaitForAnswerName,
            TimeOutName,
            PriorityName,
            ErrorCodeName,
            OnStartPageName,
            OnEndPageName,
            RemoteObjectName
         );
      var
        MemberIdx        : integer;
        MembNamLowerCase : string;
      begin
        MemberIdx := 1;
        MembNamLowerCase := LowerCase(MemberName);
        while (MemberIdx < RemoteDispId) and (LocalMembers[MemberIdx] <> MembNamLowerCase) do
          inc(MemberIdx);
        result := MemberIdx
      end;

    var
      MemberName : string;
    begin
      {$IFDEF AutoServer}
      if not Succeeded(inherited GetIDsOfNames(IID, Names, NameCount, LocaleID, DispIDs))
        then
          begin
      {$ENDIF}
            PInteger(DispIds)^ := MemberNameToDispId(WideCharToString(POLEStrList(Names)^[0]));
            if PInteger(DispIds)^ = RemoteDispId
              then
                begin
                  MemberName := POLEStrList(Names)^[0];
                  TLSSetValue(MembNameTLSIdx, StrNew(PChar(MemberName)))
                end;
            result := NOERROR
      {$IFDEF AutoServer}
          end
        else
          result := S_OK
      {$ENDIF}
    end;

  {$IFNDEF AutoServer}
  function TRDOObjectProxy.GetTypeInfo(Index, LocaleID : Integer; out TypeInfo) : HResult;
    begin
      pointer(TypeInfo) := nil;
      result := E_NOTIMPL
    end;

  function TRDOObjectProxy.GetTypeInfoCount(out Count : Integer) : HResult;
    begin
      Count := 0;
      result := NOERROR
    end;
  {$ENDIF}

  function TRDOObjectProxy.Invoke(DispID : Integer; const IID : TGUID; LocaleID : Integer; Flags : Word; var Params; VarResult, ExcepInfo, ArgErr : Pointer) : HResult;
    const
      MaxParams = 32;
    var
      Parameters  : TDispParams;
      RetValue    : variant;
      Handled     : boolean;
      MemberName  : PChar;
    begin
      {$IFDEF AutoServer}
      result := inherited Invoke(DispId, IID, LocaleID, Flags, Params, VarResult, ExcepInfo, ArgErr);
      if not Succeeded(result)
        then
          begin
      {$ENDIF}
            Parameters := TDispParams(Params);
            result := S_OK;
            case DispId of
              BindToDispId:
                if fRDOConnection <> nil
                  then
                    if Flags and DISPATCH_METHOD <> 0
                      then
                        if Parameters.cArgs = 1
                          then
                            begin
                              if VarResult <> nil
                                then
                                  PVariant(VarResult)^ := true;
                              case Parameters.rgvarg[0].vt and VT_TYPEMASK of
                                VT_INT:
                                  if Parameters.rgvarg[0].vt and VT_BYREF <> 0
                                    then
                                      fObjectId := Parameters.rgvarg[0].pintVal^
                                    else
                                      fObjectId := Parameters.rgvarg[0].intVal;
                                VT_I4:
                                  if Parameters.rgvarg[0].vt and VT_BYREF <> 0
                                    then
                                      fObjectId := Parameters.rgvarg[0].plVal^
                                    else
                                      fObjectId := Parameters.rgvarg[0].lVal;
                                VT_BSTR:
                                  begin
                                    if Parameters.rgvarg[0].vt and VT_BYREF <> 0
                                      then
                                        fObjectId := MarshalObjIdGet(Parameters.rgvarg[0].pbstrVal^, fRDOConnection, fTimeOut, fPriority, fErrorCode)
                                      else
                                        fObjectId := MarshalObjIdGet(Parameters.rgvarg[0].bstrVal, fRDOConnection, fTimeOut, fPriority, fErrorCode);
                                    if VarResult <> nil
                                      then
                                        if fErrorCode <> errNoError
                                          then
                                            PVariant(VarResult)^ := false
                                  end
                                else
                                  result := DISP_E_TYPEMISMATCH
                              end
                            end
                          else
                            result := DISP_E_BADPARAMCOUNT
                      else
                        result := DISP_E_MEMBERNOTFOUND
                  else
                    if VarResult <> nil
                      then
                        PVariant(VarResult)^ := false;
              SetConnectionDispId:
                if Flags and DISPATCH_METHOD <> 0
                  then
                    if Parameters.cArgs = 1
                      then
                        begin
                          if Parameters.rgvarg[0].vt and VT_TYPEMASK = VT_DISPATCH
                            then
                              if Parameters.rgvarg[0].vt and VT_BYREF <> 0
                                then
                                  try
                                    fRDOConnection := Parameters.rgvarg[0].pdispVal^ as IRDOConnection
                                  except
                                    result := DISP_E_TYPEMISMATCH
                                  end
                                else
                                  try
                                    fRDOConnection := IDispatch(Parameters.rgvarg[0].dispVal) as IRDOConnection
                                  except
                                    result := DISP_E_TYPEMISMATCH
                                  end
                            else
                              if Parameters.rgvarg[0].vt and VT_TYPEMASK = VT_UNKNOWN
                                then
                                  if Parameters.rgvarg[0].vt and VT_BYREF <> 0
                                    then
                                      try
                                        fRDOConnection := Parameters.rgvarg[0].punkVal^ as IRDOConnection
                                      except
                                        result := DISP_E_TYPEMISMATCH
                                      end
                                    else
                                      try
                                        fRDOConnection := IUnknown(Parameters.rgvarg[0].unkVal) as IRDOConnection
                                      except
                                        result := DISP_E_TYPEMISMATCH
                                      end
                                else
                                  if Parameters.rgvarg[0].vt and VT_TYPEMASK = VT_VARIANT
                                    then
                                      try
                                        fRDOConnection := IDispatch(Parameters.rgvarg[0].pvarVal^) as IRDOConnection
                                      except
                                        result := DISP_E_TYPEMISMATCH
                                      end
                                    else
                                      result := DISP_E_TYPEMISMATCH
                        end
                      else
                        result := DISP_E_BADPARAMCOUNT
                  else
                    result := DISP_E_MEMBERNOTFOUND;
              WaitForAnswerDispId .. ErrorCodeDispId, RemoteObjectId:
                if Flags and DISPATCH_PROPERTYGET <> 0 // reading the property
                  then
                    begin
                      if Parameters.cArgs = 0
                        then
                          if VarResult <> nil
                            then
                              case DispId of
                                WaitForAnswerDispId:
                                  PVariant(VarResult)^ := fWaitForAnswer;
                                TimeOutDispId:
                                  PVariant(VarResult)^ := fTimeOut;
                                PriorityDispId:
                                  PVariant(VarResult)^ := fPriority;
                                ErrorCodeDispId:
                                  PVariant(VarResult)^ := fErrorCode;
                                RemoteObjectId:
                                  PVariant(VarResult)^ := fObjectId;
                              end
                            else
                              result := E_INVALIDARG
                        else
                          result := DISP_E_BADPARAMCOUNT;
                    end
                  else
                    if Flags and DISPATCH_METHOD <> 0 // method call
                      then
                        result := DISP_E_MEMBERNOTFOUND
                      else // setting the property, must make certain by checking a few other things
                        if Parameters.cArgs = 1
                          then
                            if (Parameters.cNamedArgs = 1) and (Parameters.rgdispidNamedArgs[0] = DISPID_PROPERTYPUT)
                              then
                                case DispId of
                                  WaitForAnswerDispId:
                                    fWaitForAnswer := OleVariant(Parameters.rgvarg[0]);
                                  TimeOutDispId:
                                    fTimeOut := OleVariant(Parameters.rgvarg[0]);
                                  PriorityDispId:
                                    fPriority := OleVariant(Parameters.rgvarg[0]);
                                  ErrorCodeDispId:
                                    fErrorCode := OleVariant(Parameters.rgvarg[0])
                                end
                              else
                                result := DISP_E_PARAMNOTOPTIONAL
                          else
                            result := DISP_E_BADPARAMCOUNT;
              OnStartPageDispId, OnEndPageDispId: ;
              RemoteDispId:
                if fRDOConnection <> nil
                  then
                    begin
                      MemberName := PChar(TLSGetValue(MembNameTLSIdx));
                      try
                        Handled := false;
                        if (Flags and DISPATCH_PROPERTYGET <> 0) and (Parameters.cArgs = 0) // property get or call to a method with no args
                          then
                            begin
                              if VarResult <> nil
                                then
                                  begin
                                    Handled := true;
                                    RetValue := MarshalPropertyGet(fObjectId, string(MemberName), fRDOConnection, fTimeOut, fPriority, fErrorCode);
                                    result := MapQueryErrorToHResult(fErrorCode);
                                    if result = S_OK
                                      then PVariant(VarResult)^ := RetValue
                                  end
                                else
                                  if Flags and DISPATCH_METHOD = 0
                                    then
                                      begin
                                        Handled := true;
                                        result := E_INVALIDARG
                                      end
                            end;
                        if not Handled
                          then
                            if Flags and DISPATCH_METHOD <> 0 // method call
                              then
                                if Parameters.cNamedArgs =  0
                                  then
                                    begin
                                      if VarResult <> nil
                                        then TVarData(RetValue).VType := varVariant
                                        else RetValue := UnAssigned;

                                      if fWaitForAnswer or (VarResult <> nil)
                                        then MarshalMethodCall(fObjectId, string(MemberName), @Params, RetValue, fRDOConnection, fTimeOut, fPriority, fErrorCode)
                                        else MarshalMethodCall(fObjectId, string(MemberName), @Params, RetValue, fRDOConnection, 0, fPriority, fErrorCode);

                                      result := MapQueryErrorToHResult(fErrorCode);
                                      if (result = S_OK) and (VarResult <> nil)
                                        then PVariant(VarResult)^ := RetValue;
                                    end
                                  else result := DISP_E_NONAMEDARGS
                              else // property put but, must make certain by checking a few other things
                                if Parameters.cArgs = 1
                                  then
                                    if (Parameters.cNamedArgs = 1) and (Parameters.rgdispidNamedArgs[0] = DISPID_PROPERTYPUT)
                                      then
                                        begin
                                          if fWaitForAnswer
                                            then MarshalPropertySet(fObjectId, string(MemberName), OleVariant(Parameters.rgvarg[0]), fRDOConnection, fTimeOut, fPriority, fErrorCode)
                                            else MarshalPropertySet(fObjectId, string(MemberName), OleVariant(Parameters.rgvarg[0]), fRDOConnection, 0, fPriority, fErrorCode);
                                          result := MapQueryErrorToHResult(fErrorCode)
                                        end
                                      else result := DISP_E_PARAMNOTOPTIONAL
                                  else result := DISP_E_BADPARAMCOUNT;
                      finally
                        StrDispose(MemberName);
                        TLSSetValue(MembNameTLSIdx, nil)
                      end
                    end
                  else result := E_FAIL
            end
      {$IFDEF AutoServer}
          end
      {$ENDIF}
    end;

initialization
  MembNameTLSIdx := TLSAlloc;
  if MembNameTLSIdx = $FFFFFFFF
    then
      raise Exception.Create('Unable to use thread local storage');
  {$IFDEF AutoServer}
  TAutoObjectFactory.Create(ComServer, TRDOObjectProxy, Class_RDOObjectProxy, ciMultiInstance)
  {$ENDIF}
finalization
  if MembNameTLSIdx <> $FFFFFFFF
    then
      TLSFree(MembNameTLSIdx)
end.
