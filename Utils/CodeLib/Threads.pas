unit Threads;

interface

  uses
    Windows, SysUtils, messages, VCLUtils;  //AxlDebug, //.rag

  type
    TThreadMethod = procedure (const params : array of const) of object;
    TOnException  = procedure (e : Exception) of object;

  type
    TJoinMethod = procedure (const params : array of const) of object;
    TPostMethod = procedure of object;

  type
    TThreadPriority = (priIdle, priLowest, priLower, priNormal, priHigher, priHighest, priTimeCritical);
    TThreadState    = (tsRunning, tsSuspended, tsDefering, tsTerminated, tsGoingOut, tsFinished, tsDestroying);
    TThreadStates   = set of TThreadState;

  type
    TAxlThread =
      class
        public
          constructor Create(aPriority : TThreadPriority);
          destructor  Destroy;   override;
        private
          fHandle      : integer;
          fThreadID    : dword;
          fState       : TThreadStates;
          fFreeIfIdle  : boolean;
          fOnException : TOnException;
          function  GetSuspended  : boolean;
          function  GetTerminated : boolean;
          function  GetGoingOut   : boolean;
          function  GetPriority : TThreadPriority;
          procedure SetPriority(which : TThreadPriority);
          procedure SetSuspended(which : boolean);
        public
          property Handle      : integer         read fHandle;
          property ThreadID    : dword           read fThreadID;
          property Priority    : TThreadPriority read GetPriority  write SetPriority;
          property State       : TThreadStates   read fState;
          property Suspended   : boolean         read GetSuspended write SetSuspended;
          property FreeIfIdle  : boolean         read fFreeIfIdle  write fFreeIfIdle;
          property OnException : TOnException    read fOnException write fOnException;
        protected
          property Terminated : boolean read GetTerminated;
          property GoingOut   : boolean read GetGoingOut;
        public
          procedure Defer(const which : TThreadMethod; const params : array of const);
          procedure Suspend;
          procedure Resume;
          procedure Terminate;
          procedure GoOut;
          procedure WaitFor;
          function  GetExitCode : dword;
          procedure ClearMethodList;
        protected
          fMethods  : pointer;
          csMethods : TRTLCriticalSection;
          procedure AddMethod(const which : TThreadMethod; const params : array of const);  virtual; abstract;
        private
          fNext         : TAxlThread;
          fSuspendCount : integer;
          procedure AddToList;
          procedure RemoveFromList;
          procedure StartDefering;
          procedure StopDefering;
        private
          fDeferEvent : THandle;
        {$ifndef _NOASSERTIONS}
        public
          fDebugName : string;
        {$endif}
      end;

  type
    TFifoThread =
      class(TAxlThread)
        protected
          procedure AddMethod(const which : TThreadMethod; const params : array of const);  override;
      end;

  type
    TLifoThread =
      class(TAxlThread)
        protected
          procedure AddMethod(const which : TThreadMethod; const params : array of const);  override;
      end;

  type
    TExclusiveThread =
      class(TAxlThread)
        protected
          procedure AddMethod(const which : TThreadMethod; const params : array of const);  override;
      end;

  type
    IEnumThreads =
      interface
        function  Next(out which : array of TAxlThread) : integer;
        function  Skip(count : integer) : integer;
        procedure Reset;
      end;

  procedure BeginThreads;
  procedure EndThreads;

  procedure Join(const which : TJoinMethod; const params : array of const);
  procedure Post(const which : TPostMethod);

  procedure Fork(const which : TThreadMethod; aPriority : TThreadPriority; const params : array of const);
  procedure Defer(const which : TThreadMethod; aPriority : TThreadPriority; const params : array of const);

  function Threaded    : boolean;
  function ThisThread  : TAxlThread;
  function EnumThreads : IEnumThreads;

  procedure TerminateThisThread;
  function  ThreadIsTerminated : boolean;

  procedure FreeAllThreads;

  procedure VoidJoins;

  var
    axlThreadCount : integer = 0;

implementation

  // Utils

  type
    PArrayOfConst = ^TArrayOfConst;
    TArrayOfConst = array[0..0] of TVarRec;

  procedure StrAddRef(const which : string);
    var
      aux : pointer;
    begin
      aux := nil;
      string(aux) := which;
    end;

  procedure WideStrAddRef(const which : widestring);
    var
      aux : pointer;
    begin
      aux := nil;
      widestring(aux) := which;
    end;

  procedure StrRelease(which : pointer);
    begin
      string(which) := '';
    end;

  procedure WideStrRelease(which : pointer);
    begin
      widestring(which) := '';
    end;

  function CloneArrayOfConst(const which : array of const) : PArrayOfConst;
    var
      i    : integer;
      size : integer;
      p    : pchar;

    procedure Clone(var which : pointer; size : integer);
      begin
        move(which^, p^, size);
        which := p;
        inc(p, size);
      end;

    begin
      size := sizeof(which);
      for i := low(which) to high(which) do
        with which[i] do
          case VType of
            vtExtended : inc(size, sizeof(VExtended^));
            vtString   : inc(size, succ(length(VString^)));
            vtCurrency : inc(size, sizeof(VCurrency^));
          end;
      getmem(Result, size);
      move(which, Result^, sizeof(which));
      p := pchar(Result) + sizeof(which);
      for i := low(which) to high(which) do
        with Result[i] do
          case VType of
            vtExtended   : Clone(pointer(VExtended), sizeof(VExtended^));
            vtString     : Clone(pointer(VString), succ(length(VString^)));
            vtAnsiString : StrAddRef(string(VAnsiString));
            vtCurrency   : Clone(pointer(VCurrency), sizeof(VCurrency^));
            vtInterface  : IUnknown(VInterface)._AddRef;
            vtWideString : WideStrAddRef(widestring(VWideString));
          end;
    end;

  procedure FreeArrayOfConst(which : PArrayOfConst; count : integer);
    var
      i : integer;
    begin
      for i := 0 to pred(count) do
        with which[i] do
          case VType of
            vtAnsiString : StrRelease(VAnsiString);
            vtInterface  : //IUnknown(VInterface)._Release;  //.rag
                         IUnknown(VInterface) := nil;    //.rag
            vtWideString : WideStrRelease(VWideString);
          end;
      freemem(which);
    end;

  // TAxlThread

  const
    Priorities : array [TThreadPriority] of integer =
                   (
                     THREAD_PRIORITY_IDLE,
                     THREAD_PRIORITY_LOWEST,
                     THREAD_PRIORITY_BELOW_NORMAL,
                     THREAD_PRIORITY_NORMAL,
                     THREAD_PRIORITY_ABOVE_NORMAL,
                     THREAD_PRIORITY_HIGHEST,
                     THREAD_PRIORITY_TIME_CRITICAL
                   );

  type
    PThreadMethodInfo = ^TThreadMethodInfo;
    TThreadMethodInfo =
      record
        Method   : TThreadMethod;
        Params   : PArrayOfConst;
        ParCount : integer;
        Next     : PThreadMethodInfo;
      end;

  type
    TEnumThreads =
      class(TInterfacedObject, IEnumThreads)
        public
          constructor Create;
        private
          function  Next(out which : array of TAxlThread) : integer;
          function  Skip(count : integer) : integer;
          procedure Reset;
        private
          fCurrent : TAxlThread;
      end;

  threadvar
    vThisThread : TAxlThread;

  var
    MainThreadId : THandle;
    LastThread   : TAxlThread = nil;
    csThreadList : TRTLCriticalSection;
    DeferThreads : array[TThreadPriority] of TAxlThread = (nil, nil, nil, nil, nil, nil, nil);
    csDeferCache : TRTLCriticalSection;
    vJoinsVoided : boolean = false;

  function ThreadProc(thread : TAxlThread) : integer;   forward;

  constructor TAxlThread.Create(aPriority : TThreadPriority);
    begin
      inherited Create;
      AddToList;
      fDeferEvent := CreateEvent(nil, true, false, nil);
      //include(fState, tsDefering);
      InitializeCriticalSection(Self.csMethods);
      //fHandle  := BeginThread(nil, 0, @ThreadProc, pointer(Self), CREATE_SUSPENDED, fThreadID);
      fHandle  := BeginThread(nil, 0, @ThreadProc, pointer(Self), 0, fThreadID);
      Priority := aPriority;
      inc(axlThreadCount);
    end;

  destructor TAxlThread.Destroy;
    begin
      try
        ClearMethodList;
        Terminate;
        dec(axlThreadCount);
        include(fState, tsDestroying);
        GoOut;
        if not (tsFinished in fState)
          then
            begin
              assert(Self <> vThisThread);
              while (tsSuspended in fState) do
                Resume;
              if tsDefering in fState
                then StopDefering;
              WaitFor;
            end;
        DeleteCriticalSection(Self.csMethods);
        if fHandle <> 0
          then CloseHandle(fHandle);
        CloseHandle(fDeferEvent);
        RemoveFromList;
      except
        {$ifopt d+}
          outputdebugstring('TAxlThread.Destroy');
        {$endif}
      end;
      inherited;
    end;

  function TAxlThread.GetSuspended  : boolean;
    begin
      Result := tsSuspended in fState;
    end;

  function TAxlThread.GetTerminated : boolean;
    begin
      Result := tsTerminated in fState;
    end;

  function TAxlThread.GetGoingOut : boolean;
    begin
      Result := tsGoingOut in fState;
    end;

  function TAxlThread.GetPriority : TThreadPriority;
    var
      p : integer;
      i : TThreadPriority;
    begin
      p := GetThreadPriority(fHandle);
      i := low(TThreadPriority);
      while (i < high(TThreadPriority)) and (Priorities[i] <> p) do
        inc(i);
      if Priorities[i] = p
        then Result := i
        else Result := priNormal;
    end;

  procedure TAxlThread.SetPriority(which : TThreadPriority);
    begin
      SetThreadPriority(fHandle, Priorities[which]);
    end;

  procedure TAxlThread.SetSuspended(which : boolean);
    begin
      if which <> Suspended
        then
          if which
            then Suspend
            else Resume;
    end;

  procedure TAxlThread.Defer(const which : TThreadMethod; const params : array of const);
    begin
      if fState*[tsFinished, tsGoingOut] = []
        then
          begin
            AddMethod(which, params);
            if tsDefering in fState
              then StopDefering;
          end
        else // >>>> Exception?
    end;

  procedure TAxlThread.Suspend;
    begin
      inc(fSuspendCount);
      include(fState, tsSuspended);
      SuspendThread(fHandle);
    end;

  procedure TAxlThread.Resume;
    begin
      assert(tsSuspended in fState);
      assert(fSuspendCount > 0);
      dec(fSuspendCount);
      ResumeThread(fHandle);
      if fSuspendCount = 0
        then exclude(fState, tsSuspended);
    end;

  procedure TAxlThread.Terminate;
    begin
      include(fState, tsTerminated);
    end;

  procedure TAxlThread.GoOut;
    begin
      include(fState, tsGoingOut);
    end;

  procedure TAxlThread.WaitFor;
    var
      msg : TMsg;
    begin
      //if vThisThread = nil // Main Thread
      if GetCurrentThreadId = MainThreadId // Main Thread
        then
          while MsgWaitForMultipleObjects(1, fHandle, false, INFINITE, QS_SENDMESSAGE) = WAIT_OBJECT_0 + 1 do
            PeekMessage(msg, 0, 0, 0, PM_NOREMOVE)
        else WaitForSingleObject(fHandle, INFINITE);
    end;

  function TAxlThread.GetExitCode : dword;
    begin
      GetExitCodeThread(fHandle, Result);
    end;

  procedure TAxlThread.ClearMethodList;
    var
      info : PThreadMethodInfo;
      aux  : PThreadMethodInfo;
    begin
      EnterCriticalSection(csMethods);
      try
        if fMethods <> nil
          then
            begin
              aux := fMethods;
              while aux.Next <> nil do
                begin
                  info := aux;
                  aux := aux.Next;
                  FreeArrayOfConst(info.Params, info.ParCount);
                  dispose(info);
                end;
            end;
      finally
        fMethods := nil;
        LeaveCriticalSection(csMethods);
      end;
    end;

  procedure TAxlThread.AddToList;
    begin
      if axlThreadCount = 0
        then InitializeCriticalSection(csThreadList);
      BeginThreads;
      EnterCriticalSection(csThreadList);
      try
        fNext := LastThread;
        LastThread := Self;
      finally
        LeaveCriticalSection(csThreadList);
      end;
    end;

  procedure TAxlThread.RemoveFromList;
    var
      aux : TAxlThread;
    begin
      EnterCriticalSection(csThreadList);
      try
        assert(LastThread <> nil);
        if LastThread = Self
          then LastThread := fNext
          else
            begin
              aux := LastThread;
              while aux.fNext <> Self do
                begin
                  aux := aux.fNext;
                  assert(aux <> nil);
                end;
              aux.fNext := fNext;
            end;
      finally
        LeaveCriticalSection(csThreadList);
        EndThreads;
        if axlThreadCount = 0
          then DeleteCriticalSection(csThreadList);
      end;
    end;

  procedure TAxlThread.StartDefering;
    begin
      assert(not (tsDefering in fState));
      ResetEvent(fDeferEvent);
      include(fState, tsDefering);
      WaitForSingleObject(fDeferEvent, INFINITE);
      exclude(fState, tsDefering);
    end;

  procedure TAxlThread.StopDefering;
    begin
      assert(tsDefering in fState);
      SetEvent(fDeferEvent);
    end;

  procedure TFifoThread.AddMethod(const which : TThreadMethod; const params : array of const);
    var
      info : PThreadMethodInfo;
      aux  : PThreadMethodInfo;
    begin
      new(info);
      info.Method   := which;
      info.Params   := CloneArrayOfConst(params);
      info.ParCount := succ(high(params));
      info.Next     := nil;
      EnterCriticalSection(csMethods);
      try
        if fMethods = nil
          then fMethods := info
          else
            begin
              aux := fMethods;
              while aux.Next <> nil do
                aux := aux.Next;
              aux.Next := info;
            end;
      finally
        LeaveCriticalSection(csMethods);
      end;
    end;

  procedure TLifoThread.AddMethod(const which : TThreadMethod; const params : array of const);
    var
      info : PThreadMethodInfo;
    begin
      new(info);
      info.Method   := which;
      info.Params   := CloneArrayOfConst(params);
      info.ParCount := succ(high(params));
      EnterCriticalSection(csMethods);
      try
        info.Next := fMethods;
        fMethods  := info;
      finally
        LeaveCriticalSection(csMethods);
      end;
    end;

  procedure TExclusiveThread.AddMethod(const which : TThreadMethod; const params : array of const);
    var
      info : PThreadMethodInfo;
    begin
      Terminate;
      new(info);
      info.Method   := which;
      info.Params   := CloneArrayOfConst(params);
      info.ParCount := succ(high(params));
      info.Next     := nil;
      EnterCriticalSection(csMethods);
      try
        fMethods := info;
      finally
        LeaveCriticalSection(csMethods);
      end;
    end;

  // Joins

  const
    CM_EXECPROC      = $8FFF;
    CM_POSTPROC      = $8FFE;
    CM_DESTROYWINDOW = $8FFD;

  type
    TJoinOpenArray =
      record
        Data  : ^TArrayOfConst;
        Count : integer;
      end;

  var
    joinWindow : HWND    = 0;
    joinCount  : integer = 0;

  function joinWindowProc(wnd : HWND; msg, wParam, lParam : integer) : integer;   stdcall;   forward;

  var
    joinWindowClass : TWndClass =
      (
        style         : 0;
        lpfnWndProc   : @joinWindowProc;
        cbClsExtra    : 0;
        cbWndExtra    : 0;
        hInstance     : 0;
        hIcon         : 0;
        hCursor       : 0;
        hbrBackground : 0;
        lpszMenuName  : nil;
        lpszClassName : 'MerchiseJoinsWindow'
      );

  procedure BeginThreads;
    var
      TempClass  : TWndClass;
      Registered : boolean;
    begin
      inc(joinCount);
      if joinCount = 1
        then
          begin
            joinWindowClass.hInstance := hInstance;
            Registered := GetClassInfo(hInstance, joinWindowClass.lpszClassName, TempClass);
            if not Registered or (TempClass.lpfnWndProc <> @joinWindowProc)
              then
                begin
                  if Registered
                    then Windows.UnregisterClass(joinWindowClass.lpszClassName, hInstance);
                  Windows.RegisterClass(joinWindowClass);
                end;
            joinWindow := CreateWindow(joinWindowClass.lpszClassName, '', 0, 0, 0, 0, 0, 0, 0, hInstance, nil);
          end;
    end;

  procedure EndThreads;
    begin
      dec(joinCount);
      if joinCount = 0
        then PostMessage(joinWindow, CM_DESTROYWINDOW, 0, 0); // DeallocateWindow;
    end;

  procedure Join(const which : TJoinMethod; const params : array of const);
    var
      lException : TObject;
      ParamsData : TJoinOpenArray;
    begin
      if MainThreadId = GetCurrentThreadID
        then which(params)
        else
          begin
            ParamsData.Data  := @params;
            ParamsData.Count := succ(high(params));
            lException  := TObject(SendMessage(joinWindow, CM_EXECPROC, integer(@@which), integer(@ParamsData)));
            if lException <> nil
              then raise lException;
          end;
    end;

  procedure Post(const which : TPostMethod);
    begin
      PostMessage(joinWindow, CM_POSTPROC, integer(TMethod(which).Data), integer(TMethod(which).Code));
    end;

  function joinWindowProc(wnd : HWND; msg, wParam, lParam : integer) : integer;
    type
      TRaiseFrame =
        record
          NextRaise       : pointer; // ^TRaiseFrame;
          ExceptAddr      : Pointer;
          ExceptObject    : TObject;
          ExceptionRecord : PExceptionRecord;
        end;
    var
      posted : TMethod;
    begin
      case msg of
        CM_EXECPROC :
          begin
            Result := 0;
            if not vJoinsVoided
              then
                begin
                  try
                    with TJoinOpenArray(pointer(lParam)^) do
                      TJoinMethod(pointer(wParam)^)(Slice(Data^, Count));
                  except
                    if RaiseList <> nil
                      then
                        with TRaiseFrame(RaiseList^) do
                          begin
                            Result := integer(ExceptObject);
                            ExceptObject := nil;
                          end;
                  end;
                end;
          end;
        CM_POSTPROC :
          begin
            if not vJoinsVoided
              then
                begin
                  posted.Data := pointer(wparam);
                  posted.Code := pointer(lparam);
                  TPostMethod(posted)();
                end;
            Result := 0;
          end;
        CM_DESTROYWINDOW :
          begin
            DestroyWindow(wnd);
            Result := 0;
          end;
        else Result := DefWindowProc(wnd, msg, wParam, lParam);
      end;
    end;

  procedure Fork(const which : TThreadMethod; aPriority : TThreadPriority; const params : array of const);
    begin
      with TExclusiveThread.Create(aPriority) do
        begin
          FreeIfIdle := true;
          Defer(which, params);
        end;
    end;

  procedure Defer(const which : TThreadMethod; aPriority : TThreadPriority; const params : array of const);
    begin
      EnterCriticalSection(csDeferCache);
      try
        if DeferThreads[aPriority] = nil
          then DeferThreads[aPriority] := TFifoThread.Create(aPriority);
      finally
        LeaveCriticalSection(csDeferCache);
      end;
      DeferThreads[aPriority].Defer(which, params);
    end;

  function Threaded : boolean;
    begin
      Result := (vThisThread <> nil);
    end;

  function ThisThread : TAxlThread;
    begin
      Result := vThisThread;
    end;

  function EnumThreads : IEnumThreads;
    begin
      Result := TEnumThreads.Create;
    end;

  procedure TerminateThisThread;
    begin
      if vThisThread <> nil
        then vThisThread.Terminate;
    end;

  function ThreadIsTerminated : boolean;
    var
      aux : TAxlThread;
    begin
      aux := vThisThread;
      if aux <> nil
        then Result := tsTerminated in aux.fState
        else Result := false;
    end;

  procedure FreeAllThreads;
    var
      CurThread : TAxlThread;
    begin
      CurThread := LastThread;
      while CurThread <> nil do
        begin
          CurThread.Free;
          CurThread := LastThread;
        end;
    end;

  procedure VoidJoins;
    begin
      vJoinsVoided := true;
    end;

  // TEnumThreads

  constructor TEnumThreads.Create;
    begin
      inherited;
      Reset;
    end;

  function TEnumThreads.Next(out which : array of TAxlThread) : integer;
    begin
      Result := 0;
      EnterCriticalSection(csThreadList);
      try
        while (fCurrent <> nil) and (Result <= high(which)) do
          begin
            which[Result] := fCurrent;
            fCurrent := fCurrent.fNext;
            inc(Result);
          end;
      finally
        LeaveCriticalSection(csThreadList);
      end;
    end;

  function TEnumThreads.Skip(count : integer) : integer;
    begin
      Result := 0;
      EnterCriticalSection(csThreadList);
      try
        while (fCurrent <> nil) and (Result < count) do
          begin
            fCurrent := fCurrent.fNext;
            inc(Result);
          end;
      finally
        LeaveCriticalSection(csThreadList);
      end;
    end;

  procedure TEnumThreads.Reset;
    begin
      fCurrent := LastThread;
    end;

  // Backing routines

  procedure ExecuteMethod(thread : TAxlThread; info : PThreadMethodInfo);
    begin
      include(thread.fState, tsRunning);
      try
        with info^ do
          Method(Slice(Params^, ParCount));
      except
        on e : Exception do
          if assigned(thread.fOnException)
            then
              try
                Join(TJoinMethod(thread.fOnException), Slice(PArrayOfConst(e)^, 1));
              except
              end
            else // >>>>> SysUtils.ShowException(e, ExceptAddr);
      end;
      exclude(thread.fState, tsRunning);
      FreeArrayOfConst(info.Params, info.ParCount);
      dispose(info);
    end;

  function ThreadProc(thread : TAxlThread) : integer;
    var
      current : PThreadMethodInfo;
    begin
      vThisThread := thread;
      with thread do
        try
          while not (tsGoingOut in fState) and (not fFreeIfIdle or (fMethods <> nil)) do
            begin
              if fMethods <> nil
                then
                  begin
                    EnterCriticalSection(csMethods);
                    current  := fMethods;
                    fMethods := PThreadMethodInfo(fMethods).Next;
                    LeaveCriticalSection(csMethods);
                    ExecuteMethod(thread, current);
                  end;
              if not (tsGoingOut in fState) and (fMethods = nil) and not fFreeIfIdle
                then StartDefering;
            end;
          include(fState, tsFinished);
          if not (tsDestroying in fState)
            then Free;
        except
        end;
      EndThread(0);
      Result := 0;
    end;
(* //.rag
  function GetThreadNameList : string;  // Debug purposes
    var
      Threads : IEnumThreads;
      aux     : TAxlThread;
    begin
      Threads := EnumThreads;
      Result  := '';
      while Threads.Next(aux) > 0 do
        if Debugging
          then Result := Result + aux.fDebugName + '; '
          else Result := Result + aux.ClassName  + '; ';
    end;
*)
initialization
  InitializeCriticalSection(csDeferCache);
  MainThreadId := GetCurrentThreadID;
finalization
  try
    {$ifOpt D-}
       halt(0); //.rag  // para salida rapida
    {$EndIf}
    FreeAllThreads;
    // assert(LastThread = nil, GetThreadNameList);  //.rag
  except
  end;
  DeleteCriticalSection(csDeferCache);
end.

