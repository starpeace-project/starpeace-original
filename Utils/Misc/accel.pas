unit Accel;

{$BoolEval OFF}

interface

  uses
    Windows, Messages, Classes, Controls, Forms, TimerUtils, ExtCtrls;

  type
    EAccelError = class(EOutOfResources);

  const
    MaxEntryCount = 103*2*2;

  const
    DefEntryCount = 20;
    IncSize       = 16;

  type
    TNotifyCommandProc = procedure(Command : word) of object;

  type
    PAccelArray = ^TAccelArray;
    TAccelArray = array[0..Pred(MaxEntryCount)] of TAccel;

  type
    TKeyMapper =
      class
        protected
          fActive : boolean;
          fAccelArray : PAccelArray;
          fEntryCount : integer;
          fOnCommand : TNotifyCommandProc;
          Count : integer;
        public
          constructor Create;
          destructor Destroy; override;
        public
          procedure AddAccelerator(aKey : word; aFlags : byte; aCommand : word; aRepeat : boolean); virtual;
          procedure DelAccelerator(aKey : word; aFlags : byte);                  virtual;
          procedure Reset;                                                       virtual;
        public
          procedure InitMapping;   virtual;
          procedure PauseMapping;  virtual;
          procedure ResumeMapping; virtual;
          procedure DoneMapping;   virtual;
        protected
          procedure AllocTable;                     virtual;
          procedure ReallocTable;                   virtual;
          procedure NotifyCommand(aCommand : word); virtual;
        protected
          procedure SetActive(aActive : boolean);        virtual;
          function  GetAccel(index : integer) : TAccel;  virtual;
        public
          property Active : boolean read fActive write SetActive;
          property Accels[index : integer] : TAccel read GetAccel; default;
          property EntryCount : integer read fEntryCount;
          property OnCommand : TNotifyCommandProc read fOnCommand write fOnCommand;
      end;

  type
    TAccelTable =
      class(TKeyMapper)
        private
          fHandle : HAccel;
          fLoaded : boolean;
        public
          destructor  Destroy;  override;
        protected
          procedure AppHookProc(var Msg : TMsg; var Handled : boolean);
          function  HookProc(var aMsg : TMessage) : boolean;
        public
          procedure InitMapping; override;
          procedure DoneMapping; override;
          procedure Reset;       override;
        public
          property Handle : HAccel  read fHandle write fHandle;
          property Loaded : boolean read fLoaded;
      end;

  type
    TKeyInfo =
      record
        RepeatIt : boolean;
        Pressed : boolean;
        Retrace : integer;
      end;

  type
    PKeyInfoArray = ^TKeyInfoArray;
    TKeyInfoArray = array[0..Pred(MaxEntryCount)] of TKeyInfo;

  type
    TTimerKeyMapper =
      class(TKeyMapper)
        private
          fKeysInfo : PKeyInfoArray;
          SimTimer  : TTimer;
        public
          constructor Create;
          destructor  Destroy; override;
        public
          procedure AddAccelerator(aKey : word; aFlags : byte; aCommand : word; aRepeat : boolean); override;
        public
          procedure InitMapping; override;
          procedure DoneMapping; override;
          procedure Reset;       override;
          procedure AllKeysUp;
        protected
          procedure AppHookProc(var Msg : TMsg; var Handled : boolean);
          function  HookProc(var aMsg : TMessage) : boolean;
        protected
          procedure Timer( Sender : TObject );
          function  HookKeyDown(Msg : TWMKeyDown) : boolean;
          function  HookKeyUp(Msg : TWMKeyDown) : boolean;
          function  VerifyFlags(flags : word) : boolean;
        protected
          procedure AllocTable;                   override;
          procedure ReallocTable;                 override;
        public
          function  FirstIndexOf(aKey : word) : integer;
          function  NextIndexOf(aKey : word; CurrentIndex : integer) : integer;
      end;

  const
    KeyboardRate    = 50;
    KeyboardRetrace = 3;   

implementation

  uses
    SysUtils;

  // TKeyMapper

  constructor TKeyMapper.Create;
    begin
      inherited;
      Count := DefEntryCount;
      Alloctable;
    end;

  destructor TKeyMapper.Destroy;
    begin
      FreeMem(fAccelArray);
      inherited;
    end;

  procedure TKeyMapper.AddAccelerator(aKey : word; aFlags : byte; aCommand : word; aRepeat : boolean);
    begin
      if EntryCount = Count
        then ReallocTable;
      with fAccelArray[EntryCount] do
        begin
          FVirt := aFlags;
          Key := aKey;
          Cmd := aCommand;
        end;
      Inc(fEntryCount);
    end;

  procedure TKeyMapper.DelAccelerator(aKey : word; aFlags : byte);
    var
      i : integer;
    begin
      i := 0;
      while (i < EntryCount) and ( (aKey <> fAccelArray[i].Key) or (aFlags <> fAccelArray[i].FVirt)) do
        inc(i);
      if i < EntryCount
        then
          begin
            Dec(fEntryCount);
            Move(fAccelArray[Succ(i)], fAccelArray[i], EntryCount - i);
          end;
    end;

  procedure TKeyMapper.Reset;
    begin
      fEntryCount := 0;
      FreeMem(fAccelArray);
      Count := DefEntryCount;
      AllocTable;
    end;

  procedure TKeyMapper.InitMapping;
    begin
      Active := true;
    end;

  procedure TKeyMapper.PauseMapping;
    begin
      if Active
        then Active := false;
    end;

  procedure TKeyMapper.ResumeMapping;
    begin
      if not Active
        then Active := true;
    end;

  procedure TKeyMapper.DoneMapping;
    begin
      Active := false;
    end;

  procedure TKeyMapper.AllocTable;
    begin
      GetMem(fAccelArray, SizeOf(TAccel)*Count);
    end;

  procedure TKeyMapper.ReallocTable;
    begin
      Inc(Count, IncSize);
      ReallocMem(fAccelArray, Count);
    end;

  procedure TKeyMapper.NotifyCommand(aCommand : word);
    begin
      if Active and Assigned(fOncommand)
        then fOnCommand(aCommand);
    end;

  procedure TKeyMapper.SetActive(aActive : boolean);
    begin
      fActive := aActive;
    end;

  function  TKeyMapper.GetAccel(index : integer) : TAccel;
    begin
      Assert(((index >=0) and (index < EntryCount)), 'TAccelTable.GetAccel : Index Out of bounds');
      Result := fAccelArray[index];
    end;

  // TAccelTable

  destructor  TAccelTable.Destroy;
    begin
      DoneMapping;
      inherited;
    end;

  procedure TAccelTable.InitMapping;
    begin
      inherited;
      Application.OnMessage := AppHookProc;
      if (EntryCount > 0) and (not Loaded) and Assigned(fOnCommand)
        then
          begin
            fHandle := CreateAcceleratorTable(fAccelArray^, EntryCount);
            if Handle <> 0
              then
                begin
                  fLoaded := true;
                  Application.OnMessage := AppHookProc;
                  Application.HookMainWindow(HookProc);
                end
              else raise EAccelError.Create('Could not create accelerator table');
          end;
    end;

  procedure TAccelTable.DoneMapping;
    begin
      inherited;
      if Loaded and not LongBool(DestroyAcceleratorTable(Handle))
        then raise EAccelError.Create('Could not destroy accelerator table, Windows error : ' + IntToStr(GetLastError))
        else
          begin
            fLoaded := false;
            Application.OnMessage := nil;
            Application.UnhookMainWindow(HookProc);
          end;
    end;

  procedure TAccelTable.Reset;
    begin
      DoneMapping;
      fLoaded := false;
      inherited;
    end;

  procedure TAccelTable.AppHookProc(var Msg : TMsg; var Handled : boolean);
    begin
      if Active
        then Handled := longbool (TranslateAccelerator(Application.Handle, Handle, Msg))
        else Handled := false;
    end;

  function TAccelTable.HookProc(var aMsg : TMessage) : boolean;
    begin
      if aMsg.Msg = WM_COMMAND
        then
          begin
            NotifyCommand(aMsg.wParam);
            Result := true;
          end
        else Result := false;
    end;

  // TTimerKeyMapper

  constructor TTimerKeyMapper.Create;
    begin
      inherited;
      SimTimer := TTimer.Create( nil );
      SimTimer.Interval := 50;
      SimTimer.OnTimer := Timer;
    end;

  destructor  TTimerKeyMapper.Destroy;
    begin
      DoneMapping;
      SimTimer.Free;
      FreeMem(fKeysInfo);
      inherited;
    end;

  procedure TTimerKeyMapper.AddAccelerator(aKey : word; aFlags : byte; aCommand : word; aRepeat : boolean);
    begin
      inherited;
      with fKeysInfo[Pred(EntryCount)] do
        begin
          RepeatIt := aRepeat;
          Pressed := false;
          Retrace := 0;
        end;
    end;

  procedure TTimerKeyMapper.InitMapping;
    begin
      inherited;
      Application.OnMessage := AppHookProc;
      SimTimer.Enabled := true;
    end;

  procedure TTimerKeyMapper.DoneMapping;
    begin
      SimTimer.Enabled := false;
      Application.OnMessage := nil;
      inherited;
    end;

  procedure TTimerKeyMapper.Reset;
    begin
      inherited;
      SimTimer.Enabled := false;
      FreeMem(fKeysInfo);
      GetMem(fKeysInfo, SizeOf(TKeyInfo)*Count);
    end;

  procedure TTimerKeyMapper.AllKeysUp;
    var
      i : integer;
    begin
      for i := 0 to Pred(EntryCount) do
        if fKeysInfo[i].Pressed
          then fKeysInfo[i].Pressed := false;
    end;

  procedure TTimerKeyMapper.AppHookProc(var Msg : TMsg; var Handled : boolean);
    var
      TmpMsg : TMessage;
    begin
      TmpMsg.Msg := Msg.Message;
      TmpMsg.WParam := Msg.WParam;
      Handled := HookProc(TmpMsg);
    end;

  function  TTimerKeyMapper.HookProc(var aMsg : TMessage) : boolean;
    begin
      with aMsg do
        case Msg of
          WM_KeyDown :
            HookProc := HookKeyDown(TWMKeyDown(aMsg));
          WM_KeyUp :
            HookProc := HookKeyUp(TWMKeyDown(aMsg));
          else
            HookProc := false;
        end;
    end;

  procedure TTimerKeyMapper.Timer( Sender : TObject );
    var
      i : integer;
    begin
      if Active
        then
          for i := 0 to Pred(EntryCount) do
            if fKeysInfo[i].Pressed and fKeysInfo[i].RepeatIt
              then
                begin
                  if fKeysInfo[i].Retrace < KeyboardRetrace
                    then Inc(fKeysInfo[i].Retrace)
                    else
                      if VerifyFlags(fAccelArray[i].fVirt)
                        then NotifyCommand(fAccelArray[i].Cmd);
                end;
    end;

  function TTimerKeyMapper.HookKeyDown(Msg : TWMKeyDown) : boolean;
    var
      index : integer;
    begin
      if Active
        then
          begin
            index := FirstIndexOf(Msg.CharCode);
            if (index <> -1) and not fKeysInfo[index].Pressed
              then
                begin
                  Result := true;
                  repeat
                    fKeysInfo[index].Pressed := true;
                    fKeysInfo[index].Retrace := 0;
                    if Active and VerifyFlags(fAccelArray[index].fVirt)
                      then NotifyCommand(fAccelArray[index].Cmd);
                    index := NextIndexOf(Msg.CharCode, index);
                  until index = -1;
                end
              else Result := false;
          end
        else Result := false;
    end;

  function TTimerKeyMapper.HookKeyUp(Msg : TWMKeyDown) : boolean;
    var
      index : integer;
    begin
      Result := false;
      index := FirstIndexOf(Msg.CharCode);
      if index <> -1
        then
          begin
            repeat
              if fKeysInfo[index].Pressed
                then
                  begin
                    Result := true;
                    fKeysInfo[index].Pressed := false;
                  end;
              index := NextIndexOf(Msg.CharCode, index);
            until index = -1;
          end
    end;

  function  TTimerKeyMapper.VerifyFlags(flags : word) : boolean;
    begin
      Result := not(wordbool(flags and FCONTROL) xor bytebool(GetKeyState(VK_CONTROL) and 128)) and
                not(wordbool(flags and FALT)     xor bytebool(GetKeyState(VK_MENU)    and 128)) and
                not(wordbool(flags and FSHIFT)   xor bytebool(GetKeyState(VK_SHIFT)   and 128));
    end;

  procedure TTimerKeyMapper.AllocTable;
    begin
      inherited;
      GetMem(fKeysInfo, SizeOf(TKeyInfo)*Count);
    end;

  procedure TTimerKeyMapper.ReallocTable;
    begin
      inherited;
      ReallocMem(fKeysInfo, SizeOf(TKeyInfo)*Count);
    end;

  function  TTimerKeyMapper.FirstIndexOf(aKey : word) : integer;
    begin
      Result := NextIndexOf(aKey, -1);
    end;

  function  TTimerKeyMapper.NextIndexOf(aKey : word; CurrentIndex : integer) : integer;
    var
      i : integer;
    begin
      i := Succ(CurrentIndex);
      while (i < EntryCount) and (fAccelArray[i].key <> aKey) do
        inc(i);
      if i < EntryCount
        then Result := i
        else Result := -1;
    end;

end.
