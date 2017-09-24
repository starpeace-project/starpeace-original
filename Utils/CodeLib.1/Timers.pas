unit Timers;

interface

uses
  Windows, Classes, Messages, TimerTypes;

type
  TWindowsTicker =
    class(TInterfacedObject, ITicker)
      public
        constructor Create;
        destructor  Destroy;   override;
      private // ITicker
        function  GetEnabled : boolean;
        procedure SetEnabled(which : boolean);
        function  Count : integer;
        function  GetTickeable(idx : integer) : ITickeable;
        procedure Attach(const which : ITickeable);
        procedure Detach(const which : ITickeable);
      private
        fTickeables : TList;
        fHandle     : HWnd;
        fCreated    : boolean;
        fEnabled    : boolean;
        fInterval   : integer;
        fCurrent    : integer;
        procedure AdjustInterval;
        procedure CheckState;
        procedure TimerProc(var msg : TMessage);
        procedure CreateWndHandle(const which : array of const);
        procedure CloseWndHandle(const which : array of const);
    end;


implementation

uses
  Threads, Forms;  // >>>> Donot use forms


// TWindowsTicker

constructor TWindowsTicker.Create;
  begin
    inherited;
    Join(CreateWndHandle, [nil]);
    fTickeables := TList.Create;
  end;

destructor TWindowsTicker.Destroy;
  begin
    SetEnabled(false);
    assert(fTickeables.Count = 0);
    fTickeables.Free;
    Join(CloseWndHandle, [nil]);
    inherited;
  end;

function TWindowsTicker.GetEnabled : boolean;
  begin
    Result := fEnabled;
  end;

procedure TWindowsTicker.SetEnabled(which : boolean);
  begin
    if which <> fEnabled
      then
        begin
          fEnabled := which;
          CheckState;
        end;
  end;

function TWindowsTicker.Count : integer;
  begin
    Result := fTickeables.Count;
  end;

function TWindowsTicker.GetTickeable(idx : integer) : ITickeable;
  begin
    Result := ITickeable(fTickeables[idx]);
  end;

procedure TWindowsTicker.Attach(const which : ITickeable);
  begin
    assert(fTickeables.IndexOf(pointer(which)) = -1);
    which._AddRef;
    fTickeables.Add(pointer(which));
    AdjustInterval;
    CheckState;
  end;

procedure TWindowsTicker.Detach(const which : ITickeable);
  var
    idx : integer;
  begin
    idx := fTickeables.IndexOf(pointer(which));
    assert(idx <> -1);
    fTickeables.Delete(idx);
    which._Release;
    AdjustInterval;
    CheckState;
  end;

procedure TWindowsTicker.AdjustInterval;  // >>>> Generalize
  var
    aux : integer;
    i   : integer;
  begin
    assert(fTickeables.Count > 0);
    fInterval := ITickeable(fTickeables[0]).Interval;
    for i := 1 to pred(fTickeables.Count) do
      begin
        aux := ITickeable(fTickeables[i]).Interval;
        if aux < fInterval
          then
            begin
              assert(fInterval mod aux = 0, 'Wrong interval');
              fInterval := aux;
            end
          else assert(aux mod fInterval = 0, 'Wrong interval');
      end;
  end;

procedure TWindowsTicker.CheckState;
  const
    cResolution = 55; // >>>>
  var
    aux : boolean;
  begin
    aux := fEnabled and (fInterval > 0);
    if aux <> fCreated
      then
        if not fCreated
          then
            begin
              SetTimer(fHandle, integer(Self), fInterval, nil);
              fCreated := true;
              fCurrent := 0;
            end
          else
            begin
              KillTimer(fHandle, integer(Self));
              fCreated := false;
            end;
  end;

procedure TWindowsTicker.TimerProc(var msg : TMessage);
  var
    i   : integer;
    aux : ITickeable;
  begin
    inc(fCurrent, fInterval);
    for i := 0 to pred(fTickeables.Count) do
      begin
        aux := ITickeable(fTickeables[i]);
        if fCurrent mod aux.Interval = 0
          then aux.Tick(fInterval);
      end;
  end;

procedure TWindowsTicker.CreateWndHandle(const which : array of const);
 begin
   fHandle := AllocateHWnd(TimerProc);
 end;

procedure TWindowsTicker.CloseWndHandle(const which : array of const);
  begin
    DeallocateHWnd(fHandle);
  end;


end.
