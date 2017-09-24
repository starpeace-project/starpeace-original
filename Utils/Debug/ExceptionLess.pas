unit ExceptionLess;  //.rag

interface

implementation
uses
  windows, sysutils, forms;
  
type
  TLongJump = 
    packed record
      OpCode: Byte;
      Offset: DWORD;
    end;

var
  AppShowExceptionAddr : Pointer;
  ShowErrorAddr        : Pointer;
  OldBytesAppShow      : array[0..4] of Byte;
  OldBytesShow         : array[0..4] of Byte;
  NewBytesAppShow      : TLongJump;
  NewBytesShow         : TLongJump;

function ReadMem(Address,Dest: Pointer; Size: Integer): Boolean;
  begin
    Result := False;
    if (Address <> nil) and (Dest <> nil) 
      then Result := ReadProcessMemory(GetCurrentProcess, Address, Dest, Size, DWORD(nil^));
  end;

function RealAddr(RawAddr: Pointer): Pointer;
  type
    TJmpDLL = 
      packed record
        OpCode: Word;
        Offset: LongWord;
      end;
  var
    J: TJmpDLL;
    D: LongWord;
  begin
    Result := nil;
    if ReadMem(RawAddr, @J, SizeOf(J)) 
      then
        if J.OpCode <> $25FF 
          then Result := RawAddr
          else
          if ReadMem(Pointer(J.Offset), @D, SizeOf(D)) 
            then Result := Pointer(D);
  end;

function WriteMem(Address,NewBytes: Pointer; Size: Integer): Boolean;
  begin
    Result := False;
    if (Address <> nil) and (NewBytes <> nil) 
      then Result := WriteProcessMemory(GetCurrentProcess, Address, NewBytes, Size, DWORD(nil^));
  end;
  
procedure TDSShowException(ExceptObject: TObject; Address: Pointer); register;
  begin
  end;

procedure TDSAppShowException(E: Exception); register;
  begin
  end;
  
procedure InitExceptionLess;
  begin
    ShowErrorAddr    := RealAddr(Addr(SysUtils.ShowException));
    ReadMem(ShowErrorAddr, @OldBytesShow, SizeOf(OldBytesShow));
    VirtualLock(ShowErrorAddr, SizeOf(NewBytesShow));

    NewBytesShow.OpCode := $E9;
    NewBytesShow.Offset := Longint(Addr(TDSShowException))-Longint(ShowErrorAddr)-5;

    AppShowExceptionAddr := RealAddr(Addr(Forms.TApplication.ShowException));
    ReadMem(AppShowExceptionAddr, @OldBytesAppShow, SizeOf(OldBytesAppShow));
    VirtualLock(AppShowExceptionAddr, SizeOf(NewBytesAppShow));
    
    NewBytesAppShow.OpCode := $E9;
    NewBytesAppShow.Offset := Longint(Addr(TDSAppShowException))-Longint(AppShowExceptionAddr)-5;

    WriteMem(ShowErrorAddr, @NewBytesShow, SizeOf(NewBytesShow));
    WriteMem(AppShowExceptionAddr, @NewBytesAppShow, SizeOf(NewBytesAppShow));
  end;

procedure EndExceptionLess;
  begin
    WriteMem(ShowErrorAddr, @OldBytesShow, SizeOf(OldBytesShow));
    WriteMem(AppShowExceptionAddr, @OldBytesAppShow, SizeOf(OldBytesAppShow));
  end;

  procedure _ExceptProc(ExceptObject: TObject; ExceptAddr: Pointer);
    begin
      // Windows.MessageBox(0, '1', '1', 1);
    end;

  procedure _ErrorProc(ErrorCode: Integer; ErrorAddr: Pointer);
    begin
      // Windows.MessageBox(0, '2', '2', 1);
    end;

  procedure _AssertErrorHandler(const Message, Filename: string; LineNumber: Integer; ErrorAddr: Pointer);
    begin
      // Windows.MessageBox(0, '3', '3', 1);
    end;

  procedure _AbstractErrorHandler;
    begin
      // Windows.MessageBox(0, '4', '4', 1);
    end;

initialization
  InitExceptionLess;
  
  ExceptProc := @_ExceptProc;
  ErrorProc  := @_ErrorProc;
  AssertErrorProc := @_AssertErrorHandler;
  AbstractErrorProc := @_AbstractErrorHandler;
finalization
  EndExceptionLess;
end.


