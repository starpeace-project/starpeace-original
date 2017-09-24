unit SMCheck;

interface

  uses
    ShareMem;

implementation

  uses
    SysUtils;

  type
    PInteger = ^integer;

  const
    Magic = (ord('A') shl 24) or (ord('B') shl 16) or (ord('C') shl 8) or ord('D');

  var
    OldMemMgr: TMemoryManager;

  function SysGetMemCheck(Size: Integer): Pointer;
    var
      p : pchar;
    begin
      if Size > 0
        then
          begin
            inc(Size, 8);
            result := OldMemMgr.GetMem(size);
            if result <> nil
              then
                begin
                  PInteger(result)^ := Size;
                  p := pchar(result)+Size-4;
                  PInteger(p)^ := Magic;
                  inc(pchar(result), 4);
                end;
          end
        else result := nil;
    end;

  function SysFreeMemCheck(P: Pointer): Integer;
    var
      q : pchar;
    begin
      if p <> nil
        then
          begin
            dec(pchar(p), 4);
            q := pchar(p)+ PInteger(p)^-4;
            if PInteger(q)^= Magic
              then result := OldMemMgr.FreeMem(P)
              else raise Exception.Create(format('Memory corrupted detected, address: %x', [integer(p)]));
          end
        else result := 0;
    end;

  function SysReallocMemCheck(P: Pointer; Size: Integer): Pointer;
    var
      q : pchar;
    begin
      if p <> nil
        then
          begin
            dec(pchar(p), 4);
            q := pchar(p)+ PInteger(p)^-4;
            if PInteger(q)^=Magic
              then
                begin
                  inc(size, 8);
                  result := OldMemMgr.ReallocMem(P, size);
                end
              else raise Exception.Create(format('Memory corrupted detected, address: %x', [integer(p)]));
            if result <> nil
              then
                begin
                  PInteger(result)^ := Size;
                  q := pchar(result)+Size-4;
                  PInteger(q)^ := Magic;
                  inc(pchar(result), 4);
                end;
          end
        else result := SysGetMemCheck(Size);
    end;

  procedure InitMemoryManager;
    var
      SharedMemoryManager: TMemoryManager;
    begin
      SharedMemoryManager.GetMem := SysGetMemCheck;
      SharedMemoryManager.FreeMem := SysFreeMemCheck;
      SharedMemoryManager.ReallocMem := SysReallocMemCheck;
      GetMemoryManager(OldMemMgr);
      SetMemoryManager(SharedMemoryManager);
    end;

initialization

    InitMemoryManager;

finalization

    SetMemoryManager(OldMemMgr);

end.
