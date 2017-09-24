unit SMCheck;

interface

  uses
    ShareMem;

  const
    LeadingBytes  = 0;
    TrailingBytes = 0;

implementation

  uses
    SysUtils, windows;

  const
    MaskByte = $A5;

  var
    OldMemMgr : TMemoryManager;

  procedure PlaceLeadingMask(p : pointer);
    begin
      fillchar(p^, LeadingBytes, MaskByte);
    end;

  procedure PlaceTrailingMask(p : pointer);
    var
      ptrsz : integer;
    begin
      if TrailingBytes > 0
        then
          begin
            ptrsz := pinteger(integer(p) - 4)^ - 6;
            integer(p) := integer(p) + ptrsz - TrailingBytes;
            fillchar(p^, TrailingBytes, MaskByte);
          end;
    end;

  function SysGetMemCheck(size : integer): pointer;
    begin
      if size > 0
        then
          begin
            inc(size, LeadingBytes + TrailingBytes);
            result := OldMemMgr.GetMem(size);
            if result <> nil
              then
                begin
                  //PlaceLeadingMask(result);
                  //PlaceTrailingMask(result);
                  inc(integer(result), LeadingBytes);
                end;
          end
        else result := nil;
    end;

  function SysFreeMemCheck(p : Pointer): Integer;
    begin
      if p <> nil
        then
          begin
            dec(integer(p), LeadingBytes);
            result := OldMemMgr.FreeMem(p);
          end
        else result := 0;
    end;

  function SysReallocMemCheck(p : Pointer; size: integer) : pointer;
    begin
      if p <> nil
        then
          begin
            dec(integer(p), LeadingBytes);
            inc(size, LeadingBytes + TrailingBytes);
            result := OldMemMgr.ReallocMem(p, size);
            if result <> nil
              then
                begin
                  //PlaceLeadingMask(result);
                  //PlaceTrailingMask(result);
                  inc(integer(result), LeadingBytes);
                end;
          end
        else result := nil;
    end;

  procedure InitMemoryManager;
    var
      SharedMemoryManager : TMemoryManager;
    begin
      SharedMemoryManager.GetMem := SysGetMemCheck;
      SharedMemoryManager.FreeMem := SysFreeMemCheck;
      SharedMemoryManager.ReallocMem := SysReallocMemCheck;
      GetMemoryManager(OldMemMgr);
      SetMemoryManager(SharedMemoryManager);
    end;

initialization

  //InitMemoryManager;

finalization

  //SetMemoryManager(OldMemMgr);

end.
