unit WaveHdrs;

interface

  uses
    Windows, MMSystem;

  procedure DeallocateBuffer(var Buffer : TWaveHdr);
  procedure AllocateBuffer(var Buffer : TWaveHdr; Size : integer);

implementation

  procedure DeallocateBuffer(var Buffer : TWaveHdr);
    begin
      with Buffer do
        if lpData <> nil
          then
            begin
              freemem(lpData);
              lpData := nil;
            end;
    end;

  procedure AllocateBuffer(var Buffer : TWaveHdr; Size : integer);
    begin
      with Buffer do
        begin
          getmem(lpData, Size);
          fillchar(lpData^, Size, 0);
          dwBufferLength := Size;
          dwFlags := 0;
          dwLoops := 0;
          dwUser  := 0;
        end;
    end;

end.

