unit FiveMem;

interface


implementation

  uses
    SysUtils;

  var
    YeOldeMM : TMemoryManager;
    NewMM    : TMemoryManager;

  const
    CushionSize = 8;

  var
    MemCushion : array[0..CushionSize-1] of byte = ($FA,$FA,$FA,$FA,$FA,$FA,$FA,$FA);

  function FIVEGetMem( Size : Integer ) : Pointer;
    begin
      {
      if Size > 0
        then
          begin
            result := YeOldeMM.GetMem( Size + CushionSize );
            move( MemCushion[0], PByteArray(result)^[Size], CushionSize );
          end
        else result := nil;
      }
      result := YeOldeMM.GetMem( Size );
    end;

  function FIVEFreeMem( P : Pointer ) : Integer;
    begin
      result := YeOldeMM.FreeMem( P );
    end;

  function FIVEReallocMem( P : Pointer; Size: Integer ) : Pointer;
    begin
      {
      if Size > 0
        then
          begin
            result := YeOldeMM.ReallocMem( P, Size + CushionSize );
            move( MemCushion[0], PByteArray(result)^[Size], CushionSize );
          end
        else result := YeOldeMM.ReallocMem( P, 0 )
      }
      result := YeOldeMM.ReallocMem( P, Size )
    end;

  procedure InitMemoryManager;
    begin
      GetMemoryManager( YeOldeMM );
      NewMM.GetMem := FIVEGetMem;
      NewMM.ReallocMem := FIVEReallocMem;
      NewMM.FreeMem := FIVEFreeMem;
      //SetMemoryManager( NewMM );
    end;

initialization

  InitMemoryManager;

end.
