unit CodingUtils;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  procedure XorByteCode( var Buffer; Code : byte; Count : cardinal );

implementation

  procedure XorByteCode( var Buffer; Code : byte; Count : cardinal );
    asm
    @@Loop:
      xor       [eax].byte, dl
      inc       eax
      dec       ecx
      jnz       @@Loop

    @@Exit:
    end;

end.
