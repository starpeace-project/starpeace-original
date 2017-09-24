unit MiscUtils;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  function BufferEqual( Buf1, Buf2 : pointer; Count : integer) : boolean;

implementation

  function BufferEqual( Buf1, Buf2 : pointer; Count : integer) : boolean;
    asm
      push ebx

    @@Loop:
      mov  bl, [eax]
      cmp  bl, [edx]
      jne  @@Different
      inc  eax
      inc  edx
      dec  ecx
      jnz  @@Loop
      mov  eax, 1
      jmp  @@Exit

    @@Different:
      xor  eax, eax

    @@Exit:

      pop  ebx
    end;

end.

