unit FlicPlayback;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  uses
    GDI, Flics, Windows;

  // For extension purposes:

  type
    TChunkPlayerProc = function( Chunk : pointer; const FliSize : TPoint;
                                 Dest : pointer; DestWidth : integer ) : pointer;

  // Player

  procedure PlayFrame( Frame : PFliFrame; const FliSize : TPoint; Dest : pointer; DestWidth : integer;
                       UnkChunkPlayer : TChunkPlayerProc);

  function PlayChunks( FirstChunk : pointer; const FliSize : TPoint; Dest : pointer; DestWidth : integer;
                       Count : integer; UnkChunkPlayer : TChunkPlayerProc) : pointer;

  procedure GetUpdatedBounds( Frame : PFliFrame; const FliSize : TPoint; var Rect : TRect );

  // Other Flic related stuff
                       
  function CreatePaletteChunk( ChangedColors : ColorSet; var RgbQuads ) : pointer;
  
implementation

  procedure PlayFrame( Frame : PFliFrame; const FliSize : TPoint; Dest : pointer; DestWidth : integer;
                       UnkChunkPlayer : TChunkPlayerProc);
    begin
      try
        PlayChunks( pchar(Frame) + sizeof(TFliFrame), FliSize, Dest, DestWidth, Frame.Chunks, UnkChunkPlayer );
      except // Avoid any exception due to an invalid chunk
      end;
    end;

  // Chunk Players

  procedure GetUpdatedBounds( Frame : PFliFrame; const FliSize : TPoint; var Rect : TRect );
    begin
      // !!
    end;

  {$WARNINGS OFF}
  function PlayChunks( FirstChunk : pointer; const FliSize : TPoint; Dest : pointer; DestWidth : integer;
                       Count : integer; UnkChunkPlayer : TChunkPlayerProc) : pointer;
    const
      idLastKnownChunk = 18;
    var
      SaveEAX : integer;
      SaveECX : integer;
      SaveEDX : integer;
      SaveEBX : integer;
      SaveESI : integer;
      SaveEDI : integer;
      TempVar : integer;
    var
      LastPixel : word;
    label
      JumpTable;
    asm
      // EAX = FirstChunk, EDX = @FliSize, ECX = Dest

      cmp     Count, 0
      je      @Exit

      mov     SaveEAX, eax
      mov     SaveEDX, edx
      mov     SaveECX, ecx
      mov     SaveEBX, ebx
      mov     SaveESI, esi
      mov     SaveEDI, edi

    @PlayChunk:
      xor     ebx, ebx
      mov     bx, TFliChunkGeneric([eax]).Magic
      cmp     ebx, idLastKnownChunk
      ja      @PlayUnknown
      jmp     dword ptr JumpTable[ebx * 4]

    // ---------------------------------------------------------------------------------------------

    @PlayBlack:               // Uses esi, edi, eax, ebx, ecx, edx
      mov     edi, SaveECX
      mov     edx, SaveEDX

      mov     ecx, TPoint(edx).X            // FliSize.X
      mov     edx, TPoint(edx).Y            // FliSize.Y
      sub     DestWidth, ecx
      mov     ebx, ecx
      xor     eax, eax

      shr     ecx, 2
      and     ebx, 3
      mov     TempVar, ecx

    @@BlackLine:
      mov     ecx, TempVar
      rep     stosd
      mov     ecx, ebx
      rep     stosb
      add     edi, DestWidth
      dec     edx
      jnz     @@BlackLine
      jmp     @NextChunk

    // ---------------------------------------------------------------------------------------------

    @PlayLC:   // This chunk is found only in 320x200 FLIs
               // Uses esi, edi, eax, ebx, ecx, edx
      mov     esi, SaveEAX
      mov     edi, SaveECX
      xor     eax, eax
      add     esi, type TFliChunkGeneric

      lodsw                                   // Lines to skip
      mul     DestWidth
      add     edi, eax
      xor     eax, eax
      lodsw                                   // Line count
      mov     edx, eax

    @@LCLine:
      push    edi
      lodsb
      mov     ebx, eax                        // Packet count
      or      eax, eax
      jz      @@LCLineFinished

    @@LCPacket:                               // Process each packet
      lodsb                                   // Columns to skip
      add     edi, eax

      lodsb                                   // Type/Size byte
      test    al, al
      js      @@LCFill

    @@LCMove:
      mov     ecx, eax
      and     eax, 3                        //
      shr     ecx, 2                        //
      rep     movsd                         //
      mov     ecx, eax                      //
      rep     movsb

      dec     ebx
      jnz     @@LCPacket
      jmp     @@LCLineFinished

    @@LCFill:
      neg     al
      mov     ecx, eax

      lodsb                                   // value to repeat in al
      shr     ecx, 1
      mov     ah, al
      rep     stosw
      adc     cl, cl
      rep     stosb
      dec     ebx
      mov     ah, 0
      jnz     @@LCPacket

    @@LCLineFinished:
      pop     edi
      add     edi, DestWidth
      dec     edx
      jnz     @@LCLine
      jmp     @NextChunk

    // ---------------------------------------------------------------------------------------------

    @PlaySS2:  // Uses esi, edi, eax, ebx, ecx, edx
      xor     ecx, ecx
      xor     eax, eax
      mov     esi, SaveEAX
      mov     LastPixel, cx
      add     esi, type TFliChunkGeneric

      lodsw                         // Line count
      mov     edi, SaveECX
      mov     TempVar, eax

    @@SS2Line:
      lodsw
      test    ah, $40
      jnz     @@SS2SkipLines

    @@SS2OddWidth:
      push    edi

      test    ah, $80
      jz      @@SS2PacketCount

      mov     LastPixel, ax
      lodsw                         // The packet count always follows this word
      or      eax, eax
      jz      @@SS2LastPixel

    @@SS2PacketCount:
      mov     ebx, eax
      xor     eax, eax

    @@SS2Packet:                    // Process each packet
      mov     cl, [esi + 0]
      mov     al, [esi + 1]
      add     edi, ecx
      add     esi, 2
      test    al, al
      js      @@SS2Fill

    @@SS2Move:
      mov     ecx, eax
      shr     ecx, 1
      and     eax, 1
      rep     movsd
      mov     ecx, eax
      rep     movsw

      dec     ebx
      jnz     @@SS2Packet

    @@SS2LineFinished:
      mov     ax, LastPixel         // If AH = $80, we have to copy the last pixel (odd width flic)
      or      ah, ah
      jz      @@SS2Cont

    @@SS2LastPixel:
      stosb

    @@SS2Cont:
      pop     edi
      add     edi, DestWidth
      dec     TempVar
      jnz     @@SS2Line
      jmp     @NextChunk

    @@SS2SkipLines:
      neg     ax
      mul     DestWidth
      add     edi, eax
      xor     eax, eax
      jmp     @@SS2Line

    @@SS2Fill:
      neg     al
      mov     ecx, eax

      mov     ax, [esi]             // value to repeat in AX
      shl     eax, 16
      lodsw
      shr     ecx, 1
      rep     stosd
      adc     cl, cl
      rep     stosw

      xor     eax, eax
      dec     ebx
      jnz     @@SS2Packet
      jmp     @@SS2LineFinished

    // ---------------------------------------------------------------------------------------------

    @PlayBRun:
      mov     esi, SaveEAX
      mov     edi, SaveECX
      mov     edx, SaveEDX
      add     esi, type TFliChunkGeneric

      xor     eax, eax
      mov     ecx, TPoint(edx).X    // FliSize.X
      mov     edx, TPoint(edx).Y    // FliSize.Y
      sub     DestWidth, ecx
      mov     TempVar, ecx

    @@BRunLine:
      inc     esi                   // Skip packet count byte TLinePacket.Count
      mov     ebx, TempVar

    @@BRunLoop:
      lodsb                         // Type/Size byte
      test    al, al
      js      @@BRunMove

    @@BRunFill:
      mov     ecx, eax
      sub     ebx, ecx
      lodsb
      shr     ecx, 1
      mov     ah, al
      rep     stosw
      adc     cl, cl
      rep     stosb
      or      ebx, ebx
      mov     ah, 0
      jnz     @@BRunLoop
      jmp     @@BRunPacketDone

    @@BRunMove:
      neg     al
      mov     ecx, eax
      sub     ebx, eax

      and     ax, 3                 //
      shr     ecx, 2                //
      rep     movsd                 //
      mov     ecx, eax              //
      rep     movsb

      or      ebx, ebx
      jnz     @@BRunLoop

    @@BRunPacketDone:
      add     edi, DestWidth
      dec     dx
      jnz     @@BRunLine
      jmp     @NextChunk

    // ---------------------------------------------------------------------------------------------

    @PlayCopy:
      mov     esi, SaveEAX
      mov     edi, SaveECX
      mov     edx, SaveEDX
      add     esi, type TFliChunkGeneric

      mov     ecx, TPoint(edx).X    // FliSize.X
      mov     edx, TPoint(edx).Y    // FliSize.Y
      mov     ebx, ecx
      sub     DestWidth, ecx
      shr     ecx, 2
      and     ebx, 3
      mov     TempVar, ecx

    @@CopyLine:
      mov     ecx, TempVar
      rep     movsd
      mov     ecx, ebx
      rep     movsb
      add     edi, DestWidth
      dec     edx
      jnz     @@CopyLine
      jmp     @NextChunk

    // ---------------------------------------------------------------------------------------------

    @PlayUnknown:
      cmp     UnkChunkPlayer, 0
      jz      @NextChunk

      mov     eax, SaveEAX
      mov     edx, SaveEDX
      mov     ecx, SaveECX
      mov     ebx, SaveEBX
      mov     esi, SaveESI
      mov     edi, SaveEDI
                                    // Chunk, FliSize and Dest are already passsed in EAX,EDX,ECX
      push    DestWidth             // Pass DestWidth
      call    UnkChunkPlayer
      jmp     @NextChunk

    // ---------------------------------------------------------------------------------------------

    JumpTable:
      dd      @PlayUnknown //  0 ?
      dd      @PlayUnknown //  1 ?
      dd      @PlayUnknown //  2 ?
      dd      @PlayUnknown //  3 ?
      dd      @PlayUnknown //  4 o FLI_COLOR256
      dd      @PlayUnknown //  5 ?
      dd      @PlayUnknown //  6 ?
      dd      @PlaySS2     //  7 x FLI_SS2
      dd      @PlayUnknown //  8 ?
      dd      @PlayUnknown //  9 ?
      dd      @PlayUnknown // 10 ?
      dd      @PlayUnknown // 11 o FLI_COLOR
      dd      @PlayLC      // 12 x FLI_LC
      dd      @PlayBlack   // 13 x FLI_BLACK
      dd      @PlayUnknown // 14 ?
      dd      @PlayBRun    // 15 x FLI_BRUN
      dd      @PlayCopy    // 16 x FLI_COPY
      dd      @PlayUnknown // 17 ?
      dd      @PlayUnknown // 18 o FLI_PSTAMP

    @NextChunk:
      mov     eax, SaveEAX
      add     eax, TFliChunkGeneric([eax]).Size
      mov     SaveEAX, eax

      dec     Count
      jnz     @PlayChunk

      mov     ebx, SaveEBX
      mov     esi, SaveESI
      mov     edi, SaveEDI

    @Exit:
    end;
  {$WARNINGS ON}

  function CreatePaletteChunk( ChangedColors : ColorSet; var RgbQuads ) : pointer;
    var
      ColorPacket    : ^TColorPacket;
      ColorPacketOfs : integer;
      i, ColorIndx   : integer;
      SkipCnt        : integer;
      PaletteChunk   : ^TFliChunkColor256 absolute Result;
      RgbEntries     : TRgbPalette        absolute RgbQuads;
    begin
      new( PaletteChunk );
      ColorPacket := @PaletteChunk.Packets;
      with PaletteChunk^ do
        begin
          Magic := idColor256;
          Count := 0;
          Size  := 0;
        end;

      ColorIndx := 0;
      repeat
        Inc( PaletteChunk.Count );

        with PaletteChunk^, ColorPacket^ do
          begin
            // How many colors to skip
            Skip := 0;
            while not ( ( ColorIndx + Skip ) in ChangedColors ) do
              inc( Skip );
            Inc( ColorIndx, Skip );

            Count := 0;
            repeat
              while ( (ColorIndx + Count) in ChangedColors ) and ( ColorIndx + Count < 256 ) do
                Inc( Count );

              // Check if SkipCnt > 3, otherwise we should include the colors in this ColorPacket
              SkipCnt := 0;
              while not ( (ColorIndx + Count + SkipCnt) in ChangedColors ) and ( ColorIndx + Count + SkipCnt < 256 )do
                Inc( SkipCnt );
              if SkipCnt <= 3
                then Inc( Count, SkipCnt );
            until SkipCnt > 3;

            ColorPacketOfs := pchar( ColorPacket ) - PaletteChunk;
            Inc( Size, sizeof( ColorPacket ) + sizeof( Rgb[0] ) * ( Count - 1 ) );
            ReallocMem( PaletteChunk, Size );
          end;

        pchar(ColorPacket) := pchar(PaletteChunk) + ColorPacketOfs; // Relocate ColorPacket pointer

        with PaletteChunk^, ColorPacket^ do
          begin
            for i := 0 to ColorPacket.Count - 1 do
              with Rgb[i], RgbEntries[ColorIndx + i] do
                begin
                  r := rgbRed;
                  g := rgbGreen;
                  b := rgbBlue;
                end;

            inc( ColorIndx, Count );
          end;
      until ColorIndx >= 255;
    end;

end.
