unit Fli2Rle;

interface

  uses
    Classes, Windows, Dibs, Flics, FliPlayback, FliPlay;

  type
    TFliToRleDecoder =
      class( TFliPlayer )
        protected
          fRleHeader       : PDib;
          fRleEncodedImage : pointer;
          fUseRleDecoder   : boolean;
          fPartialUpdate   : boolean;

          procedure SetUseRleDecoder( aUseRleDecoder : boolean );                              virtual;

          procedure AllocDecoder;
          procedure FreeDecoder;

        public
          property RleHeader : PDib          read fRleHeader;
          property RleEncodedImage : pointer read fRleEncodedImage;
          property UseRleDecoder : boolean   read fUseRleDecoder write SetUseRleDecoder default true;
          property PartialUpdate : boolean   read fPartialUpdate write fPartialUpdate   stored false;

          constructor Create;
          destructor  Destroy;                                                                 override;

          procedure ProcessFrame;                                                              override;
          procedure LoadFromStream( aStream : TStream );                                       override;
      end;

  // FLI -> RLE low level routines

  procedure PlayFrameToRle( Frame : PFliFrame; const FliSize : TPoint; Dest : pointer; DestWidth : integer;
                            UnkChunkPlayer : TChunkPlayerProc);

  function PlayChunksToRle( FirstChunk : pointer; const FliSize : TPoint; Dest : pointer; DestWidth : integer;
                            Count : integer; UnkChunkPlayer : TChunkPlayerProc) : pointer;


implementation

  uses
    ObjUtils, DibRle;

  // TFliToRleDecoder

  procedure TFliToRleDecoder.AllocDecoder;
    var
      RleMaxSize : integer;
    begin
      RleMaxSize := DwordAlign( Size.X ) * Size.Y; // Worst case
      GetMem( fRleEncodedImage, RleMaxSize );
      pword( fRleEncodedImage )^ := rleEndOfBitmap;

      fRleHeader := DibNewHeader( Size.X, Size.Y, 8 );
      fRleHeader.biCompression := BI_RLE8;
      DibSetUsage( fRleHeader, 0, DIB_PAL_COLORS );
    end;

  procedure TFliToRleDecoder.FreeDecoder;
    begin
      if Assigned( fRleHeader )
        then
          begin
            DibFree( fRleHeader );
            fRleHeader := nil;
            FreePtr( fRleEncodedImage );
          end;
    end;

  procedure TFliToRleDecoder.SetUseRleDecoder( aUseRleDecoder : boolean );
    begin
      fUseRleDecoder := aUseRleDecoder;
      if (UseRleDecoder <> aUseRleDecoder) and (not Empty)
        then
          if UseRleDecoder
            then AllocDecoder
            else FreeDecoder;
    end;

  constructor TFliToRleDecoder.Create;
    begin
      inherited;

      fUseRleDecoder := true;
    end;

  destructor TFliToRleDecoder.Destroy;
    begin
      FreeDecoder;
      inherited;
    end;

  procedure TFliToRleDecoder.LoadFromStream( aStream : TStream );
    begin
      if UseRleDecoder
        then
          begin
            FreeDecoder;
            inherited;
            AllocDecoder;
          end
        else inherited;
    end;

  threadvar
    fCurrentPlayer : TFliToRleDecoder;

  function RleUnkChunkPlayer( Chunk : pointer; const FliSize : TPoint; Buffer : pointer; BufferWidth : integer ) : pointer;
    begin
      Result := pchar(Chunk) + PFliChunk(Chunk).Size;
      fCurrentPlayer.PartialUpdate := false;
    end;

  procedure TFliToRleDecoder.ProcessFrame;
    begin
      inherited;

      if UseRleDecoder and Assigned( fRleEncodedImage )
        then
          begin
            fCurrentPlayer := Self;
            PartialUpdate  := true;
            PlayFrameToRle( CurrentFrame, Size, fRleEncodedImage, fBufferWidth, RleUnkChunkPlayer );
          end;
    end;

  // FLI -> RLE low level routines

  procedure PlayFrameToRle( Frame : PFliFrame; const FliSize : TPoint; Dest : pointer; DestWidth : integer;
                            UnkChunkPlayer : TChunkPlayerProc);
    begin
      try
        PlayChunksToRle( pchar(Frame) + sizeof(TFliFrame), FliSize, Dest, DestWidth, Frame.Chunks, UnkChunkPlayer );
      except // Avoid any exception due to an invalid chunk
      end;
    end;

  // Chunk Players

  {$WARNINGS OFF}
  function PlayChunksToRle( FirstChunk : pointer; const FliSize : TPoint; Dest : pointer; DestWidth : integer;
                            Count : integer; UnkChunkPlayer : TChunkPlayerProc ) : pointer;
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

    @PlayLC:   // This chunk is found only in 320x200 FLIs
               // Uses esi, edi, eax, ebx, ecx, edx
      mov     esi, SaveEAX
      mov     edi, SaveECX

      xor     eax, eax
      xor     ecx, ecx
      add     esi, type TFliChunkGeneric
      mov     TempVar, eax

      lodsw                                   // Lines to skip
      or      ax, ax
      jz      @@LCLineCount

      mov     word ptr [edi+0], rleJump       // Store rleJump
      shl     ax, 8                           // Now ah = rows, al = columns (0)
      mov     word ptr [edi+2], ax
      add     edi, 4

    @@LCLineCount:
      lodsw                                   // Line count
      mov     edx, eax

    @@LCLine:
      xor     ah, ah
      lodsb                                   // Load packet count
      or      al, al
      jnz     @@LCLinePackets

      inc     TempVar
      jmp     @@LCLineNext

    @@LCLinePackets:
      mov     ebx, eax                        // ebx = packet count

      mov     eax, TempVar                    // Do we have lines to skip?
      cmp     al, 1
      jb      @@LCPacket                      // SkipCount = 0, ignore
      ja      @@LCLineSkip                    // SkipCount > 1, generate rleJump

      mov     ax, rleEndOfLine                // SkipCount = 1, an rleEndOfLine will do it
      stosw
      jmp     @@LCPacket

    @@LCLineSkip:
      shl     eax, 24
      mov     ax, rleJump
      stosd
      xor     eax, eax
      mov     TempVar, eax

    @@LCPacket:                               // Process each packet
      lodsb                                   // Columns to skip
      or      al, al
      jz      @@LCTypeSize

      mov     word ptr [edi+0], rleJump       // Store rleJump
      xor     ah, ah
      mov     word ptr [edi+2], ax            // Store coordinates (x += al, y += 0)
      add     edi, 4

    @@LCTypeSize:
      lodsb                                   // Type/Size byte
      test    al, al
      js      @@LCFill

    @@LCMove:
      cmp     al, 3
      jb      @@LCMovePatch                   // If count < 3, we're in trouble, go to patch

      mov     cl, al
      shl     ax, 8
      stosw                                   // Store move command (al=0) and count (ah)
      rep     movsb                                

      dec     ebx
      jnz     @@LCPacket
      jmp     @@LCLineFinished

    @@LCMovePatch:
      mov     cl, al
      mov     al, 1

    @@LCMovePatchLoop:
      stosb
      movsb
      dec     ecx
      jnz     @@LCMovePatchLoop

      dec     ebx
      jnz     @@LCPacket
      jmp     @@LCLineFinished

    @@LCFill:
      neg     al
      stosb                                   // Store count (al)
      movsb                                   // Store value

      dec     ebx
      jnz     @@LCPacket

    @@LCLineFinished:
      mov     ax, rleEndOfLine
      stosw

    @@LCLineNext:  
      dec     edx
      jnz     @@LCLine

    @@LCChunkFinished:  
      mov     ax, rleEndOfBitmap
      stosw
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

    @@SkipLines:
      mov     ax, 255
      sub     ax, cx                        // ax = min( cx, 255 )  (255 = maximum run in RLE)
      cwd
      and     ax, dx
      add     ax, cx
      shl     ax, 8
      stosw
      shr     ax, 8
      sub     cx, ax
      jnz     @@SkipLines


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
      dd      @PlayUnknown //  7 x FLI_SS2
      dd      @PlayUnknown //  8 ?
      dd      @PlayUnknown //  9 ?
      dd      @PlayUnknown // 10 ?
      dd      @PlayUnknown // 11 o FLI_COLOR
      dd      @PlayLC      // 12 x FLI_LC
      dd      @PlayUnknown // 13 x FLI_BLACK
      dd      @PlayUnknown // 14 ?
      dd      @PlayUnknown // 15 x FLI_BRUN
      dd      @PlayUnknown // 16 x FLI_COPY
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

end.
