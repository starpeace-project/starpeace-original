unit DibRle;

interface

  uses
    Windows, Dibs, NumUtils;

  procedure UnpackRle4( DibWidth : integer; RlePixels : pointer; DibPixels : pointer );
  procedure UnpackRle8( DibWidth : integer; RlePixels : pointer; DibPixels : pointer );

  procedure PackRle8( DibHeader : PDib; DibPixels, RlePixels, PrevFramePixels : pointer; MinJumpLength : integer );
  function  Rle8DeltaFrame( DibHeader : PDib; DibPixels, RlePixels, PrevFramePixels : pchar; Start, Len : integer; MinJump : integer ) : PDib;

  const
    RLE_ESCAPE = 0;
    RLE_EOL    = 0;
    RLE_EOF    = 1;
    RLE_JMP    = 2;
    RLE_MINABS = 3;

  const
    rleJump        = RLE_ESCAPE + RLE_JMP shl 8;
    rleEndOfLine   = RLE_ESCAPE + RLE_EOL shl 8;
    rleEndOfBitmap = RLE_ESCAPE + RLE_EOF shl 8;

  procedure DibUnpackRle( SourcePixels : pointer; DibHeader : PDib; DibPixels : pointer; ForceTopDown : boolean );

  procedure RegisterRleDibIoHooks;
  procedure UnregisterRleDibIoHooks;

implementation

  procedure RegisterRleDibIoHooks;
    begin
      RegisterDibIOHooks( [ BI_RLE4, BI_RLE8 ], DibUnpackRle );
    end;

  procedure UnregisterRleDibIoHooks;
    begin
      UnregisterDibIOHooks( [ BI_RLE4, BI_RLE8 ] );
    end;

  procedure DibUnpackRle( SourcePixels : pointer; DibHeader : PDib; DibPixels : pointer; ForceTopDown : boolean );
    begin
      case DibHeader.biCompression of
        BI_RLE4 :
          UnpackRle4( DibHeader.biWidth, SourcePixels, DibPixels );
        BI_RLE8 :
          UnpackRle8( DibHeader.biWidth, SourcePixels, DibPixels );
      end;
    end;

  procedure UnpackRle4( DibWidth : integer; RlePixels : pointer; DibPixels : pointer );
    //
    // On entry:
    //
    //     EAX: DibWidth
    //     EDX: RlePixels
    //     ECX: DibPixels
    var
      OddX : boolean;
    asm
(*
      push      esi
      push      edi
      push      ebx

      add       eax, 3
      mov       esi, edx      // esi = RlePixels
      and       eax, not 3
      mov       edi, ecx      // edi = DibPixels
      mov       ebx, eax      // ebx = scanline width

      xor       ecx, ecx
      xor       eax, eax
      mov       OddX, 0

      // Start of RLE decoding
    @RleBltStart:
      mov     edx, edi        // save start of scan

    @RleBltNext:
      lodsw                   // al := count ah := color

      or      al, al          // is it a escape?
      jz      @RleBltEscape

      // We have found a encoded run (al <> 0)
    @RleBltEncodedRun:
      mov     cl, al          //  al - run length
      mov     al, ah          //  ah - run colors 1 & 2
      mov     ah, OddX
      shr     ecx, 1
      jnc     @RleEncodedEvenCount

    @RleEncodedOddCount:
      xor     OddX, 1

    @RleEncodedEvenCount:
      or      ah, ah          // Is X odd?
      jnz     @EvenEncodedRun

    @OddEncodedRun:
      rol     al, 4           // Swap color1 & color2 in AL
      rep     stosb
      jnc     @RleBltNext

      jmp     @RleBltNext

      // We have found a RLE escape code (al = 0)
      // Possibilities are:
      //      . End of Line            -  ah = 0
      //      . End of RLE             -  ah = 1
      //      . Delta                  -  ah = 2
      //      . Unencoded run          -  ah = 3 or more
    @RleBltEscape:
      cmp     ah, al
      je      @RleBltEOL

      inc     al
      cmp     ah, al
      je      @RleBltEOF

      inc     al
      cmp     ah, al
      je      @RleBltDelta

      // We have found a un-encoded run (ah >= 3)
    @RleBltUnencodedRun:
      xchg    al, ah          //  ah      is pixel count
      mov     cl, al          //  ESI --> pixels

      shr     ecx, 1
      rep     movsb
      jnc     @UnencodedCont

      xor     OddX, 1
      jz      // !!!

    @MoveOddPixel:
      jmp     @UnencodedCont
      
    @MoveEvenPixel:

    @UnencodedCont:
      inc     esi             // !!! re-align source
      and     si, not 1
      jmp     @RleBltNext

      // We have found a delta jump, the next two bytes contain the jump values
      // note the the jump values are unsigned bytes, x first then y
    @RleBltDelta:
      lodsw                   // al = deltaX, ah = deltaY

      or      ah, ah
      jnz     @RleBltDeltaXY

    @RleBltDeltaX:
      shr     eax, 1
      jc      @RleBltDeltaXYHalf

    @RleBltDeltaXYFull:  
      add     edi, eax
      jmp     @RleBltNext

    @RleBltDeltaXY:
      add     edi, ebx
      add     edx, ebx
      dec     ah
      jnz     @RleBltDeltaXY

    @RleBltDeltaXYHalf:
      add     edi, eax
      xor     OddX, 1
      jnz     @RleBltNext
      inc     edi
      jmp     @RleBltNext

      // We have found a end of line marker, point ES:DI to the begining of the
      // next scan
    @RleBltEOL:
      mov     edi, edx        // go back to start of scan
      add     edi, ebx        // advance to next scan
      jmp     @RleBltStart    // go get some more

      // We have found a end of rle marker, clean up and exit
    @RleBltEOF:

    @Exit:
      pop       ebx
      pop       edi
      pop       esi
*)
    end;

  procedure UnpackRle8( DibWidth : integer; RlePixels : pointer; DibPixels : pointer );
    //
    // On entry:
    //
    //     EAX: DibWidth
    //     EDX: RlePixels
    //     ECX: DibPixels
    asm
      push      esi
      push      edi
      push      ebx

      add       eax, 3
      mov       esi, edx      // esi = RlePixels
      and       eax, not 3
      mov       edi, ecx      // edi = DibPixels
      mov       ebx, eax      // ebx = scanline width

      xor       ecx, ecx
      xor       eax, eax

      // Start of RLE decoding
    @RleBltStart:
      mov     edx, edi        // save start of scan

    @RleBltNext:
      lodsw                   // al := count ah := color

      or      al, al          // is it a escape?
      jz      @RleBltEscape

      // We have found a encoded run (al <> 0)
    @RleBltEncodedRun:
      mov     cl, al          //  al - run length
      mov     al, ah          //  ah - run color

      shr     ecx, 1
      rep     stosw
      adc     cl, cl
      rep     stosb

      jmp     @RleBltNext

      // We have found a RLE escape code (al = 0)
      // Possibilities are:
      //      . End of Line            -  ah = 0
      //      . End of RLE             -  ah = 1
      //      . Delta                  -  ah = 2
      //      . Unencoded run          -  ah = 3 or more
    @RleBltEscape:
      cmp     ah, al
      je      @RleBltEOL

      inc     al
      cmp     ah, al
      je      @RleBltEOF

      inc     al
      cmp     ah, al
      je      @RleBltDelta

      // We have found a un-encoded run (ah >= 3)
    @RleBltUnencodedRun:
      xchg    al, ah          //  ah      is pixel count
      mov     cl, al          //  ESI --> pixels

      shr     ecx, 2
      and     eax, 3
      rep     movsd
      mov     ecx, eax
      rep     movsb

      inc     esi             // !!! re-align source
      and     si, not 1
      jmp     @RleBltNext

      // We have found a delta jump, the next two bytes contain the jump values
      // note the the jump values are unsigned bytes, x first then y
    @RleBltDelta:
      lodsw                   // al = deltaX, ah = deltaY

      or      ah, ah
      jnz     @RleBltDeltaXY

    @RleBltDeltaX:
      add     edi, eax
      jmp     @RleBltNext

    @RleBltDeltaXY:
      add     edi, ebx
      add     edx, ebx
      dec     ah
      jnz     @RleBltDeltaXY     // !!!

      add     edi, eax
      jmp     @RleBltNext

      // We have found a end of line marker, point ES:DI to the begining of the
      // next scan
    @RleBltEOL:
      mov     edi, edx        // go back to start of scan
      add     edi, ebx        // advance to next scan
      jmp     @RleBltStart    // go get some more

      // We have found a end of rle marker, clean up and exit
    @RleBltEOF:

    @Exit:
      pop       ebx
      pop       edi
      pop       esi
    end;

  procedure PackRle8( DibHeader : PDib; DibPixels, RlePixels, PrevFramePixels : pointer; MinJumpLength : integer );
    //
    // On entry:
    //
    //     EAX: DibHeader
    //     EDX: DibPixels
    //     ECX: RlePixels
    var
      tDibHeader              : PDib;
      tDibPixels              : pointer;
      tRlePixels              : pointer;
      CurX, CurY              : word;
      ImageWidth, ImageHeight : word;
      JumpX, JumpY            : smallint;
      NextScan                : integer;
      WidthBytes              : integer;
    asm
      push      esi
      push      edi
      push      ebx

      mov       tDibHeader, eax
      mov       tDibPixels, edx
      mov       tRlePixels, ecx
      mov       edx, TDib( [eax] ).biWidth
      mov       esi, TDib( [eax] ).biHeight
      cmp       esi, 0
      jge       @DibBottomUp
      neg       esi                     // Make sure height > 0
   @DibBottomUp:
      mov       ImageWidth, dx
      mov       ImageHeight, si
      mov       eax, edx
      add       eax, 3                  // Compute scanline width
      and       eax, not 3
      mov       WidthBytes, eax
      sub       eax, edx
      mov       NextScan, eax

      xor       esi, esi
      mov       CurX, si
      mov       CurY, si
      mov       JumpX, si
      mov       JumpY, si

      // init pointers into buffers, the following registers will be constant
      // for the entire DeltaFrame process.
      mov       esi, tDibPixels        // esi = DibPixels
      mov       edi, tRlePixels        // edi = RlePixels
      mov       ebx, PrevFramePixels   // ebx = PrevFramePixels

      or        ebx, ebx               // if PrevFramePixels is NULL, no Temporal compression is wanted, just RLE
      jnz       @DeltaFrameTemporal    // the DIB and return

      // Spatial Compression
      // --------------------------------------------------------------------
      // the frame is to be compressed without relying on the previous frame
    @DeltaFrameSpatial:

    @DeltaFrameSpatialLoop:
      mov       cx, ImageWidth           // encode entire line
      call      @EncodeFragment          // ...go do it
      add       esi, NextScan            // point pbDib to next scan

      dec       ImageHeight              // another scan to do?
      jz        @DeltaFrameDone          // ...no generate EOF and exit

      mov       ax, rleEndOfLine         // generate EOL, and go for more
      stosw

      jmp       @DeltaFrameSpatialLoop

      // Temporal Compression
      // --------------------------------------------------------------------
      // the frame is to be compressed assuming the previous frame is visible
      // any pixels that are the same in both frames will be skiped over
    @DeltaFrameTemporal:
      xchg      edi, PrevFramePixels  // edi --> previous DIB

    @DeltaFrameTemporalLoop:
      mov       cx, ImageWidth        // compute amount of pixels left
      sub       cx, curX              // on the scanline
      jz        @DeltaFrameEOL        // are we at EOL?

      call      @FindFragmentLength   // calc frag length and jump value

      or        ax, ax
      jz        @DeltaFrameJump

      //  we have a fragment (ie a part of the image that changed) to encode
      //
      //  first thing we need to do is generate any outstanding jumps we have
      //  in (jump.x, jump.y)
      //
      //      AX is fragment length
      //      BX is jump length
      //
      add       edi, eax
      xchg      edi, PrevFramePixels           // edi --> RLE bits

      push      bx                             // save jump size
      push      ax                             // save fragment size

      xor       cx, cx
      xor       bx, bx
      xchg      cx, JumpX                      // check if we need to gen a jump
      xchg      bx, JumpY

    @DeltaFrameDoJump:
      mov       ax, cx                         // check if we need to gen a jump
      or        ax, bx
      jz        @DeltaFrameFragment            // no jump needed generate a frag.
      js        @DeltaFrameNegY                // negative, need a EOL

      mov       ax, rleJump
      stosw

      mov       ax, 255                        // ax = 255
      sub       ax, cx                         // ax = min( ax, cx )  (255 = maximum run)
      cwd
      and       ax, dx
      add       ax, cx

      stosb
      sub       cx, ax

      mov       ax, 255                        // ax = 255
      sub       ax, bx                         // ax = min( ax, bx )  (255 = maximum run)
      cwd
      and       ax, dx
      add       ax, bx

      stosb
      sub       bx, ax

      jmp       @DeltaFrameDoJump

    @DeltaFrameNegY:
      mov       ax, rleEndOfLine
      stosw
      mov       cx, curX
      dec       bx
      jmp       @DeltaFrameDoJump

    @DeltaFrameFragment:
      pop       cx
      add       curX, cx
      call      @EncodeFragment

      xchg      edi, PrevFramePixels           // edi --> Prev DIB
      pop       bx

    @DeltaFrameJump:
      add       jumpX, bx
      add       curX, bx
      add       esi, ebx
      add       edi, ebx
      jmp       @DeltaFrameTemporalLoop
                
    @DeltaFrameEOL:
      inc       jumpY
      dec       ImageHeight
      jz        @DeltaFrameTemporalDone
                
      mov       eax, NextScan
      add       esi, eax
      add       edi, eax

      mov       ax, curX                       // jumpX -= curX
      sub       jumpX, ax

      xor       eax, eax
      mov       curX, ax
      jmp       @DeltaFrameTemporalLoop

    @DeltaFrameTemporalDone:
      xchg      edi, PrevFramePixels           // edi --> rle data

      //  we are all done!
      //
      //  generate the final EOF and update the biSizeImage field in passed
      //  bitmapinfo and return.
    @DeltaFrameDone:
      mov       ax, rleEndOfBitmap
      stosw

      mov       esi, tDibHeader                // ESI --> BITMAPINFO

      sub       edi, tRlePixels                                     // compute length
      mov       TBitmapInfoHeader([esi]).biSizeImage, edi           // and store it.

      mov       TBitmapInfoHeader([esi]).biCompression, BI_RLE8
      jmp       @Exit

      //   RLE encodes a run of 8 bit pixels, no Temporal compression is done.
      //
      // Entry:
      //       CX           --> number of pixels to RLE
      //       ESI          --> DIB pixels to RLE
      //       EDI          --> place to store RLE data
    @EncodeFragment:
      or        cx, cx                         // anything at all to do?
      jnz       @EncodeFragmentLoop
      jmp       @EncodeFragmentExit

    @EncodeFragmentLoop:
      mov       bx, dx
      mov       ax, cx                         // eax = pixels left

      sub       ax, 255                        // eax = min( eax, 255 )  (255 = maximum run)
      cwd
      and       ax, dx
      add       ax, 255

      shl       ecx, 16                        // save old ecx
      mov       cx, ax                         // ecx = maximum run allowed
      mov       dx, bx

      //  look for a run of same pixels and generate a single RLE run.
    @EncodeFragmentSolid:
      xor       ebx, ebx                       // ebx = 0 (run count)
      mov       ah, [esi]                      // get first pixel

    @EncodeFragmentSolidScan:
      inc       bx
      cmp       bx, cx
      je        @EncodeFragmentSolidRun

      cmp       ah, [esi + ebx]                // get pixel
      je        @EncodeFragmentSolidScan

    @EncodeFragmentSolidRun:
      cmp       bx, 1                          // is run greater than one?
      jbe       @EncodeFragmentAbs

    @EncodeFragmentSolidEncode:
      mov       al, bl                         // store solid run (cnt, color)
      stosw
      add       esi, ebx                       // advance pbDib

      shr       ecx, 16                        // restore cx (length)
      sub       cx, bx
      jz        @EncodeFragmentExit            // any pixels left to encode?
      jmp       @EncodeFragmentLoop

      //  look for a run of pixels that are not the same
      //  note. we cant generate a abs run less than 3 pixels, so if we have
      //  a abs run <3 encode it as a bunch of count=1 rle runs
    @EncodeFragmentAbs:
      cmp       cx, RLE_MINABS                 // enough room left for a min abs run?
      jb        @EncodeFragmentSolidEncode

      mov       bx, RLE_MINABS - 1

    @EncodeFragmentAbsScan:
      inc       bx                             // add another pixel to the run

      // we want at least 4 pixels in a row to be the same before we
      // stop ABS mode.  otherwise leaving ABS mode to do a short run
      // then re-entering ABS mode would be bigger
      //
      // if there are not 4 pixels left on the line, encode the max
      // amount, else the four pixels must be the same

      mov       ax, cx                         // get remaining length
      sub       ax, bx
      xchg      bx, cx                         // cx = run, bx = max
      cmp       ax, 4                          // are there 4 or more pixels left?
      jb        @EncodeFragmentAbsRun          // no encode the max amount
      xchg      bx, cx                         // cx = max, bx = run

      mov       al, [esi + ebx + 0]            // get first pixel

      cmp       al, [esi + ebx + 1]            // are they the same?
      jne       @EncodeFragmentAbsScan

      cmp       al, [esi + ebx + 2]            // are they the same?
      jne       @EncodeFragmentAbsScan

      cmp       al, [esi + ebx + 3]            // are they the same?
      jne       @EncodeFragmentAbsScan

    @EncodeFragmentAbsRun:
      xor       al, al                         // store abs run (0, cnt)
      mov       ah, bl
      stosw

      shr       ecx, 16                        // restore cx (length)
      sub       cx, bx                         // subtract run length from total

      xchg      cx, bx
      shr       cx, 1
      rep       movsw
      adc       cx, cx
      rep       movsb
      mov       cx, bx

      inc       edi                            // word align RLE data
      and       edi, not 1                     // !!! store a zero?

      jcxz      @EncodeFragmentExit            // any pixels left to encode?
      jmp       @EncodeFragmentLoop            // and do it again.

    @EncodeFragmentExit:
      ret

      //   determine the number of pixels that are not the same
      //   as the previous frame, this run of pixels need to be encoded.
      //
      //   a fragment ends when we run out of pixels or we find a run of similar
      //   pixels greater than MinJumpLength
      //
      // Entry:
      //       CX           --> number of pixels in line
      //       ESI          --> DIB pixels to RLE
      //       EDI          --> Previous DIB image

    @FindFragmentLength:
      xor       eax, eax
      xor       ebx, ebx
      jcxz      @FindFragmentLengthExit

      // look for a run of pixels that are not the same
      // to the previous frame, we must find MinJumpLength pixels that
      // are the same before we stop.
      mov       ax, cx
      mov       cx, word ptr MinJumpLength     // put MinJumpLength in HIWORD(ecx)
      shl       ecx, 16
      mov       cx, ax

      push      ebp                            // save bp
      mov       ebp, $FFFF

    @FindFragmentLengthLoop1:
      mov       bx, -1

    @FindFragmentLengthLoop:
      inc       bx
      inc       bp                             // another one not the same
      cmp       bp, cx                         // do we got enough?
      je        @FindFragmentLengthDone        // ...yes all done

      mov       ah, [edi + ebp]                // !!!use words!!!
      cmp       ah, [esi + ebp]                // is it exact?
      je        @FindFragmentLengthLoop        // the same keep going (and counting)

      rol       ecx, 16                        // ax = HIWORD(ecx) = MinJumpLength
      mov       ax, cx
      rol       ecx, 16
      cmp       bx, ax                         // big enough run to stop?
      jb        @FindFragmentLengthLoop1       // no, zero "same" count and keep going

    @FindFragmentLengthDone:
      sub       cx, bp
      mov       ax, bp                         // return length - jump
      sub       ax, bx
      pop       ebp

    @FindFragmentLengthExit:
      movzx     ecx, cx
      ret

    @Exit:
      pop       ebx
      pop       edi
      pop       esi
    end;

  function Rle8DeltaFrame( DibHeader : PDib; DibPixels, RlePixels, PrevFramePixels : pchar; Start, Len : integer; MinJump : integer ) : PDib;
    var
      AllocRle  : boolean;
      Height    : integer;
      JumpCount : integer;
      DeltaY    : integer;
    begin
      assert( ( DibHeader.biBitCount = 8 ) and ( DibHeader.biCompression <> BI_RGB ), 'Non-8bpp DIB in DibRle.DibRle8DeltaFrame' );

      with DibHeader^ do
        begin
          JumpCount := 0;
          if MinJump = 0
            then MinJump := 4;
          Height := abs( DibHeader.biHeight );
          if Len <= 0
            then Len := Height;
          Len := min( Height - Start, Len );

          if DibPixels = nil
            then DibPixels := DibPtr( DibHeader );

          //  create an RLE buffer to place the RLE bits in
          Result   := DibNewHeader( biWidth, biHeight, 8 );
          AllocRle := not Assigned( RlePixels );
          if AllocRle
            then
              begin
                ReallocMem( Result, DibSize( Result ) );
                RlePixels := DibPtr( Result );
              end;
          Result.biSizeImage := 0;

          DibPixels := DibScanLine( DibHeader, DibPixels, Start );
          while Start > 0 do
            begin
              DeltaY := min( Start, 255 );
              dec( Start, DeltaY );

              byte( RlePixels[0] ) := RLE_ESCAPE;
              byte( RlePixels[1] ) := RLE_JMP;
              byte( RlePixels[2] ) := 0;
              byte( RlePixels[3] ) := DeltaY;
              inc( RlePixels, 4 );

              dec( JumpCount, 4 );
            end;

          Swap4( Result.biHeight, Len );
          PackRle8( Result, DibPixels, RlePixels, PrevFramePixels, MinJump  );
          Swap4( Result.biHeight, Len );

          inc( Result.biSizeImage, JumpCount );  // adjust size to include JUMP!

          // Hey we are done! Unlock the buffers and get out
          if AllocRle
            then ReallocMem( Result, DibSize( Result ) );
        end;
    end;

end.
