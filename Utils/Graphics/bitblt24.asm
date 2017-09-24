                        PAGE    255, 255

                        PUBLIC  BltCopyTrans24
                        PUBLIC  BltCopyShaded24
                        PUBLIC  BltCopyGlassed24
                        PUBLIC  BltCopyOpaqueCTT24
                        PUBLIC  BltCopySourceCTT24
                        PUBLIC  BltCopyGlassedCTT24

                        INCLUDE bitblt.inc

; 24-bit specific routines:
; =============================================================================================

BltCopyOpaqueCTT24      PROC PASCAL USES ESI EDI EBX, \ ; Source, Dest : ptr byte, aWidth,
                                aHeight : dword, \
                                WidthSource, WidthDest : dword, Info : ptr word
;
;     On entry:
;
;     EAX: Source
;     EDX: Dest
;     ECX: aWidth
;

                        LOCAL   PixelCount : dword

BltOpaque24Write        macro   Indx:REQ


                        mov     al, [esi + (Indx)]
                        mov     eax, [ebx + eax * 4]
                        mov     [edi + (Indx) * 3 + 0], ax
                        shr     eax, 16
                        mov     [edi + (Indx) * 3 + 2], al
                        xor     eax, eax

                        endm

Blt24CopyLoop           macro

                        mov     ecx, PixelCount
                        shr     ecx, 2
        @@LoopX:
                        BltOpaque24Write 0
                        BltOpaque24Write 1
                        BltOpaque24Write 2
                        BltOpaque24Write 3
                        add     esi, 4
                        add     edi, 12
                        dec     ecx
                        jnz     @@LoopX

                        mov     ecx, PixelCount
                        and     ecx, 11b
                        jecxz   @@Done
        @@LoopRest:
                        BltOpaque24Write 0
                        add     esi, 1
                        add     edi, 3
                        dec     ecx
                        jnz     @@LoopRest
        @@Done:

                        endm

                        BltInit8

                        sub     WidthDest, ecx
                        mov     ebx, Info
                        or      ebx, ebx
                        jz      @@Exit

                        BltOpaque Blt24CopyLoop
        @@Exit:
                        ret

BltCopyOpaqueCTT24      ENDP

BltInit24               macro

                        cmp     aHeight, 0
                        je      @@Exit
                        or      ecx, ecx
                        je      @@Exit

                        mov     edi, edx
                        mov     esi, eax

                        mov     PixelCount, ecx                 ; save this for later
                        lea     ecx, [ecx * 2 + ecx]            ; ecx := ecx * 3

                        sub     WidthSource, ecx                ; bias these
                        sub     WidthDest, ecx

                        mov     edx, Transparent
                        and     edx, mskColorKey
                        xor     ebx, ebx

                        endm

BltTrans24              macro   BltWrite:REQ

        @@MoreLines:
                        mov     ecx, PixelCount                 ; ECX is pixel counter
                        shr     ecx, 2
                        jz      @@NextScan

                        AlignTo 16
        @@Same:
                        mov     eax, [esi]
                        and     eax, mskColorKey
                        cmp     eax, edx
                        jne     @@Diff0

        @@Same0:
                        mov     eax, [esi + 3]
                        and     eax, mskColorKey
                        cmp     eax, edx
                        jne     @@Diff1

        @@Same1:
                        mov     eax, [esi + 6]
                        and     eax, mskColorKey
                        cmp     eax, edx
                        jne     @@Diff2

        @@Same2:
                        mov     eax, [esi + 9]
                        and     eax, mskColorKey
                        cmp     eax, edx
                        jne     @@Diff3

        @@Same3:
                        add     edi, 12
                        add     esi, 12
                        dec     ecx
                        jnz     @@Same
                        jmp     @@NextScan

                        AlignTo 16
        @@Diff:
                        mov     eax, [esi]
                        and     eax, mskColorKey
                        cmp     eax, edx
                        je      @@Same0

        @@Diff0:
                        BltWrite 0
                        mov     eax, [esi + 3]
                        and     eax, mskColorKey
                        cmp     eax, edx
                        je      @@Same1

        @@Diff1:
                        BltWrite 1
                        mov     eax, [esi + 6]
                        and     eax, mskColorKey
                        cmp     eax, edx
                        je      @@Same2

        @@Diff2:
                        BltWrite 2
                        mov     eax, [esi + 9]
                        and     eax, mskColorKey
                        cmp     eax, edx
                        je      @@Same3

        @@Diff3:
                        BltWrite 3

                        add     esi, 12
                        add     edi, 12
                        dec     ecx
                        jnz     @@Diff

        @@NextScan:
                        mov     ecx, PixelCount
                        and     ecx, 11b
                        jnz     @@OddLoop                       ; move on to the start of the next line

        @@NextScan1:
                        add     esi, WidthSource
                        add     edi, WidthDest

                        dec     aHeight                         ; line counter
                        jnz     @@MoreLines
                        jmp     @@Exit

        @@OddLoop:
                        mov     eax, [esi]
                        add     edi, 3
                        add     esi, 3
                        and     eax, mskColorKey
                        cmp     eax, edx
                        je      @@OddCont
                        BltWrite -1

        @@OddCont:
                        dec     ecx
                        jnz     @@OddLoop
                        jmp     @@NextScan1

                        endm

BltCopyTrans24          PROC PASCAL USES ESI EDI EBX, \ ; Source, Dest : ptr byte, aWidth,
                                aHeight : dword, Transparent : dword, \
                                WidthSource, WidthDest : dword
;
;     On entry:
;
;     EAX: Source
;     EDX: Dest
;     ECX: aWidth
;

                        LOCAL   PixelCount : dword

BltCopyTrans24Write     macro   Indx:REQ

                        mov     [edi + (Indx) * 3 + 0], ax
                        shr     eax, 16
                        mov     [edi + (Indx) * 3 + 2], al

                        endm

                        BltInit24
                        BltTrans24 BltCopyTrans24Write
        @@Exit:
                        ret

BltCopyTrans24          ENDP

lightShadeColor         equ     110             ; This is the gray level used to form the shadow...

BltCopyShaded24         PROC PASCAL USES ESI EDI, \ ; Source, Dest : ptr byte, aWidth,
                                aHeight : dword, Transparent : dword, \
                                WidthSource, WidthDest : dword, Info : ptr byte
;
;     On entry:
;
;     EAX: Source
;     EDX: Dest
;     ECX: aWidth
;

                        LOCAL   PixelCount : dword

BltCopyShaded24Write    macro   Indx:REQ

                        movzx   ax, byte ptr [edi + (Indx) * 3 + 0]
                        add     ax, lightShadeColor
                        shr     ax, 1
                        mov     [edi + (Indx) * 3 + 0], al

                        movzx   ax, byte ptr [edi + (Indx) * 3 + 1]
                        add     ax, lightShadeColor
                        shr     ax, 1
                        mov     [edi + (Indx) * 3 + 1], al

                        movzx   ax, byte ptr [edi + (Indx) * 3 + 2]
                        add     ax, lightShadeColor
                        shr     ax, 1
                        mov     [edi + (Indx) * 3 + 2], al

                        endm

                        BltInit24
                        BltTrans24 BltCopyTrans24Write

        @@Exit:
                        ret

BltCopyShaded24         ENDP

BltCopyGlassed24        PROC PASCAL USES ESI EDI EBX, \ ; Source, Dest : ptr byte, aWidth,
                                aHeight : dword, Transparent : dword, \
                                WidthSource, WidthDest : dword, Info : ptr byte
;
;     On entry:
;
;     EAX: Source
;     EDX: Dest
;     ECX: aWidth

                        LOCAL   PixelCount : dword

BltCopyGlassed24Write   macro   Indx:REQ ; // Unfinished!, see EDX problem!!

                        mov     bl, al                    ; [edi][Indx] := ( al + [edi][Indx] ) div 2
                        add     bl, [edi + (Indx) * 3]
                        adc     bh, 0
                        shr     bx, 1
                        mov     [edi + (Indx) * 3], bl

                        mov     bl, ah
                        mov     bl, [edi + (Indx) * 3 + 1]
                        mov     dl, al
                        add     bx, dx
                        shr     bx, 1
                        mov     [edi + (Indx) * 3 + 1], bl

                        shr     eax, 8
                        mov     bl, [edi + (Indx) * 3 + 2]
                        add     bx, ax
                        shr     bx, 1
                        mov     [edi + (Indx) * 3 + 2], bl

                        endm

                        BltInit24

                        xor     ebx, ebx
                        BltTrans24 BltCopyGlassed24Write

        @@Exit:
                        ret

BltCopyGlassed24        ENDP

BltCopySourceCTT24      PROC PASCAL USES ESI EDI EBX, \ ; Source, Dest : ptr byte, aWidth,
                                aHeight : dword, Transparent : dword, \
                                WidthSource, WidthDest : dword, Info : ptr dword
;
;     On entry:
;
;     EAX: Source
;     EDX: Dest
;     ECX: aWidth
;

                        LOCAL   PixelCount : dword

BltCopySourceCTT24Write macro   Indx:REQ

                        mov     eax, [ebx + eax * 4]
                        mov     [edi + (Indx) * 3], ax
                        shr     eax, 16
                        mov     [edi + (Indx) * 3 + 2], al
                        xor     eax, eax

                        endm

                        BltInit8

                        shl     ecx, 2
                        sub     WidthDest, ecx
                        mov     ebx, Info
                        or      ebx, ebx
                        jz      @@Exit

                        BltTrans8 BltCopySourceCTT24Write, 3
        @@Exit:
                        ret

BltCopySourceCTT24      ENDP

BltCopyGlassedCTT24     PROC PASCAL USES ESI EDI EBX, \ ; Source, Dest : ptr byte, aWidth,
                                aHeight : dword, Transparent : dword, \
                                WidthSource, WidthDest : dword, Info : ptr word
;
;     On entry:
;
;     EAX: Source
;     EDX: Dest
;     ECX: aWidth
;

                        LOCAL   PixelCount : dword

BltGlassCTT24Write      macro   Indx:REQ
                        endm

                        BltInit8

                        sub     WidthDest, ecx
                        mov     ebx, Info
                        or      ebx, ebx
                        jz      @@Exit

                        BltTrans8 BltGlassCTT24Write, 2
        @@Exit:
                        ret

BltCopyGlassedCTT24     ENDP

                        END
