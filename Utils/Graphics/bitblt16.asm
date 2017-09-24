                        PAGE    255, 255

                        PUBLIC  BltCopyOpaqueCTT16
                        PUBLIC  BltCopySourceCTT16
                        PUBLIC  BltCopyGlassedCTT16

                        INCLUDE bitblt.inc

; 16-bit specific routines:
; =============================================================================================

BltCopySourceCTT16      PROC PASCAL USES ESI EDI EBX, \ ; Source, Dest : ptr byte, aWidth,
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

BltCopyCTT16Write       macro   Indx:REQ

                        mov     ax, [ebx + eax * 2]
                        mov     [edi + (Indx) * 2], ax
                        xor     ax, ax

                        endm

                        BltInit8

                        sub     WidthDest, ecx
                        mov     ebx, Info
                        or      ebx, ebx
                        jz      @@Exit

                        BltTrans8 BltCopyCTT16Write, 2
        @@Exit:
                        ret

BltCopySourceCTT16      ENDP

BltCopyOpaqueCTT16      PROC PASCAL USES ESI EDI EBX, \ ; Source, Dest : ptr byte, aWidth,
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

BltOpaque16Write        macro   Indx:REQ

                        mov     al, [esi + (Indx)]
                        mov     ax, [ebx + eax * 2]
                        mov     [edi + (Indx)], ax
                        xor     ax, ax

                        endm

Blt16CopyLoop           macro

                        mov     ecx, PixelCount
                        shr     ecx, 2
        @@LoopX:
                        BltOpaque16Write 0
                        BltOpaque16Write 1
                        BltOpaque16Write 2
                        BltOpaque16Write 3
                        add     esi, 4
                        add     edi, 8
                        dec     ecx
                        jnz     @@LoopX

                        mov     ecx, PixelCount
                        and     ecx, 11b
                        jecxz   @@Done
        @@LoopRest:
                        BltOpaque16Write 0
                        add     esi, 1
                        add     edi, 2
                        dec     ecx
                        jnz     @@LoopRest
        @@Done:

                        endm

                        BltInit8

                        sub     WidthDest, ecx
                        mov     ebx, Info
                        or      ebx, ebx
                        jz      @@Exit

                        BltOpaque Blt16CopyLoop
        @@Exit:
                        ret

BltCopyOpaqueCTT16      ENDP

Unpack                  macro   Dest:REQ

                        ror     eax, 10
                        mov     byte ptr Dest[2], al
                        xor     al, al
                        rol     eax, 5
                        mov     byte ptr Dest[1], al
                        shr     eax, 27
                        mov     byte ptr Dest[0], al

                        endm

Pack                    macro   Src:REQ

                        movzx   ax, byte ptr Src[2]
                        shl     ax, 5
                        or      al, byte ptr Src[1]
                        shl     ax, 5
                        or      al, byte ptr Src[0]

                        endm

Blend                   macro   Dest:REQ, Src:REQ

                        mov     al, byte ptr Src[0]
                        add     al, byte ptr Dest[0]
                        shr     al, 1
                        mov     byte ptr Dest[0], al
                        mov     al, byte ptr Src[1]
                        add     al, byte ptr Dest[1]
                        shr     al, 1
                        mov     byte ptr Dest[1], al
                        mov     al, byte ptr Src[2]
                        add     al, byte ptr Dest[2]
                        shr     al, 1
                        mov     byte ptr Dest[2], al

                        endm

BltGlassCTT16Write      macro   Indx:REQ

                        mov     eax, [ebx + eax * 4]
                        mov     SrcRgb, eax
                        xor     eax, eax
                        mov     ax, [edi + (Indx) * 2]
                        Unpack  TmpRgb
                        Blend   TmpRgb, SrcRgb
                        Pack    TmpRgb
                        mov     [edi + (Indx) * 2], ax
                        xor     eax, eax

                        endm

BlendFuncAlpha1         macro   ; Used when alpha is 1

                        add     dl, [ebx + eax * 4]
                        shr     dl, 3

                        endm

BlendFuncAlpha2         macro

                        add     dx, dx
                        add     dl, [ebx + eax * 4]
                        shr     dl, 3

                        endm

BlendFuncAlpha3         macro

                        lea     edx, [edx + 2 * edx]
                        add     dl, [ebx + eax * 4]
                        shr     dl, 3

                        endm

BlendFuncAlpha4         macro

                        add     dl, [ebx + eax * 4]
                        shr     dl, 1

                        endm

BlendFuncAlpha6         macro

                        add     edx, edx
                        lea     edx, [edx + 2 * edx]
                        add     dl, [ebx + eax * 4]
                        shr     dl, 3

                        endm

BlendFuncAlpha7         macro

                        ; !!!
                        add     dl, [ebx + eax * 4]
                        shr     dl, 3

                        endm

BltGlass16Write         macro   Indx:REQ, BlendFunction:REQ

                        push    ecx
                        mov     dx, word ptr [edi + Indx]
                        shr     edx, 10
                        BlendFunction
                        mov     ecx, edx
                        mov     dx, word ptr [edi + Indx]
                        shl     ecx, 5
                        and     edx, $3E0
                        shr     edx, 5
                        BlendFunction
                        or      ecx, edx
                        mov     dx, word ptr [edi + Indx]
                        shl     ecx, 5
                        and     edx, $1f
                        BlendFunction
                        or      ecx, edx
                        mov     word ptr [edi + Indx], cx
                        mov     edx, word ptr Transparent
                        pop     ecx

                        endm

BltCopyGlassedCTT16     PROC PASCAL USES ESI EDI EBX, \ ; Source, Dest : ptr byte, aWidth,
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
                        LOCAL   TmpRgb : dword
                        LOCAL   SrcRgb : dword

                        BltInit8

                        sub     WidthDest, ecx
                        mov     ebx, Info
                        or      ebx, ebx
                        jz      @@Exit

                        BltTrans8 BltGlassCTT16Write, 2
        @@Exit:
                        ret

BltCopyGlassedCTT16     ENDP

                        END