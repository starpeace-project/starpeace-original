IFNDEF ??version
                        ; We must be using MASM or something, let's do nothing here!!
ELSE
                        ; ??version is defined by TASM, so we should use the LOCALS directive:
                        LOCALS  @@
                        NOSMART
ENDIF

mskColorKey             equ     00ffffffh

AlignTo                 macro   AlignSize:REQ

                        ; ( $ + AlignSize - 1 ) and not (AlignSize - 1)

                        endm

BltOpaque               macro   BltLoop:REQ, Param1, Param2, InitCode

                        or      ecx, ecx         ; aWidth = 0?
                        jz      @@Exit
                        cmp     aHeight, 0       ; aHeight = 0?
                        je      @@Exit

                        mov     edi, edx
                        mov     esi, eax

                        sub     WidthSource, ecx ; bias these
                        sub     WidthDest, ecx
                        mov     PixelCount, ecx
                        InitCode
        @@LoopY:
                        BltLoop Param1, Param2
                        add     esi, WidthSource
                        add     edi, WidthDest
                        dec     aHeight           ; line counter
                        jnz     @@LoopY

                        endm

BltInit8                macro

                        or      ecx, ecx                        ; aWidth = 0?
                        jz      @@Exit
                        cmp     aHeight, 0                      ; aHeight = 0?
                        je       @@Exit

                        mov     edi, edx
                        mov     esi, eax

                        mov     PixelCount, ecx                 ; save this for later
                        sub     WidthSource, ecx                ; bias these
                        sub     WidthDest, ecx

                        xor     eax, eax
                        mov     dl, byte ptr Transparent

                        endm

; The idea here is to not branch very often so we unroll the loop by four
; and try to not branch when a whole run of pixels is either transparent
; or not transparent.
;
; There are two loops. One loop is for a run of pixels equal to the
; transparent color, the other is for runs of pixels we need to store.
;
; When we detect a "bad" pixel we jump to the same position in the
; other loop.
;
; Here is the loop we will stay in as long as we encounter a "transparent"
; pixel in the source.

BltTrans8               macro   BltWrite:REQ, DestWidth:REQ, ExtraParams

        @@MoreLines:
                        mov     ecx, PixelCount  ; ECX is pixel counter
                        shr     ecx, 2
                        jz      @@NextScan

                        AlignTo 16
        @@Same:
                        mov     al, [esi]
                        cmp     al, dl
                        jne     @@Diff0
        @@Same0:
                        mov     al, [esi + 1]
                        cmp     al, dl
                        jne     @@Diff1
        @@Same1:
                        mov     al, [esi + 2]
                        cmp     al, dl
                        jne     @@Diff2
        @@Same2:
                        mov     al, [esi + 3]
                        cmp     al, dl
                        jne     @@Diff3
        @@Same3:
                        add     edi, DestWidth * 4
                        add     esi, 4
                        dec     ecx
                        jnz     @@Same
                        jmp     @@NextScan

                        AlignTo 16
        @@Diff:
                        mov     al, [esi]
                        cmp     al, dl
                        je      @@Same0
        @@Diff0:
                        BltWrite 0, ExtraParams
                        mov     al, [esi + 1]
                        cmp     al, dl
                        je      @@Same1
        @@Diff1:
                        BltWrite 1, ExtraParams
                        mov     al, [esi + 2]
                        cmp     al, dl
                        je      @@Same2
        @@Diff2:
                        BltWrite 2, ExtraParams
                        mov     al, [esi + 3]
                        cmp     al, dl
                        je      @@Same3
        @@Diff3:
                        BltWrite 3, ExtraParams

                        add     edi, DestWidth * 4
                        add     esi, 4
                        dec     ecx
                        jnz     @@Diff

        @@NextScan:
                        mov     ecx, PixelCount
                        and     ecx, 11b
                        jz      @@NextScan1                     ; move on to the start of the next line

        @@OddLoop:
                        mov     al, [esi]
        IF DestWidth EQ 1
                        inc     edi
        ELSE
                        add     edi, DestWidth
        ENDIF
                        inc     esi
                        cmp     al, dl
                        je      @@OddCont
                        BltWrite -1, ExtraParams
        @@OddCont:
                        dec     ecx
                        jnz     @@OddLoop
        @@NextScan1:
                        add     esi, WidthSource
                        add     edi, WidthDest

                        dec     aHeight                         ; line counter
                        jnz     @@MoreLines

                        endm

                        .586p
                        .MODEL FLAT

                        .CODE
