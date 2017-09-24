                        PAGE    255, 255

                        PUBLIC  FlipVertical
                        PUBLIC  BltInjectChannel32
                        PUBLIC  BltExtractChannel32
                        PUBLIC  BltInjectChannel24
                        PUBLIC  BltExtractChannel24

                        INCLUDE bitblt.inc

FlipVertical            PROC PASCAL USES ESI EDI, \ ; Source : ptr byte, aWidth, aHeight : dword,
                            WidthSource : dword
;
;     On entry:
;
;     EAX: Source
;     EDX: aWidth
;     ECX: aHeight
;

                        LOCAL   DwordCount : dword

                        xchg    ecx, edx

                        or      ecx, ecx         ; aWidth = 0?
                        jz      @@exit
                        or      edx, edx         ; aHeight = 0?
                        jz      @@exit

                        mov     esi, eax
                        mov     edi, edx
                        mov     eax, edx
                        dec     eax
                        mul     WidthSource
                        mov     edx, edi
                        add     eax, esi
                        mov     edi, eax

                        add     WidthSource, ecx
                        mov     DwordCount, ecx

        @@LoopY:
                        mov     ecx, DwordCount
                        shr     ecx, 2

        @@LoopX:
                        mov     eax, [esi]
                        xchg    eax, [edi]
                        mov     [esi], eax
                        add     edi, 4
                        add     esi, 4
                        dec     ecx
                        jnz     @@LoopX

                        mov     ecx, DwordCount
                        and     ecx, 11b
                        jz      @@cont

        @@LoopRest:
                        mov     al, [esi]
                        xchg    al, [edi]
                        mov     [esi], al
                        inc     edi
                        inc     esi
                        dec     ecx
                        jnz     @@LoopRest

        @@Cont:
                        sub     edi, WidthSource

                        dec     edx
                        jnz     @@LoopY

        @@Exit:
                        ret

FlipVertical            ENDP

; >>>>>>>>>>>>>>>>>>>> Channel blts >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

BltLoopCopyChannel      macro   SourceGap:REQ, DestGap:REQ

                        mov     ecx, PixelCount
                        shr     ecx, 2
        @@LoopX:
                        mov     al, [esi + 0 * SourceGap]
                        mov     bl, [esi + 1 * SourceGap]
                        mov     [edi + 0 * DestGap], al
                        mov     [edi + 1 * DestGap], bl
                        mov     al, [esi + 2 * SourceGap]
                        mov     bl, [esi + 3 * SourceGap]
                        mov     [edi + 2 * DestGap], al
                        mov     [edi + 3 * DestGap], bl

                        add     esi, SourceGap * 4
                        add     edi, DestGap * 4
                        dec     ecx
                        jz      @@LoopX
                        jmp     @@TestRest

                        AlignTo 16
        @@LoopRest:
                        mov     al, [esi]
                        mov     [edi], al
                        add     esi, SourceGap
                        add     edi, DestGap
                        dec     ecx
                        jz      @@LoopRest
                        jmp     @@Cont

        @@TestRest:
                        mov     ecx, PixelCount
                        and     ecx, 11b
                        jz      @@Cont
        @@Cont:
                        endm

BltInjectChannel24      PROC PASCAL USES EBX ESI EDI, \ ; Source, Dest : ptr byte, aWidth,
                           aHeight : dword, WidthSource, WidthDest : dword
;
;     On entry:
;
;     EAX: Source
;     EDX: Dest
;     ECX: aWidth
;
                        LOCAL   PixelCount : dword

                        BltOpaque BltLoopCopyChannel, 1, 3
        @@Exit:
                        ret

BltInjectChannel24      ENDP

BltExtractChannel24     PROC PASCAL USES EBX ESI EDI, \ ; Source, Dest : ptr byte, aWidth,
                           aHeight : dword, WidthSource, WidthDest : dword
;
;     On entry:
;
;     EAX: Source
;     EDX: Dest
;     ECX: aWidth
;
                        LOCAL   PixelCount : dword

                        BltOpaque BltLoopCopyChannel, 3, 1
        @@Exit:
                        ret

BltExtractChannel24     ENDP

BltInjectChannel32      PROC PASCAL USES EBX ESI EDI, \ ; Source, Dest : ptr byte, aWidth,
                           aHeight : dword, WidthSource, WidthDest : dword
;
;     On entry:
;
;     EAX: Source
;     EDX: Dest
;     ECX: aWidth
;
                        LOCAL   PixelCount : dword

                        BltOpaque BltLoopCopyChannel, 1, 4
        @@Exit:
                        ret

BltInjectChannel32      ENDP

BltExtractChannel32     PROC PASCAL USES EBX ESI EDI, \ ; Source, Dest : ptr byte, aWidth,
                           aHeight : dword, WidthSource, WidthDest : dword
;
;     On entry:
;
;     EAX: Source
;     EDX: Dest
;     ECX: aWidth
;
                        LOCAL   PixelCount : dword

                        BltOpaque BltLoopCopyChannel, 4, 1
        @@Exit:
                        ret

BltExtractChannel32     ENDP

                        END
