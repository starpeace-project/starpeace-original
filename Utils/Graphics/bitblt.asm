                        PAGE    255, 255

                        PUBLIC  BltCopyOpaque
                        PUBLIC  BltCopyTrans
                        PUBLIC  BltCopySourceCTT
                        PUBLIC  BltCopyDestCTT
                        PUBLIC  BltCopyGlassed

                        INCLUDE bitblt.inc

; 8-bit specific routines:
; =============================================================================================

; >>>>>>>>>>>>>>>>>>>> Opaque blts >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

BltCopyOpaque           PROC PASCAL USES ESI EDI, \ ; Source, Dest : ptr byte, aWidth,
                           aHeight : dword, WidthSource, WidthDest : dword
;
;     On entry:
;
;     EAX: Source
;     EDX: Dest
;     ECX: aWidth
;

                        LOCAL   PixelCount : dword

BltLoopCopy             macro

                        mov     ecx, PixelCount
                        shr     ecx, 2
                        rep     movsd
                        mov     ecx, PixelCount
                        and     ecx, 11b
                        rep     movsb

                        endm

                        BltOpaque BltLoopCopy
        @@Exit:
                        ret

BltCopyOpaque           ENDP

BltCopyTrans            PROC PASCAL USES ESI EDI, \ ; Source, Dest : ptr byte, aWidth,
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

BltCopyTransWrite       macro   Indx:REQ

                        mov     [edi + Indx], al

                        endm

                        BltInit8
                        BltTrans8 BltCopyTransWrite, 1
        @@Exit:
                        ret

BltCopyTrans            ENDP

BltCopySourceCTT        PROC PASCAL USES ESI EDI EBX, \ ; Source, Dest : ptr byte, aWidth,
                                aHeight : dword, Transparent : dword, \
                                WidthSource, WidthDest : dword, Info : dword
;
;     On entry:
;
;     EAX: Source
;     EDX: Dest
;     ECX: aWidth
;

                        LOCAL   PixelCount : dword

BltCopySourceCTTWrite   macro   Indx:REQ

                        mov     al, [ebx + eax]
                        mov     [edi + Indx], al

                        endm

                        BltInit8
                        mov     ebx, Info
                        or      ebx, ebx
                        jz      @@Exit

                        BltTrans8  BltCopySourceCTTWrite, 1

        @@Exit:
                        ret

BltCopySourceCTT        ENDP

BltCopyDestCTT          PROC PASCAL USES ESI EDI EBX, \ ; Source, Dest : ptr byte, aWidth,
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

BltCopyDestCTTWrite     macro   Indx:REQ

                        mov     al, [edi + Indx]
                        mov     al, [ebx + eax]
                        mov     [edi + Indx], al

                        endm

                        BltInit8
                        mov     ebx, Info
                        or      ebx, ebx
                        jz      @@Exit

                        BltTrans8 BltCopyDestCTTWrite, 1

        @@Exit:
                        ret

BltCopyDestCTT          ENDP

BltCopyGlassed          PROC PASCAL USES ESI EDI EBX, \ ; Source, Dest : ptr byte, aWidth,
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

BltCopyGlassedWrite     macro   Indx:REQ

                        mov     ah, [edi + 1]
                        mov     al, [ebx + eax]
                        mov     [edi + 1], al

                        endm

                        BltInit8
                        mov     ebx, Info
                        or      ebx, ebx
                        jz      @@Exit

                        BltTrans8 BltCopyGlassedWrite, 1

        @@Exit:
                        ret

BltCopyGlassed          ENDP

                        END

