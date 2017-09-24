                        PAGE    255, 255

                        PUBLIC  BltCopyGrid
                        PUBLIC  BltCopyMaskGrid
                        PUBLIC  BltCopyGrid24
                        PUBLIC  BltCopyMaskGrid24

                        INCLUDE bitblt.inc

; Grid routines:
; =============================================================================================

BltCopyGrid             PROC PASCAL USES ESI EDI EBX, \ ; Source, Dest : ptr byte, aWidth,
                                aHeight : dword, Transparent : dword, \
                                WidthSource, WidthDest : dword, StartWithX : byte
;
;     On entry:
;
;     EAX: Source
;     EDX: Dest
;     ECX: aWidth
;

                        or      ecx, ecx                        ; aWidth = 0?
                        jz      @@Exit
                        cmp     aHeight, 0                      ; aHeight = 0?
                        je      @@Exit

                        mov     edi, edx
                        mov     esi, eax

                        movzx   eax, StartWithX
                        dec     ax                              ; if SI = 1 -> 0    (not 0 = FFFF)
                                                ;         0 -> FFFF (not FFFF = 0)

                        sub     WidthSource, ecx ; bias these
                        mov     ebx, ecx                        ; save this for later
                        sub     WidthDest, ecx

                        mov     edx, eax
                        mov     ah, byte ptr Transparent

        @@Loop:
                        mov     ecx, ebx                        ; ECX is pixel counter
                        shr     ecx, 2

                        not     dx                              ; This will go jumping between 0 and FFFF in each line
                        or      dx, dx
                        jz      @@Same1o

                        ;
                        ; Here we do the loop x0x0
                        ;
        @@Same0x:
                        mov     al, [esi]
                        cmp     al, ah
                        jne     @@Diff0x

        @@Same2x:
                        mov     al, [esi + 2]
                        cmp     al, ah
                        jne     @@Diff2x

        @@Samex:
                        add     edi, 4
                        add     esi, 4
                        dec     ecx
                        jnz     @@Same0x
                        jmp     @@EndRunX

        @@DiffX:
                        mov     al, [esi]
                        cmp     al, ah
                        je      @@Same2x

        @@Diff0x:
                        mov     [edi], al
                        mov     al, [esi + 2]
                        cmp     al, ah
                        je      @@Samex

        @@Diff2x:
                        mov     [edi + 2], al

                        add     edi, 4
                        add     esi, 4
                        dec     ecx
                        jnz     @@DiffX

        @@EndRunX:
                        mov     ecx, ebx                        ; ECX is pixel counter
                        and     ecx, 3
                        jz      @@NextScan

                        ; Check the leftover pixels (x0x)
        @@LeftOver0x:
                        mov     al, [esi]
                        cmp     al, ah
                        jne     @@LeftOver2x
                        mov     [edi], al

        @@LeftOver2x:
                        cmp     cx, 1
                        jle     @@LeftOverx

                        mov     al, [esi + 2]
                        cmp     al, ah
                        jz      @@LeftOverx
                        mov     [edi + 2], al

        @@LeftOverx:
                        add     edi, ecx
                        add     esi, ecx
                        jmp     @@NextScan

                        ;
                        ; Here we do the loop 0x0x
                        ;
        @@DiffO:
                        mov     al, [esi + 1]
                        cmp     al, ah
                        je      @@Same3o

        @@Diff1o:
                        mov     [edi + 1], al
                        mov     al, [esi + 3]
                        cmp     al, ah
                        je      @@SameO

        @@Diff3o:
                        mov     [edi + 3], al

                        add     edi, 4
                        add     esi, 4
                        dec     ecx
                        jnz     @@DiffO
                        jmp     @@EndRunO

        @@Same1o:
                        mov     al, [esi + 1]
                        cmp     al, ah
                        jne     @@Diff1o

        @@Same3o:
                        mov     al, [esi + 3]
                        cmp     al, ah
                        jne     @@Diff3o

        @@SameO:
                        add     edi, 4
                        add     esi, 4
                        dec     ecx
                        jnz     @@Same1o

        @@EndRunO:
                        mov     ecx, ebx                        ; ECX is pixel counter
                        and     ecx, 3
                        jz      @@NextScan

                        ; Check the leftover pixels (0x0)
        @@LeftOver1o:
                        cmp     cx, 2
                        jle     @@LeftOverO

                        mov     al, [esi + 1]
                        cmp     al, ah
                        jz      @@LeftOverO
                        mov     [edi + 1], al

        @@LeftOverO:
                        add     edi, ecx
                        add     esi, ecx

        @@NextScan:
                        add     edi, WidthDest
                        add     esi, WidthSource

                        dec     aHeight                         ; line counter
                        jnz     @@Loop

        @@Exit:
                        ret

BltCopyGrid             ENDP

BltCopyMaskGrid         PROC PASCAL USES ESI EDI EBX, \ ; Source, Dest : ptr byte, aWidth,
                                aHeight : dword, Transparent : dword, \
                                WidthSource, WidthDest : dword, Color : byte, StartWithX : byte
;
;     On entry:
;
;     EAX: Source
;     EDX: Dest
;     ECX: aWidth
;

                        or      ecx, ecx                        ; aWidth = 0?
                        jz      @@Exit
                        cmp     aHeight, 0                      ; aHeight = 0?
                        je      @@Exit

                        mov     edi, edx
                        mov     esi, eax

                        movzx   eax, StartWithX
                        dec     ax                              ; if SI = 1 -> 0    (not 0 = FFFF)
                                                                ;         0 -> FFFF (not FFFF = 0)

                        sub     WidthSource, ecx ; bias these
                        mov     ebx, ecx                        ; save this for later
                        sub     WidthDest, ecx

                        mov     dl, Color
                        mov     ah, byte ptr Transparent

        @@Loop:
                        mov     ecx, ebx                        ; ECX is pixel counter
                        shr     ecx, 2

                        not     dx                              ; This will go jumping between 0 and FFFF in each line
                        or      dx, dx
                        jz      @@Same1o

                        ;
                        ; Here we do the loop x0x0
                        ;
        @@Same0x:
                        mov     al, [esi]
                        cmp     al, ah
                        jne     @@Diff0x

        @@Same2x:
                        mov     al, [esi + 2]
                        cmp     al, ah
                        jne     @@Diff2x

        @@Samex:
                        add     edi, 4
                        add     esi, 4
                        dec     ecx
                        jnz     @@Same0x
                        jmp     @@EndRunX

        @@DiffX:
                        mov     al, [esi]
                        cmp     al, ah
                        je      @@Same2x

        @@Diff0x:
                        mov     [edi], dl
                        mov     al, [esi + 2]
                        cmp     al, ah
                        je      @@Samex

        @@Diff2x:
                        mov     [edi + 2], dl

                        add     edi, 4
                        add     esi, 4
                        dec     ecx
                        jnz     @@DiffX

        @@EndRunX:
                        mov     ecx, ebx                        ; ECX is pixel counter
                        and     ecx, 3
                        jz      @@NextScan

                        ; Check the leftover pixels (x0x)
        @@LeftOver0x:
                        mov     al, [esi]
                        cmp     al, ah
                        jz      @@LeftOver2x
                        mov     [edi], ah

        @@LeftOver2x:
                        cmp     cx, 1
                        jle     @@LeftOverx

                        mov     al, [esi + 2]
                        cmp     al, ah
                        jz      @@LeftOverx
                        mov     [edi + 2], dl

        @@LeftOverx:
                        add     edi, ecx
                        add     esi, ecx
                        jmp     @@NextScan

                        ;
                        ; Here we do the loop 0x0x
                        ;
        @@DiffO:
                        mov     al, [esi + 1]
                        cmp     al, ah
                        je      @@Same3o

        @@Diff1o:
                        mov     [edi + 1], dl
                        mov     al, [esi + 3]
                        cmp     al, ah
                        je      @@SameO

        @@Diff3o:
                        mov     [edi + 3], dl

                        add     edi, 4
                        add     esi, 4
                        dec     ecx
                        jnz     @@DiffO
                        jmp     @@EndRunO

        @@Same1o:
                        mov     al, [esi + 1]
                        cmp     al, ah
                        jne     @@Diff1o

        @@Same3o:
                        mov     al, [esi + 3]
                        cmp     al, ah
                        jne     @@Diff3o

        @@SameO:
                        add     edi, 4
                        add     esi, 4
                        dec     ecx
                        jnz     @@Same1o

        @@EndRunO:
                        mov     ecx, ebx                        ; ECX is pixel counter
                        and     ecx, 3
                        jz      @@NextScan

                        ; Check the leftover pixels (0x0)
        @@LeftOver1x:
                        cmp     cx, 2
                        jle     @@LeftOverO

                        mov     al, [esi + 1]
                        cmp     al, ah
                        jz      @@LeftOverO
                        mov     [edi + 1], dl

        @@LeftOverO:
                        add     edi, ecx
                        add     esi, ecx

        @@NextScan:
                        add     edi, WidthDest
                        add     esi, WidthSource

                        dec     aHeight                         ; line counter
                        jnz     @@Loop

        @@Exit:
                        ret

BltCopyMaskGrid         ENDP

BltCopyGrid24           PROC PASCAL USES ESI EDI EBX, \ ; Source, Dest : ptr byte, aWidth,
                                aHeight : dword, Transparent : dword, \
                                WidthSource, WidthDest : dword, StartWithX : byte
;
;     On entry:
;
;     EAX: Source
;     EDX: Dest
;     ECX: aWidth
;

                        or      ecx, ecx                        ; aWidth = 0?
                        jz      @@Exit
                        cmp     aHeight, 0                      ; aHeight = 0?
                        je      @@Exit

                        mov     edi, edx
                        mov     esi, eax
                        mov     ebx, ecx                        ; save this for later

                        movzx   eax, StartWithX
                        dec     ax                              ; if SI = 1 -> 0    (not 0 = FFFF)
                                                                ;         0 -> FFFF (not FFFF = 0)

                        lea     ecx, [ecx * 2 + ecx]
                        sub     WidthSource, ecx ; bias these
                        sub     WidthDest, ecx

                        mov     edx, eax
        @@Loop:
                        mov     ecx, ebx                        ; ECX is pixel counter
                        shr     ecx, 2

                        not     dx                              ; This will go jumping between 0 and FFFF in each line
                        or      dx, dx
                        jz      @@Same1o

                        ;
                        ; Here we do the loop x0x0
                        ;
        @@Same0x:
                        mov     eax, [esi]
                        and     eax, mskColorKey
                        jnz     @@Diff0x

        @@Same2x:
                        mov     eax, [esi + 6]
                        and     eax, mskColorKey
                        jnz     @@Diff2x

        @@Samex:
                        add     edi, 12
                        add     esi, 12
                        dec     ecx
                        jnz     @@Same0x
                        jmp     @@EndRunX

        @@DiffX:
                        mov     eax, [esi]
                        and     eax, mskColorKey
                        jz      @@Same2x

        @@Diff0x:
                        mov     [edi], ax
                        shr     eax, 16
                        mov     [edi + 2], al
                        mov     eax, [esi + 6]
                        and     eax, mskColorKey
                        jz      @@Samex

        @@Diff2x:
                        mov     [edi + 6], ax
                        shr     eax, 16
                        mov     [edi + 8], ax

                        add     edi, 12
                        add     esi, 12
                        dec     ecx
                        jnz     @@DiffX

        @@EndRunX:
                        mov     ecx, ebx                        ; ECX is pixel counter
                        and     ecx, 3
                        jz      @@NextScan

                        ; Check the leftover pixels (x0x)
        @@LeftOver0x:
                        mov     eax, [esi]
                        and     eax, mskColorKey
                        jz      @@LeftOver2x
                        mov     [edi], ax
                        shr     eax, 16
                        mov     [edi + 2], al

        @@LeftOver2x:
                        cmp     cx, 1
                        jle     @@LeftOverx

                        mov     eax, [esi + 6]
                        and     eax, mskColorKey
                        jz      @@LeftOverx
                        mov     [edi + 6], ax
                        shr     eax, 16
                        mov     [edi + 8], al

        @@LeftOverx:
                        mov     eax, ecx
                        shl     ecx, 1
                        add     eax, ecx
                        add     edi, eax
                        add     esi, eax
                        jmp     @@NextScan

                        ;
                        ; Here we do the loop 0x0x
                        ;
        @@DiffO:
                        mov     eax, [esi + 3]
                        and     eax, mskColorKey
                        jz      @@Same3o

        @@Diff1o:
                        mov     [edi + 3], ax
                        shr     eax, 16
                        mov     [edi + 5], al
                        mov     eax, [esi + 9]
                        and     eax, mskColorKey
                        jz      @@SameO

        @@Diff3o:
                        mov     [edi + 9], ax
                        shr     eax, 16
                        mov     [edi + 11], al

                        add     edi, 12
                        add     esi, 12
                        dec     ecx
                        jnz     @@DiffO
                        jmp     @@EndRunO

        @@Same1o:
                        mov     eax, [esi + 3]
                        and     eax, mskColorKey
                        jnz     @@Diff1o

        @@Same3o:
                        mov     eax, [esi + 9]
                        and     eax, mskColorKey
                        jnz     @@Diff3o

        @@SameO:
                        add     edi, 12
                        add     esi, 12
                        dec     ecx
                        jnz     @@Same1o

        @@EndRunO:
                        mov     ecx, ebx                        ; ECX is pixel counter
                        and     ecx, 3
                        jz      @@NextScan

                        ; Check the leftover pixels (0x0)
        @@LeftOver1o:
                        cmp     cx, 2
                        jle     @@LeftOverO

                        mov     eax, [esi + 3]
                        and     eax, mskColorKey
                        jz      @@LeftOverO
                        mov     [edi + 3], ax
                        shr     eax, 16
                        mov     [edi + 5], al

        @@LeftOverO:
                        mov     eax, ecx
                        shl     ecx, 1
                        add     eax, ecx
                        add     edi, eax
                        add     esi, eax

        @@NextScan:
                        add     edi, WidthDest
                        add     esi, WidthSource

                        dec     aHeight                         ; line counter
                        jnz     @@Loop

        @@Exit:
                        ret

BltCopyGrid24           ENDP

BltCopyMaskGrid24       PROC PASCAL USES ESI EDI EBX, \ ; Source, Dest : ptr byte, aWidth,
                                aHeight : dword, Transparent : dword, \
                                WidthSource, WidthDest : dword, Color : dword, StartWithX : byte
;
;     On entry:
;
;     EAX: Source
;     EDX: Dest
;     ECX: aWidth
;
                        LOCAL   Count : dword
                        LOCAL   Flag  : word;

                        or      ecx, ecx                        ; aWidth = 0?
                        jz      @@Exit
                        cmp     aHeight, 0                      ; aHeight = 0?
                        je      @@Exit

                        mov     edi, edx
                        mov     esi, eax

                        movzx   eax, StartWithX
                        dec     ax                              ; if StartWithX = 1 -> 0    (not 0 = FFFF)
                                                                ;                 0 -> FFFF (not FFFF = 0)
                        mov     Flag, ax

                        mov     Count, ecx                      ; save this for later
                        shl     ecx, 1
                        add     ecx, Count
                        sub     WidthSource, ecx ; bias these
                        sub     WidthDest, ecx

                        mov     edx, Color
                        mov     ebx, Color
                        shr     edx, 16                         ; Now the color is stored in bx & dl

                        mov     ecx, Count                      ; ECX is pixel counter
                        shr     ecx, 2
                        jnz     @@Loop

        @@LoopSpecial:
                        xor     Flag, 0ffffh                    ; This will go jumping between 0 and FFFF in each line
                        jz      @@EndRunO
                        jmp     @@EndRunX

        @@Loop:
                        mov     ecx, Count                      ; ECX is pixel counter
                        shr     ecx, 2
                        jz      @@LoopSpecial

        @@LoopRow:
                        xor     Flag, 0ffffh                    ; This will go jumping between 0 and FFFF in each line
                        jz      @@Same1o

                        ;
                        ; Here we do the loop x0x0
                        ;
        @@Same0x:
                        mov     eax, [esi]
                        and     eax, mskColorKey
                        jnz     @@Diff0x

        @@Same2x:
                        mov     eax, [esi + 6]
                        and     eax, mskColorKey
                        jnz     @@Diff2x

        @@Samex:
                        add     edi, 12
                        add     esi, 12
                        dec     ecx
                        jnz     @@Same0x
                        jmp     @@EndRunX

        @@DiffX:
                        mov     eax, [esi]
                        and     eax, mskColorKey
                        jz      @@Same2x

        @@Diff0x:
                        and     [edi], bx
                        or      [edi + 2], dl
                        mov     eax, [esi + 6]
                        and     eax, mskColorKey
                        jz      @@Samex

        @@Diff2x:
                        and     [edi + 6], bx
                        or      [edi + 8], dl

                        add     edi, 12
                        add     esi, 12
                        dec     ecx
                        jnz     @@DiffX

        @@EndRunX:
                        mov     ecx, Count                      ; ECX is pixel counter
                        and     ecx, 3
                        jz      @@NextScan

                        ; Check the leftover pixels (x0x)
        @@LeftOver0x:
                        mov     eax, [esi]
                        and     eax, mskColorKey
                        jz      @@LeftOver2x

                        and     [edi], bx
                        or      [edi + 2], dl

        @@LeftOver2x:
                        cmp     cx, 1
                        jle     @@LeftOverx

                        mov     eax, [esi + 6]
                        and     eax, mskColorKey
                        jz      @@LeftOverx

                        and     [edi + 6], bx
                        or      [edi + 8], dl

        @@LeftOverx:
                        mov     eax, ecx
                        shl     ecx, 1
                        add     eax, ecx
                        add     edi, eax
                        add     esi, eax
                        jmp     @@NextScan

                        ;
                        ; Here we do the loop 0x0x
                        ;
        @@DiffO:
                        mov     eax, [esi + 3]
                        and     eax, mskColorKey
                        jz      @@Same3o

        @@Diff1o:
                        and     [edi + 3], bx
                        or      [edi + 5], dl
                        mov     eax, [esi + 9]
                        and     eax, mskColorKey
                        jz      @@SameO

        @@Diff3o:
                        and     [edi + 9], bx
                        or      [edi + 11], dl

                        add     edi, 12
                        add     esi, 12
                        dec     ecx
                        jnz     @@DiffO
                        jmp     @@EndRunO

        @@Same1o:
                        mov     eax, [esi + 3]
                        and     eax, mskColorKey
                        jnz     @@Diff1o

        @@Same3o:
                        mov     eax, [esi + 9]
                        and     eax, mskColorKey
                        jnz     @@Diff3o

        @@SameO:
                        add     edi, 12
                        add     esi, 12
                        dec     ecx
                        jnz     @@Same1o

        @@EndRunO:
                        mov     ecx, Count                      ; ECX is pixel counter
                        and     ecx, 3
                        jz      @@NextScan

                        ; Check the leftover pixels (0x0)
        @@LeftOver1o:
                        cmp     cx, 2
                        jle     @@LeftOverO

                        mov     eax, [esi + 3]
                        and     eax, mskColorKey
                        jz      @@LeftOverO
                        and     [edi + 3], bx
                        or      [edi + 5], dl

        @@LeftOverO:
                        mov     eax, ecx
                        shl     ecx, 1
                        add     eax, ecx
                        add     edi, eax
                        add     esi, eax

        @@NextScan:
                        add     edi, WidthDest
                        add     esi, WidthSource

                        dec     aHeight                         ; line counter
                        jnz     @@Loop

        @@Exit:
                        ret

BltCopyMaskGrid24       ENDP

                        END