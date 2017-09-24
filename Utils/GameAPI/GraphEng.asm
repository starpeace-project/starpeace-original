                INCLUDE ..\..\INC\GRAPH.INC

                LOCALS  @@

DATA            SEGMENT

                ASSUME  DS:DATA

                EXTRN   CurrentCanvas : DWORD
                EXTRN   DelphiCanvas  : DWORD

                EXTRN   CanvasHandle : WORD
                EXTRN   CanvasWidth  : DWORD
                EXTRN   CanvasHeight : DWORD

                EXTRN   CanvasPixels   : DWORD
                EXTRN   CanvasMixTable : DWORD

                EXTRN   PenColor   : BYTE
                EXTRN   BrushColor : BYTE

DATA            ENDS

IFDEF UseCODE32

                INCLUDE ..\..\INC\CODE32.INC

CODE            SEGMENT DWORD USE32

                ASSUME  CS:CODE
ELSE

                INCLUDE ..\..\INC\CODE16.INC

CODE            SEGMENT DWORD

                ASSUME  CS:CODE

                .386
ENDIF

                EXTRN   WinGBitBlt : FAR
                EXTRN   WinGStretchBlt : FAR
                EXTRN   DelphiHandle : NEAR

                PUBLIC  HorzLine

; HorzLine( anX, aY : integer; aWidth : word);

HorzLine        PROC    PASCAL FAR \
                          anX : WORD, aY : WORD, aWidth : WORD

                xor             edi, edi
                CanvasPixAddr   anX, aY
                mov             bx, aWidth
                mov             ecx, ebx
                mov             al, PenColor
                mov             ah, PenColor          ;!
                shr             ecx, 2
                rep             stosd
                mov             ecx, ebx
                and             ecx, 3
                rep             stosb
                fixret

HorzLine        ENDP

                PUBLIC  VertLine

; VertLine( anX, aY : integer; aHeight : word);

VertLine        PROC    PASCAL FAR \
                          anX : WORD, aY : WORD, aHeight : WORD

                xor             edi, edi
                xor             ecx, ecx
                CanvasPixAddr   anX, aY
                mov             cx, aHeight
                mov             al, PenColor
        @@Loop:
                mov             es:[edi], al
                add             edi, CanvasWidth
                dec             ecx
                jnz             @@Loop
                fixret

VertLine        ENDP

                PUBLIC  Line

; Line( x1, y1, x2, y2 : integer);

Line            PROC    PASCAL FAR \
                          x1 : WORD, y1 : WORD, x2 : WORD, y2 : WORD

                fixret

Line            ENDP

                PUBLIC  FillRect

; FillRect( x1, y1, x2, y2 : integer);

FillRect        PROC    PASCAL FAR \
                          x1 : WORD, y1 : WORD, x2 : WORD, y2 : WORD

                fixret

FillRect        ENDP

                PUBLIC  FrameRect

; FrameRect( x1, y1, x2, y2 : integer);

FrameRect       PROC    PASCAL FAR \
                          x1 : WORD, y1 : WORD, x2 : WORD, y2 : WORD

                fixret

FrameRect       ENDP

                PUBLIC  Rectangle

; Rectangle( x1, y1, x2, y2 : integer);

Rectangle       PROC    PASCAL FAR \
                          x1 : WORD, y1 : WORD, x2 : WORD, y2 : WORD

                fixret

Rectangle       ENDP

                PUBLIC  RoundRect

; RoundRect( x1, y1, x2, y2, x3, y3 : integer);

RoundRect       PROC    PASCAL FAR \
                          x1 : WORD, y1 : WORD, x2 : WORD, y2 : WORD, \
                          x3 : WORD, y3 : WORD

                fixret

RoundRect       ENDP

                PUBLIC  Arc

; Arc( x1, y1, x2, y2, x3, y3, x4, y4 : integer);

Arc             PROC    PASCAL FAR \
                          x1 : WORD, y1 : WORD, x2 : WORD, y2 : WORD, \
                          x3 : WORD, y3 : WORD, x4 : WORD, y4 : WORD

                fixret

Arc             ENDP

                PUBLIC  Chord

; Chord( x1, y1, x2, y2, x3, y3, x4, y4 : integer);

Chord           PROC    PASCAL FAR \
                          x1 : WORD, y1 : WORD, x2 : WORD, y2 : WORD, \
                          x3 : WORD, y3 : WORD, x4 : WORD, y4 : WORD

                fixret

Chord           ENDP

                PUBLIC  Ellipse

; Ellipse( x1, y1, x2, y2 : integer);

Ellipse         PROC    PASCAL FAR \
                          x1 : WORD, y1 : WORD, x2 : WORD, y2 : WORD

                fixret

Ellipse         ENDP

                PUBLIC  Pie

; Pie( x1, y1, x2, y2, x3, y3, x4, y4 : integer);

Pie             PROC    PASCAL FAR \
                          x1 : WORD, y1 : WORD, x2 : WORD, y2 : WORD, \
                          x3 : WORD, y3 : WORD, x4 : WORD, y4 : WORD

                fixret

Pie             ENDP

                PUBLIC  Polygon

; Polygon( const Points : array of TPoint);

Polygon         PROC    PASCAL FAR \
                          Count : WORD, Points : DWORD

                fixret

Polygon         ENDP

                PUBLIC  Polyline

; Polyline( const Points : array of TPoint);

Polyline        PROC    PASCAL FAR \
                          Count : WORD, Points : DWORD

                fixret

Polyline        ENDP

                PUBLIC  FloodFill

; FloodFill( anX, aY : integer; FillStyle : TFillStyle);

FloodFill       PROC    PASCAL FAR \
                          anX : WORD, aY : WORD, FillStyle : BYTE

                fixret

FloodFill       ENDP

                PUBLIC  TintRect

; TintRect( x1, y1, x2, y2 : integer);

TintRect        PROC    PASCAL FAR \
                          x1 : WORD, y1 : WORD, x2 : WORD, y2 : WORD

                fixret

TintRect        ENDP

                PUBLIC  UnzagRect

; UnzagRect( x1, y1, x2, y2 : integer);

UnzagRect       PROC    PASCAL FAR \
                          x1 : WORD, y1 : WORD, x2 : WORD, y2 : WORD

                fixret

UnzagRect       ENDP

                PUBLIC  ZoomSprite

; ZoomSprite( const Image : TSpriteImage; Dest : TSpriteImage);

ZoomSprite      PROC    PASCAL FAR \
                          Image : DWORD, Dest : DWORD

                fixret

ZoomSprite      ENDP

;
;          dx
;        ษอออออออออออออออออออป
;      d บ    |              บ
;      y บ    |              บ
;        บ----ษออออออออออออออฮออออออออออป
;        บ    บ              บ          บ
;        บ    บ  ImageArea   บ          บ
;        บ    บ              บ          บ
;        ศออออฮออออออออออออออผ          บ
;             บ                         บ
;             บ                         บ
;             บ                         บ
;             ศอออออออออออออออออออออออออผ
;

                PUBLIC  Copy

; Copy( Image : TSpriteImage; anX, aY : integer; aFrame : integer; ClipArea : TRect);

Copy            PROC    PASCAL FAR \
                          Image : DWORD, anX : WORD, aY : WORD, aFrame : WORD, \
                          ClipArea : DWORD

                LOCAL           ImageArea   : TRect
                LOCAL           DestWidth   : DWORD
                LOCAL           SourceWidth : DWORD

                USES            DS

                xor             esi, esi
                xor             edi, edi
                xor             ebx, ebx
                xor             ecx, ecx

                                ; ImageArea = (x, y, x + Size.X, y + Size.Y)
                lfs             di, Image
                CreateRect      ImageArea, anX, aY, fs:[di].sprSize
                                
                                ; IntersectRect( ImageArea, ClipArea)
                les             di, ClipArea
                IntersectRect   ImageArea, es:[di]

                mov             eax, ImageArea.A        ; B.x = B.x - A.x (ClipSize)
                sub             ImageArea.B, eax

                ClipCoords      ImageArea, anX, aY      ; A.x = (dx, dy)

                mov             bx, ImageArea.B.x       ; pixel counter

                mov             eax, CanvasWidth
                sub             eax, ebx
                mov             DestWidth, eax
                CanvasPixAddr   anX, aY                 ; ES:EDI = (x, y)

                mov             ax, fs:[si].sprSize.x   ; DS:ESI = (dx, dy)
                sub             eax, ebx
                mov             SourceWidth, eax
                SpritePixAddr   ImageArea.A, fs:[si], aFrame

                mov             bx, ImageArea.B.x       ; pixel counter
                mov             dx, ImageArea.B.y       ; scan lines

                mov             eax, ebx
                and             eax, 11b
                shr             ebx, 2

                align 4
        @@Loop:
                mov             ecx, ebx
                rep             movsd
                mov             ecx, eax
                rep             movsb

                add             esi, SourceWidth
                add             edi, DestWidth
                dec             edx                 ; line counter
                jnz             short @@Loop

        @@Exit:
                fixret

Copy            ENDP

PixelCopy       macro   indx
                mov     es:[edi + indx], al
                endm

PixelGlass      macro   indx
                mov     ah, es:[edi + indx]
                mov     al, gs:[ebx + eax]
                mov     es:[edi + indx], al
                endm

PixelShade      macro   indx
                mov     al, es:[edi + indx]
                mov     al, gs:[ebx + eax]
                mov     es:[edi + indx], al
                endm

PixelUnzag      macro   indx
                mov     ah, es:[edi + indx - 1]
                mov     al, gs:[ebx + eax]
                mov     es:[edi + indx], al
                endm

                PUBLIC  Draw

; Draw( Image : TSpriteImage; anX, aY : integer; aFrame : integer; ClipArea : TRect);

Draw            PROC    PASCAL FAR \
                          Image : DWORD, anX : WORD, aY : WORD, aFrame : WORD, \
                          ClipArea : DWORD

                LOCAL           ImageArea   : TRect
                LOCAL           DestWidth   : DWORD
                LOCAL           SourceWidth : DWORD
                LOCAL           bmpWidth    : DWORD

                USES            DS

                xor             esi, esi
                xor             edi, edi
                xor             ebx, ebx
                xor             ecx, ecx

                                ; ImageArea = (x, y, x + Size.X, y + Size.Y)
                lfs             di, Image
                CreateRect      ImageArea, anX, aY, fs:[di].sprSize
                                
                                ; IntersectRect( ImageArea, ClipArea)
                les             di, ClipArea
                IntersectRect   ImageArea, es:[di]

                mov             eax, ImageArea.A        ; B.x = B.x - A.x (ClipSize)
                sub             ImageArea.B, eax

                ClipCoords      ImageArea, anX, aY      ; A.x = (dx, dy)

                mov             bx, ImageArea.B.x       ; pixel counter

                mov             eax, CanvasWidth
                sub             eax, ebx
                mov             DestWidth, eax
                CanvasPixAddr   anX, aY                 ; ES:EDI = (x, y)

                mov             ax, fs:[si].sprSize.x   ; DS:ESI = (dx, dy)
                sub             eax, ebx
                mov             SourceWidth, eax
                SpritePixAddr   ImageArea.A, fs:[si], aFrame

                mov             bx, ImageArea.B.x       ; pixel counter
                mov             dx, ImageArea.B.y       ; scan lines
                xor             eax, eax                ; transparent color = 0

                mov             bmpWidth, ebx
                TransDoAction   PixelCopy

        @@Exit:
                fixret

Draw            ENDP

                PUBLIC  Glass

; Glass( Image : TSpriteImage; anX, aY : integer; aFrame : integer; ClipArea : TRect);

Glass           PROC    PASCAL FAR \
                          Image : DWORD, anX : WORD, aY : WORD, aFrame : WORD, \
                          ClipArea : DWORD

                LOCAL           ImageArea   : TRect
                LOCAL           DestWidth   : DWORD
                LOCAL           SourceWidth : DWORD
                LOCAL           bmpWidth    : DWORD

                USES            DS

                xor             esi, esi
                xor             edi, edi
                xor             ebx, ebx
                xor             ecx, ecx

                lgs             bx, CanvasMixTable
                push            ebx                     ; Save CanvasMixTable

                                ; ImageArea = (x, y, x + Size.X, y + Size.Y)
                lfs             di, Image
                CreateRect      ImageArea, anX, aY, fs:[di].sprSize
                                
                                ; IntersectRect( ImageArea, ClipArea)
                les             di, ClipArea
                IntersectRect   ImageArea, es:[di]

                mov             eax, ImageArea.A        ; B.x = B.x - A.x (ClipSize)
                sub             ImageArea.B, eax

                ClipCoords      ImageArea, anX, aY      ; A.x = (dx, dy)

                mov             bx, ImageArea.B.x       ; pixel counter

                mov             eax, CanvasWidth
                sub             eax, ebx
                mov             DestWidth, eax
                CanvasPixAddr   anX, aY                 ; ES:EDI = (x, y)

                mov             ax, fs:[si].sprSize.x   ; DS:ESI = (dx, dy)
                sub             eax, ebx
                mov             SourceWidth, eax
                SpritePixAddr   ImageArea.A, fs:[si], aFrame

                mov             bx, ImageArea.B.x       ; pixel counter
                mov             dx, ImageArea.B.y       ; scan lines

                mov             bmpWidth, ebx
                xor             eax, eax
                sub             bx, 256

                pop             ebx                     ; Restore CanvasMixTable
                TransDoAction   PixelGlass

        @@Exit:
                fixret

Glass           ENDP

                PUBLIC  Shade

; Shade( Image : TSpriteImage; anX, aY : integer; aFrame : integer; aColor : byte; ClipArea : TRect);

Shade           PROC    PASCAL FAR \
                          Image : DWORD, anX : WORD, aY : WORD, aFrame : WORD, \
                          aColor : BYTE, ClipArea : DWORD

                LOCAL           ImageArea   : TRect
                LOCAL           DestWidth   : DWORD
                LOCAL           SourceWidth : DWORD
                LOCAL           bmpWidth    : DWORD

                USES            DS

                xor             esi, esi
                xor             edi, edi
                xor             ebx, ebx
                xor             ecx, ecx

                lgs             bx, CanvasMixTable
                push            ebx                     ; Save CanvasMixTable

                                ; ImageArea = (x, y, x + Size.X, y + Size.Y)
                lfs             di, Image
                CreateRect      ImageArea, anX, aY, fs:[di].sprSize
                                
                                ; IntersectRect( ImageArea, ClipArea)
                les             di, ClipArea
                IntersectRect   ImageArea, es:[di]

                mov             eax, ImageArea.A        ; B.x = B.x - A.x (ClipSize)
                sub             ImageArea.B, eax

                ClipCoords      ImageArea, anX, aY      ; A.x = (dx, dy)

                mov             bx, ImageArea.B.x       ; pixel counter

                mov             eax, CanvasWidth
                sub             eax, ebx
                mov             DestWidth, eax
                CanvasPixAddr   anX, aY                 ; ES:EDI = (x, y)

                mov             ax, fs:[si].sprSize.x   ; DS:ESI = (dx, dy)
                sub             eax, ebx
                mov             SourceWidth, eax
                SpritePixAddr   ImageArea.A, fs:[si], aFrame

                mov             bx, ImageArea.B.x       ; pixel counter
                mov             dx, ImageArea.B.y       ; scan lines

                mov             bmpWidth, ebx
                xor             eax, eax
                sub             bx, 256

                pop             ebx                     ; Restore CanvasMixTable
                TransDoAction   PixelShade

        @@Exit:
                fixret

Shade           ENDP

                PUBLIC  Tint

; Tint( Image : TSpriteImage; anX, aY : integer; aFrame : integer; aColor : byte; ClipArea : TRect);

Tint            PROC    PASCAL FAR \
                          Image : DWORD, anX : WORD, aY : WORD, aFrame : WORD, \
                          aColor : BYTE, ClipArea : DWORD

                fixret

Tint            ENDP

                PUBLIC  Zoom

; Zoom( Image : TSpriteImage; anX, aY : integer; aFrame : integer; w, h : integer; ClipArea : TRect);

Zoom            PROC    PASCAL FAR \
                          Image : DWORD, anX : WORD, aY : WORD, aFrame : WORD, \
                          w : WORD, h : WORD, ClipArea : DWORD

                fixret

Zoom            ENDP

                PUBLIC  UnzagDraw

; UnzagDraw( Image : TSpriteImage; anX, aY : integer; aFrame : integer; ClipArea : TRect);

UnzagDraw       PROC    PASCAL FAR \
                          Image : DWORD, anX : WORD, aY : WORD, aFrame : WORD, \
                          ClipArea : DWORD

                LOCAL           ImageArea   : TRect
                LOCAL           DestWidth   : DWORD
                LOCAL           SourceWidth : DWORD
                LOCAL           bmpWidth    : DWORD

                USES            DS

                xor             esi, esi
                xor             edi, edi
                xor             ebx, ebx
                xor             ecx, ecx

                lgs             bx, CanvasMixTable
                push            ebx                     ; Save CanvasMixTable

                                ; ImageArea = (x, y, x + Size.X, y + Size.Y)
                lfs             di, Image
                CreateRect      ImageArea, anX, aY, fs:[di].sprSize
                                
                                ; IntersectRect( ImageArea, ClipArea)
                les             di, ClipArea
                IntersectRect   ImageArea, es:[di]

                mov             eax, ImageArea.A        ; B.x = B.x - A.x (ClipSize)
                sub             ImageArea.B, eax

                ClipCoords      ImageArea, anX, aY      ; A.x = (dx, dy)

                mov             bx, ImageArea.B.x       ; pixel counter

                mov             eax, CanvasWidth
                sub             eax, ebx
                mov             DestWidth, eax
                CanvasPixAddr   anX, aY                 ; ES:EDI = (x, y)

                mov             ax, fs:[si].sprSize.x   ; DS:ESI = (dx, dy)
                sub             eax, ebx
                mov             SourceWidth, eax
                SpritePixAddr   ImageArea.A, fs:[si], aFrame

                mov             bx, ImageArea.B.x       ; pixel counter
                mov             dx, ImageArea.B.y       ; scan lines
                xor             eax, eax                ; transparent color = 0

                mov             bmpWidth, ebx
                xor             eax, eax
                sub             bx, 256

                pop             ebx                     ; Restore CanvasMixTable
                UnzagDoAction   PixelCopy, PixelUnzag

        @@Exit:
                fixret

UnzagDraw       ENDP

                PUBLIC  UnzagZoom

; UnzagZoom( Image : TSpriteImage; anX, aY : integer; aFrame : integer; w, h : integer; ClipArea : TRect);

UnzagZoom       PROC    PASCAL FAR Image : DWORD, \
                          anX : WORD, aY : WORD, aFrame : WORD, \
                          w : WORD, h : WORD, ClipArea : DWORD

                fixret

UnzagZoom       ENDP

                PUBLIC  Paint

; procedure Paint;

Paint           PROC    PASCAL FAR

;     with Size do
;       WinGBitBlt( Canvas.Handle, 0, 0, x, y,
;                   Handle, 0, 0);

                call            DelphiHandle            ; Canvas.Handle
                push            ax

                mov             ecx, CanvasWidth
                xor             eax, eax
                mov             edx, CanvasHeight
                                                        ; (0, 0)
                push            eax
                                                        ; (w, h)
                push            cx
                push            dx
                                                        ; Handle
                push            CanvasHandle
                                                        ; (0, 0)
                push            eax
                call            WinGBitBlt

                fixret

Paint           ENDP

                PUBLIC  PaintRect

; procedure PaintRect( const ClipArea : TRect);

PaintRect       PROC    PASCAL FAR \
                          ClipArea : DWORD

;   var
;     ImageArea : TRect;
;   begin
;     CreateRect( ImageArea, 0, 0, Size);
;     IntersectRect( ImageArea, ClipArea);
;     with ImageArea, RectSize( ClipArea) do
;       WinGBitBlt( Canvas.Handle, Left, Top, X, Y,
;                   Handle, Left, Top);
                fixret

PaintRect       ENDP

                PUBLIC  ZoomPaint

; procedure ZoomPaint( const ClipArea : TRect; zx, zy : integer);

ZoomPaint       PROC    PASCAL FAR \
                          ClipArea : DWORD, zx : WORD, zy : WORD

;   var
;     ImageArea : TRect;
;   begin
;     CreateRect( ImageArea, 0, 0, Size);
;     IntersectRect( ImageArea, ClipArea);
;     with ImageArea, RectSize( ClipArea) do
;       WinGStretchBlt( Canvas.Handle, Left * zx, Top * zy, X * zx, Y * zy,
;                       Handle, Left, Top, X, y);
                fixret

ZoomPaint       ENDP

CODE            ENDS

                END

