unit DibDraw;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  uses
    Windows, Dibs;

  // GDI routines (Dib to Screen)
  // =============================================================================================

  function DibPaint( dc : HDC; x, y : integer; DibHeader : PDib; DibPixels : pointer; CopyMode : dword ) : integer;

  function DibBlt( dc : HDC; DstX, DstY : integer; DstWidth, DstHeight : integer;
                   DibHeader : PDib; DibPixels : pointer; SrcX, SrcY : integer; Rop : dword; wUsage : uint ) : integer;
  function StretchDibBlt( dc : HDC; DstX, DstY : integer; DstWidth, DstHeight : integer; DibHeader : PDib; DibPixels : pointer;
                          SrcX, SrcY : integer; SrcWidth, SrcHeight : integer; Rop : dword; wUsage : uint ) : integer;

  // Higher level stuff

  procedure DibClipStretchDraw( DibHeader : PDib; DibPixels : pointer; dc : HDC; const Rect, ClippingRect : TRect; aCopyMode : dword );

  // Dib to Dib (neither tested nor finished: nobody uses plain Dibs anymore)
  // =============================================================================================

  // if you don't want any clipping pass nil as ClipArea:
  procedure DibCopyOpaque( SourceHeader : PDib; SourcePixels : pointer; DestHeader : PDib; DestPixels : pointer;
                           x, y : integer; ClipArea : PRect );

  // 8 & 24-bit specific routines:
  function GetPixelAddr( Dib : PDib; Pixels : pointer; x, y : integer ) : pointer;

implementation

  uses
    Classes, Rects, BitBlt;

  // GDI stuff
  // ---------

  function DibPaint( dc : HDC; x, y : integer; DibHeader : PDib; DibPixels : pointer; CopyMode : dword ) : integer;
    begin
      if CopyMode = 0
        then CopyMode := SRCCOPY;
      Result := DibBlt( dc, x, y, 0, 0, DibHeader, DibPixels, 0, 0, CopyMode, 0 );
    end;

  function DibBlt( dc : HDC; DstX, DstY : integer; DstWidth, DstHeight : integer;
                   DibHeader : PDib; DibPixels : pointer; SrcX, SrcY : integer; Rop : dword; wUsage : uint ) : integer;
    begin
      if wUsage = 0
        then wUsage := DIB_RGB_COLORS;
      if DibPixels = nil
        then DibPixels := DibPtr( DibHeader );
      if ( DstWidth = -1 ) and ( DstHeight = -1 )
        then
          begin
            DstWidth  := DibHeader.biWidth;
            DstHeight := DibHeader.biHeight;
          end;
      Result := StretchDIBits( dc, DstX, DstY, DstWidth, DstHeight,
                                   SrcX, SrcY, DstWidth, DstHeight, DibPixels, PBitmapInfo( DibHeader )^, wUsage, Rop );
    end;

  function StretchDibBlt( dc : HDC; DstX, DstY : integer; DstWidth, DstHeight : integer; DibHeader : PDib; DibPixels : pointer;
                          SrcX, SrcY : integer; SrcWidth, SrcHeight : integer; Rop : dword; wUsage : uint ) : integer;
    begin
      if wUsage = 0
        then wUsage := DIB_RGB_COLORS;
      if DibPixels = nil
        then DibPixels := DibPtr( DibHeader );
      if ( SrcWidth = -1 ) and ( SrcHeight = -1 )
        then
          begin
            SrcWidth  := DibHeader.biWidth;
            SrcHeight := DibHeader.biHeight;
          end;
      if ( DstWidth < 0 ) and ( DstHeight < 0 )
        then
          begin
            DstWidth  := SrcWidth * -DstWidth;
            SrcHeight := SrcHeight * -DstHeight;
          end;
      Result := StretchDIBits( dc, DstX, DstY, DstWidth, DstHeight,
                                   SrcX, SrcY, SrcWidth, SrcHeight, DibPixels, PBitmapInfo( DibHeader )^, wUsage, Rop );
    end;

  // Higher stuff

  procedure DibClipDraw( DibHeader : PDib; DibPixels : pointer; dc : HDC; x, y : integer; const ClippingRect : TRect; aCopyMode : dword );
    var
      SrcOfs  : TPoint;
      DstSize : TPoint;
      DstRect : TRect;
      Height  : integer;
    begin
      with DstRect, DibHeader^ do
        begin
          Height  := abs( biHeight );
          DstRect := RectFromBounds( x, y, biWidth, Height );
          if IntersectRect( DstRect, DstRect, ClippingRect )
            then
              begin
                DstSize := RectSize( DstRect );
                if x < DstRect.Left
                  then
                    begin
                      SrcOfs.x := DstRect.Left - x;
                      x        := DstRect.Left;
                    end
                  else SrcOfs.x := 0;
                if y < DstRect.Top
                  then
                    begin
                      SrcOfs.y := DstRect.Top - y;
                      y        := DstRect.Top;
                    end
                  else SrcOfs.y := 0;

                DibBlt( dc, x, y, DstSize.x, DstSize.y, DibHeader, DibPixels, SrcOfs.x, SrcOfs.y, aCopyMode, DIB_PAL_COLORS );
              end;
        end;
    end;

  procedure DibClipStretchDraw( DibHeader : PDib; DibPixels : pointer; dc : HDC; const Rect, ClippingRect : TRect; aCopyMode : dword );
    var
      SrcOfs     : TPoint;
      SrcSize    : TPoint;
      DstRect    : TRect;
      DstSize    : TPoint;
      Height     : integer;
    begin
      with DibHeader^ do
        if IntersectRect( DstRect, ClippingRect, Rect )
          then
            begin
              DstSize := RectSize( DstRect );
              Height  := abs( biHeight );

              if Rect.Left < DstRect.Left
                then SrcOfs.x := (DstRect.Left - Rect.Left) * biWidth div (Rect.Right - Rect.Left)
                else SrcOfs.x := 0;
              SrcSize.x := DstSize.x * biWidth div (Rect.Right - Rect.Left);

              if Rect.Top < DstRect.Top
                then SrcOfs.y := (DstRect.Top - Rect.Top) * Height div (Rect.Bottom - Rect.Top)
                else SrcOfs.y := 0;
              SrcSize.y := DstSize.y * Height div (Rect.Bottom - Rect.Top);

              StretchDibBlt( dc, DstRect.Left, DstRect.Top, DstSize.x, DstSize.y, DibHeader, DibPixels, SrcOfs.x, SrcOfs.y, SrcSize.x, SrcSize.y, aCopyMode, DIB_PAL_COLORS );
            end;
    end;

  // DIB to DIB

  procedure DibCopyOpaque( SourceHeader : PDib; SourcePixels : pointer; DestHeader : PDib; DestPixels : pointer;
                     x, y : integer; ClipArea : PRect );
    var
      ImageRect : TRect;
      Size      : TPoint;
      dx, dy    : integer;
      ClipRect  : TRect;
      SrcWidth  : integer;
      DstWidth  : integer;
    begin
      assert( (SourceHeader.biBitCount = 8) and (DestHeader.biBitCount = 8), 'Bitmap in DibDraw.DibCopyOpaque doesn''t have 8 bits!!' );

      if not Assigned( ClipArea )
        then
          begin
            ClipArea  := @ClipRect;
            ClipArea^ := RectFromBounds( x, y, DestHeader.biWidth, abs( DestHeader.biHeight ) );
          end;
      if IntersectRect( ImageRect, RectFromBounds( x, y, SourceHeader.biWidth, abs( SourceHeader.biHeight ) ), ClipArea^ )
        then
          begin
            Size := RectSize( ImageRect );
            if x < ImageRect.Left
              then
                begin
                  dx := ImageRect.Left - x;
                  x  := ImageRect.Left;
                end
              else dx := 0;
            if y < ImageRect.Top
              then
                begin
                  dy := ImageRect.Top - y;
                  y  := ImageRect.Top;
                end
              else dy := 0;

            if SourceHeader.biHeight > 0
              then SrcWidth := DibWidthBytes( SourceHeader )
              else SrcWidth := -DibWidthBytes( SourceHeader );
            if DestHeader.biHeight > 0
              then DstWidth := DibWidthBytes( DestHeader )
              else DstWidth := -DibWidthBytes( DestHeader );

            assert( SourceHeader.biBitCount = DestHeader.biBitCount, 'Different bit depth in DibDraw.DibCopyOpaque!!' );
            BltCopyOpaque( DibPixelAddr( SourceHeader, SourcePixels, dx, dy ),
                           DibPixelAddr( DestHeader, DestPixels, x, y ), Size.x, Size.y, SrcWidth, DstWidth );
          end;
    end;

  // 8-bit specific routines
  // -----------------------

  function GetPixelAddr( Dib : PDib; Pixels : pointer; x, y : integer ) : pointer;
    begin
      assert( Dib.biBitCount = 8, 'Bitmap in DibDraw.GetPixelAddr doesn''t have 8 bits!!' );

      with Dib^ do
        if biHeight < 0
          then Result := pchar( Pixels ) + y * biWidth + x
          else Result := pchar( Pixels ) + ( biHeight - y ) * biWidth + x;
    end;
(*
  // This stuff is no longer supported, since I don't think anybody will use it any way...
  // Left here in case you want to make some cut&paste

  procedure DibCopyTrans( SourceHeader : PDib; SourcePixels : pointer; DestHeader : PDib; DestPixels : pointer;
                          Transparent : integer; x, y : integer; ClipArea : PRect );
    var
      ImageRect : TRect;
      Size      : TPoint;
      dx, dy    : integer;
      ClipRect  : TRect;
      SrcWidth  : integer;
      DstWidth  : integer;
    begin
      assert( (SourceHeader.biBitCount = 8) and (DestHeader.biBitCount = 8), 'Bitmap in DibDraw.DibCopyTrans doesn''t have 8 bits!!' );

      if not Assigned( ClipArea )
        then
          begin
            ClipArea  := @ClipRect;
            ClipArea^ := RectFromBounds( x, y, DestHeader.biWidth, abs( DestHeader.biHeight ) );
          end;
      if IntersectRect( ImageRect, RectFromBounds( x, y, SourceHeader.biWidth, abs( SourceHeader.biHeight ) ), ClipArea^ )
        then
          begin
            Size := RectSize( ImageRect );
            if x < ImageRect.Left
              then
                begin
                  dx := ImageRect.Left - x;
                  x  := ImageRect.Left;
                end
              else dx := 0;
            if y < ImageRect.Top
              then
                begin
                  dy := ImageRect.Top - y;
                  y  := ImageRect.Top;
                end
              else dy := 0;

            if SourceHeader.biHeight > 0
              then SrcWidth := DibWidthBytes( SourceHeader )
              else SrcWidth := -DibWidthBytes( SourceHeader );
            if DestHeader.biHeight > 0
              then DstWidth := DibWidthBytes( DestHeader )
              else DstWidth := -DibWidthBytes( DestHeader );

            assert( SourceHeader.biBitCount = DestHeader.biBitCount, 'Different bit depth in DibDraw.DibCopyTrans!!' );
            assert( SourceHeader.biBitCount in [8, 24], 'Unsupported bit depth in DibDraw.DibCopyTrans!!' );

            if SourceHeader.biBitCount = 8
              then BltCopyTrans( GetPixelAddr( SourceHeader, SourcePixels, dx, dy ),
                                 GetPixelAddr( DestHeader, DestPixels, x, y ),
                                 Transparent, Size.x, Size.y, SrcWidth, DstWidth )
              else BltCopyTrans24( GetPixelAddr( SourceHeader, SourcePixels, dx, dy ),
                                   GetPixelAddr( DestHeader, DestPixels, x, y ),
                                   Transparent, Size.x, Size.y, SrcWidth, DstWidth );
          end;
    end;

  procedure DibCopyGlassed( SourceHeader : PDib; SourcePixels : pointer; DestHeader : PDib; DestPixels : pointer;
                            Transparent : integer; x, y : integer; ClipArea : PRect; MixTable : pointer );
    var
      ImageRect : TRect;
      Size      : TPoint;
      dx, dy    : integer;
      ClipRect  : TRect;
      SrcWidth  : integer;
      DstWidth  : integer;
    begin
      assert( (SourceHeader.biBitCount = 8) and (DestHeader.biBitCount = 8), 'Bitmap in DibDraw.DibCopyGlassed doesn''t have 8 bits!!' );

      if not Assigned( ClipArea )
        then
          begin
            ClipArea  := @ClipRect;
            ClipArea^ := RectFromBounds( x, y, DestHeader.biWidth, abs( DestHeader.biHeight ) );
          end;
      if IntersectRect( ImageRect, RectFromBounds( x, y, SourceHeader.biWidth, abs( SourceHeader.biHeight ) ), ClipArea^ )
        then
          begin
            Size := RectSize( ImageRect );
            if x < ImageRect.Left
              then
                begin
                  dx := ImageRect.Left - x;
                  x  := ImageRect.Left;
                end
              else dx := 0;
            if y < ImageRect.Top
              then
                begin
                  dy := ImageRect.Top - y;
                  y  := ImageRect.Top;
                end
              else dy := 0;

            if SourceHeader.biHeight > 0
              then SrcWidth := DibWidthBytes( SourceHeader )
              else SrcWidth := -DibWidthBytes( SourceHeader );
            if DestHeader.biHeight > 0
              then DstWidth := DibWidthBytes( DestHeader )
              else DstWidth := -DibWidthBytes( DestHeader );

            assert( SourceHeader.biBitCount = DestHeader.biBitCount, 'Different bit depth in DibDraw.DibCopyGlassed!!' );
            assert( SourceHeader.biBitCount in [8, 24], 'Unsupported bit depth in DibDraw.DibCopyGlassed!!' );

            if SourceHeader.biBitCount = 8
              then BltCopyGlassed( GetPixelAddr( SourceHeader, SourcePixels, dx, dy ),
                                   GetPixelAddr( DestHeader, DestPixels, x, y ),
                                   Transparent, Size.x, Size.y, SrcWidth, DstWidth, MixTable )
              else BltCopyGlassed24( GetPixelAddr( SourceHeader, SourcePixels, dx, dy ),
                                     GetPixelAddr( DestHeader, DestPixels, x, y ),
                                     Transparent, Size.x, Size.y, SrcWidth, DstWidth, nil );
          end;
    end;

  procedure DibCopyShaded( SourceHeader : PDib; SourcePixels : pointer; DestHeader : PDib; DestPixels : pointer;
                           Transparent : integer; x, y : integer; ClipArea : PRect; ColorTransTable : pointer );
    var
      ImageRect : TRect;
      Size      : TPoint;
      dx, dy    : integer;
      ClipRect  : TRect;
      SrcWidth  : integer;
      DstWidth  : integer;
    begin
      assert( (SourceHeader.biBitCount = 8) and (DestHeader.biBitCount = 8), 'Bitmap in DibDraw.DibCopyGlassed doesn''t have 8 bits!!' );

      if not Assigned( ClipArea )
        then
          begin
            ClipArea  := @ClipRect;
            ClipArea^ := RectFromBounds( x, y, DestHeader.biWidth, abs( DestHeader.biHeight ) );
          end;
      if IntersectRect( ImageRect, RectFromBounds( x, y, SourceHeader.biWidth, abs( SourceHeader.biHeight ) ), ClipArea^ )
        then
          begin
            Size := RectSize( ImageRect );
            if x < ImageRect.Left
              then
                begin
                  dx := ImageRect.Left - x;
                  x  := ImageRect.Left;
                end
              else dx := 0;
            if y < ImageRect.Top
              then
                begin
                  dy := ImageRect.Top - y;
                  y  := ImageRect.Top;
                end
              else dy := 0;

            if SourceHeader.biHeight > 0
              then SrcWidth := DibWidthBytes( SourceHeader )
              else SrcWidth := -DibWidthBytes( SourceHeader );
            if DestHeader.biHeight > 0
              then DstWidth := DibWidthBytes( DestHeader )
              else DstWidth := -DibWidthBytes( DestHeader );

            assert( SourceHeader.biBitCount = DestHeader.biBitCount, 'Different bit depth in DibDraw.DibCopyGlassed!!' );
            assert( SourceHeader.biBitCount in [8, 24], 'Unsupported bit depth in DibDraw.DibCopyGlassed!!' );

            if SourceHeader.biBitCount = 8
              then BltCopyDestCTT( GetPixelAddr( SourceHeader, SourcePixels, dx, dy ),
                                   GetPixelAddr( DestHeader, DestPixels, x, y ),
                                   Transparent, Size.x, Size.y, SrcWidth, DstWidth, ColorTransTable )
              else BltCopyShaded24( GetPixelAddr( SourceHeader, SourcePixels, dx, dy ),
                                     GetPixelAddr( DestHeader, DestPixels, x, y ),
                                     Transparent, Size.x, Size.y, SrcWidth, DstWidth, nil );
          end;
    end;

  procedure DibCopyGrid( SourceHeader : PDib; SourcePixels : pointer; DestHeader : PDib; DestPixels : pointer;
                         Transparent : integer; x, y : integer; ClipArea : PRect );
    var
      ImageRect : TRect;
      Size      : TPoint;
      dx, dy    : integer;
      ClipRect  : TRect;
      SrcWidth  : integer;
      DstWidth  : integer;
    begin
      assert( (SourceHeader.biBitCount = 8) and (DestHeader.biBitCount = 8), 'Bitmap in DibDraw.DibCopyMaskGrid doesn''t have 8 bits!!' );

      if not Assigned( ClipArea )
        then
          begin
            ClipArea  := @ClipRect;
            ClipArea^ := RectFromBounds( x, y, DestHeader.biWidth, abs( DestHeader.biHeight ) );
          end;
      if IntersectRect( ImageRect, RectFromBounds( x, y, SourceHeader.biWidth, abs( SourceHeader.biHeight ) ), ClipArea^ )
        then
          begin
            Size := RectSize( ImageRect );
            if x < ImageRect.Left
              then
                begin
                  dx := ImageRect.Left - x;
                  x  := ImageRect.Left;
                end
              else dx := 0;
            if y < ImageRect.Top
              then
                begin
                  dy := ImageRect.Top - y;
                  y  := ImageRect.Top;
                end
              else dy := 0;

            if SourceHeader.biHeight > 0
              then SrcWidth := DibWidthBytes( SourceHeader )
              else SrcWidth := -DibWidthBytes( SourceHeader );
            if DestHeader.biHeight > 0
              then DstWidth := DibWidthBytes( DestHeader )
              else DstWidth := -DibWidthBytes( DestHeader );

            assert( SourceHeader.biBitCount = DestHeader.biBitCount, 'Different bit depth in DibDraw.DibCopyGrid!!' );
            assert( SourceHeader.biBitCount in [8, 24], 'Unsupported bit depth in DibDraw.DibCopyGrid!!' );

//          SrcPoint   := Point( Left - fShadowDistance.x, Top - fShadowDistance.y );
//          StartWithX := boolean( ( x and 1 ) and ( y and 1 ) ) or
//                        boolean( ( (x + 1) and 1 ) and ( (y + 1) and 1 ) );

            if SourceHeader.biBitCount = 8
              then BltCopyGrid( GetPixelAddr( SourceHeader, SourcePixels, dx, dy ),
                                GetPixelAddr( DestHeader, DestPixels, x, y ),
                                Transparent, Size.x, Size.y, SrcWidth, DstWidth, true )
              else BltCopyGrid24( GetPixelAddr( SourceHeader, SourcePixels, dx, dy ),
                                  GetPixelAddr( DestHeader, DestPixels, x, y ),
                                  Transparent, Size.x, Size.y, SrcWidth, DstWidth, true );
          end;
    end;

  procedure DibCopyMaskGrid( SourceHeader : PDib; SourcePixels : pointer; DestHeader : PDib; DestPixels : pointer; Color : integer;
                             Transparent : integer; x, y : integer; ClipArea : PRect );
    var
      ImageRect : TRect;
      Size      : TPoint;
      dx, dy    : integer;
      ClipRect  : TRect;
      SrcWidth  : integer;
      DstWidth  : integer;
    begin
      assert( (SourceHeader.biBitCount = 8) and (DestHeader.biBitCount = 8), 'Bitmap in DibDraw.DibCopyMaskGrid doesn''t have 8 bits!!' );

      if not Assigned( ClipArea )
        then
          begin
            ClipArea  := @ClipRect;
            ClipArea^ := RectFromBounds( x, y, DestHeader.biWidth, abs( DestHeader.biHeight ) );
          end;
      if IntersectRect( ImageRect, RectFromBounds( x, y, SourceHeader.biWidth, abs( SourceHeader.biHeight ) ), ClipArea^ )
        then
          begin
            Size := RectSize( ImageRect );
            if x < ImageRect.Left
              then
                begin
                  dx := ImageRect.Left - x;
                  x  := ImageRect.Left;
                end
              else dx := 0;
            if y < ImageRect.Top
              then
                begin
                  dy := ImageRect.Top - y;
                  y  := ImageRect.Top;
                end
              else dy := 0;

            if SourceHeader.biHeight > 0
              then SrcWidth := DibWidthBytes( SourceHeader )
              else SrcWidth := -DibWidthBytes( SourceHeader );
            if DestHeader.biHeight > 0
              then DstWidth := DibWidthBytes( DestHeader )
              else DstWidth := -DibWidthBytes( DestHeader );

            assert( SourceHeader.biBitCount = DestHeader.biBitCount, 'Different bit depth in DibDraw.DibCopyMaskGrid!!' );
            assert( SourceHeader.biBitCount in [8, 24], 'Unsupported bit depth in DibDraw.DibCopyMaskGrid!!' );

//          SrcPoint   := Point( Left - fShadowDistance.x, Top - fShadowDistance.y );
//          StartWithX := boolean( ( x and 1 ) and ( y and 1 ) ) or
//                        boolean( ( (x + 1) and 1 ) and ( (y + 1) and 1 ) );

            if SourceHeader.biBitCount = 8
              then BltCopyMaskGrid( GetPixelAddr( SourceHeader, SourcePixels, dx, dy ),
                                    GetPixelAddr( DestHeader, DestPixels, x, y ),
                                    Transparent, Size.x, Size.y, SrcWidth, DstWidth, Color, true )
              else BltCopyMaskGrid24( GetPixelAddr( SourceHeader, SourcePixels, dx, dy ),
                                      GetPixelAddr( DestHeader, DestPixels, x, y ),
                                      Transparent, Size.x, Size.y, SrcWidth, DstWidth, Color, true );
          end;
    end;

  procedure DibCopySourceCTT( SourceHeader : PDib; SourcePixels : pointer; DestHeader : PDib; DestPixels : pointer;
                              Transparent : integer; x, y : integer; ClipArea : PRect; ColorTransTable : pointer );
    var
      ImageRect : TRect;
      Size      : TPoint;
      dx, dy    : integer;
      ClipRect  : TRect;
      SrcWidth  : integer;
      DstWidth  : integer;
    begin
      assert( (SourceHeader.biBitCount = 8) and (DestHeader.biBitCount = 8), 'Bitmap in DibDraw.DibCopyMaskGrid doesn''t have 8 bits!!' );
      
      if not Assigned( ClipArea )
        then
          begin
            ClipArea  := @ClipRect;
            ClipArea^ := RectFromBounds( x, y, DestHeader.biWidth, abs( DestHeader.biHeight ) );
          end;
      if IntersectRect( ImageRect, RectFromBounds( x, y, SourceHeader.biWidth, abs( SourceHeader.biHeight ) ), ClipArea^ )
        then
          begin
            Size := RectSize( ImageRect );
            if x < ImageRect.Left
              then
                begin
                  dx := ImageRect.Left - x;
                  x  := ImageRect.Left;
                end
              else dx := 0;
            if y < ImageRect.Top
              then
                begin
                  dy := ImageRect.Top - y;
                  y  := ImageRect.Top;
                end
              else dy := 0;

            if SourceHeader.biHeight > 0
              then SrcWidth := DibWidthBytes( SourceHeader )
              else SrcWidth := -DibWidthBytes( SourceHeader );
            if DestHeader.biHeight > 0
              then DstWidth := DibWidthBytes( DestHeader )
              else DstWidth := -DibWidthBytes( DestHeader );

            assert( SourceHeader.biBitCount = DestHeader.biBitCount, 'Different bit depth in DibDraw.DibCopyMaskGrid!!' );
            assert( SourceHeader.biBitCount = 8, 'Unsupported bit depth in DibDraw.DibCopyMaskGrid!!' );

            BltCopySourceCTT( GetPixelAddr( SourceHeader, SourcePixels, dx, dy ),
                              GetPixelAddr( DestHeader, DestPixels, x, y ),
                              Transparent, Size.x, Size.y, SrcWidth, DstWidth, ColorTransTable );
          end;
    end;

  procedure DibCopyDestCTT( SourceHeader : PDib; SourcePixels : pointer; DestHeader : PDib; DestPixels : pointer;
                            Transparent : integer; x, y : integer; ClipArea : PRect; ColorTransTable : pointer );
    var
      ImageRect : TRect;
      Size      : TPoint;
      dx, dy    : integer;
      ClipRect  : TRect;
      SrcWidth  : integer;
      DstWidth  : integer;
    begin
      assert( (SourceHeader.biBitCount = 8) and (DestHeader.biBitCount = 8), 'Bitmap in DibDraw.DibCopyMaskGrid doesn''t have 8 bits!!' );

      if not Assigned( ClipArea )
        then
          begin
            ClipArea  := @ClipRect;
            ClipArea^ := RectFromBounds( x, y, DestHeader.biWidth, abs( DestHeader.biHeight ) );
          end;
      if IntersectRect( ImageRect, RectFromBounds( x, y, SourceHeader.biWidth, abs( SourceHeader.biHeight ) ), ClipArea^ )
        then
          begin
            Size := RectSize( ImageRect );
            if x < ImageRect.Left
              then
                begin
                  dx := ImageRect.Left - x;
                  x  := ImageRect.Left;
                end
              else dx := 0;
            if y < ImageRect.Top
              then
                begin
                  dy := ImageRect.Top - y;
                  y  := ImageRect.Top;
                end
              else dy := 0;

            if SourceHeader.biHeight > 0
              then SrcWidth := DibWidthBytes( SourceHeader )
              else SrcWidth := -DibWidthBytes( SourceHeader );
            if DestHeader.biHeight > 0
              then DstWidth := DibWidthBytes( DestHeader )
              else DstWidth := -DibWidthBytes( DestHeader );

            assert( SourceHeader.biBitCount = DestHeader.biBitCount, 'Different bit depth in DibDraw.DibCopyMaskGrid!!' );
            assert( SourceHeader.biBitCount = 8, 'Unsupported bit depth in DibDraw.DibCopyMaskGrid!!' );

            BltCopyDestCTT( GetPixelAddr( SourceHeader, SourcePixels, dx, dy ),
                            GetPixelAddr( DestHeader, DestPixels, x, y ),
                            Transparent, Size.x, Size.y, SrcWidth, DstWidth, ColorTransTable );
          end;
    end;
*)
end.
