unit BufferDraw;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  uses
    Windows, Buffer, SpeedBmp;

  // if you don't want any clipping pass nil as ClipArea:
  procedure BufferCopyGDI( Source, Dest : TSpeedBitmap; aX, aY : integer; ClipArea : PRect );
  procedure BufferCopyOpaque( Source, Dest : TBuffer; aX, aY : integer; ClipArea : PRect );

  // Warning: These are intended only for 8-bpp and 24-bpp bitmaps
  procedure BufferCopyTrans( Source, Dest : TBuffer; aX, aY : integer; ClipArea : PRect );
  procedure BufferCopyGlassed( Source, Dest : TBuffer; aX, aY : integer; ClipArea : PRect );
  procedure BufferCopyShaded( Source, Dest : TBuffer; aX, aY : integer; ClipArea : PRect );

  // These 2 are only for 8-bit bitmaps
  procedure BufferCopySourceCTT( Source, Dest : TBuffer; aX, aY : integer; ClipArea : PRect; Table : pointer );
  procedure BufferCopyDestCTT( Source, Dest : TBuffer; aX, aY : integer; ClipArea : PRect; Table : pointer );

  // Clipping engine ================================================================

  type
    TClipResults =
      record
        ImageRect : TRect;
        Size      : TPoint;
        x, y      : integer;
        dx, dy    : integer;
        SrcWidth  : integer;
        DstWidth  : integer;
      end;

  function DoClipping( Source, Dest : TBuffer; aX, aY : integer; ClipArea : PRect; var Results : TClipResults ) : boolean;

implementation

  uses
    Rects, BitBlt;

  function DoClipping( Source, Dest : TBuffer; aX, aY : integer; ClipArea : PRect; var Results : TClipResults ) : boolean;
    var
      ClipRect : TRect;
    begin
      with Results do
        begin
          if not Assigned( ClipArea )
            then
              begin
                ClipArea  := @ClipRect;
                ClipArea^ := Dest.ClientRect;
              end;
          Result := IntersectRect( ImageRect, RectFromBounds( aX, aY, Source.Width, Source.Height ), ClipArea^ );
          if Result
            then
              begin
                Size := RectSize( ImageRect );
                if aX < ImageRect.Left
                  then
                    begin
                      dx := ImageRect.Left - aX;
                      x  := ImageRect.Left;
                    end
                  else
                    begin
                      dx := 0;
                      x  := aX;
                    end;
                if aY < ImageRect.Top
                  then
                    begin
                      dy := ImageRect.Top - aY;
                      y  := ImageRect.Top;
                    end
                  else
                    begin
                      dy := 0;
                      y  := aY;
                    end;

                if Source.TopDown
                  then SrcWidth := Source.StorageWidth
                  else SrcWidth := -Source.StorageWidth;
                if Dest.TopDown
                  then DstWidth := Dest.StorageWidth
                  else DstWidth := -Dest.StorageWidth;
              end;
        end;
    end;

  procedure BufferCopyGDI( Source, Dest : TSpeedBitmap; aX, aY : integer; ClipArea : PRect );
    begin
      with Dest.Canvas do
        if Assigned( ClipArea )
          then Source.DrawOnDC( Handle, aX, aY, CopyMode )
          else Source.ClipDrawOnDC( Handle, aX, aY, ClipArea^, CopyMode );
    end;

  procedure BufferCopyOpaque( Source, Dest : TBuffer; aX, aY : integer; ClipArea : PRect );
    var
      Results : TClipResults;
    begin
      with Results do
        if DoClipping( Source, Dest, aX, aY, ClipArea, Results )
          then // In this case we don't have to bother with bitmap bits per pixel
            BltCopyOpaque( Source.PixelAddr[dx, dy], Dest.PixelAddr[x, y],
                           Size.x, Size.y, SrcWidth, DstWidth );
    end;

  procedure BufferCopyTrans( Source, Dest : TBuffer; aX, aY : integer; ClipArea : PRect );
    var
      Results : TClipResults;
    begin
      assert( ( Source.BitCount = Dest.BitCount ) and
              ( Source.BitCount in [8, 16, 24] ) and
              ( Dest.BitCount in [8, 24] ), 'Invalid bits per pixel in BufferDraw.BufferCopyTrans!!' );
      assert( Source.BitCount in [8, 24], 'Only 8 and 24 bits-per-pixel supported in BufferDraw.BufferCopyTrans!!' );

      with Results do
        if DoClipping( Source, Dest, aX, aY, ClipArea, Results )
          then
            case Source.BitCount of
              8: BltCopyTrans( Source.PixelAddr[dx, dy], Dest.PixelAddr[x, y], Source.TransparentIndx, Size.x, Size.y, SrcWidth, DstWidth );
              24: BltCopyTrans24( Source.PixelAddr[dx, dy], Dest.PixelAddr[x, y], Source.Width, Source.Height, Source.TransparentColor, 0, SrcWidth, DstWidth );
            end;
    end;

  procedure BufferCopyGlassed( Source, Dest : TBuffer; aX, aY : integer; ClipArea : PRect );
    var
      Results : TClipResults;
    begin
      assert( ( Source.BitCount = Dest.BitCount ) and
              ( Source.BitCount in [8, 16, 24] ) and
              ( Dest.BitCount in [8, 16, 24] ), 'Invalid bits per pixel in BufferDraw.BufferCopyGlassed!!' );
      assert( Source.BitCount in [8, 16, 24], 'Only 8 and 24 bits-per-pixel supported in BufferDraw.BufferCopyGlassed!!' );

      with Results do
        if DoClipping( Source, Dest, aX, aY, ClipArea, Results )
          then
            case Source.BitCount of
              8:
                BltCopyGlassed( Source.PixelAddr[dx, dy], Dest.PixelAddr[x, y], Source.TransparentIndx,
                                Size.x, Size.y, SrcWidth, DstWidth, nil );
              16:
                BltCopyGlassedCTT16( Source.PixelAddr[dx, dy], Dest.PixelAddr[x, y], Source.Width, Source.Height,
                                     Source.TransparentIndx, SrcWidth, DstWidth, nil);
{              24: BltCopyGlassed24( Source.PixelAddr[dx, dy], Dest.PixelAddr[x, y], Source.TransparentColor,
                                    Size.x, Size.y, SrcWidth, DstWidth, nil );}
            end;
    end;

  procedure BufferCopyShaded( Source, Dest : TBuffer; aX, aY : integer; ClipArea : PRect );
    var
      Results : TClipResults;
    begin
      assert( ( Source.BitCount = Dest.BitCount ) and
              ( Source.BitCount in [8] ), 'Invalid bits per pixel in BufferDraw.BufferCopyShaded!!' );
      assert( Source.BitCount in [8, 24], 'Only 8 and 24 bits-per-pixel supported in BufferDraw.BufferCopyShaded!!' );

      with Results do
        if DoClipping( Source, Dest, aX, aY, ClipArea, Results )
          then
            if Source.BitCount = 8
              then //BltCopyDestCTT( Source.PixelAddr[dx, dy], Dest.PixelAddr[x, y], Source.TransparentIndx,
                   //                Size.x, Size.y, SrcWidth, DstWidth, Dest.BufferPalette.MixTable )
              else BltCopyShaded24( Source.PixelAddr[dx, dy], Dest.PixelAddr[x, y], Source.TransparentColor,
                                    Size.x, Size.y, SrcWidth, DstWidth, nil );
    end;

  procedure BufferCopySourceCTT( Source, Dest : TBuffer; aX, aY : integer; ClipArea : PRect; Table : pointer );
    var
      Results : TClipResults;
    begin
      assert( Source.BitCount = 8, 'Only 8 bits-per-pixel source bitmaps supported in BufferDraw.BufferCopySourceCTT!!' );
      assert( Dest.BitCount in [8, 16, 24], 'Only 8, 16 or 24 bits-per-pixel dest bitmaps supported in BufferDraw.BufferCopySourceCTT!!' );

      with Results do
        if DoClipping( Source, Dest, aX, aY, ClipArea, Results )
          then
            case Dest.BitCount of
              8 :
                BltCopySourceCTT( Source.PixelAddr[dx, dy], Dest.PixelAddr[x, y], Source.TransparentIndx,
                                  Size.x, Size.y, SrcWidth, DstWidth, Table );
              16 :
                BltCopySourceCTT16( Source.PixelAddr[dx, dy], Dest.PixelAddr[x, y], Source.TransparentIndx,
                                    Size.x, Size.y, SrcWidth, DstWidth, Table );
{              24 :
                BltCopySourceCTT24( Source.PixelAddr[dx, dy], Dest.PixelAddr[x, y], Source.TransparentIndx,
                                    Size.x, Size.y, SrcWidth, DstWidth, Table );}
            end;
    end;

  procedure BufferCopyDestCTT( Source, Dest : TBuffer; aX, aY : integer; ClipArea : PRect; Table : pointer );
    var
      Results : TClipResults;
    begin
      assert( ( Source.BitCount = Dest.BitCount ) and
              ( Source.BitCount = 8 ), 'Only 8 bits-per-pixel supported in BufferDraw.BufferCopyDestCTT!!' );

      with Results do
        if DoClipping( Source, Dest, aX, aY, ClipArea, Results )
          then BltCopyDestCTT( Source.PixelAddr[dx, dy], Dest.PixelAddr[x, y], Source.TransparentIndx,
                               Size.x, Size.y, SrcWidth, DstWidth, Table );
    end;

end.
