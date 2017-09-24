unit SpriteToSurface;

interface

  uses
    Windows, DDraw, SpriteImages, ColorTables;

  procedure RenderSpriteToSurface(Sprite : TFrameImage; x, y : integer; aFrame, Alpha : cardinal; const ClipArea : TRect; const surfacedesc : TDDSurfaceDesc2; aPaletteInfo : TPaletteInfo);

implementation

  uses
    SysUtils, Rects, BitBlt;

  function BitCountFromDDPixFormat(const DDPixFormat : TDDPixelFormat) : integer;
    begin
      if DDPixFormat.dwRGBBitCount = 16
        then
          if DDPixFormat.dwGBitMask = $7e0
            then Result := 16
            else Result := 15
        else Result := DDPixFormat.dwRGBBitCount;
    end;

  procedure RenderSpriteToSurface(Sprite : TFrameImage; x, y : integer; aFrame, Alpha : cardinal; const ClipArea : TRect; const surfacedesc : TDDSurfaceDesc2; aPaletteInfo : TPaletteInfo);
    var
      ImageArea   : TRect;
      ClipSize    : TPoint;
      dx, dy      : integer;
      dstPitch    : integer;
      srcAddr     : pointer;
      dstAddr     : pointer;
      PaletteInfo : TPaletteInfo;
    begin
      if aFrame > Sprite.FrameCount
        then raise Exception.Create( 'Bad frame specified in TFrameImage.DrawGlassed!!' );

      with ImageArea do
        begin
          Left   := x;
          Right  := x + Sprite.Size.X;
          Top    := y;
          Bottom := y + Sprite.Size.Y;
        end;
      IntersectRect(ImageArea, ImageArea, ClipArea);
      ClipSize := RectSize(ImageArea);
      if x < ClipArea.Left
        then
          begin
            dx := ClipArea.Left - x;
            x  := ClipArea.Left;
          end
        else dx := 0;
      if y < ClipArea.Top
        then
          begin
            dy := ClipArea.Top - y;
            y  := ClipArea.Top;
          end
        else dy := 0;

      dstPitch := surfacedesc.lPitch;
      srcAddr := Sprite.PixelAddr[dx, dy, aFrame];
      dstAddr := pchar(surfacedesc.lpSurface) + surfacedesc.lPitch*y + x*(surfacedesc.ddpfPixelFormat.dwRGBBitCount div 8);

      PaletteInfo := aPaletteInfo;
      if PaletteInfo = nil
        then PaletteInfo := Sprite.Tables.MainTable;

      case BitCountFromDDPixFormat(surfacedesc.ddpfPixelFormat) of
        8 :
          if Alpha = 0
            then BltCopyTrans(srcAddr, dstAddr,
                              ClipSize.x, ClipSize.y, Sprite.TransparentIndx,
                              Sprite.FramePitch[ aFrame ], dstPitch)
            else
              begin
                if not (tsMixMatrixValid in PaletteInfo.ValidTables)
                  then
                    begin
                      OutputDebugString(pchar('Unassigned MixMatrix!!'));
                      PaletteInfo.RequiredTable(tsMixMatrixValid);
                    end;
                // In this case we ignore the value of alpha since in 8 bits 50/50 is the only alpha we support (Alpha=4)
                BltCopyBlend(srcAddr, dstAddr,
                             ClipSize.x, ClipSize.y, Sprite.TransparentIndx,
                             Sprite.FramePitch[ aFrame ], dstPitch, PaletteInfo.MixMatrix);
              end;
        15 :
          begin
            if not (tsHiColor555TableValid in PaletteInfo.ValidTables)
              then
                begin
                  OutputDebugString(pchar('Unassigned HiColor555Table!!'));
                  PaletteInfo.RequiredTable(tsHiColor555TableValid);
                end;
            BltCopySourceCTT16(srcAddr, dstAddr,
                               ClipSize.x, ClipSize.y, Sprite.TransparentIndx, Alpha,
                               Sprite.FramePitch[ aFrame ], dstPitch, PaletteInfo.ColorTable);
          end;
        16 :
          begin
            if not (tsHiColor565TableValid in PaletteInfo.ValidTables)
              then
                begin
                  OutputDebugString(pchar('Unassigned HiColor565Table!!'));
                  PaletteInfo.RequiredTable(tsHiColor565TableValid);
                end;
            BltCopySourceCTT16(srcAddr, dstAddr,
                               ClipSize.x, ClipSize.y, Sprite.TransparentIndx, Alpha,
                               Sprite.FramePitch[ aFrame ], dstPitch, PaletteInfo.ColorTable);
          end;
        24 :
          begin
            if not (tsTrueColorTableValid in PaletteInfo.ValidTables)
              then
                begin
                  OutputDebugString(pchar('Unassigned TrueColorTable!!'));
                  PaletteInfo.RequiredTable(tsTrueColorTableValid);
                end;
            BltCopySourceCTT24(srcAddr, dstAddr,
                               ClipSize.x, ClipSize.y, Sprite.TransparentIndx, Alpha,
                               Sprite.FramePitch[ aFrame ], dstPitch, PaletteInfo.ColorTable);
          end;
        32 :
          begin
            if not (tsTrueColorTableValid in PaletteInfo.ValidTables)
              then
                begin
                  OutputDebugString(pchar('Unassigned TrueColorTable!!'));
                  PaletteInfo.RequiredTable(tsTrueColorTableValid);
                end;
            BltCopySourceCTT32(srcAddr, dstAddr,
                               ClipSize.x, ClipSize.y, Sprite.TransparentIndx, Alpha,
                               Sprite.FramePitch[ aFrame ], dstPitch, PaletteInfo.ColorTable);
          end;
      end;
    end;

end.
