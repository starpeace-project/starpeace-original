unit GlassedBuffers;

// This is a patch! OK? This is the only way I found to draw glassed vector graphics.

interface

  uses
    Windows, SpriteImages, SpeedBmp, ColorTableMgr, GDI, Palettes;

  procedure CreateBuffer(Width, Height : integer);
  procedure ResizeBuffer(NewWidth, NewHeight : integer);
  procedure FreeBuffer;
  function  GetBuffer(Width, Height : integer) : TSpeedBitmap;
  procedure SetupBufferBackground(const ClipRect : TRect);
  procedure RenderBuffer(Dest : TSpeedBitmap; const ClipRect : TRect);

implementation

  uses
    Classes, Graphics, BitBlt, Dibs;

  var
    PalettedBuffer : TSpeedBitmap;
    FrameImage     : TFrameImage;
    RGBPalette     : TRGBPalette;
    PaletteInfo    : TPaletteInfo;

  procedure CreateBuffer(Width, Height : integer);
    var
      BuffPal : TBufferPalette;
      i       : integer;
    begin
      PalettedBuffer := TSpeedBitmap.CreateSized(Width, Height, 8);
      PalettedBuffer.TransparentColor := clBlack;
      PalettedBuffer.Canvas.Brush.Color := clBlack;
      PalettedBuffer.Canvas.FillRect(Rect(0, 0, Width - 1, Height - 1));
      FrameImage := TFrameImage.Create(Width, Height);
      FrameImage.NewFrames(1);
      FrameImage.FrameDelay[0] := 0;
      FrameImage.TranspIndx := PalettedBuffer.TransparentIndx;
      PaletteInfo := TPaletteInfo.Create;
      FrameImage.PaletteInfo := PaletteInfo;
      BuffPal := PalettedBuffer.BufferPalette;
      with BuffPal.Palette do
        begin
          for i := 0 to pred(NumberOfEntries) do
            begin
              RGBPalette[i].rgbRed := Entries[i].peRed;
              RGBPalette[i].rgbGreen := Entries[i].peGreen;
              RGBPalette[i].rgbBlue := Entries[i].peBlue;
              RGBPalette[i].rgbReserved := 0;
            end;
          PaletteInfo.AttachPalette(@RGBPalette, NumberOfEntries);
        end;
    end;

  procedure ResizeBuffer(NewWidth, NewHeight : integer);
    begin
      PalettedBuffer.NewSize(NewWidth, NewHeight, 8);
      PalettedBuffer.TransparentColor := clBlack;
      PalettedBuffer.Canvas.Brush.Color := clBlack;
      PalettedBuffer.Canvas.FillRect(Rect(0, 0, NewWidth - 1, NewHeight - 1));
      FrameImage.Free;
      FrameImage := TFrameImage.Create(NewWidth, NewHeight);
      FrameImage.NewFrames(1);
      FrameImage.FrameDelay[0] := 0;
      FrameImage.TranspIndx := PalettedBuffer.TransparentIndx;
      FrameImage.PaletteInfo := PaletteInfo;
    end;

  procedure FreeBuffer;
    begin
      PaletteInfo.Free;
      FrameImage.Free;
      PalettedBuffer.Free;
    end;

  function GetBuffer(Width, Height : integer) : TSpeedBitmap;
    begin
      if PalettedBuffer <> nil
        then
          begin
            if (PalettedBuffer.Width <> Width) or (PalettedBuffer.Height <> Height)
              then ResizeBuffer(Width, Height);
          end
        else CreateBuffer(Width, Height);
      Result := PalettedBuffer;
    end;

  procedure SetupBufferBackground(const ClipRect : TRect);
    var
      SrcWidth : integer;
    begin
      if PalettedBuffer.TopDown
        then SrcWidth := PalettedBuffer.StorageWidth
        else SrcWidth := -PalettedBuffer.StorageWidth;
      PalettedBuffer.Canvas.Brush.Color := clBlack;
      PalettedBuffer.Canvas.FillRect(ClipRect);
      BltCopyOpaque(PalettedBuffer.PixelAddr[ClipRect.Left, ClipRect.Top], FrameImage.PixelAddr[ClipRect.Left, ClipRect.Top, 0],
                    ClipRect.Right - ClipRect.Left, ClipRect.Bottom - ClipRect.Top, SrcWidth, FrameImage.StorageWidth);
    end;

  procedure RenderBuffer(Dest : TSpeedBitmap; const ClipRect : TRect);
    var
      SrcWidth : integer;
    begin
      if PalettedBuffer.TopDown
        then SrcWidth := PalettedBuffer.StorageWidth
        else SrcWidth := -PalettedBuffer.StorageWidth;
      BltCopyOpaque(PalettedBuffer.PixelAddr[ClipRect.Left, ClipRect.Top], FrameImage.PixelAddr[ClipRect.Left, ClipRect.Top, 0],
                    ClipRect.Right - ClipRect.Left, ClipRect.Bottom - ClipRect.Top, SrcWidth, FrameImage.StorageWidth);
      FrameImage.Draw(0, 0, 1, 0, ClipRect, Dest, nil);
    end;

end.
