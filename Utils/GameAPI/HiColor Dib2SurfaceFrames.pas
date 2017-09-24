unit Dib2SurfaceFrames;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  uses
    Windows, Dibs, SurfaceSpriteImages,
    ColorTrans, ColorTableMgr;

  // Dibs conversion

  function  SurfaceFrameFromDib(DibHeader : PDib; DibPixels : pointer) : TSurfaceFrameImage;
  //procedure AddDibToSurfaceFrame(FrameImage : TSurfaceFrameImage; DibHeader : PDib; DibPixels : pointer);

implementation

  uses
    Classes, DDrawD3DManager, GDI, BitBlt{$IFDEF MEMREPORTS}, MemoryReports{$ENDIF};

  // Dibs conversion

  function PackRGBQuad(const RGBQuad : TRGBQuad) : word;
    const
      rRight = 11;
      gRight = 5;
      rLeft  = 3;
      gLeft  = 3;
      bLeft  = 3;
    begin
      with RGBQuad do
        Result := ( (rgbRed   shr rLeft) shl rRight ) or
                  ( (rgbGreen shr gLeft) shl gRight ) or
                  ( rgbBlue shr bLeft );
    end;

  function SurfaceFrameFromDib(DibHeader : PDib; DibPixels : pointer) : TSurfaceFrameImage;
    var
      i          : cardinal;
      ImgFrame   : TFrame;
      rgbpalette : PRGBPalette;
      frame      : pchar;
      palinfo    : TPaletteInfo;
      Transcolor : word;
    begin
      with DibHeader^ do
        begin
          if not Assigned(DibPixels)
            then DibPixels := DibPtr(DibHeader);
          Result := TSurfaceFrameImage.Create(biWidth, abs(biHeight));
          ImgFrame.surface := DDrawD3DMgr.CreateTextureSurface(Result.Width, Result.Height, 16, true);
          {$IFDEF MEMREPORTS}
          //ReportMemoryAllocation(cSpriteAllocCause, Size);
          {$ENDIF}
          getmem(frame, biWidth*abs(biHeight));
          Result.AddFrame(ImgFrame);
          getmem(rgbpalette, DibPaletteSize(DibHeader));
          Move(DibColors(DibHeader)^, rgbpalette^, DibPaletteSize(DibHeader));
          palinfo := TPaletteInfo.Create;
          palinfo.AttachPalette(rgbpalette, DibPaletteSize(DibHeader) div sizeof(rgbpalette[0]));
          palinfo.Owned := true;
          palinfo.RequiredState([tsHiColor565TableValid]);
          Result.OwnsPalette := true;
          Result.LockFrame(0, true);
          try
            if DibIsTopDown(DibHeader)
              then
                for i := 0 to Result.Height - 1 do
                  begin
                    Move(DibPixels^, (frame + i*biWidth)^, biWidth);
                    inc(pchar(DibPixels), DwordAlign(biWidth));
                  end
              else
                for i := Result.Height - 1 downto 0 do
                  begin
                    Move(DibPixels^, (frame + i*biWidth)^, biWidth);
                    inc(pchar(DibPixels), DwordAlign(biWidth));
                  end;
            TransColor := PackRGBQuad(rgbpalette[byte(frame[0])]);
            fillchar(Result.PixelAddr[0, 0, 0]^, Result.StorageWidth[0]*Result.Height, TransColor);
            BltCopySourceCTT16(frame, Result.PixelAddr[0, 0, 0], Result.Width, Result.Height, byte(frame[0]), Result.Width, Result.StorageWidth[0], palinfo.HiColor565Table);
            //BltCopyOpaqueCTT16(frame, Result.PixelAddr[0, 0, 0], Result.Width, Result.Height, Result.Width, Result.StorageWidth[0], palinfo.HiColor565Table);
            Result.TranspIndx := Result.Pixel[0, 0, 0];
          finally
            Result.UnlockFrame(0);
            palinfo.Free;
            freemem(frame);
          end;

          {$IFDEF MEMREPORTS}
          //ReportMemoryAllocation(cSpriteAllocCause, 3*DibPaletteSize(DibHeader) + sizeof(Result) + sizeof(Result.PaletteInfo));
          {$ENDIF}
        end;
    end;

  {
  procedure AddDibToFrame( FrameImage : TFrameImage; DibHeader : PDib; DibPixels : pointer );
    var
      Buf    : TFrame;
      Size   : cardinal;
      i      : cardinal;
    begin
      with DibHeader^ do
        begin
          if not Assigned( DibPixels )
            then DibPixels := DibPtr( DibHeader );
          Size := biWidth * Abs( biHeight );
          GetMem( Buf.pixels, Size );
          if DibIsTopDown( DibHeader )
            then
              for i := 0 to FrameImage.Height - 1 do
                begin
                  Move( DibPixels^, pchar(Buf.pixels)[FrameImage.PixelOfs(0, i, 0)], biWidth);
                  inc( pchar(DibPixels), DwordAlign(biWidth) );
                end
            else
              for i := FrameImage.Height - 1 downto 0 do
                begin
                  Move( DibPixels^, pchar(Buf.pixels)[FrameImage.PixelOfs(0, i, 0)], biWidth);
                  inc( pchar(DibPixels), DwordAlign(biWidth) );
                end;
          FrameImage.AddFrame( Buf );
        end;
    end;
  }

end.
