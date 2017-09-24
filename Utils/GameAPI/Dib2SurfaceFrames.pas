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
    Classes, DDrawD3DManager{$IFDEF MEMREPORTS}, MemoryReports{$ENDIF};

  // Dibs conversion

  function SurfaceFrameFromDib(DibHeader : PDib; DibPixels : pointer) : TSurfaceFrameImage;
    var
      i          : cardinal;
      ImgFrame   : TFrame;
      rgbpalette : array [0..255] of TRGBQuad;
      palentries : array [0..255] of TPaletteEntry;
    begin
      with DibHeader^ do
        begin
          if not Assigned(DibPixels)
            then DibPixels := DibPtr(DibHeader);
          Result := TSurfaceFrameImage.Create(biWidth, abs(biHeight));
          ImgFrame.surface := DDrawD3DMgr.CreateTextureSurface(Result.Width, Result.Height, 8, true);
          {$IFDEF MEMREPORTS}
          //ReportMemoryAllocation(cSpriteAllocCause, Size);
          {$ENDIF}
          Result.AddFrame(ImgFrame);
          Result.LockFrame(0, true);
          try
            if DibIsTopDown(DibHeader)
              then
                for i := 0 to Result.Height - 1 do
                  begin
                    Move(DibPixels^, Result.PixelAddr[0, i, 0]^, biWidth);
                    inc(pchar(DibPixels), DwordAlign(biWidth));
                  end
              else
                for i := Result.Height - 1 downto 0 do
                  begin
                    Move(DibPixels^, Result.PixelAddr[0, i, 0]^, biWidth);
                    inc(pchar(DibPixels), DwordAlign(biWidth));
                  end;
            Result.TranspIndx := Result.Pixel[0, 0, 0];
          finally
            Result.UnlockFrame(0);
          end;
          Move(DibColors(DibHeader)^, rgbpalette[0], DibPaletteSize(DibHeader));
          for i := 0 to pred(DibPaletteSize(DibHeader) div sizeof(TRGBQuad)) do
            begin
              palentries[i].peRed := rgbpalette[i].rgbRed;
              palentries[i].peGreen := rgbpalette[i].rgbGreen;
              palentries[i].peBlue := rgbpalette[i].rgbBlue;
              palentries[i].peFlags := 0;
            end;
          Result.Palette := DDrawD3DMgr.CreatePalette(@palentries[0]);
          Result.OwnsPalette := true;
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
