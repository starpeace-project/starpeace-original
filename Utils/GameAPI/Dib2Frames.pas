unit Dib2Frames;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  uses
    Windows, Dibs, SpriteImages,
    ColorTrans, ColorTableMgr;

  // Dibs conversion

  function  FrameFromDib( DibHeader : PDib; DibPixels : pointer ) : TFrameImage;
  procedure AddDibToFrame( FrameImage : TFrameImage; DibHeader : PDib; DibPixels : pointer );

implementation

  uses
    Classes{$IFDEF MEMREPORTS}, MemoryReports{$ENDIF};

  var
    PaletteInfoMgr : TPaletteInfoManager;

  // Dibs conversion

  function FrameFromDib( DibHeader : PDib; DibPixels : pointer ) : TFrameImage;
    var
      Size     : cardinal;
      i        : cardinal;
      ImgFrame : TFrame;
      Buf      : pchar;
    begin
      with DibHeader^ do
        begin
          if not Assigned( DibPixels )
            then DibPixels := DibPtr( DibHeader );
          Result := TFrameImage.Create( biWidth, abs(biHeight) );
          Size   := biWidth * Abs( biHeight );
          GetMem( ImgFrame.pixels, Size );
          {$IFDEF MEMREPORTS}
          ReportMemoryAllocation(cSpriteAllocCause, Size);
          {$ENDIF}
          Result.AddFrame( ImgFrame);
          if DibIsTopDown( DibHeader )
            then
              for i := 0 to Result.Height - 1 do
                begin
                  Move( DibPixels^, pchar(ImgFrame.pixels)[Result.PixelOfs(0, i, 0)], biWidth);
                  inc( pchar(DibPixels), DwordAlign(biWidth) );
                end
            else
              for i := Result.Height - 1 downto 0 do
                begin
                  Move( DibPixels^, pchar(ImgFrame.pixels)[Result.PixelOfs(0, i, 0)], biWidth);
                  inc( pchar(DibPixels), DwordAlign(biWidth) );
                end;
          Result.TranspIndx := pbyte(ImgFrame.pixels)^;
          getmem( Buf, DibPaletteSize( DibHeader ) );
          Move( DibColors( DibHeader )^, Buf^, DibPaletteSize( DibHeader ) );
          Result.PaletteInfo := PaletteInfoMgr.AddPalette( pointer( Buf ), DibHeader.biClrUsed );
          Result.PaletteInfo.Owned := true;
          {$IFDEF MEMREPORTS}
          ReportMemoryAllocation(cSpriteAllocCause, 3*DibPaletteSize( DibHeader ) + sizeof(Result) + sizeof(Result.PaletteInfo));
          {$ENDIF}
        end;
    end;

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

initialization
  PaletteInfoMgr := TPaletteInfoManager.Create;

finalization
  PaletteInfoMgr.Free;

end.
