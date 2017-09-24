unit SurfaceSpriteLoaders;

interface

  uses
    Classes, Windows, SysUtils, SurfaceSpriteImages;

  procedure LoadSurfaceFrameImage(out anImage : TSurfaceFrameImage; Stream : TStream);
  procedure LoadSurfaceFrameImageFromFile(out anImage : TSurfaceFrameImage; const Filename : string);
  procedure LoadSurfaceFrameImageFromResName(out anImage : TSurfaceFrameImage; Instance : THandle; const ResourceName : string);
  procedure LoadSurfaceFrameImageFromResId(out anImage : TSurfaceFrameImage; Instance : THandle; ResourceId : Integer);

implementation

  uses
    DirectDraw, ImageLoaders, GDI, Dibs, BitBlt, GifLoader, DDrawD3DManager{$IFDEF MEMREPORTS}, MemoryReports{$ENDIF};

  type
    TDisposalMethod = (dmNone, dmDoNotDispose, dmRestToBckgrnd, dmRestToPrev);

  procedure LoadSurfaceFrameImage(out anImage : TSurfaceFrameImage; Stream : TStream);
    var
      Images       : TImages;
      i            : integer;
      buffsize     : integer;
      buff1, buff2 : pchar;
      rgbpalette   : array [0..255] of TRGBQuad;
      palentries   : array [0..255] of TPaletteEntry;
      TransColor   : integer;
      dispmeth     : TDisposalMethod;
    begin
      Images := GetImageLoader( Stream, nil );
      try
        if Assigned( Images )
          then
            with Images, DibHeader^ do
              begin
                //assert( biBitCount = 8, 'Sprites must be 8-bit images in SpriteLoaders.LoadFrameImage!!' );
                LoadImages;
                anImage := TSurfaceFrameImage.Create(biWidth, abs(biHeight));
                with anImage do
                  begin
                    NewFrames(ImageCount);
                    move(DibColors(DibHeader)^, rgbpalette[0], DibPaletteSize(DibHeader));
                    for i := 0 to pred(DibPaletteSize(DibHeader) div sizeof(TRGBQuad)) do
                      begin
                        palentries[i].peRed := rgbpalette[i].rgbRed;
                        palentries[i].peGreen := rgbpalette[i].rgbGreen;
                        palentries[i].peBlue := rgbpalette[i].rgbBlue;
                        palentries[i].peFlags := 0;
                      end;
                    Palette := DDrawD3DMgr.CreatePalette(@palentries[0]);
                    OwnsPalette := true;
                    buffsize := anImage.ImageSize;
                    {$IFDEF MEMREPORTS}
                    //ReportMemoryAllocation(cSpriteAllocCause, 3*DibPaletteSize( DibHeader ) + sizeof(anImage) + sizeof(PalInfo));
                    {$ENDIF}
                    getmem( buff1, buffsize );
                    try
                      getmem( buff2, buffsize );
                      try
                        TransColor := Image[0].Transparent;
                        fillchar(buff1^, buffsize, TransColor);
                        for i := 0 to ImageCount - 1 do
                          begin
                            //!!assert( TransparentIndx = Transparent, 'Image was authored with differing transparent colors per frame, which is unsupported by sprite engine!!' );
                            if LockFrame(i, true)
                              then
                                try
                                  FrameDelay[i] := Image[i].Delay*10;
                                  Image[i].Decode(PixelAddr[0, 0, i], StorageWidth[i]);
                                  if Image[i] is TGifImageData
                                    then dispmeth := TDisposalMethod(TGifImageData(Image[i]).Disposal)
                                    else dispmeth := dmNone;
                                  case dispmeth of
                                    dmNone,dmDoNotDispose:
                                      begin
                                        BltCopyTrans(PixelAddr[0, 0, i], @buff1[Width*Image[i].Origin.y + Image[i].Origin.x],
                                                     Image[i].Width, Image[i].Height, Image[i].Transparent, StorageWidth[i], Width);
                                        BltCopyOpaque(buff1, PixelAddr[0, 0, i], Width, Height, Width, StorageWidth[i]);
                                      end;
                                    dmRestToBckgrnd:
                                      begin
                                        BltCopyTrans(PixelAddr[0, 0, i], @buff1[Width*Image[i].Origin.y + Image[i].Origin.x],
                                                     Image[i].Width, Image[i].Height, TransColor, StorageWidth[i], Width);
                                        BltCopyOpaque(buff1, PixelAddr[0, 0, i], Width, Height, Width, StorageWidth[i]);
                                        fillchar(buff1^, buffsize, TransColor);
                                      end;
                                    dmRestToPrev:
                                      begin
                                        move(buff1^, buff2^, buffsize);
                                        BltCopyTrans(PixelAddr[0, 0, i], @buff1[Width*Image[i].Origin.y + Image[i].Origin.x],
                                                     Image[i].Width, Image[i].Height, TransColor, StorageWidth[i], Width);
                                        BltCopyOpaque(buff1, PixelAddr[0, 0, i], Width, Height, Width, StorageWidth[i]);
                                        if i > 0
                                          then move(buff2^, buff1^, buffsize);
                                      end;
                                  end;
                                finally
                                  UnlockFrame(i);
                                end;
                          end;
                        if TransColor = -1 // Assume a transparent color
                          then
                            begin
                              if LockFrame(0, true)
                                then
                                  begin
                                    TranspIndx := anImage.Pixel[0, 0, 0];
                                    UnlockFrame(0);
                                  end;
                            end
                          else TranspIndx := TransColor;
                      finally
                        freemem(buff2);
                      end;
                    finally
                      freemem( buff1 );
                    end;
                  end;
              end
          else anImage := nil;
      finally
        Images.Free;
      end;
    end;

  procedure LoadSurfaceFrameImageFromFile(out anImage : TSurfaceFrameImage; const Filename : string);
    var
      Stream : TStream;
    begin
      Stream := TFileStream.Create( Filename, fmOpenRead or fmShareDenyWrite );
      try
        LoadSurfaceFrameImage(anImage, Stream);
      finally
        Stream.Free;
      end;
    end;

  procedure LoadSurfaceFrameImageFromResName(out anImage : TSurfaceFrameImage; Instance : THandle; const ResourceName : string);
    var
      Stream : TStream;
    begin
      Stream := TResourceStream.Create( Instance, ResourceName, RT_RCDATA );
      try
        LoadSurfaceFrameImage(anImage, Stream);
      finally
        Stream.Free;
      end;
    end;

  procedure LoadSurfaceFrameImageFromResId( out anImage : TSurfaceFrameImage; Instance : THandle; ResourceId : Integer);
    var
      Stream : TStream;
    begin
      Stream := TResourceStream.CreateFromID( Instance, ResourceId, RT_RCDATA );
      try
        LoadSurfaceFrameImage(anImage, Stream);
      finally
        Stream.Free;
      end;
    end;

end.
