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
    DirectDraw, ImageLoaders, GDI, Dibs, BitBlt, GifLoader, DDrawD3DManager, D3DXCore, AxlDebug, ColorTableMgr
    {$IFDEF MEMREPORTS}, MemoryReports{$ENDIF};

  type
    TDisposalMethod = (dmNone, dmDoNotDispose, dmRestToBckgrnd, dmRestToPrev);

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

  procedure LoadSurfaceFrameImage(out anImage : TSurfaceFrameImage; Stream : TStream);
    var
      Images       : TImages;
      i            : integer;
      buffsize     : integer;
      buff1, buff2 : pchar;
      rgbpalette   : PRGBPalette;
      TransColor   : integer;
      dispmeth     : TDisposalMethod;
      frames       : array [0..1000] of pchar;
      framecnt     : integer;
      palinfo      : TPaletteInfo;
      TransColorHi : word;
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
                    framecnt := ImageCount;
                    for i := 0 to pred(framecnt) do
                      getmem(frames[i], biWidth*abs(biHeight));
                    getmem(rgbpalette, DibPaletteSize(DibHeader));
                    move(DibColors(DibHeader)^, rgbpalette^, DibPaletteSize(DibHeader));
                    palinfo := TPaletteInfo.Create;
                    palinfo.AttachPalette(rgbpalette, DibPaletteSize(DibHeader) div sizeof(rgbpalette[0]));
                    palinfo.Owned := true;
                    palinfo.RequiredState([tsHiColor565TableValid]);
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
                        TransColorHi := palinfo.HiColor565Table[TransColor];
                        fillchar(buff1^, buffsize, TransColor);
                        for i := 0 to ImageCount - 1 do
                          begin
                            //!!assert( TransparentIndx = Transparent, 'Image was authored with differing transparent colors per frame, which is unsupported by sprite engine!!' );
                            if LockFrame(i, true)
                              then
                                try
                                  FrameDelay[i] := Image[i].Delay*10;
                                  Image[i].Decode(frames[i], Width);
                                  if Image[i] is TGifImageData
                                    then dispmeth := TDisposalMethod(TGifImageData(Image[i]).Disposal)
                                    else dispmeth := dmNone;
                                  case dispmeth of
                                    dmNone,dmDoNotDispose:
                                      begin
                                        BltCopyTrans(frames[i], @buff1[Width*Image[i].Origin.y + Image[i].Origin.x],
                                                     Image[i].Width, Image[i].Height, Image[i].Transparent, Width, Width);
                                        BltCopyOpaque(buff1, frames[i], Width, Height, Width, Width);
                                      end;
                                    dmRestToBckgrnd:
                                      begin
                                        BltCopyTrans(frames[i], @buff1[Width*Image[i].Origin.y + Image[i].Origin.x],
                                                     Image[i].Width, Image[i].Height, TransColor, Width, Width);
                                        BltCopyOpaque(buff1, frames[i], Width, Height, Width, Width);
                                        fillchar(buff1^, buffsize, TransColor);
                                      end;
                                    dmRestToPrev:
                                      begin
                                        move(buff1^, buff2^, buffsize);
                                        BltCopyTrans(frames[i], @buff1[Width*Image[i].Origin.y + Image[i].Origin.x],
                                                     Image[i].Width, Image[i].Height, TransColor, Width, Width);
                                        BltCopyOpaque(buff1, frames[i], Width, Height, Width, Width);
                                        if i > 0
                                          then move(buff2^, buff1^, buffsize);
                                      end;
                                  end;
                                  fillchar(PixelAddr[0, 0, i]^, StorageWidth[i]*Height, TransColorHi);
                                  BltCopySourceCTT16(frames[i], PixelAddr[0, 0, i], Width, Height, TransColor, Width, StorageWidth[i], palinfo.HiColor565Table);
                                  //BltCopyOpaqueCTT16(frames[i], PixelAddr[0, 0, i], Width, Height, Width, StorageWidth[i], palinfo.HiColor565Table);
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
                          else TranspIndx := anImage.Pixel[0, 0, 0];
                        for i := 0 to pred(framecnt) do
                          freemem(frames[i]);
                        palinfo.Free;
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
