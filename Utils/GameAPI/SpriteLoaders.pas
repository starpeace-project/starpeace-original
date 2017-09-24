unit SpriteLoaders;

interface

  uses
    Classes, Windows, SysUtils,
    SpriteImages, ImageLoaders;

  procedure LoadFrameImage( out anImage : TFrameImage; Stream : TStream );
  procedure LoadFrameImageFromFile( out anImage : TFrameImage; const Filename : string );
  procedure LoadFrameImageFromResName( out anImage : TFrameImage; Instance : THandle; const ResourceName : string );
  procedure LoadFrameImageFromResId( out anImage : TFrameImage; Instance : THandle; ResourceId : Integer );

implementation

  uses
    Dibs, ColorTableMgr, GDI, BitBlt, GifLoader{$IFDEF MEMREPORTS}, MemoryReports{$ENDIF};

  type
    TDisposalMethod = (dmNone, dmDoNotDispose, dmRestToBckgrnd, dmRestToPrev);

  procedure LoadFrameImage( out anImage : TFrameImage; Stream : TStream );
    var
      Images       : TImages;
      i            : integer;
      buffsize     : integer;
      buff1, buff2 : pchar;
      palbuf       : pchar;
      PalInfo      : TPaletteInfo;
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
                anImage := TFrameImage.Create( biWidth, abs(biHeight) );
                with anImage do
                  begin
                    NewFrames( ImageCount );
                    getmem( palbuf, DibPaletteSize( DibHeader ) );
                    Move( DibColors( DibHeader )^, palbuf^, DibPaletteSize( DibHeader ) );
                    PalInfo := TPaletteInfo.Create;
                    PalInfo.AttachPalette( PRGBPalette(palbuf), biClrUsed );
                    PalInfo.Owned := true;
                    anImage.PaletteInfo := PalInfo;
                    anImage.OwnsPalette := true;
                    buffsize := anImage.ImageSize;
                    {$IFDEF MEMREPORTS}
                    ReportMemoryAllocation(cSpriteAllocCause, 3*DibPaletteSize( DibHeader ) + sizeof(anImage) + sizeof(PalInfo));
                    {$ENDIF}
                    getmem( buff1, buffsize );
                    try
                      getmem( buff2, buffsize );
                      try
                        TransColor := Image[0].Transparent;
                        fillchar( buff1^, buffsize, TransColor );
                        for i := 0 to ImageCount - 1 do
                          begin
                            //!!assert( TransparentIndx = Transparent, 'Image was authored with differing transparent colors per frame, which is unsupported by sprite engine!!' );
                            FrameDelay[i] := Image[i].Delay*10;
                            Image[i].Decode( PixelAddr[ 0, 0, i ], StorageWidth );
                            if Image[i] is TGifImageData
                              then dispmeth := TDisposalMethod(TGifImageData(Image[i]).Disposal)
                              else dispmeth := dmNone;
                            case dispmeth of
                              dmNone,dmDoNotDispose:
                                begin
                                  BltCopyTrans(PixelAddr[0, 0, i], @buff1[Width*Image[i].Origin.y + Image[i].Origin.x],
                                               Image[i].Width, Image[i].Height, Image[i].Transparent, StorageWidth, Width);
                                  BltCopyOpaque(buff1, PixelAddr[0, 0, i], Width, Height, Width, StorageWidth);
                                end;
                              dmRestToBckgrnd:
                                begin
                                  BltCopyTrans(PixelAddr[0, 0, i], @buff1[Width*Image[i].Origin.y + Image[i].Origin.x],
                                               Image[i].Width, Image[i].Height, TransColor, StorageWidth, Width);
                                  BltCopyOpaque(buff1, PixelAddr[0, 0, i], Width, Height, Width, StorageWidth);
                                  fillchar(buff1^, buffsize, TransColor);
                                end;
                              dmRestToPrev:
                                begin
                                  move(buff1^, buff2^, buffsize);
                                  BltCopyTrans(PixelAddr[0, 0, i], @buff1[Width*Image[i].Origin.y + Image[i].Origin.x],
                                               Image[i].Width, Image[i].Height, TransColor, StorageWidth, Width);
                                  BltCopyOpaque(buff1, PixelAddr[0, 0, i], Width, Height, Width, StorageWidth);
                                  if i > 0
                                    then move(buff2^, buff1^, buffsize);
                                end;
                            end;
                          end;
                        if TransColor = -1 // Assume a transparent color
                          then TranspIndx := anImage.Pixel[0,0,0]
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

  procedure LoadFrameImageFromFile( out anImage : TFrameImage; const Filename : string );
    var
      Stream : TStream;
    begin
      Stream := TFileStream.Create( Filename, fmOpenRead or fmShareDenyWrite );
      try
        LoadFrameImage( anImage, Stream );
      finally
        Stream.Free;
      end;
    end;

  procedure LoadFrameImageFromResName( out anImage : TFrameImage; Instance : THandle; const ResourceName : string );
    var
      Stream : TStream;
    begin
      Stream := TResourceStream.Create( Instance, ResourceName, RT_RCDATA );
      try
        LoadFrameImage( anImage, Stream );
      finally
        Stream.Free;
      end;
    end;

  procedure LoadFrameImageFromResId( out anImage : TFrameImage; Instance : THandle; ResourceId : Integer );
    var
      Stream : TStream;
    begin
      Stream := TResourceStream.CreateFromID( Instance, ResourceId, RT_RCDATA );
      try
        LoadFrameImage( anImage, Stream );
      finally
        Stream.Free;
      end;
    end;

end.
