unit SpriteUtils;

interface

  uses
    SpriteImages;

  type
    TFlipType = ( ftNone, ftHorizontal, ftVertical, ftBoth );

  function SpriteFlip( Source : TFrameImage; FlipType : TFlipType ) : TFrameImage;
  function SpriteStretch( Source : TFrameImage; NewWidth, NewHeight : cardinal ) : TFrameImage;
  function SpriteFlipStretch( Source : TFrameImage; NewWidth, NewHeight : cardinal; Flip : TFlipType ) : TFrameImage;

implementation

  uses
    Windows, Classes, BitBlt, Graphics, CanvasBmp, SpeedBmp;

  function SpriteFlip( Source : TFrameImage; FlipType : TFlipType ) : TFrameImage;
    begin
      Result := SpriteFlipStretch( Source, Source.Width, Source.Height, FlipType );
    end;

  function SpriteStretch( Source : TFrameImage; NewWidth, NewHeight : cardinal ) : TFrameImage;
    begin
      Result := SpriteFlipStretch( Source, NewWidth, NewHeight, ftNone );
    end;

  function SpriteFlipStretch( Source : TFrameImage; NewWidth, NewHeight : cardinal; Flip : TFlipType ) : TFrameImage;
    var
      i         : cardinal;
      SrcWidth  : cardinal;
      SrcBuffer : TSpeedBitmap;
      DstBuffer : TSpeedBitmap;
    begin
      SrcWidth := Source.Width;
      if ( cardinal(SrcWidth) <> NewWidth ) or ( cardinal(Source.Height) <> NewHeight )
        then
          begin
            SrcBuffer := TSpeedBitmap.CreateSized( SrcWidth, -abs(Source.Height), 8 );
            DstBuffer := TSpeedBitmap.CreateSized( NewWidth, -abs(NewHeight), 8 );
            try
              Result := TFrameImage.Create( NewWidth, NewHeight );
              try
                Result.NewFrames( Source.FrameCount );
                Result.TranspIndx := Source.TranspIndx;
                for i := 0 to Source.FrameCount - 1 do
                  begin
                    Result.FrameDelay[i] := Source.FrameDelay[i];
                    case Flip of
                      ftNone :
                        BltCopyOpaque( Source.PixelAddr[0, 0, i], SrcBuffer.ScanLines, SrcWidth, Source.Height, Source.StorageWidth, SrcBuffer.StorageWidth );
                      ftVertical :
                        BltCopyFlipVertical( Source.PixelAddr[0, 0, i], SrcBuffer.ScanLines, SrcWidth, Source.Height, Source.StorageWidth, SrcBuffer.StorageWidth );
                      ftHorizontal :
                        BltCopyFlipHorizontal( Source.PixelAddr[0, 0, i], SrcBuffer.ScanLines, SrcWidth, Source.Height, Source.StorageWidth, SrcBuffer.StorageWidth );
                      ftBoth :
                        BltCopyFlipBoth( Source.PixelAddr[0, 0, i], SrcBuffer.ScanLines, SrcWidth, Source.Height, Source.StorageWidth, SrcBuffer.StorageWidth );
                    end;
                    SrcBuffer.StretchDrawOnDC( DstBuffer.Canvas.Handle, DstBuffer.ClientRect, cmSrcCopy );
                    BltCopyOpaque( DstBuffer.ScanLines, Result.PixelAddr[0, 0, i], NewWidth, NewHeight, DstBuffer.StorageWidth, Result.StorageWidth );
                  end;
              except
                Result := nil;
                raise;
              end;
            finally
              SrcBuffer.Free;
              DstBuffer.Free;
            end;
          end
        else
          begin
            Result := TFrameImage.Create( NewWidth, NewHeight );
            try
              Result.NewFrames( Source.FrameCount );
              Result.TranspIndx := Source.TranspIndx;
              for i := 0 to Source.FrameCount - 1 do
                begin
                  Result.FrameDelay[i] := Source.FrameDelay[i];
                  case Flip of
                    ftNone :
                      BltCopyOpaque( Source.PixelAddr[0, 0, i], Result.PixelAddr[0, 0, i], SrcWidth, Source.Height, Source.StorageWidth, Result.StorageWidth );
                    ftVertical :
                      BltCopyFlipVertical( Source.PixelAddr[0, 0, i], Result.PixelAddr[0, 0, i], SrcWidth, Source.Height, Source.StorageWidth, Result.StorageWidth );
                    ftHorizontal :
                      BltCopyFlipHorizontal( Source.PixelAddr[0, 0, i], Result.PixelAddr[0, 0, i], SrcWidth, Source.Height, Source.StorageWidth, Result.StorageWidth );
                    ftBoth :
                      BltCopyFlipBoth( Source.PixelAddr[0, 0, i], Result.PixelAddr[0, 0, i], SrcWidth, Source.Height, Source.StorageWidth, Result.StorageWidth );
                  end;
                end;
            except
              raise;
            end;
          end;
    end;

end.
