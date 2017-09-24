unit Images;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  uses
    Classes, Windows, Graphics, Dibs, Buffer,
    ColorTrans, ColorTableMgr;

  type
    PPixel = ^TPixel;
    TPixel = byte;        // 256 colors

  // TSpriteImage

  type
    PFrameDelayList = ^TFrameDelayList;
    TFrameDelayList = array[0..0] of word;

  type
    TSpriteImage =
      class
        public
          constructor Create( aWidth, aHeight : integer );

          constructor LoadFromStream( Stream : TStream );                                                                      virtual; abstract;
          procedure   SaveToStream( Stream : TStream );                                                                        virtual; abstract;
          constructor LoadFromFile( const FileName : string );                                                                 virtual;
          procedure   SaveToFile( const FileName : string );                                                                   virtual;

        protected
          fSize            : TPoint;
          fFrameCount      : integer;
          fFrameDelays     : PFrameDelayList;
          fTransparentIndx : byte;
          fPaletteInfo     : TPaletteInfo;

          procedure SetPixel( x, y : integer; aFrame : integer; Color : TPixel );                                              virtual; abstract;
          function  GetPixel( x, y : integer; aFrame : integer ) : TPixel;                                                     virtual; abstract;
          function  GetFrameDelay( aFrame : integer ) : integer;                                                               virtual;
          function  GetImageSize : integer;
          function  GetClientRect : TRect;
          procedure SetPaletteInfo( Value : TPaletteInfo );

        public
          property  PaletteInfo : TPaletteInfo                         read fPaletteInfo     write SetPaletteInfo;
          property  Pixel[ x, y : integer; aFrame : integer ] : TPixel read GetPixel         write SetPixel;
          property  FrameDelay[ aFrame : integer ] : integer           read GetFrameDelay;
          property  FrameCount : integer                               read fFrameCount;
          property  ClientRect : TRect                                 read GetClientRect;
          property  Size : TPoint                                      read fSize;
          property  Width : longint                                    read fSize.x;
          property  Height : longint                                   read fSize.y;
          property  ImageSize : integer                                read GetImageSize;
          property  TransparentIndx : byte                             read fTransparentIndx write fTransparentIndx;

          function  IsSolid( x, y : integer; aFrame : integer ) : boolean;
          function  PixelOfs( x, y : integer ) : integer;

          procedure Draw( x, y : integer; aFrame : integer; const ClipArea : TRect; Buffer : TBuffer );                        virtual; abstract;
          procedure DrawGlassed( x, y : integer; aFrame : integer; const ClipArea : TRect; Buffer : TBuffer );                 virtual; abstract;
      end;

  // TComboImage

  type
    PImageList = ^TImageList;
    TImageList = array[0..0] of TSpriteImage;

  type
    POffsetList = ^TOffsetList;
    TOffsetList = array[0..0] of TPoint;

  type
    TComboImage =
      class( TSpriteImage )
        protected
          fImages     : PImageList;
          fOffsets    : POffsetList;
          fImageCount : integer;

          procedure SetPixel( x, y : integer; aFrame : integer; Color : TPixel );                 override;
          function  GetPixel( x, y : integer; aFrame : integer ) : TPixel;                         override;

        public
          property ImageCount : integer read fImageCount;

          destructor  Destroy;                                                                    override;
          constructor LoadFromStream( Stream : TStream );                                         override;
          procedure   SaveToStream( Stream : TStream );                                           override;

          procedure DeleteImages( Indx : integer; Count : integer );
          procedure AddImages( Value : PImageList; Count : integer );
          procedure AddImage( Value : pointer; Offset : TPoint );

          procedure Draw( x, y : integer; aFrame : integer; const ClipArea : TRect; Buffer : TBuffer );        override;
          procedure DrawGlassed( x, y : integer; aFrame : integer; const ClipArea : TRect; Buffer : TBuffer ); override;
      end;

  // TFrameImage

  type
    PFrameList = ^TFrameList;
    TFrameList = array[0..0] of pointer;

  type
    TFrameImage =
      class( TSpriteImage )
        protected
          fFrames : PFrameList;

          procedure SetPixel( x, y : integer; aFrame : integer; Color : TPixel );                 override;
          function  GetPixel( x, y : integer; aFrame : integer ) : TPixel;                        override;

        public
          destructor  Destroy;                                                                    override;
          constructor LoadFromStream( Stream : TStream );                                         override;
          procedure   SaveToStream( Stream : TStream );                                           override;

          procedure DeleteFrames( Indx : integer; Count : integer );
          procedure AddFrames( Value : PFrameList; Count : integer );
          procedure AddFrame( Value : pointer );

          function  PixelAddr( x, y : integer; aFrame : integer ) : pointer;

          procedure Draw( x, y : integer; aFrame : integer; const ClipArea : TRect; Buffer : TBuffer );        override;
          procedure DrawGlassed( x, y : integer; aFrame : integer; const ClipArea : TRect; Buffer : TBuffer ); override;
      end;

implementation

  uses
    SysUtils, MemUtils, Rects, BitBlt;

  // TSpriteImage

  constructor TSpriteImage.Create( aWidth, aHeight : integer );
    begin
      inherited Create;
      fSize := Point( aWidth, aHeight );
    end;

  constructor TSpriteImage.LoadFromFile( const FileName : string );
    var
      Stream : TStream;
    begin
      Stream := TFileStream.Create( FileName, fmOpenRead or fmShareDenyWrite );
      try
        LoadFromStream( Stream );
      finally
        Stream.Free;
      end;
    end;

  procedure TSpriteImage.SaveToFile( const FileName : string );
    var
      Stream : TStream;
    begin
      Stream := TFileStream.Create( FileName, fmCreate );
      try
        SaveToStream( Stream );
      finally
        Stream.Free;
      end;
    end;

  function TSpriteImage.GetClientRect : TRect;
    begin
      with Result do
        begin
          TopLeft     := Point( 0, 0 );
          BottomRight := Size;
        end;
    end;

  function TSpriteImage.GetImageSize : integer;
    begin
      Result := Size.X * Size.Y;
    end;

  function TSpriteImage.GetFrameDelay( aFrame : integer ) : integer;
    begin
      Result := fFrameDelays[aFrame];
    end;

  procedure TSpriteImage.SetPaletteInfo( Value : TPaletteInfo );
    begin
      fPaletteInfo := Value;
    end;
    
  // TComboImage

  destructor TComboImage.Destroy;
    begin
      DeleteImages( 0, FrameCount );

      inherited;
    end;

  constructor TComboImage.LoadFromStream( Stream : TStream );
    begin
      //Stream.
    end;

  procedure TComboImage.SaveToStream( Stream : TStream );
    begin
      // !!!
    end;

  procedure TComboImage.DeleteImages( Indx : integer; Count : integer );
    var
      i : integer;
    begin
      assert( (ImageCount > 0 ) and (Indx >= 0 ) and (Indx < ImageCount ), 'Bad index or Count in TComboImage.DeleteImages' );

      if Indx + Count > ImageCount
        then Count := ImageCount - Indx;
      for i := Indx  to Indx + Count - 1 do
        fImages[i].Free;
      if Count <> ImageCount
        then Move( fImages[Indx + Count], fImages[Indx], Count * sizeof(pointer ) );
       ReallocMem( fImages, (ImageCount - Count ) * sizeof(pointer ) );
       Dec( fImageCount, Count );
    end;

  procedure TComboImage.AddImages( Value : PImageList; Count : integer );
    var
      i : integer;
    const
      ofsZero : TPoint = ( x : 0; y : 0 );
    begin
      ReallocMem( fImages, (ImageCount + Count ) * sizeof(TImageList) );
      ReallocMem( fOffsets, (ImageCount + Count ) * sizeof(TOffsetList) );
      for i := 0 to Count - 1 do
        begin
          fImages[ImageCount + i]  := Value[i];
          fOffsets[ImageCount + i] := ofsZero;
        end;
      Inc( fImageCount, Count );
    end;

  procedure TComboImage.AddImage( Value : pointer; Offset : TPoint );
    begin
      inc( fImageCount );

      ReallocMem( fImages, ImageCount * sizeof(TImageList) );
      ReallocMem( fOffsets, ImageCount * sizeof(TOffsetList) );

      fImages[ImageCount]  := Value;
      fOffsets[ImageCount] := Offset;
    end;

  procedure TComboImage.SetPixel( x, y : integer; aFrame : integer; Color : TPixel );
    begin
      fImages[0].SetPixel( x, y, aFrame, Color );
    end;

  function TComboImage.GetPixel( x, y : integer; aFrame : integer ) : TPixel;
    var
      i : integer;
    begin
      i := 0;
      repeat
        Result := fImages[i].GetPixel( x, y, aFrame );
        inc( i );
      until ( Result <> TransparentIndx ) or ( i > ImageCount );
    end;

  procedure TComboImage.Draw( x, y : integer; aFrame : integer; const ClipArea : TRect; Buffer : TBuffer );
    var
      i : integer;
    begin
      for i := ImageCount - 1 downto 0 do
        fImages[i].Draw( x, y, aFrame, ClipArea, Buffer );
    end;

  procedure TComboImage.DrawGlassed( x, y : integer; aFrame : integer; const ClipArea : TRect; Buffer : TBuffer );
    begin
    end;

  // TFrameImage

  destructor TFrameImage.Destroy;
    begin
      DeleteFrames( 0, FrameCount );

      inherited;
    end;

  constructor TFrameImage.LoadFromStream( Stream : TStream );
    var
      i       : integer;
      ImgSize : integer;
    begin
      Stream.ReadBuffer( fSize, sizeof( fSize ) );
      Stream.ReadBuffer( fFrameCount, sizeof( fFrameCount ) );
      ImgSize := ImageSize;
      GetMem( fFrames, sizeof(pointer ) * FrameCount );
      for i := 0 to FrameCount - 1 do
        begin
          GetMem( fFrames[i], ImgSize );
          Stream.ReadBuffer( fFrames[i]^, ImgSize );
        end;
    end;

  procedure TFrameImage.SaveToStream( Stream : TStream );
    var
      i       : integer;
      ImgSize : integer;
    begin
      Stream.WriteBuffer( fSize, sizeof( fSize ) );
      Stream.WriteBuffer( fFrameCount, sizeof( fFrameCount ) );
      ImgSize := ImageSize;
      for i := 0 to FrameCount - 1 do
        Stream.WriteBuffer( fFrames[i]^, ImgSize );
    end;

  procedure TFrameImage.DeleteFrames( Indx : integer; Count : integer );
    var
      i : integer;
    begin
      assert( (FrameCount > 0 ) and (Indx >= 0 ) and (Indx < FrameCount ), 'Bad index or Count in TFrameImage.DeleteFrames' );

      if Indx + Count > FrameCount
        then Count := FrameCount - Indx;
      for i := Indx  to Indx + Count - 1 do
        FreeMem( fFrames[i] );
      if Count <> FrameCount
        then Move( fFrames[Indx + Count], fFrames[Indx], Count * sizeof(pointer ) );
       ReallocMem( fFrames, (FrameCount - Count ) * sizeof(pointer ) );
       Dec( fFrameCount, Count );
    end;

  procedure TFrameImage.AddFrames( Value : PFrameList; Count : integer );
    var
      i : integer;
    begin
      ReallocMem( fFrames, (FrameCount + Count ) * sizeof(TFrameList ) );
      for i := 0 to Count - 1 do
        fFrames[FrameCount + i] := Value[i];
      Inc( fFrameCount, Count );
    end;

  procedure TFrameImage.AddFrame( Value : pointer );
    begin
      AddFrames( @Value, 1 );
    end;

  // TSpriteImage drawing

  function TSpriteImage.PixelOfs( x, y : integer ) : integer;
    begin
      Result := (y * Size.X + x );
    end;

  function TSpriteImage.IsSolid( x, y : integer; aFrame : integer ) : boolean;
    begin
      Result := Pixel[ x, y, aFrame ] <> TransparentIndx;
    end;

  // TFrameImage drawing

  function TFrameImage.PixelAddr( x, y : integer; aFrame : integer ) : pointer;
    begin
      Result := pchar(fFrames[aFrame] ) + (y * Size.X + x );
   end;

  procedure TFrameImage.SetPixel( x, y : integer; aFrame : integer; color : TPixel );
    begin
      PPixel( PixelAddr( x, y, aFrame ) )^ := color;
    end;

  function TFrameImage.GetPixel( x, y : integer; aFrame : integer ) : TPixel;
    begin
      Result := PPixel( PixelAddr( x, y, aFrame ) )^;
    end;

  procedure TFrameImage.Draw( x, y : integer; aFrame : integer; const ClipArea : TRect; Buffer : TBuffer );
    var
      ImageArea : TRect;
      ClipSize  : TPoint;
      dx, dy    : integer;
      DstWidth  : integer;
    begin
      assert( (aFrame >= 0 ) and (aFrame < fFrameCount ), 'Bad frame specified in TSpriteImage.Draw!!' );

      with ImageArea do
        begin
          Left   := x;
          Right  := x + Size.X;
          Top    := y;
          Bottom := y + Size.Y;
        end;
      IntersectRect( ImageArea, ImageArea, ClipArea );
      ClipSize := RectSize( ImageArea );
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

      if Buffer.TopDown
        then DstWidth := Buffer.StorageWidth
        else DstWidth := -Buffer.StorageWidth;
      case Buffer.BitCount of
        8 :
          BltCopyTrans( PixelAddr( dx, dy, aFrame ), Buffer.PixelAddr[x, y],
                        ClipSize.x, ClipSize.y, TransparentIndx, Size.X, DstWidth );
        16 :
          with PaletteInfo do
            begin
              RequiredState( [tsHiColorTableValid] );
              BltCopySourceCTT16( PixelAddr( dx, dy, aFrame ), Buffer.PixelAddr[x, y],
                                  ClipSize.x, ClipSize.y, TransparentIndx, Size.X, DstWidth, TabHiColor );
            end;
        24 :
          BltCopySourceCTT24( PixelAddr( dx, dy, aFrame ), Buffer.PixelAddr[x, y],
                              ClipSize.x, ClipSize.y, TransparentIndx, Size.X, DstWidth, PaletteInfo.RgbPalette );
        32 :
          with PaletteInfo do
            begin
              RequiredState( [tsTrueColorTableValid] );
              BltCopySourceCTT24( PixelAddr( dx, dy, aFrame ), Buffer.PixelAddr[x, y],
                                  ClipSize.x, ClipSize.y, TransparentIndx, Size.X, DstWidth, TabTrueColor );
            end;
      end;
    end;

  procedure TFrameImage.DrawGlassed( x, y : integer; aFrame : integer; const ClipArea : TRect; Buffer : TBuffer );
    var
      ImageArea : TRect;
      ClipSize  : TPoint;
      dx, dy    : integer;
      DstWidth  : integer;
    begin
      assert( (aFrame >= 0 ) and (aFrame < fFrameCount ), 'Bad frame specified in TSpriteImage.DrawGlassed!!' );

      with ImageArea do
        begin
          Left   := x;
          Right  := x + Size.X;
          Top    := y;
          Bottom := y + Size.Y;
        end;
      IntersectRect( ImageArea, ImageArea, ClipArea );
      ClipSize := RectSize( ImageArea );
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

      if Buffer.TopDown
        then DstWidth := Buffer.StorageWidth
        else DstWidth := -Buffer.StorageWidth;
      case Buffer.BitCount of
        8 :
          begin
            //assert( (Buffer.BufferPalette.MixTable <> nil ), 'MixTable is nil in TSpriteImage.DrawGlassed' );
            //BltCopyGlassed( PixelAddr( dx, dy, aFrame ), Buffer.PixelAddr[x, y],
            //                ClipSize.x, ClipSize.y, TransparentIndx, Size.X, DstWidth, PaletteInfo.MixTable );
          end;
        16 :
          with PaletteInfo do
            begin
              RequiredState( [tsHiColorUnpackedTableValid] );
              BltCopyGlassedCTT16( PixelAddr( dx, dy, aFrame ), Buffer.PixelAddr[x, y],
                                   ClipSize.x, ClipSize.y, TransparentIndx, Size.X, DstWidth, PaletteInfo.TabHiColorUnpacked );
            end;
        24 :
          begin
            BltCopyGlassedCTT24( PixelAddr( dx, dy, aFrame ), Buffer.PixelAddr[x, y],
                                 ClipSize.x, ClipSize.y, TransparentIndx, Size.X, DstWidth, PaletteInfo.RgbPalette );
          end;
      end;
    end;

end.
