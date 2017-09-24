unit SpriteImages;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  uses
    Classes, Windows, Graphics, Dibs, Buffer,
    ColorTrans, ColorTableMgr, Filters;

  type
    PPixel = ^TPixel;
    TPixel = byte;        // 256 colors

  // TSpriteImage

  type
    TSpriteImage =
      class
        public
          constructor Create( aWidth, aHeight : integer );

          constructor LoadFromStream( Stream : TStream );                                                                      virtual; abstract;
          destructor  Destroy;                                                                                                 override;

          procedure   SaveToStream( Stream : TStream );                                                                        virtual; abstract;
          constructor LoadFromFile( const FileName : string );                                                                 virtual;
          procedure   SaveToFile( const FileName : string );                                                                   virtual;

        private
          fRefCount : integer;
        public
          function AddRef : integer;
          function ReleaseRef : integer;

        protected
          fSize             : TPoint;
          fFrameCount       : integer;
          fTranspIndx       : byte;
          fPaletteInfo      : TPaletteInfo;
          fOwnsPalette      : boolean;
          fFilteredPalettes : TList;

          function  GetStorageWidth : integer;                                                                                 virtual; abstract;
          procedure SetPixel( x, y : integer; aFrame : integer; Color : TPixel );                                              virtual; abstract;
          function  GetPixel( x, y : integer; aFrame : integer ) : TPixel;                                                     virtual; abstract;
          procedure SetFrameDelay( aFrame : integer; Delay : integer );                                                        virtual; abstract;
          function  GetFrameDelay( aFrame : integer ) : integer;                                                               virtual; abstract;
          function  GetImageSize : integer;
          function  GetClientRect : TRect;
          procedure SetPaletteInfo( Value : TPaletteInfo );

        public
          property  PaletteInfo : TPaletteInfo                           read fPaletteInfo       write SetPaletteInfo;
          property  OwnsPalette : boolean                                read fOwnsPalette       write fOwnsPalette;
          property  Pixel[ x, y : integer; aFrame : integer ] : TPixel   read GetPixel           write SetPixel;
          property  FrameDelay[ aFrame : integer ] : integer             read GetFrameDelay      write SetFrameDelay;
          property  FrameCount : integer                                 read fFrameCount;
          property  ClientRect : TRect                                   read GetClientRect;
          property  Size : TPoint                                        read fSize;
          property  Width : longint                                      read fSize.x;
          property  Height : longint                                     read fSize.y;
          property  StorageWidth : integer                               read GetStorageWidth;
          property  ImageSize : integer                                  read GetImageSize;
          property  TranspIndx : byte                                    read fTranspIndx        write fTranspIndx;

          function  IsSolid( x, y : integer; aFrame : integer ) : boolean;
          function  PixelOfs( x, y : integer; aFrame : integer ) : integer;

          function  GetFilteredPalette(Filter : TPaletteFilter; Data : array of const) : TPaletteInfo;

          procedure Draw( x, y : integer; Alpha, aFrame : integer; const ClipArea : TRect; Buffer : TBuffer; aPaletteInfo : TPaletteInfo ); virtual; abstract;
          procedure DrawOpaque( x, y : integer; aFrame : integer; const ClipArea : TRect; Buffer : TBuffer; aPaletteInfo : TPaletteInfo );  virtual; abstract;

          procedure NewFrames( Count : integer );                                                                                           virtual; abstract;
          procedure Release;                                                                                                                virtual; abstract;
        private
         {$ifOpt d+}
          fFileName: string
         {$endif}
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

          procedure SetPixel( x, y : integer; aFrame : integer; Color : TPixel );                                              override;
          function  GetPixel( x, y : integer; aFrame : integer ) : TPixel;                                                     override;
          procedure SetFrameDelay( aFrame : integer; Delay : integer );                                                        override;
          function  GetFrameDelay( aFrame : integer ) : integer;                                                               override;

        public
          property ImageCount : integer read fImageCount;

          constructor LoadFromStream( Stream : TStream );                                                                      override;
          procedure   SaveToStream( Stream : TStream );                                                                        override;

          procedure DeleteImages( Indx : integer; Count : integer );
          procedure AddImages( Value : PImageList; Count : integer );
          procedure AddImage( Value : pointer; Offset : TPoint );

          procedure Draw( x, y : integer; Alpha, aFrame : integer; const ClipArea : TRect; Buffer : TBuffer; aPaletteInfo : TPaletteInfo ); override;
          procedure DrawOpaque( x, y : integer; aFrame : integer; const ClipArea : TRect; Buffer : TBuffer; aPaletteInfo : TPaletteInfo );  override;

          procedure NewFrames( Count : integer );                                                                              override;
          procedure Release;                                                                                                   override;
      end;

  // TFrameImage

  type
    TFrame =
      record
        pixels : pointer;
        delay  : integer;
      end;

  type
    PFrameList = ^TFrameList;
    TFrameList = array[0..0] of TFrame;

  type
    TFrameImage =
      class( TSpriteImage )
        protected
          fFrames : PFrameList;

          procedure SetPixel( x, y : integer; aFrame : integer; Color : TPixel );                                              override;
          function  GetPixel( x, y : integer; aFrame : integer ) : TPixel;                                                     override;
          procedure SetFrameDelay( aFrame : integer; Delay : integer );                                                        override;
          function  GetFrameDelay( aFrame : integer ) : integer;                                                               override;
          function  GetStorageWidth : integer;                                                                                 override;
          function  GetPixelAddr( x, y : integer; aFrame : integer ) : pointer;

        public
          property PixelAddr[ x, y : integer; aFrame : integer ] : pointer read GetPixelAddr;
        public
          constructor LoadFromStream( Stream : TStream );                                                                      override;
          procedure   SaveToStream( Stream : TStream );                                                                        override;

          procedure DeleteFrames( Indx : integer; Count : integer );
          procedure AddFrames( Value : PFrameList; Count : integer );
          procedure AddFrame( Value : TFrame );

          procedure Draw( x, y : integer; Alpha, aFrame : integer; const ClipArea : TRect; Buffer : TBuffer; aPaletteInfo : TPaletteInfo ); override;
          procedure DrawOpaque( x, y : integer; aFrame : integer; const ClipArea : TRect; Buffer : TBuffer; aPaletteInfo : TPaletteInfo );  override;

          procedure NewFrames( Count : integer );                                                                              override;
          procedure Release;                                                                                                   override;
      end;

  type
    TBmpSprite =
      class(TSpriteImage)

      end;

  {$IFDEF MEMREPORTS}
  const
    cSpriteAllocCause = 'Sprites';
  {$ENDIF}

implementation

  uses
    SysUtils, MemUtils, Rects, BitBlt, NumUtils
    {$IFDEF PROFILES}, Profiler, IsoProfile{$ENDIF}
    {$IFDEF RENDERREPORTS}, RenderReports{$ENDIF}
    {$IFDEF MEMREPORTS}, MemoryReports{$ENDIF};

  type
    PVarRecArray = ^TVarRecArray;
    TVarRecArray = array [0 .. 0] of TVarRec;

    ESpriteException = Exception;

  type
    PFilteredPaletteInfo = ^TFilteredPaletteInfo;
    TFilteredPaletteInfo =
      record
        Filter     : TPaletteFilter;
        Params     : PVarRecArray;
        ParamCount : integer;
        Palette    : TPaletteInfo;
      end;

  // TSpriteImage

  constructor TSpriteImage.Create( aWidth, aHeight : integer );
    begin
      inherited Create;
      fRefCount := 1;
      fSize := Point( aWidth, aHeight );
      fFilteredPalettes := TList.Create;
    end;

  destructor TSpriteImage.Destroy;
    var
      i           : integer;
      filtpalinfo : PFilteredPaletteInfo;
    begin
      Release;
      if fOwnsPalette
        then fPaletteInfo.Free;
      for i := 0 to pred(fFilteredPalettes.Count) do
        begin
          filtpalinfo := PFilteredPaletteInfo(fFilteredPalettes[i]);
          finalize(filtpalinfo.Params^, filtpalinfo.ParamCount);
          freemem(filtpalinfo.Params);
          FreeAndNil(filtpalinfo.Palette);  //.rag
          dispose(filtpalinfo);
          fFilteredPalettes[i] := nil;
        end;
      fFilteredPalettes.Free;
      inherited;
    end;

  constructor TSpriteImage.LoadFromFile( const FileName : string );
    var
      Stream : TStream;
    begin
      Stream := TFileStream.Create( FileName, fmOpenRead or fmShareDenyWrite );
      {$ifopt d+}
      fFileName := FileName;
      {$endif}
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

  function TSpriteImage.AddRef : integer;
    begin
      inc(fRefCount);
      Result := fRefCount;
    end;

  function TSpriteImage.ReleaseRef : integer;
    begin
      dec(fRefCount);
      Result := fRefCount;
      if fRefCount = 0
        then Free;
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
      Result := Width * Height;
    end;

  procedure TSpriteImage.SetPaletteInfo( Value : TPaletteInfo );
    begin
      fPaletteInfo := Value;
    end;

  function TSpriteImage.GetFilteredPalette(Filter : TPaletteFilter; Data : array of const) : TPaletteInfo;
    var
      i           : integer;
      found       : boolean;
      filtpalinfo : PFilteredPaletteInfo;
    begin
      i := 0;
      found := false;
      while (i < fFilteredPalettes.Count) and not found do
        begin
          filtpalinfo := PFilteredPaletteInfo(fFilteredPalettes[i]);
          found := (filtpalinfo.Filter = Filter) and CheckParams(Filter, Slice(filtpalinfo.Params^, filtpalinfo.ParamCount), Data);
          if not found
            then inc(i);
        end;
      if found
        then Result := PFilteredPaletteInfo(fFilteredPalettes[i]).Palette
        else
          begin
            Result := FilterPalette(fPaletteInfo, Filter, Data);
            new(filtpalinfo);
            filtpalinfo.Filter := Filter;
            filtpalinfo.Palette := Result;
            filtpalinfo.ParamCount := high(Data) - low(Data) + 1;
            getmem(filtpalinfo.Params, filtpalinfo.ParamCount*sizeof(TVarRec));
            initialize(filtpalinfo.Params^, filtpalinfo.ParamCount);
            for i := low(Data) to high(Data) do
              filtpalinfo.Params[i] := Data[i];
            fFilteredPalettes.Add(filtpalinfo);
          end;
    end;

  // TComboImage

  procedure TComboImage.NewFrames( Count : integer );
    begin
      //
    end;

  procedure TComboImage.Release;
    begin
      DeleteImages( 0, FrameCount );
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
      until ( Result <> TranspIndx ) or ( i > ImageCount );
    end;

  procedure TComboImage.SetFrameDelay( aFrame : integer; Delay : integer );
    begin
    end;

  function TComboImage.GetFrameDelay( aFrame : integer ) : integer;
    begin
      Result := 0;
    end;

  procedure TComboImage.Draw( x, y : integer; Alpha, aFrame : integer; const ClipArea : TRect; Buffer : TBuffer; aPaletteInfo : TPaletteInfo );
    var
      i : integer;
    begin
      for i := ImageCount - 1 downto 0 do
        fImages[i].Draw( x, y, Alpha, aFrame, ClipArea, Buffer, aPaletteInfo );
    end;

  procedure TComboImage.DrawOpaque( x, y : integer; aFrame : integer; const ClipArea : TRect; Buffer : TBuffer; aPaletteInfo : TPaletteInfo );
    var
      i : integer;
    begin
      for i := ImageCount - 1 downto 0 do
        fImages[i].DrawOpaque( x, y, aFrame, ClipArea, Buffer, aPaletteInfo );
    end;

  // TFrameImage

  procedure TFrameImage.Release;
    begin
      DeleteFrames( 0, FrameCount );
    end;

  constructor TFrameImage.LoadFromStream( Stream : TStream );
    var
      i       : integer;
      ImgSize : integer;
    begin
      Stream.ReadBuffer( fSize, sizeof( fSize ) );
      Stream.ReadBuffer( fFrameCount, sizeof( fFrameCount ) );
      ImgSize := ImageSize;
      GetMem( fFrames, sizeof(TFrame ) * FrameCount );
      for i := 0 to FrameCount - 1 do
        begin
          GetMem( fFrames[i].pixels, ImgSize );
          Stream.ReadBuffer( fFrames[i].pixels^, ImgSize );
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
        Stream.WriteBuffer( fFrames[i].pixels^, ImgSize );
    end;

  procedure TFrameImage.DeleteFrames( Indx : integer; Count : integer );
    var
      i : integer;
    begin
      assert( (FrameCount > 0 ) and (Indx >= 0 ) and (Indx < FrameCount ), 'Bad index or Count in TFrameImage.DeleteFrames' );

      if Indx + Count > FrameCount
        then Count := FrameCount - Indx;
      for i := Indx  to Indx + Count - 1 do
        FreeMem( fFrames[i].pixels );
      if Count <> FrameCount
        then Move( fFrames[Indx + Count], fFrames[Indx], Count * sizeof(TFrame ) );
       ReallocMem( fFrames, (FrameCount - Count ) * sizeof(TFrame ) );
       Dec( fFrameCount, Count );
    end;

  procedure TFrameImage.AddFrames( Value : PFrameList; Count : integer );
    var
      i : integer;
    begin
      ReallocMem( fFrames, (FrameCount + Count ) * sizeof(TFrameList ) );
      {$IFDEF MEMREPORTS}
      ReportMemoryAllocation(cSpriteAllocCause, (FrameCount + Count ) * sizeof(TFrameList ));
      {$ENDIF}
      for i := 0 to Count - 1 do
        fFrames[FrameCount + i] := Value[i];
      Inc( fFrameCount, Count );
    end;

  procedure TFrameImage.AddFrame( Value : TFrame );
    begin
      AddFrames( @Value, 1 );
    end;

  procedure TFrameImage.NewFrames( Count : integer );
    var
      i : integer;
    begin
      ReallocMem( fFrames, (FrameCount + Count ) * sizeof(TFrameList ) );
      {$IFDEF MEMREPORTS}
      ReportMemoryAllocation(cSpriteAllocCause, (FrameCount + Count ) * sizeof(TFrameList ));
      {$ENDIF}
      for i := 0 to Count - 1 do
        begin
          getmem( fFrames[FrameCount + i].pixels, ImageSize );
          {$IFDEF MEMREPORTS}
          ReportMemoryAllocation(cSpriteAllocCause, ImageSize);
          {$ENDIF}
        end;
      Inc( fFrameCount, Count );
    end;

  // TSpriteImage drawing
  function TSpriteImage.PixelOfs( x, y : integer; aFrame : integer ) : integer;
    begin
      Result := (y * StorageWidth + x );
    end;

  function TSpriteImage.IsSolid( x, y : integer; aFrame : integer ) : boolean;
    begin
      Result := Pixel[ x, y, aFrame ] <> TranspIndx;
    end;

  // TFrameImage drawing

  function TFrameImage.GetStorageWidth : integer;
    begin
      Result := Width;
    end;

  function TFrameImage.GetPixelAddr( x, y : integer; aFrame : integer ) : pointer;
    begin
      Result := pchar(fFrames[aFrame].pixels ) + (y * StorageWidth + x );
    end;

  procedure TFrameImage.SetPixel( x, y : integer; aFrame : integer; color : TPixel );
    begin
      PPixel( PixelAddr[ x, y, aFrame ] )^ := color;
    end;

  function TFrameImage.GetPixel( x, y : integer; aFrame : integer ) : TPixel;
    begin
      Result := PPixel( PixelAddr[ x, y, aFrame ] )^;
    end;

  procedure TFrameImage.SetFrameDelay( aFrame : integer; Delay : integer );
    begin
      fFrames[aFrame].delay := Delay;
    end;

  function TFrameImage.GetFrameDelay( aFrame : integer ) : integer;
    begin
      Result := fFrames[aFrame].delay;
    end;

  procedure TFrameImage.Draw( x, y : integer; Alpha, aFrame : integer; const ClipArea : TRect; Buffer : TBuffer; aPaletteInfo : TPaletteInfo );
    var
      ImageArea : TRect;
      ClipSize  : TPoint;
      dx, dy    : integer;
      DstWidth  : integer;
      PalInfo   : TPaletteInfo;
      {$IfOpt d+}
       temp : string;
      {$endif}
    begin
      {$IFDEF PROFILES}
      Profiler.ProcEnded(prfKind_Main, prfId_Rendering);
      Profiler.ProcStarted(prfKind_Main, prfId_ImageDrawing);
      {$ENDIF}
      if (aFrame < 0 ) or (aFrame >= fFrameCount)
        then
          begin
            aFrame := 0;
            {$IfOpt d+}
              temp := format('SpriteImage: Bad frame filename: %s', [fFileName]);
              OutputDebugString(pchar(temp));
              // raise ESpriteException.Create( 'Bad frame specified in TSpriteImage.Draw!!' );
            {$endif}
          end;

      with ImageArea do
        begin
          Left   := x;
          Right  := x + Width;
          Top    := y;
          Bottom := y + Height;
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


      if aPaletteInfo = nil
        then PalInfo := PaletteInfo
        else PalInfo := aPaletteInfo;

      {$IFDEF RENDERREPORTS}
      ImgRendered(Width, Height);
      {$ENDIF}

      case Buffer.BitCount of
        8 :
          if Alpha = 0
            then BltCopyTrans( PixelAddr[ dx, dy, aFrame ], Buffer.PixelAddr[x, y],
                               ClipSize.x, ClipSize.y, TranspIndx, StorageWidth, DstWidth )
            else
              begin
                // In this case we ignore the value of alpha since in 8 bits 50/50 is the only alpha we support (Alpha=4)
                PalInfo.RequiredState( [tsMixMatrixValid] );
                BltCopyGlassed( PixelAddr[ dx, dy, aFrame ], Buffer.PixelAddr[x, y],
                                ClipSize.x, ClipSize.y, TranspIndx, StorageWidth, DstWidth, PalInfo.MixMatrix );
              end;
        16 :
          begin
            if Alpha = 0
              then
                begin
                  PalInfo.RequiredState( [tsHiColor555TableValid] );
                  BltCopySourceCTT16( PixelAddr[ dx, dy, aFrame ], Buffer.PixelAddr[x, y],
                                      ClipSize.x, ClipSize.y, TranspIndx, StorageWidth, DstWidth, PalInfo.HiColor555Table );
                end
              else
                begin
                  PalInfo.RequiredState( [tsHiColorUnpackTableValid] );
                  BltCopyGlassedCTT16( PixelAddr[ dx, dy, aFrame ], Buffer.PixelAddr[x, y],
                                      ClipSize.x, ClipSize.y, TranspIndx, StorageWidth, DstWidth, PalInfo.HiColorUnpackTable );
                end;
          end;
        24 : // this code should be fixed so that it works with 24 bit images specially if you want them glassed
          begin
            PalInfo.RequiredState( [tsTrueColorTableValid] );
            BltCopySourceCTT24( PixelAddr[ dx, dy, aFrame ], Buffer.PixelAddr[x, y],
                                ClipSize.x, ClipSize.y, TranspIndx, Alpha, StorageWidth, DstWidth, PalInfo.TrueColorTable );
          end;
        32 : // this code should be fixed so that it works with 32 bit images specially if you want them glassed
          begin
            PalInfo.RequiredState( [tsTrueColorTableValid] );
            BltCopySourceCTT32( PixelAddr[ dx, dy, aFrame ], Buffer.PixelAddr[x, y],
                                ClipSize.x, ClipSize.y, TranspIndx, Alpha, StorageWidth, DstWidth, PalInfo.TrueColorTable );
          end;
      end;
      {$IFDEF PROFILES}
      Profiler.ProcEnded(prfKind_Main, prfId_ImageDrawing);
      Profiler.ProcStarted(prfKind_Main, prfId_Rendering);
      {$ENDIF}
    end;

  procedure TFrameImage.DrawOpaque( x, y : integer; aFrame : integer; const ClipArea : TRect; Buffer : TBuffer; aPaletteInfo : TPaletteInfo );
    var
      ImageArea : TRect;
      ClipSize  : TPoint;
      dx, dy    : integer;
      DstWidth  : integer;
      PalInfo   : TPaletteInfo;
    begin
      assert( (aFrame >= 0 ) and (aFrame < fFrameCount ), 'Bad frame specified in TSpriteImage.DrawGlassed!!' );

      with ImageArea do
        begin
          Left   := x;
          Right  := x + Width;
          Top    := y;
          Bottom := y + Height;
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

      PalInfo := aPaletteInfo;
      if PalInfo = nil
        then PalInfo := PaletteInfo;

      case Buffer.BitCount of
        8 :
          BltCopyOpaque( PixelAddr[ dx, dy, aFrame ], Buffer.PixelAddr[x, y],
                         ClipSize.x, ClipSize.y, StorageWidth, DstWidth );
        16 :
          begin
            PalInfo.RequiredState( [tsHiColor565TableValid] );
            BltCopyOpaqueCTT16( PixelAddr[ dx, dy, aFrame ], Buffer.PixelAddr[x, y],
                                ClipSize.x, ClipSize.y, StorageWidth, DstWidth, PalInfo.HiColor565Table );
          end;
        24 :
          begin
            PalInfo.RequiredState( [tsTrueColorTableValid] );
            BltCopyOpaqueCTT24( PixelAddr[ dx, dy, aFrame ], Buffer.PixelAddr[x, y],
                                ClipSize.x, ClipSize.y, StorageWidth, DstWidth, PalInfo.TrueColorTable );
          end;
        32 :
          begin
            PalInfo.RequiredState( [tsTrueColorTableValid] );
            BltCopyOpaqueCTT32( PixelAddr[ dx, dy, aFrame ], Buffer.PixelAddr[x, y],
                                ClipSize.x, ClipSize.y, StorageWidth, DstWidth, PalInfo.TrueColorTable );
          end;
      end;
    end;

end.
