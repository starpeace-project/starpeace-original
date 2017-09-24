unit GifLoader;

// Copyright (c) 1998 Jorge Romero Gomez, Merchise

interface

  uses
    Windows, Classes,
    MemUtils, ListUtils, GDI, Dibs, ImageLoaders, Gifs;

  type
    TGifImages =
      class( TImages )
        protected
          function  GetImageClass : CImageData;                                                                                override;
          function  GetDibHeader : PDib;                                                                                       override;
          function  GetImageCount : integer;                                                                                   override;
        public  
          procedure LoadFromStream( Stream : TStream );                                                                        override;

        public
          destructor Destroy;                                                                                                  override;
        public
          procedure LoadImages;                                                                                                override;
      end;

    TGifImageData =
      class( TImageData )
        protected
          fOrigin         : TPoint;
          fDelay          : integer;
          fTransparent    : integer;
          fDisposalMethod : integer;
          fInterlaced     : boolean;

          function GetDibHeader : PDib;                                                                                        override;
          function GetOrigin : TPoint;                                                                                         override;
          function GetDelay : integer;                                                                                         override;
          function GetTransparent : integer;                                                                                   override;
          function GetDisposal : integer;                                                                                      override;

        public
          constructor Create( anOwner : TImages );                                                                             override;
          destructor  Destroy;                                                                                                 override;

        public
          procedure Decode( Dest : pointer; DestWidth : integer );                                                             override;
      end;

  // Registration, just do something like RegisterLoader( GetGifLoader, Overhead );

  function GetGifLoader( aStream : TStream; Info : pointer ) : TImages;

implementation

  // TGifImages

  function TGifImages.GetImageClass : CImageData;
     begin
       Result := TGifImageData;
     end;

  function TGifImages.GetImageCount : integer;
    begin
      if fImageCount < 0
        then LoadImages;
      Result := fImageCount
    end;

  function TGifImages.GetDibHeader : PDib;
    begin
      assert( Assigned( fDibHeader ), 'Unassigned DibHeader in GifLoader.TGifImages.GetDibHeader!!' );
      Result := fDibHeader;
    end;

  destructor TGifImages.Destroy;
    begin
      if Assigned( fDibHeader )
        then FreePtr( fDibHeader );
      inherited;
    end;

  procedure TGifImages.LoadFromStream( Stream : TStream );
    var
      Header   : TGifFileHeader;
      RgbCount : integer;
      RgbData  : TGifColorTable;
    begin
      inherited;

      GifReadBlockInfo( Stream, bidGifHeader, Header );
      with Header do
        begin
          fDibHeader := GifDibHeaderFromGlobal( Header );
          if Header.BitMask and lbmGlobalColorTable <> 0
            then
              begin
                RgbCount := fDibHeader.biClrUsed;
                Stream.ReadBuffer( RgbData, RgbCount * 3 );
                DosToRgbEntries( 0, RgbCount, RgbData, DibColors( fDibHeader )^ );
              end
            else
              fDibHeader.biClrUsed := 0;
          with fDibHeader^ do
            begin
              fBitCount   := biBitCount;
              fWidth      := biWidth;
              fHeight     := biHeight;
            end;
        end;
      fFirstHeaderStreamPos := Stream.Position;
    end;

  procedure TGifImages.LoadImages;
    var
      ImageData : TImageData;
    begin
      fImageCount := 0;
      ImageData := TGifImageData.Create( Self );
      while ImageData.ImageStreamPos <> 0 do
        begin
          fImages.Add( ImageData );
          inc( fImageCount );
          ImageData := TGifImageData.Create( Self );
        end;
     if ImageData.ImageStreamPos = 0
       then
         ImageData.Free;
    end;

  // TGifImageData

  destructor TGifImageData.Destroy;
    begin
      if Assigned( fDibHeader )
        then FreePtr( fDibHeader );

      inherited;
    end;

  constructor TGifImageData.Create( anOwner : TImages );
    var
      Header        : TGifImageDescriptor;
      GraphControl  : TGraphicControlExtension;
      BlockId       : integer;
      RgbCount      : integer;
      RgbData       : TGifColorTable;
      InputCodeSize : byte;
    begin
      inherited;
      try
        BlockId := GifSkipBlocksExcept( Stream, [bidGifTerminator, bidGifImageDescriptor, bidGraphControlExt] );
      except
        on EReadError do
          BlockId := bidGifTerminator;
      end;
      if BlockId = bidGifTerminator
        then
        else
          begin
            if BlockId = bidGraphControlExt
              then
                begin
                  GifReadBlockInfo( Stream, bidGraphControlExt, GraphControl );
                  with GraphControl do
                    begin
                      if BitMask and gbmTransparent = 0
                        then fTransparent := -1
                        else fTransparent := TransparentIndx;
                      fDelay          := DelayTime;
                      fDisposalMethod := ( BitMask and gbmDisposalMethod ) shr 2;
                    end;
                end;
            GifReadBlockInfo( Stream, bidGifImageDescriptor, Header );
            with Header do
              begin
                if Header.BitMask and ibmLocalColorTable = 0
                  then
                    Header.BitMask := ( Header.BitMask and $F8 ) or ( anOwner.DibHeader.biBitCount - 1 );
                fDibHeader := GifDibHeaderFromLocal( Header );
                try
                  if Header.BitMask and ibmLocalColorTable <> 0
                    then
                      begin
                        RgbCount             := 1 shl (1 + Header.BitMask and ibmColorTableSize );
                        fDibHeader.biClrUsed := RgbCount;
                        Stream.ReadBuffer( RgbData, RgbCount * 3 );
                        DosToRgbEntries( 0, RgbCount, RgbData, DibColors( fDibHeader )^ );
                      end
                    else
                      if Owner.DibHeader.biClrUsed <> 0
                        then
                          begin
                            fDibHeader.biClrUsed  := Owner.DibHeader.biClrUsed;
                            fDibHeader.biBitCount := Owner.DibHeader.biBitCount;
                            Move( DibColors( Owner.DibHeader )^, DibColors( fDibHeader )^, fDibHeader.biClrUsed * sizeof( TRgbQuad ) );
                          end;
                  with fDibHeader^ do
                    begin
                      fBitCount    := biBitCount;
                      fWidth       := biWidth;
                      fHeight      := biHeight;
                      fOrigin.x    := Left;
                      fOrigin.y    := Top;
                      fInterlaced  := ( Header.BitMask and ibmInterlaced <> 0 );
                    end;
                except
                  FreePtr( fDibHeader );
                  raise;
                end;
              end;
            fImageStreamPos := Stream.Position;
            Stream.Read( InputCodeSize, sizeof( InputCodeSize ) );
            assert( InputCodeSize <= 8, 'Invalid Code Size!' );
            GifSkipSubBlocks( Stream );
            fNextHeaderStreamPos := Stream.Position;
          end;
    end;

  procedure TGifImageData.Decode( Dest : pointer; DestWidth : integer );
    begin
      Stream.Seek( ImageStreamPos, soFromBeginning );
      UnpackGifPixels( Stream, Width, Height, Dest, DestWidth, fInterlaced );
    end;

  function TGifImageData.GetDibHeader : PDib;
    begin
      assert( Assigned( fDibHeader ), 'Unassigned DibHeader in GifLoader.TGifImageData.GetDibHeader!!' );
      Result := fDibHeader;
    end;

  function TGifImageData.GetOrigin : TPoint;
    begin
      Result := fOrigin;
    end;

  function TGifImageData.GetDelay : integer;
    begin
      Result := fDelay;
    end;

  function TGifImageData.GetTransparent : integer;
    begin
      Result := fTransparent;
    end;

  function TGifImageData.GetDisposal : integer;
    begin
      Result := fDisposalMethod;
    end;
    
  // Registration

  function GetGifLoader( aStream : TStream; Info : pointer ) : TImages;
    var
      fStartingPos : integer;
      GifId        : array[0..2] of char;
    begin
      try
        fStartingPos := aStream.Position;
        aStream.Read( GifId, sizeof( GifId ) );
        if ( GifId[0] = 'G' ) and ( GifId[1] = 'I' ) and ( GifId[2] = 'F' )
          then
            begin
              aStream.Position := fStartingPos;
              Result := TGifImages.Create;
              Result.LoadFromStream( aStream );
            end
          else Result := nil;
      except
        Result := nil;
      end;
    end;

end.
