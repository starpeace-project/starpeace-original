unit CodingGif;

interface

  uses
    Windows, SysUtils, Classes, MemUtils, 
    Dibs, CodingAnim, ListUtils;

  type
    TGifDecoder =
      class( TAnimDecoder )
          destructor Destroy;                           override;
        public//protected
          fFrameList : TObjectList;
          fDibHeader : PDib;

          function  GetFrameDelay( FrameIndx : integer ) : longint;                                                            override;
          function  GetFirstAnimFrame : integer;                                                                               override;
          procedure SeekStart;                                                                                                 override;

        public
          procedure LoadFromStream( aStream : TStream );                                                                       override;
          procedure SeekFrame( FrameIndx : integer );									       override;
  	  procedure NextFrame;												       override;
          procedure ProcessFrame;                                                                                              override;
      end;

implementation

  uses
    BitBlt, Gifs, ImageLoaders, GifLoader;

  // TGifFrame

  type
    TGifFrame =
      class
        fDibHeader : PDib;
        fDibPixels : pointer;
        fDelay     : integer;

        constructor Create( aOwner : TGifDecoder; PrevFrame : TGifFrame; Data : TImageData );
        destructor  Destroy;                            override;
      end;

  constructor TGifFrame.Create( aOwner : TGifDecoder; PrevFrame : TGifFrame; Data : TImageData );
    var
      wb : integer;
    begin
      inherited Create;
      fDelay := Data.Delay;
      if Data.LocalPalette
        then
          begin
            //fDibHeader :=
          end;
       wb := DwordAlign( aOwner.Width );
       getmem( fDibPixels, wb * aOwner.Height );
       if Assigned( PrevFrame )
         then BltCopyOpaque( PrevFrame.fDibPixels, fDibPixels, aOwner.Width, aOwner.Height, wb, wb );
       Data.Decode( pchar(fDibPixels) + Data.Origin.y * wb + Data.Origin.x, wb );
    end;

  destructor TGifFrame.Destroy;     //.rag
    begin
      FreePtr(fDibPixels);
      inherited;
    end;  // TGifDecoder

  procedure TGifDecoder.ProcessFrame;
    begin
      with fBufferRec do
        BltCopyOpaque( TGifFrame(fFrameList[CurrentFrameIndx-1]).fDibPixels, fBufferAddr, Width, Height, DwordAlign( Width ), fBufferWidth );
    end;

  function TGifDecoder.GetFrameDelay( FrameIndx : integer ) : longint;
    begin
      Result := TGifFrame( fFrameList[FrameIndx-1] ).fDelay;
    end;

  function TGifDecoder.GetFirstAnimFrame : integer;
    begin
      Result := 1;
    end;

  procedure TGifDecoder.SeekFrame( FrameIndx : integer );
    begin
      fCurrentFrameIndx := FrameIndx;
    end;

  procedure TGifDecoder.NextFrame;
    begin
      if CurrentFrameIndx >= EndingFrame
        then
          begin
            if Assigned( OnFinished )
              then OnFinished( Self );
            SeekFrame( StartingFrame );
          end
        else
          begin
            inc( fCurrentFrameIndx );
            //fCurrentFrame := Stream.RelockChunk( fCurrentFrame );
          end;
    end;

  procedure TGifDecoder.SeekStart;
    begin
      fCurrentFrameIndx := FirstAnimFrame;
    end;

  procedure TGifDecoder.LoadFromStream( aStream : TStream );
    var
      i         : integer;
      GifFrame  : TGifFrame;
      GifImages : TGifImages;
    begin
      GifImages := TGifImages.Create;
      GifImages.LoadFromStream( aStream );

      fWidth    := GifImages.Width;
      fHeight   := GifImages.Height;
      fBitCount := 8;
      fSize     := Point( Width, Height );

      fDiskSize         := -1;
      fFrameCount       := GifImages.ImageCount;
      ResetFrameRange;
      fCurrentFrameIndx := 1;

      if fDibHeader<>nil
        then freemem(fDibHeader);
      fDibHeader := DibCopyHeader( GifImages.DibHeader );

      fFrameList.free;
      fFrameList := TObjectList.Create;
      fFrameList.AssertCapacity( FrameCount );

      GifFrame := nil;
      for i := 0 to FrameCount - 1 do
        begin
          GifFrame := TGifFrame.Create( Self, GifFrame, GifImages[i] );
          fFrameList[i] := GifFrame;
        end;

      GifImages.Free;
    end;


  destructor TGifDecoder.destroy;
    begin
      fFrameList.Free;
      if fDibHeader<>nil
        then FreePtr( fDibHeader );
      inherited;
    end;

end.
