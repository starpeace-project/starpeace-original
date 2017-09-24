unit CodingRle;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  uses
    Windows, SysUtils, Classes,
    MemUtils, ChunkStream, GDI, Dibs, DibRle,
    CodingAnim,
    Flics, FlicPlayback;      // We'll be using a format based somehow on Flics

  type
    ERleInvalid = Exception;

  // RLE File Format -----------------------------------------

  const
    idHeaderRLE = $AF12;
    idFrame     = $F1FA;
    idPrefix    = $F100;
    idRLE       = 64;
    idColor256  = 4;

   type
     PRleChunk = PFliChunk;
     TRleChunk = TFliChunkGeneric;

   type
     TRleStream =
       class( TChunkStream )
           procedure LoadChunkHeader;                                                                                          override;
           function  ChunkHeaderSize : integer;                                                                                override;
           function  CurrentChunkSize : integer;                                                                               override;
       end;

  // RLE Decoder -----------------------------------------

  type
    TRleDecoder =
      class( TAnimDecoder )
        protected
          fHeader    : TFliHeader;
          fOfsFrame1 : integer;
          fOfsFrame2 : integer;

          function  GetFrameDelay( FrameIndx : integer ) : longint;                                                            override;
          function  GetFirstAnimFrame : integer;                                                                               override;
          procedure SeekStart;                                                                                                 override;

        public
          UnkChunkPlayer : TChunkPlayerProc;

          procedure LoadFromStream( aStream : TStream );                                                                       override;
          procedure SetFrameRange( aStartingFrame, aEndingFrame : integer );                                                   override;
          procedure ProcessFrame;                                                                                              override;

          property OfsFrame1 : integer    read fOfsFrame1;
          property OfsFrame2 : integer    read fOfsFrame2;
          property Header    : TFliHeader read fHeader;
      end;

  // RLE Encoder -----------------------------------------

  type
    TRleEncoder =
      class( TAnimEncoder )
        private
          fRleData : pointer;

        protected
          fRleHeader : TFliHeader;

          procedure EncodeFrame;                                                                                               override;
          procedure SaveHeader;                                                                                                override;

          procedure Close;                                                                                                     override;

        public
          constructor Create( Width, Height, BitCount : integer; aFlags : integer );                                           override;
          destructor  Destroy;                                                                                                 override;

          property RleHeader : TFliHeader read fRleHeader write fRleHeader;
      end;

  resourcestring
    sRleCorruptedFrame = 'Corrupted RLE frame found while playing back, file is damaged!!';
    sRleInvalid        = 'Error trying to load a corrupt or invalid .RLA file!!';

implementation

  // TRleStream

  procedure TRleStream.LoadChunkHeader;
    begin
      fChunkHeader := Relock( fChunkHeader, ChunkHeaderSize );
    end;

  function TRleStream.CurrentChunkSize : integer;
    begin
      assert( Assigned( fChunkHeader ), 'Chunk Header not loaded in CodingRle.TRleStream.CurrentChunkSize!' );
      Result := PFliFrame( fChunkHeader )^.Size;
    end;

  function TRleStream.ChunkHeaderSize : integer;
    begin
      Result := sizeof( TFliFrame );
    end;

  // TRlePlayer

  function TRleDecoder.GetFirstAnimFrame : integer;
    begin
      if (Header.Flags and idLooped) = 0
        then Result := 1
        else Result := 2;
    end;

  function TRleDecoder.GetFrameDelay( FrameIndx : integer ) : longint;
    begin
      Result := Header.Speed; // Speed in ms, OK
    end;

  procedure TRleDecoder.SetFrameRange( aStartingFrame, aEndingFrame : integer );
    begin
      inherited;
      if aStartingFrame = 1
        then StartingFrame := FirstAnimFrame;
    end;

  procedure TRleDecoder.SeekStart;
    begin
      if (Header.Flags and idLooped) <> 0
        then
          begin
            Stream.Seek( OfsFrame2, soFromBeginning );
            fCurrentFrameIndx := 2;
          end
        else
          begin
            Stream.Seek( OfsFrame1, soFromBeginning );
            fCurrentFrameIndx := 1;
          end;
    end;

  procedure TRleDecoder.LoadFromStream( aStream : TStream );
    var
      RlePrefix : TFliPrefix;
      StartPos  : integer;
    begin
      Detach;

      StartPos := aStream.Position;
      aStream.ReadBuffer( fHeader, sizeof(fHeader) );       // Load Rle Header
      if fHeader.Magic <> idHeaderRLE
        then raise ERleInvalid.Create( sRleInvalid )
        else
          begin
            aStream.ReadBuffer( RlePrefix, sizeof(RlePrefix) );   // Skip Rle Prefix
            if RlePrefix.Magic <> idPrefix
              then RlePrefix.Size := 0;
            aStream.Seek( RlePrefix.Size - sizeof(RlePrefix), soFromCurrent );

            fWidth      := integer( fHeader.Width );
            fHeight     := integer( fHeader.Height );
            fFrameCount := integer( fHeader.Frames );
            fDiskSize   := fHeader.Size;

            fStream.Free;
            StartPos := aStream.Position - StartPos;
            fStream  := TRleStream.Create( aStream, fHeader.Size - StartPos );

            fSize := Point( Width, Height );
            
            ResetFrameRange;
            NextFrame;

            fOfsFrame1 := 0;
            fOfsFrame2 := Stream.Position;

            // Fix header fields
            fHeader.oFrame1 := fOfsFrame1 + StartPos;
            fHeader.oFrame2 := fOfsFrame2 + StartPos;
          end;
    end;

  procedure TRleDecoder.ProcessFrame;
    var
      Chunk : PFliChunk;
      i     : integer;
    begin
      with PFliFrame( CurrentFrame )^ do
        if Magic <> idFrame
          then raise Exception.Create( sRleCorruptedFrame )
          else
            begin
              pchar( Chunk ) := pchar( CurrentFrame ) + sizeof( TFliFrame ); // Addr of first chunk
              for i := 0 to Chunks - 1 do
                begin
                  if Magic = idRLE
                    then UnpackRle8( BufferWidth, Chunk, fBufferAddr )
                    else
                      if Assigned( UnkChunkPlayer )
                        then UnkChunkPlayer( Chunk, Self.Size, fBufferAddr, fBufferWidth );
                  inc( pchar(Chunk), Chunk.Size ); // Go to next chunk
                end;
            end;
    end;

  // TRleEncoder

  procedure TRleEncoder.EncodeFrame;
    var
      RleFrame       : TFliFrame;
      ChunkRle       : TRleChunk;
      ChunkPalette   : PFliChunkColor;
      SaveColorChunk : boolean;
    begin
      if fRleData = nil
        then GetMem( fRleData, FrameSize * 129 div 127 + 2 * DibHeight( DibHeader ) ); // Worst case
      PackRle8( DibHeader, CurrentFrame, fRleData, PrevFrame, 4 );
      with ChunkRle do
        begin
          Size  := sizeof( ChunkRle ) + DibSizeImage( DibHeader );
          Magic := idRLE;
        end;
      with RleFrame do
        begin
          Size   := sizeof( RleFrame ) + ChunkRle.Size;
          Magic  := idFrame;
          Chunks := 1;
        end;
      SaveColorChunk := fChangedColors <> [];
      if SaveColorChunk
        then
          begin
            ChunkPalette := CreatePaletteChunk( fChangedColors,  DibColors( DibHeader )^ );

            inc( RleFrame.Chunks );
            inc( RleFrame.Size, ChunkPalette.Size );
            Stream.WriteBuffer( RleFrame, sizeof( RleFrame ) );
            Stream.WriteBuffer( ChunkPalette^, ChunkPalette.Size );

            FreeMem( ChunkPalette );
            fChangedColors := [];
          end
        else
          Stream.WriteBuffer( RleFrame, sizeof( RleFrame ) );

      Stream.WriteBuffer( ChunkRle, sizeof( ChunkRle ) );
      Stream.WriteBuffer( fRleData^, ChunkRle.Size );
    end;

  procedure TRleEncoder.SaveHeader;
    begin
      if State <> esClosing
        then
          begin
(*
            RleHeader.Width  := Width;
            RleHeader.Height := Height;
            with RleHeader do
              begin
                Magic  := idHeaderRLE;
                Frames := FrameCount;
                Depth  := BitCount;
                AspectX := ;
                AspectY := ;
              end;
*)
          end
        else
          begin
(*
            RleHeader.Flags := ;
            with RleHeader do
              begin
                Size    := ;
                Created := ;
                Creator := ;
                Updated := ;
                Updater := ;
                oFrame1 := ;
                oFrame2 := ;
              end;
*)
          end;
      Stream.WriteBuffer( RleHeader, sizeof( RleHeader ) );
    end;

  procedure TRleEncoder.Close;
    begin
      FreePtr( fRleData );
      FillChar( fRleHeader, sizeof( fRleHeader ), 0 );

      if fFlags and fLooped <> 0
        then
          begin
            //
          end;

      inherited;
    end;

  constructor TRleEncoder.Create( Width, Height, BitCount : integer; aFlags : integer );
    begin
      inherited;

      fDibHeader := DibNewHeader( Width, Height, BitCount );
    end;

  destructor TRleEncoder.Destroy;
    begin
      DibFree( fDibHeader );

      inherited;
    end;

end.
