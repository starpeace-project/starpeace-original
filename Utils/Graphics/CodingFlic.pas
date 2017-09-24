unit CodingFlic;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  uses
    Windows, SysUtils, Classes,
    ChunkStream, CodingAnim, Flics, FlicPlayback, Dibs;

  type
    EFlicInvalid = Exception;

   type
     TFliStream =
       class( TChunkStream )
         public
           procedure LoadChunkHeader;                                                                                          override;
           function  ChunkHeaderSize : integer;                                                                                override;
           function  CurrentChunkSize : integer;                                                                               override;
       end;

  type
    TFlicDecoder =
      class( TAnimDecoder )
        protected
          fHeader    : TFliHeader;
          fOfsFrame1 : integer;
          fOfsFrame2 : integer;

          function  GetFrameDelay( FrameIndx : integer ) : longint;                                                            override;
          function  GetFirstAnimFrame : integer;                                                                               override;
          procedure SeekStart;                                                                                                 override;

        public
          constructor Create;

        public
          UnkChunkPlayer : TChunkPlayerProc;

          procedure ProcessFrame;                                                                                              override;

          procedure LoadFromStream( aStream : TStream );                                                                       override;
          procedure SetFrameRange( aStartingFrame, aEndingFrame : integer );                                                   override;

          property OfsFrame1 : integer    read fOfsFrame1;
          property OfsFrame2 : integer    read fOfsFrame2;
          property Header    : TFliHeader read fHeader;
      end;

  resourcestring
    sFliCorruptedFrame = 'Corrupted FLI/FLC frame found while playing back, file is damaged!!';
    sFliInvalid        = 'Error trying to load a corrupt or invalid .FLI/.FLC file!!';

implementation

  // TFliStream

  procedure TFliStream.LoadChunkHeader;
    begin
      fChunkHeader := Relock( fChunkHeader, ChunkHeaderSize );
    end;

  function TFliStream.CurrentChunkSize : integer;
    begin
      assert( Assigned( fChunkHeader ), 'Chunk Header not loaded in CodingGif.TFliStream.CurrentChunkSize!' );
      Result := PFliFrame( fChunkHeader )^.Size;
    end;

  function TFliStream.ChunkHeaderSize : integer;
    begin
      Result := sizeof( TFliFrame );
    end;

  // TFlicDecoder

  constructor TFlicDecoder.Create;
    begin
      inherited;

      fBitCount := 8;
    end;

  function TFlicDecoder.GetFirstAnimFrame : integer;
    begin
      if (Header.Flags and idLooped) = 0
        then Result := 1
        else Result := 2;
    end;

  function TFlicDecoder.GetFrameDelay( FrameIndx : integer ) : longint;
    begin
      if Header.Magic = idHeaderFLC
        then Result := Header.Speed                 // Speed is in 'ms', OK
        else Result := Header.Speed * 1427 div 100; // Speed is in 'jiffies', 1 jiffie = 14.27ms
    end;

  procedure TFlicDecoder.SetFrameRange( aStartingFrame, aEndingFrame : integer );
    begin
      inherited;
      if aStartingFrame = 1
        then StartingFrame := FirstAnimFrame;
    end;

  procedure TFlicDecoder.SeekStart;
    begin
      if ( CurrentFrameIndx > FrameCount ) and
         ( (Header.Flags and idLooped) <> 0 )
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

  procedure TFlicDecoder.LoadFromStream( aStream : TStream );
    var
      FliPrefix : TFliPrefix;
      StartPos  : integer;
    begin
      Detach;

      StartPos := aStream.Position;
      aStream.ReadBuffer( fHeader, sizeof(fHeader) );       // Load Fli Header
      if (fHeader.Magic <> idHeaderFLC) and (fHeader.Magic <> idHeaderFLI)
        then raise EFlicInvalid.Create( sFliInvalid )
        else
          begin
            aStream.ReadBuffer( FliPrefix, sizeof(FliPrefix) );   // Skip Fli Prefix
            if FliPrefix.Magic <> idPrefix
              then FliPrefix.Size := 0;
            aStream.Seek( FliPrefix.Size - sizeof(FliPrefix), soFromCurrent );

            if fHeader.Size > aStream.Size // Bogus field, fix it!!
              then fHeader.Size := aStream.Size;

            fWidth      := integer( fHeader.Width );
            fHeight     := integer( fHeader.Height );
            fFrameCount := integer( fHeader.Frames );
            fDiskSize   := fHeader.Size;

            fStream.Free;
            StartPos := aStream.Position - StartPos;
            fStream  := TFliStream.Create( aStream, fHeader.Size - StartPos );

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

  procedure TFlicDecoder.ProcessFrame;
    begin
      if PFliFrame(CurrentFrame).Magic <> idFrame
        then raise Exception.Create( sFliCorruptedFrame );

      PlayFrame( CurrentFrame, Size, fBufferAddr, fBufferWidth, UnkChunkPlayer );
    end;

end.
