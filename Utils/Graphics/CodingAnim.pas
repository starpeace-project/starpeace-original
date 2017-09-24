unit CodingAnim;

interface

  uses
    Windows, SysUtils, Classes,
    MemUtils, ChunkStream, Dibs;

  type
    TBufferRec =
      record
        Owned     : boolean;
        Header    : PDib;
        ScanLines : pointer;
      end;

  type
    TAnimDecoder = // Renders an animation to a buffer
      class
        protected
          fStream           : TChunkStream;
          fBufferRec        : TBufferRec;
          fCurrentFrame     : pointer;
          fCurrentFrameIndx : integer;
          fSize             : TPoint;
          fBufferAddr       : pointer;
          fBufferWidth      : integer;
          fWidth            : integer;
          fHeight           : integer;
          fBitCount         : integer;
          fOnFinished       : TNotifyEvent;
          fStartingFrame    : integer;
          fEndingFrame      : integer;
          fKeyFramed        : boolean;
          fFrameCount       : integer;
          fDiskSize         : integer;

          function  GetEmpty : boolean;                                                                                        virtual;
          function  GetFrameDelay( FrameIndx : integer ) : longint;                                                            virtual; abstract;
          function  GetFirstAnimFrame : integer;                                                                               virtual; abstract;
          procedure SeekStart;                                                                                                 virtual; abstract;

        public
          property Stream : TChunkStream                       read fStream;
          property Buffer : TBufferRec                         read fBufferRec;
          property CurrentFrame : pointer                      read fCurrentFrame;
          property CurrentFrameIndx : integer                  read fCurrentFrameIndx;
          property Size : TPoint                               read fSize;
          property BufferWidth : integer                       read fBufferWidth;
          property Width  : integer                            read fWidth;
          property Height : integer                            read fHeight;
          property BitCount : integer                          read fBitCount;
          property DiskSize : longint                          read fDiskSize;
          property FrameDelay[ FrameIndx : integer ] : longint read GetFrameDelay;
          property FrameCount : integer                        read fFrameCount;
          property OnFinished : TNotifyEvent                   read fOnFinished        write fOnFinished;
          property Empty : boolean                             read GetEmpty;

        public
          procedure AttachToDib( x, y : integer; DibHeader : PDib; Pixels : pointer );                                         virtual;
          // Here this guy has to take care of:
          //   fBufferRec, fBufferAddr, fBufferWidth

          procedure  Detach;                                                                                                   virtual;

          destructor Destroy;                                                                                                  override;

          procedure LoadFromStream( aStream : TStream );                                                                       virtual; abstract;
          // Here this guy has to take care of:
          //   fStream, fWidth, fHeight, fSize, fFrameCount, fDiskSize, fKeyFramed
          //   fStartingFrame, fEndingFrame     -> call ResetFrameRange for it
          //   fCurrentFrame, fCurrentFrameIndx

          procedure LoadFromFile( const FileName : string );                                                                   virtual;
          procedure SeekFrame( FrameIndx : integer );                                                                          virtual;

          procedure DoFrame;        // Just does ProcessFrame; NextFrame;

          procedure ProcessFrame;                                                                                              virtual; abstract;
          procedure NextFrame;                                                                                                 virtual;

          procedure SetFrameRange( aStartingFrame, aEndingFrame : integer );                                                   virtual;
          procedure ResetFrameRange;                                                                                           virtual;

          property FirstAnimFrame : integer read GetFirstAnimFrame;
          property StartingFrame  : integer read fStartingFrame write fStartingFrame;
          property EndingFrame    : integer read fEndingFrame   write fEndingFrame;
      end;

  // Defined flags
  const
    fLooped = $0001;

  // Encoder states
  const
    esNone     = 0;
    esCreating = 1;
    esFrames   = 2;
    esClosing  = 3;

  type
    TAnimEncoder =
      class
        protected
          fCurrentFrame  : pointer;
          fPrevFrame     : pointer;
          fStream        : TStream;
          fFrameCount    : integer;
          fFlags         : integer;
          fState         : integer;
          fDibHeader     : PDib;
          fChangedColors : set of byte;

          procedure EncodeFrame;                                                                 virtual; abstract;
          procedure SaveHeader;                                                                  virtual; abstract;

          procedure Close;                                                                       virtual;

          function GetFrameSize : integer;                                                       virtual;

        public
          constructor Create( aWidth, aHeight, aBitCount : integer; aFlags : integer );          virtual;
          destructor  Destroy;                                                                   override;

          procedure CreateFile( const Filename : string );
          procedure CreateStream( aStream : TStream );

          procedure ChangePalette( aIndx, aCount : integer; const RgbQuads );                    virtual;
          procedure SaveFrame( Pixels : pointer );                                               virtual;

          property Stream : TStream       read fStream       write CreateStream;
          property Flags : integer        read fFlags;
          property FrameCount : integer   read fFrameCount;
          property FrameSize : integer    read GetFrameSize;
          property CurrentFrame : pointer read fCurrentFrame;
          property PrevFrame : pointer    read fPrevFrame;
          property State : integer        read fState;
          property DibHeader : PDib       read fDibHeader;
      end;

  procedure BufferCreate( var Buffer : TBufferRec; Width, Height : integer; Bits : integer );
  procedure BufferFree( var BufferRec : TBufferRec );

implementation

  // TBufferRec

  procedure BufferFree( var BufferRec : TBufferRec );
    begin
      with BufferRec do
        if Owned and (Header <> nil)
          then
            begin
              DibFree( Header );
              Header := nil;
            end;
    end;

  procedure BufferCreate( var Buffer : TBufferRec; Width, Height : integer; Bits : integer );
    begin
      with Buffer do
        begin
          Owned     := true;
          Header    := DibCreate( Width, -abs(Height), Bits );
          ScanLines := DibPtr( Header );
        end;
    end;

  // TAnimDecoder

  function TAnimDecoder.GetEmpty : boolean;
    begin
      Result := FrameCount = 0;
    end;

  procedure TAnimDecoder.DoFrame;
    begin
      ProcessFrame;
      NextFrame;
    end;

  procedure TAnimDecoder.ResetFrameRange;
    begin
      StartingFrame := FirstAnimFrame;
      EndingFrame   := FrameCount;
    end;

  procedure TAnimDecoder.SeekFrame( FrameIndx : integer );
    begin
      assert( FrameIndx <= FrameCount, 'Invalid frame index in CodingAnim.TAnimDecoder.SeekFrame!!' );
      if FrameIndx < CurrentFrameIndx
        then
          begin
            SeekStart;
            fCurrentFrame := Stream.RelockChunk( fCurrentFrame );
          end;

      while CurrentFrameIndx < FrameIndx do
        DoFrame;
    end;

  procedure TAnimDecoder.NextFrame;
    begin
      if CurrentFrameIndx > EndingFrame
        then
          begin
            if Assigned( OnFinished )
              then OnFinished( Self );
            SeekFrame( StartingFrame );
          end
        else
          begin
            inc( fCurrentFrameIndx );
            fCurrentFrame := Stream.RelockChunk( fCurrentFrame );
          end;
    end;

  procedure TAnimDecoder.AttachToDib( x, y : integer;
                                      DibHeader : PDib; Pixels : pointer );
    begin
      Detach;
      if DibHeader <> nil
        then
          begin
            fBufferRec.Owned     := false;
            fBufferRec.Header    := DibHeader;
            fBufferRec.ScanLines := pchar(Pixels) + DibPixelOfs( DibHeader, x, y );
          end
        else BufferCreate( fBufferRec, Width, Height, 8 );

      with fBufferRec do
        begin
          fBufferWidth := DwordAlign( Header.biWidth );
          fBufferAddr  := Pixels;
        end;
    end;

  procedure TAnimDecoder.Detach;
    begin
      BufferFree( fBufferRec );
    end;

  destructor TAnimDecoder.Destroy;
    begin
      Detach;
      if Assigned( Stream )
        then
          begin
            Stream.Unlock( fCurrentFrame );
            Stream.free;
          end;

      inherited;
    end;

  procedure TAnimDecoder.SetFrameRange( aStartingFrame, aEndingFrame : integer );
    begin
      StartingFrame := aStartingFrame;
      EndingFrame   := aEndingFrame;
    end;

  procedure TAnimDecoder.LoadFromFile( const FileName : string );
    begin
      LoadFromStream( TFileStream.Create( FileName, fmOpenRead or fmShareDenyWrite ) );
    end;

  // TAnimEncoder

  procedure TAnimEncoder.CreateFile( const Filename : string );
    begin
      CreateStream( TFileStream.Create( Filename, fmCreate ) );
    end;

  procedure TAnimEncoder.CreateStream( aStream : TStream );
    begin
      if Assigned( Stream )
        then Close;

      fStream     := aStream;
      fFrameCount := 0;

      fState := esCreating;
      SaveHeader;

      fState := esFrames;
    end;

  procedure TAnimEncoder.ChangePalette( aIndx, aCount : integer; const RgbQuads );
    begin
      Move( RgbQuads, DibColors(fDibHeader)[aIndx], aCount * sizeof( TRgbQuad ) );
      fChangedColors := fChangedColors + [aIndx, aIndx + aCount - 1];
    end;

  function TAnimEncoder.GetFrameSize : integer;
    begin
      Result := DibSizeImage( DibHeader );
    end;

  procedure TAnimEncoder.SaveFrame( Pixels : pointer );
    begin
      inc( fFrameCount );
      if Assigned( fCurrentFrame )
        then
          begin
            if not Assigned( fPrevFrame )
              then GetMem( fPrevFrame, FrameSize );
            Move( fCurrentFrame^, fPrevFrame^, FrameSize );
          end;
      fCurrentFrame := Pixels;
      EncodeFrame;
    end;

  constructor TAnimEncoder.Create( aWidth, aHeight, aBitCount : integer; aFlags : integer );
    begin
      inherited Create;

      fFlags      := aFlags;
      fDibHeader  := DibNewHeader( aWidth, aHeight, aBitCount );
    end;

  procedure TAnimEncoder.Close;
    begin
      Stream.Seek( 0, soFromBeginning );
      fState := esClosing;
      SaveHeader;

      FreeObject( fStream );
    end;

  destructor TAnimEncoder.Destroy;
    begin
      Close;
      FreeMem( fPrevFrame );

      inherited;
    end;

end.
