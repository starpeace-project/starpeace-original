unit ChunkStream;

interface

  uses
    Classes, MemUtils;

  type
    TChunkStream =
      class
        protected
          fSize        : integer;
          fStream      : TStream;
          fStartPos    : integer;
          fPosition    : integer;

          fInMemory    : boolean;
          fBuffer      : pointer;
          fChunkHeader : pointer;

          function GetPosition : integer;

        public
          property  InMemory : boolean read fInMemory;
          property  Stream   : TStream read fStream;
          property  StartPos : integer read fStartPos;
          property  Position : integer read GetPosition;

        public
          constructor Create( aStream : TStream; aSize : integer );
          destructor  Destroy;                                            override;

        public
          procedure LoadChunkHeader;                                      virtual; abstract;
          function  ChunkHeaderSize : integer;                            virtual; abstract;
          function  CurrentChunkSize : integer;                           virtual; abstract;

          function  Seek( Pos : integer; Origin : word ) : integer;
          function  SeekChunk( ChunkIndx : integer ) : integer;

          function  LockChunk : pointer;
          function  Lock( Count : integer ) : pointer;
          procedure Unlock( Locked : pointer );

          function  Relock( Locked : pointer; Count : integer ) : pointer;
          function  RelockChunk( Locked : pointer ) : pointer;

          procedure LoadInMemory;
      end;

implementation

  // TChunkStream

  function TChunkStream.Lock( Count : integer ) : pointer;
    begin
      if InMemory
        then
          begin
            Result := pchar( fBuffer ) + fPosition;
            Inc( fPosition, Count );
          end
        else
          begin
            GetMem( Result, Count );
            Stream.ReadBuffer( Result^, Count );
          end;
    end;

  function TChunkStream.LockChunk : pointer;
    var
      ChunkSize  : integer;
      HeaderSize : integer;
    begin
      LoadChunkHeader;
      ChunkSize  := CurrentChunkSize;
      HeaderSize := ChunkHeaderSize;
      if InMemory
        then
          begin
            Result := pchar( fBuffer ) + fPosition - HeaderSize;
            inc( fPosition, ChunkSize - HeaderSize );
          end
        else
          begin
            GetMem( Result, ChunkSize );
            Stream.ReadBuffer( pchar( Result )[ HeaderSize ], ChunkSize - HeaderSize );
            Move( fChunkHeader^, Result^, HeaderSize );
          end;
    end;

  function TChunkStream.RelockChunk( Locked : pointer ) : pointer;
    var
      ChunkSize  : integer;
      HeaderSize : integer;
    begin
      LoadChunkHeader;
      ChunkSize  := CurrentChunkSize;
      HeaderSize := ChunkHeaderSize;
      if InMemory
        then
          begin
            Result := pchar( fBuffer ) + fPosition - HeaderSize;
            inc( fPosition, ChunkSize - HeaderSize );
          end
        else
          begin
            Result := Locked;
            if AllocatedSize( Result ) < ChunkSize
              then ReallocMem( Result, ChunkSize );
            Stream.ReadBuffer( pchar( Result )[ HeaderSize ], ChunkSize - HeaderSize );
            Move( fChunkHeader^, Result^, HeaderSize );
          end;
    end;

  function TChunkStream.Relock( Locked : pointer; Count : integer ) : pointer;
    begin
      if InMemory
        then
          begin
            Result := pchar( fBuffer ) + fPosition;
            Inc( fPosition, Count );
          end
        else
          begin
            Result := Locked;
            if AllocatedSize( Result ) < Count
              then ReallocMem( Result, Count );
            Stream.ReadBuffer( Result^, Count );
          end;
    end;

  procedure TChunkStream.Unlock( Locked : pointer );
    begin
      if not InMemory
        then FreePtr( Locked );
    end;

  constructor TChunkStream.Create( aStream : TStream; aSize : integer );
    begin
      inherited Create;

      fStream := aStream;
      if aSize < 0
        then fSize := Stream.Size
        else fSize := aSize;
      fStartPos := Stream.Position;
    end;

  destructor TChunkStream.Destroy;
    begin
      Unlock( fChunkHeader );
      if InMemory
        then FreeMem( fBuffer );
      Stream.Free;

      inherited;
    end;

  function TChunkStream.GetPosition : integer;
    begin
      if InMemory
        then Result := fPosition
        else Result := Stream.Position - StartPos;
    end;

  function TChunkStream.SeekChunk( ChunkIndx : integer ) : integer;
    var
      Indx : integer;
    begin
      Seek( StartPos, soFromBeginning );
      if ChunkIndx > 1
        then
          begin
            Indx := 1;
            repeat
              LoadChunkHeader;
              if InMemory
                then inc( fPosition, CurrentChunkSize - ChunkHeaderSize )
                else Stream.Seek( CurrentChunkSize - ChunkHeaderSize, soFromCurrent );
              inc( Indx );
            until Indx = ChunkIndx;
          end;
      Result := Position;    
    end;

  function TChunkStream.Seek( Pos : integer; Origin : word ) : integer;
    begin
      if InMemory
        then
          begin
            case Origin of
              soFromBeginning :
                fPosition := Pos;
              soFromCurrent :
                Inc( fPosition, Pos );
              soFromEnd :
                fPosition := fSize - Pos;
            end;
            Result := fPosition;
          end
        else
          case Origin of
            soFromBeginning :
              Result := Stream.Seek( Pos + StartPos, soFromBeginning );
            else
              Result := Stream.Seek( Pos, Origin );
          end;
    end;

  procedure TChunkStream.LoadInMemory;
    begin
      if not InMemory
        then
          with Stream do
            begin
              GetMem( fBuffer, fSize );
              fPosition := GetPosition;
              try
                Seek( StartPos, soFromBeginning );  // Ignore any error reading stream
                ReadBuffer( fBuffer^, fSize );
              except
              end;
              FreeObject( fStream );
              fInMemory := true;
            end;
    end;

end.
