unit StreamUtils;

interface

  uses
    Classes;

  // Calls StreamProc and destroys the stream. Ex: StreamObject( TFileStream.Create(...), LoadFromStream );
  type
    TStreamProc = procedure ( Stream : TStream ) of object;

  procedure StreamObject( Stream : TStream; StreamProc : TStreamProc );

  // TPointerStream: A stream from a memory pointer
  type
    TPointerStream =
      class( TCustomMemoryStream )
        constructor Create( Data : pointer; aSize : integer );
        function    Write( const Buffer; Count : longint ) : longint;                         override;
      end;

implementation

  procedure StreamObject( Stream : TStream; StreamProc : TStreamProc );
    begin
      try
        StreamProc( Stream );
      finally
        Stream.Free;
      end;
    end;

  procedure WriteStr( Stream : TStream; str : string );
    var
      len : integer;
    begin
      len := length(str);
      Stream.Write( len, sizeof(len) );
      Stream.Write( str[1], len );
    end;

  function ReadStr( Stream : TStream ) : string;
    var
      len : integer;
    begin
      Stream.Read( len, sizeof(len) );
      SetLength( result, len );
      Stream.Read( result[1], len );
    end;


  // TPointerStream
  constructor TPointerStream.Create( Data : pointer; aSize : integer );
    begin
      inherited Create;

      SetPointer( Data, aSize );
    end;

  function TPointerStream.Write( const Buffer; Count : longint ) : longint;
    var
      Pos : longint;
    begin
      Result := 0;
      if (Position >= 0) and (Count >= 0)
        then
          begin
            Pos := Position + Count;
            if Pos > 0
              then
                begin
                  assert( Pos <= Seek(0, 2), 'Writing beyond buffer limits in StreamUtils.TPointerStream.Write!!' );

                  System.Move( Buffer, pointer( longint( Memory ) + Position )^, Count );
                  Seek( Pos, soFromBeginning );
                  Result := Count;
                end;
          end;
    end;

end.

