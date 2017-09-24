unit BufferRle;

interface

  uses
    Buffer, DibRle;
    
  function  BitmapPack( Source : TBuffer ) : pointer;
  function  BitmapPackDelta( SourcePrev, Source : TBuffer ) : pointer;
  procedure BitmapUnpack( Source : pointer; Dest : TBuffer );

implementation

  function BitmapPack( Source : TBuffer ) : pointer;
    begin
      Result := nil; // !! Unfinished!!
    end;

  function BitmapPackDelta( SourcePrev, Source : TBuffer ) : pointer;
    begin
      Result := nil; // !! Unfinished!!
    end;

  procedure BitmapUnpack( Source : pointer; Dest : TBuffer );
    begin
    end;

end.
