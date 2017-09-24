unit BufferLoaders;

// Copyright (c) 1998 Jorge Romero Gomez, Merchise

interface

  uses
    Classes, Windows, SysUtils,
    Buffer, ImageLoaders;

  procedure LoadBuffer( Buffer : TBuffer; Stream : TStream );
  procedure LoadBufferFromFile( Buffer : TBuffer; const Filename : string );
  procedure LoadBufferFromResName( Buffer : TBuffer; Instance : THandle; const ResourceName : string );
  procedure LoadBufferFromResId( Buffer : TBuffer; Instance : THandle; ResourceId : Integer );

implementation

  uses
    Dibs;

  procedure LoadBuffer( Buffer : TBuffer; Stream : TStream );
    var
      Images : TImages;
    begin
      Buffer.Release;
      Images := GetImageLoader( Stream, nil );
      if Assigned( Images )
        then
          with Images, DibHeader^ do
            begin
              Buffer.CreateSized( biWidth, -abs(biHeight), biBitCount );
              Buffer.ChangePaletteEntries( 0, DibNumColors( Buffer.DibHeader ), DibColors( DibHeader)^ );
              with Images.Image[0], Origin, Buffer do
                Decode( PixelAddr[ x, y ], StorageWidth );
            end;
    end;

  procedure LoadBufferFromFile( Buffer : TBuffer; const Filename : string );
    var
      Stream : TStream;
    begin
      Stream := TFileStream.Create( Filename, fmOpenRead or fmShareDenyWrite );
      try
        LoadBuffer( Buffer, Stream );
      finally
        Stream.Free;
      end;
    end;

  procedure LoadBufferFromResName( Buffer : TBuffer; Instance : THandle; const ResourceName : string );
    var
      Stream : TStream;
    begin
      Stream := TResourceStream.Create( Instance, ResourceName, RT_RCDATA );
      try
        LoadBuffer( Buffer, Stream );
      finally
        Stream.Free;
      end;
    end;

  procedure LoadBufferFromResId( Buffer : TBuffer; Instance : THandle; ResourceId : Integer );
    var
      Stream : TStream;
    begin
      Stream := TResourceStream.CreateFromID( Instance, ResourceId, RT_RCDATA ); 
      try
        LoadBuffer( Buffer, Stream );
      finally
        Stream.Free;
      end;
    end;

end.
