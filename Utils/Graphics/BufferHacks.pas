unit BufferHacks;

interface

  uses
    Windows, Buffer;
    
  procedure BitmapDrawGradientZone( Buffer : TBuffer; Color1, Color2 : TRgbQuad; LineCount : integer );

implementation

  uses
    Graphics, GDI;

  procedure BitmapDrawGradientZone( Buffer : TBuffer; Color1, Color2 : TRgbQuad; LineCount : integer );
    var
      i, x : integer;
      pix  : pchar;
    begin
      BitmapSetPaletteSize( Buffer, 256 );
      with Buffer do
        begin
          FillWithGradient( Color1, Color2, 236, 10, RgbEntries[0] );
          FillWithGradient( Color2, Color1, 246, 10, RgbEntries[0] );
          for i := 0 to LineCount - 1 do
            begin
              pix := pchar( ScanLine[399 - i] );
              for x := 0 to 319 do
                pix[x] := char( 236 + x * 20 div 320 );
            end;
          DibHeader.biClrImportant := 236;
        end;
    end;

end.
 