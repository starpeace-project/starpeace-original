unit BufferColors;

interface

  uses
    Windows, Graphics, Dibs, Buffer, ColorSpaces;

  procedure BitmapTint( Source : TBuffer; TintColor, BaseColor : TColor );

implementation

  procedure BitmapTint( Source : TBuffer; TintColor, BaseColor : TColor );
    var
      x, y         : integer;
      TintLight    : integer;
      BaseLight    : integer;
      NewLight     : integer;
      TintRgb      : integer;
      BaseRgb      : integer;
      CurrentHls   : THlsQuad;
      RgbData      : pointer;
      CurrScanLine : pointer;
    begin
      with Source, DibHeader^ do
        begin
          if BaseColor = clNone
            then BaseColor := clGray;
          TintRgb := ColorToRgb( TintColor );
          BaseRgb := ColorToRgb( BaseColor );
          if UsesPalette
            then
              begin
                TintRgbEntries( 0, biClrUsed, RgbEntries^, TintRgb, BaseRgb );
                ChangePaletteEntries( 0, biClrUsed, RgbEntries^ );
              end
            else
              begin
                CurrentHls := RgbToHls( TRgbQuad( TintRgb ) );
                BaseLight  := HlsLight( TRgbQuad( BaseRgb ) );
                TintLight  := CurrentHls.hlsLight;
                DibGetRgbBegin( DibHeader, RgbData );

                for y := 0 to Height - 1 do
                  begin
                    CurrScanLine := ScanLine[y];
                    for x := 0 to Width - 1 do
                      begin
                        NewLight := HlsLight( DibGetRgb( CurrScanLine, x, RgbData ) ) + TintLight - BaseLight;
                        if NewLight > maxHls
                          then CurrentHls.hlsLight := maxHls
                          else
                            if NewLight < 0
                              then CurrentHls.hlsLight := 0
                              else CurrentHls.hlsLight := NewLight;
                        DibSetRgb( CurrScanLine, x, RgbData, HlsToRgb( CurrentHls ) );
                      end;
                  end;
              end;

        end;
    end;

end.
