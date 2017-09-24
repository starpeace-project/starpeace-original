unit ColorSpaces;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  uses
    Windows, NumUtils;

  const
    maxRgb = 255;
    maxHls = 240;

  type
    THlsQuad =
      packed record
        hlsHue      : byte;
        hlsLight    : byte;
        hlsSat      : byte;
        hlsReserved : byte;
      end;

  function RgbToBgr( RgbQuad : dword ) : dword;

  function RgbToHls( Rgb : TRgbQuad ) : THlsQuad;
  function HlsToRgb( Hls : THlsQuad ) : TRgbQuad;

  function HlsHue( Rgb : TRgbQuad ) : integer;
  function HlsLight( Rgb : TRgbQuad ) : integer;
  function HlsSaturation( Rgb : TRgbQuad ) : integer;

  procedure ChangeHueOf( var Rgb : TRgbQuad; NewHue : integer );
  procedure ChangeLightOf( var Rgb : TRgbQuad; NewLight : integer );
  procedure ScaleLightOf( var Rgb : TRgbQuad; scale : single );
  procedure ChangeSaturationOf( var Rgb : TRgbQuad; NewSat : integer );

  // Tint Colors ------------------------------------------------------------------------
  procedure TintRgbEntries( Indx, Count : integer; var RgbEntries; TintRgb, BaseRgb : longint );

  // Gamma Correction -------------------------------------------------------------------
  procedure GammaCorrectRgbEntries( Indx, Count : integer; var RgbEntries; Percent : integer );

implementation

  function RgbToBgr( RgbQuad : dword ) : dword;
    var
      Rgb : TRgbQuad absolute RgbQuad;
      Res : TRgbQuad absolute Result;
    begin
      Result      := RgbQuad;
      Res.rgbRed  := Rgb.rgbBlue;
      Res.rgbBlue := Rgb.rgbRed;
    end;

  procedure ChangeHueOf( var Rgb : TRgbQuad; NewHue : integer );
    var
      tmpHls : THlsQuad;
    begin
      tmpHls        := RgbToHls( Rgb );
      tmpHls.hlsHue := NewHue;
      Rgb           := HlsToRgb( tmpHls );
    end;

  procedure ChangeLightOf( var Rgb : TRgbQuad; NewLight : integer );
    var
      tmpHls : THlsQuad;
    begin
      tmpHls          := RgbToHls( Rgb );
      tmpHls.hlsLight := NewLight;
      Rgb             := HlsToRgb( tmpHls );
    end;

  procedure ScaleLightOf( var Rgb : TRgbQuad; scale : single );
    var
      tmpHls : THlsQuad;
    begin
      tmpHls          := RgbToHls( Rgb );
      tmpHls.hlsLight := min( 255, round(sqr(0.4 + scale)*integer(tmpHls.hlsLight) ));
      Rgb             := HlsToRgb( tmpHls );
    end;

  procedure ChangeSaturationOf( var Rgb : TRgbQuad; NewSat : integer );
    var
      tmpHls : THlsQuad;
    begin
      tmpHls        := RgbToHls( Rgb );
      tmpHls.hlsSat := NewSat;
      Rgb           := HlsToRgb( tmpHls );
    end;

  function HlsHue( Rgb : TRgbQuad ) : integer;  // !!!! Could be optimized..
    begin
      Result := RgbToHls( Rgb ).hlsHue;
    end;

  function HlsSaturation( Rgb : TRgbQuad ) : integer;  // !!!! Could be optimized..
    begin
      Result := RgbToHls( Rgb ).hlsSat;
    end;

  const
    hueUndefined = 160;

  function HlsLight( Rgb : TRgbQuad ) : integer;
    var
      RgbMax, RgbMin : cardinal;
    begin
      with Rgb do
        begin
          RgbMax := Max3( rgbBlue, rgbGreen, rgbRed );
          RgbMin := Min3( rgbBlue, rgbGreen, rgbRed );
          Result := ( (RgbMax + RgbMin) * maxHls + maxRgb ) div (maxRgb * 2 );
        end;
    end;

  function RgbToHls( Rgb : TRgbQuad ) : THlsQuad;
    var
      s, r, k                : cardinal;
      Hue, Light, Sat        : integer;
      deltaR, deltaG, deltaB : cardinal;
      RgbMax, RgbMin         : cardinal;
    begin
      with {Result,} Rgb do
        begin
          RgbMax := Max3( rgbBlue, rgbGreen, rgbRed );
          RgbMin := Min3( rgbBlue, rgbGreen, rgbRed );
          s := RgbMax + RgbMin;
          r := RgbMax - RgbMin;

          Light := (s * maxHls + maxRgb ) div (maxRgb * 2 );
          if RgbMax = RgbMin
            then  // Achromatic case, R = G = B
              begin
                Sat := 0;
                Hue := hueUndefined;
              end
            else  // Chromatic case
              begin
                // Saturation
                if Light <= maxHls div 2
                  then Sat := (r * maxHls + s div 2 ) div s
                  else
                    begin
                      k := 2 * maxRgb - s;
                      Sat := (r * maxHls + k div 2 ) div k;
                    end;

                // Hue
                deltaR := ((RgbMax - rgbRed )   * (maxHls div 6 ) + r div 2 ) div r;
                deltaG := ((RgbMax - rgbGreen ) * (maxHls div 6 ) + r div 2 ) div r;
                deltaB := ((RgbMax - rgbBlue )  * (maxHls div 6 ) + r div 2 ) div r;
                if RgbMax = rgbRed
                  then Hue := deltaB - deltaG
                  else
                    if RgbMax = rgbGreen
                      then Hue := maxHls div 3 + deltaR - deltaB
                      else Hue := maxHls * 2 div 3 + deltaG - deltaR;
                if Hue < 0
                  then inc( Hue, maxHls );
                if Hue > maxHls
                  then dec( Hue, maxHls );
              end;
        end;
      with Result do
        begin
          hlsHue      := Hue;
          hlsLight    := Light;
          hlsSat      := Sat;
          hlsReserved := 0;
        end;
    end;

  function HueToRgb( magic1, magic2, hue : integer ) : dword;
    var
      r : integer;
    begin
      // Range check: note values passed add/substract thirds of range
      if hue < 0
        then inc( hue, maxHls );
      if hue > maxHls
        then dec( hue, maxHls );

      r := magic2 - magic1;

      // Return r, g, or b value from this tridant
      if hue < maxHls div 6
        then Result := magic1 + (r * hue + (maxHls div 12 ) ) div (maxHls div 6 )
        else
          if hue < maxHls div 2
            then Result := magic2
            else
              if hue < maxHls * 2 div 3
                then Result := magic1 + (r * (maxHls * 2 div 3 - hue ) + maxHls div 12 )
                                 div (maxHls div 6 )
                else Result := magic1;
    end;

  function HlsToRgb( Hls : THlsQuad ) : TRgbQuad;
    var
      r              : cardinal;
      magic1, magic2 : cardinal; // Calculated magic numbers (really!!)
    begin
      with Result, Hls do
        begin
          if hlsSat = 0
            then  // Achromatic case
              begin
                r := hlsLight * maxRgb div maxHls;
                rgbRed   := r;
                rgbGreen := r;
                rgbBlue  := r;
                if hlsHue <> hueUndefined
                  then ; // Error!!
              end
            else  // Chromatic case
              begin
                // Set up magic numbers
                if hlsLight <= maxHls div 2
                  then magic2 := (hlsLight * (maxHls + hlsSat ) + maxHls div 2 ) div maxHls
                  else magic2 := hlsLight + hlsSat - (hlsLight * hlsSat + maxHls div 2 ) div maxHls;
                magic1 := 2 * hlsLight - magic2;

                // Get Rgb, change units from maxHls to maxRgb
                rgbRed   := (HueToRgb( magic1, magic2, hlsHue + maxHls div 3 ) * maxRgb + maxHls div 2 ) div maxHls;
                rgbGreen := (HueToRgb( magic1, magic2, hlsHue ) * maxRgb + maxHls div 2 ) div maxHls;
                rgbBlue  := (HueToRgb( magic1, magic2, hlsHue - maxHls div 3 ) * maxRgb + maxHls div 2 ) div maxHls;
              end;
        end;
    end;

  procedure GammaCorrectRgbEntries( Indx, Count : integer; var RgbEntries; Percent : integer );
    var
      Entries : array[0..255] of TRgbQuad absolute RgbEntries;
      i       : integer;
      Hls     : THlsQuad;
    begin
      for i := Indx to Indx + Count - 1 do
        begin
          Hls := RgbToHls( Entries[i] );
          with Hls do
            begin
              hlsLight := hlsLight * Percent div 100;
              if hlsLight > maxHls
                then hlsLight := maxHls;
            end;
          Entries[i] := HlsToRgb( Hls );
        end;
    end;

  procedure TintRgbEntries( Indx, Count : integer; var RgbEntries; TintRgb, BaseRgb : longint );
    var
      i          : integer;
      Entries    : array[0..255] of TRgbQuad absolute RgbEntries;
      TintLight  : integer;
      BaseLight  : integer;
      CurrentHls : THlsQuad;
      NewLight   : integer;
    begin
      CurrentHls := RgbToHls( TRgbQuad( TintRgb ) );
      BaseLight  := HlsLight( TRgbQuad( BaseRgb ) );
      TintLight  := CurrentHls.hlsLight;
      for i := Indx to Indx + Count - 1 do
        begin
          NewLight := HlsLight( Entries[i] ) + TintLight - BaseLight;
          if NewLight > maxHls
            then CurrentHls.hlsLight := maxHls
            else
              if NewLight < 0
                then CurrentHls.hlsLight := 0
                else CurrentHls.hlsLight := NewLight;
          Entries[i] := HlsToRgb( CurrentHls );
        end;
    end;

end.
