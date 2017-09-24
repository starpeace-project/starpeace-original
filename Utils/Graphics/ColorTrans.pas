unit ColorTrans;

// Palette Transform Stuff. Copyright (c) 1997 Jorge Romero Gomez, Merchise.

interface

  uses
    Windows, GDI;

  // Color Translation Stuff

  type
    PColorTransTable = ^TColorTransTable;
    TColorTransTable = array[byte] of byte;

  function TabPaletteConv( const SourceRgbEntries; SourceCount : integer;
                           const DestRgbEntries;   DestCount : integer ) : PColorTransTable;
  function TabPaletteReduce( const RgbEntries; Count, NewCount : integer ) : PColorTransTable;
  //function TabPaletteTint( const RgbEntries; Count : integer; Color : TRgbQuad; Percent : integer ) : PColorTransTable;

  // Color Mixing Stuff

  type
    PColorMixMatrix = ^TColorMixMatrix;
    TColorMixMatrix = array[byte, byte] of byte;

  const // Alpha constants
    alphaBlend5050 = 128;

  function MixColors( const SourceRgbEntries; Count : integer; Alpha : integer ) : PColorMixMatrix;

  // Use these tables as CTTs to transfer 8-bit images to 16, 24 or 32-bit images

  type
    PColorTransTableHi = ^TColorTransTableHi;
    TColorTransTableHi = array[byte] of word;

  type
    PColorTransTableTrue = ^TColorTransTableTrue;
    TColorTransTableTrue = array[0..256] of dword;

  function Tab8toHiColor( const SourceRgbEntries; Count : integer; rMask, gMask, bMask : integer ) : PColorTransTableHi;
  function Tab8toHiColorUnpacked( const SourceRgbEntries; Count : integer; rMask, gMask, bMask : integer ) : PColorTransTableTrue;

  function Tab8toTrueColor( const SourceRgbEntries; Count : integer; rMask, gMask, bMask : integer ) : PColorTransTableTrue;

  procedure Tab8toHiColorInPlace( const SourceRgbEntries; Count : integer; var HiColorEntries; rMask, gMask, bMask : integer );
  procedure Tab8toHiColorUnpackedInPlace( const SourceRgbEntries; Count : integer; var HiColorUnpackEntries; rMask, gMask, bMask : integer );

  procedure Tab8toTrueColorInPlace( const SourceRgbEntries; Count : integer; var TrueColorEntries; rMask, gMask, bMask : integer );

implementation

  uses
    NumUtils;

  function Tab8toHiColor( const SourceRgbEntries; Count : integer; rMask, gMask, bMask : integer ) : PColorTransTableHi;
    begin
      getmem( Result, sizeof( TColorTransTableHi ) );
      Tab8toHiColorInPlace( SourceRgbEntries, Count, Result^, rMask, gMask, bMask );
    end;

  function Tab8toTrueColor( const SourceRgbEntries; Count : integer; rMask, gMask, bMask : integer ) : PColorTransTableTrue;
    begin
      getmem( Result, sizeof( TColorTransTableTrue ) );
      Tab8toTrueColorInPlace( SourceRgbEntries, Count, Result^, rMask, gMask, bMask );
    end;

  function Tab8toHiColorUnpacked( const SourceRgbEntries; Count : integer; rMask, gMask, bMask : integer ) : PColorTransTableTrue;
    begin
      getmem( Result, sizeof( TColorTransTableTrue ) );
      Tab8toHiColorUnpackedInPlace( SourceRgbEntries, Count, Result^, rMask, gMask, bMask );
    end;

  procedure Tab8toHiColorInPlace( const SourceRgbEntries; Count : integer; var HiColorEntries; rMask, gMask, bMask : integer );
    var
      i                      : integer;
      rRight, gRight, bRight : integer;
      rLeft, gLeft, bLeft    : integer;
      RgbEntries             : TRgbPalette        absolute SourceRgbEntries;
      HiColorTable           : TColorTransTableHi absolute HiColorEntries;
    begin
      if rMask = 0
        then
          begin
            rMask := $7C00;
            gMask := $03E0;
            bMask := $001F;
          end;

      rRight := RightShiftCount( rMask );
      gRight := RightShiftCount( gMask );
      bRight := RightShiftCount( bMask );

      rLeft := LeftShiftCount( rMask );
      gLeft := LeftShiftCount( gMask );
      bLeft := LeftShiftCount( bMask );

      for i := 0 to Count - 1 do
        with RgbEntries[i] do
          begin
            HiColorTable[i] := ( (rgbRed   shr rLeft) shl rRight ) or
                               ( (rgbGreen shr gLeft) shl gRight ) or
                               ( (rgbBlue  shr bLeft) shl bRight );
          end;
    end;

  procedure Tab8toHiColorUnpackedInPlace( const SourceRgbEntries; Count : integer; var HiColorUnPackEntries; rMask, gMask, bMask : integer );
    var
      i                      : integer;
      rRight, gRight, bRight : integer;
      rLeft, gLeft, bLeft    : integer;
      RgbEntries             : TRgbPalette          absolute SourceRgbEntries;
      HiColorUnpackTable     : TColorTransTableTrue absolute HiColorUnPackEntries;
    begin
      if rMask = 0
        then
          begin
            rMask   := $ff0000;
            gMask   := $00ff00;
            bMask   := $0000ff;
          end;

      rRight := RightShiftCount( rMask );
      gRight := RightShiftCount( gMask );
      bRight := RightShiftCount( bMask );

      rLeft := LeftShiftCount( rMask );
      gLeft := LeftShiftCount( gMask );
      bLeft := LeftShiftCount( bMask );

      for i := 0 to Count - 1 do
        with RgbEntries[i] do
          begin
            HiColorUnpackTable[i] := rgbRed   shr ( rLeft + 3 ) shl rRight or
                                     rgbGreen shr ( gLeft + 3 ) shl gRight or
                                     rgbBlue  shr ( bLeft + 3 ) shl bRight;
          end;
    end;

  procedure Tab8toTrueColorInPlace( const SourceRgbEntries; Count : integer; var TrueColorEntries; rMask, gMask, bMask : integer );
    var
      i                      : integer;
      rRight, gRight, bRight : integer;
      rLeft, gLeft, bLeft    : integer;
      RgbEntries             : TRgbPalette          absolute SourceRgbEntries;
      TrueColorTable         : TColorTransTableTrue absolute TrueColorEntries;
    begin
      if rMask = 0
        then
          begin
            rMask   := $ff0000;
            gMask   := $00ff00;
            bMask   := $0000ff;
          end;

      rRight := RightShiftCount( rMask );
      gRight := RightShiftCount( gMask );
      bRight := RightShiftCount( bMask );

      rLeft := LeftShiftCount( rMask );
      gLeft := LeftShiftCount( gMask );
      bLeft := LeftShiftCount( bMask );

      for i := 0 to Count - 1 do
        with RgbEntries[i] do
          begin
            TrueColorTable[i] := rgbRed   shr rLeft shl rRight or
                                 rgbGreen shr gLeft shl gRight or
                                 rgbBlue  shr bLeft shl bRight;
          end;
    end;

  function MixColors( const SourceRgbEntries; Count : integer; Alpha : integer ) : PColorMixMatrix;
    var
      x, y, i             : integer;
      Dist, MinDist, Indx : integer;
      Average             : TColorRef;
    var
      RgbEntries : TRgbPalette absolute SourceRgbEntries;
      AvgRgb     : TColorRec   absolute Average;
    begin
      new( Result );
      with AvgRgb do
        for y := 0 to 255 do
          for x := 0 to 255 do
            begin
              with RgbEntries[x] do
                begin
                  crRed   := ( Alpha * rgbRed   + ( 256 - Alpha ) * RgbEntries[y].rgbRed )   div 256;
                  crGreen := ( Alpha * rgbGreen + ( 256 - Alpha ) * RgbEntries[y].rgbGreen ) div 256;
                  crBlue  := ( Alpha * rgbBlue  + ( 256 - Alpha ) * RgbEntries[y].rgbBlue )  div 256;
                end;

              MinDist := MaxInt;
              Indx    := 0;
              for i := 0 to 255 do
                with RgbEntries[i] do
                  begin
                    Dist := Square[ crRed - rgbRed ] + Square[ crGreen - rgbGreen ] + Square[ crBlue - rgbBlue ];
                    if Dist < MinDist
                      then
                        begin
                          Indx := i;
                          if Dist > 0
                            then MinDist := Dist
                            else break;
                        end;
                  end;

              Result[x, y] := Indx;
            end;
    end;

  function TabPaletteConv( const SourceRgbEntries; SourceCount : integer;
                           const DestRgbEntries;   DestCount : integer ) : PColorTransTable;
    var
      i          : integer;
      RgbEntries : TRgbPalette absolute SourceRgbEntries;
    begin
      assert( (SourceCount < 256) and (DestCount < 256), 'Invalid count in ColorTrans.TabPaletteConv!!' );
      new( Result );
      for i := 0 to SourceCount - 1 do
        Result[i] := GetNearestEntry(  DestRgbEntries, DestCount, dword( RgbEntries[i] ) );
    end;

  function TabPaletteReduce( const RgbEntries; Count, NewCount : integer ) : PColorTransTable;
    var
      i       : integer;
      Entries : TRgbPalette absolute RgbEntries;
    begin
      assert( (Count < 256) and (Count < 256), 'Invalid count in ColorTrans.TabPaletteConv!!' );
      new( Result );
      for i := 0 to NewCount - 1 do
        Result[i] := GetNearestEntry( Entries, Count, dword( Entries[i] ) );
    end;

end.
