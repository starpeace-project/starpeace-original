unit GDI;

// Copyright ( c) 1996 Jorge Romero Gomez, Merchise.

interface

  uses
    Windows, MiscUtils, NumUtils;

  const
    LogPaletteVersion = $300;

  type
    PRgbPalette = ^TRgbPalette;
    TRgbPalette = array[0..255] of TRGBQuad;

  type
    ColorSet = set of byte;
    
  type
    P256PalEntries = ^T256PalEntries;
    T256PalEntries = array[0..255] of TPaletteEntry;

  type
    P256LogPalette = ^T256LogPalette;
    T256LogPalette =
      packed record
        Version         : word;
        NumberOfEntries : word;
        Entries         : T256PalEntries;
      end;

  type
    TColorRec = // This record is compatible with a TColorRef
      packed record  
        crRed, crGreen, crBlue, crReserved : byte;
      end;

  function RgbQuad( Red, Green, Blue : byte ) : TRgbQuad;

  type
    PRGBQuad   = ^TRGBQuad;
    PRGBTriple = ^TRGBTriple;

  type
    P256Bitmap = ^T256Bitmap;
    T256Bitmap =
      packed record
        bmiHeader : TBitmapInfoHeader;
        bmiColors : TRgbPalette;
        Bits      : array[0..0] of byte;
      end;

  // Returns the index within RgbQuads of the closest matching entry
  function  GetNearestEntry( const RgbQuads; Count : integer; RgbQuad : dword ) : integer;

  function  PaletteHandle( const LogPalette ) : HPALETTE;
  function  ClearSystemPalette : boolean;
  function  CreateIdentityPalette( Count : integer; const RgbQuads ) : HPALETTE;
  function  CreateIdentityLogPalette( Count : integer; const RgbQuads ) : T256LogPalette;

  function  GetBitmapDC( Handle : HBITMAP; var OldBitmap : HBITMAP ) : HDC;
  procedure ReleaseBitmapDC( dc : HDC; OldBitmap : HBITMAP );

  // Set foreground & background colors in a DC, optionally returning previous values
  procedure SetGdiColors( dc : HDC; ForeColor, BackColor : integer );
  procedure SetGdiColorsEx( dc : HDC; ForeColor, BackColor : integer; var OldForeColor, OldBackColor : integer );

  function  GetPaletteHandle( DC : HDC ) : HPALETTE;
  function  GetBitmapHandle( DC : HDC ) : HBITMAP;

  function  CopyPalette( Palette : HPALETTE ) : HPALETTE;

  function  IsIdentityPalette( DC : HDC ) : boolean;

  // ActivateAppPalette sets the system palette use to NOSTATIC and
  // remaps the system colors accordingly.
  procedure ActivateAppPalette( Active : boolean );

  procedure GetDefaultPalette( Count : cardinal; var RgbQuads ); // I'm using halftone palette as default
  procedure GetLogPalette( hPal : HPALETTE; Indx, Count : integer; var LogEntries; PreserveFlags : boolean );
  procedure GetLogSysPalette( Indx, Count : integer; var LogEntries );
  procedure GetRgbPalette( hPal : HPALETTE; Indx, Count : integer; var RgbQuads );
  procedure GetRgbSysPalette( Indx, Count : integer; var RgbQuads );

  procedure BlackLogEntries( var LogEntries );

  procedure MarkReserved( Indx, Count : integer; var LogEntries );
  procedure MarkNoCollapse( Indx, Count : integer; var LogEntries );
  procedure MarkNone( Indx, Count : integer; var LogEntries );

  procedure RgbToLogEntries( Indx, Count : integer; const RgbQuads; var LogEntries );
  procedure LogToRgbEntries( Indx, Count : integer; const LogEntries; var RgbQuads );

  procedure FillWithGradient( Source, Dest : TRgbQuad; Indx, aCount : integer; var RgbQuads );

  function  BitCount( PaletteSize : integer ) : cardinal;     // eg. BitCount( 256 ) = 8
  function  PaletteSize( BitCount : integer ) : cardinal;     // PaletteSize( 8 ) = 256, PaletteSize( 24 ) = 0

  // Screen information -------------------------------------------------

  function  FirstAvailColor : integer;
  function  LastAvailColor : integer;
  function  AvailableColors : integer;
  procedure GetColorAvailInfo( var FirstAvailColor, LastAvailColor, AvailColors : integer );

  function  ScreenDeviceWidth : integer;
  function  ScreenDeviceHeihgt : integer;
  function  ScreenDeviceBitsPerPixel : integer;
  function  ScreenDeviceIsPaletted : boolean;

  // Stock objects --------------------------------------------------------------

  var
    fStockPen   : HPEN;
    fStockBrush : HBRUSH;
    fStockFont  : HFONT;

  const
    rgbBlack : TRGBQuad = ( rgbBlue : 0; rgbGreen : 0; rgbRed : 0; rgbReserved : 0 );
    rgbWhite : TRGBQuad = ( rgbBlue : 255; rgbGreen : 255; rgbRed : 255; rgbReserved : 0 );

  const
    RgbMonochromePalette : array[0..1] of TRgbQuad =
      (
        ( rgbBlue : 0; rgbGreen : 0; rgbRed : 0; rgbReserved : 0 ),
        ( rgbBlue : 255; rgbGreen : 255; rgbRed : 255; rgbReserved : 0 )
      );

  function LogBlackPalette : P256LogPalette;    // Black stock palette

  // Win95 Halftone palette stuff

  function LogHalftonePalette : P256LogPalette; // Halftone stock palette
  function RgbHalftonePalette : PRgbPalette;

  function HalftonePaletteIndx( x, y : integer; RgbColor : TRgbQuad ) : byte;
  // Returns the correct color in the Win95 Halftone palette for the specified Rgb

  // DOS Palette Stuff ----------------------------------------------------------

  type
    TDosRgb =  // This is the format most used by DOS software for RGB entries
      packed record
        r, g, b : byte;
      end;

  type
    PDosRgbPalette = ^TDosRgbPalette;
    TDosRgbPalette = array[0..255] of TDosRgb;

  procedure DosToRgbEntries( Indx, Count : integer; const DosRgbEntries; var RgbQuads );

  // Misc -------------------------------------------------------------------------

  function GetTextHeight( FontHandle : HFONT ) : integer;

implementation

  function RgbQuad( Red, Green, Blue : byte ) : TRgbQuad;
    begin
      with Result do
        begin
          rgbRed      := Red;
          rgbGreen    := Green;
          rgbBlue     := Blue;
          rgbReserved := 0;
        end;
    end;

  function GetTextHeight( FontHandle : HFONT ) : integer;
    var
      DC       : HDC;
      SaveFont : HFONT;
      Metrics  : TTextMetric;
    begin
      DC := GetDC( 0 );
      try
        SaveFont := SelectObject( DC, FontHandle );
        GetTextMetrics( DC, Metrics );
        SelectObject( DC, SaveFont );
        Result := Metrics.tmHeight;
      finally
        ReleaseDC( 0, DC );
      end;
    end;

  procedure SetGdiColors( dc : HDC; ForeColor, BackColor : integer );
    begin
      SetTextColor( dc, ForeColor );
      SetBkColor( dc, BackColor );
    end;

  procedure SetGdiColorsEx( dc : HDC; ForeColor, BackColor : integer; var OldForeColor, OldBackColor : integer );
    begin
      OldForeColor := SetTextColor( dc, ForeColor );
      OldBackColor := SetBkColor( dc, BackColor );
    end;

  function GetBitmapDC( Handle : HBITMAP; var OldBitmap : HBITMAP ) : HDC;
    begin
      OldBitmap := 0;
      Result := CreateCompatibleDC( 0 );
      if Result <> 0
        then OldBitmap := SelectObject( Result, Handle );
    end;

  procedure ReleaseBitmapDC( dc : HDC; OldBitmap : HBITMAP );
    begin
      if OldBitmap <> 0
        then SelectObject( dc, OldBitmap );
      if dc <> 0
        then DeleteDC( dc );
    end;

  function PaletteSize( BitCount : integer ) : cardinal;
    begin
      if BitCount > 8
        then Result := 0
        else Result := 1 shl BitCount;
    end;

  function BitCount( PaletteSize : integer ) : cardinal;
    begin
      if PaletteSize = 0
        then Result := 0
        else
          begin
            Result := 32;
            while PaletteSize and ( 1 shl Result ) = 0 do
              Dec( Result );
          end;
    end;

  // Stock objects

  var
    fLogBlackPalette : P256LogPalette = nil;

  function LogBlackPalette : P256LogPalette;
    begin
      if not Assigned( fLogBlackPalette )
        then
          begin
            new( fLogBlackPalette );
            with fLogBlackPalette^ do
              begin
                Version         := $300;
                NumberOfEntries := 256;
                BlackLogEntries( Entries );
              end;
          end;
      Result := fLogBlackPalette;
    end;

  procedure BlackLogEntries( var LogEntries );
    var
      i       : integer;
      Entries : T256PalEntries absolute LogEntries;
    begin
      for i := low( Entries ) to high( Entries ) do
        with Entries[i] do
          begin
            peRed   := 0;
            peGreen := 0;
            peBlue  := 0;
            peFlags := PC_RESERVED or PC_NOCOLLAPSE;
          end;
    end;

  function PaletteHandle( const LogPalette ) : HPALETTE;
    var
      Palette : TLogPalette absolute LogPalette;
    begin
      Result := CreatePalette( Palette );
    end;

  function ClearSystemPalette : boolean;
    var
      ScreenDC      : HDC;
      ScreenPalette : HPALETTE;
    begin
      // Create, select, realize, deselect, and delete the palette
      ScreenDC := GetDC( 0 );
      ScreenPalette := PaletteHandle( LogBlackPalette^ );
      if ScreenPalette <> 0
        then
          begin
            ScreenPalette := SelectPalette( ScreenDC, ScreenPalette, false );
            Result := RealizePalette( ScreenDC ) > 0;
            ScreenPalette := SelectPalette( ScreenDC, ScreenPalette, false );
            DeleteObject( ScreenPalette );
          end
        else Result := false;
      ReleaseDC( 0, ScreenDC );
    end;

  function CreateIdentityLogPalette( Count : integer; const RgbQuads ) : T256LogPalette;
    var
      ScreenDC     : HDC;
      StaticColors : integer;
      Rgb          : TRgbPalette absolute RgbQuads;
    begin
      with Result do
        begin
          Version := $300;
          NumberOfEntries := 256;

          ScreenDC := GetDC( 0 );


          // For SYSPAL_NOSTATIC, just copy the color table into
          // a PALETTEENTRY array and replace the first and last entries
          // with black and white
          if GetSystemPaletteUse( ScreenDC ) = SYSPAL_NOSTATIC
            then
              begin
                // Fill in the palette with the given values, marking each
                // as PC_RESERVED

                RgbToLogEntries( 1, Count - 1, Rgb, Entries );
                MarkReserved( 1, Count - 1, Entries );

                // Make sure the last entry is white
                with Entries[255] do
                  begin
                    peRed   := 255;
                    peGreen := 255;
                    peBlue  := 255;
                    peFlags := 0;
                  end;

                // And the first is black
                with Entries[0] do
                  begin
                    peRed   := 0;
                    peGreen := 0;
                    peBlue  := 0;
                    peFlags := 0;
                  end;
              end
            else
              begin
                // For SYSPAL_STATIC, get the twenty static colors into
                // the array, then fill in the empty spaces with the
                // given color table
                // Get the static colors from the system palette
                GetSystemPaletteEntries( ScreenDC, 0, 256, Entries );

                // Set the peFlags of the lower static colors to zero
                StaticColors := GetDeviceCaps( ScreenDC, NUMCOLORS ) div 2;

                // Fill in the entries from the given color table
                RgbToLogEntries( StaticColors, Count - 2 * StaticColors, Rgb, Entries );
                MarkReserved( StaticColors, 256 - StaticColors, Entries );

                // Set the peFlags of the static colors to zero
                MarkNone( 0, StaticColors, Entries );
                MarkNone( 256 - StaticColors, StaticColors, Entries );
              end;

          ReleaseDC( 0, ScreenDC );
        end;
    end;

  function CreateIdentityPalette( Count : integer; const RgbQuads ) : HPALETTE;
    var
      LogPalette : T256LogPalette;
    begin
      LogPalette := CreateIdentityLogPalette( Count, RgbQuads );
      Result     := PaletteHandle( LogPalette );
    end;

  procedure LogToRgbEntries( Indx, Count : integer; const LogEntries; var RgbQuads );
    var
      i      : integer;
      LogPal : T256PalEntries absolute LogEntries;
      RgbPal : TRgbPalette    absolute RgbQuads;
    begin
      for i := Indx to Indx + Count - 1 do
        with RgbPal[i], LogPal[i] do
          begin
            rgbRed      := peRed;
            rgbGreen    := peGreen;
            rgbBlue     := peBlue;
            rgbReserved := 0;
          end;
    end;

  procedure RgbToLogEntries( Indx, Count : integer; const RgbQuads; var LogEntries );
    var
      i      : integer;
      LogPal : T256PalEntries absolute LogEntries;
      RgbPal : TRgbPalette    absolute RgbQuads;
    begin
      for i := Indx to Indx + Count - 1 do
        with RgbPal[i], LogPal[i] do
          begin
            peRed   := rgbRed;
            peGreen := rgbGreen;
            peBlue  := rgbBlue;
          end;
    end;

  procedure DosToRgbEntries( Indx, Count : integer; const DosRgbEntries; var RgbQuads );
    var
      i      : integer;
      DosPal : TDosRgbPalette absolute DosRgbEntries;
      RgbPal : TRgbPalette    absolute RgbQuads;
    begin
      for i := Indx to Indx + Count - 1 do
        with RgbPal[i], DosPal[i] do
          begin
            rgbRed      := r;
            rgbGreen    := g;
            rgbBlue     := b;
            rgbReserved := 0;
          end;
    end;

  procedure FillWithGradient( Source, Dest : TRgbQuad; Indx, aCount : integer; var RgbQuads );
    var
      i            : integer;
      Entries      : TRgbPalette absolute RgbQuads;
      CurrentRed   : integer;
      CurrentGreen : integer;
      CurrentBlue  : integer;
      DeltaRed     : integer;
      DeltaGreen   : integer;
      DeltaBlue    : integer;
    begin
      Entries[Indx + aCount - 1] := Dest;   // Ending color..
      Entries[Indx]              := Source; // Starting color..

      with Source do          // Set up delta & current..
        begin
          CurrentRed   := 1024 * rgbRed; // To gain precision we must multiply delta & keep a "current"
          DeltaRed     := ( Dest.rgbRed - rgbRed     ) * 1024 div aCount;
          CurrentGreen := 1024 * rgbGreen;
          DeltaGreen   := ( Dest.rgbGreen - rgbGreen ) * 1024 div aCount;
          CurrentBlue  := 1024 * rgbBlue;
          DeltaBlue    := ( Dest.rgbBlue - rgbBlue   ) * 1024 div aCount;
        end;

      for i := Indx + 1 to Indx + aCount - 2 do
        begin
          with Entries[i] do
            begin
              inc( CurrentRed, DeltaRed );
              inc( CurrentGreen, DeltaGreen );
              inc( CurrentBlue, DeltaBlue );

              rgbRed   := CurrentRed   div 1024;
              rgbGreen := CurrentGreen div 1024;
              rgbBlue  := CurrentBlue  div 1024;
            end;
        end;
    end;

  procedure GetDefaultPalette( Count : cardinal; var RgbQuads );
    begin
      if Count = 2
        then Move( RgbMonochromePalette, RgbQuads, Count * sizeof(TRgbQuad) )
        else Move( RgbHalftonePalette^, RgbQuads, Count * sizeof(TRgbQuad) );
    end;

  procedure GetLogPalette( hPal : HPALETTE; Indx, Count : integer; var LogEntries; PreserveFlags : boolean );
    var
      PaletteSize : integer;
      i           : integer;
      PalEntries  : T256PalEntries absolute LogEntries;
      TmpEntries  : T256PalEntries;
    begin
      PaletteSize := 0;
      if ( GetObject( hPal, sizeof( PaletteSize ), @PaletteSize ) <> 0 )
          and ( PaletteSize <> 0 )
        then
          begin
            Count := Min( PaletteSize - Indx, Count );
            if PreserveFlags
              then
                begin
                  GetPaletteEntries( hpal, Indx, Count, TmpEntries );
                  for i := Indx to Indx + Count - 1 do
                    with PalEntries[i] do
                      begin
                        peRed   := TmpEntries[i].peRed;
                        peGreen := TmpEntries[i].peGreen;
                        peBlue  := TmpEntries[i].peBlue;
                      end;
                end
              else GetPaletteEntries( hpal, Indx, Count, LogEntries );
          end;
    end;

  procedure GetLogSysPalette( Indx, Count : integer; var LogEntries );
    var
      ScreenDC   : HDC;
    begin
      ScreenDC := GetDC( 0 );
      GetSystemPaletteEntries( ScreenDC, Indx, Count, LogEntries );
      ReleaseDC( 0, ScreenDC );
    end;

  procedure GetRgbPalette( hPal : HPALETTE; Indx, Count : integer; var RgbQuads );
    var
      LogEntries  : T256PalEntries;
      Palette     : TRgbPalette absolute RgbQuads;
    begin
      GetLogPalette( hPal, Indx, Count, LogEntries, false );
      LogToRgbEntries( Indx, Count, LogEntries, Palette );
    end;

  procedure GetRgbSysPalette( Indx, Count : integer; var RgbQuads );
    var
      LogEntries : T256PalEntries;
      Palette    : TRGBPalette absolute RgbQuads;
    begin
      GetLogSysPalette( Indx, Count, LogEntries );
      LogToRgbEntries( Indx, Count, LogEntries, Palette );
    end;

  procedure MarkReserved( Indx, Count : integer; var LogEntries );
    var
      i       : integer;
      Entries : T256PalEntries absolute LogEntries;
    begin
      for i := Indx to Indx + Count - 1 do
        with Entries[i] do
          peFlags := PC_RESERVED or PC_NOCOLLAPSE;
    end;

  procedure MarkNoCollapse( Indx, Count : integer; var LogEntries );
    var
      i       : integer;
      Entries : T256PalEntries absolute LogEntries;
    begin
      for i := Indx to Indx + Count - 1 do
        with Entries[i] do
          peFlags := PC_NOCOLLAPSE;
    end;

  procedure MarkNone( Indx, Count : integer; var LogEntries );
    var
      i       : integer;
      Entries : T256PalEntries absolute LogEntries;
    begin
      for i := Indx to Indx + Count - 1 do
        with Entries[i] do
          peFlags := 0;
    end;

  function GetBitmapHandle( DC : HDC ) : HBITMAP;
    var
      TmpBitmap : HBITMAP;
    begin
      TmpBitmap := Windows.CreateBitmap( 1, 1, 1, 1, nil );
      Result := SelectObject( DC, TmpBitmap );
      SelectObject( DC, Result );
      DeleteObject( TmpBitmap );
    end;

  function GetPaletteHandle( DC : HDC ) : HPALETTE;
    begin
      Result := SelectPalette( DC, GetStockObject( DEFAULT_PALETTE ), true );
      DeleteObject( SelectPalette( DC, Result, true ) );
    end;

  function IsIdentityPalette( DC : HDC ) : boolean;
    var
      SysPalette : TRgbPalette;
      LogPalette : TRgbPalette;
    begin
      GetRgbSysPalette( 0, 256, SysPalette );
      GetRgbPalette( GetPaletteHandle( DC ), 0, 256, LogPalette );
      Result := BufferEqual( @LogPalette, @SysPalette, sizeof( LogPalette ) );
    end;

  const
    clBlack = $00000000;
    clWhite = $00FFFFFF;

  const     // These are the GetSysColor display element identifiers
    NumSysColors = 19;
    SysPalIndex  : array[0..NumSysColors - 1] of integer =
      ( 
        COLOR_ACTIVEBORDER,    COLOR_ACTIVECAPTION,
        COLOR_APPWORKSPACE,    COLOR_BACKGROUND,
        COLOR_BTNFACE,         COLOR_BTNSHADOW,
        COLOR_BTNTEXT,         COLOR_CAPTIONTEXT,
        COLOR_GRAYTEXT,        COLOR_HIGHLIGHT,
        COLOR_HIGHLIGHTTEXT,   COLOR_INACTIVEBORDER,
        COLOR_INACTIVECAPTION, COLOR_MENU,
        COLOR_MENUTEXT,        COLOR_SCROLLBAR,
        COLOR_WINDOW,          COLOR_WINDOWFRAME,
        COLOR_WINDOWTEXT
      );

  var // This array holds the old color mapping so we can restore them
    OldColors : array[0..NumSysColors - 1] of longint =
      ( 
        clBlack, clWhite, clWhite, clWhite,
        clWhite, clBlack, clBlack, clBlack,
        clBlack, clBlack, clWhite, clWhite,
        clWhite, clWhite, clBlack, clWhite,
        clWhite, clBlack, clBlack
      );

  procedure ActivateAppPalette( Active : boolean );

    const // This array translates the display elements to black and white
      MonoColors : array[0..NumSysColors - 1] of longint =
        (
          clBlack, clWhite, clWhite, clWhite,
          clWhite, clBlack, clBlack, clBlack,
          clBlack, clBlack, clWhite, clWhite,
          clWhite, clWhite, clBlack, clWhite,
          clWhite, clBlack, clBlack
        );

    var
      ScrDC : HDC;
      i     : integer;
    begin
      ScrDC := GetDC( 0 );  // Just use the screen DC

      // If the app is activating, save the current color mapping
      // and switch to SYSPAL_NOSTATIC
      if Active and ( GetSystemPaletteUse( ScrDC ) = SYSPAL_STATIC )
        then
          begin
            // Store the current mapping
            for i :=0 to NumSysColors do
              OldColors[i] := GetSysColor( SysPalIndex[i] );

            // Switch to SYSPAL_NOSTATIC and remap the colors
            SetSystemPaletteUse( ScrDC, SYSPAL_NOSTATIC );
            SetSysColors( NumSysColors, SysPalIndex, MonoColors );
          end
      else
        if not Active
          then
            begin
              // Switch back to SYSPAL_STATIC and the old mapping
              SetSystemPaletteUse( ScrDC, SYSPAL_STATIC );
              SetSysColors( NumSysColors, SysPalIndex, OldColors );
             end;

      ReleaseDC( 0, ScrDC ); // Be sure to release the DC!
    end;

  function CopyPalette( Palette : HPALETTE ) : HPALETTE;
    var
      PaletteSize : integer;
      LogSize     : integer;
      LogPalette  : PLogPalette;
    begin
      Result := 0;
      if Palette <> 0
        then
          begin
            PaletteSize := 0;
            if ( GetObject( Palette, sizeof( PaletteSize ), @PaletteSize ) <> 0 )
                and ( PaletteSize <> 0 )
              then
                begin
                  LogSize := sizeof( TLogPalette ) + ( PaletteSize - 1 ) * sizeof( TPaletteEntry );
                  GetMem( LogPalette, LogSize );
                  try
                    with LogPalette^ do
                      begin
                        palVersion    := $0300;
                        palNumEntries := PaletteSize;
                        GetPaletteEntries( Palette, 0, PaletteSize, palPalEntry );
                      end;
                    Result := CreatePalette( LogPalette^ );
                  finally
                    FreeMem( LogPalette );
                  end;
                end;
          end;
    end;

  // Screen information ---------------------------------------------------------

  procedure GetColorAvailInfo( var FirstAvailColor, LastAvailColor, AvailColors : integer );
    var
      ScreenDC  : HDC;
      SysColors : integer;
    begin
      ScreenDC := GetDC( 0 );
      SysColors := GetDeviceCaps( ScreenDC, NUMCOLORS );
      if SysColors > 0
        then
          if GetSystemPaletteUse( ScreenDC ) = SYSPAL_NOSTATIC
            then
              begin
                FirstAvailColor := 1;
                LastAvailColor  := 254;
                AvailColors     := 254;
              end
            else
              begin
                FirstAvailColor := SysColors div 2;
                LastAvailColor  := 256 - SysColors div 2;
                AvailColors     := 256 - SysColors;
              end
        else
          begin
            AvailColors := 256;
            FirstAvailColor := 0;
            LastAvailColor := 255;
          end;
      ReleaseDC( 0, ScreenDC );
    end;

  function AvailableColors : integer;
    var
      ScreenDC  : HDC;
      SysColors : integer;
    begin
      ScreenDC := GetDC( 0 );
      SysColors := GetDeviceCaps( ScreenDC, NUMCOLORS );
      if SysColors > 0
        then
          if GetSystemPaletteUse( ScreenDC ) = SYSPAL_NOSTATIC
            then Result := 254
            else Result := 256 - SysColors
        else Result := 256;
      ReleaseDC( 0, ScreenDC );
    end;

  function FirstAvailColor : integer;
    var
      ScreenDC  : HDC;
      SysColors : integer;
    begin
      ScreenDC := GetDC( 0 );
      SysColors := GetDeviceCaps( ScreenDC, NUMCOLORS );
      if SysColors > 0
        then
          if GetSystemPaletteUse( ScreenDC ) = SYSPAL_NOSTATIC
            then Result := 1
            else Result := SysColors div 2
        else Result := 0;
      ReleaseDC( 0, ScreenDC );
    end;

  function LastAvailColor : integer;
    var
      ScreenDC  : HDC;
      SysColors : integer;
    begin
      ScreenDC := GetDC( 0 );
      SysColors := GetDeviceCaps( ScreenDC, NUMCOLORS );
      if SysColors > 0
        then
          if GetSystemPaletteUse( ScreenDC ) = SYSPAL_NOSTATIC
            then Result := 254
            else Result := 255 - SysColors div 2
        else Result := 255;
      ReleaseDC( 0, ScreenDC );
    end;

  function ScreenDeviceWidth : integer;
    begin
      Result := GetSystemMetrics( SM_CXSCREEN );
    end;

  function ScreenDeviceHeihgt : integer;
    begin
      Result := GetSystemMetrics( SM_CYSCREEN );
    end;

  function ScreenDeviceBitsPerPixel : integer;
    var
      dc : HDC;
    begin
      dc := GetDC( 0 );
      Result := GetDeviceCaps( dc, BITSPIXEL ) * GetDeviceCaps( dc, PLANES );
      ReleaseDC( 0, dc );
    end;

  function ScreenDeviceIsPaletted : boolean;
    var
      dc : HDC;
    begin
      dc := GetDC( 0 );
      Result := GetDeviceCaps( dc, RASTERCAPS ) and RC_PALETTE <> 0;
      ReleaseDC( 0, dc );
    end;

  // Halftoning stuff -----------------------------------------------------------

  // Division lookup tables. These tables compute 0-255 divided by 51 and
  // modulo 51. These tables could approximate gamma correction.

  const
(*
    DividedBy51Rounded : array[0..255] of byte =
      (
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
        2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
        4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
        4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
        5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
      );
*)
    DividedBy51 : array[0..255] of byte =
      (
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
        2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
        4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
        4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5
      );

    Modulo51 : array[0..255] of byte =
      (
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
        20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37,
        38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 0, 1, 2, 3, 4, 5, 6,
        7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
        26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43,
        44, 45, 46, 47, 48, 49, 50, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
        13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
        31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48,
        49, 50, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
        18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35,
        36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 0, 1, 2, 3,
        4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22,
        23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
        41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 0
      );

    // Multiplication LUTs.  These compute 0-5 times 6 and 36.

    Times6 : array[0..5] of byte =
      ( 
        0, 6, 12, 18, 24, 30
      );
    Times36 : array[0..5] of byte =
      (
        0, 36, 72, 108, 144, 180
      );

    // Dither matrices for 8 bit to 2.6 bit halftones.

    Halftone8x8 : array[0..63] of byte =
      ( 
         0, 38,  9, 47,  2, 40, 11, 50,
        25, 12, 35, 22, 27, 15, 37, 24,
         6, 44,  3, 41,  8, 47,  5, 43,
        31, 19, 28, 15, 34, 21, 31, 18,
         1, 39, 11, 49,  0, 39, 10, 48,
        27, 14, 36, 23, 26, 13, 35, 23,
         7, 46,  4, 43,  7, 45,  3, 42,
        33, 20, 30, 17, 32, 19, 29, 16
      );

    // Translates a 2.6 bit-per-pixel halftoned representation into the
    // slightly rearranged WinG Halftone Palette.

    HalftoneTranslation : array[0..215] of byte =
      (
        0,       29,      30,      31,      32,      249,     33,      34,
        35,      36,      37,      38,      39,      40,      41,      42,
        43,      44,      45,      46,      47,      48,      49,      50,
        51,      52,      53,      54,      55,      56,      250,     250,
        57,      58,      59,      251,     60,      61,      62,      63,
        64,      65,      66,      67,      68,      69,      70,      71,
        72,      73,      74,      75,      76,      77,      78,      79,
        80,      81,      82,      83,      84,      85,      86,      87,
        88,      89,      250,     90,      91,      92,      93,      94,
        95,      96,      97,      98,      99,      100,     101,     102,
        103,     104,     105,     106,     107,     108,     109,     110,
        111,     227,     112,     113,     114,     115,     116,     117,
        118,     119,     151,     120,     121,     122,     123,     124,
        228,     125,     126,     229,     133,     162,     135,     131,
        132,     137,     166,     134,     140,     130,     136,     143,
        138,     139,     174,     141,     142,     177,     129,     144,
        145,     146,     147,     148,     149,     150,     157,     152,
        153,     154,     155,     156,     192,     158,     159,     160,
        161,     196,     163,     164,     165,     127,     199,     167,
        168,     169,     170,     171,     172,     173,     207,     175,
        176,     210,     178,     179,     180,     181,     182,     183,
        184,     185,     186,     187,     188,     189,     190,     191,
        224,     193,     194,     195,     252,     252,     197,     198,
        128,     253,     252,     200,     201,     202,     203,     204,
        205,     206,     230,     208,     209,     231,     211,     212,
        213,     214,     215,     216,     217,     218,     219,     220,
        221,     222,     254,     223,     232,     225,     226,     255
      );

  function HalftonePaletteIndx( x, y : integer; RgbColor : TRgbQuad ) : byte;
    var
      RedTemp, GreenTemp, BlueTemp : byte;
      PaletteIndx                  : integer;
      HalftonePos                  : integer;
    begin
      with RgbColor do
        begin
          // Now, look up each value in the halftone matrix using an 8x8 ordered dither.
          HalftonePos := (x mod 8) * 8 + y mod 8;
          if Modulo51[ rgbRed ] > Halftone8x8[ HalftonePos ]
            then RedTemp := succ( DividedBy51[ rgbRed ] )
            else RedTemp := DividedBy51[ rgbRed ];
          if Modulo51[ rgbGreen ] > Halftone8x8[ HalftonePos ]
            then GreenTemp := succ( DividedBy51[ rgbGreen ] )
            else GreenTemp := DividedBy51[ rgbGreen ];
          if Modulo51[ rgbBlue ] > Halftone8x8[ HalftonePos ]
            then BlueTemp := succ( DividedBy51[ rgbBlue ] )
            else BlueTemp := DividedBy51[ rgbBlue ];

          // Recombine the halftoned RGB values into a palette index
          PaletteIndx := RedTemp + Times6[GreenTemp] + Times36[BlueTemp];

          // And translate through the Halftone Palette translation vector to give the correct value
          Result := HalftoneTranslation[PaletteIndx];
        end;
    end;

  var
    fRgbHalftonePalette : PRgbPalette    = nil;
    fLogHalftonePalette : P256LogPalette = nil;

  function LogHalftonePalette : P256LogPalette;
    var
      hPal : HPALETTE;
    begin
      if fLogHalftonePalette = nil
        then
          begin
            new( fLogHalftonePalette );
            with fLogHalftonePalette^ do
              begin
                Version         := $300;
                NumberOfEntries := 256;
                hPal            := CreateHalftonePalette( 0 );
                try
                  GetLogPalette( hPal, 0, 256, Entries, false );
                  MarkReserved( 0, 256, Entries );
                finally
                  DeleteObject( hPal );
                end;
              end;
          end;
      Result := fLogHalftonePalette;
    end;

  function RgbHalftonePalette : PRgbPalette;
    begin
      if not Assigned( fRgbHalftonePalette )
        then
          begin
            new( fRgbHalftonePalette );
            LogToRgbEntries( 0, 256, LogHalftonePalette^, fRgbHalftonePalette^ );
          end;
      Result := fRgbHalftonePalette;
    end;

  function GetNearestEntry( const RgbQuads; Count : integer; RgbQuad : dword ) : integer;
    var
      i             : integer;
      Dist, MinDist : integer;
      RgbEntries    : TRgbPalette absolute RgbQuads;
      Color         : TRgbQuad    absolute RgbQuad;
    begin
      MinDist := MaxInt;
      Result  := 0;
      for i := 0 to Count - 1 do
        with RgbEntries[i] do
          begin
            Dist := Square[ Color.rgbRed - rgbRed ] + Square[ Color.rgbGreen - rgbGreen ] + Square[ Color.rgbBlue - rgbBlue ];
            if Dist < MinDist
              then
                begin
                  Result := i;
                  if Dist > 0
                    then MinDist := Dist
                    else break;
                end;
          end;
    end;

initialization
  fStockPen   := GetStockObject( BLACK_PEN );
  fStockBrush := GetStockObject( HOLLOW_BRUSH );
  fStockFont  := GetStockObject( SYSTEM_FONT );

finalization
  DeleteObject( fStockFont );
  DeleteObject( fStockBrush );
  DeleteObject( fStockPen );

  if Assigned( fLogBlackPalette )
    then  dispose( fLogBlackPalette );
  if Assigned( fLogHalftonePalette )
    then  dispose( fLogHalftonePalette );
  if Assigned( fRgbHalftonePalette )
    then  dispose( fRgbHalftonePalette );

end.
