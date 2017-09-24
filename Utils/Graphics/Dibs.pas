unit Dibs;

// Copyright ( c) 1996 Jorge Romero Gomez, Merchise.

interface

  uses
    Windows, Classes, SysUtils, BitBlt, GDI, Rects;

  function DwordAlign( Size : integer ) : integer;
  function DibStorageWidth( biWidth, biBitCount : integer ) : integer;

  // Dib functions ----------------------------------------------------

  type
    TDib = TBitmapInfoHeader;
    PDib = PBitmapInfoHeader;

  function  DibNewHeader( Width, Height : integer; BitCount : integer ) : PDib;
  // Use a negative BitCount to avoid adding the RGB palette array to the header

  procedure DibFixHeader( DibHeader : PDib );
  function  DibCopyHeader( DibHeader : PDib ) : PDib;
  procedure SetDefaultHeader( Width, Height : integer; BitCount : integer; Size : cardinal; DibHeader : PDib );

  function  DibCreate( Width, Height : integer; BitCount : integer ) : PDib;
  procedure DibFree( Dib : PDib );
  function  DibCopy( DibHeader : PDib; DibPixels : pointer ) : PDib;
  function  DibClipboardFormat( DibHeader : PDib; DibPixels : pointer ) : HGLOBAL;

  function  DibIsTopDown( DibHeader : PDib ) : boolean;
  function  DibWidthBytes( DibHeader : PDib ) : cardinal;
  function  DibHeight( DibHeader : PDib ) : cardinal;
  function  DibSizeImage( DibHeader : PDib ) : cardinal;
  function  DibColors( DibHeader : PDib ) : PRgbPalette;

  function  DibNumColors( DibHeader : PDib ) : integer;
  function  DibPaletteSize( DibHeader : PDib ) : cardinal;
  function  DibUsesPalette( DibHeader : PDib ) : boolean;
  function  DibPalette( DibHeader : PDib; ReserveColors : boolean ) : HPALETTE;

  // Dib access ----------------------------------------------------

  function  DibSize( Dib : PDib ) : cardinal;
  function  DibPtr( Dib : PDib ) : pointer;

  function  DibPixelOfs( Dib : PDib; x, y : integer ) : cardinal;
  function  DibRowOfs( Dib : PDib; Row : integer ) : cardinal;
  function  DibPixelAddr( Dib : PDib; DibPixels : pointer; x, y : integer ) : pointer;
  function  DibScanLine( Dib : PDib; DibPixels : pointer; Row : integer ) : pointer;

  function  GetPixelMono( RowStart : pointer; x : integer ) : boolean; // true if pixel is white
  function  GetPixel4( RowStart : pointer; x : integer ) : byte;       // returns 0..15
  procedure SetPixelMono( RowStart : pointer; x : integer; Pixel : boolean );
  procedure SetPixel4( RowStart : pointer; x : integer; Pixel : byte );

  type
    PGetRgbData = ^TGetRgbData;
    TGetRgbData =
      record
        BitCount               : integer;
        Colors                 : PRgbPalette;
        rRight, gRight, bRight : integer;
        rMask, gMask, bMask    : integer;
        rLeft, gLeft, bLeft    : integer;
      end;

  procedure DibGetRgbBegin( Source : PDib; var Data : pointer );
    // Call this one first, keep the data returned
  function DibGetRgb( SourceScanLine : pointer; x : integer; Data : pointer ) : TRgbQuad;
    // Get pixel at Scanline[x]
  procedure DibSetRgb( SourceScanLine : pointer; x : integer; Data : pointer; Rgb : TRgbQuad );
    // Set pixel at Scanline[x]
  procedure DibGetRgbFree( Data : pointer );
    // Clean up

  // Dib drawing ----------------------------------------------------

  procedure DibDrawOnDC( dc : HDC; x, y : integer; DibHeader : PDib; DibPixels : pointer; Usage, aCopyMode : dword );
  procedure DibStretchDrawOnDC( dc : HDC; const Rect : TRect; DibHeader : PDib; DibPixels : pointer; Usage, aCopyMode : dword );
  procedure DibClipDrawOnDC( dc : HDC; x, y : integer; const ClippingRect : TRect; DibHeader : PDib; DibPixels : pointer; Usage, aCopyMode : dword );
  procedure DibClipStretchDrawOnDC( dc : HDC; const Rect, ClippingRect : TRect; DibHeader : PDib; DibPixels : pointer; Usage, aCopyMode : dword );

  // This only works with paletted DIBs
  function  DibCreateMask8( DibHeader : PDib; DibPixels : pointer; TransparentRgb : longint ) : HBITMAP;

  // Dib I/O ----------------------------------------------------

  procedure DibWriteFileHeader( DibHeader : PDib; const Stream : TStream );
  function  DibReadHeader( const Stream : TStream ) : PDib;
  procedure DibReadPixels( const Stream : TStream; DibHeader : PDib; DibPixels : pointer; ForceTopDown : boolean );
  procedure DibWriteHeader( const Stream : TStream; DibHeader : PDib );
  procedure DibWritePixels( const Stream : TStream; DibHeader : PDib; DibPixels : pointer );

  function  DibLoadFromStream( const Stream : TStream; ForceTopDown : boolean ) : PDib;
  procedure DibSaveToStream( const Stream : TStream; DibHeader : PDib; DibPixels : pointer );

  const
    bfIcon   = $4349; // 'IC'
    bfBitmap = $4d42; // 'BM'
    bfCursor = $5450; // 'PT'

  function  DibLoadFromFile( const Filename : string; ForceTopDown : boolean ) : PDib;
  procedure DibSaveToFile( const Filename : string; DibHeader : PDib; DibPixels : pointer );

  function  DibSectionLoadFromFile( const Filename : string; var DibHeader : PDib; var DibPixels : pointer;
                                    ForceTopDown : boolean ) : HBITMAP;
  function  DibSectionLoadFromStream( const Stream : TStream; var DibHeader : PDib; var DibPixels : pointer;
                                      ForceTopDown : boolean ) : HBITMAP;


  // Dib I/O Hooking --------------------------------------------

  // A procedure able to process data from SourcePixels to DibPixels, leaving an uncompressed single plane DIB 
  type
    TDibIOHook = procedure ( SourcePixels : pointer;
                             DibHeader : PDib; DibPixels : pointer; ForceTopDown : boolean );

  function  SearchHookById( anId : integer ) : TDibIOHook; 

  procedure RegisterDibIOHooks( CompressionIDs : array of integer; NewHook : TDibIOHook );
  procedure UnregisterDibIOHooks( CompressionIDs : array of integer );

  // Dib Sections ----------------------------------------------------

  const
    dsUseDefaultPalette = pointer( -1 );
    dsUseNoPalette      = pointer( 0 );

  function  DibSectionCreate( Width, Height : integer; BitCount : integer; UsePalette : PRgbPalette;
                              var DibHeader : PDib; var DibPixels : pointer ) : HBITMAP;
  procedure DibSectionFree( DibHeader : PDib; Section : HBITMAP );

  // Halftoning (to Win95 palette) ------------------------------------------------------

  procedure DibCopyHalftoned( Source : PDib; DibPixels : pointer; Dest : PDib; DestPixels : pointer );

  function  DibHalftoned( Source : PDib ) : PDib;
  function  DibSectionHalftoned( Source : PDib; DibPixels : pointer; var DestHeader : PDib; var DestPixels : pointer ) : HBITMAP;

  // Flipping & rotation ----------------------------------------------------------------

  procedure DibRotate90( SourceHeader : PDib; DibPixels, DestPixels : pointer );
  procedure DibRotate180( SourceHeader : PDib; DibPixels, DestPixels : pointer );
  procedure DibRotate270( SourceHeader : PDib; DibPixels, DestPixels : pointer );

  procedure DibCopyRotated( SourceHeader : PDib; DibPixels, DestPixels : pointer; Angle : integer );
  procedure DibCopyFlippedHor( SourceHeader : PDib; DibPixels, DestPixels : pointer );
  procedure DibCopyFlippedVert( SourceHeader : PDib; DibPixels, DestPixels : pointer );

  procedure DibFlipHorizontal( DibHeader : PDib; DibPixels : pointer; Width, Height : integer );
  procedure DibFlipVertical( DibHeader : PDib; DibPixels : pointer; Width, Height : integer );

  // Dib conversions -------------------------------------------------------------

  function  DibSetUsage( DibHeader : PDib; Palette : HPALETTE; Usage : cardinal ) : boolean;

  function  DibInvert( DibHeader : PDib; DibPixels : pointer ) : PDib; // Returns a top-down dib from a bottom-up and viceversa

  function  DibFromSection( DibHeader : PDib; DibPixels : pointer ) : PDib;
  function  DibFromBitmap( Handle : HBITMAP; Palette : HPALETTE ) : PDib;
  function  BitmapFromDib( DibHeader : PDib; DibPixels : pointer ) : HBITMAP;
  function  SectionFromDib( DibHeader : PDib; DibPixels : pointer ) : HBITMAP;
  function  DibSectionFromDib( SourceHeader : PDib; DibPixels : pointer; var newHeader : PDib; var newPixels : pointer ) : HBITMAP;

  // These ones reconstruct DibSection's information from an HDC or an HBITMAP
  function  DibSectionFromDC( SectionDC : HDC; var Handle : HBITMAP; var DibHeader : PDib; var DibPixels : pointer ) : boolean;
  function  DibSectionFromHandle( Section : HBITMAP; var DibHeader : PDib; var DibPixels : pointer ) : boolean;

  // Exceptions

  type
    EBitmapError             = class( Exception );
    EUnsupportedBitmapPlanes = class( EBitmapError );
    EUnsupportedBitmapFormat = class( EBitmapError );

implementation

  uses
     NumUtils, MemUtils;

  function DibCopyHeader( DibHeader : PDib ) : PDib;
    var
      size : integer;
    begin
      size := AllocatedSize( DibHeader );
      getmem( Result, size );
      move( DibHeader^, Result^, size );
    end;

  function DibCreateMask8( DibHeader : PDib; DibPixels : pointer; TransparentRgb : longint ) : HBITMAP;
    // It's a quite simple idea: I'm using the palette to change all colors except TransparentRgb
    // to black (and TransparentRgb to white), then I bitblt it to a bitmap. All that's left
    // is to restore the original palette  
    var
      SaveColors : PRgbPalette;
      Colors     : PRgbPalette;
      ColorCount : integer;
      dc         : HDC;
      oldBitmap  : HBITMAP;
      i          : integer;
    begin
      ColorCount := DibNumColors( DibHeader );

      assert( ColorCount > 0, 'Non-paletted DIB in Dibs.DibCreateMask8!!' );

      Colors := DibColors( DibHeader );
      GetMem( SaveColors, ColorCount * sizeof( TRgbQuad ) );
      try
        move( Colors^, SaveColors^, ColorCount * sizeof( TRgbQuad ) );

        for i := 0 to ColorCount - 1 do
          begin
            if longint( Colors[i] ) <> TransparentRgb
              then Colors[i] := rgbBlack
              else Colors[i] := rgbWhite;
          end;

        Result := Windows.CreateBitmap( DibHeader.biWidth, DibHeight( DibHeader ), 1, 1, nil );
        dc     := GetBitmapDC( Result, OldBitmap );
        try
          DibDrawOnDC( dc, 0, 0, DibHeader, DibPixels, DIB_RGB_COLORS, SRCCOPY );
        finally
          ReleaseBitmapDC( dc, OldBitmap );
        end;

        move( SaveColors^, Colors^, ColorCount * sizeof( TRgbQuad ) );
      finally
        FreeMem( SaveColors );
      end;
    end;

  procedure DibDrawOnDC( dc : HDC; x, y : integer; DibHeader : PDib; DibPixels : pointer; Usage, aCopyMode : dword );
    var
      Height : integer;
      Header : PBitmapInfo absolute DibHeader;
    begin
      with DibHeader^ do
        begin
          Height := DibHeight( DibHeader );
          if biHeight > 0
            then y := pred( biHeight ) - y;
          StretchDIBits( dc, x, y, biWidth, Height,
                         0, 0, biWidth, Height,
                         DibPixels, Header^, Usage, aCopyMode );
        end;
    end;

  procedure DibStretchDrawOnDC( dc : HDC; const Rect : TRect; DibHeader : PDib; DibPixels : pointer; Usage, aCopyMode : dword );
    var
      Header : PBitmapInfo absolute DibHeader;
      y      : integer;
    begin
      with DibHeader^, Rect do
        begin
          if biHeight > 0
            then y := pred( biHeight )
            else y := 0;
          StretchDIBits( dc, Left, Top, Right - Left, Bottom - Top,
                         0, y, biWidth, DibHeight( DibHeader ),
                         DibPixels, Header^, Usage, aCopyMode );
        end;
    end;

  procedure DibClipDrawOnDC( dc : HDC; x, y : integer; const ClippingRect : TRect; DibHeader : PDib; DibPixels : pointer; Usage, aCopyMode : dword );
    var
      SrcOfs  : TPoint;
      DstSize : TPoint;
      DstRect : TRect;
      Header  : PBitmapInfo absolute DibHeader;
    begin
      with DibHeader^, DstRect do
        begin
          DstRect := RectFromBounds( x, y, biWidth, DibHeight( DibHeader ) );
          if IntersectRect( DstRect, DstRect, ClippingRect )
            then
              begin
                DstSize := RectSize( DstRect );
                if x < DstRect.Left
                  then
                    begin
                      SrcOfs.x := DstRect.Left - x;
                      x        := DstRect.Left;
                    end
                  else SrcOfs.x := 0;
                if y < DstRect.Top
                  then
                    begin
                      SrcOfs.y := DstRect.Top - y;
                      y        := DstRect.Top;
                    end
                  else SrcOfs.y := 0;

                with SrcOfs do
                  if biHeight > 0
                    then y := pred( biHeight ) - y;
                StretchDIBits( dc, x, y, DstSize.x, DstSize.y,
                               SrcOfs.x, SrcOfs.y, DstSize.x, DstSize.y,
                               DibPixels, Header^, Usage, aCopyMode );
              end;
        end;
    end;

  procedure DibClipStretchDrawOnDC( dc : HDC; const Rect, ClippingRect : TRect; DibHeader : PDib; DibPixels : pointer; Usage, aCopyMode : dword );
    var
      SrcOfs  : TPoint;
      SrcSize : TPoint;
      DstRect : TRect;
      DstSize : TPoint;
      Height  : integer;
      Header  : PBitmapInfo absolute DibHeader;
    begin
      if IntersectRect( DstRect, ClippingRect, Rect )
        then
          with DibHeader^ do
            begin
              Height  := DibHeight( DibHeader );
              DstSize := RectSize( DstRect );

              if Rect.Left < DstRect.Left
                then SrcOfs.x := (DstRect.Left - Rect.Left) * biWidth div (Rect.Right - Rect.Left)
                else SrcOfs.x := 0;
              SrcSize.x := DstSize.x * biWidth div (Rect.Right - Rect.Left);

              if Rect.Top < DstRect.Top
                then SrcOfs.y := (DstRect.Top - Rect.Top) * Height div (Rect.Bottom - Rect.Top)
                else SrcOfs.y := 0;
              SrcSize.y := DstSize.y * Height div (Rect.Bottom - Rect.Top);

              with SrcOfs do
                if biHeight > 0
                  then y := pred( biHeight ) - y;
              StretchDIBits( dc, DstRect.Left, DstRect.Top, DstSize.x, DstSize.y,
                             SrcOfs.x, SrcOfs.y, SrcSize.x, SrcSize.y,
                             DibPixels, Header^, Usage, aCopyMode );
            end;
    end;

  function DwordAlign( Size : integer ) : integer;
    asm
      add  eax, 3
      and  al, not 3
    end;

  function DibStorageWidth( biWidth, biBitCount : integer ) : integer;
    // eax = biWidth
    // edx = biBitCount
    asm
      mov    ecx, edx
      mul    edx     // * biBitCount
      shr    eax, 3  // div 8
      add    eax, 3
      and    al, not 3
    end;//Result := DwordAlign( biWidth * ( biBitCount div 8 ) );

  const
    MonoMask  : array[0..7] of byte = ( 128, 64, 32, 16, 8, 4, 2, 1 );
    ColorMask : array[0..1] of byte = ( $F0, $0F );

  function GetPixelMono( RowStart : pointer; x : integer ) : boolean;
    // eax = RowStart
    // edx = x
    asm
      mov  ecx, edx
      and  ecx, 7  // mod 8
      shr  edx, 3  // div 8
      add  edx, eax
      xor  eax, eax
      mov  al, byte ptr MonoMask[ecx]
      and  al, [edx]
    end;

  function GetPixel4( RowStart : pointer; x : integer ) : byte;
    // eax = RowStart
    // edx = x
    asm
      mov  ecx, edx
      shr  edx, 1
      add  edx, eax
      xor  eax, eax
      mov  al, [edx]
      rcr  ecx, 1
      jc   @@LoPacket

    @@HiPacket:
      shr  al, 4
      jmp  @@Exit

    @@LoPacket:
      and  al, $F

    @@Exit:
    end;

  procedure SetPixelMono( RowStart : pointer; x : integer; Pixel : boolean );
    // eax = RowStart
    // edx = x
    // ecx = Pixel
    asm
      push ebx
      mov  ebx, ecx  // ebx = Pixel
      mov  ecx, edx  // ecx = x
      shr  edx, 3    // div 8
      and  ecx, 7    // mod 8
      add  edx, eax
      mov  cl, byte ptr MonoMask[ecx]
      or   ebx, ebx
      jz   @nopixel

    @setpixel:
      or   [edx], cl
      jmp  @exit

    @nopixel:
      not  cl
      and  [edx], cl

    @exit:
      pop  ebx
    end;

  procedure SetPixel4( RowStart : pointer; x : integer; Pixel : byte );
    // eax = RowStart
    // edx = x
    // ecx = Pixel
    asm
      push ebx
      mov  ebx, edx
      shr  edx, 1
      add  edx, eax
      xor  eax, eax
      rcr  ebx, 1
      jnc  @@HiPacket

    @@LoPacket:
      mov  al, byte ptr ColorMask[0]
      and  [edx], al
      or   [edx], cl
      jmp  @@exit

    @@HiPacket:
      shl  cl, 4
      mov  al, byte ptr ColorMask[1]
      and  [edx], al
      or   [edx], cl

    @@exit:
      pop  ebx
    end;

  procedure DibFixHeader( DibHeader : PDib );
    var
      ImgSize : integer;
    begin
     if ( DibHeader.biCompression = BI_BITFIELDS ) and ( DibHeader.biClrUsed = 0 )
        then DibHeader.biClrUsed := 3
        else
          if DibHeader.biClrUsed = 0
           then DibHeader.biClrUsed := DibNumColors( DibHeader );
      ImgSize := DibStorageWidth( DibHeader.biWidth, DibHeader.biBitCount ) * abs( DibHeader.biHeight );
      if ( DibHeader.biSizeImage = 0 ) or ( DibHeader.biSizeImage > ImgSize )
        then DibHeader.biSizeImage := ImgSize;
    end;

  function DibSetUsage( DibHeader : PDib; Palette : HPALETTE; Usage : cardinal ) : boolean;
  //
  // Modifies the color table of the passed DibHeader for use with the wUsage
  // parameter specifed.
  //
  // if wUsage is DIB_PAL_COLORS the DibHeader color table is set to 0-256
  // if wUsage is DIB_RGB_COLORS the DibHeader color table is set to the RGB values
  //   in the passed palette
  var
    RgbColors  : PRgbPalette;
    ColorCount : integer;
    i          : integer;
  begin
    Result := false;

    if Assigned( DibHeader )
      then
        begin
          ColorCount := DibNumColors( DibHeader );
          if ( ColorCount > 0 ) and ( ( ColorCount <> 3 ) or ( DibHeader.biCompression <> BI_BITFIELDS ) )
            then
              begin
                Result := true;
                RgbColors := DibColors( DibHeader );
                if Usage = DIB_PAL_COLORS
                  then
                    for i := 0 to ColorCount - 1 do // Set the DibHeader color table to palette indexes
                      pword( pchar( RgbColors ) + i * 2 )^ := i
                  else
                    begin                           // Set the DibHeader color table to RGBQUADS
                      
                      if Palette = 0
                        then
                          begin
                            Palette := CreateHalftonePalette( 0 );
                          end;
                      try
                        if ColorCount > 256
                          then ColorCount := 256;
                        GetRgbPalette( Palette, 0, ColorCount, RgbColors^ );
                      finally
                      end;  
                    end;
              end;
        end;
    end;

  procedure SetDefaultHeader( Width, Height : integer; BitCount : integer; Size : cardinal; DibHeader : PDib );
    begin
      with DibHeader^ do
        begin
          biSize          := sizeof( TBitmapInfoHeader );
          biWidth         := Width;
          biHeight        := Height;
          biPlanes        := 1;
          biBitCount      := BitCount;
          biCompression   := BI_RGB;
          biSizeImage     := Size;
          biXPelsPerMeter := 0;
          biYPelsPerMeter := 0;
          biClrImportant  := 0;
          case BitCount of
            1 : biClrUsed := 2;
            4 : biClrUsed := 16;
            8 : biClrUsed := 256;
            else
              biClrUsed := 0;
          end;
        end;
    end;

  function DibNewHeader( Width, Height : integer; BitCount : integer ) : PDib;
    var
      PalSize : integer;
    begin
      if BitCount > 0
        then PalSize := sizeof( TRGBQuad ) * PaletteSize( BitCount )
        else PalSize := 0;
      GetMem( Result, sizeof( TBitmapInfoHeader ) + PalSize );
      SetDefaultHeader( Width, Height, abs( BitCount ), Abs( Height ) * DibStorageWidth( Width, BitCount ), Result );
      GetDefaultPalette( Result.biClrUsed, DibColors( Result )^ );
    end;

  function DibCreate( Width, Height : integer; BitCount : integer ) : PDib;
    var
      SizeImage : cardinal;
    begin
      SizeImage := abs( Height ) * DibStorageWidth( Width, BitCount );
      GetMem( Result, sizeof( TBitmapInfoHeader ) + sizeof( TRGBQuad ) * PaletteSize( BitCount ) + SizeImage );
      SetDefaultHeader( Width, Height, BitCount, SizeImage, Result );
      GetDefaultPalette( Result.biClrUsed, DibColors( Result )^ );
    end;

  function DibClipboardFormat( DibHeader : PDib; DibPixels : pointer ) : HGLOBAL;
    var
      DibData : PDib;
    begin
      Result := GlobalAlloc( GMEM_MOVEABLE or GMEM_SHARE, DibSize( DibHeader ) );
      if Result <> 0
        then
          begin
            DibData := GlobalLock( Result );
            move( DibHeader^, DibData^, DibHeader.biSize + DibPaletteSize( DibHeader ) );
            move( DibPixels^, DibPtr( DibData )^, DibSizeImage( DibData ) );
            GlobalUnlock( Result );
          end;
    end;

  function DibCopy( DibHeader : PDib; DibPixels : pointer ) : PDib;
    begin
      if DibPixels = nil
        then DibPixels := DibPtr( DibHeader );
      getmem( Result, DibSize( DibHeader ) );
      move( DibHeader^, Result^, DibHeader.biSize + DibPaletteSize( DibHeader ) );
      move( DibPixels^, DibPtr( Result )^, DibSizeImage( Result ) );
    end;

  procedure DibFree( Dib : PDib );
    begin
      FreeMem( Dib );
    end;

  function DibHeight( DibHeader : PDib ) : cardinal;
    begin
      Result := abs( DibHeader.biHeight );
    end;

  function DibIsTopDown( DibHeader : PDib ) : boolean;
    begin
      Result := DibHeader.biHeight < 0;
    end;

  function DibColors( DibHeader : PDib ) : PRgbPalette;
    begin
      Result := pointer( pchar( DibHeader ) + sizeof( TBitmapInfoHeader ) );
    end;

  function DibNumColors( DibHeader : PDib ) : integer;
    begin
      if ( DibHeader.biClrUsed = 0 ) and ( DibHeader.biBitCount <= 8 )
        then Result := 1 shl DibHeader.biBitCount
        else Result := DibHeader.biClrUsed;
    end;

  function DibPaletteSize( DibHeader : PDib ) : cardinal;
    begin
      Result := DibNumColors( DibHeader ) * sizeof( TRGBQuad );
    end;

  function DibUsesPalette( DibHeader : PDib ) : boolean;
    var
      Colors : cardinal;
    begin
      Colors := DibNumColors( DibHeader );
      Result := ( Colors > 0 ) and ( ( Colors <> 3 ) or ( DibHeader.biCompression <> BI_BITFIELDS ) );
    end;

  function DibSizeImage( DibHeader : PDib ) : cardinal;
    begin
      if DibHeader.biSizeImage <> 0
        then Result := DibHeader.biSizeImage
        else Result := DibStorageWidth( DibHeader.biWidth, DibHeader.biBitCount ) * abs( DibHeader.biHeight );
    end;

  function DibPalette( DibHeader : PDib; ReserveColors : boolean ) : HPALETTE;
    var
      i            : integer;
      LogPalette   : PLogPalette;
      Colors       : integer;
      RgbPalette   : PRgbPalette;
    begin
      Colors := DibNumColors( DibHeader );
      if Colors <= 256
        then
          begin
            RgbPalette := DibColors( DibHeader );

            GetMem( LogPalette, sizeof( TLogPalette ) + ( Colors - 1 ) * sizeof( TPaletteEntry ) );
            with LogPalette^ do
              begin
                palVersion := $300;
                palNumEntries := Colors;

                for i:= 0 to Colors - 1 do
                  begin
                    palPalEntry[i].peRed   := RgbPalette[i].rgbRed;
                    palPalEntry[i].peGreen := RgbPalette[i].rgbGreen;
                    palPalEntry[i].peBlue  := RgbPalette[i].rgbBlue;
                    if ReserveColors
                      then palPalEntry[i].peFlags := PC_RESERVED
                      else palPalEntry[i].peFlags := PC_NOCOLLAPSE;
                  end;
              end;
            Result := PaletteHandle( LogPalette^ );
          end
        else Result := 0;
    end;

  function DibSize( Dib : PDib ) : cardinal;
    begin
      Result := Dib.biSize + DibPaletteSize( Dib ) + DibSizeImage( Dib );
    end;

  function DibPtr( Dib : PDib ) : pointer;
    begin
      if Dib.biCompression = BI_BITFIELDS
        then Result := pchar( Dib ) + sizeof( TBitmapInfoHeader ) + 3 * sizeof( TRgbQuad )
        else Result := pchar( Dib ) + sizeof( TBitmapInfoHeader ) + Dib.biClrUsed * sizeof( TRgbQuad );
    end;

  function DibWidthBytes( DibHeader : PDib ) : cardinal;
    begin
      Result := DibStorageWidth( DibHeader.biWidth, DibHeader.biBitCount );
    end;

  function DibPixelOfs( Dib : PDib; x, y : integer ) : cardinal;
    begin
      if Dib.biHeight > 0 // Dib is bottom-up?
        then y := Dib.biHeight - 1 - y;
      Result := y * DibStorageWidth( Dib.biWidth, Dib.biBitCount ) + x * Dib.biBitCount div 8;
    end;

  function DibRowOfs( Dib : PDib; Row : integer ) : cardinal;
    begin
      assert( Assigned( Dib ), 'Dib = NULL in Dibs.DibRowOfs!!' );

      if Dib.biHeight > 0 // Dib is bottom-up?
        then Row := Dib.biHeight - 1 - Row;
      Result := Row * DibStorageWidth( Dib.biWidth, Dib.biBitCount );
    end;

  function DibPixelAddr( Dib : PDib; DibPixels : pointer; x, y : integer ) : pointer;
    begin
      assert( Assigned( Dib ), 'Dib = NULL in Dibs.DibPixelAddr!!' );
      assert( Assigned( DibPixels ), 'DibPixels = NULL in Dibs.DibPixelAddr!!' );

      if Dib.biHeight > 0 // Dib is bottom-up?
        then y := Dib.biHeight - 1 - y;
      Result := pchar(DibPixels) + y * DibStorageWidth( Dib.biWidth, Dib.biBitCount ) + x * Dib.biBitCount div 8;
    end;

  function DibScanLine( Dib : PDib; DibPixels : pointer; Row : integer ) : pointer;
    begin
      assert( Assigned( Dib ), 'Dib = NULL in Dibs.DibScanLine!!' );
      assert( Assigned( DibPixels ), 'DibPixels = NULL in Dibs.DibScanLine!!' );

      if Dib.biHeight > 0 // Dib is bottom-up?
        then Row := Dib.biHeight - 1 - Row;
      Result := pchar(DibPixels) + Row * DibStorageWidth( Dib.biWidth, Dib.biBitCount );
    end;

  // Dib I/O

  procedure BitmapCore2BitmapInfo( var bi : TBitmapInfoHeader );
    var
      bc : TBitmapCoreHeader;
    begin
      move( bi, bc, sizeof( bc ) );
      with bi do
        begin
          biSize          := sizeof( TBitmapInfoHeader );
          biWidth         := bc.bcWidth;
          biHeight        := bc.bcHeight;
          biPlanes        := bc.bcPlanes;
          biBitCount      := bc.bcBitCount;
          biCompression   := BI_RGB;
          biSizeImage     := 0;
          biXPelsPerMeter := 0;
          biYPelsPerMeter := 0;
          biClrUsed       := 0;
          biClrImportant  := 0;
        end;
    end;

  procedure ColorTriple2ColorQuad( NumColors : integer; rgb : PRgbPalette );
    type
      TPalTriple = array[0..0] of TRGBTriple;
      PPalTriple = ^TPalTriple;
    var
      TmpRGB : TRGBQuad;
      i      : integer;
      rgbt   : PPalTriple absolute rgb;
    begin
      for i := NumColors - 1 downto 0 do
        with TmpRGB do
          begin
            rgbRed      := rgbt[i].rgbtRed;
            rgbBlue     := rgbt[i].rgbtBlue;
            rgbGreen    := rgbt[i].rgbtGreen;
            rgbReserved := 0;

            Rgb[i] := TmpRGB;
          end;
    end;

  // Dib Sections

  function DibSectionCreate( Width, Height : integer; BitCount : integer; UsePalette : PRgbPalette;
                             var DibHeader : PDib; var DibPixels : pointer ) : HBITMAP;
    var
      NumColors : integer;
    begin
      DibHeader := DibNewHeader( Width, Height, BitCount );

      NumColors := PaletteSize( BitCount );
      if (NumColors > 0) and (UsePalette <> dsUseNoPalette)
        then
          if UsePalette <> dsUseDefaultPalette
            then Move( UsePalette^, DibColors( DibHeader )^, NumColors * sizeof( TRGBQuad ) )
            else GetDefaultPalette( NumColors, DibColors( DibHeader )^ );

      Result := CreateDibSection( 0, PBitmapInfo( DibHeader )^, DIB_RGB_COLORS, DibPixels, 0, 0 );
      if Result = 0
        then DibFree( DibHeader );
    end;

  procedure DibSectionFree( DibHeader : PDib; Section : HBITMAP );
    begin
      FreeMem( DibHeader );
      DeleteObject( Section );
    end;

  procedure DibRotate180( SourceHeader : PDib; DibPixels, DestPixels : pointer );
    var
      x, y        : integer;
      SrcScanLine : pchar;
      DstScanLine : pchar;
      WidthBytes  : integer;
    begin
      if DibPixels = nil
        then DibPixels := DibPtr( SourceHeader );

      with SourceHeader^ do
        begin
          WidthBytes  := DibStorageWidth( biWidth, biBitCount );         // Cache this value
          SrcScanLine := DibPixels;
          DstScanLine := pchar( DestPixels ) + WidthBytes * ( abs( biHeight) - 1 );

          for y := 0 to abs( biHeight ) - 1 do
            begin
              case biBitCount of
                1 :
                  for x := 0 to biWidth - 1 do
                    SetPixelMono( DstScanLine, x, GetPixelMono( SrcScanLine, biWidth - 1 - x ) );
                4 :
                  for x := 0 to biWidth - 1 do
                    SetPixel4( DstScanLine, x, GetPixel4( SrcScanLine, biWidth - 1 - x ) );
                8 :
                  for x := 0 to biWidth - 1 do
                    pbyte( DstScanLine + x )^ := pbyte( SrcScanLine + biWidth - 1 - x )^;
                16 :
                  for x := 0 to biWidth - 1 do
                    pword( DstScanLine + 2 * x )^ := pword( SrcScanLine + 2 * ( biWidth - 1 - x ) )^;
                24 :
                  for x := 0 to biWidth - 1 do
                    begin
                      pword( DstScanLine + 3 * x )^     := pword( SrcScanLine + 3 * ( biWidth - 1 - x ) )^;
                      pbyte( DstScanLine + 3 * x + 2 )^ := pbyte( SrcScanLine + 3 * ( biWidth - 1 - x ) + 2 )^;
                    end;
                32 :
                  for x := 0 to biWidth - 1 do
                    pdword( DstScanLine + 4 * x )^ := pdword( SrcScanLine + 4 * ( biWidth - 1 - x ) )^;
              end;
              inc( SrcScanLine, WidthBytes );
              dec( DstScanLine, WidthBytes );
            end;
        end;
    end;

  procedure DibRotate90( SourceHeader : PDib; DibPixels, DestPixels : pointer );
    var
      x, y        : integer;
      DstScanLine : pchar;
      DstWidth    : integer;
      SrcScanLine : pchar;
      SrcWidth    : integer;
      SrcHeight   : integer;
    begin
      if DibPixels = nil
        then DibPixels := DibPtr( SourceHeader );

      with SourceHeader^ do
        begin
          SrcHeight   := abs( biHeight );
          SrcWidth    := DibStorageWidth( biWidth, biBitCount );
          DstWidth    := DibStorageWidth( SrcHeight, biBitCount );
          SrcScanLine := DibScanLine( SourceHeader, DibPixels, 0 );
          if biHeight < 0
            then DstScanLine := pchar( DestPixels ) + ( SrcHeight - 1 ) * biBitCount div 8
            else
              begin
                DstScanLine := pchar( DestPixels ) + ( biWidth - 1 ) * DstWidth + ( SrcHeight - 1 ) * biBitCount div 8;
                DstWidth    := -DstWidth;
                SrcWidth    := -SrcWidth;
              end;

          for y := 0 to SrcHeight - 1 do
            begin
              case biBitCount of
                1 :
                  for x := 0 to biWidth - 1 do
                    SetPixelMono( DstScanLine + x * DstWidth, 7 - (y mod 8), GetPixelMono( SrcScanLine, x ) );
                4 :
                  for x := 0 to biWidth - 1 do
                    SetPixel4( DstScanLine + x * DstWidth, 1 - (y mod 2), GetPixel4( SrcScanLine, x ) );
                8 :
                  for x := 0 to biWidth - 1 do
                    pbyte( DstScanLine + x * DstWidth )^ := pbyte( SrcScanLine + x )^;
                16 :
                  for x := 0 to biWidth - 1 do
                    pword( DstScanLine + x * DstWidth )^ := pword( SrcScanLine + 2 * x )^;
                24 :
                  for x := 0 to biWidth - 1 do
                    begin
                      pword( DstScanLine + x * DstWidth )^     := pword( SrcScanLine + 3 * x )^;
                      pbyte( DstScanLine + x * DstWidth + 2 )^ := pbyte( SrcScanLine + 3 * x + 2 )^;
                    end;
                32 :
                  for x := 0 to biWidth - 1 do
                    pdword( DstScanLine + x * DstWidth )^ := pdword( SrcScanLine + 4 * x )^;
              end;

              inc( SrcScanLine, SrcWidth );
              dec( DstScanLine, biBitCount div 8 );
            end;
        end;
    end;

  procedure DibRotate270( SourceHeader : PDib; DibPixels, DestPixels : pointer );
    var
      x, y        : integer;
      DstScanLine : pchar;
      DstWidth    : integer;
      SrcScanLine : pchar;
      SrcWidth    : integer;
      SrcHeight   : integer;
    begin
      if DibPixels = nil
        then DibPixels := DibPtr( SourceHeader );

      with SourceHeader^ do
        begin
          SrcHeight   := abs( biHeight );
          SrcWidth    := DibStorageWidth( biWidth, biBitCount );
          DstWidth    := DibStorageWidth( SrcHeight, biBitCount );
          SrcScanLine := DibScanLine( SourceHeader, DibPixels, 0 );

          if biHeight < 0
            then DstScanLine := DestPixels
            else
              begin
                DstScanLine := pchar(DestPixels) + ( biWidth - 1 ) * DstWidth;
                SrcWidth    := -SrcWidth;
                DstWidth    := -DstWidth;
              end;

          for y := 0 to SrcHeight - 1 do
            begin
              case biBitCount of
                1 :
                  for x := 0 to biWidth - 1 do
                    SetPixelMono( DstScanLine + ( biWidth - 1 - x ) * DstWidth, y mod 8, GetPixelMono( SrcScanLine, x ) );
                4 :
                  for x := 0 to biWidth - 1 do
                    SetPixel4( DstScanLine + ( biWidth - 1 - x ) * DstWidth, y mod 2, GetPixel4( SrcScanLine, x ) );
                8 :
                  for x := 0 to biWidth - 1 do
                    pbyte( DstScanLine + ( biWidth - 1 - x ) * DstWidth )^ := pbyte( SrcScanLine + x )^;
                16 :
                  for x := 0 to biWidth - 1 do
                    pword( DstScanLine + ( biWidth - 1 - x ) * DstWidth )^ := pword( SrcScanLine + 2 * x )^;
                24 :
                  for x := 0 to biWidth - 1 do
                    begin
                      pword( DstScanLine + ( biWidth - 1 - x ) * DstWidth )^     := pword( SrcScanLine + 3 * x )^;
                      pbyte( DstScanLine + ( biWidth - 1 - x ) * DstWidth + 2 )^ := pbyte( SrcScanLine + 3 * x + 2 )^;
                    end;
                32 :
                  for x := 0 to biWidth - 1 do
                    pdword( DstScanLine + ( biWidth - 1 - x ) * DstWidth )^ := pdword( SrcScanLine + 4 * x )^;
              end;

              inc( SrcScanLine, SrcWidth );
              inc( DstScanLine, biBitCount div 8 );
            end;
        end;
    end;

  procedure DibCopyRotated( SourceHeader : PDib; DibPixels, DestPixels : pointer; Angle : integer );
    begin
      case Angle of
        90 :
          DibRotate90( SourceHeader, DibPixels, DestPixels );
        180 :
          DibRotate180( SourceHeader, DibPixels, DestPixels );
        270 :
          DibRotate270( SourceHeader, DibPixels, DestPixels );
      end;
    end;

  procedure DibFlipVertical( DibHeader : PDib; DibPixels : pointer; Width, Height : integer );
    begin
      if DibPixels = nil
        then DibPixels := DibPtr( DibHeader );
      with DibHeader^ do
        FlipVertical( DibPixels, Width, Height, DibStorageWidth( biWidth, biBitCount ) );
    end;

  procedure DibFlipHorizontal( DibHeader : PDib; DibPixels : pointer; Width, Height : integer );
    var
      x, y         : integer;
      ScanLine      : pchar;
      WidthBytes   : integer;
      Addr1, Addr2 : pchar;
      m1, m2       : boolean;
      b1, b2       : byte;
      w1, w2       : word;
      d1, d2       : integer;
    begin
      if DibPixels = nil
        then DibPixels := DibPtr( DibHeader );

      with DibHeader^ do
        begin
          WidthBytes := DibStorageWidth( biWidth, biBitCount );         // Cache this value
          ScanLine    := DibPixels;
          for y := 0 to Height - 1 do
            begin
              case biBitCount of
                1 :
                  for x := 0 to ( Width - 1 ) div 2 do
                    begin
                      m1 := GetPixelMono( ScanLine, x );
                      m2 := GetPixelMono( ScanLine, Width - 1 - x );
                      SetPixelMono( ScanLine, Width - 1 - x, m1 );
                      SetPixelMono( ScanLine, x, m2 );
                    end;
                4 :
                  for x := 0 to ( Width - 1 ) div 2 do
                    begin
                      b1 := GetPixel4( ScanLine, x );
                      b2 := GetPixel4( ScanLine, Width - 1 - x );
                      SetPixel4( ScanLine, Width - 1 - x, b1 );
                      SetPixel4( ScanLine, x, b2 );
                    end;
                8 :
                  for x := 0 to ( Width - 1 ) div 2 do
                    begin
                      Addr1 := pchar(ScanLine) + x;
                      Addr2 := pchar(ScanLine) + Width - 1 - x;
                      b1 := pbyte( Addr1 )^;
                      b2 := pbyte( Addr2 )^;
                      pbyte( Addr2 )^ := b1;
                      pbyte( Addr1 )^ := b2;
                    end;
                16 :
                  for x := 0 to ( Width - 1 ) div 2 do
                    begin
                      Addr1 := pchar(ScanLine) + 2 * x;
                      Addr2 := pchar(ScanLine) + 2 * ( Width - 1 - x );
                      w1 := pword( Addr1 )^;
                      w2 := pword( Addr2 )^;
                      pword( Addr2 )^ := w1;
                      pword( Addr1 )^ := w2;
                    end;
                24 :
                  for x := 0 to ( Width - 1 ) div 2 do
                    begin
                      Addr1 := pchar(ScanLine) + 3 * x;
                      Addr2 := pchar(ScanLine) + 3 * ( Width - 1 - x );
                      d1 := pdword( Addr1 )^;
                      d2 := pdword( Addr2 )^;
                      pword( Addr2 )^ := d1;
                      pbyte( Addr2 + 2 )^ := d1 shr 16;
                      pword( Addr1 )^ := d2;
                      pbyte( Addr1 + 2 )^ := d2 shr 16;
                    end;
                32 :
                  for x := 0 to ( Width - 1 ) div 2 do
                    begin
                      Addr1 := pchar(ScanLine) + 3 * x;
                      Addr2 := pchar(ScanLine) + 3 * ( Width - 1 - x );
                      d1 := pdword( Addr1 )^;
                      d2 := pdword( Addr2 )^;
                      pdword( Addr2 )^ := d1;
                      pdword( Addr1 )^ := d2;
                    end;
              end;
              inc( ScanLine, WidthBytes );
            end;
        end;
    end;

  procedure DibCopyFlippedHor( SourceHeader : PDib; DibPixels : pointer; DestPixels : pointer );
    var
      x, y         : integer;
      SrcScanLine   : pchar;
      DstScanLine   : pchar;
      WidthBytes   : integer;
    begin
      if DibPixels = nil
        then DibPixels := DibPtr( SourceHeader );
      with SourceHeader^ do
        begin
          WidthBytes  := DibStorageWidth( biWidth, biBitCount );         // Cache this value
          SrcScanLine := DibPixels;
          DstScanLine := DestPixels;

          for y := 0 to abs( biHeight ) - 1 do
            begin
              case biBitCount of
                1 :
                  for x := 0 to biWidth - 1 do
                    SetPixelMono( DstScanLine, biWidth - 1 - x, GetPixelMono( SrcScanLine, x ) );
                4 :
                  for x := 0 to biWidth - 1 do
                    SetPixel4( DstScanLine, biWidth - 1 - x, GetPixel4( SrcScanLine, x ) );
                8 :
                  for x := 0 to biWidth - 1 do
                    pbyte( DstScanLine + biWidth- 1 - x )^ := pbyte( SrcScanLine + x )^;
                16 :
                  for x := 0 to biWidth - 1 do
                    pword( DstScanLine + 2 * ( biWidth - 1 - x ) )^ := pword( SrcScanLine + 2 * x )^;
                24 :
                  for x := 0 to biWidth - 1 do
                    begin
                      pword( DstScanLine + 3 * ( biWidth - 1 - x ) )^ := pword( SrcScanLine + 3 * x )^;
                      pbyte( DstScanLine + 3 * ( biWidth - 1 - x ) + 2 )^ := pbyte( SrcScanLine + 3 * x + 2 )^;
                    end;
                32 :
                  for x := 0 to biWidth - 1 do
                    pdword( DstScanLine + 4 * ( biWidth - 1 - x ) )^ := pdword( SrcScanLine + 4 * x )^;
              end;

              inc( SrcScanLine, WidthBytes );
              inc( DstScanLine, WidthBytes );
            end;
        end;
    end;

  procedure DibCopyFlippedVert( SourceHeader : PDib; DibPixels : pointer; DestPixels : pointer );
    var
      WidthBytes : integer;
    begin
      assert( DibPixels <> DestPixels, 'Copying to Self in Dibs.DibCopyInverted!!' );

      with SourceHeader^ do
        begin
          WidthBytes := DibStorageWidth( biWidth, biBitCount );
          BltCopyOpaque( DibPixels, pchar( DestPixels ) + WidthBytes * ( abs(biHeight) - 1 ), WidthBytes, abs(biHeight), WidthBytes, -WidthBytes );
        end;
    end;

  // Dib I/O ----------------------------

  procedure DibWriteFileHeader( DibHeader : PDib; const Stream : TStream );
    var
      FileHeader : TBitmapFileHeader;
    begin
      with FileHeader, DibHeader^ do
        begin
          bfType      := bfBitmap;
          bfSize      := sizeof( FileHeader ) + DibSize( DibHeader );
          bfReserved1 := 0;
          bfReserved2 := 0;
          bfOffBits   := sizeof( FileHeader ) + DibHeader.biSize + DibPaletteSize( DibHeader );
        end;
      Stream.WriteBuffer( FileHeader, sizeof( FileHeader ) );
    end;

  procedure DibWriteHeader( const Stream : TStream; DibHeader : PDib );
    begin
      Stream.WriteBuffer( DibHeader^, DibHeader.biSize + DibPaletteSize( DibHeader ) );
    end;

  procedure DibWritePixels( const Stream : TStream; DibHeader : PDib; DibPixels : pointer );
    begin
      if DibPixels = nil
        then DibPixels := DibPtr( DibHeader );
      Stream.WriteBuffer( DibPixels^, DibSizeImage( DibHeader ) );
    end;

  function DibReadHeader( const Stream : TStream ) : PDib;
    var
      ofs       : integer;
      Size      : integer;
      NumColors : integer;
      Rgb       : PRgbPalette;
      bi        : TBitmapInfoHeader;
      bf        : TBitmapFileHeader;
    begin
      Result := nil;
      ofs    := Stream.Position;
      if Stream.Read( bf, sizeof( bf ) ) = sizeof( bf )
        then
          begin
            if bf.bfType <> bfBitmap // do we have an RC HEADER?
              then
                begin
                  bf.bfOffBits := 0;
                  Stream.Seek( ofs, soFromBeginning );
                end;
            if Stream.Read( bi, sizeof( bi ) ) = sizeof( bi )
              then
                begin
                  Size := bi.biSize;
                  if Size = sizeof( TBitmapCoreHeader )
                    then
                      begin
                        BitmapCore2BitmapInfo( bi );
                        Stream.Seek( sizeof( TBitmapCoreHeader ) - sizeof( TBitmapInfoHeader ), soFromCurrent );
                      end;
                  NumColors := DibNumColors( @bi );
                  DibFixHeader( @bi );
                  GetMem( Result, bi.biSize + NumColors * sizeof( TRGBQuad ) );
                  if Assigned( Result )
                    then
                      begin
                        Result^ := bi;
                        Rgb := DibColors( Result );
                        if NumColors <> 0
                          then
                            begin
                              if Size = sizeof( TBitmapCoreHeader )
                                then
                                  begin
                                    // convert a old color table ( 3 byte entries )
                                    // to a new color table ( 4 byte entries )
                                    Stream.ReadBuffer( Rgb^, NumColors * sizeof( TRGBTriple ) );
                                    ColorTriple2ColorQuad( NumColors, Rgb );
                                  end
                                else
                                  Stream.ReadBuffer( Rgb^, NumColors * sizeof( TRGBQuad ) );
                            end;
                        if ( bf.bfOffBits <> 0 ) and ( ( ofs + bf.bfOffBits ) <> Stream.Position )
                          then Stream.Seek( ofs + bf.bfOffBits, soFromBeginning );
                      end;
                end;
          end;
    end;

  type
    PHookData = ^THookData;
    THookData =
      record
        CompressionId : integer;
        Hook          : TDibIOHook;
      end;

  var
    IoHooks : TList; // This list will store THookData elements

  function SearchHookById( anId : integer ) : TDibIOHook; // Do a binary search...
    var
      i, j, mid : integer;
      Delta     : integer;
    begin
      with IoHooks do
        if Count = 0
          then Result := nil
          else
            begin
              i   := 0;
              j   := Count - 1;
              repeat
                mid := (j - i + 1) div 2;
                Delta := PHookData( List[mid] ).CompressionId - anId;
                if Delta < 0 // Mid < anId, anId in [mid + 1, j]
                  then i := mid + 1
                  else
                    if Delta > 0 // Mid > anId, anId in [i, mid - 1]
                      then j := mid - 1;
              until (Delta = 0) or (i > j);
              if Delta = 0
                then Result := PHookData( List[mid] ).Hook
                else Result := nil;
            end;
    end;

  function CompareIds( Item1, Item2 : pointer ) : integer;
    begin
      Result := PHookData( Item1 ).CompressionId - PHookData( Item2 ).CompressionId;
    end;

  procedure RegisterDibIOHooks( CompressionIDs : array of integer; NewHook : TDibIOHook );
    var
      i  : integer;
      d  : PHookData;
      Id : integer;
    begin
      if IoHooks = nil
        then IoHooks := TList.Create;
      with IoHooks do
        begin
          for i := low( CompressionIDs ) to high( CompressionIDs ) do
            begin
              Id := CompressionIDs[i];
              if ( Id <> 0 ) and not Assigned( SearchHookById( Id ) )
                then
                  begin
                    new( d );
                    d.CompressionId := Id;
                    d.Hook          := NewHook;
                    Add( d );
                  end;
            end;
          Sort( CompareIds );
        end;
    end;

  procedure UnregisterDibIOHooks( CompressionIDs : array of integer );
    var
      i, j : integer;
    begin
      for i := low( CompressionIDs ) to high( CompressionIDs ) do
        with IoHooks do
          begin
            j := Count - 1;
            repeat
              if PHookData( List[j] ).CompressionId = CompressionIDs[i]
                then
                  begin
                    Delete( j );
                    FreeMem( List[j] );
                    j := 0;
                  end
                else dec( j );
            until j = 0;
          end;
      if IoHooks.Count = 0
        then FreeObject( IoHooks );
    end;

  procedure DibReadPixels( const Stream : TStream; DibHeader : PDib; DibPixels : pointer; ForceTopDown : boolean );
    var
      i, ImgSize : integer;
      OldHeight  : integer;
      WidthBytes : integer;
      TmpBuff    : pointer;
      DibIOHook  : TDibIOHook;
    begin
      with DibHeader^ do
        begin
          if DibPixels = nil
            then DibPixels := DibPtr( DibHeader );
          ImgSize := DibSizeImage( DibHeader );

          try
            if ( biCompression = BI_RGB ) or
               ( biCompression = BI_BITFIELDS )
              then
                begin
                  OldHeight := biHeight;
                  if ( biHeight > 0 ) and ForceTopDown
                    then biHeight := -abs( biHeight );
                  if OldHeight * biHeight > 0
                    then Stream.ReadBuffer( DibPixels^, ImgSize )
                    else
                      begin
                        WidthBytes := DibStorageWidth( biWidth, biBitCount );         // Cache this value
                        DibPixels  := pchar( DibPixels ) + WidthBytes * ( abs( biHeight ) - 1 );
                        for i := 0 to abs( biHeight ) - 1 do
                          Stream.ReadBuffer( ( pchar(DibPixels) - i * WidthBytes )^, WidthBytes );
                      end;
                end
              else
                begin
                  if Assigned( IoHooks )
                    then DibIoHook := SearchHookById( biCompression )
                    else DibIoHook := nil;
                  if Assigned( DibIoHook )
                    then
                      begin
                        GetMem( TmpBuff, biSizeImage );
                        try
                          Stream.ReadBuffer( TmpBuff^, biSizeImage );
                          DibIOHook( TmpBuff, DibHeader, DibPixels, ForceTopDown );
                        finally
                          FreeMem( TmpBuff );
                        end;
                      end
                    else fillchar( DibPixels^, ImgSize, 0 );
                end;
          except // Ignore any error reading stream
          end
        end;
    end;

  function DibLoadFromStream( const Stream : TStream; ForceTopDown : boolean ) : PDib;
    begin
      Result := DibReadHeader( Stream );
      if Assigned( Result )
        then
          with Result^ do
            begin
              ReAllocMem( Result, DibSize( Result ) );
              DibReadPixels( Stream, Result, DibPtr( Result ), ForceTopDown );
            end;
      end;

  function DibSectionLoadFromStream( const Stream : TStream; var DibHeader : PDib; var DibPixels : pointer;
                                     ForceTopDown : boolean ) : HBITMAP;
    var
      OldHeight      : integer;
      bakCompression : dword;
    begin
      Result := 0;
      DibHeader := DibReadHeader( Stream );
      if Assigned( DibHeader )
        then
          with DibHeader^ do
            begin
              OldHeight := biHeight;
              if ForceTopDown
                then biHeight := -abs( biHeight );
              bakCompression := biCompression;
              if ( biCompression <> BI_RGB ) and ( biCompression <> BI_BITFIELDS ) // These are the only formats DIBSECTIONS accept
                then biCompression := BI_RGB;

              Result := CreateDibSection( 0, PBitmapInfo( DibHeader )^, DIB_RGB_COLORS, DibPixels, 0, 0 );

              biCompression := bakCompression;
              biHeight := OldHeight;

              if Result <> 0
                then DibReadPixels( Stream, DibHeader, DibPixels, ForceTopDown );
            end;
    end;

  function DibLoadFromFile( const Filename : string; ForceTopDown : boolean ) : PDib;
    var
      s : TStream;
    begin
      s := TFileStream.Create( Filename, fmOpenRead or fmShareDenyWrite );
      try
        Result := DibLoadFromStream( s, ForceTopDown );
      finally
        S.Free;
      end;
    end;

  function DibSectionLoadFromFile( const Filename : string; var DibHeader : PDib; var DibPixels : pointer;
                                   ForceTopDown : boolean ) : HBITMAP;
    var
      s : TStream;
    begin
      s := TFileStream.Create( Filename, fmOpenRead or fmShareDenyWrite );
      try
        Result := DibSectionLoadFromStream( s, DibHeader, DibPixels, ForceTopDown );
      finally
        S.Free;
      end;
    end;

  procedure DibSaveToStream( const Stream : TStream; DibHeader : PDib; DibPixels : pointer );
    begin
      DibWriteHeader( Stream, DibHeader );
      DibWritePixels( Stream, DibHeader, DibPixels );
    end;

  procedure DibSaveToFile( const Filename : string; DibHeader : PDib; DibPixels : pointer );
    var
      Stream : TStream;
    begin
      Stream := TFileStream.Create( Filename, fmCreate );
      try
        DibWriteFileHeader( DibHeader, Stream );
        DibSaveToStream( Stream, DibHeader, DibPixels );
      finally
        Stream.Free;
      end;
    end;

  // Halftoning -------------------------------------------------------------------------

  procedure DibGetRgbFree( Data : pointer );
    begin
      freemem( Data );
    end;

  procedure DibGetRgbBegin( Source : PDib; var Data : pointer );
    var
      UseMask    : boolean;
      GetRgbData : PGetRgbData absolute Data;
    begin
      new( GetRgbData );
      with Source^, GetRgbData^ do
        begin
          BitCount := biBitCount;
          Colors   := DibColors( Source );
          if biCompression = BI_BITFIELDS
            then
              begin
                UseMask := true;
                rMask   := integer( Colors[0] );
                gMask   := integer( Colors[1] );
                bMask   := integer( Colors[2] );
              end
            else
              case BitCount of
                16 :
                  begin
                    UseMask := true;
                    rMask   := $007C00;
                    gMask   := $0003E0;
                    bMask   := $00001F;
                  end;
                32 :
                  begin
                    UseMask := true;
                    rMask   := $FF0000;
                    gMask   := $00FF00;
                    bMask   := $0000FF;
                  end;
                else
                  UseMask := false;
              end;

          if UseMask
            then
              begin
                rRight := RightShiftCount( rMask );
                gRight := RightShiftCount( gMask );
                bRight := RightShiftCount( bMask );

                if BitCount = 16
                  then
                    begin
                      rLeft := LeftShiftCount( rMask );
                      gLeft := LeftShiftCount( gMask );
                      bLeft := LeftShiftCount( bMask );
                    end;
              end;
        end;
    end;

  procedure DibSetRgb( SourceScanLine : pointer; x : integer; Data : pointer; Rgb : TRgbQuad );
    var
      SetRgbData  : PGetRgbData absolute Data;
      ScanLine    : pchar       absolute SourceScanLine;
    begin
      with SetRgbData^ do
        case BitCount of
          16 :
            with Rgb do
              pword( ScanLine + (x * 2) )^ := rgbRed   shr rLeft shl rRight or
                                              rgbGreen shr gLeft shl gRight or
                                              rgbBlue  shr bLeft shl bRight;
          24 :
            begin
              pword( ScanLine + (x * 3) )^ := pword( @Rgb )^;
              pbyte( ScanLine + (x * 3 + 2) )^ := Rgb.rgbRed;
            end;
          32 :
            with Rgb do
              pdword( ScanLine + (x * 4) )^ := rgbRed   shl rRight or
                                               rgbGreen shl gRight or
                                               rgbBlue  shl bRight;
          else // Unrecognized bitmap format
            raise EUnsupportedBitmapFormat.Create( '' );
        end;
    end;

  function DibGetRgb( SourceScanLine : pointer; x : integer; Data : pointer ) : TRgbQuad;
    var
      wPixelColor : word;
      dPixelColor : dword;
      GetRgbData  : PGetRgbData absolute Data;
      ScanLine    : pchar       absolute SourceScanLine;
    begin
      with GetRgbData^ do
        case BitCount of
          1 :
            Result := Colors[ byte( GetPixelMono( ScanLine, x ) ) ];
          4 :
            Result := Colors[ GetPixel4( ScanLine, x ) ];
          8 :
            Result := Colors[ byte( ScanLine[x] ) ];
          24 :
            Result := PRgbQuad( ScanLine + (x * 3) )^;
          16 :
            with Result do
              begin
                wPixelColor := pword( ScanLine + (x * 2) )^;
                rgbRed      := ( (wPixelColor and rMask) shr rRight ) shl rLeft;
                rgbGreen    := ( (wPixelColor and gMask) shr gRight ) shl gLeft;
                rgbBlue     := ( (wPixelColor and bMask) shr bRight ) shl bLeft;
              end;
          32 :
            with Result do
              begin
                dPixelColor := pdword( ScanLine + (x * 4) )^;
                rgbRed      := (dPixelColor and rMask) shr rRight;
                rgbGreen    := (dPixelColor and gMask) shr gRight;
                rgbBlue     := (dPixelColor and bMask) shr bRight;
              end;
          else // Unrecognized bitmap format
            raise EUnsupportedBitmapFormat.Create( '' );
        end;
    end;

  procedure DibCopyHalftoned( Source : PDib; DibPixels : pointer; Dest : PDib; DestPixels : pointer );
    var
      PixelRgb       : TRgbQuad;
      DstWidth       : integer;
      x, y           : integer;
      SourceScanline : pchar;
      DestScanline   : pchar;
      SrcWidth       : integer;
      RgbData        : pointer;
    begin
      DstWidth := Source.biWidth;
      SrcWidth := DibStorageWidth( Source.biWidth, Source.biBitCount ); // Cache this value

      move( RgbHalftonePalette^, DibColors( Dest )^, sizeof( TRgbPalette ) );    // Initialize color table

      SourceScanLine := DibPixels;
      DestScanLine   := DestPixels;

      DibGetRgbBegin( Source, RgbData );
      for y := 0 to abs( Source.biHeight ) - 1 do
        begin
          for x := 0 to DstWidth - 1 do
            begin
              PixelRgb := DibGetRgb( SourceScanLine, x, RgbData );

              // This is the meat of the halftoning algorithm:
              // Convert an RGB into an index into the halftone palette
              byte( DestScanline[x] ) := HalftonePaletteIndx( x, y, PixelRgb );
            end;

          inc( SourceScanline, SrcWidth );
          inc( DestScanline, DstWidth );
        end;
      DibGetRgbFree( RgbData );
    end;

  function DibHalftoned( Source : PDib ) : PDib;
    begin
      Result := DibCreate( Source.biWidth, Source.biHeight, 8 );
      if Assigned( Result )
        then DibCopyHalftoned( Source, DibPtr( Source ), Result, DibPtr( Result ) );
    end;

  function DibSectionHalftoned( Source : PDib; DibPixels : pointer; var DestHeader : PDib; var DestPixels : pointer ) : HBITMAP;
    begin
      Result := DibSectionCreate( Source.biWidth, Source.biHeight, 8, dsUseNoPalette, DestHeader, DestPixels );
      if Result <> 0
        then DibCopyHalftoned( Source, DibPixels, DestHeader, DestPixels );
    end;

  // Dib conversions

  procedure GetDibFromSection( DibHeader : PDib; DibPixels : pointer; var Dib );
    begin
      move( DibHeader^, Dib, DibHeader.biSize + DibPaletteSize( DibHeader ) );
      move( DibPixels^, DibPtr( @Dib )^, DibSizeImage( DibHeader ) );
    end;

  function  DibFromSection( DibHeader : PDib; DibPixels : pointer ) : PDib;
    begin
      getmem( Result, DibSize( DibHeader ) );
      GetDibFromSection( DibHeader, DibPixels, Result^ );
    end;

  function DibFromBitmap( Handle : HBITMAP; Palette : HPALETTE ) : PDib;
    begin
      Result := nil;
    end;
(*
/*
 *  DibFromBitmap()
 *
 *  Will create a global memory block in DIB format that represents the DDB
 *  passed in
 *
 */
HANDLE DibFromBitmap(HBITMAP hbm, DWORD biStyle, WORD biBits, HPALETTE hpal, WORD wUsage)
{
    BITMAP               bm;
    BITMAPINFOHEADER     bi;
    BITMAPINFOHEADER FAR *lpbi;
    DWORD                dwLen;
    int                  nColors;
    HANDLE               hdib;
    HANDLE               h;
    HDC                  hdc;

    if (wUsage == 0)
        wUsage = DIB_RGB_COLORS;

    if (!hbm)
        return NULL;

    if (biStyle == BI_RGB && wUsage == DIB_RGB_COLORS)
        return CreateLogicalDib(hbm, biBits, hpal);

    if (hpal == NULL)
        hpal = GetStockObject(DEFAULT_PALETTE);

    GetObject(hbm, sizeof(bm), (LPSTR)&bm);
    GetObject(hpal, sizeof(nColors), (LPSTR)&nColors);

    if (biBits == 0)
        biBits = bm.bmPlanes * bm.bmBitsPixel;

    bi.biSize               = sizeof(BITMAPINFOHEADER);
    bi.biWidth              = bm.bmWidth;
    bi.biHeight             = bm.bmHeight;
    bi.biPlanes             = 1;
    bi.biBitCount           = biBits;
    bi.biCompression        = biStyle;
    bi.biSizeImage          = 0;
    bi.biXPelsPerMeter      = 0;
    bi.biYPelsPerMeter      = 0;
    bi.biClrUsed            = 0;
    bi.biClrImportant       = 0;

    dwLen  = bi.biSize + PaletteSize(&bi);

    hdc = CreateCompatibleDC(NULL);
    hpal = SelectPalette(hdc, hpal, FALSE);
    RealizePalette(hdc);  // why is this needed on a MEMORY DC? GDI bug??

    hdib = GAlloc(dwLen);

    if (!hdib) {
        goto exit;
    }

    lpbi = GLock(hdib);

    *lpbi = bi;

    /*
     *  call GetDIBits with a NULL lpBits param, so it will calculate the
     *  biSizeImage field for us
     */
    GetDIBits(hdc, hbm, 0, (WORD)bi.biHeight,
        NULL, (LPBITMAPINFO)lpbi, wUsage);

    bi = *lpbi;
    GUnlock(hdib);

    /*
     * HACK! if the driver did not fill in the biSizeImage field, make one up
     */
    if (bi.biSizeImage == 0)
    {
        bi.biSizeImage = (DWORD)WIDTHBYTES(bm.bmWidth * biBits) * bm.bmHeight;

        if (biStyle != BI_RGB)
            bi.biSizeImage = (bi.biSizeImage * 3) / 2;
    }

    /*
     *  realloc the buffer big enough to hold all the bits
     */
    dwLen = bi.biSize + PaletteSize(&bi) + bi.biSizeImage;
    if (h = GReAlloc(hdib, dwLen))
    {
        hdib = h;
    }
    else
    {
        GFree(hdib);
        hdib = NULL;
        goto exit;
    }

    /*
     *  call GetDIBits with a NON-NULL lpBits param, and actualy get the
     *  bits this time
     */
    lpbi = GLock(hdib);

    GetDIBits(hdc, hbm, 0, (WORD)bi.biHeight,
        (LPSTR)lpbi + (WORD)lpbi->biSize + PaletteSize(lpbi),
        (LPBITMAPINFO)lpbi, wUsage);

    bi = *lpbi;
    GUnlock(hdib);

exit:
    SelectPalette(hdc, hpal, FALSE);
    DeleteDC(hdc);
    return hdib;
}
*)

  function DibInvert( DibHeader : PDib; DibPixels : pointer ) : PDib;
    begin
      with DibHeader^ do
        begin
          Result := DibCreate( biWidth, biHeight, biBitCount );
          if Result <> nil
            then DibCopyFlippedVert( DibHeader, DibPixels, DibPtr( Result ) );
        end;
    end;

  function BitmapFromDib( DibHeader : PDib; DibPixels : pointer ) : HBITMAP;
    var
      dc : HDC;
    begin
      dc := GetDC( 0 );
      if DibPixels = nil
        then DibPixels := DibPtr( DibHeader );
      Result := CreateDIBitmap( dc, DibHeader^, CBM_INIT, DibPixels, PBitmapInfo( DibHeader )^, DIB_RGB_COLORS );
      ReleaseDC( 0, dc );
    end;

  function SectionFromDib( DibHeader : PDib; DibPixels : pointer ) : HBITMAP;
    var
      SectionPixels : pointer;
    begin
      with DibHeader^ do
        begin
          if DibPixels = nil
            then DibPixels := DibPtr( DibHeader );
          Result := CreateDibSection( 0, PBitmapInfo( DibHeader )^, DIB_RGB_COLORS, SectionPixels, 0, 0 );
          if Result <> 0
            then Move( DibPixels^, SectionPixels^, DibSizeImage( DibHeader ) );
        end;
    end;

  function DibSectionFromHandle( Section : HBITMAP; var DibHeader : PDib; var DibPixels : pointer ) : boolean;
    var
      dc         : HDC;
      oldBitmap  : HBITMAP;
      DibSection : TDIBSECTION;
    begin
      Result := false;
      dc     := GetBitmapDC( Section, OldBitmap );
      try
        if GetObject( Section, sizeof( DibSection ), @DibSection ) > 0
          then
            begin
              GetMem( DibHeader, sizeof( TDib ) + DibNumColors( @DibSection.dsBmih ) * sizeof( TRGBQuad ) );
              Move( DibSection.dsBmih, DibHeader^, sizeof( TDib ) );
              DibPixels := DibSection.dsBm.bmBits;
              GetDibColorTable( dc, 0, DibNumColors( DibHeader ), DibColors( DibHeader )^ );
              Result := true;
            end;
      finally
        ReleaseBitmapDC( dc, OldBitmap );
      end;
   end;

  function DibSectionFromDib( SourceHeader : PDib; DibPixels : pointer; var newHeader : PDib; var newPixels : pointer ) : HBITMAP;
    begin
      with SourceHeader^ do
        begin
          if DibPixels = nil
            then DibPixels := DibPtr( SourceHeader );
          Result := DibSectionCreate( biWidth, biHeight, biBitCount, DibColors(SourceHeader), newHeader, newPixels );
          if Result <> 0
            then
              if biHeight * newHeader.biHeight > 0  // Same orientation?
                then Move( DibPixels^, newPixels^, DibSizeImage( NewHeader ) )
                else DibCopyFlippedVert( SourceHeader, DibPixels, newPixels );
        end;
    end;

  function DibSectionFromDC( SectionDC : HDC; var Handle : HBITMAP; var DibHeader : PDib; var DibPixels : pointer ) : boolean;
    var
      DibSection : TDibSection;
    begin
      Result := false;

      Handle := GetBitmapHandle( SectionDC );
      if GetObject( Handle, sizeof( TDibSection ), @DibSection ) > 0
        then
          begin
            GetMem( DibHeader, sizeof( TDib ) + DibNumColors( @DibSection.dsBmih ) * sizeof( TRGBQuad ) );
            Move( DibSection.dsBmih, DibHeader^, sizeof( TDib ) );
            DibPixels := DibSection.dsBm.bmBits;
            GetDibColorTable( SectionDC, 0, DibNumColors( DibHeader ), DibColors( DibHeader )^ );

            Result := true;
          end;
    end;

end.

(*

For DIBs (device independent bitmaps), the 16 and 32-bit formats contain three
DWORD masks in the bmiColors member of the BITMAPINFO structure. These masks
specify which bits in the pel correspond to which color.

The three masks must have contiguous bits, and their order is assumed to be
R, G, B (high bits to low bits). The order of the three masks in the color
table must also be first red, then green, then blue (RGB). In this manner,
the programmer can specify a mask indicating how many shades of each RGB
color will be available for bitmaps created with CreateDIBitmap(). For
16-bits-per-pixel DIBs, CreateDIBitmap() defaults to the RGB555 format.
For 32-bits-per-pixel DIBs, CreateDIBitmap() defaults to an RGB888 format.

NOTE: The DIB engine in Windows 95 supports only RGB555 and RGB565 for 16-bit
DIBs and only RGB888 for 32-bit DIBs.

Example

The RGB555 format masks would look like:

   0x00007C00  red   (0000 0000 0000 0000 0111 1100 0000 0000)
   0x000003E0  green (0000 0000 0000 0000 0000 0011 1110 0000)
   0x0000001F  blue  (0000 0000 0000 0000 0000 0000 0001 1111)

NOTE: For 16 bits-per-pel, the upper half of the DWORDs are always zeroed.

The RGB888 format masks would look like:

   0x00FF0000  red   (0000 0000 1111 1111 0000 0000 0000 0000)
   0x0000FF00  green (0000 0000 0000 0000 1111 1111 0000 0000)
   0x000000FF  blue  (0000 0000 0000 0000 0000 0000 1111 1111)

Usage

When using 16 and 32-bit formats, there are also certain fields of the
BITMAPINFOHEADER structure that must be set to the correct values:

1. The biCompression member must be set to either BI_RGB or BI_BITFIELDS.
   Using BI-RGB indicates that no bit masks are included in the color table
   and that the default (RGB555 for 16bpp and RGB888 for 32bpp) format is
   implied. Using BI_BITFIELDS indicates that there are masks (bit fields)
   specified in the color table.

2. As with 24-bits-per-pixel formats, the biClrUsed member specifies the
   Size of the color table used to optimize performance of Windows color
   palettes. If the biCompression is set to BI_BITFIELDS, then the optimal
   color palette starts immediately following the three DWORD masks. Note
   that an optimal color palette is optional and many applications will
   ignore it.


Windows 95
In Windows 95, if the BI_BITFIELDS flag is set, then a color mask must be
specified and it must be one of the following:

Resolution  Bits per color  Color Mask
16bpp       5,5,5           0x00007c00 0x000003e0 0x0000001f
16bpp       5,6,5           0x0000f800 0x000007e0 0x0000001f
32bpp       8,8,8           0x00ff0000 0x0000ff00 0x000000ff

*)

