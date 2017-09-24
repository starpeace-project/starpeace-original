unit Buffer;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise

// Notes & Tips:
//
// - If you know the images you are using do not touch the system color entries (eg. were designed by
//   you with this in mind), call Buffer.ForcePaletteIndentity( true ) to make sure these reserved
//   entries match the ones in the system. You may also want (if you know it's safe) to turn off
//   IdentityLock check for speed.
//

interface

  uses
    Classes, Consts, Windows, SysUtils, Graphics,
    MemUtils, Rects, Gdi, Dibs, BitBlt, Palettes;

  type
    ESpeedBitmapError = class( Exception );

  type
    TOnGetPalette = function : CBufferPalette of object;

  const  // ParamsChanged mask
    pcmPaletteChanged    = $00000000;
    pcmPaletteNotChanged = $00000001;
    pcmPaletteFromDib    = $00000002;
    pcmPaletteFromSource = $00000004;
    pcmSizeChanged       = $00000008;

    pcmUseDefaultPalette = pcmSizeChanged;
    pcmUseCurrentPalette = pcmSizeChanged or pcmPaletteNotChanged;
    pcmUseDibPalette     = pcmSizeChanged or pcmPaletteFromDib;
    pcmUseSourcePalette  = pcmSizeChanged or pcmPaletteFromSource;

  // TBuffer =========================================================================================

  {$TYPEINFO ON}
  type
    TBuffer =
      class( TPersistent )
        protected
          fOnChange         : TNotifyEvent;
          fOnProgress       : TProgressEvent;

          fBufferPalette    : TBufferPalette;
          fClientRect       : TRect;
          fWidth            : integer;
          fHeight           : integer;
          fBitCount         : integer;
          fOriginalHeight   : integer;

          fDibHeader        : PDib;
          fDibPixels        : pointer;

          fRgbEntries       : PRgbPalette;
          fBytesPerPixel    : integer;
          fStorageWidth     : integer;
          fOnGetPalette     : TOnGetPalette;

          fTransparent      : boolean;
          fTransparentMode  : TTransparentMode;
          fTransparentColor : TColor;
          fTransparentIndx  : integer;
          fIgnorePalette    : boolean;
          fTopDown          : boolean;

          procedure SetBitCount( aBitCount : integer );
          procedure SetWidth( aWidth : integer );
          procedure SetHeight( aHeight : integer );
          procedure SetTopDown( aTopDown : boolean );
          procedure SetEmpty( Value : boolean );

          function  TransparentColorStored : boolean;

        protected
          function  GetPixelAddr( x, y : integer ) : pbyte;
          function  GetScanLine( Row : integer ) : pointer;
          function  GetPaletteEntry( Indx : integer ) : longint;
          procedure SetPaletteEntry( Indx : integer; anRgb : longint );

        protected
          fUsesPalette     : boolean;
          fIdentityLock    : boolean;
          fPaletteModified : boolean;

          procedure SetBufferPalette( Value : TBufferPalette );                                                 virtual;
          procedure SetUsesPalette( Value : boolean );                                                          virtual;

          procedure ParamsChanged( Mask : integer );                                                            virtual;
          procedure ReleaseImage( aReleasePalette : boolean );                                                  virtual;
          procedure PaletteNeeded;

          procedure CreateBitmap( aWidth, aHeight : integer; aBitCount : integer; aRgbEntries : pointer );      virtual; abstract;
          procedure AssignBitmap( Source : TBuffer );                                                           virtual; abstract;
          procedure FreeContext;                                                                                virtual; abstract;

          function GetPixelFormat : TPixelFormat;
          procedure SetPixelFormat( Value : TPixelFormat );

        protected
          function  GetHandleType : TBitmapHandleType;                                                          virtual;

          procedure DefineProperties( Filer : TFiler );                                                         override;
          procedure ReadData( Stream : TStream );                                                               virtual;
          procedure WriteData( Stream : TStream );                                                              virtual;

          function  GetEmpty : boolean;                                                                         virtual;
          procedure SetMonochrome( Value : boolean );                                                           virtual;
          function  GetMonochrome : boolean;                                                                    virtual;

          procedure GetRgbEntries( Indx, Count : integer; RgbQuads : pointer );                                 virtual; abstract;
          procedure SetRgbEntries( Indx, Count : integer; RgbQuads : pointer );                                 virtual; abstract;

          function  GetTransparentColor : TColor;                                                               virtual;
          procedure SetTransparent( Value : boolean );                                                          virtual;
          procedure SetTransparentColor( Value : TColor );                                                      virtual;
          procedure SetTransparentMode( Value : TTransparentMode );                                             virtual;
          function  GetPixels( x, y : integer ) : TColor;                                                       virtual; abstract;
          procedure SetPixels( x, y : integer; Value : TColor );                                                virtual; abstract;

        public
          constructor Create;                                                                                   virtual;
          constructor CreateSized( aWidth, aHeight : integer; aBitCount : integer );                            virtual;
          destructor  Destroy;                                                                                  override;

          function  PaletteClass : CBufferPalette;                                                              virtual;

          procedure SetPaletteEntries( Indx : integer; Colors : array of longint );
          procedure GetPaletteEntries( Indx : integer; Count : integer; var Colors );                           virtual;
          procedure ChangePaletteEntries( Indx, Count : integer; var RgbQuads );                                virtual;

          procedure ForcePaletteIdentity( IdentityWanted : boolean; LockEntries : boolean );                    virtual;
          procedure SyncPalette;                                                                                virtual;
          procedure RecreatePalette;                                                                            virtual;

          procedure FreeImage;                                                                                  virtual;
          procedure Release;                                                                                    virtual;

          procedure Changed( Sender : TObject );                                                                virtual;
          procedure Progress( Sender : TObject; Stage : TProgressStage; PercentDone : byte;
                              RedrawNow : boolean; const R : TRect; const Msg : string );                       dynamic;

        public
          property DibHeader : PDib                    read fDibHeader;
          property RgbEntries : PRgbPalette            read fRgbEntries;
          property ScanLines : pointer                 read fDibPixels;
          property ScanLine[Row : integer] : pointer   read GetScanLine;
          property PixelAddr[x, y : integer] : pbyte   read GetPixelAddr;
          property BytesPerPixel : integer             read fBytesPerPixel;
          property PixelFormat : TPixelFormat          read GetPixelFormat   write SetPixelFormat;
          property StorageWidth : integer              read fStorageWidth;
          property Rgb[ Indx : integer ] : longint     read GetPaletteEntry  write SetPaletteEntry;
          property Pixels[x, y : integer] : TColor     read GetPixels        write SetPixels; default;

          property IdentityLock : boolean              read fIdentityLock    write fIdentityLock;
          property UsesPalette  : boolean              read fUsesPalette     write SetUsesPalette;
          property Empty : boolean                     read GetEmpty         write SetEmpty;

          property ClientRect : TRect                  read fClientRect;
          property OriginalHeight : integer            read fOriginalHeight;
          property BufferPalette : TBufferPalette      read fBufferPalette   write SetBufferPalette;

          property OnGetPalette : TOnGetPalette        read fOnGetPalette    write fOnGetPalette;
          property PaletteModified : boolean           read fPaletteModified write fPaletteModified;

        public
          function  Equals( Value : TBuffer ) : boolean;                                                      virtual;

          procedure NewSize( NewWidth, NewHeight : integer; NewBitCount : integer );                          virtual; abstract;
          procedure LoadFromDib( DibHeader : PDib; DibPixels : pointer );                                     virtual; abstract;
          procedure LoadFromStream( Stream : TStream );                                                       virtual; abstract;

          procedure Halftone;                                                                                 virtual;
          procedure Flip( fFlipHorizontal : boolean );                                                        virtual;

          procedure LoadFromFile( const Filename : string );                                                  virtual;
          procedure LoadFromResourceName( Instance : THandle; const ResourceName : string );
          procedure LoadFromResourceID( Instance : THandle; ResourceId : integer );

          procedure SaveToStream( Stream : TStream );                                                         virtual;
          procedure SaveToFile( const Filename : string );                                                    virtual;

          // Only accepts the CF_DIB clipboard format. Win32 takes cares of translating it when CF_BITMAP is requested
          procedure SaveToClipboardFormat( var Format : word; var Data : THandle; var aPalette : HPALETTE );  virtual;
          procedure LoadFromClipboardFormat( aFormat : word; aData : THandle; aPalette : HPALETTE);           virtual;

          procedure Flush;                                                                                    virtual; abstract;

        published
          property IgnorePalette : boolean            read fIgnorePalette      write fIgnorePalette      default false;
          property TopDown : boolean                  read fTopDown            write SetTopDown          stored false;
          property Height : integer                   read fHeight             write SetHeight           stored false;
          property Width : integer                    read fWidth              write SetWidth            stored false;
          property BitCount : integer                 read fBitCount           write SetBitCount         stored false;
          property Monochrome : boolean               read GetMonochrome       write SetMonochrome       stored false;

          property OnChange : TNotifyEvent            read fOnChange           write fOnChange;
          property OnProgress : TProgressEvent        read fOnProgress         write fOnProgress;

          property TransparentIndx : integer          read fTransparentIndx                              stored false;
          property TransparentColor : TColor          read GetTransparentColor write SetTransparentColor stored TransparentColorStored;
          property TransparentMode : TTransparentMode read fTransparentMode    write SetTransparentMode  default tmAuto;
          property Transparent : boolean              read fTransparent        write SetTransparent;
      end;
  {$TYPEINFO OFF}

  // Helper Functions ======================================================================================

  function BitmapCreateRegion( Source : TBuffer; ColorIndx : integer ): HRGN; // Use with SetWindowRgn

  // Returns an 8-bit halftoned bitmap
  function BitmapHalftone( Source : TBuffer ) : TBuffer;

  procedure BitmapFakeInvert( Source : TBuffer );
  procedure BitmapSnapshotPalette( dc : HDC; Dest : TBuffer ); // Copy the palette from a given DC

  procedure BitmapSetPaletteSize( Dest : TBuffer; aNewSize : integer );

  function BitmapRotate( Source : TBuffer; Angle : integer ) : TBuffer;
  function BitmapFlip( Source : TBuffer; FlipHorizontal : boolean ) : TBuffer;
  function BitmapCopy( Source : TBuffer; Width, Height : integer; BitCount : integer; ForceTopDown : boolean ) : TBuffer;
  function BitmapCopyPiece( Source : TBuffer; const Rect : TRect; Width, Height : integer; BitCount : integer; ForceTopDown : boolean; UseColors : boolean ) : TBuffer;
  function BitmapColor( Source : TBuffer; Color : TColor ) : dword; // Convert a TColor in a ColorIndx

implementation

  uses
    StreamUtils;

  // TBuffer ===========================================================================================

  constructor TBuffer.Create;
    begin
      inherited;

      fIgnorePalette    := ScreenDeviceBitsPerPixel > 8;
      fBitCount         := 8;
      fTransparentColor := clDefault;
    end;

  destructor TBuffer.Destroy;
    begin
      Release;
      inherited;
    end;

  procedure TBuffer.Release;
    begin
      ReleaseImage( true );
    end;

  constructor TBuffer.CreateSized( aWidth, aHeight : integer; aBitCount : integer );
    begin
      Create;
      CreateBitmap( aWidth, aHeight, aBitCount, nil );
    end;

  procedure TBuffer.SaveToClipboardFormat( var Format : word; var Data : THandle; var aPalette : HPALETTE );
    begin
      Format   := CF_DIB;
      Data     := DibClipboardFormat( DibHeader, ScanLines );
      aPalette := 0;
    end;

  procedure TBuffer.LoadFromClipboardFormat( aFormat : word; aData : THandle; aPalette : HPALETTE );
    var
      NewDib : PDib;
    begin
      if ( aFormat <> CF_DIB ) or ( aData = 0 )
        then raise EInvalidGraphic.Create( SUnknownClipboardFormat );
      NewDib := GlobalLock( aData );
      try
        LoadFromDib( NewDib, nil );
      finally
        GlobalUnlock( aData );
      end;
    end;

  procedure TBuffer.Changed( Sender : TObject );
    begin
      if Assigned( fOnChange )
        then fOnChange( Self );
    end;

  procedure TBuffer.Progress( Sender : TObject; Stage : TProgressStage; PercentDone : byte; RedrawNow : Boolean; const R : TRect; const Msg : string );
    begin
      if Assigned( fOnProgress )
        then fOnProgress( Sender, Stage, PercentDone, RedrawNow, R, Msg );
    end;

  function TBuffer.GetPixelFormat : TPixelFormat;
    begin
      Result := pfCustom;
      case BitCount of
        1 :
          Result := pf1Bit;
        4 :
          Result := pf4Bit;
        8 :
          Result := pf8Bit;
        16 :
          case DibHeader.biCompression of
            BI_RGB :
              Result := pf15Bit;
            BI_BITFIELDS:
              case pdword( @RgbEntries[1] )^ of
                $3E0 :
                  Result := pf16Bit;
                $7E0 :
                  Result := pf16Bit;
              end;
          end;
        24 :
          Result := pf24Bit;
        32:
          if DibHeader.biCompression = BI_RGB
            then Result := pf32Bit;
      end;
    end;

  procedure TBuffer.SetPixelFormat( Value : TPixelFormat );
    begin
      // Unfinished!!
    end;

  procedure TBuffer.ReleaseImage( aReleasePalette : boolean );
    begin
      if Assigned( fDibHeader )
        then
          begin
            DibFree( fDibHeader );
            fDibHeader := nil;
          end;
      FreeContext;
      if aReleasePalette
        then UsesPalette := false;
    end;

  procedure TBuffer.FreeImage;
    begin
      // Since we must keep anyway the DIBSECTION info & pixels, do nothing.
    end;

  procedure TBuffer.ReadData( Stream : TStream );
    begin
      LoadFromStream( Stream );
    end;

  procedure TBuffer.WriteData( Stream : TStream );
    begin
      SaveToStream( Stream );
    end;

  function TBuffer.Equals( Value : TBuffer ) : boolean;
    var
      MyImage, ValueImage : TMemoryStream;
    begin
      if Value = nil
        then Result := false
        else
          if Empty or Value.Empty
            then Result := Empty and Value.Empty
            else
              begin
                Result := ClassType = Value.ClassType;
                if Result
                  then
                    begin
                      MyImage := TMemoryStream.Create;
                      try
                        WriteData( MyImage );
                        ValueImage := TMemoryStream.Create;
                        try
                          Value.WriteData( ValueImage );
                          Result := ( MyImage.Size = ValueImage.Size ) and
                            CompareMem( MyImage.Memory, ValueImage.Memory, MyImage.Size );
                        finally
                          ValueImage.Free;
                        end;
                      finally
                        MyImage.Free;
                      end;
                    end;
              end;
    end;

  procedure TBuffer.DefineProperties( Filer : TFiler );
    function DoWrite : boolean;
      begin
        if Filer.Ancestor <> nil
          then Result := not (Filer.Ancestor is TBuffer) or not Equals( TBuffer( Filer.Ancestor ) )
          else Result := not Empty;
      end;
    begin
      Filer.DefineBinaryProperty( 'Data', ReadData, WriteData, DoWrite );
    end;

  procedure TBuffer.ParamsChanged( Mask : integer );
    begin
      if Assigned( fDibHeader )
        then
          begin
            fUsesPalette := DibUsesPalette( fDibHeader );
            fRgbEntries  := DibColors( fDibHeader );

            with fDibHeader^ do
              begin
                if Mask and pcmSizeChanged <> 0
                  then
                    begin
                      fTopDown        := (biHeight < 0);
                      fBytesPerPixel  := biBitCount div 8;
                      fStorageWidth   := DibStorageWidth( biWidth, biBitCount );
                      fWidth          := biWidth;
                      fHeight         := abs(biHeight);
                      fBitCount       := biBitCount;
                      fOriginalHeight := biHeight;

                      with fClientRect do
                        begin
                          Left   := 0;
                          Top    := 0;
                          Right  := fWidth;
                          Bottom := fHeight;
                        end;
                    end;
                if Mask and (pcmPaletteNotChanged or pcmPaletteFromSource) = 0
                  then
                    begin
                      FreeObject( fBufferPalette );

                      if UsesPalette
                        then
                          begin
                            PaletteModified := true;
                            if Mask and pcmPaletteFromDib <> 0
                              then
                                begin
                                  fBufferPalette := PaletteClass.CreateFromRgbPalette( fRgbEntries^, biClrUsed );
                                  SetRgbEntries( 0, biClrUsed, fRgbEntries );
                                end;
                          end;
                    end;
                if not Assigned( fBufferPalette )
                  then fBufferPalette := PaletteClass.CreateFromRgbPalette( fRgbEntries^, biClrUsed );
              end;
          end;
    end;

  procedure TBuffer.RecreatePalette;
    begin
      ParamsChanged( pcmPaletteChanged );
    end;

  procedure TBuffer.ForcePaletteIdentity( IdentityWanted : boolean; LockEntries : boolean );
    begin
      if IdentityWanted and (AvailableColors < 256)
        then
          begin
            fIdentityLock := true;
            ChangePaletteEntries( 0, DibHeader.biClrUsed, fRgbEntries^ );
            fIdentityLock := LockEntries;
          end
        else
          fIdentityLock := false;
    end;

  procedure TBuffer.ChangePaletteEntries( Indx, Count : integer; var RgbQuads );
    var
      Entries : TRgbPalette absolute RgbQuads;
    begin
      if UsesPalette
        then
          begin
            if IdentityLock
              then
                begin
                  if Indx < FirstAvailColor
                    then GetRgbSysPalette( 0, FirstAvailColor, Entries );
                  if Indx + Count > LastAvailColor
                    then GetRgbSysPalette( LastAvailColor + 1, FirstAvailColor, Entries );
                end;

            if @Entries <> fRgbEntries
              then Move( Entries[Indx], fRgbEntries[Indx], Count * sizeof(TRGBQuad) );

            BufferPalette.AssignRgbEntries( Indx, Count, Entries );
            SetRgbEntries( Indx, Count, @Entries );
            PaletteModified := true;
          end;
    end;

  procedure TBuffer.SyncPalette;
    begin
      if UsesPalette
        then
          with fDibHeader^ do
            begin
              GetRgbPalette( BufferPalette.Handle, 0, biClrUsed, fRgbEntries^ );
              SetRgbEntries( 0, biClrUsed, fRgbEntries );
              PaletteModified := true;
            end;
    end;

  procedure TBuffer.SetTransparentMode( Value : TTransparentMode );
    begin
      if Value <> TransparentMode
        then
          if Value = tmAuto
            then SetTransparentColor( clDefault )
            else SetTransparentColor( TransparentColor );
    end;

  procedure TBuffer.PaletteNeeded;
    begin
      if UsesPalette
        then BufferPalette.HandleNeeded;
    end;

  function TBuffer.GetMonochrome : boolean;
    begin
      Result := ( BitCount = 1 );
    end;

  procedure TBuffer.SetMonochrome( Value : boolean );
    begin
      if (Value <> Monochrome)
        then
          if Value
            then BitCount := 1   // Convert color bitmap to monochrome
            else BitCount := 4;  // Convert monochrome bitmap to a 16 color
    end;

  procedure TBuffer.Flip( fFlipHorizontal : boolean );
    begin
      if fFlipHorizontal
        then DibFlipHorizontal( DibHeader, ScanLines, Width, Height )
        else FlipVertical( ScanLines, StorageWidth, Height, StorageWidth );
    end;

  procedure TBuffer.Halftone;
    var
      TmpBuffer : TBuffer;
    begin
      TmpBuffer := TBuffer( ClassType.Create );
      try
        TmpBuffer.CreateBitmap( Width, Height, 8, nil );
        DibCopyHalftoned( DibHeader, ScanLines, TmpBuffer.DibHeader, TmpBuffer.ScanLines );
      finally
        TmpBuffer.Free;
      end;
    end;

  procedure TBuffer.SaveToStream( Stream : TStream );
    begin
      DibSaveToStream( Stream, DibHeader, ScanLines );
    end;

  procedure TBuffer.LoadFromFile( const Filename : string );
    begin
      StreamObject( TFileStream.Create( Filename, fmOpenRead or fmShareDenyWrite ), LoadFromStream );
    end;

  procedure TBuffer.SaveToFile( const Filename : string );
    var
      Stream : TStream;
    begin
      Stream := TFileStream.Create( Filename, fmCreate );
      DibWriteFileHeader( DibHeader, Stream );
      StreamObject( Stream, SaveToStream );
    end;

  procedure TBuffer.LoadFromResourceName( Instance : THandle; const ResourceName : string );
    begin
      StreamObject( TResourceStream.Create( Instance, ResourceName, RT_BITMAP ), LoadFromStream );
    end;

  procedure TBuffer.LoadFromResourceID( Instance : THandle; ResourceId : Integer );
    begin
      StreamObject( TResourceStream.CreateFromID( Instance, ResourceId, RT_BITMAP ), LoadFromStream );
    end;

  procedure TBuffer.SetBitCount( aBitCount : integer );
    begin
      if BitCount <> aBitCount
        then NewSize( Width, OriginalHeight, aBitCount );
    end;

  procedure TBuffer.SetWidth( aWidth : integer );
    begin
      if Width <> aWidth
        then NewSize( aWidth, OriginalHeight, BitCount );
    end;

  procedure TBuffer.SetHeight( aHeight : integer );
    begin
      if OriginalHeight <> aHeight
        then NewSize( Width, aHeight, BitCount );
    end;

  procedure TBuffer.SetTopDown( aTopDown : boolean );
    begin
      if aTopDown <> TopDown
        then
          if aTopDown
            then NewSize( Width, -Height, BitCount )
            else NewSize( Width, Height, BitCount );
    end;

  procedure TBuffer.SetUsesPalette( Value : boolean );
    begin
      if Value <> UsesPalette
        then
          begin
            if UsesPalette
              then FreeObject( fBufferPalette )
              else ParamsChanged( pcmPaletteChanged );
            fUsesPalette := Value;
          end;
    end;

  procedure TBuffer.SetBufferPalette( Value : TBufferPalette );
    begin
      if (Value <> BufferPalette)
        then
          begin
            if Assigned( Value ) and (not Empty)
              then
                with Value do
                  begin
                    if BufferPalette = nil
                      then fBufferPalette := TBufferPalette( ClassType ).Create;

                    BufferPalette.Assign( Value );
                    SyncPalette;
                  end
              else
                begin
                  fUsesPalette := false;
                  FreeObject( fBufferPalette );
                end;
            Changed( Self );
          end;
    end;

  function TBuffer.GetPixelAddr( x, y : integer ) : pbyte;
    begin
      if TopDown
        then Result := pointer( pchar(fDibPixels) + y * fStorageWidth + x * BytesPerPixel )
        else Result := pointer( pchar(fDibPixels) + ( pred(fHeight) - y ) * fStorageWidth + x * BytesPerPixel );
    end;

  function TBuffer.GetScanLine( Row : integer ) : pointer;
    begin
      if TopDown
        then Result := pchar(fDibPixels) + Row * fStorageWidth
        else Result := pchar(fDibPixels) + ( pred(fHeight) - Row ) * fStorageWidth;
    end;

  function TBuffer.GetPaletteEntry( Indx : integer ) : longint;
    begin
      Result := longint( fRgbEntries[Indx] );
    end;

  procedure TBuffer.GetPaletteEntries( Indx : integer; Count : integer; var Colors );
    begin
      Move( fRgbEntries[Indx], Colors, Count * sizeof(TRGBQuad) );
    end;

  procedure TBuffer.SetPaletteEntry( Indx : integer; anRgb : longint );
    begin
      ChangePaletteEntries( Indx, 1, anRgb );
    end;

  procedure TBuffer.SetPaletteEntries( Indx : integer; Colors : array of longint );
    begin
      ChangePaletteEntries( Indx, High(Colors) - Low(Colors) + 1, Colors );
    end;

  procedure TBuffer.SetEmpty( Value : boolean );
    begin
      if Value and (not Empty)
        then
          begin
            ReleaseImage( true );
            Changed( Self );
          end;
    end;

  function TBuffer.GetEmpty : boolean;
    begin
      Result := not Assigned( DibHeader );
    end;

  function TBuffer.PaletteClass : CBufferPalette;
    begin
      if Assigned( fOnGetPalette )
        then Result := fOnGetPalette
        else Result := TBufferPalette;
    end;

  function TBuffer.GetHandleType : TBitmapHandleType;
    begin
      Result := bmDIB;
    end;

  function TBuffer.TransparentColorStored : boolean;
    begin
      Result := TransparentMode = tmFixed;
    end;

  procedure TBuffer.SetTransparent( Value : boolean );
    begin
      if Value <> Transparent
        then
          begin
            fTransparent := Value;
            Changed( Self );
          end;
    end;

  function TBuffer.GetTransparentColor : TColor;
    begin
      if fTransparentColor = clDefault
        then
          if Monochrome
            then Result := clWhite
            else Result := Pixels[0, Height - 1]
        else Result := ColorToRgb( fTransparentColor );
    end;

  procedure TBuffer.SetTransparentColor( Value : TColor );
    begin
      if Value <> fTransparentColor
        then
          begin
            fTransparentColor := Value;
            if Value = clDefault
              then fTransparentMode := tmAuto
              else fTransparentMode := tmFixed;
            fTransparentIndx := BitmapColor( Self, Value );
            Changed( Self );
          end;
    end;

  // Helper Functions ========================================================================

  function BitmapCreateRegion( Source : TBuffer; ColorIndx : integer ): HRGN;
    var
      Pix        : pchar;
      y, x       : integer;
      OldX       : integer;
      TempRgn    : HRGN;
      bColorIndx : byte absolute ColorIndx;
      wColorIndx : word absolute ColorIndx;
    begin
      with Source do
        begin
          Result := CreateRectRgn( 0, 0, 0, 0 );
          y      := 0;
          while y < Height do
            begin
              x      := 0;
              Pix := ScanLine[ y ];

              while x < Width do
                begin
                  // Skip over transparent space
                  case BitCount of
                    1 :
                      while ( x < Width ) and ( bColorIndx = byte( GetPixelMono( Pix, x ) ) ) do
                        inc( x );
                    4 :
                      while ( x < Width ) and ( bColorIndx = GetPixel4( Pix, x ) ) do
                        inc( x );
                    8 :
                      while ( x < Width ) and ( bColorIndx = byte( Pix[x] ) ) do
                        inc( x );
                    16 :
                      while ( x < Width ) and ( wColorIndx = pword( Pix + 2 * x )^ ) do
                        inc( x );
                    24 :
                      while ( x < Width ) and ( ColorIndx = pdword( Pix + 3 * x )^ and mskColorKey ) do
                        inc( x );
                    32 :
                      while ( x < Width ) and ( ColorIndx = pdword( Pix + 4 * x )^ ) do
                        inc( x );
                  end;

                  OldX := x;

                  // Find the opaque area size
                  case BitCount of
                    1 :
                      while ( x < Width ) and ( bColorIndx <> byte( GetPixelMono( Pix, x ) ) ) do
                        inc( x );
                    4 :
                      while ( x < Width ) and ( bColorIndx <> GetPixel4( Pix, x ) ) do
                        inc( x );
                    8 :
                      while ( x < Width ) and ( bColorIndx <> byte( Pix[x] ) ) do
                        inc( x );
                    16 :
                      while ( x < Width ) and ( wColorIndx <> pword( Pix + 2 * x )^ ) do
                        inc( x );
                    24 :
                      while ( x < Width ) and ( ColorIndx <> pdword( Pix + 3 * x )^ and mskColorKey ) do
                        inc( x );
                    32 :
                      while ( x < Width ) and ( ColorIndx <> pdword( Pix + 4 * x )^ ) do
                        inc( x );
                  end;

                  // Combine the new region
                  TempRgn := CreateRectRgn( OldX, y, x, y + 1 );
                  CombineRgn( Result, Result, TempRgn, RGN_OR );
                  DeleteObject( TempRgn );
                end;
              inc( y );
            end;
        end;
    end;

  function BitmapHalftone( Source : TBuffer ) : TBuffer;
    begin
      try
        with Source do
          begin
            Result := TBuffer( ClassType ).Create;
            Result.CreateBitmap( Width, Height, 8, nil );
            DibCopyHalftoned( DibHeader, ScanLines, Result.DibHeader, Result.ScanLines );
          end;
      except
        FreeObject( Result );
      end;
    end;

  function BitmapRotate( Source : TBuffer; Angle : integer ) : TBuffer;
    begin
      assert( ( Angle div 90 ) mod 4 in [1, 2, 3], 'Bad angle in Buffer.BitmapRotate!!' );

      try
        with Source do
          begin
            Result := TBuffer( ClassType ).Create;
            if ( Angle div 90 ) mod 4 in [1, 3]
              then Result.CreateBitmap( Height, Width * (OriginalHeight div Height), BitCount, RgbEntries )
              else Result.CreateBitmap( Width, OriginalHeight, BitCount, RgbEntries );
            DibCopyRotated( DibHeader, ScanLines, Result.ScanLines, Angle );
          end;
      except
        FreeObject( Result );
      end;
    end;

  function BitmapFlip( Source : TBuffer; FlipHorizontal : boolean ) : TBuffer;
    begin
      try
        with Source do
          begin
            Result := TBuffer( ClassType ).Create;
            Result.CreateBitmap( Width, OriginalHeight, BitCount, RgbEntries );
            if FlipHorizontal
              then DibCopyFlippedHor( DibHeader, ScanLines, Result.ScanLines )
              else DibCopyFlippedVert( DibHeader, ScanLines, Result.ScanLines );
          end;
      except
        FreeObject( Result );
      end;
    end;

  function BitmapCopy( Source : TBuffer; Width, Height : integer; BitCount : integer; ForceTopDown : boolean ) : TBuffer;
    begin
      Result := BitmapCopyPiece( Source, Source.ClientRect, Width, Height, BitCount, ForceTopDown, true );
    end;

  function BitmapCopyPiece( Source : TBuffer; const Rect : TRect; Width, Height : integer; BitCount : integer; ForceTopDown : boolean; UseColors : boolean ) : TBuffer;
    begin
      assert( Assigned( Source ), 'NULL Source in Buffer.BitmapCopyPiece!!' );

      try
        if ForceTopDown or Source.TopDown
          then Result := TBuffer( Source.ClassType ).CreateSized( Width, -Height, BitCount )
          else Result := TBuffer( Source.ClassType ).CreateSized( Width, Height, BitCount );
        //!! Result.StretchSnapshotDC( Source.Canvas.Handle, Rect, UseColors );
      except
        FreeObject( Result );
      end;
    end;

  function BitmapColor( Source : TBuffer; Color : TColor ) : dword;
    var
      Data     : PGetRgbData;
      RgbColor : TColorRec absolute Color;
    begin
      Color  := Color and mskColorKey;
      Result := 0;

      with Source do
        if BitCount <= 8
          then Result := GetNearestEntry( RgbEntries^, 255, Color )
          else
            if BitCount = 24
              then Result := Color
              else
                begin
                  DibGetRgbBegin( DibHeader, pointer( Data ) );
                  with RgbColor, Data^ do
                    case BitCount of
                      16 :
                        Result := ( (crRed shr rLeft) shl rRight ) or
                                  ( (crGreen shr gLeft) shl gRight ) or
                                  ( (crBlue shr bLeft) shl bRight );
                      32 :
                        Result := (crRed shl rRight) or
                                  (crGreen shl gRight) or
                                  (crBlue shl bRight);
                    end;
                  DibGetRgbFree( Data  );
                end;
    end;

  procedure BitmapFakeInvert( Source : TBuffer );
    begin
      with Source do
        begin
          DibHeader.biHeight := -DibHeader.biHeight;
          fTopDown           := not fTopDown;
        end;
    end;

  procedure BitmapSnapshotPalette( dc : HDC; Dest : TBuffer );
    var
      hpal : HPALETTE;
    begin
      assert( Dest.UsesPalette, 'Modifying non-existant palette in Buffer.BitmapSnapshotPalette' );

      hpal := GetPaletteHandle( dc );
      try
        with Dest, DibHeader^ do
          begin
            BufferPalette.Handle := hpal;
            GetRgbPalette( hpal, 0, biClrUsed, RgbEntries^ );
            ChangePaletteEntries( 0, biClrUsed, RgbEntries^ );
          end;
      finally
        DeleteObject( hpal );
      end;
    end;

  procedure BitmapSetPaletteSize( Dest : TBuffer; aNewSize : integer );
    begin
      with Dest do
        begin
          ReallocMem( fDibHeader, DibHeader.biSize + aNewSize * sizeof( TRGBQuad ) );
          fRgbEntries         := DibColors( DibHeader );
          DibHeader.biClrUsed := aNewSize;
        end;
    end;

end.
