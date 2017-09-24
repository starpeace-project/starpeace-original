unit SpeedBmp;

interface

// Copyright (c) 1996-97 Jorge Romero Gomez, Merchise

// Notes & Tips:
//
// - When using a SpeedBitmap's BufferPalette to perform animations remember to follow these steps:
//      1.- Call SetCanvasPalette( Form.Handle, Buffer.BufferPalette ), saving the returned HPALETTE
//      2.- Call SelectPalette( OldPalette ) to restore the saved palette
//      3.- Before painting the bitmap, be sure to update the color table with
//          Buffer.ChangePaletteEntries

  uses
    Classes, Consts, Windows, Graphics, SysUtils, 
    MemUtils, Dibs, Gdi, GdiExt, Rects, BitBlt, Palettes, Buffer, CanvasBmp; 

  // TSpeedBitmap ====================================================================================

  type
    TSpeedBitmap =
      class( TCanvasedBitmap )
        private
          fMaskHandle   : HBITMAP;
          fSourceMasked : boolean;

          fHandle       : HBITMAP;

          procedure SetHandle( Value : HBITMAP );
          function  GetHandle : HBITMAP;
          function  FreeHandle( NeedHandle : boolean ) : HBITMAP;
          function  GetMaskHandle : HBITMAP;
          function  GetPalette : HPALETTE;
          procedure SetPalette( Value : HPALETTE );
          function GetHandleType: TBitmapHandleType;

        protected
          procedure HandleNeeded;
          procedure MaskHandleNeeded;

          procedure CreateBitmap( aWidth, aHeight : integer; aBitCount : integer; aRgbEntries : pointer );                     override;
          procedure AssignBitmap( Source : TBuffer );                                                                          override;
          procedure ReleaseImage( aReleasePalette : boolean );                                                                 override;

          procedure SetNewDib( newHandle : HBITMAP; newHeader : PDib; newDibPixels : pointer;
                               newPalette : TBufferPalette; ChangeMask : integer );                                            virtual;

          procedure GetRgbEntries( Indx, Count : integer; RgbQuads : pointer );                                                override;
          procedure SetRgbEntries( Indx, Count : integer; RgbQuads : pointer );                                                override;

        public
          procedure   NewSize( NewWidth, NewHeight : integer; NewBitCount : integer );                                         override;

          procedure   Assign( Source : TPersistent );                                                                          override;

          procedure   LoadFromDib( DibHeader : PDib; DibPixels : pointer );                                                    override;
          procedure   LoadFromStream( Stream : TStream );                                                                      override;

          procedure   Dormant;                                                                                                 virtual;
          function    ReleasePalette : HPALETTE;                                                                               virtual;
          function    ReleaseHandle : HBITMAP;                                                                                 virtual;
          function    GetCanvas : TBufferCanvas;                                                                               override;

          procedure   SetTransparent( Value : boolean );                                                                       override;
          procedure   SetTransparentColor( Value : TColor );                                                                   override;
          function    CreateMask( aMaskSource : boolean ) : HBITMAP;                                                           virtual;
          procedure   RestoreSource;                                                                                           virtual;
          function    ReleaseMaskHandle : HBITMAP;                                                                             virtual;
          procedure   ReleaseMask;                                                                                             virtual;

          procedure   AssignDibSection( DibHandle : HBITMAP; DibHeader : PDib; DibPixels : pointer );                          virtual;

          procedure   Flush;                                                                                                   override;
          procedure   SnapshotDC( dc : HDC; x, y : integer; UseColors : boolean );                                             override;
          procedure   StretchSnapshotDC( dc : HDC; const Rect : TRect; UseColors : boolean );                                  override;
          procedure   ClipSnapshotDC( dc : HDC; x, y : integer; const ClippingRect : TRect; UseColors : boolean );             override;
          procedure   ClipStretchSnapshotDC( dc : HDC; const Rect, ClippingRect : TRect; UseColors : boolean );                override;

          procedure   TileOnDC( dc : HDC; x, y : integer; const ClippingRect : TRect; aCopyMode : dword );                     override;
          procedure   DrawOnDC( dc : HDC; x, y : integer; aCopyMode : dword );                                                 override;
          procedure   StretchDrawOnDC( dc : HDC; const Rect : TRect; aCopyMode : dword );                                      override;
          procedure   ClipDrawOnDC( dc : HDC; x, y : integer; const ClippingRect : TRect; aCopyMode : dword );                 override;
          procedure   ClipStretchDrawOnDC( dc : HDC; const Rect, ClippingRect : TRect; aCopyMode : dword );                    override;
          procedure   DrawTransparent(const dc : HDC; const x, y: integer; const color: TColor);                               overload;
          procedure   DrawTransparent(const dc : HDC; const x, y: integer; const ClippingRect: TRect; const color: TColor);     overload;
        public
          property Handle : HBITMAP               read GetHandle           write SetHandle;
          property MaskHandle : HBITMAP           read GetMaskHandle;
          property HandleType : TBitmapHandleType read GetHandleType;
          property Palette : HPALETTE             read GetPalette          write SetPalette;
        end;

  type
    TSpeedCanvas =
      class( TBufferCanvas )
        private
          fOldBitmap : HBITMAP;

        protected
          procedure CreateHandle;                                                                                              override;
          procedure FreeContext;                                                                                               override;
      end;

  // Helper Functions --------------------------------------------------

  function LoadBitmapFromDib( DibHeader : PDib; DibPixels : pointer ) : TSpeedBitmap;
  function LoadBitmapFile( const Filename : string ) : TSpeedBitmap;
  function LoadBitmapFromStream( const Stream : TStream ) : TSpeedBitmap;
  function LoadBitmapFromResource( Instance : THandle; const ResourceName : string ) : TSpeedBitmap;
  function LoadBitmapFromResourceID( Instance : THandle; ResourceId : Integer ) : TSpeedBitmap;

  // Buffer Drawing

  procedure DrawPieceOnDC( Source : TSpeedBitmap; DestDC : HDC; x, y : integer; ClippingRect : TRect; aCopyMode : dword );
  procedure DrawPiece( Source : TSpeedBitmap; DestCanvas : TCanvas; x, y : integer; ClippingRect : TRect );
  procedure DrawOnBuffer( Source, Dest : TSpeedBitmap; x, y : integer );
  procedure StretchDrawOnBuffer( Source, Dest : TSpeedBitmap; const Rect : TRect );
  procedure ClipDrawOnBuffer( Source, Dest : TSpeedBitmap; x, y : integer; const ClippingRect : TRect );
  procedure ClipStretchDrawOnBuffer( Source, Dest : TSpeedBitmap; const Rect, ClippingRect : TRect );

// !!  function  MixTableBitmap( Source : TBufferPalette ) : TSpeedBitmap;

implementation

  // TSpeedBitmap
  // ============================================================================

  procedure TSpeedBitmap.Flush;
    begin
      GdiFlush;
    end;

  procedure TSpeedBitmap.ReleaseImage( aReleasePalette : boolean );
    begin
      fSourceMasked := false;

      inherited;

      if fHandle <> 0
        then
          begin
            if Shared
              then fShared := false         // Leave handle alone
              else DeleteObject( fHandle ); // Dispose bitmap handle
            fHandle    := 0;
            fDibPixels := nil;
          end;
      ReleaseMask;
    end;

  procedure TSpeedBitmap.GetRgbEntries( Indx, Count : integer; RgbQuads : pointer );
    var
      Entries : PRgbPalette absolute RgbQuads;
    begin
      GetDibColorTable( Canvas.Handle, Indx, Count, Entries^ );
    end;

  procedure TSpeedBitmap.SetRgbEntries( Indx, Count : integer; RgbQuads : pointer );
    var
      Entries : PRgbPalette absolute RgbQuads;
    begin
      with Canvas do
        begin
          RequiredState( [csHandleValid] );
          SetDibColorTable( CanvasHandle, Indx, Count, Entries^ );
        end;
    end;

  procedure TSpeedBitmap.SetNewDib( newHandle : HBITMAP; newHeader : PDib; newDibPixels : pointer; newPalette : TBufferPalette;
                                    ChangeMask : integer );
    begin
      ReleaseImage( (ChangeMask and pcmPaletteNotChanged) = 0 );

      if newHandle <> 0
        then
          begin
            fHandle    := newHandle;
            fDibHeader := newHeader;
            fDibPixels := newDibPixels;
            if ChangeMask and pcmPaletteFromSource <> 0
              then fBufferPalette := newPalette;

            ParamsChanged( ChangeMask );
            if ChangeMask and ( pcmPaletteFromDib or pcmPaletteNotChanged ) = 0
              then SyncPalette;
          end;
      Changed( Self );
    end;

  procedure TSpeedBitmap.CreateBitmap( aWidth, aHeight : integer; aBitCount : integer; aRgbEntries : pointer );
    var
      newHandle    : HBITMAP;
      newHeader    : PDib;
      newDibPixels : pointer;
      pcmask       : integer;
    begin
      if ( RgbEntries <> aRgbEntries )
        then pcmask := pcmUseDibPalette      // We're changing the palette
        else pcmask := pcmPaletteNotChanged; // Source already has the correct palette

      if aRgbEntries = nil
        then aRgbEntries := dsUseDefaultPalette;
      newHandle := DibSectionCreate( aWidth, aHeight, aBitCount, aRgbEntries, newHeader, newDibPixels );
      SetNewDib( newHandle, newHeader, newDibPixels, nil, pcmask or pcmSizeChanged );
    end;

  procedure TSpeedBitmap.AssignBitmap( Source : TBuffer );
    begin
      with Source do
        begin
          Self.SetNewDib( fHandle, fDibHeader, fDibPixels, nil, pcmUseDibPalette );
          fShared := true;
        end;
    end;

  procedure TSpeedBitmap.Assign( Source : TPersistent );
    var
      newHandle    : HBITMAP;
      newHeader    : PDib;
      newDibPixels : pointer;
    begin
      if Source is TSpeedBitmap
        then
          with TSpeedBitmap( Source ) do
            try
              newHandle := DibSectionFromDib( fDibHeader, fDibPixels, newHeader, newDibPixels );
              Self.SetNewDib( newHandle, newHeader, newDibPixels, nil, pcmUseDibPalette );
              Self.fTransparentColor := fTransparentColor;
              Self.fTransparentIndx  := fTransparentIndx;
              Self.fTransparentMode  := fTransparentMode;
              if Assigned( fCanvas )
                then Self.Canvas.Assign( Canvas );
              if UsesPalette
                then Self.BufferPalette.Assign( BufferPalette );
             finally   
              Changed( Self );  
            end
        else
          if Source is TBitmap
            then
              with TBitmap( Source ) do
                try
                  DibSectionFromHandle( Handle, newHeader, newDibPixels );
                  newHandle := 0; // !!! 
                  Self.SetNewDib( newHandle, newHeader, newDibPixels, nil, pcmUseDibPalette );
                  Self.TransparentColor := fTransparentColor;
                  Self.fTransparentMode  := fTransparentMode;
                  if Assigned( fCanvas )
                    then Self.Canvas.Assign( Canvas );
                finally
                  Changed( Self );
                end
            else
              if Source is TBufferPalette
                then
                  try
                    SetBufferPalette( TBufferPalette( Source ) )
                  finally
                    Changed( Self );
                  end
                else
                  inherited;
    end;

  procedure TSpeedBitmap.Dormant;
    begin
      FreeHandle( false );
    end;

  procedure TSpeedBitmap.SetTransparentColor( Value : TColor );
    begin
      if Value <> fTransparentColor
        then
          begin
            ReleaseMask;
            inherited;
          end;
    end;
    
  procedure TSpeedBitmap.SetTransparent( Value : boolean );
    begin
      if Value <> Transparent
        then
          begin
            ReleaseMask;
            inherited;
          end;
    end;

  function TSpeedBitmap.GetMaskHandle : HBITMAP;
    begin
      if fMaskHandle = 0
        then fMaskHandle := CreateMask( false );
      Result := fMaskHandle;
    end;

  procedure TSpeedBitmap.ReleaseMask;
    begin
      if fMaskHandle <> 0
        then
          begin
            DeleteObject( fMaskHandle );
            fMaskHandle := 0;
          end;
    end;

  function TSpeedBitmap.ReleaseMaskHandle : HBITMAP;
    begin
      Result      := MaskHandle;
      fMaskHandle := 0;
    end;

  // Creates a mask bitmap, if MaskSource is true also sets transparent pixels to black
  function TSpeedBitmap.CreateMask( aMaskSource : boolean ) : HBITMAP;
    var
      MaskDC       : HDC;
      SrcDC        : HDC;
      SrcBmp       : HBITMAP;
      OldSrcBmp    : HBITMAP;
      OldMaskBmp   : HBITMAP;
      OldBackColor : integer;
    begin
      Result := Windows.CreateBitmap( Width, Height, 1, 1, nil );

      SrcDC := GetDC( 0 );
      try
        SrcBmp := Windows.CreateCompatibleBitmap( SrcDC, Width, Height );
      finally
        ReleaseDC( 0, SrcDC );
      end;

      try
        MaskDC := GetBitmapDC( Result, OldMaskBmp );
        SrcDC  := GetBitmapDC( SrcBmp, OldSrcBmp );
        try
          with Canvas do // Copy DibSection to a temporal DDB
            begin
              RequiredState( [csHandleValid] );
              Windows.BitBlt( SrcDC, 0, 0, Width, Height, fHandle, 0, 0, SRCCOPY );

              SetGdiColors( SrcDC, clWhite, TransparentColor );
              Windows.BitBlt( MaskDC, 0, 0, Width, Height, SrcDC, 0, 0, SRCCOPY );

              if aMaskSource
                then     // Make all transparent pixels black
                  begin
                    OldBackColor := SetBkColor( fHandle, clBlack );
                    Windows.BitBlt( fHandle, 0, 0, Width, Height, MaskDC, 0, 0, SRCAND );
                    SetBkColor( fHandle, OldBackColor );
                  end;
            end;
        finally
          ReleaseBitmapDC( SrcDC, OldSrcBmp );
          ReleaseBitmapDC( MaskDC, OldMaskBmp );
        end;
      finally
        DeleteObject( SrcBmp );
      end;
    end;

  // Restores the blacked pixels to TransparentColor
  procedure TSpeedBitmap.RestoreSource;
    var
      MaskDC       : HDC;
      OldBitmap    : HBITMAP;
      OldForeColor : integer;
      OldBackColor : integer;
    begin
      if fMaskHandle <> 0
        then
          begin
            MaskDC := GetBitmapDC( fMaskHandle, OldBitmap );
            try
              with Canvas do
                begin
                  RequiredState( [csHandleValid] );
                  SetGdiColorsEx( fHandle, clBlack, TransparentColor, OldForeColor, OldBackColor );
                  try
                    Windows.BitBlt( fHandle, 0, 0, Width, Height, MaskDC, 0, 0, SRCPAINT );
                  finally
                    SetGdiColors( fHandle, OldForeColor, OldBackColor );
                  end;
                end;
            finally
              ReleaseBitmapDC( MaskDC, OldBitmap );
            end;
          end;
    end;

  procedure TSpeedBitmap.HandleNeeded;
    begin
      if fHandle <> 0
        then GetHandle;
    end;

  procedure TSpeedBitmap.MaskHandleNeeded;
    begin
      if fMaskHandle <> 0
        then GetMaskHandle;
    end;

  function TSpeedBitmap.GetHandle : HBITMAP;
    begin
      if fHandle = 0
        then
          begin
            assert( Assigned( DibHeader ), 'NULL bitmap in Buffer.TSpeedBitmap.GetHandle!!' );
            LoadFromDib( DibHeader, ScanLines );
          end;
      Result := fHandle;
    end;

  function TSpeedBitmap.FreeHandle( NeedHandle : boolean ) : HBITMAP;
    var
      Dib : PDib;
    begin
      if NeedHandle
        then Result := GetHandle // Make sure a valid handle is returned
        else Result := 0;

      assert( ( not NeedHandle ) or ( fHandle <> 0 ), 'Bitmap handle could not be created in Buffer.TSpeedBitmap.FreeHandle!!' );

      if fHandle <> 0
        then
          begin
            Dib := DibCopy( DibHeader, ScanLines );

            if NeedHandle
              then fHandle := 0; // This way fHandle is not freed in ReleaseImage
            ReleaseImage( false );

            fDibHeader := Dib;
            fDibPixels := DibPtr( Dib );
          end;
    end;

  function TSpeedBitmap.ReleaseHandle : HBITMAP;
    begin
      if Shared // Create a copy of the shared DibSection
        then LoadFromDib( DibHeader, ScanLines );
      Result := FreeHandle( true );
    end;

  function TSpeedBitmap.ReleasePalette : HPALETTE;
    begin
      if UsesPalette
        then Result := BufferPalette.ReleaseHandle
        else Result := 0;
    end;

  procedure TSpeedBitmap.NewSize( NewWidth, NewHeight : integer; NewBitCount : integer );
    var
      BitCountChanged : boolean;
      newHandle       : HBITMAP;
      newHeader       : PDib;
      newDibPixels    : pointer;
      tmpDC           : HDC;
      oldBitmap       : HBITMAP;
    begin
      if (NewWidth > 0) and (NewHeight <> 0) and (NewBitCount > 0)
        then
          if Empty
            then CreateBitmap( NewWidth, NewHeight, NewBitCount, nil )
            else
              begin
                BitCountChanged := NewBitCount <> BitCount;

                if BitCountChanged or ( NewWidth <> Width ) or ( NewHeight <> OriginalHeight )
                  then
                    begin
                      if NewBitCount = 1
                        then fRgbEntries := @rgbMonochromePalette;
                      newHandle := DibSectionCreate( NewWidth, NewHeight, NewBitCount, fRgbEntries, newHeader, newDibPixels );

                      // Copy original pixels
                      tmpDC     := CreateCompatibleDC( 0 );
                      oldBitmap := SelectObject( tmpDC, newHandle );
                      with Canvas do
                        if (NewBitCount < BitCount) and (NewBitCount <= 8)
                          then
                            begin
                              SetStretchBltMode( tmpDC, STRETCH_HALFTONE );
                              SetBrushOrgEx( tmpDC, 0, 0, nil );
                              Windows.StretchBlt( tmpDC, 0, 0, Width, Height, Handle, 0, 0, Width, Height, CopyMode );
                            end
                          else
                            Windows.BitBlt( tmpDC, 0, 0, Width, Height, Handle, 0, 0, CopyMode );
                      SelectObject( tmpDC, oldBitmap );
                      DeleteDC( tmpDC );

                      if BitCountChanged
                        then SetNewDib( newHandle, newHeader, newDibPixels, nil, pcmUseDibPalette )
                        else SetNewDib( newHandle, newHeader, newDibPixels, nil, pcmUseCurrentPalette );
                    end;
              end
        else
          begin
            ReleaseImage( true );
            fBitCount       := NewBitCount;
            fWidth          := NewWidth;
            fOriginalHeight := NewHeight;
            fHeight         := abs( fOriginalHeight );
          end;
    end;

  // SnapshotDC methods --------------------

  procedure TSpeedBitmap.SnapshotDC( dc : HDC; x, y : integer; UseColors : boolean );
    var
      hpal : HPALETTE;
    begin
      assert( Assigned(Canvas) and (not Empty), 'Null HBITMAP in Buffer.TSpeedBitmap.SnapshotDC' );

      with Canvas do
        begin
          RequiredState( [csHandleValid] );
          if UseColors and UsesPalette
            then
              with fDibHeader^ do
                begin
                  hpal := GetPaletteHandle( dc );
                  try
                    GetRgbPalette( hpal, 0, biClrUsed, fRgbEntries^ );
                  finally
                    DeleteObject( hpal );
                  end;
                  ChangePaletteEntries( 0, biClrUsed, fRgbEntries^ );
                end;

          Windows.BitBlt( CanvasHandle, 0, 0, Width, Height, dc, x, y, CopyMode );
        end;
    end;

  procedure TSpeedBitmap.StretchSnapshotDC( dc : HDC; const Rect : TRect; UseColors : boolean );
    var
      hpal : HPALETTE;
    begin
      assert( fHandle <> 0, 'Null HBITMAP in Buffer.TSpeedBitmap.StretchSnapshotDC' );

      with Rect, Canvas do
        begin
          RequiredState( [csHandleValid] );
          SetStretchBltMode( dc, StretchBltMode );
          if StretchBltMode = STRETCH_HALFTONE
            then SetBrushOrgEx( dc, 0, 0, nil );

          if UseColors and UsesPalette
            then
              with fDibHeader^ do
                begin
                  hpal := GetPaletteHandle( dc );
                  try
                    GetRgbPalette( hpal, 0, biClrUsed, fRgbEntries^ );
                  finally
                    DeleteObject( hpal );
                  end;
                  ChangePaletteEntries( 0, biClrUsed, fRgbEntries^ );
                end;

          Windows.StretchBlt( CanvasHandle, 0, 0, Self.Width, Self.Height,
                              dc, Left, Top, Right - Left, Bottom - Top, CopyMode );
        end;
    end;

  procedure TSpeedBitmap.ClipSnapshotDC( dc : HDC; x, y : integer; const ClippingRect : TRect; UseColors : boolean );
    var
      SrcOfs  : TPoint;
      DstRect : TRect;
      DstSize : TPoint;
      hpal    : HPALETTE;
    begin
      assert( Assigned(Canvas) and (not Empty), 'Null HBITMAP in Buffer.TSpeedBitmap.SnapshotDC' );

      with Canvas do
        begin
          RequiredState( [csHandleValid] );
          if UseColors and UsesPalette
            then
              with fDibHeader^ do
                begin
                  hpal := GetPaletteHandle( dc );
                  try
                    GetRgbPalette( hpal, 0, biClrUsed, fRgbEntries^ );
                  finally
                    DeleteObject( hpal );
                  end;
                  ChangePaletteEntries( 0, biClrUsed, fRgbEntries^ );
                end;

          DstRect := RectFromBounds( x, y, Width, Height );
          if IntersectRect( DstRect, DstRect, ClippingRect )
            then
              begin
                DstSize := RectSize( DstRect );

                if x < DstRect.Left
                  then SrcOfs.x := DstRect.Left - x
                  else SrcOfs.x := 0;

                if y < DstRect.Top
                  then SrcOfs.y := DstRect.Top - y
                  else SrcOfs.y := 0;

                Windows.BitBlt( CanvasHandle, SrcOfs.x, SrcOfs.y, DstSize.x, DstSize.y,
                                dc, DstRect.Left, DstRect.Top, CopyMode );
              end;
        end;
    end;

  procedure TSpeedBitmap.ClipStretchSnapshotDC( dc : HDC; const Rect, ClippingRect : TRect; UseColors : boolean );
    var
      SrcOfs  : TPoint;
      SrcSize : TPoint;
      DstRect : TRect;
      DstSize : TPoint;
      hpal    : HPALETTE;
    begin
      assert( Assigned(Canvas) and (not Empty), 'Null HBITMAP in Buffer.TSpeedBitmap.StretchSnapshotDC' );

      with Rect, Canvas do
        begin
          RequiredState( [csHandleValid] );
          if StretchBltMode = STRETCH_HALFTONE
            then SetBrushOrgEx( dc, 0, 0, nil );
          SetStretchBltMode( dc, StretchBltMode );

          if UseColors and UsesPalette
            then
              with fDibHeader^ do
                begin
                  hpal := GetPaletteHandle( dc );
                  try
                    GetRgbPalette( hpal, 0, biClrUsed, fRgbEntries^ );
                  finally
                    DeleteObject( hpal );
                  end;
                  ChangePaletteEntries( 0, biClrUsed, fRgbEntries^ );
                end;

          if IntersectRect( DstRect, ClippingRect, Rect )
            then
              begin
                DstSize := RectSize( DstRect );

                if Rect.Left < DstRect.Left
                  then SrcOfs.x := (DstRect.Left - Rect.Left) * (Rect.Right - Rect.Left) div Width
                  else SrcOfs.x := 0;
                SrcSize.x := DstSize.x * (Rect.Right - Rect.Left) div Width;

                if Rect.Top < DstRect.Top
                  then SrcOfs.y := (DstRect.Top - Rect.Top) * (Rect.Bottom - Rect.Top) div Height
                  else SrcOfs.y := 0;
                SrcSize.y := DstSize.y * (Rect.Bottom - Rect.Top) div Width;

                StretchBlt( CanvasHandle, SrcOfs.x, SrcOfs.y, SrcSize.x, SrcSize.y,
                            dc, DstRect.Left, DstRect.Top, DstSize.x, DstSize.y, CopyMode );
              end;
        end;
    end;

  // DrawOnDC methods --------------------

  {$WARNINGS OFF}
  procedure TSpeedBitmap.DrawOnDC( dc : HDC; x, y : integer; aCopyMode : dword );
    var
      OldPalette : HPALETTE;
      ChgPalette : boolean;
    begin
      assert( Assigned(Canvas) and (not Empty), 'Null HBITMAP in Buffer.TSpeedBitmap.Draw' );

      with Canvas do
        begin
          ChgPalette := (not IgnorePalette) and UsesPalette;
          if ChgPalette
            then
              begin
                OldPalette := SelectPalette( dc, Palette, false );
                RealizePalette( dc );
              end;
          try
            RequiredState( [csHandleValid] );
            if Transparent
              then
                if GdiExtAvailable
                  then TransparentBlt( dc, x, y, Width, Height, CanvasHandle, 0, 0, Width, Height, TransparentColor )
                  else TransparentStretchBlt( dc, x, y, Width, Height, CanvasHandle, 0, 0, Width, Height, MaskHandle, fSourceMasked )
              else Windows.BitBlt( dc, x, y, Width, Height, CanvasHandle, 0, 0, aCopyMode );
          finally
            if ChgPalette
              then SelectPalette( dc, OldPalette, false );
          end;
        end;
    end;

  procedure TSpeedBitmap.StretchDrawOnDC( dc : HDC; const Rect : TRect; aCopyMode : dword );
    var
      OldPalette : HPALETTE;
      ChgPalette : boolean;
    begin
      assert( Assigned(Canvas) and (not Empty), 'Null HBITMAP in Buffer.TSpeedBitmap.StretchDraw' );

      with Rect, Canvas do
        begin
          if StretchBltMode = STRETCH_HALFTONE
            then SetBrushOrgEx( dc, 0, 0, nil );
          SetStretchBltMode( dc, StretchBltMode );

          ChgPalette := (not IgnorePalette) and UsesPalette;
          if ChgPalette
            then
              begin
                OldPalette := SelectPalette( dc, Palette, false );
                RealizePalette( dc );
              end;

          try
            RequiredState( [csHandleValid] );
            if Transparent
              then 
                if GdiExtAvailable
                  then TransparentBlt( dc, Left, Top, Right - Left, Bottom - Top, CanvasHandle, 0, 0, Width, Height, TransparentColor )
                  else TransparentStretchBlt( dc, Left, Top, Right - Left, Bottom - Top, CanvasHandle, 0, 0, Width, Height, MaskHandle, fSourceMasked )
              else StretchBlt( dc, Left, Top, Right - Left, Bottom - Top, CanvasHandle, 0, 0, Width, Height, aCopyMode );
          finally
            if ChgPalette
              then SelectPalette( dc, OldPalette, false );
          end;
        end;
    end;

  procedure TSpeedBitmap.TileOnDC( dc : HDC; x, y : integer; const ClippingRect : TRect; aCopyMode : dword );
    var
      OldPalette : HPALETTE;
      SrcOfs     : TPoint;
      DstSize    : TPoint;
      Origin     : TPoint;
      SaveX      : integer;
      DstRect    : TRect;
      ChgPalette : boolean;
    begin
      assert( Assigned(Canvas) and (not Empty), 'Null HBITMAP in Buffer.TSpeedBitmap.TileOnDC' );

      with DstRect, Canvas do
        begin
          ChgPalette := (not IgnorePalette) and UsesPalette;
          if ChgPalette
            then
              begin
                OldPalette := SelectPalette( dc, Palette, false );
                RealizePalette( dc );
              end;
          try
            RequiredState( [csHandleValid] );

            if x < 0
              then SaveX := -( ClippingRect.Left - x mod Width )
              else SaveX := ( (x - ClippingRect.Left) div Width ) * Width;
            if y < 0
              then Origin.y := -( ClippingRect.Top - y mod Height )
              else Origin.y := ( (y - ClippingRect.Top) div Height ) * Height;

            repeat                                                   // Loop rows
              Origin.x := SaveX;

              repeat                                                 // Loop columns
                x := Origin.x;
                y := Origin.y;
                DstRect := RectFromBounds( x, y, Width, Height );
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

                      Windows.BitBlt( dc, x, y, DstSize.x, DstSize.y, CanvasHandle, SrcOfs.x, SrcOfs.y, aCopyMode );
                    end;

                inc( Origin.x, Width );
              until Origin.x > ClippingRect.Right;

              inc( Origin.y, Height );
            until Origin.y > ClippingRect.Bottom;
          finally
            if ChgPalette
              then SelectPalette( dc, OldPalette, false );
          end;
        end;
    end;

  procedure TSpeedBitmap.ClipDrawOnDC( dc : HDC; x, y : integer; const ClippingRect : TRect; aCopyMode : dword );
    var
      OldPalette : HPALETTE;
      SrcOfs     : TPoint;
      DstSize    : TPoint;
      DstRect    : TRect;
      ChgPalette : boolean;
    begin
      assert( Assigned(Canvas) and (not Empty), 'Null HBITMAP in Buffer.TSpeedBitmap.ClipDrawOnDC' );

      with DstRect, Canvas do
        begin
          ChgPalette := (not IgnorePalette) and UsesPalette;
          if ChgPalette
            then
              begin
                OldPalette := SelectPalette( dc, Palette, false );
                RealizePalette( dc );
              end;
          try
            DstRect := RectFromBounds( x, y, Width, Height );
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

                  RequiredState( [csHandleValid] );
                  if Transparent
                    then
                      if GdiExtAvailable
                        then TransparentBlt( dc, x, y, DstSize.x, DstSize.y, CanvasHandle, SrcOfs.x, SrcOfs.y, DstSize.x, DstSize.y, TransparentColor )
                        else TransparentStretchBlt( dc, x, y, DstSize.x, DstSize.y, CanvasHandle, SrcOfs.x, SrcOfs.y, DstSize.x, DstSize.y, MaskHandle, fSourceMasked )
                    else Windows.BitBlt( dc, x, y, DstSize.x, DstSize.y, CanvasHandle, SrcOfs.x, SrcOfs.y, aCopyMode );
                end;
          finally
            if ChgPalette
              then SelectPalette( dc, OldPalette, false );
          end;
        end;
    end;

  procedure TSpeedBitmap.ClipStretchDrawOnDC( dc : HDC; const Rect, ClippingRect : TRect; aCopyMode : dword );
    var
      OldPalette : HPALETTE;
      ChgPalette : boolean;
      SrcOfs     : TPoint;
      SrcSize    : TPoint;
      DstRect    : TRect;
      DstSize    : TPoint;
    begin
      assert( Assigned(Canvas) and (not Empty), 'Null HBITMAP in Buffer.TSpeedBitmap.ClipStretchDrawOnDC' );

      with Canvas do
        begin
          if StretchBltMode = STRETCH_HALFTONE
            then SetBrushOrgEx( dc, 0, 0, nil );
          SetStretchBltMode( dc, StretchBltMode );

          ChgPalette := (not IgnorePalette) and UsesPalette;
          if ChgPalette
            then
              begin
                OldPalette := SelectPalette( dc, Palette, false );
                RealizePalette( dc );
              end;
          try
            RequiredState( [csHandleValid] );
            if IntersectRect( DstRect, ClippingRect, Rect )
              then
                begin
                  DstSize := RectSize( DstRect );

                  if Rect.Left < DstRect.Left
                    then SrcOfs.x := (DstRect.Left - Rect.Left) * Width div (Rect.Right - Rect.Left)
                    else SrcOfs.x := 0;
                  SrcSize.x := DstSize.x * Width div (Rect.Right - Rect.Left);

                  if Rect.Top < DstRect.Top
                    then SrcOfs.y := (DstRect.Top - Rect.Top) * Height div (Rect.Bottom - Rect.Top)
                    else SrcOfs.y := 0;
                  SrcSize.y := DstSize.y * Height div (Rect.Bottom - Rect.Top);

                  if Transparent
                    then
                      if GdiExtAvailable
                        then TransparentBlt( dc, DstRect.Left, DstRect.Top, DstSize.x, DstSize.y, CanvasHandle, SrcOfs.x, SrcOfs.y, SrcSize.x, SrcSize.y, TransparentColor )
                        else TransparentStretchBlt( dc, DstRect.Left, DstRect.Top, DstSize.x, DstSize.y, CanvasHandle, SrcOfs.x, SrcOfs.y, SrcSize.x, SrcSize.y, MaskHandle, fSourceMasked )
                    else StretchBlt( dc, DstRect.Left, DstRect.Top, DstSize.x, DstSize.y, CanvasHandle, SrcOfs.x, SrcOfs.y, SrcSize.x, SrcSize.y, aCopyMode );
                end;
          finally
            if ChgPalette
              then SelectPalette( dc, OldPalette, false );
          end;
        end;
    end;
  {$WARNINGS ON}

  // Bitmap loading methods -----

  procedure TSpeedBitmap.AssignDibSection( DibHandle : HBITMAP; DibHeader : PDib; DibPixels : pointer );
    begin
      if DibHandle = 0
        then ReleaseImage( true )
        else
          begin
            if (DibHeader = nil) or (DibPixels = nil)
              then
                begin
                  if Assigned( DibHeader )
                    then DibFree( DibHeader );
                  DibSectionFromHandle( DibHandle, DibHeader, DibPixels );
                end;
            SetNewDib( DibHandle, DibHeader, DibPixels, nil, pcmUseDibPalette );
          end;
    end;

  procedure TSpeedBitmap.LoadFromDib( DibHeader : PDib; DibPixels : pointer );
    var
      newHandle    : HBITMAP;
      newHeader    : PDib;
      newDibPixels : pointer;
    begin
      if DibPixels = nil
        then DibPixels := DibPtr( DibHeader );
      newHandle := DibSectionFromDib( DibHeader, DibPixels, newHeader, newDibPixels );
      SetNewDib( newHandle, newHeader, newDibPixels, nil, pcmUseDibPalette );
    end;

  procedure TSpeedBitmap.LoadFromStream( Stream : TStream );
    var
      newHandle    : HBITMAP;
      newHeader    : PDib;
      newDibPixels : pointer;
    begin
      try
        newHandle := DibSectionLoadFromStream( Stream, newHeader, newDibPixels, false );
        if newHandle <> 0
          then SetNewDib( newHandle, newHeader, newDibPixels, nil, pcmUseDibPalette )
          else raise EInvalidGraphic( SInvalidBitmap );
      except
        ReleaseImage( true );
        raise;
      end;
    end;

  // TSpeedBitmap continues...

  procedure TSpeedBitmap.SetPalette( Value : HPALETTE );
    begin
      if not UsesPalette
        then fBufferPalette := PaletteClass.CreateFromHandle( Value )
        else BufferPalette.Handle := Value;
      SyncPalette;
    end;

  procedure TSpeedBitmap.SetHandle( Value : HBITMAP );
    begin
      AssignDibSection( Value, nil, nil );
      fShared := true;
    end;

  function TSpeedBitmap.GetPalette : HPALETTE;
    begin
      if UsesPalette
        then Result := BufferPalette.Handle
        else Result := 0;
    end;

  function TSpeedBitmap.GetCanvas : TBufferCanvas;
    begin
      if not Assigned( fCanvas )
        then fCanvas := TSpeedCanvas.Create( Self ); // !!
      Result := fCanvas;
    end;

  // TSpeedCanvas =====================================================================

  procedure TSpeedCanvas.FreeContext;
    begin
      if fHandle <> 0
        then
          begin
            if fOldBitmap <> 0
              then SelectObject( fHandle, fOldBitmap );
            inherited;
          end;
    end;

  procedure TSpeedCanvas.CreateHandle;
    var
      dc : HDC;
    begin
      assert( Assigned( fBitmap ), 'Null fBitmap in Buffer.TSpeedCanvas.CreateHandle!!' );

      FreeContext;
      dc         := CreateCompatibleDC( 0 );
      fOldBitmap := SelectObject( dc, TSpeedBitmap( fBitmap ).Handle );
      Handle     := dc;
      BitmapCanvasList.Add( Self );
    end;

  // Bitmap helper functions ==========================================================
(* // !!
  function MixTableBitmap( Source : TBufferPalette ) : TSpeedBitmap;
    var
      x, y    : integer;
      c       : byte;
    begin
      Result := TSpeedBitmap.CreateSized( 256, 256, 8 );
      Result.Assign( Source );
      with Result, Source do
        begin
          for x := 0 to 255 do
            begin
              PixelAddr[x, x]^ := MixTable[x, x];
              for y := x + 1 to 255 do
                begin
                  c := MixTable[x, y];
                  with Result do
                    begin
                      PixelAddr[x, y]^ := c;
                      PixelAddr[y, x]^ := c;
                    end;
                end;
            end;
        end;
    end;
*)
  // DrawPiece methods --------------------

  procedure DrawPieceOnDC( Source : TSpeedBitmap; DestDC : HDC; x, y : integer; ClippingRect : TRect; aCopyMode : dword );
    var
      NewX, NewY : integer;
    begin
      with ClippingRect do
        begin
          NewX := x - Left;
          NewY := y - Top;
          OffsetRect( ClippingRect, NewX, NewY );
          Source.ClipDrawOnDC( DestDC, NewX, NewY, ClippingRect, aCopyMode );
        end;
    end;

  procedure DrawPiece( Source : TSpeedBitmap; DestCanvas : TCanvas; x, y : integer; ClippingRect : TRect );
    begin
      DrawPieceOnDC( Source, DestCanvas.Handle, x, y, ClippingRect, DestCanvas.CopyMode );
    end;

  // DrawOnBuffer methods --------------------

  procedure DrawOnBuffer( Source, Dest : TSpeedBitmap; x, y : integer );
    begin
      assert( Assigned(Source.Canvas) and Assigned(Dest.Canvas) and
              not (Source.Empty or Dest.Empty), 'Null HBITMAP in Buffer.DrawOnBuffer' );
      with Dest.Canvas do
        Source.DrawOnDC( Handle, x, y, CopyMode );
    end;

  procedure StretchDrawOnBuffer( Source, Dest : TSpeedBitmap; const Rect : TRect );
    begin
      assert( Assigned(Source.Canvas) and Assigned(Dest.Canvas) and
              not (Source.Empty or Dest.Empty), 'Null HBITMAP in Buffer.StretchDrawOnBuffer' );
      with Dest.Canvas do
        Source.StretchDrawOnDC( Handle, Rect, CopyMode );
    end;

  procedure ClipDrawOnBuffer( Source, Dest : TSpeedBitmap; x, y : integer; const ClippingRect : TRect );
    begin
      assert( Assigned(Source.Canvas) and Assigned(Dest.Canvas) and
              not (Source.Empty or Dest.Empty), 'Null HBITMAP in Buffer.ClipDrawOnBuffer' );
      with Dest.Canvas do
        Source.ClipDrawOnDC( Handle, x, y, ClippingRect, CopyMode );
    end;

  procedure ClipStretchDrawOnBuffer( Source, Dest : TSpeedBitmap; const Rect, ClippingRect : TRect );
    begin
      assert( Assigned(Source.Canvas) and Assigned(Dest.Canvas) and
              not (Source.Empty or Dest.Empty), 'Null HBITMAP in Buffer.ClipStretchDrawOnBuffer' );
      with Dest.Canvas do
        Source.ClipStretchDrawOnDC( Handle, Rect, ClippingRect, CopyMode );
    end;

  // Loading helper functions

  function LoadBitmapFromDib( DibHeader : PDib; DibPixels : pointer ) : TSpeedBitmap;
    begin
      try
        Result := TSpeedBitmap.Create;
        Result.LoadFromDib( DibHeader, DibPixels );
      except
        FreeObject( Result );
      end;
    end;

  function LoadBitmapFromStream( const Stream : TStream ) : TSpeedBitmap;
    begin
      try
        Result := TSpeedBitmap.Create;
        Result.LoadFromStream( Stream );
      except
        FreeObject( Result );
      end;
    end;

  function LoadBitmapFromResource( Instance : THandle; const ResourceName : string ) : TSpeedBitmap;
    begin
      try
        Result := TSpeedBitmap.Create;
        Result.LoadFromResourceName( Instance, ResourceName );
      except
        FreeObject( Result );
      end;
    end;

  function LoadBitmapFromResourceID( Instance : THandle; ResourceId : Integer ) : TSpeedBitmap;
    begin
      try
        Result := TSpeedBitmap.Create;
        Result.LoadFromResourceId( Instance, ResourceId );
      except
        FreeObject( Result );
      end;
    end;

  function LoadBitmapFile( const Filename : string ) : TSpeedBitmap;
    begin
      try
        Result := TSpeedBitmap.Create;
        Result.LoadFromFile( Filename );
      except
        FreeObject( Result );
      end;
    end;

procedure TSpeedBitmap.DrawTransparent(const dc: HDC; const x, y: integer; const color: TColor);
  var
    crBack : TColor;
    dcImage, dcTrans : HDC;
    bitmapTrans, pOldBitmapTrans : HBitmap;
    crOldBack, crOldText : TColor;
  begin
    crOldBack := windows.SetBkColor(dc, clWhite);
    crOldText := windows.SetTextColor(dc, clBlack);
    // Create two memory dcs for the image and the mask
    dcTrans := windows.CreateCompatibleDC(dc);

    // Create the mask bitmap
    bitmapTrans := windows.CreateBitmap(fWidth, fHeight, 1, 1, nil);

    // Select the mask bitmap into the appropriate dc
    windows.SelectObject(dcTrans, bitmapTrans);

    // Build mask based on transparent colour
     windows.SetBkColor(Canvas.Handle, color);
     windows.BitBlt(dcTrans, 0, 0, fWidth, fHeight, Canvas.Handle, 0, 0, SRCCOPY);

    // Do the work - True Mask method - cool if not actual display
     windows.BitBlt(dc, x, y, fWidth, fHeight, Canvas.Handle, 0, 0, SRCINVERT);
     windows.BitBlt(dc, x, y, fWidth, fHeight, dcTrans, 0, 0, SRCAND);
     windows.BitBlt(dc, x, y, fWidth, fHeight, Canvas.Handle, 0, 0, SRCINVERT);

    // Restore settings
     windows.SetBkColor(dc, crOldBack);
     windows.SetTextColor(dc, crOldText);
     windows.DeleteDc(dcTrans);
     windows.DeleteObject(bitmapTrans);
  end;

procedure TSpeedBitmap.DrawTransparent(const dc: HDC; const x, y: integer; const ClippingRect: TRect; const color: TColor);
  var
    dcImage, dcTrans : HDC;
    bitmapTrans: HBitmap;
    Transx,Transy : integer;
    crOldBack, crOldText : TColor;
  begin
    // Create two memory dcs for the image and the mask
    dcTrans := windows.CreateCompatibleDC(dc);

    crOldBack := windows.SetBkColor(dc, clWhite);
    crOldText := windows.SetTextColor(dc, clBlack);
    with ClippingRect do
      begin
        // Create the mask bitmap
        Transx := right-left;
        Transy := bottom-top;
        bitmapTrans := windows.CreateBitmap(Transx, Transy, 1, 1, nil);

        // Select the mask bitmap into the appropriate dc
        windows.SelectObject(dcTrans, bitmapTrans);

        // Build mask based on transparent colour
         windows.SetBkColor(Canvas.Handle, color);
         windows.BitBlt(dcTrans, 0, 0, Transx, Transy, Canvas.Handle, left, top, SRCCOPY);

        // Do the work - True Mask method - cool if not actual display
         windows.BitBlt(dc, x, y, Transx, Transy, Canvas.Handle, left, top, SRCINVERT);
         windows.BitBlt(dc, x, y, Transx, Transy, dcTrans, 0, 0, SRCAND);
         windows.BitBlt(dc, x, y, Transx, Transy, Canvas.Handle, left, top, SRCINVERT);
      end;

    // Restore settings
     windows.SetBkColor(dc, crOldBack);
     windows.SetTextColor(dc, crOldText);
     windows.DeleteDc(dcTrans);
     windows.DeleteObject(bitmapTrans);
  end;

function TSpeedBitmap.GetHandleType: TBitmapHandleType;
begin

end;


end.
