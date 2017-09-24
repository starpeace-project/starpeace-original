unit CanvasBmp;

interface

  uses
    Classes, Consts, Windows, SysUtils, Graphics,
    MemUtils, Gdi, GdiExt, Buffer;

  // TCanvasedBuffer ===============================================================================

  const
    csAllValid = [csHandleValid..csBrushValid];

  type
    TStretchBltMode = type integer;

  type
    TBufferCanvas = class;

    TCanvasedBitmap =
      class( TBuffer )
        protected
          fCanvas         : TBufferCanvas;
          fStretchBltMode : TStretchBltMode;
          fShared         : boolean;

        protected
          function  GetCanvas : TBufferCanvas;                                                                                 virtual; abstract;
          procedure FreeContext;                                                                                               override;
          function  GetPixels( x, y : integer ) : TColor;                                                                      override;
          procedure SetPixels( x, y : integer; Value : TColor );                                                               override;

        public
          property Canvas : TBufferCanvas read GetCanvas;

          procedure   SnapshotDC( dc : HDC; x, y : integer; UseColors : boolean );                                             virtual; abstract;
          procedure   StretchSnapshotDC( dc : HDC; const Rect : TRect; UseColors : boolean );                                  virtual; abstract;
          procedure   ClipSnapshotDC( dc : HDC; x, y : integer; const ClippingRect : TRect; UseColors : boolean );             virtual; abstract;
          procedure   ClipStretchSnapshotDC( dc : HDC; const Rect, ClippingRect : TRect; UseColors : boolean );                virtual; abstract;

          procedure   TileOnDC( dc : HDC; x, y : integer; const ClippingRect : TRect; aCopyMode : dword );                     virtual; abstract;
          procedure   DrawOnDC( dc : HDC; x, y : integer; aCopyMode : dword );                                                 virtual; abstract;
          procedure   StretchDrawOnDC( dc : HDC; const Rect : TRect; aCopyMode : dword );                                      virtual; abstract;
          procedure   ClipDrawOnDC( dc : HDC; x, y : integer; const ClippingRect : TRect; aCopyMode : dword );                 virtual; abstract;
          procedure   ClipStretchDrawOnDC( dc : HDC; const Rect, ClippingRect : TRect; aCopyMode : dword );                    virtual; abstract;

          procedure   Tile( aCanvas : TCanvas; x, y : integer; const ClippingRect : TRect );                                   virtual;
          procedure   Draw( aCanvas : TCanvas; x, y : integer );                                                               virtual;
          procedure   StretchDraw( aCanvas : TCanvas; const Rect : TRect );                                                    virtual;
          procedure   ClipDraw( aCanvas : TCanvas; x, y : integer; const ClippingRect : TRect );                               virtual;
          procedure   ClipStretchDraw( aCanvas : TCanvas; const Rect, ClippingRect : TRect );                                  virtual;

        public
          constructor Create;                                                                                                  override;
          destructor  Destroy;                                                                                                 override;

          property Shared : boolean                   read fShared             write fShared;

        published
          property StretchBltMode : TStretchBltMode   read fStretchBltMode     write fStretchBltMode;
      end;

    TBufferCanvas =
      class( TPersistent )
        protected
          fHandle     : HDC;
          fBitmap     : TCanvasedBitmap;
          fFont       : TFont;
          fPen        : TPen;
          fBrush      : TBrush;
          fPenPos     : TPoint;
          fCopyMode   : TCopyMode;
          State       : TCanvasState;

        protected
          procedure CreateBrush;
          procedure SetBrush( Value : TBrush );
          procedure BrushChanged( aBrush : TObject );

          procedure CreateFont;
          procedure SetFont( Value : TFont );
          procedure FontChanged( aFont : TObject );

          procedure CreatePen;
          procedure SetPen( Value : TPen );
          procedure PenChanged( aPen : TObject );

          function  GetClipRect : TRect;
          function  GetPenPos : TPoint;
          procedure SetPenPos( const Value : TPoint );
          function  GetPixels( x, y : integer ) : TColor;
          procedure SetPixels( x, y : integer; Value : TColor );
          function  GetScanLines : pointer;
          function  GetPixelAddr(x, y : integer ) : pointer;

          function  GetHandle : HDC;
          procedure SetHandle( Value : HDC );
          procedure DeselectHandles;

        protected
          procedure CreateHandle;                                                                                              virtual; abstract;
          procedure FreeContext;                                                                                               virtual;

        public
          procedure   RequiredState( ReqState : TCanvasState );

        public
          constructor Create( aBitmap : TCanvasedBitmap );
          destructor  Destroy;                                                                                                 override;

          procedure   Assign( Source : TPersistent );                                                                          override;
          procedure   CopyRect( const Dest : TRect; Canvas : TBufferCanvas;
                                const Source : TRect );

          procedure   Draw( x, y : integer; Graphic : TGraphic );
          procedure   StretchDraw( const Rect : TRect; Graphic : TGraphic );

          procedure   LineTo( x, y : integer );
          procedure   MoveTo( x, y : integer );

          procedure   Arc( x1, y1, x2, y2, X3, Y3, X4, Y4 : integer );
          procedure   Chord( x1, y1, x2, y2, X3, Y3, X4, Y4 : integer );
          procedure   Ellipse( x1, y1, x2, y2 : integer );
          procedure   Pie( x1, y1, x2, y2, x3, y3, x4, y4 : integer );

          procedure   Polygon( const Points : array of TPoint );
          procedure   PolyLine( const Points : array of TPoint );

          procedure   Rectangle( x1, y1, x2, y2 : integer );
          procedure   RoundRect( x1, y1, x2, y2, x3, y3 : integer );
          procedure   FrameRect( const Rect : TRect );
          procedure   FrameRgn( Region : HRGN; FrameWidth, FrameHeight : integer );
          procedure   DrawFocusRect( const Rect : TRect );
          procedure   FillRect( const Rect : TRect );
          procedure   PaintRgn( Region : HRGN );
          procedure   FillRgn( Region : HRGN; aBrush : HBRUSH );
          procedure   FloodFill( x, y : integer; Color : TColor; FillStyle : TFillStyle );

          procedure   Refresh;

          function    TextHeight( const Text : string ) : integer;
          function    TextWidth( const Text : string ) : integer;
          function    TextExtent( const Text : string ) : TSize;
          procedure   TextOut( x, y : integer; const Text : string );
          procedure   TextRect( const Rect : TRect; x, y : integer; const Text : string );

        public
          property Pixels[x, y : integer] : TColor     read GetPixels write SetPixels; default;
          property Handle   : HDC                      read GetHandle write SetHandle;
          property PenPos   : TPoint                   read GetPenPos write SetPenPos;
          property ClipRect : TRect                    read GetClipRect;

        public
          property CanvasHandle : HDC                  read fHandle;
          property ScanLines : pointer                 read GetScanLines;
          property PixelAddr[x, y : integer] : pointer read GetPixelAddr;

        published
          property Owner    : TCanvasedBitmap          read fBitmap;
          property CopyMode : TCopyMode                read fCopyMode write fCopyMode default cmSrcCopy;
          property Brush    : TBrush                   read fBrush    write SetBrush;
          property Font     : TFont                    read fFont     write SetFont;
          property Pen      : TPen                     read fPen      write SetPen;
      end;

  function TransparentStretchBlt( DstDC : HDC; DstX, DstY, DstWidth, DstHeight : integer;
                                  SrcDC : HDC; SrcX, SrcY, SrcWidth, SrcHeight : integer; Mask : HBITMAP; SourceMasked : boolean ) : BOOL;

  // Internal helpers
  // ============================================================================

  var
    BitmapCanvasList : TList = nil;

implementation

  // GDI Stuff

  function SetBrushOrgEx( DC: HDC; X, Y: Integer; Points : pointer ): BOOL; stdcall; external 'GDI32.DLL' name 'SetBrushOrgEx';

  function TransparentStretchBlt( DstDC : HDC; DstX, DstY, DstWidth, DstHeight : integer;
                                  SrcDC : HDC; SrcX, SrcY, SrcWidth, SrcHeight : integer; Mask : HBITMAP; SourceMasked : boolean ) : BOOL;
    var
      MaskDC       : HDC;
      OldBitmap    : HBITMAP;
      OldOffScrBmp : HBITMAP;
      OldForeColor : integer;
      OldBackColor : integer;
      OffScrBitmap : HBITMAP;
      OffScrDC     : HDC;
    const
      ROP_DstCopy = $00AA0029;
    begin
      Result := true;
      MaskDC := GetBitmapDC( Mask, OldBitmap );
      try
        if (Win32Platform = VER_PLATFORM_WIN32_NT) and (SrcWidth = DstWidth) and (SrcHeight = DstHeight)
          then MaskBlt( DstDC, DstX, DstY, DstWidth, DstHeight, SrcDC, SrcX, SrcY, Mask, SrcX, SrcY, MakeRop4( ROP_DstCopy, SRCCOPY ) )
          else
            if SourceMasked
              then
                begin
                  SetGdiColorsEx( DstDC, clBlack, clWhite, OldForeColor, OldBackColor );
                  try
                    StretchBlt( DstDC, DstX, DstY, DstWidth, DstHeight, MaskDC, SrcX, SrcY, SrcWidth, SrcHeight, SRCAND );
                    StretchBlt( DstDC, DstX, DstY, DstWidth, DstHeight, SrcDC, SrcX, SrcY, SrcWidth, SrcHeight, SRCPAINT );
                  finally
                    SetGdiColors( DstDC, OldForeColor, OldBackColor );
                  end;
                end
              else
                begin
                  OffScrBitmap := CreateCompatibleBitmap( SrcDC, SrcWidth, SrcHeight );
                  OffScrDC     := GetBitmapDC( OffScrBitmap, OldOffScrBmp );
                  try
                    SetGdiColors( OffScrDC, clWhite, clBlack );
                    StretchBlt( OffScrDC, 0, 0, SrcWidth, SrcHeight, SrcDC, SrcX, SrcY, SrcWidth, SrcHeight, SRCCOPY );
                    StretchBlt( OffScrDC, 0, 0, SrcWidth, SrcHeight, MaskDC, SrcX, SrcY, SrcWidth, SrcHeight, SRCAND );
                    try
                      SetGdiColorsEx( DstDC, clBlack, clWhite, OldForeColor, OldBackColor );
                      StretchBlt( DstDC, DstX, DstY, DstWidth, DstHeight, MaskDC, SrcX, SrcY, SrcWidth, SrcHeight, SRCAND );
                      StretchBlt( DstDC, DstX, DstY, DstWidth, DstHeight, OffScrDC, 0, 0, SrcWidth, SrcHeight, SRCPAINT );

                    finally
                      SetGdiColors( DstDC, OldForeColor, OldBackColor );
                    end;
                  finally
                    ReleaseBitmapDC( OffScrDC, OldOffScrBmp );
                    DeleteObject( OffScrBitmap );
                  end;
                end;
      finally
        ReleaseBitmapDC( MaskDC, OldBitmap );
      end;
    end;

  // TCanvasedBitmap =====================================================================

  constructor TCanvasedBitmap.Create;
    begin
      inherited;
      fStretchBltMode  := STRETCH_DELETESCANS;
    end;

  destructor TCanvasedBitmap.Destroy;
    begin
      FreeObject( fCanvas );
      inherited;
    end;

  procedure TCanvasedBitmap.FreeContext;
    begin
      if Assigned( fCanvas )
        then fCanvas.FreeContext;
    end;

  function TCanvasedBitmap.GetPixels( x, y : integer ) : TColor;
    begin
      Result := Canvas.GetPixels( x, y );
    end;

  procedure TCanvasedBitmap.SetPixels( x, y : integer; Value : TColor );
    begin
      Canvas.SetPixels( x, y, Value );
    end;

  // Draw on VCL canvas methods --------------------

  procedure TCanvasedBitmap.Tile( aCanvas : TCanvas; x, y : integer; const ClippingRect : TRect );
    begin
      TileOnDC( aCanvas.Handle, x, y, ClippingRect, aCanvas.CopyMode );
    end;

  procedure TCanvasedBitmap.Draw( aCanvas : TCanvas; x, y : integer );
    begin
      DrawOnDC( aCanvas.Handle, x, y, aCanvas.CopyMode );
    end;

  procedure TCanvasedBitmap.StretchDraw( aCanvas : TCanvas; const Rect : TRect );
    begin
      StretchDrawOnDC( aCanvas.Handle, Rect, aCanvas.CopyMode );
    end;

  procedure TCanvasedBitmap.ClipDraw( aCanvas : TCanvas; x, y : integer; const ClippingRect : TRect );
    begin
      ClipDrawOnDC( aCanvas.Handle, x, y, ClippingRect, aCanvas.CopyMode );
    end;

  procedure TCanvasedBitmap.ClipStretchDraw( aCanvas : TCanvas; const Rect, ClippingRect : TRect );
    begin
      ClipStretchDrawOnDC( aCanvas.Handle, Rect, ClippingRect, aCanvas.CopyMode );
    end;

  // TBufferCanvas ======================================================================

  constructor TBufferCanvas.Create( aBitmap : TCanvasedBitmap );
    begin
      inherited Create;

      fFont           := TFont.Create;
      fFont.OnChange  := FontChanged;
      fPen            := TPen.Create;
      fPen.OnChange   := PenChanged;
      fBrush          := TBrush.Create;
      fBrush.OnChange := BrushChanged;
      fBitmap         := aBitmap;
      fCopyMode       := cmSrcCopy;
      State           := [];
    end;

  destructor TBufferCanvas.Destroy;
    begin
      FreeContext;
      fFont.Free;
      fPen.Free;
      fBrush.Free;

      inherited;
    end;

  procedure TBufferCanvas.Assign( Source : TPersistent );
    begin
      if Source is TBufferCanvas
        then
          with Source do
          begin
            Self.fCopyMode := fCopyMode;
            Self.fPen.Assign( fPen );
            Self.fFont.Assign( fFont );
            Self.fBrush.Assign( fBrush );
          end
        else
          if Source is TCanvas
            then
              begin
                Self.fCopyMode := fCopyMode;
                Self.fPen.Assign( fPen );
                Self.fFont.Assign( fFont );
                Self.fBrush.Assign( fBrush );
              end
            else inherited;
    end;

  procedure TBufferCanvas.Arc( x1, y1, x2, y2, X3, Y3, X4, Y4 : integer );
    begin
      RequiredState( [csHandleValid, csPenValid] );
      Windows.Arc( fHandle, x1, y1, x2, y2, X3, Y3, X4, Y4 );
    end;

  procedure TBufferCanvas.Chord( x1, y1, x2, y2, X3, Y3, X4, Y4 : integer );
    begin
      RequiredState( [csHandleValid, csPenValid, csBrushValid] );
      Windows.Chord( fHandle, x1, y1, x2, y2, X3, Y3, X4, Y4 );
    end;

  procedure TBufferCanvas.CopyRect( const Dest : TRect; Canvas : TBufferCanvas; const Source : TRect );
    begin
      RequiredState( [csHandleValid] );
      Canvas.RequiredState( [csHandleValid] );
      StretchBlt( fHandle, Dest.Left, Dest.Top, Dest.Right - Dest.Left, Dest.Bottom - Dest.Top,
                  Canvas.fHandle, Source.Left, Source.Top, Source.Right - Source.Left, Source.Bottom - Source.Top,
                  CopyMode );
    end;

  {$WARNINGS OFF}
  procedure DrawGraphic( Graphic : TGraphic; Rect : TRect; aCanvas : TBufferCanvas );
    var
      MetaPalette : HPALETTE;
      OldPalette  : HPALETTE;
      ChgPalette  : boolean;
      dc          : HDC;
      R           : TRect;
    begin
      if Graphic is TBitmap
        then
          with TBitmap( Graphic ), Rect, aCanvas do
            begin
              assert( Assigned(aCanvas) and (not Graphic.Empty), 'Null Graphic in Buffer.DrawGraphic' );

              dc := Canvas.Handle;
              with fBitmap do
                begin
                  SetStretchBltMode( dc, StretchBltMode );
                  if StretchBltMode = STRETCH_HALFTONE
                    then SetBrushOrgEx( dc, 0, 0, nil );
                end;

              ChgPalette := (not IgnorePalette) and (Palette <> 0);
              if ChgPalette
                then
                  begin
                    OldPalette := SelectPalette( fHandle, Palette, false );
                    RealizePalette( fHandle );
                  end;
              try
                if Transparent
                  then
                    if GdiExtAvailable
                      then TransparentBlt( fHandle, Left, Top, Right - Left, Bottom - Top, dc, 0, 0, Width, Height, TransparentColor )
                      else TransparentStretchBlt( fHandle, Left, Top, Right - Left, Bottom - Top, dc, 0, 0, Width, Height, MaskHandle, false )
                  else StretchBlt( fHandle, Left, Top, Right - Left, Bottom - Top, dc, 0, 0, Width, Height, CopyMode );
              finally
                if ChgPalette
                  then SelectPalette( fHandle, OldPalette, false );
              end;
            end
        else
          if Graphic is TIcon
            then
              with Rect.TopLeft do
                DrawIcon( aCanvas.fHandle, x, y, TIcon(Graphic).Handle )
            else
              if Graphic is TMetafile
                then
                  with aCanvas, TMetafile( Graphic ) do
                    begin
                      MetaPalette := Palette;
                      OldPalette  := 0;
                      if MetaPalette <> 0
                        then
                          begin
                            OldPalette := SelectPalette( fHandle, MetaPalette, true );
                            RealizePalette( fHandle );
                          end;
                      R := Rect;
                      Dec( R.Right );  // Metafile rect includes right and bottom coords
                      Dec( R.Bottom );
                      PlayEnhMetaFile( fHandle, Handle, R);
                      if MetaPalette <> 0
                        then SelectPalette( fHandle, OldPalette, true );
                    end;
    end;
  {$WARNINGS ON}

  procedure TBufferCanvas.Draw( x, y : integer; Graphic : TGraphic );
    begin
      assert( Assigned(Graphic) and not Graphic.Empty, 'Empty graphic in Buffer.TSpeedCanvas.Draw!!' );

      RequiredState( csAllValid );
      SetBkColor( fHandle, ColorToRGB( fBrush.Color ) );
      SetTextColor( fHandle, ColorToRGB( fFont.Color ) );
      DrawGraphic( Graphic, Rect( x, y, x + Graphic.Width, y + Graphic.Height), Self );
    end;

  procedure TBufferCanvas.StretchDraw( const Rect : TRect; Graphic : TGraphic );
    begin
      assert( Assigned( Graphic ) and not Graphic.Empty, 'Empty graphic in Buffer.TSpeedCanvas.StretchDraw!!' );

      RequiredState( csAllValid );
      DrawGraphic( Graphic, Rect, Self );
    end;

  procedure TBufferCanvas.DrawFocusRect( const Rect : TRect );
    begin
      RequiredState( [csHandleValid, csBrushValid] );
      Windows.DrawFocusRect( fHandle, Rect );
    end;

  procedure TBufferCanvas.Ellipse( x1, y1, x2, y2 : integer );
    begin
      RequiredState( [csHandleValid, csPenValid, csBrushValid] );
      Windows.Ellipse( fHandle, x1, y1, x2, y2 );
    end;

  procedure TBufferCanvas.FillRect( const Rect : TRect );
    begin
      RequiredState( [csHandleValid, csBrushValid] );
      Windows.FillRect( fHandle, Rect, Brush.Handle );
    end;

  procedure TBufferCanvas.FillRgn( Region : HRGN; aBrush : HBRUSH );
    begin
      RequiredState( [csHandleValid, csBrushValid] );
      Windows.FillRgn( fHandle, Region, aBrush );
    end;

  procedure TBufferCanvas.PaintRgn( Region : HRGN );
    begin
      RequiredState( [csHandleValid, csBrushValid] );
      Windows.PaintRgn( fHandle, Region );
    end;

  procedure TBufferCanvas.FloodFill( x, y : integer; Color : TColor; FillStyle : TFillStyle );
    const
      FillStyles : array[TFillStyle] of word = ( FLOODFILLSURFACE, FLOODFILLBORDER );
    begin
      RequiredState( [csHandleValid, csBrushValid] );
      Windows.ExtFloodFill( fHandle, x, y, Color, FillStyles[FillStyle] );
    end;

  procedure TBufferCanvas.FrameRect( const Rect : TRect );
    begin
      RequiredState( [csHandleValid, csBrushValid] );
      Windows.FrameRect( fHandle, Rect, Brush.Handle );
    end;

  procedure TBufferCanvas.FrameRgn( Region : HRGN; FrameWidth, FrameHeight : integer );
    begin
      RequiredState( [csHandleValid, csBrushValid] );
      Windows.FrameRgn( fHandle, Region, Brush.Handle, FrameWidth, FrameHeight );
    end;

  procedure TBufferCanvas.LineTo( x, y : integer );
    begin
      RequiredState( [csHandleValid, csPenValid] );
      Windows.LineTo( fHandle, x, y );
    end;

  procedure TBufferCanvas.MoveTo( x, y : integer );
    begin
      RequiredState( [csHandleValid] );
      Windows.MoveToEx( fHandle, x, y, nil );
    end;

  procedure TBufferCanvas.Pie( x1, y1, x2, y2, X3, Y3, X4, Y4 : integer );
    begin
      RequiredState( [csHandleValid, csPenValid, csBrushValid] );
      Windows.Pie( fHandle, x1, y1, x2, y2, X3, Y3, X4, Y4 );
    end;

  type
    PPoints = ^TPoints;
    TPoints = array[0..0] of TPoint;

  procedure TBufferCanvas.Polygon( const Points : array of TPoint );
    begin
      RequiredState( [csHandleValid, csPenValid, csBrushValid] );
      Windows.Polygon( fHandle, PPoints( @Points)^, High( Points) + 1 );
    end;

  procedure TBufferCanvas.Polyline( const Points : array of TPoint );
    begin
      RequiredState( [csHandleValid, csPenValid, csBrushValid] );
      Windows.Polyline( fHandle, PPoints( @Points)^, High( Points) + 1 );
    end;

  procedure TBufferCanvas.Rectangle( x1, y1, x2, y2 : integer );
    begin
      RequiredState( [csHandleValid, csBrushValid, csPenValid] );
      Windows.Rectangle( fHandle, x1, y1, x2, y2 );
    end;

  procedure TBufferCanvas.Refresh;
    begin
      DeselectHandles;
    end;

  procedure TBufferCanvas.RoundRect( x1, y1, x2, y2, X3, Y3 : integer );
    begin
      RequiredState( [csHandleValid, csBrushValid, csPenValid] );
      Windows.RoundRect( fHandle, x1, y1, x2, y2, X3, Y3 );
    end;

  procedure TBufferCanvas.TextOut( x, y : integer; const Text : string );
    begin
      RequiredState( [csHandleValid, csFontValid, csBrushValid] );
      Windows.TextOut( fHandle, x, y, pchar(Text), Length( Text) );
      Windows.MoveToEx( fHandle, x + TextWidth( Text), y, nil );
    end;

  procedure TBufferCanvas.TextRect( const Rect : TRect; x, y : integer; const Text : string );
    var
      Options : integer;
    begin
      RequiredState( [csHandleValid, csFontValid, csBrushValid] );
      Options := ETO_CLIPPED;
      if Brush.Style <> bsClear
         then Inc( Options, ETO_OPAQUE );
      Windows.ExtTextOut( fHandle, x, y, Options, @Rect, pchar(Text), length(Text), nil );
    end;

  function TBufferCanvas.TextWidth( const Text : String ) : integer;
    var
      Extent : TSize;
    begin
      RequiredState( [csHandleValid, csFontValid] );
      if Windows.GetTextExtentPoint( fHandle, pchar( Text), Length( Text), Extent)
        then Result := Extent.cX
        else Result := 0;
    end;

  function TBufferCanvas.TextHeight( const Text : String ) : integer;
    var
      Extent : TSize;
    begin
      RequiredState( [csHandleValid, csFontValid] );
      if Windows.GetTextExtentPoint( fHandle, pchar( Text), Length( Text), Extent)
        then Result := Extent.cY
        else Result := 0;
    end;

  function TBufferCanvas.TextExtent( const Text : string ) : TSize;
    begin
      RequiredState( [csHandleValid, csFontValid] );
      Windows.GetTextExtentPoint( fHandle, pchar( Text), Length( Text), result )
    end;

  procedure TBufferCanvas.SetFont( Value : TFont );
    begin
      fFont.Assign( Value );
    end;

  procedure TBufferCanvas.SetPen( Value : TPen );
    begin
      fPen.Assign( Value );
    end;

  procedure TBufferCanvas.SetBrush( Value : TBrush );
    begin
      fBrush.Assign( Value );
    end;

  function TBufferCanvas.GetPenPos : TPoint;
    begin
      RequiredState( [csHandleValid] );
      Windows.GetCurrentPositionEx( fHandle, @Result );
    end;

  procedure TBufferCanvas.SetPenPos( const Value : TPoint );
    begin
      MoveTo( Value.x, Value.y );
    end;

  function TBufferCanvas.GetScanLines : pointer;
    begin
      Result := fBitmap.ScanLines;
    end;

  function TBufferCanvas.GetPixelAddr(x, y : integer ) : pointer;
    begin
      Result := fBitmap.GetPixelAddr( x, y );
    end;

  function TBufferCanvas.GetPixels( x, y : integer ) : TColor;
    begin
      RequiredState( [csHandleValid] );
      Result := Windows.GetPixel( fHandle, x, y );
    end;

  procedure TBufferCanvas.SetPixels( x, y : integer; Value : TColor );
    begin
      RequiredState( [csHandleValid, csPenValid] );
      Windows.SetPixel( fHandle, x, y, ColorToRGB( Value) );
    end;

  function TBufferCanvas.GetClipRect : TRect;
    begin
      RequiredState( [csHandleValid] );
      Windows.GetClipBox( fHandle, Result );
    end;

  function TBufferCanvas.GetHandle : HDC;
    begin
      RequiredState( csAllValid );
      Result := fHandle;
    end;

  procedure TBufferCanvas.DeselectHandles;
    begin
      if (fHandle <> 0) and (State - [csPenValid, csBrushValid, csFontValid] <> State)
        then
          begin
            SelectObject( fHandle, fStockPen );
            SelectObject( fHandle, fStockBrush );
            SelectObject( fHandle, fStockFont );
            State := State - [csPenValid, csBrushValid, csFontValid];
          end;
    end;

  procedure TBufferCanvas.FreeContext;
    var
      dc : HDC;
    begin
      if fHandle <> 0
        then
          begin
            dc := fHandle;
            SetHandle( 0 );
            DeleteDC( dc );
            BitmapCanvasList.Remove( Self );
          end;
    end;

  procedure TBufferCanvas.SetHandle( Value : HDC );
    begin
      if fHandle <> Value
        then
          begin
            if fHandle <> 0
              then
                begin
                  DeselectHandles;
                  fPenPos := GetPenPos;
                  fHandle := 0;
                  Exclude( State, csHandleValid );
                end;
            if Value <> 0
              then
                begin
                  Include( State, csHandleValid );
                  fHandle := Value;
                  SetPenPos( fPenPos );
                end;
          end;
    end;

  procedure TBufferCanvas.RequiredState( ReqState : TCanvasState );
    var
      NeededState : TCanvasState;
    begin
      NeededState := ReqState - State;
      if NeededState <> []
        then
          begin
            if csHandleValid in NeededState
              then
                begin
                  CreateHandle;
                  if fHandle = 0
                    then raise EInvalidGraphicOperation.Create( SNoCanvasHandle );
                end;
            if csFontValid in NeededState
              then CreateFont;
            if csPenValid in NeededState
              then CreatePen;
            if csBrushValid in NeededState
              then CreateBrush;
            State := State + NeededState;
          end;
    end;

  procedure TBufferCanvas.CreateFont;
    begin
      SelectObject( fHandle, Font.Handle );
      SetTextColor( fHandle, ColorToRGB( Font.Color) );
    end;

  procedure TBufferCanvas.CreatePen;
    const
      PenModes : array[TPenMode] of word =
        (
          R2_BLACK,       R2_WHITE,      R2_NOP,         R2_NOT,         R2_COPYPEN,    R2_NOTCOPYPEN,
          R2_MERGEPENNOT, R2_MASKPENNOT, R2_MERGENOTPEN, R2_MASKNOTPEN,  R2_MERGEPEN,   R2_NOTMERGEPEN,
          R2_MASKPEN,     R2_NOTMASKPEN, R2_XORPEN,      R2_NOTXORPEN
        );
    begin
      SelectObject( fHandle, Pen.Handle );
      SetROP2( fHandle, PenModes[Pen.Mode] );
    end;

  procedure TBufferCanvas.CreateBrush;
    begin
      UnrealizeObject( Brush.Handle );
      SelectObject( fHandle, Brush.Handle );
      if Brush.Style = bsSolid
        then
          begin
            SetBkColor( fHandle, ColorToRGB( Brush.Color) );
            SetBkMode( fHandle, OPAQUE );
          end
        else
          begin
            // Win95 doesn't draw brush hatches if bkcolor = brush color
            // Since bkmode is transparent, nothing should use bkcolor anyway
            SetBkColor( fHandle, not ColorToRGB( Brush.Color) );
            SetBkMode( fHandle, TRANSPARENT );
          end;
    end;

  procedure TBufferCanvas.FontChanged( AFont : TObject );
    begin
      if csFontValid in State
        then
          begin
            Exclude( State, csFontValid );
            SelectObject( fHandle, fStockFont );
          end;
    end;

  procedure TBufferCanvas.PenChanged( APen : TObject );
    begin
      if csPenValid in State
        then
          begin
            Exclude( State, csPenValid );
            SelectObject( fHandle, fStockPen );
          end;
    end;

  procedure TBufferCanvas.BrushChanged( ABrush : TObject );
    begin
      if csBrushValid in State
        then
          begin
            Exclude( State, csBrushValid );
            SelectObject( fHandle, fStockBrush );
          end;
    end;

initialization
  BitmapCanvasList := TList.Create;

finalization
  BitmapCanvasList.Free;

end.
