unit FloatingLogo;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
    GDI, ColorSpaces, Buffer, Dibs, BitBlt, ObjUtils,
    Flics, FliPlayer, TimerUtils;

  type
    TFloatingLogo =
      class( TCustomControl )
        published
          property Align;
          property Enabled;
          property ParentShowHint;
          property ShowHint;
          property Visible;

        public
          property Canvas;

          constructor Create( anOwner : TComponent );                                            override;
          destructor  Destroy;                                                                   override;
          procedure   TintColors( Indx, Count : integer; TintColor, BaseColor : TColor );        virtual;

        protected
          fAnimated       : boolean;
          fOriginalBitmap : TSpeedBitmap;
          fFliPlayer      : TFliPlayer;
          fBackground     : TSpeedBitmap;
          fBitmap         : TSpeedBitmap;
          fBuffer         : TSpeedBitmap;
          fTicker         : TTicker;
          WndProcData     : integer;

          procedure WMNCPaint( WinHandle : HWND; Msg : integer; wPar : WPARAM; lPar : LPARAM );
          procedure WMEraseBkgnd( var Message : TWMEraseBkgnd );                                 message WM_ERASEBKGND;
          procedure WMPaint( var Message : TWMPaint );                                           message WM_PAINT;

          procedure CreateParams( var Params : TCreateParams );                                  override;
          procedure SetParent( aParent : TWinControl );                                          override;

          procedure ClipPaint( dc : HDC; x, y : integer; Rect : TRect; ScrCapture : boolean );
          procedure BitmapChanged( Sender : TObject );

          procedure LoadFromStream( aStream : TStream );                                         virtual;
          procedure LoadFromFile( const Filename : string );                                     virtual;

        protected
          procedure TimerTick;
          procedure ReserveTimer;
          procedure NextFrame;

          procedure InitPlayer;
          procedure InitBitmap;
          procedure UpdateBitmap;

        protected
          function  GetPalette : HPALETTE;                                                       override;
          function  Empty : boolean;

        protected
          fFilename         : string;
          fShadowDistance   : TPoint;
          fBitCount         : integer;
          fCastShadow       : boolean;
          fBackgroundFixed  : boolean;
          fPaused           : boolean;
          fBackgroundOK     : boolean;

          procedure SetShadowDistance( Value : TPoint );

        protected
          procedure SetStartingFrame( aStartingFrame : integer );                                virtual;
          procedure SetEndingFrame( aEndingFrame : integer );                                    virtual;
          function  GetStartingFrame : integer;                                                  virtual;
          function  GetEndingFrame : integer;                                                    virtual;

        published
          property Filename : string          read fFilename         write LoadFromFile;
          property ShadowDistance : TPoint    read fShadowDistance   write SetShadowDistance;
          property CastShadow : boolean       read fCastShadow       write fCastShadow           default false;
          property Paused : boolean           read fPaused                                       default false;
          property Animated : boolean         read fAnimated                                     default false;
          property StartingFrame : integer    read GetStartingFrame  write SetStartingFrame;
          property EndingFrame   : integer    read GetEndingFrame    write SetEndingFrame;

        public
          procedure SetFrameRange( aStartingFrame, aEndingFrame : integer );                     virtual;
          procedure ResetFrameRange;                                                             virtual;

        public
          property BackgroundFixed : boolean  read fBackgroundFixed  write fBackgroundFixed      default false;

          procedure Pause;
          procedure Resume;

          procedure SetPalette( var RgbPalette );                                                virtual;
      end;

    procedure Register;

implementation

  uses
    Rects, StreamUtils,
    WinUtils, TransparentWindows;

  // TFloatingLogo stuff

  procedure TFloatingLogo.CreateParams( var Params : TCreateParams);
    begin
      inherited;
      if not (csDesigning in ComponentState)
        then
          with Params do
            ExStyle := ExStyle or WS_EX_TRANSPARENT;
    end;

  procedure TFloatingLogo.WMEraseBkgnd( var Message : TWMEraseBkgnd );
    begin
      if fOriginalBitmap = nil
        then inherited
        else Message.Result := 1;
    end;

  constructor TFloatingLogo.Create( anOwner : TComponent );
    begin
      inherited;

      fShadowDistance := Point( 10, 10 );
      fCastShadow     := true;
      Width           := 50;
      Height          := 50;
      fFliPlayer      := TFliPlayer.Create;
    end;

  destructor TFloatingLogo.Destroy;
    begin
      fOriginalBitmap.Free;
      fBitmap.Free;
      fBuffer.Free;
      fBackground.Free;

      inherited;
    end;

  procedure TFloatingLogo.SetShadowDistance( Value : TPoint );
    begin
      fShadowDistance := Value;

      BitmapChanged( Self );
    end;

  procedure TFloatingLogo.BitmapChanged( Sender : TObject );
    begin
      if (fOriginalBitmap = nil) or fOriginalBitmap.Empty
        then SetBounds( Left, Top, 50, 50 )
        else
          begin
            FreeObject( fBitmap );
            FreeObject( fBuffer );

            InitBitmap;
            SetBounds( Left, Top, fBuffer.Width, fBuffer.Height );
          end;

      ControlStyle := ControlStyle - [csOpaque];
      Invalidate;
    end;

  function TFloatingLogo.Empty : boolean;
    begin
      Empty := not ( Assigned( Self ) and Assigned( fOriginalBitmap ) );
    end;

  function TFloatingLogo.GetPalette : HPALETTE;
    begin
      {if Assigned( fOriginalBitmap )
        then Result := fOriginalBitmap.Palette
        else} Result := 0;
    end;

  procedure TFloatingLogo.LoadFromStream( aStream : TStream );
var
  TB : TSpeedBitmap;
    begin
      FreeObject( fOriginalBitmap );

      if not (csDesigning in ComponentState)
        then
          try
            fFliPlayer.LoadFromStream( aStream );
            InitPlayer;
            fAnimated := true;
          except
            fOriginalBitmap := LoadBitmapFromStream( aStream );
            aStream.Free;
            fAnimated := false;
          end;
      BitmapChanged( Self );

  TB := TSpeedBitmap.Create;
  TB.LoadFromFile( 'e:\jrg\extra\intro.bmp' );
  SetPalette( TB.RgbEntries^ );

    end;

  procedure TFloatingLogo.SetPalette( var RgbPalette );
    begin
      with fOriginalBitmap do
        begin
          ChangePaletteEntries( 0, DibHeader.biClrUsed, RgbPalette );
          if fBitCount = 8
            then fBuffer.ChangePaletteEntries( 0, DibHeader.biClrUsed, RgbPalette );
        end;
      PaletteChanged( true );
    end;

  procedure TFloatingLogo.LoadFromFile( const Filename : string );
    var
      Stream : TStream;
    begin
      fFilename := Filename;
      Stream    := TFileStream.Create( Filename, fmOpenRead or fmShareDenyWrite );
      LoadFromStream( Stream );
    end;

  procedure TFloatingLogo.TintColors( Indx, Count : integer; TintColor, BaseColor : TColor );
    begin
      with fOriginalBitmap do
        begin
          TintRgbEntries( Indx, Count, RgbEntries^, ColorToRgb( TintColor ), ColorToRgb( BaseColor ) );
          ChangePaletteEntries( 0, DibHeader.biClrUsed, RgbEntries^ );
        end;
      BitmapChanged( Self );
    end;

  procedure TFloatingLogo.ClipPaint( DC : HDC; x, y : integer; Rect : TRect; ScrCapture : boolean );
    var
      LogoRect   : TRect;
      ShadeRect  : TRect;
      SrcPoint   : TPoint;
      StartWithX : boolean;
    begin
      if not IsRectEmpty( Rect )
        then
          begin
            // Get background from screen (if needed)
            if ScrCapture
              then
                begin
                  if not fBackgroundOK
                    then fBackgroundOK  := true;

                  fBackground.ClipSnapshotDC( DC, x, y, Rect, true );
                  if fBitCount = 8
                    then
                      with fBuffer do
                        ChangePaletteEntries( 0, DibHeader.biClrUsed, fBackground.RgbEntries^ );
                end;
            OffsetRect( Rect, -x, -y );

            // Get original background
            fBuffer.ClipSnapshotDC( fBackground.Canvas.Handle, 0, 0, Rect, false );

            // Cast logo's shadows
            if CastShadow
              then
                begin
                  ShadeRect := RectFromBounds( fShadowDistance.x, fShadowDistance.y, fOriginalBitmap.Width, fOriginalBitmap.Height );
                  if IntersectRect( ShadeRect, ShadeRect, Rect )
                    then
                      with ShadeRect, SrcPoint do
                        begin
                          SrcPoint   := Point( Left - fShadowDistance.x, Top - fShadowDistance.y );
                          StartWithX := boolean( ( x and 1 ) and ( y and 1 ) ) or
                                        boolean( ( (x + 1) and 1 ) and ( (y + 1) and 1 ) );
                          if fBitCount = 8
                            then GridShade( fOriginalBitmap.PixelAddr[x, y], fBuffer.PixelAddr[Left, Top], Right - Left, Bottom - Top,
                                            fOriginalBitmap.StorageWidth, fBuffer.StorageWidth, StartWithX )
                            else GridShade24( fBitmap.PixelAddr[x, y], fBuffer.PixelAddr[Left, Top], Right - Left, Bottom - Top,
                                              fBitmap.StorageWidth, fBuffer.StorageWidth, StartWithX );
                        end;
                end;

            // Draw the logo
            LogoRect := RectFromBounds( 0, 0, fOriginalBitmap.Width, fOriginalBitmap.Height );
            if IntersectRect( LogoRect, LogoRect, Rect )
              then
                with LogoRect do
                  begin
                    if fBitCount = 8
                      then TransCopy( fOriginalBitmap.PixelAddr[Left, Top], fBuffer.PixelAddr[Left, Top], Right - Left, Bottom - Top,
                                      fOriginalBitmap.StorageWidth, fBuffer.StorageWidth )
                      else TransCopy24( fBitmap.PixelAddr[Left, Top], fBuffer.PixelAddr[Left, Top], Right - Left, Bottom - Top,
                                        fBitmap.StorageWidth, fBuffer.StorageWidth );
                  end;

            // On screen drawing (finally!)
            OffsetRect( Rect, x, y );
            fBuffer.ClipDrawOnDC( DC, x, y, Rect, Canvas.CopyMode );
          end;
    end;

  procedure TFloatingLogo.WMPaint( var Message : TWMPaint );
    var
      dc : HDC;
      PS : TPaintStruct;
    begin
      if (fOriginalBitmap = nil) or fOriginalBitmap.Empty
        then
          with Canvas do
            begin
              Pen.Style   := psDash;
              Brush.Style := bsSolid;
              Brush.Color := clBtnFace;
              Rectangle( 0, 0, Width, Height );
            end
        else
          begin
            dc := BeginPaint( Handle, PS );
            try
              ClipPaint( dc, 0, 0, PS.rcPaint, not BackgroundFixed );
            finally
              EndPaint( Handle, PS );
            end;
          end;
    end;

  // Non-client area stuff

  var
    CurrentLogo : TFloatingLogo;

  function PaintWndProc( WinHandle : HWND; Msg : integer; wPar : WPARAM; lPar : LPARAM ) : LRESULT; stdcall;
    begin
      Result := CallWindowProc( PrevWndProc( CurrentLogo.WndProcData ), WinHandle, Msg, wPar, lPar );
      if (Msg = WM_NCPAINT) or (Msg = WM_NCACTIVATE)
        then CurrentLogo.WMNCPaint( WinHandle, Msg, wPar, lPar );
    end;

  procedure TFloatingLogo.SetParent( aParent : TWinControl );
    begin
      if Assigned( Parent ) and ( WndProcData <> 0 )
        then RestoreWndProc( Parent.Handle, WndProcData );

      inherited;
      ParentChanged( Self );

      if (Parent <> nil) and ( (Left < 0) or (Top < 0) )
        then
          with Parent as TForm do
            begin
              WndProcData := ChangeWndProc( Handle, @PaintWndProc );
              CurrentLogo := Self;
            end;
    end;

  procedure TFloatingLogo.WMNCPaint( WinHandle : HWND; Msg : integer; wPar : WPARAM; lPar : LPARAM );
    var
      Rect    : TRect;
      WinRect : TRect;
      LogoTop : TPoint;
      dc      : HDC;
    begin
      if Assigned( fOriginalBitmap ) and ( not fOriginalBitmap.Empty )
        then
          begin
            GetWindowRect( WinHandle, Rect );
            with Parent.ClientOrigin do
              begin
                LogoTop.x := (x - Rect.Left) + Left;
                LogoTop.y := (y - Rect.Top) + Top;
                WinRect := RectFromBounds( 0, 0, Parent.Width, y - Rect.Top );
              end;
            dc := GetWindowDC( WinHandle );
            ClipPaint( dc, LogoTop.x, LogoTop.y, WinRect, not BackgroundFixed );
            ReleaseDC( WinHandle, dc );
          end;
    end;

  procedure TFloatingLogo.TimerTick;
    begin
      if (not Empty) and Enabled and (not Paused)
        then NextFrame;
    end;

  procedure TFloatingLogo.ReserveTimer;
    begin
      fTicker := TSimpleTimer.Create;
      with fTicker do
        begin
          PostTicks  := true;
          OnTimer    := TimerTick;
          Resolution := FliTimerResolution div 2;
          Interval   := fFliPlayer.FrameDelay;
          Enabled    := (not Paused) and (not Empty);
        end;
    end;

  procedure TFloatingLogo.NextFrame;
    begin
      CurrentLogo := Self;
      if fBackgroundOK
        then fBackgroundFixed := true;
      
      fFliPlayer.DoFrame;
      UpdateBitmap;
      Invalidate;
    end;

  procedure TFloatingLogo.InitBitmap;
    begin
      if AvailableColors <= 254
        then fBitCount := 8
        else fBitCount := 24;
      with fOriginalBitmap, fShadowDistance do
        begin
          fBackground := TSpeedBitmap.CreateSized( Width + x, -(Height + y), fBitCount );
          fBuffer     := TSpeedBitmap.CreateSized( Width + x, -(Height + y), fBitCount );
          if fBitCount = 24
            then
              begin
                fBitmap := BitmapCopy( fOriginalBitmap, Width, -Height, fBitCount, false );
                fBuffer.IgnorePalette := true;
              end;
        end;

      assert( fOriginalBitmap.BitCount = 8, 'Invalid Bitmap in TFloatingLogo.LoadFromStream!!' );
    end;

  procedure TFloatingLogo.UpdateBitmap;
    begin
      if Assigned( fBitmap )
        then fBitmap.SnapshotDC( fOriginalBitmap.Canvas.Handle, 0, 0, false );
    end;

  procedure TFloatingLogo.InitPlayer;
    begin
      fFliPlayer.Stream.LoadInMemory;
      fFliPlayer.UnkChunkPlayer := nil;

      fOriginalBitmap := TSpeedBitmap.CreateSized( fFliPlayer.Width, -fFliPlayer.Height, 8 );
      with fOriginalBitmap do
        fFliPlayer.AttachToDib( 0, 0, DibHeader, ImageAddr );
      NextFrame;
      ReserveTimer;
    end;

  procedure TFloatingLogo.SetStartingFrame( aStartingFrame : integer );
    begin
      fFliPlayer.StartingFrame := aStartingFrame;
    end;

  procedure TFloatingLogo.SetEndingFrame( aEndingFrame : integer );
    begin
      fFliPlayer.EndingFrame := aEndingFrame;
    end;

  function TFloatingLogo.GetStartingFrame : integer;
    begin
      Result := fFliPlayer.StartingFrame;
    end;

  function TFloatingLogo.GetEndingFrame : integer;
    begin
      Result := fFliPlayer.EndingFrame;
    end;

  procedure TFloatingLogo.SetFrameRange( aStartingFrame, aEndingFrame : integer );
    begin
      StartingFrame := aStartingFrame;
      EndingFrame   := aEndingFrame;
    end;

  procedure TFloatingLogo.ResetFrameRange;
    begin
      StartingFrame := 1;
      EndingFrame   := fFliPlayer.FrameCount;
    end;

  procedure TFloatingLogo.Pause;
    begin
      fTicker.Pause;
    end;

  procedure TFloatingLogo.Resume;
    begin
      fTicker.Resume;
    end;

  // Component registration

  procedure Register;
    begin
      RegisterComponents( 'Merchise', [TFloatingLogo] );
    end;

end.
