unit MainWindow;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    DDraw, Menus, ExtDlgs, StdCtrls;

  type
    TMainForm =
      class(TForm)
          Log: TMemo;
          procedure FormShow(Sender: TObject);
        private
          { Private declarations }
          fDirectDraw     : IDirectDraw4;
          fPrimarySurface : IDirectDrawSurface4;
          fOverlay        : IDirectDrawSurface4;
          //fBackBuffer     : IDirectDrawSurface4;
          //fClipper        : IDirectDrawClipper;
          //fOverlayVisible : boolean;
          //fBitmap         : TBitmap;
          //fBitmapSurface  : IDirectDrawSurface4;
        public
          { Public declarations }
      end;

  var
    MainForm: TMainForm;

implementation

  {$R *.DFM}

  uses
    ComObj, LogFile;

  function CopyBitmapToSurface(Bitmap : TBitmap; const Surface : IDirectDrawSurface4; x, y : integer) : boolean;
    var
      dc    : HDC;
      memdc : HDC;
    begin
      Result := false;
      if Surface.GetDC(dc) = DD_OK
        then
          try
            memdc := CreateCompatibleDC(dc);
            if memdc <> 0
              then
                try
                  SelectObject(memdc, Bitmap.Handle);
                  if not BitBlt(dc, x, y, Bitmap.Width, Bitmap.Height, memdc, 0, 0, SRCCOPY)
                    then LogThis('BitBlt FAILED')
                    else Result := true;
                finally
                  DeleteDC(memdc);
                end
              else LogThis('CreateCompatibleDC FAILED');
          finally
            Surface.ReleaseDC(dc);
          end
        else LogThis('GetDC FAILED');
    end;

  const
    cMaxPixelFormats = 6;

  const
    cPixelFormats : array [0..pred(cMaxPixelFormats)] of TDDPixelFormat =
      (
        (dwSize : sizeof(TDDPixelFormat); dwFlags : DDPF_RGB; dwFourCC : 0; dwRGBBitCount : 16; dwRBitMask : $7C00; dwGBitMask : $03e0; dwBBitMask : $001F; dwRGBAlphaBitMask : 0), // 16-bit RGB 5:5:5
        (dwSize : sizeof(TDDPixelFormat); dwFlags : DDPF_RGB; dwFourCC : 0; dwRGBBitCount : 16; dwRBitMask : $F800; dwGBitMask : $07e0; dwBBitMask : $001F; dwRGBAlphaBitMask : 0), // 16-bit RGB 5:6:5
        (dwSize : sizeof(TDDPixelFormat); dwFlags : DDPF_RGB; dwFourCC : 0; dwRGBBitCount : 24; dwRBitMask : $FF0000; dwGBitMask : $00FF00; dwBBitMask : $0000FF; dwRGBAlphaBitMask : 0), // 24-bit RGB 8:8:8
        (dwSize : sizeof(TDDPixelFormat); dwFlags : DDPF_RGB; dwFourCC : 0; dwRGBBitCount : 24; dwRBitMask : $0000FF; dwGBitMask : $00FF00; dwBBitMask : $FF0000; dwRGBAlphaBitMask : 0), // 24-bit BGR 8:8:8
        (dwSize : sizeof(TDDPixelFormat); dwFlags : DDPF_RGB; dwFourCC : 0; dwRGBBitCount : 32; dwRBitMask : $FF0000; dwGBitMask : $00FF00; dwBBitMask : $0000FF; dwRGBAlphaBitMask : 0), // 32-bit RGB 8:8:8
        (dwSize : sizeof(TDDPixelFormat); dwFlags : DDPF_RGB; dwFourCC : 0; dwRGBBitCount : 32; dwRBitMask : $0000FF; dwGBitMask : $00FF00; dwBBitMask : $FF0000; dwRGBAlphaBitMask : 0) // 32-bit BGR 8:8:8
      );

  const
    cPixelFormatNames : array [0..pred(cMaxPixelFormats)] of string =
      (
        '16-bit RGB 5:5:5',
        '16-bit RGB 5:6:5',
        '24-bit RGB 8:8:8',
        '24-bit BGR 8:8:8',
        '32-bit RGB 8:8:8',
        '32-bit BGR 8:8:8'
      );

  function CreateFlippingOverlay(const DirectDraw : IDirectDraw4; Control : TWinControl; out BackBuffer : IDirectDrawSurface4; pixformat : integer) : IDirectDrawSurface4;
    var
      hRet    : HRESULT;
      ddsd    : TDDSurfaceDesc2;
      ddscaps : TDDSCaps2;
      devcaps : TDDCaps;
      HELcaps : TDDCaps;
    begin
      if DirectDraw <> nil
        then
          if (DirectDraw.GetCaps(@devcaps, @HELCaps) = DD_OK) and ((devcaps.dwCaps and DDCAPS_OVERLAY) <> 0)
            then
              begin
                fillchar(ddsd, sizeof(ddsd), 0);
                ddsd.dwSize := sizeof(ddsd);
                ddsd.dwFlags := DDSD_CAPS or DDSD_HEIGHT or DDSD_WIDTH or DDSD_BACKBUFFERCOUNT or DDSD_PIXELFORMAT;
                ddsd.ddsCaps.dwCaps := DDSCAPS_OVERLAY or DDSCAPS_FLIP or DDSCAPS_COMPLEX or DDSCAPS_VIDEOMEMORY;
	        ddsd.dwWidth := Control.ClientWidth;
	        ddsd.dwHeight := Control.ClientHeight;
                ddsd.dwBackBufferCount := 2;
                ddsd.ddpfPixelFormat := cPixelFormats[pixformat];
                hRet := DirectDraw.CreateSurface(ddsd, Result, nil);
                if hRet <> DD_OK
                  then Result := nil;
                if Result <> nil
                  then
                    begin
                      ddscaps.dwCaps := DDSCAPS_BACKBUFFER;
                      hRet := Result.GetAttachedSurface(ddscaps, BackBuffer);
                      if hRet <> DD_OK
                        then
                          begin
                            LogThis('GetAttachedSurface FAILED');
                            Result := nil;
                            BackBuffer := nil;
                          end;
                    end;
              end
            else Result := nil
        else Result := nil;
    end;

  function CreateOverlay(const DirectDraw : IDirectDraw4; Control : TWinControl; pixformat : integer) : IDirectDrawSurface4;
    var
      hRet    : HRESULT;
      ddsd    : TDDSurfaceDesc2;
      devcaps : TDDCaps;
      HELcaps : TDDCaps;
    begin
      if DirectDraw <> nil
        then
          begin
            fillchar(devcaps, sizeof(devcaps), 0);
            devcaps.dwSize := sizeof(devcaps);
            fillchar(HELCaps, sizeof(HELCaps), 0);
            HELCaps.dwSize := sizeof(HELCaps);
            if DirectDraw.GetCaps(@devcaps, @HELCaps) = DD_OK
              then
                if devcaps.dwCaps and DDCAPS_OVERLAY <> 0
                  then
                    begin
                      fillchar(ddsd, sizeof(ddsd), 0);
                      ddsd.dwSize := sizeof(ddsd);
                      ddsd.dwFlags := DDSD_CAPS or DDSD_HEIGHT or DDSD_WIDTH or DDSD_PIXELFORMAT;
                      ddsd.ddsCaps.dwCaps := DDSCAPS_OVERLAY or DDSCAPS_VIDEOMEMORY;
                      ddsd.dwWidth := Control.ClientWidth;
                      ddsd.dwHeight := Control.ClientHeight;
                      ddsd.ddpfPixelFormat := cPixelFormats[pixformat];
                      hRet := DirectDraw.CreateSurface(ddsd, Result, nil);
                      if hRet <> DD_OK
                        then Result := nil;
                    end
                  else Result := nil
              else Result := nil;
          end
        else Result := nil;
    end;

  function HideOverlay(const PrimarySurface, Overlay : IDirectDrawSurface4) : boolean;
    begin
      Result := Overlay.UpdateOverlay(nil, PrimarySurface, nil, DDOVER_HIDE, nil) = DD_OK;
    end;

  function ShowOverlay(Control : TWinControl; const Overlay, Primary : IDirectDrawSurface4) : boolean;
    var
      SrcRect  : TRect;
      DestRect : TRect;
      ScPt     : TPoint;
    begin
      SrcRect := Rect(0, 0, pred(Control.ClientWidth), pred(Control.ClientHeight));
      DestRect := SrcRect;
      ScPt := Control.ClientToScreen(Point(0, 0));
      OffsetRect(DestRect, ScPt.x, ScPt.y);
      // fix rects to account for strecth factors as well as size and alignment restrictions
      if Overlay.UpdateOverlay(@SrcRect, Primary, @DestRect, DDOVER_SHOW, nil) <> DD_OK
        then
          begin
            LogThis('UpdateOverlay FAILED');
            Result := false;
          end
        else Result := true;
    end;

  function ResizeOverlay(const DirectDraw : IDirectDraw4; Control : TWinControl; const PrimarySurface : IDirectDrawSurface4; var Overlay : IDirectDrawSurface4) : boolean;
    begin
      //HideOverlay(PrimarySurface, Overlay);
      ShowOverlay(Control, Overlay, PrimarySurface);
      Result := Overlay <> nil;
    end;

  function MoveOverlay(Control : TWinControl; const Overlay : IDirectDrawSurface4) : boolean;
    var
      ScPt : TPoint;
    begin
      ScPt := Control.ClientToScreen(Point(0, 0));
      // adjust position accounting for destination alignment restrictions
      Result := Overlay.SetOverlayPosition(ScPt.x, ScPt.y) = DD_OK;
    end;

  function CreateClipper(const DirectDraw : IDirectDraw4; Control : TWinControl) : IDirectDrawClipper;
    var
      hRet : HRESULT;
    begin
      if DirectDraw <> nil
        then
          begin
            hRet := DirectDraw.CreateClipper(0, Result, nil);
            if hRet <> DD_OK
              then Result := nil;
          end
        else Result := nil;
    end;

  function FillSurface(const Surface : IDirectDrawSurface4; rgbColor : integer) : boolean;
    var
      DDBltFx : TDDBltFx;
    begin
      fillchar(DDBltFx, sizeof(DDBltFx), 0);
      DDBltFx.dwSize := sizeof(DDBltFx);
      DDBltFx.dwFillColor := rgbColor;
      Result := Surface.Blt(nil, nil, nil, DDBLT_COLORFILL, @DDBltFx) = DD_OK;
    end;

  procedure TMainForm.FormShow(Sender: TObject);
    var
      ddsd    : TDDSurfaceDesc2;
      //ddscaps : TDDSCAPS2;
      hRet    : HRESULT;
      pDD     : IDirectDraw;
      i       : integer;
    begin
      hRet := DirectDrawCreate(nil, pDD, nil);
      if hRet <> DD_OK
        then LogThis('DirectDrawCreate FAILED')
        else
          begin
            hRet := pDD.QueryInterface(IID_IDirectDraw4, fDirectDraw);
            if hRet <> DD_OK
              then LogThis('QueryInterface FAILED')
              else
                begin
                  hRet := fDirectDraw.SetCooperativeLevel(Handle, DDSCL_NORMAL);
                  if hRet <> DD_OK
                    then LogThis('SetCooperativeLevel FAILED')
                    else
                      begin
                        fillchar(ddsd, sizeof(ddsd), 0);
                        ddsd.dwSize := sizeof(ddsd);
                        ddsd.dwFlags := DDSD_CAPS;
                        ddsd.ddsCaps.dwCaps := DDSCAPS_PRIMARYSURFACE;
                        hRet := fDirectDraw.CreateSurface(ddsd, fPrimarySurface, nil);
                        if hRet <> DD_OK
                          then LogThis('CreateSurface FAILED');
                      end;
                end;
          end;
      for i := 0 to pred(cMaxPixelFormats) do
        begin
          fOverlay := CreateOverlay(fDirectDraw, Self, i);
          if fOverlay <> nil
            then
              begin
                Log.Lines.Add(cPixelFormatNames[i] + ' overlay successfully created');
                fOverlay := nil;
              end
            else Log.Lines.Add(cPixelFormatNames[i] + ' overlay could not be created');
        end;
    end;

initialization
  SetLogFile(ExtractFileName(Application.ExeName) + '.log');
end.
