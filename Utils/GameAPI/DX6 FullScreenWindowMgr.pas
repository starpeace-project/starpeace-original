unit FullScreenWindowMgr;

interface

  uses
    Windows, DDraw, Classes;    

  type
    TFullScreenWindowManager =
      class
        public
          constructor Create(hwndApp : HWND; const dd : IDirectDraw4; const FrontBuffer, BackBuffer : IDirectDrawSurface4);
          destructor  Destroy; override;
        public
          procedure RegisterWindow(Wnd : HWND; static : boolean; zorder : integer);
          procedure UnregisterWindow(Wnd : HWND);
          function  UpdateScreen(const DirtyRects : array of TRect) : boolean;
          function  IsAnyWindowShowing : boolean;
        private
          fDirectDraw       : IDirectDraw4;
          fFrontBuffer      : IDirectDrawSurface4;
          fBackBuffer       : IDirectDrawSurface4;
          fClipper          : IDirectDrawClipper;
          fMainWindow       : HWND;
          fWindows          : TList;
          fNoGDIHardSupport : boolean;
          fSurfaceSize      : TPoint;
          fSurfaceBpp       : integer;
          fMouseCursor      : HCURSOR;
          fIconInfo         : TIconInfo;
      end;

implementation

  uses
    AxlDebug;

  type
    PWindowData = ^TWindowData;
    TWindowData =
      record
        handle : HWND;
        static : boolean;
        zorder : integer;
        bitmap : HBITMAP;
      end;

  //-------------------------------------------------------------------------------
  // Name: CreateDibBMP()
  // Desc: Creates an empty bitmap, used exclusively in CreateBMPFromWindow().
  //       Note that this is an internal (not exported) function.
  //-------------------------------------------------------------------------------

  function CreateDibBMP(dc : HDC; w, h : integer; bpp : word) : HBITMAP;
    type
      TDib =
        record
          bi : TBitmapInfoHeader;
          ct : array [0..255] of dword;
        end;
    var
      lpBits : pointer;
      dib    : TDib;
    begin
      dib.bi.biSize := sizeof(TBitmapInfoHeader);
      dib.bi.biWidth := w;
      dib.bi.biHeight := h;
      dib.bi.biBitCount := bpp;
      dib.bi.biPlanes := 1;
      dib.bi.biCompression := 0;
      dib.bi.biSizeImage := 0;
      dib.bi.biClrUsed := 0;

      if bpp = 15
        then dib.bi.biBitCount := 16
        else
          if bpp = 16
            then
              begin
                dib.bi.biCompression := BI_BITFIELDS;
                dib.ct[0] := $F800;
                dib.ct[1] := $07E0;
                dib.ct[2] := $001F;
              end;

      Result := CreateDIBSection(dc, PBitmapInfo(@dib)^, DIB_RGB_COLORS, lpBits, 0, 0);
    end;

  //-------------------------------------------------------------------------------
  // Name: CreateBMPFromWindow()
  // Desc: Takes the hwnd of the content window, and returns a bitmap handle.
  //       Note that this is an internal (not exported) function.
  //-------------------------------------------------------------------------------

  function CreateBMPFromWindow(wnd : HWND; bpp : integer) : HBITMAP;
    var
      rc        : TRect;
      x         : integer;
      y         : integer;
      cx        : integer;
      cy        : integer;
      hdcScreen : HDC;
      hdcMemory : HDC;
      hbmBitmap : HBITMAP;
    begin
      // Create a bitmap of the window passed in
      GetWindowRect(wnd, rc);
      x := rc.left;
      y := rc.top;
      cx := rc.right - rc.left;
      cy := rc.bottom - rc.top;
      hdcScreen := GetDC(0);
      hdcMemory := CreateCompatibleDC(0);
      hbmBitmap := CreateDibBMP(hdcScreen, cx, cy, bpp);

      // BLT the image from screen to bitmap
      SelectObject(hdcMemory, hbmBitmap);
      BitBlt(hdcMemory, 0, 0, cx, cy, hdcScreen, x, y, SRCCOPY);
      DeleteDC(hdcMemory);
      ReleaseDC(0, hdcScreen);

      Result := hbmBitmap;
    end;

  //-------------------------------------------------------------------------------
  // Name: Create (hwndApp : HWND; const dd : IDirectDraw7; const FrontBuffer,
  //            BackBuffer : IDirectDrawSurface7);
  // Desc: Does preliminary setup of global values for FSWindow. It should get
  //       called each time DirectDraw surfaces are altered (i.e. changes to the
  //       device that the client application is running under).
  //-------------------------------------------------------------------------------

  constructor TFullScreenWindowManager.Create(hwndApp : HWND; const dd : IDirectDraw4; const FrontBuffer, BackBuffer : IDirectDrawSurface4);
    var
      ddsd   : TDDSurfaceDesc2;
      ddcaps : TDDCaps;
    begin
      // Save handle to application window
      fMainWindow := hwndApp;

      ZeroMemory(@ddcaps, sizeof(ddcaps));
      ddcaps.dwSize := sizeof(ddcaps);
      dd.GetCaps(@ddcaps, nil);
      if (ddcaps.dwCaps2 and DDCAPS2_CANRENDERWINDOWED) <> 0
        then fNoGDIHardSupport := false
        else fNoGDIHardSupport := true;

      // Save DirectDraw object passed in
      fDirectDraw := dd;

      // Save buffers passed in
      fFrontBuffer := FrontBuffer;
      fBackBuffer := BackBuffer;

      // Get DirectDraw surface dimensions
      ZeroMemory(@ddsd, sizeof(ddsd));
      ddsd.dwSize := sizeof(ddsd);
      ddsd.dwFlags := DDSD_HEIGHT or DDSD_WIDTH;
      fBackBuffer.GetSurfaceDesc(ddsd);
      fSurfaceSize.x := ddsd.dwWidth;
      fSurfaceSize.y := ddsd.dwHeight;
      fSurfaceBpp := ddsd.ddpfPixelFormat.dwRGBBitCount;
      fWindows := TList.Create;
    end;

  destructor TFullScreenWindowManager.Destroy;
    begin
      fWindows.Free;
      inherited;
    end;

  //-------------------------------------------------------------------------------
  // Name: RegisterWindow
  // Desc: Prepairs the DirectDraw surface depending on 3D hardware. It should
  //       get called whenever a window (represented by the hwnd parameter) needs
  //       to be displayed under DirectDraw. RegisterWindow should also get
  //       called if the window changes its content (if its static content
  //       becomes dynamic, and vice-versa).
  //-------------------------------------------------------------------------------

  procedure TFullScreenWindowManager.RegisterWindow(wnd : HWND; static : boolean; zorder : integer);
    var
      rc      : TRect;
      wnddata : PWindowData;
    begin
      if wnd <> 0
        then
          begin
            new(wnddata);
            wnddata.handle := wnd;
            wnddata.static := static;
            wnddata.zorder := zorder;
            if fNoGDIHardSupport
              then
                begin
                  // Constrain cursor to DirectDraw surface
                  rc.left := 0;
                  rc.top := 0;
                  rc.right := fSurfaceSize.x;
                  rc.bottom := fSurfaceSize.y;
                  ClipCursor(@rc);

                  // Need to create an image of content window just once
                  if static
                    then
                      begin
                        UpdateWindow(wnd);

                        // Assign content window image to global
                        wnddata.bitmap := CreateBMPFromWindow(wnd, fSurfaceBpp);
                      end;
                end
            else
              if not IsAnyWindowShowing
                then
                  begin
                    // Create a clipper (used in IDirectDrawSurface::Blt call)
                    if fDirectDraw.CreateClipper(0, fClipper, nil) = DD_OK
                      then fClipper.SetHWnd(0, fMainWindow);

                    // Normal GDI device, so just flip to GDI so content window can be seen
                    fDirectDraw.FlipToGDISurface;
                    fFrontBuffer.SetClipper(fClipper);
                  end;
            fWindows.Add(wnddata);
          end;
    end;

  //-------------------------------------------------------------------------------
  // Name: UnregisterWindow
  // Desc: Deletes objects associated with a content window. Note that these
  //       are objects created within this module, not objects created by the
  //       calling client (e.g. content window). Call this function whenever the
  //       content window is destroyed (e.g. WM_CLOSE).
  //-------------------------------------------------------------------------------

  procedure TFullScreenWindowManager.UnregisterWindow(wnd : HWND);
    var
      i       : integer;
      found   : boolean;
      wnddata : PWindowData;
    begin
      i := 0;
      found := false;
      wnddata := nil;
      while not found and (i < fWindows.count) do
        begin
          wnddata := PWindowData(fWindows[i]);
          if wnddata.handle = wnd
            then found := true
            else inc(i);
        end;
      if found
        then
          begin
            if wnddata.bitmap <> 0
              then DeleteObject(wnddata.bitmap);
            dispose(wnddata);
            fWindows.Delete(i);
          end;

      if fWindows.Count = 0
        then
          begin
            if fNoGDIHardSupport
              then ClipCursor(nil);

            // Get rid of clipper object
            if fClipper <> nil
              then fClipper := nil;
          end;
    end;

  //-------------------------------------------------------------------------------
  // Name: UpdateScreen
  // Desc: Is responsible for the actual rendering of the content windows
  //       (held in fWindows). This function must be called each
  //       time a DirectDraw frame gets rendered and IsAnyWindowShowing() returns
  //       True, so it should be placed in the main application's DirectDraw
  //       rendering routine. An example of this might look like the following:
  //
  //  procedure RenderFrame;
  //    begin
  //      if IsAnyWindowShowing
  //        then UpdateScreen;
  //        else FrontBuffer.Blt(...);
  //    end;
  //
  //-------------------------------------------------------------------------------

  function TFullScreenWindowManager.UpdateScreen(const DirtyRects : array of TRect) : boolean;
    var
      i              : integer;
      wnddata        : PWindowData;
      pt             : TPoint;
      rc             : TRect;
      x              : integer;
      y              : integer;
      cx             : integer;
      cy             : integer;
      hdcScreen      : HDC;
      hdcBackBuffer  : HDC;
      rgn            : HRGN;
      hdcMemory      : HDC;
      MouseCursorCur : HCURSOR;
      R              : TRect;
      hres           : HRESULT;
    begin
      if fNoGDIHardSupport
        then
          begin
            // Get a DC to the screen (where our windows are) and
            // Get a DC to the backbuffer on the non-GDI device (where we need to copy them)
            hdcScreen := GetDC(0);
            fBackBuffer.GetDC(hdcBackBuffer);
            for i := 0 to pred(fWindows.Count) do
              begin
                wnddata := PWindowData(fWindows[i]);
                GetWindowRect(wnddata.handle, rc);
                x := rc.left;
                y := rc.top;
                cx := rc.right - rc.left;
                cy := rc.bottom - rc.top;

                // If window has a complex region associated with it, be sure to include it in the draw
                rgn := CreateRectRgn(0, 0, 0, 0);
                if GetWindowRgn(wnddata.handle, rgn) // = COMPLEXREGION
                  then
                    begin
                      OffsetRgn(rgn, rc.left, rc.top);
                      SelectClipRgn(hdcBackBuffer, rgn);
                    end;
                // If content window is static (no animations, roll-overs, etc.) then
                // create a dc for the bitmap and blt to the back buffer
                if wnddata.static
                  then
                    begin
                      hdcMemory := CreateCompatibleDC(0);
                      SelectObject(hdcMemory, wnddata.bitmap);
                      BitBlt(hdcBackBuffer, x, y, cx, cy, hdcMemory, 0, 0, SRCCOPY);
                      DeleteDC(hdcMemory);
                    end
                  else
                    begin
                      // Special case for potentially quirky non-GDI drivers
                      {$IFDEF QUIRKY}
                      // If content is dynamic (updated each frame), always grab the screen copy
                      // by calling CreateBMPFromWindow to update image held in Bitmap
                      hdcMemory := CreateCompatibleDC(nil);
                      wnddata.bitmap := CreateBMPFromWindow(wnddata.handle);
                      SelectObject(hdcMemory, wnddata.bitmap);
                      BitBlt(hdcBackBuffer, x, y, cx, cy, hdcMemory, 0, 0, SRCCOPY);
                      DeleteDC(hdcMemory);
                      DeleteObject(Bitmap);
                      {$ELSE}
                      // Do a blt directly from the windows screen to the backbuffer
                      BitBlt(hdcBackBuffer, x, y, cx, cy, hdcScreen, x, y, SRCCOPY);
                      {$ENDIF}
                    end;

                // Remove clipping region and clean up
                SelectClipRgn(hdcBackBuffer, 0);
                DeleteObject(rgn);
              end;
            // Now draw the mouse on the backbuffer
            MouseCursorCur := GetCursor;
            if MouseCursorCur <> fMouseCursor
              then
                begin
                  fMouseCursor := MouseCursorCur;
                  GetIconInfo(fMouseCursor, fIconInfo);

                  if fIconInfo.hbmMask <> 0
                    then DeleteObject(fIconInfo.hbmMask);

                  if fIconInfo.hbmColor <> 0
                    then DeleteObject(fIconInfo.hbmColor);
                end;

            GetCursorPos(pt);
            dec(pt.x, fIconInfo.xHotspot);
            dec(pt.y, fIconInfo.yHotspot);
            DrawIcon(hdcBackBuffer, pt.x, pt.y, fMouseCursor);

            fBackBuffer.ReleaseDC(hdcBackBuffer);
            ReleaseDC(0, hdcScreen);

            //hres := fFrontBuffer.Flip(nil, DDFLIP_WAIT);
            hres := fFrontBuffer.Flip(nil, 0);
            if hres = DD_OK
              then Result := true
              else
                if hres = DDERR_SURFACELOST
                  then Result := (fFrontBuffer._Restore = DD_OK) and (fFrontBuffer.Flip(nil, 0) = DD_OK)
                  else Result := false;
          end
        else
          begin
            // GDI hardware
            // Update the surface with a blt
            //fFrontBuffer.SetClipper(fClipper);
            //Result := fFrontBuffer.Blt(nil, fBackBuffer, nil, 0, nil) = DD_OK;
            Result := true;
            i := low(DirtyRects);
            while Result and (i <= high(DirtyRects)) do
              if not IsRectEmpty(DirtyRects[i])
                then
                  begin
                    R := DirtyRects[i];
                    hres := fFrontBuffer.Blt(@R, fBackBuffer, @R, 0, nil);
                    if hres = DD_OK
                      then inc(i)
                      else
                        if hres = DDERR_SURFACELOST
                          then Result := fFrontBuffer._Restore = DD_OK
                          else Result := false;
                  end
                else inc(i);
            if not Result
              then WriteDebugStr('Blit to surface failed');
          end;
    end;

  //-------------------------------------------------------------------------------
  // Name: IsAnyWindowShowing
  // Desc: Simply checks to see if there's any content window displayed. This check
  //       should be made prior to calling UpdateWindows.
  //-------------------------------------------------------------------------------

  function TFullScreenWindowManager.IsAnyWindowShowing : boolean;
    begin
      Result := fWindows.Count > 0;
    end;

end.
