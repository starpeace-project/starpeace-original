program FullScreen;

  uses
    Windows, Messages, ComObj, DDraw, LogFile, TimerUtils, Graphics;

  const
    Name  = 'DDrawSample';
    Title = 'My Own DirectDraw Sample';

  var
    DDrawObj       : IDirectDraw4;        // DirectDraw object
    PrimarySurface : IDirectDrawSurface4; // DirectDraw primary surface
    BackBuffer     : IDirectDrawSurface4; // DirectDraw back surface

  const
    Active : boolean = FALSE;   // Is application active?

  procedure ReleaseAllObjects;
    begin
      if DDrawObj <> nil
        then
          begin
            if PrimarySurface <> nil
              then PrimarySurface := nil;
            DDrawObj := nil;
          end;
    end;

  function WindowProc(Wnd : HWND; msg, wParam, lParam : integer) : integer; stdcall;
    begin
      case msg of
        WM_ACTIVATEAPP:
          begin
            // Pause if minimized or not the top window
            Active := (wParam = WA_ACTIVE) or (wParam = WA_CLICKACTIVE);
            Result := 0;
          end;
        WM_DESTROY:
          begin
            // Clean up and close the app
            ReleaseAllObjects;
            PostQuitMessage(0);
            Result := 0;
          end;
        WM_KEYDOWN:
          // Handle any non-accelerated key commands
          case wParam of
            VK_ESCAPE,
            VK_F12:
              begin
                PostMessage(Wnd, WM_CLOSE, 0, 0);
                Result := 0;
              end
            else Result := 0;
          end;
        WM_SETCURSOR:
           begin
             // Turn off the cursor since this is a full-screen app
             SetCursor(0);
             Result := 1;
           end;
        else Result := DefWindowProc(Wnd, msg, wParam, lParam);
      end;
  end;

  function LoadBitmapToSurface(const BitmapName : string; const Surface : IDirectDrawSurface4) : boolean;
    var
      bmp   : TBitmap;
      dc    : HDC;
      memdc : HDC;
    begin
      Result := true;
      try
        bmp := TBitmap.Create;
        try
          bmp.LoadFromFile(BitmapName);
          if Surface.GetDC(dc) = DD_OK
            then
              try
                memdc := CreateCompatibleDC(dc);
                if memdc <> 0
                  then
                    try
                      SelectObject(memdc, bmp.Handle);
                      if not BitBlt(dc, 0, 0, bmp.Width, bmp.Height, memdc, 0, 0, SRCCOPY)
                        then LogThis('BitBlt FAILED');
                    finally
                      DeleteDC(memdc);
                    end
                  else LogThis('Could not create memory device context');
              finally
                Surface.ReleaseDC(dc);
              end
            else LogThis('Could not get surface device context');
        finally
          bmp.Free;
        end;
      except
        LogThis('Exception generated while loading object');
        Result := false;
      end;
    end;

  type
    TFlipperTimer = TSimpleTimer;

  type
    TPageFlipper =
      class
        public
          constructor Create;
        private
          fTimer : TFlipperTimer;
          procedure TimerTick;
      end;

  constructor TPageFlipper.Create;
    begin
      inherited;
      fTimer := TFlipperTimer.Create;
      fTimer.Interval := 200;
      fTimer.OnTimer := TimerTick;
      fTimer.Enabled := true;
    end;

  procedure TPageFlipper.TimerTick;
    var
      hret : HRESULT;
    begin
      while true do
        begin
          hRet := PrimarySurface.Flip(nil, 0);
          if hRet = DD_OK
            then break;
          if hRet = DDERR_SURFACELOST
            then
              begin
                hRet := PrimarySurface._Restore;
                if hRet <> DD_OK
                  then break;
                if not LoadBitmapToSurface('C:\Work\Tests\DirectDraw\shipping.bmp', BackBuffer)
                  then break;
              end;
          if hRet <> DDERR_WASSTILLDRAWING
            then break;
        end;
    end;

  var
    PageFlipper : TPageFlipper;

  procedure InitApp(hInstance, nCmdShow : integer);
    var
      Wnd     : HWND;
      wc      : TWndClass;
      ddsd    : TDDSurfaceDesc2;
      ddscaps : TDDSCAPS2;
      hRet    : HRESULT;
      pDD     : IDirectDraw;
    begin
      // Set up and register window class
      wc.style := CS_HREDRAW or CS_VREDRAW;
      wc.lpfnWndProc := @WindowProc;
      wc.cbClsExtra := 0;
      wc.cbWndExtra := 0;
      wc.hInstance := hInstance;
      wc.hIcon := 0;//LoadIcon(hInstance, MAKEINTRESOURCE(IDI_MAIN_ICON));
      wc.hCursor := 0;//LoadCursor(NULL, IDC_ARROW);
      wc.hbrBackground := HBRUSH(GetStockObject(BLACK_BRUSH));
      wc.lpszMenuName := Name;
      wc.lpszClassName := Name;
      RegisterClass(wc);

      // Create a window
      Wnd := CreateWindowEx(WS_EX_TOPMOST,
                            Name,
                            Title,
                            WS_POPUP,
                            0,
                            0,
                            GetSystemMetrics(SM_CXSCREEN),
                            GetSystemMetrics(SM_CYSCREEN),
                            0,
                            0,
                            hInstance,
                            nil);
      if Wnd <> 0
        then
          begin
            ShowWindow(Wnd, nCmdShow);
            UpdateWindow(Wnd);
            SetFocus(Wnd);
            ///////////////////////////////////////////////////////////////////////////
            // Create the main DirectDraw object
            ///////////////////////////////////////////////////////////////////////////
            hRet := DirectDrawCreate(nil, pDD, nil);
            if hRet <> DD_OK
              then LogThis('DirectDrawCreate FAILED')
              else
                begin
                  // Fetch DirectDraw4 interface
                  hRet := pDD.QueryInterface(IID_IDirectDraw4, DDrawObj);
                  if hRet <> DD_OK
                    then LogThis('QueryInterface FAILED')
                    else
                      begin
                        // Get exclusive mode
                        hRet := DDrawObj.SetCooperativeLevel(Wnd, DDSCL_EXCLUSIVE or DDSCL_FULLSCREEN);
                        if hRet <> DD_OK
                          then LogThis('SetCooperativeLevel FAILED')
                          else
                            begin
                              // Set the video mode to 640x480x8
                              hRet := DDrawObj.SetDisplayMode(640, 480, 8, 0, 0);
                              if hRet <> DD_OK
                                then LogThis('SetDisplayMode FAILED')
                                else
                                  begin
                                    // Create the primary surface with 1 back buffer
                                    fillchar(ddsd, sizeof(ddsd), 0);
                                    ddsd.dwSize := sizeof(ddsd);
                                    ddsd.dwFlags := DDSD_CAPS or DDSD_BACKBUFFERCOUNT;
                                    ddsd.ddsCaps.dwCaps := DDSCAPS_PRIMARYSURFACE or DDSCAPS_FLIP or DDSCAPS_COMPLEX;
                                    ddsd.dwBackBufferCount := 1;
                                    hRet := DDrawObj.CreateSurface(ddsd, PrimarySurface, nil);
                                    if hRet <> DD_OK
                                      then LogThis('CreateSurface FAILED')
                                      else
                                        begin
                                          // Get a pointer to the back buffer
                                          // Load a bitmap into the front buffer.
                                          if LoadBitmapToSurface('C:\Work\Tests\DirectDraw\handshak.bmp', PrimarySurface)
                                            then
                                              begin
                                                ddscaps.dwCaps := DDSCAPS_BACKBUFFER;
                                                hRet := PrimarySurface.GetAttachedSurface(ddscaps, BackBuffer);
                                                if hRet <> DD_OK
                                                  then LogThis('GetAttachedSurface FAILED')
                                                  else
                                                    begin
                                                      // Load a bitmap into the back buffer.
                                                      if LoadBitmapToSurface('C:\Work\Tests\DirectDraw\shipping.bmp', BackBuffer)
                                                        then PageFlipper := TPageFlipper.Create // Create object to flip the pages
                                                        else LogThis('Load bitmap into back buffer FAILED');
                                                    end;
                                              end
                                            else LogThis('Load bitmap into primary surface FAILED');
                                        end;
                                  end;
                            end;
                      end;
                end;
          end;
    end;

  var
    msg : TMsg;
  begin
    SetLogFile('C:\Work\Tests\DirectDraw\DDrawSample.log');
    InitApp(HInstance, CmdShow);
    while GetMessage(msg, 0, 0, 0) do
      begin
        TranslateMessage(msg);
        DispatchMessage(msg);
      end;
  end.

