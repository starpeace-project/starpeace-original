unit FullScreenWindow;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    DDraw, ExtCtrls, SpriteImages, FullScreenWindowMgr;

  type
    TFullScreenForm =
      class(TForm)
          OpenDialog: TOpenDialog;
          procedure FormCreate(Sender: TObject);
          procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
          procedure FormClose(Sender: TObject; var Action: TCloseAction);
        private
          { Private declarations }
          fDirectDraw  : IDirectDraw4;
          fPrimary     : IDirectDrawSurface4;
          fBackBuffer  : IDirectDrawSurface4;
          fClipper     : IDirectDrawClipper;
          fTimer       : TTimer;
          fAnimations  : TList;
          fFSWindowMgr : TFullScreenWindowManager;
          procedure InitDirectDraw;
          procedure ReleaseAllObjects;
          procedure TimerTick(Sender : TObject);
        public
          { Public declarations }
          procedure WindowOpened(Wnd : HWND);
          procedure WindowClosed(Wnd : HWND);
      end;

  var
    FullScreenForm: TFullScreenForm;

implementation

  {$R *.DFM}

  uses
    LogFile, SubWindow, ImageLoaders, GifLoader, SpriteLoaders, SpriteToSurface,
    Animations;

  const
    cScreenWidth  = 1024;
    cScreenHeight = 768;
    cScreenBpp    = 32;

  procedure TFullScreenForm.FormCreate(Sender: TObject);
    const
      cSpriteFileNames : array [0..8] of string =
        (
          'C:\Work\Five\Release\Client\cache\buildingimages\mapdismine64x32x0.gif',
          'C:\Work\Five\Release\Client\cache\buildingimages\mapmoabmine64x32x0.gif',
          'C:\Work\Five\Release\Client\cache\buildingimages\mapmoabfarm64x32x0.gif',
          'C:\Work\Five\Release\Client\cache\buildingimages\mapmoabfood64x32x0.gif',
          'C:\Work\Five\Release\Client\cache\buildingimages\MapDisHiResBEmpty64x32x0.gif',
          'C:\Work\Five\Release\Client\cache\buildingimages\MapDisSmallFoodProc64x32x0.gif',
          'C:\Work\Five\Release\Client\cache\buildingimages\mapdistv64x32x0.gif',
          'C:\Work\Five\Release\Client\cache\buildingimages\mapdisrestaurantb64x32x0.gif',
          'C:\Work\Five\Release\Client\cache\buildingimages\mapdisfarm64x32x0.gif'
        );
    var
      SpriteImage : TFrameImage;
      curx, cury  : integer;
      i           : integer;
    begin
      fAnimations := TList.Create;
      fFSWindowMgr := TFullScreenWindowManager.Create;
      curx := 0;
      cury := 0;
      for i := 0 to 8 do
        begin
          LoadFrameImageFromFile(SpriteImage, cSpriteFileNames[i]);
          if curx + SpriteImage.Width > cScreenWidth
            then
              begin
                curx := 0;
                inc(cury, 250);
              end;
          fAnimations.Add(TAnimation.Create(SpriteImage, curx, cury));
          inc(curx, SpriteImage.Width);
        end;
      fTimer := TTimer.Create(Self);
      fTimer.Interval := 40;
      fTimer.OnTimer := TimerTick;
      fTimer.Enabled := true;
      InitDirectDraw;
      fFSWindowMgr.Init(Handle, fDirectDraw, fPrimary, fBackBuffer);
      SetBounds(0, 0, Screen.Width, Screen.Height);
    end;

  procedure TFullScreenForm.InitDirectDraw;
    var
      ddsd     : TDDSurfaceDesc2;
      ddscaps  : TDDSCAPS2;
      hRet     : HRESULT;
      ddrawobj : IDirectDraw;
    begin
      // Create the main DirectDraw object
      hRet := DirectDrawCreate(nil, ddrawobj, nil);
      if hRet <> DD_OK
        then LogThis('DirectDrawCreate FAILED')
        else
          begin
            // Fetch DirectDraw4 interface
            hRet := ddrawobj.QueryInterface(IID_IDirectDraw4, fDirectDraw);
            if hRet <> DD_OK
              then LogThis('QueryInterface FAILED')
              else
                begin
                  // Get exclusive mode
                  hRet := fDirectDraw.SetCooperativeLevel(Handle, DDSCL_EXCLUSIVE or DDSCL_FULLSCREEN);
                  if hRet <> DD_OK
                    then LogThis('SetCooperativeLevel FAILED')
                    else
                      begin
                        // Set the video mode to 640x480x8
                        hRet := fDirectDraw.SetDisplayMode(cScreenWidth, cScreenHeight, cScreenBpp, 0, 0);
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
                              hRet := fDirectDraw.CreateSurface(ddsd, fPrimary, nil);
                              if hRet <> DD_OK
                                then LogThis('CreateSurface FAILED')
                                else
                                  begin
                                    // Get a pointer to the back buffer
                                    // Load a bitmap into the front buffer.
                                    ddscaps.dwCaps := DDSCAPS_BACKBUFFER;
                                    hRet := fPrimary.GetAttachedSurface(ddscaps, fBackBuffer);
                                    if hRet <> DD_OK
                                      then LogThis('GetAttachedSurface FAILED')
                                  end;
                            end;
                      end;
                end;
          end;
    end;

  procedure TFullScreenForm.ReleaseAllObjects;
    begin
      if fDirectDraw <> nil
        then
          begin
            fDirectDraw.RestoreDisplayMode;
            fDirectDraw.SetCooperativeLevel(Handle, DDSCL_NORMAL);
            if fPrimary <> nil
              then fPrimary := nil;
            fDirectDraw := nil;
          end;
    end;

  procedure TFullScreenForm.TimerTick(Sender : TObject);
    var
      hret         : HRESULT;
      DDBltFx      : TDDBltFx;
      success      : boolean;
      criterror    : boolean;
      surfacedesc  : TDDSurfaceDesc2;
      i            : integer;
      CurAnimation : TAnimation;
    begin
      fillchar(DDBltFx, sizeof(DDBltFx), 0);
      DDBltFx.dwSize := sizeof(DDBltFx);
      DDBltFx.dwFillColor := RGB(0, 0, 0);
      fBackBuffer.Blt(nil, nil, nil, DDBLT_COLORFILL or DDBLT_WAIT, @DDBltFx);
      fillchar(surfacedesc, sizeof(surfacedesc), 0);
      surfacedesc.dwSize := sizeof(surfacedesc);
      if fBackBuffer.Lock(nil, surfacedesc, DDLOCK_WAIT, 0) = DD_OK
        then
          begin
            try
              for i := 0 to pred(fAnimations.Count) do
                begin
                  CurAnimation := TAnimation(fAnimations[i]);
                  with CurAnimation do
                    begin
                      Tick;
                      RenderSpriteToSurface(Image, Pos.X, Pos.Y, Frame, 0, Rect(Pos.X, Pos.Y, Pos.X + Image.Width, Pos.Y + Image.Height), surfacedesc, nil);
                    end;
                end;
            finally
              fBackBuffer.Unlock(nil);
            end;
            if not fFSWindowMgr.IsAnyWindowShowing
              then
                begin
                  LogThis('Flipping surface.');
                  success := false;
                  criterror := false;
                  repeat
                    hRet := fPrimary.Flip(nil, 0);
                    if hRet = DD_OK
                      then success := true
                      else
                        begin
                          if hRet = DDERR_SURFACELOST
                            then
                              begin
                                hRet := fPrimary._Restore;
                                if hRet <> DD_OK
                                  then criterror := true;
                              end;
                          if hRet <> DDERR_WASSTILLDRAWING
                            then criterror := true;
                        end;
                  until success or criterror;
                end
              else
                begin
                  LogThis('Updating windows. Blitting.');
                  fFSWindowMgr.UpdateWindows;
                end;
          end
        else LogThis('Lock surface failed');
    end;

  procedure TFullScreenForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    begin
      case Key of
        VK_ESCAPE:
          Close;
        VK_F1:
          begin
            SubForm.Left := 0;
            SubForm.Top := 0;
            SubForm.Parent := Self;
            SubForm.Show;
            // Create a clipper (used in IDirectDrawSurface::Blt call)
            if fDirectDraw.CreateClipper(0, fClipper, nil) = DD_OK
              then fClipper.SetHWnd(0, Handle);
            // Normal GDI device, so just flip to GDI so content window can be seen
            fDirectDraw.FlipToGDISurface;
          end;
      end;
    end;

  procedure TFullScreenForm.FormClose(Sender: TObject; var Action: TCloseAction);
    begin
      fTimer.Enabled := false;
      ReleaseAllObjects;
    end;

  procedure TFullScreenForm.WindowOpened(Wnd : HWND);
    begin
      fFSWindowMgr.RegisterWindow(Wnd, false, 0);
    end;

  procedure TFullScreenForm.WindowClosed(Wnd : HWND);
    begin
      fFSWindowMgr.UnregisterWindow(Wnd);
    end;

initialization
  InitSprites;
  RegisterLoader(GetGifLoader, 0);
  SetLogFile('C:\Temp\' + ExtractFileName(Application.ExeName) + '.log');
finalization
  DoneSprites;
end.
