unit MainWindow;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    D3DXCore, DirectDraw, Direct3D, SurfaceSpriteImages, ExtCtrls, FullScreenWindowMgr;

  const
    cMaxImages = $FFFFF;

  type
    PImageArray = ^TImageArray;
    TImageArray = array [0..cMaxImages] of TSurfaceFrameImage;

  const
    cImageFileNamesFile = 'C:\Work\Five\Release\Client\LoadedImages.log';

  type
    TMainForm =
      class(TForm)
        StatusLine: TPanel;
        procedure FormCreate(Sender: TObject);
        procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
      private
        { Private declarations }
        function  RenderTest(Alpha : single) : HRESULT;
        procedure LoadSprites;
        procedure ApplicationOnIdle(Sender : TObject; var Done : boolean);
        //HandleModeChanges : HRESULT;
      public
        { Public declarations }
        fD3DXContext     : ID3DXContext;
        fDirectDraw      : IDirectDraw7;
        fDirect3DDevice  : IDirect3DDevice7;
        fPrimarySurface  : IDirectDrawSurface7;
        fBackBuffer      : IDirectDrawSurface7;
        fFSWindowManager : TFullScreenWindowManager;
        fImageFileNames  : TStringList;
        fImages          : PImageArray;
        fImageCount      : integer;
      end;

  var
    MainForm: TMainForm;

implementation

  {$R *.DFM}

  uses
    D3DXSprite, D3DXErr, ImageLoader, GifLoader, DDrawD3DManager, ClipBrd, DirectDrawUtils;

  const
    cScreenWidth  = 1024;
    cScreenHeight =  768;

  procedure TMainForm.FormCreate(Sender: TObject);
    begin
      Left := 0;
      Top := 0;
      Width := cScreenWidth;
      Height := cScreenHeight;
      InitDDrawD3D(Handle, cScreenWidth, cScreenHeight);
      fD3DXContext := DDrawD3DMgr.D3DXContext;
      fDirectDraw := DDrawD3DMgr.DirectDraw;
      fDirect3DDevice := DDrawD3DMgr.Direct3DDevice;
      fPrimarySurface := DDrawD3DMgr.PrimarySurface;
      fBackBuffer     := DDrawD3DMgr.BackBuffer;
      fFSWindowManager := TFullScreenWindowManager.Create(Handle, fDirectDraw, fPrimarySurface, fBackBuffer);
      fFSWindowManager.RegisterWindow(StatusLine.Handle, false, 0);
      //Application.OnIdle := ApplicationOnIdle;
      fImageFileNames := TStringList.Create;
      fImageFileNames.LoadFromFile(cImageFileNamesFile);
    end;

  function TMainForm.RenderTest(Alpha : single) : HRESULT;
    const
      cLoopCount = 1500;
    var
      hr               : HRESULT;
      viewport         : TD3DVIEWPORT7;
      x, y             : integer;
      initialtickcount : integer;
      elapsedticks     : integer;
      i                : integer;
      errstr           : string;
      ImageRect        : TRect;
      imgidx           : integer;
    begin
      if fImageCount > 0
        then
          if Succeeded(fDirect3DDevice.BeginScene)
            then
              begin
                //fD3DXContext.Clear(D3DCLEAR_TARGET or D3DCLEAR_ZBUFFER);
                fDirect3DDevice.Clear(0, nil, D3DCLEAR_TARGET, D3DRGBA(0.0, 0.0, 0.0, 0.0), 0, 0);
                // We need to setup the rasterizer for rendering sprites;
                // this only needs to be done once for all the sprites that
                // are rendered; however this function does need to be called
                // again if any render state changes are made outside of the
                // bltsprite call.
                D3DXPrepareDeviceForSprite(fDirect3DDevice, false);
                fDirect3DDevice.SetRenderState(D3DRENDERSTATE_COLORKEYENABLE,    1);

                // Get our current viewport
                hr := fDirect3DDevice.GetViewport(viewport);
                if Succeeded(hr)
                  then
                    begin
                      initialtickcount := GetTickCount();
                      // Go ahead and do the render
                      for i := 0 to pred(cLoopCount) do
                        begin
                          x := random(cScreenWidth);
                          y := random(cScreenHeight);
                          imgidx := random(fImageCount);
                          ImageRect := Rect(x, y, x + fImages[imgidx].Width, y + fImages[imgidx].Height);
                          IntersectRect(ImageRect, ImageRect, ClientRect);
                          fImages[imgidx].Draw(x, y, 0, 0, 1.0, ImageRect, fDirect3DDevice, nil);
                        end;
                      fDirect3DDevice.EndScene;
                      //hr := fD3DXContext.UpdateFrame(D3DX_DEFAULT);
                      //hr := fPrimarySurface.Flip(nil, DDFLIP_DONOTWAIT);
                      fFSWindowManager.UpdateScreen([ClientRect]);
                      if Failed(hr)
                        then
                          begin
                            SetLength(errstr, 1000);
                            D3DXGetErrorString(hr, 1000, pchar(errstr));
                            Application.MessageBox(pchar(errstr), 'Error', MB_OK);
                          end;
                      {
                      if (hr = DDERR_SURFACELOST) or (hr = DDERR_SURFACEBUSY)
                        then hr = HandleModeChanges();
                      }
                      elapsedticks := GetTickCount() - initialtickcount;
                      StatusLine.Caption := IntToStr(i) + ' images rendered in ' + IntToStr(elapsedticks) + ' milliseconds.';
                      //Clipboard.AsText := StatusLine.Caption;
                      Sleep(1);
                      fDirect3DDevice.BeginScene;
                      initialtickcount := GetTickCount();
                      // Go ahead and do the render
                      for i := 0 to pred(cLoopCount) do
                        begin
                          x := random(cScreenWidth);
                          y := random(cScreenHeight);
                          imgidx := random(fImageCount);
                          ImageRect := Rect(x, y, x + fImages[imgidx].Width, y + fImages[imgidx].Height);
                          IntersectRect(ImageRect, ImageRect, ClientRect);
                          fImages[imgidx].Draw(x, y, 0, 0, 1.0, ImageRect, fDirect3DDevice, nil);
                        end;
                      fDirect3DDevice.EndScene;
                      //hr := fD3DXContext.UpdateFrame(D3DX_DEFAULT);
                      //hr := fPrimarySurface.Flip(nil, DDFLIP_DONOTWAIT);
                      fFSWindowManager.UpdateScreen([ClientRect]);
                      if Failed(hr)
                        then
                          begin
                            SetLength(errstr, 1000);
                            D3DXGetErrorString(hr, 1000, pchar(errstr));
                            Application.MessageBox(pchar(errstr), 'Error', MB_OK);
                          end;
                      {
                      if (hr = DDERR_SURFACELOST) or (hr = DDERR_SURFACEBUSY)
                        then hr = HandleModeChanges();
                      }
                      elapsedticks := GetTickCount() - initialtickcount;
                      StatusLine.Caption := StatusLine.Caption + ' ' + IntToStr(i) + ' images rendered in ' + IntToStr(elapsedticks) + ' milliseconds.';
                      Clipboard.AsText := StatusLine.Caption;
                      //Application.MessageBox(pchar(IntToStr(i) + ' images rendered in ' + IntToStr(elapsedticks) + ' milliseconds.'), 'Report', MB_OK);
                      //OutputDebugString(pchar(IntToStr(i) + ' images rendered in ' + IntToStr(elapsedticks) + ' milliseconds.'));
                    end;
              end
            else hr := E_FAIL
        else hr := E_FAIL;
      Result := hr;
    end;
    
  procedure TMainForm.LoadSprites;
    var
      i : integer;
    begin
      fImageCount := fImageFileNames.Count;
      getmem(fImages, fImageCount*sizeof(fImages[0]));
      for i := 0 to pred(fImageCount) do
        fImages[i] := LoadGameImage(fImageFileNames[i]);
    end;

  procedure TMainForm.ApplicationOnIdle(Sender : TObject; var Done : boolean);
    var
      ddscaps      : TDDSCaps2;
      totalmemfree : dword;
      freemem      : dword;
      megs         : dword;
      tmprest      : dword;
      kilobytes    : dword;
      bytes        : dword;
    begin
      InitRecord(ddscaps, sizeof(ddscaps));
      with ddscaps do
        //ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN;
        ddsCaps.dwCaps := DDSCAPS_TEXTURE;
      if Succeeded(fDirectDraw.GetAvailableVidMem(ddscaps, totalmemfree, freemem))
        then
          begin
            megs := totalmemfree div (1024*1024);
            tmprest := totalmemfree mod (1024*1024);
            kilobytes := tmprest div 1024;
            bytes := tmprest mod 1024;
            StatusLine.Caption := IntToStr(megs) + ' Megs ' + IntToStr(kilobytes) + ' K and ' + IntToStr(bytes) + ' bytes.';
            megs := freemem div (1024*1024);
            tmprest := freemem mod (1024*1024);
            kilobytes := tmprest div 1024;
            bytes := tmprest mod 1024;
            StatusLine.Caption := StatusLine.Caption + ' ' + IntToStr(megs) + ' Megs ' + IntToStr(kilobytes) + ' K and ' + IntToStr(bytes) + ' bytes.';
          end
        else StatusLine.Caption := 'Error';
    end;

  procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    begin
      case key of
        VK_F3:
          LoadSprites;
        VK_F4:
          RenderTest(8);
        VK_F5:
          RenderTest(4);
      end;
    end;

end.
