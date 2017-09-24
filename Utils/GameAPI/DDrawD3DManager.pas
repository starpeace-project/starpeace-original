unit DDrawD3DManager;

interface

  uses
    Windows, DirectDraw, Direct3D, D3DXCore;

  type
    TDDrawD3DManager =
      class
        public
          constructor Create(MainWindow : HWND; ScreenWidth, ScreenHeight : integer);
          destructor  Destroy; override;
        private
          function InitD3DX : HRESULT;
          function ReleaseD3DX : HRESULT;
          function InitRenderer : HRESULT;
        public
          function CreateCompatibleOffscreenSurface(width, height : integer) : IDirectDrawSurface7;
          function CreateOffscreenSurface(width, height, bppixel : integer) : IDirectDrawSurface7;
          function CreateTextureSurface(width, height, bppixel : integer; managed : boolean) : IDirectDrawSurface7;
          function CreatePalette(palentries : pointer) : IDirectDrawPalette;
        private
          fMainWindow     : HWND;
          fScreenWidth    : integer;
          fScreenHeight   : integer;
          fD3DXContext    : ID3DXContext;
          fDirectDraw     : IDirectDraw7;
          fDirect3D       : IDirect3D7;
          fDirect3DDevice : IDirect3DDevice7;
          fPrimarySurface : IDirectDrawSurface7;
          fBackBuffer     : IDirectDrawSurface7;
          fD3DXReady      : boolean;
          fD3DDeviceCaps  : TD3DDeviceDesc7;
        public
          property D3DXContext    : ID3DXContext        read fD3DXContext;
          property DirectDraw     : IDirectDraw7        read fDirectDraw;
          property Direct3D       : IDirect3D7          read fDirect3D;
          property Direct3DDevice : IDirect3DDevice7    read fDirect3DDevice;
          property PrimarySurface : IDirectDrawSurface7 read fPrimarySurface;
          property BackBuffer     : IDirectDrawSurface7 read fBackBuffer;
      end;

  var
    DDrawD3DMgr : TDDrawD3DManager;

  procedure InitDDrawD3D(MainWindow : HWND; ScreenWidth, ScreenHeight : integer);
  procedure ReleaseDDrawD3D;

implementation

  uses
    SysUtils, D3DXErr, DirectDrawUtils;

  // Utilities

  procedure SetPixFormat(BitCount : dword; var Dest : TDDPixelFormat );
    begin
      with Dest do
        begin
          // TODO 1 -oJRG -cGraphics : Tengo que añadir soporte para el formato BGR!!
          dwSize        := sizeof( TDDPixelFormat );
          dwFourCC      := 0;
          dwRGBBitCount := BitCount;
          case BitCount of
            1 :
              dwFlags := DDPF_RGB or DDPF_PALETTEINDEXED1;
            2 :
              dwFlags := DDPF_RGB or DDPF_PALETTEINDEXED2;
            4 :
              dwFlags := DDPF_RGB or DDPF_PALETTEINDEXED4;
            8 :
              dwFlags := DDPF_RGB or DDPF_PALETTEINDEXED8;
            15 :
              begin
                dwFlags           := DDPF_RGB;
                dwRGBBitCount     := 16;
                dwRBitMask        := $7C00;
                dwGBitMask        := $03E0;
                dwBBitMask        := $001F;
                dwRGBAlphaBitMask := 0;
              end;
            16 :
              begin
                dwFlags           := DDPF_RGB;
                dwRBitMask        := $F800;
                dwGBitMask        := $07E0;
                dwBBitMask        := $001F;
                dwRGBAlphaBitMask := 0;
              end;
            24 :
              begin
                dwFlags           := DDPF_RGB;
                dwRBitMask        := $FF0000;
                dwGBitMask        := $00FF00;
                dwBBitMask        := $0000FF;
                dwRGBAlphaBitMask := 0;
              end;
//            24 :
//              begin
//                dwFlags           := DDPF_RGB;
//                dwRBitMask        := $0000FF;
//                dwGBitMask        := $00FF00;
//                dwBBitMask        := $FF0000;
//                dwRGBAlphaBitMask := 0;
//              end;
            32 :
              begin
                dwFlags           := DDPF_RGB;
                dwRBitMask        := $FF0000;
                dwGBitMask        := $00FF00;
                dwBBitMask        := $0000FF;
                dwRGBAlphaBitMask := 0;
              end;
//            32 :
//              begin
//                dwFlags           := DDPF_RGB;
//                dwRBitMask        := $0000FF;
//                dwGBitMask        := $00FF00;
//                dwBBitMask        := $FF0000;
//                dwRGBAlphaBitMask := 0;
//              end;
          end;
        end;
    end;

  // TDDrawD3DManager

  constructor TDDrawD3DManager.Create(MainWindow : HWND; ScreenWidth, ScreenHeight : integer);
    begin
      inherited Create;
      fMainWindow := MainWindow;
      fScreenWidth := ScreenWidth;
      fScreenHeight := ScreenHeight;
      if Failed(InitD3DX)
        then raise Exception.Create('D3DX Initialization failed!');
    end;

  destructor TDDrawD3DManager.Destroy;
    begin
      ReleaseD3DX;
    end;

  function TDDrawD3DManager.InitD3DX : HRESULT;
    var
      hr            : HRESULT;
      dwDevice      : dword;
      dwDeviceCount : dword;
      dev           : TD3DX_DEVICEDESC;
      d3dDesc       : TD3DDEVICEDESC7;
      devDesc       : TD3DX_DEVICEDESC;
    begin
      hr := D3DXInitialize;
      if Succeeded(hr)
        then
          begin
            // Look for fastest device which supports the desired blending for sprites
            dwDeviceCount := D3DXGetDeviceCount;
            dev.deviceIndex := D3DX_DEFAULT;
            dev.hwLevel     := D3DX_DEFAULT;
            dev.onPrimary   := TRUE;

            for dwDevice := 0 to pred(dwDeviceCount) do
              begin
                if Succeeded(D3DXGetDeviceCaps(dwDevice, nil, @d3dDesc, nil, nil))
                  then
                    if (((d3dDesc.dpcTriCaps.dwSrcBlendCaps and D3DPBLENDCAPS_SRCALPHA) <> 0) and
                        ((d3dDesc.dpcTriCaps.dwDestBlendCaps and D3DPBLENDCAPS_INVSRCALPHA) <> 0) and
                        ((d3dDesc.dpcTriCaps.dwTextureFilterCaps and D3DPTFILTERCAPS_LINEAR) <> 0) and
                        ((d3dDesc.dpcTriCaps.dwTextureBlendCaps and D3DPTBLENDCAPS_MODULATE) <> 0))
                      then
                        if Succeeded(D3DXGetDeviceDescription(dwDevice, devDesc))
                          then
                            if (D3DX_DEFAULT = dev.hwLevel) or
                               (dev.hwLevel > devDesc.hwLevel) or
                               (dev.hwLevel = devDesc.hwLevel) and (devDesc.onPrimary)
                              then dev := devDesc;
              end;

            if D3DX_DEFAULT = dev.hwLevel
              then hr := D3DXERR_NODIRECT3DDEVICEAVAILABLE
              else
                begin
                  hr := D3DXCreateContext(
                                          dev.hwLevel,                    // D3DX device
                                          D3DX_CONTEXT_FULLSCREEN,        // flags
                                          fMainWindow,                    // Main window
                                          fScreenWidth,                   // width
                                          fScreenHeight,		  // height
                                          fD3DXContext);                  // returned D3DX interface
                  if Succeeded(hr)
                    then
                      begin
                        fD3DXReady := true;
                        hr := InitRenderer;
                      end;
                end;
          end;
      Result := hr;
    end;

  function TDDrawD3DManager.ReleaseD3DX : HRESULT;
    begin
      fBackBuffer     := nil;
      fPrimarySurface := nil;
      fDirectDraw     := nil;
      fDirect3DDevice := nil;
      fDirect3D       := nil;
      fD3DXContext    := nil;
      Result := D3DXUninitialize;
    end;

  function TDDrawD3DManager.InitRenderer : HRESULT;
    begin
      if fD3DXReady
        then
          begin
            fDirectDraw := IDirectDraw7(fD3DXContext.GetDD); // >>> this a temporary patch
            fPrimarySurface := IDirectDrawSurface7(fD3DXContext.GetPrimary);
            fBackBuffer := IDirectDrawSurface7(fD3DXContext.GetBackBuffer(0));
            fDirect3DDevice := IDirect3DDevice7(fD3DXContext.GetD3DDevice);
            fDirect3DDevice.GetCaps(fD3DDeviceCaps);
            if fDirect3DDevice <> nil
              then
                begin
                  fDirectDraw := IDirectDraw7(fD3DXContext.GetDD);
                  if fDirectDraw <> nil
                    then
                      begin
                        // Enable dither, specular, lighting and z-buffer usage
                        fDirect3DDevice.SetRenderState(D3DRENDERSTATE_COLORKEYENABLE,    1);
                        fDirect3DDevice.SetRenderState(D3DRENDERSTATE_DITHERENABLE,      0);
                        fDirect3DDevice.SetRenderState(D3DRENDERSTATE_SPECULARENABLE,    0);
                        fDirect3DDevice.SetRenderState(D3DRENDERSTATE_LIGHTING,          0);
                        fDirect3DDevice.SetRenderState(D3DRENDERSTATE_ZENABLE,           0);
                        // Enable vertices to have colors
                        fDirect3DDevice.SetRenderState(D3DRENDERSTATE_COLORVERTEX,       0);
                        fDirect3DDevice.SetRenderState(D3DRENDERSTATE_DIFFUSEMATERIALSOURCE,     0);

                        // Set the background to bright blue (red/green/blue/alpha)
                        fD3DXContext.SetClearColor(TD3DCOLOR(D3DRGBA(0.0, 0.0, 0.0, 0.0)));

                        fD3DXContext.Clear(D3DCLEAR_TARGET or D3DCLEAR_ZBUFFER);

                        Result := S_OK;
                      end
                    else Result := E_FAIL;
                end
              else Result := E_FAIL;
          end
        else Result := E_FAIL;
    end;

  function TDDrawD3DManager.CreateCompatibleOffscreenSurface(width, height : integer) : IDirectDrawSurface7;
    var
      ddsd : TDDSurfaceDesc2;
    begin
      Result := nil;
      InitRecord(ddsd, sizeof(ddsd));
      with ddsd do
        begin
          dwFlags        := DDSD_CAPS or DDSD_HEIGHT or DDSD_WIDTH;
          ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN;
          dwWidth        := Width;
          dwHeight       := Height;
        end;
      fDirectDraw.CreateSurface(ddsd, Result, nil);
    end;

  function TDDrawD3DManager.CreateOffscreenSurface(width, height, bppixel : integer) : IDirectDrawSurface7;
    var
      ddsd  : TDDSurfaceDesc2;
    begin
      InitRecord(ddsd, sizeof(ddsd));
      with ddsd do
        begin
          dwFlags        := DDSD_CAPS or DDSD_HEIGHT or DDSD_WIDTH or DDSD_PIXELFORMAT;
          ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
          dwWidth        := Width;
          dwHeight       := Height;
          SetPixFormat(bppixel, ddpfPixelFormat);
        end;
      fDirectDraw.CreateSurface(ddsd, Result, nil);
    end;

  function TDDrawD3DManager.CreateTextureSurface(width, height, bppixel : integer; managed : boolean) : IDirectDrawSurface7;
    var
      ddsd : TDDSurfaceDesc2;
    begin
      Result := nil;
      InitRecord(ddsd, sizeof(ddsd));
      with ddsd do
        begin
          dwFlags         := DDSD_CAPS or DDSD_HEIGHT or DDSD_WIDTH or DDSD_PIXELFORMAT or DDSD_TEXTURESTAGE;
          ddsCaps.dwCaps  := DDSCAPS_TEXTURE;
          if managed
            then ddsCaps.dwCaps2 := DDSCAPS2_TEXTUREMANAGE;
          dwWidth         := Width;
          dwHeight        := Height;
          SetPixFormat(bppixel, ddpfPixelFormat);
          if (fD3DDeviceCaps.dpcTriCaps.dwTextureCaps and D3DPTEXTURECAPS_POW2) <> 0
            then
              begin
                dwWidth := 1;
                while width > dwWidth do
                  dwWidth := dwWidth shl 1;
                dwHeight := 1;
                while height > dwHeight do
                  dwHeight := dwHeight shl 1;
              end;
          if (fD3DDeviceCaps.dpcTriCaps.dwTextureCaps and D3DPTEXTURECAPS_SQUAREONLY) <> 0
            then
              if dwWidth > dwHeight
                then dwHeight := dwWidth
                else dwWidth  := dwHeight;
        end;
      fDirectDraw.CreateSurface(ddsd, Result, nil);
    end;

  function TDDrawD3DManager.CreatePalette(palentries : pointer) : IDirectDrawPalette;
    begin
      fDirectDraw.CreatePalette(DDPCAPS_8BIT or DDPCAPS_ALLOW256, palentries, Result, nil);
    end;

  procedure InitDDrawD3D(MainWindow : HWND; ScreenWidth, ScreenHeight : integer);
    begin
      DDrawD3DMgr := TDDrawD3DManager.Create(MainWindow, ScreenWidth, ScreenHeight);
    end;

  procedure ReleaseDDrawD3D;
    begin
      DDrawD3DMgr.Free;
    end;

end.
