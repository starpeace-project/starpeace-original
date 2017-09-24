unit SurfaceUtils;

interface

  uses
    DirectDraw, Direct3D;

  function CreateCompatibleOffscreenSurface(width, height : integer) : IDirectDrawSurface7;
  function CreateOffscreenSurface(width, height, bppixel : integer) : IDirectDrawSurface7;
  function CreateTextureSurface(width, height, bppixel : integer; managed : boolean) : IDirectDrawSurface7;
  function CreatePalette(palentries : pointer) : IDirectDrawPalette;

  var
    DirectDrawObj : IDirectDraw7;
    DeviceCaps    : TD3DDeviceDesc7;

implementation

  uses
    Windows, DirectDrawUtils;

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

  function CreateCompatibleOffscreenSurface(width, height : integer) : IDirectDrawSurface7;
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
      DirectDrawObj.CreateSurface(ddsd, Result, nil);
    end;

  function CreateOffscreenSurface(width, height, bppixel : integer) : IDirectDrawSurface7;
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
      DirectDrawObj.CreateSurface(ddsd, Result, nil);
    end;

  function CreateTextureSurface(width, height, bppixel : integer; managed : boolean) : IDirectDrawSurface7;
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
          if (DeviceCaps.dpcTriCaps.dwTextureCaps and D3DPTEXTURECAPS_POW2) <> 0
            then
              begin
                dwWidth := 1;
                while width > dwWidth do
                  dwWidth := dwWidth shl 1;
                dwHeight := 1;
                while height > dwHeight do
                  dwHeight := dwHeight shl 1;
              end;
          if (DeviceCaps.dpcTriCaps.dwTextureCaps and D3DPTEXTURECAPS_SQUAREONLY) <> 0
            then
              if dwWidth > dwHeight
                then dwHeight := dwWidth
                else dwWidth  := dwHeight;
        end;
      DirectDrawObj.CreateSurface(ddsd, Result, nil);
    end;

  function CreatePalette(palentries : pointer) : IDirectDrawPalette;
    begin
      DirectDrawObj.CreatePalette(DDPCAPS_8BIT or DDPCAPS_ALLOW256, palentries, Result, nil);
    end;

end.
