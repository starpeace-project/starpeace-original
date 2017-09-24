#include "stdafx.h"
#include "d3dx.h"

extern "C"
{

  ///////////////////////////////////////////////////////////////////////////
  // APIs: 
  ///////////////////////////////////////////////////////////////////////////

  _declspec(dllexport) HRESULT _cdecl _D3DXInitialize()
    {
      return D3DXInitialize();
    }

  _declspec(dllexport) HRESULT _cdecl _D3DXUninitialize()
    {
      return D3DXUninitialize();
    }

  _declspec(dllexport) DWORD _cdecl _D3DXGetDeviceCount()
    {
      return D3DXGetDeviceCount();
    }

  _declspec(dllexport) HRESULT _cdecl _D3DXGetDeviceDescription(DWORD            deviceIndex, 
                                                                D3DX_DEVICEDESC* pd3dxDeviceDesc)
    {
      return D3DXGetDeviceDescription(deviceIndex, pd3dxDeviceDesc);
    }

  _declspec(dllexport) DWORD _cdecl _D3DXGetMaxNumVideoModes(DWORD       deviceIndex, 
															 DWORD       flags)
    {
      return D3DXGetMaxNumVideoModes(deviceIndex, flags);
    }

  _declspec(dllexport) HRESULT _cdecl _D3DXGetVideoMode(DWORD             deviceIndex, 
													    DWORD             flags, 
													    DWORD             modeIndex, 
													    D3DX_VIDMODEDESC* pModeDesc)
    {
      return D3DXGetVideoMode(deviceIndex, flags, modeIndex, pModeDesc);
    }

  _declspec(dllexport) DWORD _cdecl _D3DXGetMaxSurfaceFormats(DWORD             deviceIndex, 
															  D3DX_VIDMODEDESC* pDesc,
														 	  DWORD             surfClassFlags)
    {
      return D3DXGetMaxSurfaceFormats(deviceIndex, pDesc, surfClassFlags);
    }

  _declspec(dllexport) HRESULT _cdecl _D3DXGetSurfaceFormat(DWORD               deviceIndex,
														    D3DX_VIDMODEDESC*   pDesc,
														    DWORD               surfClassFlags,                   
														    DWORD               surfaceIndex, 
														    D3DX_SURFACEFORMAT* pFormat)
    {
      return D3DXGetSurfaceFormat(deviceIndex, pDesc, surfClassFlags, surfaceIndex, pFormat);
    }

  _declspec(dllexport) HRESULT _cdecl _D3DXGetCurrentVideoMode(DWORD             deviceIndex, 
															   D3DX_VIDMODEDESC* pVidMode)
    {
      return D3DXGetCurrentVideoMode(deviceIndex, pVidMode);
    }

  _declspec(dllexport) HRESULT _cdecl _D3DXGetDeviceCaps(DWORD             deviceIndex, 
														 D3DX_VIDMODEDESC* pVidMode,
														 D3DDEVICEDESC7*   pD3DCaps,
														 DDCAPS*           pDDHALCaps,
														 DDCAPS*           pDDHELCaps)
    {
      return D3DXGetDeviceCaps(deviceIndex, pVidMode, pD3DCaps, pDDHALCaps, pDDHELCaps);
    }

  _declspec(dllexport) HRESULT _cdecl _D3DXCreateContext(DWORD          deviceIndex,  
														 DWORD          flags,
														 HWND           hwnd,
														 DWORD          width, 
														 DWORD          height,
														 LPD3DXCONTEXT* ppCtx)
    {
      return D3DXCreateContext(deviceIndex, flags, hwnd, width, height, ppCtx);
    }

  _declspec(dllexport) HRESULT _cdecl _D3DXCreateContextEx(DWORD          deviceIndex,  
														   DWORD          flags,
														   HWND           hwnd,
														   HWND           hwndFocus, 
														   DWORD          numColorBits,
														   DWORD          numAlphaBits,
														   DWORD          numDepthbits,
														   DWORD          numStencilBits,
														   DWORD          numBackBuffers,
														   DWORD          width, 
														   DWORD          height,
														   DWORD          refreshRate,
														   LPD3DXCONTEXT* ppCtx)
    {
      return D3DXCreateContextEx(deviceIndex, flags, hwnd, hwndFocus, numColorBits, numAlphaBits, numDepthbits, numStencilBits, numBackBuffers, width, height, refreshRate, ppCtx);
    }


  _declspec(dllexport) void _cdecl _D3DXGetErrorString(HRESULT hr, 
													   DWORD   strLength, 
													   LPSTR   pStr)
    {
      D3DXGetErrorString(hr, strLength, pStr);
    }

  _declspec(dllexport) HRESULT _cdecl _D3DXMakeDDPixelFormat(D3DX_SURFACEFORMAT d3dxFormat, 
															 DDPIXELFORMAT*     pddpf)
    {
      return D3DXMakeDDPixelFormat(d3dxFormat, pddpf);
    }

  _declspec(dllexport) D3DX_SURFACEFORMAT _cdecl _D3DXMakeSurfaceFormat(DDPIXELFORMAT* pddpf)
    {
      return D3DXMakeSurfaceFormat(pddpf);
    }

  ///////////////////////////////////////////////////////////////////////////
  // Texturing APIs:
  ///////////////////////////////////////////////////////////////////////////

  _declspec(dllexport) HRESULT _cdecl _D3DXCheckTextureRequirements(LPDIRECT3DDEVICE7     pd3dDevice,
																	LPDWORD               pFlags, 
																	LPDWORD               pWidth,  
																	LPDWORD               pHeight,  
																	D3DX_SURFACEFORMAT*   pPixelFormat)
    {
      return D3DXCheckTextureRequirements(pd3dDevice, pFlags, pWidth, pHeight, pPixelFormat);
    }

  _declspec(dllexport) HRESULT _cdecl _D3DXCreateTexture(LPDIRECT3DDEVICE7     pd3dDevice,
														 LPDWORD               pFlags, 
														 LPDWORD               pWidth,  
														 LPDWORD               pHeight,  
														 D3DX_SURFACEFORMAT*   pPixelFormat,
														 LPDIRECTDRAWPALETTE   pDDPal,
														 LPDIRECTDRAWSURFACE7* ppDDSurf,
														 LPDWORD               pNumMipMaps)
    {
      return D3DXCreateTexture(pd3dDevice, pFlags, pWidth, pHeight, pPixelFormat, pDDPal, ppDDSurf, pNumMipMaps);
    }

  _declspec(dllexport) HRESULT _cdecl _D3DXCreateCubeMapTexture(LPDIRECT3DDEVICE7     pd3dDevice,
																LPDWORD               pFlags, 
																DWORD                 cubefaces,
																D3DCOLOR              colorEmptyFaces,
																LPDWORD               pWidth,  
																LPDWORD               pHeight,  
																D3DX_SURFACEFORMAT    *pPixelFormat,
																LPDIRECTDRAWPALETTE   pDDPal,
																LPDIRECTDRAWSURFACE7* ppDDSurf,
																LPDWORD               pNumMipMaps)
    {
      return D3DXCreateCubeMapTexture(pd3dDevice, pFlags, cubefaces, colorEmptyFaces, pWidth, pHeight, pPixelFormat, pDDPal, ppDDSurf, pNumMipMaps);
    }

  _declspec(dllexport) HRESULT _cdecl _D3DXCreateTextureFromFile(LPDIRECT3DDEVICE7     pd3dDevice,
																 LPDWORD               pFlags, 
																 LPDWORD               pWidth,  
																 LPDWORD               pHeight,  
																 D3DX_SURFACEFORMAT*   pPixelFormat,
																 LPDIRECTDRAWPALETTE   pDDPal,
																 LPDIRECTDRAWSURFACE7* ppDDSurf,
																 LPDWORD               pNumMipMaps,
																 LPSTR                 pSrcName,
																 D3DX_FILTERTYPE       filterType)
    {
      return D3DXCreateTextureFromFile(pd3dDevice, pFlags, pWidth, pHeight, pPixelFormat, pDDPal, ppDDSurf, pNumMipMaps, pSrcName, filterType);
    }

  _declspec(dllexport) HRESULT _cdecl _D3DXLoadTextureFromFile(LPDIRECT3DDEVICE7    pd3dDevice,
															   LPDIRECTDRAWSURFACE7 pTexture,
															   DWORD                mipMapLevel,
															   LPSTR                pSrcName, 
															   RECT*                pSrcRect, 
															   RECT*                pDestRect,
															   D3DX_FILTERTYPE      filterType)
    {
      return D3DXLoadTextureFromFile(pd3dDevice, pTexture, mipMapLevel, pSrcName, pSrcRect, pDestRect, filterType);
    }

  _declspec(dllexport) HRESULT _cdecl _D3DXLoadTextureFromSurface(LPDIRECT3DDEVICE7    pd3dDevice,
																  LPDIRECTDRAWSURFACE7 pTexture,
																  DWORD                mipMapLevel,
																  LPDIRECTDRAWSURFACE7 pSurfaceSrc, 
																  RECT*                pSrcRect, 
																  RECT*                pDestRect,
																  D3DX_FILTERTYPE      filterType)
    {
      return D3DXLoadTextureFromSurface(pd3dDevice, pTexture, mipMapLevel, pSurfaceSrc, pSrcRect, pDestRect, filterType);
    }

  _declspec(dllexport) HRESULT _cdecl _D3DXLoadTextureFromMemory(LPDIRECT3DDEVICE7    pd3dDevice, 
																 LPDIRECTDRAWSURFACE7 pTexture,
																 DWORD                mipMapLevel, 
																 LPVOID               pMemory,
																 LPDIRECTDRAWPALETTE  pDDPal,
																 D3DX_SURFACEFORMAT   srcPixelFormat,
																 DWORD                srcPitch,
																 RECT*                pDestRect,
																 D3DX_FILTERTYPE      filterType)
    {
      return D3DXLoadTextureFromMemory(pd3dDevice, pTexture, mipMapLevel, pMemory, pDDPal, srcPixelFormat, srcPitch, pDestRect, filterType);
    }

}
