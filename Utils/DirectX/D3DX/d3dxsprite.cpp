#include "stdafx.h"

#include "d3dx.h"

struct _D3DXVECTOR2
  {
    float x, y;
  };

struct _D3DXVECTOR3
  {
    float x, y, z;
  };

struct _D3DXVECTOR4
  {
    float x, y, z, w;
  };

struct _D3DXMATRIX
  {
    float m00, m01, m02, m03;
    float m10, m11, m12, m13;
    float m20, m21, m22, m23;
    float m30, m31, m32, m33;
  };

extern "C"
{

  _declspec(dllexport) HRESULT _cdecl _D3DXPrepareDeviceForSprite( LPDIRECT3DDEVICE7 pd3dDevice, BOOL ZEnable)
    {
      return D3DXPrepareDeviceForSprite(pd3dDevice, ZEnable);
    }

  _declspec(dllexport) HRESULT _cdecl _D3DXDrawSpriteSimple(LPDIRECTDRAWSURFACE7  pd3dTexture, 
														    LPDIRECT3DDEVICE7     pd3dDevice, 
														    _D3DXVECTOR3           *ppointDest, 
														    float                 alpha,
														    float                 scale,
														    float                 angleRad,
														    _D3DXVECTOR2           *pOffset,
														    RECT                  *pSourceRect)
    {
      D3DXVECTOR3 pointDest(ppointDest->x, ppointDest->y, ppointDest->z);
      if (pOffset)
        {
          D3DXVECTOR2 Offset(pOffset->x, pOffset->y);
          return D3DXDrawSpriteSimple(pd3dTexture, pd3dDevice, &pointDest, alpha, scale, angleRad, &Offset, pSourceRect);
        }
      else return D3DXDrawSpriteSimple(pd3dTexture, pd3dDevice, &pointDest, alpha, scale, angleRad, NULL, pSourceRect);
    }

  _declspec(dllexport) HRESULT _cdecl _D3DXDrawSpriteTransform(LPDIRECTDRAWSURFACE7  pd3dTexture, 
															   LPDIRECT3DDEVICE7     pd3dDevice, 
															   _D3DXMATRIX            *pMatrixTransform, 
															   float                 alpha,
															   RECT                  *pSourceRect)
    {
      D3DXMATRIX MatrixTransform(pMatrixTransform->m00, pMatrixTransform->m01, pMatrixTransform->m02, pMatrixTransform->m03, 
								 pMatrixTransform->m10, pMatrixTransform->m11, pMatrixTransform->m12, pMatrixTransform->m13,
								 pMatrixTransform->m20, pMatrixTransform->m21, pMatrixTransform->m22, pMatrixTransform->m23,
								 pMatrixTransform->m30, pMatrixTransform->m31, pMatrixTransform->m32, pMatrixTransform->m33);
      return D3DXDrawSpriteTransform(pd3dTexture, pd3dDevice, &MatrixTransform, alpha, pSourceRect);
    }

  _declspec(dllexport) void _cdecl _D3DXBuildSpriteTransform(_D3DXMATRIX            *pMatrix,
															 RECT                  *prectDest,
															 float                 angleRad,
															 _D3DXVECTOR2           *pOffset)
    {
      D3DXMATRIX Matrix(pMatrix->m00, pMatrix->m01, pMatrix->m02, pMatrix->m03, 
						pMatrix->m10, pMatrix->m11, pMatrix->m12, pMatrix->m13,
						pMatrix->m20, pMatrix->m21, pMatrix->m22, pMatrix->m23,
						pMatrix->m30, pMatrix->m31, pMatrix->m32, pMatrix->m33);
      D3DXVECTOR2 Offset(pOffset->x, pOffset->y);
      D3DXBuildSpriteTransform(&Matrix, prectDest, angleRad, &Offset);
    }

  _declspec(dllexport) HRESULT _cdecl _D3DXDrawSprite3D(LPDIRECTDRAWSURFACE7  pd3dTexture, 
														LPDIRECT3DDEVICE7     pd3dDevice, 
														_D3DXVECTOR4           quad[4], 
														float                 alpha,
														RECT                  *pSourceRect)
    {
      D3DXVECTOR4 lquad[4];
      lquad[0].x = quad[0].x;
      lquad[0].y = quad[0].y;
      lquad[0].z = quad[0].z;
      lquad[0].w = quad[0].w;
      lquad[1].x = quad[1].x;
      lquad[1].y = quad[1].y;
      lquad[1].z = quad[1].z;
      lquad[1].w = quad[1].w;
      lquad[2].x = quad[2].x;
      lquad[2].y = quad[2].y;
      lquad[2].z = quad[2].z;
      lquad[2].w = quad[2].w;
      lquad[3].x = quad[3].x;
      lquad[3].y = quad[3].y;
      lquad[3].z = quad[3].z;
      lquad[3].w = quad[3].w;
      return D3DXDrawSprite3D(pd3dTexture, pd3dDevice, lquad, alpha, pSourceRect);
    }

}