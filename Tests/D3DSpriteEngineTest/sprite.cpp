//-----------------------------------------------------------------------------
// File: Sprite.cpp
//
// Desc: Example code showing how to use sprites with DrawPrim along with
//       D3DX. A "sprite" is loosely defined as a 2D image that you want 
//       to transfer to the rendering target.
//
// Copyright (c) 1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

#define APP_NAME "D3DX - Sprite"

#define D3D_OVERLOADS

#define RELEASENULL(object) if (object) {object->Release();}

#include "d3dx.h"
#include "resource.h"
#include "mmsystem.h"
#include "stdio.h"

//-----------------------------------------------------------------------------
// Defines, constants, and global variables
//-----------------------------------------------------------------------------

// To allow WinMain to access the main application class, we use
// a global pointer to the application.
class CSpriteApp;

CSpriteApp* g_pSpriteApp = NULL;

const int cScreenWidth  = 1024;
const int cScreenHeight =  768;

//-----------------------------------------------------------------------------
// Name: CSpriteApp
// Desc: This class contains all the application specific state for
//       rendering the sample.
//-----------------------------------------------------------------------------
class CSpriteApp
{
public:
    CSpriteApp(HINSTANCE hInstance);
    ~CSpriteApp();

    HRESULT                 InitD3DX();
    HRESULT                 ReleaseD3DX();
    HRESULT                 InitRenderer();
    HRESULT                 LoadTexture();
    HRESULT                 RenderTest(float Alpha);
    HRESULT                 HandleModeChanges();
    
    void                    PauseDrawing();
    void                    RestartDrawing();

    BOOL                    m_bD3DXReady;
    BOOL                    m_bActive;

    HWND                    m_hwndMain;
	HINSTANCE    			m_hInstance;

    LPDIRECTDRAWSURFACE7    m_pBackBuffer;
    LPDIRECT3DDEVICE7       m_pd3dDevice;
    LPDIRECTDRAW7           m_pddraw;
    ID3DXContext*           m_pd3dx;

private:
    // Application specific state
   
  
    // Texture used to blt from
    IDirectDrawSurface7 *m_ptex;

    // Media path
    CHAR m_szPath[512];
    
}; // class CSpriteApp

//-----------------------------------------------------------------------------
// Name: CSpriteApp::CSpriteApp()
// Desc: Constructor for CSpriteApp; this method initializes
//       all the member values to good defaults. It is important
//       to NULL out any pointer that might get freed in the
//       destructor.
//-----------------------------------------------------------------------------
char v_msgstr[1000];

CSpriteApp::CSpriteApp(HINSTANCE hInstance)
{
	m_hInstance             = hInstance;
    m_bD3DXReady            = FALSE;
    m_pBackBuffer			= NULL;
    m_pd3dDevice            = NULL;
    m_pd3dx                 = NULL;
    m_pddraw                = FALSE;
   
    m_bActive               = TRUE;
    
    m_ptex                  = NULL;



    // Get media path from registry
    HKEY key;
    m_szPath[0] = '\0';

    if(ERROR_SUCCESS == RegOpenKeyEx(HKEY_LOCAL_MACHINE,
        "Software\\Microsoft\\DirectX", 0, KEY_READ, &key))
    {
        DWORD dwType;
        DWORD dwSize = sizeof(m_szPath);

        if(ERROR_SUCCESS == RegQueryValueEx( key, 
            "DXSDK Samples Path", NULL, &dwType, (BYTE*) m_szPath, &dwSize))
        {
            if(REG_SZ == dwType)
                strcat(m_szPath, "\\D3DX\\Media\\");
            else
                m_szPath[0] = '\0';
        }

        RegCloseKey(key);
    }
} // CSpriteApp::CSpriteApp

//-----------------------------------------------------------------------------
// Name: CSpriteApp::~CSpriteApp()
// Desc: Destructor for CSpriteApp; This is
//       a good time to free memory that we've allocated. Also, it's a good
//       time to release interfaces that we are holding references to. 
//-----------------------------------------------------------------------------
CSpriteApp::~CSpriteApp()
{
    ReleaseD3DX();
} // CSpriteApp::~CSpriteApp

//-----------------------------------------------------------------------------
// Name: ReleaseD3DX
// Desc: Release all the created objects.
//-----------------------------------------------------------------------------
HRESULT CSpriteApp::ReleaseD3DX()
{
    RELEASENULL(m_ptex);
    RELEASENULL(m_pddraw);
    RELEASENULL(m_pd3dDevice);
    RELEASENULL(m_pBackBuffer);
    RELEASENULL(m_pd3dx);
    D3DXUninitialize();
    return S_OK;
}

//-----------------------------------------------------------------------------
// Name: InterpretError
// Desc: Debugging helper that tries to give a helpful message when
//       something goes wrong
//-----------------------------------------------------------------------------
void InterpretError(HRESULT hr)
{
    char errStr[256];

    if(D3DXERR_NODIRECT3DDEVICEAVAILABLE  == hr) 
    {
        strcpy(errStr, "D3DXERR_NODIRECT3DDEVICEAVAILABLE\n\n"
                       "No suitable 3D device found.  "
                       "Try enabling the reference rasterizer.");
    }
    else
    {
        D3DXGetErrorString(hr, 256, errStr);
    }

    MessageBox(NULL, errStr, "D3DX Error", MB_OK);
} // ::InterpretError

//-----------------------------------------------------------------------------
// Name: CSpriteApp::PauseDrawing()
// Desc: This method is called whenever the rendering loop is paused.
//-----------------------------------------------------------------------------
void CSpriteApp::PauseDrawing()
{
    g_pSpriteApp->m_bActive = FALSE;
    if (g_pSpriteApp->m_pddraw)
        g_pSpriteApp->m_pddraw->FlipToGDISurface();
    RedrawWindow(m_hwndMain, NULL, NULL, RDW_FRAME);
    ShowCursor(TRUE);
} // PauseDrawing

//-----------------------------------------------------------------------------
// Name: CSpriteApp::RestartDrawing()
// Desc: This method is called whenever the rendering loop is restarted after
//       a pause.
//-----------------------------------------------------------------------------
void CSpriteApp::RestartDrawing()
{
    g_pSpriteApp->m_bActive = TRUE;
    ShowCursor(FALSE);
} // RestartDrawing

//-----------------------------------------------------------------------------
// Name: CSpriteApp::InitD3DX()
// Desc: This method initializes D3DX just using defaults. Much greater control
//       is possible if explicit parameters are passed.
//-----------------------------------------------------------------------------
HRESULT CSpriteApp::InitD3DX()
{
    HRESULT hr;
    
    if( FAILED( hr = D3DXInitialize()) )
        return hr;

    // Look for fastest device which supports the desired blending for sprites
    DWORD dwDevice;
    DWORD dwDeviceCount = D3DXGetDeviceCount();

    D3DX_DEVICEDESC dev;

    dev.deviceIndex = D3DX_DEFAULT;
    dev.hwLevel     = D3DX_DEFAULT;
    dev.onPrimary   = TRUE;


    for(dwDevice = 0; dwDevice < dwDeviceCount; dwDevice++)
    {
        D3DDEVICEDESC7 d3dDesc;
        D3DX_DEVICEDESC devDesc;

        if(FAILED(D3DXGetDeviceCaps(dwDevice, NULL, &d3dDesc, NULL, NULL)))
            continue;

        if(!((d3dDesc.dpcTriCaps.dwSrcBlendCaps & D3DPBLENDCAPS_SRCALPHA) &&
             (d3dDesc.dpcTriCaps.dwDestBlendCaps & D3DPBLENDCAPS_INVSRCALPHA) &&
             (d3dDesc.dpcTriCaps.dwTextureFilterCaps & D3DPTFILTERCAPS_LINEAR) &&
             (d3dDesc.dpcTriCaps.dwTextureBlendCaps & D3DPTBLENDCAPS_MODULATE)
            ))
            continue;

        if(FAILED(D3DXGetDeviceDescription(dwDevice, &devDesc)))
            continue;

        if( D3DX_DEFAULT == dev.hwLevel || 
            dev.hwLevel > devDesc.hwLevel ||
            dev.hwLevel == devDesc.hwLevel && devDesc.onPrimary )
        {
            dev = devDesc;
        }
    }

    if(D3DX_DEFAULT == dev.hwLevel)
        return D3DXERR_NODIRECT3DDEVICEAVAILABLE;

    hr = D3DXCreateContext(
            dev.hwLevel,                    // D3DX device
            D3DX_CONTEXT_FULLSCREEN,        // flags
            m_hwndMain,                     // Main window
            cScreenWidth,                   // colorbits
            cScreenHeight,					// numdepthbits
            &m_pd3dx);                      // returned D3DX interface
    if (FAILED(hr))
        return hr;
    
    m_bD3DXReady = TRUE;
    return InitRenderer();
} // InitD3DX

//-----------------------------------------------------------------------------
// Name: CSpriteApp::InitRenderer()
// Desc: This function is called to initialize the application
//       state when the device changes.
//-----------------------------------------------------------------------------
HRESULT CSpriteApp::InitRenderer()
{
    HRESULT hr;
    
    if (!m_bD3DXReady)
        return E_FAIL;
    
    m_pBackBuffer = m_pd3dx->GetBackBuffer(0);
    m_pd3dDevice = m_pd3dx->GetD3DDevice();
    if (m_pd3dDevice == NULL)
        return E_FAIL;
    
    m_pddraw = m_pd3dx->GetDD();
    if (m_pddraw == NULL)
        return E_FAIL;
    
    CHAR szTex[512];
    sprintf(szTex, "%s%s", m_szPath, "donut24.bmp");

 
    hr = m_pd3dDevice->SetRenderState(D3DRENDERSTATE_COLORKEYENABLE,     TRUE);
    if (FAILED(hr))
        return hr;
    // Enable dither, specular, lighting and z-buffer usage
    hr = m_pd3dDevice->SetRenderState(D3DRENDERSTATE_DITHERENABLE,      FALSE);
    if (FAILED(hr))
        return hr;
    hr = m_pd3dDevice->SetRenderState(D3DRENDERSTATE_SPECULARENABLE,    FALSE);
    if (FAILED(hr))
        return hr;
    hr = m_pd3dDevice->SetRenderState(D3DRENDERSTATE_LIGHTING,          FALSE);
    if (FAILED(hr))
        return hr;
    hr = m_pd3dDevice->SetRenderState(D3DRENDERSTATE_ZENABLE,           FALSE);
    if (FAILED(hr))
        return hr;
    
    // Enable vertices to have colors 
    hr = m_pd3dDevice->SetRenderState(D3DRENDERSTATE_COLORVERTEX,       FALSE);
    if (FAILED(hr))
        return hr;
    hr = m_pd3dDevice->SetRenderState(D3DRENDERSTATE_DIFFUSEMATERIALSOURCE,     FALSE);
    if (FAILED(hr))
        return hr;
    
    // Set the background to bright blue (red/green/blue/alpha)
    hr = m_pd3dx->SetClearColor((D3DCOLOR) D3DRGBA(0.0f, 0.0f, 1.0f, 1.0f));
    if (FAILED(hr))
        return hr;
    
    hr = m_pd3dx->Clear(D3DCLEAR_TARGET | D3DCLEAR_ZBUFFER);
    if (FAILED(hr))
        return hr;
    
    return S_OK;
} // InitRenderer

HRESULT CSpriteApp::LoadTexture()
{
	HRESULT        hr;
	DDCOLORKEY     colorkey;
	DDSURFACEDESC2 surfacedesc;

	/*
	OPENFILENAME ofn;
	memset(&ofn, 0, sizeof(ofn)); 
	ofn.lStructSize = sizeof(ofn);
	ofn.hInstance = m_hInstance;
	ofn.lpstrDefExt = "BMP";
	ofn.lpstrInitialDir = "TestImages";
	ofn.lpstrFile = new char[1000];
	ofn.lpstrFile[0] = '\0';
	ofn.nMaxFile = 1000;
	if (GetOpenFileName(&ofn))
	{*/

       hr = D3DXCreateTextureFromFile(
                   m_pd3dDevice,
				   NULL,                   // dwFlags
				   NULL,                   // auto-width
				   NULL,                   // auto-height
				   NULL,                   // auto-surface type
				   NULL,                   // pointer to Palette
				   &m_ptex,                // returned pointer to texture
				   NULL,                   // returned number of mip-maps
				   "TestImages\\mine2.bmp",          // file name for texture
				   D3DX_FT_DEFAULT);       // default scaling
	   if (SUCCEEDED(hr))
	   { 
           memset(&surfacedesc, 0, sizeof(surfacedesc));
           surfacedesc.dwSize = sizeof(surfacedesc);
		   if (SUCCEEDED(m_ptex->Lock(NULL, &surfacedesc, DDLOCK_WAIT, NULL)))
             {
		       colorkey.dwColorSpaceHighValue = colorkey.dwColorSpaceLowValue = *(int*)surfacedesc.lpSurface;
               m_ptex->Unlock(NULL);
             }
		   else 
			 colorkey.dwColorSpaceHighValue = colorkey.dwColorSpaceLowValue = 0;
		   m_ptex->SetColorKey(DDCKEY_SRCBLT, &colorkey);
	   }

       return hr;
	/*
	else return S_FALSE;
	delete ofn.lpstrFile;*/
}

//-----------------------------------------------------------------------------
// Name: 
// Desc: 
//       
//-----------------------------------------------------------------------------

const int cLoopCount = 1000;

HRESULT CSpriteApp::RenderTest(float Alpha)
{
    HRESULT hr;
    
    if (m_ptex == NULL)
		return E_FAIL;
    if (!m_bD3DXReady)
        return E_FAIL;
    
    if (!m_bActive)
        return S_OK;
    
    if( SUCCEEDED( hr = m_pd3dDevice->BeginScene() ) )
    {
        m_pd3dx->Clear(D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER);
    
        // We need to setup the rasterizer for rendering sprites;
        // this only needs to be done once for all the sprites that
        // are rendered; however this function does need to be called
        // again if any render state changes are made outside of the
        // bltsprite call.
        ::D3DXPrepareDeviceForSprite(m_pd3dDevice, FALSE);
        
      
        // Get our current viewport
        D3DVIEWPORT7 viewport;
        hr = m_pd3dDevice->GetViewport(&viewport);
        if (FAILED(hr))
            return hr;
        
        // Convert the viewport into a proper Rect
        RECT rectViewport;
        rectViewport.left   = viewport.dwX;
        rectViewport.top    = viewport.dwY;
        rectViewport.right  = viewport.dwX + viewport.dwWidth;
        rectViewport.bottom = viewport.dwY + viewport.dwHeight;
        
        // Our non-rotated render target should be centered in the viewport;
        D3DXVECTOR3 pointDest; 
        pointDest.x = (float)viewport.dwX + (float)viewport.dwWidth  / 2.0f;
        pointDest.y = (float)viewport.dwY + (float)viewport.dwHeight / 2.0f;
        pointDest.z = 0.0f;
        
        
        int initialtickcount = GetTickCount();
        // Go ahead and do the render
        int i;
        if (Alpha == 1.0f)
          for (i = 0; i < cLoopCount; i++)
          {
		    pointDest.x = rand() % cScreenWidth;
			pointDest.y = rand() % cScreenHeight;
            hr = m_pBackBuffer->BltFast(
                                        pointDest.x, 
										pointDest.y, 
										m_ptex,             // texture
                                        NULL,				// src sub rect
							            DDBLTFAST_DONOTWAIT
                                       );     
            if (FAILED(hr))
              return hr;
          }
        else
          for (i = 0; i < cLoopCount; i++)
          {
		    pointDest.x = rand() % cScreenWidth;
			pointDest.y = rand() % cScreenHeight;
            hr = (
                m_ptex,             // texture
                m_pd3dDevice,       // 3D device
                &pointDest,         // destination point (center)
                Alpha,				// alpha
                1.0f,				// scale
                0.0f,				// rotation
                NULL,               // offset
                NULL				// src sub rect
                );     
            if (FAILED(hr))
              return hr;
          }
        m_pd3dDevice->EndScene();
        hr = m_pd3dx->UpdateFrame(D3DX_DEFAULT);
        if ( hr == DDERR_SURFACELOST || hr == DDERR_SURFACEBUSY )
            hr = HandleModeChanges();
		int elapsedticks = GetTickCount() - initialtickcount;
		char numstr[33];
		itoa(i, numstr, 10);
		strcpy(v_msgstr, numstr);
		strcat(v_msgstr, " images rendered in ");
		itoa(elapsedticks, numstr, 10);
		strcat(v_msgstr, numstr);
		strcat(v_msgstr, " milliseconds.");
		//MessageBox(m_hwndMain, v_msgstr, "Report", MB_OK);
		OutputDebugString(v_msgstr);
    }
    

    return hr;
} // Draw

//-----------------------------------------------------------------------------
// Name: HandleWindowedModeChanges
// Desc: Handle mode changes 
//       
//-----------------------------------------------------------------------------
HRESULT CSpriteApp::HandleModeChanges()
{
    HRESULT hr;
    hr = m_pddraw->TestCooperativeLevel();

    if( SUCCEEDED( hr ) )
    {
        // This means that mode changes had taken place, surfaces
        // were lost but still we are in the original mode, so we
        // simply restore all surfaces and keep going.
        if( FAILED( m_pddraw->RestoreAllSurfaces() ) )
            return hr;

        // Load the texture back from the file
        CHAR szTex[512];
        sprintf(szTex, "%s%s", m_szPath, "donut24.bmp");

        hr = D3DXLoadTextureFromFile(
             m_pd3dDevice,
             m_ptex,                 // texture
             D3DX_DEFAULT,           // Num Mipmaps
             szTex,                  // file name for texture
             NULL,                   // srcRect
             NULL,                   // dstRect
             D3DX_FT_DEFAULT);       // default scaling

        if (FAILED(hr))
            return hr;
    
    }
    else if( hr == DDERR_WRONGMODE )
    {
        // This means that the desktop mode has changed
        // we can destroy and recreate everything back again.
        if(FAILED(hr = ReleaseD3DX()))
            return hr;
        if(FAILED(hr = InitD3DX()))
            return hr;
    }
    else if( hr == DDERR_EXCLUSIVEMODEALREADYSET )
    {
        // This means that some app took exclusive mode access
        // we need to sit in a loop till we get back to the right mode.
        do
        {
            Sleep( 500 );
        } while( DDERR_EXCLUSIVEMODEALREADYSET == 
                 (hr = m_pddraw->TestCooperativeLevel()) );
        if( SUCCEEDED( hr ) )
        {
            // This means that the exclusive mode app relinquished its 
            // control and we are back to the safe mode, so simply restore
            if( FAILED( m_pddraw->RestoreAllSurfaces() ) )
                return hr;
            // Load the texture back from the file
            CHAR szTex[512];
            sprintf(szTex, "%s%s", m_szPath, "donut24.bmp");
            
            hr = D3DXLoadTextureFromFile(
                m_pd3dDevice,
                m_ptex,                 // texture
                D3DX_DEFAULT,           // Num Mipmaps
                szTex,                  // file name for texture
                NULL,                   // srcRect
                NULL,                   // dstRect
                D3DX_FT_DEFAULT);       // default scaling
            
            if (FAILED(hr))
                return hr;
        }
        else if( DDERR_WRONGMODE == hr )
        {
            // This means that the exclusive mode app relinquished its 
            // control BUT we are back to some strange mode, so destroy
            // and recreate.
            if(FAILED(hr = ReleaseD3DX()))
                return hr;
            if(FAILED(hr = InitD3DX()))
                return hr;
        }
        else
        {
            // Busted!!
            return hr;
        }
    }
    else
    {
        // Busted!!
        return hr;
    }
    return S_OK;
}


//-----------------------------------------------------------------------------
// Name: 
// Desc: 
//       
//-----------------------------------------------------------------------------
LRESULT CALLBACK WndProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
    switch(uMsg)
    {
    case WM_CLOSE:
        PostQuitMessage(0);
        break;
    case WM_SIZE:
        if (g_pSpriteApp && 
                g_pSpriteApp->m_bD3DXReady && 
                LOWORD(lParam) > 0 && 
                HIWORD(lParam) > 0)
        {
            HRESULT hr;

            hr = g_pSpriteApp->m_pd3dx->Resize(LOWORD(lParam),HIWORD(lParam));

            if (FAILED(hr))
            {
                InterpretError(hr);
                g_pSpriteApp->m_bD3DXReady = FALSE;
                PostQuitMessage(0);
            }
            
        }
        break;
    case WM_KEYDOWN:
        switch (wParam)
        {
		case VK_F3:
			g_pSpriteApp->LoadTexture();
			break;
		case VK_F4:
			g_pSpriteApp->RenderTest(1.0f);
			break;
		case VK_F5:
			g_pSpriteApp->RenderTest(0.5f);
			break;
        case VK_ESCAPE:
            PostQuitMessage(0);
        }
        break;
    default:
        break;
    }
   
    return DefWindowProc(hwnd,uMsg,wParam,lParam);
    
} // WndProc

//-----------------------------------------------------------------------------
// Name: 
// Desc: 
//       
//-----------------------------------------------------------------------------
int PASCAL WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, 
                   LPSTR lpszCmdLine, int nCmdShow) 
{
    HRESULT     hr;	
    MSG         msg; 
    WNDCLASS    wc; 
    HCURSOR     hcur = NULL;
    int         ret = 0;

    // Allocate a new instance of our SpriteApp object
    g_pSpriteApp = new CSpriteApp(hInstance); // set up our data AFTER starting up d3dx!
    if (!g_pSpriteApp)
    {
        ret = -1;
        goto Exit;
    }
    
    // Register the window class for the main window. 
    if (!hPrevInstance) 
    { 
        hcur = CopyCursor(LoadCursor(NULL, IDC_ARROW));

        wc.style = 0; 
        wc.lpfnWndProc = (WNDPROC) WndProc; 
        wc.cbClsExtra = 0; 
        wc.cbWndExtra = 0; 
        wc.hInstance = hInstance; 
        wc.hIcon = LoadIcon(hInstance, MAKEINTRESOURCE(IDI_APP_ICON));
        wc.hCursor = hcur;
        wc.hbrBackground = (HBRUSH)GetStockObject(BLACK_BRUSH); 
        wc.lpszMenuName =  NULL;
        wc.lpszClassName = APP_NAME; 
        
        if (!RegisterClass(&wc)) 
        {
            ret = -1;
            goto Exit;
        }
    } 
    
    // Create the main window. 
    g_pSpriteApp->m_hwndMain = CreateWindow(
            APP_NAME, 
            APP_NAME, 
            WS_OVERLAPPEDWINDOW, 
            0, 
            0, 
            cScreenWidth, 
            cScreenHeight, 
            (HWND) NULL, 
            (HMENU) NULL, 
            hInstance,
            (LPVOID) NULL); 
    
    // If the main window cannot be created, terminate 
    // the application. 
    if (!g_pSpriteApp->m_hwndMain)
    {
        ret = -1;
        goto Exit;
    }
    
    // Show the window and paint its contents. 
    ShowWindow(g_pSpriteApp->m_hwndMain, nCmdShow); 
    UpdateWindow(g_pSpriteApp->m_hwndMain); 

    // Init the D3DX portion of the SpriteApp object
    hr = g_pSpriteApp->InitD3DX();
    if (FAILED(hr))
    {
        InterpretError(hr);
        ret = -1;
        goto Exit;
    }
    
    // Start the message loop. 
    
    // Now we're ready to receive and process Windows messages.
    while (GetMessage(&msg, NULL, 0U, 0U))
    {
        
        TranslateMessage(&msg);
        DispatchMessage(&msg);
	}
    delete g_pSpriteApp; // clean up our data BEFORE shutting down d3dx!
 
	MessageBox(NULL, v_msgstr, "Report", MB_OK);
Exit:
    if(hcur)
        DestroyCursor(hcur);

    return ret;
} // WinMain

// End Of File

