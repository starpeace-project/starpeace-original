unit D3DXCore;

  ///////////////////////////////////////////////////////////////////////////
  //
  //  Copyright (C) 1999 Microsoft Corporation.  All Rights Reserved.
  //
  //  File:       d3dxcore.pas
  //  Content:    D3DX core types and functions
  //
  ///////////////////////////////////////////////////////////////////////////

interface 

  uses
    Windows, Math, DirectDraw, Direct3D, {Limits} D3DXErr;

  const
    IID_ID3DXContext : TGuid = '{9B74ED7A-BBEF-11d2-9F8E-0000F8080835}';

  ///////////////////////////////////////////////////////////////////////////
  // Interfaces:
  ///////////////////////////////////////////////////////////////////////////

  //-------------------------------------------------------------------------
  // ID3DXContext interface:
  //
  // This encapsulates all the stuff that the app might
  // want to do at initialization time and any global control over d3d and
  // ddraw.
  //-------------------------------------------------------------------------

  type
    ID3DXContext =
      interface(IUnknown)
        ['{9B74ED7A-BBEF-11d2-9F8E-0000F8080835}']
        // Get the DDraw and Direct3D objects to call DirectDraw or
        // Direct3D Immediate Mode functions.
        // If the objects don't exist (because they have not
        // been created for some reason) NULL is returned.
        // All the objects returned in the following Get* functions
        // are addref'ed. It is the application's responsibility to
        // release them when no longer needed.
        function GetDD : pointer; stdcall;
        function GetD3D : pointer; stdcall;
        function GetD3DDevice : pointer; stdcall;

        // Get the various buffers that get created at the init time
        // These are addref'ed as well. It is the application's responsibility
        // to release them before the app quits or when it needs a resize.
        function GetPrimary : pointer; stdcall;
        function GetZBuffer : pointer; stdcall;
        function GetBackBuffer(which : dword) : pointer; stdcall;

        // Get the associated window handles
        function GetWindow : HWND; stdcall;
        function GetFocusWindow : HWND; stdcall;

        //
        // Various Get methods, in case the user had specified default
        // parameters
        //
        function GetDeviceIndex(out DeviceIndex, HwLevel : dword) : HRESULT; stdcall;

        function GetNumBackBuffers : dword; stdcall;

        function GetNumBits(out ColorBits, DepthBits, AlphaBits, StencilBits : dword) : HRESULT; stdcall;

        function GetBufferSize(out Width, Height : dword) : HRESULT; stdcall;

        // Get the flags that were used to create this context
        function GetCreationFlags : dword; stdcall;
        function GetRefreshRate : dword; stdcall;

        // Restoring surfaces in case stuff is lost
        function RestoreSurfaces: HRESULT; stdcall;

        // Resize all the buffers to the new width and height
        function Resize(width, height : dword) : HRESULT; stdcall;

        // Update the frame using a flip or a blit,
        // If the D3DX_UPDATE_NOVSYNC flag is set, blit is used if the
        // driver cannot flip without waiting for vsync in full-screen mode.
        function UpdateFrame(flags : dword) : HRESULT; stdcall;

        // Render a string at the specified coordinates, with the specified
        // colour. This is only provided as a convenience for
        // debugging/information during development.
        // topLeftX and topLeftY represent the location of the top left corner
        // of the string, on the render target.
        // The coordinate and color parameters each have a range of 0.0-1.0
        function DrawDebugText(topLeftX, topLeftY : single; color : TD3DCOLOR; pString : pchar) : HRESULT; stdcall;

        // Clears to the current viewport
        // The following are the valid flags:
        // D3DCLEAR_TARGET  (to clear the render target )
        // D3DCLEAR_ZBUFFER (to clear the depth-buffer )
        // D3DCLEAR_STENCIL (to clear the stencil-buffer )
        function Clear(ClearFlags : dword) : HRESULT; stdcall;

        function SetClearColor(color : TD3DCOLOR) : HRESULT; stdcall;
        function SetClearDepth(z : single) : HRESULT; stdcall;
        function SetClearStencil(stencil : dword) : HRESULT; stdcall;
      end;

  ///////////////////////////////////////////////////////////////////////////
  // Defines and Enumerators used below:
  ///////////////////////////////////////////////////////////////////////////

  //-------------------------------------------------------------------------
  // D3DX_DEFAULT:
  // ---------
  // A predefined value that could be used for any parameter in D3DX APIs or
  // member functions that is an enumerant or a handle.  The D3DX
  // documentation indicates wherever D3DX_DEFAULT may be used,
  // and how it will be interpreted in each situation.
  //-------------------------------------------------------------------------

  const
    D3DX_DEFAULT = $FFFFFFFF;

  //-------------------------------------------------------------------------
  // D3DX_DEFAULT_FLOAT:
  // ------------------
  // Similar to D3DX_DEFAULT, but used for floating point parameters.
  // The D3DX documentation indicates wherever D3DX_DEFAULT_FLOAT may be used,
  // and how it will be interpreted in each situation.
  //-------------------------------------------------------------------------

  const
    D3DX_DEFAULT_FLOAT = MaxSingle;

  //-------------------------------------------------------------------------
  // Hardware Acceleration Level:
  // ---------------------------
  // These constants represent pre-defined hardware acceleration levels,
  // and may be used as a default anywhere a (DWORD) deviceIndex is required.
  // Each pre-define indicates a different level of hardware acceleration.
  // They are an alternative to using explicit deviceIndices retrieved by
  // D3DXGetDeviceDescription().
  //
  // The only case these pre-defines should be used as device indices is if
  // a particular level of acceleration is required, and given more than
  // one capable device on the computer, it does not matter which one
  // is used.
  //
  // The method of selection is as follows: If one of the D3DX devices on
  // the primary DDraw device supports a requested hardware acceleration
  // level, it will be used. Otherwise, the first matching device discovered
  // by D3DX will be used.
  //
  // Of course, it is possible for no match to exist for any of the
  // pre-defines on a particular computer.  Passing such a value into the
  // D3DX apis will simply cause them to fail, reporting that no match
  // is available.
  //
  // D3DX_HWLEVEL_NULL:      Null implementation (draws nothing)
  // D3DX_HWLEVEL_REFERENCE: Reference implementation (slowest)
  // D3DX_HWLEVEL_2D:        2D acceleration only (RGB rasterizer used)
  // D3DX_HWLEVEL_RASTER:    Rasterization acceleration (likely most useful)
  // D3DX_HWLEVEL_TL:        Transform and lighting acceleration
  // D3DX_DEFAULT:           The highest level of acceleration available
  //                         on the primary DDraw device.
  //-------------------------------------------------------------------------

  const
    D3DX_HWLEVEL_NULL      = D3DX_DEFAULT - 1;
    D3DX_HWLEVEL_REFERENCE = D3DX_DEFAULT - 2;
    D3DX_HWLEVEL_2D        = D3DX_DEFAULT - 3;
    D3DX_HWLEVEL_RASTER    = D3DX_DEFAULT - 4;
    D3DX_HWLEVEL_TL        = D3DX_DEFAULT - 5;

  //-------------------------------------------------------------------------
  // Surface Class:
  // -------------
  // These are the various types of 2D-surfaces classified according to their
  // usage. Note that a number of them overlap. e.g. STENCILBUFFERS and
  // DEPTHBUFFERS overlap (since in DX7 implementation the stencil and depth
  // bits are part of the same pixel format).
  //
  // Mapping to the DX7 DDPIXELFORMAT concepts:
  // -----------------------------------------
  // D3DX_SC_DEPTHBUFFER:    All ddpfs which have the DDPF_ZPIXELS or the
  //                           DDPF_ZBUFFER flags set.
  // D3DX_SC_STENCILBUFFER:  All ddpfs which have the DDPF_STENCILBUFFER
  //                          flag set.
  // D3DX_SC_BUMPMAP:        All ddpfs which have the DDPF_BUMPLUMINANCE
  //                           or the DDPF_BUMPDUDV flags set.
  // D3DX_SC_LUMINANCEMAP:   All ddpfs which have the DDPF_BUMPLUMINANCE
  //                           or the DDPF_LUMINANCE flags set.
  // D3DX_SC_COLORTEXTURE:   All the surfaces that have color information in
  //                           them and can be used for texturing.
  // D3DX_SC_COLORRENDERTGT: All the surfaces that contain color
  //                           information and can be used as render targets.
  //-------------------------------------------------------------------------

  const
    D3DX_SC_DEPTHBUFFER    = $01;
    D3DX_SC_STENCILBUFFER  = $02;
    D3DX_SC_COLORTEXTURE   = $04;
    D3DX_SC_BUMPMAP        = $08;
    D3DX_SC_LUMINANCEMAP   = $10;
    D3DX_SC_COLORRENDERTGT = $20;

  //-------------------------------------------------------------------------
  // Surface Formats:
  // ---------------
  // These are the various types of surface formats that can be enumerated,
  // there is no DDPIXELFORMAT structure in D3DX, the enums carry the meaning
  // (like FOURCCs).
  //
  // All the surface classes are represented here.
  //
  //-------------------------------------------------------------------------

  type
    PD3DX_SURFACEFORMAT = ^TD3DX_SURFACEFORMAT;
    TD3DX_SURFACEFORMAT = dword;

  const
    D3DX_SF_UNKNOWN    =  0;
    D3DX_SF_R8G8B8     =  1;
    D3DX_SF_A8R8G8B8   =  2;
    D3DX_SF_X8R8G8B8   =  3;
    D3DX_SF_R5G6B5     =  4;
    D3DX_SF_R5G5B5     =  5;
    D3DX_SF_PALETTE4   =  6;
    D3DX_SF_PALETTE8   =  7;
    D3DX_SF_A1R5G5B5   =  8;
    D3DX_SF_X4R4G4B4   =  9;
    D3DX_SF_A4R4G4B4   = 10;
    D3DX_SF_L8         = 11;      // 8 bit luminance-only
    D3DX_SF_A8L8       = 12;      // 16 bit alpha-luminance
    D3DX_SF_U8V8       = 13;      // 16 bit bump map format
    D3DX_SF_U5V5L6     = 14;      // 16 bit bump map format with luminance
    D3DX_SF_U8V8L8     = 15;      // 24 bit bump map format with luminance
    D3DX_SF_UYVY       = 16;      // UYVY format (PC98 compliance)
    D3DX_SF_YUY2       = 17;      // YUY2 format (PC98 compliance)
    D3DX_SF_DXT1       = 18;      // S3 texture compression technique 1
    D3DX_SF_DXT3       = 19;      // S3 texture compression technique 3
    D3DX_SF_DXT5       = 20;      // S3 texture compression technique 5
    D3DX_SF_R3G3B2     = 21;      // 8 bit RGB texture format
    D3DX_SF_A8         = 22;      // 8 bit alpha-only
    D3DX_SF_TEXTUREMAX = 23;      // Last texture format

    D3DX_SF_Z16S0      = 256;
    D3DX_SF_Z32S0      = 257;
    D3DX_SF_Z15S1      = 258;
    D3DX_SF_Z24S8      = 259;
    D3DX_SF_S1Z15      = 260;
    D3DX_SF_S8Z24      = 261;
    D3DX_SF_DEPTHMAX   = 262;     // Last depth format

    D3DX_SF_FORCEMAX   = -1;

  //-------------------------------------------------------------------------
  // Filtering types for Texture APIs
  //
  // -------------
  // These are the various filter types for generation of mip-maps
  //
  // D3DX_FILTERTYPE
  // -----------------------------------------
  // D3DX_FT_POINT:   Point sampling only - no filtering
  // D3DX_FT_LINEAR:  Bi-linear filtering
  //
  //-------------------------------------------------------------------------

  type
    TD3DX_FILTERTYPE = longint;

  const
    D3DX_FT_POINT   = $01;
    D3DX_FT_LINEAR  = $02;
    D3DX_FT_DEFAULT = D3DX_DEFAULT;

  ///////////////////////////////////////////////////////////////////////////
  // Structures used below:
  ///////////////////////////////////////////////////////////////////////////

  //-------------------------------------------------------------------------
  // D3DX_VIDMODEDESC: Display mode description.
  // ----------------
  // width:       Screen Width
  // height:      Screen Height
  // bpp:         Bits per pixel
  // refreshRate: Refresh rate
  //-------------------------------------------------------------------------

  type
    PD3DX_VIDMODEDESC = ^PD3DX_VIDMODEDESC;
    TD3DX_VIDMODEDESC =
      record
        width       : dword;
        height      : dword;
        bpp         : dword;
        refreshRate : dword;
      end;

  //-------------------------------------------------------------------------
  // D3DX_DEVICEDESC: Description of a device that can do 3D
  // ---------------
  // deviceIndex:   Unique (DWORD) number for the device.
  // hwLevel:       Level of acceleration afforded.  This is one of the
  //                predefined Device Indices, and exists in this
  //                structure for informational purposes only.  More than
  //                one device on the system may have the same hwLevel.
  //                To refer to a particular device with the D3DX apis,
  //                use the value in the deviceIndex member instead.
  // ddGuid:        The ddraw GUID
  // d3dDeviceGuid: Direct3D Device GUID
  // ddDeviceID:    DDraw's GetDeviceIdentifier GUID.  This GUID is unique to
  //                a particular driver revision on a particular video card.
  // driverDesc:    String describing the driver
  // monitor:       Handle to the video monitor used by this device (multimon
  //                specific).  Devices that use different monitors on a
  //                multimon system report different values in this field.
  //                Therefore, to test for a multimon system, an application
  //                should look for more than one different monitor handle in
  //                the list of D3DX devices.
  // onPrimary:     Indicates if this device is on the primary monitor
  //                (multimon specific).
  //-------------------------------------------------------------------------

  const
    D3DX_DRIVERDESC_LENGTH = 256;

  type
    PD3DX_DEVICEDESC = ^TD3DX_DEVICEDESC;
    TD3DX_DEVICEDESC =
      record
        deviceIndex   : dword;
        hwLevel       : dword;
        ddGuid        : TGuid;
        d3dDeviceGuid : TGuid;
        ddDeviceID    : TGuid;
        driverDesc    : array [0..pred(D3DX_DRIVERDESC_LENGTH)] of char;
        monitor       : HMONITOR;
        onPrimary     : BOOL;
      end;

  ///////////////////////////////////////////////////////////////////////////
  // APIs:
  ///////////////////////////////////////////////////////////////////////////

  //-------------------------------------------------------------------------
  // D3DXInitialize: The very first call a D3DX app must make.
  //-------------------------------------------------------------------------

  function D3DXInitialize : HRESULT; cdecl; external 'd3dx.dll' name '_D3DXInitialize';

  //-------------------------------------------------------------------------
  // D3DXUninitialize: The very last call a D3DX app must make.
  //-------------------------------------------------------------------------

  function D3DXUninitialize : HRESULT; cdecl; external 'd3dx.dll' name '_D3DXUninitialize';

  //-------------------------------------------------------------------------
  // D3DXGetDeviceCount: Returns the maximum number of D3DXdevices
  // ------------------  available.
  //
  // D3DXGetDeviceDescription: Lists the 2D and 3D capabilities of the devices.
  // ------------------------  Also, the various guids needed by ddraw and d3d.
  //
  // Params:
  //     [in] DWORD deviceIndex: Which device? Starts at 0.
  //     [in] D3DX_DEVICEDESC* pd3dxDevice: Pointer to the D3DX_DEVICEDESC
  //                                        structure to be filled in.
  //-------------------------------------------------------------------------

  function D3DXGetDeviceCount : dword; cdecl; external 'd3dx.dll' name '_D3DXGetDeviceCount';

  function D3DXGetDeviceDescription(deviceIndex : dword; out d3dxDeviceDesc : TD3DX_DEVICEDESC) : HRESULT; cdecl; external 'd3dx.dll' name '_D3DXGetDeviceDescription';

  //-------------------------------------------------------------------------
  // D3DXGetMaxNumVideoModes: Returns the maximum number of video-modes .
  // -----------------------
  //
  // Params:
  //     [in]  DWORD deviceIndex: The device being referred to.
  //     [in]  DWORD flags: If D3DX_GVM_REFRESHRATE is set, then the refresh
  //                        rates are not ignored.
  //
  // D3DXGetVideoMode:  Describes a particular video mode for this device
  // ----------------
  //
  // Note:  These queries will simply give you a list of modes that the
  //        display adapter tells DirectX that it supports.
  //        There is no guarantee that D3DXCreateContext(Ex) will succeed
  //        with all listed video modes.  This is a fundamental limitation
  //        of the current DirectX architecture which D3DX cannot hide in
  //        any clean way.
  //
  // Params:
  //     [in]  DWORD deviceIndex: The device being referred to.
  //     [in]  DWORD flags: If D3DX_GVM_REFRESHRATE is set, then the refresh
  //                        rates are returned
  //     [in]  DWORD which: Which VideoMode ? Starts at 0.
  //     [out] D3DX_VIDMODEDESC* pModeList: Pointer to the D3DX_VIDMODEDESC
  //                        structure that will be filled in.
  //-------------------------------------------------------------------------

  function D3DXGetMaxNumVideoModes(deviceIndex, flags : dword) : dword; cdecl; external 'd3dx.dll' name '_D3DXGetMaxNumVideoModes';

  function D3DXGetVideoMode(deviceIndex, flags, modeIndex : dword; out ModeList : TD3DX_VIDMODEDESC) : HRESULT; cdecl; external 'd3dx.dll' name '_D3DXGetVideoMode';

  const
    D3DX_GVM_REFRESHRATE = $00000001;

  //-------------------------------------------------------------------------
  // D3DXGetMaxSurfaceFormats: Returns the maximum number of surface
  // ------------------------  formats supported by the device at that
  //                           video mode.
  //
  // D3DXGetSurfaceFormat: Describes one of the supported surface formats.
  // ---------------------
  //
  // Params:
  //     [in]  DWORD  deviceIndex: The device being referred to.
  //     [in]  D3DX_VIDMODEDESC* pDesc: The display mode at which the supported
  //                                    surface formats are requested. If it is
  //                                    NULL, the current display mode is
  //                                    assumed.
  //     [in]  DWORD surfClassFlags: Required surface classes.  Only surface
  //                                 formats which support all specified
  //                                 surface classes will be returned.
  //                                 (Multiple surface classes may be specified
  //                                 using bitwise OR.)
  //     [in]  DWORD which: Which surface formats to retrieve. Starts at 0.
  //     [out] D3DX_SURFACEFORMAT* pFormat: The surface format
  //-------------------------------------------------------------------------

  function D3DXGetMaxSurfaceFormats(deviceIndex : dword; const Desc : TD3DX_VIDMODEDESC; surfClassFlags : dword) : dword; cdecl; external 'd3dx.dll' name '_D3DXGetMaxSurfaceFormats';

  function D3DXGetSurfaceFormat(deviceIndex : dword; const Desc : TD3DX_VIDMODEDESC; surfClassFlags, surfaceIndex : dword; out Format : TD3DX_SURFACEFORMAT) : HRESULT; cdecl; external 'd3dx.dll' name '_D3DXGetSurfaceFormat';

  //-------------------------------------------------------------------------
  // D3DXGetCurrentVideoMode: Retrieves the current video mode for this device.
  // -------------------
  //
  // Params:
  //     [in]  DWORD deviceIndex: The device being referred to.
  //     [out] D3DX_VIDMODEDESC* pVidMode: The current video mode
  //-------------------------------------------------------------------------

  function D3DXGetCurrentVideoMode(deviceIndex : dword; out VidMode : TD3DX_VIDMODEDESC) : HRESULT; cdecl; external 'd3dx.dll' name '_D3DXGetCurrentVideoMode';

  //-------------------------------------------------------------------------
  // D3DXGetDeviceCaps: Lists all the capabilities of a device at a display
  //                    mode.
  // ----------------
  //
  // Params:
  //     [in]  DWORD  deviceIndex: The device being referred to.
  //     [in]  D3DX_VIDMODEDESC* pDesc:  If this is NULL, we will return the
  //                                     caps at the current display mode of
  //                                     the device.
  //     [out] D3DDEVICEDESC7* pD3DDeviceDesc7: D3D Caps ( NULL to ignore
  //                                              parameter)
  //     [out] DDCAPS7* pDDHalCaps: DDraw HAL Caps (NULL to ignore parameter)
  //     [out] DDCAPS7* pDDHelCaps: DDraw HEL Caps (NULL to ignore  paramter)
  //-------------------------------------------------------------------------

  function D3DXGetDeviceCaps(deviceIndex : dword; pVidMode : PD3DX_VIDMODEDESC; pD3DCaps : PD3DDEVICEDESC7; pDDHALCaps, pDDHELCaps : PDDCAPS) : HRESULT; cdecl; external 'd3dx.dll' name '_D3DXGetDeviceCaps';

  //-------------------------------------------------------------------------
  // D3DXCreateContext: Initializes the chosen device. It is the simplest init
  // -----------------  function available.  Parameters are treated the same
  //                    as the matching subset of parameters in
  //                    D3DXCreateContextEx, documented below.
  //                    Remaining D3DXCreateContextEx parameters that are
  //                    not present in D3DXCreateContext are treated as
  //                    D3DX_DEFAULT.  Note that multimon is not supported
  //                    with D3DXCreateContext.
  //
  // D3DXCreateContextEx: A more advanced function to initialize the device.
  // -------------------  Also accepts D3DX_DEFAULT for most of the parameters
  //                      and then will do what D3DXCreateContext did.
  //
  // Note: Do not expect D3DXCreateContext(Ex) to be fail-safe (as with any
  //       API).  Supported device capablilites should be used as a guide
  //       for choosing parameter values.  Keep in mind that there will
  //       inevitably be some combinations of parameters that just do not work.
  //
  // Params:
  //     [in]  DWORD deviceIndex: The device being referred to.
  //     [in]  DWORD flags: The valid flags are D3DX_CONTEXT_FULLSCREEN, and
  //                        D3DX_CONTEXT_OFFSCREEN.  These flags cannot both
  //                        be specified.  If no flags are specified, the
  //                        context defaults to windowed mode.
  //
  //     [in]  HWND  hwnd: Device window.  See note.
  //     [in]  HWND  hwndFocus: Window which receives keyboard messages from
  //                            the device window.  The device window should be
  //                            a child of focus window.  Useful for multimon
  //                            applications.  See note.
  //     NOTE:
  //         windowed:
  //             hwnd must be a valid window.  hwndFocus must be NULL or
  //             D3DX_DEFAULT.
  //
  //         fullscreen:
  //             Either hwnd or hwndFocus must be a valid window.  (Both cannot
  //             be NULL or D3DX_DEFAULT).  If hwnd is NULL or D3DX_DEFAULT,
  //             a default device window will be created as a child of hwndFocus.
  //
  //         offscreen:
  //             Both hwnd and hwndFocus must be NULL or D3DX_DEFAULT
  //
  //     [in]  DWORD numColorBits: If D3DX_DEFAULT is passed for windowed mode,
  //                               the current desktop's color depth is chosen.
  //                               For full screen mode, D3DX_DEFAULT causes 16
  //                               bit color to be used.
  //     [in]  DWORD numAlphaBits: If D3DX_DEFAULT is passed, 0 is chosen.
  //     [in]  DWORD numDepthbits: If D3DX_DEFAULT is passed,
  //                               the highest available number of depth bits
  //                               is chosen.  See note.
  //     [in]  DWORD numStencilBits: If D3DX_DEFAULT is passed, the highest
  //                                 available number of stencil bits is chosen.
  //                                 See note.
  //
  //     NOTE: If both numDepthBits and numStencilBits are D3DX_DEFAULT,
  //           D3DX first picks the highest available number of stencil
  //           bits.  Then, for the chosen number of stencil bits,
  //           the highest available number of depth bits is chosen.
  //           If only one of numStencilBits or numDepthBits
  //           is D3DX_DEFAULT, the highest number of bits available
  //           for this parameter is chosen out of only the formats
  //           that support the number of bits requested for the
  //           fixed parameter.
  //
  //     [in]  DWORD numBackBuffers: Number of back buffers, or D3DX_DEFAULT.
  //                                 See note.
  //
  //     NOTE:
  //        windowed: D3DX_DEFAULT means 1.  You must specify one back buffer.
  //
  //        fullscreen: D3DX_DEFAULT means 1.  Any number of back buffers can be
  //            specified.
  //
  //        offscreen: D3DX_DEFAULT means 0.  You cannot specify additional back
  //            buffers.
  //
  //     [in]  DWORD width: Width, in pixels, or D3DX_DEFAULT.  See note.
  //     [in]  DWORD height: Height, in pixels, or D3DX_DEFAULT.  See note.
  //
  //     NOTE:
  //        windowed: If either width or height is D3DX_DEFAULT, both values
  //            default to the dimensions of the client area of hwnd.
  //
  //        fullscreen: If either width or height is D3DX_DEFAULT, width
  //            defaults to 640, and height defaults to 480.
  //
  //        offscreen: An error is returned if either width or height is
  //            D3DX_DEFAULT.
  //
  //     [in]  DWORD refreshRate: D3DX_DEFAULT means we let ddraw choose for
  //                              us.  Ignored for windowed and offscreen modes.
  //     [out] LPD3DXCONTEXT* ppCtx: This is the Context object that is used for
  //                                 rendering on that device.
  //
  //-------------------------------------------------------------------------

  function D3DXCreateContext(deviceIndex, flags : dword; wnd : HWND; width, height : dword; out Ctx : ID3DXContext) : HRESULT; cdecl; external 'd3dx.dll' name '_D3DXCreateContext';

  function D3DXCreateContextEx(deviceIndex, flags : dword; wnd : HWND; hwndFocus : HWND; numColorBits, numAlphaBits, numDepthbits, numStencilBits, numBackBuffers, width, height, refreshRate : integer; out Ctx : ID3DXContext) : HRESULT; cdecl; external 'd3dx.dll' name '_D3DXCreateContextEx';

  // The D3DXCreateContext(Ex) flags are:
  const
    D3DX_CONTEXT_FULLSCREEN = $00000001;
    D3DX_CONTEXT_OFFSCREEN  = $00000002;

  //-------------------------------------------------------------------------
  // D3DXGetErrorString: Prints out the error string given an hresult. Prints
  // ------------------  Win32 as well as DX6 error messages besides the D3DX
  //                     messages.
  //
  // Params:
  //     [in]  HRESULT hr: The error code to be deciphered.
  //     [in]  DWORD strLength: Length of the string passed in.
  //     [out] LPSTR pStr:  The string output. This string of appropriate
  //                       size needs to be passed in.
  //-------------------------------------------------------------------------

  procedure D3DXGetErrorString(hr : HRESULT; strLength : dword; pStr : pchar); cdecl; external 'd3dx.dll' name '_D3DXGetErrorString';

  //-------------------------------------------------------------------------
  // D3DXMakeDDPixelFormat: Fills in a DDPIXELFORMAT structure based on the
  // ---------------------   D3DX surface format requested.
  //
  // Params:
  //     [in]  D3DX_SURFACEFORMAT d3dxFormat: Surface format.
  //     [out] DDPIXELFORMAT*     pddpf:      Pixel format matching the given
  //                                          surface format.
  //-------------------------------------------------------------------------

  function D3DXMakeDDPixelFormat(d3dxFormat : TD3DX_SURFACEFORMAT; out ddpf : TDDPIXELFORMAT) : HRESULT; cdecl; external 'd3dx.dll' name '_D3DXMakeDDPixelFormat';

  //-------------------------------------------------------------------------
  // D3DXMakeSurfaceFormat: Determines the surface format corresponding to
  // ---------------------  a given DDPIXELFORMAT.
  //
  // Params:
  //     [in]  DDPIXELFORMAT* pddpf: Pixel format.
  // Return Value:
  //     D3DX_SURFACEFORMAT: Surface format matching the given pixel format.
  //                         D3DX_SF_UNKNOWN if the format is not supported
  //-------------------------------------------------------------------------

  function D3DXMakeSurfaceFormat(const ddpf : TDDPIXELFORMAT) : TD3DX_SURFACEFORMAT; cdecl; external 'd3dx.dll' name '_D3DXMakeSurfaceFormat';

  //-------------------------------------------------------------------------
  // Flags for Update member function:
  //

  // Flag to indicate that blit should be used instead of a flip
  // for full-screen rendering.
  const
    D3DX_UPDATE_NOVSYNC = 1 shl 0;

  ///////////////////////////////////////////////////////////////////////////
  // Texturing APIs:
  ///////////////////////////////////////////////////////////////////////////

  //-------------------------------------------------------------------------
  // D3DXCheckTextureRequirements: Return information about texture creation
  // ----------------------------  (used by CreateTexture, CreateTextureFromFile
  //                                and CreateCubeMapTexture)
  //
  // Parameters:
  //
  //  pd3dDevice
  //      The D3D device with which the texture is going to be used.
  //  pFlags
  //      allows specification of D3DX_TEXTURE_NOMIPMAP
  //      D3DX_TEXTURE_NOMIPMAP may be returned in the case where mipmap creation
  //      is not supported.
  //  pWidth
  //      width in pixels or NULL
  //      returns corrected width
  //  pHeight
  //      height in pixels or NULL
  //      returns corrected height
  //  pPixelFormat
  //      surface format
  //      returns best match to input format
  //
  //  Notes: 1. Unless the flags is set to specifically prevent creating
  //            mipmaps, mipmaps are generated all the way till 1x1 surface.
  //         2. width, height and pixelformat are altered based on available
  //            hardware. For example:
  //              a. Texture dimensions may be required to be powers of 2
  //              b. We may require width == height for some devices
  //              c. If PixelFormat is unavailable, a best fit is made
  //-------------------------------------------------------------------------

  function D3DXCheckTextureRequirements(const d3dDevice : IDirect3DDevice7; pFlags, pWidth, pHeight : pdword; var PixelFormat : TD3DX_SURFACEFORMAT) : HRESULT; cdecl; external 'd3dx.dll' name '_D3DXCheckTextureRequirements';

  //-------------------------------------------------------------------------
  // D3DXCreateTexture: Create an empty texture object
  // -----------------
  //
  // Parameters:
  //
  //  pd3dDevice
  //      The D3D device with which the texture is going to be used.
  //  pFlags
  //      allows specification of D3DX_TEXTURE_NOMIPMAP
  //      D3DX_TEXTURE_NOMIPMAP may be returned in the case where mipmap creation
  //      is not supported. Additionally, D3DX_TEXTURE_STAGE<n> can be specified
  //      to indicate which texture stage the texture is for e.g.
  //      D3D_TEXTURE_STAGE1 indicates that the texture is for use with texture
  //      stage one. Stage Zero is the default if no TEXTURE_STAGE flags are
  //      set.
  //  pWidth
  //      width in pixels; 0 or NULL is unacceptable
  //      returns corrected width
  //  pHeight
  //      height in pixels; 0 or NULL is unacceptable
  //      returns corrected height
  //  pPixelFormat
  //      surface format. D3DX_DEFAULT is unacceptable.
  //      returns actual format that was used
  //  pDDPal
  //      DDraw palette that is set (if present) on paletted surfaces.
  //      It is ignored even if it is set, for non-paletted surfaces.
  //  ppDDSurf
  //      The ddraw surface that will be created
  //  pNumMipMaps
  //      the number of mipmaps actually generated
  //
  //  Notes: See notes for D3DXCheckTextureRequirements.
  //-------------------------------------------------------------------------

  function D3DXCreateTexture(const d3dDevice : IDirect3DDevice7;
                             pFlags,
                             pWidth,
                             pHeight : pdword;
                             pPixelFormat : PD3DX_SURFACEFORMAT;
                             const DDPal : IDirectDrawPalette;
                             out DDSurf : IDirectDrawSurface7;
                             pNumMipMaps : pdword) : HRESULT; cdecl; external 'd3dx.dll' name '_D3DXCreateTexture';

  //-------------------------------------------------------------------------
  // D3DXCreateCubeMapTexture: Create blank cube-map texture
  // ------------------------
  //
  // Parameters:
  //
  //  pd3dDevice
  //      The D3D device with which the texture is going to be used.
  //  pFlags
  //      allows specification of D3DX_TEXTURE_NOMIPMAP
  //      D3DX_TEXTURE_NOMIPMAP may be returned in the case where mipmap creation
  //      is not supported. Additionally, D3DX_TEXTURE_STAGE<n> can be specified
  //      to indicate which texture stage the texture is for e.g.
  //      D3D_TEXTURE_STAGE1 indicates that the texture is for use with texture
  //      stage one. Stage Zero is the default if no TEXTURE_STAGE flags are
  //      set.
  //  cubefaces
  //      allows specification of which faces of the cube-map to generate.
  //      D3DX_DEFAULT, 0, and DDSCAPS2_CUBEMAP_ALLFACES all mean
  //      "create all 6 faces of the cubemap". Any combination of
  //      DDSCAPS2_CUBEMAP_POSITIVEX, DDSCAPS2_CUBEMAP_NEGATIVEX,
  //      DDSCAPS2_CUBEMAP_POSITIVEY, DDSCAPS2_CUBEMAP_NEGATIVEY,
  //      DDSCAPS2_CUBEMAP_POSITIVEZ, or DDSCAPS2_CUBEMAP_NEGATIVEZ, is
  //      valid.
  //  colorEmptyFaces
  //      allows specification of the color to use for the faces that were not
  //      specified in the cubefaces parameter.
  //  pWidth
  //      width in pixels; 0 or NULL is unacceptable
  //      returns corrected width
  //  pHeight
  //      height in pixels; 0 or NULL is unacceptable
  //      returns corrected height
  //  pPixelFormat
  //      surface format. D3DX_DEFAULT is unacceptable.
  //      returns actual format that was used
  //  pDDPal
  //      DDraw palette that is set (if present) on paletted surfaces.
  //      It is ignored even if it is set, for non-paletted surfaces.
  //  ppDDSurf
  //      the ddraw surface that will be created
  //  pNumMipMaps
  //      the number of mipmaps generated for a particular face of the
  //      cubemap.
  //
  //  Notes: See notes for D3DXCheckTextureRequirements.
  //-------------------------------------------------------------------------

  function D3DXCreateCubeMapTexture(const d3dDevice : IDirect3DDevice7;
                                    pFlags : pdword;
                                    cubefaces : dword;
                                    colorEmptyFaces : TD3DCOLOR;
                                    pWidth,
                                    pHeight : pdword;
                                    pPixelFormat : PD3DX_SURFACEFORMAT;
                                    const DDPal : IDirectDrawPalette;
                                    out DDSurf : IDirectDrawSurface7;
                                    pNumMipMaps : pdword) : HRESULT; cdecl; external 'd3dx.dll' name '_D3DXCreateCubeMapTexture';

  //-------------------------------------------------------------------------
  // D3DXCreateTextureFromFile: Create a texture object from a file or from the
  // -------------------------  resource. Only BMP and DIB are supported from the
  //                            resource portion of the executable.
  //
  // Parameters:
  //
  //  pd3dDevice
  //      The D3D device with which the texture is going to be used.
  //  pFlags
  //      allows specification of D3DX_TEXTURE_NOMIPMAP
  //      D3DX_TEXTURE_NOMIPMAP may be returned in the case where mipmap creation
  //      is not supported. Additionally, D3DX_TEXTURE_STAGE<n> can be specified
  //      to indicate which texture stage the texture is for e.g.
  //      D3D_TEXTURE_STAGE1 indicates that the texture is for use with texture
  //      stage one. Stage Zero is the default if no TEXTURE_STAGE flags are
  //      set.
  //  pWidth
  //      Width in pixels. If 0 or D3DX_DEFAULT, the width will be taken
  //      from the file
  //      returns corrected width
  //  pHeight
  //      Height in pixels. If 0 or D3DX_DEFAULT, the height will be taken
  //      from the file
  //      returns corrected height
  //  pPixelFormat
  //      If D3DX_SF_UNKNOWN is passed in, pixel format closest to the bitmap
  //      will be chosen
  //      returns actual format that was used
  //  pDDPal
  //      DDraw palette that is set (if present) on paletted surfaces.
  //      It is ignored even if it is set, for non-paletted surfaces.
  //  ppDDSurf
  //      The ddraw surface that will be created.
  //  pNumMipMaps
  //      The number of mipmaps generated.
  //  pSrcName
  //      File name. BMP, DIB, DDS, are supported.
  //
  //      TGA is supported for the following cases: 16, 24, 32bpp direct color and 8bpp palettized.
  //      Also, 8, 16bpp grayscale is supported. RLE versions of the above
  //      TGA formats are also supported. ColorKey and Premultiplied Alpha
  //      are not currently supported for TGA files.
  //      returns created format
  //
  //  Notes: See notes for D3DXCheckTextureRequirements.
  //-------------------------------------------------------------------------

  function D3DXCreateTextureFromFile(const d3dDevice : IDirect3DDevice7;
                                     pFlags,
                                     pWidth,
                                     pHeight : pdword;
                                     pPixelFormat : PD3DX_SURFACEFORMAT;
                                     const DDPal : IDirectDrawPalette;
                                     out DDSurf : IDirectDrawSurface7;
                                     pNumMipMaps : pdword;
                                     pSrcName : pchar;
                                     filterType : TD3DX_FILTERTYPE) : HRESULT; cdecl; external 'd3dx.dll' name '_D3DXCreateTextureFromFile';

  //-------------------------------------------------------------------------
  // D3DXLoadTextureFromFile: Load from a file into a mipmap level. Doing the
  // -----------------------  necessary color conversion and rescaling. File
  //                          format support is identical to
  //                          D3DXCreateTextureFromFile's.
  //
  //  pd3dDevice
  //      The D3D device with which the texture is going to be used.
  // pTexture
  //      a pointer to a DD7Surface which was created with either
  //      CreateTextureFromFile or CreateTexture.
  // mipMapLevel
  //      indicates mipmap level
  //      Note:
  //          1. Error if mipmap level doesn't exist
  //          2. If D3DX_DEFAULT and equal number of mipmap levels exist
  //             then all the source mip-levels are loaded
  //          3. If the source has mipmaps and the dest doesn't, use the top one
  //          4. If the dest has miplevels and source doesn't, we expand
  //          5. If there are unequal numbers of miplevels, we expand
  // pSrcName
  //      File name. BMP, DIB, DDS, are supported.
  //      For details on TGA support, refer to the comments for
  //      D3DXCreateTextureFromFile
  // pSrcRect
  //      the source rectangle or null (whole surface)
  // pDestRect
  //      the destination rectangle or null (whole surface)
  // filterType
  //      filter used for mipmap generation
  //-------------------------------------------------------------------------

  function D3DXLoadTextureFromFile(const d3dDevice : IDirect3DDevice7;
                                   const Texture : IDirectDrawSurface7;
                                   mipMapLevel : dword;
                                   pSrcName : pchar;
                                   pSrcRect,
                                   pDestRect : PRect;
                                   filterType : TD3DX_FILTERTYPE) : HRESULT; cdecl; external 'd3dx.dll' name '_D3DXLoadTextureFromFile';

  //-------------------------------------------------------------------------
  // D3DXLoadTextureFromSurface: Load from a DDraw Surface into a mipmap level.
  // --------------------------  Doing the necessary color conversion.
  //
  //  pd3dDevice
  //      The D3D device with which the texture is going to be used.
  // pTexture
  //      a pointer to a DD7Surface which was created with either
  //      CreateTextureFromFile or CreateTexture.
  // mipMapLevel
  //      indicates mipmap level
  //      Note:
  //          1. Error if mipmap level doesn't exist
  //          2. If D3DX_DEFAULT and equal number of mipmap levels exist
  //             then all the source mip-levels are loaded
  //          3. If the source has mipmaps and the dest doesn't, use the top one
  //          4. If the dest has miplevels and source doesn't, we expand
  //          5. If there are unequal numbers of miplevels, we expand
  // pSurfaceSrc
  //      the source surface
  // pSrcRect
  //      the source rectangle or null (whole surface)
  // pDestRect
  //      the destination rectangle or null (whole surface)
  // filterType
  //      filter used for mipmap generation
  //-------------------------------------------------------------------------

  function D3DXLoadTextureFromSurface(const d3dDevice : IDirect3DDevice7;
                                      const Texture : IDirectDrawSurface7;
                                      mipMapLevel : dword;
                                      const SurfaceSrc : IDirectDrawSurface7;
                                      pSrcRect,
                                      pDestRect : PRect;
                                      filterType : TD3DX_FILTERTYPE) : HRESULT; cdecl; external 'd3dx.dll' name '_D3DXLoadTextureFromSurface';

  //-------------------------------------------------------------------------
  // D3DXLoadTextureFromMemory: Load a mip level from memory. Doing the necessary
  // -------------------------  color conversion.
  //
  //  pd3dDevice
  //      The D3D device with which the texture is going to be used.
  // pTexture
  //      a pointer to a DD7Surface which was created with either
  //      CreateTextureFromFile or CreateTexture.
  // mipMapLevel
  //      indicates mipmap level
  //      Note:
  //          1. Error if mipmap level doesn't exist
  //          2. If D3DX_DEFAULT and equal number of mipmap levels exist
  //             then all the source mip-levels are loaded
  //          3. If the source has mipmaps and the dest doesn't, use the top one
  //          4. If the dest has miplevels and source doesn't, we expand
  //          5. If there are unequal numbers of miplevels, we expand
  // pMemory
  //      pointer to source memory from which the texture will be loaded
  // pDDPal
  //      DirectDraw Palette, that the app passes in optionally if the memory is
  //      supposed to be paletteized.
  // srcPixelFormat
  //      PixelFormat of the source.
  // srcPitch
  //      The pitch of the memory or D3DX_DEFAULT (based on srcPixelFormat)
  // pDestRect
  //      The destination rectangle or null (whole surface)
  // filterType
  //      filter used for mipmap generation
  //
  // Assumptions: The source (memory) is loaded in full
  //-------------------------------------------------------------------------

  function D3DXLoadTextureFromMemory(const d3dDevice : IDirect3DDevice7;
                                     const Texture : IDirectDrawSurface7;
                                     mipMapLevel : dword;
                                     pMemory : pointer;
                                     const pDDPal : IDirectDrawPalette;
                                     psrcPixelFormat : PD3DX_SURFACEFORMAT; // >>
                                     srcPitch : dword;
                                     pDestRect : PRect;
                                     filterType : TD3DX_FILTERTYPE) : HRESULT; cdecl; external 'd3dx.dll' name '_D3DXLoadTextureFromMemory';

  //-------------------------------------------------------------------------
  // Flags for texture create functions; applies to
  // D3DXCreateTexture, D3DXCreateCubeMapTexture and D3DXCreateTextureFromFile.
  //

  // Flag to indicate that mipmap generation is not desired.
  const
    D3DX_TEXTURE_NOMIPMAP = 1 shl 8;

  // Flags to indicate which texture stage the texture is
  // intended for use with. Specifying the stage is necessary at
  // texture creation time for HW devices that expose the
  // D3DDEVCAPS_SEPARATETEXTUREMEMORIES bit in their D3DDEVICEDESC
  // structure.
  const
    D3DX_TEXTURE_STAGE0 = 0;
    D3DX_TEXTURE_STAGE1 = 1;
    D3DX_TEXTURE_STAGE2 = 2;
    D3DX_TEXTURE_STAGE3 = 3;
    D3DX_TEXTURE_STAGE4 = 4;
    D3DX_TEXTURE_STAGE5 = 5;
    D3DX_TEXTURE_STAGE6 = 6;
    D3DX_TEXTURE_STAGE7 = 7;

  // Mask to extract the texture stage value out of the flags to
  // the texture create functions.
  const
    D3DX_TEXTURE_STAGE_MASK = $7;

implementation

end.
