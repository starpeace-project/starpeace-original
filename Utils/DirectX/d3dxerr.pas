unit D3DXErr;
  //----------------------------------------------------------------------
  //
  //   d3dxerr.pas --  0xC code definitions for the D3DX API
  //
  //   Copyright (c) 1991-1999, Microsoft Corp. All rights reserved.
  //
  //----------------------------------------------------------------------

interface

  uses
    Windows;

  //
  //
  //  Values are 32 bit values layed out as follows:
  //
  //   3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
  //   1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
  //  +---+-+-+-----------------------+-------------------------------+
  //  |Sev|C|R|     Facility          |               Code            |
  //  +---+-+-+-----------------------+-------------------------------+
  //
  //  where
  //
  //      Sev - is the severity code
  //
  //          00 - Success
  //          01 - Informational
  //          10 - Warning
  //          11 - Error
  //
  //      C - is the Customer code flag
  //
  //      R - is a reserved bit
  //
  //      Facility - is the facility code
  //
  //      Code - is the facility's status code
  //
  //
  // Define the facility codes
  //
  const
    FACILITY_D3DX = $877;

  //
  // MessageId: D3DXERR_NOMEMORY
  //
  // MessageText:
  //
  //  Out of memory.
  //
  const
    D3DXERR_NOMEMORY : HRESULT = $C8770BB8;

  //
  // MessageId: D3DXERR_NULLPOINTER
  //
  // MessageText:
  //
  //  A NULL pointer was passed as a parameter.
  //
  const
    D3DXERR_NULLPOINTER : HRESULT = $C8770BB9;

  //
  // MessageId: D3DXERR_INVALIDD3DXDEVICEINDEX
  //
  // MessageText:
  //
  //  The Device Index passed in is invalid.
  //
  const
    D3DXERR_INVALIDD3DXDEVICEINDEX : HRESULT = $C8770BBA;

  //
  // MessageId: D3DXERR_NODIRECTDRAWAVAILABLE
  //
  // MessageText:
  //
  //  DirectDraw has not been created.
  //
  const
    D3DXERR_NODIRECTDRAWAVAILABLE : HRESULT = $C8770BBB;

  //
  // MessageId: D3DXERR_NODIRECT3DAVAILABLE
  //
  // MessageText:
  //
  //  Direct3D has not been created.
  //
  const
    D3DXERR_NODIRECT3DAVAILABLE : HRESULT = $C8770BBC;

  //
  // MessageId: D3DXERR_NODIRECT3DDEVICEAVAILABLE
  //
  // MessageText:
  //
  //  Direct3D device has not been created.
  //
  const
    D3DXERR_NODIRECT3DDEVICEAVAILABLE : HRESULT = $C8770BBD;

  //
  // MessageId: D3DXERR_NOPRIMARYAVAILABLE
  //
  // MessageText:
  //
  //  Primary surface has not been created.
  //
  const
    D3DXERR_NOPRIMARYAVAILABLE : HRESULT = $C8770BBE;

  //
  // MessageId: D3DXERR_NOZBUFFERAVAILABLE
  //
  // MessageText:
  //
  //  Z buffer has not been created.
  //
  const
    D3DXERR_NOZBUFFERAVAILABLE : HRESULT = $C8770BBF;

  //
  // MessageId: D3DXERR_NOBACKBUFFERAVAILABLE
  //
  // MessageText:
  //
  //  Backbuffer has not been created.
  //
  const
    D3DXERR_NOBACKBUFFERAVAILABLE : HRESULT = $C8770BC0;

  //
  // MessageId: D3DXERR_COULDNTUPDATECAPS
  //
  // MessageText:
  //
  //  Failed to update caps database after changing display mode.
  //
  const
    D3DXERR_COULDNTUPDATECAPS : HRESULT = $C8770BC1;

  //
  // MessageId: D3DXERR_NOZBUFFER
  //
  // MessageText:
  //
  //  Could not create Z buffer.
  //
  const
    D3DXERR_NOZBUFFER : HRESULT = $C8770BC2;

  //
  // MessageId: D3DXERR_INVALIDMODE
  //
  // MessageText:
  //
  //  Display mode is not valid.
  //
  const
    D3DXERR_INVALIDMODE : HRESULT = $C8770BC3;

  //
  // MessageId: D3DXERR_INVALIDPARAMETER
  //
  // MessageText:
  //
  //  One or more of the parameters passed is invalid.
  //
  const
    D3DXERR_INVALIDPARAMETER : HRESULT = $C8770BC4;

  //
  // MessageId: D3DXERR_INITFAILED
  //
  // MessageText:
  //
  //  D3DX failed to initialize itself.
  //
  const
    D3DXERR_INITFAILED : HRESULT = $C8770BC5;

  //
  // MessageId: D3DXERR_STARTUPFAILED
  //
  // MessageText:
  //
  //  D3DX failed to start up.
  //
  const
    D3DXERR_STARTUPFAILED : HRESULT = $C8770BC6;

  //
  // MessageId: D3DXERR_D3DXNOTSTARTEDYET
  //
  // MessageText:
  //
  //  D3DXInitialize() must be called first.
  //
  const
    D3DXERR_D3DXNOTSTARTEDYET : HRESULT = $C8770BC7;

  //
  // MessageId: D3DXERR_NOTINITIALIZED
  //
  // MessageText:
  //
  //  D3DX is not initialized yet.
  //
  const
    D3DXERR_NOTINITIALIZED : HRESULT = $C8770BC8;

  //
  // MessageId: D3DXERR_FAILEDDRAWTEXT
  //
  // MessageText:
  //
  //  Failed to render text to the surface.
  //
  const
    D3DXERR_FAILEDDRAWTEXT : HRESULT = $C8770BC9;

  //
  // MessageId: D3DXERR_BADD3DXCONTEXT
  //
  // MessageText:
  //
  //  Bad D3DX context.
  //
  const
    D3DXERR_BADD3DXCONTEXT : HRESULT = $C8770BCA;

  //
  // MessageId: D3DXERR_CAPSNOTSUPPORTED
  //
  // MessageText:
  //
  //  The requested device capabilities are not supported.
  //
  const
    D3DXERR_CAPSNOTSUPPORTED : HRESULT = $C8770BCB;

  //
  // MessageId: D3DXERR_UNSUPPORTEDFILEFORMAT
  //
  // MessageText:
  //
  //  The image file format is unrecognized.
  //
  const
    D3DXERR_UNSUPPORTEDFILEFORMAT : HRESULT = $C8770BCC;

  //
  // MessageId: D3DXERR_IFLERROR
  //
  // MessageText:
  //
  //  The image file loading library error.
  //
  const
    D3DXERR_IFLERROR : HRESULT = $C8770BCD;

  //
  // MessageId: D3DXERR_FAILEDGETCAPS
  //
  // MessageText:
  //
  //  Could not obtain device caps.
  //
  const
    D3DXERR_FAILEDGETCAPS : HRESULT = $C8770BCE;

  //
  // MessageId: D3DXERR_CANNOTRESIZEFULLSCREEN
  //
  // MessageText:
  //
  //  Resize does not work for full-screen.
  //
  const
    D3DXERR_CANNOTRESIZEFULLSCREEN : HRESULT = $C8770BCF;

  //
  // MessageId: D3DXERR_CANNOTRESIZENONWINDOWED
  //
  // MessageText:
  //
  //  Resize does not work for non-windowed contexts.
  //
  const
    D3DXERR_CANNOTRESIZENONWINDOWED : HRESULT = $C8770BD0;

  //
  // MessageId: D3DXERR_FRONTBUFFERALREADYEXISTS
  //
  // MessageText:
  //
  //  Front buffer already exists.
  //
  const
    D3DXERR_FRONTBUFFERALREADYEXISTS : HRESULT = $C8770BD1;

  //
  // MessageId: D3DXERR_FULLSCREENPRIMARYEXISTS
  //
  // MessageText:
  //
  //  The app is using the primary in full-screen mode.
  //
  const
    D3DXERR_FULLSCREENPRIMARYEXISTS : HRESULT = $C8770BD2;

  //
  // MessageId: D3DXERR_GETDCFAILED
  //
  // MessageText:
  //
  //  Could not get device context.
  //
  const
    D3DXERR_GETDCFAILED : HRESULT = $C8770BD3;

  //
  // MessageId: D3DXERR_BITBLTFAILED
  //
  // MessageText:
  //
  //  Could not bitBlt.
  //
  const
    D3DXERR_BITBLTFAILED : HRESULT = $C8770BD4;

  //
  // MessageId: D3DXERR_NOTEXTURE
  //
  // MessageText:
  //
  //  There is no surface backing up this texture.
  //
  const
    D3DXERR_NOTEXTURE : HRESULT = $C8770BD5;

  //
  // MessageId: D3DXERR_MIPLEVELABSENT
  //
  // MessageText:
  //
  //  There is no such miplevel for this surface.
  //
  const
    D3DXERR_MIPLEVELABSENT : HRESULT = $C8770BD6;

  //
  // MessageId: D3DXERR_SURFACENOTPALETTED
  //
  // MessageText:
  //
  //  The surface is not paletted.
  //
  const
    D3DXERR_SURFACENOTPALETTED : HRESULT = $C8770BD7;

  //
  // MessageId: D3DXERR_ENUMFORMATSFAILED
  //
  // MessageText:
  //
  //  An error occured while enumerating surface formats.
  //
  const
    D3DXERR_ENUMFORMATSFAILED : HRESULT = $C8770BD8;

  //
  // MessageId: D3DXERR_COLORDEPTHTOOLOW
  //
  // MessageText:
  //
  //  D3DX only supports color depths of 16 bit or greater.
  //
  const
    D3DXERR_COLORDEPTHTOOLOW : HRESULT = $C8770BD9;

  //
  // MessageId: D3DXERR_INVALIDFILEFORMAT
  //
  // MessageText:
  //
  //  The file format is invalid.
  //
  const
    D3DXERR_INVALIDFILEFORMAT : HRESULT = $C8770BDA;

  //
  // MessageId: D3DXERR_NOMATCHFOUND
  //
  // MessageText:
  //
  //  No suitable match found.
  //
  const
    D3DXERR_NOMATCHFOUND : HRESULT = $C8770BDB;

implementation

end.
