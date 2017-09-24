(*==========================================================================;
 *
 *  Copyright (C) 1995-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:	d3drm.h
 *  Content:	Direct3DRM include file
 *
 *  DirectX 6 Delphi adaptation by Erik Unger
 *
 *  Modyfied: 29.Dec.98
 *
 *  Download: http://www.bigfoot.com/~ungerik/
 *  E-Mail: ungerik@bigfoot.com
 *
 ***************************************************************************)

unit D3DRMObj;

interface

uses
{$IFDEF D2COM}
  OLE2,
{$ENDIF}
  Windows,
  D3DRMDef,
  Direct3D,
  DirectDraw;

(*
 * Direct3DRM Object classes
 *)

const
  CLSID_CDirect3DRMDevice: TGUID =
      (D1:$4fa3568e;D2:$623f;D3:$11cf;D4:($ac,$4a,$00,$00,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMViewport: TGUID =
      (D1:$4fa3568f;D2:$623f;D3:$11cf;D4:($ac,$4a,$00,$00,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMFrame: TGUID =
      (D1:$4fa35690;D2:$623f;D3:$11cf;D4:($ac,$4a,$00,$00,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMMesh: TGUID =
      (D1:$4fa35691;D2:$623f;D3:$11cf;D4:($ac,$4a,$00,$00,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMMeshBuilder: TGUID =
      (D1:$4fa35692;D2:$623f;D3:$11cf;D4:($ac,$4a,$00,$00,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMFace: TGUID =
      (D1:$4fa35693;D2:$623f;D3:$11cf;D4:($ac,$4a,$00,$00,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMLight: TGUID =
      (D1:$4fa35694;D2:$623f;D3:$11cf;D4:($ac,$4a,$00,$00,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMTexture: TGUID =
      (D1:$4fa35695;D2:$623f;D3:$11cf;D4:($ac,$4a,$00,$00,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMWrap: TGUID =
      (D1:$4fa35696;D2:$623f;D3:$11cf;D4:($ac,$4a,$00,$00,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMMaterial: TGUID =
      (D1:$4fa35697;D2:$623f;D3:$11cf;D4:($ac,$4a,$00,$00,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMAnimation: TGUID =
      (D1:$4fa35698;D2:$623f;D3:$11cf;D4:($ac,$4a,$00,$00,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMAnimationSet: TGUID =
      (D1:$4fa35699;D2:$623f;D3:$11cf;D4:($ac,$4a,$00,$00,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMUserVisual: TGUID =
      (D1:$4fa3569a;D2:$623f;D3:$11cf;D4:($ac,$4a,$00,$00,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMShadow: TGUID =
      (D1:$4fa3569b;D2:$623f;D3:$11cf;D4:($ac,$4a,$00,$00,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMViewportInterpolator: TGUID =
      (D1:$0de9eaa1;D2:$3b84;D3:$11d0;D4:($9b,$6d,$00,$00,$c0,$78,$1b,$c3));
  CLSID_CDirect3DRMFrameInterpolator: TGUID =
      (D1:$0de9eaa2;D2:$3b84;D3:$11d0;D4:($9b,$6d,$00,$00,$c0,$78,$1b,$c3));
  CLSID_CDirect3DRMMeshInterpolator: TGUID =
      (D1:$0de9eaa3;D2:$3b84;D3:$11d0;D4:($9b,$6d,$00,$00,$c0,$78,$1b,$c3));
  CLSID_CDirect3DRMLightInterpolator: TGUID =
      (D1:$0de9eaa6;D2:$3b84;D3:$11d0;D4:($9b,$6d,$00,$00,$c0,$78,$1b,$c3));
  CLSID_CDirect3DRMMaterialInterpolator: TGUID =
      (D1:$0de9eaa7;D2:$3b84;D3:$11d0;D4:($9b,$6d,$00,$00,$c0,$78,$1b,$c3));
  CLSID_CDirect3DRMTextureInterpolator: TGUID =
      (D1:$0de9eaa8;D2:$3b84;D3:$11d0;D4:($9b,$6d,$00,$00,$c0,$78,$1b,$c3));
  CLSID_CDirect3DRMProgressiveMesh: TGUID =
      (D1:$4516ec40;D2:$8f20;D3:$11d0;D4:($9b,$6d,$00,$00,$c0,$78,$1b,$c3));
  CLSID_CDirect3DRMClippedVisual: TGUID =
      (D1:$5434e72d;D2:$6d66;D3:$11d1;D4:($bb,$0b,$00,$00,$f8,$75,$86,$5a));

(*
 * Direct3DRM Object interfaces
 *)
  IID_IDirect3DRMObject: TGUID =
      (D1:$eb16cb00;D2:$d271;D3:$11ce;D4:($ac,$48,$00,$00,$c0,$38,$25,$a1));
  IID_IDirect3DRMObject2: TGUID =
      (D1:$4516ec7c;D2:$8f20;D3:$11d0;D4:($9b,$6d,$00,$00,$c0,$78,$1b,$c3));
  IID_IDirect3DRMDevice: TGUID =
      (D1:$e9e19280;D2:$6e05;D3:$11cf;D4:($ac,$4a,$00,$00,$c0,$38,$25,$a1));
  IID_IDirect3DRMDevice2: TGUID =
      (D1:$4516ec78;D2:$8f20;D3:$11d0;D4:($9b,$6d,$00,$00,$c0,$78,$1b,$c3));
  IID_IDirect3DRMDevice3: TGUID =
      (D1:$549f498b;D2:$bfeb;D3:$11d1;D4:($8e,$d8,$00,$a0,$c9,$67,$a4,$82));
  IID_IDirect3DRMViewport: TGUID =
      (D1:$eb16cb02;D2:$d271;D3:$11ce;D4:($ac,$48,$00,$00,$c0,$38,$25,$a1));
  IID_IDirect3DRMViewport2: TGUID =
      (D1:$4a1b1be6;D2:$bfed;D3:$11d1;D4:($8e,$d8,$00,$a0,$c9,$67,$a4,$82));
  IID_IDirect3DRMFrame: TGUID =
      (D1:$eb16cb03;D2:$d271;D3:$11ce;D4:($ac,$48,$00,$00,$c0,$38,$25,$a1));
  IID_IDirect3DRMFrame2: TGUID =
      (D1:$c3dfbd60;D2:$3988;D3:$11d0;D4:($9e,$c2,$00,$00,$c0,$29,$1a,$c3));
  IID_IDirect3DRMFrame3: TGUID =
      (D1:$ff6b7f70;D2:$a40e;D3:$11d1;D4:($91,$f9,$00,$00,$f8,$75,$8e,$66));
  IID_IDirect3DRMVisual: TGUID =
      (D1:$eb16cb04;D2:$d271;D3:$11ce;D4:($ac,$48,$00,$00,$c0,$38,$25,$a1));
  IID_IDirect3DRMMesh: TGUID =
      (D1:$a3a80d01;D2:$6e12;D3:$11cf;D4:($ac,$4a,$00,$00,$c0,$38,$25,$a1));
  IID_IDirect3DRMMeshBuilder: TGUID =
      (D1:$a3a80d02;D2:$6e12;D3:$11cf;D4:($ac,$4a,$00,$00,$c0,$38,$25,$a1));
  IID_IDirect3DRMMeshBuilder2: TGUID =
      (D1:$4516ec77;D2:$8f20;D3:$11d0;D4:($9b,$6d,$00,$00,$c0,$78,$1b,$c3));
  IID_IDirect3DRMMeshBuilder3: TGUID =
      (D1:$ff6b7f71;D2:$a40e;D3:$11d1;D4:($91,$f9,$00,$00,$f8,$75,$8e,$66));
  IID_IDirect3DRMFace: TGUID =
      (D1:$eb16cb07;D2:$d271;D3:$11ce;D4:($ac,$48,$00,$00,$c0,$38,$25,$a1));
  IID_IDirect3DRMFace2: TGUID =
      (D1:$4516ec81;D2:$8f20;D3:$11d0;D4:($9b,$6d,$00,$00,$c0,$78,$1b,$c3));
  IID_IDirect3DRMLight: TGUID =
      (D1:$eb16cb08;D2:$d271;D3:$11ce;D4:($ac,$48,$00,$00,$c0,$38,$25,$a1));
  IID_IDirect3DRMTexture: TGUID =
      (D1:$eb16cb09;D2:$d271;D3:$11ce;D4:($ac,$48,$00,$00,$c0,$38,$25,$a1));
  IID_IDirect3DRMTexture2: TGUID =
      (D1:$120f30c0;D2:$1629;D3:$11d0;D4:($94,$1c,$00,$80,$c8,$0c,$fa,$7b));
  IID_IDirect3DRMTexture3: TGUID =
      (D1:$ff6b7f73;D2:$a40e;D3:$11d1;D4:($91,$f9,$00,$00,$f8,$75,$8e,$66));
  IID_IDirect3DRMWrap: TGUID =
      (D1:$eb16cb0a;D2:$d271;D3:$11ce;D4:($ac,$48,$00,$00,$c0,$38,$25,$a1));
  IID_IDirect3DRMMaterial: TGUID =
      (D1:$eb16cb0b;D2:$d271;D3:$11ce;D4:($ac,$48,$00,$00,$c0,$38,$25,$a1));
  IID_IDirect3DRMMaterial2: TGUID =
      (D1:$ff6b7f75;D2:$a40e;D3:$11d1;D4:($91,$f9,$00,$00,$f8,$75,$8e,$66));
  IID_IDirect3DRMAnimation: TGUID =
      (D1:$eb16cb0d;D2:$d271;D3:$11ce;D4:($ac,$48,$00,$00,$c0,$38,$25,$a1));
  IID_IDirect3DRMAnimation2: TGUID =
      (D1:$ff6b7f77;D2:$a40e;D3:$11d1;D4:($91,$f9,$00,$00,$f8,$75,$8e,$66));
  IID_IDirect3DRMAnimationSet: TGUID =
      (D1:$eb16cb0e;D2:$d271;D3:$11ce;D4:($ac,$48,$00,$00,$c0,$38,$25,$a1));
  IID_IDirect3DRMAnimationSet2: TGUID =
      (D1:$ff6b7f79;D2:$a40e;D3:$11d1;D4:($91,$f9,$00,$00,$f8,$75,$8e,$66));
  IID_IDirect3DRMObjectArray: TGUID =
      (D1:$242f6bc2;D2:$3849;D3:$11d0;D4:($9b,$6d,$00,$00,$c0,$78,$1b,$c3));
  IID_IDirect3DRMDeviceArray: TGUID =
      (D1:$eb16cb10;D2:$d271;D3:$11ce;D4:($ac,$48,$00,$00,$c0,$38,$25,$a1));
  IID_IDirect3DRMViewportArray: TGUID =
      (D1:$eb16cb11;D2:$d271;D3:$11ce;D4:($ac,$48,$00,$00,$c0,$38,$25,$a1));
  IID_IDirect3DRMFrameArray: TGUID =
      (D1:$eb16cb12;D2:$d271;D3:$11ce;D4:($ac,$48,$00,$00,$c0,$38,$25,$a1));
  IID_IDirect3DRMVisualArray: TGUID =
      (D1:$eb16cb13;D2:$d271;D3:$11ce;D4:($ac,$48,$00,$00,$c0,$38,$25,$a1));
  IID_IDirect3DRMLightArray: TGUID =
      (D1:$eb16cb14;D2:$d271;D3:$11ce;D4:($ac,$48,$00,$00,$c0,$38,$25,$a1));
  IID_IDirect3DRMPickedArray: TGUID =
      (D1:$eb16cb16;D2:$d271;D3:$11ce;D4:($ac,$48,$00,$00,$c0,$38,$25,$a1));
  IID_IDirect3DRMFaceArray: TGUID =
      (D1:$eb16cb17;D2:$d271;D3:$11ce;D4:($ac,$48,$00,$00,$c0,$38,$25,$a1));
  IID_IDirect3DRMAnimationArray: TGUID =
      (D1:$d5f1cae0;D2:$4bd7;D3:$11d1;D4:($b9,$74,$00,$60,$08,$3e,$45,$f3));
  IID_IDirect3DRMUserVisual: TGUID =
      (D1:$59163de0;D2:$6d43;D3:$11cf;D4:($ac,$4a,$00,$00,$c0,$38,$25,$a1));
  IID_IDirect3DRMShadow: TGUID =
      (D1:$af359780;D2:$6ba3;D3:$11cf;D4:($ac,$4a,$00,$00,$c0,$38,$25,$a1));
  IID_IDirect3DRMShadow2: TGUID =
      (D1:$86b44e25;D2:$9c82;D3:$11d1;D4:($bb,$0b,$00,$a0,$c9,$81,$a0,$a6));
  IID_IDirect3DRMInterpolator: TGUID =
      (D1:$242f6bc1;D2:$3849;D3:$11d0;D4:($9b,$6d,$00,$00,$c0,$78,$1b,$c3));
  IID_IDirect3DRMProgressiveMesh: TGUID =
      (D1:$4516ec79;D2:$8f20;D3:$11d0;D4:($9b,$6d,$00,$00,$c0,$78,$1b,$c3));
  IID_IDirect3DRMPicked2Array: TGUID =
      (D1:$4516ec7b;D2:$8f20;D3:$11d0;D4:($9b,$6d,$00,$00,$c0,$78,$1b,$c3));
  IID_IDirect3DRMClippedVisual: TGUID =
      (D1:$5434e733;D2:$6d66;D3:$11d1;D4:($bb,$0b,$00,$00,$f8,$75,$86,$5a));

type
{$IFDEF D2COM}
  IDirect3DRMObject = class;
//  IDirect3DRMObject2 = class; moved to D3DRM, to avoid circular unit reference
  IDirect3DRMDevice = class;
  IDirect3DRMDevice2 = class;
  IDirect3DRMDevice3 = class;
  IDirect3DRMViewport = class;
  IDirect3DRMViewport2 = class;
  IDirect3DRMFrame = class;
  IDirect3DRMFrame2 = class;
  IDirect3DRMFrame3 = class;
  IDirect3DRMVisual = class;
  IDirect3DRMMesh = class;
  IDirect3DRMMeshBuilder = class;
  IDirect3DRMMeshBuilder2 = class;
  IDirect3DRMMeshBuilder3 = class;
  IDirect3DRMFace = class;
  IDirect3DRMFace2 = class;
  IDirect3DRMLight = class;
  IDirect3DRMTexture = class;
  IDirect3DRMTexture2 = class;
  IDirect3DRMTexture3 = class;
  IDirect3DRMWrap = class;
  IDirect3DRMMaterial = class;
  IDirect3DRMMaterial2 = class;
  IDirect3DRMAnimation = class;
  IDirect3DRMAnimation2 = class;
  IDirect3DRMAnimationSet = class;
  IDirect3DRMArray = class;
  IDirect3DRMObjectArray = class;
  IDirect3DRMDeviceArray = class;
  IDirect3DRMViewportArray = class;
  IDirect3DRMFrameArray = class;
  IDirect3DRMVisualArray = class;
  IDirect3DRMLightArray = class;
  IDirect3DRMPickedArray = class;
  IDirect3DRMFaceArray = class;
  IDirect3DRMAnimationArray = class;
  IDirect3DRMUserVisual = class;
  IDirect3DRMShadow = class;
  IDirect3DRMShadow2 = class;
  IDirect3DRMInterpolator = class;
  IDirect3DRMProgressiveMesh = class;
  IDirect3DRMPicked2Array = class;
  IDirect3DRMClippedVisual = class;
{$ELSE}
  IDirect3DRMObject = interface;
//  IDirect3DRMObject2 = interface; moved to D3DRM, to avoid circular unit reference
  IDirect3DRMDevice = interface;
  IDirect3DRMDevice2 = interface;
  IDirect3DRMDevice3 = interface;
  IDirect3DRMViewport = interface;
  IDirect3DRMViewport2 = interface;
  IDirect3DRMFrame = interface;
  IDirect3DRMFrame2 = interface;
  IDirect3DRMFrame3 = interface;
  IDirect3DRMVisual = interface;
  IDirect3DRMMesh = interface;
  IDirect3DRMMeshBuilder = interface;
  IDirect3DRMMeshBuilder2 = interface;
  IDirect3DRMMeshBuilder3 = interface;
  IDirect3DRMFace = interface;
  IDirect3DRMFace2 = interface;
  IDirect3DRMLight = interface;
  IDirect3DRMTexture = interface;
  IDirect3DRMTexture2 = interface;
  IDirect3DRMTexture3 = interface;
  IDirect3DRMWrap = interface;
  IDirect3DRMMaterial = interface;
  IDirect3DRMMaterial2 = interface;
  IDirect3DRMAnimation = interface;
  IDirect3DRMAnimation2 = interface;
  IDirect3DRMAnimationSet = interface;
  IDirect3DRMArray = interface;
  IDirect3DRMObjectArray = interface;
  IDirect3DRMDeviceArray = interface;
  IDirect3DRMViewportArray = interface;
  IDirect3DRMFrameArray = interface;
  IDirect3DRMVisualArray = interface;
  IDirect3DRMLightArray = interface;
  IDirect3DRMPickedArray = interface;
  IDirect3DRMFaceArray = interface;
  IDirect3DRMAnimationArray = interface;
  IDirect3DRMUserVisual = interface;
  IDirect3DRMShadow = interface;
  IDirect3DRMShadow2 = interface;
  IDirect3DRMInterpolator = interface;
  IDirect3DRMProgressiveMesh = interface;
  IDirect3DRMPicked2Array = interface;
  IDirect3DRMClippedVisual = interface;
{$ENDIF}

  PIDirect3DRMFaceArray = ^IDirect3DRMFaceArray;

  TD3DRMObjectCallback = procedure (lpD3DRMobj: IDirect3DRMObject;
      lpArg: Pointer); cdecl;
  TD3DRMFrameMoveCallback = procedure (lpD3DRMFrame: IDirect3DRMFrame3;
      lpArg: Pointer; delta: TD3DValue); cdecl;
  TD3DRMFrame3MoveCallback = procedure (lpD3DRMFrame: IDirect3DRMFrame3;
      lpArg: Pointer; delta: TD3DValue); cdecl;
  TD3DRMUpdateCallback = procedure (lpobj: IDirect3DRMDevice; lpArg: Pointer;
      iRectCount: Integer; const d3dRectUpdate: TD3DRect); cdecl;
  TD3DRMDevice3UpdateCallback = procedure (lpobj: IDirect3DRMDevice3;
      lpArg: Pointer; iRectCount: Integer; const d3dRectUpdate: TD3DRect);cdecl;
  TD3DRMUserVisualCallback = function (lpD3DRMUV: IDirect3DRMUserVisual;
      lpArg: Pointer; lpD3DRMUVreason: TD3DRMUserVisualReason;
      lpD3DRMDev: IDirect3DRMDevice;
      lpD3DRMview: IDirect3DRMViewport) : Integer; cdecl;
  TD3DRMLoadTextureCallback = function (tex_name: PAnsiChar; lpArg: Pointer;
      out lpD3DRMTex: IDirect3DRMTexture) : HResult; cdecl;
  TD3DRMLoadTexture3Callback = function (tex_name: PAnsiChar; lpArg: Pointer;
      out lpD3DRMTex: IDirect3DRMTexture3) : HResult; cdecl;
  TD3DRMLoadCallback = procedure (lpObject: IDirect3DRMObject;
      const ObjectGuid: TGUID; lpArg: Pointer); cdecl;
  TD3DRMDownSampleCallback = function (lpDirect3DRMTexture: IDirect3DRMTexture3;
      pArg: pointer; pDDSSrc, pDDSDst: IDirectDrawSurface) : HResult;
  TD3DRMValidationCallback = function (lpDirect3DRMTexture: IDirect3DRMTexture3;
      pArg: pointer; dwFlags, DWcRects: DWORD; const pRects: TRect) : HResult;

  PD3DRMPickDesc = ^TD3DRMPickDesc;
  TD3DRMPickDesc = packed record
    ulFaceIdx: DWORD;
    lGroupIdx: LongInt;
    vPosition: TD3DVector;
  end;

  PD3DRMPickDesc2 = ^TD3DRMPickDesc2;
  TD3DRMPickDesc2 = packed record
    ulFaceIdx: DWORD;
    lGroupIdx: LongInt;
    dvPosition: TD3DVector;
    tu, tv: TD3DValue;
    dvNormal: TD3DVector;
    dcColor: TD3DColor;
  end;

(*
 * Base class
 *)
{$IFDEF D2COM}
  IDirect3DRMObject = class (IUnknown)
{$ELSE}
  IDirect3DRMObject = interface (IUnknown)
    ['{eb16cb00-d271-11ce-ac48-0000c03825a1}']
{$ENDIF}
    (*
     * The methods for IDirect3DRMObject
     *)
    function Clone (pUnkOuter: IUnknown; riid: TGUID;
        var ppvObj: Pointer) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddDestroyCallback (lpCallback: TD3DRMObjectCallback;
        lpArg: Pointer) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function DeleteDestroyCallback (d3drmObjProc: TD3DRMObjectCallback;
        lpArg: Pointer) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetAppData (ulData: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetAppData: DWORD;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetName (lpName: PAnsiChar) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetName (var lpdwSize: DWORD; lpName: PAnsiChar) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetClassName (var lpdwSize: DWORD; lpName: PAnsiChar) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

// IDirect3DRMObject2 moved to D3DRM, to avoid circular unit reference

{$IFDEF D2COM}
  IDirect3DRMVisual = class (IDirect3DRMObject);
{$ELSE}
  IDirect3DRMVisual = interface (IDirect3DRMObject)
  end;
{$ENDIF}

{$IFDEF D2COM}
  IDirect3DRMDevice = class (IDirect3DRMObject)
{$ELSE}
  IDirect3DRMDevice = interface (IDirect3DRMObject)
    ['{e9e19280-6e05-11cf-ac4a-0000c03825a1}']
{$ENDIF}
    (*
     * IDirect3DRMDevice methods
     *)
    function Init (width: LongInt; height: LongInt) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function InitFromD3D (lpD3D: IDirect3D; lpD3DIMDev: IDirect3DDevice) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function InitFromClipper (lpDDClipper: IDirectDrawClipper; lpGUID: PGUID;
        width: Integer; height: Integer) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Update: HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddUpdateCallback (d3drmUpdateProc: TD3DRMUpdateCallback;
        arg: Pointer) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function DeleteUpdateCallback (d3drmUpdateProc: TD3DRMUpdateCallback;
        arg: Pointer) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetBufferCount (dwCount: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetBufferCount: DWORD;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetDither (bDither: BOOL) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetShades (ulShades: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetQuality (rqQuality: TD3DRMRenderQuality) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetTextureQuality (tqTextureQuality: TD3DRMTextureQuality) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetViewports (var lplpViewports: IDirect3DRMViewportArray) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetDither: BOOL;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetShades: DWORD;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetHeight: DWORD;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetWidth: DWORD;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetTrianglesDrawn: DWORD;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetWireframeOptions: DWORD;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetQuality: TD3DRMRenderQuality;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetColorModel: TD3DColorModel;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetTextureQuality: TD3DRMTextureQuality;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetDirect3DDevice (var lplpD3DDevice: IDirect3DDevice) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

{$IFDEF D2COM}
  IDirect3DRMDevice2 = class (IDirect3DRMDevice)
{$ELSE}
  IDirect3DRMDevice2 = interface (IDirect3DRMDevice)
    ['{4516ec78-8f20-11d0-9b6d-0000c0781bc3}']
{$ENDIF}
    (*
     * IDirect3DRMDevice2 methods
     *)
    function InitFromD3D2(lpD3D: IDirect3D2; lpD3DIMDev: IDirect3DDevice2) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function InitFromSurface(var lpGUID: TGUID; lpDD: IDirectDraw;
	    lpDDSBack: IDirectDrawSurface) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetRenderMode(dwFlags: DWORD ) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetRenderMode : DWORD;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetDirect3DDevice2(var lplpD3DDevice: IDirect3DDevice2) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

{$IFDEF D2COM}
  IDirect3DRMDevice3 = class (IDirect3DRMObject)
{$ELSE}
  IDirect3DRMDevice3 = interface (IDirect3DRMObject)
    ['{549f498b-bfeb-11d1-8ed8-00a0c967a482}']
{$ENDIF}
    (*
     * IDirect3DRMDevice methods
     *)
    function Init (width: LongInt; height: LongInt) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function InitFromD3D (lpD3D: IDirect3D2; lpD3DIMDev: IDirect3DDevice2) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function InitFromClipper (lpDDClipper: IDirectDrawClipper; lpGUID: PGUID;
        width: Integer; height: Integer) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Update: HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddUpdateCallback (d3drmUpdateProc: TD3DRMDevice3UpdateCallback;
        arg: Pointer) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function DeleteUpdateCallback (d3drmUpdateProc: TD3DRMDevice3UpdateCallback;
        arg: Pointer) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetBufferCount (dwCount: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetBufferCount: DWORD;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetDither (bDither: BOOL) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetShades (ulShades: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetQuality (rqQuality: TD3DRMRenderQuality) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetTextureQuality (tqTextureQuality: TD3DRMTextureQuality) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetViewports (var lplpViewports: IDirect3DRMViewportArray) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetDither: BOOL;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetShades: DWORD;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetHeight: DWORD;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetWidth: DWORD;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetTrianglesDrawn: DWORD;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetWireframeOptions: DWORD;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetQuality: TD3DRMRenderQuality;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetColorModel: TD3DColorModel;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetTextureQuality: TD3DRMTextureQuality;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetDirect3DDevice (var lplpD3DDevice: IDirect3DDevice) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    (*
     * IDirect3DRMDevice2 methods
     *)
    function InitFromD3D2(lpD3D: IDirect3D2; lpD3DIMDev: IDirect3DDevice2) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function InitFromSurface(var lpGUID: TGUID; lpDD: IDirectDraw;
	    lpDDSBack: IDirectDrawSurface) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetRenderMode(dwFlags: DWORD ) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetRenderMode : DWORD;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetDirect3DDevice2(var lplpD3DDevice: IDirect3DDevice2) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    (*
     * IDirect3DRMDevice3 methods
     *)
    function FindPreferredTextureFormat (dwBitDepths, dwFlags: DWORD;
        var lpDDPF: TDDPixelFormat) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function RenderStateChange (dwStateNum, dwVal, dwFlags: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}

    function LightStateChange (drsType: TD3DLightStateType; // defined different in header and help
        dwVal, dwFlags: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetStateChangeOptions (dwStateClass, dwStateNum: DWORD;
        var pdwFlags: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetStateChangeOptions ( dwStateClass, dwStateNum, dwFlags: DWORD)
         : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}

  end;

{$IFDEF D2COM}
  IDirect3DRMViewport = class (IDirect3DRMObject)
{$ELSE}
  IDirect3DRMViewport = interface (IDirect3DRMObject)
    ['{eb16cb02-d271-11ce-ac48-0000c03825a1}']
{$ENDIF}
    (*
     * IDirect3DRMViewport methods
     *)
    function Init (lpD3DRMDevice: IDirect3DRMDevice;
        lpD3DRMFrameCamera: IDirect3DRMFrame; xpos, ypos,
        width, height: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Clear: HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Render (lpD3DRMFrame: IDirect3DRMFrame) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetFront (rvFront: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetBack (rvBack: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetField (rvField: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetUniformScaling (bScale: BOOL) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetCamera (lpCamera: IDirect3DRMFrame) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetProjection (rptType: TD3DRMProjectionType) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Transform (var lprvDst: TD3DRMVector4D; var lprvSrc: TD3DVector) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function InverseTransform (var lprvDst: TD3DVector;
        var lprvSrc: TD3DRMVector4D) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Configure (lX, lY: LongInt; dwWidth, dwHeight: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function ForceUpdate (dwX1, dwY1, dwX2, dwY2: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetPlane (rvLeft, rvRight, rvBottom, rvTop: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetCamera (var lpCamera: IDirect3DRMFrame) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetDevice (var lpD3DRMDevice: IDirect3DRMDevice) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetPlane (var lpd3dvLeft, lpd3dvRight, lpd3dvBottom, lpd3dvTop:
        TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Pick (lX, lY: LongInt; var lplpVisuals: IDirect3DRMPickedArray) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetUniformScaling: BOOL;
         {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetX: LongInt;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetY: LongInt;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetWidth: DWORD;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetHeight: DWORD;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetField: TD3DValue;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetBack: TD3DValue;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetFront: TD3DValue;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetProjection: TD3DRMProjectionType;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetDirect3DViewport (var lplpD3DViewport: IDirect3DViewport) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

{$IFDEF D2COM}
  IDirect3DRMViewport2 = class (IDirect3DRMObject)
{$ELSE}
  IDirect3DRMViewport2 = interface (IDirect3DRMObject)
    ['{4a1b1be6-bfed-11d1-8ed8-00a0c967a482}']
{$ENDIF}
    (*
     * IDirect3DRMViewport2 methods
     *)
    function Init (lpD3DRMDevice: IDirect3DRMDevice3;
        lpD3DRMFrameCamera: IDirect3DRMFrame3; xpos, ypos,
        width, height: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Clear (dwFlags: DWORD): HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Render (lpD3DRMFrame: IDirect3DRMFrame3) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetFront (rvFront: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetBack (rvBack: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetField (rvField: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetUniformScaling (bScale: BOOL) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetCamera (lpCamera: IDirect3DRMFrame3) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetProjection (rptType: TD3DRMProjectionType) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Transform (var lprvDst: TD3DRMVector4D;
        var lprvSrc: TD3DVector) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function InverseTransform (var lprvDst: TD3DVector;
        var lprvSrc: TD3DRMVector4D) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Configure (lX, lY: LongInt; dwWidth, dwHeight: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function ForceUpdate (dwX1, dwY1, dwX2, dwY2: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetPlane (rvLeft, rvRight, rvBottom, rvTop: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetCamera (var lpCamera: IDirect3DRMFrame3) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetDevice (var lpD3DRMDevice: IDirect3DRMDevice3) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetPlane (var lpd3dvLeft, lpd3dvRight, lpd3dvBottom, lpd3dvTop:
        TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Pick (lX, lY: LongInt; var lplpVisuals: IDirect3DRMPickedArray) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetUniformScaling: BOOL;
         {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetX: LongInt;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetY: LongInt;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetWidth: DWORD;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetHeight: DWORD;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetField: TD3DValue;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetBack: TD3DValue;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetFront: TD3DValue;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetProjection: TD3DRMProjectionType;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetDirect3DViewport (var lplpD3DViewport: IDirect3DViewport) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function TransformVectors (dwNumVectors: DWORD; var lpDstVectors:
        TD3DRMVector4D; var lpSrcVectors: TD3DVector) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function InverseTransformVectors (dwNumVectors: DWORD; var lpDstVectors:
        TD3DRMVector4D; var lpSrcVectors: TD3DVector) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

{$IFDEF D2COM}
  IDirect3DRMFrame = class (IDirect3DRMVisual)
{$ELSE}
  IDirect3DRMFrame = interface (IDirect3DRMVisual)
    ['{eb16cb03-d271-11ce-ac48-0000c03825a1}']
{$ENDIF}
    (*
     * IDirect3DRMFrame methods
     *)
    function AddChild (lpD3DRMFrameChild: IDirect3DRMFrame) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddLight (lpD3DRMLight: IDirect3DRMLight) : HResult;
       {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddMoveCallback (d3drmFMC: TD3DRMFrameMoveCallback;
        lpArg: Pointer) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddTransform (rctCombine: TD3DRMCombineType;
        rmMatrix: TD3DRMMatrix4D) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddTranslation (rctCombine: TD3DRMCombineType; rvX, rvY, rvZ:
        TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddScale (rctCombine: TD3DRMCombineType; rvX, rvY, rvZ: TD3DValue) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddRotation (rctCombine: TD3DRMCombineType; rvX, rvY, rvZ,
        rvTheta: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddVisual (lpD3DRMVisual: IDirect3DRMVisual) : HResult;
      {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetChildren (var lplpChildren: IDirect3DRMFrameArray) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetColor: TD3DColor;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetLights (var lplpLights: IDirect3DRMLightArray) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetMaterialMode: TD3DRMMaterialMode;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetParent (var lplpParent: IDirect3DRMFrame) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetPosition (lpRef: IDirect3DRMFrame; var lprvPos: TD3DVector) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetRotation (lpRef: IDirect3DRMFrame; var lprvAxis: TD3DVector;
        var lprvTheta: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetScene (var lplpRoot: IDirect3DRMFrame) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetSortMode: TD3DRMSortMode;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetTexture (var lplpTexture: IDirect3DRMTexture) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetTransform (var rmMatrix: TD3DRMMatrix4D) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetVelocity (lpRef: IDirect3DRMFrame; var lprvVel: TD3DVector;
        fRotVel: BOOL) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetOrientation (lpRef: IDirect3DRMFrame; var lprvDir: TD3DVector;
        var lprvUp: TD3DVector) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetVisuals (var lplpVisuals: IDirect3DRMVisualArray) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetTextureTopology (var lpU, lpV: BOOL) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function InverseTransform (var lprvDst, lprvSrc: TD3DVector) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Load (lpvObjSource: Pointer; lpvObjID: Pointer;
        d3drmLOFlags: TD3DRMLoadOptions; d3drmLoadTextureProc:
        TD3DRMLoadTextureCallback; lpArgLTP: Pointer) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function LookAt (lpTarget, lpRef: IDirect3DRMFrame;
        rfcConstraint: TD3DRMFrameConstraint ) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Move (delta: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function DeleteChild (lpChild: IDirect3DRMFrame) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function DeleteLight (lpD3DRMLight: IDirect3DRMLight) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function DeleteMoveCallback (d3drmFMC: TD3DRMFrameMoveCallback;
        lpArg: Pointer) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function DeleteVisual (lpD3DRMVisual: IDirect3DRMVisual) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetSceneBackground: TD3DColor;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetSceneBackgroundDepth (var lplpDDSurface: IDirectDrawSurface) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetSceneFogColor: TD3DColor;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetSceneFogEnable: BOOL;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetSceneFogMode: TD3DRMFogMode;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetSceneFogParams (var lprvStart, lprvEnd, lprvDensity: TD3DValue) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetSceneBackground (rcColor: TD3DColor) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetSceneBackgroundRGB (rvRed, rvGreen, rvBlue: TD3DValue) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetSceneBackgroundDepth (lpImage: IDirectDrawSurface) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetSceneBackgroundImage (lpTexture: IDirect3DRMTexture) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetSceneFogEnable (bEnable: BOOL) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetSceneFogColor (rcColor: TD3DColor) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetSceneFogMode (rfMode: TD3DRMFogMode) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetSceneFogParams (rvStart, rvEnd, rvDensity: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetColor (rcColor: TD3DColor) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetColorRGB (rvRed, rvGreen, rvBlue: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetZbufferMode: TD3DRMZBufferMode;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetMaterialMode (rmmMode: TD3DRMMaterialMode) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetOrientation (lpRef: IDirect3DRMFrame; rvDx, rvDy, rvDz, rvUx,
        rvUy, rvUz: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetPosition (lpRef: IDirect3DRMFrame; rvX, rvY, rvZ: TD3DValue) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetRotation (lpRef: IDirect3DRMFrame; rvX, rvY, rvZ,
        rvTheta: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetSortMode (d3drmSM: TD3DRMSortMode) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetTexture (lpD3DRMTexture: IDirect3DRMTexture) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetTextureTopology (cylU, cylV: BOOL) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetVelocity (lpRef: IDirect3DRMFrame; rvX, rvY, rvZ: TD3DValue;
        fRotVel: BOOL) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetZbufferMode (d3drmZBM: TD3DRMZBufferMode) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Transform (var lpd3dVDst, lpd3dVSrc: TD3DVector) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

{$IFDEF D2COM}
  IDirect3DRMFrame2 = class (IDirect3DRMFrame)
{$ELSE}
  IDirect3DRMFrame2 = interface (IDirect3DRMFrame)
    ['{c3dfbd60-3988-11d0-9ec2-0000c0291ac3}']
{$ENDIF}
    (*
     * IDirect3DRMFrame2 methods
     *)
    function AddMoveCallback2 (d3drmFMC: TD3DRMFrameMoveCallback; lpArg:
        Pointer; dwFlags: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetBox (var lpTD3DRMBox: TD3DRMBox) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetBoxEnable : boolean;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetAxes (var dir, up: TD3DVector) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetMaterial (var lplpMaterial: IDirect3DRMMaterial) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetInheritAxes : boolean;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetHierarchyBox (var lpTD3DRMBox: TD3DRMBox) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetBox (var lpTD3DRMBox: TD3DRMBox) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetBoxEnable (bEnableFlag: BOOL) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetAxes (dx, dy, dz, ux, uy, uz: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetInheritAxes (inherit_from_parent: BOOL) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetMaterial (var lplpMaterial: IDirect3DRMMaterial) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetQuaternion (lpRef: IDirect3DRMFrame;
        var quat: TD3DRMQuaternion) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function RayPick (lpRefFrame: IDirect3DRMFrame; var ray: TD3DRMRay;
        dwFlags: DWORD; var lplpPicked2Array: IDirect3DRMPicked2Array) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Save (lpFilename: PAnsiChar; d3dFormat: TD3DRMXOFFormat;
        d3dSaveFlags: TD3DRMSaveOptions) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

{$IFDEF D2COM}
  IDirect3DRMFrame3 = class (IDirect3DRMVisual)
{$ELSE}
  IDirect3DRMFrame3 = interface (IDirect3DRMVisual)
    ['{ff6b7f70-a40e-11d1-91f9-0000f8758e66}']
{$ENDIF}
    (*
     * IDirect3DRMFrame3 methods
     *)
    function AddChild (lpD3DRMFrameChild: IDirect3DRMFrame3) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddLight (lpD3DRMLight: IDirect3DRMLight) : HResult;
       {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddMoveCallback (d3drmFMC: TD3DRMFrame3MoveCallback;
        lpArg: Pointer; Flags : DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddTransform (rctCombine: TD3DRMCombineType;
        rmMatrix: TD3DRMMatrix4D) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddTranslation (rctCombine: TD3DRMCombineType; rvX, rvY, rvZ:
        TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddScale (rctCombine: TD3DRMCombineType; rvX, rvY, rvZ: TD3DValue) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddRotation (rctCombine: TD3DRMCombineType; rvX, rvY, rvZ,
        rvTheta: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddVisual (lpD3DRMVisual: IDirect3DRMVisual) : HResult;
      {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetChildren (var lplpChildren: IDirect3DRMFrameArray) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetColor: TD3DColor;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetLights (var lplpLights: IDirect3DRMLightArray) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetMaterialMode: TD3DRMMaterialMode;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetParent (out lplpParent: IDirect3DRMFrame3) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetPosition (lpRef: IDirect3DRMFrame3; var lprvPos: TD3DVector) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetRotation (lpRef: IDirect3DRMFrame3; var lprvAxis: TD3DVector;
        var lprvTheta: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetScene (var lplpRoot: IDirect3DRMFrame3) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetSortMode: TD3DRMSortMode;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetTexture (var lplpTexture: IDirect3DRMTexture3) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetTransform (lpRefFrame: IDirect3DRMFrame3;
        var rmMatrix: TD3DRMMatrix4D) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetVelocity (lpRef: IDirect3DRMFrame3; var lprvVel: TD3DVector;
        fRotVel: BOOL) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetOrientation (lpRef: IDirect3DRMFrame3; var lprvDir: TD3DVector;
        var lprvUp: TD3DVector) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetVisuals (var pdwNumVisuals : DWORD; out Unk : IUnknown ) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function InverseTransform (var lprvDst, lprvSrc: TD3DVector) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Load (lpvObjSource: Pointer; lpvObjID: Pointer;
        d3drmLOFlags: TD3DRMLoadOptions; d3drmLoadTextureProc:
        TD3DRMLoadTexture3Callback; lpArgLTP: Pointer) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function LookAt (lpTarget, lpRef: IDirect3DRMFrame3;
        rfcConstraint: TD3DRMFrameConstraint ) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Move (delta: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function DeleteChild (lpChild: IDirect3DRMFrame3) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function DeleteLight (lpD3DRMLight: IDirect3DRMLight) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function DeleteMoveCallback (d3drmFMC: TD3DRMFrame3MoveCallback;
        lpArg: Pointer) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function DeleteVisual (lpD3DRMVisual: IDirect3DRMVisual) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetSceneBackground: TD3DColor;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetSceneBackgroundDepth (var lplpDDSurface: IDirectDrawSurface) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetSceneFogColor: TD3DColor;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetSceneFogEnable: BOOL;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetSceneFogMode: TD3DRMFogMode;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetSceneFogParams (var lprvStart, lprvEnd, lprvDensity: TD3DValue) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetSceneBackground (rcColor: TD3DColor) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetSceneBackgroundRGB (rvRed, rvGreen, rvBlue: TD3DValue) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetSceneBackgroundDepth (lpImage: IDirectDrawSurface) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetSceneBackgroundImage (lpTexture: IDirect3DRMTexture3) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetSceneFogEnable (bEnable: BOOL) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetSceneFogColor (rcColor: TD3DColor) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetSceneFogMode (rfMode: TD3DRMFogMode) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetSceneFogParams (rvStart, rvEnd, rvDensity: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetColor (rcColor: TD3DColor) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetColorRGB (rvRed, rvGreen, rvBlue: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetZbufferMode: TD3DRMZBufferMode;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetMaterialMode (rmmMode: TD3DRMMaterialMode) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetOrientation (lpRef: IDirect3DRMFrame3; rvDx, rvDy, rvDz, rvUx,
        rvUy, rvUz: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetPosition (lpRef: IDirect3DRMFrame3; rvX, rvY, rvZ: TD3DValue) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetRotation (lpRef: IDirect3DRMFrame3; rvX, rvY, rvZ,
        rvTheta: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetSortMode (d3drmSM: TD3DRMSortMode) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetTexture (lpD3DRMTexture: IDirect3DRMTexture3) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetVelocity (lpRef: IDirect3DRMFrame3; rvX, rvY, rvZ: TD3DValue;
        fRotVel: BOOL) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetZbufferMode (d3drmZBM: TD3DRMZBufferMode) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Transform (var lpd3dVDst, lpd3dVSrc: TD3DVector) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}

    function GetBox (var lpTD3DRMBox: TD3DRMBox) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetBoxEnable : boolean;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetAxes (var dir, up: TD3DVector) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetMaterial (var lplpMaterial: IDirect3DRMMaterial2) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetInheritAxes : boolean;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetHierarchyBox (var lpTD3DRMBox: TD3DRMBox) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetBox (var lpTD3DRMBox: TD3DRMBox) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetBoxEnable (bEnableFlag: BOOL) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetAxes (dx, dy, dz, ux, uy, uz: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetInheritAxes (inherit_from_parent: BOOL) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetMaterial (var lplpMaterial: IDirect3DRMMaterial2) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetQuaternion (lpRef: IDirect3DRMFrame3;
        var quat: TD3DRMQuaternion) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function RayPick (lpRefFrame: IDirect3DRMFrame3; var ray: TD3DRMRay;
        dwFlags: DWORD; var lplpPicked2Array: IDirect3DRMPicked2Array) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Save (lpFilename: PAnsiChar; d3dFormat: TD3DRMXOFFormat;
        d3dSaveFlags: TD3DRMSaveOptions) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function TransformVectors (lpRefFrame: IDirect3DRMFrame3;
        dwNumVectors: DWORD; var lpDstVectors: TD3DVector;
        var lpSrcVectors: TD3DVector) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function InverseTransformVectors (lpRefFrame: IDirect3DRMFrame3;
        dwNumVectors: DWORD; var lpDstVectors: TD3DVector;
        var lpSrcVectors: TD3DVector) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetTraversalOptions (dwFlags: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetTraversalOptions (var lpdwFlags: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetSceneFogMethod (dwFlags: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetSceneFogMethod (var lpdwFlags: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetMaterialOverride (
        var lpdmOverride: TD3DRMMaterialOverride) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetMaterialOverride (
        var lpdmOverride: TD3DRMMaterialOverride) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;


{$IFDEF D2COM}
  IDirect3DRMMesh = class (IDirect3DRMVisual)
{$ELSE}
  IDirect3DRMMesh = interface (IDirect3DRMVisual)
    ['{a3a80d01-6e12-11cf-ac4a-0000c03825a1}']
{$ENDIF}
    (*
     * IDirect3DRMMesh methods
     *)
    function Scale (sx, sy, sz: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Translate (tx, ty, tz: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetBox (var lpTD3DRMBox: TD3DRMBox) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddGroup (vCount, fCount, vPerFace: DWORD; var fData: DWORD;
        var returnId: TD3DRMGroupIndex) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetVertices (id: TD3DRMGroupIndex; index, count: DWORD;
        var values: TD3DRMVertex) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetGroupColor (id: TD3DRMGroupIndex; value: TD3DColor) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetGroupColorRGB (id: TD3DRMGroupIndex; red, green,
        blue: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetGroupMapping (id: TD3DRMGroupIndex;
        value: TD3DRMMapping) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetGroupQuality (id: TD3DRMGroupIndex;
        value: TD3DRMRenderQuality) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetGroupMaterial (id: TD3DRMGroupIndex; value:
        IDirect3DRMMaterial) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetGroupTexture (id: TD3DRMGroupIndex; value: IDirect3DRMTexture) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetGroupCount: DWORD;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetGroup (id: TD3DRMGroupIndex; var vCount, fCount, vPerFace,
        fDataSize; fData: pointer) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetVertices (id: TD3DRMGroupIndex; index, count : DWORD;
    	returnPtr : pointer) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetGroupColor (id: TD3DRMGroupIndex) : TD3DColor;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetGroupMapping (id: TD3DRMGroupIndex) : TD3DRMMapping;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetGroupQuality (id: TD3DRMGroupIndex) : TD3DRMRenderQuality;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetGroupMaterial (id: TD3DRMGroupIndex;
        var returnPtr: IDirect3DRMMaterial) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetGroupTexture (id: TD3DRMGroupIndex;
        var returnPtr: IDirect3DRMTexture) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

{$IFDEF D2COM}
  IDirect3DRMProgressiveMesh = class (IDirect3DRMVisual)
{$ELSE}
  IDirect3DRMProgressiveMesh = interface (IDirect3DRMVisual)
    ['{4516ec79-8f20-11d0-9b6d-0000c0781bc3}']
{$ENDIF}
    (*
     * IDirect3DRMProgressiveMesh methods
     *)
    function Load (lpSource, lpObjID: pointer; dloLoadflags : TD3DRMLoadOptions;
        lpCallback: TD3DRMLoadTextureCallback; lpArg: pointer) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetLoadStatus (var lpStatus: TD3DRMPMeshLoadStatus) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetMinRenderDetail (d3dVal: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Abort (dwFlags: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetFaceDetail (var lpdwCount: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetVertexDetail (var lpdwCount: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetFaceDetail (dwCount: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetVertexDetail (dwCount: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetFaceDetailRange (var lpdwMin, lpdwMax: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetVertexDetailRange (var lpdwMin, lpdwMax: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetDetail (var lpdvVal: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetDetail (lpdvVal: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function RegisterEvents (hEvent: THANDLE; dwFlags, dwReserved: DWORD)
        : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateMesh (var lplpD3DRMMesh: IDirect3DRMMesh) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Duplicate (var lplpD3DRMPMesh: IDirect3DRMProgressiveMesh)
        : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetBox (var lpBBox: TD3DRMBox) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetQuality (quality: TD3DRMRenderQuality) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetQuality (var lpdwquality: TD3DRMRenderQuality) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

{$IFDEF D2COM}
  IDirect3DRMShadow = class (IDirect3DRMVisual)
{$ELSE}
  IDirect3DRMShadow = interface (IDirect3DRMVisual)
    ['{af359780-6ba3-11cf-ac4a-0000c03825a1}']
{$ENDIF}
    (*
     * IDirect3DRMShadow methods
     *)
    function Init (lpD3DRMVisual: IDirect3DRMVisual;
        lpD3DRMLight: IDirect3DRMLight;
        px, py, pz, nx, ny, nz: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

{$IFDEF D2COM}
  IDirect3DRMShadow2 = class (IDirect3DRMShadow)
{$ELSE}
  IDirect3DRMShadow2 = interface (IDirect3DRMShadow)
    ['{86b44e25-9c82-11d1-bb0b-00a0c981a0a6}']
{$ENDIF}
    (*
     * IDirect3DRMShadow2 methods
     *)
    function GetVisual (var lplpDirect3DRMVisual: IDirect3DRMVisual) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetVisual (lpDirect3DRMVisual: IDirect3DRMVisual;
        dwFlags: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetLight (var lplpDirect3DRMLight: IDirect3DRMLight) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetLight (lplpDirect3DRMLight: IDirect3DRMLight;
        dwFlags: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetPlane (
        var pdvPX, pdvPY, pdvPZ, pdvNX, pdvNY, pdvNZ: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetPlane (px, py, pz, nx, ny, nz: TD3DValue;
        dwFlags: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetOptions (var pdwOptions: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetOptions (dwOptions: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}

  end;

{$IFDEF D2COM}
  IDirect3DRMFace = class (IDirect3DRMObject)
{$ELSE}
  IDirect3DRMFace = interface (IDirect3DRMObject)
    ['{eb16cb07-d271-11ce-ac48-0000c03825a1}']
{$ENDIF}
    (*
     * IDirect3DRMFace methods
     *)
    function AddVertex (x, y, z: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddVertexAndNormalIndexed (vertex: DWORD; normal: DWORD) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetColorRGB (red, green, blue: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetColor (color: TD3DColor) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetTexture (lpD3DRMTexture: IDirect3DRMTexture) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetTextureCoordinates (vertex: DWORD; u, v: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetMaterial (lpMat: IDirect3DRMMaterial) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetTextureTopology (cylU, cylV: BOOL) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetVertex (index: DWORD; var lpPosition: TD3DVector;
        var lpNormal: TD3DVector) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetVertices (var lpdwVertexCount: DWORD;
        var lpPosition, lpNormal: TD3DVector) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetTextureCoordinates (index: DWORD; var lpU, lpV: TD3DValue) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetTextureTopology (var lpU, lpV: BOOL) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetNormal (var lpNormal: TD3DVector) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetTexture (var lplpTexture: IDirect3DRMTexture) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetMaterial (var lpMat: IDirect3DRMMaterial) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetVertexCount: Integer;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetVertexIndex (dwIndex: DWORD) : Integer;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetTextureCoordinateIndex (dwIndex: DWORD) : Integer;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetColor: TD3DColor;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

{$IFDEF D2COM}
  IDirect3DRMFace2 = class (IDirect3DRMObject)
{$ELSE}
  IDirect3DRMFace2 = interface (IDirect3DRMObject)
    ['{4516ec81-8f20-11d0-9b6d-0000c0781bc3}']
{$ENDIF}
    (*
     * IDirect3DRMFace2 methods
     *)
    function AddVertex (x, y, z: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddVertexAndNormalIndexed (vertex: DWORD; normal: DWORD) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetColorRGB (red, green, blue: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetColor (color: TD3DColor) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetTexture (lpD3DRMTexture: IDirect3DRMTexture3) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetTextureCoordinates (vertex: DWORD; u, v: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetMaterial (lpMat: IDirect3DRMMaterial2) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetTextureTopology (cylU, cylV: BOOL) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetVertex (index: DWORD; var lpPosition: TD3DVector;
        var lpNormal: TD3DVector) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetVertices (var lpdwVertexCount: DWORD;
        var lpPosition, lpNormal: TD3DVector) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetTextureCoordinates (index: DWORD; var lpU, lpV: TD3DValue) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetTextureTopology (var lpU, lpV: BOOL) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetNormal (var lpNormal: TD3DVector) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetTexture (var lplpTexture: IDirect3DRMTexture3) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetMaterial (var lpMat: IDirect3DRMMaterial2) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetVertexCount: Integer;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetVertexIndex (dwIndex: DWORD) : Integer;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetTextureCoordinateIndex (dwIndex: DWORD) : Integer;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetColor: TD3DColor;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

{$IFDEF D2COM}
  IDirect3DRMMeshBuilder = class (IDirect3DRMVisual)
{$ELSE}
  IDirect3DRMMeshBuilder = interface (IDirect3DRMVisual)
    ['{a3a80d02-6e12-11cf-ac4a-0000c03825a1}']
{$ENDIF}
    (*
     * IDirect3DRMMeshBuilder methods
     *)
    function Load (lpvObjSource, lpvObjID: Pointer; d3drmLOFlags:
        TD3DRMLoadOptions; d3drmLoadTextureProc: TD3DRMLoadTextureCallback;
        lpvArg: Pointer) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Save (lpFilename: PChar; TD3DRMXOFFormat: TD3DRMXOFFormat;
        d3drmSOContents: TD3DRMSaveOptions) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Scale (sx, sy, sz: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Translate (tx, ty, tz: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetColorSource (source: TD3DRMColorSource) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetBox (var lpTD3DRMBox: TD3DRMBox) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GenerateNormals : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetColorSource: TD3DRMColorSource;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddMesh (lpD3DRMMesh: IDirect3DRMMesh) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddMeshBuilder (lpD3DRMMeshBuild: IDirect3DRMMeshBuilder) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddFrame (lpD3DRMFrame: IDirect3DRMFrame) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddFace (lpD3DRMFace: IDirect3DRMFace) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddFaces (dwVertexCount: DWORD; var lpD3DVertices: TD3DVector;
        normalCount: DWORD; lpNormals: PD3DVector; var lpFaceData: DWORD;
        lplpD3DRMFaceArray: PIDirect3DRMFaceArray) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function ReserveSpace (vertexCount, normalCount, faceCount: DWORD) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetColorRGB (red, green, blue: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetColor (color: TD3DColor) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetTexture (lpD3DRMTexture: IDirect3DRMTexture) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetMaterial (lpIDirect3DRMmaterial: IDirect3DRMMaterial) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetTextureTopology (cylU, cylV: BOOL) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetQuality (quality: TD3DRMRenderQuality) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetPerspective (perspective: BOOL) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetVertex (index: DWORD; x, y, z: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetNormal (index: DWORD; x, y, z: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetTextureCoordinates (index: DWORD; u, v: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetVertexColor (index: DWORD; color: TD3DColor) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetVertexColorRGB (index: DWORD; red, green, blue: TD3DValue) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetFaces (var lplpD3DRMFaceArray: IDirect3DRMFaceArray) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetVertices (var vcount: DWORD; var vertices : TD3DVector;
        var ncount : DWORD;
        var normals : TD3DVector;
        var face_data_size, face_data : DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetTextureCoordinates(index : DWORD; var u, v : TD3DValue) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddVertex (x, y, z: TD3DValue) : Integer;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddNormal (x, y, z: TD3DValue) : Integer;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateFace (var lplpd3drmFace: IDirect3DRMFace) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetQuality: TD3DRMRenderQuality;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetPerspective: BOOL;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetFaceCount: Integer;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetVertexCount: Integer;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetVertexColor (index: DWORD) : TD3DColor;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateMesh (var lplpD3DRMMesh: IDirect3DRMMesh) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

{$IFDEF D2COM}
  IDirect3DRMMeshBuilder2 = class (IDirect3DRMMeshBuilder)
{$ELSE}
  IDirect3DRMMeshBuilder2 = interface (IDirect3DRMMeshBuilder)
    ['{4516ec77-8f20-11d0-9b6d-0000c0781bc3}']
{$ENDIF}
    (*
     * IDirect3DRMMeshBuilder2 methods
     *)
    function GenerateNormals2 (
        dvCreaseAngle: TD3DValue; dwFlags: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetFace (dwIndex: DWORD; lplpD3DRMFace: IDirect3DRMFace) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

{$IFDEF D2COM}
  IDirect3DRMMeshBuilder3 = class (IDirect3DRMVisual)
{$ELSE}
  IDirect3DRMMeshBuilder3 = interface (IDirect3DRMVisual)
    ['{ff6b7f71-a40e-11d1-91f9-0000f8758e66}']
{$ENDIF}
    (*
     * IDirect3DRMMeshBuilder3 methods
     *)
    function Load (lpvObjSource, lpvObjID: Pointer;
        d3drmLOFlags: TD3DRMLoadOptions;
        d3drmLoadTextureProc: TD3DRMLoadTexture3Callback;
        lpvArg: Pointer) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Save (lpFilename: PAnsiChar; TD3DRMXOFFormat: TD3DRMXOFFormat;
        d3drmSOContents: TD3DRMSaveOptions) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Scale (sx, sy, sz: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Translate (tx, ty, tz: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetColorSource (source: TD3DRMColorSource) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetBox (var lpTD3DRMBox: TD3DRMBox) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GenerateNormals (
        dvCreaseAngle: TD3DValue; dwFlags: DWORD): HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetColorSource: TD3DRMColorSource;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddMesh (lpD3DRMMesh: IDirect3DRMMesh) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddMeshBuilder (
        lpD3DRMMeshBuild: IDirect3DRMMeshBuilder3) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddFrame (lpD3DRMFrame: IDirect3DRMFrame3) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddFace (lpD3DRMFace: IDirect3DRMFace2) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddFaces (dwVertexCount: DWORD; var lpD3DVertices: TD3DVector;
        normalCount: DWORD; lpNormals: PD3DVector; var lpFaceData: DWORD;
        lplpD3DRMFaceArray: PIDirect3DRMFaceArray) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function ReserveSpace (vertexCount, normalCount, faceCount: DWORD) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetColorRGB (red, green, blue: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetColor (color: TD3DColor) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetTexture (lpD3DRMTexture: IDirect3DRMTexture3) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetMaterial (lpIDirect3DRMmaterial: IDirect3DRMMaterial2) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetTextureTopology (cylU, cylV: BOOL) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetQuality (quality: TD3DRMRenderQuality) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetPerspective (perspective: BOOL) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetVertex (index: DWORD; x, y, z: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetNormal (index: DWORD; x, y, z: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetTextureCoordinates (index: DWORD; u, v: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetVertexColor (index: DWORD; color: TD3DColor) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetVertexColorRGB (index: DWORD; red, green, blue: TD3DValue) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetFaces (var lplpD3DRMFaceArray: IDirect3DRMFaceArray) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetGeometry (var vcount: DWORD; var vertices : TD3DVector;
        var ncount : DWORD; var normals : TD3DVector;
        var face_data_size, face_data : DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetTextureCoordinates(index : DWORD; var u, v : TD3DValue) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddVertex (x, y, z: TD3DValue) : Integer;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddNormal (x, y, z: TD3DValue) : Integer;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateFace (var lplpd3drmFace: IDirect3DRMFace2) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetQuality: TD3DRMRenderQuality;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetPerspective: BOOL;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetFaceCount: Integer;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetVertexCount: Integer;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetVertexColor (index: DWORD) : TD3DColor;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateMesh (var lplpD3DRMMesh: IDirect3DRMMesh) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetFace
        (dwIndex: DWORD; lplpD3DRMFace: IDirect3DRMFace2) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetVertex (dwIndex: DWORD; var lpVector: TD3DVector) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetNormal (dwIndex: DWORD; var lpVector: TD3DVector) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function DeleteVertices (dwFirstIndex, dwCount: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function DeleteNormals (dwFirstIndex, dwCount: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function DeleteFace (lpFace: IDirect3DRMFace2) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Empty (dwFlags: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Optimize (dwFlags: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddFacesIndexed (dwFlags: DWORD; var lpdwvIndices: DWORD;
        lpdwIndexFirst, lpdwCount: PDWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateSubMesh (var lplpUnk: IUnknown) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetParentMesh (dwFlags: DWORD; var lplpUnk: IUnknown) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetSubMeshes (lpdwCount: PDWORD; var lpUnk: IUnknown) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function DeleteSubMesh (lplpUnk: IUnknown) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Enable (dwFlags: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetEnable (var lpdwFlags: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddTriangles (dwFlags, dwFormat, dwVertexCount:  DWORD;
        lpData: pointer) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetVertices
        (dwFirst, dwCount: DWORD; var lpdvVector: TD3DVector) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetVertices (dwFirst: DWORD; var lpdwCount: DWORD;
        lpdvVector: PD3DVector) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetNormals
        (dwFirst, dwCount: DWORD; var lpdvVector: TD3DVector) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetNormals (dwFirst: DWORD; lpdwCount: PDWORD;
        var lpdvVector: TD3DVector) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetNormalCount : integer;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

{$IFDEF D2COM}
  IDirect3DRMLight = class (IDirect3DRMObject)
{$ELSE}
  IDirect3DRMLight = interface (IDirect3DRMObject)
    ['{eb16cb08-d271-11ce-ac48-0000c03825a1}']
{$ENDIF}
    (*
     * IDirect3DRMLight methods
     *)
    function SetType (d3drmtType: TD3DRMLightType) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetColor (rcColor: TD3DColor) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetColorRGB (rvRed, rvGreen, rvBlue: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetRange (rvRange: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetUmbra (rvAngle: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetPenumbra (rvAngle: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetConstantAttenuation (rvAtt: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetLinearAttenuation (rvAtt: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetQuadraticAttenuation (rvAtt: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetRange: TD3DValue;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetUmbra: TD3DValue;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetPenumbra: TD3DValue;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetConstantAttenuation: TD3DValue;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetLinearAttenuation: TD3DValue;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetQuadraticAttenuation: TD3DValue;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetColor: TD3DColor;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetType: TD3DRMLightType;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetEnableFrame (lpEnableFrame: IDirect3DRMFrame) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetEnableFrame (var lplpEnableFrame: IDirect3DRMFrame) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

{$IFDEF D2COM}
  IDirect3DRMTexture = class (IDirect3DRMVisual)
{$ELSE}
  IDirect3DRMTexture = interface (IDirect3DRMVisual)
    ['{eb16cb09-d271-11ce-ac48-0000c03825a1}']
{$ENDIF}
    (*
     * IDirect3DRMTexture methods
     *)
    function InitFromFile (filename: PAnsiChar) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function InitFromSurface (lpDDS: IDirectDrawSurface) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function InitFromResource (rs: HRSRC) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Changed (bPixels, bPalette: BOOL) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetColors (ulColors: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetShades (ulShades: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetDecalSize (rvWidth, rvHeight: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetDecalOrigin (lX, lY: LongInt) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetDecalScale (dwScale: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetDecalTransparency (bTransp: BOOL) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetDecalTransparentColor (rcTransp: TD3DColor) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetDecalSize (var lprvWidth, lprvHeight: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetDecalOrigin (var lplX, lplY: LongInt) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetImage: PD3DRMImage;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetShades: DWORD;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetColors: DWORD;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetDecalScale: DWORD;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetDecalTransparency: BOOL;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetDecalTransparentColor: TD3DColor;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

{$IFDEF D2COM}
  IDirect3DRMTexture2 = class (IDirect3DRMTexture)
{$ELSE}
  IDirect3DRMTexture2 = interface (IDirect3DRMTexture)
    ['{120f30c0-1629-11d0-941c-0080c80cfa7b}']
{$ENDIF}
    (*
     * IDirect3DRMTexture2 methods
     *)
    function InitFromImage (var lpImage: TD3DRMImage) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function InitFromResource2 (hModule: HModule;
        strName, strType: PAnsiChar) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GenerateMIPMap (dwFlags: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

{$IFDEF D2COM}
  IDirect3DRMTexture3 = class (IDirect3DRMTexture2)
{$ELSE}
  IDirect3DRMTexture3 = interface (IDirect3DRMTexture2)
    ['{ff6b7f73-a40e-11d1-91f9-0000f8758e66}']
{$ENDIF}
    (*
     * IDirect3DRMTexture3 methods
     *)
    function GetSurface
        (dwFlags: DWORD; var lplpDDS: IDirectDrawSurface) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetCacheOptions (lImportance: integer; dwFlags: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetCacheOptions (var lplImportance: integer; var lpdwFlags: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetDownsampleCallback (
        pCallback: TD3DRMDownSampleCallback; pArg: pointer) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetValidationCallback (
        pCallback: TD3DRMValidationCallback; pArg: pointer) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

{$IFDEF D2COM}
  IDirect3DRMWrap = class (IDirect3DRMObject)
{$ELSE}
  IDirect3DRMWrap = interface (IDirect3DRMObject)
    ['{eb16cb0a-d271-11ce-ac48-0000c03825a1}']
{$ENDIF}
    (*
     * IDirect3DRMWrap methods
     *)
    function Init (d3drmwt: TD3DRMWrapType; lpd3drmfRef: IDirect3DRMFrame;
        ox, oy, oz, dx, dy, dz, ux, uy, uz, ou, ov, su, sv: TD3DValue)
        : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Apply (lpObject: IDirect3DRMObject) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function ApplyRelative
        (frame: IDirect3DRMFrame; mesh: IDirect3DRMObject) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

{$IFDEF D2COM}
  IDirect3DRMMaterial = class (IDirect3DRMObject)
{$ELSE}
  IDirect3DRMMaterial = interface (IDirect3DRMObject)
    ['{eb16cb0b-d271-11ce-ac48-0000c03825a1}']
{$ENDIF}
    (*
     * IDirect3DRMMaterial methods
     *)
    function SetPower (rvPower: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetSpecular (r, g, b: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetEmissive (r, g, b: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetPower: TD3DValue;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetSpecular (var lpr, lpg, lpb: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetEmissive (var lpr, lpg, lpb: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

{$IFDEF D2COM}
  IDirect3DRMMaterial2 = class (IDirect3DRMMaterial)
{$ELSE}
  IDirect3DRMMaterial2 = interface (IDirect3DRMMaterial)
    ['{ff6b7f75-a40e-11d1-91f9-0000f8758e66}']
{$ENDIF}
    (*
     * IDirect3DRMMaterial2 methods
     *)
    function GetAmbient(var r,g,b: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetAmbient(r,g,b: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

{$IFDEF D2COM}
  IDirect3DRMAnimation = class (IDirect3DRMObject)
{$ELSE}
  IDirect3DRMAnimation = interface (IDirect3DRMObject)
    ['{eb16cb0d-d271-11ce-ac48-0000c03825a1}']
{$ENDIF}
    (*
     * IDirect3DRMAnimation methods
     *)
    function SetOptions (d3drmanimFlags: TD3DRMAnimationOptions) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddRotateKey (rvTime: TD3DValue; var rqQuat: TD3DRMQuaternion) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddPositionKey (rvTime, rvX, rvY, rvZ: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddScaleKey (time, x, y, z: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function DeleteKey (time: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetFrame (lpD3DRMFrame: IDirect3DRMFrame) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetTime (rvTime: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetOptions: TD3DRMAnimationOptions;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

{$IFDEF D2COM}
  IDirect3DRMAnimation2 = class (IDirect3DRMObject)
{$ELSE}
  IDirect3DRMAnimation2 = interface (IDirect3DRMObject)
    ['{ff6b7f77-a40e-11d1-91f9-0000f8758e66}']
{$ENDIF}
    (*
     * IDirect3DRMAnimation methods
     *)
    function SetOptions (d3drmanimFlags: TD3DRMAnimationOptions) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddRotateKey (rvTime: TD3DValue; var rqQuat: TD3DRMQuaternion) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddPositionKey (rvTime, rvX, rvY, rvZ: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddScaleKey (time, x, y, z: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function DeleteKey (time: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetFrame (lpD3DRMFrame: IDirect3DRMFrame3) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetTime (rvTime: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetOptions: TD3DRMAnimationOptions;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetFrame (var lpD3DFrame: IDirect3DRMFrame3) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function DeleteKeyByID (dwID: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddKey (var lpKey: TD3DRMAnimationKey) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function ModifyKey (var lpKey: TD3DRMAnimationKey) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetKeys (dvTimeMin, dvTimeMax: TD3DValue; var lpdwNumKeys: DWORD;
        lpKey: PD3DRMAnimationKey) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

{$IFDEF D2COM}
  IDirect3DRMAnimationSet = class (IDirect3DRMObject)
{$ELSE}
  IDirect3DRMAnimationSet = interface (IDirect3DRMObject)
    ['{eb16cb0e-d271-11ce-ac48-0000c03825a1}']
{$ENDIF}
    (*
     * IDirect3DRMAnimationSet methods
     *)
    function AddAnimation (lpD3DRMAnimation: IDirect3DRMAnimation) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Load (lpvObjSource, lpvObjID: Pointer;
        d3drmLOFlags: TD3DRMLoadOptions;
        d3drmLoadTextureProc: TD3DRMLoadTextureCallback; lpArgLTP: Pointer;
        lpParentFrame: IDirect3DRMFrame) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function DeleteAnimation (lpD3DRMAnimation: IDirect3DRMAnimation) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetTime (rvTime: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

{$IFDEF D2COM}
  IDirect3DRMAnimationSet2 = class (IDirect3DRMObject)
{$ELSE}
  IDirect3DRMAnimationSet2 = interface (IDirect3DRMObject)
    ['{ff6b7f79-a40e-11d1-91f9-0000f8758e66}']
{$ENDIF}
    (*
     * IDirect3DRMAnimationSet methods
     *)
    function AddAnimation (lpD3DRMAnimation: IDirect3DRMAnimation2) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Load (lpvObjSource, lpvObjID: Pointer;
        d3drmLOFlags: TD3DRMLoadOptions;
        d3drmLoadTextureProc: TD3DRMLoadTexture3Callback; lpArgLTP: Pointer;
        lpParentFrame: IDirect3DRMFrame3) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function DeleteAnimation (lpD3DRMAnimation: IDirect3DRMAnimation2) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetTime (rvTime: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetAnimations : IDirect3DRMAnimationArray;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

{$IFDEF D2COM}
  IDirect3DRMUserVisual = class (IDirect3DRMVisual)
{$ELSE}
  IDirect3DRMUserVisual = interface (IDirect3DRMVisual)
    ['{59163de0-6d43-11cf-ac4a-0000c03825a1}']
{$ENDIF}
    (*
     * IDirect3DRMUserVisual methods
     *)
    function Init (d3drmUVProc: TD3DRMUserVisualCallback;
        lpArg: Pointer) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

{$IFDEF D2COM}
  IDirect3DRMArray = class (IUnknown)
{$ELSE}
  IDirect3DRMArray = interface (IUnknown)
{$ENDIF}
    function GetSize: DWORD;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    (* No GetElement method as it would get overloaded
     * in derived classes, and overloading is
     * a no-no in COM
     *)
  end;

{$IFDEF D2COM}
  IDirect3DRMObjectArray = class (IDirect3DRMArray)
{$ELSE}
  IDirect3DRMObjectArray = interface (IDirect3DRMArray)
  	['{242f6bc2-3849-11d0-9b6d-0000c0781bc3}']
{$ENDIF}
    function GetElement (index: DWORD; var lplpD3DRMObject:
        IDirect3DRMObject) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

{$IFDEF D2COM}
  IDirect3DRMDeviceArray = class (IDirect3DRMArray)
{$ELSE}
  IDirect3DRMDeviceArray = interface (IDirect3DRMArray)
    ['{eb16cb0e-d271-11ce-ac48-0000c03825a1}']
{$ENDIF}
    function GetElement (index: DWORD; var lplpD3DRMDevice:
        IDirect3DRMDevice) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

{$IFDEF D2COM}
  IDirect3DRMFrameArray = class (IDirect3DRMArray)
{$ELSE}
  IDirect3DRMFrameArray = interface (IDirect3DRMArray)
    ['{eb16cb12-d271-11ce-ac48-0000c03825a1}']
{$ENDIF}
    function GetElement (index: DWORD; var lplpD3DRMFrame: IDirect3DRMFrame) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

{$IFDEF D2COM}
  IDirect3DRMViewportArray = class (IDirect3DRMArray)
{$ELSE}
  IDirect3DRMViewportArray = interface (IDirect3DRMArray)
    ['{eb16cb11-d271-11ce-ac48-0000c03825a1}']
{$ENDIF}
    function GetElement (index: DWORD; var lplpD3DRMViewport:
        IDirect3DRMViewport) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

{$IFDEF D2COM}
  IDirect3DRMVisualArray = class (IDirect3DRMArray)
{$ELSE}
  IDirect3DRMVisualArray = interface (IDirect3DRMArray)
    ['{eb16cb13-d271-11ce-ac48-0000c03825a1}']
{$ENDIF}
    function GetElement (index: DWORD; var lplpD3DRMVisual:
        IDirect3DRMVisual) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

{$IFDEF D2COM}
  IDirect3DRMAnimationArray = class (IDirect3DRMArray)
{$ELSE}
  IDirect3DRMAnimationArray = interface (IDirect3DRMArray)
    ['{d5f1cae0-4bd7-11d1-b974-0060083e45f3}']
{$ENDIF}
    function GetElement (index: DWORD; var lplpD3DRMAnimation2:
        IDirect3DRMAnimation2) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

{$IFDEF D2COM}
  IDirect3DRMPickedArray = class (IDirect3DRMArray)
{$ELSE}
  IDirect3DRMPickedArray = interface (IDirect3DRMArray)
    ['{eb16cb16-d271-11ce-ac48-0000c03825a1}']
{$ENDIF}
    function GetPick (index: DWORD; var lplpVisual: IDirect3DRMVisual;
        var lplpFrameArray: IDirect3DRMFrameArray;
        var lpD3DRMPickDesc: TD3DRMPickDesc) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}

  end;

{$IFDEF D2COM}
  IDirect3DRMLightArray = class (IDirect3DRMArray)
{$ELSE}
  IDirect3DRMLightArray = interface (IDirect3DRMArray)
    ['{eb16cb14-d271-11ce-ac48-0000c03825a1}']
{$ENDIF}
    function GetElement (index: DWORD; var lplpD3DRMLight: IDirect3DRMLight) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;


{$IFDEF D2COM}
  IDirect3DRMFaceArray = class (IDirect3DRMArray)
{$ELSE}
  IDirect3DRMFaceArray = interface (IDirect3DRMArray)
    ['{eb16cb17-d271-11ce-ac48-0000c03825a1}']
{$ENDIF}
    function GetElement (index: DWORD; var lplpD3DRMFace: IDirect3DRMFace) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

{$IFDEF D2COM}
  IDirect3DRMPicked2Array = class (IDirect3DRMArray)
{$ELSE}
  IDirect3DRMPicked2Array = interface (IDirect3DRMArray)
    ['{4516ec7b-8f20-11d0-9b6d-0000c0781bc3}']
{$ENDIF}
    function GetPick (index: DWORD; out lplpVisual: IDirect3DRMVisual;
        out lplpFrameArray: IDirect3DRMFrameArray; var lpD3DRMPickDesc2:
      	TD3DRMPickDesc2) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

{$IFDEF D2COM}
  IDirect3DRMInterpolator = class (IDirect3DRMObject)
{$ELSE}
  IDirect3DRMInterpolator = interface (IDirect3DRMObject)
    ['{242f6bc1-3849-11d0-9b6d-0000c0781bc3}']
{$ENDIF}
    (*
     * IDirect3DRMInterpolator methods
     *)
    function AttachObject (lpD3DRMObject: IDirect3DRMObject) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetAttachedObjects
        (lpD3DRMObjectArray: IDirect3DRMObjectArray) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function DetachObject (lpD3DRMObject: IDirect3DRMObject) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetIndex (d3dVal: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetIndex : TD3DValue;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Interpolate (d3dVal: TD3DValue; lpD3DRMObject: IDirect3DRMObject;
	d3drmInterpFlags: TD3DRMInterpolationOptions) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

{$IFDEF D2COM}
  IDirect3DRMClippedVisual = class (IDirect3DRMObject)
{$ELSE}
  IDirect3DRMClippedVisual = interface (IDirect3DRMObject)
    ['{5434e733-6d66-11d1-bb0b-0000f875865a}']
{$ENDIF}
    (*
     * IDirect3DRMClippedVisual methods
     *)
    function Init (lpD3DRMVisual: IDirect3DRMVisual) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddPlane (lpRef: IDirect3DRMFrame3;
        var lpdvPoint, lpdvNormal: TD3DVector;
        dwFlags: DWORD; var lpdwReturnID: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function DeletePlane (dwID, dwFlags: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetPlaneIDs (var lpdwCount, lpdwID: DWORD;dwFlags: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetPlane (dwID: DWORD; lpRef: IDirect3DRMFrame3;
        var lpdvPoint, lpdvNormal: TD3DVector; dwFlags: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetPlane (dwID: DWORD; lpRef: IDirect3DRMFrame3;
        var lpdvPoint, lpdvNormal: TD3DVector; dwFlags: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;
  
implementation

end.

