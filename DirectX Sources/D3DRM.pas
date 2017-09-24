(*==========================================================================;
 *
 *  Copyright (C) 1995-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:	d3drm.h
 *  Content:	Direct3DRM include file
 *
 *  DirectX 6 Delphi adaptation by Erik Unger
 *
 *  Modyfied: 23.8.98
 *
 *  Download: http://www.bigfoot.com/~ungerik/
 *  E-Mail: ungerik@bigfoot.com
 *
 ***************************************************************************)

unit D3DRM;

interface

uses
{$IFDEF D2COM}
  OLE2,
{$ENDIF}
  Windows,
  D3DRMObj,
  D3DRMDef,
  DirectDraw,
  Direct3D;

function ErrorString(Value: HResult) : string;
  
type
  TD3DRMDevicePaletteCallback = procedure (lpDirect3DRMDev: IDirect3DRMDevice;
      lpArg: Pointer; dwIndex: DWORD; red, green, blue: LongInt); cdecl;

const
  IID_IDirect3DRM: TGUID =
      (D1:$2bc49361;D2:$8327;D3:$11cf;D4:($ac,$4a,$00,$00,$c0,$38,$25,$a1));
  IID_IDirect3DRM2: TGUID =
      (D1:$4516ecc8;D2:$8f20;D3:$11d0;D4:($9b,$6d,$00,$00,$c0,$78,$1b,$c3));
  IID_IDirect3DRM3: TGUID =
      (D1:$4516ec83;D2:$8f20;D3:$11d0;D4:($9b,$6d,$00,$00,$c0,$78,$1b,$c3));

     

  // old (D1:$02e34065;D2:$c243;D3:$11d1;D4:($8e,$d8,$00,$a0,$c9,$67,$a4,$82));

(*
 * Direct3DRM Object Class (for CoCreateInstance())
 *)
  CLSID_CDirect3DRM: TGUID =
      (D1:$4516ec41;D2:$8f20;D3:$11d0;D4:($9b,$6d,$00,$00,$c0,$78,$1b,$c3));

type
{$IFDEF D2COM}
  IDirect3DRM = class (IUnknown)
{$ELSE}
  IDirect3DRM = interface (IUnknown)
    ['{2bc49361-8327-11cf-ac4a-0000c03825a1}']
{$ENDIF}
    function CreateObject (rclsid: TGUID; pUnkOuter: IUnknown;
        riid: TGUID; var ppv: Pointer) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateFrame (lpD3DRMFrame: IDirect3DRMFrame;
        var lplpD3DRMFrame: IDirect3DRMFrame) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateMesh (var lplpD3DRMMesh: IDirect3DRMMesh) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateMeshBuilder (var lplpD3DRMMeshBuilder:
        IDirect3DRMMeshBuilder) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateFace (var lplpd3drmFace: IDirect3DRMFace) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateAnimation (var lplpD3DRMAnimation: IDirect3DRMAnimation) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateAnimationSet (var lplpD3DRMAnimationSet:
        IDirect3DRMAnimationSet) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateTexture (const lpImage: TD3DRMImage;
        var lplpD3DRMTexture: IDirect3DRMTexture) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateLight (d3drmltLightType: TD3DRMLightType;
        cColor: TD3DColor; var lplpD3DRMLight: IDirect3DRMLight) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateLightRGB (ltLightType: TD3DRMLightType; vRed,
        vGreen, vBlue: TD3DValue; var lplpD3DRMLight: IDirect3DRMLight) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateMaterial (vPower: TD3DValue; var lplpD3DRMMaterial:
        IDirect3DRMMaterial) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateDevice (dwWidth, dwHeight: DWORD; var lplpD3DRMDevice:
        IDirect3DRMDevice) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}

    (* Create a Windows Device using DirectDraw surfaces *)
    function CreateDeviceFromSurface (lpGUID: PGUID; lpDD: IDirectDraw;
        lpDDSBack: IDirectDrawSurface; var lplpD3DRMDevice: IDirect3DRMDevice) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}

      (* Create a Windows Device using D3D objects *)
    function CreateDeviceFromD3D (lpD3D: IDirect3D; lpD3DDev: IDirect3DDevice;
        var lplpD3DRMDevice: IDirect3DRMDevice) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}

    function CreateDeviceFromClipper (lpDDClipper: IDirectDrawClipper;
        lpGUID: PGUID; width, height: Integer; var lplpD3DRMDevice:
        IDirect3DRMDevice) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}

    function CreateTextureFromSurface ( lpDDS: IDirectDrawSurface;
        var lplpD3DRMTexture: IDirect3DRMTexture) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}

    function CreateShadow (lpVisual: IDirect3DRMVisual;
        lpLight: IDirect3DRMLight; px, py, pz, nx, ny, nz: TD3DValue;
        var lplpShadow: IDirect3DRMVisual) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateViewport (lpDev: IDirect3DRMDevice;
        lpCamera: IDirect3DRMFrame; dwXPos, dwYPos, dwWidth, dwHeight: DWORD;
        var lplpD3DRMViewport: IDirect3DRMViewport) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateWrap (wraptype: TD3DRMWrapType; lpRef: IDirect3DRMFrame;
        ox, oy, oz, dx, dy, dz, ux, uy, uz, ou, ov, su, sv: TD3DValue;
        var lplpD3DRMWrap: IDirect3DRMWrap) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateUserVisual (fn: TD3DRMUserVisualCallback; lpArg: Pointer;
        var lplpD3DRMUV: IDirect3DRMUserVisual) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function LoadTexture (lpFileName: PAnsiChar; var lplpD3DRMTexture:
        IDirect3DRMTexture) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function LoadTextureFromResource (rs: HRSRC; var lplpD3DRMTexture:
        IDirect3DRMTexture) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}

    function SetSearchPath (lpPath: PAnsiChar) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddSearchPath (lpPath: PAnsiChar) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetSearchPath (var lpdwSize: DWORD; lpszPath: PAnsiChar) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetDefaultTextureColors (dwColors: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetDefaultTextureShades (dwShades: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}

    function GetDevices (var lplpDevArray: IDirect3DRMDeviceArray) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetNamedObject (lpName: PAnsiChar; var lplpD3DRMObject:
        IDirect3DRMObject) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}

    function EnumerateObjects (func: TD3DRMObjectCallback; lpArg: Pointer) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}

    function Load (lpvObjSource, lpvObjID: Pointer; var lplpGUIDs: PGUID;
        dwcGUIDs: DWORD; d3drmLOFlags: TD3DRMLoadOptions; d3drmLoadProc:
        TD3DRMLoadCallback; lpArgLP: Pointer; d3drmLoadTextureProc:
        TD3DRMLoadTextureCallback; lpArgLTP: Pointer; lpParentFrame:
        IDirect3DRMFrame) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Tick (d3dvalTick: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

// Moved from D3DRMObj, to avoid circular unit reference:

{$IFDEF D2COM}
  IDirect3DRMObject2 = class (IUnknown)
{$ELSE}
  IDirect3DRMObject2 = interface (IUnknown)
    ['{4516ec7c-8f20-11d0-9b6d-0000c0781bc3}']
{$ENDIF}
    (*
     * IDirect3DRMObject2 methods
     *)
    function AddDestroyCallback (lpCallback: TD3DRMObjectCallback;
        lpArg: Pointer) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Clone (pUnkOuter: IUnknown; riid: TGUID;
        var ppvObj: Pointer) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function DeleteDestroyCallback (d3drmObjProc: TD3DRMObjectCallback;
        lpArg: Pointer) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetClientData (dwID: DWORD; var lplpvData: pointer) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetDirect3DRM (var lplpDirect3DRM: IDirect3DRM) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetName (var lpdwSize: DWORD; lpName: PAnsiChar) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetClientData (dwID: DWORD; lpvData: pointer; dwFlags: DWORD)
        : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetName (lpName: PAnsiChar) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetAge (dwFlags: DWORD; var pdwAge: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;
  
{$IFDEF D2COM}
  IDirect3DRM2 = class (IUnknown)
{$ELSE}
  IDirect3DRM2 = interface (IUnknown)
    ['{4516ecc8-8f20-11d0-9b6d-0000c0781bc3}']
{$ENDIF}
    function CreateObject (rclsid: TGUID; pUnkOuter: IUnknown;
        riid: TGUID; var ppv: Pointer) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateFrame (lpD3DRMFrame: IDirect3DRMFrame2;
        var lplpD3DRMFrame: IDirect3DRMFrame2) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateMesh (var lplpD3DRMMesh: IDirect3DRMMesh) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateMeshBuilder (var lplpD3DRMMeshBuilder:
        IDirect3DRMMeshBuilder2) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateFace (var lplpd3drmFace: IDirect3DRMFace) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateAnimation (var lplpD3DRMAnimation: IDirect3DRMAnimation) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateAnimationSet (var lplpD3DRMAnimationSet:
        IDirect3DRMAnimationSet) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateTexture (const lpImage: TD3DRMImage;
        var lplpD3DRMTexture: IDirect3DRMTexture2) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateLight (d3drmltLightType: TD3DRMLightType;
        cColor: TD3DColor; var lplpD3DRMLight: IDirect3DRMLight) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateLightRGB (ltLightType: TD3DRMLightType; vRed,
        vGreen, vBlue: TD3DValue; var lplpD3DRMLight: IDirect3DRMLight) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateMaterial (vPower: TD3DValue; var lplpD3DRMMaterial:
        IDirect3DRMMaterial) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateDevice (dwWidth, dwHeight: DWORD; var lplpD3DRMDevice:
        IDirect3DRMDevice2) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}

    (* Create a Windows Device using DirectDraw surfaces *)
    function CreateDeviceFromSurface (lpGUID: PGUID; lpDD: IDirectDraw;
        lpDDSBack: IDirectDrawSurface; var lplpD3DRMDevice: IDirect3DRMDevice2) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}

      (* Create a Windows Device using D3D objects *)
    function CreateDeviceFromD3D (lpD3D: IDirect3D2; lpD3DDev: IDirect3DDevice2;
        var lplpD3DRMDevice: IDirect3DRMDevice2) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}

    function CreateDeviceFromClipper (lpDDClipper: IDirectDrawClipper;
        lpGUID: PGUID; width, height: Integer; var lplpD3DRMDevice:
        IDirect3DRMDevice2) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}

    function CreateTextureFromSurface ( lpDDS: IDirectDrawSurface;
        var lplpD3DRMTexture: IDirect3DRMTexture2) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}

    function CreateShadow (lpVisual: IDirect3DRMVisual;
        lpLight: IDirect3DRMLight; px, py, pz, nx, ny, nz: TD3DValue;
        var lplpShadow: IDirect3DRMVisual) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateViewport (lpDev: IDirect3DRMDevice;
        lpCamera: IDirect3DRMFrame; dwXPos, dwYPos, dwWidth, dwHeight: DWORD;
        var lplpD3DRMViewport: IDirect3DRMViewport) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateWrap (wraptype: TD3DRMWrapType; lpRef: IDirect3DRMFrame;
        ox, oy, oz, dx, dy, dz, ux, uy, uz, ou, ov, su, sv: TD3DValue;
        var lplpD3DRMWrap: IDirect3DRMWrap) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateUserVisual (fn: TD3DRMUserVisualCallback; lpArg: Pointer;
        var lplpD3DRMUV: IDirect3DRMUserVisual) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function LoadTexture (lpFileName: PAnsiChar; var lplpD3DRMTexture:
        IDirect3DRMTexture2) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function LoadTextureFromResource (rs: HRSRC; var lplpD3DRMTexture:
        IDirect3DRMTexture2) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}

    function SetSearchPath (lpPath: PAnsiChar) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddSearchPath (lpPath: PAnsiChar) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetSearchPath (var lpdwSize: DWORD; lpszPath: PAnsiChar) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetDefaultTextureColors (dwColors: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetDefaultTextureShades (dwShades: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}

    function GetDevices (var lplpDevArray: IDirect3DRMDeviceArray) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetNamedObject (lpName: PAnsiChar; var lplpD3DRMObject:
        IDirect3DRMObject) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}

    function EnumerateObjects (func: TD3DRMObjectCallback; lpArg: Pointer) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}

    function Load (lpvObjSource, lpvObjID: Pointer; var lplpGUIDs: PGUID;
        dwcGUIDs: DWORD; d3drmLOFlags: TD3DRMLoadOptions; d3drmLoadProc:
        TD3DRMLoadCallback; lpArgLP: Pointer; d3drmLoadTextureProc:
        TD3DRMLoadTextureCallback; lpArgLTP: Pointer; lpParentFrame:
        IDirect3DRMFrame) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Tick (d3dvalTick: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateProgressiveMesh (var lplpD3DRMProgressiveMesh:
        IDirect3DRMProgressiveMesh) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

{$IFDEF D2COM}
  IDirect3DRM3 = class (IUnknown)
{$ELSE}
  IDirect3DRM3 = interface (IUnknown)
    ['{4516EC83-8f20-11d0-9B6D-0000c0781bc3}']
    // old ['{02e34065-c243-11d1-8ed8-00a0c967a482}']
{$ENDIF}
    function CreateObject (rclsid: TGUID; pUnkOuter: IUnknown;
        riid: TGUID; var ppv: Pointer) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateFrame (lpD3DRMFrame: IDirect3DRMFrame3;
        var lplpD3DRMFrame: IDirect3DRMFrame3) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateMesh (var lplpD3DRMMesh: IDirect3DRMMesh) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateMeshBuilder (var lplpD3DRMMeshBuilder:
        IDirect3DRMMeshBuilder3) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateFace (var lplpd3drmFace: IDirect3DRMFace) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateAnimation (var lplpD3DRMAnimation: IDirect3DRMAnimation2) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateAnimationSet (var lplpD3DRMAnimationSet:
        IDirect3DRMAnimationSet2) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateTexture (const lpImage: TD3DRMImage;
        var lplpD3DRMTexture: IDirect3DRMTexture3) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateLight (d3drmltLightType: TD3DRMLightType;
        cColor: TD3DColor; var lplpD3DRMLight: IDirect3DRMLight) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateLightRGB (ltLightType: TD3DRMLightType; vRed,
        vGreen, vBlue: TD3DValue; var lplpD3DRMLight: IDirect3DRMLight) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateMaterial (vPower: TD3DValue; var lplpD3DRMMaterial:
        IDirect3DRMMaterial2) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateDevice (dwWidth, dwHeight: DWORD; var lplpD3DRMDevice:
        IDirect3DRMDevice3) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}

    (* Create a Windows Device using DirectDraw surfaces *)
    function CreateDeviceFromSurface (lpGUID: PGUID; lpDD: IDirectDraw;
        lpDDSBack: IDirectDrawSurface; dwFlags: DWORD;
        var lplpD3DRMDevice: IDirect3DRMDevice3) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}

      (* Create a Windows Device using D3D objects *)
    function CreateDeviceFromD3D (lpD3D: IDirect3D2; lpD3DDev: IDirect3DDevice2;
        var lplpD3DRMDevice: IDirect3DRMDevice3) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}

    function CreateDeviceFromClipper (lpDDClipper: IDirectDrawClipper;
        lpGUID: PGUID; width, height: Integer;
        var lplpD3DRMDevice: IDirect3DRMDevice3) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}

    function CreateTextureFromSurface ( lpDDS: IDirectDrawSurface;
        var lplpD3DRMTexture: IDirect3DRMTexture3) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}

    function CreateShadow (pUnk: IUnknown; lpLight: IDirect3DRMLight;
        px, py, pz, nx, ny, nz: TD3DValue;
        var lplpShadow: IDirect3DRMShadow2) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateViewport (lpDev: IDirect3DRMDevice3;
        lpCamera: IDirect3DRMFrame3; dwXPos, dwYPos, dwWidth, dwHeight: DWORD;
        var lplpD3DRMViewport: IDirect3DRMViewport2) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateWrap (wraptype: TD3DRMWrapType; lpRef: IDirect3DRMFrame3;
        ox, oy, oz, dx, dy, dz, ux, uy, uz, ou, ov, su, sv: TD3DValue;
        var lplpD3DRMWrap: IDirect3DRMWrap) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateUserVisual (fn: TD3DRMUserVisualCallback; lpArg: Pointer;
        var lplpD3DRMUV: IDirect3DRMUserVisual) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function LoadTexture (lpFileName: PAnsiChar; var lplpD3DRMTexture:
        IDirect3DRMTexture3) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function LoadTextureFromResource (rs: HRSRC; var lplpD3DRMTexture:
        IDirect3DRMTexture3) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}

    function SetSearchPath (lpPath: PAnsiChar) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function AddSearchPath (lpPath: PAnsiChar) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetSearchPath (var lpdwSize: DWORD; lpszPath: PAnsiChar) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetDefaultTextureColors (dwColors: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetDefaultTextureShades (dwShades: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}

    function GetDevices (var lplpDevArray: IDirect3DRMDeviceArray) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetNamedObject (lpName: PAnsiChar; var lplpD3DRMObject:
        IDirect3DRMObject) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}

    function EnumerateObjects (func: TD3DRMObjectCallback; lpArg: Pointer) :
        HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}

    function Load (lpvObjSource, lpvObjID: Pointer; var lplpGUIDs: PGUID;
        dwcGUIDs: DWORD; d3drmLOFlags: TD3DRMLoadOptions; d3drmLoadProc:
        TD3DRMLoadCallback; lpArgLP: Pointer; d3drmLoadTextureProc:
        TD3DRMLoadTexture3Callback; lpArgLTP: Pointer; lpParentFrame:
        IDirect3DRMFrame3) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function Tick (d3dvalTick: TD3DValue) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function CreateProgressiveMesh (var lplpD3DRMProgressiveMesh:
        IDirect3DRMProgressiveMesh) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}

    (* Used with IDirect3DRMObject2 *)
    function RegisterClient (const rguid: TGUID; var lpdwID: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function UnregisterClient (const rguid: TGUID) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}

    function CreateClippedVisual (lpVisual: IDirect3DRMVisual;
        lpClippedVisual: IDirect3DRMClippedVisual) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function SetOptions (dwOptions: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
    function GetOptions (var lpdwOptions: DWORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}

  end;

const
  D3DRMERR_GENERIC                = HRESULT($88760000);

const
  D3DRM_OK                        = DD_OK;
  D3DRMERR_BADOBJECT              = D3DRMERR_GENERIC + 781;
  D3DRMERR_BADTYPE                = D3DRMERR_GENERIC + 782;
  D3DRMERR_BADALLOC               = D3DRMERR_GENERIC + 783;
  D3DRMERR_FACEUSED               = D3DRMERR_GENERIC + 784;
  D3DRMERR_NOTFOUND               = D3DRMERR_GENERIC + 785;
  D3DRMERR_NOTDONEYET             = D3DRMERR_GENERIC + 786;
  D3DRMERR_FILENOTFOUND           = D3DRMERR_GENERIC + 787;
  D3DRMERR_BADFILE                = D3DRMERR_GENERIC + 788;
  D3DRMERR_BADDEVICE              = D3DRMERR_GENERIC + 789;
  D3DRMERR_BADVALUE               = D3DRMERR_GENERIC + 790;
  D3DRMERR_BADMAJORVERSION        = D3DRMERR_GENERIC + 791;
  D3DRMERR_BADMINORVERSION        = D3DRMERR_GENERIC + 792;
  D3DRMERR_UNABLETOEXECUTE        = D3DRMERR_GENERIC + 793;
  D3DRMERR_LIBRARYNOTFOUND        = D3DRMERR_GENERIC + 794;
  D3DRMERR_INVALIDLIBRARY         = D3DRMERR_GENERIC + 795;
  D3DRMERR_PENDING                = D3DRMERR_GENERIC + 796;
  D3DRMERR_NOTENOUGHDATA          = D3DRMERR_GENERIC + 797;
  D3DRMERR_REQUESTTOOLARGE        = D3DRMERR_GENERIC + 798;
  D3DRMERR_REQUESTTOOSMALL        = D3DRMERR_GENERIC + 799;
  D3DRMERR_CONNECTIONLOST         = D3DRMERR_GENERIC + 800;
  D3DRMERR_LOADABORTED            = D3DRMERR_GENERIC + 801;
  D3DRMERR_NOINTERNET             = D3DRMERR_GENERIC + 802;
  D3DRMERR_BADCACHEFILE           = D3DRMERR_GENERIC + 803;
  D3DRMERR_BOXNOTSET	          = D3DRMERR_GENERIC + 804;
  D3DRMERR_BADPMDATA              = D3DRMERR_GENERIC + 805;
  D3DRMERR_CLIENTNOTREGISTERED    = D3DRMERR_GENERIC + 806;
  D3DRMERR_NOTCREATEDFROMDDS      = D3DRMERR_GENERIC + 807;
  D3DRMERR_NOSUCHKEY              = D3DRMERR_GENERIC + 808;
  D3DRMERR_INCOMPATABLEKEY        = D3DRMERR_GENERIC + 809;
  D3DRMERR_ELEMENTINUSE           = D3DRMERR_GENERIC + 810;
  D3DRMERR_TEXTUREFORMATNOTFOUND  = D3DRMERR_GENERIC + 811;

(* Create a Direct3DRM API *)
function Direct3DRMCreate (var lplpDirect3DRM: IDirect3DRM) : HResult; stdcall;

implementation

function Direct3DRMCreate; external 'D3DRM.DLL';

function ErrorString(Value: HResult) : string;
begin
  case Value of
    D3DRM_OK: Result := 'No error. Equivalent to DD_OK.';
    D3DRMERR_BADALLOC: Result := 'Out of memory.';
    D3DRMERR_BADDEVICE: Result := 'Device is not compatible with renderer.';
    D3DRMERR_BADFILE: Result := 'Data file is corrupt.';
    D3DRMERR_BADMAJORVERSION: Result := 'Bad DLL major version.';
    D3DRMERR_BADMINORVERSION: Result := 'Bad DLL minor version.';
    D3DRMERR_BADOBJECT: Result := 'Object expected in argument.';
    D3DRMERR_BADPMDATA: Result := 'The data in the .x file is corrupted. The conversion to a progressive mesh succeeded but produced an invalid progressive mesh in the .x file.';
    D3DRMERR_BADTYPE: Result := 'Bad argument type passed.';
    D3DRMERR_BADVALUE: Result := 'Bad argument value passed.';
    D3DRMERR_BOXNOTSET: Result := 'An attempt was made to access a bounding box (for example, with IDirect3DRMFrame3::GetBox) when no bounding box was set on the frame.';
    D3DRMERR_CLIENTNOTREGISTERED: Result := 'Client has not been registered. Call IDirect3DRM3::RegisterClient.';
    D3DRMERR_CONNECTIONLOST: Result := 'Data connection was lost during a load, clone, or duplicate.';
    D3DRMERR_ELEMENTINUSE: Result := 'Element can´t be modified or deleted while in use. To empty a submesh, call Empty() against its parent.';
//    D3DRMERR_ENTRYINUSE: Result := 'Vertex or normal entries are currently in use by a face and cannot be deleted.';
    D3DRMERR_FACEUSED: Result := 'Face already used in a mesh.';
    D3DRMERR_FILENOTFOUND: Result := 'File cannot be opened.';
//    D3DRMERR_INCOMPATIBLEKEY: Result := 'Specified animation key is incompatible. The key cannot be modified.';
    D3DRMERR_INVALIDLIBRARY: Result := 'Specified libary is invalid.';
//    D3DRMERR_INVALIDOBJECT: Result := 'Method received a pointer to an object that is invalid.';
//    D3DRMERR_INVALIDPARAMS: Result := 'One of the parameters passed to the method is invalid.';
    D3DRMERR_LIBRARYNOTFOUND: Result := 'Specified libary not found.';
    D3DRMERR_LOADABORTED: Result := 'Load aborted by user.';
    D3DRMERR_NOSUCHKEY: Result := 'Specified animation key does not exist.';
    D3DRMERR_NOTCREATEDFROMDDS: Result := 'Specified texture was not created from a DirectDraw Surface.';
    D3DRMERR_NOTDONEYET: Result := 'Unimplemented.';
    D3DRMERR_NOTENOUGHDATA: Result := 'Not enough data has been loaded to perform the requested operation.';
    D3DRMERR_NOTFOUND: Result := 'Object not found in specified place.';
//    D3DRMERR_OUTOFRANGE: Result := 'Specified value is out of range.';
    D3DRMERR_PENDING: Result := 'Data required to supply the requested information has not finished loading.';
    D3DRMERR_REQUESTTOOLARGE: Result := 'Attempt was made to set a level of detail in a progressive mesh greater than the maximum available.';
    D3DRMERR_REQUESTTOOSMALL: Result := 'Attempt was made to set the minimum rendering detail of a progressive mesh smaller than the detail in the base mesh (the minimum for rendering).';
    D3DRMERR_TEXTUREFORMATNOTFOUND: Result := 'Texture format could not be found that meets the specified criteria and that the underlying Immediate Mode device supports.';
    D3DRMERR_UNABLETOEXECUTE: Result := 'Unable to carry out procedure.';
    DDERR_INVALIDOBJECT: Result := 'Received pointer that was an invalid object.';
    DDERR_INVALIDPARAMS: Result := 'One or more of the parameters passed to the method are incorrect.';
    DDERR_NOTFOUND: Result := 'The requested item was not found.';
    DDERR_NOTINITIALIZED: Result := 'An attempt was made to call an interface method of an object created by CoCreateInstance before the object was initialized.';
    DDERR_OUTOFMEMORY: Result := 'DirectDraw does not have enough memory to perform the operation.';
    else Result := 'Unrecognized Error';
  end;
end;

end.

