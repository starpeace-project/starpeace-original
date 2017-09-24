(*==========================================================================;
 *
 *  Copyright (C) 1995-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:	d3drm.h
 *  Content:	Direct3DRM include file
 *
 *  DirectX 6 Delphi adaptation by Erik Unger
 *
 *  Modyfied: 16.Jul.98
 *
 *  Download: http://www.bigfoot.com/~ungerik/
 *  E-Mail: ungerik@bigfoot.com
 *
 ***************************************************************************)

unit D3DRMWin;

interface

uses
{$IFDEF D2COM}
  OLE2,
{$ENDIF}
  Windows,
  D3DRM,
  DirectDraw,
  Direct3D,
  D3DRMObj;

(*
 * GUIDS used by Direct3DRM Windows interface
 *)
const
  IID_IDirect3DRMWinDevice: TGUID =
    (D1:$c5016cc0;D2:$d273;D3:$11ce;D4:($ac,$48,$00,$00,$c0,$38,$25,$a1));

type
{$IFDEF D2COM}
  IDirect3DRMWinDevice = class (IDirect3DRMObject)
{$ELSE}
  IDirect3DRMWinDevice = interface (IDirect3DRMObject)
    ['{c5016cc0-d273-11ce-ac48-0000c03825a1}']
{$ENDIF}
    (*
     * IDirect3DRMWinDevice methods
     *)

    (* Repaint the window with the last frame which was rendered. *)
    function HandlePaint (hDC: HDC) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}

    (* Respond to a WM_ACTIVATE message. *)
    function HandleActivate (wparam: WORD) : HResult;
        {$IFDEF D2COM} virtual; stdcall; abstract; {$ELSE} stdcall; {$ENDIF}
  end;

implementation

end.

