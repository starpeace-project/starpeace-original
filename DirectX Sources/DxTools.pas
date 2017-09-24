(***************************************************************************;
 *
 *  Modyfied: 20.7.98
 *
 *  Download: http://www.bigfoot.com/~ungerik/
 *  E-Mail: ungerik@bigfoot.com
 *
 ***************************************************************************)

unit DXTools;

interface

uses
  Windows,
  SysUtils,
  Graphics,
  DirectDraw,
  Direct3D;

const
  UnrecognizedError = 'Unrecognized Error';
  NoError = 'No error';
// This string is displayed when an EDirectX exception occurs:
var
  DXStat : string;
  Exceptions : boolean;

type
  PTrueColor = ^TTrueColor;
  TTrueColor = record
    case integer of
      1 : (Data : DWORD);
      2 : (R,G,B,A : byte);
  end;

  PColorTable = ^TColorTable;
  TColorTable = array [0..255] of TTrueColor;

  TSingleQuadruppel = array[0..4-1] of single;

  EDirectX = class (Exception)
  public
    constructor Create(Error: integer);
  end;

const
  IdentityMatrix : TD3DMatrix = (
    _11: 1; _12: 0; _13: 0; _14: 0;
    _21: 0; _22: 1; _23: 0; _24: 0;
    _31: 0; _32: 0; _33: 1; _34: 0;
    _41: 0; _42: 0; _43: 0; _44: 1 );


  ZeroMatrix : TD3DMatrix = (
    _11: 0; _12: 0; _13: 0; _14: 0;
    _21: 0; _22: 0; _23: 0; _24: 0;
    _31: 0; _32: 0; _33: 0; _34: 0;
    _41: 0; _42: 0; _43: 0; _44: 0 );

type
  PMatrix1D = ^TMatrix1D;
  TMatrix1D = record
    case integer of
      0 : (D3DVector: TD3DVector);
      1 : (D3DColorValue: TD3DColorValue);
      2 : (a: array [0..4-1] of TD3DValue);
  end;

  PMatrix4D = ^TMatrix4D;
  TMatrix4D = record
    case integer of
      0 : (D3DMatrix: TD3DMatrix);
      1 : (a: array [0..4*4-1] of TD3DValue);
  end;

// Camera settings for Direct3D:
function ProjectionMatrix(near_plane,     // distance to near clipping plane
                          far_plane,      // distance to far clipping plane
                          fov: TD3DValue) : TD3DMatrix; // field of view angle,
                                                        // in radians
// Camera positioning for Direct3D:
function ViewMatrix(from,                  // camera location
                    at,                    // camera look-at target
                    world_up: TD3DVector;  // world's up, usually 0, 1, 0
                    roll: TD3DValue) : TD3DMatrix; // clockwise roll around
                                                 //    viewing direction,
                                                 //    in radians


function TransformationYZ(y, z: TD3DVector) : TD3DMatrix;
function TranslateMatrix(dx, dy, dz: TD3DValue) : TD3DMatrix;
function RotateXMatrix(rads: TD3DValue) : TD3DMatrix;
function RotateYMatrix(rads: TD3DValue) : TD3DMatrix;
function RotateZMatrix(rads: TD3DValue) : TD3DMatrix;
function ScaleMatrix(size: TD3DValue) : TD3DMatrix;
function MatrixMul(const a, b: TD3DMatrix) : TD3DMatrix;
// Fills a DirectX-record with zero and sets the size in the first DWORD:
procedure InitRecord(var DXRecord; Size: integer);
// Computes the brightness of a RGB-color:
function GetBrightness(Red,Green,Blue: TD3DValue) : TD3DValue;
procedure SetBrightness(var Red,Green,Blue: TD3DValue; Brightness: TD3DValue);
// Releases an Object (initialised or not) and sets the pointer to nil:
function ReleaseObj(var Obj) : boolean;
// Releases an Delphi2 or 3 COM-interface (initialised or not) and sets the pointer to nil:
function ReleaseCOM(var COM) : boolean;
// Releases an initialised Delphi2 or 3 COM-interface and sets the pointer to nil:
procedure ReleaseCOMe(var COM);
// Increases the reference-counter of an Delphi2 or 3 COM-interface
function AddCOM(const COM) : IUnknown;
// Computes the bounding box of an retained mode frame:
procedure SM(Message: string);
// Loads colorpalette-data from a Paint Shop Pro file:
function LoadPaletteFromJASCFile(Filename: string; var Palette: TColorTable) : boolean;
// Fills a DirectDraw suface with black (0):
procedure ClearSurface(Surface: IDirectDrawSurface; Color: integer);
// Finds out, how many bit per pixel of a TBitmap are used:
function GetBitsPerPixelFromBitmap(Bitmap: Graphics.TBitmap) : integer;

// Delivers an interface for the default DirectDraw2 Object:
function GetDefaultDirectDraw2 : IDirectDraw2;
// Delivers an interface for the primary DirectDraw2 Object:
function GetFirstDirectDraw2 : IDirectDraw2;
// Delivers an interface for the secondary DirectDraw2 Object:
function GetSecondDirectDraw2 : IDirectDraw2;


procedure ReadOnlyProperty;
procedure NotReady;

// Error handling:

function DXErrorString(Value: HResult) : string;

// Checks a Direct3D returnvalue for an error,
// and raises an EDirect3D exception if necessary:
procedure D3DCheck(Value: HResult);
// Checks a DirectDraw returnvalue for an error,
// and raises an EDirectDraw exception if necessary:
procedure DDCheck(Value: HResult);
// Checks a DirectInput returnvalue for an error,
// and raises an EDirectInput exception if necessary:
procedure DICheck(Value: HResult);
// Checks a DirectPlay returnvalue for an error,
// and raises an EDirectPlay exception if necessary:
procedure DPCheck(Value: HResult);
// Checks a DirectSound returnvalue for an error,
// and raises an EDirectSound exception if necessary:
procedure DSCheck(Value: HResult);
// Checks a DirectSetup returnvalue for an error,
// and raises an EDirectSetup exception if necessary:
procedure DSetupCheck(Value: HResult);
// Checks a DirectSetup returnvalue for an error,
// and raises an EDirectSetup exception if necessary:
procedure DXFCheck(Value: HResult);
// Checks a DirectX returnvalue for an error,
// and raises an EDirectX exception if necessary:
procedure DXCheck(Value: HResult);

function D3DVECTOR( x, y, z : TD3DValue ) : TD3DVECTOR;
function D3DVERTEX( v, n : TD3DVector; _u, _v : TD3DValue ) : TD3DVERTEX;

implementation

////////////////////////////////////////////////////////////////////////////////
// DXTools
////////////////////////////////////////////////////////////////////////////////

function GetDefaultDirectDraw2 : IDirectDraw2;
var
  DD : IDirectDraw;
begin
  Result := nil;
  DD := nil;
  try
    DDCheck( DirectDrawCreate(nil,DD,nil) );
    DDCheck( DD.QueryInterface(IDirectDraw2,Result) );
  finally
    ReleaseCOM(DD);
  end;
end;

function GetFirstDirectDraw2_DDEnumCallback (lpGUID: PGUID;
    lpDriverDescription: LPSTR; lpDriverName: LPSTR; lpContext: pointer) : BOOL;
    stdcall;
begin
  Result := True;
  PGUID(lpContext^) := lpGUID;
  if Assigned(lpGUID) then Result := False;
end;

function GetFirstDirectDraw2 : IDirectDraw2;
var
  DD : IDirectDraw;
  GUID : PGUID;
begin
  Result := nil;
  DD := nil;
  GUID := nil;
  try
    DDCheck( DirectDrawEnumerateA(GetFirstDirectDraw2_DDEnumCallback,@GUID) );
    DDCheck( DirectDrawCreate(GUID,DD,nil) );
    DDCheck( DD.QueryInterface(IDirectDraw2,Result) );
  finally
    ReleaseCOM(DD);
  end;
end;

function GetSecondDirectDraw2_DDEnumCallback (lpGUID: PGUID;
    lpDriverDescription: LPSTR; lpDriverName: LPSTR; lpContext: pointer) : BOOL;
    stdcall;
begin
  Result := True;
  if Assigned(pointer(lpContext^)) then Result := False;
  PGUID(lpContext^) := lpGUID;
end;

function GetSecondDirectDraw2 : IDirectDraw2;
var
  DD : IDirectDraw;
  GUID : PGUID;
begin
  Result := nil;
  DD := nil;
  GUID := nil;
  try
    DDCheck( DirectDrawEnumerateA(GetSecondDirectDraw2_DDEnumCallback,@GUID) );
    if not Assigned(GUID) then exit;
    DDCheck( DirectDrawCreate(GUID,DD,nil) );
    DDCheck( DD.QueryInterface(IDirectDraw2,Result) );
  finally
    ReleaseCOM(DD);
  end;
end;

function ArcTan2(Y, X: Extended): Extended;
asm
        FLD     Y
        FLD     X
        FPATAN
        FWAIT
end;

function TransformationYZ(y, z: TD3DVector) : TD3DMatrix;
var
  ret : TD3DMatrix;
begin
  with y do
    if X <> 0.0 then
      ret := RotateZMatrix( ArcTan2(Y,X) )
    else
      ret := IdentityMatrix;

  with z do
    if Z <> 0.0 then
      ret := MatrixMul(ret, RotateYMatrix( ArcTan2(X,Z) ));

  with y do
    if Z <> 0.0 then
      ret := MatrixMul(ret, RotateXMatrix( ArcTan2(Y,Z) ));

  Result := ret;
end;

function ProjectionMatrix(near_plane,     // distance to near clipping plane
                          far_plane,      // distance to far clipping plane
           fov: TD3DValue) : TD3DMatrix;    // field of view angle, in radians
var
  c, s, Q : TD3DValue;
begin
    c := cos(fov*0.5);
    s := sin(fov*0.5);
    Q := s/(1.0 - near_plane/far_plane);

    Result := ZeroMatrix;
    Result._11 := c;
    Result._22 := c;
    Result._33 := Q;

    Result._43 := -Q*near_plane;
    Result._34 := s;
end;

function TranslateMatrix(dx, dy, dz: TD3DValue) : TD3DMatrix;
begin
    Result := IdentityMatrix;
    Result._41 := dx;
    Result._42 := dy;
    Result._43 := dz;
end;

function RotateXMatrix(rads: TD3DValue) : TD3DMatrix;
var
  cosine, sine : TD3DValue;
begin
    cosine := cos(rads);
    sine := sin(rads);
    Result := IdentityMatrix;
    Result._22 := cosine;
    Result._33 := cosine;
    Result._23 := -sine;
    Result._32 := sine;
end;

function RotateYMatrix(rads: TD3DValue) : TD3DMatrix;
var
  cosine, sine : TD3DValue;
begin
    cosine := cos(rads);
    sine := sin(rads);
    Result := IdentityMatrix;
    Result._11 := cosine;
    Result._33 := cosine;
    Result._13 := sine;
    Result._31 := -sine;
end;

function RotateZMatrix(rads: TD3DValue) : TD3DMatrix;
var
  cosine, sine : TD3DValue;
begin
    cosine := cos(rads);
    sine := sin(rads);
    Result := IdentityMatrix;
    Result._11 := cosine;
    Result._22 := cosine;
    Result._12 := -sine;
    Result._21 := sine;
end;

function ScaleMatrix(size: TD3DValue) : TD3DMatrix;
begin
    Result := IdentityMatrix;
    Result._11 := size;
    Result._22 := size;
    Result._33 := size;
end;

function ViewMatrix(from,                  // camera location
                    at,                    // camera look-at target
                    world_up: TD3DVector;  // world's up, usually 0, 1, 0
                    roll: TD3DValue) : TD3DMatrix; // clockwise roll around
                                                 //    viewing direction,
                                                 //    in radians
var
  up, right, view_dir : TD3DVector;
begin
    Result := IdentityMatrix;

    view_dir := VectorNormalize(VectorSub(at,from));
    right := VectorCrossProduct(world_up, view_dir);
    up := VectorCrossProduct(view_dir, right);

    right := VectorNormalize(right);
    up := VectorNormalize(up);

    Result._11 := right.x;
    Result._21 := right.y;
    Result._31 := right.z;
    Result._12 := up.x;
    Result._22 := up.y;
    Result._32 := up.z;
    Result._13 := view_dir.x;
    Result._23 := view_dir.y;
    Result._33 := view_dir.z;

    Result._41 := -VectorDotProduct(right, from);

    Result._42 := -VectorDotProduct(up, from);
    Result._43 := -VectorDotProduct(view_dir, from);

    if roll <> 0.0 then
        // MatrixMult function shown below
        Result := MatrixMul(RotateZMatrix(-roll), TD3DMatrix(Result));
end;

// Multiplies two matrices.
function MatrixMul(const a, b: TD3DMatrix) : TD3DMatrix;
var
  i,j,k : integer;
begin
  Result := ZeroMatrix;
  for i := 0 to 3 do
    for j := 0 to 3 do
      for k := 0 to 3 do
        Result.m[i,j] := Result.m[i,j] + (a.m[k,j] * b.m[i,k]);
end;


function GetBitsPerPixelFromBitmap(Bitmap: Graphics.TBitmap) : integer;
var
  bm : Windows.TBitmap;
begin
  if GetObject(Bitmap.Handle, sizeof(bm), @bm) = 0 then Result := 0
    else Result := bm.bmBitsPixel;
end;

procedure InitRecord(var DXRecord; Size: integer);
begin
  ZeroMemory(@DXRecord,Size);
  DWORD(DXRecord) := Size;
end;

function GetDDFromDevice2(Device2: IDirect3DDevice2) : IDirectDraw;
var
  DirectDraw : IDirectDraw;
  Target : IDirectDrawSurface;
  Target2 : IDirectDrawSurface2;
begin
  DirectDraw := nil;
  Target := nil;
  Target2 := nil;
  try
    // get the render target (we need it to get the IDirectDraw)
    DxCheck( Device2.GetRenderTarget(Target) );
    // get the DirectDraw object, but first we need a IDirectDrawSurface2
    DxCheck( Target.QueryInterface(IDirectDrawSurface2,Target2) );
    DxCheck( Target2.GetDDInterface(DirectDraw) );
  finally
    ReleaseCOM( Target );
    ReleaseCOM( Target2 );
    Result := DirectDraw;
  end
end;

procedure ReadOnlyProperty;
begin
  if Exceptions then Exception.Create('Property is Read-Only !');
end;

procedure NotReady;
begin
  if Exceptions then Exception.Create('Not implemented, yet !');
end;

procedure ClearSurface(Surface: IDirectDrawSurface; Color: integer);
var
  bltfx : TDDBltFX;
begin
  InitRecord(bltfx,sizeof(bltfx));
  bltfx.dwFillColor := Color;
  dxCheck( Surface.Blt(nil,nil,nil,DDBLT_COLORFILL + DDBLT_WAIT,@bltfx) );
end;

function LoadPaletteFromJASCFile(Filename: string; var Palette: TColorTable) : boolean;
var
  f : text;
  i : integer;
  s : string;
  b : byte;
  Code : integer;

procedure ReadWd;
var
  c : AnsiChar;
begin
  s := '';
  repeat
    read(f,c);
    if c <> ' ' then s := s + c;
  until c = ' ';
end;

label
  ende;
begin
  Result := false;
  assign(f,Filename);
  {$i-}  reset(f);
  if ioResult <> 0 then goto ende;
  readln(f,s);
  readln(f,s);
  readln(f,s);
  for i := 0 to 255 do begin
    ReadWd;
    Val(s,b,Code);
    if Code <> 0 then goto ende;
    Palette[i].R := b;
    ReadWd;
    Val(s,b,Code);
    if Code <> 0 then goto ende;
    Palette[i].G := b;
    ReadLn(f,s);
    Val(s,b,Code);
    if Code <> 0 then goto ende;
    Palette[i].B := b;
    Palette[i].A := PC_EXPLICIT;
  end;
  Result := true;
ende:
  close(f); {$I+}
end;

function GetBrightness(Red,Green,Blue: TD3DValue) : TD3DValue;
begin
  Result := (Red * 0.3) + (Green * 0.59) + (Blue * 0.11);
end;

procedure SetBrightness(var Red,Green,Blue: TD3DValue; Brightness: TD3DValue);
// var  factor : TD3DValue;
begin
// Setzt entsprechenden Grauton:
  Red := Brightness;
  Green := Brightness;
  Blue := Brightness;
//Behält Farbe bei Helligkeitsänderung bei:
{  if GetBrightness(Red,Green,Blue) = 0.0 then begin
    Red := 0.0;
    Green := 0.0;
    Blue := 0.0;
  end else begin
    factor := Brightness / GetBrightness(Red,Green,Blue);
    Red := Red * factor;
    if Red > 1.0 then Red := 1.0;
    Green := Green * factor;
    if Green > 1.0 then Green := 1.0;
    Blue := Blue * factor;
    if Blue > 1.0 then Blue := 1.0;
  end;}
end;

procedure SM(Message: string);
begin
  MessageBox(0,PChar(Message),'DirectX-Application:',MB_APPLMODAL);
end;

function AddCOM(const COM) : IUnknown;
begin
{$IFDEF D2COM}
  if Assigned( IUnknown(COM) ) then IUnknown(COM).AddRef;
{$ELSE}
  if Assigned( IUnknown(COM) ) then IUnknown(COM)._AddRef;
{$ENDIF}
  Result := IUnknown(COM);
end;

function ReleaseObj(var Obj) : boolean;
begin
  if assigned( TObject(Obj) ) then
    begin
      TObject(Obj).Destroy;
      TObject(Obj) := nil;
      Result := True;
    end
  else
    Result := False;
end;

function ReleaseCOM(var COM) : boolean;  // Interfaceobjekt freigeben
begin
  if Assigned( IUnknown(COM) ) then // wenn Zeigerwert nicht nil dann:
    begin
{$IFDEF D2COM}
      IUnknown(COM).Release;        // Referenzzähler um eins erniedrigen
{$ENDIF}
      IUnknown(COM) := nil;         // Zeiger auf null setzt,
      Result := True;
    end     // um weitere versehentlicher Zugriffe zu vermeiden
  else
    Result := false;
end;

procedure ReleaseCOMe(var COM);
begin
  if Assigned( IUnknown(COM) ) then
    begin
{$IFDEF D2COM}
       IUnknown(COM).Release;
{$ELSE}
       IUnknown(COM)._Release;
{$ENDIF}
       IUnknown(COM) := nil;
    end
  else
    raise Exception.Create(DXStat+#13+'ReleaseCOM of NULL object');
end;

function FormatError(ErrorString,At: string) : string;
begin
  Result := #13+#13+ErrorString+#13+#13;
  if At <> '' then Result := Result +'At: '+At+ #13+#13;
end;

constructor EDirectX.Create(Error: integer);
begin
  inherited Create( FormatError(DXErrorString(Error),DXStat) );
end;

function DXErrorString(Value: HResult) : string;
begin
 if Value = 0 then
   Result := NoError
 else
   begin
     Result := 'Error ';
   end;
end;

procedure DXCheck(Value: HResult); { Check the Result of a COM operation }
begin
  if Value <> 0 then raise EDirectX.Create(Value);
end;

procedure DXCheck_(Value: HResult); { Check the Result of a COM operation }
var
  s : string;
begin
  if Value <> 0 then
  begin
    s := IntToHex(Value,8);  // for debugging
    raise EDirectX.Create(Value);
  end;
end;

procedure DDCheck(Value: HResult);
begin
end;

procedure D3DCheck(Value: HResult);
begin
end;

procedure DICheck(Value: HResult);
begin
end;

procedure DPCheck(Value: HResult);
begin
end;

procedure DSCheck(Value: HResult);
begin
end;

procedure DSetupCheck(Value: HResult);
begin
end;

procedure DXFCheck(Value: HResult);
begin
end;

function D3DVECTOR( x, y, z : TD3DValue ) : TD3DVECTOR;
begin
  result.x := x;
  result.y := y;
  result.z := z;
end;

function D3DVERTEX( v, n : TD3DVector; _u, _v : TD3DValue ) : TD3DVERTEX;
begin
  with result do
    begin
      x  := v.x;             (* Homogeneous coordinates *)
      y  := v.y;
      z  := v.z;
      nx := n.x;           (* Normal *)
      ny := n.y;
      nz := n.z;
      tu := _u;            (* Texture coordinates *)
      tv := _v;
    end;
end;

initialization
begin
  DXStat := '';
  Exceptions := True;
end;

finalization
begin
end;

end.
