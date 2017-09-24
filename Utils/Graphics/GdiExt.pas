unit GdiExt;

// Copyright (c) 1994-1997 Microsoft Corporation. All Rights Reserved.
// Translated to Delphi by Jorge Romero, Merchise

interface

  uses
    Windows, Classes;

  // AlphaBlend stuff >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

  const // Currentlly defined blend operations
    AC_SRC_OVER = 00;

  const // Alpha format flags
    AC_SRC_NO_PREMULT_ALPHA = $01;
    AC_SRC_NO_ALPHA         = $02;
    AC_DST_NO_PREMULT_ALPHA = $10;
    AC_DST_NO_ALPHA         = $20;

  type
    PBlendFunction = ^TBlendFunction;
    TBlendFunction =
      packed record
        BlendOp             : byte;
        BlendFlags          : byte;
        SourceConstantAlpha : byte;
        AlphaFormat         : byte;
      end;
    LPBLENDFUNCTION = PBlendFunction;

  function BlendFunction( aBlendOp, aBlendFlags, aSourceConstantAlpha, aAlphaFormat : byte ) : TBlendFunction;

  // GradientFill stuff >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

  type
    TColor16 = word; // !! ushort;

  type
    PTriVertex = ^TTriVertex;
    TTriVertex =
      packed record
        x          : longint;
        y          : longint;
        Red        : TColor16;
        Green      : TColor16;
        Blue       : TColor16;
        Alpha      : TColor16;
      end;
    LPTRIVERTEX = PTriVertex;

  type
    PTriVertices = ^TTriVertices;
    TTriVertices = array[0..2] of TTriVertex;

  function TriVertex( aX, aY : integer; aColor : TColorRef ) : TTriVertex;
  function AllocTriVertices( VertArray : array of TTriVertex; var VertPtr : PTriVertices ) : integer;

  type
    PGradientTriangle = ^TGradientTriangle;
    TGradientTriangle =
      packed record
        Vertex1 : ulong;
        Vertex2 : ulong;
        Vertex3 : ulong;
      end;
    LPGRADIENT_TRIANGLE = PGradientTriangle;

  type
    PTriangles = ^TTriangles;
    TTriangles = array[0..0] of TGradientTriangle;

  function Triangle( aVertex1, aVertex2, aVertex3 : integer ) : TGradientTriangle;
  function AllocTriangles( TriArray : array of TGradientTriangle; var TriPtr : PTriangles ) : integer;

  type
    PGradientRect = ^TGradientRect;
    TGradientRect =
      packed record
        UpperLeft  : ulong;
        LowerRight : ulong;
      end;
    LPGRADIENT_RECT = PGradientRect;

  const // Gradient drawing modes
    GRADIENT_FILL_RECT_H   = $00000000;
    GRADIENT_FILL_RECT_V   = $00000001;
    GRADIENT_FILL_TRIANGLE = $00000002;
    GRADIENT_FILL_OP_FLAG  = $000000ff;

  const
    gfVerticalRect   = GRADIENT_FILL_RECT_V;
    gfHorizontalRect = GRADIENT_FILL_RECT_H;
    gfTriangle       = GRADIENT_FILL_TRIANGLE;
    gfOpFlag         = GRADIENT_FILL_OP_FLAG;

  // These are the functions >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

{$IFDEF LinkStatic}
  const
    GdiExtAvailable = true;

  function TransparentBlt ( hdcDest : HDC;
                              XOriginDest,  YOriginDest,  WidthDest, HeightDest : integer;
                            hdcSrc : HDC;
                              XOriginSrc,  YOriginSrc, WidthSrc, HeightSrc : integer;
                            crTransparent : TColorRef ) : BOOL; stdcall;

//function TransparentDIBits( hdcDest : HDC;
//                              XOriginDest,  YOriginDest, WidthDest, HeightDest : integer;
//                            Bits : pointer; BitsInfo : PBitmapInfoHeader; Usage : uint;
//                              XOriginSrc,  YOriginSrc, WidthSrc, HeightSrc : integer;
//                            crTransparent : TColorRef ) : BOOL; stdcall;

  function AlphaBlend( hdcDest : HDC;
                         XOriginDest,  YOriginDest,  WidthDest, HeightDest : integer;
                       hdcSrc : HDC;
                         XOriginSrc,  YOriginSrc, WidthSrc, HeightSrc : integer;
                       const blendFunction : TBlendFunction ) : BOOL; stdcall;

//function AlphaDibBlend( hdcDest : HDC;
//                          XOriginDest,  YOriginDest,  WidthDest, HeightDest : integer;
//                        Bits : pointer; BitsInfo : PBitmapInfoHeader; Usage : uint;
//                          XOriginSrc,  YOriginSrc, WidthSrc, HeightSrc : integer;
//                        const blendFunction : TBlendFunction ) : BOOL; stdcall;

  function GradientFill( hdcDest : HDC; Vertex : pointer; NumVertex : ulong;
                           Mesh : pointer; NumMesh : ulong; Mode : dword ) : BOOL; stdcall;

{$ELSE}
  var
    GdiExtAvailable : boolean;

  type
    TTransparentBlt = function ( hdcDest : HDC;
                                   XOriginDest,  YOriginDest,  WidthDest, HeightDest : integer;
                                 hdcSrc : HDC;
                                   XOriginSrc,  YOriginSrc, WidthSrc, HeightSrc : integer;
                                 crTransparent : TColorRef ) : BOOL; stdcall;

    TTransparentDIBits = function( hdcDest : HDC;
                                     XOriginDest,  YOriginDest, WidthDest, HeightDest : integer;
                                   Bits : pointer; BitsInfo : PBitmapInfoHeader; Usage : uint;
                                      XOriginSrc,  YOriginSrc, WidthSrc, HeightSrc : integer;
                                   crTransparent : TColorRef ) : BOOL; stdcall;

  type
    TAlphaBlend = function ( hdcDest : HDC;
                               XOriginDest,  YOriginDest,  WidthDest, HeightDest : integer;
                             hdcSrc : HDC;
                               XOriginSrc,  YOriginSrc, WidthSrc, HeightSrc : integer;
                             const blendFunction : TBlendFunction ) : BOOL; stdcall;

    TAlphaDibBlend = function ( hdcDest : HDC;
                                  XOriginDest,  YOriginDest,  WidthDest, HeightDest : integer;
                                Bits : pointer; BitsInfo : PBitmapInfoHeader; Usage : uint;
                                  XOriginSrc,  YOriginSrc, WidthSrc, HeightSrc : integer;
                                const blendFunction : TBlendFunction ) : BOOL; stdcall;

  type
    TGradientFill = function ( hdcDest : HDC; Vertex : pointer; NumVertex : ulong;
                                 Mesh : pointer; NumMesh : ulong; Mode : dword ) : BOOL; stdcall;
  var
    AlphaBlend        : TAlphaBlend        = nil;
    AlphaDibBlend     : TAlphaDibBlend     = nil;
    TransparentBlt    : TTransparentBlt    = nil;
    TransparentDIBits : TTransparentDIBits = nil;
    GradientFill      : TGradientFill      = nil;
{$ENDIF}

  // Helper functions

  type
    PTriVertexInfo = ^TTriVertexInfo;
    TTriVertexInfo =
      record
        x, y  : integer;
        Color : TColorRef;
      end;

  function ListFromTriVertices( TriVertices : PTriVertices; Count : integer ) : TList;
  function TriVerticesFromList( List : TList; var TriVertices : PTriVertices ) : integer;

  type
    PTriangleInfo = ^TTriangleInfo;
    TTriangleInfo =
      record
        Vertex1, Vertex2, Vertex3 : integer;
      end;

  function ListFromTriangles( Triangles : PTriangles; Count : integer ) : TList;
  function TrianglesFromList( List : TList; var Triangles : PTriangles ) : integer;

implementation

  function ListFromTriVertices( TriVertices : PTriVertices; Count : integer ) : TList;
    var
      i : integer;
      p : PTriVertexInfo;
    begin
      Result := TList.Create;
      for i := 0 to Count - 1 do
        begin
          new( p );
          with TriVertices[i] do
            begin
              p.x     := x;
              p.y     := y;
              p.Color := ( Red div 256 ) or
                         ( ( Green div 256 ) shl 8 ) or
                         ( ( Blue div 256 ) shl 16 );
            end;
          Result.Add( p );
        end;
    end;

  function TriVerticesFromList( List : TList; var TriVertices : PTriVertices ) : integer;
    var
      i : integer;
    begin
      getmem( TriVertices, List.Count * sizeof( TTriVertexInfo ) );
      for i := 0 to List.Count - 1 do
        begin
          with PTriVertexInfo( List.Items[i] )^ do
            TriVertices[i] := TriVertex( x, y, Color );
        end;
      Result := List.Count;  
    end;

  function ListFromTriangles( Triangles : PTriangles; Count : integer ) : TList;
    var
      i : integer;
      p : PTriangleInfo;
    begin
      Result := TList.Create;
      for i := 0 to Count - 1 do
        begin
          new( p );
          with Triangles[i] do
            begin
              p.Vertex1 := Vertex1;
              p.Vertex2 := Vertex2;
              p.Vertex3 := Vertex3;
            end;
          Result.Add( p );
        end;
    end;

  function TrianglesFromList( List : TList; var Triangles : PTriangles ) : integer;
    var
      i : integer;
    begin
      getmem( Triangles, List.Count * sizeof( TTriangleInfo ) );
      for i := 0 to List.Count - 1 do
        begin
          with PTriangleInfo( List.Items[i] )^ do
            Triangles[i] := Triangle( Vertex1, Vertex2, Vertex3 );
        end;
      Result := List.Count;  
    end;

  const
    msimg32 = 'MSIMG32.DLL';

  // AlphaBlend stuff >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

  function BlendFunction( aBlendOp, aBlendFlags, aSourceConstantAlpha, aAlphaFormat : byte ) : TBlendFunction;
    begin
      with Result do
        begin
          BlendOp             := aBlendOp;
          BlendFlags          := aBlendFlags;
          SourceConstantAlpha := aSourceConstantAlpha;
          AlphaFormat         := aAlphaFormat;
        end;
    end;

  // GradientFill stuff >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

  function TriVertex( aX, aY : integer; aColor : TColorRef ) : TTriVertex;
    begin
      with Result do
        begin
          x   := aX;
          y   := aY;
          Red   := byte( aColor ) * 256;
          Green := byte( aColor shr 8 ) * 256;
          Blue  := byte( aColor shr 16 ) * 256;
        end;
    end;

  function AllocTriVertices( VertArray : array of TTriVertex; var VertPtr : PTriVertices ) : integer;
    var
      i : integer;
    begin
      Result := High( VertArray ) - Low( VertArray ) + 1;
      if Result > 0
        then
          begin
            getmem( VertPtr, sizeof( TTriVertex ) * Result );
            for i := Low( VertArray ) to High( VertArray ) do
              VertPtr[i - Low( VertArray ) ] := VertArray[i];
          end
        else VertPtr := nil;
    end;

  function Triangle( aVertex1, aVertex2, aVertex3 : integer ) : TGradientTriangle;
    begin
      with Result do
        begin
          Vertex1 := aVertex1;
          Vertex2 := aVertex2;
          Vertex3 := aVertex3;
        end;
    end;

  function AllocTriangles( TriArray : array of TGradientTriangle; var TriPtr : PTriangles ) : integer;
    var
      i : integer;
    begin
      Result := High( TriArray ) - Low( TriArray ) + 1;
      if Result > 0
        then
          begin
            getmem( TriPtr, sizeof( TGradientTriangle ) * Result );
            for i := Low( TriArray ) to High( TriArray ) do
              TriPtr[i - Low( TriArray ) ] := TriArray[i];
          end
        else TriPtr := nil;
    end;

{$IFDEF LinkStatic}
  function AlphaBlend;
    external msimg32 name 'AlphaBlend';
//function AlphaDibBlend;
//  external msimg32 name 'AlphaDIBBlend';
  function TransparentBlt;
    external msimg32 name 'TransparentBlt';
//function TransparentDIBits;
//  external msimg32 name 'TransparentDIBits';
  function GradientFill;
    external msimg32 name 'GradientFill';
{$ELSE}
  var
    hLib : THandle = 0;

  procedure InitLibrary;
    begin
      hLib := LoadLibrary( msimg32 );
      if hLib <> 0
        then
          begin
            AlphaBlend        := GetProcAddress( hLib, 'AlphaBlend' );
            AlphaDibBlend     := GetProcAddress( hLib, 'AlphaDibBlend' );
            TransparentBlt    := GetProcAddress( hLib, 'TransparentBlt' );
            TransparentDIBits := GetProcAddress( hLib, 'TransparentDIBits' );
            GradientFill      := GetProcAddress( hLib, 'GradientFill' );
          end;
    end;

  procedure DoneLibrary;
    begin
      if hLib <> 0
        then FreeLibrary( hLib );
    end;

initialization
  InitLibrary;
  
finalization
  DoneLibrary;
{$ENDIF}  
end.

