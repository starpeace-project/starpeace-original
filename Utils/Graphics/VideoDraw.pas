unit VideoDraw;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise

interface

  uses
    Windows, MMSystem;

  // DRAWDIB - Routines for drawing to the display.

  type
    HDRAWDIB = type THANDLE; // hdd

  const // DrawDib Flags
    DDF_UPDATE          = $0002;          // re-draw the last DIB
    DDF_SAME_HDC        = $0004;          // HDC same as last call (all setup)
    DDF_SAME_DRAW       = $0008;          // draw params are the same
    DDF_DONTDRAW        = $0010;          // dont draw frame, just decompress
    DDF_ANIMATE         = $0020;          // allow palette animation
    DDF_BUFFER          = $0040;          // always buffer image
    DDF_JUSTDRAWIT      = $0080;          // just draw it with GDI
    DDF_FULLSCREEN      = $0100;          // use DisplayDib
    DDF_BACKGROUNDPAL   = $0200;          // Realize palette in background
    DDF_NOTKEYFRAME     = $0400;          // this is a partial frame update, hint
    DDF_HURRYUP         = $0800;          // hurry up please!
    DDF_HALFTONE        = $1000;          // always halftone

    DDF_PREROLL         = DDF_DONTDRAW;   // Builing up a non-keyframe
    DDF_SAME_DIB        = DDF_SAME_DRAW;
    DDF_SAME_SIZE       = DDF_SAME_DRAW;

  // DrawDib functions

  function DrawDibOpen : HDRAWDIB; stdcall;
  function DrawDibClose( hdd : HDRAWDIB ) : BOOL; stdcall;
  function DrawDibGetBuffer( hdd : HDRAWDIB; lpbi : PBitmapInfoHeader; dwSize : dword; dwFlags : dword ) : pointer; stdcall;

  function DrawDibGetPalette( hdd : HDRAWDIB ) : HPALETTE; stdcall;              // Get the palette used for drawing DIBs
  function DrawDibSetPalette( hdd : HDRAWDIB; hpal : HPALETTE ) : BOOL; stdcall; // Set the palette used for drawing DIBs
  function DrawDibChangePalette( hdd : HDRAWDIB; iStart, iLen : integer; Entries : pointer ) : BOOL; stdcall;
  function DrawDibRealize( hdd : HDRAWDIB; dc : HDC; fBackground : BOOL ) : uint; stdcall;
  function DrawDibStart( hdd : HDRAWDIB; Rate : dword ) : BOOL; stdcall;         // Start of streaming playback
  function DrawDibStop( hdd : HDRAWDIB ) : BOOL; stdcall;                        // Stop of streaming playback
  function DrawDibBegin( hdd : HDRAWDIB; dc : HDC;
                         dxDst, dyDst : integer; lpbi : PBitmapInfoHeader;
                         dxSrc, dySrc : integer; wFlags : uint ) : BOOL; stdcall;
  function DrawDibDraw( hdd : HDRAWDIB;  dc : HDC;
                        xDst, yDst : integer; dxDst, dyDst : integer; lpbi : PBitmapInfoHeader; lpBits : pointer;
                        xSrc, ySrc : integer; dxSrc, dySrc : integer; wFlags : uint ) : BOOL; stdcall;
  function DrawDibUpdate( hdd : HDRAWDIB;  dc : HDC; x, y : integer ) : BOOL;

  function DrawDibEnd( hdd : HDRAWDIB ) : BOOL; stdcall;

  type
    TDrawDibTime =
      packed record
        timeCount      : longint;
        timeDraw       : longint;
        timeDecompress : longint;
        timeDither     : longint;
        timeStretch    : longint;
        timeBlt        : longint;
        timeSetDIBits  : longint;
      end;

  function DrawDibTime( hdd : HDRAWDIB; var ddtime : TDrawDibTime ) : BOOL; stdcall;

  // display profiling
  const
    PD_CAN_DRAW_DIB      = $0001;      // if you can draw at all
    PD_CAN_STRETCHDIB    = $0002;      // basicly RC_STRETCHDIB
    PD_STRETCHDIB_1_1_OK = $0004;      // is it fast?
    PD_STRETCHDIB_1_2_OK = $0008;      // ...
    PD_STRETCHDIB_1_N_OK = $0010;      // ...

  function DrawDibProfileDisplay( lpbi : PBitmapInfoHeader ) : dword; stdcall;

implementation

  // DrawDib functions

  function DrawDibOpen; external 'MSVFW32.DLL';
  function DrawDibClose; external 'MSVFW32.DLL';
  function DrawDibGetBuffer; external 'MSVFW32.DLL';
  function DrawDibGetPalette; external 'MSVFW32.DLL';
  function DrawDibSetPalette; external 'MSVFW32.DLL';
  function DrawDibChangePalette; external 'MSVFW32.DLL';
  function DrawDibRealize; external 'MSVFW32.DLL';
  function DrawDibStart; external 'MSVFW32.DLL';
  function DrawDibStop; external 'MSVFW32.DLL';
  function DrawDibBegin; external 'MSVFW32.DLL';
  function DrawDibDraw; external 'MSVFW32.DLL';
  function DrawDibEnd; external 'MSVFW32.DLL';
  function DrawDibTime; external 'MSVFW32.DLL';
  function DrawDibProfileDisplay; external 'MSVFW32.DLL';

  function DrawDibUpdate( hdd : HDRAWDIB;  dc : HDC; x, y : integer ) : BOOL;
    begin
      Result := DrawDibDraw( hdd, dc, x, y, 0, 0, nil, nil, 0, 0, 0, 0, DDF_UPDATE );
    end;

end.

