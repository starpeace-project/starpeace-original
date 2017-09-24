unit VideoICM;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise

interface

  uses
    Windows, MMSystem, Dibs;

  type
    HIC = type THandle;

  const
    ICTYPE_VIDEO  = $63646976;  // 'vidc': Video compressor/decompressor
    ICTYPE_AUDIO  = $63647561;  // 'audc': Audio compressor/decompressor

  const
    COMP_MSVIDEO1 = $6376736D; // 'msvc': Microsoft Video 1
    COMP_MSRLE    = $656C726D; // 'mrle': Microsoft RLE
    COMP_JPEG     = $6765706A; // 'jpeg': Texas Instruments JPEG
    COMP_CINEPAK  = $64697663; // 'cvid': Supermac Cinepak
    COMP_YUV9     = $39767579; // 'yuv9': Intel 411 YUV format
    COMP_RT21     = $31327472; // 'rt21': Intel Indeo 2.1 format
    COMP_IV31     = $31337669; // 'iv31': Intel Indeo 3.0 format
    COMP_IV41     = $31347669; // 'iv41': Intel Indeo Interactive format

  function StrToFourCC( const FourCC : string ) : dword;
  function FourCC( ch1, ch2, ch3, ch4 : char ) : dword;
  function FourCCToStr( FourCC : dword ) : string;

  function ICCompressDecompressImage( // Compresses or decompresses a given image.
             ic       : HIC;                 // compressor (NULL if any will do)
             uiFlags  : uint;                // silly flags
             lpbiIn   : PBitmapInfoHeader;   // input DIB format
             lpBits   : pointer;             // input DIB bits
             lpbiOut  : PBitmapInfoHeader;   // output format (NULL => default)
             lQuality : longint              // the reqested quality
           ) : PDib;
  //  input:
  //      hic         compressor to use, if NULL is specifed a
  //                  compressor will be located that can handle the conversion.
  //      uiFlags     flags (not used, must be 0)
  //      lpbiIn      input DIB format
  //      lpBits      input DIB bits
  //      lpbiOut     output format, if NULL is specifed the default
  //                  format choosen be the compressor will be used.
  //      lQuality    the reqested compression quality

  function ICCompressImage( // Compresses a given image.
             ic       : HIC;                 // compressor (NULL if any will do)
             uiFlags  : uint;                // silly flags
             lpbiIn   : PBitmapInfoHeader;   // input DIB format
             lpBits   : pointer;             // input DIB bits
             lpbiOut  : PBitmapInfoHeader;   // output format (NULL => default)
             lQuality : longint              // the reqested quality
           ) : PDib;
  //  input:
  //      hic         compressor to use, if NULL is specifed a
  //                  compressor will be located that can handle the conversion.
  //      uiFlags     flags (not used, must be 0)
  //      lpbiIn      input DIB format
  //      lpBits      input DIB bits
  //      lpbiOut     output format, if NULL is specifed the default
  //                  format choosen be the compressor will be used.
  //      lQuality    the reqested compression quality


  function ICDecompressImage( // Decompresses a given image.
             ic       : HIC;                 // compressor (NULL if any will do)
             uiFlags  : uint;                // silly flags
             lpbiIn   : PBitmapInfoHeader;   // input DIB format
             lpBits   : pointer;             // input DIB bits
             lpbiOut  : PBitmapInfoHeader    // output format (NULL => default)
           ) : PDib;
  //  input:
  //      hic         compressor to use, if NULL is specifed a
  //                  compressor will be located that can handle the conversion.
  //      uiFlags     flags (not used, must be 0)
  //      lpbiIn      input DIB format
  //      lpBits      input DIB bits
  //      lpbiOut     output format, if NULL is specifed the default
  //                  format choosen be the compressor will be used.
  //      lQuality    the reqested compression quality

  //============================================================================

  const // this code in biCompression means the DIB must be accesed via
        // 48 bit pointers! using *ONLY* the selector given.
    BI_1632      = $32333631;  // '1632'

  const // Error conditions
    ICERR_OK           = 00;
    ICERR_DONTDRAW     = 01;
    ICERR_NEWPALETTE   = 02;
    ICERR_GOTOKEYFRAME = 03;
    ICERR_STOPDRAWING  = 04;

    ICERR_UNSUPPORTED  = -1;
    ICERR_BADFORMAT    = -2;
    ICERR_MEMORY       = -3;
    ICERR_INTERNAL     = -4;
    ICERR_BADFLAGS     = -5;
    ICERR_BADPARAM     = -6;
    ICERR_BADSIZE      = -7;
    ICERR_BADHANDLE    = -8;
    ICERR_CANTUPDATE   = -9;
    ICERR_ABORT        = -10;
    ICERR_ERROR        = -100;
    ICERR_BADBITDEPTH  = -200;
    ICERR_BADIMAGESIZE = -201;

    ICERR_CUSTOM           = -400;    // errors less than ICERR_CUSTOM...

  const // Values for dwFlags of ICOpen()
    ICMODE_COMPRESS       = 1;
    ICMODE_DECOMPRESS     = 2;
    ICMODE_FASTDECOMPRESS = 3;
    ICMODE_QUERY          = 4;
    ICMODE_FASTCOMPRESS   = 5;
    ICMODE_DRAW           = 8;

  const // Flags for AVI file index
    AVIIF_LIST     = $00000001;
    AVIIF_TWOCC    = $00000002;
    AVIIF_KEYFRAME = $00000010;

  const // quality flags
    ICQUALITY_LOW     = 0;
    ICQUALITY_HIGH    = 10000;
    ICQUALITY_DEFAULT = -1;

  const // ICM specific messages.
    ICM_USER                   = DRV_USER + $0000;

    ICM_COMPRESS_GET_FORMAT    = ICM_USER + 4;    // get compress format or size
    ICM_COMPRESS_GET_SIZE      = ICM_USER + 5;    // get output size
    ICM_COMPRESS_QUERY         = ICM_USER + 6;    // query support for compress
    ICM_COMPRESS_BEGIN         = ICM_USER + 7;    // begin a series of compress calls.
    ICM_COMPRESS               = ICM_USER + 8;    // compress a frame
    ICM_COMPRESS_END           = ICM_USER + 9;    // end of a series of compress calls.

    ICM_DECOMPRESS_GET_FORMAT  = ICM_USER + 10;   // get decompress format or size
    ICM_DECOMPRESS_QUERY       = ICM_USER + 11;   // query support for dempress
    ICM_DECOMPRESS_BEGIN       = ICM_USER + 12;   // start a series of decompress calls
    ICM_DECOMPRESS             = ICM_USER + 13;   // decompress a frame
    ICM_DECOMPRESS_END         = ICM_USER + 14;   // end a series of decompress calls
    ICM_DECOMPRESS_SET_PALETTE = ICM_USER + 29;   // fill in the DIB color table
    ICM_DECOMPRESS_GET_PALETTE = ICM_USER + 30;   // fill in the DIB color table

    ICM_DRAW_QUERY             = ICM_USER + 31;   // query support for dempress
    ICM_DRAW_BEGIN             = ICM_USER + 15;   // start a series of draw calls
    ICM_DRAW_GET_PALETTE       = ICM_USER + 16;   // get the palette needed for drawing
    ICM_DRAW_START             = ICM_USER + 18;   // start decompress clock
    ICM_DRAW_STOP              = ICM_USER + 19;   // stop decompress clock
    ICM_DRAW_END               = ICM_USER + 21;   // end a series of draw calls
    ICM_DRAW_GETTIME           = ICM_USER + 32;   // get value of decompress clock
    ICM_DRAW                   = ICM_USER + 33;   // generalized "render" message
    ICM_DRAW_WINDOW            = ICM_USER + 34;   // drawing window has moved or hidden
    ICM_DRAW_SETTIME           = ICM_USER + 35;   // set correct value for decompress clock
    ICM_DRAW_REALIZE           = ICM_USER + 36;   // realize palette for drawing
    ICM_DRAW_FLUSH             = ICM_USER + 37;   // clear out buffered frames
    ICM_DRAW_RENDERBUFFER      = ICM_USER + 38;   // draw undrawn things in queue

    ICM_DRAW_START_PLAY        = ICM_USER + 39;   // start of a play
    ICM_DRAW_STOP_PLAY         = ICM_USER + 40;   // end of a play

    ICM_DRAW_SUGGESTFORMAT     = ICM_USER + 50;   // Like ICGetDisplayFormat
    ICM_DRAW_CHANGEPALETTE     = ICM_USER + 51;   // for animating palette

    ICM_GETBUFFERSWANTED       = ICM_USER + 41;   // ask about prebuffering

    ICM_GETDEFAULTKEYFRAMERATE = ICM_USER + 42;   // get the default value for key frames

    ICM_DECOMPRESSEX_BEGIN     = ICM_USER + 60;   // start a series of decompress calls
    ICM_DECOMPRESSEX_QUERY     = ICM_USER + 61;   // start a series of decompress calls
    ICM_DECOMPRESSEX           = ICM_USER + 62;   // decompress a frame
    ICM_DECOMPRESSEX_END       = ICM_USER + 63;   // end a series of decompress calls

    ICM_COMPRESS_FRAMES_INFO   = ICM_USER + 70;   // tell about compress to come
    ICM_SET_STATUS_PROC        = ICM_USER + 72;   // set status callback

  const // Messages ========================
    ICM_RESERVED_LOW      = DRV_USER + $1000;
    ICM_RESERVED_HIGH     = DRV_USER + $2000;
    ICM_RESERVED          = ICM_RESERVED_LOW;

    ICM_GETSTATE          = ICM_RESERVED + 0;    // Get compressor state
    ICM_SETSTATE          = ICM_RESERVED + 1;    // Set compressor state
    ICM_GETINFO           = ICM_RESERVED + 2;    // Query info about the compressor

    ICM_CONFIGURE         = ICM_RESERVED + 10;   // show the configure dialog
    ICM_ABOUT             = ICM_RESERVED + 11;   // show the about box

    ICM_GETDEFAULTQUALITY = ICM_RESERVED + 30;   // get the default value for quality
    ICM_GETQUALITY        = ICM_RESERVED + 31;   // get the current value for quality
    ICM_SETQUALITY        = ICM_RESERVED + 32;   // set the default value for quality

    ICM_SET               = ICM_RESERVED + 40;   // Tell the driver something
    ICM_GET               = ICM_RESERVED + 41;   // Ask the driver something

  const // Constants for ICM_SET:
    ICM_FRAMERATE    : array[0..3] of char = ('F','r','m','R');
    ICM_KEYFRAMERATE : array[0..3] of char = ('K','e','y','R');

  type
    TIcOpen =
      packed record
        dwSize      : dword;   // sizeof(ICOPEN)
        fccType     : dword;   // 'vidc'
        fccHandler  : dword;   //
        dwVersion   : dword;   // version of compman opening you
        dwFlags     : dword;   // LOWORD is type specific
        dwError     : LRESULT; // error return.
        pV1Reserved : pointer; // Reserved
        pV2Reserved : pointer; // Reserved
        dnDevNode   : dword;   // Devnode for PnP devices
      end;

  const // Flags for the <dwFlags> field of the <ICINFO> structure
    VIDCF_QUALITY        = $0001;  // supports quality
    VIDCF_CRUNCH         = $0002;  // supports crunching to a frame size
    VIDCF_TEMPORAL       = $0004;  // supports inter-frame compress
    VIDCF_COMPRESSFRAMES = $0008;  // wants the compress all frames message
    VIDCF_DRAW           = $0010;  // supports drawing
    VIDCF_FASTTEMPORALC  = $0020;  // does not need prev frame on compress
    VIDCF_FASTTEMPORALD  = $0080;  // does not need prev frame on decompress
    VIDCF_QUALITYTIME    = $0040;  // supports temporal quality

    VIDCF_FASTTEMPORAL   = VIDCF_FASTTEMPORALC or VIDCF_FASTTEMPORALD;

  type
    TIcInfo =
      packed record
        dwSize       : dword; // sizeof(ICINFO)
        fccType      : dword; // compressor type     'vidc' 'audc'
        fccHandler   : dword; // compressor sub-type 'rle ' 'jpeg' 'pcm '
        dwFlags      : dword; // flags LOWORD is type specific
        dwVersion    : dword; // version of the driver
        dwVersionICM : dword; // version of the ICM used

        // under Win32, the driver always returns UNICODE strings.
        szName        : array[0..15]  of WideChar; // short name
        szDescription : array[0..127] of WideChar; // long name
        szDriver      : array[0..127] of WideChar; // driver that contains compressor
      end;

  const
    ICCOMPRESS_KEYFRAME = $00000001;

  type
    TIcCompress =
      packed record
        dwFlags     : dword;             // flags

        lpbiOutput  : PBitmapInfoHeader; // output format
        lpOutput    : pointer;           // output data

        lpbiInput   : PBitmapInfoHeader; // format of frame to compress
        lpInput     : pointer;           // frame data to compress

        lpckid      : pdword;            // ckid for data in AVI file
        lpdwFlags   : pdword;            // flags in the AVI index.
        lFrameNum   : longint;           // frame number of seq.
        dwFrameSize : longint;           // reqested size in bytes. (if non zero)

        dwQuality   : dword;             // quality

        // these are new fields
        lpbiPrev    : PBitmapInfoHeader; // format of previous frame
        lpPrev      : pointer;           // previous frame
      end;

  const
    ICCOMPRESSFRAMES_PADDING = $00000001;

  type
    TGetDataProc = function ( lInput : LPARAM; lFrame : longint; lpBits : pointer; len : longint ) : longint;
    TPutDataProc = function ( lInput : LPARAM; lFrame : longint; lpBits : pointer; len : longint ) : longint;

  type
    TIcCompressFrames =
      packed record
        dwFlags            : dword;             // flags

        lpbiOutput         : PBitmapInfoHeader; // output format
        lOutput            : LPARAM;            // output data

        lpbiInput          : PBitmapInfoHeader; // format of frame to compress
        lInput             : LPARAM;            // frame data to compress

        lStartFrame        : longint;           // start frame
        lFrameCount        : longint;           // # of frames

        lQuality           : longint;           // quality
        lDataRate          : longint;           // data rate
        lKeyRate           : longint;           // key frame rate

        dwRate             : dword;             // frame rate, as always
        dwScale            : dword;

        dwOverheadPerFrame : dword;
        dwReserved2        : dword;

        GetData            : TGetDataProc;
        PutData            : TPutDataProc;
      end;

  const // messages for Status callback
    ICSTATUS_START  = 0;
    ICSTATUS_STATUS = 1; // l == % done
    ICSTATUS_END    = 2;
    ICSTATUS_ERROR  = 3; // l == error string (LPSTR)
    ICSTATUS_YIELD  = 4;

  type
    TStatusProc = function ( lParam : LPARAM; Msg : uint; l : longint ) : LRESULT;

  type
    TIcSetStatusProc =
      packed record
        dwFlags : dword;
        Param   : LPARAM;
        Status  : TStatusProc; // return nonzero means abort operation in progress
      end;

  const
    ICDECOMPRESS_HURRYUP     = $80000000; // don't draw just buffer (hurry up!)
    ICDECOMPRESS_UPDATE      = $40000000; // don't draw just update screen
    ICDECOMPRESS_PREROLL     = $20000000; // this frame is before real start
    ICDECOMPRESS_NULLFRAME   = $10000000; // repeat last frame
    ICDECOMPRESS_NOTKEYFRAME = $08000000; // this frame is not a key frame

  type
    TIcDecompress =
      packed record
        dwFlags     : dword;             // flags (from AVI index...)

        lpbiInput   : PBitmapInfoHeader; // BITMAPINFO of compressed data, biSizeImage has the chunk size
        lpInput     : pointer;           // compressed data

        lpbiOutput  : PBitmapInfoHeader; // DIB to decompress to
        lpOutput    : pointer;           //

        lpckid      : pdword;            // ckid from AVI file
      end;

  type
    TIcDecompressEx =
      packed record
        dwFlags  : dword;             // flags (from AVI index...)

        lpbiSrc  : PBitmapInfoHeader; // BITMAPINFO of compressed data
        lpSrc    : pointer;           // compressed data

        lpbiDst  : PBitmapInfoHeader; // DIB to decompress to
        lpDst    : pointer;           // output data

        xDst     : integer;           // destination rectangle
        yDst     : integer;
        dxDst    : integer;
        dyDst    : integer;

        xSrc     : integer;           // source rectangle
        ySrc     : integer;
        dxSrc    : integer;
        dySrc    : integer;
      end;

  const
    ICDRAW_QUERY       = $00000001;   // test for support
    ICDRAW_FULLSCREEN  = $00000002;   // draw to full screen
    ICDRAW_HDC         = $00000004;   // draw to a HDC/HWND
    ICDRAW_ANIMATE     = $00000008;   // expect palette animation
    ICDRAW_CONTINUE    = $00000010;   // draw is a continuation of previous draw
    ICDRAW_MEMORYDC    = $00000020;   // DC is offscreen, by the way
    ICDRAW_UPDATING    = $00000040;   // We're updating, as opposed to playing
    ICDRAW_RENDER      = $00000080;   // used to render data not draw it
    ICDRAW_BUFFER      = $00000100;   // please buffer this data offscreen, we will need to update it

  type
    TIcDrawBegin =
      packed record
        dwFlags        : dword;             // flags

        hpal           : HPALETTE;          // palette to draw with
        wnd            : HWND;              // window to draw to
        dc             : HDC;               // HDC to draw to

        xDst           : integer;           // destination rectangle
        yDst           : integer;
        dxDst          : integer;
        dyDst          : integer;

        lpbi           : PBitmapInfoHeader; // format of frame to draw

        xSrc           : integer;           // source rectangle
        ySrc           : integer;
        dxSrc          : integer;
        dySrc          : integer;

        dwRate         : dword;             // frames/second = (dwRate/dwScale)
        dwScale        : dword;
      end;

  const
    ICDRAW_HURRYUP      = $80000000;   // don't draw just buffer (hurry up!)
    ICDRAW_UPDATE       = $40000000;   // don't draw just update screen
    ICDRAW_PREROLL      = $20000000;   // this frame is before real start
    ICDRAW_NULLFRAME    = $10000000;   // repeat last frame
    ICDRAW_NOTKEYFRAME  = $08000000;   // this frame is not a key frame

  type
    TIcDraw =
      packed record
        dwFlags  : dword;   // flags
        lpFormat : pointer; // format of frame to decompress
        lpData   : pointer; // frame data to decompress
        cbData   : dword;
        lTime    : longint; // time in drawbegin units (see dwRate and dwScale)
      end;

  type
    TIcDrawSuggest =
      packed record
        lpbiIn          : PBitmapInfoHeader; // format to be drawn
        lpbiSuggest     : PBitmapInfoHeader; // location for suggested format (or NULL to get size)
        dxSrc           : integer;           // source extent or 0
        dySrc           : integer;
        dxDst           : integer;           // dest extent or 0
        dyDst           : integer;
        hicDecompressor : HIC;               // decompressor you can talk to
      end;

  type
    TIcPalette =
      packed record
        dwFlags : dword;   // flags (from AVI index...)
        iStart  : integer; // first palette to change
        iLen    : integer; // count of entries to change.
        lppe    : pointer; // palette
      end;

  // ICM function declarations =====================================================

  function ICInfo( fccType : dword; fccHandler : dword; var Info : TICInfo ) : BOOL; stdcall;
  function ICGetInfo( ic : HIC; var Info : TICInfo; cb : dword ) : BOOL; stdcall;

  function ICOpen( fccType : dword; fccHandler : dword; wMode : uint ) : HIC; stdcall;
  function ICOpenFunction( fccType : dword; fccHandler : dword; wMode : uint; HandlerProc : pointer ) : HIC; stdcall;
  function ICClose( ic : HIC ) : LRESULT; stdcall;

  function ICSendMessage( ic : HIC; msg : UINT; dw1, dw2 : dword ) : LRESULT; stdcall;

  const // Values for wFlags of ICInstall()
    ICINSTALL_UNICODE  = $8000;

    ICINSTALL_FUNCTION = $0001;  // lParam is a DriverProc (function ptr)
    ICINSTALL_DRIVER   = $0002;  // lParam is a driver name (string)
    ICINSTALL_HDRV     = $0004;  // lParam is a HDRVR (driver handle)

    ICINSTALL_DRIVERW  = $8002;  // lParam is a unicode driver name

  function ICInstall( fccType : dword; fccHandler : dword; Param : LPARAM; szDesc : pchar; wFlags : uint ) : BOOL; stdcall;
  function ICRemove( fccType : dword; fccHandler : dword; wFlags : uint ) : BOOL; stdcall;

  // Query macros

  const
    ICMF_CONFIGURE_QUERY = $00000001;
    ICMF_ABOUT_QUERY     = $00000001;

  function ICQueryAbout( ic : HIC ) : boolean;
  function ICAbout( ic : HIC; Wnd : HWND ) : LRESULT;

  function ICQueryConfigure( ic : HIC ) : boolean;
  function ICConfigure( ic : HIC; Wnd : HWND ) : LRESULT;

  // Get/Set state macros

  function ICGetState( ic : HIC; info : pointer; size : dword ) : dword;
  function ICSetState( ic : HIC; info : pointer; size : dword ) : dword;
  function ICGetStateSize( ic : HIC ) : integer;

  // Get value macros

  function ICGetDefaultQuality( ic : HIC ) : integer;
  function ICGetDefaultKeyFrameRate( ic : HIC ) : integer;

  // Draw Window macro

  function ICDrawWindow( ic : HIC; Dest : PRect ) : dword;

  // Compression functions =============================================================================

  // Compress a single frame
  function ICCompress(
             ic          : HIC;
             dwFlags     : dword;                        // flags
             lpbiOutput  : PBitmapInfoHeader;            // output format
             lpData      : pointer;                      // output data
             lpbiInput   : PBitmapInfoHeader;            // format of frame to compress
             lpBits      : pointer;                      // frame data to compress
             lpckid      : pdword;                       // ckid for data in AVI file
             lpdwFlags   : pdword;                       // flags in the AVI index.
             lFrameNum   : longint;                      // frame number of seq.
             dwFrameSize : dword;                        // reqested size in bytes. (if non zero)
             dwQuality   : dword;                        // quality within one frame
             lpbiPrev    : PBitmapInfoHeader;            // format of previous frame
             lpPrev      : pointer                       // previous frame
           ) : dword; stdcall;

  // Start compression from a source format (lpbiInput) to a dest format (lpbiOuput) if supported.
  function ICCompressBegin( ic : HIC; lpbiInput : PBitmapInfoHeader; lpbiOutput : PBitmapInfoHeader ) : LRESULT;
  function ICCompressEnd( ic : HIC ) : LRESULT;

  // determines if compression from a source format (lpbiInput) to a dest format (lpbiOuput) is supported.
  function ICCompressQuery( ic : HIC; lpbiInput : PBitmapInfoHeader; lpbiOutput : PBitmapInfoHeader ) : LRESULT;

  // get the output format, (format of compressed data), if lpbiOutput is NULL return the size in bytes needed for format
  function ICCompressGetFormat( ic : HIC; lpbiInput : PBitmapInfoHeader; lpbiOutput : PBitmapInfoHeader ) : LRESULT;
  function ICCompressGetFormatSize( ic : HIC; lpbiInput : PBitmapInfoHeader ) : dword;

  // return the maximal size of a compressed frame
  function ICCompressGetSize( ic : HIC; lpbiInput : PBitmapInfoHeader; lpbiOutput : PBitmapInfoHeader ) : dword;

  // Decompression functions =============================================================================

  function ICDecompress( // decompress a single frame
             ic : HIC;
             dwFlags : dword;                  // flags (from AVI index...)
             lpbiFormat : PBitmapInfoHeader;   // BITMAPINFO of compressed data, biSizeImage has the chunk size
             lpData     : pointer;             // data
             lpbi       : PBitmapInfoHeader;   // DIB to decompress to
             lpBits     : pointer ) : LRESULT; stdcall;

  function ICDecompressBegin( ic : HIC; lpbiInput : PBitmapInfoHeader; lpbiOutput : PBitmapInfoHeader ) : LRESULT;
  function ICDecompressEnd( ic : HIC ) : LRESULT;

  function ICDecompressQuery( ic : HIC; lpbiInput : PBitmapInfoHeader; lpbiOutput : PBitmapInfoHeader ) : LRESULT;

  function ICDecompressGetFormat( ic : HIC; lpbiInput : PBitmapInfoHeader; lpbiOutput : PBitmapInfoHeader ) : LRESULT;
  function ICDecompressGetFormatSize( ic : HIC; lpbiInput : PBitmapInfoHeader ) : dword;

  // Get the output palette in lpbiOutput
  function ICDecompressGetPalette( ic : HIC; lpbiInput : PBitmapInfoHeader; lpbiOutput : PBitmapInfoHeader ) : LRESULT;
  function ICDecompressSetPalette( ic : HIC; lpbiPalette : PBitmapInfoHeader ) : LRESULT;

  // Decompression (ex) functions =========================================================================

  function ICDecompressEx( // Decompress a single frame
              ic : HIC; dwFlags : dword;
              lpbiSrc : PBitmapInfoHeader; lpSrc : pointer; xSrc, ySrc : integer; dxSrc, dySrc : integer;
              lpbiDst : PBitmapInfoHeader; lpDst : pointer; xDst, yDst : integer; dxDst, dyDst : integer ) : LRESULT;

  function ICDecompressExBegin( // Start compression from a source format (lpbiInput) to a dest format (lpbiOutput) is supported.
              ic : HIC; dwFlags : dword;
              lpbiSrc : PBitmapInfoHeader; lpSrc : pointer; xSrc, ySrc : integer; dxSrc, dySrc : integer;
              lpbiDst : PBitmapInfoHeader; lpDst : pointer; xDst, yDst : integer; dxDst, dyDst : integer ) : LRESULT;

  function ICDecompressExQuery(
              ic : HIC; dwFlags : dword;
              lpbiSrc : PBitmapInfoHeader; lpSrc : pointer; xSrc, ySrc : integer; dxSrc, dySrc : integer;
              lpbiDst : PBitmapInfoHeader; lpDst : pointer; xDst, yDst : integer; dxDst, dyDst : integer ) : LRESULT;

  function ICDecompressExEnd( ic : HIC ) : LRESULT;

  // Higher level functions  ============================================================================

  function ICImageCompress(
             ic        : HIC;         // compressor to use
             uiFlags   : uint;        // flags (none yet)
             lpbiIn    : PBitmapInfo; // format to compress from
             lpBits    : pointer;     // data to compress
             lpbiOut   : PBitmapInfo; // compress to this (NULL ==> default)
             lQuality  : longint;     // quality to use
             var lSize : longint      // compress to this size (0=whatever)
           ) : PDib; stdcall;

  function ICImageDecompress(
             ic        : HIC;         // compressor to use
             uiFlags   : uint;        // flags (none yet)
             lpbiIn    : PBitmapInfo; // format to decompress from
             lpBits    : pointer;     // data to decompress
             lpbiOut   : PBitmapInfo  // decompress to this (NULL ==> default)
           ) : PDib; stdcall;

  // Drawing functions =========================================================================

  function ICDrawBegin( // Start decompressing data with format (lpbiInput) directly to the screen, return zero if the decompressor supports drawing.
              ic           : HIC;
              dwFlags      : dword;
              hpal         : HPALETTE;
              dc           : HDC;
              xDst, yDst   : integer;             // source rectangle
              dxDst, dyDst : integer;
              lpbi         : PBitmapInfoHeader;
              xSrc, ySrc   : integer;
              dxSrc, dySrc : integer;
              dwRate       : dword;              // frames/second = (dwRate/dwScale)
              dwScale      : dword
            ) : dword; stdcall;

  function ICDraw(  // Decompress data directly to the screen
               ic       : HIC;
               dwFlags  : dword;           // flags
               lpFormat : pointer;         // format of frame to decompress
               lpData   : pointer;         // frame data to decompress
               cbData   : dword;           // size of data
               lTime    : longint          // time to draw this frame
             ) : dword; stdcall;

  function ICDrawSuggestFormat(
             ic : HIC;
             lpbiIn : PBitmapInfoHeader;
             lpbiOut : PBitmapInfoHeader;
             dxSrc, dySrc : integer;
             dxDst, dyDst : integer;
             icDecomp : HIC ) : LRESULT;

  function ICDrawQuery( // Determines if the compressor is willing to render the specified format.
             ic : HIC; lpbiInput : PBitmapInfoHeader ) : dword;

  function ICDrawChangePalette( ic : HIC; lpbiInput : PBitmapInfoHeader ) : dword;
  function ICGetBuffersWanted( ic : HIC; var Buffers : dword ) : LRESULT;
  function ICDrawStart( ic : HIC ) : LRESULT;
  function ICDrawEnd( ic : HIC ) : LRESULT;
  function ICDrawStartPlay( ic : HIC; lFrom, lTo : longint ) : LRESULT;
  function ICDrawStop( ic : HIC ) : LRESULT;
  function ICDrawStopPlay( ic : HIC ) : LRESULT;
  function ICDrawGetTime( ic : HIC; var lTime : longint ) : LRESULT;
  function ICDrawSetTime( ic : HIC; lTime : longint ) : LRESULT;
  function ICDrawRealize( ic : HIC; dc : HDC; fBackground : BOOL ) : LRESULT;
  function ICDrawFlush( ic : HIC ) : LRESULT;
  function ICDrawRenderBuffer( ic : HIC ) : LRESULT;

  // Status callback functions =========================================================================

  function ICSetStatusProc( // Set the status callback function
             ic : HIC; dwFlags : dword; Param : LPARAM; StatusProc : TStatusProc ) : LRESULT;

  // Helper routines for DrawDib and MCIAVI  ===========================================================

  function ICDecompressOpen( fccType : dword; fccHandler : dword; lpbiIn, lpbiOut : PBitmapInfoHeader ) : HIC;
  function ICDrawOpen( fccType : dword; fccHandler : dword; lpbiIn : PBitmapInfoHeader ) : HIC;

  function ICLocate( fccType : dword; fccHandler : dword; lpbiIn, lpbiOut : PBitmapInfoHeader; wFlags : word ) : HIC; stdcall;
  function ICGetDisplayFormat( ic : HIC; lpbiIn, lpbiOut : PBitmapInfoHeader; BitDepth : integer; dx, dy : integer ) : HIC; stdcall;

  // Structure used by ICSeqCompressFrame and ICCompressorChoose routines

  type
    PCompVars = ^TCompVars;
    TCompVars =
      packed record
        cbSize     : longint;       // set to sizeof(COMPVARS) before calling ICCompressorChoose
        dwFlags    : dword;         // see below...
        ic         : HIC;           // HIC of chosen compressor
        fccType    : dword;         // basically ICTYPE_VIDEO
        fccHandler : dword;         // handler of chosen compressor or
                                    // "" or "DIB "
        lpbiIn     : PBitmapInfo;   // input format
        lpbiOut    : PBitmapInfo;   // output format - will compress to this
        lpBitsOut  : pointer;
        lpBitsPrev : pointer;
        lFrame     : longint;
        lKey       : longint;       // key frames how often?
        lDataRate  : longint;       // desired data rate KB/Sec
        lQ         : longint;       // desired quality
        lKeyCount  : longint;
        lpState    : pointer;       // state of compressor
        cbState    : longint;       // size of the state
      end;

  const // FLAGS for dwFlags element of COMPVARS structure:
    // set this flag if you initialize COMPVARS before calling ICCompressorChoose
    ICMF_COMPVARS_VALID = $00000001;     // COMPVARS contains valid data


  function ICCompressorChoose( //  allows user to choose compressor, quality etc...
             WinHandle : HWND;          // parent window for dialog
             uiFlags   : uint;          // flags
             pvIn      : pointer;       // input format (optional)
             lpData    : pointer;       // input data (optional)
             var cv    : TCompVars;     // data about the compressor/dlg
             lpszTitle : pchar          // dialog title (optional)
           ) : BOOL; stdcall;

  const // defines for uiFlags
    ICMF_CHOOSE_KEYFRAME       = $0001;  // show KeyFrame Every box
    ICMF_CHOOSE_DATARATE       = $0002;  // show DataRate box
    ICMF_CHOOSE_PREVIEW        = $0004;  // allow expanded preview dialog
    ICMF_CHOOSE_ALLCOMPRESSORS = $0008;  // don't only show those that can handle the input format or input data

  function  ICSeqCompressFrameStart( pc : PCompVars; lpbiIn : PBitmapInfo ) : BOOL; stdcall;
  procedure ICSeqCompressFrameEnd( pc : PCompVars ); stdcall;

  function ICSeqCompressFrame(
             pc      : PCompVars;       // set by ICCompressorChoose
             uiFlags : uint;            // flags
             lpBits  : pointer;         // input DIB bits
             var Key : boolean;         // did it end up being a key frame?
             var lSize : longint        // size to compress to/of returned image
           ) : pointer; stdcall;

  procedure ICCompressorFree( pc : PCompVars ); stdcall;

implementation

  uses
    NumUtils;

  function StrToFourCC( const FourCC : string ) : dword;
    begin
      Result := VideoICM.FourCC( FourCC[1], FourCC[2], FourCC[3], FourCC[4] );
    end;

  function FourCC( ch1, ch2, ch3, ch4 : char ) : dword;
    begin
      Result := ( byte( ch4 ) shl 24 ) or ( byte( ch3 ) shl 16 ) or ( byte( ch2 ) shl 8 ) or byte( ch1 );
    end;

  function FourCCToStr( FourCC : dword ) : string;
    begin
      SetLength( Result, 4 );
      pdword( Result )^ := FourCC;
    end;

  function ICInfo; external 'MSVFW32.DLL';
  function ICGetInfo; external 'MSVFW32.DLL';
  function ICOpen; external 'MSVFW32.DLL';
  function ICOpenFunction; external 'MSVFW32.DLL';
  function ICClose; external 'MSVFW32.DLL';
  function ICSendMessage; external 'MSVFW32.DLL';

  function ICInstall; external 'MSVFW32.DLL';
  function ICRemove; external 'MSVFW32.DLL';

  function ICCompress; external 'MSVFW32.DLL';
  function ICDecompress; external 'MSVFW32.DLL';

  function ICDrawBegin; external 'MSVFW32.DLL';
  function ICDraw; external 'MSVFW32.DLL';

  function  ICLocate; external 'MSVFW32.DLL';
  function  ICGetDisplayFormat; external 'MSVFW32.DLL';
  function  ICImageCompress; external 'MSVFW32.DLL';
  function  ICImageDecompress; external 'MSVFW32.DLL';
  function  ICCompressorChoose; external 'MSVFW32.DLL';
  function  ICSeqCompressFrameStart; external 'MSVFW32.DLL';
  procedure ICSeqCompressFrameEnd; external 'MSVFW32.DLL';
  function  ICSeqCompressFrame; external 'MSVFW32.DLL';
  procedure ICCompressorFree; external 'MSVFW32.DLL';

  // Query macros

  function ICQueryAbout( ic : HIC ) : boolean;
    begin
      Result := ICSendMessage( ic, ICM_ABOUT, -1, ICMF_ABOUT_QUERY ) = ICERR_OK;
    end;

  function ICAbout( ic : HIC; Wnd : HWND ) : LRESULT;
    begin
      Result := ICSendMessage( ic, ICM_ABOUT, Wnd, 0 );
    end;

  function ICQueryConfigure( ic : HIC ) : boolean;
    begin
      Result := ICSendMessage( ic, ICM_CONFIGURE, -1, ICMF_CONFIGURE_QUERY ) = ICERR_OK;
    end;

  function ICConfigure( ic : HIC; Wnd : HWND ) : LRESULT;
    begin
      Result := ICSendMessage( ic, ICM_CONFIGURE, Wnd, 0 );
    end;

  // Get/Set state macros

  function ICGetState( ic : HIC; info : pointer; size : dword ) : dword;
    begin
      Result := ICSendMessage( ic, ICM_GETSTATE, integer( info ), size );
    end;

  function ICSetState( ic : HIC; info : pointer; size : dword ) : dword;
    begin
      Result := ICSendMessage( ic, ICM_SETSTATE, integer( info ), size );
    end;

  function ICGetStateSize( ic : HIC ) : integer;
    begin
      Result := ICGetState( ic, nil, 0 );
    end;

  // Get value macros

  function ICGetDefaultQuality( ic : HIC ) : integer;
    begin
      ICSendMessage( ic, ICM_GETDEFAULTQUALITY, integer( @Result ), sizeof( Result ) );
    end;

  function ICGetDefaultKeyFrameRate( ic : HIC ) : integer;
    begin
      ICSendMessage( ic, ICM_GETDEFAULTKEYFRAMERATE, integer( @Result ), sizeof( Result ) );
    end;

  // Draw Window macro

  function ICDrawWindow( ic : HIC; Dest : PRect ) : dword;
    begin
      Result := ICSendMessage( ic, ICM_DRAW_WINDOW, integer( Dest ), sizeof( TRect ) );
    end;

  // Compression functions =============================================================================

  function ICCompressBegin( ic : HIC; lpbiInput : PBitmapInfoHeader; lpbiOutput : PBitmapInfoHeader ) : LRESULT;
    begin
      Result := ICSendMessage( IC, ICM_COMPRESS_BEGIN, integer(lpbiInput), integer(lpbiOutput) );
    end;

  function ICCompressQuery( ic : HIC; lpbiInput : PBitmapInfoHeader; lpbiOutput : PBitmapInfoHeader ) : LRESULT;
    begin
      Result := ICSendMessage( IC, ICM_COMPRESS_QUERY, integer(lpbiInput), integer(lpbiOutput) );
    end;

  function ICCompressGetFormat( ic : HIC; lpbiInput : PBitmapInfoHeader; lpbiOutput : PBitmapInfoHeader ) : LRESULT;
    begin
      Result := ICSendMessage( ic, ICM_COMPRESS_GET_FORMAT, integer(lpbiInput), integer(lpbiOutput) );
    end;

  function ICCompressGetFormatSize( ic : HIC; lpbiInput : PBitmapInfoHeader ) : dword;
    begin
      Result := ICCompressGetFormat( ic, lpbiInput, nil );
    end;

  function ICCompressGetSize( ic : HIC; lpbiInput : PBitmapInfoHeader; lpbiOutput : PBitmapInfoHeader ) : dword;
    begin
      Result := ICSendMessage( ic, ICM_COMPRESS_GET_SIZE, integer(lpbiInput), integer(lpbiOutput) );
    end;

  function ICCompressEnd( ic : HIC ) : LRESULT;
    begin
      Result := ICSendMessage( ic, ICM_COMPRESS_END, 0, 0 );
    end;

  // Decompression functions =============================================================================

  function ICDecompressBegin( ic : HIC; lpbiInput : PBitmapInfoHeader; lpbiOutput : PBitmapInfoHeader ) : LRESULT;
    begin
      Result := ICSendMessage( ic, ICM_DECOMPRESS_BEGIN, integer(lpbiInput), integer(lpbiOutput) );
    end;

  function ICDecompressQuery( ic : HIC; lpbiInput : PBitmapInfoHeader; lpbiOutput : PBitmapInfoHeader ) : LRESULT;
    begin
      Result := ICSendMessage( IC, ICM_DECOMPRESS_QUERY, integer(lpbiInput), integer(lpbiOutput) );
    end;

  function ICDecompressGetFormat( ic : HIC; lpbiInput : PBitmapInfoHeader; lpbiOutput : PBitmapInfoHeader ) : LRESULT;
    begin
      Result := ICSendMessage( ic, ICM_DECOMPRESS_GET_FORMAT, integer(lpbiInput), integer(lpbiOutput) );
    end;

  function ICDecompressGetFormatSize( ic : HIC; lpbiInput : PBitmapInfoHeader ) : dword;
    begin
      Result := ICDecompressGetFormat( ic, lpbiInput, nil );
    end;

  function ICDecompressEnd( ic : HIC ) : LRESULT;
    begin
      Result := ICSendMessage( ic, ICM_DECOMPRESS_END, 0, 0 );
    end;

  function ICDecompressGetPalette( ic : HIC; lpbiInput : PBitmapInfoHeader; lpbiOutput : PBitmapInfoHeader ) : LRESULT;
    begin
      Result := ICSendMessage( ic, ICM_DECOMPRESS_GET_PALETTE, integer(lpbiInput), integer(lpbiOutput) );
    end;

  function ICDecompressSetPalette( ic : HIC; lpbiPalette : PBitmapInfoHeader ) : LRESULT;
    begin
      Result := ICSendMessage( ic, ICM_DECOMPRESS_SET_PALETTE, integer(lpbiPalette), 0 );
    end;

  // Decompression (ex) functions =========================================================================

  function ICDecompressEx( // Decompress a single frame
              ic : HIC; dwFlags : dword;
              lpbiSrc : PBitmapInfoHeader; lpSrc : pointer; xSrc, ySrc : integer; dxSrc, dySrc : integer;
              lpbiDst : PBitmapInfoHeader; lpDst : pointer; xDst, yDst : integer; dxDst, dyDst : integer ) : LRESULT;
    var
      ICData : TICDecompressEx;
    begin
      ICData.dwFlags := dwFlags;
      ICData.lpbiSrc := lpbiSrc;
      ICData.lpSrc   := lpSrc;
      ICData.xSrc    := xSrc;
      ICData.ySrc    := ySrc;
      ICData.dxSrc   := dxSrc;
      ICData.dySrc   := dySrc;
      ICData.lpbiDst := lpbiDst;
      ICData.lpDst   := lpDst;
      ICData.xDst    := xDst;
      ICData.yDst    := yDst;
      ICData.dxDst   := dxDst;
      ICData.dyDst   := dyDst;

      // Note that ICM swaps round the length and pointer length in lparam2, pointer in lparam1
      Result := ICSendMessage( ic, ICM_DECOMPRESSEX, integer( @ICData ), sizeof( ICData ) ) ;
    end;

  function ICDecompressExBegin( // Start compression from a source format (lpbiInput) to a dest format (lpbiOutput) is supported.
              ic : HIC; dwFlags : dword;
              lpbiSrc : PBitmapInfoHeader; lpSrc : pointer; xSrc, ySrc : integer; dxSrc, dySrc : integer;
              lpbiDst : PBitmapInfoHeader; lpDst : pointer; xDst, yDst : integer; dxDst, dyDst : integer ) : LRESULT;
    var
      ICData : TICDecompressEx;
    begin
      ICData.dwFlags := dwFlags;
      ICData.lpbiSrc := lpbiSrc;
      ICData.lpSrc   := lpSrc;
      ICData.xSrc    := xSrc;
      ICData.ySrc    := ySrc;
      ICData.dxSrc   := dxSrc;
      ICData.dySrc   := dySrc;
      ICData.lpbiDst := lpbiDst;
      ICData.lpDst   := lpDst;
      ICData.xDst    := xDst;
      ICData.yDst    := yDst;
      ICData.dxDst   := dxDst;
      ICData.dyDst   := dyDst;

      // Note that ICM swaps round the length and pointer length in lparam2, pointer in lparam1
      Result := ICSendMessage( ic, ICM_DECOMPRESSEX_BEGIN, integer( @ICData ), sizeof( ICData ) ) ;
    end;

  function ICDecompressExQuery(
              ic : HIC; dwFlags : dword;
              lpbiSrc : PBitmapInfoHeader; lpSrc : pointer; xSrc, ySrc : integer; dxSrc, dySrc : integer;
              lpbiDst : PBitmapInfoHeader; lpDst : pointer; xDst, yDst : integer; dxDst, dyDst : integer ) : LRESULT;
    var
      ICData : TICDecompressEx;
    begin
      ICData.dwFlags := dwFlags;
      ICData.lpbiSrc := lpbiSrc;
      ICData.lpSrc   := lpSrc;
      ICData.xSrc    := xSrc;
      ICData.ySrc    := ySrc;
      ICData.dxSrc   := dxSrc;
      ICData.dySrc   := dySrc;
      ICData.lpbiDst := lpbiDst;
      ICData.lpDst   := lpDst;
      ICData.xDst    := xDst;
      ICData.yDst    := yDst;
      ICData.dxDst   := dxDst;
      ICData.dyDst   := dyDst;

      // Note that ICM swaps round the length and pointer length in lparam2, pointer in lparam1
      Result := ICSendMessage( ic, ICM_DECOMPRESSEX_QUERY, integer( @ICData ), sizeof( ICData ) ) ;
    end;

  function ICDecompressExEnd( ic : HIC ) : LRESULT;
    begin
      Result := ICSendMessage( ic, ICM_DECOMPRESSEX_END, 0, 0 );
    end;

  // Drawing functions =========================================================================

  function ICDrawSuggestFormat(
             ic : HIC;
             lpbiIn : PBitmapInfoHeader;
             lpbiOut : PBitmapInfoHeader;
             dxSrc, dySrc : integer;
             dxDst, dyDst : integer;
             icDecomp : HIC ) : LRESULT;
    var
      ICData : TICDrawSuggest;
    begin
      ICData.hicDecompressor := icDecomp;
      ICData.lpbiIn          := lpbiIn;
      ICData.lpbiSuggest     := lpbiOut;
      ICData.dxSrc           := dxSrc;
      ICData.dySrc           := dySrc;
      ICData.dxDst           := dxDst;
      ICData.dyDst           := dyDst;

      // Note that ICM swaps round the length and pointer length in lparam2, pointer in lparam1
      Result := ICSendMessage( ic, ICM_DRAW_SUGGESTFORMAT, integer( @ICData ), sizeof( ICData ) ) ;
    end;

  function ICDrawQuery( // Determines if the compressor is willing to render the specified format.
             ic : HIC; lpbiInput : PBitmapInfoHeader ) : dword;
    begin
      Result := ICSendMessage( ic, ICM_DRAW_QUERY, dword( lpbiInput ), 0 );
    end;

  function ICDrawChangePalette( ic : HIC; lpbiInput : PBitmapInfoHeader ) : dword;
    begin
      Result := ICSendMessage( ic, ICM_DRAW_CHANGEPALETTE, dword( lpbiInput ), 0 );
    end;

  function ICGetBuffersWanted( ic : HIC; var Buffers : dword ) : LRESULT;
    begin
      Result := ICSendMessage( ic, ICM_DRAW_CHANGEPALETTE, dword( @Buffers ), 0 );
    end;

  function ICDrawStart( ic : HIC ) : LRESULT;
    begin
      Result := ICSendMessage( ic, ICM_DRAW_START, 0, 0 );
    end;

  function ICDrawEnd( ic : HIC ) : LRESULT;
    begin
      Result := ICSendMessage( ic, ICM_DRAW_END, 0, 0 );
    end;

  function ICDrawStartPlay( ic : HIC; lFrom, lTo : longint ) : LRESULT;
    begin
      Result := ICSendMessage( ic, ICM_DRAW_START_PLAY, lFrom, lTo );
    end;

  function ICDrawStop( ic : HIC ) : LRESULT;
    begin
      Result := ICSendMessage( ic, ICM_DRAW_STOP, 0, 0 );
    end;

  function ICDrawStopPlay( ic : HIC ) : LRESULT;
    begin
      Result := ICSendMessage( ic, ICM_DRAW_STOP_PLAY, 0, 0 );
    end;

  function ICDrawGetTime( ic : HIC; var lTime : longint ) : LRESULT;
    begin
      Result := ICSendMessage( ic, ICM_DRAW_GETTIME, dword( @lTime ), 0 );
    end;

  function ICDrawSetTime( ic : HIC; lTime : longint ) : LRESULT;
    begin
      Result := ICSendMessage( ic, ICM_DRAW_SETTIME, dword( lTime ), 0 );
    end;

  function ICDrawRealize( ic : HIC; dc : HDC; fBackground : BOOL ) : LRESULT;
    begin
      Result := ICSendMessage( ic, ICM_DRAW_REALIZE, dword( dc ), dword( fBackground ) );
    end;

  function ICDrawFlush( ic : HIC ) : LRESULT;
    begin
      Result := ICSendMessage( ic, ICM_DRAW_FLUSH, 0, 0 );
    end;

  function ICDrawRenderBuffer( ic : HIC ) : LRESULT;
    begin
      Result := ICSendMessage( ic, ICM_DRAW_RENDERBUFFER, 0, 0 );
    end;

  // Status callback functions =========================================================================

  function ICSetStatusProc( // Set the status callback function
             ic : HIC; dwFlags : dword; Param : LPARAM; StatusProc : TStatusProc ) : LRESULT;
    var
      ICData : TICSetStatusProc;
    begin
      ICData.dwFlags := dwFlags;
      ICData.Param   := Param;
      ICData.Status  := StatusProc;

      // Note that ICM swaps round the length and pointer length in lparam2, pointer in lparam1
      Result := ICSendMessage( ic, ICM_SET_STATUS_PROC, integer( @ICData ), sizeof( ICData ) ) ;
    end;

  // Helper routines for DrawDib and MCIAVI  ===========================================================

  function ICDecompressOpen( fccType : dword; fccHandler : dword; lpbiIn, lpbiOut : PBitmapInfoHeader ) : HIC;
    begin
      Result := ICLocate( fccType, fccHandler, lpbiIn, lpbiOut, ICMODE_DECOMPRESS );
    end;

  function ICDrawOpen( fccType : dword; fccHandler : dword; lpbiIn : PBitmapInfoHeader ) : HIC;
    begin
      Result := ICLocate( fccType, fccHandler, lpbiIn, nil, ICMODE_DRAW );
    end;

  function ICCompressDecompressImage(
             ic       : HIC;                 // compressor (NULL if any will do)
             uiFlags  : uint;                // silly flags
             lpbiIn   : PBitmapInfoHeader;   // input DIB format
             lpBits   : pointer;             // input DIB bits
             lpbiOut  : PBitmapInfoHeader;   // output format (NULL => default)
             lQuality : longint              // the reqested quality
           ) : PDib;
    begin
      if lpbiIn.biCompression = 0
        then
          begin
            Result := ICCompressImage( ic, uiFlags, lpbiIn, lpBits, lpbiOut, lQuality );
            if Result = nil
              then Result := ICDecompressImage( ic, uiFlags, lpbiIn, lpBits, lpbiOut );
          end
        else
          begin
            Result := ICDecompressImage( ic, uiFlags, lpbiIn, lpBits, lpbiOut );
            if Result = nil
              then Result := ICCompressImage( ic, uiFlags, lpbiIn, lpBits, lpbiOut, lQuality );
          end
    end;

  function ICCompressImage( ic       : HIC;                 // compressor (NULL if any will do)
                            uiFlags  : uint;                // silly flags
                            lpbiIn   : PBitmapInfoHeader;   // input DIB format
                            lpBits   : pointer;             // input DIB bits
                            lpbiOut  : PBitmapInfoHeader;   // output format (NULL => default)
                            lQuality : longint              // the reqested quality
                          ) : PDib;
  var
    l             : longint;
    UseDefaultIC  : boolean;
    SourceTopDown : boolean;
    dwFlags       : dword;
    ckid          : dword;
    lpbi          : PDib;
  begin
    dwFlags       := 0;
    ckid          := 0;
    lpbi          := nil;
    Result        := nil;
    UseDefaultIC  := false;
    SourceTopDown := false;

    try
      SourceTopDown := ( lpbiIn.biHeight < 0 ); // This ICM thing can't handle top-down DIBs!! Can you believe it?
      if SourceTopDown
        then
          begin
            lpbiIn := DibInvert( lpbiIn, lpBits );
            lpBits := DibPtr( lpbiIn );
          end;

      UseDefaultIC := ( ic = 0 );
      if UseDefaultIC
        then // either locate a compressor or use the one supplied
          ic := ICLocate( ICTYPE_VIDEO, 0, lpbiIn, lpbiOut, ICMODE_COMPRESS );
      if ic <> 0
        then // make sure the found compressor can handle this format.
          if ICCompressQuery( ic, lpbiIn, nil ) = ICERR_OK
            then // now make a DIB header big enough to hold the ouput format
              begin
                l := ICCompressGetFormatSize( ic, lpbiIn );
                if l > 0
                  then
                    begin
                      getmem( lpbi, l + 256 * sizeof( TRGBQuad ) );

                      // if the compressor likes the passed format, use it else use the default
                      // format of the compressor.
                      if (lpbiOut = nil) or
                          ( ICCompressQuery( ic, lpbiIn, lpbiOut ) <> ICERR_OK )
                        then ICCompressGetFormat( ic, lpbiIn, lpbi )
                        else move( lpbiOut^, lpbi^, lpbiOut.biSize + lpbiOut.biClrUsed * sizeof(TRGBQuad) );

                      with lpbi^ do
                        begin
                          biSizeImage := ICCompressGetSize( ic, lpbiIn, lpbi );
                          biClrUsed   := DibNumColors( lpbi );
                        end;

                      // now resize the DIB to be the maximal size.
                      ReallocMem( lpbi, DibSize( lpbi ) );

                      if ICCompressBegin( ic, lpbiIn, lpbi ) = ICERR_OK // now compress it.
                        then
                          begin
                            if lpBits = nil
                              then lpBits := DibPtr( lpbiIn );

                            if lQuality = ICQUALITY_DEFAULT
                              then lQuality := ICGetDefaultQuality( ic );

                            l := ICCompress(
                                   ic,
                                   0,                // flags
                                   lpbi,             // output format
                                   DibPtr( lpbi ),   // output data
                                   lpbiIn,           // format of frame to compress
                                   lpBits,           // frame data to compress
                                   @ckid,            // ckid for data in AVI file
                                   @dwFlags,         // flags in the AVI index.
                                   0,                // frame number of seq.
                                   0,                // reqested size in bytes. (if non zero)
                                   lQuality,         // quality
                                   nil,              // format of previous frame
                                   nil );            // previous frame
                            if ( l >= ICERR_OK ) and ( ICCompressEnd( ic ) = ICERR_OK )
                              then // now resize the DIB to be the real size.
                                begin
                                  ReallocMem( lpbi, DibSize( lpbi ) );
                                  Result := lpbi;
                                end;
                          end;
                    end;
              end;
      except
        Result := nil;
      end;
    if UseDefaultIC and ( ic <> 0 )
      then ICClose( ic );
    if SourceTopDown
      then DibFree( lpbiIn );
    if ( Result = nil ) and ( lpbi <> nil ) // There was an error...
      then freemem( lpbi );
  end;

  function ICDecompressImage( ic       : HIC;                 // compressor (NULL if any will do)
                              uiFlags  : uint;                // silly flags
                              lpbiIn   : PBitmapInfoHeader;   // input DIB format
                              lpBits   : pointer;             // input DIB bits
                              lpbiOut  : PBitmapInfoHeader    // output format (NULL => default)
                            ) : PDib;
  var
    l            : longint;
    UseDefaultIC : boolean;
    lpbi         : PBitmapInfoHeader;
  begin
    lpbi         := nil;
    Result       := nil;
    UseDefaultIC := false;

    try
      UseDefaultIC := ( ic = 0 );
      if UseDefaultIC
        then // either locate a compressor or use the one supplied.
          ic := ICLocate( ICTYPE_VIDEO, 0, lpbiIn, lpbiOut, ICMODE_DECOMPRESS );
      if ic <> 0
        then // make sure the found compressor can handle this format.
          if ICDecompressQuery( ic, lpbiIn, nil ) = ICERR_OK
            then // now make a DIB header big enought to hold the ouput format
              begin
                l := ICDecompressGetFormatSize( ic, lpbiIn );
                if l > 0
                  then
                    begin
                      getmem( lpbi, l + 256 * sizeof( TRGBQuad ) );

                      // if the compressor likes the passed format, use it else use the default
                      // format of the compressor.
                      if ( lpbiOut = nil ) or
                         ( ICDecompressQuery( ic, lpbiIn, lpbiOut ) <> ICERR_OK )
                        then ICDecompressGetFormat( ic, lpbiIn, lpbi )
                        else move( lpbiOut^, lpbi^, lpbiOut.biSize + lpbiOut.biClrUsed * sizeof(TRGBQuad) );

                      with lpbi^ do
                        begin
                          biSizeImage := DibSize( lpbi ); //ICDecompressGetSize( ic, lpbi );
                          biClrUsed   := DibNumColors( lpbi );
                        end;

                      // for decompress make sure the palette (ie color table) is correct
                      if lpbi.biBitCount <= 8
                        then ICDecompressGetPalette( ic, lpbiIn, lpbi );

                      // now resize the DIB to be the maximal size.
                      ReallocMem( lpbi, DibSize( lpbi ) );

                      if ICDecompressBegin( ic, lpbiIn, lpbi ) = ICERR_OK // now deccompress it.
                        then
                          begin
                            if lpBits = nil
                              then lpBits := DibPtr( lpbiIn );

                            l := ICDecompress(
                                   ic,
                                   0,                // flags
                                   lpbiIn,           // format of frame to decompress
                                   lpBits,           // frame data to decompress
                                   lpbi,             // output format
                                   DibPtr( lpbi ) ); // output data
                            if ( l >= ICERR_OK ) and ( ICDecompressEnd( ic ) = ICERR_OK )
                              then Result := lpbi;
                          end;
                    end;
              end;
      except
        Result := nil;
      end;
    if UseDefaultIC and ( ic <> 0 )
      then ICClose( ic );
    if ( Result = nil ) and ( lpbi <> nil ) // There was an error...
      then freemem( lpbi );
  end;

end.
