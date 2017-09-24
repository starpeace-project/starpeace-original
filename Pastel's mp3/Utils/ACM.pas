unit ACM;

interface

  uses
    Windows, MMSystem;

  type
    HINSTANCE   = THandle;
    HACMSTREAM  = THandle;
    HACMDRIVER  = THandle;
    PWAVEFILTER = pointer;

  //  acmFormatDetails()

  const
    ACMFORMATDETAILS_FORMAT_CHARS = 128;

  type
    PACMFORMATDETAILS = ^TACMFORMATDETAILS;
    TACMFORMATDETAILS =
      packed record
        cbStruct      : DWORD;
        dwFormatIndex : DWORD;
        dwFormatTag   : DWORD;
        fdwSupport    : DWORD;
        pwfx          : PWAVEFORMATEX;
        cbwfx         : DWORD;
        szFormat      : array[0..pred(ACMFORMATDETAILS_FORMAT_CHARS)] of char;
      end;

  function acmFormatDetails(
                            had        : HACMDRIVER;
                            pafd       : PACMFORMATDETAILS;
                            fdwDetails : DWORD
                            ) : MMRESULT; stdcall;

  const
    ACM_FORMATDETAILSF_INDEX     = $00000000;
    ACM_FORMATDETAILSF_FORMAT    = $00000001;
    ACM_FORMATDETAILSF_QUERYMASK = $0000000F;

  // acmFormatTagDetails()

  const
    ACMFORMATTAGDETAILS_FORMATTAG_CHARS = 48;

  type
    PACMFORMATTAGDETAILS = ^TACMFORMATTAGDETAILS;
    TACMFORMATTAGDETAILS =
      packed record
        cbStruct         : DWORD;
        dwFormatTagIndex : DWORD;
        dwFormatTag      : DWORD;
        cbFormatSize     : DWORD;
        fdwSupport       : DWORD;
        cStandardFormats : DWORD;
        szFormatTag      : array[0..pred(ACMFORMATTAGDETAILS_FORMATTAG_CHARS)] of char;
      end;

  function acmFormatTagDetails(
                               had        : HACMDRIVER;
                               paftd      : PACMFORMATTAGDETAILS;
                               fdwDetails : DWORD
                               ) : MMRESULT; stdcall;



  const
    ACM_FORMATTAGDETAILSF_INDEX       = $00000000;
    ACM_FORMATTAGDETAILSF_FORMATTAG   = $00000001;
    ACM_FORMATTAGDETAILSF_LARGESTSIZE = $00000002;
    ACM_FORMATTAGDETAILSF_QUERYMASK   = $0000000F;


  // acmStreamOpen()

  type
    PACMSTREAMHEADER = ^TACMSTREAMHEADER;
    TACMSTREAMHEADER =
      packed record
        cbStruct             : DWORD;
        fdwStatus            : DWORD;
        dwUser               : DWORD;
        pbSrc                : pointer;
        cbSrcLength          : DWORD;
        cbSrcLengthUsed      : DWORD;
        dwSrcUser            : DWORD;
        pbDst                : pointer;
        cbDstLength          : DWORD;
        cbDstLengthUsed      : DWORD;
        dwDstUser            : DWORD;
        dwReservedDriver     : array[0..9] of DWORD;
      end;

  const
    //  ACMSTREAMHEADER.fdwStatus
    ACMSTREAMHEADER_STATUSF_DONE     = $00010000; // done bit for async conversions.
    ACMSTREAMHEADER_STATUSF_PREPARED = $00020000;
    ACMSTREAMHEADER_STATUSF_INQUEUE  = $00100000;

  function acmStreamOpen(
                         var phas   : HACMSTREAM;    // pointer to stream handle
                         had        : HACMDRIVER;    // optional driver handle
                         pwfxSrc    : PWAVEFORMATEX; // source format to convert
                         pwfxDst    : PWAVEFORMATEX; // required destination format
                         pwfltr     : PWAVEFILTER;   // optional filter
                         dwCallback : DWORD;         // callback
                         dwInstance : DWORD;         // callback instance data
                         fdwOpen    : DWORD          // ACM_STREAMOPENF_* and CALLBACK_*
                        ) : MMRESULT; stdcall;

  const
    ACM_STREAMOPENF_QUERY       = $00000001;
    ACM_STREAMOPENF_ASYNC       = $00000002;
    ACM_STREAMOPENF_NONREALTIME = $00000004;


  // acmStreamClose()

  function acmStreamClose(has : HACMSTREAM; fdwClose : DWORD) : MMRESULT; stdcall;


  //  acmStreamSize()

  function acmStreamSize(
                         has                : HACMSTREAM;
                         cbInput            : DWORD;
                         var pdwOutputBytes : DWORD;
                         fdwSize            : DWORD
                         ) : MMRESULT; stdcall;

  const
    ACM_STREAMSIZEF_SOURCE      = $00000000;
    ACM_STREAMSIZEF_DESTINATION = $00000001;
    ACM_STREAMSIZEF_QUERYMASK   = $0000000F;

  // acmStreamReset()

  function acmStreamReset(has : HACMSTREAM; fdwReset : DWORD) : MMRESULT; stdcall;

  // acmStreamMessage()

  function acmStreamMessage(
                            has     : HACMSTREAM;
                            uMsg    : UINT;
                            lParam1 : LPARAM;
                            lParam2 : LPARAM
                            ) : MMRESULT; stdcall;

  // acmStreamConvert()

  function acmStreamConvert(
                            has        : HACMSTREAM;
                            pash       : PACMSTREAMHEADER;
                            fdwConvert : DWORD
                            ) : MMRESULT; stdcall;

  const
    ACM_STREAMCONVERTF_BLOCKALIGN = $00000004;
    ACM_STREAMCONVERTF_START      = $00000010;
    ACM_STREAMCONVERTF_END        = $00000020;


  // acmStreamPrepareHeader()

  function acmStreamPrepareHeader(
                                  has        : HACMSTREAM;
                                  pash       : PACMSTREAMHEADER;
                                  fdwPrepare : DWORD
                                  ) : MMRESULT; stdcall;

  // acmStreamUnprepareHeader()

  function acmStreamUnprepareHeader(
                                    has          : HACMSTREAM;
                                    pash         : PACMSTREAMHEADER;
                                    fdwUnprepare : DWORD
                                    ) : MMRESULT; stdcall;

  // acmMetrics()

  type
    HACMOBJ = THandle;

  function acmMetrics(
                      hao     : HACMOBJ;
                      uMetric : UINT;
                      var Metric
                      ) : MMRESULT; stdcall;

  const
    ACM_METRIC_COUNT_DRIVERS          = 1;
    ACM_METRIC_COUNT_CODECS           = 2;
    ACM_METRIC_COUNT_CONVERTERS       = 3;
    ACM_METRIC_COUNT_FILTERS          = 4;
    ACM_METRIC_COUNT_DISABLED         = 5;
    ACM_METRIC_COUNT_HARDWARE         = 6;
    ACM_METRIC_COUNT_LOCAL_DRIVERS    = 20;
    ACM_METRIC_COUNT_LOCAL_CODECS     = 21;
    ACM_METRIC_COUNT_LOCAL_CONVERTERS = 22;
    ACM_METRIC_COUNT_LOCAL_FILTERS    = 23;
    ACM_METRIC_COUNT_LOCAL_DISABLED   = 24;
    ACM_METRIC_HARDWARE_WAVE_INPUT    = 30;
    ACM_METRIC_HARDWARE_WAVE_OUTPUT   = 31;
    ACM_METRIC_MAX_SIZE_FORMAT        = 50;
    ACM_METRIC_MAX_SIZE_FILTER        = 51;
    ACM_METRIC_DRIVER_SUPPORT         = 100;
    ACM_METRIC_DRIVER_PRIORITY        = 101;


  //  acmFormatSuggest()

  function acmFormatSuggest(
                            had        : HACMDRIVER;
                            pwfxSrc    : PWAVEFORMATEX;
                            pwfxDst    : PWAVEFORMATEX;
                            cbwfxDst   : DWORD;
                            fdwSuggest : DWORD
                            ) : MMRESULT; stdcall;

  const
    ACM_FORMATSUGGESTF_WFORMATTAG     = $00010000;
    ACM_FORMATSUGGESTF_NCHANNELS      = $00020000;
    ACM_FORMATSUGGESTF_NSAMPLESPERSEC = $00040000;
    ACM_FORMATSUGGESTF_WBITSPERSAMPLE = $00080000;

    ACM_FORMATSUGGESTF_TYPEMASK       = $00FF0000;

  //  acmFormatChoose()

  const
    ACMHELPMSGSTRING      = 'acmchoose_help';
    ACMHELPMSGCONTEXTMENU = 'acmchoose_contextmenu';
    ACMHELPMSGCONTEXTHELP = 'acmchoose_contexthelp';

    //  MM_ACM_FORMATCHOOSE is sent to hook callbacks by the Format Chooser
    //  Dialog...

    MM_ACM_FORMATCHOOSE = $8000;

    FORMATCHOOSE_MESSAGE           = 0;
    FORMATCHOOSE_FORMATTAG_VERIFY  = (FORMATCHOOSE_MESSAGE + 0);
    FORMATCHOOSE_FORMAT_VERIFY     = (FORMATCHOOSE_MESSAGE + 1);
    FORMATCHOOSE_CUSTOM_VERIFY     = (FORMATCHOOSE_MESSAGE + 2);

  type
    TACMFORMATCHOOSEHOOKPROC = function(wnd : hWnd; uMsg : uint; awParam : wParam; alParam : lParam) : uint; stdcall;

  PACMFORMATCHOOSE = ^TACMFORMATCHOOSE;
  TACMFORMATCHOOSE =
    packed record
      cbStruct        : DWORD;           // sizeof(ACMFORMATCHOOSE)
      fdwStyle        : DWORD;           // chooser style flags
      hwndOwner       : HWND;            // caller's window handle
      pwfx            : PWAVEFORMATEX;   // ptr to wfx buf to receive choice
      cbwfx           : DWORD;           // size of mem buf for pwfx
      pszTitle        : LPCSTR;          // dialog box title bar
      szFormatTag     : array[0..pred(ACMFORMATTAGDETAILS_FORMATTAG_CHARS)] of char;
      szFormat        : array[0..pred(ACMFORMATDETAILS_FORMAT_CHARS)] of char;
      pszName         : LPSTR;           // custom name selection
      cchName         : DWORD;           // size in chars of mem buf for pszName
      fdwEnum         : DWORD;           // format enumeration restrictions
      pwfxEnum        : PWAVEFORMATEX ;  // format describing restrictions
      hInstance       : HINSTANCE;       // app instance containing dlg template
      pszTemplateName : LPCSTR;          // custom template name
      lCustData       : LPARAM;          // data passed to hook fn.
      pfnHook         : TACMFORMATCHOOSEHOOKPROC; // ptr to hook function
    end;

  const
    //  ACMFORMATCHOOSE.fdwStyle
    ACMFORMATCHOOSE_STYLEF_SHOWHELP             = $00000004;
    ACMFORMATCHOOSE_STYLEF_ENABLEHOOK           = $00000008;
    ACMFORMATCHOOSE_STYLEF_ENABLETEMPLATE       = $00000010;
    ACMFORMATCHOOSE_STYLEF_ENABLETEMPLATEHANDLE = $00000020;
    ACMFORMATCHOOSE_STYLEF_INITTOWFXSTRUCT      = $00000040;
    ACMFORMATCHOOSE_STYLEF_CONTEXTHELP          = $00000080;

    function acmFormatChoose(pafmtc : PACMFORMATCHOOSE) : MMRESULT; stdcall;

implementation

  const
    ACMlib = 'msacm32.dll';

  function acmFormatDetails;         stdcall; external ACMlib;
  function acmFormatTagDetails;      stdcall; external ACMlib;

  function acmStreamOpen;            stdcall; external ACMlib;
  function acmStreamClose;           stdcall; external ACMlib;
  function acmStreamSize;            stdcall; external ACMlib;
  function acmStreamReset;           stdcall; external ACMlib;
  function acmStreamMessage;         stdcall; external ACMlib;
  function acmStreamConvert;         stdcall; external ACMlib;
  function acmStreamPrepareHeader;   stdcall; external ACMlib;
  function acmStreamUnprepareHeader; stdcall; external ACMlib;

  function acmMetrics;               stdcall; external ACMlib;
  function acmFormatSuggest;         stdcall; external ACMlib;

  function acmFormatChoose;          stdcall; external ACMlib;

end.






