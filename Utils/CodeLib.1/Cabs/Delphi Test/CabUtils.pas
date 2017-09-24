unit CabUtils;

interface

  uses
    Windows;

  type
    TFDIError = integer;

  const
    FDIERROR_NONE                    = 0;
    FDIERROR_CABINET_NOT_FOUND       = 1;
    FDIERROR_NOT_A_CABINET           = 2;
    FDIERROR_UNKNOWN_CABINET_VERSION = 3;
    FDIERROR_CORRUPT_CABINET         = 4;
    FDIERROR_ALLOC_FAIL              = 5;
    FDIERROR_BAD_COMPR_TYPE          = 6;
    FDIERROR_MDI_FAIL                = 7;
    FDIERROR_TARGET_FILE             = 8;
    FDIERROR_RESERVE_MISMATCH        = 9;
    FDIERROR_WRONG_CABINET           = 10;
    FDIERROR_USER_ABORT              = 11;


  type
    TFDInotificationType = ( fdintCABINET_INFO, fdintPARTIAL_FILE, fdintCOPY_FILE, fdintCLOSE_FILE_INFO, fdintNEXT_CABINET, fdintPROGRESS );

  type
    TFDINotification =
      packed record
        cb       : longint;
        psz1     : pchar;
        psz2     : pchar;
        psz3     : pchar;         // Points to a 256 character buffer
        pv       : pointer;       // Value for client
        hf       : integer;
        date     : SHORT;
        time     : SHORT;
        attribs  : SHORT;
        setID    : SHORT;        // Cabinet set ID
        iCabinet : SHORT;        // Cabinet number (0-based)
        fdie     : TFDIError;
      end;

  type
    TDecompressNotifier = function( UserId : pointer; fdint : TFDInotificationType; var pfdin : TFDINotification ) : integer; stdcall;

  function DecompressCabFiles( CabFile : pchar; DestDir : pchar; UserData : pointer; Notifier : TDecompressNotifier ) : BOOL; stdcall;

implementation

  const
    CabDllName = 'E:\Esc\Cabs\CabDll\Debug\CabDll.dll';

  function DecompressCabFiles( CabFile : pchar; DestDir : pchar; UserData : pointer; Notifier : TDecompressNotifier ) : BOOL; stdcall; external CabDllName;

end.
