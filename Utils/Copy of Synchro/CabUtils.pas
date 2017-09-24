unit CabUtils;

interface

  uses
    Windows, SysUtils, Classes;

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
  function SetupIterateCabinetA(CabinetFile : pchar; Reserved : dword; MsgHandler, Context : pointer) : BOOL; stdcall;

  type
    TDescompressNotifyCommand = (dncCopyFile, dncCopyProgress, dncCloseFile);

    PNotifyDescompressProc = ^TNotifyDescompressProc;
    TNotifyDescompressProc = function(command : TDescompressNotifyCommand; text : string; value : integer) : boolean of object;

  function DecompressFile(filePath, destPath : string; NotifyProc : TNotifyDescompressProc) : boolean;

  const
    SPFILENOTIFY_CABINETINFO    = $10;
    SPFILENOTIFY_FILEINCABINET  = $11;
    SPFILENOTIFY_NEEDNEWCABINET = $12;
    SPFILENOTIFY_FILEEXTRACTED  = $13;

    FILEOP_ABORT                = 0;
    FILEOP_DOIT                 = 1;
    FILEOP_SKIP                 = 2;
    FILEOP_RETRY                = FILEOP_DOIT;
    FILEOP_NEWPATH              = 4;

  type
    PFileInCabinetInfo = ^TFileInCabinetInfo;
    TFileInCabinetInfo =
      packed record
        NameInCabinet  : pchar;
        FileSize       : dword;
        Win32Error     : dword;
        DosDate        : short;
        DosTime        : short;
        DosAttribs     : short;
        FullTargetName : array[0..MAX_PATH-1] of char;
      end;

    PNotifyListCabFileProc = ^TNotifyListCabFileProc;
    TNotifyListCabFileProc = procedure(cabfile : pchar; Info : PFileInCabinetInfo) of object;

  function ListCabFile(filePath : string; NotifyProc : TNotifyListCabFileProc) : boolean;

  const
    CabDllName = 'CABS.DLL';

implementation

  function DecompressCabFiles( CabFile : pchar; DestDir : pchar; UserData : pointer; Notifier : TDecompressNotifier ) : BOOL; stdcall; external CabDllName;
  function SetupIterateCabinetA(CabinetFile : pchar; Reserved : dword; MsgHandler, Context : pointer) : BOOL; stdcall; external 'setupapi.dll';

  function DecompressNotify( UserId : pointer; fdint : TFDINotificationType; var pfdin : TFDINotification ) : integer; stdcall;
    var
      Notify : TNotifyDescompressProc;
    begin
      TMethod(Notify) := TMethod(UserId^);
      case fdint of
        fdintCOPY_FILE:
          begin
            if Notify(dncCopyFile, 'Decompressing ' + pfdin.psz1, 0)
              then Result := 0
              else Result := -1;
          end;
        fdintNEXT_CABINET : Result :=  -1; //abort
        fdintPROGRESS :
          begin
            if Notify(dncCopyProgress, '', pfdin.cb)
              then Result := 0
              else Result := -1;
          end;
        fdintCLOSE_FILE_INFO :
          begin
            if Notify(dncCloseFile, '', 0)
              then Result := 0
              else Result := -1;
          end;
        else Result := 0;
      end;
    end;

  function DecompressFile(filePath, destPath : string; NotifyProc : TNotifyDescompressProc) : boolean;
    var
      Method : TMethod;
    begin
      Method := TMethod(NotifyProc);
      result := DecompressCabFiles(pchar(filePath), pchar(destPath), @Method, DecompressNotify);
    end;

  function ListCabNotify(content : pointer; notification, param1, param2 : integer) : integer; stdcall;
    var
      Notify : TNotifyListCabFileProc;
    begin
      TMethod(Notify) := TMethod(content^);
      case notification of
        SPFILENOTIFY_FILEINCABINET :
          begin
            Notify(pchar(param2), PFileInCabinetInfo(param1));
            result := FILEOP_SKIP;
          end
        else result := FILEOP_ABORT;
      end;
    end;

  function ListCabFile(filePath : string; NotifyProc : TNotifyListCabFileProc) : boolean;
    var
      Method : TMethod;
    begin
      Method := TMethod(NotifyProc);
      result := SetupIterateCabinetA(pchar(filePath), 0, @ListCabNotify, @Method);
    end;

{
  function DecompressCabNotify(content : pointer; notification, param1, param2 : integer) : integer; stdcall;
    var
      path : pchar absolute content;
      Info : PFileInCabinetInfo absolute param1;
      trg  : string;
    begin
      case notification of
        SPFILENOTIFY_FILEINCABINET :
          begin
            trg := path;
            trg := trg + Info.NameInCabinet;
            StrCopy(Info.FullTargetName, pchar(trg));
            result := FILEOP_DOIT;
          end
        else result := FILEOP_ABORT;
      end;
    end;
}

end.

