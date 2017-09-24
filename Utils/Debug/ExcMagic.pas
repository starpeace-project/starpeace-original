unit ExcMagic;

{$I ExcMagicDefines.inc}

{ $D-,L-,Y-} // turn off all debug-info
{$R-}       // turn off range checking
{$H+}       // use huge strings
{$Q-}       // OVERFLOWCHECKS OFF

(*
 -----------------------------------------------------------------------------
 ExceptionalMagic unit. [D3,D4,D5,CB4,CB5]   version 1.51
 (c)Dimus Gremyakoff,1999-2000

 WEB:    http://dimus.virtualave.net
 e-mail: dimus@dimus.virtualave.net (russian/english)
 FIDO:   2:5020/768.57
 -----------------------------------------------------------------------------
*)

(*
 -----------------------------------------------------------------------------

 Conditional defines:

 EXCMAGIC_GUI	create GUI version of unit (uses Forms; intercept Application.ShowException)
 EXCMAGIC_CON	...... Console .....

 EXCMAGIC_DEMO	create unit with some limitations
 EXCMAGIC_DEBUG perform debugging output to ExcMagic.Debug
 -----------------------------------------------------------------------------
*)

interface

{$IFDEF EXCMAGIC_BADCOMPILER}
  Compile with Delphi 3,4,5 or Builder 4,5 !
{$ENDIF}

{$IFDEF EXCMAGIC_Delphi4plus}
  uses Windows, Messages, SysUtils, Classes, CommCtrl, SysConst,
       ExcMemMap, ExcUnmangle, ExcMagicUtils;
{$ELSE}
  uses Windows, Messages, SysUtils, Classes, CommCtrl,
       ExcMemMap, ExcUnmangle, ExcMagicUtils;
{$ENDIF}

{$IFDEF EXCMAGIC_Delphi3}
type
  Longword  = Longint;
  PLongWord = ^Longword;

  PImageDosHeader = ^TImageDosHeader;
    {EXTERNALSYM _IMAGE_DOS_HEADER}
  TImageDosHeader = packed record        { DOS .EXE header                  }
      e_magic: Word;                     { Magic number                     }
      e_cblp: Word;                      { Bytes on last page of file       }
      e_cp: Word;                        { Pages in file                    }
      e_crlc: Word;                      { Relocations                      }
      e_cparhdr: Word;                   { Size of header in paragraphs     }
      e_minalloc: Word;                  { Minimum extra paragraphs needed  }
      e_maxalloc: Word;                  { Maximum extra paragraphs needed  }
      e_ss: Word;                        { Initial (relative) SS value      }
      e_sp: Word;                        { Initial SP value                 }
      e_csum: Word;                      { Checksum                         }
      e_ip: Word;                        { Initial IP value                 }
      e_cs: Word;                        { Initial (relative) CS value      }
      e_lfarlc: Word;                    { File address of relocation table }
      e_ovno: Word;                      { Overlay number                   }
      e_res: array [0..3] of Word;       { Reserved words                   }
      e_oemid: Word;                     { OEM identifier (for e_oeminfo)   }
      e_oeminfo: Word;                   { OEM information; e_oemid specific}
      e_res2: array [0..9] of Word;      { Reserved words                   }
      _lfanew: LongInt;                  { File address of new exe header   }
  end;

  TC_ITEM = packed record
    mask: UINT;
    dwState: UINT;
    dwStateMask: UINT;
    pszText: PAnsiChar;
    cchTextMax: Integer;
    iImage: Integer;
    lParam: LPARAM;
  end;

{$ENDIF}

{$IFDEF EXCMAGIC_DEMO}
const
  MAX_SRCMODULE = 3;
{$ENDIF}

type
  TLongArray = array[0..0] of Longword;
  PLongArray = ^TLongArray;
  TLongwordArray = array[0..0] of Longword;
  PLongwordArray = ^TLongwordArray;

type
  TModuleDebugInfo = class;

  EExcMagicError = class( Exception );

  PDS_SubsectionHeader = ^TDS_SubsectionHeader;
  TDS_SubsectionHeader = packed record
    sshHeaderSize  : Word;
    sshRecordSize  : Word;
    sshRecordCount : Longint;
    reserved: array[0..7] of Byte;
  end;

  TDS_SubsectionEntry = packed record
    sseType:     Word;
    sseModIndex: Word;
    sseOffset:   Longword;
    sseSize:     Longword;
  end;
  TDS_SubsectionEntryArray  = array[0..0] of TDS_SubsectionEntry;
  PTDS_SubsectionEntryArray = ^TDS_SubsectionEntryArray;

  TDS_SegmentRange = packed record
    sStart,sEnd: Longword;
  end;
  TDS_SegmentRangeArray  = array[0..0] of TDS_SegmentRange;
  PTDS_SegmentRangeArray = ^TDS_SegmentRangeArray;

  TDS_LineNum = packed record
    LineNum : Word;
    Offset  : Longword;
  end;

  PExceptionRecord = ^TExceptionRecord;
  TExceptionRecord = packed record
    ExceptionCode        : LongWord;
    ExceptionFlags       : LongWord;
    OuterException       : PExceptionRecord;
    ExceptionAddress     : Pointer;
    NumberParameters     : Longint;
    case {IsOsException:} Boolean of
    True:  (ExceptionInformation : array [0..14] of Longint);
    False: (
       ExceptAddr: Pointer;
       ExceptObject: Pointer;
       ExceptEBX: LongWord;
       ExceptESI: LongWord;
       ExceptEDI: LongWord;
       ExceptEBP: LongWord;
       ExceptESP: LongWord;
    );
  end;

  TModuleDebugInfo = class
  private
    FMMFile   : TMMFileStream;
    FName     : String;
    FLoaded   : Boolean;
    FInstance : THandle;
    FPDosHdr  : PImageDosHeader;
    FPImgHdr  : PImageNtHeaders;
    //
    FSignature    : LongWord;
    FTDSTotalSize : LongWord;
    FTDSOffset    : LongWord;
    FTDSDataSize  : LongWord;
    FTDSSubdir    : TDS_SubsectionHeader;
    FTDSEntries   : PTDS_SubsectionEntryArray;
{$IFDEF EXCMAGIC_DEMO}
    FsstSrcTable  : array[0..MAX_SRCMODULE-1] of TDS_SubsectionEntry;
    FsstSrcCount  : Integer;
{$ENDIF}
    //
    FNamesCount  : Longint;
    FNameIndexes : PLongArray;  // Offsets of all names in debug file
    //
    function  GetConvertedAddress( Address: Pointer ): Pointer;
    function  EntryOffset( Index: Integer ): Longword;
    procedure GetModuleName( Address: Pointer; ModuleName: PChar; ModuleNameSize: Integer );
    function  FindProc( Address: Pointer; Module: Integer ): Integer;
    function  GetSourceLine( Address: Pointer; var SrcFileNameIndex,SrcLineNum: Integer ): Boolean;
    function  GetProcName( Address: Pointer; var ModuleNameIndex,ProcNameIndex: Integer ): Boolean;
    function  GetName( Index: Integer ): String;
    function  GetIsDelphi: Boolean;
    function  CreateNamesArray: Boolean;
    function  LoadTDS( AFileName: String ): Boolean;
    function  LoadTDS2: Boolean;
    function  LoadDebugInfo( FileName: String ): Boolean;
    procedure UnLoadDebugInfo;

  public
    constructor Create( const FileName: String; const Inst: THandle );
    destructor  Destroy; override;

    function  IsInCode( Address: Pointer ): Boolean;
    function  GetLogicalAddr( Address: Pointer ): Pointer;
    function  SourceLine( Address: Pointer; var SrcFileNameIndex,SrcLineNum: Integer ): Boolean;
    function  ProcName( Address: Pointer; var ModuleNameIndex,ProcNameIndex: Integer ): Boolean;

    property  ModuleName: String read FName;
    property  Names[Index: Integer]: String read GetName;
    property  Loaded: Boolean read FLoaded;
    property  IsDelphiModule: Boolean read GetIsDelphi;
  end;

  TCallStackItem = packed record
    DebugModule     : TModuleDebugInfo;
    CallAddress     : Pointer;
    ModuleNameIndex : Integer;
    ProcNameIndex   : Integer;
    FileNameIndex   : Integer;
    FileLineNumber  : Integer;
    NestingLevel    : Integer;
  end;
  PCallStackItem = ^TCallStackItem;

  TExcCallStack = class( TList )
  private
    function    GetItem(Index: Integer): PCallStackItem;
  public
    procedure   GenerateFromAddr( Address: Pointer; RegEBP: LongWord; MaxSize: Integer; SuppressRecursion: Boolean ); stdcall;
    procedure   Generate( MaxSize: Integer; SuppressRecursion: Boolean ); stdcall;
    procedure   Clear; {$IFNDEF EXCMAGIC_Delphi3} override; {$ENDIF}
    procedure   Dump( StrList: TStringList );
    property    Items[Index: Integer]: PCallStackItem read GetItem;
  end;

  { -------------------------- TExcMagic ----------------------- }

  TExceptionMessageInfo = record
    miMessage: String;
    miModuleName: array[0..MAX_PATH] of Char;
    miVirtualAddress: Pointer;
    miModuleAddress: Pointer;
    miModuleNameIndex: Integer;
    miProcNameIndex: Integer;
    miSrcNameIndex: Integer;
    miSrcLineNum: Integer;
    miDebugModule: TModuleDebugInfo;
  end;


  TExcMagicMsgProc  = procedure( ExceptionObject: TObject;
                                 MessageInfo: TExceptionMessageInfo;
                                 Buffer: PChar;
                                 BufferSize: Integer ) of object;
  TExcMagicShowProc = procedure( Title,
                                 ExceptionMessage: String;
                                 CallStack,
                                 Registers,
                                 CustomInfo: TStringList ) of object;
  TExcMagicLogProc = procedure(  Buffer: PChar;
                                 BufferSize: Integer;
                                 CallStack,
                                 Registers,
                                 CustomInfo: TStringList ) of object;
  TExcMagicCustomInfoProc = procedure( CustomInfo: TStringList ) of object;
  TExcMagicTerminateProc  = procedure( var CloseDialog: Boolean ) of object;

  TExcMagicOption   = ( excDlgDetailed,   // show expanded dialog (Detailed)
                        excDlgCallStack,  // show call stack
                        excDlgRegisters,  // show registers
                        excDlgCustomInfo, // show additional custom info
                        excDlgTerminate,  // show terminate button
                        excShowDialog     // show dialog on exception
                      );
  TExcMagicOptions  = set of TExcMagicOption;

  TSourceInfo = record
    ModuleDebugInfo: TModuleDebugInfo;
    ModuleName: String;
    FileName: String;
    ProcName: String;
    LineNumber: Integer;
  end;

  TExcMagic = class
  private
    FCallStack : TExcCallStack;
    FCallStackStrings  : TStringList;
    FContextStrings    : TStringList;
    FCustomInfoStrings : TStringList;
    FCustomTab         : String;
    //
    FModules: TList; { list of TModuleDebugInfo }
    FIcon: PChar;
    FEnabled: Boolean;
    FLogFile: String;
    FLogEnabled: Boolean;
    FLogHandled: Boolean;
    FOptions: TExcMagicOptions;
    FMaxCallStack: Integer;
    FSuppressRecursion: Boolean;
    FOnExceptionMsg:  TExcMagicMsgProc;
    FOnExceptionShow: TExcMagicShowProc;
    FOnExceptionLog:  TExcMagicLogProc;
    FOnCustomInfo:    TExcMagicCustomInfoProc;
    FOnTerminate:     TExcMagicTerminateProc;
    //
    procedure SetEnabled( Value: Boolean );
    function  GetExcMagicAbout: String;
    function  GetExcMagicVersion: String;
    //
    function  FindDebugModule( Address: Pointer ): TModuleDebugInfo;
    procedure LogExceptionData( Buffer: PChar; BufLen: Integer );
    procedure DumpAll;
    procedure DumpContext( StrList: TStringList );
    function GetContext: TContext;
    function GetExceptionRec: TExceptionRecord;
    function GetExceptionInfo: TExceptionMessageInfo;

  public
    constructor Create;
    destructor  Destroy; override;

    function  UnMangle( Source: String; IsDelphi: Boolean ): String;
    function  GetAddressSourceInfo( Address: Pointer; var ModuleDebugInfo: TModuleDebugInfo; var ModuleName: String; var FileName: String;
                             var ProcName: String; var LineNumber: Integer ): Boolean;
    function  GetSourceInfo( var ModuleDebugInfo: TModuleDebugInfo; var ModuleName: String; var FileName: String;
                             var ProcName: String; var LineNumber: Integer ): Boolean; stdcall;
    function  GetAddressSourceInfoRec( Address: Pointer ): TSourceInfo;
    function  GetSourceInfoRec: TSourceInfo; stdcall;

    procedure LogException;
    procedure Log( Text: PChar; WithHeader: Boolean );

    property  About: String read GetExcMagicAbout;
    property  CallStack: TExcCallStack read FCallStack;
    property  CustomTab: String read FCustomTab write FCustomTab;
    property  Enabled: Boolean read FEnabled write SetEnabled default True;
    property  ExceptionContext: TContext read GetContext;
    property  ExceptionInfo: TExceptionMessageInfo read GetExceptionInfo;
    property  ExceptionRec: TExceptionRecord read GetExceptionRec;
    property  Icon: PChar read FIcon write FIcon;
    property  LogEnabled: Boolean read FLogEnabled write FLogEnabled default True;
    property  LogFile: String read FLogFile write FLogFile;
    property  LogHandled: Boolean read FLogHandled write FLogHandled default False;
    property  MaxCallStack: Integer read FMaxCallStack write FMaxCallStack default 100;
    property  SuppressRecursion: Boolean read FSuppressRecursion write FSuppressRecursion default True;
    property  Options: TExcMagicOptions read FOptions write FOptions;
    property  Version: String read GetExcMagicVersion;

    property  OnExceptionMsg:  TExcMagicMsgProc  read FOnExceptionMsg  write FOnExceptionMsg;
    property  OnExceptionShow: TExcMagicShowProc read FOnExceptionShow write FOnExceptionShow;
    property  OnExceptionLog:  TExcMagicLogProc  read FOnExceptionLog  write FOnExceptionLog;
    property  OnCustomInfo:    TExcMagicCustomInfoProc read FOnCustomInfo write FOnCustomInfo;
    property  OnTerminate:     TExcMagicTerminateProc  read FOnTerminate write FOnTerminate;
  end;

var
  ExceptionHook: TExcMagic;

{ =========================== } implementation { =========================== }

{$IFNDEF EXCMAGIC_GUI}
  {$IFNDEF EXCMAGIC_CON}
      Define EXCMAGIC_GUI or EXCMAGIC_CON !!!
  {$ENDIF}
{$ENDIF}


{$IFDEF EXCMAGIC_GUI}
  uses Forms;
{$ENDIF}

{$R ExcMagic.RES}
{ $I MapFiles.inc}

const
  ExcMagicVerNum  = '1.52';
  ExcMagicAbout   = '';
  //
  SOnlyOneAllowed = 'Only one instance of TExcMagic allowed';
  SDefaultLog     = 'ErrorTrace.Log';
  SDefaultCustom  = 'Additional';
{$IFDEF EXCMAGIC_GUI}
  ExcMagicType    = 'GUI';
{$ELSE}
  ExcMagicType    = 'CONSOLE';
{$ENDIF}
{$IFDEF EXCMAGIC_DEMO}
  ExcMagicVersion = 'Demo freeware ver ' + ExcMagicVerNum + ' (' + ExcMagicType + ')';
{$ELSE}
  ExcMagicVersion = 'Full registered ver ' + ExcMagicVerNum + ' (' + ExcMagicType + ')';
{$ENDIF}

const
  FB09_SIGNATURE = $39304246; // 'FB09' - Delphi  TDS
  FB0A_SIGNATURE = $41304246; // 'FB0A' - Builder TDS
  DSI1_SIGNATURE = $31495344; // 'DSI1'

const
  SST_MODULE      = $120;
  SST_ALIGNSYM    = $125;
  SST_SRCMODULE   = $127;
  SST_NAMES       = $130;
  SST_GLOBALTYPES = $12B;

  SST_PREFIXNAMES = $F000 + SST_NAMES;
  SST_PROCARRAY   = $F000 + SST_ALIGNSYM; // new in 1.51

const
  ALIGN_SEARCH    = 5;       // must be 1st chunk
  ALIGN_END       = 6;
  ALIGN_LPROC     = $204;
  ALIGN_GPROC     = $205;
  ALIGN_DIM_LPROC = $F000 + ALIGN_LPROC;
  ALIGN_DIM_GPROC = $F000 + ALIGN_GPROC;

type
  TLongJump = packed record
    OpCode: Byte;
    Offset: DWORD;
  end;

  TDSIHeader = packed record
    Signature    : Longint; // = DSI1_SIGNATURE
    HeaderSize   : Longint; // = SizeOf(TDSIHeader)
    PackedSize   : Longint;
    OriginalSize : Longint;
  end;
  PTDSIHeader = ^TDSIHeader;

  TDSITail = packed record
    Signature    : Longint; // = DSI1_SIGNATURE
    TotalSize    : Longint; // Header + Data + Tail
  end;
  PTDSITail = ^TDSITail;

  PExcFrame = ^TExcFrame;
  TExcFrame =
  record
    next: PExcFrame;
    desc: Pointer;
    hEBP: Pointer;
  end;

var
  ErrorMessageAddr,ShowErrorAddr,ExceptionHandlerAddr: Pointer;
  HandleAnyExceptAddr,HandleAutoExceptAddr,HandleOnExceptAddr: Pointer;

  OldBytesMessage,OldBytesShow: array[0..4] of Byte;
  OldBytesHandleAny:  array[0..10] of Byte;
  OldBytesHandleOn:   array[0..10] of Byte;
  OldBytesHandleAuto: array[0..10] of Byte;
  OldBytesExcHandler: array[0..10] of Byte;
  NewBytesHandleAny:  array[0..10] of Byte = ( $E8, 0, 0, 0, 0,
                                               $66, $F7, $40, $04, $06, $00 );
  NewBytesHandleOn:   array[0..10] of Byte = ( $E8, 0, 0, 0, 0,
                                               $66, $F7, $40, $04, $06, $00 );
  NewBytesHandleAuto: array[0..10] of Byte = ( $E8, 0, 0, 0, 0,
                                               $66, $F7, $40, $04, $06, $00 );
  NewBytesExcHandler: array[0..10] of Byte = ( $E8, 0, 0, 0, 0,
                                               $66, $F7, $40, $04, $06, $00 );
  NewBytesMessage,NewBytesShow: TLongJump;

{$IFDEF EXCMAGIC_GUI}
var
  AppShowExceptionAddr: Pointer;
  OldBytesAppShow: array[0..4] of Byte;
  NewBytesAppShow: TLongJump;
{$ENDIF}

type
  PsstModuleHeader = ^TsstModuleHeader;
  TsstModuleHeader = packed record // 1Ch bytes
    OvlNum    : Word;
    LibIndex  : Word;
    SegCount  : SmallInt;
    DebugStyle: Word;
    NameIndex : Dword;
    TimeStamp : Dword;
    res       : array[0..2] of Dword;
  end;

  PsstModuleSegInfo = ^TsstModuleSegInfo;
  TsstModuleSegInfo = packed record // 0Ch bytes
    SegNumber : Word;
    Flags     : Word; // 0001 = CODE else DATA ???
    Start     : LongWord;
    Size      : LongWord;
  end;

  PsstAlignProc32 = ^TsstAlignProc32;
  TsstAlignProc32 = packed record
    dwReserved1:  LongWord;
    dwParentEnd:  LongWord;
    dwParentNext: LongWord;
    dwLength:     LongWord;
    dwDebugStart: LongWord;
    dwDebugEnd:   LongWord;
    dwStart:      LongWord;
    wSegNumber:   Word;
    wReserved2:   Word;
    dwTypeNumber: LongWord;
    dwNameIndex:  LongWord;
    dwBrowserOfs: LongWord;
    // optional ShortString (linker name)
  end;

  PsstAlignDimusProc32 = ^TsstAlignDimusProc32;
  TsstAlignDimusProc32 = packed record
    dwStart:     LongWord;
    dwEnd:       LongWord;
    dwNameIndex: LongWord;
  end;

// ---------------------------------------------------------------------------

threadvar
  _ExcContext  : TContext;
  _ExcRecord   : TExceptionRecord;
  _ExcMsgInfo  : TExceptionMessageInfo;

var
  ExcMagicLock: TRTLCriticalSection;

procedure SwitchMagicHandler( TurnON: Boolean ); forward;

// ---------------------------------------------------------------------------

function GetModuleName(Module: HMODULE): string;
var
  ModName: array[0..MAX_PATH] of Char;
begin
  SetString(Result, ModName, Windows.GetModuleFileName(Module, ModName, SizeOf(ModName)));
end;

// ---------------------------------------------------------------------------

//
//  Inst = instance of module (= starting virtual address)
//
constructor TModuleDebugInfo.Create( const FileName: String; const Inst: THandle );
begin
  inherited Create;

  FName      := FileName;
  FInstance  := Inst;
  FLoaded    := False;
  // setup pointers to MZ & PE EXE headers
  FPDosHdr   := Pointer( Inst );
  FPImgHdr   := Pointer( Longint(FPDosHdr) + FPDosHdr^._lfanew );

{$IFDEF EXCMAGIC_DEBUG}
  with FPImgHdr^.OptionalHeader do
    begin
      DebugFmt( 'Creating TModuleDebugInfo for %s'#13#10 +
                'Instance %Xh, Base of code: %Xh, Size of code: %Xh',
                [FileName,Inst,BaseOfCode,SizeOfCode] );
    end;
{$ENDIF}

  LoadDebugInfo( FileName );
end;

destructor  TModuleDebugInfo.Destroy;
begin
  UnLoadDebugInfo;
  inherited Destroy;
end;

function TModuleDebugInfo.IsInCode( Address: Pointer ): Boolean;
var
  Info: TMemoryBasicInformation;
begin
{ // this code doesn't work in packed EXEs !!!
  with FPImgHdr^.OptionalHeader do
    Result := (Longword(Address) >= (FInstance + BaseOfCode)) and
              (Longword(Address) <  (FInstance + BaseOfCode + SizeOfCode));
  // so we use less accurate method :-(
  PAGE_EXECUTE and PAGE_EXECUTE_READ and PAGE_EXECUTE_READWRITE and PAGE_EXECUTE_WRITECOPY
}
  Result := False;
  if VirtualQuery(Address, Info, sizeof(Info)) = sizeof(Info) then
    Result := (Info.AllocationBase = Pointer(FInstance)) and
              (Info.State = MEM_COMMIT);
end;

function TModuleDebugInfo.GetLogicalAddr( Address: Pointer ): Pointer;
begin
{
  hard-coded $1000 instead of more correct FPImgHdr^.OptionalHeader.BaseOfCode
  because there are problems with corrupted header in packed EXEs
  BTW Inprise linkers always set code base = $1000 :)
}
  if Address <> nil then Result := Pointer(Longword(Address)-$1000)
                    else Result := nil;
end;

function TModuleDebugInfo.GetConvertedAddress( Address: Pointer ): Pointer;
var
  Info: TMemoryBasicInformation;
begin
  VirtualQuery(Address, Info, sizeof(Info));
  if Info.State <> MEM_COMMIT then
    Result := GetLogicalAddr(Address)
  else
    Result := GetLogicalAddr( Pointer(Integer(Address)-Integer(Info.AllocationBase)) );
end;

procedure TModuleDebugInfo.GetModuleName( Address: Pointer; ModuleName: PChar; ModuleNameSize: Integer );
var
  Info: TMemoryBasicInformation;
  Temp: array[0..MAX_PATH] of Char;
begin
  VirtualQuery(Address, Info, sizeof(Info));
  if (Info.State <> MEM_COMMIT) or (GetModuleFilename(THandle(Info.AllocationBase), Temp, SizeOf(Temp)) = 0) then
    GetModuleFileName(HInstance, Temp, SizeOf(Temp));

  StrLCopy(ModuleName, AnsiStrRScan(Temp, '\') + 1, ModuleNameSize - 1);
end;

function TModuleDebugInfo.GetIsDelphi: Boolean;
begin
  Result := FSignature = FB09_SIGNATURE;
end;

function TModuleDebugInfo.CreateNamesArray: Boolean;
var
  i,j: Integer;
  NameLen: Longword;
{$IFDEF EXCMAGIC_DEMO}
  MH: TsstModuleHeader;
  ModuleOfs: Dword;
{$ENDIF}
begin
  Result := False;
  for i := FTDSSubdir.sshRecordCount-1 downto 0 do
    if FTDSEntries^[i].sseType = SST_NAMES then
      begin
        FMMFile.Seek( EntryOffset(i), soFromBeginning );
        FNamesCount := FMMFile.ReadDword;
        GetMem( FNameIndexes, FNamesCount * SizeOf(Dword) );
        for j := 0 to FNamesCount-1 do
          begin
            FNameIndexes^[j] := FMMFile.Position;
            NameLen := Word(FMMFile.ReadByte);
            FMMFile.Seek( NameLen+1, soFromCurrent );
          end;
        Result := True;
        Break;
      end;
{$IFDEF EXCMAGIC_DEMO}
  FsstSrcCount := 0;
  for i := FTDSSubdir.sshRecordCount-1 downto 0 do
    if FsstSrcCount >= MAX_SRCMODULE then Break
    else
      if FTDSEntries^[i].sseType = SST_SRCMODULE then
        begin
          FsstSrcTable[FsstSrcCount] := FTDSEntries^[i];
          Inc(FsstSrcCount);

          // 1. Find sstModule for current sstSrcModule
          for j := 0 to FTDSSubdir.sshRecordCount-1 do
            if (FTDSEntries^[j].sseType = SST_MODULE) and
               (FTDSEntries^[j].sseModIndex = FTDSEntries^[i].sseModIndex) then
              begin
                ModuleOfs := EntryOffset( j );
                FMMFile.Seek( ModuleOfs, soFromBeginning );
                FMMFile.Read( MH, SizeOf(MH) );
              {$IFDEF EXCMAGIC_DEBUG}
                DebugFmt( '  demo module #%d: %s', [FsstSrcCount,Names[MH.NameIndex]] );
              {$ENDIF}
              end;
        end;
{$ENDIF};
end;

{ index = 1..FNamesCount }
function TModuleDebugInfo.GetName( Index: Integer ): String;
begin
  if (Index > 0) and (Index <= FNamesCount) and (FNameIndexes <> nil) then
    begin
      FMMFile.Seek( FNameIndexes^[Index-1], soFromBeginning );
      SetLength( Result, FMMFile.ReadByte );
      FMMFile.Read( Result[1], Length(Result) );
    end
  else
    Result := '';
end;

{ returns offset from start of file, not start of debuginfo }
function  TModuleDebugInfo.EntryOffset( Index: Integer ): Longword;
begin
  Result := 0;
  if (Index >= 0) and (Index < FTDSSubdir.sshRecordCount) then
    Result := FTDSOffset + FTDSEntries^[Index].sseOffset;
end;

function TModuleDebugInfo.FindProc( Address: Pointer; Module: Integer ): Integer;
var
  i: Integer;
  Proc32: TsstAlignProc32;
  DimProc32: TsstAlignDimusProc32;
  PtrDimProc32: PsstAlignDimusProc32;
  StartOfs,CurOffset,ProcCount: Longword;
  wChunkType,wChunkSize: Word;
begin
  Result := -1;

  if not FLoaded then Exit;

  // 1. Find Module with Address
  for i := 0 to FTDSSubdir.sshRecordCount-1 do
    if FTDSEntries^[i].sseModIndex = Module then
      case FTDSEntries^[i].sseType of
        SST_ALIGNSYM:
          begin
            StartOfs := EntryOffset( i );
            FMMFile.Seek( StartOfs, soFromBeginning );
            if FMMFile.ReadDword <> 1 then Continue; // Check sstAlignSym signature

            // Find S_LPROC32 or S_GPROC32 chunk
            CurOffset := 4;
            while CurOffset < FTDSEntries^[i].sseSize do
              begin
                wChunkSize := FMMFile.ReadWord;
                wChunkType := FMMFile.ReadWord;
                case wChunkType of
                  ALIGN_LPROC,
                  ALIGN_GPROC:
                    begin
                      FMMFile.Read( Proc32, SizeOf(Proc32) );
                      if (Longword(Address) >= Proc32.dwStart) and (Longword(Address) < Proc32.dwStart+Proc32.dwLength) then
                        begin
                          Result := Proc32.dwNameIndex;
                          Exit;
                        end;
                    end;
                  ALIGN_DIM_LPROC,
                  ALIGN_DIM_GPROC:
                    begin
                      FMMFile.Read( DimProc32, SizeOf(DimProc32) );
                      if (Longword(Address) >= DimProc32.dwStart) and (Longword(Address) <= DimProc32.dwEnd) then
                        begin
                          Result := DimProc32.dwNameIndex;
                          Exit;
                        end;
                    end
                end;
                Inc( CurOffset, wChunkSize+2 );
                FMMFile.Seek( StartOfs + CurOffset, soFromBeginning );
              end;
          end;
        SST_PROCARRAY: // new in 1.51
          begin
            StartOfs := EntryOffset( i );
            FMMFile.Seek( StartOfs, soFromBeginning );
            ProcCount := FMMFile.ReadDword;

            PtrDimProc32 := FMMFile.MapData( StartOfs+4, FTDSEntries^[i].sseSize-4 );
            while ProcCount > 0 do
              begin
                if (Longword(Address) >= PtrDimProc32^.dwStart) and (Longword(Address) <= PtrDimProc32^.dwEnd) then
                  begin
                    Result := PtrDimProc32^.dwNameIndex;
                    Exit;
                  end;
                Inc( PtrDimProc32 );
              end;

            FMMFile.UnMapData;
          end;
      end;
end;

function TModuleDebugInfo.GetProcName( Address: Pointer; var ModuleNameIndex,ProcNameIndex: Integer ): Boolean;
var
  i,n: Integer;
  MH: TsstModuleHeader;
  MS: TsstModuleSegInfo;
  EntryOfs: Longword;
begin
  Result          := False;
  ModuleNameIndex := -1;
  ProcNameIndex   := -1;

  if not FLoaded then Exit;

  // 1. Find Module with Address

  for i := 0 to FTDSSubdir.sshRecordCount-1 do
    if FTDSEntries^[i].sseType = SST_MODULE then
      begin
        // check if sstModule in allowed demo array of modules
      {$IFDEF EXCMAGIC_DEMO}
        EntryOfs := 0;
        for n := 0 to FsstSrcCount-1 do
          if FsstSrcTable[n].sseModIndex = FTDSEntries^[i].sseModIndex then
            begin
              EntryOfs := EntryOffset( i );
              Break;
            end;
        if EntryOfs = 0 then Continue;
      {$ELSE}
        EntryOfs := EntryOffset( i );
      {$ENDIF}

        FMMFile.Seek( EntryOfs, soFromBeginning );
        FMMFile.Read( MH, SizeOf(MH) );
        for n := 0 to MH.SegCount-1 do
          begin
            FMMFile.Read( MS, SizeOf(MS) );
            if ((MS.Flags and 1) <> 0) and
               (LongWord(Address) >= MS.Start) and
               (LongWord(Address) <  MS.Start+MS.Size) then
                 begin
                   // 2. Find proc in AlignSymbol info for current module
                   Result := True;
                   ModuleNameIndex := MH.NameIndex;
                   ProcNameIndex   := FindProc( Address, FTDSEntries^[i].sseModIndex );
                   Exit;
                 end;
          end;
      end;
end;


{$HINTS OFF}
function TModuleDebugInfo.GetSourceLine( Address: Pointer; var SrcFileNameIndex,SrcLineNum: Integer ): Boolean;
var
  i: Longint;
  pSrcInfoOffsets: PLongArray;
  ww,w,u: Integer;
  pSrcModule,pSrcFileInfo,pLinesInfo: Pointer;
  SrcModuleSize: Dword;
  SrcModuleOffset,SrcFileInfoOffset,LinesInfoOffset: Dword;
  SrcInfoOffsetsSize: Longint;
  wSrcFiles,wSegments,wLineNumBlocks: Word;
  pSegRanges: PTDS_SegmentRangeArray;
  pSegNumbers: PWordArray;
  dwFilenameIndex: Longword;
  wLinesSegIdx,wLinesCount: Word;
  pLineNum: PWordArray;
  pLineOffset: PLongArray;
  pSrcBaseLines: PLongArray;
  pSrcStartEnds: PTDS_SegmentRangeArray;
  binLeft,binRight,binMid: Word;
begin
  Result      := False;
  SrcLineNum  := -1;
  SrcFileNameIndex := -1;

  if not FLoaded then Exit;

{$IFDEF EXCMAGIC_DEMO}
  for i := 0 to FsstSrcCount-1 do
      begin
        SrcModuleOffset := FTDSOffset + FsstSrcTable[i].sseOffset;
        SrcModuleSize   := FsstSrcTable[i].sseSize;
{$ELSE}
  for i := 0 to FTDSSubdir.sshRecordCount-1 do
    if FTDSEntries^[i].sseType = SST_SRCMODULE then
      begin
        SrcModuleOffset := EntryOffset(i);
        SrcModuleSize   := FTDSEntries^[i].sseSize;
{$ENDIF}
        FMMFile.Seek( SrcModuleOffset, soFromBeginning );

        pSrcModule      := FMMFile.MapData( SrcModuleOffset, SrcModuleSize );

        wSrcFiles       := PWord(pSrcModule)^;
        wSegments       := PWord(Longint(pSrcModule)+2)^;
        pSrcInfoOffsets := Pointer(Longint(pSrcModule)+4);
        pSegRanges      := Pointer(Longint(pSrcInfoOffsets) + wSrcFiles*4);
        pSegNumbers     := Pointer(Longint(pSegRanges) + wSegments*8);

        // find range with ADDRESS
        for ww := 0 to Integer(wSegments)-1 do
          if (Longword(Address) >= pSegRanges^[ww].sStart) and (Longword(Address) <= pSegRanges^[ww].sEnd) then
              for w := 0 to Integer(wSrcFiles)-1 do
                begin
                  // offset of source file info # w
                  pSrcFileInfo := Pointer( Longword(pSrcModule) + pSrcInfoOffsets^[w] );
                  wLineNumBlocks  := PWord(pSrcFileInfo)^;
                  // name index in my string list
                  dwFilenameIndex := PLongword( Longint(pSrcFileInfo) + 2 )^;

                  pSrcBaseLines   := Pointer( Longint(pSrcFileInfo) + 6 );
                  pSrcStartEnds   := Pointer( Longint(pSrcBaseLines) + wLineNumBlocks * 4 );

                  // find range with ADDRESS
                  for u := 0 to Integer(wLineNumBlocks)-1 do
                    if (Longword(Address) >= pSrcStartEnds^[u].sStart) and (Longword(Address) <= pSrcStartEnds^[u].sEnd) then
                      begin
                        pLinesInfo   := Pointer( Longword(pSrcModule) + pSrcBaseLines^[u] );
                        wLinesSegIdx := PWord(pLinesInfo)^;
                        wLinesCount  := PWord(Longint(pLinesInfo)+2)^;

                        pLineOffset  := Pointer(Longint(pLinesInfo)+4);
                        pLineNum     := Pointer(Longint(pLineOffset)+4*wLinesCount);

                        // binsearch

                        binLeft  := 0;
                        binRight := wLinesCount-1;
                        binMid   := 0;
                        while binLeft < binRight-1 do
                          begin
                            binMid := (binLeft + binRight) div 2;
                            if Longword(Address) >= pLineOffset^[binMid] then
                              binLeft := binMid
                            else
                              binRight := binMid;
                          end;

                        // now Address in [binLeft..binRight[, so get binLeft as linenum index

                        SrcLineNum := Longword( pLineNum^[binLeft] );
                        SrcFileNameIndex := dwFilenameIndex;
                        Result := True;
                        // unmap data
                        FMMFile.UnMapData;
                        Exit;
                      end;
                end;
        FMMFile.UnMapData;
      end;
end;
{$HINTS ON}

function  TModuleDebugInfo.SourceLine( Address: Pointer; var SrcFileNameIndex,SrcLineNum: Integer ): Boolean;
begin
  Result := GetSourceLine( GetConvertedAddress(Address), SrcFileNameIndex, SrcLineNum );
end;

function  TModuleDebugInfo.ProcName( Address: Pointer; var ModuleNameIndex,ProcNameIndex: Integer ): Boolean;
begin
  Result := GetProcName( GetConvertedAddress(Address), ModuleNameIndex, ProcNameIndex );
end;

function TModuleDebugInfo.LoadTDS2: Boolean;
var
  D: Longword;
begin
  Result := False;
  try
    FMMFile.Seek( -8, soFromEnd );
    D := FMMFile.ReadDword;
    if (D = FB09_SIGNATURE) or (D = FB0A_SIGNATURE) then
      begin
        FTDSTotalSize := FMMFile.ReadDword;
        FMMFile.Seek( -FTDSTotalSize, soFromEnd );
        D := FMMFile.ReadDword;
        if (D = FB09_SIGNATURE) or (D = FB0A_SIGNATURE) then
          begin
            FSignature   := D;
            FTDSOffset   := FMMFile.Position - 4;
            FTDSDataSize := FMMFile.ReadDword;
            // read Subsection directory header
            FMMFile.Seek( FTDSOffset + FTDSDataSize, soFromBeginning );
            FMMFile.Read( FTDSSubdir, SizeOf(FTDSSubdir) );
            // check Subsection Header
            Result := (FTDSSubdir.sshHeaderSize = $0010) and
                      (FTDSSubdir.sshRecordSize = $000C);
            if Result then
              begin
                // alloc and read Subsection entries
                GetMem( FTDSEntries, SizeOf(TDS_SubsectionEntry) * FTDSSubdir.sshRecordCount );
                FMMFile.Read( FTDSEntries^, SizeOf(TDS_SubsectionEntry) * FTDSSubdir.sshRecordCount );
                // Create array with Names offsets
                Result  := Result and CreateNamesArray;
                FLoaded := True;
                {$IFDEF EXCMAGIC_DEBUG}
                  DebugFmt( 'Loaded. (Result %d)', [Ord(Result)] );
                {$ENDIF}
                FMMFile.UnMapData;
              end;
          end;
      end;
  except
  end;
end;

function TModuleDebugInfo.LoadTDS( AFileName: String ): Boolean;
begin
  Result := False;
  UnLoadDebugInfo;

  {$IFDEF EXCMAGIC_DEBUG}
    DebugMsg( 'Looking for debug-info in ' + AFileName );
  {$ENDIF}

  if FileExists(AFileName) then
    try
      FMMFile := TMMFileStream.Create( AFileName, fmOpenRead + fmShareDenyNone, 'ExcMagic.'+AFileName );
      try
        Result := LoadTDS2;
      except
        FreeAndNil( FMMFile );
        FLoaded := False;
      end;
    except
      {$IFDEF EXCMAGIC_DEBUG}
        DebugMsg( '  Error opening ' + AFileName );
      {$ENDIF}
      Exit;
    end;
end;

function TModuleDebugInfo.LoadDebugInfo( FileName: String ): Boolean;
begin
  UnLoadDebugInfo;
  Result := True;
  if not LoadTDS(FileName) then
    if not LoadTDS( ChangeFileExt(FileName,'.TDS') ) then
      if not LoadTDS( ChangeFileExt(FileName,'.DSI') ) then Result := False;
end;

procedure TModuleDebugInfo.UnLoadDebugInfo;
begin
  FreeAndNil( FMMFile );
  if Assigned(FTDSEntries)  then FreeMem( FTDSEntries, SizeOf(TDS_SubsectionEntry) * FTDSSubdir.sshRecordCount );
  if Assigned(FNameIndexes) then FreeMem( FNameIndexes, FNamesCount * SizeOf(Pointer) );
  FTDSEntries  := nil;
  FNameIndexes := nil;
  FNamesCount  := 0;
  FLoaded      := False;
end;

// ---------------------------------------------------------------------------

procedure TExcCallStack.Clear;
var
  i: Integer;
begin
  for i := 0 to Count-1 do FreeMem( Items[i] );
  inherited;
end;

function  TExcCallStack.GetItem(Index: Integer): PCallStackItem;
begin
  Result := PCallStackItem( inherited Items[Index] );
end;

{
  Address: return address (i.e. address of instruction after CALL)
  RegEBP : current value of EBP (stack frame pointer)

  function _must_ retrieve args from stack (so use stdcall) !!!
}
procedure TExcCallStack.GenerateFromAddr(
      Address: Pointer; RegEBP: LongWord; MaxSize: Integer; SuppressRecursion: Boolean ); stdcall;
type
  PPointer = ^Pointer;
var
  CurModule: TModuleDebugInfo;
  ConvertedAddress,PrevAddress: Pointer;
  pCallItem: PCallStackItem;
begin
  Clear;
  PrevAddress := nil;

  {$IFDEF EXCMAGIC_DEBUG}
    DebugFmt( 'Generating CallStack. Addr %p, EBP %.08X', [Address,RegEBP] );
  {$ENDIF}

  while True do
    begin
      if IsBadReadPtr( Address, 4 ) then Break;

      CurModule := ExceptionHook.FindDebugModule( Address );
      if CurModule = nil then Break;
      // if not CurModule.Loaded then Break;

      {$IFDEF EXCMAGIC_DEBUG}
        DebugFmt( '  call from %p', [Address] );
      {$ENDIF}

      ConvertedAddress := CurModule.GetConvertedAddress( Address );

      if (Address = PrevAddress) and SuppressRecursion then
        begin
          Inc( Items[Count-1]^.NestingLevel );
        end
      else
        begin
          GetMem( pCallItem, SizeOf(pCallItem^) );
          with pCallItem^ do
            begin
              DebugModule := CurModule;
              CallAddress := Address;
              // get source line with CALL instruction
              // so use ReturnAddress - 1 !!!
              CurModule.GetSourceLine( Pointer(Longword(ConvertedAddress)-1), FileNameIndex, FileLineNumber );
              CurModule.GetProcName( ConvertedAddress, ModuleNameIndex, ProcNameIndex );
              NestingLevel := 1;
            end;
          Add( pCallItem );
        end;
{
  standard stack frame:

  push ebp
  mov  ebp,esp
  [sub esp,xxx]
  ...
  mov  esp,ebp
  ret [xxx]

      -------------
  +4  | ret.addr. |
      -------------
  +0  | EBP_old   | <---- EBP_new
      -------------
}
      if Count >= MaxSize then Break;
      if IsBadReadPtr( PPointer(PChar(RegEBP)+4), 4 ) then Break;
      if IsBadReadPtr( Pointer(RegEBP), 4 ) then Break;

      PrevAddress     := Address;
      Address         := PPointer(PChar(RegEBP)+4)^;
      Pointer(RegEBP) := PPointer(RegEBP)^;

      // if (LongWord(Address) and $80000000) <> 0 then Break; // from OS (win9x)
    end;
end;

{
  Generates call-stack for location from it was called.
  this function must have a stack frame (stdcall) !!!
}
procedure TExcCallStack.Generate( MaxSize: Integer; SuppressRecursion: Boolean ); stdcall;
var
  Address: Pointer;
  RegEBP: LongWord;
begin
  asm
    mov     eax,[ebp]     // get caller EBP
    mov     RegEBP,eax
    mov     eax,[ebp+4]   // return address
    mov     Address,eax
  end;
  GenerateFromAddr( Address, RegEBP, MaxSize, SuppressRecursion );
end;

procedure TExcCallStack.Dump( StrList: TStringList );
var
  S: String;
  i: Integer;
begin
  StrList.Clear;
  StrList.Add( 'Call stack:' );
  for i := 0 to Count-1 do
    with Items[i]^ do
      begin
        if NestingLevel > 1 then
          StrList.Add( Format( 'Recursive call (%d times):', [NestingLevel] ) );

        S := Format( ':%p [%s]', [CallAddress, ExtractFileName(DebugModule.ModuleName) ] );

        if ProcNameIndex <> -1 then
          S := S + Format( ' %s', [ ExceptionHook.UnMangle(DebugModule.GetName(ProcNameIndex), DebugModule.IsDelphiModule) ] );

        if FileLineNumber <> -1 then
          S := S + Format( ' (%s, line %d)', [ ExtractFileName(DebugModule.Names[FileNameIndex]),FileLineNumber ] );

        StrList.Add( S );
      end;
end;


// Dialog Procedure ----------------------------------------------------------

type
  TExceptionDlgParams = record
    Title:     PChar;
    Text:      PChar;
    CustomTab: PChar; // title of Additional Custom Tab
  end;
  PExceptionDlgParams = ^TExceptionDlgParams;

  TDialogInstanceData = record
    DlgFont: HFont;
    DlgIcon: HIcon;
    Details: Boolean;
  end;
  PDialogInstanceData = ^TDialogInstanceData;

{$IFDEF EXCMAGIC_GUI}
function TDSDialogProc( hwndDlg: HWND; uMsg: Word; wParam: Word; lParam: Longint ): LongBool; stdcall;
const
  DLG_OK        = 1000;
  DLG_DETAILS   = 1001;
  DLG_TERMINATE = 1002;
  DLG_TEXT      = 2000;
  DLG_STACK     = 3000;
  DLG_CONTEXT   = 4000;
  DLG_CUSTOM    = 5000;
  DLG_TABS      = 8000;
  DLG_ICON      = 9000;
const
  DetailTexts: array[Boolean] of PChar = ( 'Details >>', 'Compact <<' );
  Heights:  array[Boolean] of Integer = ( 500, 500 );
  Width:    Integer = 500;
var
  TC: TC_ITEM;
  R1,R2: TRect;
  CloseDlg: Boolean;

  procedure AllocDialogInstanceData;
  var
    pData: PDialogInstanceData;
  begin
    GetMem( pData, SizeOf(TDialogInstanceData) );
    FillChar( pData^, SizeOf(TDialogInstanceData), 0 );
    SetWindowLong( hwndDlg, GWL_USERDATA, Integer(pData) );
  end;

  procedure FreeDialogInstanceData;
  begin
    FreeMem( Pointer(GetWindowLong(hwndDlg,GWL_USERDATA)), SizeOf(TDialogInstanceData) );
  end;

  function DlgDataPtr: PDialogInstanceData; // pointer to dialog instance record
  begin
    Result := PDialogInstanceData( GetWindowLong( hwndDlg, GWL_USERDATA ) );
  end;

  procedure ShowTab( ActiveTabIndex: Integer );
  var
    i,N: Integer;
  begin
    // get total number of
    N := SendDlgItemMessage( hwndDlg, DLG_TABS, TCM_GETITEMCOUNT, 0, 0 );
    // hide all text controls attached to tabs
    ShowWindow( GetDlgItem(hwndDlg,DLG_STACK),   SW_HIDE );
    ShowWindow( GetDlgItem(hwndDlg,DLG_CONTEXT), SW_HIDE );
    ShowWindow( GetDlgItem(hwndDlg,DLG_CUSTOM),  SW_HIDE );
    // show text control attached to active tab
    for i := 0 to N-1 do
      begin
        TC.mask := TCIF_PARAM;
        SendDlgItemMessage( hwndDlg, DLG_TABS, TCM_GETITEM, i, Longint(@TC) );
        if i = ActiveTabIndex then
          begin
            ShowWindow( GetDlgItem(hwndDlg,TC.lParam), SW_SHOW );
            SendDlgItemMessage( hwndDlg, TC.lParam, EM_SETSEL, -1, 0 ); // remove selection
            SendDlgItemMessage( hwndDlg, DLG_TABS, TCM_SETCURSEL, ActiveTabIndex, 0 );
          end
      end;
    // set focus to 'OK' button
    SetFocus( GetDlgItem(hwndDlg, DLG_OK) );
  end;

  procedure InsertTab( Name: String; ID: Integer; Text: PChar );
  begin
    ZeroMemory( @TC, SizeOf(TC) );
    TC.mask    := TCIF_TEXT or TCIF_PARAM;
    TC.pszText := PChar(Name);
    TC.lParam  := ID;
    SendDlgItemMessage( hwndDlg, DLG_TABS, TCM_INSERTITEM, 0, Longint(@TC) );
    SendDlgItemMessage( hwndDlg, ID, WM_SETTEXT, 0, Longint(Text) );
    SendDlgItemMessage( hwndDlg, ID, WM_SETFONT, DlgDataPtr^.DlgFont, 0 );
  end;

  procedure ShowDetails;
  begin
    SetWindowPos( hwndDlg, 0, 0, 0, Width, Heights[DlgDataPtr^.Details], SWP_NOMOVE + SWP_NOOWNERZORDER );
    SetDlgItemText( hwndDlg, DLG_DETAILS, DetailTexts[DlgDataPtr^.Details] );
  end;

begin
  // default result
  Result := False;
  case uMsg of
    WM_COMMAND:
        case wParam of
          DLG_OK:
            begin
              EndDialog( hwndDlg, 0 );
              Result := True;
            end;
          DLG_DETAILS:
            begin
              DlgDataPtr^.Details := not DlgDataPtr^.Details;
              ShowDetails;
            end;
          DLG_TERMINATE:
            begin
              if Assigned(ExceptionHook.FOnTerminate) then
                begin
                  CloseDlg := True;
                  ExceptionHook.FOnTerminate( CloseDlg );
                  if CloseDlg then
                    begin
                      EndDialog( hwndDlg, 0 );
                      Result := True;
                    end;
                end
              else
                TerminateProcess( GetCurrentProcess, 0 );
            end;
        end;

    WM_NOTIFY:
        if wParam = DLG_TABS then
          if PNMHDR(lParam)^.code = TCN_SELCHANGE then
            ShowTab( SendDlgItemMessage( hwndDlg, DLG_TABS, TCM_GETCURSEL, 0, 0 ) );

    WM_DESTROY:
        begin
          with DlgDataPtr^ do
            begin
              if DlgFont <> 0 then DeleteObject( DlgFont );
              if DlgIcon <> 0 then DeleteObject( DlgIcon );
            end;
          FreeDialogInstanceData;
          Result := False;
        end;

    WM_INITDIALOG:
        begin
          // allocate record with unique data for each dialog instance
          AllocDialogInstanceData;

          // load custom icon
          if Assigned(ExceptionHook.Icon) then
            begin
              // default Windows icon ?
              if Dword(ExceptionHook.Icon) and $FFFF0000 = 0 then
                DlgDataPtr^.DlgIcon := LoadIcon( 0, ExceptionHook.Icon )
              else
                DlgDataPtr^.DlgIcon := LoadImage( hInstance, ExceptionHook.Icon, IMAGE_ICON, 0, 0, 0 );
              SendDlgItemMessage( hwndDlg,
                                  DLG_ICON,
                                  STM_SETIMAGE,
                                  IMAGE_ICON,
                                  DlgDataPtr^.DlgIcon );
            end;
          // create & set fixed font
          DlgDataPtr^.DlgFont := CreateFont( 14, 0, 0, 0,
                                  FW_NORMAL, 0, 0, 0,
                                  DEFAULT_CHARSET,
                                  OUT_DEFAULT_PRECIS,
                                  CLIP_DEFAULT_PRECIS,
                                  DEFAULT_QUALITY,
                                  FIXED_PITCH,
                                  'Courier' );
          // what is initial state of dialog ?
          DlgDataPtr^.Details := excDlgDetailed in ExceptionHook.Options;

          // Check options. If no tabs - disable DETAILS button
          if [] = ExceptionHook.Options * [excDlgCallStack,excDlgRegisters,excDlgCustomInfo] then
            begin
              //EnableWindow( GetDlgItem(hwndDlg,DLG_DETAILS), False );
              ShowWindow( GetDlgItem(hwndDlg,DLG_DETAILS), SW_HIDE );
              DlgDataPtr^.Details := False;
            end
          else
            begin  // Add tabs
              if excDlgCustomInfo in ExceptionHook.Options then
                InsertTab( PExceptionDlgParams(lParam).CustomTab,
                           DLG_CUSTOM, PChar(ExceptionHook.FCustomInfoStrings.Text) );
              if excDlgRegisters in ExceptionHook.Options then
                InsertTab( 'Registers',  DLG_CONTEXT, PChar(ExceptionHook.FContextStrings.Text) );
              if excDlgCallStack in ExceptionHook.Options then
                InsertTab( 'Call Stack', DLG_STACK, PChar(ExceptionHook.FCallStackStrings.Text) );
              ShowTab( 0 );
            end;

          // Get MIN & MAX heights of dialog
          GetWindowRect( hwndDlg, R1 );
          Heights[True] := R1.Bottom - R1.Top;
          Width := R1.Right - R1.Left;
          GetWindowRect( GetDlgItem(hwndDlg, DLG_TABS), R2 );
          Heights[False] := R2.Top - R1.Top;

          // show/hide TERMINATE button
          if excDlgTerminate in ExceptionHook.Options then
            ShowWindow( GetDlgItem(hwndDlg,DLG_TERMINATE), SW_SHOW )
          else
            ShowWindow( GetDlgItem(hwndDlg,DLG_TERMINATE), SW_HIDE );

          // lParam = ptr to TExceptionDlgParams record
          SetWindowText( hwndDlg, PExceptionDlgParams(lParam).Title );
          SendDlgItemMessage( hwndDlg, DLG_TEXT, WM_SETTEXT, 0,
                              Longint(PExceptionDlgParams(lParam).Text) );
          ShowDetails;

          Result := False;
        end;
  end;
end;
{$ENDIF}

{$STACKFRAMES ON}
function TDSExceptionErrorMessage(ExceptObject: TObject; Address: Pointer;
                                  Buffer: PChar; Size: Integer): Integer; register;
const
  DEFAULT_EXCEPTION_MSG: PChar =
    'Exception ''%s'' in module %s at %p'#13#10 +
    '%s'#13#10#13#10 +
    'Source file: %s, Line %s';
  UNKNOWN: String = 'UNKNOWN';
var
  MsgSrcFile: String;
  MsgSrcLine: String;
  MsgInfo: TExceptionMessageInfo;

begin
{$IFDEF EXCMAGIC_DEBUG}
    DebugMsg( '--> SysUtils.ExceptionErrorMessage' );
{$ENDIF}

  try
    EnterCriticalSection(ExcMagicLock);
    { make local copy of _ExcMsgInfo. workaround for D3,D4 compiler bug }
    MsgInfo := _ExcMsgInfo;

    with MsgInfo do
      begin
        miDebugModule := ExceptionHook.FindDebugModule( Address );
        // if no module contains this address then use main module (always exists)
        if miDebugModule = nil then
          miDebugModule := ExceptionHook.FindDebugModule( Addr(TDSExceptionErrorMessage) );

        miDebugModule.GetModuleName( Address, miModuleName, SizeOf(miModuleName) );
        miVirtualAddress := Address;
        miModuleAddress  := miDebugModule.GetConvertedAddress( Address );

        miMessage := '';
        if ExceptObject is Exception then
          miMessage := Exception(ExceptObject).Message;

        // ATTENTION ! Decrement Address by 1 because initial Address points to
        // instruction just AFTER the one that raised exception
        miDebugModule.GetSourceLine( Pointer(Longword(miModuleAddress)-1), miSrcNameIndex, miSrcLineNum );
        miDebugModule.GetProcName( miModuleAddress, miModuleNameIndex, miProcNameIndex );

        if miSrcNameIndex <> -1 then MsgSrcFile := ExtractFileName(miDebugModule.Names[miSrcNameIndex])
                                else MsgSrcFile := UNKNOWN;
        if miSrcLineNum   <> -1 then MsgSrcLine := IntToStr(miSrcLineNum)
                                else MsgSrcLine := UNKNOWN;

        // do not load from resource - it's failed when compiled with runtime packages
        //LoadString( FindResourceHInstance(HInstance), PResStringRec(@SException).Identifier, Format, SizeOf(Format));
        //LoadString( FindHInstance(@SException), PResStringRec(@SException).Identifier, Fmt, SizeOf(Fmt));
        StrLFmt( Buffer, Size, DEFAULT_EXCEPTION_MSG,
          [ ExceptObject.ClassName,
            miModuleName,
            miModuleAddress,
            miMessage,
            MsgSrcFile,
            MsgSrcLine ] );

        _ExcMsgInfo := MsgInfo; // copy local var back

        if Assigned(ExceptionHook.FOnExceptionMsg) then
          ExceptionHook.FOnExceptionMsg( ExceptObject, MsgInfo, Buffer, Size );

        Result := StrLen(Buffer);
      end;
  finally
    LeaveCriticalSection(ExcMagicLock);
  end;
end;

procedure TDSShowException( ExceptObject: TObject; Address: Pointer ); register;
var
  {$IFDEF EXCMAGIC_GUI}
    DlgParams: TExceptionDlgParams;
  {$ENDIF}
  Title:   array[0..63] of Char;
  Buffer:  array[0..1023] of Char;
  BufLen:  Integer;
begin
  try
    EnterCriticalSection(ExcMagicLock);

  {$IFDEF EXCMAGIC_DEBUG}
    DebugMsg( '--> SysUtils.ShowException' );
  {$ENDIF}

    Buffer[0] := #0;
    BufLen := TDSExceptionErrorMessage(ExceptObject, Address, Buffer, SizeOf(Buffer) );

    ExceptionHook.DumpAll;

    ExceptionHook.FCustomInfoStrings.Clear;
    if Assigned(ExceptionHook.FOnCustomInfo) then
      ExceptionHook.FOnCustomInfo( ExceptionHook.FCustomInfoStrings );

    if not ExceptionHook.LogHandled then
      ExceptionHook.LogExceptionData( Buffer, BufLen );

    if excShowDialog in ExceptionHook.FOptions then
      begin
        //LoadString(FindResourceHInstance(HInstance), PResStringRec(@SExceptTitle).Identifier, Title, SizeOf(Title));
        LoadString( FindHInstance(@SExceptTitle), PResStringRec(@SExceptTitle).Identifier, Title, SizeOf(Title));

        if Assigned(ExceptionHook.FOnExceptionShow) then
          ExceptionHook.FOnExceptionShow( String(Title), String(Buffer),
                                          ExceptionHook.FCallStackStrings,
                                          ExceptionHook.FContextStrings,
                                          ExceptionHook.FCustomInfoStrings )
        else
          begin
          {$IFDEF EXCMAGIC_CON}
            WriteLn( Buffer  );
            WriteLn( ExceptionHook.FCallStackStrings.Text );
            WriteLn( ExceptionHook.FContextStrings.Text );
          {$ENDIF}
          {$IFDEF EXCMAGIC_GUI}
            DlgParams.Title     := Title;
            DlgParams.Text      := Buffer;
            DlgParams.CustomTab := PChar(ExceptionHook.CustomTab);
            //DlgParams.Stack   := PChar(ExceptionHook.FCallStackStrings.Text);
            //DlgParams.Context := PChar(ExceptionHook.FContextStrings.Text);
            DialogBoxParam( HInstance, MakeIntResource(2000), Application.Handle,  @TDSDialogProc, Longint(@DlgParams) );
          {$ENDIF}
          end;
      end;

    // clear context pointer for next exception !!!!
    // PtrContext := nil;

  finally
    LeaveCriticalSection(ExcMagicLock);
  end;
end;
{$STACKFRAMES OFF}

procedure TDSAppShowException(E: Exception); register;
begin
  try
    EnterCriticalSection(ExcMagicLock);
  {$IFDEF EXCMAGIC_DEBUG}
    DebugMsg( '--> Application.ShowException' );
  {$ENDIF}
    TDSShowException( ExceptObject, ExceptAddr );
  finally
    LeaveCriticalSection(ExcMagicLock);
  end;
end;

// ---------------------------------------------------------------------------

constructor TExcMagic.Create;
begin
  if Assigned(ExceptionHook) then
    raise EExcMagicError.Create( SOnlyOneAllowed );

  inherited Create;

  FEnabled        := False;
  FLogFile        := ExtractFilePath(ParamStr(0)) + SDefaultLog;
  FLogEnabled     := True;
  FIcon           := nil;
  FLogHandled     := False;
  FOptions        := [ excDlgCallStack, excDlgRegisters, excShowDialog ];
  FMaxCallStack      := 100;
  FSuppressRecursion := True;

  FCustomTab      := SDefaultCustom;

  FCallStack         := TExcCallStack.Create;
  FCallStackStrings  := TStringList.Create;
  FContextStrings    := TStringList.Create;
  FCustomInfoStrings := TStringList.Create;

  FModules := TList.Create;
  // Add main application to modules list
  FModules.Add( TModuleDebugInfo.Create(GetModuleName(HInstance),HInstance) );
end;

destructor  TExcMagic.Destroy;
begin
  while FModules.Count > 0 do
    begin
      {$IFDEF EXCMAGIC_DEBUG}
        with TModuleDebugInfo(FModules.Items[0]) do
          DebugFmt( '  module :%.8X %s', [FInstance,FName] );
      {$ENDIF}
      TModuleDebugInfo(FModules.Items[0]).Free;
      FModules.Delete(0);
    end;
  FModules.Free;

  FCustomInfoStrings.Free;
  FCallStackStrings.Free;
  FContextStrings.Free;
  FCallStack.Free;

  inherited Destroy;
end;

function  TExcMagic.GetExcMagicAbout: String;
begin
  Result := ExcMagicAbout;
end;

function  TExcMagic.GetExcMagicVersion: String;
begin
  Result := ExcMagicVersion;
end;

function TExcMagic.GetContext: TContext;
begin
  Result := _ExcContext;
end;

function TExcMagic.GetExceptionRec: TExceptionRecord;
begin
  Result := _ExcRecord;
end;

function TExcMagic.GetExceptionInfo: TExceptionMessageInfo;
begin
  Result := _ExcMsgInfo;
end;

procedure TExcMagic.SetEnabled( Value: Boolean );
begin
  if FEnabled <> Value then
    begin
      FEnabled := Value;

      if Value then
        begin
          WriteMem( ErrorMessageAddr, @NewBytesMessage, SizeOf(NewBytesMessage) );
          WriteMem( ShowErrorAddr, @NewBytesShow, SizeOf(NewBytesShow) );
          WriteMem( ExceptionHandlerAddr, @NewBytesExcHandler, SizeOf(NewBytesExcHandler) );
          WriteMem( HandleAnyExceptAddr, @NewBytesHandleAny, SizeOf(NewBytesHandleAny) );
          WriteMem( HandleOnExceptAddr,  @NewBytesHandleOn,  SizeOf(NewBytesHandleOn)  );
          WriteMem( HandleAutoExceptAddr, @NewBytesHandleAuto, SizeOf(NewBytesHandleAuto) );
          {$IFDEF EXCMAGIC_GUI}
          WriteMem( AppShowExceptionAddr, @NewBytesAppShow, SizeOf(NewBytesAppShow) );
          {$ENDIF}

          //SwitchMagicHandler( True );
        end
      else
        begin
          WriteMem( ErrorMessageAddr, @OldBytesMessage, SizeOf(OldBytesMessage) );
          WriteMem( ShowErrorAddr, @OldBytesShow, SizeOf(OldBytesShow) );
          WriteMem( ExceptionHandlerAddr, @OldBytesExcHandler, SizeOf(OldBytesExcHandler) );
          WriteMem( HandleAnyExceptAddr, @OldBytesHandleAny, SizeOf(OldBytesHandleAny) );
          WriteMem( HandleOnExceptAddr,  @OldBytesHandleOn,  SizeOf(OldBytesHandleOn)  );
          WriteMem( HandleAutoExceptAddr, @OldBytesHandleAuto, SizeOf(OldBytesHandleAuto) );
          {$IFDEF EXCMAGIC_GUI}
          WriteMem( AppShowExceptionAddr, @OldBytesAppShow, SizeOf(OldBytesAppShow) );
          {$ENDIF}

          //SwitchMagicHandler( False );
        end;
    end;
end;

function  TExcMagic.UnMangle( Source: String; IsDelphi: Boolean ): String;
var
  Dest: array[0..2048] of Char;
begin
  _UnMangle( PChar(Source), Dest, SizeOf(Dest), nil, nil, False, IsDelphi );
  Result := Dest;
end;

function  TExcMagic.FindDebugModule( Address: Pointer ): TModuleDebugInfo;
var
  i: Integer;
  Info: TMemoryBasicInformation;
  Temp: array[0..MAX_PATH] of Char;
begin
  Result := nil;
  for i := 0 to FModules.Count-1 do
    if TModuleDebugInfo(FModules.Items[i]).IsInCode( Address ) then
      begin
        Result := TModuleDebugInfo(FModules.Items[i]);
        Break;
      end;
  if Result = nil then // create new TModuleDebugInfo
    begin
      VirtualQuery(Address, Info, sizeof(Info));
      if (Info.State and MEM_COMMIT) <> 0 then
        begin
          // return NIL if module already in list
          for i := 0 to FModules.Count-1 do
            if TModuleDebugInfo(FModules.Items[i]).FInstance = THandle(Info.AllocationBase) then Exit;

          if GetModuleFilename(THandle(Info.AllocationBase), Temp, SizeOf(Temp)) <> 0 then
            begin
              Result := TModuleDebugInfo.Create( Temp, THandle(Info.AllocationBase) );
              FModules.Add( Result );
            end;
        end;
    end;
end;

function  TExcMagic.GetAddressSourceInfo( Address: Pointer;
                                        var ModuleDebugInfo: TModuleDebugInfo;
                                        var ModuleName: String;
                                        var FileName: String;
                                        var ProcName: String;
                                        var LineNumber: Integer ): Boolean;
var
  FileNameIndex,ModuleNameIndex,ProcNameIndex: Integer;
begin
  Result     := False;
  ModuleName := '';
  FileName   := '';
  ProcName   := '';
  LineNumber := -1;
  ModuleDebugInfo := FindDebugModule( Address );
  if ModuleDebugInfo <> nil then
    begin
      ModuleDebugInfo.SourceLine( Address, FileNameIndex, LineNumber );
      ModuleDebugInfo.ProcName( Address, ModuleNameIndex, ProcNameIndex );
      ModuleName := ModuleDebugInfo.Names[ModuleNameIndex];
      FileName   := ModuleDebugInfo.Names[FileNameIndex];
      ProcName   := UnMangle( ModuleDebugInfo.Names[ProcNameIndex], ModuleDebugInfo.IsDelphiModule );
      Result     := True;
    end;
end;

function  TExcMagic.GetSourceInfo( var ModuleDebugInfo: TModuleDebugInfo;
                                   var ModuleName: String;
                                   var FileName: String;
                                   var ProcName: String;
                                   var LineNumber: Integer ): Boolean; stdcall;
var
  Address: Pointer;
begin
  asm
        mov     eax,[ebp+4]     // get return address
        mov     Address,eax
  end;
  Result := GetAddressSourceInfo( Address, ModuleDebugInfo, ModuleName, FileName, ProcName, LineNumber );
end;

function  TExcMagic.GetAddressSourceInfoRec( Address: Pointer ): TSourceInfo;
begin
  GetAddressSourceInfo( Address,
              Result.ModuleDebugInfo,
              Result.ModuleName,
              Result.FileName,
              Result.ProcName,
              Result.LineNumber );
end;

function  TExcMagic.GetSourceInfoRec: TSourceInfo; stdcall;
var
  Address: Pointer;
begin
  asm
        mov     eax,[ebp+4]     // get return address
        mov     Address,eax
  end;
  GetAddressSourceInfo( Address,
              Result.ModuleDebugInfo,
              Result.ModuleName,
              Result.FileName,
              Result.ProcName,
              Result.LineNumber );
end;


procedure TExcMagic.DumpContext( StrList: TStringList );
var
  S: String;
  i: Integer;
  Code: array[0..15] of Byte;
  Stck: array[0..19] of LongWord;
begin
  StrList.Clear;
  with _ExcContext do
    begin
      StrList.Add( 'Registers:' );
      StrList.Add( Format( 'EAX = %.8X  CS = %.4X  EIP = %.8X  Flags = %.8X', [eax, segcs, eip, eflags] ) );
      StrList.Add( Format( 'EBX = %.8X  SS = %.4X  ESP = %.8X    EBP = %.8X', [ebx, segss, esp, ebp] ) );
      StrList.Add( Format( 'ECX = %.8X  DS = %.4X  ESI = %.8X    FS  = %.4X', [ecx, segds, esi, segfs] ) );
      StrList.Add( Format( 'EDX = %.8X  ES = %.4X  EDI = %.8X    GS  = %.4X', [edx, seges, edi, seggs] ) );

      // dump 16 bytes of code
      if ReadMem( Pointer(eip), @Code, SizeOf(Code) ) then
        begin
          StrList.Add( 'Code at CS:EIP' );
          S := '';
          for i := 0 to 15 do S := S + Format( '%.2X ', [ Code[i] ] );
          StrList.Add( S );
        end;

      // dump 16 dwords from stack
      if ReadMem( Pointer(esp), @Stck, SizeOf(Stck) div SizeOf(Stck[0]) ) then
        begin
          StrList.Add( 'Stack:' );
          i := 0;
          while i < 20 do
            begin
              StrList.Add( Format( '%.8X %.8X %.8X %.8X %.8X ',
                      [ Stck[i],Stck[i+1],Stck[i+2],Stck[i+3],Stck[i+4] ]) );
              Inc(i,5);
            end;
        end;
    end;
end;

const
  cDelphiException    = $0EEDFADE;
  cDelphiReRaise      = $0EEDFADF;
  cDelphiExcept       = $0EEDFAE0;
  cDelphiFinally      = $0EEDFAE1;
  cDelphiTerminate    = $0EEDFAE2;
  cDelphiUnhandled    = $0EEDFAE3;
  cNonDelphiException = $0EEDFAE4;
  cDelphiExitFinally  = $0EEDFAE5;
  cCppException       = $0EEFFACE;
  //
  cDelphiExcMask      = $0EEDFA00;

procedure TExcMagic.DumpAll;
var
  E_EBP: Longword;
  E_Addr: Pointer;
begin
    if (_ExcRecord.ExceptionCode and cDelphiExcMask) = cDelphiExcMask then
      begin
        E_EBP  := _ExcRecord.ExceptEBP;
        E_Addr := _ExcRecord.ExceptAddr;
      end
    else { OS exception }
      begin
        E_EBP  := _ExcContext.EBP;
        E_Addr := _ExcRecord.ExceptionAddress;
      end;
    CallStack.GenerateFromAddr( E_Addr, E_EBP, FMaxCallStack, FSuppressRecursion );
    CallStack.Dump( FCallStackStrings );
    DumpContext( FContextStrings );
end;

const
  CRLF: Word = $0A0D;
  HDRLINE = '---------------------------';

procedure TExcMagic.Log( Text: PChar; WithHeader: Boolean );
var
  HLog: THandle;
  S: String;
  Written: DWORD;
begin
  if FLogEnabled and (FLogFile <> '') then
    begin
      HLog := CreateFile( PChar(FLogFile), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
      if HLog <> INVALID_HANDLE_VALUE then
        begin
          SetFilePointer( HLog, 0, nil, FILE_END );
          if WithHeader then
            begin
              S := HDRLINE + LocalTimeStr + HDRLINE + #13#10;
              WriteFile( HLog, S[1], Length(S), Written, nil);
            end;
          WriteFile( HLog, Text^, StrLen(Text), Written, nil);
          WriteFile( HLog, CRLF, SizeOf(CRLF), Written, nil);
          CloseHandle( HLog );
        end;
    end;
end;

procedure TExcMagic.LogExceptionData( Buffer: PChar; BufLen: Integer );
var
  HLog: THandle;
  S: String;
  Written: DWORD;
begin
  if FLogEnabled then
    if Assigned(FOnExceptionLog) then
      FOnExceptionLog( Buffer, BufLen, FCallStackStrings, FContextStrings, FCustomInfoStrings )
    else
      if FLogFile <> '' then
        begin
          HLog := CreateFile( PChar(FLogFile), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
          if HLog <> INVALID_HANDLE_VALUE then
            begin
              SetFilePointer( HLog, 0, nil, FILE_END );
              S := HDRLINE + LocalTimeStr + HDRLINE + #13#10;
              WriteFile( HLog, S[1], Length(S), Written, nil);
              WriteFile( HLog, Buffer^, BufLen, Written, nil);
              WriteFile( HLog, CRLF, SizeOf(CRLF), Written, nil);
              // write call stack
              WriteFile( HLog, CRLF, SizeOf(CRLF), Written, nil);
              WriteFile( HLog, PChar(FCallStackStrings.Text)^,
                         Length(FCallStackStrings.Text), Written, nil );
              // write registers
              WriteFile( HLog, CRLF, SizeOf(CRLF), Written, nil);
              WriteFile( HLog, PChar(FContextStrings.Text)^,
                         Length(FContextStrings.Text), Written, nil );
              // write custom info
              WriteFile( HLog, CRLF, SizeOf(CRLF), Written, nil);
              WriteFile( HLog, PChar(FCustomInfoStrings.Text)^,
                         Length(FCustomInfoStrings.Text), Written, nil );
              WriteFile( HLog, CRLF, SizeOf(CRLF), Written, nil);
              WriteFile( HLog, CRLF, SizeOf(CRLF), Written, nil);
              CloseHandle( HLog );
            end;
        end;
end;

procedure TExcMagic.LogException;
type
  TExceptClsProc = function( P: PExceptionRecord ): ExceptClass;
  TExceptObjProc = function( P: PExceptionRecord ): Exception;
var
  Buffer: array[0..1024] of Char;
  BufLen: Integer;
//  ExcCls: ExceptClass;
  ExcObj: Exception;
  E_Addr: Pointer;
(*
  TExceptionRecord = packed record
    ExceptionCode        : LongWord;
    ExceptionFlags       : LongWord;
    OuterException       : PExceptionRecord;
    ExceptionAddress     : Pointer;
    NumberParameters     : Longint;
    case {IsOsException:} Boolean of
    True:  (ExceptionInformation : array [0..14] of Longint);
    False: (
       ExceptAddr: Pointer;
       ExceptObject: Pointer;
       ExceptEBX: LongWord;
       ExceptESI: LongWord;
       ExceptEDI: LongWord;
       ExceptEBP: LongWord;
       ExceptESP: LongWord;
    );
*)
begin
  if FLogEnabled then
    begin
      (*
        ExcCls := TExceptClsProc(ExceptClsProc)( @ExcRecord );
        ExcObj := TExceptObjProc(ExceptObjProc)( @ExcRecord );
      *)

    {$IFDEF EXCMAGIC_DEBUG}
      DebugFmt( 'LogException'#13#10'  Main thread %X, Current thread %X',
                [ MainThreadID,GetCurrentThreadID ] );
    {$ENDIF}

      if (_ExcRecord.ExceptionCode and cDelphiExcMask) = cDelphiExcMask then
        begin
          ExcObj := _ExcRecord.ExceptObject;
          E_Addr := _ExcRecord.ExceptAddr;
        end
      else { OS exception }
        begin
          ExcObj := TExceptObjProc(ExceptObjProc)( @_ExcRecord );
          E_Addr := _ExcRecord.ExceptionAddress;
        end;

      BufLen := TDSExceptionErrorMessage( ExcObj, E_Addr, Buffer, SizeOf(Buffer));
      DumpAll;

      LogExceptionData( Buffer, BufLen );
    end;
end;

// -----------------------------------------------------------------------------

procedure CopyContextAndLog( pCont: PContext; pRec: PExceptionRecord ); stdcall;
begin
{$IFDEF EXCMAGIC_DEBUG}
  DebugFmt( #13#10'EXCEPTION %.8X at %p', [ pRec^.ExceptionCode,pRec^.ExceptionAddress ] );
{$ENDIF}
  _ExcContext := pCont^;
  _ExcRecord  := pRec^;
  if ExceptionHook.LogHandled then ExceptionHook.LogException;
end;

procedure MagicHandleAllExceptions; assembler;
asm
        { ->    [ESP   ] ret addr to KERNEL              }
        {       [ESP+ 4] ret addr to _Handle*Exception   }
        {       [ESP+ 8] excPtr: PExceptionRecord        }
        {       [ESP+12] errPtr: PExcFrame               }
        {       [ESP+16] ctxPtr: Pointer                 }
        {       [ESP+20] dspPtr: Pointer                 }

       pushad   { push 20h bytes }
       cld
       push    dword ptr [esp+ 8 + 20h ] { excPtr: PExceptionRecord }
       push    dword ptr [esp+16 + 24h ] { ptr to exception context }
       call    CopyContextAndLog
       popad

       mov     eax,[esp+8]     { first line of _Handle*Exception }
end;

(*
procedure       _RaiseExcept;
asm
        { ->    EAX     Pointer to exception object     }
        {       [ESP]   Error address           }

        POP     EDX

        PUSH    ESP
        PUSH    EBP
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        PUSH    EAX               { pass class argument           }
        PUSH    EDX               { pass address argument         }

        PUSH    ESP               { pass pointer to arguments             }
        PUSH    7                 { there are seven arguments               }
        PUSH    cNonContinuable   { we can't continue execution   }
        PUSH    cDelphiException  { our magic exception code              }
        PUSH    EDX               { pass the user's return address        }
        JMP     RaiseException
end;
*)

// -----------------------------------------------------------------------------

{ returns real address of proc/var if it is located in run-time package }
function RealAddr( RawAddr: Pointer ): Pointer;
type
  TJmpDLL = packed record
    OpCode: Word;
    Offset: LongWord;
  end;
var
  J: TJmpDLL;
  D: LongWord;
begin
  Result := nil;
  // read 6 bytes
  if ReadMem( RawAddr, @J, SizeOf(J) ) then
    if J.OpCode <> $25FF then // check if it's JMP to DLL and read real address of function
      Result := RawAddr
    else
      if ReadMem( Pointer(J.Offset), @D, SizeOf(D) ) then Result := Pointer(D);
end;

var
  MagicFrame: TExcFrame;
  PrevFrame,LastFrame: PExcFrame;

function MagicExceptionHandler( pRec: PExceptionRecord;
                                pFrame: PExcFrame;
                                pCtx: PContext;
                                pDispatcherContext: Pointer ): Integer; cdecl;
const
  cUnwinding          = 2;
  cUnwindingForExit   = 4;
  cUnwindInProgress   = cUnwinding or cUnwindingForExit;
  EXCEPTION_CONTINUE_SEARCH    = 0;
  EXCEPTION_EXECUTE_HANDLER    = 1;
  EXCEPTION_CONTINUE_EXECUTION = -1;
begin
  if (pRec^.ExceptionFlags and cUnwindInProgress) = 0 then
    begin
      _ExcContext := pCtx^;
      _ExcRecord  := pRec^;
    {$IFDEF EXCMAGIC_DEBUG}
      DebugFmt( 'Magic Handler: %.8X at %p', [ pRec^.ExceptionCode,pRec^.ExceptionAddress ] );
    {$ENDIF}
      Result := -1;
    end
  else
    Result := EXCEPTION_CONTINUE_SEARCH;
end;

function GetExceptionHandlerAddr: Pointer;
var
  FirstModule,Module: TModuleDebugInfo;
begin
  asm
    push  eax
    mov   eax,fs:[0] { first SEH handler in chain }
    mov   LastFrame,eax
    pop   eax
  end;
  //  find System._ExceptionHandler in SEH handlers chain
  //  (get _last_ handler from chain in "my" code)

{$IFDEF EXCMAGIC_DEBUG}
  DebugMsg( 'Tracing SEH chain' );
{$ENDIF}
  PrevFrame   := LastFrame;
  Result      := nil;
  FirstModule := ExceptionHook.FindDebugModule(LastFrame^.desc);
  while LastFrame <> Pointer($FFFFFFFF) do
    begin
      Module := ExceptionHook.FindDebugModule(LastFrame^.desc);
      {$IFDEF EXCMAGIC_DEBUG}
        DebugFmt( '  Frame %p, Handler %p in %s', [LastFrame,LastFrame^.desc,Module.FName] );
      {$ENDIF}
      if FirstModule <> Module then Break else Result := LastFrame^.desc;
      PrevFrame := LastFrame;
      LastFrame := LastFrame^.next;
    end;
end;

procedure SwitchMagicHandler( TurnON: Boolean );
begin
  case TurnON of
    True:
      begin
        //  now insert my handler before last one:
        //  -> PrevHandler -> LastHandler -> 0xFFFFFFFF
        //  -> PrevHandler -> MyHandler -> LastHandler -> 0xFFFFFFFF
        //  MyHandler returns ExceptionContinueSearch
        MagicFrame.desc := PrevFrame^.desc;
        MagicFrame.next := PrevFrame^.next;
        PrevFrame^.desc := @MagicExceptionHandler;
        PrevFrame^.next := @MagicFrame;
      end;
    False:
      begin
        PrevFrame^.desc := MagicFrame.desc;
        PrevFrame^.next := MagicFrame.next;
      end;
  end;
end;


function CheckHandlerCode( PHandler: Pointer ): Pointer;
const
  ORGCODE: array [0..10] of Byte = ( $8B,$44,$24,$04,$F7,$40,$04,$06,$00,$00,$00 );
var
  PtrCode: array [0..10] of Byte;
begin
  Result := nil;
  if ReadMem( PHandler, @PtrCode, SizeOf(PtrCode) ) then
    if CompareMem( @PtrCode, @ORGCODE, SizeOf(PtrCode) ) = 0 then
      Result := PHandler;
end;

// -----------------------------------------------------------------------------

{$L ExcHandle.obj}
function GetHandleAnyExceptionAddr : Pointer; external;
function GetHandleOnExceptionAddr  : Pointer; external;
function GetHandleAutoExceptionAddr: Pointer; external;

// -----------------------------------------------------------------------------

procedure InitExcMagic;
begin
  InitializeCriticalSection(ExcMagicLock);

{$IFDEF EXCMAGIC_CON}
  DebugHook := 1;     // workaround for D5 bug with 'app error' messagebox in console apps
{$ENDIF}

{$IFDEF EXCMAGIC_DEBUG}
  DebugFmt( #13#10'Running %s (console=%d)'#13#10'InitExcMagic %s.',
            [ GetModuleName(HInstance),Ord(IsConsole),ExcMagicVersion] );
{$ENDIF}

  ExceptionHook := TExcMagic.Create;

  ErrorMessageAddr     := RealAddr( Addr(SysUtils.ExceptionErrorMessage) );
  ShowErrorAddr        := RealAddr( Addr(SysUtils.ShowException) );
  HandleAnyExceptAddr  := CheckHandlerCode( RealAddr( GetHandleAnyExceptionAddr ) );
  HandleOnExceptAddr   := CheckHandlerCode( RealAddr( GetHandleOnExceptionAddr ) );
  HandleAutoExceptAddr := CheckHandlerCode( RealAddr( GetHandleAutoExceptionAddr ) );
  ExceptionHandlerAddr := CheckHandlerCode( RealAddr( GetExceptionHandlerAddr ) );

  ReadMem( ErrorMessageAddr, @OldBytesMessage, SizeOf(OldBytesMessage) );
  ReadMem( ShowErrorAddr, @OldBytesShow, SizeOf(OldBytesShow) );
  ReadMem( HandleAnyExceptAddr, @OldBytesHandleAny, SizeOf(OldBytesHandleAny) );
  ReadMem( HandleOnExceptAddr, @OldBytesHandleOn, SizeOf(OldBytesHandleOn) );
  ReadMem( HandleAutoExceptAddr, @OldBytesHandleAuto, SizeOf(OldBytesHandleAuto) );
  ReadMem( ExceptionHandlerAddr, @OldBytesExcHandler, SizeOf(OldBytesExcHandler) );

{$IFDEF EXCMAGIC_DEBUG}
  DebugFmt(
      'Original bytes of spliced procs:'#13#10 +
      '  %p ErrorMessage     [%s]'#13#10 +
      '  %p ShowError        [%s]'#13#10 +
      '  %p HandleAnyExcept  [%s]'#13#10 +
      '  %p HandleOnExcept   [%s]'#13#10 +
      '  %p HandleAutoExcept [%s]'#13#10 +
      '  %p ExceptionHandler [%s]',
      [ ErrorMessageAddr, DumpBytes(@OldBytesMessage,SizeOf(OldBytesMessage)),
        ShowErrorAddr, DumpBytes(@OldBytesShow, SizeOf(OldBytesShow) ),
        HandleAnyExceptAddr, DumpBytes(@OldBytesHandleAny, SizeOf(OldBytesHandleAny) ),
        HandleOnExceptAddr, DumpBytes(@OldBytesHandleOn, SizeOf(OldBytesHandleOn) ),
        HandleAutoExceptAddr, DumpBytes(@OldBytesHandleAuto, SizeOf(OldBytesHandleAuto) ),
        ExceptionHandlerAddr, DumpBytes(@OldBytesExcHandler, SizeOf(OldBytesExcHandler) )
      ] );
{$ENDIF}

  // lock pages in RAM to prevent swapping to pagefile
  VirtualLock( ErrorMessageAddr,     SizeOf(NewBytesMessage)    );
  VirtualLock( ShowErrorAddr,        SizeOf(NewBytesShow)       );
  VirtualLock( ExceptionHandlerAddr, SizeOf(NewBytesExcHandler) );
  VirtualLock( HandleAnyExceptAddr,  SizeOf(NewBytesHandleAny)  );
  VirtualLock( HandleOnExceptAddr,   SizeOf(NewBytesHandleOn)   );
  VirtualLock( HandleAutoExceptAddr, SizeOf(NewBytesHandleAuto) );

  NewBytesMessage.OpCode := $E9;
  NewBytesMessage.Offset := Longint(Addr(TDSExceptionErrorMessage)) -
                            Longint(ErrorMessageAddr) - 5;
  NewBytesShow.OpCode := $E9;
  NewBytesShow.Offset := Longint(Addr(TDSShowException)) -
                         Longint(ShowErrorAddr) - 5;

  {
    old 11 bytes in _Handle*Exception and _ExceptionHandler:
      8B.44.24.04              mov     eax,[esp+4]
      F7.40.04.06.00.00.00     test    dword ptr [eax+4],6
    new 11 bytes:
      E8.xx.xx.xx.xx           call    MagicHandleAllExceptions
      66.F7.40.04.06.00        test    word ptr [eax+4],6
  }
  PLongWord(@NewBytesHandleAny[1])^ := Longint(Addr(MagicHandleAllExceptions)) -
                                       Longint(HandleAnyExceptAddr) - 5;
  PLongWord(@NewBytesHandleOn[1])^  := Longint(Addr(MagicHandleAllExceptions)) -
                                       Longint(HandleOnExceptAddr) - 5;
  PLongWord(@NewBytesHandleAuto[1])^:= Longint(Addr(MagicHandleAllExceptions)) -
                                       Longint(HandleAutoExceptAddr) - 5;
  PLongWord(@NewBytesExcHandler[1])^ := Longint(Addr(MagicHandleAllExceptions)) -
                                       Longint(ExceptionHandlerAddr) - 5;

{$IFDEF EXCMAGIC_GUI}
  AppShowExceptionAddr := RealAddr( Addr(Forms.TApplication.ShowException) );
  ReadMem( AppShowExceptionAddr, @OldBytesAppShow, SizeOf(OldBytesAppShow) );

  {$IFDEF EXCMAGIC_DEBUG}
    DebugFmt( '  %p AppShowException [%s]',
        [ AppShowExceptionAddr, DumpBytes(@OldBytesAppShow,SizeOf(OldBytesAppShow)) ] );
  {$ENDIF}

  NewBytesAppShow.OpCode := $E9;
  NewBytesAppShow.Offset := Longint(Addr(TDSAppShowException)) -
                            Longint(AppShowExceptionAddr) - 5;
  // lock pages in RAM to prevent swapping to pagefile
  VirtualLock( AppShowExceptionAddr, SizeOf(NewBytesAppShow) );
{$ENDIF}

  ExceptionHook.Enabled := True;
end;

procedure ShutExcMagic;
begin
{$IFDEF EXCMAGIC_DEBUG}
  DebugMsg( #13#10'ShutExcMagic' );
{$ENDIF}

  if Assigned(ExceptionHook) then
    begin
      ExceptionHook.Enabled := False;
      FreeAndNil( ExceptionHook );
    end;

  DeleteCriticalSection( ExcMagicLock );
end;

initialization
  InitExcMagic;

finalization
  ShutExcMagic;

end.