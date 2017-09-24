unit psapi;

{$WEAKPACKAGEUNIT ON}

interface

uses Windows;

type
PHInst = ^HInst;
TModuleInfo = record
  lpBaseOfDll : pointer;
  SizeOfImage : Integer;
  EntryPoint : pointer
end;

TPSAPIWsWatchInformation = record
  FaultingPc : pointer;
  FaultingVa : pointer
end;

TProcessMemoryCounters = record
  cb : Integer;
  PageFaultCount : Integer;
  PeakWorkingSetSize : Integer;
  WorkingSetSize : Integer;
  QuotaPeakPagedPoolUsage : Integer;
  QuotaPagedPoolUsage : Integer;
  QuotaPeakNonPagedPoolUsage : Integer;
  QuotaNonPagedPoolUsage : Integer;
  PagefileUsage : Integer;
  PeakPagefileUsage : Integer
end;

function EnumProcesses (pidList : PInteger; cb : Integer; var cbNeeded : Integer): boolean; stdcall;
function EnumProcessModules (hProcess : THandle; moduleList : PHInst; cb : Integer; var cbNeeded : Integer) : boolean; stdcall;
function GetModuleBaseName (hProcess : THandle; module : HInst; BaseName : Pchar; size : Integer) : Integer; stdcall;
function GetModuleFileNameEx (hProcess : THandle; module : HInst; FileName : PChar; size : Integer) : Integer; stdcall;
function GetModuleInformation(hProcess : THandle; module : HInst; var info : TModuleInfo; size : Integer) : boolean; stdcall;
function EmptyWorkingSet (hProcess : THandle) : boolean; stdcall;
function QueryWorkingSet (hProcess : THandle; var pv; size : Integer) : boolean; stdcall;
function InitializeProcessForWsWatch (hProcess : THandle) : boolean; stdcall;
function GetWsChanges (hProcess : THandle; var WatchInfo : TPSAPIWsWatchInformation; size : Integer) : boolean; stdcall;
function GetMappedFileName (hProcess : THandle; pv : pointer; FileName : PChar; size : Integer) : Integer; stdcall;
function EnumDeviceDrivers (var ImageBase : Integer; cb : Integer; var cbNeeded : Integer) : boolean; stdcall;
function GetDeviceDriverBaseName (ImageBase : Integer; BaseName : PChar; size : Integer) : Integer; stdcall;
function GetDeviceDriverFileName (ImageBase : Integer; FileName : PChar; size : Integer) : Integer; stdcall;
function GetProcessMemoryInfo (hProcess : THandle; var ProcessMemoryCounters : TProcessMemoryCounters; size : Integer) : boolean; stdcall;

implementation

const psapidll = 'psapi.dll';

function EnumProcesses; external psapidll;
function EnumProcessModules; external psapidll;
function GetModuleBaseName; external psapidll name 'GetModuleBaseNameA';
function GetModuleFileNameEx; external psapidll name 'GetModuleFileNameExA';
function GetModuleInformation; external psapidll;
function EmptyWorkingSet; external psapidll;
function QueryWorkingSet; external psapidll;
function InitializeProcessForWsWatch; external psapidll;
function GetWsChanges; external psapidll;
function GetMappedFileName; external psapidll name 'GetMappedFileNameA';
function EnumDeviceDrivers; external psapidll;
function GetDeviceDriverBaseName; external psapidll name 'GetDeviceDriverBaseNameA';
function GetDeviceDriverFileName; external psapidll name 'GetDeviceDriverFileNameA';
function GetProcessMemoryInfo; external psapidll;
end.
