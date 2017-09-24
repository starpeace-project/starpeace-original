unit ShareMM;

interface              

//  function SysGetMem(size: Integer): Pointer;
//  function SysFreeMem(p: Pointer): Integer;
//  function SysReallocMem(p: Pointer; size: Integer): Pointer;
//  function GetHeapStatus: THeapStatus;
//  function GetAllocMemCount: Integer;
//  function GetAllocMemSize: Integer;
//  procedure DumpBlocks; 
  
implementation
uses
  windows;
  
const
  MMemory = 'MMemory.dll';

var
  OldMemMgr: TMemoryManager;

  function SysGetMem(Size: Integer): Pointer; external MMemory;
  function SysFreeMem(P: Pointer): Integer; external MMemory;
  function SysReallocMem(P: Pointer; Size: Integer): Pointer; external MMemory;
  function GetHeapStatus: THeapStatus; external MMemory;
  function GetAllocMemCount: Integer; external MMemory;
  function GetAllocMemSize: Integer; external MMemory;
  procedure DumpBlocks; external MMemory;
  {
  function GetModuleHandle(lpModuleName: PChar): Integer; stdcall; 
    external 'kernel32.dll' name 'GetModuleHandleA';
  function GetProcAddress(hModule: Integer; lpProcName: PChar): Pointer; stdcall;
    external 'kernel32.dll' name 'GetProcAddress';
   }

procedure InitMemoryManager;
  var
    SharedMemoryManager: TMemoryManager;
    MM: Integer;
  begin
    // force a static reference to borlndmm.dll, so we don't have to LoadLibrary
    SharedMemoryManager.GetMem := SysGetMem;

    MM := GetModuleHandle(MMemory);
//    MM := LoadLibrary(MMemory);
    if MM<>0
      then
        begin
          SharedMemoryManager.GetMem := GetProcAddress(MM,'SysGetMem');
          SharedMemoryManager.FreeMem := GetProcAddress(MM,'SysFreeMem');
          SharedMemoryManager.ReallocMem := GetProcAddress(MM, 'SysReallocMem');
          GetMemoryManager(OldMemMgr);
          SetMemoryManager(SharedMemoryManager);
        end;
      mm := GetLastError;
  end;
  
initialization
  if not IsMemoryManagerSet
    then InitMemoryManager;
finalization
  SetMemoryManager(OldMemMgr);
end.  

