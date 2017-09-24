unit PersistenTest;

interface

type
  TTestMemory =
    class                        
      public
        procedure CreateMemory;
        procedure CreateObj;
      private
        fStr      : string;
        fArr      : array of integer;
        fObjLink  : TTestMemory;
    end;

  function CreateObj: TTestMemory;
  function GetMemmory(const i: integer):pointer;
  procedure Save;
  
  var
    fLocal : TTestMemory;
  
implementation

// This is de initializaton
const
  MemDll = 'MMemory.dll';
  
var
  OldMemMgr: TMemoryManager;
  
  function SysGetMem(Size: Integer): Pointer; external MemDll;
  function SysFreeMem(P: Pointer): Integer; external MemDll;
  function SysReallocMem(P: Pointer; Size: Integer): Pointer; external MemDll;
  
  function SaveToDisk(FileName: pchar): boolean;  external MemDll;
  function LoadFromDisk(FileName: pchar): boolean; external MemDll;

  procedure InitPersistent; external MemDll; 
  

procedure InitMemoryManager;
  var
    SharedMemoryManager: TMemoryManager;
  begin
    SharedMemoryManager.GetMem := SysGetMem;
    SharedMemoryManager.FreeMem := SysFreeMem;
    SharedMemoryManager.ReallocMem := SysReallocMem;
    GetMemoryManager(OldMemMgr);
    SetMemoryManager(SharedMemoryManager);
  end;

function CreateObj: TTestMemory;
  begin
    fLocal := TTestMemory.Create;
    fLocal.CreateMemory;
    result := fLocal;
  end;

function GetMemmory(const i: integer):pointer;
  begin
    getmem(result, i);
  end;
  
procedure Save;
  begin
    SaveToDisk('c:\temp\kk.aaa');
  end;
  
{ TTestMemory }
procedure TTestMemory.CreateMemory;
  begin
    fStr := 'Castastrofico';
    setlength(fArr, 40000);
    fillchar(fArr, sizeof(fArr), 'A');
  end;

procedure TTestMemory.CreateObj;
  begin
    fObjLink := TTestMemory.Create;
    fObjLink.CreateMemory;
  end;

initialization
  if not IsMemoryManagerSet 
    then InitMemoryManager;
finalization
  SetMemoryManager(OldMemMgr);
end.  

