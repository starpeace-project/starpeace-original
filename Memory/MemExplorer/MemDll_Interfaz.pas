unit MemDll_Interfaz;

interface

  function SysGetMem(Size: Integer): Pointer; external 'MMemory.dll';
  function SysFreeMem(P: Pointer): Integer; external 'MMemory.dll';
  function SysReallocMem(P: Pointer; Size: Integer): Pointer; external 'MMemory.dll';

  function SaveToDisk(FileName: pchar): boolean;  external 'MMemory.dll';
  function LoadFromDisk(FileName: pchar): boolean; external 'MMemory.dll';

  procedure InitPersistent; external 'MMemory.dll';

const
  cAlign        = 4;
  cThisUsedFlag = 2;
  cPrevFreeFlag = 1;
  cFillerFlag   = Integer($80000000);
  cFlags        = cThisUsedFlag or cPrevFreeFlag or cFillerFlag;
  cSmallSize    = 4*1024;

type
  PBlockDesc = ^TBlockDesc;
  TBlockDesc = 
    packed record
      next: PBlockDesc;
      prev: PBlockDesc;
      addr: PChar;
      size: Integer;
    end;

  PBlockDescBlock = ^TBlockDescBlock;
  TBlockDescBlock = 
    packed record
      next: PBlockDescBlock;
      data: array [0..99] of TBlockDesc;
    end;

  PFree = ^TFree;
  TFree = 
    packed record
      prev: PFree;
      next: PFree;
      size: Integer;
    end;
    
  PUsed = ^TUsed;
  TUsed = 
    packed record
      sizeFlags: Integer;
    end;

    
  PSmallTab = ^TSmallTab; 
  TSmallTab = array [sizeof(TFree) div cAlign .. cSmallSize div cAlign] of PFree;
    
  function GetspaceRoot: PBlockDesc;            external 'MMemory.dll';
  function GetdecommittedRoot: PBlockDesc;      external 'MMemory.dll';
  function GetcommittedRoot: PBlockDesc;        external 'MMemory.dll';

  function Get_avail        : PFree;            external 'MMemory.dll';
  function Get_rover        : PFree;            external 'MMemory.dll';
  function Get_remBytes     : Integer;          external 'MMemory.dll';
  function Get_curAlloc     : pchar;            external 'MMemory.dll';
  function Get_smallTab     : PSmallTab;       external 'MMemory.dll';
 

implementation

end.
