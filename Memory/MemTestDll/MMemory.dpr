library MMemory;


uses
  MemManeger in 'MemManeger.pas';


exports
  SysGetMem,
  SysFreeMem, 
  SysReallocMem,
  GetHeapStatus,
  GetAllocMemCount,
  GetAllocMemSize,
  DumpBlocks,            
  
  SaveToDisk,
  LoadFromDisk,
  GetspaceRoot,
  GetdecommittedRoot,
  GetcommittedRoot,
  Get_avail,
  Get_rover,        
  Get_remBytes,
  Get_curAlloc,     
  Get_smallTab;     
  
end.
