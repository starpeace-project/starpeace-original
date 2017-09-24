unit SMCheck;

interface   
  uses
    ShareMem; 

const
  LeadingBytes  = 34;
  TrailingBytes = 25;

  function PrevBlock(pp: pointer): pointer;
  function IsObject(a: pointer): boolean;
  
implementation

uses
  SysUtils, windows;
  
type
  PInteger = ^integer;

const
  Magic   = (ord('A') shl 24) or (ord('B') shl 16) or (ord('C') shl 8) or ord('D');
  cAlign  = 4;
  
var
  OldMemMgr: TMemoryManager;

function SysGetMemCheck(Size: Integer): Pointer; 
  var
    p : pchar;
  begin
    inc(Size, 8+LeadingBytes+TrailingBytes);
    result := OldMemMgr.GetMem(size);
    if result<>nil
      then 
        begin
          Size := PInteger(integer(result)-4)^ and (not 3);
          PInteger(result)^ := Size;
          p := pchar(result)+Size-8;
          PInteger(p)^ := Magic;
          inc(pchar(result), 4+LeadingBytes);
        end;
  end;
  
function SysFreeMemCheck(P: Pointer): Integer; 
  var
    q : pchar;
  begin
    if p<>nil
      then
        begin
          dec(pchar(p), 4+LeadingBytes);
          if pinteger(p)^ <> (pinteger(integer(p)-4)^ and (not 3))
            then raise Exception.create(format('SF Before overwrite, or free pointer = %x', [integer(p)]));
          q := pchar(p)+ PInteger(p)^-8;
          if PInteger(q)^=Magic
            then result := OldMemMgr.FreeMem(P)
            else raise Exception.create(format('SF After = %x',[integer(p)]));
        end
      else result := 0;
  end;
  
function SysReallocMemCheck(P: Pointer; Size: Integer): Pointer; 
  var
    q : pchar;
  begin
    if p<>nil
      then
        begin
          dec(pchar(p), 4+LeadingBytes);
          if pinteger(p)^ <> (pinteger(integer(p)-4)^ and (not 3))
            then raise Exception.create(format('RM Before overwrite, or free pointer = %x', [integer(p)]));
          q := pchar(p)+ PInteger(p)^-8;
          if PInteger(q)^=Magic
            then 
              begin
                if size>0
                  then inc(size, 8+LeadingBytes+TrailingBytes);
                result := OldMemMgr.ReallocMem(P, size);
              end
            else raise Exception.create(format('RM After = %x', [integer(p)]));
          if result<>nil
            then 
              begin
                Size := PInteger(integer(result)-4)^ and (not 3);
                PInteger(result)^ := Size;
                q := pchar(result)+Size-8;
                PInteger(q)^ := Magic;
                inc(pchar(result), 4+LeadingBytes);
              end;
        end
      else result := nil;
  end;
 
procedure InitMemoryManager;
  var
    SharedMemoryManager: TMemoryManager;
  begin
    SharedMemoryManager.GetMem := SysGetMemCheck;
    SharedMemoryManager.FreeMem := SysFreeMemCheck;
    SharedMemoryManager.ReallocMem := SysReallocMemCheck;
    GetMemoryManager(OldMemMgr);
    SetMemoryManager(SharedMemoryManager);
  end;

////////////////////////////////////////////////////////////////////////
type
  PPointer = ^pointer;
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

  const
    cThisUsedFlag = 2;
    cPrevFreeFlag = 1;
    cFillerFlag   = Integer($80000000);
    cFlags        = cThisUsedFlag or cPrevFreeFlag or cFillerFlag;
    cSmallSize    = 4*1024;
  
function IsObject(a: pointer): boolean;
  var
    AObject: TObject;
    AClass: TClass;
  type
    PPointer = ^Pointer;
  begin
    try
      AObject := TObject(a);
      AClass := AObject.ClassType;
      result := (Integer(AClass) >= 64*1024) and (PPointer(PChar(AClass) + vmtSelfPtr)^ = Pointer(AClass));
    except
      result := false
    end;
  end;
  
function PrevBlock(pp: pointer): pointer;
  var
    P            : pointer absolute OldMemMgr;
    avail        : PFree;
    rover        : PFree;
    remBytes     : PInteger;
    curAlloc     : PChar;
    smallTab     : pointer;
    committedRoot: PBlockDesc;
    AllocMemCount: PInteger;
    heapLock     : ^TRTLCriticalSection;
  function SearchPrev: pointer;
    var
      a, e: PChar;
      b: PBlockDesc;
      f : PFree;
      prevFree: Boolean;
      size  : integer;
      freeSize : integer;
      tmp: pointer;
    begin
      b := committedRoot.next;
      FreeSize := 0;
      prevFree := False;
      result := nil;
      if (b<>nil) 
        then 
          begin
            while b <> committedRoot do 
              begin
                a := b.addr;
                e := a + b.size;
                while a < e do 
                  begin
                    if (a = curAlloc) and (remBytes^ > 0) 
                      then 
                        begin
                          size := remBytes^;
                          Inc(freeSize, size);
                          if prevFree 
                            then ;// 'Bad Cur Alloc';
                          prevFree := False;
                        end 
                      else 
                        begin
                          if prevFree <> ((PUsed(a).sizeFlags and cPrevFreeFlag) <> 0) 
                            then ;// 'Bad Cur Alloc';
                          if (PUsed(a).sizeFlags and cThisUsedFlag) = 0 
                            then 
                              begin
                                f := PFree(a);
                                if (f.prev.next <> f) or (f.next.prev <> f) or (f.size < sizeof(TFree)) 
                                  then ;// 'Bad Free Block';
                                size := f.size;
                                Inc(freeSize, size);
                                prevFree := True;
                              end 
                            else 
                              begin
                                size := PUsed(a).sizeFlags and not cFlags;
                                if (PUsed(a).sizeFlags and cFillerFlag) <> 0 
                                  then 
                                    begin
                                      if (a > b.addr) and (a + size < e) 
                                        then ;// 'Bad Used Block';
                                    end 
                                  else 
                                    begin
                                      tmp := pchar(a+sizeof(TUsed)+4+LeadingBytes);
                                      if (integer(tmp)<integer(pp)) and (integer(tmp)>integer(result))
                                        then result := tmp;
                                    end;
                                prevFree := False;
                              end;
                        end;
                    inc(a, size);
                  end;
                b := b.next;
              end;
          end;
    end;
  begin
    avail := pointer(integer(P) + $2BAC);
    rover := avail;           
    inc(pchar(rover), sizeof(TFree));
    remBytes := pointer(rover);
    inc(pchar(remBytes), sizeof(PFree));
    curAlloc := pointer(remBytes);
    inc(pchar(curAlloc), sizeof(integer));
    smallTab := pointer(curAlloc);
    inc(pchar(smallTab), sizeof(pointer));
    committedRoot := pointer(smallTab);
    inc(pchar(committedRoot), sizeof(pointer));
    
    heapLock := pointer(integer(P) + $2B6C);

    avail         := PPointer(avail)^;
    rover         := PPointer(rover)^;
    curAlloc      := PPointer(curAlloc)^;
    smallTab      := PPointer(smallTab)^;
    try
      EnterCriticalSection(heapLock^);
      result := SearchPrev
    finally
      LeaveCriticalSection(heapLock^);
    end;
  end;
    
initialization
  InitMemoryManager;
finalization
  SetMemoryManager(OldMemMgr);
end.
