unit FIFO;

interface

  uses
    Classes,
    SyncObjs;

  type
    TFIFO =
      class
        public
          constructor Create;
          destructor  Destroy;                                                  override;
        public
          procedure Write(const Data; aSize : integer);
          function  Read(var Data; aSize : integer) : integer;
          function  Peek(var Data; aSize : integer) : integer;
          function  Advance(aSize : integer) : integer;
        public
          procedure Clear;
        private
          fBlocks : TList;
          fSize   : integer;
          fWPos   : integer;
          fRPos   : integer;
          fLock   : TCriticalSection;
          function GetSize : integer;
        public
          property Size : integer read GetSize;
      end;

implementation

  uses
    SysUtils;

  const
    BlockSize = 4096;

  function Min(a, b : integer) : integer; register;
    begin
      if a < b
        then Result := a
        else Result := b;
    end;

  constructor TFIFO.Create;
    begin
      fBlocks := TList.Create;
      fLock := TCriticalSection.Create;
    end;

  destructor TFIFO.Destroy;
    begin
      Clear;
      fBlocks.Free;
      fLock.Free;
      inherited;
    end;

  procedure TFIFO.Write(const Data; aSize : integer);

    function GetWBlock : pointer;
      begin
        if (fBlocks.Count > 0) and (fWPos < BlockSize)
          then Result := fBlocks.Last
          else
            begin
              getmem(Result, BlockSize);
              fBlocks.Add(Result);
              fWPos := 0;
            end;
      end;
      
    var
      ptr     : pointer;
      Left    : integer;
      ToWrite : integer;
    begin
      fLock.Enter;
      try
        Left := aSize;
        while Left > 0 do
          begin
            ptr := GetWBlock;
            ToWrite := min(BlockSize - fWPos, Left);
            move(TByteArray(Data)[aSize - Left], PByteArray(ptr)[fWPos], ToWrite);
            inc(fWPos, ToWrite);
            dec(Left, ToWrite);
          end;
        inc(fSize, aSize);
      finally
        fLock.Leave;
      end;
    end;

  function TFIFO.Read(var Data; aSize : integer) : integer;
    begin
      fLock.Enter;
      try
        Result := Peek(Data, aSize);
        Advance(Result);
      finally
        fLock.Leave;
      end;
    end;

  function TFIFO.Peek(var Data; aSize : integer) : integer;
    var
      TempBlockPos : integer;
      TempSize     : integer;
      CurrentBlock : integer;
      SizeLeft     : integer;
      ToFeed       : integer;
    begin
      fLock.Enter;
      try
        SizeLeft := aSize;

        TempBlockPos := fRPos;
        TempSize     := fSize;
        CurrentBlock := 0;
        while (SizeLeft > 0) and (TempSize > 0) do
          begin
            if TempBlockPos = BlockSize
              then
                begin
                  TempBlockPos := 0;
                  inc(CurrentBlock);
                end;
            if CurrentBlock < pred(fBlocks.Count)
              then ToFeed := Min(BlockSize - TempBlockPos, SizeLeft)
              else ToFeed := Min(fWPos - TempBlockPos, SizeLeft);
            move(PByteArray(fBlocks[CurrentBlock])[TempBlockPos], PByteArray(@Data)[aSize - SizeLeft], ToFeed);
            dec(SizeLeft, ToFeed);
            inc(TempBlockPos, ToFeed);
            dec(TempSize, ToFeed);
          end;

        Result := aSize - SizeLeft;
      finally
        fLock.Leave;
      end;
    end;

  function TFIFO.Advance(aSize : integer) : integer;
    var
      ToFeed   : integer;
      SizeLeft : integer;
    begin
      fLock.Enter;
      try
        SizeLeft := aSize;
        while (SizeLeft > 0) and (fSize > 0) do
          begin
            if fRPos = BlockSize
              then
                begin
                  fRPos := 0;
                  freemem(fBlocks[0]);
                  fBlocks.Delete(0);
                end;
            if fBlocks.Count = 1
              then ToFeed := Min(fWPos - fRPos, SizeLeft)
              else ToFeed := Min(BlockSize - fRPos, SizeLeft);
            inc(fRPos, ToFeed);
            dec(SizeLeft, ToFeed);
            dec(fSize, ToFeed);
          end;
        Result := aSize - SizeLeft;
      finally
        fLock.Leave;
      end;
    end;

  procedure TFIFO.Clear;
    var
      i : integer;
    begin
      fLock.Enter;
      try
        for i := 0 to pred(fBlocks.Count) do
          freemem(fBlocks[i]);
        fBlocks.Clear;
        fRPos := 0;
        fWPos := 0;
        fSize := 0;
      finally
        fLock.Leave;
      end;
    end;

  function TFIFO.GetSize : integer;
    begin
      fLock.Enter;
      try
        Result := fSize;
      finally
        fLock.Leave;
      end;
    end;

end.
