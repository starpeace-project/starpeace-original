unit BitStrmr;

interface

  uses
    FIFOIntf;

  const
    BufferBits = 32;

  type
    TBitStreamer =
      class
        public
          constructor Create;
          destructor  Destroy;                                                  override;
        public
          procedure Reset;
        public
          procedure Read(out Data; Bits : integer);
          procedure Write(const Data; Bits : integer);
        private
          fFIFO : IFIFO;
          procedure SetFIFO(const aFIFO : IFIFO);
        public
          property FIFO : IFIFO read fFIFO write SetFIFO;
        private
          fAcc : array[0..pred(BufferBits div 8)] of byte;
          fPos : integer;
      end;

implementation

  uses
    SysUtils,
    Math;

  procedure moveBits(const From; FromPos : integer; var Dest; DestPos, Bits : integer);

    function SignShift(x, y : integer) : byte;
      begin
        if y >= 0
          then Result := x shl y
          else Result := x shr -y;
      end;

    const
      Align = 8;
    var
      bFrom   : TByteArray absolute From;
      bDest   : TByteArray absolute Dest;
      byFrom  : integer;
      biFrom  : integer;
      biCount : integer;
      biMove  : integer;
      byTo    : integer;
      biTo    : integer;
      bimFrom : integer;
      bimTo   : integer;
      mFrom   : byte;
      mTo     : byte;
      mCount  : byte;
    begin
      byFrom := FromPos div Align;
      biFrom := FromPos - byFrom * Align;

      byTo := DestPos div Align;
      biTo := DestPos - byTo * Align;

      biCount := Bits;
      while biCount > 0 do
        begin
          bimFrom := min(biCount, Align - biFrom);
          bimTo   := min(biCount, Align - biTo);

          biMove := min(bimFrom, bimTo);

          mCount := 255 and pred(1 shl biMove); // Generate biMove "1"s

          // Make a mask with biFrom and biMove
          mFrom := mCount shl biFrom;
          // Make a mask with biTo and biMove
          mTo := not(mCount shl biTo);

          bDest[byTo] := bDest[byTo] and mTo or SignShift(bFrom[byFrom] and mFrom, biTo - biFrom);

          dec(biCount, biMove);

          inc(biFrom, biMove);
          if biFrom >= Align
            then
              begin
                biFrom := 0;
                inc(byFrom);
              end;

          inc(biTo, biMove);
          if biTo >= Align
            then
              begin
                biTo := 0;
                inc(byTo);
              end;
        end;
    end;

  constructor TBitStreamer.Create;
    begin
      inherited;
      Reset;
    end;

  destructor TBitStreamer.Destroy;
    begin
      fFIFO := nil;
      inherited;
    end;

  procedure TBitStreamer.Reset;
    begin
      fillchar(fAcc, sizeof(fAcc), 0);
      fPos := 0;
    end;

  procedure TBitStreamer.Read(out Data; Bits : integer);
    var
      Actual   : integer;
      DataLeft : integer;
      ToCopy   : integer;
    begin
      Assert(Bits <= BufferBits);
      DataLeft := Bits;
      while DataLeft > 0 do
        begin
          if fPos = 0
            then fFIFO.Read(fAcc, sizeof(fAcc), Actual);
          ToCopy := Min(DataLeft, BufferBits - fPos);
          moveBits(fAcc, fPos, Data, Bits - DataLeft, ToCopy);
          dec(DataLeft, ToCopy);
          inc(fPos, ToCopy);
          if fPos = BufferBits
            then fPos := 0;
        end;
    end;

  procedure TBitStreamer.Write(const Data; Bits : integer);
    var
      DataLeft : integer;
      ToCopy   : integer;
    begin
      Assert(Bits <= BufferBits);
      DataLeft := Bits;
      while DataLeft > 0 do
        begin
          if fPos = BufferBits
            then
              begin
                fFIFO.Write(fAcc, sizeof(fAcc));
                fPos := 0;
              end;
          ToCopy := Min(DataLeft, BufferBits - fPos);
          moveBits(Data, Bits - DataLeft, fAcc, fPos, ToCopy);
          dec(DataLeft, ToCopy);
          inc(fPos, ToCopy);
        end;
    end;

  procedure TBitStreamer.SetFIFO(const aFIFO : IFIFO);
    begin
      fFIFO := aFIFO;
      Reset;
    end;

end.
