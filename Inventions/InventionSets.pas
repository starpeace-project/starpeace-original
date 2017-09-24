unit InventionSets;

interface

  const
    BitCount = 8;

  type
    Bits              = 0..BitCount-1;
    TBitSet           = set of Bits;
    PInventionIdArray = ^TInventionIdArray;
    TInventionIdArray = array[0..$FFFF div BitCount] of TBitSet;

    TInventionNumId   = integer;

    TInventionSet =
      class
        public
          constructor Create(IniCount : integer);
          destructor  Destroy; override;
        private
          fSets : PInventionIdArray;
        private
          function  CountToSets(count : integer) : integer;
          function  Capacity : integer;
          procedure Decompose(Elem : TInventionNumId; var Index : integer; var Value : Bits);
        public
          function Include(Elem : TInventionNumId)  : boolean;
          function Exclude(Elem : TInventionNumId)  : boolean;
          function Included(Elem : TInventionNumId) : boolean;
      end;

implementation

  uses
    SysUtils;

  type
    pinteger = ^integer;

  // TInventionSet

  constructor TInventionSet.Create(IniCount : integer);
    var
      size : integer;
    begin
      inherited Create;
      size := CountToSets(IniCount)*sizeof(fSets[0]);
      ReallocMem(fSets, size);
      if fSets <> nil
        then FillChar(fSets[0], Capacity*sizeof(fSets[0]), 0);
    end;

  destructor TInventionSet.Destroy;
    begin
      ReallocMem(fSets, 0);
      inherited;
    end;

  function TInventionSet.CountToSets(count : integer) : integer;
    begin
      result := succ(count div BitCount);
    end;

  function TInventionSet.Capacity : integer;
    begin
      if fSets <> nil
        then result := (pinteger(pchar(fSets)-4)^ - 6) div sizeof(fSets[0])
        else result := 0;
    end;

  procedure TInventionSet.Decompose(Elem : TInventionNumId; var Index : integer; var Value : Bits);
    begin
      Index := Elem div BitCount;
      Value := Elem mod BitCount;
    end;

  function TInventionSet.Include(Elem : TInventionNumId) : boolean;
    var
      idx    : integer;
      val    : Bits;
      OldCap : integer;
      NewCap : integer;
    begin
      Decompose(Elem, idx, val);
      if idx >= Capacity
        then
          begin
            OldCap := Capacity;
            ReallocMem(fSets, succ(idx)*sizeof(fSets[0]));
            NewCap := Capacity;
            FillChar(fSets[OldCap], (NewCap - OldCap)*sizeof(fSets[0]), 0);
          end;
      if val in fSets[idx]
        then result := false
        else
          begin
            System.Include(fSets[idx], val);
            result := true;
          end;
    end;

  function TInventionSet.Exclude(Elem : TInventionNumId) : boolean;
    var
      idx : integer;
      val : Bits;
    begin
      Decompose(Elem, idx, val);
      if idx < Capacity
        then
          begin
            result := val in fSets[idx];
            System.Exclude(fSets[idx], val);
          end
        else result := false;
    end;

  function TInventionSet.Included(Elem : TInventionNumId) : boolean;
    var
      idx : integer;
      val : Bits;
    begin
      Decompose(Elem, idx, val);
      if idx < Capacity
        then result := val in fSets[idx]
        else result := false;
    end;

end.
