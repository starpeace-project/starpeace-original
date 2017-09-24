unit HashTables;

interface

  uses
    SysUtils, Classes, Math, Windows;

  const
    EMPTY = Pointer(-1);
    DELETED = Pointer(-2);

  type
    THashTable = class(TObject)
    private
      Alpha: Double;
      FTable: PPointerList;
      FCount: Integer;
      FCapacity: Integer;
      FMaximumFillRatio: Double;
      FPosition: Integer;
      FCollisions: Integer;
      FInsertions: Integer;
      function GetAverageCollision: Real;
      procedure SetMaximumFillRatio(Value: Double);
    protected
      procedure Error(const msg: string);
      function Get(Key: Integer): Pointer; virtual;
      function GetIndex(Key: Integer): Integer;
      procedure Grow; virtual;
      function Hash(Key: Integer): Integer; virtual;
      procedure Put(Key: Integer; Item: Pointer); virtual;
      procedure Rehash(OldTable: PPointerList; OldCount: Integer);
      procedure SetCapacity(NewCapacity: Integer);
    public
      constructor Create;
      destructor Destroy; override;
      procedure Clear; virtual;
      function Current: Pointer; virtual;
      function DeleteCurrent: Pointer;
      function First: Pointer; virtual;
      function Insert(Key: Integer; Item: Pointer): Pointer; virtual;
      function Next: Pointer; virtual;
      function NextSame(Key: Integer): Pointer;
      function Remove(Key: Integer): Pointer; virtual;
      procedure Pack;
      property Capacity: Integer read FCapacity write SetCapacity;
      property Count: Integer read FCount;
      property MaximumFillRatio: Double read FMaximumFillRatio write
        SetMaximumFillRatio;
      property Items[Key: Integer]: Pointer read Get write Put; default;
      property AverageCollisions: Real read GetAverageCollision;
    end;

    TStringTableNode = record
      FKey: string;
      FObject: TObject;
    end;
    PStringTableNode = ^TStringTableNode;

    TStringTable = class(THashTable)
    private
      function ConvertKey(const Key: string): Integer;
      function FindKey(const Key: string; var Node: PStringTableNode): Boolean;
    protected
      function Get(const Key: string): TObject; reintroduce;
      procedure Put(const Key: string; Item: TObject); reintroduce;
    public
      destructor Destroy; override;
      procedure Clear; override;
      function Current: TObject; reintroduce;
      function CurrentKey: string; reintroduce;
      function First: TObject; reintroduce;
      function Insert(const Key: string; Item: TObject): Pointer; reintroduce;
      function Next: TObject; reintroduce;
      function Remove(const Key: string): TObject; reintroduce;
      property Items[const Key: string]: TObject read Get write Put; default;
    end;

    EHashTableError = class(Exception);

implementation

  constructor THashTable.Create;
    begin
      Alpha := (Sqrt(5.0) - 1) / 2.0;
      FMaximumFillRatio := 0.8;
      SetCapacity(256);
    end;

  destructor THashTable.Destroy;
    begin
      FreeMem(FTable, FCapacity * (SizeOf(Pointer) * 2));
    end;

  procedure THashTable.Clear;
    begin
      FCount := 0;
      FPosition := -2;
      FillChar(FTable^, FCapacity * (SizeOf(Pointer) * 2), Char(EMPTY));
    end;

  function THashTable.Current: Pointer;
    begin
      if (FPosition >= 0) and (FPosition < FCapacity) and
        (FTable[FPosition] <> EMPTY) and (FTable[FPosition] <> DELETED) then
        Result := FTable[FPosition + 1]
      else Result := nil;
    end;

  function THashTable.DeleteCurrent: Pointer;
    begin
      FTable[FPosition] := DELETED;
      Result := FTable[FPosition + 1];
      Dec(FCount);
    end;

  procedure THashTable.Error(const msg: string);
    begin
      raise EHashTableError.Create(msg);
    end;

  function THashTable.First: Pointer;
    begin
      FPosition := -2;
      Result := Next;
    end;

  function THashTable.Get(Key: Integer): Pointer;
    begin
      FPosition := GetIndex(Key);
      if Integer(FTable[FPosition]) = Key then Result := FTable[FPosition + 1]
      else Result := nil;
    end;

  function THashTable.GetAverageCollision: Real;
    begin
      if FInsertions = 0 then Result := 0.0
      else Result := FCollisions / FInsertions;
    end;

  function THashTable.GetIndex(Key: Integer): Integer;
    var
      I: Integer;
    begin
      Result := Hash(Key) * 2;
      I := 0;
      while (I < FCapacity) and (FTable[Result] <> Pointer(Key)) and
        (FTable[Result] <> EMPTY) do
      begin
        Inc(Result, 2);
        Inc(I);
        Result := Result mod (FCapacity * 2);
      end;
    end;

  procedure THashTable.Grow;
    begin
      SetCapacity(FCapacity * 2);
    end;

  function THashTable.Hash(Key: Integer): Integer;
    begin
      if Key < 0 then Error('Keys must be positive');
      Result := Trunc(FCapacity * Frac(Alpha * Key));
    end;

  function THashTable.Insert(Key: Integer; Item: Pointer): Pointer;
    begin
      if (FCount + 1) >= Round(FCapacity * FMaximumFillRatio) then Grow;
      Inc(FCount);
      FPosition := Hash(Key) * 2;
      while (FTable[FPosition] <> EMPTY) and (FTable[FPosition] <> DELETED) do
      begin
        Inc(FCollisions);
        Inc(FPosition, 2);
        FPosition := FPosition mod (FCapacity * 2);
      end;
      FTable[FPosition] := Pointer(Key);
      FTable[FPosition + 1] := Item;
      Result := @FTable[FPosition + 1];
      Inc(FInsertions)
    end;

  function THashTable.Next: Pointer;
    begin
      Inc(FPosition, 2);
      while (FPosition < (FCapacity * 2)) and ((FTable[FPosition] = EMPTY) or
        (FTable[FPosition] = DELETED)) do Inc(FPosition, 2);
      if FPosition < (FCapacity * 2) then Result := FTable[FPosition + 1]
      else Result := nil;
    end;

  function THashTable.NextSame(Key: Integer): Pointer;
    var
      oldpos: Integer;
    begin
      oldpos := FPosition;
      Inc(FPosition, 2);
      while (FPosition <> oldpos) and (FTable[FPosition] <> EMPTY) and
        (FTable[FPosition] <> Pointer(Key)) do
      begin
        Inc(FPosition, 2);
        FPosition := FPosition mod (FCapacity * 2);
      end;
      if FTable[FPosition] = Pointer(Key) then Result := FTable[FPosition + 1]
      else Result := nil;
    end;

  procedure THashTable.Pack;
    begin
      SetCapacity(Round(FCount * (1 / FMaximumFillRatio)) + 2);
    end;

  procedure THashTable.Put(Key: Integer; Item: Pointer);
    begin
      FPosition := GetIndex(Key);
      if Integer(FTable[FPosition]) = Key then FTable[FPosition + 1] := Item
      else Insert(Key, Item);
    end;

  function THashTable.Remove(Key: Integer): Pointer;
    begin
      FPosition := GetIndex(Key);
      if Integer(FTable[FPosition]) = Key then
      begin
        FTable[FPosition] := DELETED;
        Result := FTable[FPosition + 1];
        Dec(FCount);
      end
      else Result := nil;
    end;

  procedure THashTable.Rehash(OldTable: PPointerList; OldCount: Integer);
    var
      I: Integer;
    begin
      I := 0;
      while FCount < OldCount do
      begin
        while (OldTable[I] = EMPTY) or (OldTable[I] = DELETED) do Inc(I, 2);
        Insert(Integer(OldTable[I]), OldTable[I + 1]);
        Inc(I, 2);
      end;
    end;

  procedure THashTable.SetCapacity(NewCapacity: Integer);
    var
      OldTable: Pointer;
      OldCapacity, OldCount: Integer;
    begin
      if (FCount >= Round(NewCapacity * FMaximumFillRatio)) or
        (NewCapacity > (MaxListSize div 2)) then Error('Invalid capacity');
      if NewCapacity <> FCapacity then
      begin
        OldTable := FTable;
        FTable := AllocMem(NewCapacity * (SizeOf(Pointer) * 2));
        OldCapacity := FCapacity;
        FCapacity := NewCapacity;
        OldCount := FCount;
        FPosition := -1;
        Clear;
        ReHash(OldTable, OldCount);
        FreeMem(OldTable, OldCapacity * (SizeOf(Pointer) * 2));
      end;
    end;

  procedure THashTable.SetMaximumFillRatio(Value: Double);
    begin
      if (Value < 0.5) or (Value > 1.0) then
        Error('Maximum fill ratio must be between 0.5 and 1.0');
      FMaximumFillRatio := Value;
      if FCount > Round(FCapacity * FMaximumFillRatio) then Grow;
    end;

  { TStringTable }

  procedure TStringTable.Clear;
    var
      pt: PStringTableNode;
    begin
      pt := PStringTableNode(inherited First);
      while pt <> nil do
      begin
        Dispose(pt);
        pt := inherited Next;
      end;
      inherited Clear;
    end;

  function TStringTable.ConvertKey(const Key: string): Integer;
    var
      i: Integer;
    begin
      Result := 0;
      for i := 1 to Length(Key) do Result := (131 * Result) + Ord(Key[i]);
      Result := Abs(Result);
    end;

  function TStringTable.Current: TObject;
    var
      pt: PStringTableNode;
    begin
      pt := inherited Current;
      if pt <> nil then Result := pt^.FObject
      else Result := nil;
    end;

  function TStringTable.CurrentKey: string;
    var
      pt: PStringTableNode;
    begin
      pt := inherited Current;
      if pt <> nil then Result := pt^.FKey
      else Result := '';
    end;

  destructor TStringTable.Destroy;
    begin
      Clear;
      inherited Destroy;
    end;

  function TStringTable.FindKey(const Key: string; var Node: PStringTableNode): Boolean;
    var
      k: Integer;
    begin
      k := ConvertKey(Key);
      Node := inherited Get(k);
      while (Node <> nil) and (Node^.FKey <> Key) do NextSame(k);
      Result := (Node <> nil);
    end;

  function TStringTable.First: TObject;
    var
      pt: PStringTableNode;
    begin
      pt := inherited First;
      if pt <> nil then Result := pt^.FObject
      else Result := nil;
    end;

  function TStringTable.Get(const Key: string): TObject;
    var
      pt: PStringTableNode;
    begin
      if FindKey(Key, pt) then Result := pt^.FObject
      else Result := nil;
    end;

  function TStringTable.Insert(const Key: string; Item: TObject): Pointer;
    var
      pt: PStringTableNode;
    begin
      New(pt);
      pt^.FKey := Key;
      pt^.FObject := Item;
      inherited Insert(ConvertKey(Key), pt);
      Result := @(pt^.FObject);
    end;

  function TStringTable.Next: TObject;
    var
      pt: PStringTableNode;
    begin
      pt := inherited Next;
      if pt <> nil then Result := pt^.FObject
      else Result := nil;
    end;

  procedure TStringTable.Put(const Key: string; Item: TObject);
    var
      pt: PStringTableNode;
    begin
      if FindKey(Key, pt) then pt^.FObject := Item
      else Insert(Key, Item);
    end;

  function TStringTable.Remove(const Key: string): TObject;
    var
      pt: PStringTableNode;
    begin
      if FindKey(Key, pt) then
      begin
        DeleteCurrent;
        Result := pt^.FObject;
        Dispose(pt);
      end
      else Result := nil;
    end;

end.
