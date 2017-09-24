unit MapStringToObject;

interface

  uses
    Classes;

  const
    HashTableSize = 256;

  type
    TMapMode = (mmUse, mmOwn);

    TMapStringToObject =
      class
        public
          constructor Create(aMode : TMapMode);
          destructor  Destroy; override;
        private
          HashTable : array[0..pred(HashTableSize)] of TStringList;
        private
          fMode : TMapMode;
          function  GetCount : integer;
          function  GetIndexes(pos : integer) : string;
          function  GetItems(index : string) : TObject;
          procedure SetItems(index : string; Item : TObject);
        public
          property Count : integer                 read GetCount;
          property Indexes[pos : integer] : string read GetIndexes;
          property Mode : TMapMode                 read fMode;
          property Items[index : string] : TObject read GetItems write SetItems; default;
      end;

implementation

  uses
    SysUtils, MathUtils;

  function HashValue(const s : string) : cardinal;
    var
      i : integer;
    begin
      {$Q-}
      Result := 0;
      for i := 1 to min(5, length(s)) do
        Result := (Result shl 5) + Result + ord(s[i]);
      {$Q+}
    end;


  constructor TMapStringToObject.Create(aMode : TMapMode);
    begin
      inherited Create;
      fMode := aMode;
    end;

  destructor TMapStringToObject.Destroy;

    procedure FreeList(aList : TStringList);
      var
        i : integer;
      begin
        if aList <> nil
          then
            begin
              for i := 0 to pred(aList.Count) do
                aList.Objects[i].Free;
              aList.Free;
            end;
      end;
      
    var
      i : integer;
    begin
      if fMode = mmUse
        then
          for i := low(HashTable) to high(HashTable) do
            HashTable[i].Free
        else
          for i := low(HashTable) to high(HashTable) do
            FreeList(HashTable[i]);
      inherited;
    end;

  function TMapStringToObject.GetCount : integer;
    var
      i : integer;
    begin
      Result := 0;
      for i := low(HashTable) to high(HashTable) do
        if HashTable[i] <> nil
          then inc(Result, HashTable[i].Count);
    end;

  function TMapStringToObject.GetIndexes(pos : integer) : string;
    var
      i     : integer;
      c     : integer;
      Found : boolean;
    begin
      c := 0;
      i := low(HashTable);

      Found := false;
      repeat
        if HashTable[i] <> nil
          then
           if pos < c + HashTable[i].count
             then Found := true
             else
               begin
                 inc(c, HashTable[i].count);
                 inc(i);
               end
          else
            inc(i);
      until Found or (i = high(HashTable));

      if Found
        then Result := HashTable[i][pos - c]
        else raise Exception.Create('Index out of bounds');
    end;

  function TMapStringToObject.GetItems(index : string) : TObject;
    var
      hash : integer;
      ndx  : integer;
    begin
      hash := HashValue(index) mod HashTableSize;
      if HashTable[hash] <> nil
        then
          with HashTable[hash] do
            begin
              ndx := IndexOf(index);
              if ndx >= 0
                then Result := Objects[ndx]
                else Result := nil;
            end
        else Result := nil;
    end;

  procedure TMapStringToObject.SetItems(index : string; Item : TObject);
    var
      hash : integer;
    begin
      hash := HashValue(index) mod HashTableSize;
      if HashTable[hash] = nil
        then
          begin
            HashTable[hash] := TStringList.Create;
            HashTable[hash].Sorted := true;
            HashTable[hash].Duplicates := dupError;
          end;
      HashTable[hash].AddObject(index, Item);
    end;

end.

