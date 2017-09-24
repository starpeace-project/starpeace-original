// Visual C++'s MFC pulped CMapStringToObj class
// Copyright (C) 1996 by Merchise Group [PastelCAD]

unit MapStringToObject;

interface

  uses
    {$ifdef PERSISTENTMAP}
    Persistent,
    {$endif}
    Classes;

  const
    HashTableSize = 512;

  {$ifdef PERSISTENTMAP}
    {$TYPEINFO ON}
  {$endif}

  type
    TMapMode = (mmUse, mmOwn);

    TMapStringToObject =
      {$ifdef PERSISTENTMAP}
      class(TStreamable)
      {$else}
      class(TObject)
      {$endif}
        public
          constructor Create(aMode : TMapMode = mmUse);
          destructor  Destroy;                                               override;
        {$ifdef PERSISTENTMAP}
        public
          constructor Read(Storage : TInput);                                override;
          procedure   Write(Storage : TOutput);                              override;
        {$endif}
        private
          HashTable : array[0..pred(HashTableSize)] of TStringList;
        private
          fMode     : TMapMode;
          function  GetCount : integer;
          function  GetIndexes(pos : integer) : string;
          function  GetItems(index : string) : TObject;
          procedure SetItems(index : string; Item : TObject);
          function  GetValues(index : string) : string;
          procedure SetValues(index : string; value : string);
        public
          property Mode : TMapMode                 read fMode;
          property Count : integer                 read GetCount;
          property Indexes[pos : integer] : string read GetIndexes;
          property Items[index : string] : TObject read GetItems write SetItems; default;
          property Values[index : string] : string read GetValues write SetValues; 
        public
          procedure Clear;
          procedure Remove(index : string);
          procedure RemoveObject(item : TObject);
        protected
          procedure FreeObject(anObject : TObject);                          virtual;
        public
          procedure LoadFromFile( filename : string );
          procedure SaveToFile( filename : string );
      end;

implementation

  uses
    {$IFDEF USELogs}
    SysUtils, Logs;
    {$ELSE}
    SysUtils;
    {$ENDIF}


  function HashValue(const s : string) : cardinal;
    var
      i : integer;
    begin
      Result := 0;
      {$Q-}
      for i := 1 to length(s) do
        Result := (Result shl 5) + Result + ord(s[i]);
    end;


  constructor TMapStringToObject.Create(aMode : TMapMode);
    begin
      inherited Create;
      fMode := aMode;
    end;

  destructor TMapStringToObject.Destroy;
    begin
     {$IFDEF USELogs}
      try
        Logs.Log( 'Demolition', TimeToStr(Now) + ' - ' + ClassName );
      except
      end;
      {$ENDIF}
      Clear;
      inherited;
    end;

  {$ifdef PERSISTENTMAP}
  constructor TMapStringToObject.Read(Storage : TInput);
    var
      cnt  : integer;
      i    : integer;
      name : string;
    begin
      inherited;
      with Storage do
        begin
          fMode := TMapMode(ReadOrdValue(sizeof(fMode)));
          cnt := ReadOrdValue(sizeof(cnt));
          for i := 0 to pred(cnt) do
            begin
              name := ReadString;
              Items[name] := ReadObject;
            end;
        end;
    end;

  procedure TMapStringToObject.Write(Storage : TOutput);
    var
      cnt  : integer;
      i    : integer;
      name : string;
    begin
      inherited;
      with Storage do
        begin
          cnt := Count;
          WriteOrdValue(longint(fMode), sizeof(fMode));
          WriteOrdValue(cnt, sizeof(cnt));
          for i := 0 to pred(cnt) do
            begin
              name := Indexes[i];
              WriteString(name);
              WriteObject(Items[name]);
            end;
        end;
    end;
  {$endif}

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
      until Found or (i > high(HashTable));

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
            with HashTable[hash] do
              begin
                Sorted := true;
                Duplicates := dupError;
              end;
          end;
      HashTable[hash].AddObject(index, Item);
    end;

  function TMapStringToObject.GetValues(index : string) : string;
    var
      hash : integer;
    begin
      hash := HashValue(index) mod HashTableSize;
      if HashTable[hash] <> nil
        then result := HashTable[hash].Values[index]
        else Result := '';
    end;

  procedure TMapStringToObject.SetValues(index : string; value : string);
    var
      hash : integer;
    begin
      hash := HashValue(index) mod HashTableSize;
      if HashTable[hash] = nil
        then
          begin
            HashTable[hash] := TStringList.Create;
            with HashTable[hash] do
              begin
                Sorted := true;
                Duplicates := dupError;
              end;
          end;
      HashTable[hash].Values[index] := value;
    end;

  procedure TMapStringToObject.Clear;

    procedure FreeList(var aList : TStringList);
      var
        i : integer;
      begin
        if aList <> nil
          then
            begin
              for i := 0 to pred(aList.Count) do
                FreeObject(aList.Objects[i]);
              aList.Free;
              aList := nil;
            end;
      end;

    var
      i : integer;
    begin
      if fMode = mmUse
        then
          for i := low(HashTable) to high(HashTable) do
            begin
              HashTable[i].Free;
              HashTable[i] := nil;
            end
        else
          for i := low(HashTable) to high(HashTable) do
            FreeList(HashTable[i]);
    end;

  procedure TMapStringToObject.Remove(index : string);
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
                then
                  begin
                    if fMode = mmOwn
                      then FreeObject(Objects[ndx]);
                    Delete(ndx);
                  end;
            end;
    end;

  procedure TMapStringToObject.RemoveObject(item : TObject);
    var
      ndx  : integer;
      i    : integer;
    begin
      i := low(HashTable);
      ndx := -1;
      repeat
        if HashTable[i] <> nil
          then
            begin
              ndx := HashTable[i].IndexOfObject(item);
              if ndx < 0
                then inc(i);
            end
          else inc(i);
      until (i > high(HashTable)) or (ndx >=0);

      if ndx >= 0
        then
          with HashTable[i] do
            begin
              if fMode = mmOwn
                then FreeObject(Objects[ndx]);
              Delete(ndx);
            end;
    end;

  procedure TMapStringToObject.FreeObject(anObject : TObject);
    begin
      anObject.Free;
    end;

  procedure TMapStringToObject.LoadFromFile( filename : string );
    begin
    end;
   {
    var
      Lines : TStringList;
      i     : integer;
    begin
      Lines := TStringList.Create;
      try
        Lines.LoadFromFile( filename );
        for i := 0 to pred(Lines.Count) do
          Lines.V
          Values[
      except
        Lines.Free;
      end;
    end;
    }

  procedure TMapStringToObject.SaveToFile( filename : string );
    begin
    end;


initialization
  {$ifdef PERSISTENTMAP}
  TMapStringToObject.Register;
  {$endif}
end.

