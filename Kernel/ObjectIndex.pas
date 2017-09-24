unit ObjectIndex;

interface

  uses
    SysUtils, Classes;

  type
    TObjectID = integer;

  type
    PIndexEntry = ^TIndexEntry;
    TIndexEntry =
      packed record
        Id  : TObjectID;
        Obj : TObject;
        Flg : boolean;
      end;

  const
    MaxIndexSixe = $7FFFFFFF  div sizeof(TIndexEntry); // high(integer); Drives Delphi crazy

  // IndexEntry utility function

  function IndexEntry(Id  : TObjectID; Obj : TObject; Flg : boolean) : TIndexEntry;

  type
    TObjectIndex      = class;
    TObjectHash       = class;
    EObjectIndexError = class(Exception);

    PIndexArray = ^TIndexArray;
    TIndexArray = array[0..MaxIndexSixe - 1] of TIndexEntry;

    PEntryHash  = ^TEntryHash;
    TEntryHash  = array[0..255] of pointer;

    TItrEntry   = procedure(var Entry : TIndexEntry) of object;

    TObjectHash =
      class
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fHash : TEntryHash;
        private
          function Hash1(id : integer) : integer;
          function Hash2(id : integer) : integer;
        public
          procedure AddEntry(Entry : TIndexEntry);
          procedure AddObject(Id : TObjectID; Obj : TObject);
          procedure Iterate(proc : TItrEntry);
        private
          function  GetEntry(Id  : TObjectID) : PIndexEntry;
        public
          property Entries[Id  : TObjectID] : PIndexEntry read GetEntry;
      end;

    TObjectIndex =
      class
        public
          constructor Create(InitialSize : integer);
          destructor  Destroy; override;
        private
          fIndexArray : PIndexArray;
          fSize       : integer;
          fCount      : integer;
        public
          procedure AddEntry(Entry : TIndexEntry);
          procedure AddObject(Id : TObjectID; Obj : TObject);
          procedure DeleteObject(Id : TObjectID);
          function  Exists(Id : TObjectID) : boolean;
          procedure Iterate(proc : TItrEntry);
        private
          function  FindId(Id : TObjectID) : integer;
          function  GetPlaceOf(Id : TObjectID) : integer;
          procedure InsertAt(place : integer; Entry : TIndexEntry);
          procedure DeleteAt(place : integer);
          function  GetObject(Id : TObjectID) : TObject;
          function  GetIdIndex(Id : TObjectID) : integer;
          function  GetIndexEntry(index : integer) : TIndexEntry;
          procedure SetIndexEntry(index : integer; Entry : TIndexEntry);
        public
          property Count : integer read fCount write fCount;
          property Index  [Id  : TObjectID] : TObject read GetObject write AddObject; default;
          property IdIndex[Id  : TObjectID] : integer read GetIdIndex;
          property Entries[idx : integer]   : TIndexEntry read GetIndexEntry write SetIndexEntry;
      end;

implementation

  // TObjectIndex

  const
    NoPlace = -1;

  function IndexEntry(Id : TObjectID; Obj : TObject; Flg : boolean) : TIndexEntry;
    begin
      result.Id  := Id;
      result.Obj := Obj;
      result.Flg := Flg;
    end;

  type
    TIntRec =
      record
        b1 : byte;
        b2 : byte;
        b3 : byte;
        b4 : byte;
      end;


  // TObjectHash

  constructor TObjectHash.Create;
    var
      i : integer;
      E : PEntryHash;
    begin
      inherited;
      for i := low(fHash) to high(fHash) do
        begin
          New(E);
          FillChar(E^, sizeof(E^), 0);
          fHash[i] := E;
        end;
    end;

  destructor TObjectHash.Destroy;
    var
      i, j : integer;
      Hash : PEntryHash;
    begin
      for i := low(fHash) to high(fHash) do
        begin
          Hash := PEntryHash(fHash[i]);
          for j := low(Hash^) to high(Hash^) do
            TObjectIndex(Hash[j]).Free;
        end;
    end;

  function TObjectHash.Hash1(id : integer) : integer;
    begin
      with TIntRec(id) do
        result := (b1 + b2 + b3 + b4) mod 256;
    end;

  function TObjectHash.Hash2(id : integer) : integer;
    begin
      with TIntRec(id) do
        result := (b1 + 2*b2 + 3*b3 + b4) mod 256;
    end;

  procedure TObjectHash.AddEntry(Entry : TIndexEntry);
    var
      idx1 : integer;
      idx2 : integer;
      H1   : PEntryHash;
      OIdx : TObjectIndex;
    begin
      idx1 := Hash1(Entry.Id);
      idx2 := Hash2(Entry.Id);
      H1   := fHash[idx1];
      OIdx := TObjectIndex(H1[idx2]);
      if OIdx = nil
        then
          begin
            OIdx := TObjectIndex.Create(256);
            H1[idx2] := OIdx;
          end;
      OIdx.AddEntry(Entry);
    end;

  procedure TObjectHash.AddObject(Id : TObjectID; Obj : TObject);
    begin
      AddEntry(IndexEntry(Id, Obj, false));
    end;

  procedure TObjectHash.Iterate(proc : TItrEntry);
    var
      H    : PEntryHash;
      i, j : integer;
      OIdx : TObjectIndex;
    begin
      for i := low(fHash) to high(fHash) do
        begin
          H := PEntryHash(fHash[i]);
          for j := low(H^) to high(H^) do
            begin
              OIdx := TObjectIndex(H[j]);
              if OIdx <> nil
                then OIdx.Iterate(proc);
            end;
        end;
    end;

  function TObjectHash.GetEntry(Id  : TObjectID) : PIndexEntry;
    var
      idx1, idx2 : integer;
      OIdx       : TObjectIndex;
      eidx       : integer;
    begin
      idx1 := Hash1(Id);
      idx2 := Hash2(Id);
      OIdx := TObjectIndex(PEntryHash(fHash[idx1])[idx2]);
      if OIdx <> nil
        then
          begin
            eidx := OIdx.IdIndex[Id];
            if eidx <> NoPlace
              then result := @OIdx.fIndexArray[eidx]
              else result := nil;
          end
        else result := nil;
    end;


  // TObjectIndex

  constructor TObjectIndex.Create(InitialSize : integer);
    begin
      inherited Create;
      fSize  := InitialSize;
      ReallocMem(fIndexArray, fSize * sizeof(fIndexArray[0]));
    end;

  destructor TObjectIndex.Destroy;
    begin
      ReallocMem(fIndexArray, 0);
      inherited;
    end;

  procedure TObjectIndex.AddEntry(Entry : TIndexEntry);
    var
      place : integer;
    begin
      if fCount = fSize
        then
          begin
            //inc(fSize, fSize * 3 div 10 + 1);
            inc(fSize, fSize div 8);
            ReallocMem(fIndexArray, fSize * sizeof(fIndexArray[0]));
          end;
      place := GetPlaceOf(Entry.Id);
      if place = fCount
        then
          begin
            fIndexArray[fCount] := Entry;
            inc(fCount);
          end
        else
          begin
            {$IFDEF DEBUGGING}
            Assert((place < fCount) and (fIndexArray[place].Id <> Id), 'Trying to reinsert an object in the index...');
            {$ENDIF}
            InsertAt(place, Entry);
            inc(fCount);
          end;
    end;

  procedure TObjectIndex.AddObject(Id : TObjectID; Obj : TObject);
    begin
      AddEntry(IndexEntry(Id, Obj, false));
    end;

  procedure TObjectIndex.DeleteObject(Id : TObjectID);
    var
      place : integer;
    begin
      place := FindId(Id);
      if (place <> NoPlace) and (fIndexArray[place].Id = Id)
        then
          begin
            DeleteAt(place);
            dec(fCount);
          end;
    end;

  function TObjectIndex.Exists(Id : TObjectID) : boolean;
    var
      place : integer;
    begin
      place  := FindId(Id);
      result := (place <> NoPlace) and (fIndexArray[place].Id = Id);
    end;

  procedure TObjectIndex.Iterate(proc : TItrEntry);
    var
      i : integer;
    begin
      for i := 0 to pred(fCount) do
        proc(fIndexArray[i]);
    end;

  function TObjectIndex.FindId(Id : TObjectID) : integer;
    var
      l : integer;
      m : integer;
      h : integer;
      c : integer;
    begin
      if fCount > 0
        then
          begin
            l := 0;
            h := pred(fCount);
            repeat
              m := (l + h) div 2;
              c := fIndexArray[m].Id;
              if Id >= c
                then l := m
                else h := m
            until (h - l <= 1) or (c = Id);
            if Id = fIndexArray[l].Id
              then result := l
              else result := h
          end
        else result := NoPlace;
    end;

  function TObjectIndex.GetPlaceOf(Id : TObjectID) : integer;
    begin
      if (fCount = 0) or (fIndexArray[0].Id >= Id)
        then result := 0
        else
          if fIndexArray[pred(fCount)].Id <= Id
            then result := fCount
            else result := FindId(Id);
    end;

  procedure TObjectIndex.InsertAt(place : integer; Entry : TIndexEntry);
    begin
      move(fIndexArray[place], fIndexArray[place+1], (fCount - place)*sizeof(TIndexEntry));
      fIndexArray[place] := Entry;
    end;

  procedure TObjectIndex.DeleteAt(place : integer);
    begin
      move(fIndexArray[place+1], fIndexArray[place], fCount - place - 1);
    end;

  function TObjectIndex.GetObject(Id : TObjectID) : TObject;
    var
      place : integer;
    begin
      place := GetPlaceOf(Id);
      if place <> NoPlace
        then result := fIndexArray[place].Obj
        else result := nil;
    end;

  function TObjectIndex.GetIdIndex(Id : TObjectID) : integer;
    var
      Last : TObjectID;
    begin
      if fCount > 0
        then
          begin
            Last := fIndexArray[pred(fCount)].Id;
            if Id = Last
              then
                result := pred(fCount)
              else
                if Id < Last
                  then
                    begin
                      result := FindId(Id);
                      if fIndexArray[result].Id <> Id
                        then result := NoPlace;
                    end
                  else result := NoPlace
          end
        else result := NoPlace;
    end;

  function TObjectIndex.GetIndexEntry(index : integer) : TIndexEntry;
    begin
      if index < fCount
        then result := fIndexArray[index]
        else raise EObjectIndexError.Create('');
    end;

  procedure TObjectIndex.SetIndexEntry(index : integer; Entry : TIndexEntry);
    begin
      if index < fCount
        then fIndexArray[index] := Entry 
        else raise EObjectIndexError.Create('');
    end;


end.
