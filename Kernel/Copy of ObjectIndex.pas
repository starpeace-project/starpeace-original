unit ObjectIndex;

interface

  uses
    SysUtils, Classes;

  type
    TObjectID = integer;

  type
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
    EObjectIndexError = class(Exception);

    PIndexArray = ^TIndexArray;
    TIndexArray = array[0..MaxIndexSixe - 1] of TIndexEntry;

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
