unit BuildClasses;

interface

  uses
    MapTypes;

  type
    TFastIndex  = array[idBuilding] of word;
    TClassArray = array[word] of TBuildingClass;

  type
    TBuildingClasses =
      class(TInterfacedObject, IBuildingClassBag)
        public
          destructor Destroy; override;
        private // IBuildingClassBag
          procedure Add(const which : TBuildingClass);
          function  Get(id : idBuilding) : PBuildingClass;
          function  GetMaxId : idBuilding;
          procedure Clear;
        private
          fIndexes : integer;
          fCount   : integer;
          fLimit   : integer;
          fMaxId   : integer;
          fIndex   : ^TFastIndex;
          fItems   : ^TClassArray;
      end;

implementation

  // TBuildingClasses

  destructor TBuildingClasses.Destroy;
    begin
      assert(RefCount = 0);
      Clear;
      inherited;
    end;

  procedure TBuildingClasses.Add(const which : TBuildingClass);
    const
      cIndexDelta = 8;
      cClassDelta = 32;
    begin
      if which.id >= fIndexes
        then
          begin
            reallocmem(fIndex, (which.id + cIndexDelta)*sizeof(fIndex[0]));
            fillchar(fIndex[fIndexes], (which.id + cIndexDelta - fIndexes)*sizeof(fIndex[0]), $FF);
            fIndexes := which.id + cIndexDelta;
          end;
      if which.id > fMaxId
        then fMaxId := which.id;
      if fIndex[which.id] <> $FFFF
        then
          begin
            assert(fIndex[which.id] < fCount);
            fItems[fIndex[which.id]] := which;
          end
        else
          begin
            if fCount = fLimit
              then
                begin
                  reallocmem(fItems, (fLimit + cClassDelta)*sizeof(fItems[0]));
                  initialize(fItems[fLimit], cClassDelta);
                  inc(fLimit, cClassDelta);
                end;
            fIndex[which.id] := fCount;
            fItems[fCount]   := which;
            inc(fCount);
          end;
    end;

  function TBuildingClasses.Get(id : idBuilding) : PBuildingClass;
    begin
      if (id < fIndexes) and (fIndex[id] <> $FFFF)
        then Result := @fItems[fIndex[id]]
        else Result := nil;
    end;

  function TBuildingClasses.GetMaxId : idBuilding;
    begin
      Result := fMaxId;
    end;

  procedure TBuildingClasses.Clear;
    var
      i : integer;
    begin
      if fIndexes > 0
        then
          begin
            freemem(fIndex);
            fIndexes := 0;
            fIndex   := nil;
          end;
      if fCount > 0
        then
          begin
            for i := 0 to pred(fCount) do
              begin
                with fItems[i].SoundData do
                  if (Kind <> ssNone) and (Sounds <> nil)
                    then
                      begin
                        finalize(Sounds^, Count);
                        freemem(Sounds);
                      end;
                with fItems[i].EfxData do
                  if Efxs <> nil
                    then freemem(Efxs);
              end;
            finalize(fItems[0], fCount);
            freemem(fItems);
            fCount := 0;
            fLimit := 0;
            fItems := nil;
          end;

    end;

end.
