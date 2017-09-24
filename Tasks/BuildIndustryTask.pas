unit BuildIndustryTask;

interface

  uses
    Tasks, Kernel, Collection, BuildFacilitiesTask, FacIds;

  type
    PFacIdArray = ^TFacIdArray;
    TFacIdArray = array[0..0] of TFacId;

  type
    TMetaChoice =
      class
        public
          constructor Create(aBlockClass, aServiceId : string; FacIds : array of TFacId);
          destructor  Destroy; override;
        private
          fBlockClass : string;
          fServiceId  : string;
          fFacIds     : PFacIdArray;
          fCount      : integer;
        public
          property BlockClass : string      read fBlockClass;
          property ServiceId  : string      read fServiceId;
          property FacIds     : PFacIdArray read fFacIds;
          property FacIdCount : integer     read fCount;
      end;

    TMetaBuildIndustryTask =
      class(TMetaBuildFacilitiesTask)
        public
          constructor Create(anId, aName, aDesc, aSuperTaskId : string; aPeriod : integer; aTaskClass : CTask);
          destructor  Destroy; override;
        private
          fMetaChoices : TCollection;
        public
          procedure AddChoice(Choice : TMetaChoice);
        private
          function GetChoiceCount : integer;
          function GetChoice(index : integer) : TMetaChoice;
        public
          property ChoiceCount : integer read GetChoiceCount;
          property Choices[index : integer] : TMetaChoice read GetChoice;
      end;

    TBuildIndustryTask =
      class(TBuildFacilitiesTask)
        private
          fChoiceIndex : integer;
        protected
          procedure DefineTarget; override;
          function  GetTargetBlock : string; override;
      end;

implementation

  uses
    SysUtils, ServiceInfo, Population, MathUtils, TaskUtils;

  type
    PChoiceIndex = ^TChoiceIndex;
    TChoiceIndex = array[0..0] of integer;

  // TMetaChoice

  constructor TMetaChoice.Create(aBlockClass, aServiceId : string; FacIds : array of TFacId);
    begin
      inherited Create;
      fBlockClass := aBlockClass;
      fServiceId  := aServiceId;
      if FacIds[0] = FID_None
        then fCount := 0
        else
          begin
            fCount := high(FacIds) - low(FacIds) + 1;
            GetMem(fFacIds, fCount*sizeof(fFacIds[0]));
            move(FacIds, fFacIds^, fCount*sizeof(fFacIds[0]));
          end;
    end;

  destructor TMetaChoice.Destroy;
    begin
      if fFacIds <> nil
       then FreeMem(fFacIds);
      inherited;
    end;

  // TMetaBuildIndustryTask

  constructor TMetaBuildIndustryTask.Create(anId, aName, aDesc, aSuperTaskId : string; aPeriod : integer; aTaskClass : CTask);
    begin
      inherited Create(anId, aName, aDesc, aSuperTaskId, aPeriod, aTaskClass);
      fMetaChoices := TCollection.Create(0, rkBelonguer);
    end;

  destructor TMetaBuildIndustryTask.Destroy;
    begin
      fMetaChoices.Free;
      inherited;
    end;

  procedure TMetaBuildIndustryTask.AddChoice(Choice : TMetaChoice);
    begin
      fMetaChoices.Insert(Choice);
    end;

  function TMetaBuildIndustryTask.GetChoiceCount : integer;
    begin
      result := fMetaChoices.Count;
    end;

  function TMetaBuildIndustryTask.GetChoice(index : integer) : TMetaChoice;
    begin
      result := TMetaChoice(fMetaChoices[index]);
    end;

  // TBuildIndustryTask

  procedure TBuildIndustryTask.DefineTarget;
    var
      Indexes  : PChoiceIndex;
      Ratios   : PChoiceIndex;
      count    : integer;
      i, j     : integer;
      TownHall : TTownHall;
      Choice   : TMetaChoice;
      Service  : TServiceInfo;

    procedure Swap(var v1, v2 : integer);
      var
        t : integer;
      begin
        t  := v1;
        v1 := v2;
        v2 := t;
      end;

    function PossibleChoice(idx : integer) : boolean;
      begin
        Choice := TMetaBuildIndustryTask(MetaTask).Choices[idx];
        result := (Choice.FacIdCount = 0) or TaskUtils.TownHasFacilities(TTown(Context.getContext(tcIdx_Town)), Slice(Choice.FacIds^, Choice.FacIdCount));
        if Choice.FacIdCount <> 0
          then
          else result := true;
      end;

    begin
      count := TMetaBuildIndustryTask(MetaTask).ChoiceCount;
      GetMem(Indexes, count*sizeof(Indexes[0]));
      GetMem(Ratios, count*sizeof(Ratios[0]));
      try
        FillChar(Indexes^, count*sizeof(Indexes[0]), 0);
        TownHall := TTownHall(TInhabitedTown(Context.getContext(tcIdx_Town)).TownHall);
        // Get the ratios
        for i := 0 to pred(count) do
          begin
            Choice  := TMetaBuildIndustryTask(MetaTask).Choices[i];
            Service := TServiceInfo(TownHall.ServiceInfoById[Choice.ServiceId]);
            if Service <> nil
              then Ratios[i] := min(100, round(100*Service.Ratio))
              else Ratios[i] := 100;
            Indexes[i] := i;
          end;
        // Sort the indexes usising bubble sort
        for i := 0 to pred(pred(count)) do
          for j := succ(i) to pred(count) do
            if Ratios[i] > Ratios[j]
              then
                begin
                  Swap(Ratios[i], Ratios[j]);
                  Swap(Indexes[i], Indexes[j]);
                end;
        // Get the best choice
        i := 0;
        while (i < count) and not PossibleChoice(i) do
          inc(i);
        if i < count
          then fChoiceIndex := i
          else fChoiceIndex := -1;
      finally
        FreeMem(Indexes);
        FreeMem(Ratios);
      end;
    end;

  function TBuildIndustryTask.GetTargetBlock : string;
    begin
      if fChoiceIndex <> -1
        then result := TMetaBuildIndustryTask(MetaTask).Choices[fChoiceIndex].BlockClass
        else result := '';
    end;

end.
