unit ResidentialTasks;

interface

  uses
    Tasks, Kernel, Tutorial, Population, BuildFacilitiesTask;

  const
    tidTask_BuildResidentials      = 'BuildRes';
    tidTask_HighClassResidential   = 'hiRes';
    tidTask_MiddleClassResidential = 'midRes';
    tidTask_LowClassResidential    = 'lowRes';

  type
    TMetaResidentialsTask =
      class(TMetaBuildFacilitiesTask)
        private
          fKind : TPeopleKind;
        public
          property Kind : TPeopleKind read fKind  write fKind;
      end;

    TBuildResidentialsTask =
      class(TSuperTask)
        protected
          class function GetPriority(MetaTask : TMetaTask; SuperTask : TTask; Context : ITaskContext) : integer; override;
      end;

    TBuildSpecificResidentialsTask =
      class(TBuildFacilitiesTask)
        protected
          class function GetPriority(MetaTask : TMetaTask; SuperTask : TTask; Context : ITaskContext) : integer; override;
          procedure DefineTarget; override;
      end;

  // Return how many houses requires this town
  function  HousesRequired(TownHall : TTownHall; kinds : array of TPeopleKind) : integer;

  procedure RegisterBackup;

implementation


  uses
    MathUtils, FacIds, BackupInterfaces;

  type
    TDemandToHouse = array[TPeopleKind] of integer;
    TMinPopulation = array[TPeopleKind] of integer;

  const
    Demands   : TDemandToHouse = (50, 80, 100);
    MinPeople : TMinPopulation = (50, 100, 300);

  const
    MaxResidentialTiBuild = 3;

  function HousesRequired(TownHall : TTownHall; kinds : array of TPeopleKind) : integer;
    var
      rdInput : TInput;
      wdInput : TInput;
      i       : integer;
      HsRq    : integer;
      ppd     : TFluidValue;
      rsd     : TFluidValue;
      ratio   : single;
    begin
      result := 0;
      for i := low(kinds) to high(kinds) do
        if TownHall.Unemployment[kinds[i]] = 0
          then
            begin
              rdInput := TownHall.InputsByName[PeopleKindPrefix[kinds[i]] + tidGate_ResDemand];
              wdInput := TownHall.InputsByName[PeopleKindPrefix[kinds[i]] + tidGate_WorkDemand];
              if (rdInput <> nil) and (wdInput <> nil)
                then
                  begin
                    rsd := rdInput.LastValue.Q;
                    ppd := WorkForceToPeople(kinds[i], wdInput.LastValue.Q);
                    if rsd = 0
                      then ratio := 0
                      else ratio := ppd/rsd;
                    if ratio >= 0.5
                      then HsRq := min(3, round((ppd - rsd)/Demands[kinds[i]]))
                      else
                        if TownHall.LastPop[kinds[i]].Q < MinPeople[kinds[i]]
                          then HsRq := 1
                          else HsRq := 0;
                    inc(result, HsRq);
                  end;
            end;
    end;


  // TBuildResidentialsTask

  class function TBuildResidentialsTask.GetPriority(MetaTask : TMetaTask; SuperTask : TTask; Context : ITaskContext) : integer;
    var
      Town : TInhabitedTown;
    begin
      result := inherited GetPriority(MetaTask, SuperTask, Context);
      if result <> tprIgnoreTask
        then
          begin
            Town := TInhabitedTown(Context.getContext(tcIdx_Town));
            if (Town = nil) or (HousesRequired(TTownHall(Town.TownHall.CurrBlock), [pkHigh, pkMiddle, pkLow]) = 0)
              then result := tprIgnoreTask;
          end;
    end;

  // TBuildSpecificResidentialsTask

  class function TBuildSpecificResidentialsTask.GetPriority(MetaTask : TMetaTask; SuperTask : TTask; Context : ITaskContext) : integer;
    var
      Town  : TInhabitedTown;
      count : integer;
    begin
      result := inherited GetPriority(MetaTask, SuperTask, Context);
      if result <> tprIgnoreTask
        then
          begin
            Town  := TInhabitedTown(Context.getContext(tcIdx_Town));
            count := HousesRequired(TTownHall(Town.TownHall.CurrBlock), TMetaResidentialsTask(MetaTask).Kind);
            if count > 0
              then result := MetaTask.Priority + count
              else result := tprIgnoreTask;
          end;
    end;

  procedure TBuildSpecificResidentialsTask.DefineTarget;
    var
      Town : TInhabitedTown;
    begin
      Town     := TInhabitedTown(Context.getContext(tcIdx_Town));
      ReqCount := HousesRequired(TTownHall(Town.TownHall.CurrBlock), TMetaResidentialsTask(MetaTask).Kind);
    end;


  procedure RegisterBackup;
    begin
      RegisterClass( TBuildResidentialsTask );
      RegisterClass( TBuildSpecificResidentialsTask );
    end;

end.
