unit HeadquarterTasks;

interface

  uses
    Tasks, Kernel, Tutorial, BuildFacilitiesTask, CacheAgent, BackupInterfaces, Inventions;

  {const
    tidTask_MainHeadquarter = 'MainHq';
    tidTask_OffcHeadquarter = 'OfficesHq';
    tidTask_IndHeadquarter  = 'IndustriesHq';
    tidTask_CommHeadquarter = 'CommerceHq';
    tidTask_PublHeadquarter = 'PublicHq';}

  const
    stgMainHqBuildRes    = 3;
    stgMainHqWaitResConn = 4;
    stgMainHqWaitReserch = 5;

  type
    TMetaHeadquarterTask =
      class(TMetaBuildFacilitiesTask)
        public
          constructor Create(srcTaskId, newId, newSuperTaskId : string); overload;
        private
          fTechnology  : string;
          fInventionId : TInventionNumId;
        private
          procedure SetTechnology(aTech : string);
        public
          property Technology  : string read fTechnology write SetTechnology;
          property InventionId : TInventionNumId read fInventionId;
      end;

    TBuildHeadquarterTask =
      class(TBuildFacilitiesTask)
        protected
          class function GetPriority(MetaTask : TMetaTask; SuperTask : TTask; Context : ITaskContext) : integer; override;
          procedure Initialize; override;
          procedure Activate;   override;
        public
          function Execute : TTaskResult; override;
      end;

    THouseRequired = array[TPeopleKind] of byte;
    TPeopleCapcity = array[TPeopleKind] of byte;

    TBuildMainHeadquarterTask =
      class(TBuildHeadquarterTask)
        private
          fHouseReq  : THouseRequired;
          fHouseDone : THouseRequired;
          fFacTrack  : byte;
        public
          function Execute : TTaskResult; override;
        private
          procedure MsgBuildFacility(var Msg : TMsgNewBlock); message msgKernel_NewBlock;
          procedure MsgDelFacility(var Msg : TMsgFacilityDeleted); message msgKernel_FacilityDeleted;
          //function  SetRequiredHouses : boolean;
          //function  HouseRequired : boolean;
        public
          procedure StoreToCache(Prefix : string; Cache : TObjectCache); override;
        public
          procedure LoadFromBackup(Reader : IBackupReader); override;
          procedure StoreToBackup (Writer : IBackupWriter); override;
          procedure Activate; override;
      end;

  procedure RegisterBackup;

implementation

  uses
    FacIds, TaskUtils, ModelServerCache, WorkCenterBlock, MathUtils, Population,
    ResidentialTasks, ClassStorage;

  const
    TrackMax = 2;

  const
    HouseCapacity : TPeopleCapcity = (20, 30, 50);

  // TMetaHeadquarterTask

  constructor TMetaHeadquarterTask.Create(srcTaskId, newId, newSuperTaskId : string);
    var
      Source : TMetaHeadquarterTask;
    begin
      inherited Create(srcTaskId, newId, newSuperTaskId);
      Source := TMetaHeadquarterTask(TheClassStorage.ClassById[tidClassFamily_Tasks, srcTaskId]);
      fInventionId := Source.fInventionId;
      fTechnology  := Source.fTechnology;
    end;

  procedure TMetaHeadquarterTask.SetTechnology(aTech : string);
    var
      Invention : TInvention;
    begin
      fTechnology := aTech;
      Invention := TInvention(TheClassStorage.ClassById[tidClassFamily_Inventions, aTech]);
      if Invention <> nil
        then fInventionId := Invention.NumId;
    end;

  // TBuildHeadquarterTask

  class function TBuildHeadquarterTask.GetPriority(MetaTask : TMetaTask; SuperTask : TTask; Context : ITaskContext) : integer;
    var
      Company : TCompany;
    begin
      result := inherited GetPriority(MetaTask, SuperTask, Context);
      if result <> tprIgnoreTask
        then
          begin
            Company := TCompany(Context.getContext(tcIdx_Company));
            if (Company <> nil) and Company.HasInvention[TMetaHeadquarterTask(MetaTask).InventionId]
              then result := tprAccomplished;
          end
    end;

  function TBuildHeadquarterTask.Execute : TTaskResult;
    var
      MetaTask : TMetaHeadquarterTask;
      Company  : TCompany;
    begin
      result := inherited Execute;
      MetaTask := TMetaHeadquarterTask(Self.MetaTask);
      if MetaTask.InventionId <> 0
        then
          begin
            Company  := TCompany(Context.getContext(tcIdx_Company));
            if (Company <> nil) and Company.HasInvention[MetaTask.InventionId]
              then result := trFinished
              else result := trContinue;
          end;
    end;

  procedure TBuildHeadquarterTask.Initialize;
    var
      Company : TCompany;
      Fac     : TFacility;
    begin
      Company := TCompany(Context.getContext(tcIdx_Company));
      if (Company <> nil ) and TaskUtils.FindFacility(TMetaHeadquarterTask(MetaTask).FacId, Company, Fac)
        then
          if (Fac.CurrBlock.ClassName = tidConstBlockClassName) and not TaskUtils.ConstructionConnected(Fac.CurrBlock)
            then
              begin
                fFacs[0] := Fac;
                Stage    := stgConnect;
                fCount   := 1;
              end
            else
              begin
                if MetaTask.StageCount >= stgWait
                  then
                    begin
                      Stage := stgWait;
                      UpdateObjectCache(Context.getContext(tcIdx_Tycoon), -1, -1);
                    end;
              end;
    end;

  procedure TBuildHeadquarterTask.Activate;
    begin
      if Stage <> stgWait
        then inherited Activate; // >> this is a patch...
    end;


  // TBuildMainHeadquarterTask

  function TBuildMainHeadquarterTask.Execute : TTaskResult;
    begin
      result := inherited Execute;
      {
      if result = trContinue
        then
          if fFacTrack <> 0
            then
              if fFacTrack = TrackMax
                then
                  begin
                    if not HouseRequired
                      then
                        begin
                          if Stage <> stgMainHqWaitReserch
                            then
                              begin
                                Stage := stgMainHqWaitReserch;
                                UpdateObjectCache(Context.getContext(tcIdx_Tycoon), -1, -1);
                              end;
                          result := trContinue;
                        end
                      else
                        begin
                          if (fCount = fReqCount) and (Stage <> stgMainHqWaitResConn)
                            then
                              if not AllConnected
                                then
                                  begin
                                    Stage := stgMainHqWaitResConn;
                                    //NotifyTycoon('');
                                  end;
                          result := trContinue;
                        end;
                  end
                else
                  begin
                    inc(fFacTrack);
                    if fFacTrack = TrackMax
                      then
                        begin
                          if SetRequiredHouses
                            then Stage := stgMainHqBuildRes
                            else Stage := stgMainHqWaitReserch;
                          //NotifyTycoon('');
                        end;
                    result := trContinue;
                  end;
      }
    end;

  procedure TBuildMainHeadquarterTask.MsgBuildFacility(var Msg : TMsgNewBlock);
    begin
      if Msg.Block <> nil
        then
          case Msg.Block.Facility.MetaFacility.FacId of
            FID_MainHeadquarter :
              begin
                if Context.getContext(tcIdx_Company) = nil
                  then Context.setContext(tcIdx_Company, Msg.Block.Facility.Company);
                Context.setContext(tcIdx_Town, Msg.Block.Facility.Town);
                if (fFacTrack = 0) and (Msg.Block.ClassName = 'TMainHeadquarter')
                  then fFacTrack := 1;
                inherited;
              end;
            FID_hiClassLoCost:
              if fHouseDone[pkHigh] < fHouseReq[pkHigh]
                then
                  if Msg.Block.ClassName = 'TPopulatedBlock'
                    then fHouseDone[pkHigh] := fHouseDone[pkHigh] + 1
                    else
                      if (Msg.Block.ClassName = tidConstBlockClassName)
                        then AddFacility(Msg.Block.Facility, TaskUtils.ConstructionConnected(Msg.Block));
            FID_midClassLoCost:
              if fHouseDone[pkMiddle] < fHouseReq[pkMiddle]
                then
                  if Msg.Block.ClassName = 'TPopulatedBlock'
                    then fHouseDone[pkMiddle] := fHouseDone[pkMiddle] + 1
                    else
                      if (Msg.Block.ClassName = tidConstBlockClassName)
                        then AddFacility(Msg.Block.Facility, TaskUtils.ConstructionConnected(Msg.Block));
            FID_lowClassLoCost:
              if fHouseDone[pkLow] < fHouseReq[pkLow]
                then
                  if Msg.Block.ClassName = 'TPopulatedBlock'
                    then fHouseDone[pkLow] := fHouseDone[pkLow] + 1
                    else
                      if (Msg.Block.ClassName = tidConstBlockClassName)
                        then AddFacility(Msg.Block.Facility, TaskUtils.ConstructionConnected(Msg.Block));
          end;
    end;

  procedure TBuildMainHeadquarterTask.MsgDelFacility(var Msg : TMsgFacilityDeleted);
    var
      idx : integer;
    begin
      idx := FacilityIndex(Msg.Facility);
      case Msg.Facility.MetaFacility.FacId of
        FID_MainHeadquarter :
          if idx >= 0
            then
              begin
                fFacTrack := 0;
                ReqCount  := 1;
                fFacs[0]  := Msg.Facility;
                inherited;
              end;
        FID_hiClassLoCost:
          begin
            if DelFacility(Msg.Facility) and (Msg.Facility.CurrBlock.ClassName = 'TPopulatedBlock')
              then
                begin
                  fHouseDone[pkHigh] := min(0, fHouseDone[pkHigh] - 1);
                  Stage := stgMainHqBuildRes;
                  NotifyTycoon('');
                end;
          end;
        FID_midClassLoCost:
          begin
            if DelFacility(Msg.Facility) and (Msg.Facility.CurrBlock.ClassName = 'TPopulatedBlock')
              then
                begin
                  fHouseDone[pkMiddle] := min(0, fHouseDone[pkMiddle] - 1);
                  Stage := stgMainHqBuildRes;
                  NotifyTycoon('');
                end;
          end;
        FID_lowClassLoCost:
          begin
            if DelFacility(Msg.Facility) and (Msg.Facility.CurrBlock.ClassName = 'TPopulatedBlock')
              then
                begin
                  fHouseDone[pkLow] := min(0, fHouseDone[pkLow] - 1);
                  Stage := stgMainHqBuildRes;
                  NotifyTycoon('');
                end;
          end;
      end;
    end;

  {function TBuildMainHeadquarterTask.SetRequiredHouses : boolean;
    var
      Company : TCompany;
      //Town    : TInhabitedTown;
      Fac     : TFacility;
      kind    : TPeopleKind;
      HCnt    : integer;
    begin
      HCnt := 0;
      if not fWFChecked
        then
          begin
            fWFChecked := true;
            Company := TCompany(Context.getContext(tcIdx_Company));
            //Town    := TInhabitedTown(Context.getContext(tcIdx_Town));
            if TaskUtils.FindFacility(FID_MainHeadquarter, Company, Fac)
              then
                for kind := low(kind) to high(kind) do
                  if TWorkCenter(Fac.CurrBlock).Workers[kind].Q < TWorkCenter(Fac.CurrBlock).WorkersMax[kind].Q
                    then
                      begin
                        fHouseReq[kind] := 0;//ResidentialTasks.HousesRequired(TTownHall(Town.TownHall.CurrBlock), [kind]);
                        inc(HCnt, fHouseReq[kind]);
                      end
                    else fHouseReq[kind] := 0;
          end
        else
          for kind := low(kind) to high(kind) do
            inc(HCnt, min(3, fHouseReq[kind] - fHouseDone[kind]));
      if HCnt > 0
        then
          begin
            ReqCount := HCnt + 1;
            fCount   := 1;
            fFacs[0] := Fac;
            result   := true;
          end
        else result := false;
    end;}

  {function TBuildMainHeadquarterTask.HouseRequired : boolean;
    var
      kind : TPeopleKind;
      cnt  : integer;
      done : integer;
    begin
      cnt  := 0;
      done := 0;
      for kind := low(kind) to high(kind) do
        begin
          inc(cnt,  fHouseReq [kind]);
          inc(done, fHouseDone[kind]);
        end;
      result := cnt > done;
    end;}

  procedure TBuildMainHeadquarterTask.StoreToCache(Prefix : string; Cache : TObjectCache);
    var
      kind : TPeopleKind;
    begin
      inherited;
      for kind := low(kind) to high(kind) do
        Cache.WriteInteger(Prefix + PeopleKindPrefix[kind] + 'Req', min(3, fHouseReq[kind] - fHouseDone[kind]));
    end;

  procedure TBuildMainHeadquarterTask.LoadFromBackup(Reader : IBackupReader);
    begin
      inherited;
      Reader.ReadBuffer('HouseReq', fHouseReq, nil, sizeof(fHouseReq));
      Reader.ReadBuffer('HouseDone', fHouseDone, nil, sizeof(fHouseDone));
      fFacTrack  := Reader.ReadByte('FacTrack', 0);
    end;

  procedure TBuildMainHeadquarterTask.StoreToBackup (Writer : IBackupWriter);
    begin
      inherited;
      Writer.WriteBuffer('HouseReq', fHouseReq, sizeof(fHouseReq));
      Writer.WriteBuffer('HouseDone', fHouseDone, sizeof(fHouseDone));
      Writer.WriteByte('FacTrack', fFacTrack);
    end;

  procedure TBuildMainHeadquarterTask.Activate;
    begin
      if Stage <> stgMainHqWaitReserch
        then inherited Activate;
    end;


  // RegisterBackup

  procedure RegisterBackup;
    begin
      RegisterClass( TBuildHeadquarterTask );
      RegisterClass( TBuildMainHeadquarterTask );
    end;


end.
