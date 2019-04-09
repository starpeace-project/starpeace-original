unit WorkCenterBlock;

interface

  uses
    ClassStorageInt, Kernel, Population, CacheAgent, BackupInterfaces, ConnectedBlock,
    Surfaces, Accounts, Inventions, Languages, Variants;

  const
    MinWorkForcePerc = 5;
    noWFIndex        = 255;

  const
    modStrengthOfPeople = 5;

  const
    tidInvAttr_WorkQ     = 'workq';
    tidInvAttr_WorkBoost = 'workboost';

  const
    UpgradeWFReduction = 1; // >> most change later  

  type
    TPeopleSupportArray = array[TPeopleKind] of single;
    TWorkForceInportances = array[TPeopleKind] of byte;

  const
    WorkCenterSupports   : TPeopleSupportArray = (1/5, 1/20, 1/100);
    WorkForceInportances : TWorkForceInportances = (5, 3, 1);

  type
    TWorkForceIndex =
      record
        workersIn  : byte;
        workersOut : byte;
        demand     : byte;
        recycle    : byte;
        workersRep : byte;
      end;

  type
    TSalaries            = array[TPeopleKind] of TMoney;
    TWorkForceWeights    = array[TPeopleKind] of byte;
    PActualSalaries      = ^TActualSalaries;
    TActualSalaries      = array[TPeopleKind] of TPercent;
    TWorkForceCapacities = array[TPeopleKind] of TFluidValue;
    TWorkForceIndexes    = array[TPeopleKind] of TWorkForceIndex;
    TWorkForceParams     = array[TPeopleKind] of TTownParameter;

  type
    TMetaWorkCenter =
      class( TMetaBlock )
        public
          constructor Create( anId           : string;
                              aCapacities    : array of TFluidValue;
                              aSupplyAccount : TAccountId;
                              aProdAccount   : TAccountId;
                              aSalaryAccount : TAccountId;
                              aBlockClass    : CBlock );
        private
          fSalaries        : TSalaries;
          fCapacity        : TWorkForceCapacities;
          fWFWeights       : TWorkForceWeights;
          fWFIndexes       : TWorkForceIndexes;
          fMinWFRequired   : single;
          fMinCompSup      : single;
          fSalaryAccount   : TAccountId;
          fHasPeopleCargo  : boolean;
        private
          function GetWorkForceCost : TMoney;
        public
          property Salaries        : TSalaries read fSalaries write fSalaries;
          property Capacity        : TWorkForceCapacities read fCapacity;
          property WFWeights       : TWorkForceWeights read fWFWeights write fWFWeights;
          property MinWFRequired   : single read fMinWFRequired write fMinWFRequired;
          property WFIndexes       : TWorkForceIndexes read fWFIndexes write fWFIndexes;
          property MinCompSup      : single read fMinCompSup write fMinCompSup;
          property SalaryAccount   : TAccountId read fSalaryAccount;
          property WorkForceCost   : TMoney read GetWorkForceCost;
          property HasPeopleCargo  : boolean read fHasPeopleCargo write fHasPeopleCargo;
        public
          procedure Register( ClassFamily : TClassFamilyId );
      end;

    PPeopleInArray  = ^TPeopleInArray;
    PPeopleOutArray = ^TPeopleOutArray;
    PPeopleArray    = ^TPeopleArray;

    TWorkCenter =
      class( TConnectedBlock )
        protected
          constructor Create( aMetaBlock : TMetaBlock; aFacility : TFacility ); override;
        public
          destructor  Destroy; override;
        private
          fWorkers    : TPeopleArray;
          fWorkersMax : TPeopleArray;
        protected
          fWorkForceIn  : TPeopleInArray;
          fWorkForceOut : TPeopleInArray;
          fWorkDemand   : TPeopleOutArray;
          fWorkRecycle  : TPeopleOutArray;
          fWorkersRep   : TPeopleOutArray;
          fWFParams     : TWorkForceParams;
        protected
          function Evaluate : TEvaluationResult; override;
        public
          procedure AutoConnect( loaded : boolean ); override;
          procedure Stop;        override;
        private
          fSalaries   : TActualSalaries;
          fWFRequired : TPeopleKind;
        private
          fPeopleModifier : array [TPeopleKind] of TSurfaceModifier;
        private
          function GetWorkers    : PPeopleArray;
          function GetWorkersMax : PPeopleArray;
          function GetSalaries   : PActualSalaries;
        public
          property WFRequired : TPeopleKind     read fWFRequired;
          property Workers    : PPeopleArray    read GetWorkers;
          property WorkersMax : PPeopleArray    read GetWorkersMax;
          property Salaries   : PActualSalaries read GetSalaries;
        private
          fExtraAdmitance : array[TPeopleKind] of TAdmitance;
        private
          function  GetExtraAdmitance( kind : TPeopleKind ) : TAdmitance;
          procedure SetExtraAdmitance( kind : TPeopleKind; value : TAdmitance );
        protected
          property ExtraAdmitance[kind : TPeopleKind] : TAdmitance read GetExtraAdmitance write SetExtraAdmitance;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        public
          procedure StoreToCache( Cache : TObjectCache ); override;
        published
          procedure RDOSetSalaries( hiSal, miSal, loSal : integer );
          function  RDOTransferWorkForce( kind : integer; ToFacility : TFacility ) : OleVariant;
          function  RDOGetWorkers(kind : integer) : OleVariant;
        protected
          function WorkForceEfficiency    : single;
          function MinWorkForceEfficiency : single;
          function WorkForceRatioAvg      : single;
        public
          function  WorkForceCost(kind : TPeopleKind) : TMoney;
        protected
          procedure HireWorkForce( OpRatio : single );
          procedure ClearInputs;
          procedure CopySettingsFrom(Block : TBlock; Options : integer); override;
          function  RenderCloneMenu(lang : string) : string; override;
          procedure BlockLoaded; override;
          procedure Deleted; override;
      end;

    TFinanciatedWorkCenter =
      class( TWorkCenter )
        protected
          function Evaluate : TEvaluationResult; override;
        public
          procedure AdjustMinimunWages(perc : TPercent);
      end;

    TWorkCenterInvention =
      class(TInvention)
        public
          constructor Load(xmlObj : OleVariant); override;
        //private
          //fWorkQ     : integer;
          //fWorkBoost : integer;
        //public
          //property WorkQ     : integer read fWorkQ;
          //property WorkBoost : integer read fWorkBoost;
        public
          function GetClientProps(Company : TObject; LangId : TLanguageId) : string; override;
      end;

  const                                                            
    MaxSalary = high(TPercent);
    MaxMovers = 0.005;

  procedure RegisterBackup;

implementation

  uses
    ClassStorage, SysUtils, MathUtils, PyramidalModifier, Classes, Protocol, SimHints, 
    MetaInstances, Logs, CloneOptions;


  // TMetaWorkCenter

  constructor TMetaWorkCenter.Create( anId           : string;
                                      aCapacities    : array of TFluidValue;
                                      aSupplyAccount : TAccountId;
                                      aProdAccount   : TAccountId;
                                      aSalaryAccount : TAccountId;
                                      aBlockClass    : CBlock );
    var
      i      : TPeopleKind;
      Sample : TWorkCenter;
      MF     : TMetaFluid;
      idxIn  : integer;
      idxOut : integer;
    begin
      inherited Create( anId, aSupplyAccount, aProdAccount, aBlockClass );
      fSalaryAccount := aSalaryAccount;
      Sample := nil;
      fMinWFRequired := 0.7;
      fMinCompSup    := 0.75;
      idxIn  := 0;
      idxOut := 0;
      for i := low(i) to high(i) do
        if aCapacities[integer(i)] > 0
          then
            begin
              fWFWeights[i] := WorkForceInportances[i];
              MF := TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, PeopleKindPrefix[i] + tidFluid_WorkForce]);
              fSalaries[i] := WorkForcePrice[i];
              fCapacity[i] := aCapacities[integer(i)];
              fWFIndexes[i].workersIn := idxIn;
              inc( idxIn );
              MetaInputs.Insert(
                TMetaInput.Create(
                  PeopleKindPrefix[i] + tidGate_WorkForceIn,
                  inputZero,
                  InputData(fCapacity[i], kIlimited),
                  inputZero,
                  fCapacity[i],
                  TPushInput,
                  MF,
                  1,
                  mglBasic,
                  [],
                  sizeof(Sample.fWorkForceIn[i]),
                  Sample.Offset(Sample.fWorkForceIn[i])));
              fWFIndexes[i].workersOut := idxIn;
              inc( idxIn );
              MetaInputs.Insert(
                TMetaInput.Create(
                  PeopleKindPrefix[i] + tidGate_WorkForceOut,
                  inputZero,
                  inputZero,
                  inputZero,
                  fCapacity[i],
                  TPushInput,
                  MF,
                  1,
                  mglBasic,
                  [],
                  sizeof(Sample.fWorkForceOut[i]),
                  Sample.Offset(Sample.fWorkForceOut[i])));
              fWFIndexes[i].demand := idxOut;
              inc( idxOut );
              MetaOutputs.Insert(
                TMetaOutput.Create(
                  PeopleKindPrefix[i] + tidGate_WorkDemand,
                  FluidData(fCapacity[i], kIlimited),
                  TPushOutput,
                  MF,
                  1,
                  [],
                  sizeof(Sample.fWorkDemand[i]),
                  Sample.Offset(Sample.fWorkDemand[i])));
              fWFIndexes[i].recycle := idxOut;
              inc( idxOut );
              MetaOutputs.Insert(
                TMetaOutput.Create(
                  PeopleKindPrefix[i] + tidGate_WorkRecycleIn,
                  FluidData(fCapacity[i], kIlimited),
                  TPushOutput,
                  MF,
                  1,
                  [],
                  sizeof(Sample.fWorkRecycle[i]),
                  Sample.Offset(Sample.fWorkRecycle[i])));
              fWFIndexes[i].workersRep := idxOut;
              inc( idxOut );
              MetaOutputs.Insert(
                TMetaOutput.Create(
                  PeopleKindPrefix[i] + tidGate_Workers,
                  FluidData(fCapacity[i], kIlimited),
                  TPushOutput,
                  MF,
                  1,
                  [],
                  sizeof(Sample.fWorkersRep[i]),
                  Sample.Offset(Sample.fWorkersRep[i])));
              Prestige := Prestige + aCapacities[integer(i)]/40;
            end
          else
            begin
              fWFIndexes[i].workersIn  := noWFIndex;
              fWFIndexes[i].workersOut := noWFIndex;
              fWFIndexes[i].demand     := noWFIndex;
              fWFIndexes[i].recycle    := noWFIndex;
            end;
    end;

  function TMetaWorkCenter.GetWorkForceCost : TMoney;
    var
      kind : TPeopleKind;
    begin
      result := 0;
      for kind := low(kind) to high(kind) do
        result := result + Capacity[kind]*Salaries[kind];
    end;
    
  procedure TMetaWorkCenter.Register( ClassFamily : TClassFamilyId );
    var
      WFList : TStringList;
      kind   : TPeopleKind;
      i      : integer;
    begin
      WFList := TStringList.Create;
      for kind := low(kind) to high(kind) do
        if Capacity[kind] > 0
          then WFList.Add( IntToStr(round(Capacity[kind])) + ' ' + lowercase(mtidWorkforceKindName[kind].Values[langDefault]) );
      if WFList.Count > 0  // >> MLS Alarm
        then
          begin
            if Desc <> ''
              then Desc := Desc + ' ';
            Desc := Desc + 'Employs ';
            for i := 0 to pred(WFList.Count) do
              begin
                Desc := Desc + WFList[i];
                if i < WFList.Count - 1
                  then
                    if i < WFList.Count - 2
                      then Desc := Desc + ', '
                      else Desc := Desc + ' and ';
              end;
            Desc := Desc + '.';
          end;
      WFList.Free;
      inherited Register( ClassFamily );
    end;


  // TWorkCenter            

  constructor TWorkCenter.Create( aMetaBlock : TMetaBlock; aFacility : TFacility );
    var
      i : TPeopleKind;
    begin
      inherited;
      for i := low(i) to high(i) do
        begin
          fSalaries[i]     := 100;
          fWorkersMax[i].Q := TMetaWorkCenter(MetaBlock).Capacity[i];
        end;
    end;

  destructor TWorkCenter.Destroy;
    begin
      inherited;
    end;

  function TWorkCenter.Evaluate : TEvaluationResult;
    var
      i        : TPeopleKind;
      Parm     : TTownParameter;
      Adm      : TAdmitance;
      OwnerAdm : TAdmitance;
      Effs     : single;
      Wghs     : single;
      mDif     : single;
      tDif     : single;
      People   : single;
    begin
      result := inherited Evaluate;
      Effs := 0;
      Wghs := 0;
      mDif := 0;
      if Facility.Budget <= 0
        then Facility.ReportTrouble( facNeedsBudget );

      if not Facility.CriticalTrouble and (Facility.CompanyDir <> nil)
        then Facility.CompanyDir.Count  := Facility.CompanyDir.Count + 1;

      for i := low(i) to high(i) do
        if TMetaWorkCenter(MetaBlock).Capacity[i] > 0
          then
            begin
              if not Facility.CriticalTrouble
                then
                  begin
                    if Facility.CompanyDir <> nil
                      then Facility.CompanyDir.Demand := Facility.CompanyDir.Demand + WorkCenterSupports[i]*fWorkers[i].Q;

                    fWorkers[i].Q := fWorkers[i].Q + fWorkForceIn[i].Q - fWorkForceOut[i].Q;
                    fWorkers[i].K := fWorkForceIn[i].K;

                    fWorkRecycle[i].Q := realmin( fWorkers[i].Q, (1 - fSalaries[i]/MaxSalary)*MaxMovers*fWorkers[i].Q*dt );
                    fWorkRecycle[i].K := fWorkers[i].K;
                    fWorkers[i].Q     := fWorkers[i].Q - fWorkRecycle[i].Q;

                    tDif := fWorkers[i].Q - fWorkersMax[i].Q;
                    if tDif > 0
                      then
                        begin
                          fWorkRecycle[i].Q := fWorkRecycle[i].Q + tDif;
                          fWorkers[i].Q     := fWorkers[i].Q - tDif;
                        end;

                    if (Facility.Company <> nil) and (Facility.Company.Owner <> nil)
                      then OwnerAdm := min( 20, round(Facility.Company.Owner.Prestige) div 10 )
                      else OwnerAdm := 0;

                    {
                    if Facility.ToBeDemolished = 0
                      then Adm := max( 0, fSalaries[i] + ExtraAdmitance[i] + OwnerAdm )
                      else Adm := 0;
                    }
                    Adm := max(0, fSalaries[i] + ExtraAdmitance[i] + OwnerAdm);

                    fWorkForceIn[i].S  := Adm;
                    fWorkForceOut[i].S := 10000 div (Adm + 1);
                    Inputs[TMetaWorkCenter(MetaBlock).WFIndexes[i].workersIn].ActualMaxFluid.Q  := fWorkersMax[i].Q - fWorkers[i].Q;
                    Inputs[TMetaWorkCenter(MetaBlock).WFIndexes[i].workersIn].MaxCapacity       := fWorkersMax[i].Q - fWorkers[i].Q;
                    Inputs[TMetaWorkCenter(MetaBlock).WFIndexes[i].workersOut].ActualMaxFluid.Q := fWorkers[i].Q;
                    Inputs[TMetaWorkCenter(MetaBlock).WFIndexes[i].workersOut].MaxCapacity      := fWorkers[i].Q;

                    fWorkDemand[i].Q  := fWorkersMax[i].Q - fWorkers[i].Q - fWorkRecycle[i].Q;
                    fWorkDemand[i].K  := fSalaries[i];

                    fWorkersRep[i].Q := Workers[i].Q;
                    fWorkersRep[i].K := Workers[i].K;

                    fPeopleModifier[i].Value := fWorkers[i].Q;
                    with TMetaWorkCenter(MetaBlock) do
                      begin
                        Effs := Effs + WFWeights[i]*fWorkers[i].Q;
                        Wghs := Wghs + WFWeights[i]*fWorkersMax[i].Q;
                        tDif := WFWeights[i]*(fWorkersMax[i].Q - fWorkers[i].Q);
                        if tDif > mDif
                          then
                            begin
                              mDif := tDif;
                              fWFRequired := i;
                            end;
                      end;
                  end
                else
                  begin
                    fWorkForceIn[i].S := 0;
                    fWorkDemand[i].Q  := 0;
                    fWorkDemand[i].K  := 0;
                    Inputs[TMetaWorkCenter(MetaBlock).WFIndexes[i].workersIn].ActualMaxFluid.Q  := 0;
                    Inputs[TMetaWorkCenter(MetaBlock).WFIndexes[i].workersIn].MaxCapacity       := 0;
                    Inputs[TMetaWorkCenter(MetaBlock).WFIndexes[i].workersOut].ActualMaxFluid.Q := 0;
                    Inputs[TMetaWorkCenter(MetaBlock).WFIndexes[i].workersOut].MaxCapacity      := 0;
                    fWorkRecycle[i].Q := fWorkers[i].Q;
                    fWorkers[i].Q     := 0;
                    fWorkersMax[i].Q  := 0;
                    fPeopleModifier[i].Value := 0;
                  end;
              Parm := fWFParams[i]; //Facility.Town.Parameters[tidTownParameter_Salary + PeopleKindPrefix[i]];
              Parm.CurrValue := Parm.CurrValue + max( fSalaries[i], Facility.Town.MinSalary[i] )*fWorkers[i].Q;
              Parm.IncCount( fWorkers[i].Q );
            end;
      if not Facility.CriticalTrouble
        then
          begin
            // Check company support
            if (Facility.CompanyDir = nil) or (Facility.CompanyDir.Support >= TMetaWorkCenter(MetaBlock).MinCompSup)
              then Facility.ClearTrouble(facNeedCompSupport)
              else Facility.ReportTrouble(facNeedCompSupport);
            // Check work force
            if Wghs > 0
              then
                if Effs/Wghs < TMetaWorkCenter(MetaBlock).MinWFRequired
                  then Facility.ReportTrouble(facNeedsWorkForce)
                  else Facility.ClearTrouble(facNeedsWorkForce)
              else Facility.ReportTrouble(facNeedsWorkForce);
          end;

      // Generating trafic
      if TMetaWorkCenter(MetaBlock).HasPeopleCargo
        then
          begin
            People := 0;
            for i := low(i) to high(i) do
              People := People + fWorkers[i].Q;
            SetCargoValue( carPeople, -People );
          end;
    end;

  procedure TWorkCenter.AutoConnect( loaded : boolean );
    var
      TownHall : TBlock;
      kind     : TPeopleKind;
      idx      : integer;
    begin
      inherited;
      TownHall := TInhabitedTown(Facility.Town).TownHall.CurrBlock;
      for kind := low(kind) to high(kind) do
        if TMetaWorkCenter(MetaBlock).Capacity[kind] > 0
          then
            begin
              idx := TMetaWorkCenter(MetaBlock).WFIndexes[kind].workersIn;
              Inputs[idx].ConnectTo( TownHall.OutputsByName[PeopleKindPrefix[kind] + tidGate_WorkForceIn] );
              idx := TMetaWorkCenter(MetaBlock).WFIndexes[kind].workersOut;
              Inputs[idx].ConnectTo( TownHall.OutputsByName[PeopleKindPrefix[kind] + tidGate_WorkForceOut] );
              idx := TMetaWorkCenter(MetaBlock).WFIndexes[kind].demand;
              Outputs[idx].ConnectTo( TownHall.InputsByName[PeopleKindPrefix[kind] + tidGate_WorkDemand] );
              idx := TMetaWorkCenter(MetaBlock).WFIndexes[kind].recycle;
              Outputs[idx].ConnectTo( TownHall.InputsByName[PeopleKindPrefix[kind] + tidGate_WorkRecycleIn] );
              idx := TMetaWorkCenter(MetaBlock).WFIndexes[kind].workersRep;
              Outputs[idx].ConnectTo( TownHall.InputsByName[PeopleKindPrefix[kind] + tidGate_Workers] );

              // People modifier
              fPeopleModifier[kind] :=
                TPyramidalModifier.Create(
                  PeopleKindPrefix[kind] + tidEnvironment_People,
                  Point(xOrigin, yOrigin),
                  0,
                  modStrengthOfPeople );
              fWFParams[kind] := Facility.Town.Parameters[tidTownParameter_Salary + PeopleKindPrefix[kind]];
            end;
    end;

  procedure TWorkCenter.Stop;
    begin
      inherited;
    end;

  procedure TWorkCenter.LoadFromBackup( Reader : IBackupReader );
    var
      kind : TPeopleKind;
    begin
      inherited;                               
      for kind := low(kind) to high(kind) do
        begin
          LoadFluidData( 'Workers.' + PeopleKindPrefix[kind], fWorkers[kind], Reader );
          LoadFluidData( 'WorkersMax.' + PeopleKindPrefix[kind], fWorkersMax[kind], Reader );
          fSalaries[kind] := Reader.ReadByte( 'Salaries.' + PeopleKindPrefix[kind], 0 );
          fExtraAdmitance[kind] := Reader.ReadWord( 'ExtraAdmitance.' + PeopleKindPrefix[kind], 0 );
        end;
    end;

  procedure TWorkCenter.StoreToBackup( Writer : IBackupWriter );
    var
      kind : TPeopleKind;
      aux  : string;
    begin
      inherited;
      for kind := low(kind) to high(kind) do
        begin
          aux := 'Workers.' + PeopleKindPrefix[kind];
          StoreFluidData( aux, fWorkers[kind], Writer );
          aux := 'WorkersMax.' + PeopleKindPrefix[kind];
          StoreFluidData( aux, fWorkersMax[kind], Writer );
          aux := 'Salaries.' + PeopleKindPrefix[kind];
          Writer.WriteByte( aux, fSalaries[kind] );
          aux := 'ExtraAdmitance.' + PeopleKindPrefix[kind];
          Writer.WriteWord( aux, fExtraAdmitance[kind] );
        end;
      aux := '';
    end;

  procedure TWorkCenter.StoreToCache( Cache : TObjectCache );
    var
      kind  : TPeopleKind;
    begin
      inherited;
      for kind := low(kind) to high(kind) do
        if TMetaWorkCenter(MetaBlock).Capacity[kind] > 0
          then
            begin
              Cache.WriteInteger( 'Salaries' + IntToStr(integer(kind)), fSalaries[kind] );
              Cache.WriteInteger( 'MinSalaries' + IntToStr(integer(kind)), Facility.Town.MinSalary[kind] );
              Cache.WriteInteger( 'WorkForcePrice' + IntToStr(integer(kind)), round(WorkForcePrice[kind]) );
              Cache.WriteInteger( 'SalaryValues' + IntToStr(integer(kind)), round(fSalaries[kind]*WorkForcePrice[kind]/100) );
              Cache.WriteInteger( 'Workers' + IntToStr(integer(kind)), round(fWorkers[kind].Q) );
              Cache.WriteInteger( 'WorkersK' + IntToStr(integer(kind)), fWorkers[kind].K );
              Cache.WriteInteger( 'WorkersMax' + IntToStr(integer(kind)), round(fWorkersMax[kind].Q) );
              Cache.WriteInteger( 'WorkersCap' + IntToStr(integer(kind)), round(TMetaWorkCenter(MetaBlock).Capacity[kind]) );
            end;
    end;

  procedure TWorkCenter.RDOSetSalaries( hiSal, miSal, loSal : integer );
    begin
      Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' Setting salaries: ' + IntToStr(hiSal) + ', ' + IntToStr(miSal) + ', ' + IntToStr(loSal) );
      try
        Facility.Lock;
        try
          if Facility.CheckOpAuthenticity
            then
              begin
                fSalaries[pkHigh]   := hiSal;
                fSalaries[pkMiddle] := miSal;
                fSalaries[pkLow]    := loSal;
              end;
        finally
          Facility.Unlock;
        end;
        Facility.UpdateCache(true);
      except
        Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' Error in RDOSetSalaries.' );
      end;
      Logs.Log( tidLog_Survival,  'OK!');
    end;

  function TWorkCenter.RDOTransferWorkForce( kind : integer; ToFacility : TFacility ) : OleVariant;
    begin
      result := -1
    end;

  function TWorkCenter.GetWorkers : PPeopleArray;
    begin
      result := @fWorkers;
    end;

  function TWorkCenter.GetWorkersMax : PPeopleArray;
    begin
      result := @fWorkersMax;
    end;

  function TWorkCenter.GetSalaries : PActualSalaries;
    begin
      result := @fSalaries;
    end;

  function TWorkCenter.GetExtraAdmitance( kind : TPeopleKind ) : TAdmitance;
    begin
      result := fExtraAdmitance[kind];
    end;

  procedure TWorkCenter.SetExtraAdmitance( kind : TPeopleKind; value : TAdmitance );
    begin
      fExtraAdmitance[kind] := value;    
    end;

  function TWorkCenter.RDOGetWorkers(kind : integer) : OleVariant;
    begin
      result := SmartRound(fWorkers[TPeopleKind(lo(kind))].Q);
    end;

  function TWorkCenter.WorkForceEfficiency : single;
    var
      kind : TPeopleKind;
      Effs : single;
      Wghs : single;
      NeedsWF : boolean; 
    begin
      with TMetaWorkCenter(MetaBlock) do
        begin
          Effs := 0;
          Wghs := 0;
          NeedsWF  := false;
          for kind := low(kind) to high(kind) do
            if Capacity[kind] > 0
              then
                begin
                  NeedsWF := true;
                  Effs    := Effs + WFWeights[kind]*(fWorkers[kind].Q + fWorkRecycle[kind].Q);
                  Wghs    := Wghs + WFWeights[kind]*fWorkersMax[kind].Q;
                end;
          if Wghs > 0
            then result := Effs/Wghs
            else
              if NeedsWF 
                then result := 0
                else result := 1; // This might never happen, "o para que vamos a vivir?"
        end;
    end;

  function TWorkCenter.MinWorkForceEfficiency : single;
    const
      invEffic = 100;
    var
      kind  : TPeopleKind;
      eff   : single;
    begin
      with TMetaWorkCenter(MetaBlock) do
        begin
          result := invEffic;
          for kind := low(kind) to high(kind) do
            // If the kind of work force exists...
            if Capacity[kind] > 0
              then
                begin
                  if fWorkersMax[kind].Q > 0
                    then eff := fWorkers[kind].Q/(fWorkersMax[kind].Q)
                    else eff := 0;
                  if eff < result
                    then result := eff;
                end;
          if result = invEffic
            then result := 0;
        end;
    end;

  function TWorkCenter.WorkForceRatioAvg : single;
    var
      kind  : TPeopleKind;
      sum   : single;
      count : single;
    begin
      with TMetaWorkCenter(MetaBlock) do
        begin
          sum   := 0;
          count := 0;
          for kind := low(kind) to high(kind) do
            if Capacity[kind] > 0
              then
                begin
                  sum   := sum   + fWorkers[kind].Q;
                  count := count + fWorkersMax[kind].Q;
                end;
          if count > 0
            then result := sum/count
            else result := 0;
        end;
    end;

  function TWorkCenter.WorkForceCost(kind : TPeopleKind) : TMoney;
    var
      Salary : TPercent;
      theDt  : single;
    begin
      theDt := dt;
      if TMetaWorkCenter(MetaBlock).Capacity[kind] > 0
        then
          begin
            Salary := max(fSalaries[kind], Facility.Town.MinSalary[kind]);
            result := theDt*fWorkers[kind].Q*TMetaWorkCenter(MetaBlock).Salaries[kind]*Salary/100;
          end
        else result := 0;
    end;

  procedure TWorkCenter.HireWorkForce(OpRatio : single);
    var
      kind  : TPeopleKind;
      upgrd : single;
    begin
      //OpRatio := realmin( 1, OpRatio + 0.1 );
      upgrd := 1 + (Facility.UpgradeLevel - 1)*UpgradeWFReduction;
      for kind := low(kind) to high(kind) do
        fWorkersMax[kind].Q := SmartRound(upgrd*OpRatio*TMetaWorkCenter(MetaBlock).Capacity[kind]);
    end;

  procedure TWorkCenter.ClearInputs;
    var
      i : integer;
    begin
      for i := 0 to pred(InputCount) do
        Inputs[i].ActualMaxFluid.Q := 0;
    end;

  procedure TWorkCenter.CopySettingsFrom(Block : TBlock; Options : integer); 
    var
      kind : TPeopleKind;
    begin
      inherited;
      if ObjectIs(ClassName, Block)
        then
          try
            if (Block.Facility.MetaFacility.FacId = Facility.MetaFacility.FacId) and (Options and cloneOption_Salaries <> 0)
              then
                for kind := low(kind) to high(kind) do
                  fSalaries[kind] := TWorkCenter(Block).fSalaries[kind];
          except
            Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' Error in TWorkCenter.CopySettingsFrom.');
          end;
    end;

  function TWorkCenter.RenderCloneMenu(lang : string) : string;
    var
      aux : string;
    begin
      aux := mtidWCenterClone.Values[lang];
      if aux <> ''
        then result := Format(aux, [cloneOption_Salaries]) //'Salaries|%d|'
        else result := '';
    end;

  procedure TWorkCenter.BlockLoaded;
    var
      i     : integer;
      Input : TInput;
    begin
      inherited;
      for i := 0 to pred(InputCount) do
        begin
          Input := Inputs[i];
          if mfTradeable in Input.MetaInput.MetaFluid.Options
            then Input.MaxCapacity := Input.MetaInput.MaxCapacity;
        end;
    end;

  procedure TWorkCenter.Deleted;
    var
      i : TPeopleKind;
    begin
      for i := low(i) to high(i) do
        if fPeopleModifier[i] <> nil
          then fPeopleModifier[i].Delete;
      inherited;
    end;
     

  // TFinaciatedWorkCenter

  function TFinanciatedWorkCenter.Evaluate : TEvaluationResult;
    var
      i      : TPeopleKind;
      Salary : TPercent;
    begin
      result := inherited Evaluate;
      if not Facility.CriticalTrouble
        then
          begin
            for i := low(i) to high(i) do
              if TMetaWorkCenter(MetaBlock).Capacity[i] > 0
                then
                  begin
                    Salary := max( fSalaries[i], Facility.Town.MinSalary[i] );
                    BlockGenMoney( -dt*fWorkers[i].Q*TMetaWorkCenter(MetaBlock).Salaries[i]*Salary/100, TMetaWorkCenter(MetaBlock).SalaryAccount );
                  end;
          end;
    end;

  procedure TFinanciatedWorkCenter.AdjustMinimunWages(perc : TPercent);
    var
      i : TPeopleKind;
    begin
      for i := low(i) to high(i) do
        if fSalaries[i] < perc
          then fSalaries[i] := perc;
    end;

  // TWorkCenterInvention

  constructor TWorkCenterInvention.Load(xmlObj : OleVariant);
    //var
      //Aux   : OleVariant;
    begin
      inherited Load(xmlObj);
      //Aux        := xmlObj.children.item(tidInvElement_Props, Unassigned);
      //fWorkQ     := GetProperty(Aux, tidInvAttr_WorkQ);
      //fWorkBoost := GetProperty(Aux, tidInvAttr_WorkBoost);
    end;

  function TWorkCenterInvention.GetClientProps(Company : TObject; LangId : TLanguageId) : string;
    begin
      result := inherited GetClientProps(Company, LangId );
      //if fWorkBoost <> 0
        //then result := result + SimHints.GetHintText(mtidInvPayReduct.Values[LangId], [FormatDelta(fWorkBoost)]) + LineBreak;
      //if fWorkQ <> 0
        //then result := result + SimHints.GetHintText(mtidInvJobQ.Values[LangId], [FormatDelta(fWorkQ)]) + LineBreak;
    end;


  // RegisterBackup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass( TWorkCenter );
    end;


end.






