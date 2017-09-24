unit PopulatedBlock;

interface

  uses
    ClassStorageInt, Kernel, Population, Surfaces, BackupInterfaces, Protocol, CacheAgent,
    Accounts, ConnectedBlock, Inventions, Languages;

  const
    modStrengthOfCrime     = 10;
    modStrengthOfPeople    = 10;
    modStrengthOfPollution = 10;
    modBeautyRatio         = 10;
    modCrimeRatio          = 1;
    modPollutionRatio      = 1;
    modQOLRatio            = 7;
    modPeopleRatio         = 2;

  const
    MaxQOL       = 10;
    MaxBeauty    = 5;
    MaxCrime     = 5;
    MaxPollution = 5;

  const
    MaxPopulationForSpecialSeals = 2000000; // two millions // 3000000

  type
    TParmImportance = array[TPeopleKind] of integer;

  const
    BeautyWeight      : TParmImportance = ( 200,  80,  30);
    QOLWeight         : TParmImportance = (  30, 150,  60);
    CrimeWeight       : TParmImportance = ( 100, 150,  30);
    PollWeight        : TParmImportance = ( 500, 200, 120);
    MaintenanceWeight : TParmImportance = ( 800, 300,  40);
    PriceWeight       : TParmImportance = (  20,  50, 100);
    NeighborsWeight   : TParmImportance = (1000, 500,  40);
    EfficiencyWeight  : TParmImportance = ( 100, 100, 100);

  var
    TotalWeight : TParmImportance = (0, 0, 0);

  const
    PriceWeightMax = 2000;

  const
    //ProfitLimit       = 60;     // Minimal occupancy of positive-profit residential
    MinOccupancy        = 40;     // Minimal occupancy of old-looking residential
    BrandNewEffect      = 365*24; // Minimal age of old-looking residential
    BuildingUglyness    = -70;    // For the beauty modifier if in ugly state
    RepairPriceShare    = 1/4;
    NeighborsQualityMax = 200;
    ResRecovTime        = 10*365*24;
    PeoplePerApt        = 2;
    NewbiewHelp         = 10;

  type
    TResidentialVisualState  = (restNormal, restReparing, restHalfEmpty);
    TResidentialVisualStates = set of TResidentialVisualState;

  type
    TMetaPopulatedBlock =
      class( TMetaBlock )
        public
          constructor Create( anId        : string;
                              aPeopleKind : TPeopleKind;
                              aCapacity   : integer;
                              aBlockClass : CBlock );
        private
          fPeopleKind      : TPeopleKind;
          fCapacity        : TFluidValue;
          fEfficiency      : single;
          fCrimeResist     : single;
          fPollResist      : single;
          fAvailableStates : TResidentialVisualStates;
          fModifyPrice     : boolean;
          fLowCost         : boolean;
          fProfitLimit     : byte;
          fResRecovTime    : integer;
          fTournamentOn    : boolean;
        public
          property PeopleKind      : TPeopleKind              read fPeopleKind;
          property Capacity        : TFluidValue              read fCapacity;
          property Efficiency      : single                   read fEfficiency      write fEfficiency;
          property CrimeResist     : single                   read fCrimeResist     write fCrimeResist;
          property PollResist      : single                   read fPollResist      write fPollResist;
          property AvailableStates : TResidentialVisualStates read fAvailableStates write fAvailableStates;
          property ModifyPrice     : boolean                  read fModifyPrice     write fModifyPrice;
          property LowCost         : boolean                  read fLowCost         write fLowCost;
          property ProfitLimit     : byte                     read fProfitLimit     write fProfitLimit;
          property TournamentOn    : boolean                  read fTournamentOn;
        protected
          function  ModifyStageStack( Stage : TMetaBlock ) : boolean; override;
          procedure ModifyMetaFacility( MetaFacility : TMetaFacility ); override;
        public
          procedure Register( ClassFamily : TClassFamilyId );
          procedure EvaluateTexts; override;
      end;

    TPeopleIntegrators = array[TPeopleKind] of TSurfaceIntegrator;

    TPopulatedBlock =
      class( TConnectedBlock )
        protected
          constructor Create( aMetaBlock : TMetaBlock; aFacility : TFacility ); override;
        public
          destructor  Destroy; override;
        private
          fEmigration  : TPushInputData;
          fInmigration : TPushInputData;
          fRecycleIn   : TPushInputData;
          fDemand      : TOutputData;
          fPopulation  : TOutputData;
          fRecycleOut  : TOutputData;
        protected
          function GetSurfaceValue( SurfaceId : TSurfaceId ) : TSurfaceValue; override;
        protected
          function Evaluate : TEvaluationResult; override;
        public
          procedure AutoConnect( loaded : boolean ); override;
        public
          procedure Stop; override;
        protected
          function GetVisualClassId  : TVisualClassId; override;
        protected
          fPeople      : TFluidData;
          fRent        : TPercent;
          fMaintenance : TPercent;
          fRepair      : TPercent;
          fRepairDate  : TVirtDateAbs;
          fMovedParm   : TTownParameter;
          fRentParm    : TTownParameter;
          fQidxParm    : TTownParameter;
          fPopParm     : TTownParameter;
          finvCrimeRes : single;
          finvPollRes  : single;
          finvBeauty   : single;
          finvPrivacy  : single;
          finvMaint    : single;
        private
          function  GetRent : TPercent;
          procedure SetRent( aRent : TPercent );
          function  GetMaintenance : TPercent;
          procedure SetMaintenance( aMaintenance : TPercent );
          function  GetOccupancy : TPercent;
        published
          property Rent        : TPercent read GetRent        write SetRent;
          property Maintenance : TPercent read GetMaintenance write SetMaintenance;
          property Occupancy   : TPercent read GetOccupancy;
        published
          procedure RdoRepair(useless : integer);
          procedure RdoStopRepair(useless : integer);
        public
          property People : TFluidData read fPeople;
        private
          function GetMarketPrice : TMoney;
        published
          function GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string; override;
        private
          fBeautyModifier      : TSurfaceModifier;
          fPeopleModifier      : TSurfaceModifier;
          fCrimeModifier       : TSurfaceModifier;
          fPollutionModifier   : TSurfaceModifier;
          fBeautyIntegrator    : TSurfaceIntegrator;
          fPollutionIntegrator : TSurfaceIntegrator;
          fCrimeIntegrator     : TSurfaceIntegrator;
          fQOLIntegrator       : TSurfaceIntegrator;
          fPeopleIntegrators   : TPeopleIntegrators;
          fAdm                 : TAdmitance;
          fNeighborsQuality    : single;
        protected
          function ComputeCrime            : TSurfaceValue; virtual;
          function ComputePollution        : TSurfaceValue; virtual;
          function ComputeNeighborsQuality : single;        virtual;
        private
          function GetQOLPercent   ( value : TSurfaceValue ) : TPercent;
          function GetBeautyPercent( value : TSurfaceValue ) : TPercent;
          function GetCrimePercent ( value : TSurfaceValue ) : TPercent;
          function GetPollPercent  ( value : TSurfaceValue ) : TPercent;
        public
          procedure StoreToCache  ( Cache  : TObjectCache  ); override;
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        private
          function LooksUgly : boolean;
        private
          procedure InitEnvironment;
        protected
          procedure CopySettingsFrom(Block : TBlock; Options : integer); override;
          function  RenderCloneMenu(lang : string) : string; override;
          procedure Deleted; override;
        protected
          procedure RecalculateInventionsEffect; override;
      end;

  type
    TResidentialInvention =
      class( TInvention )
        public
          constructor Load( xmlObj : OleVariant ); override;
        private
          fCrimeRes    : integer;
          fPollRes     : integer;
          fPrivacy     : integer;
          fBeauty      : integer;
          fMaintenance : integer;
        public
          property CrimeRes    : integer read fCrimeRes;
          property PollRes     : integer read fPollRes;
          property Privacy     : integer read fPrivacy;
          property Beauty      : integer read fBeauty;
          property Maintenance : integer read fMaintenance;
        public
          function GetClientProps(Company : TObject; LangId : TLanguageId ) : string; override;
      end;

  const
    TrashPerc       = 50;
    NoPollMaintPerc = 100;

  const
    CrimeProb       : array[TPeopleKind] of single = (0.05, 0.1, 0.4);
    PollProb        : array[TPeopleKind] of single = (0, 0.03, 0.05);
    UnempCrimeProb  : array[TPeopleKind] of single = (0.1, 0.5, 1);
    BornCrimeProb   : array[TPeopleKind] of single = (0.04, 0.1, 0.2);

  const
    NeighborParm : array[TPeopleKind, TPeopleKind] of single =
      ((0,  -1, -10),
       (1, 0.5, -10),
       (1,   1,   1));

  const
    tidInventionClass_Residentials = 'Residentials';
    tidInvAttr_CrimeRes            = 'crime';
    tidInvAttr_PollRes             = 'poll';
    tidInvAttr_Privacy             = 'privacy';
    tidInvAttr_Beauty              = 'beauty';
    tidInvAttr_Maintenance         = 'maint';

  const
    tidCFGParm_TournamentLen = 'TornamentLength';

  procedure RegisterInventionClass;
  procedure RegisterBackup;
  procedure RegisterTownParameters;

implementation

  uses
    MetaInstances, SysUtils, ClassStorage, MathUtils, PyramidalModifier, Classes, SimHints,
    Construction, BasicAccounts, StdFluids, Logs, Standards, ModelServerCache, CloneOptions;

  const
    MoveInDays = 10*TimeUnits;
    InhabitantsBoost = 3;


  // TMetaPopulatedBlock

  constructor TMetaPopulatedBlock.Create( anId : string; aPeopleKind : TPeopleKind; aCapacity : integer; aBlockClass : CBlock );
    var
      Sample : TPopulatedBlock;                   
      People : TMetaFluid;                                                            
    begin
      inherited Create( anId, accIdx_None, accIdx_Residentials, aBlockClass );
      fPeopleKind   := aPeopleKind;
      fCapacity     := StrToInt(TheGlobalConfigHandler.GetConfigParm('ResInhabBoost', '3'))*aCapacity; //InhabitantsBoost*aCapacity;
      fProfitLimit  := StrToInt(TheGlobalConfigHandler.GetConfigParm('ResProfitPerc', '40'));
      fResRecovTime := 24*365*StrToInt(TheGlobalConfigHandler.GetConfigParm('ResRecYears', '10'));
      fEfficiency := 1;
      People      := TMetaFluid(TheClassStorage.ClassById['Fluids', PeopleKindPrefix[aPeopleKind] + tidFluid_People]);
      Sample      := nil;
      MetaInputs.Insert(
        TMetaInput.Create(
          PeopleKindPrefix[aPeopleKind] + tidGate_ResInmigration,
          inputZero,
          InputData( fCapacity/MoveInDays, kIlimited ),
          inputZero,
          fCapacity,
          TPushInput,
          People,
          1,
          mglBasic,
          [],
          sizeof(Sample.fInmigration),
          Sample.Offset( Sample.fInmigration )));
      MetaInputs.Insert(
        TMetaInput.Create(
          PeopleKindPrefix[aPeopleKind] + tidGate_ResEmigration,
          inputZero,
          InputData( fCapacity, kIlimited ),
          inputZero,
          fCapacity,
          TPushInput,
          People,
          1,
          mglBasic,
          [],
          sizeof(Sample.fEmigration),
          Sample.Offset( Sample.fEmigration )));
      MetaInputs.Insert(
        TMetaInput.Create(
          PeopleKindPrefix[aPeopleKind] + tidGate_RecycleIn,
          inputZero,
          InputData( fCapacity, kIlimited ),
          inputZero,
          fCapacity,
          TPushInput,
          People,
          1,
          mglBasic,
          [],
          sizeof(Sample.fRecycleIn),
          Sample.Offset( Sample.fRecycleIn )));
      MetaOutputs.Insert(
        TMetaOutput.Create(
          PeopleKindPrefix[aPeopleKind] + tidGate_ResDemand,
          FluidData( fCapacity, kIlimited ),
          TPushOutput,
          People,
          1,
          [],
          sizeof(Sample.fDemand),
          Sample.Offset( Sample.fDemand )));
      MetaOutputs.Insert(
        TMetaOutput.Create(
          PeopleKindPrefix[aPeopleKind] + tidGate_People,
          FluidData( fCapacity, kIlimited ),
          TPushOutput,
          People,
          1,
          [],
          sizeof(Sample.fPopulation),
          Sample.Offset( Sample.fPopulation )));
      MetaOutputs.Insert(
        TMetaOutput.Create(
          PeopleKindPrefix[aPeopleKind] + tidGate_RecycleOut,
          FluidData( fCapacity, kIlimited ),
          TPushOutput,
          People,
          1,
          [],
          sizeof(Sample.fRecycleOut),
          Sample.Offset( Sample.fRecycleOut )));
      fAvailableStates := [restNormal];
      fModifyPrice := true;
      MaxUpgrade := StrToInt(TheGlobalConfigHandler.GetConfigParm('ResMaxUpgrade', '10'));
      fTournamentOn := StrToInt(TheGlobalConfigHandler.GetConfigParm(tidCFGParm_TournamentLen, '0')) > 0;
    end;

  function TMetaPopulatedBlock.ModifyStageStack( Stage : TMetaBlock ) : boolean;
    var
      OptRent    : single;
      Profit     : TMoney;
      ConstPrice : TMoney;
      Cost       : TMoney;
    begin
      if fModifyPrice and ObjectIs( TMetaBlockUnderConstruction.ClassName, Stage )
        then
          begin
            OptRent    := realmax(0, 1 + (CrimeResist + PollResist)/10 + Efficiency/3);
            Profit     := (100 - fProfitLimit)*OptRent*Capacity*PeoplePrice[PeopleKind]/100;
            Cost       := realmax(0, Efficiency*fResRecovTime*Profit);
            ConstPrice := TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_ConstructionForce]).MarketPrice;
            TMetaBlockUnderConstruction(Stage).ConstVolumeRequired := Cost/ConstPrice;
            result := true;
          end
        else result := false;  
    end;

  procedure TMetaPopulatedBlock.ModifyMetaFacility( MetaFacility : TMetaFacility );
    begin
      inherited;
      MetaFacility.MinistryId := nidMinistry_Housing;
    end;
    
  procedure TMetaPopulatedBlock.Register( ClassFamily : TClassFamilyId );
    begin
      inherited Register( ClassFamily );
    end;
                                                            
  procedure TMetaPopulatedBlock.EvaluateTexts;
    var
      i    : integer;
      lang : TLanguageId;
    begin
      for i := 0 to pred(LangList.Count) do
        begin
          lang := LangList[i];
          Desc_MLS.Values[lang] := SimHints.GetHintText( mtidDescResidential.Values[lang], [mtidPeopleKindName[PeopleKind].Values[lang], round(Capacity), round(100*CrimeResist), round(100*PollResist), round(100*Efficiency)] );
        end;
    end;
     

  // TPopulatedBlock      

  constructor TPopulatedBlock.Create( aMetaBlock : TMetaBlock; aFacility : TFacility );
    begin
      inherited;
      fRent        := 100;
      fMaintenance := 100;
    end;

  destructor TPopulatedBlock.Destroy;
    begin
      inherited;
    end;

  function TPopulatedBlock.GetSurfaceValue( SurfaceId : TSurfaceId ) : TSurfaceValue;
    begin
      if SurfaceId = tidEnvironment_Beauty
        then result := fBeautyIntegrator.Media
        else
          if SurfaceId = tidEnvironment_Pollution
            then result := fPollutionIntegrator.Media
            else
              if SurfaceId = tidEnvironment_Crime
                then result := fCrimeIntegrator.Media
                else
                  if SurfaceId = tidEnvironment_QOL
                    then result := fQOLIntegrator.Media
                    else result := inherited GetSurfaceValue( SurfaceId );
    end;

  const
    GlobalAdm : array [TPeopleKind] of integer = (0, 0, 0);
    AdmCount  : array [TPeopleKind] of integer = (0, 0, 0);

  function TPopulatedBlock.Evaluate : TEvaluationResult;

    function GetBostCurveValue( maxFacCount : integer ) : single;
      var
        Tycoon : TTycoon;
      begin
        if (Facility.Company <> nil) and (Facility.Company.Owner <> nil)
          then
            begin
              Tycoon := Facility.Company.Owner;
              if TMetaPopulatedBlock(MetaBlock).TournamentOn
                then result := realmax(0, (1 - Tycoon.FacCount/maxFacCount))
                else result := realmax(0, (1 - Tycoon.FacCount/maxFacCount)*(1 - min(500, Tycoon.NobPoints)/500));
            end
          else result := 0;
      end;

    var
      PeopleIn    : TFluidData;
      PeopleOut   : TFluidData;
      realAdmit   : single;
      avgAdmit    : single;
      Admitance   : TAdmitance;
      Price       : TMoney;
      MarketPrice : TMoney;
      Maint       : TPercent;
      dRepair     : integer;
      admEfc      : single;
      admBeauty   : single;
      admQOL      : single;
      admCrime    : single;
      admPoll     : single;
      admMaint    : single;
      admPrice    : single;
      admNeigh    : single;
      ExtraPriceWeight : single;
      TWeight          : single;
      UpgrLevel        : byte;
      TownHall         : TTownHall;
      RentMoney        : TMoney;
      MaintMoney       : TMoney;
    begin
      result := inherited Evaluate;
      fNeighborsQuality := ComputeNeighborsQuality;
      UpgrLevel := UpgradeLevel;
      if Facility.Trouble and facStoppedByTycoon = 0
        then
          with TMetaPopulatedBlock(MetaBlock) do
            begin
              TownHall := TTownHall(TInhabitedTown(Facility.Town).TownHall.CurrBlock);
              if Facility.CompanyDir <> nil
                then
                  begin
                    Facility.CompanyDir.Demand := Facility.CompanyDir.Demand + UpgrLevel*TMetaPopulatedBlock(MetaBlock).Capacity/100;
                    Facility.CompanyDir.Count  := Facility.CompanyDir.Count + 1;
                  end;                                     
              if not Facility.CriticalTrouble
                then Maint := fMaintenance
                else Maint := 0;
              if not LowCost //and (Facility.ToBeDemolished = 0)
                then
                  begin
                    // Computing residential quality
                    //IntegrateInventions( invCrimeRes, invPollRes, invBeauty, invPrivacy, invMaint );
                    ExtraPriceWeight := PriceWeightMax*(1 - realmax(0, realmin(1, TownHall.SalaryRatio[PeopleKind])));
                    TWeight          := TotalWeight[PeopleKind] + ExtraPriceWeight;
                    admEfc    := EfficiencyWeight[PeopleKind]*Efficiency/TWeight;
                    admBeauty := BeautyWeight[PeopleKind]*realmin(fBeautyIntegrator.Media, 2*MaxBeauty)/(TWeight*MaxBeauty) + finvBeauty;
                    admQOL    := QOLWeight[PeopleKind]*realmin(fQOLIntegrator.Media, 2*MaxQOL)/(TWeight*MaxQOL);
                    admCrime  := CrimeWeight[PeopleKind]*(1 - CrimeResist - finvCrimeRes)*realmax(0, fCrimeIntegrator.Media)/(TWeight*MaxCrime);
                    admPoll   := PollWeight[PeopleKind]*(1 - PollResist - finvPollRes)*realmax(0, fPollutionIntegrator.Media)/(TWeight*MaxPollution);
                    admMaint  := MaintenanceWeight[PeopleKind]*Maint/(100*TWeight);
                    admPrice  := (PriceWeight[PeopleKind] + ExtraPriceWeight)*(100 - Rent)/(100*TWeight);
                    admNeigh  := NeighborsWeight[PeopleKind]*fNeighborsQuality/TWeight + finvPrivacy;
                    if Facility.CompanyDir <> nil
                      then admMaint := admMaint*realmin(1, Facility.CompanyDir.Support);
                    realAdmit := (admEfc + admBeauty + admQOL - admCrime - admPoll + admMaint + admPrice + admNeigh);
                    Admitance := round(realmin(1, Maint/100)*realmax( 0, 10 + 50*realAdmit ) + GetBostCurveValue( 500 )*NewbiewHelp) + 2*UpgrLevel;
                  end
                else Admitance := 0;

              // Residentials belonging to Mayors, Ministers & Presidents will attract half of the population
              if (Facility.Company.Owner <> nil) and Facility.Company.Owner.IsRole
                then Admitance := round(0.75*Admitance)
                else
                  if Facility.Company.Cluster.SpecialSeal
                    then Admitance := round(realmin(1, TownHall.TotalPopulation/MaxPopulationForSpecialSeals)*Admitance);
              fAdm := Admitance;

              // Finding class based admitance
              if GlobalAdm[fPeopleKind] < High(GlobalAdm[fPeopleKind]) div 2
                then
                  begin
                    GlobalAdm[fPeopleKind] := GlobalAdm[fPeopleKind] + Admitance;
                    inc( AdmCount[fPeopleKind] )
                  end
                else
                  begin
                    GlobalAdm[fPeopleKind] := Admitance;
                    AdmCount[fPeopleKind]  := 1;
                  end;
              if AdmCount[fPeopleKind] > 0
                then avgAdmit := GlobalAdm[fPeopleKind]/AdmCount[fPeopleKind]
                else avgAdmit := 100;
              if avgAdmit > 0
                then avgAdmit := (Admitance/avgAdmit)*100
                else avgAdmit := Admitance;

              // >> Patching crazy traumas!!!
              fInmigration.Q := realmax(0, fInmigration.Q);
              fRecycleIn.Q   := realmax(0, fRecycleIn.Q);

              // Acepting inmigration
              PeopleIn.Q := fInmigration.Q + fRecycleIn.Q;
              PeopleIn.K := AverageK( @fInmigration, @fRecycleIn );

              // Population Recycle
              fRecycleOut.Q := realmax(0, realmin( fPeople.Q, sqr(100.0 - fDemand.K)*EmigrationProb[TMetaPopulatedBlock(MetaBlock).fPeopleKind]*fPeople.Q{*dt}/(2*{4*}100*EmigrationTimeSlope*100)));
              fRecycleOut.K := fPeople.K;
              fRecycleIn.S  := sqr(Admitance) + 1;

              // >> Patching crazy traumas!!!
              fEmigration.Q := realmax(0, fEmigration.Q);
              fRecycleOut.Q := realmax(0, fRecycleOut.Q);

              // Emigration
              PeopleOut.Q := fEmigration.Q + fRecycleOut.Q;
              PeopleOut.K := AverageK( @fEmigration, @fRecycleOut );

              // Update town parameter for internal move out
              fMovedParm.CurrValue := fMovedParm.CurrValue + fRecycleOut.Q;

              // Generate population signals
              if PeopleOut.Q > PeopleIn.Q
                then
                  begin
                    PeopleOut.Q   := realmax(0, PeopleOut.Q - PeopleIn.Q);
                    fPeople.Q     := realmax(0, fPeople.Q - PeopleOut.Q);
                    fRecycleOut.Q := realmax(0, PeopleOut.Q - fEmigration.Q);
                    fPeople.K     := AverageK( @fPeople, @PeopleOut );
                  end
                else
                  begin
                    PeopleIn.Q    := realmax(0, PeopleIn.Q - PeopleOut.Q);
                    fPeople.K     := AverageK( @fPeople, @PeopleIn );
                    fRecycleOut.Q := 0;
                    fPeople.Q     := realmax(0, fPeople.Q + PeopleIn.Q);
                  end;
              fPeople.Q := realmax(0, realmin(fPeople.Q, UpgrLevel*Capacity));

              // Sending demand
              fDemand.Q := realmax(0, UpgrLevel*Outputs[0].MetaOutput.MaxFluid.Q - fPeople.Q);
              fDemand.K := min( 100, round(avgAdmit) );

              // Feeding back population
              fPopulation.Q := realmax(0, fPeople.Q);
              fPopulation.K := fPeople.K;

              // Adjusting Emigration and Inmigration
              fInmigration.S := sqr(Admitance) + 1;
              if Admitance > 0
                then fEmigration.S := 10000 div Admitance
                else fEmigration.S := 0;
              Inputs[0].MaxCapacity := UpgrLevel*TMetaPopulatedBlock(MetaBlock).Capacity - fPeople.Q;
              Inputs[0].ActualMaxFluid.Q := Inputs[0].MaxCapacity;
              Inputs[1].MaxCapacity := fPeople.Q;
              Inputs[1].ActualMaxFluid.Q := Inputs[1].MaxCapacity;
              Inputs[2].MaxCapacity := Inputs[0].MaxCapacity;
              Inputs[2].ActualMaxFluid.Q := Inputs[2].MaxCapacity;

              // Generating money
              MarketPrice := GetMarketPrice;
              Price := fRent*MarketPrice/100;
              { << old way...
              BlockGenMoney( 2.3*fPeople.Q*Price*dt, accIdx_Residentials_Rents );
              BlockGenMoney( -TMetaPopulatedBlock(MetaBlock).ProfitLimit*(Maint - finvMaint)*UpgrLevel*TMetaPopulatedBlock(MetaBlock).Capacity*MarketPrice*dt/10000, accIdx_Residentials_Maintenance );
              }
              RentMoney  := (fPeople.Q/PeoplePerApt)*Price*dt;
              MaintMoney := -UpgrLevel*(TMetaPopulatedBlock(MetaBlock).Capacity/PeoplePerApt)*MarketPrice*dt;
              MaintMoney := realmax(0, Maint/100 - finvMaint)*TMetaPopulatedBlock(MetaBlock).ProfitLimit*MaintMoney/100;
              BlockGenMoney( RentMoney, accIdx_Residentials_Rents );
              BlockGenMoney( MaintMoney, accIdx_Residentials_Maintenance );

              // Modifying environment
              fPeopleModifier.Value    := fPeople.Q;
              fCrimeModifier.Value     := ComputeCrime;
              fPollutionModifier.Value := ComputePollution;
              if not LooksUgly
                then fBeautyModifier.Value := MetaBlock.Beauty
                else fBeautyModifier.Value := BuildingUglyness;
            end;
      // Repairing-wearing
      if fRepair > 0
        then
          if fRepair < 100        
            then
              begin
                dRepair := fRepair;
                if fRepair + dt < 100
                  then inc( fRepair, round(dt) )
                  else fRepair := 100;
                dRepair := fRepair - dRepair;
                BlockGenMoney( -dRepair/100*RepairPriceShare*Facility.MetaFacility.Price, accIdx_Residentials_Repairs );
              end
            else
              begin
                fRepairDate := Facility.Town.Timer.GetVirtualTimeAbs;
                fRepair     := 0;
              end;

      // >> Patching crazy traumas!!!
      fPeople.Q := realmax(0, fPeople.Q);

      // Generating trafic
      SetCargoValue( carPeople, fPeople.Q );

      // Updating parameters
      fRentParm.CurrValue := fRentParm.CurrValue + fPeople.Q*fRent;
      fRentParm.IncCount( fPeople.Q );
      fQidxParm.CurrValue := fQidxParm.CurrValue + fPeople.Q*fAdm;
      fQidxParm.IncCount( fPeople.Q );
      fPopParm.CurrValue := fPopParm.CurrValue + fPeople.Q;
      fPopParm.IncCount( 1 );
    end;

  procedure TPopulatedBlock.AutoConnect( loaded : boolean );
    var
      kd       : TPeopleKind;
      TownHall : TBlock;
    begin
      inherited;
      kd := TMetaPopulatedBlock(MetaBlock).fPeopleKind;

      // TownHall connections
      TownHall := TInhabitedTown(Facility.Town).TownHall.CurrBlock;
      InputsByName[PeopleKindPrefix[kd] + tidGate_ResInmigration].ConnectTo( TownHall.OutputsByName[PeopleKindPrefix[kd] + tidGate_ResInmigration] );
      InputsByName[PeopleKindPrefix[kd] + tidGate_ResEmigration].ConnectTo( TownHall.OutputsByName[PeopleKindPrefix[kd] + tidGate_ResEmigration] );
      InputsByName[PeopleKindPrefix[kd] + tidGate_RecycleIn].ConnectTo( TownHall.OutputsByName[PeopleKindPrefix[kd] + tidGate_RecycleIn] );
      OutputsByName[PeopleKindPrefix[kd] + tidGate_People].ConnectTo( TownHall.InputsByName[PeopleKindPrefix[kd] + tidGate_People] );
      OutputsByName[PeopleKindPrefix[kd] + tidGate_ResDemand].ConnectTo( TownHall.InputsByName[PeopleKindPrefix[kd] + tidGate_ResDemand] );
      OutputsByName[PeopleKindPrefix[kd] + tidGate_RecycleOut].ConnectTo( TownHall.InputsByName[PeopleKindPrefix[kd] + tidGate_RecycleOut] );
      
      if not loaded
        then fRepairDate := Facility.Town.Timer.GetVirtualTimeAbs;

      // Environmental stuff
      InitEnvironment;

      fMovedParm := Facility.Town.Parameters[tidTownParameter_ResMovedOut + PeopleKindPrefix[kd]];
      fRentParm  := Facility.Town.Parameters[tidTownParameter_ResRent + PeopleKindPrefix[kd]];
      fQidxParm  := Facility.Town.Parameters[tidTownParameter_ResQidx + PeopleKindPrefix[kd]];
      fPopParm   := Facility.Town.Parameters[tidTownParameter_ResPop + PeopleKindPrefix[kd]];
    end;

  procedure TPopulatedBlock.Stop;
    begin
      inherited;
      with TTownHall(TInhabitedTown(Facility.Town).TownHall.CurrBlock) do
        ReportClosedBuilding( self );
      fPeople.Q     := 0;
      fPopulation.Q := 0;
      fDemand.Q     := 0;
      fRecycleOut.Q := 0;
      fPeopleModifier.Value := 0;
      fCrimeModifier.Value  := 0;
    end;

  function TPopulatedBlock.GetVisualClassId : TVisualClassId;
    var                     
      MPB : TMetaPopulatedBlock;
    begin
      MPB := TMetaPopulatedBlock(MetaBlock);
      case MPB.VisualStages of
        2 :
          if LooksUgly
            then result := 1
            else result := 0;
        else result := 0;
      end;
    end;

  function TPopulatedBlock.GetRent : TPercent;
    begin
      Facility.Lock;
      try
        result := fRent;
      finally
        Facility.Unlock;
      end;
    end;

  procedure TPopulatedBlock.SetRent( aRent : TPercent );
    begin
      Facility.Lock;
      try
        if Facility.CheckOpAuthenticity
          then
            begin
              fRent := aRent;
              ModelServerCache.BackgroundInvalidateCache(Facility); // UpdateCache(true)
            end;
      finally
        Facility.Unlock;
      end;
    end;

  function TPopulatedBlock.GetMaintenance : TPercent;
    begin
      Facility.Lock;
      try
        result := fMaintenance
      finally
        Facility.Unlock;
      end;
    end;

  procedure TPopulatedBlock.SetMaintenance( aMaintenance : TPercent );
    begin
      Facility.Lock;
      try
        if Facility.CheckOpAuthenticity
          then
            begin
              fMaintenance := aMaintenance;
              ModelServerCache.BackgroundInvalidateCache(Facility); //Facility.UpdateCache(true)
            end;
      finally
        Facility.Unlock;
      end;
    end;

  function TPopulatedBlock.GetOccupancy : TPercent;
    var
      UpgrLevel : integer;
    begin
      UpgrLevel := UpgradeLevel;
      with TMetaPopulatedBlock(MetaBlock) do
        result := round(100*fPeople.Q/(UpgrLevel*Capacity));
    end;

  procedure TPopulatedBlock.RdoRepair(useless : integer);
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Repairing: ' + Facility.Name );
      if Facility.CheckOpAuthenticity
        then fRepair := 1;
      Logs.Log( tidLog_Survival,  'OK!');
    end;

  procedure TPopulatedBlock.RdoStopRepair(useless : integer);
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Stop Repairing: ' + Facility.Name );
      if Facility.CheckOpAuthenticity
        then fRepair := 0;
      Logs.Log( tidLog_Survival,  'OK!');
    end;

  function TPopulatedBlock.GetMarketPrice : TMoney;
    var
      kd : TPeopleKind;
    begin
      kd := TMetaPopulatedBlock(MetaBlock).fPeopleKind;
      result := PeoplePrice[kd];
    end;

  function TPopulatedBlock.GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string;
    var
      FillRatio : single;
      TownHall  : TTownHall;
    begin
      result := inherited GetStatusText( kind, ToTycoon );
      case kind of
        sttMain :
          begin
            if Facility.Trouble and facStoppedByTycoon = 0
              then
                result :=
                  result +
                  SimHints.GetHintText( mtidResWorking.Values[ToTycoon.Language], [round(100*int(fPeople.Q)/(UpgradeLevel*TMetaPopulatedBlock(MetaBlock).Capacity)), mtidPeopleKindName[TMetaPopulatedBlock(MetaBlock).fPeopleKind].Values[ToTycoon.Language]] )
                  {
                  IntToStr(round(100*int(fPeople.Q)/TMetaPopulatedBlock(MetaBlock).Capacity)) +
                  '% ' + mtidPeopleKindName[TMetaPopulatedBlock(MetaBlock).fPeopleKind].Values[ToTycoon.Language] + ' occupancy'
                  }
              else
                result :=
                  result +
                  SimHints.GetHintText( mtidResClosedHeader.Values[ToTycoon.Language], [mtidPeopleKindName[TMetaPopulatedBlock(MetaBlock).fPeopleKind].Values[ToTycoon.Language]] ) + LineBreak +
                  SimHints.GetHintText( mtidResClosedByLine.Values[ToTycoon.Language], [0] );
                  {
                  mtidPeopleKindName[TMetaPopulatedBlock(MetaBlock).fPeopleKind].Values[ToTycoon.Language] + ' residential' + LineBreak +
                  '[closed]';
                  }
            if fRepair > 0
              then
                result :=
                  result + LineBreak +
                  SimHints.GetHintText( mtidResRepaired.Values[ToTycoon.Language], [fRepair] );
                  //IntToStr(fRepair) + '% repaired';
          end;
        sttSecondary :
          begin
            TownHall := TTownHall(TInhabitedTown(Facility.Town).TownHall.CurrBlock);
            result := Format(mtidUpgradeLevel.Values[ToTycoon.Language], [UpgradeLevel]) + '  ' +
              SimHints.GetHintText(
                mtidResSecReport.Values[ToTycoon.Language],
                [
                max(0, round(fPeople.Q)),
                max(0, fAdm),
                max(0, round(realmin(100, 100*TownHall.GQOL*fQOLIntegrator.Media/MaxQOL))),
                max(0, round(100*fNeighborsQuality)),
                max(0, round(realmin(100, 100*fBeautyIntegrator.Media/MaxBeauty))),
                max(0, round(realmin(100, 100*fCrimeIntegrator.Media/MaxCrime))),
                max(0, round(realmin(100, 100*fPollutionIntegrator.Media/MaxPollution)))
                ] );
          end;
        sttHint :
          case Facility.AccessLevelOf( ToTycoon ) of
            acsFull, acsModerate :
              if Facility.Trouble = facNoTrouble
                then
                  begin
                    FillRatio := 100*People.Q/(UpgradeLevel*TMetaPopulatedBlock(MetaBlock).Capacity);
                    if FillRatio >= (100 + TMetaPopulatedBlock(MetaBlock).ProfitLimit) div 2
                      then
                        if FillRatio >= 93
                          then result := GetHintText( mtidResWorkingFine.Values[ToTycoon.Language], [0] )
                          else result := GetHintText( mtidResMildUnderPopulated.Values[ToTycoon.Language], [0] )
                      else
                        if FillRatio >= TMetaPopulatedBlock(MetaBlock).ProfitLimit
                          then result := GetHintText( mtidResUnderPopulated.Values[ToTycoon.Language], [0] )
                          else result := GetHintText( mtidResVeryUnderPopulated.Values[ToTycoon.Language], [0] )
                  end
                else result := GetHintText( mtidVisitWebSite.Values[ToTycoon.Language], [0] );
          end;
      end;
    end;
                                            
  function TPopulatedBlock.ComputeCrime : TSurfaceValue;
    var
      kind     : TPeopleKind;
      TownHall : TTownHall;
    begin
      kind     := TMetaPopulatedBlock(MetaBlock).fPeopleKind;
      TownHall := TTownHall(TInhabitedTown(Facility.Town).TownHall.CurrBlock);
      result   := CrimeProb[kind]*(BornCrimeProb[kind] + UnempCrimeProb[kind]*TownHall.Unemployment[kind]/100.0 + (1 - fPeople.K/100.0))*fPeople.Q
    end;

  function TPopulatedBlock.ComputePollution : TSurfaceValue;
    var
      kind : TPeopleKind;
    begin
      kind   := TMetaPopulatedBlock(MetaBlock).fPeopleKind;
      //result := PollProb[kind]*realmax(0, 2*(NoPollMaintPerc - fMaintenance){*fPeople.Q/TMetaPopulatedBlock(MetaBlock).Capacity})
      result := PollProb[kind]*fPeople.Q*(NoPollMaintPerc - fMaintenance)/100;
    end;                  

  function TPopulatedBlock.ComputeNeighborsQuality : single;
    var
      kind, i : TPeopleKind;
    begin
      kind   := TMetaPopulatedBlock(MetaBlock).fPeopleKind;
      result := 0;
      for i := low(i) to high(i) do
        result := result + NeighborParm[kind, i]*fPeopleIntegrators[i].Media;
      result := realmax( 0, realmin( 2*NeighborsQualityMax, NeighborsQualityMax + result ))/NeighborsQualityMax; 
    end;
    
  function TPopulatedBlock.GetQOLPercent( value : TSurfaceValue ) : TPercent;
    begin
      result := max(0, round(realmin(100, 100*fQOLIntegrator.Media/MaxQOL)));
    end;

  function TPopulatedBlock.GetBeautyPercent( value : TSurfaceValue ) : TPercent;
    begin
      result := max(0, round(realmin(100, 100*value/MaxBeauty)));
    end;

  function TPopulatedBlock.GetCrimePercent( value : TSurfaceValue ) : TPercent;
    begin
      result := max(0, round(realmin(100, 100*value/MaxCrime)));
    end;

  function TPopulatedBlock.GetPollPercent( value : TSurfaceValue ) : TPercent;
    begin
      result := max(0, round(realmin(100, 100*value/MaxPollution)));
    end;

  procedure TPopulatedBlock.StoreToCache( Cache : TObjectCache );
    begin
      inherited;
      Cache.WriteInteger( 'Inhabitants', round(People.Q) );
      Cache.WriteInteger( 'Rent', fRent );
      Cache.WriteInteger( 'Maintenance', fMaintenance );
      Cache.WriteInteger( 'Repair', fRepair );
      Cache.WriteInteger( 'QOL', GetQOLPercent( fQOLIntegrator.Media ));
      Cache.WriteInteger( 'Beauty', GetBeautyPercent( fBeautyIntegrator.Media ));
      Cache.WriteInteger( 'Crime', GetCrimePercent( fCrimeIntegrator.Media ));
      Cache.WriteInteger( 'Pollution', GetPollPercent( fPollutionIntegrator.Media ));
      Cache.WriteString( 'RepairPrice', FormatMoney( RepairPriceShare*Facility.MetaFacility.Price ));
      with TMetaPopulatedBlock(MetaBlock) do
        begin
          Cache.WriteInteger( 'ActualCrime', GetCrimePercent( CrimeResist*fCrimeIntegrator.Media ));
          Cache.WriteInteger( 'ActualPollution', GetPollPercent( PollResist*fPollutionIntegrator.Media ));
          Cache.WriteInteger( 'Efficiency', round( 100*Efficiency ));
        end;
      Cache.WriteInteger( 'InvBeauty', round(100*finvBeauty) );
      Cache.WriteInteger( 'invCrimeRes', round(100*finvCrimeRes) );
      Cache.WriteInteger( 'invPollutionRes', round(100*finvPollRes) );
      Cache.WriteInteger( 'invPrivacy', round(100*finvPrivacy) );
    end;

  procedure TPopulatedBlock.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      LoadFluidData( 'People', fPeople, Reader );
      if fPeople.Q < 0
        then fPeople.Q := 0;
      fRent := Reader.ReadByte( 'Rent', 100 );
      fMaintenance := Reader.ReadByte( 'Maintenance', 100 );
      fRepair := Reader.ReadByte( 'Repair', 100 );
      fRepairDate := Reader.ReadInteger( 'RepairDate', 0 ); // >>
      try
        vVisualClassId := GetVisualClassId;
      except
        asm
          nop
        end;
      end;
    end;

  procedure TPopulatedBlock.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      StoreFluidData( 'People', fPeople, Writer );
      Writer.WriteByte( 'Rent', fRent );
      Writer.WriteByte( 'Maintenance', fMaintenance );
      Writer.WriteByte( 'Repair', fRepair );
      Writer.WriteInteger( 'RepairDate', fRepairDate );
    end;

  function TPopulatedBlock.LooksUgly : boolean;
    var
      MPB : TMetaPopulatedBlock;
    begin
      MPB := TMetaPopulatedBlock(MetaBlock);
      result := (Facility.Town.Timer.GetVirtualTimeAbs - fRepairDate > BrandNewEffect) and
                ((100*People.Q/(UpgradeLevel*MPB.Capacity) < MinOccupancy) or
                 (GetPollPercent( fPollutionIntegrator.Media ) > 20) or
                 (GetCrimePercent( fCrimeIntegrator.Media ) > 30) or
                 (Maintenance < 60));
    end;

  procedure TPopulatedBlock.InitEnvironment;
    var
      kd : TPeopleKind;
      i  : TPeopleKind;
    begin
      kd := TMetaPopulatedBlock(MetaBlock).fPeopleKind;
      fBeautyModifier :=
        TPyramidalModifier.Create(
          tidEnvironment_Beauty,
          Point(xOrigin, yOrigin),
          MetaBlock.Beauty,
          MetaBlock.BeautyStrength );
      fPeopleModifier :=
        TPyramidalModifier.Create(
          PeopleKindPrefix[kd] + tidEnvironment_People,
          Point(xOrigin, yOrigin),
          fPeople.Q,
          modStrengthOfPeople );
      fCrimeModifier :=
        TPyramidalModifier.Create(
          tidEnvironment_Crime,
          Point(xOrigin, yOrigin),
          ComputeCrime,
          modStrengthOfCrime );
      fPollutionModifier :=
        TPyramidalModifier.Create(
          tidEnvironment_Pollution,
          Point(xOrigin, yOrigin),
          ComputePollution,
          modStrengthOfPollution );

      fBeautyIntegrator    := TSurfaceIntegrator.Create( tidEnvironment_Beauty, GetArea( modBeautyRatio, amdIncludeBlock ));
      fPollutionIntegrator := TSurfaceIntegrator.Create( tidEnvironment_Pollution, GetArea( modPollutionRatio, amdIncludeBlock ));
      fCrimeIntegrator     := TSurfaceIntegrator.Create( tidEnvironment_Crime, GetArea( modCrimeRatio, amdExcludeBlock ));
      fQOLIntegrator       := TSurfaceIntegrator.Create( tidEnvironment_QOL, GetArea( modQOLRatio, amdExcludeBlock ));

      for i := low(i) to high(i) do
        fPeopleIntegrators[i] := TSurfaceIntegrator.Create( PeopleKindPrefix[i] + tidEnvironment_People, GetArea( modPeopleRatio, amdExcludeBlock ));
    end;

  procedure TPopulatedBlock.CopySettingsFrom(Block : TBlock; Options : integer); 
    begin
      if ObjectIs(ClassName, Block)
        then
          try
            if Block.Facility.MetaFacility.FacId = Facility.MetaFacility.FacId
              then
                begin
                  if Options and cloneOption_Rent <> 0
                    then fRent := TPopulatedBlock(Block).Rent;
                  if Options and cloneOption_Maint <> 0
                    then fMaintenance := TPopulatedBlock(Block).Maintenance;
                end;
          except
            Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' Error in TPopulatedBlock.CopySettingsFrom.');
          end;
    end;

  function TPopulatedBlock.RenderCloneMenu(lang : string) : string;
    var
      aux : string;
    begin
      aux := mtidPopBlkClone.Values[lang];
      if aux <> ''
        then result := Format(aux, [cloneOption_Rent, cloneOption_Maint]) // FIX MLS 'Rent|%d|Maintenance|%d|'
        else result := ''; 
    end;

  procedure TPopulatedBlock.Deleted;
    var
      i : TPeopleKind;
    begin
      fBeautyModifier.Delete;
      fPeopleModifier.Delete;
      fCrimeModifier.Delete;
      fPollutionModifier.Delete;
      fBeautyIntegrator.Delete;
      fPollutionIntegrator.Delete;
      fCrimeIntegrator.Delete;
      fQOLIntegrator.Delete;
      for i := low(i) to high(i) do
        fPeopleIntegrators[i].Delete;
      inherited;
    end;

  procedure TPopulatedBlock.RecalculateInventionsEffect;
    var
      Invention : TResidentialInvention;
      i         : integer;
    begin
      finvCrimeRes := 0;
      finvPollRes  := 0;
      finvBeauty   := 0;
      finvPrivacy  := 0;
      finvMaint    := 0;
      for i := 0 to pred(MetaBlock.Inventions.Count) do
        begin
          Invention := TResidentialInvention(MetaBlock.Inventions[i]);
          if Facility.Company.HasInvention[Invention.NumId]
            then
              begin
                finvCrimeRes := finvCrimeRes + Invention.CrimeRes;
                finvPollRes  := finvPollRes  + Invention.PollRes;
                finvBeauty   := finvBeauty   + Invention.Beauty;
                finvPrivacy  := finvPrivacy  + Invention.Privacy;
                finvMaint    := finvMaint    + Invention.Maintenance;
              end;
        end;
      finvCrimeRes := finvCrimeRes/100;
      finvPollRes  := finvPollRes/100;
      finvBeauty   := finvBeauty/100;
      finvPrivacy  := finvPrivacy/100;
      finvMaint    := finvMaint/100;
    end;


  // TResidentialInvention

  constructor TResidentialInvention.Load( xmlObj : OleVariant );
    var
      Aux : OleVariant;
    begin
      inherited Load( xmlObj );
      Aux          := xmlObj.children.item( tidInvElement_Props, Unassigned );
      fCrimeRes    := GetProperty( Aux, tidInvAttr_CrimeRes );
      fPollRes     := GetProperty( Aux, tidInvAttr_PollRes );
      fPrivacy     := GetProperty( Aux, tidInvAttr_Privacy );
      fBeauty      := GetProperty( Aux, tidInvAttr_Beauty );
      fMaintenance := GetProperty( Aux, tidInvAttr_Maintenance );
    end;

  function TResidentialInvention.GetClientProps(Company : TObject; LangId : TLanguageId) : string;
    begin
      result := inherited GetClientProps(Company, LangId);
      if fBeauty <> 0
        then result := result + SimHints.GetHintText(mtidInvBeauty.Values[LangId], [FormatDelta(fBeauty)]) + LineBreak;
      if fMaintenance <> 0
        then result := result + SimHints.GetHintText(mtidInvMaintenance.Values[LangId], [FormatDelta(fMaintenance)]) + LineBreak;
      if fPrivacy <> 0
        then result := result + SimHints.GetHintText(mtidInvPrivacy.Values[LangId], [FormatDelta(fPrivacy)]) + LineBreak;
      if fCrimeRes <> 0
        then result := result + SimHints.GetHintText(mtidInvCrimeRes.Values[LangId], [FormatDelta(fCrimeRes)]) + LineBreak;
      if fPollRes <> 0
        then result := result + SimHints.GetHintText(mtidInvPollRes.Values[LangId], [FormatDelta(fPollRes)]) + LineBreak;
    end;


  // RegisterInventionClass

  procedure RegisterInventionClass;
    begin
      TheClassStorage.RegisterClass(
        tidClassFamily_InvClasses,
        tidInventionClass_Residentials,
        TInventionClass.Create(TResidentialInvention));
    end;

  // RegisterBackup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass( TPopulatedBlock );
    end;

  // RegisterTownParameters

  procedure RegisterTownParameters;
    var
      kind : TPeopleKind;
    begin
      for kind := low(kind) to high(kind) do
        begin
          TMetaTownParameter.Create( tidTownParameter_ResMovedOut + PeopleKindPrefix[kind], '', true ).Register;
          TMetaTownParameter.Create( tidTownParameter_ResRent + PeopleKindPrefix[kind], '', true ).Register;
          TMetaTownParameter.Create( tidTownParameter_ResQidx + PeopleKindPrefix[kind], '', true ).Register;
          TMetaTownParameter.Create( tidTownParameter_ResPop + PeopleKindPrefix[kind], '', true ).Register;
        end;
    end;
    

var                                            
  kind : TPeopleKind;

initialization

  for kind := low(kind) to high(kind) do
    TotalWeight[kind] :=
      BeautyWeight[kind] +                  
      QOLWeight[kind] +                     
      CrimeWeight[kind] +
      PollWeight[kind] +
      MaintenanceWeight[kind] +
      PriceWeight[kind] +
      NeighborsWeight[kind] +
      EfficiencyWeight[kind];

end.




