unit Construction;

interface

{$DEFINE REMOVEUNFINISHED}

  uses
    Kernel, ConnectedBlock, Surfaces, BackupInterfaces, Protocol, Collection,
    StdFluids, Accounts, CacheCommon;

  const
    modConstructionBeauty         = -5;
    modConstructionBeautyStrength = 0.6;

  const
    TheUnrealConstructionBoost = 1;
    MaxHoursToBuild            = 5*24*365;

  const
    TolEps = 1e-4;

  type
    TMetaBlockUnderConstruction =
      class( TMetaBlock )
        public
          constructor Create( anId           : string;
                              aCost          : TMoney;
                              aPercents      : array of TPercent;
                              aConstTime     : integer;
                              aBlockClass    : CBlock );
        private
          fConstVolumeRequired : TFluidValue;
          fMachVolumeRequired  : TFluidValue;
          fEquipVolumeRequired : TFluidValue;
          fConstTime           : integer;
          fConstIndex          : integer;
          fMachIndex           : integer;
          fEquipIndex          : integer;
        private
          procedure SetConstVolumeRequired( aValue : TFluidValue );
          procedure SetMachVolumeRequired ( aValue : TFluidValue );
          procedure SetEquipVolumeRequired( aValue : TFluidValue );
          function  GetEstimatedPrice : TMoney;
        public
          property ConstTime           : integer     read fConstTime;
          property ConstVolumeRequired : TFluidValue read fConstVolumeRequired write SetConstVolumeRequired;
          property MachVolumeRequired  : TFluidValue read fMachVolumeRequired  write SetMachVolumeRequired;
          property EquipVolumeRequired : TFluidValue read fEquipVolumeRequired write SetEquipVolumeRequired;
          property EstimatedPrice      : TMoney      read GetEstimatedPrice;
          property ConstIndex          : integer     read fConstIndex;
          property MachIndex           : integer     read fMachIndex;
          property EquipIndex          : integer     read fEquipIndex;
      end;

    TBlockUnderConstruction =
      class( TConnectedBlock )
        public
          destructor Destroy; override;
        private
          fConstructionForce : TInputData;
          fMachinery         : TInputData;
          fEquipments        : TInputData;
        private
          fConstVolume : TFluidValue;
          fMachVolume  : TFluidValue;
          fEquipVolume : TFluidValue;
        public
          function  Evaluate : TEvaluationResult; override;
          procedure AutoConnect( loaded : boolean ); override;
          procedure Deleted; override;
        published
          function GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string; override;
        private
          fBeautyModifier : TSurfaceModifier;
        public
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        protected
          function  GetRole : TFacilityRole; override;
        private
          procedure CheckForDemolition;
      end;

    procedure RegisterBackup;

implementation

  uses
    SysUtils, ClassStorage, Classes, PyramidalModifier, SimHints, BasicAccounts,
    MathUtils, Languages, Trade;

  // Utility functions

  function CalculateResultantFluid(Input : TInputData) : TFluidValue;
    var
      k : TPercent;
    begin
      k := max(0, min(TradeCenterQuality, Input.K));
      result := (k/TradeCenterQuality)*Input.Q;
    end;

  // TMetaBlockUnderConstruction

  constructor TMetaBlockUnderConstruction.Create( anId           : string;
                                                  aCost          : TMoney;
                                                  aPercents      : array of TPercent;
                                                  aConstTime     : integer;
                                                  aBlockClass    : CBlock );
    var
      ConstPrice : TMoney;
      MachPrice  : TMoney;
      EquipPrice : TMoney;
    begin
      inherited Create( anId, accIdx_Construction, accIdx_None, aBlockClass );
      Beauty         := modConstructionBeauty;
      BeautyStrength := modConstructionBeautyStrength;
      fConstTime     := aConstTime;

      ConstPrice   := TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_ConstructionForce]).MarketPrice;
      MachPrice    := TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_Machinery]).MarketPrice;
      EquipPrice   := TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_BusinessMachines]).MarketPrice;

      ConstVolumeRequired := aCost*(aPercents[0]/100)/ConstPrice;
      MachVolumeRequired  := aCost*(aPercents[1]/100)/MachPrice;
      EquipVolumeRequired := aCost*(aPercents[2]/100)/EquipPrice;

      RecordCost := true; // This avoids Warehouses to concider cost the Construction force.

      fConstIndex := -1;
      fMachIndex  := -1;
      fEquipIndex := -1;
    end;

  procedure TMetaBlockUnderConstruction.SetConstVolumeRequired( aValue : TFluidValue );
    var
      MetaInput : TMetaInput;
      Capacity  : TFluidValue;
      Sample    : TBlockUnderConstruction;
    begin
      fConstVolumeRequired := aValue;
      MetaInput := InputByName[tidGate_ConstructionForce];
      if aValue > 0
        then
          begin
            Capacity := TheUnrealConstructionBoost*fConstVolumeRequired/fConstTime;
            if MetaInput <> nil
              then MetaInput.MaxFluid.Q := Capacity
              else
                begin
                  Sample := nil;
                  MetaInput :=
                    TMetaInput.Create(
                      tidGate_ConstructionForce,
                      inputZero,
                      InputData( Capacity, kIlimited ),
                      inputZero,
                      fConstVolumeRequired,
                      TPullInput,
                      TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_ConstructionForce]),
                      5,
                      mglBasic,
                      [mgoptCacheable, mgoptEditable],
                      sizeof(Sample.fConstructionForce),
                      Sample.Offset( Sample.fConstructionForce ));
                  MetaInputs.Insert(MetaInput);
                end;
            fConstIndex := MetaInput.Index;
          end
        else MetaInputs.Delete( MetaInput );
    end;

  procedure TMetaBlockUnderConstruction.SetMachVolumeRequired( aValue : TFluidValue );
    var
      MetaInput : TMetaInput;
      Capacity  : TFluidValue;
      Sample    : TBlockUnderConstruction;
    begin
      fMachVolumeRequired := aValue;
      MetaInput := InputByName[tidGate_Machinery];
      if aValue > 0
        then
          begin
            Capacity := TheUnrealConstructionBoost*fMachVolumeRequired/fConstTime;
            if MetaInput <> nil
              then MetaInput.MaxFluid.Q := Capacity
              else
                begin
                  Sample := nil;
                  MetaInput :=
                    TMetaInput.Create(
                      tidGate_Machinery,
                      inputZero,
                      InputData( Capacity, kIlimited ),
                      inputZero,
                      fMachVolumeRequired,
                      TPullInput,
                      TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_Machinery]),
                      5,
                      mglBasic,
                      [mgoptCacheable, mgoptEditable],
                      sizeof(Sample.fMachinery),
                      Sample.Offset( Sample.fMachinery ));
                  MetaInputs.Insert(MetaInput);
                end;
             fMachIndex := MetaInput.Index;
          end
        else MetaInputs.Delete( MetaInput );
    end;

  procedure TMetaBlockUnderConstruction.SetEquipVolumeRequired( aValue : TFluidValue );
    var
      MetaInput : TMetaInput;
      Capacity  : TFluidValue;
      Sample    : TBlockUnderConstruction;
    begin
      fEquipVolumeRequired := aValue;
      MetaInput := InputByName[tidFluid_BusinessMachines];
      if aValue > 0
        then
          begin
            Capacity := TheUnrealConstructionBoost*fEquipVolumeRequired/fConstTime;
            if MetaInput <> nil
              then MetaInput.MaxFluid.Q := Capacity
              else
                begin
                  Sample := nil;
                  MetaInput :=
                    TMetaInput.Create(
                      tidFluid_BusinessMachines,
                      inputZero,
                      InputData( Capacity, kIlimited ),
                      inputZero,
                      fEquipVolumeRequired,
                      TPullInput,
                      TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_BusinessMachines]),
                      5,
                      mglBasic,
                      [mgoptCacheable, mgoptEditable],
                      sizeof( Sample.fEquipments ),
                      Sample.Offset( Sample.fEquipments ));
                  MetaInputs.Insert(MetaInput);
                end;
            fEquipIndex := MetaInput.Index;
          end
        else MetaInputs.Delete( MetaInput );
    end;

  function TMetaBlockUnderConstruction.GetEstimatedPrice : TMoney;
    begin
      result := ConstVolumeRequired*(TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_ConstructionForce])).MarketPrice +
                MachVolumeRequired*(TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_Machinery])).MarketPrice +
                EquipVolumeRequired*(TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_BusinessMachines])).MarketPrice;
    end;


  // TBlockUnderConstruction

  destructor TBlockUnderConstruction.Destroy;
    begin
      fBeautyModifier.Delete;
      inherited;
    end;

  function TBlockUnderConstruction.Evaluate : TEvaluationResult;
    var
      ConstrInput  : TInput;
      MachinInput  : TInput;
      EquipInput   : TInput;
      dtCorrection : single;
      hourday      : single;
      isPolitician : boolean;
    begin
      result := inherited Evaluate;
      with TMetaBlockUnderConstruction(MetaBlock) do
        begin
          isPolitician := (Facility.Company <> nil) and (Facility.Company.Owner <> nil) and Facility.Company.Owner.IsRole;
          if fConstVolume < ConstVolumeRequired
            then fConstVolume := fConstVolume + CalculateResultantFluid(fConstructionForce); //fConstructionForce.Q;
          if fMachVolume < MachVolumeRequired
            then fMachVolume := fMachVolume + CalculateResultantFluid(fMachinery); //fMachinery.Q;
          if fEquipVolume < EquipVolumeRequired
            then fEquipVolume := fEquipVolume + CalculateResultantFluid(fEquipments); //fEquipments.Q;
          if (fConstVolume + TolEps >= ConstVolumeRequired) and (fMachVolume  + TolEps >= MachVolumeRequired) and (fEquipVolume  + TolEps >= EquipVolumeRequired)
            then result := evrEvolve
            else
              begin
                ConstrInput := InputsByName[tidFluid_ConstructionForce];
                MachinInput := InputsByName[tidFluid_Machinery];
                EquipInput  := InputsByName[tidFluid_BusinessMachines];

                hourday := Facility.Town.WorldLocator.GetHoursADay;
                if hourday <> 0
                  then dtCorrection := realmin(5, 24/hourday)
                  else dtCorrection := 1;

                if ConstrInput <> nil
                  then
                    begin
                      ConstrInput.ActualMaxFluid.Q := ConstrInput.MetaInput.MaxFluid.Q*dtCorrection;
                      if ConstVolumeRequired > fConstVolume
                        then ConstrInput.MaxCapacity := realmax(ConstrInput.ActualMaxFluid.Q, ConstVolumeRequired - fConstVolume)
                        else ConstrInput.MaxCapacity := 0;
                      if isPolitician
                        then
                          begin
                            TPullInput(ConstrInput).MaxPrice := min(TPullInput(ConstrInput).MaxPrice, TradeCenterPrice);
                            TPullInput(ConstrInput).MinK     := max(TPullInput(ConstrInput).MinK, TradeCenterQuality);
                          end;
                    end;

                if MachinInput <> nil
                  then
                    begin
                      MachinInput.ActualMaxFluid.Q := MachinInput.MetaInput.MaxFluid.Q*dtCorrection;
                      if MachVolumeRequired > fMachVolume
                        then MachinInput.MaxCapacity := realmax(MachinInput.ActualMaxFluid.Q, MachVolumeRequired - fMachVolume)
                        else MachinInput.MaxCapacity := 0;
                      if isPolitician
                        then
                          begin
                            TPullInput(MachinInput).MaxPrice := min(TPullInput(MachinInput).MaxPrice, TradeCenterPrice);
                            TPullInput(MachinInput).MinK     := max(TPullInput(MachinInput).MinK, TradeCenterQuality);
                          end;
                    end;

                if EquipInput <> nil
                  then
                    begin
                      EquipInput.ActualMaxFluid.Q := EquipInput.MetaInput.MaxFluid.Q*dtCorrection;
                      if EquipVolumeRequired > fEquipVolume
                        then EquipInput.MaxCapacity := realmax(EquipInput.ActualMaxFluid.Q, EquipVolumeRequired - fEquipVolume)
                        else EquipInput.MaxCapacity := 0;
                      if isPolitician
                        then
                          begin
                            TPullInput(EquipInput).MaxPrice := min(TPullInput(EquipInput).MaxPrice, TradeCenterPrice);
                            TPullInput(EquipInput).MinK     := max(TPullInput(EquipInput).MinK, TradeCenterQuality);
                          end;
                    end;

                CheckForDemolition;
              end;
        end;
    end;

  procedure TBlockUnderConstruction.AutoConnect( loaded : boolean );
    begin
      inherited;
      fBeautyModifier :=
        TPyramidalModifier.Create(
          tidEnvironment_Beauty,
          Point(xOrigin, yOrigin),
          // MetaBlock.Beauty*MetaBlock.xSize*MetaBlock.ySize,
          MetaBlock.Beauty*Facility.MetaFacility.xSize*Facility.MetaFacility.ySize,
          MetaBlock.BeautyStrength );
    end;

  procedure TBlockUnderConstruction.Deleted;
    begin
      fBeautyModifier.Delete;
      fBeautyModifier := nil;
      inherited;
    end;

  function TBlockUnderConstruction.GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string;
    var
      PercAvrg : single;
      InpCount : integer;
    begin
      result := inherited GetStatusText( kind, ToTycoon );
      case kind of
        sttMain :
          with TMetaBlockUnderConstruction(MetaBlock) do
            begin
              InpCount := 3;
              PercAvrg := 0;
              if ConstVolumeRequired > 0
                then PercAvrg := fConstVolume / ConstVolumeRequired
                else dec(InpCount);
              if MachVolumeRequired > 0
                then PercAvrg := PercAvrg + fMachVolume / MachVolumeRequired
                else dec(InpCount);
              if EquipVolumeRequired > 0
                then PercAvrg := PercAvrg + fEquipVolume / EquipVolumeRequired
                else dec(InpCount);
              if InpCount > 0
                then PercAvrg := PercAvrg / InpCount;
              result :=
                SimHints.GetHintText( mtidConstruction.Values[ToTycoon.Language], [min(100, round(100*PercAvrg))] );
            end;
        sttSecondary :
          result := '';
        sttHint :
          case Facility.AccessLevelOf( ToTycoon ) of
            acsFull, acsModerate :
              //result := GetHintText( mtidConstFromTradeCenter.Values[ToTycoon.Language], [0] );
          end;
      end;
    end;

  procedure TBlockUnderConstruction.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      fConstVolume := Reader.ReadSingle( 'ConstVolume', 0 );
      fMachVolume := Reader.ReadSingle( 'MachVolume', 0 );
    end;

  procedure TBlockUnderConstruction.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteSingle( 'ConstVolume', fConstVolume );
      Writer.WriteSingle( 'MachVolume', fMachVolume );
    end;

  function TBlockUnderConstruction.GetRole : TFacilityRole;
    begin
      result := rolBuyer;
    end;

  procedure TBlockUnderConstruction.CheckForDemolition;
    begin
      {$IFDEF REMOVEUNFINISHED}
      if (Facility.ToBeDemolished = 0) and (Facility.Age > MaxHoursToBuild) // and (Facility.Trouble <> facNoTrouble)
        then Facility.ToBeDemolished := 1;
      {$ENDIF}
    end;

  // RegisterBackup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass( TBlockUnderConstruction );
    end;

end.

