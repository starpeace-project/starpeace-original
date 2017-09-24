unit LumberMill;

interface

  uses
    ClassStorageInt, Protocol, Kernel, WorkCenterBlock, Surfaces, OutputEvaluators, Accounts,
    StdFluids, BackupInterfaces, Inventions, Languages, PolluterWorkCenter, Population;

  type
    TLumberMillBlock       = class;
    TLumberOutputEvaluator = class;

    TMetaLumberMillBlock =
      class(TMetaPolluterWorkCenterBlock)
        public
          constructor Create(anId                : string;
                             aCapacities         : array of TFluidValue;
                             aChemicalMax        : TFluidValue;
                             aMaxWoodPerHour     : TFluidValue;
                             aWoodMaxVol         : single;
                             aMinExpVol          : single;
                             aGrowTime           : integer;
                             aMaxBudget          : TMoney;
                             aBlockClass         : CBlock);
        private
          fMaxWoodVol : single;
          fMinExpVol  : single;
          fGrowRate   : single;
      end;

    TLumberOutputEvaluator =
      class(TOutputEvaluator)
        public
          function BiasOpenLimit(Block : TBlock) : TBiasRatio; override;
      end;

    TLumberMillBlock =
      class(TPolluterWorkCenterBlock)
        private
          fTimberVolume : single;
          fGrowing      : boolean;
        protected
          function Evaluate : TEvaluationResult; override;
          function GetVisualClassId  : TVisualClassId; override;
          function GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string; override;
        public
          procedure AutoConnect( loaded : boolean ); override;
          procedure LoadFromBackup(Reader : IBackupReader); override;
          procedure StoreToBackup (Writer : IBackupWriter); override;
        private
          fChemicals : TInputData;
          fTimber    : TOutputData;
      end;

  procedure RegisterBackup;

implementation

  uses
    SysUtils, ClassStorage, MathUtils, StdAccounts, SimHints;

  // TMetaLumberMillBlock

  constructor TMetaLumberMillBlock.Create(anId                : string;
                                          aCapacities         : array of TFluidValue;
                                          aChemicalMax        : TFluidValue;
                                          aMaxWoodPerHour     : TFluidValue;
                                          aWoodMaxVol         : single;
                                          aMinExpVol          : single;
                                          aGrowTime           : integer;
                                          aMaxBudget          : TMoney;
                                          aBlockClass         : CBlock);
    var
      Sample : TLumberMillBlock;
    begin
      inherited Create(anId,
                  aCapacities,
                  accIdx_LumberMill_Supplies,
                  accIdx_LumberMill_Products,
                  accIdx_LumberMill_Salaries,
                  accIdx_LumberMill_Maintenance,
                  aBlockClass);

      fMaxWoodVol := 10*aWoodMaxVol; // ten times..
      fGrowRate   := fMaxWoodVol/aGrowTime; //aWoodMaxVol/aGrowTime;
      fMinExpVol  := aMinExpVol;

      Sample := nil;

      // Inputs
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Chemicals,
          inputZero,
          InputData(aChemicalMax, 100),
          InputData(0, 0),
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_Chemicals]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fChemicals),
          Sample.Offset(Sample.fChemicals)));

      // Outputs
      MetaOutputs.Insert(
        TMetaOutput.Create(
          tidGate_Timber,
          FluidData(aMaxWoodPerHour, 100),
          TPullOutput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_Timber]),
          5,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fTimber),
          Sample.Offset(Sample.fTimber)));

      // MetaEvaluators
      with TMetaOutputEvaluator.Create(
        aMaxBudget,
        0,
        OutputByName[tidGate_Timber],
        TLumberOutputEvaluator) do
        begin
          FullOpenTime  := 30;
          FullCloseTime := 31;

          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[tidGate_Chemicals],
              200,
              1));

          Register(MetaEvaluatorPool);
        end;
    end;


  // TLumberOutputEvaluator

  function TLumberOutputEvaluator.BiasOpenLimit(Block : TBlock) : TBiasRatio;
    begin
      if TLumberMillBlock(Block).fGrowing
        then result := 0
        else result := inherited BiasOpenLimit(Block);
    end;


  // TLumberMillBlock

  function TLumberMillBlock.Evaluate : TEvaluationResult;
    var
      MB : TMetaLumberMillBlock;
    begin
      result := inherited Evaluate;
      MB := TMetaLumberMillBlock(MetaBlock);
      if fGrowing
        then
          begin
            fTimberVolume := realmin(MB.fMaxWoodVol, fTimberVolume + MB.fGrowRate*dt);
            fGrowing      := fTimberVolume < MB.fMaxWoodVol;
          end
        else
          begin
            fTimberVolume := realmax(0, fTimberVolume - fTimber.Q);
            fGrowing      := fTimberVolume <= MB.fMinExpVol;
          end;
    end;

  function TLumberMillBlock.GetVisualClassId  : TVisualClassId;
    var
      wperc : single;
      MB    : TMetaLumberMillBlock;
    begin
      MB    := TMetaLumberMillBlock(MetaBlock);
      wperc := fTimberVolume/MB.fMaxWoodVol;
      if fGrowing
        then
          begin
            if wperc < 0.05
              then result := 0
              else
                if wperc < 0.30
                  then result := 1
                  else
                    if wperc < 0.60
                      then result := 2
                      else
                        if wperc < 0.90
                          then result := 3
                          else result := 4;
          end
        else
          begin
            if wperc > 0.60
              then result := 4
              else
                if wperc > 0.20
                  then result := 5
                  else result := 6;
          end;
    end;

  function TLumberMillBlock.GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string;
    var
      MB : TMetaLumberMillBlock;
    begin
      result := inherited GetStatusText( kind, ToTycoon );
      case kind of
        sttSecondary :
          if fGrowing
            then
              begin
                MB := TMetaLumberMillBlock(MetaBlock);
                result := result + ' ' + Format(mtidGrowingTrees.Values[ToTycoon.Language], [SmartRound(100*fTimberVolume/MB.fMaxWoodVol)]);
              end;
      end;
    end;

  procedure TLumberMillBlock.AutoConnect(loaded : boolean);
    begin
      inherited AutoConnect(loaded);
      if not Loaded
        then fGrowing := true;
    end;

  procedure TLumberMillBlock.LoadFromBackup(Reader : IBackupReader);
    begin
      inherited;
      fTimberVolume := Reader.ReadSingle('Volume', 0);
      fGrowing    := Reader.ReadBoolean('Growing', true);
    end;

  procedure TLumberMillBlock.StoreToBackup (Writer : IBackupWriter);
    begin
      inherited;
      Writer.WriteSingle('Volume', fTimberVolume);
      Writer.WriteBoolean('Growing', fGrowing);
    end;


  // RegisterBackup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass(TLumberMillBlock);
    end;


end.
