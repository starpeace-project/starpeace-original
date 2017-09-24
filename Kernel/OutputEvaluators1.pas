unit OutputEvaluators;

interface

  uses
    Collection, Kernel, BackupInterfaces, Surfaces, Protocol, Accounts, Inventions;

  const
    MaxEval     = 8;
    NoInvention = 1;

  const
    MaxEvaluators        = 10;
    MaxInputsByEvaluator = 16;
    DefMaxBiasRatio      = 1000;
    MaxSupplyRatio       = 100;
    DefDesignMaxK        = 50;

  const
    EmergencyCloseDec = 5;
    DefBiasRatioInc   = 1;
    DefBiasRatioDec   = 1;
    LowestBiasRatio   = 1;
    InitialBiasRatio  = 0;
    SupplyRatioFrac   = 10;
    WorkForceSlices   = 3;
    DeltaSupplyRatio  = 5;
    EnlargeFactor     = 3;

  const
    RequiredSupplyRatio = 95;

  const
    DefaultAdtnQFrac  = 0; // 5%
    DefaultAdtnKFrac  = 0; // 5%

  const
    MinQuality = 5;

  const
    noIllInput = 255;

  const
    ModSupplyRatio = 90;
    ModWorkForce   = 1;

  const
    MinWorkForcePerc = 5;

  const
    SignMoney = 1;

  function IncreasingEffect(OptValue, ActValue : TSurfaceValue) : single;
  function DecreasingEffect(OptValue, ActValue : TSurfaceValue) : single;

  type
    // Classes
    TMetaInputInfo       = class;
    TInventionInfo       = class;
    TIntegratorInfo      = class;
    TMetaOutputEvaluator = class;
    TOutputEvaluator     = class;
    TMetaEvaluatorPool   = class;
    TEvaluatorPool       = class;

    // MetaClasses
    COutputEvaluator     = class of TOutputEvaluator;
    CEvaluatorPool       = class of TEvaluatorPool;

    TExtraHistory        = byte;
    TBiasRatio           = word;

    TInputKind           = (ikBasic, ikAditional);

    // TMetaInputInfo is a small profile to store the information
    // related to an input, this information is used latter to
    // evaluate the output.
    // MetaInput: Class of the input, using this its posible to get
    // the input within the block and some other info.
    // Kind: This the kind of input, upto now "ikBasic" if it's a normal
    // input; or ikAditional it's a aditional input such as software
    // Importance: Importance of this input, this is an unbounded figure
    // which determines the importance or weight of this input
    // Share: Is an the share to use of an input, this number must in the range (0,1]

    TMetaInputInfo =
      class
        public
          constructor Create(aMetaInput   : TMetaInput;
                             anImportance : single;
                             aShare       : single);
        private
          fMetaInput  : TMetaInput;
          fImportance : single;
          fShare      : single;
        private
          function GetInputKind : TInputKind;
        public
          property MetaInput  : TMetaInput read fMetaInput;
          property InputKind  : TInputKind read GetInputKind;
          property Importance : single     read fImportance write fImportance;
          property Share      : single     read fShare  write fShare;
      end;

    // TInventionInfo: reflex how a kind of invention affect the production of
    // an Output in a block. Notice this effect can be positive or negative.
    // Kind: is the kind of inventions
    // NumId: is the id of the invention
    // Effect: represent the effect of this invention, as previusly mentioned
    // effect can be positive or negative. This number must be in [-1, 1]. It
    // is strongly recomended to use a shorter interval [-0.1, 0.1] because
    // the whole effect of inventions will be added 1 and after that multiplied
    // to the production.

    TInventionEfficiencyEffect = single;
    TInventionQualityEffect    = TPercent;

    TInventionInfo =
      class
        public
          constructor Create(aKind : string; aNumId : TInventionNumId; aQEffect : TInventionEfficiencyEffect; aKEffect : TInventionQualityEffect);
        private
          fKind    : string;
          fNumId   : TInventionNumId;
          fQEffect : TInventionEfficiencyEffect;
          fKEffect : TInventionQualityEffect;
        public
          property Kind   : string           read fKind;
          property NumId  : TInventionNumId  read fNumId;
          property QEffect : TInventionEfficiencyEffect read fQEffect;
          property KEffect : TInventionQualityEffect read fKEffect;
      end;

    // Functions of this kind calculate the effect of an integrator in the
    // production of any output, the result must be in the range (0, 1].

    TIntegratorEffectFunction = function(OptValue, ActValue : TSurfaceValue) : single;

    // TIntegratorInfo: stores the information to calculate the effect of an
    // integrator in the production of an output.
    // Surface: is the name of the surface in which the integrator works
    // OptValue: is the value that do not affect the production, for instance
    // "Polution" has zero as the optimal value because no polution is
    // the ideal for any production.
    // EffectFunct: is a fucntion that calculates the real effect in a given
    // place, this function must return a value in the range [0,1]

    TIntegratorInfo =
      class
        public
          constructor Create(aSurface : string; anOptValue : TSurfaceValue; anEffectFunct : TIntegratorEffectFunction);
        private
          fSurface     : TSurfaceId;
          fOptValue    : TSurfaceValue;
          fEffectFunct : TIntegratorEffectFunction;
        public
          property Surface     : TSurfaceId                read fSurface;
          property OptValue    : TSurfaceValue             read fOptValue;
          property EffectFunct : TIntegratorEffectFunction read fEffectFunct;
      end;

    PCompanyInputInfo = ^TCompanyInputInfo;
    TCompanyInputInfo =
      record
        Index : integer;
        Effic : single;
        K     : single;
        Max   : TFluidValue;
      end;

    PCompanyInputArray = ^TCompanyInputArray;
    TCompanyInputArray = array[0..31] of TCompanyInputInfo;

    // TMetaOutputEvaluator: A meta class that is Instantiated for
    // every kind of output that is calculated by means of an output evaluator
    // InputInfoList: is a list of TMetaInputInfo intances
    // DepFactor: Represents the degree in which the quality of the
    // remaining products in output to calculate decrease when calculated
    // MetaOutput: is the class of output to calculate

    TMetaOutputEvaluator =
      class
        public
          constructor Create(aBudget      : TMoney;
                             aGenPercent  : TPercent;
                             aMetaOutput  : TMetaOutput;
                             aDelphiClass : COutputEvaluator);
          destructor  Destroy; override;
        private
          fMetaInfoList    : TCollection;
          fIntegrators     : TCollection;
          fCompanyInputs   : PCompanyInputArray;
          fCompInputCount  : integer;
          fBudget          : TMoney;
          fWFPerc          : TPercent;
          fGenPercent      : TPercent;
          fMetaOutput      : TMetaOutput;
          fDelphiClass     : COutputEvaluator;
          fAdtnQFrac       : single;
          fAdtnKFrac       : single;
          fDesignEffic     : single;
          fIgnoreWorkForce : boolean;
          fBiasInc         : TBiasRatio;
          fBiasDec         : TBiasRatio;
          fTurnCount       : integer;
          fMaxBiasRatio    : integer;
          fDesignMaxK      : TPercent;
          fImportance      : TPercent;
        public
          procedure RegisterInput(MetaInfo : TMetaInputInfo);
          procedure RegisterIntegrator(IntgrInfo : TIntegratorInfo);
          procedure RegisterCompanyInput(anIndex : integer; anEffic, aK : single; aMax : TFluidValue);
        public
          property InputInfoList   : TCollection read fMetaInfoList;
          property Integrators     : TCollection read fIntegrators;
          property Budget          : TMoney      read fBudget;
          property WFPerc          : TPercent    read fWFPerc          write fWFPerc;
          property GenPercent      : TPercent    read fGenPercent;
          property MetaOutput      : TMetaOutput read fMetaOutput;
          property AdtnQFrac       : single      read fAdtnQFrac       write fAdtnQFrac;
          property AdtnKFrac       : single      read fAdtnKFrac       write fAdtnKFrac;
          property DesignEffic     : single      read fDesignEffic     write fDesignEffic;
          property IgnoreWorkForce : boolean     read fIgnoreWorkForce write fIgnoreWorkForce;
          property MaxBiasRatio    : integer     read fMaxBiasRatio    write fMaxBiasRatio;
          property DesignMaxK      : TPercent    read fDesignMaxK      write fDesignMaxK;
          property BiasInc         : TBiasRatio  read fBiasInc         write fBiasInc;
          property BiasDec         : TBiasRatio  read fBiasDec         write fBiasDec;
          property Importance      : TPercent    read fImportance      write fImportance;
        private
          function  GetFullOpenTime : integer;
          procedure SetFullOpenTime(turns : integer);
          function  GetFullCloseTime : integer;
          procedure SetFullCloseTime(turns : integer);
          function  GetTurnCount : byte;
          procedure SetTurnCount(count : byte);
          function  GetNetProfit : TMoney;
        public
          property FullOpenTime  : integer read GetFullOpenTime  write SetFullOpenTime;
          property FullCloseTime : integer read GetFullCloseTime write SetFullCloseTime;
          property TurnCount     : byte    read GetTurnCount     write SetTurnCount;
          property NetProfit     : TMoney  read GetNetProfit;
        private
          function GetInfo(Input : TInput) : TMetaInputInfo;
          function GetMetaInputInfo(index : integer) : TMetaInputInfo;
        protected
          property Info[Input : TInput] : TMetaInputInfo read GetInfo;
          property MetaInputInfo[index : integer] : TMetaInputInfo read GetMetaInputInfo;
        public
          function  Instantiate(aBlock : TBlock) : TOutputEvaluator;
          procedure Register(aMetaEvaluatorPool : TMetaEvaluatorPool);
      end;

    // TOutputEvaluator: is a class that is Instantiated for every block
    // as many times as the count of output to calculated by means of an
    // evaluator
    // Block: reference to the block the output belongs to
    // MetaEvaluator: is the MetaEvaluter to use for evaluating the output.
    // The output to evaluate is retrieve using MetaEvaluator.MetaOutput and
    // the Block.Outputs property

    TOutputEvaluator =
      class
        public
          constructor Create(aMetaEvaluator : TMetaOutputEvaluator); virtual;
        private
          fBiasRatio      : TBiasRatio;
          fSupplyRatio    : TPercent;
          fOperationRatio : TPercent;
          fOperationEffic : TPercent;
          fExtraHistory   : TExtraHistory;
          fIllInputIndex  : byte;
          fMetaEvaluator  : TMetaOutputEvaluator;
        public
          property MetaEvaluator : TMetaOutputEvaluator read fMetaEvaluator;
        public
          function  NeedsMoreSupply : boolean;
          function  MeaningfulValue(Output : TOutput; value : TFluidValue) : boolean;
          procedure UpdateBiasRatio(Block : TBlock; WFEffic : single);
          function  ShouldProduce(Block : TBlock) : boolean;
          function  ShareToPullFromImput(Input : TInput) : single;
          function  MaxToPullFromImput(Block : TBlock; Input : TInput) : TFluidValue;
          function  GetIntegratorsEffect(Block : TBlock) : single;
          procedure GetInventionsEffect(Block : TBlock; var QEffect : TInventionEfficiencyEffect; var KEffect : TInventionQualityEffect);
          procedure GetCompanyInputsEfect(Block : TBlock; var QEffect : single; var KEffect : single);
          function  EfficToOpenRatio(effic : single) : single;
          procedure PlanInputForNextPeriod(Block : TBlock; Effic : single);
          procedure PlanCompanyInputs(Block : TBlock; Share : single);
          function  CheckSupplies(Block : TBlock) : boolean;
          procedure ClearInputs(Block : TBlock);
        public
          function  FormatOutput(Block : TBlock; kind : TStatusKind; ShowName : boolean) : string;
        protected
          function  CanIncProduction : boolean;
          function  CanDecProduction : boolean;
          procedure EnsureProduction(Block : TBlock);
          procedure UpdateExtraHistory(Block : TBlock);
          function  HasClients(Output : TOutput) : boolean;
        public
          procedure BeforeProduce(Block : TBlock); virtual;
          procedure AfterProduce(Block : TBlock);  virtual;
          function  DoProduce(Block : TBlock; WFEffic : single) : single;
          procedure DonotProduce(Block : TBlock);
        private
          function  GetEfficiency : TPercent;
        public
          property  BiasRatio      : TBiasRatio read fBiasRatio      write fBiasRatio;
          property  OperationRatio : TPercent   read fOperationRatio write fOperationRatio;
          property  SupplyRatio    : TPercent   read fSupplyRatio    write fSupplyRatio;
          property  Efficiency     : TPercent   read GetEfficiency;
        public
          procedure LoadFromBackup(Reader : IBackupReader);
          procedure StoreToBackup(Writer : IBackupWriter);
      end;

    TInputValues     = array[0..MaxInputsByEvaluator-1] of TFluidValue;
    TEvaluatorShares = array[0..MaxEvaluators-1] of TFluidValue;
    TShareOutTable   = array[0..MaxEvaluators-1, 0..MaxInputsByEvaluator-1] of TFluidValue;
    TEvaluateTable   = array[0..MaxEvaluators-1] of boolean;
    TEfficTable      = array[0..MaxEvaluators-1] of single;

    TMetaEvaluatorPool =
      class
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fMetaEvaluators     : TCollection;
          fEvaluatorPoolClass : CEvaluatorPool;
        public
          procedure EnlargeInputs(factor : single);
          procedure AddMetaEvaluator(MetaEvaluator : TMetaOutputEvaluator);
          function  Instantiate(aBlock : TBlock) : TEvaluatorPool;
          function  GetNetProfit : TMoney;
        private
          function GetMetaEvaluator(index : integer) : TMetaOutputEvaluator;
          function GetEvaluatorCount : integer;
        public
          property EvaluatorCount : integer read GetEvaluatorCount;
          property MetaEvaluators[index : integer] : TMetaOutputEvaluator read GetMetaEvaluator; default;
          property EvaluatorPoolClass : CEvaluatorPool read fEvaluatorPoolClass write fEvaluatorPoolClass;
          property NetProfit : TMoney read GetNetProfit;
      end;

    // TEvaluatorPool: has a list of evaluators and it is responsible of sharing
    // the inputs among every OutputEvaluator it holds

    TEvaluatorPool =
      class
        public
          constructor Create(aBlock : TBlock);
          destructor  Destroy; override;
        private
          fBlock      : TBlock;
          fEvaluators : TCollection;
        private
          function GetEvaluatorCount : integer;
          function GetEvaluator(index : integer) : TOutputEvaluator;
          function GetOperationRatio : TPercent;
        public
          property EvaluatorCount : integer read GetEvaluatorCount;
          property Evaluators[index : integer] : TOutputEvaluator read GetEvaluator;
          property Block : TBlock read fBlock;
          property OperationRatio : TPercent read GetOperationRatio;
        public
          procedure AddEvaluator(Evaluator : TOutputEvaluator);
          function  Evaluate(WFEffic : single) : TEvaluationResult;
        public
          function  GetHint(ToTycoon : TTycoon) : string;
          function  GetSecondaryTextHeader : string; virtual;
        public
          procedure LoadFromBackup(Reader : IBackupReader);
          procedure StoreToBackup(Writer : IBackupWriter);
        public
          function  WorkForceNeeded : single;
          procedure SetIllInputIndex(index : byte);
      end;


implementation

  uses
    Population, MathUtils, SysUtils, WorkCenterBlock, SimHints, EvaluatedBlock,
    ClassStorage, Standards;

  function IncreasingEffect(OptValue, ActValue : TSurfaceValue) : single;
    begin
      if ActValue < OptValue
        then result := 1/(OptValue - ActValue + 1)
        else result := 1;
    end;

  function DecreasingEffect(OptValue, ActValue : TSurfaceValue) : single;
    begin
      if ActValue > OptValue
        then result := 1/(ActValue - OptValue + 1)
        else result := 1;
    end;

  // TMetaInputInfo

  constructor TMetaInputInfo.Create(aMetaInput   : TMetaInput;
                                    anImportance : single;
                                    aShare       : single);
    begin
      fMetaInput  := aMetaInput;
      fImportance := anImportance;
      fShare      := aShare;
    end;

  function TMetaInputInfo.GetInputKind : TInputKind;
    begin
      if MetaInput.Level = mglBasic
        then result := ikBasic
        else result := ikAditional;
    end;

  // TInventionInfo

  constructor TInventionInfo.Create(aKind    : string;
                                    aNumId   : TInventionNumId;
                                    aQEffect : TInventionEfficiencyEffect;
                                    aKEffect : TInventionQualityEffect);
    begin
      inherited Create;
      fKind    := aKind;
      fNumId   := aNumId;
      fQEffect := aQEffect;
      fKEffect := aKEffect;
    end;

  // TIntegratorInfo

  constructor TIntegratorInfo.Create(aSurface      : string;
                                     anOptValue    : TSurfaceValue;
                                     anEffectFunct : TIntegratorEffectFunction);
    begin
      fSurface     := aSurface;
      fOptValue    := anOptValue;
      fEffectFunct := anEffectFunct;
    end;

  // TMetaOutputEvaluator

  constructor TMetaOutputEvaluator.Create(aBudget      : TMoney;
                                          aGenPercent  : TPercent;
                                          aMetaOutput  : TMetaOutput;
                                          aDelphiClass : COutputEvaluator);
    begin
      inherited Create;
      fWFPerc         := 100;
      fGenPercent     := aGenPercent;
      fMetaOutput     := aMetaOutput;
      fBudget         := aBudget;
      fDelphiClass    := aDelphiClass;
      fMetaInfoList   := TCollection.Create(0, rkBelonguer);
      fIntegrators    := TCollection.Create(0, rkBelonguer);
      fDesignEffic    := 1;
      fAdtnQFrac      := DefaultAdtnQFrac;
      fAdtnKFrac      := DefaultAdtnKFrac;
      fBiasInc        := DefBiasRatioInc;
      fBiasDec        := DefBiasRatioDec;
      fMaxBiasRatio   := DefMaxBiasRatio;
      fDesignMaxK     := DefDesignMaxK;
      fImportance     := 100;
    end;

  destructor TMetaOutputEvaluator.Destroy;
    begin
      fMetaInfoList.Free;
      fIntegrators.Free;
      ReAllocMem(fCompanyInputs, 0);
      inherited;
    end;

  procedure TMetaOutputEvaluator.RegisterInput(MetaInfo : TMetaInputInfo);
    begin
      if MetaInfo.fMetaInput <> nil
        then fMetaInfoList.Insert(MetaInfo)
        else MetaInfo.Free;
    end;

  procedure TMetaOutputEvaluator.RegisterIntegrator(IntgrInfo : TIntegratorInfo);
    begin
      fIntegrators.Insert(IntgrInfo);
    end;

  procedure TMetaOutputEvaluator.RegisterCompanyInput(anIndex : integer; anEffic, aK : single; aMax : TFluidValue);
    begin
      ReAllocMem(fCompanyInputs, succ(fCompInputCount)*sizeof(fCompanyInputs[0]));
      with fCompanyInputs[fCompInputCount] do
        begin
          Index := anIndex;
          Effic := anEffic;
          K     := aK;
          Max   := aMax;
        end;
      inc(fCompInputCount);
    end;

  function TMetaOutputEvaluator.GetFullOpenTime : integer;
    begin
      result := round(MaxBiasRatio/fBiasInc);
    end;

  procedure TMetaOutputEvaluator.SetFullOpenTime(turns : integer);
    begin
      fBiasInc := max(1, round(MaxBiasRatio/turns));
    end;

  function TMetaOutputEvaluator.GetFullCloseTime : integer;
    begin
      result := round(MaxBiasRatio/fBiasDec);
    end;

  procedure TMetaOutputEvaluator.SetFullCloseTime(turns : integer);
    begin
      fBiasDec := max(1, round(MaxBiasRatio/turns)); //max(1, round(TurnCount*MaxBiasRatio/turns));
    end;

  function TMetaOutputEvaluator.GetTurnCount : byte;
    begin
      result := 8*sizeof(TExtraHistory) - fTurnCount;
    end;

  procedure TMetaOutputEvaluator.SetTurnCount(count : byte);
    begin
      fTurnCount := 8*sizeof(TExtraHistory) - min(8*sizeof(TExtraHistory), count);
    end;

  function TMetaOutputEvaluator.GetNetProfit : TMoney;
    begin
      result := fMetaOutput.MaxFluid.Q*fMetaOutput.MetaFluid.MarketPrice - fBudget;
    end;

  function TMetaOutputEvaluator.GetInfo(Input : TInput) : TMetaInputInfo;
    var
      i : integer;
      c : integer;
    begin
      c := fMetaInfoList.Count;
      i := 0;
      while (i < c) and (TMetaInputInfo(fMetaInfoList[i]).MetaInput <> Input.MetaInput) do
        inc(i);
      if i < c
        then result := TMetaInputInfo(fMetaInfoList[i])
        else result := nil;
    end;

  function TMetaOutputEvaluator.GetMetaInputInfo(index : integer) : TMetaInputInfo;
    begin
      result := TMetaInputInfo(fMetaInfoList[index]);
    end;

  function TMetaOutputEvaluator.Instantiate(aBlock : TBlock) : TOutputEvaluator;
    begin
      result := fDelphiClass.Create(Self);
    end;

  procedure TMetaOutputEvaluator.Register(aMetaEvaluatorPool : TMetaEvaluatorPool);
    begin
      aMetaEvaluatorPool.AddMetaEvaluator(self);
    end;

  // TOutputEvaluator

  constructor TOutputEvaluator.Create(aMetaEvaluator : TMetaOutputEvaluator);
    begin
      inherited Create;
      fMetaEvaluator := aMetaEvaluator;
      // >>!fBudgetPercent := 100;
      fBiasRatio     := InitialBiasRatio;
      fSupplyRatio   := 100;
    end;                                          

  function TOutputEvaluator.NeedsMoreSupply : boolean;
    begin
      result := (fBiasRatio > 0) and (fSupplyRatio < RequiredSupplyRatio);
    end;

  function TOutputEvaluator.MeaningfulValue(Output : TOutput; value : TFluidValue) : boolean;
    begin
      if value > 0
        then result := value*Output.MetaOutput.MetaFluid.MarketPrice > SignMoney
        else result := false;
    end;

  procedure TOutputEvaluator.UpdateBiasRatio(Block : TBlock; WFEffic : single);
    var
      Output  : TOutput;
      Deficit : TFluidValue;
      Extra   : TFluidValue;
      Delta   : integer;
    begin
      // Get the output of this evaluator
      Output := Block.Outputs[fMetaEvaluator.MetaOutput.Index];
      // Get the production extra
      Extra := POutputData(Output.FluidData).Extra.Q;
      // Deficit is the amount of fluid it didn't provide the last period
      if Extra > 0
        then Deficit := 0
        else Deficit := Output.Demand;
      if Deficit > 0
        then
          begin
            // The block can only incress the production rate when the supplies and the work force are OK
            if (Block.Facility.Trouble and facNeedsWorkForce = 0) and not NeedsMoreSupply
              then
                begin
                  // Calculate the delta increment
                  Delta := min(fMetaEvaluator.fBiasInc, max(1, round(Deficit*fMetaEvaluator.MaxBiasRatio/(Block.dt*Output.MetaOutput.MaxFluid.Q))));
                  // Adjust the BiasRatio
                  fBiasRatio := min(fMetaEvaluator.MaxBiasRatio, fBiasRatio + Delta);
                end
              else
                if CanIncProduction
                  then
                    begin
                      fBiasRatio    := max(0, fBiasRatio - fMetaEvaluator.fBiasDec);
                      fExtraHistory := (fExtraHistory shl 1) or byte($10);
                    end;
          end
        else
          begin
            // If the block has extra...
            if Extra > 0
              then
                begin
                  // Check if the block still has clients
                  if HasClients(Output)
                    then
                      begin
                        // Check if it is time to decress the production rate
                        if CanDecProduction
                          then
                            begin
                              // Calculate delta decrement
                              Delta := min(fMetaEvaluator.fBiasDec, max(1, round(Extra*fMetaEvaluator.MaxBiasRatio/(Block.dt*Output.MetaOutput.MaxFluid.Q))));
                              // Adjust the BiasRatio
                              if fBiasRatio - Delta*Block.dt > 0
                                then fBiasRatio := fBiasRatio - Delta*Block.dt
                                else fBiasRatio := Delta*Block.dt div 2;
                              // Clear the history
                              fExtraHistory := 0;
                            end;
                      end
                    else fBiasRatio := 0; //max(0, fBiasRatio - fMetaEvaluator.fBiasDec);
                end;
          end;
    end;

  function TOutputEvaluator.ShouldProduce(Block : TBlock) : boolean;
    begin
      result := (fBiasRatio > 0); // >>! and ((fMetaEvaluator.Budget = 0) or (fBudgetPercent > 0));
    end;

  function TOutputEvaluator.ShareToPullFromImput(Input : TInput) : single;
    var
      Info : TMetaInputInfo;
    begin
      Info := fMetaEvaluator.Info[Input];
      if Info <> nil
        then
          if Info.Share < 1
            then result := Info.Share*fBiasRatio/fMetaEvaluator.MaxBiasRatio
            else result := 1
        else result := 0;
    end;

  function TOutputEvaluator.MaxToPullFromImput(Block : TBlock; Input : TInput) : TFluidValue;
    var
      Output : TOutput;
      Info   : TMetaInputInfo;
      MaxCap : TFluidValue;
      Extra  : TFluidValue;
    begin
      Output := Block.Outputs[fMetaEvaluator.MetaOutput.Index];
      Info   := fMetaEvaluator.Info[Input];
      MaxCap := Output.MetaOutput.MaxFluid.Q;
      Extra  := POutputData(Output.FluidData).Extra.Q;
      if (Info <> nil) and (MaxCap > 0)
        then result := Info.Share*(1 - Extra/MaxCap)*Input.MetaInput.MaxFluid.Q*Block.dt
        else result := 0;
    end;

  function TOutputEvaluator.GetIntegratorsEffect(Block : TBlock) : single;
    var
      i   : integer;
      sum : single;
    begin
      sum := 1;
      for i := 0 to pred(fMetaEvaluator.Integrators.Count) do
        with TIntegratorInfo(fMetaEvaluator.Integrators[i]) do
          sum := sum*EffectFunct(OptValue, Block.SurfaceValue[Surface]);
      result := sqrt(sqrt(sum));
    end;

  procedure TOutputEvaluator.GetInventionsEffect(Block : TBlock; var QEffect : TInventionEfficiencyEffect; var KEffect : TInventionQualityEffect);
    begin
      TEvaluatedBlock(Block).GetInventionsEffect(QEffect, KEffect);
    end;

  procedure TOutputEvaluator.DonotProduce(Block : TBlock);
    begin
      with Block.Outputs[fMetaEvaluator.MetaOutput.Index] do
        begin
          FluidData.Q := 0; //POutputData(FluidData).Extra.Q;
          FluidData.K := 0; //POutputData(FluidData).Extra.K;
          POutputData(FluidData).Extra.Q := 0;
          POutputData(FluidData).Extra.K := 0;
        end;
      fOperationRatio := 0;
      fOperationEffic := 0;
    end;

  function TOutputEvaluator.CanIncProduction : boolean;
    begin
      result := fExtraHistory = 0;
    end;

  function TOutputEvaluator.CanDecProduction : boolean;
    begin
      result := fExtraHistory = TExtraHistory(high(fExtraHistory) shr MetaEvaluator.fTurnCount);
    end;

  procedure TOutputEvaluator.EnsureProduction(Block : TBlock);
    begin
      if fExtraHistory <> high(fExtraHistory)
        then
          with Block.Outputs[fMetaEvaluator.MetaOutput.Index] do
            Block.BlockGenMoney(POutputData(FluidData).Extra.Q*MetaOutput.MetaFluid.MarketPrice, Block.MetaBlock.ProdAccount);
    end;

  procedure TOutputEvaluator.UpdateExtraHistory(Block : TBlock);
    begin
      if POutputData(Block.Outputs[fMetaEvaluator.MetaOutput.Index].FluidData).Extra.Q > 0
        then fExtraHistory := byte(byte(fExtraHistory shl 1) or byte($01))
        else fExtraHistory := byte(fExtraHistory shl 1);
    end;

  function TOutputEvaluator.HasClients(Output : TOutput) : boolean;
    var
      Count : integer;
      conn  : integer;
      i     : integer;
    begin
      Count := Output.ConnectionCount;
      conn  := 0;
      i     := 0;
      while (i < Count) and (conn = 0) do
        if not Output.Connections[i].Block.Facility.CriticalTrouble //and (Output.ExtraConnectionInfo[i] <> nil)
          then conn := conn + 1
          else inc(i);
      result := conn > 0;
    end;

  procedure TOutputEvaluator.BeforeProduce(Block : TBlock);
    begin
    end;

  procedure TOutputEvaluator.AfterProduce(Block : TBlock);
    begin
    end;

  function TOutputEvaluator.DoProduce(Block : TBlock; WFEffic : single) : single;
    var
      OpenRatio     : single;
      Production    : POutputData;
      TotalToProd   : TFluidValue;
      ActEffic      : single;
      MetaInputInfo : TMetaInputInfo;
      Output        : TOutput;
      Input         : TInput;
      i             : integer;
      MaxInpReq     : TFluidValue;
      BasicCap      : TFluidValue;
      AdtnlEffic    : TFluidValue;
      InvQEffect    : TInventionEfficiencyEffect;
      InvKEffect    : TInventionQualityEffect;
      IntEffect     : TFluidValue;
      InputCount    : integer;
      InputRatio      : TFluidValue;
      Kb            : single;
      Ka            : single;
      //Kg            : single;
      ImpKb         : single;
      ImpKa         : single;
      ImpAdtnl      : single;
      CompSup       : single;
      CompInpQ      : single;
      CompInpK      : single;
      dt            : byte;
    begin
      try
        // Dt
        dt := Block.dt;
        // Open Ratio
        OpenRatio := fBiasRatio/fMetaEvaluator.fMaxBiasRatio;
        // Company support
        if Block.Facility.CompanyDir <> nil
          then CompSup := realmin(1, Block.Facility.CompanyDir.Support)
          else CompSup := 1;
        // Retrieve the output to calculate
        Output := Block.Outputs[fMetaEvaluator.MetaOutput.Index];
        // Production struct
        Production := POutputData(Output.FluidData);
        // Total of inputs that affect the production
        InputCount := fMetaEvaluator.InputInfoList.Count;
        // Evaluate the amount of product to produce
        TotalToProd := fMetaEvaluator.MetaOutput.MaxFluid.Q*Block.dt;
        BasicCap    := 1;
        AdtnlEffic    := 0;
        // HasBscInputs := false;
        Kb       := 0;
        Ka       := 0;
        ImpKb    := 0;
        ImpKa    := 0;
        ImpAdtnl := 0;
        // Iterate on inputs
        for i := 0 to pred(InputCount) do
          begin
            // Get the ith input info
            MetaInputInfo := TMetaInputInfo(fMetaEvaluator.InputInfoList[i]);
            // Get the ith input in the block
            Input := Block.Inputs[MetaInputInfo.MetaInput.Index];
            // Update minimun values
            case MetaInputInfo.InputKind of
              ikBasic :
                begin
                  MaxInpReq := Input.ActualMaxFluid.Q*dt;
                  if MaxInpReq > 0
                    then
                      begin
                        InputRatio := Input.FluidData.Q/MaxInpReq;
                        Kb         := Kb + MetaInputInfo.Importance*Input.FluidData.K;
                        ImpKb      := ImpKb + MetaInputInfo.Importance;
                        BasicCap   := realmin(BasicCap, InputRatio);
                      end
                end;
              ikAditional :
                begin
                  MaxInpReq := Input.ActualMaxFluid.Q*dt;
                  if MaxInpReq > 0
                    then
                      begin
                        InputRatio := Input.FluidData.Q/MaxInpReq;
                        ImpAdtnl := ImpAdtnl + MetaInputInfo.Importance;
                        Ka       := Ka + InputRatio*MetaInputInfo.Importance*Input.FluidData.K;
                        ImpKa    := ImpKa + InputRatio*MetaInputInfo.Importance;
                        AdtnlEffic := AdtnlEffic + MetaInputInfo.Importance*InputRatio;
                      end;
                end;
            end;
          end;
        // Basic Capacity
        BasicCap := BasicCap*OpenRatio;
        // Inventions
        GetInventionsEffect(Block, InvQEffect, InvKEffect);
        // Integrators
        IntEffect := GetIntegratorsEffect(Block);
        // Company Inputs
        GetCompanyInputsEfect(Block, CompInpQ, CompInpK);
        // Calculate aditional quality
        if ImpKa > 0
          then Ka := Ka/ImpKa
          else Ka := 0;
        // Calculate basic quality
        if ImpKb > 0
          then Kb := Kb/ImpKb
          else Kb := 100;
        // Production quality
        Production.K := min(100, round(CompSup*CompInpK*((InvKEffect*(1 - fMetaEvaluator.DesignMaxK/100)) + round((fMetaEvaluator.DesignMaxK/100)*((1 - fMetaEvaluator.AdtnKFrac)*Kb) + fMetaEvaluator.AdtnKFrac*Ka)))); //Production.K := min(100, round(CompInpK*(InvKEffect*(1 - fMetaEvaluator.DesignMaxK/100)) + round(CompSup*(fMetaEvaluator.DesignMaxK/100)*((1 - fMetaEvaluator.AdtnKFrac)*(GenShare*Kg + (1 - GenShare)*Kb) + fMetaEvaluator.AdtnKFrac*Ka))));
        // Production capacity
        if ImpAdtnl > 0
          then AdtnlEffic := OpenRatio*AdtnlEffic/ImpAdtnl
          else AdtnlEffic := 0;
        result   := fMetaEvaluator.DesignEffic*InvQEffect*CompInpQ;
        ActEffic := CompSup*IntEffect*((1 - fMetaEvaluator.AdtnQFrac)*BasicCap + AdtnlEffic*fMetaEvaluator.fAdtnQFrac);
        fOperationEffic := round(100*result*WFEffic);
        Production.Q := realmin(1, ActEffic*WFEffic)*TotalToProd;
        // Calculate operation ratio
        fOperationRatio := min(100, SmartRound(100*Production.Q/TotalToProd));
        // Spend budget
        Block.BlockGenMoney(-OpenRatio*fMetaEvaluator.Budget*(1 - fMetaEvaluator.fAdtnQFrac*AdtnlEffic), TMetaEvaluatedBlock(Block.MetaBlock).MantainanceAccount); // >>! Block.BlockGenMoney(-ActEffic*fMetaEvaluator.Budget*(fBudgetPercent/100)*(1 - fMetaEvaluator.fAdtnQFrac*AdtnlEffic), TMetaEvaluatedBlock(Block.MetaBlock).MantainanceAccount);
      except
        result := 1;
      end;
    end;

  function TOutputEvaluator.GetEfficiency : TPercent;
    begin
      result := fOperationEffic;
    end;

  procedure TOutputEvaluator.LoadFromBackup(Reader : IBackupReader);
    begin
      fBiasRatio      := Reader.ReadInteger('BiasRatio', 0);
      fSupplyRatio    := Reader.ReadByte('SupplyRatio', 0);
      fOperationRatio := Reader.ReadByte('OperationRatio', 0);
    end;

  procedure TOutputEvaluator.StoreToBackup(Writer : IBackupWriter);
    begin             
      Writer.WriteInteger('BiasRatio', fBiasRatio);
      Writer.WriteByte('SupplyRatio', fSupplyRatio);
      Writer.WriteByte('OperationRatio', fOperationRatio);
    end;

  procedure TOutputEvaluator.GetCompanyInputsEfect(Block : TBlock; var QEffect : single; var KEffect : single);
    var
      i         : integer;
      Info      : TCompanyInputInfo;
      InputData : PCompanyInputData;
      CompSuppl : single;
    begin
      QEffect := 1;
      KEffect := 1;
      for i := 0 to pred(MetaEvaluator.fCompInputCount) do
        begin
          Info      := MetaEvaluator.fCompanyInputs[i];
          InputData := Block.CompanyInputs[0];
          if InputData.Max > 0
            then
              begin
                CompSuppl := (InputData.K/100)*(InputData.Q/(Block.dt*InputData.Max));
                QEffect   := QEffect + CompSuppl*Info.Effic;
                KEffect   := KEffect + CompSuppl*Info.K;
              end;
        end;
    end;

  function TOutputEvaluator.EfficToOpenRatio(effic : single) : single;
    begin
      if effic < 1
        then result := realmin(EnlargeFactor, EnlargeFactor - pred(EnlargeFactor)*effic)
        else result := realmax(0.25, (3 - effic)/2);
    end;

  procedure TOutputEvaluator.PlanInputForNextPeriod(Block : TBlock; Effic : single);
    var
      InputInfo : TMetaInputInfo;
      Input     : TInput;
      i         : integer;
      ReqFluid  : TFluidValue;
    begin
      if fBiasRatio > 0
        then
          for i := 0 to pred(fMetaEvaluator.InputInfoList.Count) do
            begin
              InputInfo := TMetaInputInfo(fMetaEvaluator.InputInfoList[i]);
              Input     := Block.Inputs[InputInfo.MetaInput.Index];
              if not (mfWorkForce in Input.MetaInput.MetaFluid.Options)
                then
                  begin
                    ReqFluid :=
                      (Input.MetaInput.MaxFluid.Q/EnlargeFactor)*
                      InputInfo.Share*
                      (fBiasRatio/fMetaEvaluator.MaxBiasRatio)*
                      EfficToOpenRatio(Effic);
                    Input.ActualMaxFluid.Q := realmin(Input.MetaInput.MaxFluid.Q, Input.ActualMaxFluid.Q + ReqFluid);
                    Input.MaxCapacity := Input.MetaInput.MaxCapacity;
                  end;
            end;
    end;

  procedure TOutputEvaluator.PlanCompanyInputs(Block : TBlock; Share : single);
    var
      i : integer;
    begin
      for i := 0 to pred(MetaEvaluator.fCompInputCount) do
        with Block.CompanyInputs[MetaEvaluator.fCompanyInputs[i].Index]^ do
          Max := Max + Share*(fBiasRatio/MetaEvaluator.MaxBiasRatio)*MetaEvaluator.fCompanyInputs[i].Max;
    end;

  function TOutputEvaluator.FormatOutput(Block : TBlock; kind : TStatusKind; ShowName : boolean) : string;
    var
      Output : TOutput;
    begin
      case kind of
        sttMain :
          begin
            if ShowName
              then result := MetaEvaluator.MetaOutput.MetaFluid.Name + ' production: ' + IntToStr(fOperationRatio) + '%'
              else result := 'Producing: ' + IntToStr(fOperationRatio) + '%';
          end;
        sttSecondary :
          begin
            Output := Block.Outputs[fMetaEvaluator.MetaOutput.Index];
            result := Output.MetaOutput.MetaFluid.FormatValue(Output.FluidData.Q);
            if ShowName
              then result := result + ' of ' + Output.MetaOutput.MetaFluid.Name + ', ' + IntToStr(Output.FluidData.K) + '% quality index, ' + IntToStr(Efficiency) + '% efficiency';
          end;
      end;
    end;

  function TOutputEvaluator.CheckSupplies(Block : TBlock) : boolean;
    var
      i, count : integer;
      InputIdx : integer;
      CurInput : TInput;
      MinSupRt : single;
      tmpSupRt : single;
      minSpIdx : integer;
      dt       : byte;
    begin
      // No ill input at the beginning
      dt       := Block.dt;
      result   := true;
      MinSupRt := 1;
      minSpIdx := noIllInput;
      count    := MetaEvaluator.InputInfoList.Count;

      // Check all the inputs
      i := 0;
      while (i < count) and result do
        begin
          InputIdx := TMetaInputInfo(MetaEvaluator.InputInfoList[i]).MetaInput.Index;
          CurInput := Block.Inputs[InputIdx];
          if CurInput.MetaInput.Level = mglBasic
            then
              if CurInput.FluidData.Q <= CurInput.MetaInput.MinFluid.Q*dt
                then
                  begin
                    minSpIdx := InputIdx;
                    minSupRt := 0;
                    result   := false;
                    // Report Trouble with the supply
                    Block.Facility.ReportTrouble(facInsuficientInput);
                  end
                else
                  begin
                    inc(i);
                    tmpSupRt := CurInput.FluidData.Q/(dt*CurInput.ActualMaxFluid.Q);
                    if tmpSupRt < MinSupRt
                      then
                        begin
                          MinSupRt := tmpSupRt;
                          minSpIdx := InputIdx;
                        end;
                  end
            else inc(i);
        end;

      // Calculate the supply ratio
      fSupplyRatio := round(100*MinSupRt);

      if fSupplyRatio < MaxSupplyRatio
        then fIllInputIndex := minSpIdx
        else fIllInputIndex := noIllInput;
    end;

  procedure TOutputEvaluator.ClearInputs(Block : TBlock);
    var
      i : integer;
    begin
      for i := 0 to pred(MetaEvaluator.InputInfoList.Count) do
        Block.Inputs[TMetaInputInfo(MetaEvaluator.InputInfoList[i]).MetaInput.Index].ActualMaxFluid.Q := 0;
    end;

  // TMetaEvaluatorPool

  constructor TMetaEvaluatorPool.Create;
    begin
      inherited;
      fEvaluatorPoolClass := TEvaluatorPool;
      fMetaEvaluators     := TCollection.Create(0, rkBelonguer);
    end;

  destructor TMetaEvaluatorPool.Destroy;
    begin
      fMetaEvaluators.Free;
      inherited;
    end;

  procedure TMetaEvaluatorPool.EnlargeInputs(factor : single);
    var
      Inputs    : TCollection;
      i, j      : integer;
      Evaluator : TMetaOutputEvaluator;
      Input     : TMetaInput;
    begin
      Inputs := TCollection.Create(0, rkUse);
      try
        for i := 0 to pred(EvaluatorCount) do
          begin
            Evaluator := MetaEvaluators[i];
            for j := 0 to pred(Evaluator.InputInfoList.Count) do
              begin
                Input := TMetaInputInfo(Evaluator.InputInfoList[j]).MetaInput;
                if Inputs.IndexOf(Input) = noIndex
                  then
                    begin
                      Inputs.Insert(Input);
                      Input.MaxFluid.Q  := Input.MaxFluid.Q*factor;
                      Input.MaxCapacity := Input.MaxCapacity*factor;
                    end;
              end;
          end;
      finally
        Inputs.Free;
      end;
    end;

  procedure TMetaEvaluatorPool.AddMetaEvaluator(MetaEvaluator : TMetaOutputEvaluator);
    begin
      fMetaEvaluators.Insert(MetaEvaluator);
    end;

  function TMetaEvaluatorPool.Instantiate(aBlock : TBlock) : TEvaluatorPool;
    var
      i : integer;
    begin
      result := fEvaluatorPoolClass.Create(aBlock);
      for i := 0 to pred(fMetaEvaluators.count) do
        result.AddEvaluator(TMetaOutputEvaluator(fMetaEvaluators[i]).Instantiate(aBlock));
    end;

  function TMetaEvaluatorPool.GetEvaluatorCount : integer;
    begin
      result := fMetaEvaluators.Count;
    end;

  function TMetaEvaluatorPool.GetMetaEvaluator(index : integer) : TMetaOutputEvaluator;
    begin
      result := TMetaOutputEvaluator(fMetaEvaluators[index]);
    end;

  function TMetaEvaluatorPool.GetNetProfit : TMoney;
    var
      i : integer;
    begin
      result := 0;
      for i := 0 to pred(fMetaEvaluators.Count) do
        result := result + TMetaOutputEvaluator(fMetaEvaluators[i]).NetProfit;
    end;

  // TEvaluatorPool

  constructor TEvaluatorPool.Create(aBlock : TBlock);
    begin
      inherited Create;
      fBlock := aBlock;
      fEvaluators := TCollection.Create(0, rkBelonguer);
    end;

  destructor TEvaluatorPool.Destroy;
    begin
      fEvaluators.Free;
      inherited;
    end;

  function TEvaluatorPool.GetEvaluatorCount : integer;
    begin
      result := fEvaluators.Count;
    end;

  function TEvaluatorPool.GetOperationRatio : TPercent;
    var
      i   : integer;
      sum : integer;
      imp : integer;
    begin
      sum := 0;
      imp := 0;
      for i := 0 to pred(fEvaluators.Count) do
        with Evaluators[i] do
          begin
            sum := sum + MetaEvaluator.fImportance*fOperationRatio;
            imp := imp + MetaEvaluator.fImportance;
          end;
      result := min(100, round(sum/imp));
    end;

  function TEvaluatorPool.GetEvaluator(index : integer) : TOutputEvaluator;
    begin
      result := TOutputEvaluator(fEvaluators[index]);
    end;

  procedure TEvaluatorPool.AddEvaluator(Evaluator : TOutputEvaluator);
    begin
      if Block.OutputsByName[Evaluator.MetaEvaluator.MetaOutput.Name] <> nil
        then fEvaluators.Insert(Evaluator)
        else raise Exception.Create('Error registering an existent output in a block');
    end;


  function TEvaluatorPool.Evaluate(WFEffic : single) : TEvaluationResult;

    var
      EvlCnt        : integer;
      i             : integer;
      EvaluateTable : TEvaluateTable;
      EfficTable    : TEfficTable;
      CurEvaluator  : TOutputEvaluator;

    begin
      try
        EvlCnt := EvaluatorCount;
        // Check the trouble
        if not fBlock.Facility.CriticalTrouble
          then
            begin
              // Clear minor troubles but not the work force lack
              fBlock.Facility.ClearTrouble(facInsuficientInput or facNeedsConnection);

              // Create the evaluator table
              for i := 0 to pred(EvlCnt) do
                with Evaluators[i] do
                  EvaluateTable[i] := ShouldProduce(fBlock) and CheckSupplies(fBlock);

              // If more than one evaluator is going to be evaluated then...
              for i := 0 to pred(EvlCnt) do
                begin
                  CurEvaluator := Evaluators[i];
                  // Let ensured the extra production
                  CurEvaluator.EnsureProduction(fBlock);
                  // Update the extra record
                  CurEvaluator.UpdateExtraHistory(fBlock);
                  if EvaluateTable[i]
                    then
                      begin
                        CurEvaluator.BeforeProduce(fBlock);
                        EfficTable[i] := CurEvaluator.DoProduce(fBlock, WFEffic);
                        CurEvaluator.AfterProduce(fBlock);
                      end
                    else
                      begin
                        CurEvaluator.BeforeProduce(fBlock);
                        CurEvaluator.DonotProduce(fBlock);
                        CurEvaluator.AfterProduce(fBlock);
                        EfficTable[i] := 1;
                      end;
                  // CurEvaluator.CalcuteOperationRatio(fBlock);
                  CurEvaluator.UpdateBiasRatio(fBlock, WFEffic);
                end;

              // Clear Inputs
              for i := 0 to pred(EvlCnt) do
                Evaluators[i].ClearInputs(fBlock);

              // Clear Company Inputs
              for i := 0 to pred(Block.CompanyInputCount) do
                with Block.CompanyInputs[i]^ do
                  Max := 0;

              // Plans next period inputs
              for i := 0 to pred(EvlCnt) do
                begin
                  CurEvaluator := Evaluators[i];
                  CurEvaluator.PlanInputForNextPeriod(fBlock, EfficTable[i]);
                  CurEvaluator.PlanCompanyInputs(Block, 1/EvlCnt);
                end;
            end
          else
            for i := 0 to pred(EvlCnt) do
              Evaluators[i].DonotProduce(fBlock);
        result := evrNormal;
      except
        result := evrError;
      end;
    end;

  function TEvaluatorPool.GetHint(ToTycoon : TTycoon) : string;
    var
      count : integer;
      i     : integer;
    begin
      count := EvaluatorCount;
      if (fBlock.Facility.Trouble and facInsuficientInput <> 0) or (fBlock.Facility.Trouble and facNeedsConnection <> 0)
        then
          begin
            // Check is there are troubles with the basic inputs
            i := 0;
            while (i < count) and (Evaluators[i].fIllInputIndex = noIllInput) do
              inc(i);
            if i < count
              then result := GetHintText(hidEvalBlockNeedsBasicInput, [fBlock.Inputs[Evaluators[i].fIllInputIndex].MetaInput.MetaFluid.Name])
              else result := GetHintText(hidEvalBlockNeedsBasicInput, ['supplies']);
          end
        else
          begin
            // Check is there is somebody in troubles with the supply
            i := 0;
            while (i < count) and not Evaluators[i].NeedsMoreSupply do
              inc(i);
            if i < count
              then
                if Evaluators[i].fIllInputIndex <> noIllInput
                  then result := GetHintText(hidEvalBlockNeedsMoreSupplies, [fBlock.Inputs[Evaluators[i].fIllInputIndex].MetaInput.MetaFluid.Name, Evaluators[i].MetaEvaluator.MetaOutput.MetaFluid.Name])
                  else result := GetHintText(hidEvalBlockNeedsMoreSupplies, ['supplies', Evaluators[i].MetaEvaluator.MetaOutput.MetaFluid.Name])
              else result := GetHintText(hidVisitWebSite, [0]);
          end;
    end;

  function TEvaluatorPool.GetSecondaryTextHeader : string;
    begin
      result := 'Producing: ';
    end;

  procedure TEvaluatorPool.LoadFromBackup(Reader : IBackupReader);
    var
      i : integer;
    begin
      for i := 0 to pred(EvaluatorCount) do
        Evaluators[i].LoadFromBackup(Reader);
    end;

  procedure TEvaluatorPool.StoreToBackup(Writer : IBackupWriter);
    var
      i : integer;
    begin
      for i := 0 to pred(EvaluatorCount) do
        Evaluators[i].StoreToBackup(Writer);
    end;

  function TEvaluatorPool.WorkForceNeeded : single;
    var
      i : integer;
    begin
      result := 0;
      for i := 0 to pred(EvaluatorCount) do
        with Evaluators[i] do
          result := result + (OperationRatio/100)*(MetaEvaluator.WFPerc/100);
    end;

  procedure TEvaluatorPool.SetIllInputIndex(index : byte);
    var
      i : integer;
    begin
      for i := 0 to pred(EvaluatorCount) do
        Evaluators[i].fIllInputIndex := index;
    end;

end.


