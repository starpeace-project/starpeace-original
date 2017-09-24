unit Warehouses;

interface

  uses
    Collection, Kernel, Protocol, Surfaces, WorkCenterBlock, BackupInterfaces,
    CacheAgent, EvaluatedBlock, OutputEvaluators;

  const
    MinQualityIndex = 5;

  const
    WFEfficLimit = 0.80;
    MaxWares     = 10;

  type
    TMetaWarehouse = class;
    TMetaWare      = class;
    TWarehouse     = class;

    TMetaWarehouse =
      class(TMetaEvaluatedBlock)
        public
          constructor Create(anId : string; aCapacities : array of TFluidValue; aBlockClass : CBlock);
          destructor  Destroy; override;
        protected
          procedure NewMetaInput (Gate, Fluid : string; MaxCap : TFluidValue; datasize, offset : integer);
          procedure NewMetaOutput(Gate, Fluid : string; MaxCap : TFluidValue; datasize, offset : integer);
        private
          fMetaWares : TCollection;
        public
          procedure RegisterWare(InputName : string; aWFPerc, DepFac, DefOverPrice : TPercent; OpenTurns, CloseTurns : TBiasRatio; MaxCapacity : TFluidValue);
        private
          function GetWareCount : integer;
          function GetWare(index : integer) : TMetaWare;
        public
          property WareCount : integer read GetWareCount;
          property Wares[index : integer] : TMetaWare read GetWare;
      end;

    TMetaWare =
      class
        public
          constructor Create(MetaInput  : TMetaInput; MetaOutput : TMetaOutput; DepFac, OverPrice : TPercent);
        private
          fMetaInput  : TMetaInput;
          fMetaOutput : TMetaOutput;
          fDepFac     : TPercent;
          fOverPrice  : TPercent;
      end;

    TWareInfo      = TPercent;
    PWareInfoArray = ^TWareInfoArray;
    TWareInfoArray = array[0..MaxWares - 1] of TWareInfo;

    TWarehouse =
      class(TFinanciatedWorkCenter)
        private
          fWares : PWareInfoArray;
        private
          procedure FixPrices(count : integer);
          //function  GetPriceOf(Input : TInput; DefVal : TPercent) : single;
        protected
          function  GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string; override;
          function  Evaluate : TEvaluationResult; override;
        public
          procedure AutoConnect( loaded : boolean ); override;
        private
          function  GetWareCount : integer;
          function  GetWare(index : integer) : TMetaWare;
        public
          property  WareCount : integer read GetWareCount;
          property  Wares[index : integer] : TMetaWare read GetWare;
      end;

    TWarehouseOutputEvaluator =
      class(TOutputEvaluator)
        private
          fDemandCoverage : TPercent;
        private
          function  GetDemandCoverRatio(Output : TOutput) : TPercent;
        public
          procedure BeforeProduce(Block : TBlock); override;
          function  FormatOutput(Block : TBlock; kind : TStatusKind; ShowName : boolean) : string; override;
      end;

    TWarehouseEvaluatorPool =
      class(TEvaluatorPool)
        public
          //function  GetHint(ToTycoon : TTycoon) : string; override;
          function  GetSecondaryTextHeader : string;      override;
      end;

implementation

  uses
    SysUtils, ClassStorage, MathUtils, Population, SimHints, StdAccounts;

  // TMetaWarehouse

  constructor TMetaWarehouse.Create(anId : string; aCapacities : array of TFluidValue; aBlockClass : CBlock);
    begin
      inherited Create(anId,
        aCapacities,
        accIdx_Warehouses_Supplies,
        accIdx_Warehouses_Products,
        accIdx_Warehouses_Salaries,
        accIdx_Warehouses_Maintenance,
        aBlockClass);
      fMetaWares := TCollection.Create(0, rkBelonguer);
      MetaEvaluatorPool.EvaluatorPoolClass := TWarehouseEvaluatorPool;
    end;

  destructor TMetaWarehouse.Destroy;
    begin
      fMetaWares.Free;
      inherited;
    end;

  procedure TMetaWarehouse.NewMetaInput(Gate, Fluid : string; MaxCap : TFluidValue; datasize, offset : integer);
    begin
      MetaInputs.Insert(
        TMetaInput.Create(
          Gate,
          inputZero,
          InputData(MaxCap, 100),
          inputZero,
          MaxCap,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, Fluid]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          datasize,
          offset));
    end;

  procedure TMetaWarehouse.NewMetaOutput(Gate, Fluid : string; MaxCap : TFluidValue; datasize, offset : integer);
    begin
      MetaOutputs.Insert(
        TMetaOutput.Create(
          Gate,
          FluidData(MaxCap, 100),
          TPullOutput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, Fluid]),
          5,
          [mgoptCacheable, mgoptEditable],
          datasize,
          offset));
    end;

  procedure TMetaWarehouse.RegisterWare(InputName : string; aWFPerc, DepFac, DefOverPrice : TPercent; OpenTurns, CloseTurns : TBiasRatio; MaxCapacity : TFluidValue);
    var
      Input  : TMetaInput;
      Output : TMetaOutput;
    begin
      Input  := InputByName[InputName];
      Output := OutputByName[InputName];
      if (Input <> nil) and (Output <> nil)
        then
          begin
            fMetaWares.Insert(
              TMetaWare.Create(
                Input,
                Output,
                DepFac,
                DefOverPrice));

            // MetaEvaluators
            with TMetaOutputEvaluator.Create(
              0,
              0,
              Output,
              TWarehouseOutputEvaluator) do
              begin
                AdtnQFrac := 0;
                AdtnKFrac := 0;
                WFPerc    := aWFPerc;
                FullOpenTime  := OpenTurns;
                FullCloseTime := CloseTurns;
                MaxBiasRatio  := min(high(TBiasRatio), round(MaxCapacity));
                RegisterInput(
                  TMetaInputInfo.Create(
                    Input,
                    1000,
                    1));
                Register( MetaEvaluatorPool );
              end;
          end;
    end;

  function TMetaWarehouse.GetWareCount : integer;
    begin
      result := fMetaWares.Count;
    end;

  function TMetaWarehouse.GetWare(index : integer) : TMetaWare;
    begin
      result := TMetaWare(fMetaWares[index]);
    end;

  // TMetaWare

  constructor TMetaWare.Create(MetaInput : TMetaInput; MetaOutput : TMetaOutput; DepFac, OverPrice : TPercent);
    begin
      fMetaInput  := MetaInput;
      fMetaOutput := MetaOutput;
      fDepFac     := DepFac;
      fOverPrice  := OverPrice;
    end;

  // TWarehouse

  procedure TWarehouse.FixPrices(count : integer);
    var
      i : integer;
    begin
      if count > 0
        then
          begin
            with TMetaWarehouse(MetaBlock) do
              for i := 0 to pred(WareCount) do
                Outputs[Wares[i].fMetaOutput.Index].PricePerc := Wares[i].fOverPrice;
          end;
    end;

  function TWarehouse.GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string;
    var
      OutputAbstract : string;
      i              : integer;
      k              : TPeopleKind;
    begin
      result := inherited GetStatusText(kind, ToTyCoon);
      case kind of
        sttMain :
          begin
            if not Facility.CriticalTrouble
              then
                if Facility.Trouble and facNeedsWorkForce <> 0
                  then result := result + 'Hiring workforce at ' + IntToStr(round(100*WorkForceEfficiency)) + '%.'
                  else
                    begin
                      for i := 0 to pred(WareCount) do
                        begin
                          OutputAbstract := OutputAbstract + ' ' + Wares[i].fMetaInput.MetaFluid.Name + ' ' + IntToStr(fWares[i]) + '%';
                          if i <> pred(WareCount)
                            then OutputAbstract := OutputAbstract + LineBreak
                            else OutputAbstract := OutputAbstract + '.'
                        end;
                      result := result + OutputAbstract;
                    end;
          end;
        sttSecondary :
          begin
            if not Facility.CriticalTrouble
              then
                if Facility.Trouble and facNeedsWorkForce = 0
                  then
                    begin
                      for i := 0 to pred(WareCount) do
                        begin
                          result := result + ' ' + Wares[i].fMetaInput.MetaFluid.Name + ' ' + Wares[i].fMetaInput.MetaFluid.FormatValue(Wares[i].fMetaOutput.MaxFluid.Q*fWares[i]/100);
                          if i <> pred(WareCount)
                            then result := result + ', '
                            else result := result + '.';
                        end;
                    end
                  else
                    with TMetaWorkCenter(MetaBlock) do
                      for k := low(k) to high(k) do
                        if Capacity[k] > 0
                          then
                            begin
                              if WorkersMax[k].Q > 0
                                then
                                  begin
                                    result := result +
                                      WorkForceKindName[k] + ': ' +
                                      IntToStr(round(Workers[k].Q)) + ' of ' +
                                      IntToStr(round(WorkersMax[k].Q)) + '. ';
                                  end;
                            end;
          end;
        sttHint :
          case Facility.AccessLevelOf( ToTycoon ) of
            acsFull, acsModerate :
              begin
                if Facility.Trouble and facNeedCompSupport <> 0
                  then result := GetHintText(hidNeedsCompSupport, [0])
              end;
          end;
      end;
    end;

  {
  function TWarehouse.GetPriceOf(Input : TInput; DefVal : TPercent) : single;
    begin
      if Input.FluidData.Q > 0
        then result := Input.LastCost/(Input.FluidData.Q*Input.MetaInput.MetaFluid.MarketPrice)
        else result := DefVal;
    end;
  }

  function TWarehouse.Evaluate : TEvaluationResult;
    var
      i       : integer;
      Output  : TOutput;
      Input   : TInput;
      Total   : TFluidValue;
      WEffic  : single;
      OpRatio : integer;
    begin
      result := inherited Evaluate;
      if not Facility.CriticalTrouble
        then
          begin
            OpRatio := 0;
            WEffic  := WorkForceEfficiency;
            for i := 0 to pred(WareCount) do
              with TMetaWarehouse(MetaBlock).Wares[i] do
                begin
                  Input  := Inputs[fMetaInput.Index];
                  Output := Outputs[fMetaOutput.Index];
                  // Total Quality
                  Output.FluidData.K := AverageK(Input.FluidData, @(POutputData(Output.FluidData).Extra));
                  // Total Fluid
                  Total := POutputData(Output.FluidData).Extra.Q + Input.FluidData.Q;
                  Output.FluidData.Q := WEffic*Total;
                  POutputData(Output.FluidData).Extra.Q := 0;
                  // Next period
                  Input.ActualMaxFluid.Q := realmax(0, Output.MetaOutput.MaxFluid.Q - Total);
                  fWares[i] := min(100, round(100*Total/(Output.MetaOutput.MaxFluid.Q*dt)));
                  OpRatio := OpRatio + fWares[i];
                end;
            OpRatio := round(OpRatio/WareCount);
            HireWorkForce(1);
            SetCargoValue( TMetaEvaluatedBlock(MetaBlock).CargoKind, OpRatio/10 );
          end;
    end;

  procedure TWarehouse.AutoConnect( loaded : boolean );
    begin
      inherited;
      ReallocMem(fWares, WareCount*sizeof(fWares[0]));
      FillChar(fWares^, WareCount*sizeof(fWares[0]), 0);
      if not loaded
        then FixPrices(WareCount);
    end;

  function TWarehouse.GetWareCount : integer;
    begin
      result := TMetaWarehouse(MetaBlock).WareCount;
    end;

  function TWarehouse.GetWare(index : integer) : TMetaWare;
    begin
      result := TMetaWarehouse(MetaBlock).Wares[index];
    end;


  // TWarehouseOutputEvaluator

  function TWarehouseOutputEvaluator.GetDemandCoverRatio(Output : TOutput) : TPercent;
    var
      Q : TFluidValue;
      D : TFluidValue;
    begin
      Q := Output.FluidData.Q;
      if POutputData(Output.FluidData).Extra.Q > 0
        then result := 100
        else
          begin
            D := Output.Demand;
            if D > 0
              then
                if Q/Output.Block.dt < Output.MetaOutput.MaxFluid.Q
                  then result := round(100*Q/(Q + D))
                  else result := 100
              else result := 100
          end;
    end;

  procedure TWarehouseOutputEvaluator.BeforeProduce(Block : TBlock);
    begin
      fDemandCoverage := GetDemandCoverRatio(Block.Outputs[MetaEvaluator.MetaOutput.Index]);
    end;

  function TWarehouseOutputEvaluator.FormatOutput(Block : TBlock; kind : TStatusKind; ShowName : boolean) : string;
    var
      Output : TOutput;
    begin
      case kind of
        sttMain :
          begin
            if ShowName
              then result := MetaEvaluator.MetaOutput.MetaFluid.Name + ' covers ' + IntToStr(fDemandCoverage) + '% of demand'
              else result := 'Covering ' + IntToStr(fDemandCoverage) + '% of demand';
          end;
        sttSecondary :
          begin
            Output := Block.Outputs[MetaEvaluator.MetaOutput.Index];
            result := Output.MetaOutput.MetaFluid.FormatValue(Output.FluidData.Q);
            if ShowName
              then result := result + ' of ' + Output.MetaOutput.MetaFluid.Name + ', ' + IntToStr(Output.FluidData.K) + '% quality index';
          end;
      end;
    end;

  // TWarehouseEvaluatorPool

{
  function TWarehouseEvaluatorPool.GetHint(ToTycoon : TTycoon) : string;
    var
      i      : integer;
      aux    : string;
      Output : TOutput;
    begin
      aux := '';
      with TWarehouse(Block) do
        begin
          if Facility.Trouble and facNeedsWorkForce <> 0
            then result := GetHintText(hidNeedsWorkForce, [0]) // >> say what kind...
            else
              begin
                for i := 0 to pred(WareCount) do
                  begin
                    Output := Block.Outputs[Wares[i].fMetaOutput.Index];
                    if Output.Demand > 0
                      then
                        if aux <> ''
                          then aux := aux + ', ' + Output.MetaOutput.Name
                          else aux := Output.MetaOutput.Name;
                  end;
                if aux <> ''
                  then aux := 'Find more: ' + aux + '.'
              end;
        end;
    end;
}

  function TWarehouseEvaluatorPool.GetSecondaryTextHeader : string;
    begin
      result := 'Delivering: ';
    end;


end.
