unit AutomaticEvaluatedBlocks;

interface

  uses
    Collection, Kernel, OutputEvaluators;

  const
    MaxEvaluators        = 8;  // >> This figure might not change
    MaxInputsByEvaluator = 10; // >> This figure might change

  const
    InputValueEps = 0.5; // Value to lower bound input values

  type
    TShareOutTable = array[0..MaxEvaluators-1, 0..MaxInputsByEvaluator-1] of TFluidValue;
    TEvaluateTable = array[0..MaxEvaluators-1] of boolean;

    TAutomaticEvaluatedBlock =
      class(TBlock)
        public
          constructor Create( aMetaBlock : TMetaBlock; aFacility : TFacility ); override;
          destructor  Destroy; override;
        private
          fEvaluators : TCollection;
        private
          function GetEvaluatorCount : integer;
          function GetEvaluator(index : integer) : TOutputEvaluator;
        public
          property EvaluatorCount : integer read GetEvaluatorCount;
          property Evaluators[index : integer] : TOutputEvaluator read GetEvaluator;
        public
          function Evaluate : TEvaluationResult; override;
        public
          procedure RegisterOutputEvaluator(Evaluator : TOutputEvaluator);
      end;

implementation

  uses
    Classes, SysUtils, MathUtils;

  // TAutomaticEvaluatedBlock

  constructor TAutomaticEvaluatedBlock.Create( aMetaBlock : TMetaBlock; aFacility : TFacility );
    begin
      inherited;
      fEvaluators := Collection.TCollection.Create(0, rkBelonguer);
      // >> ??
    end;

  destructor TAutomaticEvaluatedBlock.Destroy;
    begin
      fEvaluators.Free;
      inherited;
    end;

  function TAutomaticEvaluatedBlock.GetEvaluatorCount : integer;
    begin
      result := fEvaluators.Count;
    end;

  function TAutomaticEvaluatedBlock.GetEvaluator(index : integer) : TOutputEvaluator;
    begin
      result := TOutputEvaluator(fEvaluators[index]);
    end;

  function TAutomaticEvaluatedBlock.Evaluate : TEvaluationResult;
    var
      ShareOutTable  : TShareOutTable;
      EvaluateTable  : TEvaluateTable;
      CurInput       : TInput;
      CurEvaluator   : TOutputEvaluator;
      i, j           : integer;
      count          : integer;
      share          : TFluidValue;
      need           : boolean;
      InputValue     : TFluidValue;
    begin
      // Init shareout table
      FillChar(ShareOutTable, sizeof(ShareOutTable), 0);
      // Fill evaluator table
      count := 0;
      for i := 0 to pred(EvaluatorCount) do
        begin
          if Evaluators[i].ShouldProduce
            then
              begin
                EvaluateTable[i] := true;
                inc(count);
              end
            else EvaluateTable[i] := false;
        end;
      // Check if there is at least one to produce
      if count > 0
        then
          begin
            // Check for inputs to find out who can and who cannot evaluate
            count := 0;
            for i := 0 to pred(InputCount) do
              begin
                CurInput := Inputs[i];
                for j := 0 to pred(EvaluatorCount) do
                  begin
                    need := Evaluators[j].NeedsInput(CurInput);
                    if not need
                      then ShareOutTable[j, i] := -1
                      else EvaluateTable[i] := EvaluateTable[i] and (CurInput.FluidData.Q > 0) // >>
                  end;
                if EvaluateTable[i]
                  then inc(count);
              end;
            // Check if there are some one to evaluate
            if count > 0
              then
                begin
                  // Sharing inputs
                  for i := 0 to pred(InputCount) do
                    begin
                      CurInput   := Inputs[i];
                      InputValue := CurInput.FluidData.Q;
                      while InputValue > InputValueEps do // >>
                        begin
                          for j := 0 to pred(EvaluatorCount) do
                            begin
                              CurEvaluator := Evaluators[j];
                              if EvaluateTable[j] and (ShareOutTable[j, i] <> -1)
                                then
                                  begin
                                    share := CurEvaluator.ShareToPullFromImput(CurInput);
                                    InputValue := InputValue - share;
                                    ShareOutTable[j, i] := realmin( ShareOutTable[j, i] + share, CurEvaluator.MaxToPullFromImput(CurInput));
                                  end;
                            end;
                          CurInput.FluidData.Q := InputValue;
                        end;
                    end;
                end;
          end;
      // For suitability in the algorithm, now lets clear the fluid to pull
      for i := 0 to InputCount do
        Inputs[i].ActualMaxFluid.Q := 0;
      // Evaluate & mix the remainders
      for i := 0 to pred(EvaluatorCount) do
        begin
          CurEvaluator := Evaluators[i];
          if EvaluateTable[i]
            then
              begin
                for j := 0 to pred(InputCount) do
                  begin
                    InputValue := ShareOutTable[i, j];
                    if InputValue > 0
                      then Inputs[j].FluidData.Q := InputValue;
                  end;
                CurEvaluator.Evaluate;
              end;
          CurEvaluator.MixRemainder;
          CurEvaluator.PlanInputForNextPeriod;
        end;
      result := evrNormal;
    end;

  procedure TAutomaticEvaluatedBlock.RegisterOutputEvaluator(Evaluator : TOutputEvaluator);
    begin
      if OutputsByName[Evaluator.MetaEvaluator.MetaOutput.Name] = nil
        then raise Exception.Create('Wrong output "' + Evaluator.MetaEvaluator.MetaOutput.Name + '" for this block')
        else fEvaluators.Insert(Evaluator);
    end;

end.
