unit SimTst2Blks;

interface

  uses
    Kernel;

  type
    TA =
      class( TBlock )
        private
          fX : TInputData;
          fY : TOutputData;
        protected
          function Evaluate : TEvaluationResult; override;
        public
          class procedure RegisterMetaBlock;
      end;

    TB =
      class( TBlock )
        private
          fX     : TInputData;
          fY     : TOutputData;
          fXpull : TInputData;
          fYpull : TOutputData;
        protected
          function Evaluate : TEvaluationResult; override;
        public
          class procedure RegisterMetaBlock;
      end;

  procedure InitBlocks;
  procedure SimBlocks;

  const
    Asim : integer = 0;
    Bsim : integer = 0;

implementation

  uses
    ClassStorage, Collection;

  const
    ABlock  : TBlock = nil;
    BBlocks : TCollection = nil;

  const
    BlockCount = 10000;

  // TA

  function TA.Evaluate : TEvaluationResult;
    begin
      if fX.Q = 0
        then fX.Q := 100*BlockCount;
      fY.Q := fX.Q + fY.Extra.Q;
      inc( Asim );
      result := evrNormal;
    end;

  class procedure TA.RegisterMetaBlock;
    var
      Sample : TA;
    begin
      Sample := nil;
      with TMetaBlock.Create( 'BlockA', 0, TA ) do
        begin
          MetaInputs.Insert(
            TMetaInput.Create(
              'X',
              InputData( 0, 0, 10 ),
              {$IFDEF PUSH}
              InputData( 100000, 100, 10 ),
              {$ELSE}
              InputData( 100000, 100, 10 ),
              {$ENDIF}
              InputData( 0, 0, 10 ),
              100000,
              {$IFDEF PULL}
              TPullInput,
              {$ELSE}
              TPushInput,
              {$ENDIF}
              TheClassStorage.ClassById['Fluids', 'Test'] as TMetaFluid,
              BlockCount,
              Sample.Offset( Sample.fX )));
          MetaOutputs.Insert(
            TMetaOutput.Create(
              'Y',
              FluidData( 100000, 100 ),
              {$IFDEF PULL}
              TPullOutput,
              {$ELSE}
              TPushOutput,
              {$ENDIF}
              TheClassStorage.ClassById['Fluids', 'Test'] as TMetaFluid,
              BlockCount,
              Sample.Offset( Sample.fY )));
          Register( 'Blocks' );
        end;
    end;


  // TB

  function TB.Evaluate : TEvaluationResult;
    begin
      fY.Q     := 100*fX.Q;
      fYpull.Q := random( 60000 );
      fX.S     := random(100);
      inc( Bsim );
      result := evrNormal;
    end;

  class procedure TB.RegisterMetaBlock;
    var
      Sample : TB;
    begin
      Sample := nil;
      with TMetaBlock.Create( 'BlockB', 0, TB ), Sample do
        begin
          MetaInputs.Insert(
            TMetaInput.Create(
              'X',
              InputData( 0, 0, 10 ),
              {$IFDEF PUSH}
              InputData( 1000, 100, 10 ),
              {$ELSE}
              InputData( 2000, 100, 10 ),
              {$ENDIF}
              InputData( 0, 0, 10 ),
              100000,
              {$IFDEF PULL}
              TPullInput,
              {$ELSE}
              TPushInput,
              {$ENDIF}
              TheClassStorage.ClassById['Fluids', 'Test'] as TMetaFluid,
              1,
              Sample.Offset( Sample.fX )));
          MetaOutputs.Insert(
            TMetaOutput.Create(
              'Y',
              FluidData( 10000, 100 ),
              {$IFDEF PULL}
              TPullOutput,
              {$ELSE}
              TPushOutput,
              {$ENDIF}
              TheClassStorage.ClassById['Fluids', 'Test'] as TMetaFluid,
              1,
              Offset( fY )));
          MetaInputs.Insert(
            TMetaInput.Create(
              'XPul',
              InputData( 0, 0, 10 ),
              InputData( 2000, 100, 10 ),
              InputData( 0, 0, 10 ),
              100000,
              TPullInput,
              TheClassStorage.ClassById['Fluids', 'Test'] as TMetaFluid,
              5,
              Offset( Sample.fXpull )));
          MetaOutputs.Insert(
            TMetaOutput.Create(
              'Ypull',
              FluidData( 10000, 100 ),
              TPullOutput,
              TheClassStorage.ClassById['Fluids', 'Test'] as TMetaFluid,
              5,
              Offset( fYpull )));
          Register( 'Blocks' );
        end;
    end;

  const
    ColSize = 5;
    RowSize = BlockCount div ColSize;

  procedure InitBlocks;
    var
      i, j, k : integer;
      B       : TBlock;
      F       : TFacility;
    begin
      InitTheClassStorage;
      TMetaFluid.Create( 'Test', 'Test', 'Sample Fluid', 0, 100 ).Register( 'Fluids' );
      TA.RegisterMetaBlock;
      TB.RegisterMetaBlock;
      F := TFacility.Create;
      ABlock  := (TheClassStorage.ClassById['Blocks', 'BlockA'] as TMetaBlock).Instantiate( nil );
      ABlock.Facility := F;
      BBlocks := TCollection.Create( 0, rkBelonguer );
      for i := 1 to BlockCount do
        begin
          B := (TheClassStorage.ClassById['Blocks', 'BlockB'] as TMetaBlock).Instantiate( nil );
          B.Facility := F;
          BBlocks.Insert( B );
          with B do
            begin
              Inputs[0].ConnectTo( ABlock.Outputs[0] );
              Outputs[0].ConnectTo( ABlock.Inputs[0] );
            end;
        end;
      for i := 0 to RowSize - 2 do
        for j := 0 to ColSize - 1 do
          for k := 0 to ColSize - 1 do
            begin
              TBlock(BBlocks[ColSize*i + j]).Outputs[1].ConnectTo( TBlock(BBlocks[ColSize*succ(i) + k]).Inputs[1] );
            end;
    end;

  procedure SimBlocks;
    var
      i : integer;
    begin
      ABlock.Simulate;
      for i := 0 to pred(BBlocks.Count) do
        (BBlocks[i] as TBlock).Simulate;
    end;


end.

