unit SimTst1Blks;

interface

  uses
    Kernel;

  type
    TA =
      class( TBlock )
        public
          fX : TInputData;
          fY : TOutputData;
        protected
          function Evaluate : TEvaluationResult; override;
        public
          class procedure RegisterMetaBlock;
      end;

  var
    A1, A2, A3, A4 : TA;
    Noise : TFluidValue;

  const
    Adapt : boolean = false;

  procedure InitBlocks;

implementation

  uses
    ClassStorage;

    
  // TA

  function TA.Evaluate : TEvaluationResult;
    begin
      if (Noise > 0) and (self = A1)
        then
          begin
            fX.Q := Noise;
            Noise := 0;
          end;
      fY.Q := 9*fX.Q/10 + fY.Extra.Q;
      if Adapt and (self <> A1)
        then fX.S := round(50/(fY.Extra.Q/100 + 1))
        else fX.S := 50;
      result := evrNormal;
    end;
    
  class procedure TA.RegisterMetaBlock;
    var
      MB     : TMetaBlock;
      Sample : TA;
    begin
      Sample   := nil;
      MB       := TMetaBlock.Create( 'BlockA', 100, TA );
      MB.xSize := 3;
      MB.ySize := 3;
      MB.MetaInputs.Insert(
        TMetaInput.Create(
          'X',
          InputData( 0, 0, 10 ),
          {$IFDEF PUSH}
          InputData( 1000, 100, 10 ),
          {$ELSE}
          InputData( 2000, 100, 10 ),
          {$ENDIF}
          InputData( 0, 0, 10 ),
          1000,
          {$IFDEF PULL}
          TPullInput,
          {$ELSE}
          TPushInput,
          {$ENDIF}
          TheClassStorage.ClassById['Fluids', 'Test'] as TMetaFluid,
          3,
          Sample.Offset( Sample.fX )));
      MB.MetaOutputs.Insert(
        TMetaOutput.Create(
          'Y',
          FluidData( 10000, 100 ),
          {$IFDEF PULL}
          TPullOutput,
          {$ELSE}
          TPushOutput,
          {$ENDIF}
          TheClassStorage.ClassById['Fluids', 'Test'] as TMetaFluid,
          3,
          Sample.Offset( Sample.fY )));
      MB.Register( 'Blocks' );
    end;


  procedure InitBlocks;
    var
      MB : TMetaBlock;
      MF : TMetaFacility;
      F  : TFacility;
    begin
      TMetaFluid.Create( 'Test', 'Test', 'Sample Fluid', 0, 100 ).Register( 'Fluids' );
      TA.RegisterMetaBlock;
      F := TFacility.Create;

      MB := TheClassStorage.ClassById['Blocks', 'BlockA'] as TMetaBlock;
      
      MF := TMetaFacility.Create( 'A', 'Test Facility (A)', TFacility );
      MF.XSize := 3;
      MF.YSize := 3;
      MF.EvlStages.Insert( TEvlStage.Create( 'Normal', 'Normal', 'Suddenly achieved', MB ));
      MF.Register( 'Facilities' );

      A1 := MB.Instantiate( nil ) as TA;
      A2 := MB.Instantiate( nil ) as TA;
      A3 := MB.Instantiate( nil ) as TA;
      A4 := MB.Instantiate( nil ) as TA;

      A1.Facility := F;
      A2.Facility := F;
      A3.Facility := F;
      A4.Facility := F;

      {$IFDEF PULL}
      A2.Inputs[0].ActualMaxFluid.Q := 500;
      A3.Inputs[0].ActualMaxFluid.Q := 1000;
      A4.Inputs[0].ActualMaxFluid.Q := 10000;
      {$ENDIF}

      A1.Outputs[0].ConnectTo( A2.Inputs[0] );
      A1.Outputs[0].ConnectTo( A3.Inputs[0] );
      A1.Outputs[0].ConnectTo( A4.Inputs[0] );
      A2.Outputs[0].ConnectTo( A1.Inputs[0] );
      A3.Outputs[0].ConnectTo( A1.Inputs[0] );
      A4.Outputs[0].ConnectTo( A1.Inputs[0] );
    end;


end.


