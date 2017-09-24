unit Steps;

interface

  uses
    MissionEngine, IBSystem;

  type
    TPoliceShooting =
      class(TMissionStep)
        public
          procedure Execute( time : integer; var IsOver : boolean; var StepParameters : TStepParameters; var MissionParameters : TMissionParameters ); override;
      end;

    TArrestedWithInjuries =
      class(TMissionStep)
        public
          procedure ProcessOutcome( res : TMissionResult; StepParameters : TStepParameters; var MissionParameters : TMissionParameters ); override;
      end;

    TSurrender =
      class(TMissionStep)
        public
          procedure ProcessOutcome( res : TMissionResult; StepParameters : TStepParameters; var MissionParameters : TMissionParameters ); override;
      end;

  procedure RegisterStepClasses;

implementation


  // TPoliceShooting

  procedure TPoliceShooting.Execute( time : integer; var IsOver : boolean; var StepParameters : TStepParameters; var MissionParameters : TMissionParameters );
    begin
      //>> to do it all
    end;

  // TArrestedWithInjuries

  procedure TArrestedWithInjuries.ProcessOutcome( res : TMissionResult; StepParameters : TStepParameters; var MissionParameters : TMissionParameters );
    begin
    end;

  // TSurrender

  procedure TSurrender.ProcessOutcome( res : TMissionResult; StepParameters : TStepParameters; var MissionParameters : TMissionParameters );
    begin
    end;

  procedure RegisterStepClasses;
    begin
      RegisterStepClass( 'PoliceShooting', TPoliceShooting );
      RegisterStepClass( 'ArrestedWithInjuries', TArrestedWithInjuries );
      RegisterStepClass( 'Surrender', TSurrender );
    end;

end.
