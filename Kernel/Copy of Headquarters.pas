unit Headquarters;

interface

  uses
    Protocol, Kernel, ResearchCenter;

  type
    TMetaHeadquarterBlock =
      class( TMetaResearchCenter )
        public
          constructor Create( anId              : string;
                              aCapacities       : array of TFluidValue;
                              anInventionKind   : string;
                              aBaseTechnologyId : TInventionNumId;
                              aBlockClass       : CBlock );
        private
          fBaseTechnologyId : TInventionNumId;
        public
          property BaseTechnologyId : TInventionNumId read fBaseTechnologyId;
      end;

    THeadquarterBlock =
      class( TResearchCenter )
        public
          procedure AutoConnect( loaded : boolean ); override;
      end;

    TMetaMainHeadquarter =
      class( TMetaHeadquarterBlock )
        public
          constructor Create( anId              : string;
                              aCapacities       : array of TFluidValue;
                              anInventionKind   : string;
                              aBaseTechnologyId : TInventionNumId;
                              aBlockClass       : CBlock );
      end;

    TMainHeadquarter =
      class( THeadquarterBlock )
        private
          fAdvertisement : TInputData;
        protected
          function Evaluate : TEvaluationResult; override;
        public
          function GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string; override;
        private
          fPrestigeBoost : TPrestige;
      end;

  procedure RegisterBackup;

implementation

  uses
    SysUtils, BackupInterfaces, ClassStorage, StdFluids;


  // TMetaHeadquarterBlock

  constructor TMetaHeadquarterBlock.Create( anId : string; aCapacities : array of TFluidValue; anInventionKind : string; aBaseTechnologyId : TInventionNumId; aBlockClass : CBlock );
    begin
      inherited Create( anId, aCapacities, anInventionKind, aBlockClass );
      fBaseTechnologyId := aBaseTechnologyId;
      RegisterInvention( fBaseTechnologyId );
    end;


  // THeadquarterBlock

  procedure THeadquarterBlock.AutoConnect( loaded : boolean );
    begin
      inherited;
      if not loaded
        then QueueResearch( TMetaHeadquarterBlock(MetaBlock).BaseTechnologyId, 0 );
    end;


  // TMetaMainHeadquarter

  const
    MaxAd = 1000;

  constructor TMetaMainHeadquarter.Create( anId              : string;
                                           aCapacities       : array of TFluidValue;
                                           anInventionKind   : string;
                                           aBaseTechnologyId : TInventionNumId;
                                           aBlockClass       : CBlock );
    var
      Sample : TMainHeadquarter;
    begin
      inherited Create( anId, aCapacities, anInventionKind, aBaseTechnologyId, aBlockClass );
      Sample := nil;
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Advertisement,
          inputZero,
          InputData(MaxAd, 100),
          inputZero,
          MaxAd,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_Advertisement]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fAdvertisement),
          Sample.Offset(Sample.fAdvertisement)));
    end;


  // TMainHeadquarter

  function TMainHeadquarter.Evaluate : TEvaluationResult;
    begin
      result := inherited Evaluate;
      fPrestigeBoost := fAdvertisement.Q*fAdvertisement.K/100;
      with Facility.Company.Owner do
        CurrFacPrestige := CurrFacPrestige + fPrestigeBoost;
    end;

  function TMainHeadquarter.GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string;
    begin
      result := inherited GetStatusText( kind, ToTycoon );
      case kind of
        sttSecondary :
          result := result + ' ' + IntToStr(round(fPrestigeBoost)) + ' prestige points from publicity.';
      end;
    end;


  // RegisterBackup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass( THeadquarterBlock );
      BackupInterfaces.RegisterClass( TMainHeadquarter );
    end;


end.


