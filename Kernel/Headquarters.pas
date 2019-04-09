unit Headquarters;

interface

  uses
    Protocol, Kernel, ResearchCenter, Variants;                   

  type
    TMetaHeadquarterBlock =
      class( TMetaResearchCenter )
        public
          constructor Create( anId            : string;
                              aCapacities     : array of TFluidValue;
                              anInventionKind : string;
                              aBlockClass     : CBlock );
      end;

    THeadquarterBlock =
      class( TResearchCenter )
        public
          procedure AutoConnect( loaded : boolean ); override;
      end;                                

    TMetaMainHeadquarter =
      class( TMetaHeadquarterBlock )
        public
          constructor Create( anId            : string;
                              aCapacities     : array of TFluidValue;
                              anInventionKind : string;
                              aBlockClass     : CBlock );
      end;

    TMainHeadquarter =
      class( THeadquarterBlock )              
        private
          fAdvertisement : TInputData;
          fLegalServ     : TInputData;
          fCompServ      : TInputData;
        public
          function GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string; override;
      end;

    TMetaAreaHeadQuarter =
      class(TMetaHeadquarterBlock)
        public
          constructor Create( anId            : string;
                              aCapacities     : array of TFluidValue;
                              anInventionKind : string;
                              aSoftMax        : TFluidValue;
                              aLegalMax       : TFluidValue;
                              aBlockClass     : CBlock );
      end;

    TAreaHeadquarter =
      class(THeadquarterBlock)
      end;

    TMetaPublicAffairsHeadquarter =
      class( TMetaHeadquarterBlock )
        public
          constructor Create( anId            : string;
                              aCapacities     : array of TFluidValue;
                              anInventionKind : string;
                              aMaxHits        : TFluidValue;
                              aBlockClass     : CBlock );
      end;

    TPublicAffairsHeadquarter =
      class( THeadquarterBlock )
        protected
          function  Evaluate : TEvaluationResult; override;
        public
          function GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string; override;
        private
          fPrestigeBoost : TPrestige;
      end;

  procedure RegisterBackup;

implementation

  uses
    SysUtils, BackupInterfaces, ClassStorage, StdFluids, SimHints, Inventions,
    MathUtils;


  // TMetaHeadquarterBlock

  constructor TMetaHeadquarterBlock.Create( anId : string; aCapacities : array of TFluidValue; anInventionKind : string; aBlockClass : CBlock );
    begin
      inherited Create( anId, aCapacities, anInventionKind, aBlockClass );
    end;


  // THeadquarterBlock

  procedure THeadquarterBlock.AutoConnect( loaded : boolean );
    var
      i   : integer;
      Inv : TInvention;
    begin
      inherited;
      if not loaded
        then
          for i := 0 to pred(TMetaHeadquarterBlock(MetaBlock).InventionCount) do
            begin
              Inv := TMetaHeadquarterBlock(MetaBlock).Inventions[i];
              if Inv.Basic and not Facility.Company.HasInvention[Inv.NumId] and Inv.Enabled(Facility.Company)
                then ResearchInvention(Inv, 0);
            end;
    end;


  // TMetaMainHeadquarter

  const
    MaxAd        = 1000000;
    MaxLegalServ = 10000;
    MaxCompServ  = 10000;

  constructor TMetaMainHeadquarter.Create( anId            : string;
                                           aCapacities     : array of TFluidValue;
                                           anInventionKind : string;
                                           aBlockClass     : CBlock );
    var
      Sample : TMainHeadquarter;
    begin
      inherited Create( anId, aCapacities, anInventionKind, aBlockClass );
      Sample := nil;
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Advertisement,
          inputZero,
          InputData(MaxAd, 100),
          inputZero,
          MaxUpgrade*MaxAd,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_Advertisement]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fAdvertisement),
          Sample.Offset(Sample.fAdvertisement)));
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_LegalServ,
          inputZero,
          InputData(MaxLegalServ, 100),
          inputZero,
          MaxUpgrade*MaxLegalServ,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_LegalServ]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fLegalServ),
          Sample.Offset(Sample.fLegalServ)));
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_CompServ,
          inputZero,
          InputData(MaxCompServ, 100),
          inputZero,
          MaxUpgrade*MaxCompServ,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_CompServ]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fCompServ),
          Sample.Offset(Sample.fCompServ)));
    end;

  function TMainHeadquarter.GetStatusText(kind : TStatusKind; ToTycoon : TTycoon) : string;
    begin
      result := inherited GetStatusText(kind, ToTycoon);
      case kind of
        sttSecondary :
          result := result + ' ' + SimHints.GetHintText(mtidImplementationCost.Values[ToTycoon.Language], [MathUtils.FormatMoney(Facility.Company.ResearchCost)]);
      end;
    end;

  // TMetaAreaHeadQuarter

  constructor TMetaAreaHeadQuarter.Create( anId            : string;
                                           aCapacities     : array of TFluidValue;
                                           anInventionKind : string;
                                           aSoftMax        : TFluidValue;
                                           aLegalMax       : TFluidValue;
                                           aBlockClass     : CBlock );
    begin
      inherited Create(anId, aCapacities, anInventionKind, aBlockClass);
      RegisterCompanyInput(tidFluid_CompServ, aSoftMax, false);
      RegisterCompanyInput(tidFluid_LegalServ, aLegalMax, false);
    end;

  // TMetaPublicAffairsHeadquarter

  constructor TMetaPublicAffairsHeadquarter.Create( anId : string; aCapacities : array of TFluidValue; anInventionKind : string; aMaxHits : TFluidValue; aBlockClass : CBlock );
    begin
      inherited Create( anId, aCapacities, anInventionKind, aBlockClass );
      RegisterCompanyInput(tidFluid_Advertisement, aMaxHits, true);
    end;


  // TPublicAffairsHeadquarter

  function TPublicAffairsHeadquarter.Evaluate : TEvaluationResult;
    var
      AdvInput : PCompanyInputData;
    begin
      result := inherited Evaluate;
      AdvInput := CompanyInputs[0];
      fPrestigeBoost := AdvInput.Q*AdvInput.K/10000;
      AdvInput.Max := TMetaCompanyInput(MetaBlock.CompanyInputs[0]).Max;
      if (Facility.Company <> nil) and (Facility.Company.Owner <> nil)
        then
          with Facility.Company.Owner do
            CurrFacPrestige := CurrFacPrestige + fPrestigeBoost;
    end;

  function TPublicAffairsHeadquarter.GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string;
    begin
      result := inherited GetStatusText( kind, ToTycoon );
      case kind of
        sttSecondary :
          result := result + ' ' +
            //IntToStr(round(fPrestigeBoost)) + ' prestige points from publicity.';
            SimHints.GetHintText( mtidCiviHQPrest.Values[ToTycoon.Language], [round(fPrestigeBoost)] );
      end;
    end;


  // RegisterBackup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass( THeadquarterBlock );
      BackupInterfaces.RegisterClass( TMainHeadquarter );
      BackupInterfaces.RegisterClass( TPublicAffairsHeadquarter );
    end;


end.


