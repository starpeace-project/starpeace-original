unit BasicPolitics;

interface

  uses
    Kernel, Politics, Population, BackupInterfaces, CacheAgent, Languages, Variants;

  const
    tidRating_CampaignAccuracy = 'CampaignAccuracy';
    tidRating_PopGrowth        = 'PopGrowth';
    tidRating_CityProfit       = 'CityProfit';
    tidRating_Unemployment     = 'Unemployment';
    tidRating_Services         = 'Services';
    tidRating_Wealth           = 'Wealth';
    tidRating_Taxes            = 'Taxes';

  const
    tidProjectKind_Percent = 'Percent';

  const
    tidProject_Unemployment = 'Unemployment';
    tidProject_Services     = 'Services';
    tidProject_Wealth       = 'Wealth';
    tidProject_SalaryTaxes  = 'SalaryTaxes';
    tidProject_IncomeTaxes  = 'IncomeTaxes';

  const
    plidPopGrowth    = 10;
    plidPublicParm   = 11;
    plidUnemployment = 12;
    plidGQOS         = 14;
    plidWealth       = 15;

  type
    TMetaCampaignAccuracyRating =
      class( TMetaRating )
      end;

    TCampaignAccuracyRating =
      class( TRating )
        protected
          procedure ComputeIFELRating; override;
      end;

    TMetaPopGrowthRating =
      class( TMetaRating )
      end;

    TPopGrowthRating =
      class( TRating )
        protected
          procedure ComputeIFELRating; override;
      end;

    TMetaPublicParmRating =
      class( TMetaRating )
        public
          constructor Create( anId, aName : string; aMetaPublicFacId : string; aWeight : integer; aRatingClass : CRating );
        private
          fMetaPublicFac : TMetaPublicFacilityInfo;
      end;

    TPublicParmRating =
      class( TRating )
        protected
          procedure ComputeIFELRating; override;
      end;

    TMetaUnemploymentRating =
      class( TMetaRating )
      end;

    TUnemploymentRating =
      class( TRating )
        protected
          procedure ComputeIFELRating; override;
      end;

    TMetaServicesRating =
      class( TMetaRating )
      end;

    TServicesRating =
      class( TRating )
        protected
          procedure ComputeIFELRating; override;
      end;

    TMetaWealthRating =
      class( TMetaRating )
      end;

    TWealthRating =
      class( TRating )
        protected
          procedure ComputeIFELRating; override;
      end;

    TMetaTaxesRating =
      class( TMetaRating )
      end;

    TTaxesRating =
      class( TRating )
        protected
          procedure ComputeIFELRating; override;
      end;

    TComparisonMode = (cmdLessThan, cmdGreaterThan);

    TMetaPercentProject =
      class( TMetaProject )
        public
          constructor Create( anId, aName : string; aWeight : integer; aComparisonMode : TComparisonMode; aProjectClass : CProject );
        private
          fComparisonMode : TComparisonMode;
        public
          property ComparisonMode : TComparisonMode read fComparisonMode;
      end;

    TPercentProject =
      class( TProject )
        private
          fValue   : TPercent;
          fLessTan : boolean;
        public
          property Value   : TPercent read fValue;
          property LessTan : boolean  read fLessTan;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        public
          procedure ParseData( Data : string ); override;
          procedure SetDefaultValue; override;
        public
          procedure StoreToCache( Cache : TObjectCache ); override;
        protected
          function ComputeCloseness ( Sample : TPercent ) : TPercent;
          function ComputeAcceptance( Sample : TPercent ) : TPercent;
      end;

    TMetaPublicParmProject =
      class( TMetaPercentProject )
        public
          constructor Create( anId, aName : string; aMetaPublicFacId : string; aWeight : integer; aComparisonMode : TComparisonMode; aProjectClass : CProject );
        private
          fMetaPublicFac : TMetaPublicFacilityInfo;
      end;

    TPublicParmProject =
      class( TPercentProject )
        protected
          function GetRating   : TPercent; override;
          function GetAccuracy : TPercent; override;
      end;

    TMetaGrowthProject =
      class( TMetaPercentProject )
      end;

    TGrowthProject =
      class( TPercentProject )
        protected
          function GetRating   : TPercent; override;
          function GetAccuracy : TPercent; override;
      end;

    TMetaServicesProject =
      class( TMetaPercentProject )
      end;

    TServicesProject =
      class( TPercentProject )
        protected
          function GetRating   : TPercent; override;
          function GetAccuracy : TPercent; override;
      end;

    TMetaWealthProject =
      class( TMetaPercentProject )
      end;

    TWealthProject =
      class( TPercentProject )
        protected
          function GetRating   : TPercent; override;
          function GetAccuracy : TPercent; override;
      end;

    TMetaUnemploymentProject =
      class( TMetaPercentProject )
      end;

    TUnemploymentProject =
      class( TPercentProject )
        protected
          function GetRating   : TPercent; override;
          function GetAccuracy : TPercent; override;
      end;


  // Registration

  procedure RegisterPolitics;
  procedure RegisterBackup;


implementation

  uses
    ClassStorage, MathUtils, SysUtils;
    

  // TCampaignAccuracyRating

  procedure TCampaignAccuracyRating.ComputeIFELRating;
    var
      Campaign : TCampaign;
    begin
      Campaign := System.PoliticalEntity.getWinningCampaign;
      if Campaign <> nil
        then fIFELRating := Campaign.Accuracy
        else fIFELRating := 100;
    end;


  // TPopGrowthRating

  procedure TPopGrowthRating.ComputeIFELRating;
    var
      PopGrowth : integer;
    begin
      PopGrowth  := round(System.PoliticalEntity.getParm( plidPopGrowth, self ));
      fIFELRating := min(100, 3*PopGrowth );
    end;


  // TMetaPublicParmRating

  constructor TMetaPublicParmRating.Create( anId, aName : string; aMetaPublicFacId : string; aWeight : integer; aRatingClass : CRating );
    begin
      inherited Create( anId, aName, aWeight, aRatingClass );
      fMetaPublicFac := TMetaPublicFacilityInfo(TheClassStorage.ClassById[tidClassFamily_PublicFacilities, aMetaPublicFacId]);
    end;


  // TPublicParmRating

  procedure TPublicParmRating.ComputeIFELRating;
    begin
      fIFELRating := round(System.PoliticalEntity.getParm( plidPublicParm, TMetaPublicParmRating(MetaRating).fMetaPublicFac ));
    end;


  // TUnemploymentRating

  procedure TUnemploymentRating.ComputeIFELRating;
    begin
      fIFELRating := round(System.PoliticalEntity.getParm( plidUnemployment, self ));
    end;


  // TServicesRating

  procedure TServicesRating.ComputeIFELRating;
    begin
      fIFELRating := round(System.PoliticalEntity.getParm( plidGQOS, self ));
    end;


  // TWealthRating

  procedure TWealthRating.ComputeIFELRating;
    begin
      fIFELRating := round(System.PoliticalEntity.getParm( plidWealth, self ));
    end;


  // TTaxesRating

  procedure TTaxesRating.ComputeIFELRating;
    begin
      fIFELRating := 80 + random(20);
    end;


  // TMetaPercentProject

  constructor TMetaPercentProject.Create( anId, aName : string; aWeight : integer; aComparisonMode : TComparisonMode; aProjectClass : CProject );
    begin
      inherited Create( anId, aName, tidProjectKind_Percent, aWeight, aProjectClass );
      fComparisonMode := aComparisonMode;
    end;


  // TPercentProject

  procedure TPercentProject.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      fValue := Reader.ReadByte( 'Value', 0 );
    end;

  procedure TPercentProject.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteByte( 'Value', fValue );
    end;

  procedure TPercentProject.ParseData( Data : string );
    begin
      fValue := StrToInt(Data);
    end;

  procedure TPercentProject.SetDefaultValue;
    begin
      case TMetaPercentProject(MetaProject).ComparisonMode of
        cmdLessThan :
          fValue := 10;
        cmdGreaterThan :
          fValue := 80;
      end;
    end;

  procedure TPercentProject.StoreToCache( Cache : TObjectCache );
    begin
      inherited;
      Cache.WriteInteger( 'Value', fValue );
      Cache.WriteInteger( 'Mode', integer(TMetaPercentProject(MetaProject).ComparisonMode) );
    end;

  function TPercentProject.ComputeCloseness( Sample : TPercent ) : TPercent;
    var
      actValue  : TPercent;
      actSample : TPercent;
    begin
      if TMetaPublicParmProject(MetaProject).ComparisonMode = cmdGreaterThan
        then
          begin
            actValue  := Value;
            actSample := Sample;
          end
        else
          begin
            actValue  := 100 - Value;
            actSample := 100 - Sample;
          end;
      if actSample >= actValue
        then result := 100
        else result := 100 - (actValue - actSample)
    end;

  function TPercentProject.ComputeAcceptance( Sample : TPercent ) : TPercent;
    var
      actValue  : TPercent;
      actSample : TPercent;
    begin
      if TMetaPublicParmProject(MetaProject).ComparisonMode = cmdGreaterThan
        then
          begin
            actValue  := Value;
            actSample := Sample;
          end
        else
          begin
            actValue  := 100 - Value;
            actSample := 100 - Sample;
          end;
      if actValue div 10 >= actSample div 10
        then result := actValue
        else result := 0;
    end;


  // TMetaPublicParmProject

  constructor TMetaPublicParmProject.Create( anId, aName : string; aMetaPublicFacId : string; aWeight : integer; aComparisonMode : TComparisonMode; aProjectClass : CProject );
    begin
      inherited Create( anId, aName, aWeight, aComparisonMode, aProjectClass );
      fMetaPublicFac := TMetaPublicFacilityInfo(TheClassStorage.ClassById[tidClassFamily_PublicFacilities, aMetaPublicFacId]);
    end;


  // TPublicParmProject

  function TPublicParmProject.GetRating : TPercent;
    var
      Coverage : single;
    begin
      Coverage := Campaign.System.PoliticalEntity.getParm( plidPublicParm, TMetaPublicParmProject(MetaProject).fMetaPublicFac );
      result   := ComputeAcceptance( round(Coverage) );
    end;

  function TPublicParmProject.GetAccuracy : TPercent;
    var
      Coverage : single;
    begin
      Coverage := Campaign.System.PoliticalEntity.getParm( plidPublicParm, TMetaPublicParmProject(MetaProject).fMetaPublicFac );
      result   := ComputeCloseness( round(Coverage) );
    end;


  // TGrowthProject

  function TGrowthProject.GetRating : TPercent;
    var
      PopGrowth : integer;
    begin
      PopGrowth := round(Campaign.System.PoliticalEntity.getParm( plidPopGrowth, self ));
      result := ComputeAcceptance( min(100, PopGrowth) );
    end;

  function TGrowthProject.GetAccuracy : TPercent;
    var
      PopGrowth : integer;
    begin
      PopGrowth := round(Campaign.System.PoliticalEntity.getParm( plidPopGrowth, self ));
      result := ComputeCloseness( min(100, PopGrowth) );
    end;


  // TServicesProject

  function TServicesProject.GetRating : TPercent;
    begin
      result := ComputeAcceptance( min(100, round(100*Campaign.System.PoliticalEntity.getParm( plidGQOS, self ))) );
    end;

  function TServicesProject.GetAccuracy : TPercent;
    begin
      result := ComputeCloseness( min(100, round(100*Campaign.System.PoliticalEntity.getParm( plidGQOS, self ))) );
    end;


  // TWealthProject

  function TWealthProject.GetRating : TPercent;
    begin
      result := ComputeAcceptance( min(100, round(100*Campaign.System.PoliticalEntity.getParm( plidWealth, self ))) );
    end;

  function TWealthProject.GetAccuracy : TPercent;
    begin
      result := ComputeCloseness( min(100, round(100*Campaign.System.PoliticalEntity.getParm( plidWealth, self ))) );
    end;


  // TUnemploymentProject

  function TUnemploymentProject.GetRating : TPercent;
    begin
      result := ComputeAcceptance( round(Campaign.System.PoliticalEntity.getParm( plidUnemployment, self )) );
    end;

  function TUnemploymentProject.GetAccuracy : TPercent;
    begin
      result := ComputeCloseness( round(Campaign.System.PoliticalEntity.getParm( plidUnemployment, self )) );
    end;


  // Registration

  procedure RegisterPolitics;

    procedure RegisterPublicParmRatings;
      var
        count, i : integer;
        MPFI     : TMetaPublicFacilityInfo; 
      begin
        count := TheClassStorage.ClassCount[tidClassFamily_PublicFacilities];
        for i := 0 to pred(count) do
          begin
            MPFI := TMetaPublicFacilityInfo(TheClassStorage.ClassByIdx[tidClassFamily_PublicFacilities, i]);
            TMetaPublicParmRating.Create(
              MPFI.Id,
              MPFI.Name,
              MPFI.Id,
              MPFI.Importance,
              TPublicParmRating ).Register( tidClassFamily_Ratings );
          end;
      end;

    procedure RegisterPublicParmProjects;
      var
        count, i : integer;
        MPFI     : TMetaPublicFacilityInfo; 
      begin
        count := TheClassStorage.ClassCount[tidClassFamily_PublicFacilities];
        for i := 0 to pred(count) do
          begin
            MPFI := TMetaPublicFacilityInfo(TheClassStorage.ClassByIdx[tidClassFamily_PublicFacilities, i]);
            TMetaPublicParmProject.Create(
              MPFI.Id,
              MPFI.Name,
              MPFI.Id,
              MPFI.Importance,
              cmdGreaterThan,
              TPublicParmProject ).Register( tidClassFamily_Projects );
          end;
      end;

    begin
      // Ratings
      TMetaCampaignAccuracyRating.Create(
        tidRating_CampaignAccuracy,
        'Campaign Acomplishment',
        200,
        TCampaignAccuracyRating ).Register( tidClassFamily_Ratings );
      RegisterPublicParmRatings;
      TMetaPopGrowthRating.Create(
        tidRating_PopGrowth,
        'City Growth',
        70,
        TPopGrowthRating ).Register( tidClassFamily_Ratings );
      TMetaUnemploymentRating.Create(
        tidRating_Unemployment,
        'Employment',
        100,
        TUnemploymentRating ).Register( tidClassFamily_Ratings );
      TMetaServicesRating.Create(
        tidRating_Services,
        'Services and Amusement',
        70,
        TServicesRating ).Register( tidClassFamily_Ratings );
      TMetaWealthRating.Create(
        tidRating_Wealth,
        'Economic Wealth',
        80,
        TWealthRating ).Register( tidClassFamily_Ratings );
      TMetaTaxesRating.Create(
        tidRating_Taxes,
        'Taxes',
        60,
        TTaxesRating ).Register( tidClassFamily_Ratings );

      // Projects
      RegisterPublicParmProjects;
      TMetaServicesProject.Create(
        tidProject_Services,
        'Services and Amusement',
        100,
        cmdGreaterThan,
        TServicesProject ).Register( tidClassFamily_Projects );
      TMetaWealthProject.Create(
        tidProject_Wealth,
        'Economic Wealth',
        100,
        cmdGreaterThan,
        TWealthProject ).Register( tidClassFamily_Projects );
      TMetaUnemploymentProject.Create(
        tidProject_Unemployment,
        'Unemployment',
        200,
        cmdLessThan,
        TUnemploymentProject ).Register( tidClassFamily_Projects );
    end;

  procedure RegisterBackup;
    begin
      RegisterClass( TCampaignAccuracyRating );
      RegisterClass( TPopGrowthRating );
      RegisterClass( TPublicParmRating );
      RegisterClass( TUnemploymentRating );
      RegisterClass( TServicesRating );
      RegisterClass( TWealthRating );
      RegisterClass( TTaxesRating );
      RegisterClass( TPublicParmProject );
      RegisterClass( TGrowthProject );
      RegisterClass( TServicesProject );
      RegisterClass( TWealthProject );
      RegisterClass( TUnemploymentProject );
    end;


end.



