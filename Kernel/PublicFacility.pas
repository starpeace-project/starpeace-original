unit PublicFacility;

interface

  uses
    Protocol, Kernel, Population, Classes, Collection, BackupInterfaces, Accounts,
    WorkCenterBlock, Variants;

  type
    TPFInfoDef =
      record
        Kind     : string;
        Strength : integer;
      end;

    function PFInfoDef( Kind : string; Strength : integer ) : TPFInfoDef;

  type
    TPFInfo =
      class
        private
          constructor Create( aPFInfoDef : TPFInfoDef );
        private
          fKind     : TMetaPublicFacilityInfo;
          fStrength : integer;
        public
          property Kind     : TMetaPublicFacilityInfo read fKind;
          property Strength : integer                 read fStrength;
      end;

  type
    TMetaPublicFacility =
      class( TMetaWorkCenter )
        public
          constructor Create( anId        : string;
                              aCapacities : array of TFluidValue;
                              PFInfo      : array of TPFInfoDef;
                              aBlockClass : CBlock );
          destructor  Destroy; override;
        private
          fKinds         : TCollection;
          fUpEffectOnMod : single;
          fDissabledStop : boolean;
          fMinPubSalary  : TPercent;
          fMaintCost     : TMoney;
          fMaintBoost    : single;
        published
          property Kinds : TCollection read fKinds;
          property UpEffectOnMod : single   read fUpEffectOnMod write fUpEffectOnMod;
          property DissabledStop : boolean  read fDissabledStop write fDissabledStop;
          property MinPubSalary  : TPercent read fMinPubSalary  write fMinPubSalary;
          property MaintCost     : TMoney   read fMaintCost     write fMaintCost;
      end;

    TPublicFacility =
      class( TFinanciatedWorkCenter )
        protected
          constructor Create( aMetaBlock : TMetaBlock; aFacility : TFacility ); override;
        public
          destructor  Destroy; override;
        private
          fModifiers  : TCollection;
          fEfficiency : single;
        public
          function  Evaluate : TEvaluationResult; override;
          procedure AutoConnect( loaded : boolean ); override;
        protected
          function GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string; override;
        public
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        protected
          procedure Deleted; override;
      end;

  procedure RegisterBackup;

implementation

  uses
    ClassStorage, Surfaces, PyramidalModifier, BasicAccounts, SimHints, SysUtils,
    MathUtils, Languages;


  function PFInfoDef( Kind : string; Strength : integer ) : TPFInfoDef;
    begin
      result.Kind     := Kind;
      result.Strength := Strength;
    end;


  // TPFInfo

  constructor TPFInfo.Create( aPFInfoDef : TPFInfoDef );
    begin
      inherited Create;
      fKind     := TMetaPublicFacilityInfo(TheClassStorage.ClassById[tidClassFamily_PublicFacilities, aPFInfoDef.Kind]);
      fStrength := aPFInfoDef.Strength;
    end;


  // TMetaPublicFacility

  constructor TMetaPublicFacility.Create( anId : string; aCapacities : array of TFluidValue; PFInfo : array of TPFInfoDef; aBlockClass : CBlock );
    var
      i    : integer;
      Info : TPFInfo;
    begin
      inherited Create( anId, aCapacities, accIdx_None, accIdx_None, accIdx_PF_Salaries, aBlockClass );
      fKinds := TCollection.Create( 0, rkBelonguer );
      for i := low(PFInfo) to high(PFInfo) do
        begin
          Info := TPFInfo.Create( PFInfo[i] );
          fKinds.Insert( Info );
        end;
      Prestige       := Prestige + 5;
      MaxUpgrade     := StrToInt(TheGlobalConfigHandler.GetConfigParm('MaxCivicsUpgrade', '20'));
      fUpEffectOnMod := 1;
      fDissabledStop := TheGlobalConfigHandler.GetConfigParm('StopCivics', 'yes') = 'no';
      fMinPubSalary  := StrToInt(TheGlobalConfigHandler.GetConfigParm('MinCivicsWage', '250'));
      MinColDist     := StrToInt(TheGlobalConfigHandler.GetConfigParm('MinPubFacSep', '4'));
      fMaintBoost    := StrToFloat(TheGlobalConfigHandler.GetConfigParm('PubMaintBoost', '1.7'));
    end;

  destructor TMetaPublicFacility.Destroy;
    begin
      fKinds.Free;
      inherited;
    end;


  // TPublicFacility

  constructor TPublicFacility.Create( aMetaBlock : TMetaBlock; aFacility : TFacility );
    var
      kind : TPeopleKind;
    begin
      inherited;
      for kind := low(kind) to high(kind) do
        ExtraAdmitance[kind] := 100;
    end;

  destructor TPublicFacility.Destroy;
    begin
      fModifiers.ExtractAll;
      fModifiers.Free;
      inherited;
    end;

  function TPublicFacility.Evaluate : TEvaluationResult;
    var
      idx   : integer;
      i     : integer;
      uprlv : single;
    begin
      // Cannot Stop Civics.
      if TMetaPublicFacility(MetaBlock).DissabledStop and not Facility.Deleted and (Facility.Company <> nil) and (Facility.Company.Owner <> nil) and not Facility.Company.Owner.IsRole
        then Facility.ClearTrouble(facStoppedByTycoon);

      // Cannot pay less than req%
      AdjustMinimunWages(TMetaPublicFacility(MetaBlock).MinPubSalary);

      result := inherited Evaluate;
      uprlv := realmax(1, UpgradeLevel);
      with TMetaPublicFacility(MetaBlock), TTownHall((TInhabitedTown(Facility.Town).TownHall.CurrBlock)) do
        begin
          // Compute actual efficiency
          if not self.Facility.CriticalTrouble // >> ÑOHHHH!!!!!!!
            then
              begin
                fEfficiency := WorkForceEfficiency;
                if self.Facility.CompanyDir <> nil
                  then fEfficiency := realmin( 1, self.Facility.CompanyDir.Support )*fEfficiency;
                self.BlockGenMoney(-self.dt*realmax(1, uprlv/3)*TMetaPublicFacility(self.MetaBlock).MaintCost*TMetaPublicFacility(self.MetaBlock).fMaintBoost, accIdx_PF_PublicOperation);
              end
            else fEfficiency := 0;

          // Report coverage, update modifiers
          idx := 0;
          for i := 0 to pred(Kinds.Count) do
            with TPFInfo(Kinds[i]) do
              begin
                ReportPublicFacility( Kind, round(uprlv*fEfficiency*Strength) );
                if Kind.SurfaceId <> ''
                  then
                    begin
                      TSurfaceModifier(fModifiers[idx]).Value := (1 + UpEffectOnMod*(uprlv - 1))*fEfficiency*Kind.ModFact*Strength;
                      inc( idx );
                    end;
              end;

          // Ask workforce
          if not self.Facility.CriticalTrouble
            then HireWorkForce( 1 );
        end;
    end;

  procedure TPublicFacility.AutoConnect( loaded : boolean );
    var
      i        : integer;
      Modifier : TSurfaceModifier;
    begin
      inherited;
      fModifiers := TCollection.Create( 0, rkBelonguer );
      with TMetaPublicFacility(MetaBlock), TTownHall((TInhabitedTown(Facility.Town).TownHall.CurrBlock)) do
        for i := 0 to pred(Kinds.Count) do
          with TPFInfo(Kinds[i]) do
            if Kind.SurfaceId <> ''
              then
                begin
                  Modifier :=
                    TPyramidalModifier.Create(
                      Kind.SurfaceId,
                      Point(self.xOrigin, self.yOrigin),
                      Kind.ModFact*Strength,
                      Kind.ModStrength );
                  fModifiers.Insert( Modifier );
                end;
    end;

  function TPublicFacility.GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string;
    var
      TownHall : TTownHall;
      info     : TPublicFacilityInfo;
      i        : integer;
      CoveredPeople : single;
    begin
      result := inherited GetStatusText( kind, ToTycoon );
      case kind of
        sttMain :
          result := result +
            //IntToStr(round(100*fEfficiency)) + '% operational.';
            SimHints.GetHintText( mtidPubFacMain.Values[ToTycoon.Language], [round(100*fEfficiency)] );
        sttSecondary :
          begin
            TownHall := TTownHall(TInhabitedTown(Facility.Town).TownHall.CurrBlock);
            result   := Format(mtidUpgradeLevel.Values[ToTycoon.Language], [UpgradeLevel]) + '   ';
            if TownHall.TotalPopulation > 0
              then
                with TMetaPublicFacility(MetaBlock) do
                  for i := 0 to pred(Kinds.Count) do
                    with TPFInfo(Kinds[i]) do
                      begin
                        info := TownHall.PublicFacilities[Kind];
                        if info <> nil
                          then CoveredPeople := realmin(info.Strength, TownHall.TotalPopulation)
                          else CoveredPeople := 0;
                        result := result + SimHints.GetHintText( mtidPubFacCov.Values[ToTycoon.Language], [Kind.Name_MLS.Values[ToTycoon.Language], round(100*CoveredPeople/TownHall.TotalPopulation)] ) + ' ';
                      end
              else result := SimHints.GetHintText( mtidEmptyCity.Values[ToTycoon.Language], [0] )
          end;
        sttHint :
          if fEfficiency < 0.8
            then
              if (Facility.CompanyDir = nil) or (Facility.CompanyDir.Support > 0.9)
                then result := SimHints.GetHintText( mtidPublicFacNeedsWorkers.Values[ToTycoon.Language], [0] )
                else result := SimHints.GetHintText( mtidPublicFacNeedsSupport.Values[ToTycoon.Language], [0] )
      end;
    end;

  procedure TPublicFacility.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      // Reader.ReadObject( 'Modifiers', fModifiers, nil );
    end;

  procedure TPublicFacility.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      // Writer.WriteLooseObject( 'Modifiers', fModifiers );
    end;

  procedure TPublicFacility.Deleted;
    var
      i : integer;
    begin
      for i := 0 to pred(fModifiers.Count) do
        TSurfaceModifier(fModifiers[i]).Delete;
      inherited;
    end;
    

  // RegisterBackup;

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass( TPublicFacility );
    end;

end.

