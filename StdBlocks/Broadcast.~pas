unit Broadcast;

interface

  uses
    Protocol, Kernel, WorkCenterBlock, Classes, Collection, BackupInterfaces, Surfaces,
    Accounts, StdFluids, Languages, CacheAgent, Inventions;

  const
    tidTownParameter_Broadcast  = 'Broadcast';
    tidEnvironmental_Broadcast  = 'Broadcast';
    tidAdvertisement_Quality    = 100;

  const
    AntenaOpCost = 200;

  type
    TMetaBroadcaster =
      class( TMetaWorkCenter )
        public
          constructor Create( anId           : string;
                              aBroadcastId   : string;
                              aBroadcastCost : TMoney;
                              aCapacities    : array of TFluidValue;
                              aSupplyAccount : TAccountId;
                              aProdAccount   : TAccountId;
                              aSalaryAccount : TAccountId;
                              aBlockClass    : CBlock );
        private
          fBroadcastId   : string;
          fBroadcastCost : TMoney;
        public
          property BroadcastId   : string read fBroadcastId;
          property BroadcastCost : TMoney read fBroadcastCost;
      end;

    TBroadcaster =
      class( TFinanciatedWorkCenter )
        protected
          constructor Create( aMetaBlock : TMetaBlock; aFacility : TFacility ); override;
        public
          destructor Destroy; override;
        private
          fAdvertisement : TOutputData;
        private
          fAntennas    : TLockableCollection;
          fHoursOnAir  : byte;
          fBudget      : TPercent;
          fCommercials : TPercent;
        published
          property HoursOnAir  : byte     read fHoursOnAir  write fHoursOnAir;
          property Budget      : TPercent read fBudget      write fBudget;
          property Commercials : TPercent read fCommercials write fCommercials;
        public
          function  Evaluate : TEvaluationResult;                                     override;
          procedure Autoconnect( loaded : boolean );                                  override;
          function  GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string; override;
          function  ConnectTo     ( Block : TBlock; Symetrical : boolean ) : string; override;
          procedure DisconnectFrom( Block : TBlock; Symetrical : boolean );          override;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        public
          procedure StoreToCache( Cache : TObjectCache ); override;
        private
          fViewers       : TFluidValue;
          fPresence      : TSurfaceValue;
          fCompetition   : TSurfaceValue;
          fRating        : single;
          fLastViewers   : integer;
          fInvEfficiency : integer;
          fInvQuality    : integer;
          fQuality       : integer;
        private
          procedure IntegrateInventions(out invEfficiency, invQuality : integer);
        protected
          procedure RecalculateInventionsEffect; override;
      end;

    TMetaAntenna =
      class( TMetaWorkCenter )
        public
          constructor Create( anId           : string;
                              aCapacities    : array of TFluidValue;
                              aSupplyAccount : TAccountId;
                              aProdAccount   : TAccountId;
                              aSalaryAccount : TAccountId;
                              aBlockClass    : CBlock );
        private
          fBroadcastId  : string;
          fPower        : integer;
          fCostByViewer : TMoney;
        public
          property BroadcastId  : string  read fBroadcastId write fBroadcastId;
          property Power        : integer read fPower       write fPower;
          property CostByViewer : TMoney  read fCostByViewer;
      end;

    TPeopleIntegrators = array[TPeopleKind] of TSurfaceIntegrator;

    TAntenna =
      class( TFinanciatedWorkCenter )
        public
          destructor Destroy; override;
        private
          fBroadcaster : TBroadcaster;
          //fSignal      : TSurfaceModifier;
          //fSignalInt   : TSurfaceIntegrator;
          //fPeopleInt   : TPeopleIntegrators;
          fQuality   : single;
          fPeople    : TFluidValue;
          fTownParm  : TTownParameter;
          fIgnored   : boolean;
        public
          function  Evaluate : TEvaluationResult;                                     override;
          procedure Autoconnect( loaded : boolean );                                  override;
          procedure EndOfPeriod(PeriodType : TPeriodType; PeriodCount : integer);     override;
          function  GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string; override;
          function  ConnectTo     ( Block : TBlock; Symetrical : boolean ) : string;  override;
          procedure DisconnectFrom( Block : TBlock; Symetrical : boolean );           override;
        private
          procedure LinkToBroadcaster( aBroadcaster : TBroadcaster );
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

  const
    tidInventionClass_TVBlock = 'Television';
    tidInvAttr_TVEfficiency   = 'effic';
    tidInvAttr_TVQuality      = 'q';

  type
    TTVInvention =
      class(TWorkCenterInvention)
        public
          constructor Load(xmlObj : OleVariant); override;
        private
          fEfficiency : integer;
          fQuality    : integer;
        public
          property Efficiency : integer read fEfficiency;
          property Quality    : integer read fQuality;
        public
          function GetClientProps(Company : TObject; LangId : TLanguageId) : string; override;
      end;


  procedure RegisterBackup;
  procedure RegisterInventionClass;
  procedure RegisterTownParameters;

implementation

  uses
    ClassStorage, MetaInstances, SysUtils, PyramidalModifier, Population,
    MathUtils, SimHints, StdAccounts;

  const
    AvgAdTime = (1/(60*60))*45; // in seconds

  // TMetaAntena

  constructor TMetaAntenna.Create( anId           : string;
                                  aCapacities    : array of TFluidValue;
                                  aSupplyAccount : TAccountId;
                                  aProdAccount   : TAccountId;
                                  aSalaryAccount : TAccountId;
                                  aBlockClass    : CBlock );
    begin
      inherited;
      fCostByViewer := StrToFloat(TheGlobalConfigHandler.GetConfigParm('AntennaCostByViewer', '0.25'));
    end;


  // TMetaBroadcaster

  constructor TMetaBroadcaster.Create( anId           : string;
                                       aBroadcastId   : string;
                                       aBroadcastCost : TMoney;
                                       aCapacities    : array of TFluidValue;
                                       aSupplyAccount : TAccountId;
                                       aProdAccount   : TAccountId;
                                       aSalaryAccount : TAccountId;
                                       aBlockClass    : CBlock );
    var
      Sample : TBroadcaster;
    begin
      inherited Create( anId, aCapacities, aSupplyAccount, aProdAccount, aSalaryAccount, aBlockClass );
      fBroadcastId   := aBroadcastId;
      fBroadcastCost := aBroadcastCost;
      Sample         := nil;
      MetaOutputs.Insert(
        TMetaOutput.Create(
          tidGate_Advertisement,
          FluidData(qIlimited, 100), 
          TPullOutput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_Advertisement]),
          5,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fAdvertisement),
          Sample.Offset(Sample.fAdvertisement)));
    end;


  // TBroadcaster
  
  constructor TBroadcaster.Create( aMetaBlock : TMetaBlock; aFacility : TFacility );
    begin
      inherited;
      fHoursOnAir  := 24;
      fCommercials := 30;
      fBudget      := 100;
      fAntennas    := TLockableCollection.Create( 0, rkUse );
    end;

  destructor TBroadcaster.Destroy;
    var
      i : integer;
    begin
      fAntennas.Lock;
      try
        for i := 0 to pred(fAntennas.Count) do
          TAntenna(fAntennas[i]).fBroadcaster := nil;
      finally
        fAntennas.Unlock;
      end;
      fAntennas.Free;
      inherited;
    end;

  function TBroadcaster.Evaluate : TEvaluationResult;
    var
      Quality  : single;
      i, j     : integer;
      WEffic   : single;
      Antenna  : TAntenna;
      TownPrm  : TTownParameter;
      invEffic : single;
      InventQuality : single;
    begin
      result := inherited Evaluate;

      // Broadcasters sell to anyone
      if fTradeLevel <> tlvAnyone
        then SetTradeLevel(tlvAnyone);

      if not Facility.CriticalTrouble and Facility.HasTechnology
        then
          begin
            invEffic := realmax(0, realmin(2, 1 - fInvEfficiency/100));
            if fCompetition > 0
              then fRating := realmin(1, fPresence/fCompetition)
              else
                if fPresence > 0
                  then fRating := 1
                  else fRating := 0;
            WEffic := WorkForceEfficiency;
            //fViewers         := fViewers*fRating;

            // Balance the formula using both numbers...

            fAdvertisement.Q := (fHoursOnAir/24)*(fCommercials/100)*fViewers*dt/(10*AvgAdTime);
            Quality          := sqrt(WEffic)*(-(fCommercials-50)/50)*(1 - (fCommercials/100))*(fBudget/100);
            InventQuality    := (1 - max(fCommercials - 40, 0)/60)*fInvQuality;
            Quality          := RealMax(0.01, Quality + InventQuality/100);
            fQuality         := min(100, round((100*Quality)/2));
            fAdvertisement.K := tidAdvertisement_Quality; //
            with TMetaBroadcaster(MetaBlock) do
              BlockGenMoney( -invEffic*BroadcastCost*(fBudget/100)*(fHoursOnAir/24)*dt,  accIdx_TV_Maintenance);
            fAntennas.Lock;
            try
              // Assign Quality and Sign as not abandoned all antennas
              for i := pred(fAntennas.Count) downto 0 do
                begin
                  Antenna := TAntenna(fAntennas[i]);
                  if Antenna.Facility.Deleted
                    then fAntennas.Delete(Antenna)
                    else
                      begin
                        Antenna.fQuality := realmax(0, fQuality);
                        Antenna.fIgnored := Antenna.Facility.CriticalTrouble;
                      end;
                end;
              // Ignore second Antennas
              for i := 0 to pred(fAntennas.Count) do
                begin
                  Antenna := TAntenna(fAntennas[i]);
                  if not Antenna.fIgnored
                    then
                      begin
                        TownPrm := Antenna.fTownParm;
                        for j := i + 1 to pred(fAntennas.Count) do
                          with TAntenna(fAntennas[j]) do
                            if fTownParm = TownPrm
                              then fIgnored := true;
                      end;
                end;
            finally
              fAntennas.Unlock;
            end;
            HireWorkForce( realmin(0.7, fHoursOnAir/24) );
          end
        else
          begin
            fAdvertisement.Q := 0;
            fAdvertisement.K := 0;
            fLastViewers     := 0;
          end;
      fLastViewers := round( fViewers );
      fViewers     := 0;
      fPresence    := 0;
      fCompetition := 0;
    end;

  procedure TBroadcaster.Autoconnect( loaded : boolean );
    begin
      inherited;
      fAntennas.Pack;
      //RDOSetTradeLevel( integer(tlvAnyone) ); >>
    end;

  function TBroadcaster.GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string;

    function SalesRatio : single;
      begin
        if fAdvertisement.Q > 0
          then result := 1 - fAdvertisement.Extra.Q/fAdvertisement.Q
          else result := 0
      end;

    var
      k : TPeopleKind;
    begin
      result := inherited GetStatusText( kind, ToTycoon );
      if not Facility.CriticalTrouble and (Facility.Trouble and facNeedsWorkForce = 0)
        then
          case kind of
            sttMain :
              result := result +
                SimHints.GetHintText( mtidTVMainOne.Values[ToTycoon.Language], [fLastViewers] ) + LineBreak +
                SimHints.GetHintText( mtidTVMainTwo.Values[ToTycoon.Language], [round(100*fRating)] );
            sttSecondary :
              result := result +
                SimHints.GetHintText( mtidTVSec.Values[ToTycoon.Language], [fHoursOnAir, fCommercials, round(100*SalesRatio), fAntennas.Count, fQuality, fInvEfficiency ] );
            sttHint :
              if fAntennas.Count = 0
                then result := result + SimHints.GetHintText( mtidTVWarning.Values[ToTycoon.Language], [0] );
          end
        else
          if Facility.Trouble and facNeedsWorkForce <> 0
            then
              case kind of
                sttMain :
                  result := result +
                    SimHints.GetHintText( mtidHiringWorkForce.Values[ToTycoon.Language], [round(100*WorkForceRatioAvg)] );
                sttSecondary :
                  with TMetaWorkCenter(MetaBlock) do
                    for k := low(k) to high(k) do
                      if Capacity[k] > 0
                        then
                          if WorkersMax[k].Q > 0
                            then
                              begin
                                result := result +
                                  SimHints.GetHintText(
                                    mtidHiringWorkForceSec.Values[ToTycoon.Language],
                                    [
                                    mtidWorkforceKindName[k].Values[ToTycoon.Language],
                                    round(Workers[k].Q),
                                    round(WorkersMax[k].Q)
                                    ] );
                              end;
              end;
    end;

  function TBroadcaster.ConnectTo( Block : TBlock; Symetrical : boolean ) : string;
    var
      report : string;
    begin
      result := inherited ConnectTo( Block, Symetrical );
      if ObjectIs( TAntenna.ClassName, Block )
        then
          begin
            if Block.Facility.Company.Owner = Facility.Company.Owner
              then
                if TAntenna(Block).fBroadcaster <> self
                  then
                    begin
                      if TAntenna(Block).fBroadcaster <> nil
                        then
                          report :=
                            'Antenna previously connected to ' + Block.Facility.Name + '.' + LineBreak +
                            'Now connected to ' + Facility.Name + '.'
                        else
                          report :=
                            'Antenna successfully connected to ' + Facility.Name + '.';
                      TAntenna(Block).LinkToBroadcaster( self );
                    end
                  else report := 'Antenna was already connected.'
              else report := 'You are not allowed to do that!';
            if result <> ''
              then result := report + LineBreak + result
              else result := report;
          end;
    end;

  procedure TBroadcaster.DisconnectFrom( Block : TBlock; Symetrical : boolean );
    begin
      inherited;
    end;

  procedure TBroadcaster.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      Reader.ReadObject( 'Antennas', fAntennas, nil );
      fHoursOnAir  := Reader.ReadByte( 'HoursOnAir', 24 );
      fBudget      := Reader.ReadByte( 'Budget', 100 );
      fCommercials := Reader.ReadByte( 'Commercials', 30 );
    end;

  procedure TBroadcaster.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteObject( 'Antennas', fAntennas );
      Writer.WriteByte( 'HoursOnAir', fHoursOnAir );
      Writer.WriteByte( 'Budget', fBudget );
      Writer.WriteByte( 'Commercials', fCommercials );
    end;

  procedure TBroadcaster.StoreToCache(Cache : TObjectCache);
    var
      i       : integer;
      iStr    : string;
      Antenna : TAntenna;
    begin
      inherited;
      for i := 0 to pred(fAntennas.Count) do
        begin
          iStr    := IntToStr(i);
          Antenna := TAntenna(fAntennas[i]);
          Cache.WriteString('antName' + iStr, Antenna.Facility.Name);
          Cache.WriteInteger('antX' + iStr, Antenna.xPos);
          Cache.WriteInteger('antY' + iStr, Antenna.yPos);
          Cache.WriteString('antTown' + iStr, Antenna.Facility.Town.Name);
          if not Antenna.Facility.CriticalTrouble
            then Cache.WriteInteger('antViewers' + iStr, round(Antenna.fPeople))
            else Cache.WriteInteger('antViewers' + iStr, 0);
          if not Antenna.fIgnored
            then Cache.WriteString('antActive' + iStr, 'YES');
        end;
      Cache.WriteInteger('antCount', fAntennas.Count);
    end;

  procedure TBroadcaster.IntegrateInventions(out invEfficiency, invQuality : integer);
    var
      Invention : TTVInvention;
      i         : integer;
      MaxQuality: integer;
      MinQuality: integer;
    begin
      invEfficiency := 0;
      invQuality    := 0;
      MaxQuality    := 0;
      MinQuality    := 0;
      for i := 0 to pred(MetaBlock.Inventions.Count) do
        begin
          Invention := TTVInvention(MetaBlock.Inventions[i]);
          if Invention.Quality > 0 then
            MaxQuality := MaxQuality + Invention.Quality
          else
            MinQuality := MinQuality + Invention.Quality;
          if Facility.Company.HasInvention[Invention.NumId]
            then
              begin
                invEfficiency := invEfficiency + Invention.Efficiency;
                invQuality    := invQuality + Invention.Quality;
              end;
        end;
      invQuality := Round(((InvQuality - MinQuality)*100)/(MaxQuality - MinQuality));
    end;

  procedure TBroadcaster.RecalculateInventionsEffect;
    begin
      IntegrateInventions(fInvEfficiency, fInvQuality);
    end;

  // TAntenna

  destructor TAntenna.Destroy;
    //var
      //kind : TPeopleKind;
    begin
      if fBroadcaster <> nil
        then fBroadcaster.fAntennas.Delete( self );
      //fSignal.Delete;
      //for kind := low(kind) to high(kind) do
        //fPeopleInt[kind].Delete;
      //fSignalInt.Delete;
      inherited;
    end;

  function TAntenna.Evaluate : TEvaluationResult;
    var
      locFact  : single;
      Strength : double;
      TownHall : TTownHall;
      Viewers  : TFluidValue;
      theDt    : single;
    begin
      result := inherited Evaluate;
      if (fBroadcaster <> nil) and fBroadcaster.Facility.Deleted
        then
          begin
            fBroadcaster := nil;
            fIgnored     := false;
            fPeople      := 0;
          end;
      if (fBroadcaster <> nil) and not fIgnored and (fTownParm <> nil)
        then
          begin
            theDt := dt;
            if fBroadcaster.Facility.Town = Facility.Town
              then locFact := 1
              else locFact := 0.5;
            if fTownParm.Value > 0
              then Strength := realmin(1, fQuality/fTownParm.Value)
              else Strength := 1;

            fTownParm.CurrValue := fTownParm.CurrValue + fQuality;

            Strength := locFact*Strength;
            fBroadcaster.fPresence    := fBroadcaster.fPresence    + fQuality;
            fBroadcaster.fCompetition := fBroadcaster.fCompetition + realmax(0, fTownParm.Value - fQuality);

            // Get the town hall
            TownHall := TTownHall(TInhabitedTown(Facility.Town).TownHall.CurrBlock);

            // Viewers
            Viewers := theDt*Strength*TownHall.TotalPopulation*4/24; // (1/6) Considering every people watches TV 4 hours a day
            fBroadcaster.fViewers := fBroadcaster.fViewers + Viewers;
            fPeople := round(Viewers);

            with TMetaAntenna(MetaBlock) do
              //fBroadcaster.BlockGenMoney(-realmax(AntenaOpCost, CostByViewer*Viewers)*theDt, accIdx_TV_Maintenance);
              //We got rid of the cost per viewer for the antennas
              fBroadcaster.BlockGenMoney(-AntenaOpCost*theDt, accIdx_TV_Maintenance);
            HireWorkForce(1);
          end;
    end;

  procedure TAntenna.Autoconnect( loaded : boolean );
    {var
      kind : TPeopleKind;}
    begin
      inherited;
      {for kind := low(kind) to high(kind) do
        fPeopleInt[kind] :=
          TSurfaceIntegrator.Create(
            PeopleKindPrefix[kind] + tidEnvironment_People,
            GetArea( TMetaAntenna(MetaBlock).Power, amdIncludeBlock ));}
      {fSignalInt :=
        TSurfaceIntegrator.Create(
          TMetaAntenna(MetaBlock).BroadcastId,
          GetArea( TMetaAntenna(MetaBlock).Power, amdIncludeBlock ));}
      {fSignal :=
        TPyramidalModifier.Create(
          TMetaAntenna(MetaBlock).BroadcastId,
          Point(xOrigin, yOrigin),
          0, 1 );
      fSignal.FixedArea := true;}
      {with TMetaAntenna(MetaBlock) do
        fSignal.Area := Rect( xOrigin - Power, yOrigin - Power, xOrigin + Power, yOrigin + Power );}
      fTownParm := Facility.Town.Parameters[tidTownParameter_Broadcast];
      fIgnored  := false;
    end;

  procedure TAntenna.EndOfPeriod(PeriodType : TPeriodType; PeriodCount : integer);
    begin
      inherited;
      case PeriodType of
        perDay :
          if fIgnored and not Facility.CriticalTrouble
            then Facility.Town.ModelFactory.RequestDeletion(Facility)
            else
              if not fIgnored and (fBroadcaster = nil) and (Facility.Age > 2*24*365)
                then Facility.Town.ModelFactory.RequestDeletion(Facility);
      end;
    end;

  function TAntenna.GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string;
    begin
      result := inherited GetStatusText( kind, ToTycoon );
      case kind of
        sttSecondary :
          begin
            result :=
               result +
               SimHints.GetHintText( mtidAntenaAudience.Values[ToTycoon.Language], [round(fPeople)] );
            if (fBroadcaster <> nil) and not fIgnored
              then result := fBroadcaster.Facility.Name + ',  ' +
                result + ' ' +
                SimHints.GetHintText( mtidAntenaRating.Values[ToTycoon.Language], [round(100*fBroadcaster.fRating)] );
          end;
        sttHint :
          if fBroadcaster = nil
            then
              result :=
                result +
                SimHints.GetHintText( mtidAntenaHint.Values[ToTycoon.Language], [0] );
      end;
    end;

  function TAntenna.ConnectTo( Block : TBlock; Symetrical : boolean ) : string;
    begin
      result := inherited ConnectTo( Block, Symetrical );
    end;

  procedure TAntenna.DisconnectFrom( Block : TBlock; Symetrical : boolean );
    begin
      inherited;
    end;

  procedure TAntenna.LinkToBroadcaster( aBroadcaster : TBroadcaster );
    begin
      if fBroadcaster <> nil
        then fBroadcaster.fAntennas.Delete( self );
      fBroadcaster := aBroadcaster;
      fBroadcaster.fAntennas.Insert( self );
    end;

  procedure TAntenna.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      Reader.ReadObject( 'Broadcaster', fBroadcaster, nil );
    end;

  procedure TAntenna.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteObjectRef( 'Broadcaster', fBroadcaster );
    end;


  // TTVInvention

  constructor TTVInvention.Load(xmlObj : OleVariant);
    var
      Aux : OleVariant;
    begin
      inherited Load(xmlObj);
      Aux         := xmlObj.children.item(tidInvElement_Props, Unassigned);
      fEfficiency := GetProperty(Aux, tidInvAttr_TVEfficiency);
      fQuality    := GetProperty(Aux, tidInvAttr_TVQuality);
    end;

  function TTVInvention.GetClientProps(Company : TObject; LangId : TLanguageId) : string;
    begin
      result := inherited GetClientProps(Company, LangId );
      if fEfficiency <> 0
        then result := result + SimHints.GetHintText(mtidInvEff.Values[LangId], [FormatDelta(fEfficiency)]) + LineBreak;
      if fQuality <> 0
        then result := result + SimHints.GetHintText(mtidInvQ.Values[LangId], [FormatDelta(fQuality)]) + LineBreak;
    end;


  procedure RegisterBackup;
    begin
      RegisterClass( TBroadcaster );
      RegisterClass( TAntenna );
    end;

  procedure RegisterInventionClass;
    begin
      TheClassStorage.RegisterClass(
        tidClassFamily_InvClasses,
        tidInventionClass_TVBlock,
        TInventionClass.Create(TTVInvention));
    end;

  procedure RegisterTownParameters;
    begin
      TMetaTownParameter.Create(tidTownParameter_Broadcast, 'BroadCast', true).Register;
    end;

end.






