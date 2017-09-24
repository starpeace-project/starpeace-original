unit Broadcast;

interface

  uses
    Protocol, Kernel, WorkCenterBlock, Classes, Collection, BackupInterfaces, Surfaces,
    Accounts, StdFluids, Languages;
                                           
  const
    tidTownParameter_Broadcast = 'Broadcast';
    tidEnvironmental_Broadcast = 'Broadcast';

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
        private
          fViewers     : TFluidValue;
          fPresence    : TSurfaceValue;
          fCompetition : TSurfaceValue;
          fRating      : single;
          fLastViewers : integer;
      end;

    TMetaAntenna =
      class( TMetaWorkCenter )
        private
          fBroadcastId : string;
          fPower       : integer;
        public
          property BroadcastId : string  read fBroadcastId write fBroadcastId;
          property Power       : integer read fPower       write fPower;
      end;

    TPeopleIntegrators = array[TPeopleKind] of TSurfaceIntegrator;

    TAntenna =
      class( TFinanciatedWorkCenter )
        public
          destructor Destroy; override;
        private
          fBroadcaster : TBroadcaster;
          fSignal      : TSurfaceModifier;
          fSignalInt   : TSurfaceIntegrator;
          fPeopleInt   : TPeopleIntegrators;
          fQuality     : single;
          fPeople      : TFluidValue;     
        public
          function  Evaluate : TEvaluationResult;                                     override;
          procedure Autoconnect( loaded : boolean );                                  override;
          function  GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string; override;
          function  ConnectTo     ( Block : TBlock; Symetrical : boolean ) : string;  override;
          procedure DisconnectFrom( Block : TBlock; Symetrical : boolean );           override;
        private
          procedure LinkToBroadcaster( aBroadcaster : TBroadcaster );
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

  procedure RegisterBackup;

implementation

  uses
    ClassStorage, MetaInstances, SysUtils, PyramidalModifier, Population,
    MathUtils, SimHints;

  const
    AvgAdTime = (1/(60*60))*45; // in seconds

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
      fHoursOnAir  := 12;
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
      Quality : single;
      i       : integer;
      WEffic  : single;
    begin
      result := inherited Evaluate;
                                                                
      // Broadcasters sell to anyone
      if fTradeLevel <> tlvAnyone                                     
        then SetTradeLevel(tlvAnyone);

      if not Facility.CriticalTrouble and Facility.HasTechnology
        then
          begin
            if fCompetition > 0
              then fRating := realmin(1, fPresence/fCompetition)
              else fRating := 1;
            WEffic           := WorkForceEfficiency;
            fViewers         := fViewers*fRating;
            //fAdvertisement.Q := 100*sqrt(WEffic)*(fHoursOnAir/24)*(fCommercials/100)*fViewers*dt/AvgAdTime;
            fAdvertisement.Q := (fHoursOnAir/24)*(fCommercials/100)*fViewers*dt/(10*AvgAdTime);
            Quality          := sqrt(WEffic)*(1 - fCommercials/100)*(fBudget/100);
            fAdvertisement.K := round(100*Quality);
            with TMetaBroadcaster(MetaBlock) do
              BlockGenMoney( -BroadcastCost*(fBudget/100)*(fHoursOnAir/24)*dt, ProdAccount );
            fAntennas.Lock;
            try
              for i := 0 to pred(fAntennas.Count) do
                TAntenna(fAntennas[i]).fQuality := realmax(0, Quality);
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
                SimHints.GetHintText( mtidTVSec.Values[ToTycoon.Language], [fHoursOnAir, fCommercials, round(100*SalesRatio), fAntennas.Count] );
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


  // TAntenna

  destructor TAntenna.Destroy;
    var                             
      kind : TPeopleKind;
    begin
      if fBroadcaster <> nil
        then fBroadcaster.fAntennas.Delete( self );
      fSignal.Delete;
      for kind := low(kind) to high(kind) do
        fPeopleInt[kind].Delete;                                      
      fSignalInt.Delete;
      inherited;
    end;

  function TAntenna.Evaluate : TEvaluationResult;
    var
      kind : TPeopleKind;
    begin
      result := inherited Evaluate;
      if (fBroadcaster <> nil) and fBroadcaster.Facility.Deleted
        then fBroadcaster := nil;
      if fBroadcaster <> nil                     
        then
          begin
            fBroadcaster.fPresence    := fBroadcaster.fPresence    + fSignal.Value;
            fBroadcaster.fCompetition := fBroadcaster.fCompetition + fSignalInt.Value;
            if not fBroadcaster.Facility.CriticalTrouble
              then fSignal.Value := fQuality*TMetaAntenna(MetaBlock).Power
              else fSignal.Value := 0;
            fPeople := 0;
            for kind := low(kind) to high(kind) do
              fPeople := fPeople + sqrt(sqrt(fQuality))*fPeopleInt[kind].Value;
            fBroadcaster.fViewers := fBroadcaster.fViewers + fPeople/5;
          end;
    end;

  procedure TAntenna.Autoconnect( loaded : boolean );
    var
      kind : TPeopleKind;
    begin
      inherited;
      for kind := low(kind) to high(kind) do               
        fPeopleInt[kind] :=
          TSurfaceIntegrator.Create(
            PeopleKindPrefix[kind] + tidEnvironment_People,
            GetArea( TMetaAntenna(MetaBlock).Power, amdIncludeBlock ));
      fSignalInt :=
        TSurfaceIntegrator.Create(
          TMetaAntenna(MetaBlock).BroadcastId,
          GetArea( TMetaAntenna(MetaBlock).Power, amdIncludeBlock ));
      fSignal :=
        TPyramidalModifier.Create(
          TMetaAntenna(MetaBlock).BroadcastId,
          Point(xOrigin, yOrigin),
          0, 1 );
      fSignal.FixedArea := true;
      with TMetaAntenna(MetaBlock) do
        fSignal.Area := Rect( xOrigin - Power, yOrigin - Power, xOrigin + Power, yOrigin + Power );
    end;

  function TAntenna.GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string;
    begin
      result := inherited GetStatusText( kind, ToTycoon );
      case kind of
        sttSecondary :
          begin
            result :=
               result +                              
               //'Potential audience: ' + IntToStr(round(fPeople)) + '.';
               SimHints.GetHintText( mtidAntenaAudience.Values[ToTycoon.Language], [round(fPeople)] );
            if fBroadcaster <> nil
              then result :=
                result + ' ' +
                //' Channel rating: ' + IntToStr(round(100*fBroadcaster.fRating)) + '%.';
                SimHints.GetHintText( mtidAntenaRating.Values[ToTycoon.Language], [round(100*fBroadcaster.fRating)] );
          end;
        sttHint :
          if fBroadcaster = nil
            then
              result :=
                result +
                //'HINT: Use the "Connect" button in the settings panel to connect this antenna to a station.';
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


  // RegisterBackup

  procedure RegisterBackup;
    begin
      RegisterClass( TBroadcaster );
      RegisterClass( TAntenna );
    end;


end.






