unit Environmental;

interface

  uses
    Classes, Collection, Kernel, Surfaces, BackupInterfaces, Accounts, BasicAccounts;

  type
    TMetaEnvironmentalBlock =
      class( TMetaBlock )
        public
          constructor Create( anId : string; aBlockClass : CBlock );
          destructor  Destroy; override;
        private
          fModifiers     : TCollection;
          fMaintCost     : TMoney;
          fDissabledStop : boolean;
          fMaintBoost    : single;
        public
          procedure RegisterModifier( Surface : TSurfaceId; Value, Strength : TSurfaceValue );
        public
          property MaintCost : TMoney read fMaintCost write fMaintCost;
          property DissabledStop : boolean read fDissabledStop write fDissabledStop;
      end;

    TEnvironmentalBlock =
      class( TBlock )
        public
          constructor Create(aMetaBlock : TMetaBlock; aFacility : TFacility); override;
          destructor  Destroy; override;
        private
          fModifiers : TCollection;
        public
          function  Evaluate : TEvaluationResult; override;
          procedure AutoConnect( loaded : boolean ); override;
        protected
          procedure StoreToBackup(Writer : IBackupWriter);  override;
          procedure LoadFromBackup(Reader : IBackupReader); override;
        protected
          procedure Deleted; override;
      end;

  procedure RegisterBackup;

implementation

  uses
    SysUtils, PyramidalModifier, ClassStorage;

  type
    TModifierInfo =
      class
        private
          fSurface  : TSurfaceId;
          fValue    : TSurfaceValue;
          fStrength : TSurfaceValue;
      end;


  // TMetaEnvironmentalBlock

  constructor TMetaEnvironmentalBlock.Create( anId : string; aBlockClass : CBlock );
    begin
      inherited Create( anId, accIdx_None, accIdx_None, aBlockClass );
      fModifiers  := TCollection.Create( 0, rkBelonguer );
      fMaintBoost := StrToFloat(TheGlobalConfigHandler.GetConfigParm('PubMaintBoost', '2'));
    end;

  destructor TMetaEnvironmentalBlock.Destroy;
    begin
      fModifiers.Free;
      inherited;
    end;

  procedure TMetaEnvironmentalBlock.RegisterModifier( Surface : TSurfaceId; Value, Strength : TSurfaceValue );
    var
      ModifierInfo : TModifierInfo;
    begin
      ModifierInfo := TModifierInfo.Create;
      ModifierInfo.fSurface  := Surface;
      ModifierInfo.fValue    := Value;
      ModifierInfo.fStrength := Strength;
      fModifiers.Insert( ModifierInfo );
    end;


  // TEnvironmentalBlock

  constructor TEnvironmentalBlock.Create(aMetaBlock : TMetaBlock; aFacility : TFacility);
    begin
      inherited;
    end;

  destructor TEnvironmentalBlock.Destroy;
    begin
      fModifiers.ExtractAll;
      fModifiers.Free;
      inherited;
    end;

  function TEnvironmentalBlock.Evaluate : TEvaluationResult;
    begin
      result := inherited Evaluate;
      if TMetaEnvironmentalBlock(MetaBlock).DissabledStop and not Facility.Deleted
        then Facility.ClearTrouble(facStoppedByTycoon);
      if not Facility.CriticalTrouble
        then BlockGenMoney(-dt*UpgradeLevel*TMetaEnvironmentalBlock(MetaBlock).MaintCost, accIdx_Public);
    end;

  procedure TEnvironmentalBlock.AutoConnect( loaded : boolean );
    var
      i : integer;
    begin
      inherited;
      fModifiers := TCollection.Create( 0, rkBelonguer );
      with TMetaEnvironmentalBlock(MetaBlock) do
        for i := 0 to pred(fModifiers.Count) do
          with TModifierInfo(fModifiers[i]) do
            Self.fModifiers.Insert(
              TPyramidalModifier.Create(
                fSurface,
                Point(xOrigin, yOrigin),
                fValue,
                fStrength));
    end;

  procedure TEnvironmentalBlock.LoadFromBackup(Reader : IBackupReader);
    begin
      inherited;
    end;

  procedure TEnvironmentalBlock.StoreToBackup(Writer : IBackupWriter);
    begin
      inherited;
    end;

  procedure TEnvironmentalBlock.Deleted;
    var
      i : integer;
    begin
      for i := 0 to pred(fModifiers.Count) do
        TSurfaceModifier(fModifiers[i]).Delete;
      inherited;
    end;

  // Register backup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass(TEnvironmentalBlock);
    end;


end.

