unit Disasters;

interface

  uses
    MetaInstances, BackupInterfaces, Kernel, Population, Persistent, Variants;

  const
    tidClassFamily_Disasters = 'Disasters';

  type
    TMetaDisaster = class;
    TDisaster     = class;

    CDisaster = class of TDisaster;

    TMetaDisaster =
      class( TMetaInstance )
        public
          constructor Create( anId : string; aDisaterType : CDisaster );
        private
          fDisasterType : CDisaster;
        public
          function Instantiate( TownHall : TTownHall ) : TDisaster;
      end;

    TDisasterProgress = 0..100;
    TDisasterResult =
      record
        Dead : array[TPeopleKind] of TFluidValue;
        Left : array[TPeopleKind] of TFluidValue;
      end;

    TDisaster =
      class( TPersistent )
        protected
          constructor Create( aMetaDisaster : TMetaDisaster; aTownHall : TTownHall ); virtual;
        private
          fMetaDisaster : TMetaDisaster;
          fTownHall     : TTownHall;
          fProgress     : TDisasterProgress;
        public
          property MetaDisaster : TMetaDisaster     read fMetaDisaster;
          property TownHall     : TTownHall         read fTownHall;
          property Progress     : TDisasterProgress read fProgress;
        public
          function Act( dt : TTimeDelta ) : TDisasterResult; virtual;
        protected
          procedure SetProgress( aProgress : TDisasterProgress ); virtual;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    procedure RegisterBackup;

implementation

  uses
    ClassStorage;
    

  // TMetaDisaster

  constructor TMetaDisaster.Create( anId : string; aDisaterType : CDisaster );
    begin
      inherited Create( anId );
      fDisasterType := aDisaterType;
      Register( tidClassFamily_Disasters );
    end;

  function TMetaDisaster.Instantiate( TownHall : TTownHall ) : TDisaster;
    begin
      result := fDisasterType.Create( self, TownHall );
    end;


  // TDisaster

  constructor TDisaster.Create( aMetaDisaster : TMetaDisaster; aTownHall : TTownHall );
    begin
      inherited Create;
      fMetaDisaster := aMetaDisaster;
      fTownHall     := aTownHall;
      fProgress     := 0;
    end;

  function TDisaster.Act( dt : TTimeDelta ) : TDisasterResult; 
    var
      i : TPeopleKind;
    begin
      for i := low(i) to high(i) do
        begin
          result.Dead[i] := 0;
          result.Left[i] := 0;
        end;
    end;

  procedure TDisaster.SetProgress( aProgress : TDisasterProgress );
    begin
      fProgress := aProgress;
    end;

  procedure TDisaster.LoadFromBackup( Reader : IBackupReader );
    var
      MetaDisasterId : string;
    begin
      inherited;
      MetaDisasterId := Reader.ReadString( 'MetaDisasterId', '' );
      try
        fMetaDisaster := TMetaDisaster(TheClassStorage.ClassById[tidClassFamily_Disasters, MetaDisasterId]);
      except
        fMetaDisaster := nil;
      end;
      Reader.ReadObject( 'TownHall', fTownHall, nil );
    end;

  procedure TDisaster.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteString( 'MetaDisasterId', fMetaDisaster.Id );
      Writer.WriteObjectRef( 'TownHall', fTownHall );
    end;


  // RegisterBackup

  procedure RegisterBackup;
    begin
      RegisterClass( TDisaster );
    end;

end.
