unit Police;

interface

  uses
    PublicFacility, Surfaces, BackupInterfaces;

  const
    modPoliceStrength = 1;

  type
    TPoliceBlock =
      class( TPublicFacility )
        public
          destructor Destroy; override;
        protected
          procedure AutoConnect; override;
        private
          fCrimeModifier : TSurfaceModifier;
        public
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

  procedure RegisterBackup;

implementation

  uses
    Classes, Kernel, PyramidalModifier;

  destructor TPoliceBlock.Destroy;
    begin
      fCrimeModifier.Delete;
      inherited;
    end;

  procedure TPoliceBlock.AutoConnect;
    begin
      inherited;
      fCrimeModifier :=
        TPyramidalModifier.Create(
          tidEnvironment_Crime,
          Point(xOrigin, yOrigin),
          -TMetaPublicFacility(MetaBlock).Strength,
          modPoliceStrength );
    end;

  procedure TPoliceBlock.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      Reader.ReadObject( 'CrimeModifier', fCrimeModifier, nil );
    end;

  procedure TPoliceBlock.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteLooseObject( 'CrimeModifier', fCrimeModifier );
    end;


  // RegisterBackup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass( TPoliceBlock );
    end;    

end.


