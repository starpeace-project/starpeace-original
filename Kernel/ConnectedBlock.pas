unit ConnectedBlock;

interface

  uses
    Kernel, Protocol, Circuits, Collection, BackupInterfaces, TransportInterfaces, Variants;

  type
    TCargoArray = array[TCargoKind] of ICargoPoint;

  type
    TConnectedBlock =
      class( TBlock )
        protected
          constructor Create( aMetaBlock : TMetaBlock; aFacility : TFacility ); override;
        public
          destructor  Destroy; override;
        private
          fRoads : TCollection;
          fCargo : TCargoArray;
        protected
          procedure SetCargoValue( CargoKind : TCargoKind; CargoValue : single ); override;
          procedure DelCargoValue( CargoKind : TCargoKind );                      override;
          function  GetCircuits( CircuitId : TCircuitId ) : TCollection;          override;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

  procedure RegisterBackup;

implementation

  uses
    MathUtils;


  constructor TConnectedBlock.Create( aMetaBlock : TMetaBlock; aFacility : TFacility );
    begin
      inherited;
      fRoads := TCollection.Create( 0, rkUse );
    end;

  destructor TConnectedBlock.Destroy;
    var
      kind : TCargoKind;
    begin
      for kind := low(kind) to high(kind) do
        DelCargoValue( kind );                   
      fRoads.Free;
      inherited;                               
    end;

  procedure TConnectedBlock.SetCargoValue( CargoKind : TCargoKind; CargoValue : single );
    var
      CargoSystem : ICargoSystem;
      CargoLayer  : ICargoLayer;
      value       : TCargoValue;
    begin
      if fCargo[CargoKind] = nil
        then
          begin
            CargoSystem := Facility.Town.RoadHandler.GetCargoSystem( cirRoads );
            CargoLayer  := CargoSystem.GetLayer( ord(CargoKind) );
            fCargo[CargoKind] := CargoLayer.CreatePoint( Facility.XPos, Facility.YPos );
          end;
      value := min( high(value), round(CargoValue) );
      fCargo[CargoKind].setValue( value );
    end;
                                       
  procedure TConnectedBlock.DelCargoValue( CargoKind : TCargoKind );
    var
      CargoSystem : ICargoSystem;
      CargoLayer  : ICargoLayer;
    begin
      if fCargo[CargoKind] <> nil
        then
          begin
            CargoSystem := Facility.Town.RoadHandler.GetCargoSystem( cirRoads );
            CargoLayer  := CargoSystem.GetLayer( ord(CargoKind) );
            CargoLayer.DelPoint( Facility.XPos, Facility.YPos );
          end;
    end;

  function TConnectedBlock.GetCircuits( CircuitId : TCircuitId ) : TCollection;
    begin
      if CircuitId = cirRoads
        then result := fRoads
        else result := nil;
    end;

  procedure TConnectedBlock.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      Reader.ReadObject( 'NearRoads', fRoads, nil );
      if fRoads = nil
        then fRoads := TCollection.Create( 0, rkUse );
    end;

  procedure TConnectedBlock.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteObject( 'NearRoads', nil ); //Writer.WriteObject( 'NearRoads', fRoads );
    end;


  // RegisterBackup

  procedure RegisterBackup;
    begin
      RegisterClass( TConnectedBlock );
    end;

end.

