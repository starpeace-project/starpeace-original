unit SpontaneousBuildings;

interface

  uses
    Protocol, Kernel, PopulatedBlock, Surfaces, BackupInterfaces;

  type
    TGrowthDir = (dirN, dirE, dirS, dirW);

  type
    TSpontaneousBuilding = class;

    TNeighborBuildings = array[TGrowthDir] of TSpontaneousBuilding;
    TNeighborBlocked   = array[TGrowthDir] of boolean;

    TSpontaneousBuilding =
      class( TPopulatedBlock )
        public
          destructor Destroy; override;
        protected
          function Evaluate : TEvaluationResult; override;
        public
          procedure AutoConnect( loaded : boolean ); override;
          function  GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string; override;
        protected
          function ComputeCrime : TSurfaceValue; override;
        private
          fNeighbors  : TNeighborBuildings;
          fBlocked    : TNeighborBlocked;
          fAllBlocked : boolean;
        private
          function  GetNeighbor( dir : TGrowthDir ) : TSpontaneousBuilding;
          procedure SetNeighbor( dir : TGrowthDir; N : TSpontaneousBuilding );
          function  GetBlocked ( dir : TGrowthDir ) : boolean;
          procedure SetBlocked ( dir : TGrowthDir; Blocked : boolean );
        public
          property Neighbors[dir : TGrowthDir] : TSpontaneousBuilding read GetNeighbor write SetNeighbor;
          property Blocked[dir : TGrowthDir]   : boolean read GetBlocked write SetBlocked;
        public
          function GetAvailableDir( out dir : TGrowthDir ) : boolean;
        public
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

  const
    MinOccupancy = 2;   // Building will disappear if occupancy drops below min
    MinAge       = 48;  // Min age of building to be destroyed if empty

  procedure RegisterBackup;          

implementation

  uses
    Population, SysUtils;

                                    
  // TSpontaneousBuilding

  destructor TSpontaneousBuilding.Destroy;
    begin
      TTownHall(TInhabitedTown(Facility.Town).TownHall.CurrBlock).DestroySpontaneousBuilding( Facility );
      inherited;
    end;

  function TSpontaneousBuilding.Evaluate : TEvaluationResult;

    function OppositeDir( dir : TGrowthDir ) : TGrowthDir;
      begin
        case dir of
          dirN : result := dirS;
          dirE : result := dirW;
          dirS : result := dirN;
          else   result := dirE;
        end;
      end;
    {
    var
      dir : TGrowthDir;
    }
    begin                                       
      result := inherited Evaluate;
      {
      if (Occupancy < MinOccupancy) and (Facility.Age > MinAge)
        then
          begin
            for dir := low(dir) to high(dir) do
              if fNeighbors[dir] <> nil
                then fNeighbors[dir].fNeighbors[OppositeDir(dir)] := nil;
            TInhabitedTown(Facility.Town).World.RequestDeletion( Facility );
          end;
      }
    end;

  procedure TSpontaneousBuilding.AutoConnect( loaded : boolean );
    begin
      inherited;
      if not loaded
        then
          begin
            fPeople.Q := 1 + random(10);
            fPeople.K := 0;
          end;
    end;
    
  function TSpontaneousBuilding.GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string;
    begin
      result := inherited GetStatusText( kind, ToTycoon );
      case kind of
        sttHint :
          result :=
            'This house was constructed by poor people that wanted to move to this town ' +
            'but did not find a fair offer. You cannot demolish this building. It will ' +
            'dissapear once its inhabitants move somewhere else.';
      end;
    end;

  function TSpontaneousBuilding.ComputeCrime : TSurfaceValue;
    begin
      result := 10*inherited ComputeCrime;
    end;
    
  function TSpontaneousBuilding.GetNeighbor( dir : TGrowthDir ) : TSpontaneousBuilding;
    begin
      result := fNeighbors[dir];
    end;

  procedure TSpontaneousBuilding.SetNeighbor( dir : TGrowthDir; N : TSpontaneousBuilding );
    begin
      fNeighbors[dir] := N;
      if N = nil
        then Blocked[dir] := false;
    end;

  function TSpontaneousBuilding.GetBlocked( dir : TGrowthDir ) : boolean;
    begin
      result := fBlocked[dir];
    end;

  procedure TSpontaneousBuilding.SetBlocked( dir : TGrowthDir; Blocked : boolean );
    begin
      fBlocked[dir] := Blocked;
      dir := low(dir);
      while fBlocked[dir] and (dir < high(dir)) do
        inc( dir );
      fAllBlocked := fBlocked[dir];
    end;

  function TSpontaneousBuilding.GetAvailableDir( out dir : TGrowthDir ) : boolean;
    var
      startdir : TGrowthDir;
    begin
      if not fAllBlocked
        then
          begin
            startdir := TGrowthDir(random(ord(high(startdir)) + 1));
            dir := startdir;
            repeat
              if dir < high(dir)
                then inc( dir )
                else dir := low(dir);
            until (fNeighbors[dir] = nil) and not fBlocked[dir] or (dir = startdir);
            result := (fNeighbors[dir] = nil) and not fBlocked[dir];
        end
      else result := false;
    end;

  procedure TSpontaneousBuilding.LoadFromBackup( Reader : IBackupReader );
    var
      dir : TGrowthDir;
    begin
      inherited;
      for dir := low(dir) to high(dir) do
        Reader.ReadObject( 'Neighbors' + IntToStr(ord(dir)), fNeighbors[dir], nil );
      Reader.ReadBuffer( 'Blocked', fBlocked, nil, sizeof(fBlocked) );
      fAllBlocked := Reader.ReadBoolean( 'AllBlocked', false );
    end;

  procedure TSpontaneousBuilding.StoreToBackup( Writer : IBackupWriter );
    var
      dir : TGrowthDir;
      aux : string;
    begin
      inherited;
      for dir := low(dir) to high(dir) do
        begin
          aux := 'Neighbors' + IntToStr(ord(dir));
          Writer.WriteObjectRef( aux, fNeighbors[dir] );
        end;
      Writer.WriteBuffer( 'Blocked', fBlocked, sizeof(fBlocked) );
      Writer.WriteBoolean( 'AllBlocked', fAllBlocked );
      aux := '';
    end;


  // RegisterBackup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass( TSpontaneousBuilding );
    end;

end.
