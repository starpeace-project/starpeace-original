unit ServerTrain;

interface

  uses
    Classes, Windows, Train, Protocol, Kernel, Circuits, BackupInterfaces, Persistent, Collection,
    StateEngine, Automaton, ActorPool, ActorTypes, MetaInstances, Accounts, World;

  type
    TRailRoadCircuit = class;
    TRailRoadSegment = class;
    TRailRoadNode    = class;
    TServerCar       = class;
    TMetaCargoCar    = class;
    TCargoCar        = class;
    TMetaLocomotive  = class;
    TLocomotive      = class;
    TPathSeg         = class;
    TGeoPathSeg      = class;
    TTrain           = class;
    TRoutePoint      = class;
    TRailRoadCompany = class;

    CRailRoadCircuit = class of TRailRoadCircuit;
    CRailRoadSegment = class of TRailRoadSegment;
    CRailRoadNode    = class of TRailRoadNode;
    CServerCar       = class of TCar;
    CMetaCargoCar    = class of TMetaCargoCar;
    CCargoCar        = class of TCargoCar;
    CMetaLocomotive  = class of TMetaLocomotive;
    CLocomotive      = class of TLocomotive;
    CTrain           = class of TTrain;
    CRoutePoint      = class of TRoutePoint;
    CRailRoadCompany = class of TRailRoadCompany;

    TRailRoadCircuit =
      class( TCircuit )
      end;

    TRailRoadSegment =
      class( TSegment )
        protected
          constructor Create( aCircuit : TCircuit; anOwnerId : TOwnerId ); override;
        public
          destructor Destroy; override;          
        private
          fCars       : TLockableCollection;
          fReservedBy : TTrain;
          fMarked     : boolean;
        public
          function IsFree( Dir : TSegmentDirection; Asker : TTrain ) : boolean;
        public
          property Cars       : TLockableCollection read fCars;
          property ReservedBy : TTrain              read fReservedBy;
        protected
          procedure SegmentBroken( x, y : integer; NewSeg : TSegment ); override;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    TRailRoadNode =
      class( TNode )
      end;

    TServerCar =
      class( TCar )
        public
          destructor Destroy; override;
        protected
          procedure Act( State : TStateId; Mode : TActMode ); override;
          function  CarAtCheckPoint      : boolean;           virtual;
          function  CarAtEndOfSegment    : boolean;           virtual;
          function  CarAtEndOfGeoSegment( Tolerance : single ) : boolean; virtual;
        private
          fCheckpointPassed : boolean;
        public
          function GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string; virtual;
        protected
          procedure SetAcceleration ( anAcceleration : TCarAcceleration ); override;
          procedure SetSpeed        ( aSpeed : TCarSpeed );                override;
          procedure SetAngle        ( anAngle : single );                  override;
          procedure SetVisualStage  ( aVisualStage : integer );            override;
          procedure SetDirection    ( aDirection : TCarDirection );        override;
          procedure SetNextDirection( aNextDirection : TCarDirection );    override;
        private
          fTrain      : TTrain;
          fPrevCar    : TServerCar;
          fNextCar    : TServerCar;
          fCurrSeg    : TRailRoadSegment;
          fCurrGeoSeg : TGeoPathSeg;
          fBreaking   : boolean;
        public
          property Train      : TTrain           read fTrain      write fTrain;
          property PrevCar    : TServerCar       read fPrevCar    write fPrevCar;
          property NextCar    : TServerCar       read fNextCar    write fNextCar;
          property CurrSeg    : TRailRoadSegment read fCurrSeg    write fCurrSeg;
          property CurrGeoSeg : TGeoPathSeg      read fCurrGeoSeg write fCurrGeoSeg;
        protected
          procedure UpdateTrainParameters; virtual;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); virtual;
          procedure StoreToBackup ( Writer : IBackupWriter ); virtual;
        protected
          procedure MsgAnswerActorInViewport( var Msg : TViewportAwarenessInfo ); message msgAnswerActorInViewport;
        private
          procedure Synchronize;
      end;

    TMetaCargoCar =
      class( TMetaCar )
        public
          constructor Create( anId             : TActorKind;
                              aName            : string;
                              aDesc            : string;
                              aPrice           : Currency;
                              aWeight          : TCarWeight;
                              aOperationCost   : Currency;
                              aBaseVisualClass : TCarVisualClass;
                              aCargoType       : string;
                              aCapacity        : TFluidValue;
                              aCarClass        : CCar );

        private
          fCargoType : TMetaFluid;
          fCapacity  : TFluidValue;
        public
          property CargoType : TMetaFluid  read fCargoType;
          property Capacity  : TFluidValue read fCapacity;
      end;

    TCargoCar =
      class( TServerCar )
        private
          fLoadPerc : TPercent;
        public
          property LoadPerc : TPercent read fLoadPerc;
        protected
          procedure UpdateTrainParameters; override;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    TMetaLocomotive =
      class( TMetaCar )
        public
          constructor Create( anId             : TActorKind;
                              aName            : string;
                              aDesc            : string;
                              aPrice           : Currency;
                              aWeight          : TCarWeight;
                              aOperationCost   : Currency;
                              aBaseVisualClass : TCarVisualClass;
                              aStrength        : TCarWeight;
                              aMaxSpeed        : TCarSpeed;
                              aCarClass        : CCar );
        private
          fStrength : TCarWeight;
          fMaxSpeed : TCarSpeed;
        public
          property Strength : TCarWeight read fStrength;
          property MaxSpeed : TCarSpeed  read fMaxSpeed;
      end;

    TLocomotive =
      class( TServerCar )
        protected
          procedure UpdateTrainParameters; override;
      end;

    TPathAvailability = (pthFree, pthNearBlocked, pthFarBlocked);
    TPathSeg =
      class( TPersistent )
        public
          constructor Create( aTrain   : TTrain;
                              aDir     : TSegmentDirection;
                              aSegment : TRailroadSegment;
                              aLength  : integer );
        private
          fTrain   : TTrain;
          fDir     : TSegmentDirection;
          fSegment : TRailRoadSegment;
          fLength  : integer;
        private
          function GetStartNode : TNode;
          function GetEndNode : TNode;
        public
          property StartNode : TNode read GetStartNode;
          property EndNode : TNode read GetEndNode;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    TGeoPathSeg =
      class( TPersistent )
        public
          constructor Create( aX, aY : single; aPathSeg : TPathSeg; aPath : TCollection );
        private
          fX       : single;
          fY       : single;
          fPathSeg : TPathSeg;
          fPath    : TCollection;
          fIndex   : integer;
        private
          function GetX2 : single;
          function GetY2 : single;
          function GetAngle  : single;
          function GetNext   : TGeoPathSeg;
          function GetLength : single;
        public
          property x1      : single      read fX;
          property y1      : single      read fY;
          property x2      : single      read GetX2;
          property y2      : single      read GetY2;
          property PathSeg : TPathSeg    read fPathSeg;
          property Angle   : single      read GetAngle;
          property Next    : TGeoPathSeg read GetNext;
          property Length  : single      read GetLength;
        public
          function CarInSeg( Car : TServerCar; Tol : single ) : boolean; virtual;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    TTrainState = (stStopped, stLoading, stLeaving, stRunning, stSlowingDown, stStopping, stBlocked);
    TTrain =
      class( TPersistent )
        public
          constructor Create( aCompany : TRailRoadCompany );
          destructor  Destroy; override;
        private
          fName         : string;
          fCompany      : TRailRoadCompany;
          fRoute        : TLockableCollection;
          fCars         : TLockableCollection;
          fWeight       : TCarWeight;
          fAcceleration : TCarAcceleration;
          fStrength     : TCarWeight;
          fMaxSpeed     : TCarSpeed;
          fState        : TTrainState;
          fPath         : TCollection;
          fExtPath      : TCollection;
          fGeoPath      : TCollection;
          fPathIdx      : integer;
          fArriving     : boolean;
          fNextPoint    : integer;
          fAltX         : integer;
          fAltY         : integer;
          fLoadTurns    : integer;
          fStartX       : integer;
          fStartY       : integer;
          fStartDir     : TCarDirection;
          fStartSeg     : TRailroadSegment;
          fExtraSpace   : integer;
        private
          function GetNextX : integer;
          function GetNextY : integer;
        public
          property Name         : string              read fName     write fName;
          property Company      : TRailRoadCompany    read fCompany;
          property Route        : TLockableCollection read fRoute;
          property Cars         : TLockableCollection read fCars;
          property Weight       : TCarWeight          read fWeight   write fWeight;
          property Acceleration : TCarAcceleration    read fAcceleration;
          property Strength     : TCarWeight          read fStrength   write fStrength;
          property MaxSpeed     : TCarSpeed           read fMaxSpeed   write fMaxSpeed;
          property State        : TTrainState         read fState      write fState;
          property NextPoint    : integer             read fNextPoint  write fNextPoint;
          property NextX        : integer             read GetNextX;
          property NextY        : integer             read GetNextY;
          property AltX         : integer             read fAltX write fAltX;
          property AltY         : integer             read fAltY write fAltY;
          property StartX       : integer             read fStartX   write fStartX;
          property StartY       : integer             read fStartY   write fStartY;
          property StartDir     : TCarDirection       read fStartDir write fStartDir;
          property StartSeg     : TRailroadSegment    read fStartSeg write fStartSeg;
        public
          procedure Act; virtual;
          procedure ForceFindNewPath;
        private
          fPrevAccel : TCarAcceleration;
          fBreaksOn  : boolean;
        private
          //procedure PushBreaks;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    TRoutePoint =
      class( TPersistent )
        private
          fStation     : TBlock;
          fWhenToLeave : TWhenToLeave;
        public
          property Station     : TBlock       read fStation     write fStation;
          property WhenToLeave : TWhenToLeave read fWhenToLeave write fWhenToLeave;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    TRailRoadCompany =
      class( TCompany )
        public
          constructor Create( anId : TCompanyId ); override;
          destructor  Destroy; override;
        private
          fTrains      : TLockableCollection;
          fLastTrainId : integer;
        public
          property Trains      : TLockableCollection read fTrains;
          property LastTrainId : integer             read fLastTrainId write fLastTrainId;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

  const
    StillStates = [stStopped, stLoading];

  const
    Margin            = 10;
    RailroadTolerance = 3;
    ArrivalSpeed      = 0.4;

  function SegmentDirToCarDir( SegmentDir : TSegmentDirection ) : TCarDirection;
  function CarDirToSegmentDir( CarDir : TCarDirection ) : TSegmentDirection;

  procedure RegisterBackup;

implementation

  uses
    SysUtils, ClassStorage, DistributedStates, MathUtils, LogFile;
    

  // TRailRoadSegment

  constructor TRailRoadSegment.Create( aCircuit : TCircuit; anOwnerId : TOwnerId );
    begin
      inherited;
      fCars := TLockableCollection.Create( 0, rkUse );
    end;

  destructor TRailRoadSegment.Destroy;
    begin
      fCars.Free;
      inherited;
    end;

  function TRailRoadSegment.IsFree( Dir : TSegmentDirection; Asker : TTrain ) : boolean;

    function ContinousSegsAreFree( FromSeg : TRailroadSegment; FromDir : TSegmentDirection ) : boolean;
      var
        Node  : TNode;
        count : integer;
        dir   : TSegmentDirection;
        vdir  : TSegmentDirection;
      begin
        case FromDir of
          segNorth, segWest :
            Node := FromSeg.NodeA;
          else
            Node := FromSeg.NodeB;
        end;
        count := 0;
        vdir  := segNorth;
        for dir := low(dir) to high(dir) do
          if Node.Segments[dir] <> nil
            then
              begin
                inc( count );
                vdir := dir;
              end;
        result := ((FromSeg.ReservedBy = nil) or (FromSeg.ReservedBy = Asker)) and
                  ((FromSeg.Cars.Count = 0) or (TServerCar(FromSeg.Cars[0]).Train = Asker)) and
                  ((count <> 0) or ContinousSegsAreFree( TRailroadSegment(Node.Segments[vdir]), vdir ));
      end;

    begin
      result := ContinousSegsAreFree( self, dir );
    end;

  procedure TRailRoadSegment.SegmentBroken( x, y : integer; NewSeg : TSegment );
    var
      i   : integer;
      Car : TServerCar;
    begin
      for i := pred(Cars.Count) downto 0 do
        begin
          Car := TServerCar(Cars[i]);
          if IsHorizontal and (Car.x > x) or not IsHorizontal and (Car.y > y)
            then
              begin
                Cars.Delete( Car );
                Car.fCurrSeg := TRailRoadSegment(NewSeg);
                TRailRoadSegment(NewSeg).Cars.Insert( Car );
              end;
        end;
    end;
    
  procedure TRailRoadSegment.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      Reader.ReadObject( 'Cars', fCars, nil );
      Reader.ReadObject( 'ReservedBy', fReservedBy, nil );
    end;
    
  procedure TRailRoadSegment.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteLooseObject( 'Cars', fCars );
      Writer.WriteObjectRef( 'ReservedBy', fReservedBy );
    end;


  // TServerCar

  destructor TServerCar.Destroy;
    begin
      if fCurrSeg <> nil
        then fCurrSeg.Cars.Delete( self );
      inherited;
    end;

  procedure TServerCar.Act( State : TStateId; Mode : TActMode );

    function FindBindingMargins( out dx, dy : single ) : boolean;
      begin
        dx := 0;
        dy := 0;
        {
        if PrevCar <> nil
          then
            begin
              if round(1000*abs(cos(Angle)*(PrevCar.x - x))) < round(1000*abs(cos(Angle)*(Speed + 0.2)))
                then dx := cos(Angle)*abs(PrevCar.x - x) - cos(Angle)*(Speed + 0.2);
              if round(1000*abs(sin(Angle)*(PrevCar.y - y))) < round(1000*abs(sin(Angle)*(Speed + 0.2)))
                then dy := sin(Angle)*abs(PrevCar.y - y) - sin(Angle)*(Speed + 0.2)
            end;
        }
        {
        if PrevCar <> nil
          then
            begin
              if abs(round(1000*cos(Angle)*abs(PrevCar.x - x))) < abs(round(1000*cos(Angle)*2*Speed))
                then dx := dx + cos(Angle)*(2*Speed - abs(PrevCar.x - x));
              if abs(round(1000*sin(Angle)*abs(PrevCar.y - y))) < abs(round(1000*sin(Angle)*2*Speed))
                then dy := dy + cos(Angle)*(2*Speed - abs(PrevCar.y - y));
            end;
        }
        {
        if NextCar <> nil
          then
            begin
              if (round(1000*sin(Angle)) = 0) and (abs(NextCar.x - x) > 1)
                then dx := dx - (1 - abs(NextCar.x - x));
              if (round(1000*cos(Angle)) = 0) and (abs(NextCar.y - y) > 1)
                then dy := dy - (1 - abs(NextCar.y - y));
              if
            end;
        }
        result := (dx <> 0) or (dy <> 0);
      end;

    function TravelGeoSegs( var x, y : single ) : boolean;
      var
        xSpeed, ySpeed : single;
        module         : single;
        Left           : single;
        finished       : boolean;
      begin
        result := false;
        Left   := fSpeed;
        if fCurrGeoSeg <> nil
          then
            repeat
              module := sqrt(sqr(fCurrGeoSeg.x2 - x) + sqr(fCurrGeoSeg.y2 - y));
              if Left > module
                then
                  begin
                    Left        := Left - module;
                    x           := fCurrGeoSeg.x2;
                    y           := fCurrGeoSeg.y2;
                    fCurrGeoSeg := fCurrGeoSeg.Next;
                    result      := true;
                    finished    := false;
                  end
                else finished := true
            until finished or (fCurrGeoSeg = fCurrGeoSeg.Next);
        if fCurrGeoSeg <> nil
          then fAngle := fCurrGeoSeg.Angle;
        xSpeed := Left*cos(fAngle);
        ySpeed := Left*sin(fAngle);
        x := x + xSpeed;
        y := y + ySpeed;
      end;

    var
      NewSeg          : TSegment;
      mustSynchronize : boolean;
      // xSpeed          : single;
      // ySpeed          : single;
    begin
      fSpeed := realmax(0, realmin( MaxSpeed, fSpeed + fAcceleration ));
      mustSynchronize := TravelGeoSegs( fX, fY );
      {
      xSpeed := fSpeed*cos(fAngle) + dx;
      ySpeed := fSpeed*sin(fAngle) + dy;
      fX     := fX + xSpeed;
      fY     := fY + ySpeed;
      }
      if (PrevCar <> nil) and CarAtCheckPoint and PrevCar.fCheckpointPassed
        then
          begin
            NextDirection     := PrevCar.NextDirection;
            fCheckpointPassed := true;
          end;
      if mustSynchronize //CarAtEndOfGeoSegment( 0.5 )
        then
          begin
            if CarAtEndOfSegment
              then
                begin
                  if (PrevCar <> nil) and not fCheckpointPassed
                    then NextDirection := PrevCar.Direction;
                  LogThis( Train.Name + ': Car ' + IntToStr(Train.Cars.IndexOf( self )) + ' Reached end of segment.' );
                  case Direction of
                    cdirN, cdirW :
                      NewSeg := fCurrSeg.NodeA.Segments[CarDirToSegmentDir( NextDirection )];
                    else
                      NewSeg := fCurrSeg.NodeB.Segments[CarDirToSegmentDir( NextDirection )];
                  end;
                  if NewSeg <> nil
                    then
                      begin
                        Direction := NextDirection;
                        fCurrSeg.fCars.Delete( self );
                        fCurrSeg := TRailRoadSegment(NewSeg);
                        fCurrSeg.fCars.Insert( self );
                        fCurrSeg.fReservedBy := nil;
                        fCheckpointPassed := false;
                      end
                    else
                      begin
                        LogThis( Train.Name + ': Car ' + IntToStr(Train.Cars.IndexOf( self )) + ' NEXT SEGMENT NOT FOUND!' );
                        NextDirection := Direction;
                      end;
                end;
          end;
      LogThis( Train.Name + ': Car ' + IntToStr(Train.Cars.IndexOf( self )) + ' accel: ' + FloatToStr(fAcceleration) + ', speed: ' + FloatToStr(fSpeed) + ', x: ' + FloatToStr(x) + ', y: ' + FloatToStr(y) + ', ang: ' + IntToStr(round(180*Angle/pi)) + ', CHK: ' + IntToStr(integer( fCheckpointPassed )) );
      if mustSynchronize
        then Synchronize;
    end;

  function TServerCar.CarAtCheckPoint : boolean;
    begin
      result :=
        not fCheckpointPassed and
        ((Direction = cdirN) and (y <= CurrSeg.NodeA.y + 2) or
         (Direction = cdirE) and (x >= CurrSeg.NodeB.x - 2) or
         (Direction = cdirS) and (y >= CurrSeg.NodeB.y - 2) or
         (Direction = cdirW) and (x <= CurrSeg.NodeA.x + 2));
    end;

  {
  function TServerCar.CarAtEndOfSegment : boolean;
    var
      roundX, roundY : integer;
    begin
      roundX := trunc( x );
      roundY := trunc( y );
      result :=
        (Direction = cdirN) and (roundY <= CurrSeg.NodeA.y) or
        (Direction = cdirE) and (roundX >= CurrSeg.NodeB.x) or
        (Direction = cdirS) and (roundY >= CurrSeg.NodeB.y) or
        (Direction = cdirW) and (roundX <= CurrSeg.NodeA.x);
    end;
  }

  function TServerCar.CarAtEndOfSegment : boolean;
    begin
      result := fCurrGeoSeg.PathSeg.fSegment <> CurrSeg;
    end;

  function TServerCar.CarAtEndOfGeoSegment( Tolerance : single ) : boolean;
    var
      ang  : integer;
      xTol : single;
      yTol : single;
    begin
      xTol := abs(round(10000*Tolerance*cos(Angle))/10000);
      yTol := abs(round(10000*Tolerance*sin(Angle))/10000);
      ang := round(180*Angle/pi);
      with CurrGeoSeg do
        result :=
          (ang >= 0)  and (ang < 90)    and ((x + xTol > x2) or (y + yTol > y2)) or
          (ang >= 90) and (ang <= 180)  and ((x - xTol < x2) or (y + yTol > y2)) or
          (ang < 0)   and (ang >= -90)  and ((x + xTol > x2) or (y - yTol < y2)) or
          (ang < -90) and (ang >= -180) and ((x - xTol < x2) or (y - yTol < y2));
    end;

  function TServerCar.GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string;
    begin
      result := '';
    end;
    
  procedure TServerCar.SetAcceleration( anAcceleration : TCarAcceleration );
    var
      Event : TDistributedEvent;
    begin
      if Acceleration <> anAcceleration
        then
          begin
            Event := TDistributedEvent.Create( evnAccelerationChange, dmBroadCast, anAcceleration, sizeof(anAcceleration) );
            AutomationEngine.ThrowEvent( Event );
            LogThis( Train.Name + ': Car ' + IntToStr(Train.Cars.IndexOf( self )) + ' Synchronizing acceleration: ' + FloatToStr(anAcceleration) );
          end;
      inherited;
    end;

  procedure TServerCar.SetSpeed( aSpeed : TCarSpeed );
    var
      Event : TDistributedEvent;
    begin
      if (aSpeed = 0) and (Speed <> 0)
        then
          begin
            Event := TDistributedEvent.Create( evnSpeedChange, dmBroadCast, aSpeed, sizeof(aSpeed) );
            AutomationEngine.ThrowEvent( Event );
            LogThis( Train.Name + ': Car ' + IntToStr(Train.Cars.IndexOf( self )) + ' Synchronizing speed: ' + FloatToStr(aSpeed) );
          end;
      inherited;
    end;

  procedure TServerCar.SetAngle( anAngle : single );
    begin
      inherited;
    end;

  procedure TServerCar.SetVisualStage( aVisualStage : integer );
    var
      Event : TDistributedEvent;
    begin
      if VisualStage <> aVisualStage
        then
          begin
            Event := TDistributedEvent.Create( evnVisualStageChange, dmBroadCast, aVisualStage, sizeof(aVisualStage) );
            AutomationEngine.ThrowEvent( Event );
            LogThis( Train.Name + ': Car ' + IntToStr(Train.Cars.IndexOf( self )) + ' Synchronizing Visual stage: ' + IntToStr(aVisualStage) );
          end;
      inherited;
    end;

  procedure TServerCar.SetDirection( aDirection : TCarDirection );
    var
      Event : TDistributedEvent;
    begin
      if Direction <> aDirection
        then
          begin
            Event := TDistributedEvent.Create( evnDirectionChange, dmBroadCast, aDirection, sizeof(aDirection) );
            AutomationEngine.ThrowEvent( Event );
          end;
      LogThis( Train.Name + ': Car ' + IntToStr(Train.Cars.IndexOf( self )) + ' Synchronizing direction: ' + IntToStr(integer(aDirection)) );
      inherited;
    end;

  procedure TServerCar.SetNextDirection( aNextDirection : TCarDirection );
    var
      Event : TDistributedEvent;
    begin
      if NextDirection <> aNextDirection
        then
          begin
            Event := TDistributedEvent.Create( evnNextDirectionChange, dmBroadCast, aNextDirection, sizeof(aNextDirection) );
            AutomationEngine.ThrowEvent( Event );
          end;
      LogThis( Train.Name + ': Car ' + IntToStr(Train.Cars.IndexOf( self )) + ' Synchronizing NEXT direction: ' + IntToStr(integer(aNextDirection)) );
      inherited;
    end;

  procedure TServerCar.UpdateTrainParameters;
    begin
      Train.Weight := Train.Weight + MetaCar.Weight;
    end;

  procedure TServerCar.LoadFromBackup( Reader : IBackupReader );
    var
      MetaCarId : TActorKind;
    begin
      inherited;
      MetaCarId      := Reader.ReadInteger( 'MetaCarId', -1 );
      fMetaCar       := TMetaCar(TheClassStorage.ClassById[tidClassFamily_TrainCars, IntToStr(MetaCarId)]);
      fAcceleration  := Reader.ReadSingle( 'Acceleration', 0 );
      fSpeed         := Reader.ReadSingle( 'Speed', 0 );
      fX             := Reader.ReadSingle( 'x', 0 );
      fY             := Reader.ReadSingle( 'y', 0 );
      fDirection     := TCarDirection(Reader.ReadInteger( 'Direction', 0 ));
      fNextDirection := TCarDirection(Reader.ReadInteger( 'NextDirection', 0 ));
      fVisualStage   := Reader.ReadInteger( 'VisualStage', 0 );
      Reader.ReadObject( 'Train',   fTrain,   nil );
      Reader.ReadObject( 'PrevCar', fPrevCar, nil );
      Reader.ReadObject( 'NextCar', fNextCar, nil );
      Reader.ReadObject( 'CurrSeg', fCurrSeg, nil );
      fCheckpointPassed := Reader.ReadBoolean( 'CheckpointPassed', false );
      fBreaking := Reader.ReadBoolean( 'Breaking', false );
    end;

  procedure TServerCar.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteInteger( 'MetaCarId', MetaCar.Id );
      Writer.WriteSingle( 'Acceleration', fAcceleration );
      Writer.WriteSingle( 'Speed', fSpeed );
      Writer.WriteSingle( 'x', fX );
      Writer.WriteSingle( 'y', fY );
      Writer.WriteInteger( 'Direction', integer(fDirection) );
      Writer.WriteInteger( 'NextDirection', integer(fNextDirection) );
      Writer.WriteInteger( 'VisualStage', fVisualStage );
      Writer.WriteObjectRef( 'Train', fTrain );
      Writer.WriteObjectRef( 'PrevCar', fPrevCar );
      Writer.WriteObjectRef( 'NextCar', fNextCar );
      Writer.WriteObjectRef( 'CurrSeg', fCurrSeg );
      Writer.WriteBoolean( 'CheckpointPassed', fCheckpointPassed );
      Writer.WriteBoolean( 'Breaking', fBreaking );
    end;

  procedure TServerCar.MsgAnswerActorInViewport( var Msg : TViewportAwarenessInfo );
    begin
      with Msg.Viewport do
        Msg.InViewport := (x >= x1 - Margin) and (y >= y1 - Margin) and (x <= x2 + Margin) and (y <= y2 + Margin);
    end;

  procedure TServerCar.Synchronize;
    var
      Event    : TDistributedEvent;
      SyncInfo : TSyncInfo;
    begin
      SyncInfo.x := x;
      SyncInfo.y := y;
      SyncInfo.accel := Acceleration;
      SyncInfo.speed := Speed;
      SyncInfo.angle := Angle;
      Event := TDistributedEvent.Create( evnSyncInfo, dmBroadCast, SyncInfo, sizeof(SyncInfo) );
      AutomationEngine.ThrowEvent( Event );
      LogThis( Train.Name + ': Car ' + IntToStr(Train.Cars.IndexOf( self )) + ' Synchronizing angle: ' + IntToStr(round(180*Angle/pi)) + ' and position (x: ' + FloatToStr(x) + ', y: ' + FloatToStr(y) + ')' );
    end;
    

  // TMetaCargoCar

  constructor TMetaCargoCar.Create( anId             : TActorKind;
                                    aName            : string;
                                    aDesc            : string;
                                    aPrice           : Currency;
                                    aWeight          : TCarWeight;
                                    aOperationCost   : Currency;
                                    aBaseVisualClass : TCarVisualClass;
                                    aCargoType       : string;
                                    aCapacity        : TFluidValue;
                                    aCarClass        : CCar );
    begin
      inherited Create( anId, aName, aDesc, aPrice, aWeight, aOperationCost, aBaseVisualClass, aCarClass );
      fCargoType := TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, aCargoType]);
      fCapacity  := aCapacity;
    end;


  // TCargoCar

  procedure TCargoCar.UpdateTrainParameters;
    begin
      inherited;
      Train.Weight := Train.Weight + round(TMetaCargoCar(MetaCar).CargoType.Weight*(fLoadPerc/100)*TMetaCargoCar(MetaCar).Capacity);
    end;

  procedure TCargoCar.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      fLoadPerc := Reader.ReadByte( 'LoadPerc', 0 );
    end;

  procedure TCargoCar.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteByte( 'LoadPerc', fLoadPerc );
    end;


  // TMetaLocomotive

  constructor TMetaLocomotive.Create( anId             : TActorKind;
                                      aName            : string;
                                      aDesc            : string;
                                      aPrice           : Currency;
                                      aWeight          : TCarWeight;
                                      aOperationCost   : Currency;
                                      aBaseVisualClass : TCarVisualClass;
                                      aStrength        : TCarWeight;
                                      aMaxSpeed        : TCarSpeed;
                                      aCarClass        : CCar );
    begin
      inherited Create( anId, aName, aDesc, aPrice, aWeight, aOperationCost, aBaseVisualClass, aCarClass );
      fStrength := aStrength;
      fMaxSpeed := aMaxSpeed;
    end;


  // TLocomotive

  procedure TLocomotive.UpdateTrainParameters;
    begin
      inherited;
      Train.Strength := Train.Strength + TMetaLocomotive(MetaCar).Strength;
      Train.MaxSpeed := Train.MaxSpeed + TMetaLocomotive(MetaCar).MaxSpeed;
    end;


  // TPathSeg

  constructor TPathSeg.Create( aTrain : TTrain; aDir : TSegmentDirection; aSegment : TRailroadSegment; aLength : integer );
    begin
      inherited Create;
      fTrain   := aTrain;
      fDir     := aDir;
      fSegment := aSegment;
      fLength  := aLength;
    end;

  function TPathSeg.GetStartNode : TNode;
    begin
      case fDir of
        segNorth, segWest :
          result := fSegment.NodeB;
        else
          result := fSegment.NodeA;
      end;
    end;

  function TPathSeg.GetEndNode : TNode;
    begin
      case fDir of
        segNorth, segWest :
          result := fSegment.NodeA;
        else
          result := fSegment.NodeB;
      end;
    end;

  procedure TPathSeg.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      Reader.ReadObject( 'Train', fTrain, nil );
      fDir := TSegmentDirection(Reader.ReadInteger( 'Dir', 0 ));
      Reader.ReadObject( 'Segment', fSegment, nil );
      fLength  := Reader.ReadInteger( 'Length', 0 );
    end;

  procedure TPathSeg.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteObjectRef( 'Train', fTrain );
      Writer.WriteInteger( 'Dir', integer(fDir) );
      Writer.WriteObjectRef( 'Segment', fSegment );
      Writer.WriteInteger( 'Length', fLength );
    end;


  // TGeoPathSeg

  constructor TGeoPathSeg.Create( aX, aY : single; aPathSeg : TPathSeg; aPath : TCollection );
    begin
      inherited Create;
      fX       := aX;
      fY       := aY;
      fPathSeg := aPathSeg;
      fPath    := aPath;
      fIndex   := fPath.Count;
      fPath.Insert( self );
    end;

  function TGeoPathSeg.GetX2 : single;
    begin
      if Next <> nil
        then result := Next.x1
        else result := x1;
    end;

  function TGeoPathSeg.GetY2 : single;
    begin
      if Next <> nil
        then result := Next.y1
        else result := y1
    end;

  function TGeoPathSeg.GetAngle : single;
    begin
      if (x1 <> x2) and (y1 <> y2)
        then
          begin
            result := arctan(abs(y2 - y1)/abs(x2 - x1));
            if x2 < x1
              then
                if y2 < y1
                  then result := pi + result
                  else result := pi - result
              else
                if y2 < y1
                  then result := -result;
            if result > pi
              then result := result - 2*pi
          end
        else
          if (x1 = x2)
            then
              if y2 > y1
                then result := pi/2
                else result := -pi/2
            else
              if x2 > x1
                then result := 0
                else result := pi
    end;

  function TGeoPathSeg.GetNext : TGeoPathSeg;
    begin
      if fIndex < pred(fPath.Count)
        then result := TGeoPathSeg(fPath[fIndex + 1])
        else result := self;
    end;

  function TGeoPathSeg.GetLength : single;
    begin
      result := sqrt(sqr(x1 - x2) + sqr(y1 - y2));
    end;

  function TGeoPathSeg.CarInSeg( Car : TServerCar; Tol : single ) : boolean;
    var
      ang  : integer;
      xTol : single;
      yTol : single;
    begin
      xTol := abs(round(10000*Tol*cos(Angle))/10000);
      yTol := abs(round(10000*Tol*sin(Angle))/10000);
      ang := round(180*Angle/pi);
      result :=
        (ang >= 0)  and (ang < 90)    and (Car.x >= x1 + xTol) and (Car.x <= x2 - xTol) and (Car.y >= y1 + Tol) and (Car.y <= y2 - yTol) or
        (ang >= 90) and (ang <= 180)  and (Car.x >= x2 + xTol) and (Car.x <= x1 - xTol) and (Car.y >= y1 + Tol) and (Car.y <= y2 - yTol) or
        (ang < 0)   and (ang >= -90)  and (Car.x >= x2 + xTol) and (Car.x <= x1 - xTol) and (Car.y >= y2 + Tol) and (Car.y <= y1 - yTol) or
        (ang < -90) and (ang >= -180) and (Car.x >= x1 + xTol) and (Car.x <= x2 - xTol) and (Car.y >= y2 + Tol) and (Car.y <= y1 - yTol);
      {
      result :=
        (ang >= 0)  and (ang <= 90)  and (Car.x >= x1) and (Car.x <= x2) and (Car.y >= y1) and (Car.y <= y2) or
        (ang > 90)  and (ang <= 180) and (Car.x >= x2) and (Car.x <= x1) and (Car.y >= y1) and (Car.y <= y2) or
        (ang < 0)   and (ang >= -90) and (Car.x >= x1) and (Car.x <= x2) and (Car.y >= y2) and (Car.y <= y1) or
        (ang < -90) and (ang > -180) and (Car.x >= x2) and (Car.x <= x1) and (Car.y >= y2) and (Car.y <= y1);
      }
    end;

  procedure TGeoPathSeg.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      fX := Reader.ReadSingle( 'x', 0 );
      fY := Reader.ReadSingle( 'y', 0 );
      Reader.ReadObject( 'PathSeg', fPathSeg, nil );
    end;

  procedure TGeoPathSeg.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteSingle( 'x', fX );
      Writer.WriteSingle( 'y', fY );
      Writer.WriteObjectRef( 'PathSeg', fPathSeg );
    end;


  // TTrain

  constructor TTrain.Create( aCompany : TRailRoadCompany );
    begin
      inherited Create;
      fCompany := aCompany;
      fRoute   := TLockableCollection.Create( 0, rkBelonguer );
      fCars    := TLockableCollection.Create( 0, rkBelonguer );
      fAltX    := -1;
      fAltY    := -1;
    end;

  destructor TTrain.Destroy;
    begin
      fRoute.Free;
      fCars.Free;
      fPath.Free;
      fExtPath.Free;
      fGeoPath.Free;
      inherited;
    end;

  function TTrain.GetNextX : integer;
    begin
      if (AltX = -1) and (fRoute.Count > 0)
        then result := TRoutePoint(fRoute[NextPoint]).fStation.xOrigin
        else result := AltX
    end;

  function TTrain.GetNextY : integer;
    begin
      if (AltY = -1) and (fRoute.Count > 0)
        then result := TRoutePoint(fRoute[NextPoint]).fStation.yOrigin
        else result := AltY
    end;

  procedure TTrain.Act;
    const
      MaxLoadTurns     = 5;
      SlowDownSpeed    = 0.3;
      StopAcceleration = -0.2;

    procedure UpdateTrainParameters;
      var
        i        : integer;
        Breaking : boolean;
      begin
        fWeight   := 0;
        fStrength := 0;
        Breaking  := false;
        fCars.Lock;
        try
          for i := 0 to pred(fCars.Count) do
            try
              TServerCar(fCars[i]).UpdateTrainParameters;
              if TServerCar(fCars[i]).fBreaking
                then Breaking := true;
            except
            end;
        finally
          fCars.Unlock;
        end;
        if fBreaksOn and not Breaking
          then
            begin
              fAcceleration := fPrevAccel;
              fBreaksOn := false;
              LogThis( Name + ': Breaks released. ' );
            end;
      end;

    function PathAvailability : TPathAvailability;
      var
        Point  : TPathSeg;
        // i      : integer;
      begin
        Point := TPathSeg(fPath[fPathIdx]);
        if not Point.fSegment.IsFree( Point.fDir, self )
          then
            begin
              result := pthNearBlocked;
              if Point.fSegment.fReservedBy <> nil
                then LogThis( Name + ': Blocked by reservation from ' + Point.fSegment.fReservedBy.Name )
                else LogThis( Name + ': Blocked by cars of ' + TServerCar(Point.fSegment.Cars[0]).Train.Name )
            end
          else result := pthFree;
            {
            begin
              i := 0;
              while (i < Path.Count) and TPathSeg(Path[i]).IsFree do
                inc( i );
              if i < Path.Count
                then result := pthFarBlocked
                else result := pthFree;
            end;
            }
      end;

    function FindPath : Collection.TCollection;

      function AreOpposite( Dir, SrcDir : TSegmentDirection ) : boolean;
        begin
          case SrcDir of
            segNorth : result := Dir = segSouth;
            segEast  : result := Dir = segWest;
            segSouth : result := Dir = segNorth;
            else       result := Dir = segEast;
          end;
        end;

      function FindPathToSeg( DestSeg : TRailroadSegment; SrcDir : TSegmentDirection; SrcSeg : TRailroadSegment; level : integer; Path : Collection.TCollection ) : boolean;
        var
          Dir       : TSegmentDirection;
          Node      : TNode;
          PathPoint : TPathSeg;
        begin
          if DestSeg <> SrcSeg
            then
              begin
                if level > 0
                  then SrcSeg.fMarked := true;
                try
                  case SrcDir of
                    segNorth, segWest :
                      Node := SrcSeg.NodeA;
                    else
                      Node := SrcSeg.NodeB;
                  end;
                  Dir := low(Dir);
                  repeat
                    result :=
                      not AreOpposite( Dir, SrcDir ) and
                      (Node.Segments[Dir] <> nil) and
                      not TRailroadSegment(Node.Segments[Dir]).fMarked and
                      ((level > 0) or TRailroadSegment(Node.Segments[Dir]).IsFree( Dir, self )) and
                      FindPathToSeg( DestSeg, Dir, TRailroadSegment(Node.Segments[Dir]), succ(level), Path );
                    if not result
                      then
                        if Dir < high(Dir)
                          then inc( Dir )
                          else Dir := low(Dir);
                  until result or (Dir = low(Dir));
                  if result
                    then
                      begin
                        PathPoint := TPathSeg.Create( self, SrcDir, SrcSeg, 0 );
                        PathPoint.fLength := SrcSeg.Length;
                        Path.AtInsert( 0, PathPoint );
                      end;
                finally
                  if level > 0
                    then SrcSeg.fMarked := false;
                end;
              end
            else
              begin
                PathPoint := TPathSeg.Create( self, SrcDir, DestSeg, 0 );
                if DestSeg.IsHorizontal
                  then
                    if SrcDir = segEast
                      then PathPoint.fLength := NextX - DestSeg.NodeA.x
                      else PathPoint.fLength := DestSeg.NodeB.x - NextX
                  else
                    if SrcDir = segSouth
                      then PathPoint.fLength := NextY - DestSeg.NodeA.y
                      else PathPoint.fLength := DestSeg.NodeB.y - NextY;
                Path.AtInsert( 0, PathPoint );
                result := true;
              end;
        end;

      var
        Segments : Collection.TCollection;
        Leader   : TServerCar;
        i        : integer;
      begin
        if (NextX <> -1) and (NextY <> -1)
          then
            begin
              Leader := TServerCar(fCars[0]);
              Segments := Collection.TCollection.Create( 0, rkUse );
              Leader.fCurrSeg.Circuit.Map.FindSegsInArea( NextX, NextY - RailroadTolerance, NextX + 1, NextY + RailroadTolerance, Segments );
              Leader.fCurrSeg.Circuit.Map.FindSegsInArea( NextX - RailroadTolerance, NextY, NextX + RailroadTolerance, NextY + 1, Segments );
              for i := pred(Segments.Count) downto 0 do
                with TRailroadSegment(Segments[i]) do
                  if (Circuit <> Leader.fCurrSeg.Circuit) or (fCars.Count > 0) and (Segments.Count > 1) and not (TRailroadSegment(Segments[i]) = Leader.fCurrSeg)
                    then Segments.AtDelete( i );
              if Segments.Count > 0
                then
                  begin
                    result := Collection.TCollection.Create( 0, rkBelonguer );
                    i := 0;
                    while (i < Segments.Count) and not FindPathToSeg( TRailroadSegment(Segments[i]), CarDirToSegmentDir(Leader.Direction), Leader.fCurrSeg, 0, result ) do
                      begin
                        inc( i );
                        result.DeleteAll;
                      end;
                    if i < Segments.Count
                      then fArriving := result.Count = 1
                      else
                        begin
                          result.Free;
                          result := nil;
                        end;
                  end
                else result := nil;
              Segments.Free;
            end
          else result := nil;
      end;

    function SegIncludedInPath( Seg : TRailroadSegment; Path, ExtPath : TCollection ) : boolean;
      var
        i, j : integer;
      begin
        i := 0;
        while (i < Path.Count) and (TPathSeg(Path[i]).fSegment <> Seg) do
          inc( i );
        j := 0;
        while (j < ExtPath.Count) and (TPathSeg(ExtPath[j]).fSegment <> Seg) do
          inc( j );
        result := (i < Path.Count) and (j = ExtPath.Count);
      end;

    function GetExtendedPath( Path : TCollection ) : TCollection;
      var
        Seg : TPathSeg;
        i   : integer;
      begin
        result := TCollection.Create( 0, rkBelonguer );
        for i := pred(Cars.Count) downto 0 do
          if not SegIncludedInPath( TServerCar(Cars[i]).CurrSeg, Path, result )
            then
              begin
                Seg := TPathSeg.Create( self, CarDirToSegmentDir(TServerCar(Cars[i]).Direction), TServerCar(Cars[i]).CurrSeg, TServerCar(Cars[i]).CurrSeg.Length );
                result.Insert( Seg );
              end;
      end;

    function RenderGeoPath( Path, ExtPath : TCollection ) : TCollection;
      type
        TVector =
          record
            dx, dy : single;
          end;

      function GetVectorOfDir( D : TSegmentDirection ) : TVector;
        begin
          case D of
            segNorth :
              begin
                result.dx := 0;
                result.dy := -1;
              end;
            segEast :
              begin
                result.dx := 1;
                result.dy := 0;
              end;
            segSouth :
              begin
                result.dx := 0;
                result.dy := 1;
              end;
            segWest :
              begin
                result.dx := -1;
                result.dy := 0;
              end;
          end;
        end;

      procedure FindCorner( xo, yo : integer; D1, D2 : TSegmentDirection; out x, y : single );
        const
          CurveAttenuation = 0.2;
        var
          v1, v2 : TVector;
        begin
          v1 := GetVectorOfDir( D1 );
          v2 := GetVectorOfDir( D2 );
          v1.dx := CurveAttenuation*v1.dx;
          v1.dy := CurveAttenuation*v1.dy;
          v2.dx := CurveAttenuation*v2.dx;
          v2.dy := CurveAttenuation*v2.dy;
          x := xo + v2.dx - v1.dx;
          y := yo + v2.dy - v1.dy;
        end;

      procedure FindCornerStart( xo, yo : integer; Dir : TSegmentDirection; out x, y : single );
        var
          v : TVector;
        begin
          v := GetVectorOfDir( Dir );
          x := xo - v.dx;
          y := yo - v.dy;
        end;

      procedure FindCornerEnd( xo, yo : integer; Dir : TSegmentDirection; out x, y : single );
        var
          v : TVector;
        begin
          v := GetVectorOfDir( Dir );
          x := xo + v.dx;
          y := yo + v.dy;
        end;

      const
        PathShift = 0;
      var
        i        : integer;
        Seg      : TPathSeg;
        LastSeg  : TPathSeg;
        x, y     : single;
        FullPath : TCollection;
      begin
        result   := TCollection.Create( 0, rkBelonguer );
        FullPath := TCollection.Create( 0, rkUse );
        FullPath.InsertColl( fExtPath );
        FullPath.InsertColl( fPath );
        LastSeg := TPathSeg(FullPath[0]);
        i       := 0;
        repeat
          Seg := TPathSeg(FullPath[i]);
          if Seg.fDir <> LastSeg.fDir
            then
              begin
                FindCornerStart( LastSeg.EndNode.x, LastSeg.EndNode.y, LastSeg.fDir, x, y );
                TGeoPathSeg.Create( x + PathShift, y + PathShift, LastSeg, result );
                FindCorner( Seg.StartNode.x, Seg.StartNode.y, LastSeg.fDir, Seg.fDir, x, y );
                TGeoPathSeg.Create( x + PathShift, y + PathShift, LastSeg, result );
                FindCornerEnd( Seg.StartNode.x, Seg.StartNode.y, Seg.fDir, x, y );
                TGeoPathSeg.Create( x + PathShift, y + PathShift, Seg, result );
              end
            else
              begin
                x := Seg.StartNode.x;
                y := Seg.StartNode.y;
                TGeoPathSeg.Create( x + PathShift, y + PathShift, Seg, result );
              end;
          LastSeg := Seg;
          inc( i );
        until i = Path.Count;
        Seg := TPathSeg(Path[i - 1]);
        TGeoPathSeg.Create( Seg.EndNode.x + PathShift, Seg.EndNode.y + PathShift, Seg, result );
        FullPath.Free;
      end;

    procedure AssignGeoPath( Path : TCollection );
      var
        i, j : integer;
        Seg  : TGeoPathSeg;
        Car  : TServerCar;
      begin
        for i := 0 to pred(Path.Count) do
          begin
            Seg := TGeoPathSeg(Path[i]);
            for j := 0 to pred(Seg.fPathSeg.fSegment.Cars.Count) do
              begin
                Car := TServerCar(Seg.fPathSeg.fSegment.Cars[j]);
                if (Car.fCurrGeoSeg = nil) and Seg.CarInSeg( Car, 0 )
                  then
                    begin
                      Car.fCurrGeoSeg := Seg;
                      Car.Angle := Seg.Angle;
                    end;
              end;
          end;
      end;

    procedure CheckPath;
      begin
        if not fArriving and ((fPath = nil) or (PathAvailability = pthNearBlocked))
          then
            begin
              fPath.Free;
              fPathIdx := 0;
              fPath := FindPath;
              if fPath <> nil
                then
                  begin
                    fExtPath.Free;
                    fExtPath := GetExtendedPath( fPath );
                    fGeoPath.Free;
                    fGeoPath := RenderGeoPath( fPath, fExtPath );
                    AssignGeoPath( fGeoPath );
                    fAcceleration := 0.2; // >> Temporary!
                  end
                else
                  begin
                    fExtPath.Free;
                    fGeoPath.Free;
                    fExtPath := nil;
                    fGeoPath := nil;
                  end;
            end;
      end;

    procedure UpdateDirections;
      var
        Leader    : TServerCar;
        PathPoint : TPathSeg;
      begin
        if not fArriving
          then
            begin
              Leader := TServerCar(fCars[0]);
              if Leader.CarAtCheckPoint
                then
                  begin
                    LogThis( Name + ': Leader at checkpoint.' );
                    inc( fPathIdx );
                    PathPoint := TPathSeg(fPath[fPathIdx]);
                    PathPoint.fSegment.fReservedBy := self;
                    Leader.NextDirection := SegmentDirToCarDir( PathPoint.fDir );
                    fArriving := fPathIdx = pred(fPath.Count);
                    Leader.fCheckpointPassed := true;
                  end;
            end;
      end;

    function SlowDown( var Acceleration : TCarAcceleration ) : boolean;
      const
        SlowDownDist = 10;
      var
        Length     : integer;
        Leader     : TServerCar;
        i          : integer;
        LastSeg    : TSegment;
      begin
        Length := 0;
        for i := fPathIdx to pred(fPath.Count) do
          inc( Length, TPathSeg(fPath[i]).fLength );
        Leader := TServerCar(Cars[0]);
        case Leader.Direction of
          cdirN : dec( Length, Leader.fCurrSeg.NodeB.y - trunc(Leader.Y) );
          cdirE : dec( Length, trunc(Leader.X) - Leader.fCurrSeg.NodeA.x );
          cdirS : dec( Length, trunc(Leader.Y) - Leader.fCurrSeg.NodeA.y );
          cdirW : dec( Length, Leader.fCurrSeg.NodeB.x - trunc(Leader.X) );
        end;
        if Length > 0
          then
            if Length < SlowDownDist
              then
                begin
                  LogThis( Name + ': Slowing down.' );
                  LastSeg     := TPathSeg(fPath[pred(fPath.Count)]).fSegment;
                  fExtraSpace := LastSeg.Length - TPathSeg(fPath[pred(fPath.Count)]).fLength;
                  fExtraSpace := min( Cars.Count div 2, fExtraSpace );
                  //inc( Length, fExtraSpace );
                  Acceleration := -0.003; //-sqr(Leader.Speed)/(2*Length);
                  result       := true;
                end
              else result := false
          else
            begin
              Acceleration := -Leader.Speed;
              result       := true;
            end;
      end;

    function DestinationReached : boolean;
      var
        Leader : TServerCar;
      begin
        Leader := TServerCar(Cars[0]);
        result :=
          fArriving and
          ((Leader.Direction = cdirN) and (Leader.y <= NextY - fExtraSpace) or
           (Leader.Direction = cdirS) and (Leader.y >= NextY + fExtraSpace) or
           (Leader.Direction = cdirW) and (Leader.x <= NextX - fExtraSpace) or
           (Leader.Direction = cdirE) and (Leader.x >= NextX + fExtraSpace));
      end;

    procedure StopCars;
      var
        i : integer;
      begin
        for i := 0 to pred(Cars.Count) do
          TCar(Cars[i]).Speed := 0; 
      end;
      
    procedure SetTrainParameters;
      var
        i : integer;
      begin
        fCars.Lock;
        try
          for i := 0 to pred(fCars.Count) do
            TServerCar(fCars[i]).Acceleration := fAcceleration;
        finally
          fCars.Unlock;
        end;
      end;

    procedure ClearPathInfo;
      var
        i : integer;
      begin
        fPath.Free;
        fPath := nil;
        for i := 0 to pred(Cars.Count) do
          TServerCar(Cars[i]).fCurrGeoSeg := nil;
        fGeoPath.Free;
        fGeoPath := nil;
      end;

    var
      Leader : TServerCar;
    begin
      UpdateTrainParameters;
      if not (State in StillStates)
        then
          begin
            CheckPath;
            if fPath <> nil
              then UpdateDirections
              else State := stBlocked;
          end;
      case State of
        stStopped :
          begin
            fArriving := false;
            if (fAltX = -1) or (fAltY = -1)
              then fLoadTurns := MaxLoadTurns
              else fLoadTurns := -1;
            fAltX := -1;
            fAltY := -1;
            State := stLoading;
            ClearPathInfo;
            LogThis( Name + ': Train stopped.' );
          end;
        stLoading :
          if fLoadTurns > 0
            then
              begin
                dec( fLoadTurns );
                if fLoadTurns = 0
                  then
                    begin
                      if fNextPoint < pred(fRoute.Count)
                        then inc( fNextPoint )
                        else fNextPoint := 0;
                      State := stLeaving;
                      LogThis( Name + ': Train leaving.' );
                    end;
              end;
        stLeaving :
          if not SlowDown( fAcceleration )
            then
              begin
                Leader := TServerCar(fCars[0]);
                if Leader.Speed >= MaxSpeed
                  then
                    begin
                      fAcceleration := 0;
                      State := stRunning;
                    end;
              end
            else State := stStopping;
        stRunning :
          if SlowDown( fAcceleration )
            then State := stStopping;
        stStopping :
          begin
            inc( fLoadTurns );
            Leader := TServerCar(fCars[0]);
            if DestinationReached or (Leader.Speed <= 0)
              then
                begin
                  fAcceleration := 0;
                  StopCars;
                  State := stStopped;
                end
              else
                if Leader.Speed <= ArrivalSpeed
                  then fAcceleration := 0;
          end;
        {
        stStopping :
          begin
            Leader := TServerCar(fCars[0]);
            if Leader.Speed <= 0
              then
                begin
                  fAcceleration := 0;
                  StopCars;
                  State := stStopped;
                end;
          end;
        }
        stBlocked :
          if fPath <> nil
            then
              begin
                fAcceleration := 0.2; // >> Temporary!
                State := stLeaving;
              end
            else
              begin
                fAcceleration := 0; // >> Temporary!
                StopCars;
              end;
      end;
      SetTrainParameters;
    end;

  procedure TTrain.ForceFindNewPath;
    begin
      fPath.Free;
      fPath := nil;
    end;

  {procedure TTrain.PushBreaks;
    var
      Leader : TServerCar;
      i : integer;
    begin
      Leader := TServerCar(Cars[0]);
      if Leader.Speed > 0
        then
          begin
            LogThis( Name + ': Breaks on. ' );
            fPrevAccel := fAcceleration;
            fBreaksOn  := true;
            fCars.Lock;
            try
              for i := 0 to pred(fCars.Count) do
                TServerCar(fCars[i]).Acceleration := 0;
            finally
              fCars.Unlock;
            end;
          end;
    end;}

  procedure TTrain.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      fName := Reader.ReadString( 'Name', '' );
      Reader.ReadObject( 'Company', fCompany, nil );
      Reader.ReadObject( 'Route', fRoute, nil );
      Reader.ReadObject( 'Cars', fCars, nil );
      Reader.ReadObject( 'Path', fPath, nil );
      Reader.ReadObject( 'ExtPath', fExtPath, nil );
      Reader.ReadObject( 'GeoPath', fGeoPath, nil );
      fWeight       := Reader.ReadInteger( 'Weight', 0 );
      fAcceleration := Reader.ReadSingle( 'Acceleration', 0 );
      fStrength     := Reader.ReadInteger( 'Strength', 0 );
      fMaxSpeed     := Reader.ReadSingle( 'MaxSpeed', 0 );
      fState        := TTrainState(Reader.ReadInteger( 'State', 0 ));
      fPathIdx      := Reader.ReadInteger( 'PathIdx', 0 );
      fArriving     := Reader.ReadBoolean( 'Arriving', false );
      fNextPoint    := Reader.ReadInteger( 'NextPoint', 0 );
      fAltX         := Reader.ReadInteger( 'AltX', -1 );
      fAltY         := Reader.ReadInteger( 'AltY', -1 );
      fLoadTurns    := Reader.ReadInteger( 'LoadTurns', 0 );
      fExtraSpace   := Reader.ReadInteger( 'ExtraSpace', 0 );
      fPrevAccel    := Reader.ReadSingle( 'PrevAccel', 0 );
      fBreaksOn     := Reader.ReadBoolean( 'BreaksOn', false )
    end;

  procedure TTrain.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteString( 'Name', fName );
      Writer.WriteObjectRef( 'Company', fCompany );
      Writer.WriteLooseObject( 'Route', fRoute );
      Writer.WriteLooseObject( 'Cars', fCars );
      Writer.WriteLooseObject( 'Path', fPath );
      Writer.WriteLooseObject( 'ExtPath', fExtPath );
      Writer.WriteLooseObject( 'GeoPath', fGeoPath );
      Writer.WriteInteger( 'Weight', fWeight );
      Writer.WriteSingle( 'Acceleration', fAcceleration );
      Writer.WriteInteger( 'Strength', fStrength );
      Writer.WriteSingle( 'MaxSpeed', fMaxSpeed );
      Writer.WriteInteger( 'State', integer(fState) );
      Writer.WriteInteger( 'PathIdx', fPathIdx );
      Writer.WriteBoolean( 'Arriving', fArriving );
      Writer.WriteInteger( 'NextPoint', fNextPoint );
      Writer.WriteInteger( 'AltX', fAltX );
      Writer.WriteInteger( 'AltY', fAltY );
      Writer.WriteInteger( 'LoadTurns', fLoadTurns );
      Writer.WriteInteger( 'ExtraSpace', fExtraSpace );
      Writer.WriteSingle( 'PrevAccel', fPrevAccel );
      Writer.WriteBoolean( 'BreaksOn', fBreaksOn )
    end;


  // TRoutePoint

  procedure TRoutePoint.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      Reader.ReadObject( 'Station', fStation, nil );
      fWhenToLeave := TWhenToLeave(Reader.ReadInteger( 'WhenToLeave', 0 ));
    end;

  procedure TRoutePoint.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteObjectRef( 'Station', fStation );
      Writer.WriteInteger( 'WhenToLeave', integer(fWhenToLeave) );
    end;


  // TRailRoadCompany

  constructor TRailRoadCompany.Create( anId : TCompanyId );
    begin
      inherited;
      fTrains := TLockableCollection.Create( 0, rkBelonguer );
    end;

  destructor TRailRoadCompany.Destroy;
    begin
      fTrains.Free;
      inherited;
    end;

  procedure TRailRoadCompany.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      Reader.ReadObject( 'Trains', fTrains, nil );
      fLastTrainId := Reader.ReadInteger( 'LastTrainId', 0 );
    end;

  procedure TRailRoadCompany.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteObject( 'Trains', fTrains );
      Writer.WriteInteger( 'LastTrainId', fLastTrainId );
    end;


  // Backup Agent

  type
    TServerCarBackupAgent =
      class(TBackupAgent)
        protected
          class procedure Write(Stream : IBackupWriter; Obj : TObject); override;
          class procedure Read (Stream : IBackupReader; Obj : TObject); override;
      end;

    class procedure TServerCarBackupAgent.Write(Stream : IBackupWriter; Obj : TObject);
      begin
        try
          TServerCar(Obj).StoreToBackup(Stream);
        except
        end;
      end;

    class procedure TServerCarBackupAgent.Read(Stream : IBackupReader; Obj : TObject);
      begin
        try
          TServerCar(Obj).LoadFromBackup(Stream);
        except
        end;
      end;


  // Utils

  function SegmentDirToCarDir( SegmentDir : TSegmentDirection ) : TCarDirection;
    begin
      case SegmentDir of
        segNorth : result := cdirN;
        segEast  : result := cdirE;
        segSouth : result := cdirS;
        else       result := cdirW;
      end;
    end;

  function CarDirToSegmentDir( CarDir : TCarDirection ) : TSegmentDirection;
    begin
      case CarDir of
        cdirN : result := segNorth;
        cdirE : result := segEast;
        cdirS : result := segSouth;
        else    result := segWest;
      end;
    end;


  // RegisterBackup

  procedure RegisterBackup;
    begin
      TServerCarBackupAgent.Register([TServerCar]);
      RegisterClass( TRailRoadCircuit );
      RegisterClass( TRailRoadSegment );
      RegisterClass( TRailRoadNode );
      RegisterClass( TCargoCar );
      RegisterClass( TLocomotive );
      RegisterClass( TTrain );
      RegisterClass( TRoutePoint );
      RegisterClass( TRailRoadCompany );
      RegisterClass( TPathSeg );
      RegisterClass( TGeoPathSeg );
    end;


end.



