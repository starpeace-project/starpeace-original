unit Train;

interface

  uses
    Classes, Windows, Collection, StateEngine, Automaton, ActorPool, ActorTypes, DistributedStates;

  const
    tidClassFamily_TrainCars = 'TrainCars';

  const
    carstNone    = 0;
    carstRunning = 1;
    carstLoading = 2;

  type
    TCarVisualClass  = integer;
    TWhenToLeave     = (wtlASAP, wtlHalfFull, wtlFull);
    TCarDirection    = (cdirN, cdirE, cdirS, cdirW);
    TCarWeight       = integer;
    TCarSpeed        = single;
    TCarAcceleration = single;

  type
    TMetaCar = class;
    TCar     = class;
    CMetaCar = class of TMetaCar;
    CCar     = class of TCar;

    TMetaCar =
      class
        public
          constructor Create( anId             : TActorKind;
                              aName            : string;
                              aDesc            : string;
                              aPrice           : Currency;
                              aWeight          : TCarWeight;
                              aOperationCost   : Currency;
                              aBaseVisualClass : TCarVisualClass;
                              aCarClass        : CCar );
        private
          fId              : TActorKind;
          fName            : string;
          fDesc            : string;
          fPrice           : Currency;
          fWeight          : TCarWeight;
          fOperationCost   : Currency;
          fBaseVisualClass : TCarVisualClass;
          fCarClass        : CCar;
        public
          property Id              : TActorKind      read fId;
          property Name            : string          read fName;
          property Desc            : string          read fDesc;
          property Price           : Currency        read fPrice;
          property Weight          : TCarWeight      read fWeight;
          property OperationCost   : Currency        read fOperationCost;
          property BaseVisualClass : TCarVisualClass read fBaseVisualClass;
        public
          function Instantiate : TCar;
        public
          procedure Register;
      end;

    TCar =
      class( TInterfacedObject, IServerAutomatable, IClientAutomatable )
        protected
          constructor Create( aMetaCar : TMetaCar ); virtual;
        protected
          fMetaCar       : TMetaCar;
          fAcceleration  : TCarAcceleration;
          fSpeed         : TCarSpeed;
          fAngle         : single;
          fX             : single;
          fY             : single;
          fDirection     : TCarDirection;
          fNextDirection : TCarDirection;
          fVisualStage   : integer;
        protected
          procedure SetAcceleration ( anAceleration  : TCarAcceleration ); virtual;
          procedure SetSpeed        ( aSpeed         : TCarSpeed );        virtual;
          procedure SetAngle        ( anAngle        : single );           virtual;
          procedure SetVisualStage  ( aVisualStage   : integer );          virtual;
          procedure SetDirection    ( aDirection : TCarDirection );        virtual;
          procedure SetNextDirection( aNextDirection : TCarDirection );    virtual;
        public
          property MetaCar       : TMetaCar         read fMetaCar       write fMetaCar;
          property Acceleration  : TCarAcceleration read fAcceleration  write SetAcceleration;
          property Speed         : TCarSpeed        read fSpeed         write SetSpeed;
          property Angle         : single           read fAngle         write SetAngle;
          property X             : single           read fX             write fX; 
          property Y             : single           read fY             write fY;
          property Direction     : TCarDirection    read fDirection     write SetDirection;
          property NextDirection : TCarDirection    read fNextDirection write SetNextDirection;
        protected
          property VisualStage : integer read fVisualStage write SetVisualStage;
        private
          function GetVisualClass : integer;
        public
          property VisualClass : TCarVisualClass read GetVisualClass;
        protected
        // IAutomaton
        protected
          procedure Act( State : TStateId; Mode : TActMode );             virtual;
          function  HandleEvent( anEvent : TDistributedEvent ) : boolean; virtual;
          procedure SetAutomationEngine( anEngine : IAutomationEngine );  virtual;
          procedure SetActor( anActor : IServerActor );                   virtual;
          function  GetActor : IServerActor;                              virtual;
          procedure Load ( Stream : TStream );                            virtual;
          procedure Store( Stream : TStream );                            virtual;
          procedure Deleted;                                              virtual;
        private
          fAutomationEngine : IAutomationEngine;
          fActor            : IServerActor;
        public
          property AutomationEngine : IAutomationEngine read fAutomationEngine;
          property Actor            : IServerActor      read fActor;
      end;

      
  const
    MaxSpeed = 1;


  // Car events

  const
    evnAccelerationChange  = 1;
    evnSpeedChange         = 2;
    evnVisualStageChange   = 3;
    evnDirectionChange     = 4;
    evnNextDirectionChange = 5;
    evnAngleChange         = 6;
    evnSyncInfo            = 7;

  type
    TSyncInfo =
      record
        x, y  : single;
        accel : single;
        speed : single;
        angle : single;
      end;

implementation

  uses
    SysUtils, LogFile, MathUtils, ClassStorage;


  // TMetaCar

  constructor TMetaCar.Create( anId               : TActorKind;
                               aName, aDesc       : string;
                               aPrice             : Currency;
                               aWeight            : TCarWeight;
                               aOperationCost     : Currency;
                               aBaseVisualClass   : TCarVisualClass;
                               aCarClass          : CCar );
    begin
      fId              := anId;
      fName            := aName;
      fDesc            := aDesc;
      fPrice           := aPrice;
      fWeight          := aWeight;
      fOperationCost   := aOperationCost;
      fBaseVisualClass := aBaseVisualClass;
      fCarClass        := aCarClass;
    end;

  function TMetaCar.Instantiate : TCar;
    begin
      result := fCarClass.Create( self );
    end;

  procedure TMetaCar.Register;
    begin
      TheClassStorage.RegisterClass( tidClassFamily_TrainCars, IntToStr(Id), self );
    end;
    

  // TCar

  constructor TCar.Create( aMetaCar : TMetaCar );
    begin
      inherited Create;
      fMetaCar := aMetaCar;
    end;

  procedure TCar.SetAcceleration( anAceleration : TCarAcceleration );
    begin
      fAcceleration := anAceleration;
    end;

  procedure TCar.SetSpeed( aSpeed : TCarSpeed );
    begin
      fSpeed := aSpeed;
    end;

  procedure TCar.SetAngle( anAngle : single );
    begin
      fAngle := anAngle;
    end;
    
  procedure TCar.SetVisualStage( aVisualStage : integer );
    begin
      fVisualStage := aVisualStage;
    end;

  procedure TCar.SetDirection( aDirection : TCarDirection );
    begin
      fDirection := aDirection;
    end;

  procedure TCar.SetNextDirection( aNextDirection : TCarDirection );
    begin
      fNextDirection := aNextDirection;
    end;

  function TCar.GetVisualClass : integer;
    begin
      result := MetaCar.BaseVisualClass + VisualStage;
    end;

  procedure TCar.Act( State : TStateId; Mode : TActMode );
    begin
      if Mode = amdSynchronized
        then
          case State of
            carstRunning :
              begin
                fSpeed := realmax(0, realmin( MaxSpeed, fSpeed + fAcceleration ));
                fX := fX + fSpeed*cos( fAngle );
                fY := fY + fSpeed*sin( fAngle );
              end;
          end;
    end;

  function TCar.HandleEvent( anEvent : TDistributedEvent ) : boolean;
    begin
      result := true;
    end;

  procedure TCar.SetAutomationEngine( anEngine : IAutomationEngine );
    begin
      fAutomationEngine := anEngine;
    end;
    
  procedure TCar.SetActor( anActor : IServerActor );
    begin
      fActor := anActor;
    end;

  function TCar.GetActor : IServerActor;
    begin
      result := fActor;
    end;

  procedure TCar.Load( Stream : TStream );
    begin
      Stream.Read( fAcceleration, sizeof(fAcceleration) );
      Stream.Read( fSpeed, sizeof(fSpeed) );
      Stream.Read( fAngle, sizeof(fAngle) );
      Stream.Read( fX, sizeof(fX) );
      Stream.Read( fY, sizeof(fY) );
      Stream.Read( fDirection, sizeof(fDirection) );
      Stream.Read( fNextDirection, sizeof(fNextDirection) );
      Stream.Read( fVisualStage, sizeof(fVisualStage) );
    end;

  procedure TCar.Store( Stream : TStream );
    begin
      Stream.Write( fAcceleration, sizeof(fAcceleration) );
      Stream.Write( fSpeed, sizeof(fSpeed) );
      Stream.Write( fAngle, sizeof(fAngle) );
      Stream.Write( fX, sizeof(fX) );
      Stream.Write( fY, sizeof(fY) );
      Stream.Write( fDirection, sizeof(fDirection) );
      Stream.Write( fNextDirection, sizeof(fNextDirection) );
      Stream.Write( fVisualStage, sizeof(fVisualStage) );
    end;

  procedure TCar.Deleted;
    begin
    end;

initialization

  LogFile.SetLogFile( paramstr(0) + '.log' );

end.




