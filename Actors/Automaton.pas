unit Automaton;

interface

  uses
    Classes, ActorTypes, ActorPool, StateEngine, DistributedStates, Variants;

  type
    IAutomationEngine =
      interface
        procedure LaunchState( Launcher, State : TStateId; aCPL : integer; Frequency : integer; LauncherBehavior : TLauncherBehaviour );
        procedure ThrowEvent ( anEvent : TDistributedEvent );
      end;

    IAutomatable =
      interface
        procedure Act( State : TStateId; Mode : TActMode );
        function  HandleEvent( anEvent : TDistributedEvent ) : boolean;
        procedure Dispatch( var Msg );
      end;

    IServerAutomatable =
      interface( IAutomatable )
        procedure SetAutomationEngine( anEngine : IAutomationEngine );
        procedure SetActor( anActor : IServerActor );
        function  GetActor : IServerActor;
        procedure Store( Stream : TStream );
      end;

    IClientAutomatable =
      interface( IAutomatable )
        procedure Load( Stream : TStream );
        procedure Deleted;
      end;

  type
    TMetaAutomaton   = class;
    TAutomaton       = class;
    TServerAutomaton = class;
    TClientAutomaton = class;

    CAutomaton       = class of TAutomaton;
    CServerAutomaton = class of TServerAutomaton;
    CClientAutomaton = class of TClientAutomaton;

    TServerAutomatableFactory = function( Kind : TActorKind; const Context ) : IServerAutomatable;
    TClientAutomatableFactory = function( Kind : TActorKind; const Context ) : IClientAutomatable;

    TMetaAutomaton =
      class
        public
          constructor Create( aKind            : TActorKind;
                              aPoolId          : TActorPoolId;
                              aAutomatonClass  : CAutomaton;
                              aBehavior        : TMetaStatePool );
        private
          fKind           : TActorKind;
          fPoolId         : TActorPoolId;
          fAutomatonClass : CAutomaton;
          fBehavior       : TMetaStatePool;
        public
          property Kind           : TActorKind     read fKind;
          property PoolId         : TActorPoolId   read fPoolId;
          property AutomatonClass : CAutomaton     read fAutomatonClass;
          property Behavior       : TMetaStatePool read fBehavior;
        public
          function Instantiate( AutomatonId : TActorId ) : TAutomaton;
        public
          procedure Register;
      end;

    TMetaServerAutomaton =
      class( TMetaAutomaton )
        public
          constructor Create( aKind               : TActorKind;
                              aPoolId             : TActorPoolId;
                              aAutomatonClass     : CServerAutomaton;
                              aBehavior           : TMetaStatePool;
                              aAutomatableFactory : TServerAutomatableFactory );
        private
          fAutomatableFactory : TServerAutomatableFactory;
        public
          function Instantiate( AutomatonId : TActorId; const Context ) : TServerAutomaton;
          function InstantiateAutomatable( const Context ) : IServerAutomatable;
      end;

    TMetaClientAutomaton =
      class( TMetaAutomaton )
        public
          constructor Create( aKind               : TActorKind;
                              aPoolId             : TActorPoolId;
                              aAutomatonClass     : CClientAutomaton;
                              aBehavior           : TMetaStatePool;
                              aAutomatableFactory : TClientAutomatableFactory );
        private
          fAutomatableFactory : TClientAutomatableFactory;
        public
          function Instantiate( AutomatonId : TActorId; const Context ) : TClientAutomaton;
          function InstantiateAutomatable( const Context ) : IClientAutomatable;
      end;

    TAutomaton =
      class( TInterfacedObject, IActor )
        protected
          constructor Create( aMetaAutomaton : TMetaAutomaton; anId : TActorId ); virtual;
        private
          fMetaAutomaton : TMetaAutomaton; 
          fId            : TActorId;
        protected
          function GetAutomatable : IAutomatable; virtual; abstract;
        public
          property MetaAutomaton : TMetaAutomaton read fMetaAutomaton;
          property Id            : TActorId       read fId;
          property Automatable   : IAutomatable   read GetAutomatable;
        protected
          procedure Act( State : TStateId; Mode : TActMode );  virtual; abstract;
        // IActor
        private
          function getId   : TActorId;
          function getKind : TActorKind;
        // Pool events
        private
          procedure StateModified( State : TState; Modification : TStateModification );
      end;

    TServerAutomaton =
      class( TAutomaton, IServerActor, IAutomationEngine )
        protected
          constructor Create( aMetaAutomaton : TMetaAutomaton; anId : TActorId ); override;
        public
          destructor Destroy; override;
        private
          fAutomatable : IServerAutomatable;
          fStatePool   : TServerStatePool;
        protected
          function GetAutomatable : IAutomatable; override;
        public
          property Automatable : IServerAutomatable read fAutomatable write fAutomatable;
        protected
          procedure Act( State : TStateId; Mode : TActMode ); override;
        // IAutomationEngine
        protected
          procedure LaunchState( Launcher, State : TStateId; aCPL : integer; Frequency : integer; LauncherBehavior : TLauncherBehaviour );
          procedure ThrowEvent ( anEvent : TDistributedEvent );
        // IServerActor
        private
          function getStatePool : TServerStatePool;
          function getContext   : pointer;
        protected
          procedure Store( Stream : TStream ); virtual;
        public
          procedure DefaultHandler( var Message ); override;
      end;

    TAutomationOperation = (aopInserted, aopDeleted);
    TOnClientAutomatonModified = procedure( Automaton : TClientAutomaton; Operation : TAutomationOperation ) of object;

    TClientAutomaton =
      class( TAutomaton, IClientActor )
        public
          destructor Destroy; override;
        private
          fAutomatable : IClientAutomatable;
          fStatePool   : TClientStatePool;
        protected
          function GetAutomatable : IAutomatable; override;
        public
          property Automatable : IClientAutomatable read fAutomatable write fAutomatable;
        protected
          procedure Act( State : TStateId; Mode : TActMode ); override;
        // IClientActor
        private
          function  getStatePool : TClientStatePool;
          procedure setStatePool( aStatePool : TClientStatePool );
          procedure Inserted;
          procedure Deleted;
        protected
          procedure Load( Stream : TStream ); virtual;
        // Events to pool
        private
          fOnAutomatonModified : TOnClientAutomatonModified;
        public
          property OnAutomatonModified : TOnClientAutomatonModified read fOnAutomatonModified write fOnAutomatonModified;
      end;

  type
    TAutomatedState =
      class( TDistributedState )
        private
          fAutomaton : TAutomaton;
        public
          procedure Act( Mode : TActMode );                    override;
          function  HandleEvent( anEvent : TEvent ) : boolean; override;
      end;


implementation

  uses
    ClassStorage, SysUtils;


  // TMetaAutomaton

  constructor TMetaAutomaton.Create( aKind : TActorKind; aPoolId : TActorPoolId; aAutomatonClass : CAutomaton; aBehavior : TMetaStatePool );
    begin
      inherited Create;
      fKind               := aKind;
      fPoolId             := aPoolId; 
      fAutomatonClass     := aAutomatonClass;
      fBehavior           := aBehavior;
    end;

  function TMetaAutomaton.Instantiate( AutomatonId : TActorId ) : TAutomaton;
    begin
      result := fAutomatonClass.Create( self, AutomatonId );
    end;

  procedure TMetaAutomaton.Register;
    begin
      TheClassStorage.RegisterClass( IntToStr(PoolId), IntToStr(Kind), self );
    end;
    

  // TMetaServerAutomaton

  constructor TMetaServerAutomaton.Create( aKind               : TActorKind;
                                           aPoolId             : TActorPoolId;
                                           aAutomatonClass     : CServerAutomaton;
                                           aBehavior           : TMetaStatePool;
                                           aAutomatableFactory : TServerAutomatableFactory );
    begin
      inherited Create( aKind, aPoolId, aAutomatonClass, aBehavior );
      fAutomatableFactory := aAutomatableFactory;
    end;

  function TMetaServerAutomaton.Instantiate( AutomatonId : TActorId; const Context ) : TServerAutomaton;
    begin
      result := TServerAutomaton(inherited Instantiate( AutomatonId ));
      result.Automatable := InstantiateAutomatable( Context );
      if result.Automatable <> nil
        then result.Automatable.SetAutomationEngine( result );
    end;

  function TMetaServerAutomaton.InstantiateAutomatable( const Context ) : IServerAutomatable;
    begin
      if assigned(fAutomatableFactory)
        then result := fAutomatableFactory( Kind, Context );
    end;


  // TMetaClientAutomaton

  constructor TMetaClientAutomaton.Create( aKind               : TActorKind;
                                           aPoolId             : TActorPoolId;
                                           aAutomatonClass     : CClientAutomaton;
                                           aBehavior           : TMetaStatePool;
                                           aAutomatableFactory : TClientAutomatableFactory );
    begin
      inherited Create( aKind, aPoolId, aAutomatonClass, aBehavior );
      fAutomatableFactory := aAutomatableFactory;
    end;

  function TMetaClientAutomaton.Instantiate( AutomatonId : TActorId; const Context ) : TClientAutomaton;
    begin
      result := TClientAutomaton(inherited Instantiate( AutomatonId ));
      result.Automatable := InstantiateAutomatable( Context );
    end;

  function TMetaClientAutomaton.InstantiateAutomatable( const Context ) : IClientAutomatable;
    begin
      if assigned(fAutomatableFactory)
        then result := fAutomatableFactory( Kind, Context );
    end;


  // TAutomaton

  constructor TAutomaton.Create( aMetaAutomaton : TMetaAutomaton; anId : TActorId );
    begin
      inherited Create;
      fMetaAutomaton := aMetaAutomaton;
      fId            := anId;
    end;

  function TAutomaton.getId : TActorId;
    begin
      result := fId;
    end;

  function TAutomaton.getKind : TActorKind;
    begin
      result := fMetaAutomaton.fKind;
    end;

  procedure TAutomaton.StateModified( State : TState; Modification : TStateModification );
    begin
      if Modification = stmdCreation
        then TAutomatedState(State).fAutomaton := self;
    end;


  // TServerAutomaton

  constructor TServerAutomaton.Create( aMetaAutomaton : TMetaAutomaton; anId : TActorId );
    begin
      inherited;
      fStatePool := TServerStatePool.Create( MetaAutomaton.Behavior, 1 ); //>> puse el 1
      fStatePool.OnStateModified := StateModified;
    end;

  destructor TServerAutomaton.Destroy;
    begin
      fStatePool.Free;
      inherited;
    end;

  function TServerAutomaton.GetAutomatable : IAutomatable;
    begin
      result := fAutomatable;
    end;
    
  procedure TServerAutomaton.Act( State : TStateId; Mode : TActMode );
    begin
      Automatable.Act( state, Mode );
    end;

  procedure TServerAutomaton.LaunchState( Launcher, State : TStateId; aCPL : integer; Frequency : integer; LauncherBehavior : TLauncherBehaviour );
    var
      LauncherState : TState;
    begin
      LauncherState := fStatePool.FindState( Launcher, soBreadthFirst );
      if LauncherState <> nil
        then LauncherState.TransitToStateId( State, aCPL, Frequency, self, LauncherBehavior );
    end;

  procedure TServerAutomaton.ThrowEvent( anEvent : TDistributedEvent );
    begin
      TServerStatePool(fStatePool).DistributeEvent( anEvent );
    end;

  function TServerAutomaton.getStatePool : TServerStatePool;
    begin
      result := fStatePool;
    end;

  function TServerAutomaton.getContext : pointer;
    begin
      result := self;
    end;

  procedure TServerAutomaton.Store( Stream : TStream );
    begin
      Automatable.Store( Stream );
    end;

  procedure TServerAutomaton.DefaultHandler( var Message );
    begin
      Automatable.Dispatch( Message );
    end;


  // TClientAutomaton

  destructor TClientAutomaton.Destroy;
    begin
      fStatePool.Free;
      inherited;
    end;
    
  function TClientAutomaton.GetAutomatable : IAutomatable;
    begin
      result := fAutomatable;
    end;

  procedure TClientAutomaton.Act( State : TStateId; Mode : TActMode );
    begin
      Automatable.Act( state, Mode );
    end;

  function TClientAutomaton.getStatePool : TClientStatePool;
    begin
      result := fStatePool;
    end;
    
  procedure TClientAutomaton.setStatePool( aStatePool : TClientStatePool );
    var
      i : integer;
    begin
      fStatePool := aStatePool;
      for i := 0 to pred(aStatePool.InitialStateCount) do
        if aStatePool.InitialStates[i] is TAutomatedState
          then TAutomatedState(aStatePool.InitialStates[i]).fAutomaton := self;
      fStatePool.OnStateModified := StateModified;
    end;
    
  procedure TClientAutomaton.Inserted;
    begin
      if assigned(OnAutomatonModified)
        then OnAutomatonModified( self, aopInserted );
    end;

  procedure TClientAutomaton.Deleted;
    begin
      Automatable.Deleted;
      if assigned(OnAutomatonModified)
        then OnAutomatonModified( self, aopDeleted );
    end;

  procedure TClientAutomaton.Load( Stream : TStream );
    begin
      Automatable.Load( Stream );
    end;


  //  TAutomatedState

  procedure TAutomatedState.Act( Mode : TActMode );
    begin
      fAutomaton.Act( MetaState.Id, Mode );
    end;

  function TAutomatedState.HandleEvent( anEvent : TEvent ) : boolean;
    begin
      result := fAutomaton.Automatable.HandleEvent( TDistributedEvent(anEvent) );
    end;
    

end.



