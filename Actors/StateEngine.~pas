unit StateEngine;

interface

  uses
    Classes;

  const
    cplNormal = 0; //Normal privilege level

  type
    TStateId    = integer;
    TEventId    = integer;
    TMetaPoolId = integer;

  type
    TEventDispMethod = integer;

  const
    stNone = -1;
    stNull = 0;
    stAll  = high(TStateId);

  type
    PStateIdArray = ^TStateIdArray;
    TStateIdArray = array[word] of TStateId;

  const
    dmBroadCast             = 1;
    dmSingleThreadBroadCast = 2;
    dmAllUpBroadCast        = 3;
    dmSingleThreadAllUp     = 4;
    dmAllDown               = 5;

  const
    OwnerFrecuency = -1;

  type
    TWakerCommand = integer;

  const
    wcAct             = 1;
    wcEvent           = 2;
    wcStateTerminated = 3;

  type
    TMetaStateOptions = cardinal;

  const
    msoNone           = 0;
    msoSingleInstance = 1;
    msoJustContainer  = 2;
    msoDistributable  = 4;

  type
    TStatePool     = class;
    TMetaState     = class;
    TState         = class;
    TStateWaker    = class;
    TInitialStates = class;

    TEventDispDirection = ( ddTopDown, ddBottomUp );
    TEventTermination   = ( etSingleHandled, etNone );

    TEvent =
      class
        public
          constructor Create( aId : TEventId; aDispMethod : integer; const Data );

        public
          procedure Handled( Handler : TState ); virtual;

        protected
          fId               : TEventId;
          fDispMethod       : TEventDispMethod;
          fDispDirection    : TEventDispDirection;
          fTerminated       : boolean;
          fEventTermination : TEventTermination;

        protected
          function GetTerminated : boolean; virtual;

        public
          property Id               : TEventId read fId;
          property DispMethod       : TEventDispMethod read fDispMethod write fDispMethod;
          property DispDirection    : TEventDispDirection read fDispDirection default ddBottomUp;
          property EventTermination : TEventTermination read fEventTermination write fEventTermination;
          property Terminated       : boolean read GetTerminated;
      end;

    TEventDispatcher =
      class
        public
          procedure DispatchEvent( Sender : TObject; Event : TEvent ); virtual; abstract;
      end;

    TLauncherBehaviour = ( lbKeepAlive, lbDisappear, lbWaitFor );
    TActMode           = ( amdSynchronized, amdHighRes );

    CState = class of TState;
    TState =
      class
        public
          constructor Create( aMetaState : TMetaState; aStatePool : TStatePool; aOwner : TState; Frecuency : integer; const InitInfo ); virtual;

        public
          procedure PreAct( Mode : TActMode );                 virtual;
          procedure Act( Mode : TActMode );                    virtual;
          procedure PostAct( Mode : TActMode );                virtual;
          function  HandleEvent( anEvent : TEvent ) : boolean; virtual;
          procedure Terminate;                                 virtual;

        public
          procedure WaitFor( aStateWaker : TStateWaker );

        protected
          fOwner      : TState;
          fStatePool  : TStatePool;
          fMetaState  : TMetaState;
          fOwnerTimed : boolean;
          fFrecuency  : integer;
          procedure SetOwner( aOwner : TState );

        protected
          function  GetCPL : integer;         virtual;
          procedure SetCPL( aCPL : integer ); virtual;

        public
          function  TransitToStateId( Id : TStateId; aCPL : integer; Frecuency : integer; const StateInfo; LauncherBehaviour : TLauncherBehaviour ) : TState; virtual;
          procedure TransitToState( aState : TState; LauncherBehaviour : TLauncherBehaviour ); virtual;

        public
          property OwnerTimed : boolean    read fOwnerTimed;
          property CPL        : integer    read GetCPL write SetCPL;
          property Owner      : TState     read fOwner write SetOwner;
          property StatePool  : TStatePool read fStatePool;
          property MetaState  : TMetaState read fMetaState;
          property Frecuency  : integer    read fFrecuency;

        // Child stuff
        public
          procedure AddState( aState : TState );    virtual;
          procedure DeleteState( aState : TState ); virtual;
          procedure FreeChild( aState : TState );   virtual;
          procedure DeleteAll;                      virtual;

        protected
          function GetChild( Idx : integer ) : TState;  virtual;
          function FindChild( Id : TStateId ) : TState; virtual;
          function GetChildCount : integer;             virtual;
          function FindChildBreadthFirst( aId : TStateId; Level : integer; out MoreLevels : boolean ) : TState;
          function FindChildDepthFirst( aId : TStateId ) : TState;

        public
          property ChildState[Idx : integer] : TState read GetChild;
          property ChildCount : integer read GetChildCount;
      end;

    TInitialStates =
      class(TState)
        public
          constructor Create( aMetaState : TMetaState; aStatePool : TStatePool; aOwner : TState; Frecuency : integer; const InitInfo ); override;
          destructor Destroy;                                                                                                           override;

        public
          procedure PreAct( Mode : TActMode );  override;
          procedure Act( Mode : TActMode );     override;
          procedure PostAct( Mode : TActMode ); override;

        public
          procedure AddState( aState : TState );    override;
          procedure DeleteState( aState : TState ); override;

        protected
          function GetChild( Idx : integer ) : TState;  override;
          function GetChildCount : integer;             override;

        private
          fStates : TList;
      end;

    TMetaState =
      class
        public
          constructor Create( aId : TStateId; aStateClass : CState; ExclusiveStates : array of TStateId; aOptions : integer );

        public
          function CreateInstance( where : TStatePool; aOwner : TState; Frecuency : integer; const InitInfo ) : TState;

        private
          fId             : TStateId;
          fStateClass     : CState;
          fOptions        : cardinal;
          fExclusions     : PStateIdArray;
          fExclusionCount : integer;
          function  GetExclusion(Idx : integer) : TStateId;
          function  GetOption( aOption : cardinal ) : boolean;
          procedure SetOption( aOption : cardinal; aValue : boolean );

        public
          property Id : TStateId read fId;
          property Options[which : cardinal] : boolean read GetOption write SetOption;
          property ExclusionCount : integer read fExclusionCount;
          property Exclusions[Idx : integer] : TStateId read GetExclusion;
          property StateClass : CState read fStateClass;
      end;

    TMetaStatePool =
      class
        public
          constructor Create( anId : TMetaPoolId );
          destructor  Destroy; override;
          
        private
          fId : TMetaPoolId;
        public
          property Id : TMetaPoolId read fId;

        public
          function  CreateState( aId : TStateId; aStatePool : TStatePool; aOwner : TState; Frecuency : integer; const InitInfo ) : TState;
          procedure AddMetaState( aMetaState : TMetaState );
          procedure DeleteMetaState( aMetaState : TMetaState );
          procedure DeleteMetaStateId( aId : TStateId );

        private
          fRegistredId : TList;
          function FindId( aId : TStateId ) : TMetaState;

        public
          property MetaStates[Id : TStateId] : TMetaState read FindId;
      end;

    TVirtualIdTable =
      class
        public
          constructor Create;
          destructor  Destroy; override;

        private
          fVitualIdInfo : TList;
          function  GetRealId( aId : TStateId ) : TStateId;
          procedure SetRealId( VirtualId : TStateId; RealId : TStateId );

        public
          property RealId[Idx : TStateId] : TStateId read GetRealId write SetRealId; default;
      end;

    TSearchOptions = ( soBreadthFirst, soDepthFirst );

    TFrecuencyInfo =
      class
        public
          constructor Create( aFrecMultiplier : cardinal );
          destructor  Destroy; override;

        public
          procedure Act( Mode : TActMode; context : pointer );
          procedure AddActingState( aState : TState );
          procedure DeleteActingState( aState : TState );

        private
          fFrecMultiplier : cardinal;
          fCurrentTick    : cardinal;
          fActingStates   : TList;

        public
          property FrecMultiplier : cardinal read fFrecMultiplier;
      end;

    TStateModification = (stmdCreation, stmdTermination);
    TOnStateModified = procedure( State : TState; Modification : TStateModification ) of object;

    TStatePool =
      class
        public
          constructor Create( aMetaStatePool : TMetaStatePool; DefaultFrec : integer );
          destructor  Destroy; override;

        public
          procedure PreAct( Mode : TActMode );                 virtual;
          procedure Act( Mode : TActMode ); virtual;
          procedure PostAct( Mode : TActMode );                virtual;
          procedure ThrowEvent( Sender : TObject; anEvent : TEvent ); virtual;

        public
          function  StateIdTransition( Launcher : TState; Id : TStateId; aCPL : integer; Frecuency : integer; const StateInfo; LauncherBehaviour : TLauncherBehaviour ) : TState; virtual;
          procedure StateTransition( Launcher : TState; NewState : TState; LauncherBehaviour : TLauncherBehaviour ); virtual;
          function  FindState( who : TStateId; SearchOptions : TSearchOptions ) : TState;
          function  TerminateExclusivesStates( MetaState : TMetaState; StateCPL : integer ): boolean;

        private
          procedure SetState( Launcher : TState; NewState : TState; LauncherBehaviour : TLauncherBehaviour );
          procedure TerminateState( aState : TState );

        public
          function AddInitialState( aStateId : TStateId; aCPL : integer; const InitInfo ) : TState;                              virtual;
          function CreateState( aId : TStateId; aCPL : integer; Owner : TState; Frecuency : integer; const StateInfo ) : TState; virtual;

        protected
          procedure PutToSleep( Sleepy : TState; aStateWaker : TStateWaker );
          procedure AwakeState( aStateWaker : TStateWaker );

        protected
          function GetEventDispatcher( aEvent : TEvent ) : TEventDispatcher; virtual;

        protected
          procedure NotifyStateCreation( aState : TState );    virtual;
          procedure NotifyStateTermination( aState : TState ); virtual;
          procedure NotifyStateTransition( Launcher, NewState : TState; LauncherBehaviour : TLauncherBehaviour ); virtual;
        private
          fOnStateModified : TOnStateModified;
        public
          property OnStateModified : TOnStateModified read fOnStateModified write fOnStateModified;

        private
          fTransitions : TList;
          procedure RecordStateTransition( Launcher, NewState : TState; aLauncherBehaviour : TLauncherBehaviour );
          procedure FlushActData;

        public
          function GetStateTransition( State : TState; out NewState : TState ) : boolean;
          function GetStateIdTransition( StateId : TStateId; out NewState : TState ) : TState;

        protected
          fInitialStates  : TInitialStates;
          fVirtualIdTable : TVirtualIdTable;
          fMetaStatePool  : TMetaStatePool;
          fSleepStates    : TList;
          fDefaultFrec    : integer;
          fContext        : pointer;
          function  GetInitialState( Idx : integer ) : TState;
          function  GetInitialStateCount : integer;

        public
          property MetaStatePool : TMetaStatePool read fMetaStatePool;

        protected
          fFrecuencyInfo  : TList;
          procedure RegisterTimedState( aState : TState; aFrecMultiplier : integer ); virtual;
          procedure UnRegisterTimedState( aState : TState );                          virtual;
          function  FindFrecuencyInfo( aFrecMultiplier : cardinal ) : TFrecuencyInfo;

        public
          property InitialStateCount : integer read GetInitialStateCount;
          property InitialStates[Idx : integer] : TState read GetInitialState;
          property VirtualIdTable : TVirtualIdTable read fVirtualIdTable;
          property Context : pointer read fContext write fContext;
      end;

    TEventInfo = // for wcStateTerminated notification
      packed record
        Sender : TObject;
        Event  : TEvent;
      end;

    TStateWaker =
      class
        public
          constructor Create( aState : TState; aStatePool : TStatePool );

        public
          procedure Notify( what : TWakerCommand; const Info ); virtual;

        private
          fState     : TState;
          fStatePool : TStatePool;

        public
          property State : TState read fState;
      end;

implementation

  uses
    SysUtils;

  // Misc

  procedure FreeList( aList : TList );
    var
      i : integer;
    begin
      for i := pred(aList.Count) downto 0 do
        TObject(aList[i]).Free;
      aList.Free;
    end;

  // Event Dispatchers for everyone & Utils

  procedure GetBottomUpList( aNode : TState; Storage : TList; Exclude : TState ); //++ Fills Storage with the Node's childs sorted BottomUp
    var
      i : integer;
    begin
      if Exclude <> aNode
        then
          begin
            if aNode.ChildCount > 0
              then
                for i := 0 to pred(aNode.ChildCount) do
                  if aNode.ChildState[i] <> nil
                    then GetBottomUpList( aNode.ChildState[i], Storage, Exclude );
            Storage.Add( aNode );
          end;
    end;

  type
    TBroadCastDispatcher = // when dmBroadCast
      class(TEventDispatcher)
        public
          constructor Create( aRoot : TState );
          procedure   DispatchEvent( Sender : TObject; Event : TEvent ); override;

        private
          fRoot : TState;
      end;

    TSingleThreadBroadCast = // dmSingleThreadBroadCast
      class(TEventDispatcher)
        public
          procedure   DispatchEvent( Sender : TObject; Event : TEvent ); override;
      end;

    TAllUpBroadcast = // dmAllUpBroadCast
      class(TEventDispatcher)
        public
          constructor Create( aRoot : TState );
          procedure   DispatchEvent( Sender : TObject; Event : TEvent ); override;

        private
          fRoot : TState;
      end;

    TSingleThreadAllUp = // dmSingleThreadAllUp
      class(TEventDispatcher)
        public
          procedure DispatchEvent( Sender : TObject; Event : TEvent ); override;
      end;

    TAllDownDispatcher = // dmAllDown
      class(TEventDispatcher)
        public
          procedure DispatchEvent( Sender : TObject; Event : TEvent ); override;
      end;

  // TBroadCastDispatcher

  constructor TBroadCastDispatcher.Create( aRoot : TState );
    begin
      inherited Create;
      fRoot := aRoot;
    end;

  procedure TBroadCastDispatcher.DispatchEvent( Sender : TObject; Event : TEvent );
    var
      StateList : TList;
      i         : integer;
    begin
      StateList := TList.Create;
      try
        GetBottomUpList( fRoot, StateList, nil );
        if Event.DispDirection = ddTopDown
          then
            begin
              i := 0;
              while (i < StateList.Count) and not Event.Terminated do
                begin
                  try
                    TState(StateList[i]).HandleEvent( Event );
                  except
                  end;
                  inc( i );
                end
            end
          else
            begin
              i := pred(StateList.Count);
              while (i >= 0) and not Event.Terminated do
                begin
                  try
                    TState(StateList[i]).HandleEvent( Event );
                  except
                  end;
                  dec( i );
                end
            end;
      finally
        StateList.Free;
      end;
    end;

  // TSingleThreadBroadCast

  procedure TSingleThreadBroadCast.DispatchEvent( Sender : TObject; Event : TEvent );
    var
      StateList : TList;
      i         : integer;
      Node      : TState;
    begin
      StateList := TList.Create;
      try
        GetBottomUpList( TState(Sender), StateList, nil );

        Node := TState(Sender);
        repeat
          StateList.Add( Node );
          Node := Node.Owner;
        until Node.Owner = nil;

        if Event.DispDirection = ddBottomUp
          then
            begin
              i := 0;
              while (i < StateList.Count) and not Event.Terminated do
                begin
                  try
                    TState(StateList[i]).HandleEvent( Event );
                  except
                  end;
                  inc( i );
                end
            end
          else
            begin
              i := pred(StateList.Count);
              while (i >= 0) and not Event.Terminated do
                begin
                  try
                    TState(StateList[i]).HandleEvent( Event );
                  except
                  end;
                  dec( i );
                end
            end;
      finally
        StateList.Free;
      end;
    end;

  // TAllUpBroadcast

  constructor TAllUpBroadcast.Create( aRoot : TState );
    begin
      inherited Create;
      fRoot := aRoot;
    end;

  procedure TAllUpBroadcast.DispatchEvent( Sender : TObject; Event : TEvent );
    var
      StateList : TList;
      i         : integer;
    begin
      StateList := TList.Create;
      try
        GetBottomUpList( fRoot, StateList, TState(Sender) );
        if Event.DispDirection = ddTopDown
          then
            begin
              i := 0;
              while (i < StateList.Count) and not Event.Terminated do
                begin
                  try
                    TState(StateList[i]).HandleEvent( Event );
                  except
                  end;
                  inc( i );
                end
            end
          else
            begin
              i := pred(StateList.Count);
              while (i >= 0) and not Event.Terminated do
                begin
                  try
                    TState(StateList[i]).HandleEvent( Event );
                  except
                  end;
                  dec( i );
                end
            end;
      finally
        StateList.Free;
      end;
    end;

  // TSingleThreadAllUp

  procedure TSingleThreadAllUp.DispatchEvent( Sender : TObject; Event : TEvent );
    var
      StateList : TList;
      i         : integer;
      Node      : TState;
    begin
      StateList := TList.Create;
      try
        Node := TState(Sender);
        repeat
          StateList.Add( Node );
          Node := Node.Owner;
        until Node.Owner = nil;

        if Event.DispDirection = ddBottomUp
          then
            begin
              i := 0;
              while (i < StateList.Count) and not Event.Terminated do
                begin
                  try
                    TState(StateList[i]).HandleEvent( Event );
                  except
                  end;
                  inc( i );
                end
            end
          else
            begin
              i := pred(StateList.Count);
              while (i >= 0) and not Event.Terminated do
                begin
                  try
                    TState(StateList[i]).HandleEvent( Event );
                  except
                  end;
                  dec( i );
                end
            end;
      finally
        StateList.Free;
      end;
    end;

  // TAllDownDispatcher

  procedure TAllDownDispatcher.DispatchEvent( Sender : TObject; Event : TEvent );
    var
      StateList : TList;
      i         : integer;
    begin
      StateList := TList.Create;
      try
        GetBottomUpList( TState(Sender), StateList, nil );
        if Event.DispDirection = ddTopDown
          then
            begin
              i := 0;
              while (i < StateList.Count) and not Event.Terminated do
                begin
                  try
                    TState(StateList[i]).HandleEvent( Event );
                  except
                  end;
                  inc( i );
                end
            end
          else
            begin
              i := pred(StateList.Count);
              while (i >= 0) and not Event.Terminated do
                begin
                  try
                    TState(StateList[i]).HandleEvent( Event );
                  except
                  end;
                  dec( i );
                end
            end;
      finally
        StateList.Free;
      end;
    end;

  // TEvent

  constructor TEvent.Create( aId : TEventId; aDispMethod : integer; const Data );
    begin
      inherited Create;
      fId               := aId;
      fDispMethod       := aDispMethod;
      fDispDirection    := ddTopDown;
      fEventTermination := etSingleHandled;
      fterminated       := false;
    end;

  procedure TEvent.Handled( Handler : TState );
    begin
      if fEventTermination = etSingleHandled
        then fTerminated := true;
    end;

  function TEvent.GetTerminated : boolean;
    begin
      Result := fTerminated;
    end;

  // TState

  constructor TState.Create( aMetaState : TMetaState; aStatePool : TStatePool; aOwner : TState; Frecuency : integer; const InitInfo );
    begin
      inherited Create;
      fStatePool := aStatePool;
      fMetaState := aMetaState;
      Owner      := aOwner;
      if Frecuency = OwnerFrecuency
        then fOwnerTimed := true
        else
          begin
            fOwnerTimed := false;
            fStatePool.RegisterTimedState( self, Frecuency );
          end;
    end;

  procedure TState.PreAct( Mode : TActMode );
    begin
    end;

  procedure TState.Act( Mode : TActMode );
    begin
    end;

  procedure TState.PostAct( Mode : TActMode );
    begin
    end;

  function TState.HandleEvent( anEvent : TEvent ) : boolean;
    begin
      Result := false;
    end;

  procedure TState.Terminate;
    begin
      fStatePool.StateTransition( self, nil, lbDisappear );
    end;

  procedure TState.WaitFor( aStateWaker : TStateWaker );
    begin
      fStatePool.PutToSleep( self, aStateWaker );
    end;

  procedure TState.SetOwner( aOwner : TState );
    begin
      if fOwner <> nil
        then fOwner.DeleteState( self );
      if aOwner <> nil
        then aOwner.AddState( self );
      fOwner := aOwner;
    end;

  function TState.GetCPL : integer;
    begin
      Result := cplNormal;
    end;

  procedure TState.SetCPL( aCPL : integer );
    begin
      Assert( false, 'E_NOTIMPL' );
    end;

  function TState.TransitToStateId( Id : TStateId; aCPL : integer; Frecuency : integer; const StateInfo; LauncherBehaviour : TLauncherBehaviour ) : TState;
    begin
      Result := fStatePool.StateIdTransition( self, Id, aCPL, Frecuency, StateInfo, LauncherBehaviour );
    end;

  procedure TState.TransitToState( aState : TState; LauncherBehaviour : TLauncherBehaviour );
    begin
      fStatePool.StateTransition( self, aState, LauncherBehaviour );
    end;

  procedure TState.AddState( aState : TState );
    begin
      Assert( false, 'E_NOTIMPL' );
    end;

  procedure TState.DeleteState( aState : TState );
    begin
      Assert( false, 'E_NOTIMPL' );
    end;

  procedure TState.FreeChild( aState : TState );
    begin
      DeleteState( aState );
      aState.Free;
    end;

  procedure TState.DeleteAll;
    var
      i : integer;
    begin
      for i := pred(ChildCount) downto 0 do
        if ChildState[i] <> nil
          then ChildState[i].Terminate;
    end;

  function TState.FindChildBreadthFirst( aId : TStateId; Level : integer; out MoreLevels : boolean ) : TState;
    var
      Found  : boolean;
      i      : integer;
      Target : TState;
      Aux    : boolean;
      Child  : TState;
    begin
      if Level > 0
        then
          begin
            MoreLevels := false;
            Found      := false;
            i          := 0;
            Target     := nil;
            while (i < ChildCount) and not Found do
              begin
                Child := ChildState[i];
                if (Child <> nil) and (Child.ChildCount > 0)
                  then
                    begin
                      try
                        Target := Child.FindChildBreadthFirst( aId, pred(Level), Aux );
                        MoreLevels := MoreLevels or Aux;
                      except
                      end;
                    end;
                Found := Target <> nil;
                inc( i );
              end;
            Result := Target;
          end
        else
          begin
            Found      := false;
            i          := 0;
            MoreLevels := false;
            Child      := nil;
            while (i < ChildCount) and not Found do
              begin
                Child := ChildState[i];
                if Child <> nil
                  then
                    begin
                      Found := (Child.MetaState <> nil) and (Child.MetaState.Id = aId);
                      MoreLevels := MoreLevels or (Child.ChildCount > 0);
                    end;
                inc( i );
              end;
            if Found
              then
                begin
                  Assert( Child <> nil );
                  Result := Child;
                end
              else Result := nil;
          end;
    end;

  function TState.FindChildDepthFirst( aId : TStateId ) : TState;
    begin
      Assert( false, 'E_NOTIMPL' );
      Result := nil;
    end;

  function TState.GetChild( Idx : integer ) : TState;
    begin
      Assert( false, 'E_NOTIMPL' );
      Result := nil;
    end;

  function TState.FindChild( Id : TStateId ) : TState;
    var
      Found : boolean;
      i     : integer;
    begin
      Found := false;
      i     := 0;
      while (i < ChildCount) and not Found do
        begin
          Found := (ChildState[i] <> nil) and (ChildState[i].MetaState <> nil) and (ChildState[i].MetaState.Id = Id);
          inc( i );
        end;
      if Found
        then Result := ChildState[pred(i)]
        else Result := nil;
    end;

  function TState.GetChildCount : integer;
    begin
      Result := 0;
    end;

  // TInitialStates

  constructor TInitialStates.Create( aMetaState : TMetaState; aStatePool : TStatePool; aOwner : TState; Frecuency : integer; const InitInfo );
    begin
      inherited;
      fStates := TList.Create;
    end;

  destructor TInitialStates.Destroy;
    begin
      FreeList( fStates );
      inherited;
    end;

  procedure TInitialStates.PreAct( Mode : TActMode );
    var
      i : integer;
    begin
      for i := pred(fStates.Count) downto 0 do
        try
          TState(fStates[i]).PreAct( Mode );
        except
          try
            TState(fStates[i]).Terminate;
          except
            fStates.Delete( i );
          end;
        end;
    end;

  procedure TInitialStates.Act( Mode : TActMode );
    var
      i : integer;
    begin
      for i := pred(fStates.Count) downto 0 do
        try
          TState(fStates[i]).Act( Mode );
        except
          try
            TState(fStates[i]).Terminate;
          except
            fStates.Delete( i );
          end;
        end;
    end;

  procedure TInitialStates.PostAct( Mode : TActMode );
    var
      i : integer;
    begin
      for i := pred(fStates.Count) downto 0 do
        try
          TState(fStates[i]).PostAct( Mode );
        except
          try
            TState(fStates[i]).Terminate;
          except
            fStates.Delete( i );
          end;
        end;
    end;

  procedure TInitialStates.AddState( aState : TState );
    begin
      fStates.Add( aState );
    end;

  procedure TInitialStates.DeleteState( aState : TState );
    begin
      fStates.Delete( fStates.IndexOf( aState ));
    end;

  function TInitialStates.GetChild( Idx : integer ) : TState;
    begin
      Result := fStates[Idx];
    end;

  function TInitialStates.GetChildCount : integer;
    begin
      Result := fStates.Count;
    end;

  // TMetaState

  constructor TMetaState.Create( aId : TStateId; aStateClass : CState; ExclusiveStates : array of TStateId; aOptions : integer );
    begin
      inherited Create;
      fId         := aId;
      fStateClass := aStateClass;
      fOptions    := aOptions;

      fExclusionCount := succ(high(ExclusiveStates) - low(ExclusiveStates));
      if (fExclusionCount = 1) and (ExclusiveStates[0] = -1)
        then fExclusionCount := 0;
      if (msoSingleInstance and fOptions) <> 0
        then inc( fExclusionCount );
      if fExclusionCount > 0
        then
          begin
            getmem(fExclusions, sizeof(ExclusiveStates));
            move(ExclusiveStates, fExclusions^, sizeof(ExclusiveStates));
          end;
      if (msoSingleInstance and fOptions) <> 0
        then fExclusions[pred(fExclusionCount)] := aId;

    end;

  function TMetaState.CreateInstance( where : TStatePool; aOwner : TState; Frecuency : integer; const InitInfo ) : TState;
    begin
      Result := fStateClass.Create( Self, where, aOwner, Frecuency, InitInfo );
    end;

  function TMetaState.GetExclusion(Idx : integer) : TStateId;
    begin
      Result := fExclusions[Idx];
    end;

  function TMetaState.GetOption( aOption : cardinal ) : boolean;
    begin
      Result := (fOptions and aOption) <> 0;
    end;

  procedure TMetaState.SetOption( aOption : cardinal; aValue : boolean );
    begin
      if aValue
        then fOptions := fOptions or aOption
        else fOptions := fOptions and not aOption;
    end;

  // TMetaStatePool

  constructor TMetaStatePool.Create;
    begin
      inherited Create;
      fRegistredId := TList.Create;
    end;

  destructor TMetaStatePool.Destroy;
    begin
      FreeList( fRegistredId );
      inherited;
    end;

  function TMetaStatePool.CreateState( aId : TStateId; aStatePool : TStatePool; aOwner : TState; Frecuency : integer; const InitInfo ) : TState;
    begin
      Result := MetaStates[aId].CreateInstance( aStatePool, aOwner, Frecuency, InitInfo );
    end;

  procedure TMetaStatePool.AddMetaState( aMetaState : TMetaState );
    begin
      fRegistredId.Add( aMetaState );
    end;

  procedure TMetaStatePool.DeleteMetaState( aMetaState : TMetaState );
    begin
      fRegistredId.Delete( fRegistredId.IndexOf( aMetaState ));
    end;

  procedure TMetaStatePool.DeleteMetaStateId( aId : TStateId );
    begin
      DeleteMetaState( MetaStates[aId] ); //>> easy but inefficient
    end;

  function TMetaStatePool.FindId( aId : TStateId ) : TMetaState;
    var
      Found : boolean;
      i     : integer;
    begin
      Found := false;
      i     := 0;
      while (i < fRegistredId.Count) and not Found do
        begin
          Found := TMetaState(fRegistredId[i]).Id = aId;
          inc( i );
        end;
      if Found
        then Result := fRegistredId[pred(i)]
        else Result := nil;
    end;

  // TVirtualIdInfo used in VITs

  type
    TVirtualIdInfo =
      class
        public
          constructor Create( aVirtualId, aRealId : TStateId );

        public
          VirtualId : TStateId;
          RealId    : TStateId;
      end;

  constructor TVirtualIdInfo.Create( aVirtualId, aRealId : TStateId );
    begin
      inherited Create;
      VirtualId := aVirtualId;
      RealId    := aRealId;
    end;

  // TVirtualIdTable

  constructor TVirtualIdTable.Create;
    begin
      inherited;
      fVitualIdInfo := TList.Create;
    end;

  destructor TVirtualIdTable.Destroy;
    begin
      FreeList( fVitualIdInfo );
      inherited;
    end;

  function TVirtualIdTable.GetRealId( aId : TStateId ) : TStateId;
    var
      Found : boolean;
      i     : integer;
    begin
      Found := false;
      i     := 0;
      while (i < fVitualIdInfo.Count) and not Found do
        begin
          Found := TVirtualIdInfo(fVitualIdInfo[i]).VirtualId = aId;
          inc( i );
        end;
      if Found
        then Result := TVirtualIdInfo(fVitualIdInfo[pred(i)]).RealId
        else Result := aId;
    end;

  procedure TVirtualIdTable.SetRealId( VirtualId : TStateId; RealId : TStateId );
    begin
      fVitualIdInfo.Add( TVirtualIdInfo.Create( VirtualId, RealId ));
    end;

  type
    TStateTransition =
      class
        public
          constructor Create( aLauncher, aNewState : TState; aLauncherBehaviour : TLauncherBehaviour );

        public
          Launcher          : TState;
          NewState          : TState;
          LauncherBehaviour : TLauncherBehaviour;
      end;

  // TStateTransition

  constructor TStateTransition.Create( aLauncher, aNewState : TState; aLauncherBehaviour : TLauncherBehaviour );
    begin
      inherited Create;
      Launcher          := aLauncher;
      NewState          := aNewState;
      LauncherBehaviour := aLauncherBehaviour;
    end;

  // TFrecuencyInfo

  constructor TFrecuencyInfo.Create( aFrecMultiplier : cardinal );
    begin
      inherited Create;
      fFrecMultiplier := aFrecMultiplier;
      fCurrentTick    := aFrecMultiplier;
      fActingStates   := TList.Create;
    end;

  destructor TFrecuencyInfo.Destroy;
    begin
      fActingStates.Free;
      inherited;
    end;

  procedure TFrecuencyInfo.Act( Mode : TActMode; context : pointer );
    var
      i : integer;
    begin
      dec( fCurrentTick );
      if fCurrentTick = 0
        then
          begin
            for i := 0 to pred(fActingStates.Count) do
              TState(fActingStates[i]).Act( Mode );
            fCurrentTick := fFrecMultiplier;
          end;
    end;

  procedure TFrecuencyInfo.AddActingState( aState : TState );
    begin
      fActingStates.Add( aState );
    end;

  procedure TFrecuencyInfo.DeleteActingState( aState : TState );
    begin
      fActingStates.Delete( fActingStates.IndexOf(aState) );
    end;

  // TStatePool

  constructor TStatePool.Create( aMetaStatePool : TMetaStatePool; DefaultFrec : integer );
    var
      useless : integer;
    begin
      inherited Create;
      fFrecuencyInfo  := TList.Create;
      fDefaultFrec    := DefaultFrec;
      fInitialStates  := TInitialStates.Create( nil, self, nil, fDefaultFrec, useless );
      fVirtualIdTable := TVirtualIdTable.Create;
      fMetaStatePool  := aMetaStatePool;
      fSleepStates    := TList.Create;
      fTransitions    := TList.Create;
    end;

  destructor TStatePool.Destroy;
    begin
      fTransitions.Free;
      fInitialStates.Terminate;
      fVirtualIdTable.Free;
      FreeList( fSleepStates );
      FreeList( fFrecuencyInfo );
      inherited;
    end;

  procedure TStatePool.PreAct( Mode : TActMode );
    begin
    end;

  procedure TStatePool.Act( Mode : TActMode );
    var
      i : integer;
    begin
      try
        for i := 0 to pred(fFrecuencyInfo.Count) do
          TFrecuencyInfo(fFrecuencyInfo[i]).Act( Mode, context );

        for i := pred(fSleepStates.Count) downto 0 do
          TStateWaker(fSleepStates[i]).Notify( wcAct, self );
      except
      end;
    end;

  procedure TStatePool.PostAct( Mode : TActMode );
    begin
      FlushActData;
    end;

  procedure TStatePool.ThrowEvent( Sender : TObject; anEvent : TEvent );
    var
      EventDisp : TEventDispatcher;
      i         : integer;
      EventInfo : TEventInfo;
    begin
      EventInfo.Sender := Sender;
      EventInfo.Event  := anEvent;
      for i := 0 to pred(fSleepStates.Count) do
        TStateWaker(fSleepStates[i]).Notify( wcEvent, EventInfo );

      EventDisp := GetEventDispatcher( anEvent );
      try
        EventDisp.DispatchEvent( Sender, anEvent )
      finally
        EventDisp.Free;
      end;
    end;

  function TStatePool.StateIdTransition( Launcher : TState; Id : TStateId; aCPL : integer; Frecuency : integer; const StateInfo; LauncherBehaviour : TLauncherBehaviour ) : TState;
    var
      RealId : TStateId;
    begin
      RealId := fVirtualIdTable[Id];
      if RealId <> stNull
        then Result := CreateState( RealId, aCPL, nil, Frecuency, StateInfo )
        else Result := nil;
      RecordStateTransition( Launcher, Result, LauncherBehaviour );
    end;

  procedure TStatePool.StateTransition( Launcher : TState; NewState : TState; LauncherBehaviour : TLauncherBehaviour );
    begin
      RecordStateTransition( Launcher, NewState, LauncherBehaviour );
    end;

  procedure TStatePool.SetState( Launcher : TState; NewState : TState; LauncherBehaviour : TLauncherBehaviour );
    var
      Owner : TState;
    begin
      if NewState <> nil
        then
          begin
            if Launcher <> nil
              then Owner := Launcher.Owner
              else
                begin
                  Owner := fInitialStates;
                  NewState.fOwnerTimed := true;
                end;
            NewState.Owner := Owner;
          end
        else LauncherBehaviour := lbDisappear;

      NotifyStateTransition( Launcher, NewState, LauncherBehaviour );

      case LauncherBehaviour of
        lbDisappear :
          if Launcher <> nil
            then TerminateState( Launcher );
      end;
    end;

  procedure TStatePool.TerminateState( aState : TState );
    begin
      if not aState.OwnerTimed
        then UnRegisterTimedState( aState );
      if aState.Owner <> nil
        then aState.Owner.FreeChild( aState )
        else aState.Free;
    end;

  function TStatePool.TerminateExclusivesStates( MetaState : TMetaState; StateCPL : integer ): boolean;
    var
      DeadState : TState;
      i, j      : integer;
      ExcState  : TStateId;
      ExcList   : TList;
      Found     : boolean;
    begin
      ExcList := TList.Create;
      try
        for i := 0 to pred(MetaState.ExclusionCount) do
          begin
            ExcState := MetaState.Exclusions[i];
            if (ExcState <> stAll)
              then
                begin
                  DeadState := FindState( MetaState.Exclusions[i], soBreadthFirst );
                  if DeadState <> nil
                    then ExcList.Add( DeadState );
                end
              else
                for j := 0 to pred(fInitialStates.ChildCount) do
                  ExcList.Add( fInitialStates.ChildState[j] );
          end;

        i     := 0;
        Found := false;
        while (i < ExcList.Count) and not Found do
          begin
            Found := TState(ExcList[i]).CPL > StateCPL;
            inc( i );
          end;

        Result := not Found;
        if not Found
          then
            for i := pred(ExcList.Count) downto 0 do
              TState(ExcList[i]).Terminate;
      finally
        ExcList.Free;
      end;
    end;

  procedure TStatePool.RegisterTimedState( aState : TState; aFrecMultiplier : integer );
    var
      FrecuencyInfo : TFrecuencyInfo;
    begin
      FrecuencyInfo := FindFrecuencyInfo( aFrecMultiplier );
      if FrecuencyInfo = nil
        then
          begin
            FrecuencyInfo := TFrecuencyInfo.Create( aFrecMultiplier );
            fFrecuencyInfo.Add( FrecuencyInfo );
          end;
      FrecuencyInfo.AddActingState( aState );
    end;

  procedure TStatePool.UnRegisterTimedState( aState : TState );
    var
      FrecuencyInfo : TFrecuencyInfo;
    begin
      FrecuencyInfo := FindFrecuencyInfo( aState.Frecuency );
      Assert( FrecuencyInfo <> nil );
      FrecuencyInfo.DeleteActingState( aState );
    end;

  function TStatePool.FindFrecuencyInfo( aFrecMultiplier : cardinal ) : TFrecuencyInfo;
    var
      Found : boolean;
      i     : integer;
    begin
      Found := false;
      i     := 0;
      while (i < fFrecuencyInfo.Count) and not Found do
        begin
          Found := TFrecuencyInfo(fFrecuencyInfo[i]).fFrecMultiplier = aFrecMultiplier;
          inc( i );
        end;
      if Found
        then Result := fFrecuencyInfo[pred(i)]
        else Result := nil;
    end;

  function TStatePool.FindState( who : TStateId; SearchOptions : TSearchOptions ) : TState;
    const
      MaxLevels = 150; // Security
    var
      MoreLevel : boolean;
      Level     : integer;
      Found     : boolean;
    begin
      if SearchOptions = soBreadthFirst
        then
          begin
            MoreLevel := true;
            Level     := 0;
            Result    := nil;
            Found     := false;
            while MoreLevel and not Found and (Level <= MaxLevels) do
              begin
                Result := fInitialStates.FindChildBreadthFirst( who, Level, MoreLevel );
                Found  := Result <> nil;
                inc( Level );
              end;
            Assert( not (MoreLevel and (Level > MaxLevels)), 'There are so many Levels' );
          end
        else Result := fInitialStates.FindChildDepthFirst( who );
    end;

  function TStatePool.AddInitialState( aStateId : TStateId; aCPL : integer; const InitInfo ) : TState;
    var
      NewState : TState;
    begin
      NewState := CreateState( aStateId, aCPL, fInitialStates, OwnerFrecuency, InitInfo );
      Result := NewState;
    end;

  function TStatePool.CreateState( aId : TStateId; aCPL : integer; Owner : TState; Frecuency : integer; const StateInfo ) : TState;
    var
      RealId    : TStateId;
      MetaState : TMetaState;
    begin
      RealId    := VirtualIdTable[aId];
      MetaState := fMetaStatePool.MetaStates[RealId];
      if MetaState <> nil
        then
          if TerminateExclusivesStates( MetaState, aCPL )
            then
              begin
                Result := MetaState.CreateInstance( self, Owner, Frecuency, StateInfo );
                NotifyStateCreation( Result );
              end
            else Result := nil
        else raise Exception.Create( 'Invalid Class Id ' );
    end;

  procedure TStatePool.PutToSleep( Sleepy : TState; aStateWaker : TStateWaker );
    begin
      fSleepStates.Add( aStateWaker );
      Sleepy.Owner.DeleteState( Sleepy );
    end;

  procedure TStatePool.AwakeState( aStateWaker : TStateWaker );
    var
      Awaken : TState;
    begin
      Awaken := aStateWaker.State;
      try
        Awaken.Owner.AddState( Awaken );
      except
        Awaken.Free;
      end;
      fSleepStates.Delete( fSleepStates.IndexOf( aStateWaker ));
      aStateWaker.Free;
    end;

  function TStatePool.GetEventDispatcher( aEvent : TEvent ) : TEventDispatcher;
    begin
      case aEvent.DispMethod of
        dmBroadCast             : Result := TBroadCastDispatcher.Create( fInitialStates );
        dmSingleThreadBroadCast : Result := TSingleThreadBroadCast.Create;
        dmAllUpBroadCast        : Result := TAllUpBroadCast.Create( fInitialStates );
        dmSingleThreadAllUp     : Result := TSingleThreadAllUp.Create;
        dmAllDown               : Result := TAllDownDispatcher.Create;
        else raise Exception.Create( 'Cannot dispatch this event' );
      end;
    end;

  procedure TStatePool.NotifyStateCreation( aState : TState );
    begin
      if assigned(OnStateModified)
        then OnStateModified( aState, stmdCreation );
    end;

  procedure TStatePool.NotifyStateTermination( aState : TState );
    var
      i : integer;
    begin
      if assigned(OnStateModified)
        then OnStateModified( aState, stmdTermination );
      for i := 0 to pred(fSleepStates.Count) do
        TStateWaker(fSleepStates[i]).Notify( wcStateTerminated, aState );
    end;

  procedure TStatePool.NotifyStateTransition( Launcher, NewState : TState; LauncherBehaviour : TLauncherBehaviour );
    begin
    end;

  procedure TStatePool.RecordStateTransition( Launcher, NewState : TState; aLauncherBehaviour : TLauncherBehaviour );
    begin
      fTransitions.Add( TStateTransition.Create( Launcher, NewState, aLauncherBehaviour ));
    end;

  procedure TStatePool.FlushActData;
    procedure DoTransitions;
      var
        i : integer;
      begin
        for i := 0 to pred(fTransitions.Count) do
          SetState( TStateTransition(fTransitions[i]).Launcher, TStateTransition(fTransitions[i]).NewState, TStateTransition(fTransitions[i]).LauncherBehaviour );
      end;

    begin
      try
        try
          DoTransitions;
        finally
          fTransitions.Clear;
        end;
      except
      end;
    end;

  function TStatePool.GetStateTransition( State : TState; out NewState : TState ) : boolean;
    var
      Found : boolean;
      i     : integer;
    begin
      Found := false;
      i     := 0;
      while (i < fTransitions.Count) and not Found do
        begin
          Found := TStateTransition(fTransitions[i]).Launcher = State;
          inc( i );
        end;
      if Found
        then NewState := TStateTransition(fTransitions[pred(i)]).NewState
        else NewState := nil;
      Result := Found;
    end;

  function TStatePool.GetStateIdTransition( StateId : TStateId; out NewState : TState ) : TState;
    var
      Found : boolean;
      i     : integer;
    begin
      Found    := false;
      i        := 0;
      Result   := nil;
      NewState := nil;
      while (i < fTransitions.Count) and not Found do
        begin
          Found := TStateTransition(fTransitions[i]).Launcher.MetaState.Id = StateId;
          inc( i );
        end;
      if Found
        then
          begin
            Result   := TStateTransition(fTransitions[pred(i)]).Launcher;
            NewState := TStateTransition(fTransitions[pred(i)]).NewState;
          end;
    end;

  function TStatePool.GetInitialState( Idx : integer ) : TState;
    begin
      Result := fInitialStates.ChildState[Idx];
    end;

  function TStatePool.GetInitialStateCount : integer;
    begin
      Result := fInitialStates.ChildCount;
    end;

  // TStateWaker

  constructor TStateWaker.Create( aState : TState; aStatePool : TStatePool );
    begin
      inherited Create;
      fState     := aState;
      fStatePool := aStatePool;
    end;

  procedure TStateWaker.Notify( what : integer; const Info );
    begin
    end;

end.

