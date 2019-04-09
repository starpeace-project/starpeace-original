unit DistributedStates;

interface

  uses
    StateEngine, Classes;

  type
    TDataType = cardinal;
    TSynchId  = integer;

    TOnDistributedData = procedure (DataType : TDataType; Sender : TStatePool; const DataInfo ) of object;

    TMetaDistributedState =
      class(TMetaState)
        public
          constructor Create( aId : TStateId; aStateClass : CState; ExclusiveStates : array of TStateId; aOptions : integer; aPeerId : TStateId );

        private
          fPeerId : TStateId;
        public
          property PeerId : TStateId read fPeerId;
      end;

    CDistributedState = class of TDistributedState;
    TDistributedState =
      class(TState)
        public
          constructor Load( aMetaState : TMetaState; aStatePool : TStatePool; Source : TStream ); virtual;
          procedure   Store( Dest : TStream );                                                    virtual;
          procedure   Loaded;                                                                     virtual;
      end;

    CDistributedEvent = class of TDistributedEvent;
    TDistributedEvent =
      class(TEvent)
        public
          constructor Create( aId : TEventId; aDispMethod : integer; const Data; DataSize : integer );
          constructor Load( Source : TStream ); virtual;
          destructor  Destroy;                  override;
          procedure   Store( Dest : TStream );  virtual;

        private
          fDataSize : integer;
          fData     : pointer;

        public
          property Data : pointer read fData;
      end;

    TDistStateInfo =
      class
        State : TDistributedState;
        Owner : TDistributedState;
      end;

    TServerStatePool =
      class(TStatePool)
        public
          procedure ClientSerialize( Storage : TStream ); virtual;

        public
          procedure DistributeEvent( anEvent : TDistributedEvent );

        protected
          procedure NotifyStateTransition( Launcher, NewState : TState; LauncherBehaviour : TLauncherBehaviour ); override;

        protected
          function  GetDistributedOwner( aState : TState ) : TDistributedState;
          procedure BuildDistributedTree( Storage : TList; Root : TState );

        protected
          function GetSynchId( aState : TState ) : TSynchId;     virtual;
          function GetPeerClassId( aState : TState ) : TStateId; virtual;

        public
          procedure SerializeState( aState, aOwner : TDistributedState; Storage : TStream );
          procedure SerializeTransition( Storage : TStream; LauncherId : TStateId; NewState : TDistributedState; Behaviour : TLauncherBehaviour );

        private
          fOnDistributedData : TOnDistributedData;

        public
          property OnDistributedData : TOnDistributedData read fOnDistributedData write fOnDistributedData;
      end;

    TClientStatePool =
      class(TStatePool)
        public
          constructor LoadFromServer( aStream : TStream; aMetaStatePool : TMetaStatePool; DefaultFrec : integer );
          destructor  Destroy; override;

          function    LoadState( aStream : TStream ) : TDistStateInfo;
          procedure   LoadTransition( aStream : TStream );

        protected
          procedure NotifyStateTermination( aState :TState ); override;

        private
          fServerIdInfo : TList;
          procedure StateAdded( ServerId : TSynchId; aState : TDistributedState );
          procedure StateDeleted( aState : TState );
          function  ServerIdState( StateId : TSynchId ) : TDistributedState;
          function  GetStateIdx( aState : TState ) : integer;
      end;

  const
    dtStateTransition = 200;
    dtEvent           = 201;

  type
    TStateTransitionInfo =
      record
        StatePool  : TStatePool;
        LauncherId : TStateId;
        State      : TDistributedState;
        Behaviour  : TLauncherBehaviour;
      end;

    TEventInfo =
      record
        StatePool : TStatePool;
        Event     : TDistributedEvent;
      end;


implementation

  uses
    Logs, SysUtils;

  // TMetaDistributedState

  constructor TMetaDistributedState.Create( aId : TStateId; aStateClass : CState; ExclusiveStates : array of TStateId; aOptions : integer; aPeerId : TStateId );
    begin
      inherited Create( aId, aStateClass, ExclusiveStates, aOptions );
      fPeerId := aPeerId;
    end;

  // TDistributedState

  constructor TDistributedState.Load( aMetaState : TMetaState; aStatePool : TStatePool; Source : TStream );
    begin
      fStatePool  := aStatePool;
      fMetaState  := aMetaState;
      fFrecuency  := -1; //>>
      fOwnerTimed := true;
    end;

  procedure TDistributedState.Store( Dest : TStream );
    begin
    end;

  procedure TDistributedState.Loaded;
    begin
    end;

  // TDistributedEvent

  constructor TDistributedEvent.Create( aId : TEventId; aDispMethod : integer; const Data; DataSize : integer );
    begin
      inherited Create( aId, aDispMethod, Data );
      fDataSize := DataSize;
      getmem(fData, DataSize);
      move(Data, fData^, DataSize);
    end;

  constructor TDistributedEvent.Load( Source : TStream );
    begin
      Source.Read( fId, sizeof(fId) );
      Source.Read( fDataSize, sizeof(fDataSize) );
      getmem( fData, fDataSize );
      Source.Read( fData^, fDataSize );
      inherited Create( fId, dmBroadCast, fData ); //>> do not transmit dmBroadCast??
    end;

  destructor  TDistributedEvent.Destroy;
    begin
      freemem(fData);
      inherited;
    end;

  procedure TDistributedEvent.Store( Dest : TStream );
    begin
      Dest.Write( fId, sizeof(fId) );
      Dest.Write( fDataSize, sizeof(fDataSize) );
      Dest.Write( fData^, fDataSize );
    end;

  // TServerStatePool

  procedure TServerStatePool.ClientSerialize( Storage : TStream );
    var
      DistTree   : TList;
      i          : integer;
      StateCount : integer;
    begin
      DistTree := TList.Create;
      try
        BuildDistributedTree( DistTree, fInitialStates );

        StateCount := DistTree.Count;
        Storage.Write( StateCount, sizeof(StateCount) );

        for i := 0 to pred(DistTree.Count) do
          SerializeState( TDistStateInfo(DistTree[i]).State, TDistStateInfo(DistTree[i]).Owner, Storage );
      finally
        for i := 0 to pred(DistTree.Count) do
          TObject(DistTree[i]).Free;
      end;
    end;

  procedure TServerStatePool.DistributeEvent( anEvent : TDistributedEvent );
    var
      Info : TEventInfo;
    begin
      if Assigned(fOnDistributedData)
        then
          begin
            Info.StatePool := self; //>> Esto me preoucupa
            Info.Event     := anEvent;
            fOnDistributedData( dtEvent, self, Info );
          end;
    end;

  procedure TServerStatePool.NotifyStateTransition( Launcher, NewState : TState; LauncherBehaviour : TLauncherBehaviour );

    function DistributedTransition( out RealLauncher, RealNewState : TDistributedState ) : boolean;
      begin
        RealLauncher := nil;
        RealNewState := nil;
        if Launcher <> nil
          then
            begin
              if Launcher.MetaState.Options[msoDistributable]
                then
                  begin
                    Result := true;
                    if (NewState = nil) or NewState.MetaState.Options[msoDistributable]
                      then
                        begin
                          RealLauncher := TDistributedState(Launcher);
                          RealNewState := TDistributedState(NewState);
                        end
                      else
                        if LauncherBehaviour = lbDisappear
                          then
                            begin
                              RealLauncher := TDistributedState(Launcher);
                              RealNewState := nil;
                            end;
                  end
                else
                  begin
                    Result := NewState.MetaState.Options[msoDistributable];
                    if Result
                      then
                        begin
                          RealLauncher := GetDistributedOwner( Launcher );
                          RealNewState := TDistributedState(NewState);
                        end;
                  end;
            end
          else
            begin
              Result := NewState.MetaState.Options[msoDistributable];
              if Result
                then
                  begin
                    RealLauncher := nil;
                    RealNewState := TDistributedState(NewState);
                  end;
            end;
      end;

    var
      RealLauncher  : TDistributedState;
      RealNewState  : TDistributedState;
      Info          : TStateTransitionInfo;
    begin
      Assert( (Launcher <> nil) or (NewState <> nil) );
      if DistributedTransition( RealLauncher, RealNewState )
        then
          begin
            Assert( (RealLauncher <> nil) or (RealNewState <> nil) );
            if Assigned(fOnDistributedData)
              then
                begin
                  Info.StatePool := self;
                  if RealLauncher <> nil
                    then Info.LauncherId := RealLauncher.MetaState.Id
                    else Info.LauncherId := stNull;
                  Info.State     := RealNewState;
                  Info.Behaviour := LauncherBehaviour;
                  fOnDistributedData( dtStateTransition, self, Info );
                end;
          end;
    end;

  function TServerStatePool.GetDistributedOwner( aState : TState ) : TDistributedState;
    var
      Aux : TState;
    begin
      Aux := aState.Owner;
      while (Aux <> nil) and (Aux.MetaState <> nil) and not Aux.MetaState.Options[msoDistributable] do
        Aux := Aux.Owner;
      Result := TDistributedState(Aux);
    end;

  procedure TServerStatePool.BuildDistributedTree( Storage : TList; Root : TState );
    var
      DStateInfo : TDistStateInfo;
      i          : integer;
    begin
      try
        if (Root.MetaState <> nil) and Root.MetaState.Options[msoDistributable]
          then
            begin
              DStateInfo := TDistStateInfo.Create;
              DStateInfo.State := TDistributedState(Root);
              DStateInfo.Owner := TDistributedState(GetDistributedOwner( Root ));
              Storage.Add( DStateInfo );
            end;
        for i := 0 to pred(Root.ChildCount) do
          BuildDistributedTree( Storage, Root.ChildState[i] );
      except
      end;
    end;

  function TServerStatePool.GetSynchId( aState : TState ) : TSynchId;
    begin
      if aState <> nil
        then Result := TSynchId(aState)
        else Result := 0;
    end;

  function TServerStatePool.GetPeerClassId( aState : TState ) : TStateId;
    begin
      Result := aState.MetaState.Id// >> TMetaDistributedState(aState.MetaState).PeerId;
    end;

  procedure TServerStatePool.SerializeState( aState, aOwner : TDistributedState; Storage : TStream );
    var
      StateSynchId : TSynchId;
      OwnerSynchId : TSynchId;
      PeerClassId  : TStateId;
    begin
      PeerClassId  := GetPeerClassId( aState );

      StateSynchId := GetSynchId( aState );
      OwnerSynchId := GetSynchId( aOwner );

      Storage.Write( PeerClassId, sizeof(PeerClassId) );
      Storage.Write( StateSynchId, sizeof(StateSynchId) );
      Storage.Write( OwnerSynchId, sizeof(OwnerSynchId) );
      aState.Store( Storage );
    end;

  procedure TServerStatePool.SerializeTransition( Storage : TStream; LauncherId : TStateId; NewState : TDistributedState; Behaviour : TLauncherBehaviour );
    var
      NewStatePeerId : TStateId;
      LauncherPeerId : TStateId;
    begin
      NewStatePeerId := TMetaDistributedState(NewState.MetaState).PeerId;
      if LauncherId <> stNull
        then LauncherPeerId := TMetaDistributedState(MetaStatePool.MetaStates[LauncherId]).PeerId
        else LauncherPeerId := stNull;

      Storage.Write( LauncherPeerId, sizeof(LauncherPeerId) );
      Storage.Write( Behaviour, sizeof(Behaviour) );
      Storage.Write( NewStatePeerId, sizeof(NewStatePeerId) );
      NewState.Store( Storage );
    end;

  type
    TServerIdInfo =
      class
        StateId : TSynchId;
        State   : TDistributedState;
      end;

  // TClientStatePool

  constructor TClientStatePool.LoadFromServer( aStream : TStream; aMetaStatePool : TMetaStatePool; DefaultFrec : integer );
    var
      StateCount : integer;
      i          : integer;
      DState     : TDistStateInfo;
      Owner      : TState;
    begin
      inherited Create( aMetaStatePool, DefaultFrec );

      fServerIdInfo := TList.Create;

      aStream.Read( StateCount, sizeof(StateCount) );
      for i := 0 to pred(StateCount) do
        begin
          DState := LoadState( aStream );
          if DState.Owner <> nil
            then Owner := DState.Owner
            else Owner := fInitialStates;

          DState.State.Owner := Owner;
        end;
    end;

  destructor TClientStatePool.Destroy;
    begin
      try
        Logs.Log( 'Demolition', TimeToStr(Now) + ' - ' + ClassName );
      except
      end;
      fServerIdInfo.Free; //>> loosing memory
      inherited;
    end;

  function TClientStatePool.LoadState( aStream : TStream )  : TDistStateInfo;
    var
      StateSynchId : TSynchId;
      OwnerSynchId : TSynchId;
      ClassId      : TStateId;
      StateClass   : CDistributedState;
      MetaState    : TMetaState;
    begin
      Result := TDistStateInfo.Create;

      aStream.Read( ClassId, sizeof(ClassId) );
      aStream.Read( StateSynchId, sizeof(StateSynchId) );
      aStream.Read( OwnerSynchId, sizeof(OwnerSynchId) );

      MetaState  := fMetaStatePool.MetaStates[ClassId];
      Assert( MetaState <> nil );

      StateClass   := CDistributedState(MetaState.StateClass);
      Result.State := StateClass.Load( MetaState, self, aStream );

      if TerminateExclusivesStates( MetaState, Result.State.CPL )
        then
          begin
            Result.Owner := ServerIdState( OwnerSynchId );
            Result.State.Loaded;
            StateAdded( StateSynchId, Result.State );
            NotifyStateCreation( result.State );
          end
        else
          begin
            Result.State := nil;
            Result.Owner := nil;
          end;
    end;

  procedure TClientStatePool.LoadTransition( aStream : TStream );
    var
      ClassId     : TStateId;
      LauncherId  : TStateId;
      Behaviour   : TLauncherBehaviour;
      Launcher    : TState;
      NewState    : TDistributedState;
      MetaState   : TMetaState;
    begin
      aStream.Read( LauncherId, sizeof(LauncherId) );
      aStream.Read( Behaviour, sizeof(Behaviour) );
      aStream.Read( ClassId, sizeof(ClassId) );

      Launcher   := FindState( LauncherId, soBreadthFirst );
      MetaState  := fMetaStatePool.MetaStates[ClassId];
      NewState   := CDistributedState(MetaState.StateClass).Load( MetaState, self, aStream );
      NewState.Loaded;
      Launcher.TransitToState( NewState, Behaviour );
    end;

  procedure TClientStatePool.NotifyStateTermination( aState : TState );
    begin
      StateDeleted( aState );
      inherited;
    end;

  procedure TClientStatePool.StateAdded( ServerId : TSynchId; aState : TDistributedState );
    var
      ServerIdInfo : TServerIdInfo;
    begin
      ServerIdInfo := TServerIdInfo.Create;
      ServerIdInfo.StateId := ServerId;
      ServerIdInfo.State := aState;
      fServerIdInfo.Add( ServerIdInfo );
    end;

  procedure TClientStatePool.StateDeleted( aState : TState );
    var
      Idx : integer;
    begin
      Idx := GetStateIdx( aState );
      if Idx <> -1
        then fServerIdInfo.Delete( Idx );
    end;

  function TClientStatePool.ServerIdState( StateId : TSynchId ) : TDistributedState;
    var
      i     : integer;
      Found : boolean;
    begin
      i     := 0;
      Found := false;
      while (i < fServerIdInfo.Count) and not Found do
        begin
          Found := TServerIdInfo(fServerIdInfo[i]).StateId = StateId;
          inc( i );
        end;
      if Found
        then Result := TServerIdInfo(fServerIdInfo[i]).State
        else Result := nil;
    end;

  function TClientStatePool.GetStateIdx( aState : TState ) : integer;
    var
      i     : integer;
      Found : boolean;
    begin
      i     := 0;
      Found := false;
      while (i < fServerIdInfo.Count) and not Found do
        begin
          Found := TServerIdInfo(fServerIdInfo[i]).State = aState;
          inc( i );
        end;
      if Found
        then Result := pred( i )
        else Result := -1;
    end;

end.
