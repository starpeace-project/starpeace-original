unit ActorPool;

interface

  uses
    StateEngine, DistributedStates, ActorTypes, Classes, Collection, SyncObjs, Windows, Variants;

  type
    TActorPoolId = integer;

  type
    TServerActorData =
      class
        public
          constructor Create( anActor : IServerActor );
          destructor  Destroy; override;
        private
          fActor   : IServerActor;
          fViewers : TLockableCollection;
      end;

  type
    TServerViewerData =
      class
        public
          constructor Create( aViewer : IViewer );
          destructor  Destroy; override;
        private
          fViewer   : IViewer;
          fTickData : TStream;
        public
          procedure ClearTickData;
      end;

  type
    TScopeChange =
      class
        public
          constructor Create( aChangeType : TDataType; aViewer : TServerViewerData; anActorId : TActorId; anActor : IServerActor );
        private
          fChangeType : TDataType;
          fViewer     : TServerViewerData;
          fActorId    : TActorId;
          fActor      : IServerActor;
        public
          property ChangeType : TDataType         read fChangeType;
          property Viewer     : TServerViewerData read fViewer;
          property ActorId    : TActorId          read fActorId;
          property Actor      : IServerActor      read fActor;
      end;

  type
    TActorPool =
      class
        public
          constructor Create( anId : TActorPoolId );
          destructor  Destroy; override;
        private
          fId        : TActorPoolId;
          fActors    : TLockableCollection;
          fLock      : TCriticalSection;
          fTickCount : cardinal;
        public
          property Id : TActorPoolId read fId;
        public
          property TickCount : cardinal read fTickCount;
        public
          procedure Lock;
          procedure Unlock;
      end;

    TOnSendTickData = procedure( PoolId : TActorPoolId; ViewerId : TViewerId; TickCount : cardinal; TickData : TStream ) of object; //>> puesto por mi el of object

    TServerActorPool =
      class( TActorPool )
        public
          constructor Create( anId : TActorPoolId; aDistProc : TOnDistributedData );
          destructor  Destroy; override;
        private
          fDistProc : TOnDistributedData;
        public
          procedure Act; virtual;
        private
          fViewers : TLockableCollection;
          fNewMeat : TLockableCollection;
        public
          procedure AddActor ( Actor  : IServerActor );
          procedure DelActor ( Actor  : IServerActor );
          procedure AddViewer( Viewer : IViewer );
          procedure DelViewer( Viewer : IViewer );
        private
          fScopeChanges : TLockableCollection;
        protected
          function  CheckScopes : TCollection; virtual;
          procedure SerializeData( DataType : TDataType; Source : IServerActor; const Data; TickData : TStream ); virtual;
        private
          fSendTickData : TOnSendTickData;
        public
          property SendTickData : TOnSendTickData read fSendTickData write fSendTickData;
        protected
          procedure ProcessTickData( ViewerId : TViewerId; TickCount : cardinal; const TickData : TStream ); virtual;
        private
          procedure PoolDistProc( DataType : TDataType; Sender : TStatePool; const DataInfo );
        private
          function GetActorData( Actor : IServerActor ) : TServerActorData;
          function GetViewerData( Viewer : IViewer ) : TServerViewerData;
          function GetActorFromPool( Pool : TStatePool ) : TServerActorData;
      end;


  type
    TClientActorFactory = function( ActorKind : TActorKind; ActorId : TActorId ) : IClientActor of object;
    TMetaPoolLocator    = function( MetaPoolId : TMetaPoolId ) : TMetaStatePool;

    TClientActorData =
      class
        public
          constructor Create( anActor : IClientActor );
        private
          fActor : IClientActor;
      end;

    TClientActorPool =
      class( TActorPool )
        public
          procedure Act( TickData : TStream ); virtual;
          procedure HighResAct; virtual;
        protected
          procedure ParseServerData( DataType : TDataType; TickData : TStream );
        private
          fClientActorFactory : TClientActorFactory;
          fMetaPoolLocator    : TMetaPoolLocator;
        public
          property ClientActorFactory : TClientActorFactory read fClientActorFactory write fClientActorFactory;
          property MetaPoolLocator    : TMetaPoolLocator    read fMetaPoolLocator    write fMetaPoolLocator;
        private
          function GetActorFromId( Id : TActorId ) : TClientActorData;
      end;

  const
    dtActorPoolTick    = 210;
    dtActorEntersScope = 211;
    dtActorLeavesScope = 212;

  type
    TActorScopeInfo =
      record
        ScopeChange : TScopeChange;
      end;


implementation

  uses
    LogFile, SysUtils;

  // TServerActorData

  constructor TServerActorData.Create( anActor : IServerActor );
    begin
      inherited Create;
      fActor   := anActor;
      fViewers := TLockableCollection.Create( 0, rkUse );
    end;

  destructor TServerActorData.Destroy;
    begin
      fViewers.Free;
      inherited;
    end;


  // TScopeChange

  constructor TScopeChange.Create( aChangeType : TDataType; aViewer : TServerViewerData; anActorId : TActorId; anActor : IServerActor );
    begin
      inherited Create;
      fChangeType := aChangeType;
      fViewer     := aViewer;
      fActorId    := anActorId;
      fActor      := anActor;
    end;


  // TServerViewerData

  constructor TServerViewerData.Create( aViewer : IViewer );
    begin
      inherited Create;
      fViewer := aViewer;
      fTickData := TMemoryStream.Create;
    end;

  destructor TServerViewerData.Destroy;
    begin
      fTickData.Free;
      inherited;
    end;

  procedure TServerViewerData.ClearTickData;
    begin
      fTickData.Size     := 0;
      fTickData.Position := 0;
    end;


  // TActorPool

  constructor TActorPool.Create( anId : TActorPoolId );
    begin
      inherited Create;
      fId     := anId;
      fActors := TLockableCollection.Create( 0, rkBelonguer );
      fLock   := TCriticalSection.Create;
    end;

  destructor TActorPool.Destroy;
    begin
      fLock.Free;
      fActors.Free;
      inherited;
    end;

  procedure TActorPool.Lock;
    begin
      fLock.Enter;
    end;

  procedure TActorPool.Unlock;
    begin
      fLock.Leave;
    end;


  // TServerActorPool

  constructor TServerActorPool.Create( anId : TActorPoolId; aDistProc : TOnDistributedData );
    begin
      inherited Create( anId );
      fDistProc     := aDistProc;
      fViewers      := TLockableCollection.Create( 0, rkBelonguer );
      fNewMeat      := TLockableCollection.Create( 0, rkBelonguer );
      fScopeChanges := TLockableCollection.Create( 0, rkBelonguer );
    end;

  destructor TServerActorPool.Destroy;
    begin
      fViewers.Free;
      fNewMeat.Free;
      fScopeChanges.Free;
      inherited;
    end;

  procedure TServerActorPool.Act;

    procedure IncludeNewMeat;
      var
        i    : integer;
        Data : TServerActorData;
      begin
        fNewMeat.Lock;
        try
          for i := 0 to pred(fNewMeat.Count) do
            begin
              Data := TServerActorData.Create( IServerActor(pointer(fNewMeat[i])) );
              Data.fActor.getStatePool.OnDistributedData := PoolDistProc;
              fActors.Insert( Data );
            end;
          fNewMeat.ExtractAll;
        finally
          fNewMeat.Unlock;
        end;
      end;

    procedure InitTickData;
      var
        i : integer;
      begin
        for i := 0 to pred(fViewers.Count) do
          with TServerViewerData(fViewers[i]) do
            try
              SerializeData( dtActorPoolTick, nil, fTickCount, fTickData );
            except
            end;
      end;

    procedure PackScopeChanges;
      var
        NewChanges : TCollection;
        i          : integer;
        SC         : TScopeChange;
        VD         : TServerViewerData;
        Info       : TActorScopeInfo;
      begin
        NewChanges := CheckScopes;
        try
          fScopeChanges.InsertColl( NewChanges );
          NewChanges.ExtractAll;
        finally
          NewChanges.Free;
        end;
        for i := 0 to pred(fScopeChanges.Count) do
          begin
            SC := TScopeChange(fScopeChanges[i]);
            VD := SC.Viewer;
            if VD <> nil
              then
                begin
                  Info.ScopeChange := SC;
                  SerializeData( SC.ChangeType, nil, Info, VD.fTickData )
                end;
          end;
        fScopeChanges.DeleteAll;
      end;

    procedure ExecutePools;
      var
        i : integer;
      begin
        for i := 0 to pred(fActors.Count) do
          with TServerActorData(fActors[i]) do
            try
              fActor.getStatePool.PreAct( amdSynchronized );
              fActor.getStatePool.Act( amdSynchronized );
              fActor.getStatePool.PostAct( amdSynchronized );
            except
            end;
      end;

    procedure FlushTickData;
      var
        i : integer;
      begin
        for i := 0 to pred(fViewers.Count) do
          with TServerViewerData(fViewers[i]) do
            try
              ProcessTickData( fViewer.getId, fTickCount, fTickData );
              ClearTickData;
            except
            end;
      end;

    begin
      inherited;
      Lock;
      try
        inc( fTickCount );
        IncludeNewMeat;
        InitTickData;
        PackScopeChanges;
        ExecutePools;
        FlushTickData;
      finally
        Unlock
      end;
    end;

  procedure TServerActorPool.AddActor( Actor : IServerActor );
    begin
      Lock;
      try
        fNewMeat.Insert( TObject(Actor) );
      finally
        Unlock;
      end
    end;

  procedure TServerActorPool.DelActor( Actor : IServerActor );
    var
      AD : TServerActorData;
      VD : TServerViewerData;
      i  : integer;
      SC : TScopeChange;
    begin
      Lock;
      try
        if fNewMeat.IndexOf( TObject(Actor) ) = NoIndex
          then
            begin
              AD := GetActorData( Actor );
              if AD <> nil
                then
                  begin
                    for i := 0 to pred(AD.fViewers.Count) do
                      begin
                        VD := TServerViewerData(AD.fViewers[i]);
                        SC := TScopeChange.Create( dtActorLeavesScope, VD, Actor.getId, nil );
                        fScopeChanges.Insert( SC );
                      end;
                    fActors.Delete( AD );
                  end;
            end
          else fNewMeat.Delete( TObject(Actor) );
      finally
        Unlock;
      end
    end;

  procedure TServerActorPool.AddViewer( Viewer : IViewer );
    var
      VD : TServerViewerData;
    begin
      Lock;
      try
        VD := GetViewerData( Viewer );
        if VD = nil
          then
            begin
              VD := TServerViewerData.Create( Viewer );
              fViewers.Insert( VD );
            end;
      finally
        Unlock;
      end;
    end;

  procedure TServerActorPool.DelViewer( Viewer : IViewer );
    var
      VD : TServerViewerData;
      i  : integer;
    begin
      Lock;
      try
        VD := GetViewerData( Viewer );
        if VD <> nil
          then
            begin
              for i := 0 to pred(fActors.Count) do
                TServerActorData(fActors[i]).fViewers.Delete( VD );
              fViewers.Delete( VD );
            end;
      finally
        Unlock;
      end;
    end;

  function TServerActorPool.CheckScopes : TCollection;
    var
      i, j : integer;
      AD   : TServerActorData;
      VD   : TServerViewerData;
      SC   : TScopeChange;
    begin
      result := TCollection.Create( 0, rkUse );
      for i := 0 to pred(fActors.Count) do
        begin
          AD := TServerActorData(fActors[i]);
          for j := 0 to pred(fViewers.Count) do
            try
              VD := TServerViewerData(fViewers[j]); //>> aqui decia i
              if VD.fViewer.IsAwareOf( AD.fActor )
                then
                  begin
                    if AD.fViewers.IndexOf( VD ) = NoIndex
                      then
                        begin
                          // The actor just entered viewer's area
                          SC := TScopeChange.Create( dtActorEntersScope, VD, AD.fActor.getId, AD.fActor );
                          result.Insert( SC );
                          AD.fViewers.Insert( VD ); //>> puesto por mi
                        end;
                  end
                else
                  begin
                    if AD.fViewers.IndexOf( VD ) <> NoIndex
                      then
                        begin
                          // The actor just leaved viewer's area
                          SC := TScopeChange.Create( dtActorLeavesScope, VD, AD.fActor.getId, AD.fActor );
                          result.Insert( SC );
                          AD.fViewers.Delete( VD ); //>> puesto por mi
                        end;
                  end
            except
            end;
        end;
    end;

  procedure TServerActorPool.SerializeData( DataType : TDataType; Source : IServerActor; const Data; TickData : TStream );
    var
      ActorScope      : TActorScopeInfo       absolute Data;
      StateTransition : TStateTransitionInfo  absolute Data;
      EventInfo       : TEventInfo            absolute Data;
      ActorKind       : TActorKind;
      AD              : TServerActorData;
      ActorId         : TActorId;
    begin
      TickData.Write( DataType, sizeof(DataType) );
      case DataType of
        dtActorPoolTick :
          begin
            TickData.Write( fTickCount, sizeof(fTickCount) );
          end;
        dtActorEntersScope :
          begin
            TickData.Write( ActorScope.ScopeChange.fActorId, sizeof(ActorScope.ScopeChange.fActorId) );
            ActorKind := ActorScope.ScopeChange.Actor.getKind;
            TickData.Write( ActorKind, sizeof(ActorKind) );
            ActorScope.ScopeChange.Actor.Store( TickData );
            TickData.Write( ActorScope.ScopeChange.Actor.getStatePool.MetaStatePool.Id, sizeof(ActorScope.ScopeChange.Actor.getStatePool.MetaStatePool.Id) );
            ActorScope.ScopeChange.Actor.getStatePool.ClientSerialize( TickData );
            LogThis( 'Server Actor entry: ' + IntToStr(ActorScope.ScopeChange.fActorId) );
          end;
        dtActorLeavesScope :
          begin
            TickData.Write( ActorScope.ScopeChange.fActorId, sizeof(ActorScope.ScopeChange.fActorId) );
            LogThis( 'Server Actor exit: ' + IntToStr(ActorScope.ScopeChange.fActorId) );
          end;
        dtStateTransition :
          begin
            AD      := GetActorFromPool( StateTransition.StatePool );
            ActorId := AD.fActor.getId;
            TickData.Write( ActorId, sizeof(ActorId) ); //>> to esto es mio

            Source.getStatePool.SerializeTransition( TickData, StateTransition.LauncherId, StateTransition.State, StateTransition.Behaviour );
          end;
        dtEvent :
          begin
            AD      := GetActorFromPool( EventInfo.StatePool );
            ActorId := AD.fActor.getId;
            TickData.Write( ActorId, sizeof(ActorId) ); //>> to esto es mio
            EventInfo.Event.Store( TickData );
          end;
      end;
    end;

  procedure TServerActorPool.ProcessTickData( ViewerId : TViewerId; TickCount : cardinal; const TickData : TStream );
    begin
      if assigned(SendTickData)
        then SendTickData( Id, ViewerId, TickCount, TickData );
    end;

  procedure TServerActorPool.PoolDistProc( DataType : TDataType; Sender : TStatePool; const DataInfo );
    var
      AD : TServerActorData;
      i  : integer;
    begin
      AD := GetActorFromPool( Sender );
      if AD <> nil
        then
          for i := 0 to pred(AD.fViewers.Count) do
            SerializeData( DataType, AD.fActor, DataInfo, TServerViewerData(AD.fViewers[i]).fTickData );
    end;

  function TServerActorPool.GetActorData( Actor : IServerActor ) : TServerActorData;
    var
      i : integer;
    begin
      i := 0;
      while (i < fActors.Count) and (TServerActorData(fActors[i]).fActor <> Actor) do
        inc( i );
      if i < fActors.Count
        then result := TServerActorData(fActors[i])
        else result := nil;
    end;

  function TServerActorPool.GetViewerData( Viewer : IViewer ) : TServerViewerData;
    var
      i : integer;
    begin
      i := 0;
      while (i < fViewers.Count) and (TServerViewerData(fViewers[i]).fViewer <> Viewer) do
        inc( i );
      if i < fViewers.Count
        then result := TServerViewerData(fViewers[i])
        else result := nil;
    end;

  function TServerActorPool.GetActorFromPool( Pool : TStatePool ) : TServerActorData;
    var
      i : integer;
    begin
      i := 0;
      while (i < fActors.Count) and (TServerActorData(fActors[i]).fActor.getStatePool <> Pool) do
        inc( i );
      if i < fActors.Count
        then result := TServerActorData(fActors[i])
        else result := nil;
    end;


  // TClientActorData

  constructor TClientActorData.Create( anActor : IClientActor );
    begin
      inherited Create;
      fActor := anActor;
    end;


  // TClientActorPool

  procedure TClientActorPool.Act( TickData : TStream );

    procedure ExecutePools;
      var
        i : integer;
      begin
        for i := 0 to pred(fActors.Count) do
          try
            TClientActorData(fActors[i]).fActor.getStatePool.PreAct( amdSynchronized );
            TClientActorData(fActors[i]).fActor.getStatePool.Act( amdSynchronized );
            TClientActorData(fActors[i]).fActor.getStatePool.PostAct( amdSynchronized );
          except
          end;
      end;

    var
      DataType : TDataType;
    begin
      Lock;
      try
        while (TickData.Position < TickData.Size) do
          begin
            TickData.Read( DataType, sizeof(DataType) );
            ParseServerData( DataType, TickData );
          end;
        ExecutePools;
      finally
        Unlock;
      end;
    end;

  procedure TClientActorPool.HighResAct;
    var
      i : integer;
    begin
      Lock;
      try
        for i := 0 to pred(fActors.Count) do
          try
            TClientActorData(fActors[i]).fActor.getStatePool.PreAct( amdHighRes );
            TClientActorData(fActors[i]).fActor.getStatePool.Act( amdHighRes );
            TClientActorData(fActors[i]).fActor.getStatePool.PostAct( amdHighRes );
          except
          end;
      finally
        Unlock;
      end;
    end;

  procedure TClientActorPool.ParseServerData( DataType : TDataType; TickData : TStream );
    var
      ActorId    : TActorId;
      ActorKind  : TActorKind;
      AD         : TClientActorData;
      NewActor   : IClientActor;
      MetaPoolId : TMetaPoolId;
      MetaPool   : TMetaStatePool;
      NewPool    : TClientStatePool;
      Event      : TDistributedEvent;
    begin
      case DataType of
        dtActorPoolTick :
          begin
            TickData.Read( fTickCount, sizeof(fTickCount) );
            LogThis( 'Client TickCount: ' + IntToStr(fTickCount) );
          end;
        dtActorEntersScope :
          begin
            TickData.Read( ActorId, sizeof(ActorId) );
            TickData.Read( ActorKind, sizeof(ActorKind) );
            NewActor := ClientActorFactory( ActorKind, ActorId );
            NewActor.Load( TickData );
            TickData.Read( MetaPoolId, sizeof(MetaPoolId) );
            MetaPool := MetaPoolLocator( MetaPoolId );
            NewPool := TClientStatePool.LoadFromServer( TickData, MetaPool, 1 );
            NewActor.setStatePool( NewPool );
            fActors.Insert( TClientActorData.Create( NewActor ) );
            NewActor.Inserted;
            LogThis( 'Client Actor entry: ' + IntToStr(ActorId) );
          end;
        dtActorLeavesScope :
          begin
            TickData.Read( ActorId, sizeof(ActorId) );
            AD := GetActorFromId( ActorId );
            if AD <> nil
              then
                begin
                  AD.fActor.Deleted;
                  fActors.Delete( AD );
                end;
            LogThis( 'Client Actor exit: ' + IntToStr(ActorId) );
          end;
        dtStateTransition :
          begin
            TickData.Read( ActorId, sizeof(ActorId) );
            AD := GetActorFromId( ActorId );
            AD.fActor.getStatePool.LoadTransition( TickData );
          end;
        dtEvent :
          begin
            TickData.Read( ActorId, sizeof(ActorId) );
            AD    := GetActorFromId( ActorId );
            Event := TDistributedEvent.Load( TickData );
            AD.fActor.getStatePool.ThrowEvent( nil, Event );
          end;
      end;
    end;

  function TClientActorPool.GetActorFromId( Id : TActorId ) : TClientActorData;
    var
      i : integer;
    begin
      i := 0;
      while (i < fActors.Count) and (TClientActorData(fActors[i]).fActor.getId <> Id) do
        inc( i );
      if i < fActors.Count
        then result := TClientActorData(fActors[i])
        else result := nil;
    end;

end.



