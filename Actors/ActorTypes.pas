unit ActorTypes;

interface

  uses
    StateEngine, DistributedStates, Classes;

  type
    TActorId   = integer;
    TActorKind = integer;
    TViewerId  = integer;

  type
    IActor  = interface;
    IViewer = interface;

    IActor =
      interface
        function getId   : TActorId;
        function getKind : TActorKind;
      end;

    IServerActor =
      interface( IActor )
        function  getStatePool : TServerStatePool;
        procedure Store( Stream : TStream );
        function  getContext : pointer;
      end;

    IClientActor =
      interface( IActor )
        function  getStatePool : TClientStatePool;
        procedure setStatePool( aStatePool : TClientStatePool );
        procedure Inserted;
        procedure Deleted;
        procedure Load( Stream : TStream );
      end;

    IViewer =
      interface
        function getId : TViewerId;
        function IsAwareOf( Actor : IServerActor ) : boolean;
      end;

implementation

end.

