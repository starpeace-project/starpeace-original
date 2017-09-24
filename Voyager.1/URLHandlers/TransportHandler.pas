unit TransportHandler;

interface

  uses
    VoyagerInterfaces, VoyagerServerInterfaces, Controls, ActorPool, Vehicles, StateEngine,
    ExtCtrls, ActorTypes, Automaton, Collection, ClientTrain;

  type
    TTransportHandler =
      class( TInterfacedObject, IMetaURLHandler, IURLHandler, IVehicleArray )
        public
          constructor Create( anActorPoolId    : TActorPoolId;
                              HighResFreq      : integer;
                              aMetaPoolLocator : TMetaPoolLocator );
          destructor  Destroy; override;
        private
          fActorPool          : TClientActorPool;
          fAutomatons         : TLockableCollection;
          fHighResTimer       : TTimer;
        private
          function  ClientActorFactory( ActorKind : TActorKind; ActorId : TActorId ) : IClientActor;
          //function  MetaPoolLocator( MetaPoolId : TMetaPoolId ) : TMetaStatePool;
          procedure ClientAutomatonModified( Automaton : TClientAutomaton; Operation : TAutomationOperation );
        public
          procedure ClientCarModified( Car : TClientCar; Modification : TClientCarModification );
        // IMetaURLHandler
        private
          function getName    : string;
          function getOptions : TURLHandlerOptions;
          function getCanHandleURL( URL : TURL ) : THandlingAbility;
          function Instantiate : IURLHandler;
        // IURLHandler
        private
          function  HandleURL( URL : TURL ) : TURLHandlingResult;
          function  HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
          function  getControl : TControl;
          procedure setMasterURLHandler( URLHandler : IMasterURLHandler );
        private
          fMasterURLHandler : IMasterURLHandler;
        // IVehicleArray
        private
          function  getVehicleCount : integer;
          function  getVehicle( index : integer ) : IVehicle;
          procedure RegisterNotificationProc  ( aOnArrayChanged : TOnVehicleArrayChanged );
          procedure UnregisterNotificationProc( aOnArrayChanged : TOnVehicleArrayChanged );
        private
          fOnArrayChanged : TOnVehicleArrayChanged;
        private
          procedure OnHighResTick( Sender : TObject );
      end;

  const
    tidMetaHandler_Transport = 'Transport';

implementation

  uses
    SysUtils, ClassStorage, Events, ServerCnxEvents;


  //  TTransportHandler

  constructor TTransportHandler.Create( anActorPoolId : TActorPoolId; HighResFreq : integer; aMetaPoolLocator : TMetaPoolLocator );
    begin
      inherited Create;
      fActorPool                    := TClientActorPool.Create( anActorPoolId );
      fActorPool.ClientActorFactory := ClientActorFactory;
      fActorPool.MetaPoolLocator    := aMetaPoolLocator;
      fAutomatons                   := TLockableCollection.Create( 0, rkUse );
      fHighResTimer                 := TTimer.Create( nil );
      fHighResTimer.Interval        := HighResFreq;
      fHighResTimer.OnTimer         := OnHighResTick;
    end;

  destructor TTransportHandler.Destroy;
    begin
      fHighResTimer.Free;
      fActorPool.Free;
      fAutomatons.Free;
      inherited;
    end;

  function TTransportHandler.ClientActorFactory( ActorKind : TActorKind; ActorId : TActorId ) : IClientActor;
    var
      MA : TMetaClientAutomaton;
      CA : TClientAutomaton;
    begin
      MA := TMetaClientAutomaton(TheClassStorage.ClassById[IntToStr(fActorPool.Id), IntToStr(ActorKind)]);
      if MA <> nil
        then
          begin
            CA := MA.Instantiate( ActorId, self );
            CA.OnAutomatonModified := ClientAutomatonModified;
            CA.Automatable := MA.InstantiateAutomatable( self );
            fAutomatons.Insert( CA );
            result := CA;
          end
        else result := nil;
    end;

  {
  function TTransportHandler.MetaPoolLocator( MetaPoolId : TMetaPoolId ) : TMetaStatePool;
    var
      MA : TMetaClientAutomaton;
    begin
      MA := TMetaClientAutomaton(TheClassStorage.ClassById[IntToStr(fActorPool.Id), IntToStr(MetaPoolId)]);
      if MA <> nil
        then result := MA.Behavior
        else result := nil;
    end;
  }
  
  procedure TTransportHandler.ClientAutomatonModified( Automaton : TClientAutomaton; Operation : TAutomationOperation );
    begin
      if Operation = aopDeleted
        then fAutomatons.Delete( Automaton );
    end;

  procedure TTransportHandler.ClientCarModified( Car : TClientCar; Modification : TClientCarModification );
    var
      Vehicle : IVehicle;
    begin
      if (Modification = ccmDeleted) and assigned(fOnArrayChanged)
        then
          begin
            Vehicle := Car;
            fOnArrayChanged( self, achVehicleDeletion, Vehicle );
          end;
    end;
    
  function TTransportHandler.getName : string;
    begin
      result := tidMetaHandler_Transport + IntToStr(fActorPool.Id);
    end;

  function TTransportHandler.getOptions : TURLHandlerOptions;
    begin
      result := [hopCacheable, hopNonVisual];
    end;

  function TTransportHandler.getCanHandleURL( URL : TURL ) : THandlingAbility;
    begin
      result := 0;
    end;

  function TTransportHandler.Instantiate : IURLHandler;
    begin
      result := self;
    end;

  function TTransportHandler.HandleURL( URL : TURL ) : TURLHandlingResult;
    begin
      result := urlNotHandled;
    end;

  function TTransportHandler.HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
    var
      AnswerVehicleArrayInfo : TMsgAnswerVehicleArrayInfo absolute info;
      ActorPoolModifiedInfo  : TMsgActorPoolModifiedInfo  absolute info;
    begin
      result := evnHandled;
      case EventId of
        evnAnswerVehicleArray :
          if AnswerVehicleArrayInfo.ActorPoolId = fActorPool.Id
            then AnswerVehicleArrayInfo.VehicleArray := self;
        evnActorPoolModified :
          if ActorPoolModifiedInfo.ActorPoolId = fActorPool.Id
            then fActorPool.Act( ActorPoolModifiedInfo.Data );
        evnShutDown :
          fOnArrayChanged := nil;
        else
          result := evnNotHandled;
      end;
    end;

  function TTransportHandler.getControl : TControl;
    begin
      result := nil;
    end;

  procedure TTransportHandler.setMasterURLHandler( URLHandler : IMasterURLHandler );
    begin
      fMasterURLHandler := URLHandler;
    end;

  function TTransportHandler.getVehicleCount : integer;
    begin
      result := fAutomatons.Count;
    end;

  function TTransportHandler.getVehicle( index : integer ) : IVehicle;
    var
      A   : TClientAutomaton;
      Msg : TMsgAnswerVehicle;
    begin
      A := TClientAutomaton(fAutomatons[index]);
      Msg.MsgId := msgAnswerVehicle;
      A.Automatable.Dispatch( Msg );
      result := Msg.Vehicle;
    end;

  procedure TTransportHandler.RegisterNotificationProc( aOnArrayChanged : TOnVehicleArrayChanged );
    begin
      fOnArrayChanged := aOnArrayChanged;
    end;

  procedure TTransportHandler.UnregisterNotificationProc( aOnArrayChanged : TOnVehicleArrayChanged );
    begin
      fOnArrayChanged := nil;
    end;

  procedure TTransportHandler.OnHighResTick( Sender : TObject );
    begin
      fActorPool.HighResAct;
      if assigned(fOnArrayChanged)
        then fOnArrayChanged( self, achUpdate, self );
    end;


end.

