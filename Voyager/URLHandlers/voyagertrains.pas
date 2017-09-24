unit VoyagerTrains;

interface

  uses
    ActorTypes, Automaton, StateEngine;

  procedure RegisterMetaData;

  function MetaPoolLocator( MetaPoolId : TMetaPoolId ) : TMetaStatePool;
  function ClientAutomatableFactory( Kind : TActorKind; const Context ) : IClientAutomatable;

implementation

  uses
    SysUtils, ClassStorage, Train, TrainConst, ClientTrain, Protocol, TransportHandler;

  var
    CarBehavior : TMetaStatePool = nil;

  procedure RegisterMetaData;
    begin
      TMetaCar.Create(
        carA30,
        'A-30 Engine',
        '',
        50000,
        30000,
        100,
        vcarA30,
        TClientCar ).Register;
      TMetaCar.Create(
        carRefrigeratedA,
        'Refrigerated Wagon',
        '',
        10000,
        30000,
        100,
        vcarRefrigeratedA,
        TClientCar ).Register;

      // Define automatons
      CarBehavior := TMetaStatePool.Create( pstCarBehaviorId );
      CarBehavior.AddMetaState( TMetaState.Create( carstRunning, TAutomatedState, [0], msoDistributable ));
      CarBehavior.AddMetaState( TMetaState.Create( carstLoading, TAutomatedState, [0], msoDistributable ));
      TMetaClientAutomaton.Create(
        carA30,
        poolIdTrains,
        TClientAutomaton,
        CarBehavior,
        ClientAutomatableFactory ).Register;
      TMetaClientAutomaton.Create(
        carRefrigeratedA,
        poolIdTrains,
        TClientAutomaton,
        CarBehavior,
        ClientAutomatableFactory ).Register;
    end;

  function MetaPoolLocator( MetaPoolId : TMetaPoolId ) : TMetaStatePool;
    begin
      result := CarBehavior;
    end;

  function ClientAutomatableFactory( Kind : TActorKind; const Context ) : IClientAutomatable;
    var
      MetaCar          : TMetaCar;
      Car              : TClientCar;
      TransportHandler : TTransportHandler absolute Context;
    begin
      MetaCar := TMetaCar(TheClassStorage.ClassById[tidClassFamily_TrainCars, IntToStr(Kind)]);
      if MetaCar <> nil
        then
          begin
            Car := TClientCar(MetaCar.Instantiate);
            Car.OnClientTrainModified := TransportHandler.ClientCarModified;
            result := Car;
          end
        else result := nil
    end;

end.
