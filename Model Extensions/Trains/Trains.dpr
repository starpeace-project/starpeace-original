library Trains;

uses
  ShareMem,
  SysUtils,
  Classes,
  World in '..\..\Kernel\World.pas',
  Kernel in '..\..\Kernel\Kernel.pas',
  Protocol in '..\..\Protocol\Protocol.pas',
  TransportInterfaces in '..\..\Transport\TransportInterfaces.pas',
  VisualClassManager in '..\..\Class Packer\VisualClassManager.pas',
  NewsServerInterfaces in '..\..\News Server\NewsServerInterfaces.pas',
  Transport in '..\..\Transport\Transport.pas',
  MatrixLayer in '..\..\Transport\MatrixLayer.pas',
  ServerTrain in 'ServerTrain.pas',
  Train in 'Train.pas',
  Collection in '..\..\Kernel\Collection.pas',
  Vehicles in '..\..\Voyager\Vehicles.pas',
  ActorPool in '..\..\Actors\ActorPool.pas',
  TrainsSystem in 'TrainsSystem.pas',
  Automaton in '..\..\Actors\Automaton.pas',
  Circuits in '..\..\Circuits\Circuits.pas',
  TrainConst in 'TrainConst.pas',
  StateEngine in '..\..\Actors\StateEngine.pas',
  Population in '..\..\Kernel\Population.pas',
  StdFluids in '..\..\StdBlocks\StdFluids.pas',
  FacIds in '..\FacIds.pas',
  Standards in '..\Standards.pas',
  Logs in '..\..\Logs\Logs.pas',
  Inventions in '..\..\Inventions\Inventions.pas',
  RankProtocol in '..\..\Protocol\RankProtocol.pas',
  GenIdd in '..\..\Utils\Serial\GenIdd.pas',
  Tasks in '..\..\Tasks\Tasks.pas',
  InformativeTask in '..\..\Tasks\InformativeTask.pas',
  HeadquarterTasks in '..\..\Tasks\HeadquarterTasks.pas',
  DirectoryServerProtocol in '..\..\DServer\DirectoryServerProtocol.pas';

function ModelExtensionId : string; export;
  begin
    result := 'Trains';
  end;

function GetDependances : string; export;
  begin
    result := 'GeneralPack1';
  end;

procedure RegisterModelExtension;
  var
    CarBehavior : TMetaStatePool;
  begin
    with TCluster.Create( 'X-Trains' ) do
      begin
        CompanyClass := TRailRoadCompany;
        Register( tidClassFamily_Clusters );
      end;

    TMetaLocomotive.Create(
      carA30,
      'A-30 Engine',
      '',
      50000,
      30000,
      100,
      vcarA30,
      20000,
      MaxSpeed,
      TLocomotive ).Register;
    TMetaCargoCar.Create(
      carRefrigeratedA,
      'Refrigerated Wagon',
      '',
      10000,
      30000,
      100,
      vcarRefrigeratedA,
      tidFluid_FreshFood,
      1000,
      TCargoCar ).Register;

    // Define automatons
    CarBehavior := TMetaStatePool.Create( pstCarBehaviorId );
    CarBehavior.AddMetaState( TMetaState.Create( carstRunning, TAutomatedState, [0], msoDistributable ));
    CarBehavior.AddMetaState( TMetaState.Create( carstLoading, TAutomatedState, [0], msoDistributable ));
    TMetaServerAutomaton.Create(
      carA30,
      poolIdTrains,
      TServerAutomaton,
      CarBehavior,
      nil ).Register;
    TMetaServerAutomaton.Create(
      carRefrigeratedA,
      poolIdTrains,
      TServerAutomaton,
      CarBehavior,
      nil ).Register;

    ServerTrain.RegisterBackup;
    TrainsSystem.RegisterBackup;
  end;

var
  TrainSystem : TTrainsSystem;

procedure RegisterWorldExtension( World : TWorld; WorldLoaded : boolean );
  //var
    //Fac : TFacility;
    //TrainId : integer;
  begin
    if not WorldLoaded
      then
        begin
          TrainSystem := TTrainsSystem.Create( World );
          World.RegisterWorldExtension( TrainSystem );
          (*
          // just testing...
          World.RDONewTycoon( 'Cepero', 'Cepero' );
          World.RDONewCompany( 'Cepero', 'Coast to Coast', 'X-Trains' );
          *)
        end
      else
        begin
          TrainSystem := TTrainsSystem(World.WorldExtension[tidRDOHook_Trains]);
          (*
          // just testing...
          Fac := TInhabitedTown(World.TownByName['New Venice']).TownHall;

          // Train 1
          TrainSystem.RDOTrainCreate( World.CompanyByName['Coast to Coast'].Id, carA30, Fac.CurrBlock.xOrigin, Fac.CurrBlock.yOrigin );
          TrainId := TrainSystem.RDOTrainId( World.CompanyByName['Coast to Coast'].Id, 0 );
          TrainSystem.RDOTrainAddCar( World.CompanyByName['Coast to Coast'].Id, TrainId, carRefrigeratedA, 1 );
          TrainSystem.RDOTrainAddCar( World.CompanyByName['Coast to Coast'].Id, TrainId, carRefrigeratedA, 2 );
          TrainSystem.RDOTrainAddCar( World.CompanyByName['Coast to Coast'].Id, TrainId, carRefrigeratedA, 3 );
          TrainSystem.RDOTrainAddCar( World.CompanyByName['Coast to Coast'].Id, TrainId, carRefrigeratedA, 4 );
          TrainSystem.RDOTrainAddCar( World.CompanyByName['Coast to Coast'].Id, TrainId, carRefrigeratedA, 5 );

          TrainSystem.RDOTrainAddSchedulePoint( World.CompanyByName['Coast to Coast'].Id, TrainId, 419, 379, 0 );
          TrainSystem.RDOTrainAddSchedulePoint( World.CompanyByName['Coast to Coast'].Id, TrainId, 385, 369, 1 );

          // Train 2
          {
          TrainSystem.RDOTrainCreate( World.CompanyByName['Coast to Coast'].Id, carA30, 429, 385 );
          TrainId := TrainSystem.RDOTrainId( World.CompanyByName['Coast to Coast'].Id, 1 );
          TrainSystem.RDOTrainAddCar( World.CompanyByName['Coast to Coast'].Id, TrainId, carRefrigeratedA, 1 );
          TrainSystem.RDOTrainAddCar( World.CompanyByName['Coast to Coast'].Id, TrainId, carRefrigeratedA, 2 );
          TrainSystem.RDOTrainAddCar( World.CompanyByName['Coast to Coast'].Id, TrainId, carRefrigeratedA, 3 );
          }
          {
          TrainSystem.RDOTrainAddCar( World.CompanyByName['Coast to Coast'].Id, TrainId, carRefrigeratedA, 4 );
          TrainSystem.RDOTrainAddCar( World.CompanyByName['Coast to Coast'].Id, TrainId, carRefrigeratedA, 5 );
          }
          {
          TrainSystem.RDOTrainAddSchedulePoint( World.CompanyByName['Coast to Coast'].Id, TrainId, 385, 369, 0 );
          TrainSystem.RDOTrainAddSchedulePoint( World.CompanyByName['Coast to Coast'].Id, TrainId, 419, 379, 0 );
          }
          *)
        end;
  end;

exports
  ModelExtensionId,
  GetDependances,
  RegisterModelExtension,
  RegisterWorldExtension;

{$E mdx}

begin
end.
