{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W+,X+,Y-,Z1}
{$MINSTACKSIZE $00004000}
{$MAXSTACKSIZE $00100000}
{$IMAGEBASE $00400000}
{$APPTYPE GUI}

{$DEFINE USELogs}

library UWPack1;

uses
  ShareMem,
  SysUtils,
  Windows,
  ClassStorage,
  Classes,
  Kernel in '..\..\Kernel\Kernel.pas',
  StdFluids in '..\..\StdBlocks\StdFluids.pas',
  Warehouses in '..\..\StdBlocks\Warehouses.pas',
  UWConst in 'UWConst.pas',
  ColdStorage in '..\..\StdBlocks\ColdStorage.pas',
  Construction in '..\..\StdBlocks\Construction.pas',
  ChemicalStorage in '..\..\StdBlocks\ChemicalStorage.pas',
  GeneralStorage in '..\..\StdBlocks\GeneralStorage.pas',
  CacheCommon in '..\..\Cache\CacheCommon.pas',
  Standards in '..\Standards.pas',
  OutputEvaluators in '..\..\Kernel\OutputEvaluators.pas',
  Land in '..\..\Land\Land.pas',
  NewsServerInterfaces in '..\..\News Server\NewsServerInterfaces.pas',
  TransportInterfaces in '..\..\Transport\TransportInterfaces.pas',
  Transport in '..\..\Transport\Transport.pas',
  MatrixLayer in '..\..\Transport\MatrixLayer.pas',
  VisualClassManager in '..\..\Class Packer\VisualClassManager.pas',
  ActorPool in '..\..\Actors\ActorPool.pas',
  StateEngine in '..\..\Actors\StateEngine.pas',
  DistributedStates in '..\..\Actors\DistributedStates.pas',
  Automaton in '..\..\Actors\Automaton.pas',
  ActorTypes in '..\..\Actors\ActorTypes.pas',
  FacIds in '..\FacIds.pas',
  Logs in '..\..\Logs\Logs.pas',
  ModelServerCache in '..\..\Cache\ModelServerCache.pas',
  Inventions in '..\..\Inventions\Inventions.pas',
  FabricsStorage in '..\..\StdBlocks\FabricsStorage.pas',
  RankProtocol in '..\..\Protocol\RankProtocol.pas',
  GenIdd in '..\..\Utils\Serial\GenIdd.pas',
  OreStorage in '..\..\StdBlocks\OreStorage.pas',
  SimMLS in '..\..\Kernel\SimMLS.pas',
  DirectoryServerProtocol in '..\..\DServer\DirectoryServerProtocol.pas';

procedure RegisterInventions;
  begin
  end;

procedure RegisterFacilityKinds;
  begin
    with TFacilityKind.Create( tidFacilityKind_Warehouse ) do
      begin
        Name        := 'Warehouses';
        SuperType   := tidSuperFacKind_Warehouse;
        ClusterName := tidClusterName_UW;
        Role        := rolDistributer;
        Register( tidClassFamily_FacilityKinds );
      end;
  end;

procedure RegisterClusterFacilities;
  begin
    TCluster.Create( tidClusterName_UW ).Register( tidClassFamily_Clusters );
  end;

procedure RegisterHeadquarters;
  begin
  end;

procedure RegisterWarehouses;
  begin
    // Cold Storage
    with TMetaBlockUnderConstruction.Create(
      tidBlock_UWColdStorageConstr,
      1000000, //10000000,
      [60, 30, 10],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaColdStorage.Create(
      tidBlock_UWColdStorage,
      [0, 0, 10],
      1000000,
      250000,
      250000,
      150,
      TColdStorage ) do
      begin
        VisualStages := 3;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_UWColdStorage, 'Cold Storage', vidFacility_UWColdStorage, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 100;
        Desc  := '';
        FacId := FID_ColdWarehouse;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_UWColdStorageConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The warehouse is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_UWColdStorage])));
        ClusterName := tidClusterName_UW;
        FacilityKind := tidFacilityKind_Warehouse;
        Register( tidClassFamily_Facilities );
      end;

    // Super Cold Storage
    with TMetaBlockUnderConstruction.Create(
      tidBlock_UWSuperColdStorageConstr,
      3000000, //100000000,
      [60, 30, 10],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaColdStorage.Create(
      tidBlock_UWSuperColdStorage,
      [0, 0, 12],
      20000000,
      2500000,
      2500000,
      150,
      TColdStorage ) do
      begin
        VisualStages := 3;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_UWSuperColdStorage, 'Super Cold Storage', vidFacility_UWSuperColdStorage, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 100;
        Desc := '';
        FacId := FID_ColdWarehouse;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_UWSuperColdStorageConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The warehouse is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_UWSuperColdStorage])));
        ClusterName  := tidClusterName_UW;
        FacilityKind := tidFacilityKind_Warehouse;
        Register( tidClassFamily_Facilities );
      end;

    // Chemical Storage
    with TMetaBlockUnderConstruction.Create(
      tidBlock_UWChemicalStorageConstr,
      1000000,
      [60, 30, 10],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaChemicalStorage.Create(
      tidBlock_UWChemicalStorage,
      [0, 0, 10],
      100000,
      100000,
      100000,
      150,
      TChemicalStorage ) do
      begin
        VisualStages := 3;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_UWChemicalStorage, 'Chemical Storage', vidFacility_UWChemicalStorage, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 100;
        Desc := '';
        FacId := FID_ChemicalWarehouse;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_UWChemicalStorageConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The warehouse is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_UWChemicalStorage])));
        ClusterName := tidClusterName_UW;
        FacilityKind := tidFacilityKind_Warehouse;
        Register( tidClassFamily_Facilities );
      end;

    // General Storage
    with TMetaBlockUnderConstruction.Create(
      tidBlock_UWGeneralStorageConstr,
      1000000, //20000000,
      [60, 30, 10],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaGeneralStorage.Create(
      tidBlock_UWGeneralStorage,
      [0, 1, 80],
      10000,  // OrgMat
      10000,  // Fab&Threads
      10000,  // Clothes
      10000,  // Elect Component
      10000,  // HHA
      10000,  // Business Machines
      25000,  // Metals
      160,    // Cars
      10000,  // Toys
      10000,  // Drugs
      20000,  // Plastics
      1000,   // Machinery
      15000,  // Furniture
      10000,  // Books
      10000,  // CDs
      150,    // overprice
      TGeneralStorage ) do
      begin
        VisualStages := 3;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_UWGeneralStorage, 'General Storage', vidFacility_UWGeneralStorage, TFacility ) do
      begin
        xSize := 6;
        ySize := 6;
        Level := 1100;
        Desc  := '';
        FacId := FID_GeneralWarehouse;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_UWGeneralStorageConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The warehouse is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_UWGeneralStorage])));
        ClusterName := tidClusterName_UW;
        FacilityKind := tidFacilityKind_Warehouse;
        Register( tidClassFamily_Facilities );
      end;

    // Fabrics Storage
    with TMetaBlockUnderConstruction.Create(
      tidBlock_UWFabricsStorageConstr,
      1000000, //20000000,
      [60, 30, 10],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFabricsStorage.Create(
      tidBlock_UWFabricsStorage,
      [0, 0, 12],
      20000000,
      250000,
      100000,
      250000,
      250000,
      150,
      TFabricsStorage ) do
      begin
        VisualStages := 3;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_UWFabricsStorage, 'Fabrics Storage', vidFacility_UWFabricsStorage, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 100;
        Desc  := '';
        FacId := FID_FabricsWarehouse;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_UWFabricsStorageConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The warehouse is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_UWFabricsStorage])));
        ClusterName := tidClusterName_UW;
        FacilityKind := tidFacilityKind_Warehouse;
        Register( tidClassFamily_Facilities );
      end;

    // Ore Storage
    with TMetaBlockUnderConstruction.Create(
      tidBlock_UWOreStorageConstr,
      1000000, //20000000
      [60, 30, 10],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaOreStorage.Create(
      tidBlock_UWOreStorage,
      [0, 0, 12],
      200000,
      50000,
      500000,
      500000,
      150,
      TOreStorage ) do
      begin
        VisualStages := 3;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_UWOreStorage, 'Ore Storage', vidFacility_UWOreStorage, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 100;
        Desc := '';
        FacId := FID_OreWarehouse;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_UWOreStorageConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The warehouse is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_UWOreStorage])));
        ClusterName := tidClusterName_UW;
        FacilityKind := tidFacilityKind_Warehouse;
        Register( tidClassFamily_Facilities );
      end;
  end;

procedure RegisterPackFacilities;
  begin
    RegisterHeadquarters;
    RegisterWarehouses;
  end;

function ModelExtensionId : string; export;
  begin
    result := 'UWPack1';
  end;

function GetDependances : string; export;
  begin
    result := 'GeneralPack1';
  end;
                                
procedure RegisterModelExtension; export;
  begin
    SimMLS.LoadMLS;
    InitVisualClasses;
    RegisterInventions;
    RegisterFacilityKinds;
    RegisterClusterFacilities;
    RegisterPackFacilities;
  end;

procedure PostRegisterModelExtension; export;
  begin
    ModelServerCache.EnabledLogs := true;
  end;

exports
  ModelExtensionId,
  GetDependances,
  RegisterModelExtension,
  PostRegisterModelExtension;

{$E mdx}

begin
end.


