{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W+,X+,Y-,Z1}
{$MINSTACKSIZE $00004000}
{$MAXSTACKSIZE $00100000}
{$IMAGEBASE $00400000}
{$APPTYPE GUI}

{$DEFINE USELogs}

library GeneralPack1;

uses
  ShareMem,
  SysUtils,
  Windows,
  ClassStorage,
  Classes,
  Kernel in '..\..\Kernel\Kernel.pas',
  Population in '..\..\Kernel\Population.pas',
  Trade in '..\..\Kernel\Trade.pas',
  Protocol in '..\..\Protocol\Protocol.pas',
  Construction in '..\..\StdBlocks\Construction.pas',
  BusinessMachines in '..\..\StdBlocks\BusinessMachines.pas',
  CarIndustry in '..\..\StdBlocks\CarIndustry.pas',
  CarShop in '..\..\StdBlocks\CarShop.pas',
  Chemical in '..\..\StdBlocks\Chemical.pas',
  ClothesShop in '..\..\StdBlocks\ClothesShop.pas',
  Clothings in '..\..\StdBlocks\Clothings.pas',
  ConstructionIndustry in '..\..\StdBlocks\ConstructionIndustry.pas',
  ElectronicIndustry in '..\..\StdBlocks\ElectronicIndustry.pas',
  EvaluatedBlock in '..\..\StdBlocks\EvaluatedBlock.pas',
  Farms in '..\..\StdBlocks\Farms.pas',
  FoodProcessor in '..\..\StdBlocks\FoodProcessor.pas',
  HeavyIndustry in '..\..\StdBlocks\HeavyIndustry.pas',
  HouseHoldingAppliances in '..\..\StdBlocks\HouseHoldingAppliances.pas',
  MetalIndustry in '..\..\StdBlocks\MetalIndustry.pas',
  CoalMine in '..\..\StdBlocks\CoalMine.pas',
  StdFluids in '..\..\StdBlocks\StdFluids.pas',
  TextilIndustry in '..\..\StdBlocks\TextilIndustry.pas',
  PopulatedBlock in '..\..\Kernel\PopulatedBlock.pas',
  ResearchCenter in '..\..\Kernel\ResearchCenter.pas',
  Headquarters in '..\..\Kernel\Headquarters.pas',
  OutputEvaluators in '..\..\Kernel\OutputEvaluators.pas',
  ServiceBlock in '..\..\StdBlocks\ServiceBlock.pas',
  FoodStore in '..\..\StdBlocks\FoodStore.pas',
  Bar in '..\..\StdBlocks\Bar.pas',
  HHAStore in '..\..\StdBlocks\HHAStore.pas',
  SuperMarket in '..\..\StdBlocks\SuperMarket.pas',
  Circuits in '..\..\Circuits\Circuits.pas',
  Restaurant in '..\..\StdBlocks\Restaurant.pas',
  PublicFacility in '..\..\Kernel\PublicFacility.pas',
  Computing in '..\..\StdBlocks\Computing.pas',
  LegalServices in '..\..\StdBlocks\LegalServices.pas',
  Standards in '..\Standards.pas',
  Warehouses in '..\..\StdBlocks\Warehouses.pas',
  ColdStorage in '..\..\StdBlocks\ColdStorage.pas',
  ChemicalStorage in '..\..\StdBlocks\ChemicalStorage.pas',
  GeneralStorage in '..\..\StdBlocks\GeneralStorage.pas',
  Environmental in '..\..\StdBlocks\Environmental.pas',
  SimHints in '..\..\Kernel\SimHints.pas',
  WorkCenterBlock in '..\..\Kernel\WorkCenterBlock.pas',
  Land in '..\..\Land\Land.pas',
  BasicAccounts in '..\..\Kernel\BasicAccounts.pas',
  StdAccounts in '..\..\StdBlocks\StdAccounts.pas',
  Accounts in '..\..\Kernel\Accounts.pas',
  NewsServerInterfaces in '..\..\News Server\NewsServerInterfaces.pas',
  PolluterWorkCenter in '..\..\StdBlocks\PolluterWorkCenter.pas',
  Movie in '..\..\StdBlocks\Movie.pas',
  TransportInterfaces in '..\..\Transport\TransportInterfaces.pas',
  Transport in '..\..\Transport\Transport.pas',
  MatrixLayer in '..\..\Transport\MatrixLayer.pas',
  VisualClassManager in '..\..\Class Packer\VisualClassManager.pas',
  FacIds in '..\FacIds.pas',
  TownPolitics in '..\..\Kernel\TownPolitics.pas',
  WorldPolitics in '..\..\Kernel\WorldPolitics.pas',
  World in '..\..\Kernel\World.pas',
  Collection in '..\..\Kernel\Collection.pas',
  Ministers in '..\..\Kernel\Ministers.pas',
  Logs in '..\..\Logs\Logs.pas',
  ModelServerCache in '..\..\Cache\ModelServerCache.pas',
  Inventions in '..\..\Inventions\Inventions.pas',
  Seasons in '..\..\Kernel\Seasons.pas',
  FabricsStorage in '..\..\StdBlocks\FabricsStorage.pas',
  OreStorage in '..\..\StdBlocks\OreStorage.pas',
  PharmaIndustry in '..\..\StdBlocks\PharmaIndustry.pas',
  DrugStore in '..\..\StdBlocks\DrugStore.pas',
  ToyFactory in '..\..\StdBlocks\ToyFactory.pas',
  ToyStore in '..\..\StdBlocks\ToyStore.pas',
  OilRig in '..\..\StdBlocks\OilRig.pas',
  Refinery in '..\..\StdBlocks\Refinery.pas',
  GasStation in '..\..\StdBlocks\GasStation.pas',
  PlasticInd in '..\..\StdBlocks\PlasticInd.pas',
  LiquorFact in '..\..\StdBlocks\LiquorFact.pas',
  RankProtocol in '..\..\Protocol\RankProtocol.pas',
  GenIdd in '..\..\Utils\Serial\GenIdd.pas',
  Mine in '..\..\StdBlocks\Mine.pas',
  StoneMine in '..\..\StdBlocks\StoneMine.pas',
  ChemMine in '..\..\StdBlocks\ChemMine.pas',
  SiliconMine in '..\..\StdBlocks\SiliconMine.pas',
  SimMLS in '..\..\Kernel\SimMLS.pas',
  Languages in '..\..\Kernel\Languages.pas',
  VCLBackup in '..\..\Persistence\VCLBackup.pas',
  UWConst in '..\UW\UWConst.pas',
  MagnaMarket in '..\..\StdBlocks\MagnaMarket.pas',
  FiveMem in '..\..\Kernel\FiveMem.pas',
  TranscendBlock in '..\..\StdBlocks\TranscendBlock.pas',
  TycoonLevels in '..\..\Kernel\TycoonLevels.pas',
  LumberMill in '..\..\StdBlocks\LumberMill.pas',
  FurnitureIndustry in '..\..\StdBlocks\FurnitureIndustry.pas',
  FurnitureStore in '..\..\StdBlocks\FurnitureStore.pas',
  BookStore in '..\..\StdBlocks\BookStore.pas',
  ComputerStore in '..\..\StdBlocks\ComputerStore.pas',
  MovieStudios in '..\..\StdBlocks\MovieStudios.pas',
  FuneralParlors in '..\..\StdBlocks\FuneralParlors.pas',
  PaperIndustry in '..\..\StdBlocks\PaperIndustry.pas',
  PrintingPlant in '..\..\StdBlocks\PrintingPlant.pas',
  CDPlant in '..\..\StdBlocks\CDPlant.pas',
  CDStore in '..\..\StdBlocks\CDStore.pas',
  Portal in '..\..\Kernel\Portal.pas',
  MegaWarehouse in '..\..\StdBlocks\MegaWarehouse.pas',
  CacheCommon in '..\..\Cache\CacheCommon.pas';

function ModelExtensionId : string; export;
  begin
    result := 'GeneralPack1';
  end;

function GetDependances : string; export;
  begin
    result := '';
  end;

procedure RegisterInventions;
  begin
    {
    // Distributed Direction
    with TInvention.Create( nidInvention_DistributedDirection, 'Distributed Direction', tidInventionKind_Direction, 'Main Headquarters' ) do
      begin
        Price := 2000000;
        Time  := tmeInvention_Short;
        Register( tidClassFamily_Inventions );
      end;
    TheClassStorage.RegisterEqv( tidClassFamily_Inventions, tidInventionKind_DistributedDirection, GetInventionId( nidInvention_DistributedDirection, tidInventionKind_Direction ));

    // Basic Direction Technics
    with TInvention.Create( nidInvention_BasicDirectionTechnics, 'Basic Direction Technics', tidInventionKind_Direction, 'Main Headquarters' ) do
      begin
        Price := 2000000;
        Time  := tmeInvention_Short;
        Register( tidClassFamily_Inventions );
      end;

    // Advanced Direction Technics
    with TInvention.Create( nidInvention_AdvancedDirectionTechnics, 'Advanced Direction Technics', tidInventionKind_Direction, 'Main Headquarters' ) do
      begin
        Price := 2000000;
        Time  := tmeInvention_Short;
        Register( tidClassFamily_Inventions );
      end;

    with TInvention.Create( nidInvention_TV, 'Television', tidInventionKind_Direction, 'Main Headquarters' ) do
      begin
        Price := 10000000;
        Time  := tmeInvention_Short;
        Register( tidClassFamily_Inventions );
      end;
    }

    {// Industries
    with TInvention.Create( nidInvention_IndustrialFacilities, 'Industrial Facilities', tidInventionKind_IndustrialFacilities, 'Industry Headquarters' ) do
      begin
        Price := 3000000;
        Time  := tmeInvention_Short;
        Register( tidClassFamily_Inventions );
      end;
    TheClassStorage.RegisterEqv( tidClassFamily_Inventions, tidInventionKind_IndustrialFacilities, GetInventionId( nidInvention_IndustrialFacilities, tidInventionKind_IndustrialFacilities ));
    }

    {// Industries.Farms
    with TInvention.Create(nidInvention_BigFarms, 'Large Farms', tidInventionKind_IndustrialFacilities, 'Industry Headquarters') do
      begin
        Price := 4000000;
        Time  := tmeInvention_Normal;
        Register( tidClassFamily_Inventions );
      end;
    with TInvention.Create(nidInvention_BigFarmBiotech1, 'Biotechnology I', tidInventionKind_IndustrialFacilities, 'Industry Headquarters') do
      begin
        Price := 10000000;
        Time  := tmeInvention_Normal;
        Register( tidClassFamily_Inventions );
      end;
    with TInvention.Create(nidInvention_BigFarmBiotech2, 'Biotechnology II', tidInventionKind_IndustrialFacilities, 'Industry Headquarters') do
      begin
        Price := 10000000;
        Time  := tmeInvention_Normal;
        SetDependencies([nidInvention_BigFarmBiotech1]);
        Register( tidClassFamily_Inventions );
      end;
    with TInvention.Create(nidInvention_BigFarmBiotech3, 'Biotechnology III', tidInventionKind_IndustrialFacilities, 'Industry Headquarters') do
      begin
        Price := 10000000;
        Time  := tmeInvention_Normal;
        SetDependencies([nidInvention_BigFarmBiotech2]);
        Register( tidClassFamily_Inventions );
      end;
    }
    {
    with TInvention.Create(nidInvention_BigChemicalPlant, 'Large Chemical Factories', tidInventionKind_IndustrialFacilities, 'Industry Headquarters') do
      begin
        Price := 60000000;
        Time  := tmeInvention_Normal;
        Register( tidClassFamily_Inventions );
      end;
    }
    {
    with TInvention.Create(nidInvention_BigFoodProc, 'Large Scale Food Processing', tidInventionKind_IndustrialFacilities, 'Industry Headquarters') do
      begin
        Price := 40000000;
        Time  := tmeInvention_Normal;
        Register( tidClassFamily_Inventions );
      end;
    }
    {
    with TInvention.Create(nidInvention_BigMines, 'Large Scale Minning', tidInventionKind_IndustrialFacilities, 'Industry Headquarters') do
      begin
        Price := 20000000;
        Time  := tmeInvention_Normal;
        Register( tidClassFamily_Inventions );
      end;
    }
    {
    with TInvention.Create(nidInvention_BigMetalurgy, 'Large Scale Metallurgy', tidInventionKind_IndustrialFacilities, 'Industry Headquarters') do
      begin
        Price := 60000000;
        Time  := tmeInvention_Normal;
        Register( tidClassFamily_Inventions );
      end;
    }
    {
    with TInvention.Create(nidInvention_BigTextile, 'Large Scale Textile', tidInventionKind_IndustrialFacilities, 'Industry Headquarters') do
      begin
        Price := 50000000;
        Time  := tmeInvention_Normal;
        Register( tidClassFamily_Inventions );
      end;
    with TInvention.Create(nidInvention_BigClothesIndustry, 'Large Scale Clothe Manufacturing', tidInventionKind_IndustrialFacilities, 'Industry Headquarters') do
      begin
        Price := 2000000;
        Time  := tmeInvention_Normal;
        Register( tidClassFamily_Inventions );
      end;
    }
    {
    with TInvention.Create(nidInvention_ConstructionIndustry, 'Construction Industry', tidInventionKind_IndustrialFacilities, 'Industry Headquarters') do
      begin
        Price := 2000000;
        Time  := tmeInvention_Normal;
        Register( tidClassFamily_Inventions );
      end;
    }
    {
    with TInvention.Create(nidInvention_BigElectronicIndustry, 'Large Scale Electronics Production', tidInventionKind_IndustrialFacilities, 'Industry Headquarters') do
      begin
        Price := 80000000;
        Time  := tmeInvention_Normal;
        Register( tidClassFamily_Inventions );
      end;
    }
    {
    with TInvention.Create(nidInvention_BigHHA, 'Large Scale Appliances Production', tidInventionKind_IndustrialFacilities, 'Industry Headquarters') do
      begin
        Price := 7000000;
        Time  := tmeInvention_Normal;
        Register( tidClassFamily_Inventions );
      end;
    }
    {
    with TInvention.Create(nidInvention_HeavyIndustry, 'Machinery Industry', tidInventionKind_IndustrialFacilities, 'Industry Headquarters') do
      begin
        Price := 7000000;
        Time  := tmeInvention_Normal;
        Register( tidClassFamily_Inventions );
      end;
    }
    {
    with TInvention.Create(nidInvention_CarIndustry, 'Car Industry', tidInventionKind_IndustrialFacilities, 'Industry Headquarters') do
      begin
        Price := 7000000;
        Time  := tmeInvention_Normal;
        Register( tidClassFamily_Inventions );
      end;
    }
    {
    with TInvention.Create(nidInvention_BMIndustry, 'Business Machines Industry', tidInventionKind_IndustrialFacilities, 'Industry Headquarters') do
      begin
        Price := 7000000;
        Time  := tmeInvention_Normal;
        Register( tidClassFamily_Inventions );
      end;
    }

    {// Markets
    with TInvention.Create( nidInvention_ServiceFacilities, 'Service Facilities', tidInventionKind_ServiceFacilities, 'Commerce Headquarters' ) do
      begin
        Price := 2000000;
        Time  := tmeInvention_Short;
        Register( tidClassFamily_Inventions );
      end;
    TheClassStorage.RegisterEqv( tidClassFamily_Inventions, tidInventionKind_ServiceFacilities, GetInventionId( nidInvention_ServiceFacilities, tidInventionKind_ServiceFacilities ));
    }

    {
    with TInvention.Create(nidInvention_BigMarkets, 'Supermarkets', tidInventionKind_ServiceFacilities, 'Commerce Headquarters') do
      begin
        Price := 20000000;
        Time  := tmeInvention_Normal;
        Register( tidClassFamily_Inventions );
      end;
    with TInvention.Create(nidInvention_Bars, 'Bars', tidInventionKind_ServiceFacilities, 'Commerce Headquarters') do
      begin
        Price := 10000000;
        Time  := tmeInvention_Normal;
        Register( tidClassFamily_Inventions );
      end;
    with TInvention.Create(nidInvention_Restaurants, 'Restaurants', tidInventionKind_ServiceFacilities, 'Commerce Headquarters') do
      begin
        Price := 25000000;
        Time  := tmeInvention_Normal;
        Register( tidClassFamily_Inventions );
      end;
    with TInvention.Create(nidInvention_Movies, 'Movie Theaters', tidInventionKind_ServiceFacilities, 'Commerce Headquarters') do
      begin
        Price := 10000000;
        Time  := tmeInvention_Normal;
        Register( tidClassFamily_Inventions );
      end;
    }

    {// Offices & Residentials
    with TInvention.Create( nidInvention_OfficeAndResidentials, 'Residentials Buildings', tidInventionKind_OfficeAndResidentials, 'Residentials Headquarters' ) do
      begin
        Price := 1500000;
        Time  := tmeInvention_Normal;
        Register( tidClassFamily_Inventions );
      end;
    TheClassStorage.RegisterEqv( tidClassFamily_Inventions, tidInventionKind_OfficeAndResidentials, GetInventionId( nidInvention_OfficeAndResidentials, tidInventionKind_OfficeAndResidentials ));
    }
    {
    with TInvention.Create( nidInvention_OfficeBuildings, 'Office Buildings', tidInventionKind_OfficeAndResidentials, 'Offices Headquarters' ) do
      begin
        Price := 1500000;
        Time  := tmeInvention_Normal;
        Register( tidClassFamily_Inventions );
      end;
    with TInvention.Create( nidInvention_SoftwareFirms, 'Software Firms', tidInventionKind_OfficeAndResidentials, 'Offices Headquarters' ) do
      begin
        Price := 30000000;
        Time  := tmeInvention_Normal;
        Register( tidClassFamily_Inventions );
      end;
    with TInvention.Create( nidInvention_LegalServices, 'Lawyer Firms', tidInventionKind_OfficeAndResidentials, 'Offices Headquarters' ) do
      begin
        Price := 35000000;
        Time  := tmeInvention_Normal;
        Register( tidClassFamily_Inventions );
      end;
    with TInvention.Create( nidInvention_Banking, 'Banking', tidInventionKind_OfficeAndResidentials, 'Offices Headquarters' ) do
      begin
        Price := 20000000;
        Time  := tmeInvention_Normal;
        Register( tidClassFamily_Inventions );
      end;

    }
    // Public Facilities
    {
    with TInvention.Create( nidInvention_PublicFacilities, 'Public Facilities', tidInventionKind_PublicFacilities, 'Public Affairs Headquarters' ) do
      begin
        Price := 1000000;
        Time  := tmeInvention_VeryShort;
        Register( tidClassFamily_Inventions );
      end;
    TheClassStorage.RegisterEqv( tidClassFamily_Inventions, tidInventionKind_PublicFacilities, GetInventionId( nidInvention_PublicFacilities, tidInventionKind_PublicFacilities ));
    }
    {
    with TInvention.Create( nidInvention_Monuments, 'Monuments', tidInventionKind_PublicFacilities, 'Public Affairs Headquarters' ) do
      begin
        Price := 10000000;
        Time  := tmeInvention_Short;                      
        Register( tidClassFamily_Inventions );
      end;
    }
    //Inventions.CreateInventions(ExtractFilePath(ParamStr(0)) + 'Inventions\');
  end;

(*
procedure RegisterWarehouses;
  begin
    // Cold Storage
    with TMetaBlockUnderConstruction.Create(
      tidBlock_SPECIALCOMMON + tidBlock_UWColdStorageConstr,
      10000000,
      [60, 30, 10],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaColdStorage.Create(
      tidBlock_SPECIALCOMMON + tidBlock_UWColdStorage,
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
    with TMetaFacility.Create( tidBlock_SPECIALCOMMON + tidFacility_UWColdStorage, 'Cold Storage', vidFacility_UWColdStorage, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 100;
        Desc  := '';
        FacId := FID_ColdWarehouse;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_SPECIALCOMMON + tidBlock_UWColdStorageConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The warehouse is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_SPECIALCOMMON + tidBlock_UWColdStorage])));
        ClusterName := tidClusterName_UW;
        FacilityKind := tidFacilityKind_Warehouse;
        Register( tidClassFamily_Facilities );
      end;

    // Super Cold Storage
    with TMetaBlockUnderConstruction.Create(
      tidBlock_SPECIALCOMMON + tidBlock_UWSuperColdStorageConstr,
      100000000,
      [60, 30, 10],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaColdStorage.Create(
      tidBlock_SPECIALCOMMON + tidBlock_UWSuperColdStorage,
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
    with TMetaFacility.Create( tidBlock_SPECIALCOMMON + tidFacility_UWSuperColdStorage, 'Super Cold Storage', vidFacility_UWSuperColdStorage, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 100;
        Desc := '';
        FacId := FID_ColdWarehouse;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_SPECIALCOMMON + tidBlock_UWSuperColdStorageConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The warehouse is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_SPECIALCOMMON + tidBlock_UWSuperColdStorage])));
        ClusterName  := tidClusterName_UW;
        FacilityKind := tidFacilityKind_Warehouse;
        Register( tidClassFamily_Facilities );
      end;

    // Chemical Storage
    with TMetaBlockUnderConstruction.Create(
      tidBlock_SPECIALCOMMON + tidBlock_UWChemicalStorageConstr,
      3000000,
      [60, 30, 10],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaChemicalStorage.Create(
      tidBlock_SPECIALCOMMON + tidBlock_UWChemicalStorage,
      [0, 0, 10],
      10000,
      100000,
      100000,
      150,
      TChemicalStorage ) do
      begin
        VisualStages := 3;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidBlock_SPECIALCOMMON + tidFacility_UWChemicalStorage, 'Chemical Storage', vidFacility_UWChemicalStorage, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 100;
        Desc := '';
        FacId := FID_ChemicalWarehouse;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_SPECIALCOMMON + tidBlock_UWChemicalStorageConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The warehouse is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_SPECIALCOMMON + tidBlock_UWChemicalStorage])));
        ClusterName := tidClusterName_UW;
        FacilityKind := tidFacilityKind_Warehouse;
        Register( tidClassFamily_Facilities );
      end;

    // General Storage
    with TMetaBlockUnderConstruction.Create(
      tidBlock_SPECIALCOMMON + tidBlock_UWGeneralStorageConstr,
      20000000,
      [60, 30, 10],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaGeneralStorage.Create(
      tidBlock_SPECIALCOMMON + tidBlock_UWGeneralStorage,
      [0, 1, 80],
      10000, // OrgMat
      10000, // Fab&Threads
      10000,  // Clothes
      10000,  // Elect Component
      8000,   // HHA
      5000,   // Business Machines
      25000,  // Metals
      80,    // Cars
      8000,  // Toys
      8000,  // Drugs
      8000,  // Plastics
      800,   // Machinery
      150,   // overprice
      TGeneralStorage ) do
      begin
        VisualStages := 3;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidBlock_SPECIALCOMMON + tidFacility_UWGeneralStorage, 'General Storage', vidFacility_UWGeneralStorage, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 1100;
        Desc  := '';
        FacId := FID_GeneralWarehouse;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_SPECIALCOMMON + tidBlock_UWGeneralStorageConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The warehouse is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_SPECIALCOMMON + tidBlock_UWGeneralStorage])));
        ClusterName := tidClusterName_UW;
        FacilityKind := tidFacilityKind_Warehouse;
        Register( tidClassFamily_Facilities );
      end;

    // Fabrics Storage
    with TMetaBlockUnderConstruction.Create(
      tidBlock_SPECIALCOMMON + tidBlock_UWFabricsStorageConstr,
      20000000,
      [60, 30, 10],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFabricsStorage.Create(
      tidBlock_SPECIALCOMMON + tidBlock_UWFabricsStorage,
      [0, 0, 12],
      20000000,
      250000,
      100000,
      150,
      TFabricsStorage ) do
      begin
        VisualStages := 3;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidBlock_SPECIALCOMMON + tidFacility_UWFabricsStorage, 'Fabrics Storage', vidFacility_UWFabricsStorage, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 100;
        Desc  := '';
        FacId := FID_FabricsWarehouse;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_SPECIALCOMMON + tidBlock_UWFabricsStorageConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The warehouse is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_SPECIALCOMMON + tidBlock_UWFabricsStorage])));
        ClusterName := tidClusterName_UW;
        FacilityKind := tidFacilityKind_Warehouse;                    
        Register( tidClassFamily_Facilities );
      end;

    // Ore Storage
    with TMetaBlockUnderConstruction.Create(
      tidBlock_SPECIALCOMMON + tidBlock_UWOreStorageConstr,
      20000000,
      [60, 30, 10],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaOreStorage.Create(
      tidBlock_SPECIALCOMMON + tidBlock_UWOreStorage,
      [0, 0, 12],
      200000,
      50000,
      50000,
      150,
      TOreStorage ) do
      begin
        VisualStages := 3;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidBlock_SPECIALCOMMON + tidFacility_UWOreStorage, 'Ore Storage', vidFacility_UWOreStorage, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 100;
        Desc := '';
        FacId := FID_OreWarehouse;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_SPECIALCOMMON + tidBlock_UWOreStorageConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The warehouse is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_SPECIALCOMMON + tidBlock_UWOreStorage])));
        ClusterName := tidClusterName_UW;
        FacilityKind := tidFacilityKind_Warehouse;
        Register( tidClassFamily_Facilities );
      end;
  end;
*)

procedure RegisterWarehouses;
  begin
    // Cold Storage
    with TMetaBlockUnderConstruction.Create(
      tidBlock_WHCOMMON + tidBlock_UWColdStorageConstr,
      1000000, //10000000,
      [60, 30, 10],
      10,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaColdStorage.Create(
      tidBlock_WHCOMMON + tidBlock_UWColdStorage,
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
    with TMetaFacility.Create( tidBlock_WHCOMMON + tidFacility_UWColdStorage, 'UW Cold Storage', vidFacility_UWColdStorage, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 100;
        Desc  := '';
        FacId := FID_ColdWarehouse;
        Options := Options - [mfcInTown];
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_WHCOMMON + tidBlock_UWColdStorageConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The warehouse is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_WHCOMMON + tidBlock_UWColdStorage])));
        ClusterName := tidClusterName_UW;
        FacilityKind := tidFacilityKind_Warehouse;
        Register( tidClassFamily_Facilities );
      end;

    // Super Cold Storage
    with TMetaBlockUnderConstruction.Create(
      tidBlock_WHCOMMON + tidBlock_UWSuperColdStorageConstr,
      3000000, //100000000,
      [60, 30, 10],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaColdStorage.Create(
      tidBlock_WHCOMMON + tidBlock_UWSuperColdStorage,
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
    with TMetaFacility.Create( tidBlock_WHCOMMON + tidFacility_UWSuperColdStorage, 'UW Super Cold Storage', vidFacility_UWSuperColdStorage, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 100;
        Desc := '';
        FacId := FID_ColdWarehouse;
        Options := Options - [mfcInTown];
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_WHCOMMON + tidBlock_UWSuperColdStorageConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The warehouse is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_WHCOMMON + tidBlock_UWSuperColdStorage])));
        ClusterName  := tidClusterName_UW;
        FacilityKind := tidFacilityKind_Warehouse;
        Register( tidClassFamily_Facilities );
      end;

    {>>Test Warehouse<<}
    // Mega Storage
    with TMetaBlockUnderConstruction.Create(
      tidBlock_WHCOMMON + tidBlock_UWMegaStorageInpConstr,
      3000000, //100000000,
      [60, 30, 10],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaMegaStorage.Create(
      tidBlock_WHCOMMON + tidBlock_UWMegaStorageInp,
      [0, 2, 100],
      150,
      TMegaStorage ) do
      begin
        VisualStages := 3;
        TradeMode := rolCompInport;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidBlock_WHCOMMON + tidFacility_UWMegaStorageImp, 'Import Storage', vidFacility_UWMegaStorageImp, TFacility ) do
      begin
        xSize := 4;
        ySize := 4;
        Level := 100;
        Desc := '';
        FacId := FID_MegaWarehouseImp;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_WHCOMMON + tidBlock_UWMegaStorageInpConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The warehouse is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_WHCOMMON + tidBlock_UWMegaStorageInp])));
        ClusterName  := tidClusterName_UW;
        FacilityKind := tidFacilityKind_Warehouse;
        Register( tidClassFamily_Facilities );
      end;

    // Mega Storage
    with TMetaBlockUnderConstruction.Create(
      tidBlock_WHCOMMON + tidBlock_UWMegaStorageExpConstr,
      3000000, //100000000,
      [60, 30, 10],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaMegaStorage.Create(
      tidBlock_WHCOMMON + tidBlock_UWMegaStorageExp,
      [0, 2, 100],
      150,
      TMegaStorage ) do
      begin
        VisualStages := 3;
        TradeMode := rolCompExport;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidBlock_WHCOMMON + tidFacility_UWMegaStorageExp, 'Export Storage', vidFacility_UWMegaStorageExp, TFacility ) do
      begin
        xSize := 4;
        ySize := 4;
        Level := 100;
        Desc := '';
        FacId := FID_MegaWarehouseExp;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_WHCOMMON + tidBlock_UWMegaStorageExpConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The warehouse is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_WHCOMMON + tidBlock_UWMegaStorageExp])));
        ClusterName  := tidClusterName_UW;
        FacilityKind := tidFacilityKind_Warehouse;
        Register( tidClassFamily_Facilities );
      end;
    {>>Test Warehouse<<}

    // Chemical Storage
    with TMetaBlockUnderConstruction.Create(
      tidBlock_WHCOMMON + tidBlock_UWChemicalStorageConstr,
      1000000,
      [60, 30, 10],
      10,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaChemicalStorage.Create(
      tidBlock_WHCOMMON + tidBlock_UWChemicalStorage,
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
    with TMetaFacility.Create( tidBlock_WHCOMMON + tidFacility_UWChemicalStorage, 'UW Chemical Storage', vidFacility_UWChemicalStorage, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 100;
        Desc := '';
        FacId := FID_ChemicalWarehouse;
        Options := Options - [mfcInTown];
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_WHCOMMON + tidBlock_UWChemicalStorageConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The warehouse is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_WHCOMMON + tidBlock_UWChemicalStorage])));
        ClusterName := tidClusterName_UW;
        FacilityKind := tidFacilityKind_Warehouse;
        Register( tidClassFamily_Facilities );
      end;

    // General Storage
    with TMetaBlockUnderConstruction.Create(
      tidBlock_WHCOMMON + tidBlock_UWGeneralStorageConstr,
      1000000, //20000000,
      [60, 30, 10],
      10,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaGeneralStorage.Create(
      tidBlock_WHCOMMON + tidBlock_UWGeneralStorage,
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
    with TMetaFacility.Create( tidBlock_WHCOMMON + tidFacility_UWGeneralStorage, 'UW General Storage', vidFacility_UWGeneralStorage, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 1100;
        Desc  := '';
        FacId := FID_GeneralWarehouse;
        Options := Options - [mfcInTown];
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_WHCOMMON + tidBlock_UWGeneralStorageConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The warehouse is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_WHCOMMON + tidBlock_UWGeneralStorage])));
        ClusterName := tidClusterName_UW;
        FacilityKind := tidFacilityKind_Warehouse;
        Register( tidClassFamily_Facilities );
      end;

    // Fabrics Storage
    with TMetaBlockUnderConstruction.Create(
      tidBlock_WHCOMMON + tidBlock_UWFabricsStorageConstr,
      1000000, //20000000,
      [60, 30, 10],
      10,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFabricsStorage.Create(
      tidBlock_WHCOMMON + tidBlock_UWFabricsStorage,
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
    with TMetaFacility.Create( tidBlock_WHCOMMON + tidFacility_UWFabricsStorage, 'UW Fabrics Storage', vidFacility_UWFabricsStorage, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 100;
        Desc  := '';
        FacId := FID_FabricsWarehouse;
        Options := Options - [mfcInTown];
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_WHCOMMON + tidBlock_UWFabricsStorageConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The warehouse is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_WHCOMMON + tidBlock_UWFabricsStorage])));
        ClusterName := tidClusterName_UW;
        FacilityKind := tidFacilityKind_Warehouse;
        Register( tidClassFamily_Facilities );
      end;

    // Ore Storage
    with TMetaBlockUnderConstruction.Create(
      tidBlock_WHCOMMON + tidBlock_UWOreStorageConstr,
      1000000, //20000000
      [60, 30, 10],
      10,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaOreStorage.Create(
      tidBlock_WHCOMMON + tidBlock_UWOreStorage,
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
    with TMetaFacility.Create( tidBlock_WHCOMMON + tidFacility_UWOreStorage, 'UW Ore Storage', vidFacility_UWOreStorage, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 100;
        Desc := '';
        FacId := FID_OreWarehouse;
        Options := Options - [mfcInTown];
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_WHCOMMON + tidBlock_UWOreStorageConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The warehouse is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_WHCOMMON + tidBlock_UWOreStorage])));
        ClusterName := tidClusterName_UW;
        FacilityKind := tidFacilityKind_Warehouse;
        Register( tidClassFamily_Facilities );
      end;
  end;

procedure RegisterCommonFacilities;
  begin
    // Disposal
    with TMetaBlockUnderConstruction.Create(
      tidBlock_SRVCOMMON + 'DisposalConst',
      20000000,
      [90, 5, 5],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPublicFacility.Create(
      tidBlock_SRVCOMMON + 'Disposal',
      [0, 0, 10],
      [PFInfoDef( tidPublicFac_Disposal, 50000 )], // 10000
      TPublicFacility ) do
      begin
        UpEffectOnMod := 0.2;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidBlock_SRVCOMMON + 'Disposal', 'Dump', 8001, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 120;
        SlotCount := 0;
        FacId := FID_Disposal;
        DemoLapse := 1;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_SRVCOMMON + 'DisposalConst'])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_SRVCOMMON + 'Disposal'])));
        MinistryId := nidMinistry_Health;
        Register( tidClassFamily_Facilities );
      end;

    // Jail
    with TMetaBlockUnderConstruction.Create(
      tidBlock_SRVCOMMON + 'JailConst',
      100000000,
      [90, 5, 5],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPublicFacility.Create(
      tidBlock_SRVCOMMON + 'Jail',
      [0, 5, 150],
      [PFInfoDef( tidPublicFac_Jail, 50000 )],
      TPublicFacility ) do
      begin
        MaintCost := 5000;
        UpEffectOnMod := 0.2;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidBlock_SRVCOMMON + 'Jail', 'Jail', 8011, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 120;
        SlotCount := 0;
        FacId := FID_Police;
        DemoLapse := 1;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_SRVCOMMON + 'JailConst'])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_SRVCOMMON + 'Jail'])));
        MinistryId := nidMinistry_Defense;
        Register( tidClassFamily_Facilities );
      end;

    // Museum
    with TMetaBlockUnderConstruction.Create(
      tidBlock_SRVCOMMON + 'MuseumConst',
      300000000,
      [90, 5, 5],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPublicFacility.Create(
      tidBlock_SRVCOMMON + 'Museum',
      [20, 50, 200],
      [PFInfoDef( tidPublicFac_Museum, 50000 )],
      TPublicFacility ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidBlock_SRVCOMMON + 'Museum', 'Museum', 8031, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 120;
        SlotCount := 0;
        FacId := FID_Amusement;
        DemoLapse := 1;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_SRVCOMMON + 'MuseumConst'])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_SRVCOMMON + 'Museum'])));
        MinistryId := nidMinistry_Education;
        Register( tidClassFamily_Facilities );
      end;

    // College
    with TMetaBlockUnderConstruction.Create(
      tidBlock_SRVCOMMON + 'CollegeConst',
      800000000,
      [70, 5, 25],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPublicFacility.Create(
      tidBlock_SRVCOMMON + 'College',
      [40, 300, 50],
      [PFInfoDef( tidPublicFac_College, 200000 )],
      TPublicFacility ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidBlock_SRVCOMMON + 'College', 'College', 8041, TFacility ) do
      begin
        xSize := 8;
        ySize := 8;
        Level := 120;
        SlotCount := 0;
        FacId := FID_School;
        DemoLapse := 1;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_SRVCOMMON + 'CollegeConst'])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_SRVCOMMON + 'College'])));
        MinistryId := nidMinistry_Education;
        Register( tidClassFamily_Facilities );
      end;

    // TennisCourt
    with TMetaBlockUnderConstruction.Create(
      tidBlock_SRVCOMMON + 'TennisCourtConst',
      100000,
      [70, 5, 25],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPublicFacility.Create(
      tidBlock_SRVCOMMON + 'TennisCourt',
      [0, 0, 2],
      [PFInfoDef( tidPublicFac_Sport, 7000 )],
      TPublicFacility ) do
      begin
        MaintCost := 100;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidBlock_SRVCOMMON + 'TennisCourt', 'Tennis courts', 8021, TFacility ) do
      begin
        xSize := 3;
        ySize := 3;
        Level := 120;
        FacId := FID_Amusement;
        DemoLapse := 1;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_SRVCOMMON + 'TennisCourtConst'])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_SRVCOMMON + 'TennisCourt'])));
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // ParkA
    with TMetaBlockUnderConstruction.Create(
      tidBlock_SRVCOMMON + 'AlienParkAConst',
      1000000,
      [100, 0, 0],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaEnvironmentalBlock.Create(
      tidBlock_SRVCOMMON + 'AlienParkA',
      TEnvironmentalBlock ) do
      begin
        RegisterModifier( tidEnvironment_Beauty, 300, 10 );
        RegisterModifier( tidEnvironment_Pollution, -300, 10 );
        MaintCost := 100;
        DissabledStop := true;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidBlock_SRVCOMMON + 'AlienParkA', 'Lizzard Park', 8051, TFacility ) do
      begin
        xSize := 3;
        ySize := 3;
        Level := 200;
        FacId := FID_Park;
        DemoLapse := 1;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_SRVCOMMON + 'AlienParkAConst'])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Park Completed', 'Park is done', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_SRVCOMMON + 'AlienParkA'])));
        MinistryId := nidMinistry_Health;
        Register( tidClassFamily_Facilities );
      end;

    // ParkB
    with TMetaBlockUnderConstruction.Create(
      tidBlock_SRVCOMMON + 'AlienParkBConst',
      1000000,
      [100, 0, 0],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaEnvironmentalBlock.Create(
      tidBlock_SRVCOMMON + 'AlienParkB',
      TEnvironmentalBlock ) do
      begin
        RegisterModifier( tidEnvironment_Beauty, 300, 10 );
        RegisterModifier( tidEnvironment_Pollution, -300, 10 );
        MaintCost := 100;
        DissabledStop := true;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidBlock_SRVCOMMON + 'AlienParkB', 'Gobblin Park', 8061, TFacility ) do
      begin
        xSize := 3;
        ySize := 3;
        Level := 200;
        FacId := FID_Park;
        DemoLapse := 1;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_SRVCOMMON + 'AlienParkBConst'])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Park Completed', 'Park is done', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_SRVCOMMON + 'AlienParkB'])));
        MinistryId := nidMinistry_Health;
        Register( tidClassFamily_Facilities );
      end;

    // ParkC
    with TMetaBlockUnderConstruction.Create(
      tidBlock_SRVCOMMON + 'AlienParkCConst',
      1000000,
      [100, 0, 0],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaEnvironmentalBlock.Create(
      tidBlock_SRVCOMMON + 'AlienParkC',
      TEnvironmentalBlock ) do
      begin
        RegisterModifier( tidEnvironment_Beauty, 300, 10 );
        RegisterModifier( tidEnvironment_Pollution, -300, 10 );
        MaintCost := 100;
        DissabledStop := true;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidBlock_SRVCOMMON + 'AlienParkC', 'Chicken Park', 8071, TFacility ) do
      begin
        xSize := 3;
        ySize := 3;
        Level := 200;
        FacId := FID_Park;
        DemoLapse := 1;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_SRVCOMMON + 'AlienParkCConst'])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Park Completed', 'Park is done', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_SRVCOMMON + 'AlienParkC'])));
        MinistryId := nidMinistry_Health;
        Register( tidClassFamily_Facilities );
      end;

    // ParkD
    with TMetaBlockUnderConstruction.Create(
      tidBlock_SRVCOMMON + 'AlienParkDConst',
      1000000,
      [100, 0, 0],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaEnvironmentalBlock.Create(
      tidBlock_SRVCOMMON + 'AlienParkD',
      TEnvironmentalBlock ) do
      begin
        RegisterModifier( tidEnvironment_Beauty, 300, 10 );
        RegisterModifier( tidEnvironment_Pollution, -300, 10 );
        MaintCost := 100;
        DissabledStop := true;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidBlock_SRVCOMMON + 'AlienParkD', 'Alien Park', 8081, TFacility ) do
      begin
        xSize := 3;
        ySize := 3;
        Level := 200;
        FacId := FID_Park;
        DemoLapse := 1;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_SRVCOMMON + 'AlienParkDConst'])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Park Completed', 'Park is done', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_SRVCOMMON + 'AlienParkD'])));
        MinistryId := nidMinistry_Health;
        Register( tidClassFamily_Facilities );
      end;

    // Paradigm's Transcendence Monument
    with TMetaBlockUnderConstruction.Create(
      tidBlock_SPECIALCOMMON + 'ParadigmMausoleumConst',
      1000000000,
      [100, 0, 0],
      120,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaTranscendBlock.Create(
      tidBlock_SPECIALCOMMON + 'ParadigmMausoleum',
      tidTycoonLevel_Paradigm,
      1,
      TTranscendBlock ) do
      begin
        RegisterModifier( tidEnvironment_Beauty, 300, 10 );
        RegisterModifier( tidEnvironment_Pollution, -300, 10 );
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidBlock_SPECIALCOMMON + 'ParadigmMausoleum', 'Paradigm Mausoleum', 8091, TFacility ) do
      begin
        xSize := 3;
        ySize := 3;
        Level := 200;
        FacId := FID_Park;
        SlotCount := 0;
        if TheGlobalConfigHandler.GetConfigParm('TranscendingOn', '1') = '0'
          then Options := Options - [mfcInTown];
        Options := Options - [mfcGenerateName, mfcShowCompanyInText, mfcShowProfitInText] + [mfcIgnoreZoning];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_SPECIALCOMMON + 'ParadigmMausoleumConst'])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Park Completed', 'Park is done', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_SPECIALCOMMON + 'ParadigmMausoleum'])));
        Register( tidClassFamily_Facilities );
      end;

    // Legend's Transcendence Monument
    with TMetaBlockUnderConstruction.Create(
      tidBlock_SPECIALCOMMON + 'LegendMausoleumConst',
      1000000000,
      [100, 0, 0],
      120,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaTranscendBlock.Create(
      tidBlock_SPECIALCOMMON + 'LegendMausoleum',
      tidTycoonLevel_Legend,
      1,
      TTranscendBlock ) do
      begin
        RegisterModifier( tidEnvironment_Beauty, 300, 10 );
        RegisterModifier( tidEnvironment_Pollution, -300, 10 );
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidBlock_SPECIALCOMMON + 'LegendMausoleum', 'Legend Mausoleum', 8101, TFacility ) do
      begin
        xSize := 9;
        ySize := 9;
        Level := 200;
        SlotCount := 0;
        FacId := FID_Park;
        if TheGlobalConfigHandler.GetConfigParm('TranscendingOn', '1') = '0'
          then Options := Options - [mfcInTown];
        Options := Options - [mfcGenerateName, mfcShowCompanyInText, mfcShowProfitInText] + [mfcIgnoreZoning];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_SPECIALCOMMON + 'LegendMausoleumConst'])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Park Completed', 'Park is done', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_SPECIALCOMMON + 'LegendMausoleum'])));
        Register( tidClassFamily_Facilities );
      end;

    // Portal
    with TMetaEnvironmentalBlock.Create(
      'Portal',
      TPortal ) do
      begin
        RegisterModifier( tidEnvironment_Beauty, 300, 10 );
        RegisterModifier( tidEnvironment_Pollution, -300, 10 );
        MaintCost := 100;
        DissabledStop := true;
        VisualStages := 2;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( 'Portal', 'Portal', 6031, TFacility ) do
      begin
        xSize := 4;
        ySize := 4;
        Level := 200;
        FacId := FID_Park;
        Options := [mfcShowCompanyInText, mfcStopDisabled, mfcIgnoreZoning, mfcForbiddenRename, mfcAceptsNoOwner];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Park Completed', 'Park is done', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, 'Portal'])));
        Register( tidClassFamily_Facilities );
      end;
  end;

const
  OneMonth = 30*24;
  OneYear  = 12*OneMonth;

procedure RegisterModelExtension; export;
  begin
    SimMLS.LoadMLS;
    InitVisualClasses;
    StdAccounts.RegisterAccounts;
    ServiceBlock.RegisterInventionClass;
    Farms.RegisterBackup;
    PlasticInd.RegisterBackup;
    LiquorFact.RegisterBackup;
    Chemical.RegisterBackup;
    Mine.RegisterBackup;
    ChemMine.RegisterBackup;
    SiliconMine.RegisterBackup;
    CoalMine.RegisterBackup;
    StoneMine.RegisterBackup;
    FoodProcessor.RegisterBackup;
    MetalIndustry.RegisterBackup;
    ConstructionIndustry.RegisterBackup;
    ElectronicIndustry.RegisterBackup;
    HeavyIndustry.RegisterBackup;
    TextilIndustry.RegisterBackup;
    Clothings.RegisterBackup;
    CarIndustry.RegisterBackup;
    HouseHoldingAppliances.RegisterBackup;
    BusinessMachines.RegisterBackup;
    FoodStore.RegisterBackup;
    ClothesShop.RegisterBackup;
    CarShop.RegisterBackup;
    Bar.RegisterBackup;
    HHAStore.RegisterBackup;
    SuperMarket.RegisterBackup;
    Restaurant.RegisterBackup;
    Movie.RegisterBackup;
    Computing.RegisterBackup;
    LegalServices.RegisterBackup;
    ColdStorage.RegisterBackup;
    ChemicalStorage.RegisterBackup;
    GeneralStorage.RegisterBackup;
    FabricsStorage.RegisterBackup;
    OreStorage.RegisterBackup;
    MegaWarehouse.RegisterBackup;
    Environmental.RegisterBackup;
    PharmaIndustry.RegisterBackup;
    DrugStore.RegisterBackup;
    ToyFactory.RegisterBackup;
    ToyStore.RegisterBackup;
    Refinery.RegisterBackup;
    OilRig.RegisterBackup;
    GasStation.RegisterBackup;
    MagnaMarket.RegisterBackup;
    LumberMill.RegisterBackup;
    FurnitureIndustry.RegisterBackup;
    FurnitureStore.RegisterBackup;
    BookStore.RegisterBackup;
    ComputerStore.RegisterBackup;
    MovieStudios.RegisterInventionClass;
    MovieStudios.RegisterBackup;
    FuneralParlors.RegisterBackup;
    PaperIndustry.RegisterBackup;
    PrintingPlant.RegisterBackup;
    CDPlant.RegisterBackup;
    CDStore.RegisterBackup;
    Portal.RegisterBackup;

    // Register Public facilities
    with TMetaPublicFacilityInfo.Create( tidPublicFac_Police ) do
      begin
        Name := 'Police Coverage';
        Importance := 100;
        SurfaceId := tidEnvironment_Crime;
        ModStrength := 2;
        ModFact := -1;
        Register
      end;
    with TMetaPublicFacilityInfo.Create( tidPublicFac_Fire ) do
      begin
        Name := 'Fire Coverage';
        Importance := 30;
        Register
      end;
    with TMetaPublicFacilityInfo.Create( tidPublicFac_Health ) do
      begin
        Name := 'Health Coverage';
        Importance := 80;
        Register
      end;
    with TMetaPublicFacilityInfo.Create( tidPublicFac_School ) do
      begin
        Name := 'School Coverage';
        Importance := 70;
        SurfaceId := tidEnvironment_QOL;
        ModStrength := 2;
        ModFact := 1;
        Register
      end;
    with TMetaPublicFacilityInfo.Create( tidPublicFac_Disposal ) do
      begin
        Name := 'Garbage Disposal';
        Importance := 60;
        if TheGlobalConfigHandler.GetConfigParm('JailDumpOn', '1') = '1'
          then SurfaceId := tidEnvironment_Pollution;
        ModStrength := 2;
        ModFact := 1;
        Register
      end;
    with TMetaPublicFacilityInfo.Create( tidPublicFac_Sport ) do
      begin
        Name := 'Recreation Facilities';
        Importance := 50;
        SurfaceId := tidEnvironment_QOL;
        ModStrength := 2;
        ModFact := 1;
        Register
      end;
    with TMetaPublicFacilityInfo.Create( tidPublicFac_Museum ) do
      begin
        Name := 'Museums';
        Importance := 50;
        SurfaceId := tidEnvironment_QOL;
        ModStrength := 2;
        ModFact := 1;
        Register
      end;
    with TMetaPublicFacilityInfo.Create( tidPublicFac_College ) do
      begin
        Name := 'Colleges';
        Importance := 50;
        SurfaceId := tidEnvironment_QOL;
        ModStrength := 2;
        ModFact := 1;
        Register
      end;
    with TMetaPublicFacilityInfo.Create( tidPublicFac_Jail ) do
      begin
        Name := 'Jails';
        Importance := 40;
        if TheGlobalConfigHandler.GetConfigParm('JailDumpOn', '1') = '1'
          then SurfaceId := tidEnvironment_Beauty;
        ModStrength := 2;
        ModFact := -1;
        Register
      end;

    // Register Services
    TMetaService.Create(
      tidService_FreshFood,
      'Fresh Food',
      tidFluid_FreshFood,
      0,
      [0.2/24, 0.3/24, 0.1/24],
      [1, 1, 1],
      [1, 1, 1],
      8*OneYear,
      1*OneMonth);

    TMetaService.Create(
      tidService_ElabFood,
      'Processed Food',
      tidFluid_ElabFood,
      0,
      [0.1/24, 0.3/24, 0.3/24],
      [1, 1, 1],
      [1, 1, 1],
      13*OneYear,
      3*OneMonth);

    TMetaService.Create(
      tidService_Clothes,
      'Clothing and Footwear',
      tidFluid_Clothes,
      0,
      [5/(24*7), 3/(24*7), 2.5/(24*30)],
      [0.8, 0.5, 0.3],
      [1, 0.5, 0.3],
      8*OneYear,
      5*OneMonth);

    TMetaService.Create(
      tidService_Cars,
      'Cars',
      tidFluid_Cars,
      0,
      [1/(24*365), 1/(24*365), 1/(3*24*365)],
      [0.4, 0.3, 0.1],
      [1, 0.8, 0.8],
      2*OneYear,
      3*OneMonth);

    TMetaService.Create(
      tidService_Bar,
      'Bars',
      tidNoFluid,
      BarAveragePrice,
      [1/(2*24*7), 2/(24*7), 5/(24*7)],
      [0.2, 0.5, 0.7],
      [1, 0.5, 0.5],
      7*OneYear,
      8*OneMonth);

    TMetaService.Create(
      tidService_HouseHoldingAppliances,
      'Household Appliances',
      tidFluid_HouseHoldingAppliances,
      0,
      [6/(24*30), 5/(24*30), 2/(3*24*30)],
      [0.7, 0.8, 0.3],
      [1, 0.5, 0.3],
      9*OneYear,
      10*OneMonth);

    TMetaService.Create(
      tidService_DrugStore,
      'Pharmaceutics',
      tidFluid_Drugs,
      DrugStoreAveragePrice,
      [1/(24*7), 1/(24*7), 0.7/(24*7)],
      [0.7, 0.8, 0.3],
      [1, 0.5, 0.3],
      6*OneYear,
      5*OneMonth);

    TMetaService.Create(
      tidService_Toys,
      'Toys',
      tidFluid_Toys,
      ToyStoreAveragePrice,
      [1/(2*24*7), 1/(2*24*7), 0.7/(2*24*7)],
      [0.7, 0.8, 0.3],
      [1, 0.5, 0.3],
      9*OneYear,
      4*OneMonth);

    TMetaService.Create(
      tidService_Furniture,
      'Furniture',
      tidFluid_Furniture,
      FurnitureStoreAveragePrice,
      [1/(4*24*7), 1/(5*24*7), 0.7/(8*24*7)],
      [0.7, 0.8, 0.3],
      [1, 0.5, 0.3],
      8*OneYear,
      6*OneMonth);

    TMetaService.Create(
      tidService_Books,
      'Books',
      tidFluid_Books,
      BookStoreAveragePrice,
      [1/(2*24*7), 1/(24*7), 1/(4*24*7)],
      [0.6, 0.8, 0.3],
      [0.9, 0.8, 0.3],
      7*OneYear,
      6*OneMonth);

    TMetaService.Create(
      tidService_Computers,
      'Computers',
      tidFluid_Computers,
      ComputerStoreAveragePrice,
      [2/(365*24), 1/(365*24), 1/(3*365*24)],
      [0.7, 0.8, 0.3],
      [1, 0.5, 0.3],
      10*OneYear,
      9*OneMonth);

    TMetaService.Create(
      tidService_Gasoline,
      'Gas',
      tidNoFluid,
      GasStationAveragePrice,
      [1/(24), 1/(24), 0.4/(24)],
      [0.8, 0.8, 0.6],
      [1, 1, 0.8],
      9*OneYear,
      1*OneMonth);

    TMetaService.Create(
      tidService_Restaurant,
      'Restaurants',
      tidNoFluid,
      RestaurantAveragePrice,
      [4/(24*7), 2/(24*7), 3/(2*24*7)],
      [0.5, 0.3, 0.1],
      [1, 0.8, 0.5],
      7*OneYear,
      11*OneMonth);

    TMetaService.Create(
      tidService_Movie,
      'Movies',
      tidNoFluid,
      MovieAveragePrice,
      [5/(24*7), 5/(24*7), 4/(24*7)],
      [0.5, 0.6, 0.3],
      [1, 1, 1],
      10*OneYear,
      4*OneMonth);

    TMetaService.Create(
      tidService_Funeral,
      'Funerals',
      tidNoFluid,
      FuneralAveragePrice,
      [1/(40*24*365), 1/(40*24*365), 1/(35*24*365)],
      [0.1, 0.1, 0.1],
      [1, 1, 1],
      9*OneYear,
      3*OneMonth);

    TMetaService.Create(
      tidService_CDs,
      'CDs',
      tidFluid_CDs,
      CDStoreAveragePrice,
      [1/(24*7), 1/(24*7), 1/(2*24*7)],
      [0.6, 0.8, 0.3],
      [0.9, 0.8, 0.3],
      10*OneYear,
      7*OneMonth);

    // Register Ministries here

    // Public Ministeries
    TPublicMinistry.Create( 'Health', nidMinistry_Health, tidPublicFac_Health, TPublicMinister );
    TPublicMinistry.Create( 'Education', nidMinistry_Education, tidPublicFac_School, TPublicMinister );
    TPublicMinistry.Create( 'Defense', nidMinistry_Defense, tidPublicFac_Police, TPublicMinister );

    // Economy Ministeries
    with TEconomyMinistry.Create( 'Agriculture', nidMinistry_Agriculture, TEconomyMinister ) do
      begin
        AddService( tidService_FreshFood, 50 );
        AddService( tidService_ElabFood, 50 );
      end;
    with TEconomyMinistry.Create( 'Light Industry', nidMinistry_LightIndustry, TEconomyMinister ) do
      begin
        AddService( tidService_Clothes, 60 );
        AddService( tidService_HouseHoldingAppliances, 40 );
      end;
    with TEconomyMinistry.Create( 'Heavy Industry', nidMinistry_HeavyIndustry, TEconomyMinister ) do
      begin
        AddService( tidService_Cars, 100 );
      end;
    with TEconomyMinistry.Create( 'Commerce', nidMinistry_Commerce, TComerceMinister ) do
      begin
        AddService( tidService_Bar, 100 );
        AddService( tidService_Restaurant, 100 );
        AddService( tidService_Movie, 100 );
      end;
    TMinistry.Create( 'Housing', nidMinistry_Housing, THousingMinister );

    // Weather Envelopes
    with TWeatherEnvelope.Create(tidWeather_FarmProduction) do
      begin
        Value[seasWinter] := 0;
        Value[seasSpring] := 0.8;
        Value[seasSummer] := 1;
        Value[seasFall]   := 0.95;
        Register(tidClassFamily_WeatherEnvelopes);
      end;

    // Common facilities
    RegisterCommonFacilities;
    RegisterWarehouses;
  end;

procedure PostRegisterModelExtension; export;
  var
    minCount : integer;
    minI     : integer;
    facCount : integer;
    facI     : integer;
    facJ     : integer;
    Ministry : TMinistry;
    MF       : TMetaFacility;
    newMF    : TMetaFacility;
    FacKind  : TFacilityKind;
    NewFacs  : TCollection;
  begin
    // Create Ministry clusters
    NewFacs  := TCollection.Create( 0, rkUse );
    minCount := TheClassStorage.ClassCount[tidClassFamily_Ministries];
    for minI := 0 to pred(minCount) do
      begin
        Ministry := TMinistry(TheClassStorage.ClassByIdx[tidClassFamily_Ministries, minI]);
        FacKind  := TFacilityKind.Create( Ministry.Id );
        FacKind.ClusterName := Ministry.Name;
        FacKind.Name        := 'Facilities';
        FacKind.SuperType   := 'Ministery Facilities';
        FacKind.Technology  := Ministry.Id;
        FacKind.Register( tidClassFamily_FacilityKinds );

        // Register Basic Ministery Invention
        with TInvention.Create( 'Mnstr' + IntToStr(minI), 'Ministry Administration', Ministry.Id, 'Ministry Headquarters' ) do
          begin
            Price := 20000000;
            Time  := tmeInvention_Normal;
            Register( tidClassFamily_Inventions );
          end;

        // Register Ministry Headquarters
        with TMetaBlockUnderConstruction.Create(
          tidBlock_MinistryHeadquarterConstr + Ministry.Name,
          5000000,
          [100, 0, 0],
          7,
          TBlockUnderConstruction ) do
          begin
            Register( tidClassFamily_Blocks );
          end;
        with TMetaHeadquarterBlock.Create(
          tidBlock_MinistryHeadquarter + Ministry.Name,
          [20, 30, 12],
          Ministry.Id,
          THeadquarterBlock ) do
          begin
            Beauty := 200;
            Register( tidClassFamily_Blocks );
          end;
        newMF := TMetaFacility.Create( tidFacility_MinistryHeadquarter + Ministry.Name, 'Ministry Headquarters', vidFacility_MinistryHeadquarter, TFacility );
        with newMF do
          begin
            XSize := 2;
            YSize := 2;
            Level := 120;
            FacId := 0;
            ClusterName  := Ministry.Name;
            FacilityKind := Ministry.Id;
            Options := Options - [mfcGenerateName, mfcInTown];
            EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MinistryHeadquarterConstr + Ministry.Name])));
            EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MinistryHeadquarter + Ministry.Name])));
          end;
        NewFacs.Insert( newMF );

        // Register Ministry facilities
        facCount := TheClassStorage.ClassCount[tidClassFamily_Facilities];
        for facI := 0 to pred(facCount) do
          begin
            MF := TMetaFacility(TheClassStorage.ClassByIdx[tidClassFamily_Facilities, facI]);
            if MF.MinistryId = Ministry.MinId
              then
                begin
                  facJ := pred(NewFacs.Count);
                  while (facJ >= 0) and ((TMetaFacility(NewFacs[facJ]).VisualClass <> MF.VisualClass) or not(mfcInTown in TMetaFacility(NewFacs[facJ]).Options)) do
                    dec(facJ);
                  newMF := CMetaFacility(MF.ClassType).CopyFrom( MF, Ministry.Name + MF.Id );
                  newMF.ClusterName    := Ministry.Name;
                  newMF.FacilityKind   := Ministry.Id;
                  newMF.TechnologyKind := Ministry.Id;
                  newMF.ReqCmpSupplies := false;
                  // If not the seed facility, can't build it.
                  if (MF.CloneSource <> nil) or (facJ >= 0)
                    then newMF.Options := newMF.Options - [mfcInTown];
                  NewFacs.Insert( newMF );
                end;
          end;
      end;
    for facI := 0 to pred(NewFacs.Count) do
      TMetaFacility(NewFacs[facI]).Register( tidClassFamily_Facilities );
    NewFacs.Free;
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
    


