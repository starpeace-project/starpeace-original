{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W+,X+,Y-,Z1}
{$MINSTACKSIZE $00004000}
{$MAXSTACKSIZE $00100000}
{$IMAGEBASE $00400000}
{$APPTYPE GUI}

{$DEFINE USELogs}

library PGIPack1;

uses
  ShareMem,
  SysUtils,
  Windows,
  ClassStorage,
  Classes,
  Kernel in '..\..\Kernel\Kernel.pas',
  Population in '..\..\Kernel\Population.pas',
  Politics in '..\..\Kernel\Politics.pas',
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
  Mine in '..\..\StdBlocks\Mine.pas',
  StdFluids in '..\..\StdBlocks\StdFluids.pas',
  TextilIndustry in '..\..\StdBlocks\TextilIndustry.pas',
  PopulatedBlock in '..\..\Kernel\PopulatedBlock.pas',
  ResearchCenter in '..\..\Kernel\ResearchCenter.pas',
  Headquarters in '..\..\Kernel\Headquarters.pas',
  FoodStore in '..\..\StdBlocks\FoodStore.pas',
  Bar in '..\..\StdBlocks\Bar.pas',
  HHAStore in '..\..\StdBlocks\HHAStore.pas',
  SuperMarket in '..\..\StdBlocks\SuperMarket.pas',
  PGIConst in 'PGIConst.pas',
  FluidConsts in 'FluidConsts.pas',
  PublicFacility in '..\..\Kernel\PublicFacility.pas',
  ServiceBlock in '..\..\StdBlocks\ServiceBlock.pas',
  Restaurant in '..\..\StdBlocks\Restaurant.pas',
  Computing in '..\..\StdBlocks\Computing.pas',
  LegalServices in '..\..\StdBlocks\LegalServices.pas',
  Standards in '..\Standards.pas',
  CacheCommon in '..\..\Cache\CacheCommon.pas',
  Environmental in '..\..\StdBlocks\Environmental.pas',
  Land in '..\..\Land\Land.pas',
  SpontaneousBuildings in '..\..\Kernel\SpontaneousBuildings.pas',
  OfficeBlock in '..\..\StdBlocks\OfficeBlock.pas',
  NewsServerInterfaces in '..\..\News Server\NewsServerInterfaces.pas',
  Movie in '..\..\StdBlocks\Movie.pas',
  TransportInterfaces in '..\..\Transport\TransportInterfaces.pas',
  Transport in '..\..\Transport\Transport.pas',
  MatrixLayer in '..\..\Transport\MatrixLayer.pas',
  VisualClassManager in '..\..\Class Packer\VisualClassManager.pas',
  Broadcast in '..\..\StdBlocks\Broadcast.pas',
  StdBroadcast in '..\..\StdBlocks\StdBroadcast.pas',
  StdTaxes in '..\..\StdBlocks\StdTaxes.pas',
  StdAccounts in '..\..\StdBlocks\StdAccounts.pas',
  FacIds in '..\FacIds.pas',
  TownPolitics in '..\..\Kernel\TownPolitics.pas',
  Logs in '..\..\Logs\Logs.pas',
  ModelServerCache in '..\..\Cache\ModelServerCache.pas',
  Inventions in '..\..\Inventions\Inventions.pas',
  PharmaIndustry in '..\..\StdBlocks\PharmaIndustry.pas',
  DrugStore in '..\..\StdBlocks\DrugStore.pas',
  RankProtocol in '..\..\Protocol\RankProtocol.pas',
  GenIdd in '..\..\Utils\Serial\GenIdd.pas',
  CommonFacs in '..\CommonFacs.pas',
  CoalMine in '..\..\StdBlocks\CoalMine.pas',
  ChemMine in '..\..\StdBlocks\ChemMine.pas',
  StoneMine in '..\..\StdBlocks\StoneMine.pas',
  SiliconMine in '..\..\StdBlocks\SiliconMine.pas',
  SimMLS in '..\..\Kernel\SimMLS.pas',
  PaperIndustry in '..\..\StdBlocks\PaperIndustry.pas',
  PrintingPlant in '..\..\StdBlocks\PrintingPlant.pas',
  DirectoryServerProtocol in '..\..\DServer\DirectoryServerProtocol.pas';

procedure RegisterFacilityKinds;
  begin
    with TFacilityKind.Create( tidFacilityKind_PGIDistributedDirection ) do
      begin
        Name        := 'Headquarters';
        SuperType   := tidSuperFacKind_Headquarter;
        ClusterName := tidClusterName_PGI;
        Role        := rolBuyer;
        Register( tidClassFamily_FacilityKinds );
      end;
    with TFacilityKind.Create( tidFacilityKind_PGIFarms ) do
      begin
        Name        := 'Farms';
        SuperType   := tidSuperFacKind_Farm;
        ClusterName := tidClusterName_PGI;
        Technology  := tidInventionKind_IndustrialFacilities;
        Role        := rolProducer;
        Register( tidClassFamily_FacilityKinds );
      end;
    with TFacilityKind.Create( tidFacilityKind_PGIIndustrialFacilities ) do
      begin
        Name        := 'Factories';
        SuperType   := tidSuperFacKind_Industry;
        ClusterName := tidClusterName_PGI;
        Technology  := tidInventionKind_IndustrialFacilities;
        Role        := rolProducer;
        Register( tidClassFamily_FacilityKinds );
      end;
    with TFacilityKind.Create( tidFacilityKind_PGIResidentials ) do
      begin
        Name        := 'Residentials';
        SuperType   := tidSuperFacKind_Residential;
        ClusterName := tidClusterName_PGI;
        Register( tidClassFamily_FacilityKinds );
      end;
    with TFacilityKind.Create( tidFacilityKind_PGIServiceFacilities ) do
      begin
        Name        := 'Commerce';
        SuperType   := tidSuperFacKind_Service;
        ClusterName := tidClusterName_PGI;
        Technology  := tidInventionKind_ServiceFacilities;
        Role        := rolBuyer;
        Register( tidClassFamily_FacilityKinds );
      end;
    with TFacilityKind.Create( tidFacilityKind_PGIPublicFacilities ) do
      begin
        Name        := 'Public';
        SuperType   := tidSuperFacKind_Public;
        ClusterName := tidClusterName_PGI;
        Technology  := tidInventionKind_PublicFacilities;
        Register( tidClassFamily_FacilityKinds );
      end;
    with TFacilityKind.Create( tidFacilityKind_PGIBusinessFacilities ) do
      begin
        Name        := 'Offices';
        SuperType   := tidSuperFacKind_Business;
        ClusterName := tidClusterName_PGI;
        Technology  := tidInventionKind_OfficeAndResidentials;
        Register( tidClassFamily_FacilityKinds );
      end;
    with TFacilityKind.Create( tidFacilityKind_PGISpecial ) do
      begin
        Name        := 'Special';
        SuperType   := tidSuperFacKind_Special;
        ClusterName := tidClusterName_PGI;
        Role        := rolProducer;
        Register( tidClassFamily_FacilityKinds );
      end;
  end;
    
type
  TPGICluster =
    class( TCluster )
      public
        function NameNewspaper( TownName : string ) : string; override;
    end;
    
  function TPGICluster.NameNewspaper( TownName : string ) : string;
    begin
      result := TownName + ' Sun';
    end;

procedure RegisterClusterFacilities;
  begin
    TPGICluster.Create( tidClusterName_PGI ).Register( tidClassFamily_Clusters );
    RegisterTownHall( tidClusterName_PGI, tidFacility_PGITownHall, vidFacility_PGITownHall, 4, 4, 250, TPoliticalTownHall );
    RegisterTradeCenter( tidClusterName_PGI, tidFacility_PGITradeCenter, vidFacility_PGITradeCenter, 2, 2 );
  end;
    
procedure RegisterResidentials;
  begin
    // Low cost High Class buildings
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGILoCostHighClassConstr,
      40000,
      [100, 0, 0],
      7,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_PGILoCostHighClass,
      pkHigh,
      20,
      TPopulatedBlock ) do
      begin
        Beauty := -200;
        ModifyPrice := false;
        LowCost := true;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGILoCostHighClass, 'Generic high class buildings', vidFacility_PGILoCostHighClass, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        FacId := FID_hiClassLoCost;
        Options := Options - [mfcGenerateName, mfcInTown];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGILoCostHighClassConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGILoCostHighClass])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIResidentials;
        Register( tidClassFamily_Facilities );
      end;

    // Low cost MiddleClass buildings
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGILoCostMiddleClassConstr,
      200000,
      [100, 0, 0],
      7,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_PGILoCostMiddleClass,
      pkMiddle,
      30,
      TPopulatedBlock ) do
      begin
        Beauty := -200;
        ModifyPrice := false;
        LowCost := true;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGILoCostMiddleClass, 'Generic middle class buildings', vidFacility_PGILoCostMiddleClass, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        FacId := FID_midClassLoCost;
        Options := Options - [mfcGenerateName, mfcInTown];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGILoCostMiddleClassConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGILoCostMiddleClass])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIResidentials;
        Register( tidClassFamily_Facilities );
      end;

    // Low cost LowClass buildings
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGILoCostLowClassConstr,
      100000,
      [100, 0, 0],
      10,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_PGILoCostLowClass,
      pkLow,
      50,
      TPopulatedBlock ) do
      begin
        Beauty := -200;
        ModifyPrice := false;
        LowCost := true;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGILoCostLowClass, 'Generic low class building', vidFacility_PGILoCostLowClass, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        FacId := FID_lowClassLoCost;
        Options := Options - [mfcGenerateName, mfcInTown];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGILoCostLowClassConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGILoCostLowClass])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIResidentials;
        Register( tidClassFamily_Facilities );
      end;

    // >> PATCH!!!! (move this later to GeneralPack1.mdx)
    with TMetaPopulatedBlock.Create(
      tidFacility_SpontaneousBuilding,
      pkLow,
      50,
      TSpontaneousBuilding ) do
      begin
        Efficiency := -10;
        Beauty := -500;
        //BeautyStrength := 0.5;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_SpontaneousBuilding, 'Spontaneous building', vidFacility_PGILoCostLowClass + 1, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        Options := Options - [mfcGenerateName, mfcInTown];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidFacility_SpontaneousBuilding])));
        Register( tidClassFamily_Facilities );
      end;


    // HighClass Building A
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIHighClassBuildingConstrA,
      15000000,
      [100, 0, 0],
      25,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_PGIHighClassBuildingA,
      pkHigh,
      100,
      TPopulatedBlock ) do
      begin
        Efficiency := 1.2;
        Beauty := 100;
        VisualStages := 2;
        //BeautyStrength := 0.1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGIHighClassBuildingA, 'Xanadu Palace', vidFacility_PGIHighClassBuildingA, TFacility ) do
      begin
        XSize := 4;
        YSize := 4;
        Level := 120;
        FacId := FID_hiClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIHighClassBuildingConstrA])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIHighClassBuildingA])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // HighClass Building B
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIHighClassBuildingConstrB,
      3000000,
      [100, 0, 0],
      60,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_PGIHighClassBuildingB,
      pkHigh,
      60,
      TPopulatedBlock ) do
      begin
        Efficiency := 1;
        Beauty := 95;
        VisualStages := 2;
        //BeautyStrength := 0.1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGIHighClassBuildingB, 'Atrium', vidFacility_PGIHighClassBuildingB, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        FacId := FID_hiClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIHighClassBuildingConstrB])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIHighClassBuildingB])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // HighClass Building C
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIHighClassBuildingConstrC,
      3000000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_PGIHighClassBuildingC,
      pkHigh,
      40,
      TPopulatedBlock ) do
      begin
        Efficiency := 1.1;
        Beauty := 95;
        VisualStages := 2;
      //BeautyStrength := 0.1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGIHighClassBuildingC, 'The Willington', vidFacility_PGIHighClassBuildingC, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        FacId := FID_hiClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIHighClassBuildingConstrC])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIHighClassBuildingC])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // HighClass Building D
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIHighClassBuildingConstrD,
      2500000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_PGIHighClassBuildingD,
      pkHigh,
      50,
      TPopulatedBlock ) do
      begin
        Efficiency := 0.9;
        Beauty := 85;
        VisualStages := 2;
      //BeautyStrength := 0.1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGIHighClassBuildingD, 'Golden Cap', vidFacility_PGIHighClassBuildingD, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        FacId := FID_hiClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIHighClassBuildingConstrD])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIHighClassBuildingD])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // HighClass Building E
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIHighClassBuildingConstrE,
      2000000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_PGIHighClassBuildingE,
      pkHigh,
      23,
      TPopulatedBlock ) do
      begin
        Efficiency := 1;
        Beauty := 85;
        VisualStages := 2;
      //BeautyStrength := 0.1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGIHighClassBuildingE, 'Kent Square', vidFacility_PGIHighClassBuildingE, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        FacId := FID_hiClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIHighClassBuildingConstrE])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIHighClassBuildingE])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // HighClass Building F
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIHighClassBuildingConstrF,
      2000000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_PGIHighClassBuildingF,
      pkHigh,
      25,
      TPopulatedBlock ) do
      begin
        Efficiency := 1;
        Beauty := 90;
        VisualStages := 2;
      //BeautyStrength := 0.1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGIHighClassBuildingF, 'Lisgar Towers', vidFacility_PGIHighClassBuildingF, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        FacId := FID_hiClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIHighClassBuildingConstrF])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIHighClassBuildingF])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // HighClass Building J
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIHighClassBuildingConstrJ,
      3000000,
      [100, 0, 0],
      60,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_PGIHighClassBuildingJ,
      pkHigh,
      60,
      TPopulatedBlock ) do
      begin
        Efficiency := 1;
        Beauty := 95;
        VisualStages := 2;
        //BeautyStrength := 0.1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGIHighClassBuildingJ, 'Pink Mansion', vidFacility_PGIHighClassBuildingJ, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        FacId := FID_hiClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIHighClassBuildingConstrJ])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIHighClassBuildingJ])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // HighClass Building G
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIHighClassBuildingConstrG,
      3000000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_PGIHighClassBuildingG,
      pkHigh,
      40,
      TPopulatedBlock ) do
      begin
        Efficiency := 1.1;
        Beauty := 95;
        VisualStages := 2;
      //BeautyStrength := 0.1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGIHighClassBuildingG, 'White Hut', vidFacility_PGIHighClassBuildingG, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        FacId := FID_hiClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIHighClassBuildingConstrG])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIHighClassBuildingG])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // HighClass Building H
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIHighClassBuildingConstrH,
      2000000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_PGIHighClassBuildingH,
      pkHigh,
      60,
      TPopulatedBlock ) do
      begin
        Efficiency := 0.8;
        Beauty := 85;
        VisualStages := 2;
      //BeautyStrength := 0.1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGIHighClassBuildingH, 'Lyon Rooms', vidFacility_PGIHighClassBuildingH, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        FacId := FID_hiClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIHighClassBuildingConstrH])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIHighClassBuildingH])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // HighClass Building D
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIHighClassBuildingConstrI,
      3000000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_PGIHighClassBuildingI,
      pkHigh,
      55,
      TPopulatedBlock ) do
      begin
        Efficiency := 1;
        Beauty := 85;
        VisualStages := 2;
      //BeautyStrength := 0.1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGIHighClassBuildingI, 'Blue Mansion', vidFacility_PGIHighClassBuildingI, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        FacId := FID_hiClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIHighClassBuildingConstrI])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIHighClassBuildingI])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // Middle Class Building A
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIMiddleClassBuildingConstrA,
      3000000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_PGIMiddleClassbuildingA,
      pkMiddle,
      120,
      TPopulatedBlock ) do
      begin
        Efficiency := 1;
        Beauty := 100;
        VisualStages := 2;
      //BeautyStrength := 0.1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGIMiddleClassbuildingA, 'Gladston Apts.', vidFacility_PGIMiddleClassbuildingA, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 120;
        FacId := FID_midClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIMiddleClassbuildingConstrA])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIMiddleClassbuildingA])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // Middle Class Building B
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIMiddleClassbuildingConstrB,
      2700000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_PGIMiddleClassbuildingB,
      pkMiddle,
      100,
      TPopulatedBlock ) do
      begin
        Efficiency := 1;
        Beauty := 100;
        VisualStages := 2;
      //BeautyStrength := 0.1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGIMiddleClassbuildingB, 'Quincy', vidFacility_PGIMiddleClassbuildingB, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 120;
        FacId := FID_midClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIMiddleClassbuildingConstrB])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIMiddleClassbuildingB])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // Middle Class Building C
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIMiddleClassbuildingConstrC,
      3000000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_PGIMiddleClassbuildingC,
      pkMiddle,
      100,
      TPopulatedBlock ) do
      begin
        Efficiency := 1.1;
        Beauty := 100;
        VisualStages := 2;
      //BeautyStrength := 0.1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGIMiddleClassbuildingC, 'Metcalfe Towers', vidFacility_PGIMiddleClassbuildingC, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 120;
        FacId := FID_midClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIMiddleClassbuildingConstrC])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIMiddleClassbuildingC])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;
    
    // Middle Class Building D
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIMiddleClassbuildingConstrD,
      3000000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_PGIMiddleClassbuildingD,
      pkMiddle,
      120,
      TPopulatedBlock ) do
      begin
        Efficiency := 1;
        Beauty := 100;
        VisualStages := 2;
      //BeautyStrength := 0.1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGIMiddleClassbuildingD, 'Delmar Apts.', vidFacility_PGIMiddleClassbuildingD, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 120;
        FacId := FID_midClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIMiddleClassbuildingConstrD])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIMiddleClassbuildingD])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // Middle Class Building E
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIMiddleClassbuildingConstrE,
      10000000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_PGIMiddleClassbuildingE,
      pkMiddle,
      200,
      TPopulatedBlock ) do
      begin
        Efficiency := 1;
        Beauty := 100;
        VisualStages := 2;
      //BeautyStrength := 0.1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGIMiddleClassbuildingE, 'Sunset Apts.', vidFacility_PGIMiddleClassbuildingE, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 120;
        FacId := FID_midClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIMiddleClassbuildingConstrE])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIMiddleClassbuildingE])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;
    
    // Low Class Building A
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGILowClassbuildingConstrA,
      2000000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_PGILowClassbuildingA,
      pkLow,
      250,
      TPopulatedBlock ) do
      begin
        Efficiency := 0.8;
        Beauty := 70;
        VisualStages := 2;
      //BeautyStrength := 0.1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGILowClassbuildingA, 'Playfair Towers', vidFacility_PGILowClassbuildingA, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 120;
        FacId := FID_lowClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGILowClassbuildingConstrA])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGILowClassbuildingA])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // Low Class Building B
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGILowClassbuildingConstrB,
      3000000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_PGILowClassbuildingB,
      pkLow,
      300,
      TPopulatedBlock ) do
      begin
        Efficiency := 0.9;
        Beauty := 75;
        VisualStages := 2;
      //BeautyStrength := 0.1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGILowClassbuildingB, 'Arlington Rooms', vidFacility_PGILowClassbuildingB, TFacility ) do
      begin
        XSize := 3;
        YSize := 3;
        Level := 120;
        FacId := FID_lowClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGILowClassbuildingConstrB])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGILowClassbuildingB])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // Low Class Building B
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGILowClassbuildingConstrC,
      2500000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_PGILowClassbuildingC,
      pkLow,
      160,
      TPopulatedBlock ) do
      begin
        Efficiency := 0.9;
        Beauty := 75;
        VisualStages := 2;
      //BeautyStrength := 0.1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGILowClassbuildingC, 'Caesar''s Atrium', vidFacility_PGILowClassbuildingC, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        FacId := FID_lowClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGILowClassbuildingConstrC])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGILowClassbuildingC])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // Low Class Building D
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGILowClassbuildingConstrD,
      2000000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_PGILowClassbuildingD,
      pkLow,
      250,
      TPopulatedBlock ) do
      begin
        Efficiency := 0.8;
        Beauty := 70;
        VisualStages := 2;
      //BeautyStrength := 0.1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGILowClassbuildingD, 'The Sicily', vidFacility_PGILowClassbuildingD, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        FacId := FID_lowClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGILowClassbuildingConstrD])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGILowClassbuildingD])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // Low Class Building E
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGILowClassbuildingConstrE,
      3000000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_PGILowClassbuildingE,
      pkLow,
      290,
      TPopulatedBlock ) do
      begin
        Efficiency := 0.95;
        Beauty := 65;
        VisualStages := 2;
      //BeautyStrength := 0.1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGILowClassbuildingE, 'Mountbatten Rooms', vidFacility_PGILowClassbuildingE, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        FacId := FID_lowClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGILowClassbuildingConstrE])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGILowClassbuildingE])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // Low Class Building F
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGILowClassbuildingConstrF,
      2000000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_PGILowClassbuildingF,
      pkLow,
      250,
      TPopulatedBlock ) do
      begin
        Efficiency := 0.9;
        Beauty := 70;
        VisualStages := 2;
      //BeautyStrength := 0.1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGILowClassbuildingF, 'The Ambassador', vidFacility_PGILowClassbuildingF, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 120;
        FacId := FID_lowClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGILowClassbuildingConstrF])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGILowClassbuildingF])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

  end;

procedure RegisterOffices;
  begin
    // Office A
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIOfficeBuildingConstrA,
      10000000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaOfficeBlock.Create(
      tidBlock_PGIOfficeBuildingA,
      70,
      TOfficeBlock ) do
      begin
        Efficiency := 0.8;
        Beauty := 75;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGIOfficeBuildingA, 'Sting', vidFacility_PGIOfficeBuildingA, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        FacId := FID_Office;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIOfficeBuildingConstrA])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIOfficeBuildingA])));
        ClusterName := tidClusterName_PGI;
        TechnologyKind := tidInventionKind_Offices;
        FacilityKind := tidFacilityKind_PGIBusinessFacilities;
        MinistryId := nidMinistry_Commerce;
        Register( tidClassFamily_Facilities );
      end;

    // Office B
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIOfficeBuildingConstrB,
      20000000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaOfficeBlock.Create(
      tidBlock_PGIOfficeBuildingB,
      120,
      TOfficeBlock ) do
      begin
        Efficiency := 0.9;
        Beauty := 80;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGIOfficeBuildingB, 'Davis Bow', vidFacility_PGIOfficeBuildingB, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        FacId := FID_Office;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIOfficeBuildingConstrB])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIOfficeBuildingB])));
        ClusterName := tidClusterName_PGI;
        TechnologyKind := tidInventionKind_Offices;
        FacilityKind := tidFacilityKind_PGIBusinessFacilities;
        MinistryId := nidMinistry_Commerce;
        Register( tidClassFamily_Facilities );
      end;

    // Office C
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIOfficeBuildingConstrC,
      15000000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaOfficeBlock.Create(
      tidBlock_PGIOfficeBuildingC,
      90,
      TOfficeBlock ) do
      begin
        Efficiency := 0.75;
        Beauty := 70;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGIOfficeBuildingC, 'Mc Neon Tower', vidFacility_PGIOfficeBuildingC, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        FacId := FID_Office;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIOfficeBuildingConstrC])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIOfficeBuildingC])));
        ClusterName := tidClusterName_PGI;
        TechnologyKind := tidInventionKind_Offices;
        FacilityKind := tidFacilityKind_PGIBusinessFacilities;
        MinistryId := nidMinistry_Commerce;
        Register( tidClassFamily_Facilities );
      end;

    // Office D
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIOfficeBuildingConstrD,
      15000000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaOfficeBlock.Create(
      tidBlock_PGIOfficeBuildingD,
      70,
      TOfficeBlock ) do
      begin
        Efficiency := 0.9;
        Beauty := 78;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGIOfficeBuildingD, 'The Spire', vidFacility_PGIOfficeBuildingD, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        FacId := FID_Office;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIOfficeBuildingConstrD])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIOfficeBuildingD])));
        ClusterName := tidClusterName_PGI;
        TechnologyKind := tidInventionKind_Offices;
        FacilityKind := tidFacilityKind_PGIBusinessFacilities;
        MinistryId := nidMinistry_Commerce;
        Register( tidClassFamily_Facilities );
      end;

  end;

procedure RegisterPublicFacilities;
  begin
    // Hospital
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIHospitalConstr,
      10000000,
      [90, 5, 5],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPublicFacility.Create(
      tidBlock_PGIHospital,
      [20, 50, 30],
      [PFInfoDef(tidPublicFac_Health, 40000)],
      TPublicFacility ) do
      begin
        MaintCost := 1000;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGIHospital, 'Hospital', vidFacility_PGIHospital, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        SlotCount := 0;
        FacId := FID_Hospital;
        DemoLapse := 1;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIHospitalConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIHospital])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIPublicFacilities;
        TechnologyKind := tidInventionKind_PublicFacilities;
        MinistryId := nidMinistry_Health;
        Register( tidClassFamily_Facilities );
      end;

    // School
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGISchoolConstr,
      5000000,
      [90, 5, 5],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPublicFacility.Create(
      tidBlock_PGISchool,
      [1, 20, 10],
      [PFInfoDef(tidPublicFac_School, 20000)],
      TPublicFacility ) do
      begin
        MaintCost := 500;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGISchool, 'School', vidFacility_PGISchool, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 120;
        SlotCount := 0;
        FacId := FID_School;
        DemoLapse := 1;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGISchoolConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGISchool])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIPublicFacilities;
        TechnologyKind := tidInventionKind_PublicFacilities;
        MinistryId := nidMinistry_Education;
        Register( tidClassFamily_Facilities );
      end;

    // Police
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIPoliceConstr,
      3000000,
      [90, 5, 5],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPublicFacility.Create(
      tidBlock_PGIPolice,
      [0, 1, 60],
      [PFInfoDef(tidPublicFac_Police, 5000)],
      TPublicFacility ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGIPolice, 'Police Station', vidFacility_PGIPolice, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 120;
        SlotCount := 0;
        FacId := FID_Police;
        DemoLapse := 1;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIPoliceConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIPolice])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIPublicFacilities;
        TechnologyKind := tidInventionKind_PublicFacilities;
        MinistryId := nidMinistry_Defense;
        Register( tidClassFamily_Facilities );
      end;

    // Fire
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIFireConstr,
      2000000,
      [90, 5, 5],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPublicFacility.Create(
      tidBlock_PGIFire,
      [0, 1, 25],
      [PFInfoDef(tidPublicFac_Fire, 10000)],
      TPublicFacility ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGIFire, 'Fire Station', vidFacility_PGIFire, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        SlotCount := 0;
        Level := 120;
        FacId := FID_FireStation;
        DemoLapse := 1;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIFireConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIFire])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIPublicFacilities;
        TechnologyKind := tidInventionKind_PublicFacilities;
        MinistryId := nidMinistry_Defense;
        Register( tidClassFamily_Facilities );
      end;

    // Small park
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGISmallParkConstr,
      1000000,
      [100, 0, 0],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaEnvironmentalBlock.Create(
      tidBlock_PGISmallPark,
      TEnvironmentalBlock ) do
      begin
        RegisterModifier( tidEnvironment_Beauty, 300, 10 );
        RegisterModifier( tidEnvironment_Pollution, -300, 10 );
        MaintCost := 100;
        DissabledStop := true;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGISmallPark, 'Small Park', vidFacility_PGISmallPark, TFacility ) do
      begin
        xSize := 3;
        ySize := 3;
        Level := 200;
        FacId := FID_Park;
        DemoLapse := 1;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGISmallParkConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Park Completed', 'Park is done', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGISmallPark])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIPublicFacilities;
        TechnologyKind := tidInventionKind_PublicFacilities;
        MinistryId := nidMinistry_Health;
        Register( tidClassFamily_Facilities );
      end;

    // Medium park
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIMediumParkConstr,
      2000000,
      [100, 0, 0],
      22,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaEnvironmentalBlock.Create(
      tidBlock_PGIMediumPark,
      TEnvironmentalBlock ) do
      begin
        RegisterModifier( tidEnvironment_Beauty, 400, 10 );
        RegisterModifier( tidEnvironment_Pollution, -400, 10 );
        MaintCost := 250;
        DissabledStop := true;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGIMediumPark, 'Park', vidFacility_PGIMediumPark, TFacility ) do
      begin
        XSize := 5;
        YSize := 5;
        Level := 200;
        FacId := FID_Park;
        DemoLapse := 1;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIMediumParkConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Park Completed', 'Building is done', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGIMediumPark])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIPublicFacilities;
        TechnologyKind := tidInventionKind_PublicFacilities;
        MinistryId := nidMinistry_Health;
        Register( tidClassFamily_Facilities );
      end;

    // Central park
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGICentralParkConstr,
      3000000,
      [100, 0, 0],
      22,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaEnvironmentalBlock.Create(
      tidBlock_PGICentralPark,
      TEnvironmentalBlock ) do
      begin
        RegisterModifier( tidEnvironment_Beauty, 500, 10 );
        RegisterModifier( tidEnvironment_Pollution, -500, 10 );
        MaintCost := 400;
        DissabledStop := true;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGICentralPark, 'Big Park', vidFacility_PGICentralPark, TFacility ) do
      begin
        XSize := 7;
        YSize := 7;
        Level := 200;
        FacId := FID_Park;
        DemoLapse := 1;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGICentralParkConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Park Completed', 'Building is done', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGICentralPark])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIPublicFacilities;
        TechnologyKind := tidInventionKind_PublicFacilities;
        MinistryId := nidMinistry_Health;
        Register( tidClassFamily_Facilities );
      end;

    // Statue of Liberty
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGILibertyConstr,
      costLiberty,
      [100, 0, 0],
      50,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaEnvironmentalBlock.Create(
      tidBlock_PGILiberty,
      TEnvironmentalBlock ) do
      begin
        Prestige := prestLiberty;
        RegisterModifier( tidEnvironment_Beauty, 500, 10 );
        MinColDist := 5;
        ColIsSameComp := false;
        MaintCost := 1000;
        DissabledStop := true;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGILiberty, 'Statue of Liberty', vidFacility_PGILiberty, TFacility ) do
      begin
        XSize := 4;
        YSize := 4;
        Level := 200;
        FacId := FID_LuxuryFac;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGILibertyConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Park Completed', 'Building is done', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGILiberty])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIPublicFacilities;
        TechnologyKind := tidInventionKind_Monuments;
        MinistryId := nidMinistry_Health;
        Register( tidClassFamily_Facilities );
      end;

    // IFEL Tower
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGITowerConstr,
      costIFELTower,
      [100, 0, 0],
      50,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaEnvironmentalBlock.Create(
      tidBlock_PGITower,
      TEnvironmentalBlock ) do
      begin
        Prestige := prestIFELTower;
        RegisterModifier( tidEnvironment_Beauty, 500, 10 );
        MinColDist := 5;
        ColIsSameComp := false;
        MaintCost := 1000;
        DissabledStop := true;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGITower, 'IFEL Tower', vidFacility_PGITower, TFacility ) do
      begin
        XSize := 4;
        YSize := 4;
        Level := 200;
        FacId := FID_LuxuryFac;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGITowerConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Park Completed', 'Building is done', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_PGITower])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIPublicFacilities;
        TechnologyKind := tidInventionKind_Monuments;
        Register( tidClassFamily_Facilities );
      end;

    // Common Public services
    CopyCommonFacilities( tidBlock_SRVCOMMON, tidClusterName_PGI, tidFacilityKind_PGIPublicFacilities, tidInventionKind_PublicFacilities );
    // Common Special
    CopyCommonFacilities( tidBlock_SPECIALCOMMON, tidClusterName_PGI, tidFacilityKind_PGISpecial, '' );
    // Common Warehouses
    CopyCommonFacilities(tidBlock_WHCOMMON, tidClusterName_PGI, tidFacilityKind_PGIIndustrialFacilities, '');
  end;

procedure RegisterIndustries;
  begin
    // Farm Small
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGISmallFarmConstr,
      cost_FarmSmall,
      [80, 20, 0],
      15,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFarmBlock.Create(
      tidBlock_PGISmallFarm,
      [input_FarmSmall_ExecutivesWorkForce, input_FarmSmall_ProfessionalWorkForce, input_FarmSmall_Workers],
      input_FarmSmall_Chemicals,
      0, // input_FarmSmall_LegalServices,
      0, // input_FarmSmall_ComputerServices,
      output_FarmSmall_FreshFood,
      output_FarmSmall_OrganicMaterials,
      budget_FarmSmall,
      TFarmBlock ) do
      begin
        VisualStages := 1;
        Beauty := -20;
      //BeautyStrength := 0.1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGISmallFarm, 'Farm', vidFacility_PGISmallFarm, TFacility ) do
      begin
        xSize := 3;
        ySize := 3;
        Level := 20;
        FacId := FID_Farm;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGISmallFarmConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The farm is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGISmallFarm])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIFarms;
        TechnologyKind := tidLicence_Farms;
        MinistryId := nidMinistry_Agriculture;
        Register( tidClassFamily_Facilities );
      end;

    // Farm
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIFarmConstr,
      cost_Farm,
      [80, 15, 5],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFarmBlock.Create(
      tidBlock_PGIFarm,
      [input_Farm_ExecutivesWorkForce, input_Farm_ProfessionalWorkForce, input_Farm_Workers],
      input_Farm_Chemicals,
      0, // input_Farm_LegalServices,
      0, // input_Farm_ComputerServices,
      output_Farm_FreshFood,
      output_Farm_OrganicMaterials,
      budget_Farm,
      TFarmBlock ) do
      begin
        Beauty := -50;
      //BeautyStrength := 0.1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGIFarm, 'Large Farm', vidFacility_PGIFarm, TFacility ) do
      begin
        xSize := 4;
        ySize := 4;
        Level := 20;
        FacId := FID_Farm;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIFarmConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Farm is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIFarm])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIFarms;
        TechnologyKind := tidInventionKind_LargeFarms;
        MinistryId := nidMinistry_Agriculture;
        Register( tidClassFamily_Facilities );
      end;
    
    // Small Chemical plant
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGISmallChemicalConstr,
      cost_ChemicalPlantSmall,
      [50, 40, 10],
      50,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaChemicalBlock.Create(
      tidBlock_PGISmallChemical,
      [input_ChemicalPlantSmall_ExecutivesWorkForce, input_ChemicalPlantSmall_ProfessionalWorkForce, input_ChemicalPlantSmall_Workers],
      input_ChemicalPlantSmall_LegalServices,
      input_ChemicalPlantSmall_ComputerServices,
      input_ChemicalPlantSmall_Ore,
      output_ChemicalPlantSmall_Chemicals,
      budget_ChemicalPlantSmall,
      TChemicalBlock ) do
      begin
        Beauty := -40;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGISmallChemical, 'Chemical Plant', vidFacility_PGISmallChemical, TFacility ) do
      begin
        xSize := 3;
        ySize := 3;
        Level := 15;
        FacId := FID_Chemical;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGISmallChemicalConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGISmallChemical])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIIndustrialFacilities;
        TechnologyKind := tidLicence_Chemical;
        MinistryId := nidMinistry_HeavyIndustry;
        Register( tidClassFamily_Facilities );
      end;

    // Chemical plant
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIChemicalConstr,
      cost_ChemicalPlant,
      [50, 40, 10],
      50,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaChemicalBlock.Create(
      tidBlock_PGIChemical,
      [input_ChemicalPlant_ExecutivesWorkForce, input_ChemicalPlant_ProfessionalWorkForce, input_ChemicalPlant_Workers],
      input_ChemicalPlant_LegalServices,
      input_ChemicalPlant_ComputerServices,
      input_ChemicalPlant_Ore,
      output_ChemicalPlant_Chemicals,
      budget_ChemicalPlant,
      TChemicalBlock ) do
      begin
        Beauty := -40;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGIChemical, 'Large Chemical Plant', vidFacility_PGIChemical, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 15;
        FacId := FID_Chemical;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIChemicalConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIChemical])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIIndustrialFacilities;
        TechnologyKind := tidInventionKind_LargeChemical;
        MinistryId := nidMinistry_HeavyIndustry;
        Register( tidClassFamily_Facilities );
      end;

    // Paper industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIPaperConstr, // Id
      50000000,                // Cost
      [50, 40, 10],            // %Const %Mach %BMach
      50,                      // Const time
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPaperBlock.Create(
      tidBlock_PGIPaper, // Id
      [1, 6, 100],
      0.1,
      0.2,
      100*2,    // Wood
      100*0.2,  // OrgMat
      100*0.1,  // Chemicals
      100*1,    // Paper
      0,
      TPaperBlock ) do
      begin
        Beauty := -40;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGIPaper, 'Paper Industry', vidFacility_PGIPaper, TFacility ) do
      begin
        xSize := 4;
        ySize := 4;
        Level := 15;
        FacId := FID_PaperInd;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIPaperConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIPaper])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIIndustrialFacilities;
        TechnologyKind := tidInventionKind_PaperIndustry;
        MinistryId := nidMinistry_HeavyIndustry;
        Register( tidClassFamily_Facilities );
      end;

    // Printing plant
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIPrintingConstr, // Id
      50000000,                // Cost
      [40, 50, 10],            // %Const %Mach %BMach
      50,                      // Const time
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPrintingBlock.Create(
      tidBlock_PGIPrinting, // Id
      [1, 6, 100],
      0.1,
      0.2,
      100*0.6,  // Paper
      100*0.1,  // OrgMat
      100*0.2,  // Chemicals
      100*1,    // Book
      100*0.5,  // PrintedMaterial
      0,
      TPrintingBlock ) do
      begin
        Beauty := -40;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGIPrinting, 'Printing Plant', vidFacility_PGIPrinting, TFacility ) do
      begin
        xSize := 4;
        ySize := 4;
        Level := 15;
        FacId := FID_PrintingPlant;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIPrintingConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIPrinting])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIIndustrialFacilities;
        TechnologyKind := tidInventionKind_PrintingPlant;
        MinistryId := nidMinistry_HeavyIndustry;
        Register( tidClassFamily_Facilities );
      end;  

      
    // Pharmaceutics Industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIPharmaIndustryConstr,
      cost_PharmaIndustry,
      [50, 40, 10],
      50,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPharmaIndustryBlock.Create(
      tidBlock_PGIPharmaIndustry,
      [input_PharmaIndustry_ExecutivesWorkForce, input_PharmaIndustry_ProfessionalWorkForce, input_PharmaIndustry_Workers],
      input_PharmaIndustry_Chemicals,
      input_PharmaIndustry_Plastics,
      input_PharmaIndustry_LegalServices,
      input_PharmaIndustry_ComputerServices,
      output_PharmaIndustry_Drugs,
      budget_PharmaIndustry,
      TPharmaIndustryBlock ) do
      begin
        Beauty := -40;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGIPharmaIndustry, 'Pharmaceutical Industry', vidFacility_PGIPharmaIndustry, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 15;
        FacId := FID_Pharmaceutics;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIPharmaIndustryConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIPharmaIndustry])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIIndustrialFacilities;
        TechnologyKind := tidLicence_PharmaIndustry;
        MinistryId := nidMinistry_LightIndustry;
        Register( tidClassFamily_Facilities );
      end;

    // Small Mine
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGISmallMineConstr,
      cost_MineSmall,
      [45, 40, 15],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaMineBlock.Create(
      tidBlock_PGISmallMine,
      [input_MineSmall_ExecutivesWorkForce, input_MineSmall_ProfessionalWorkForce, input_MineSmall_Workers],
      input_MineSmall_LegalServices,
      input_MineSmall_ComputerServices,
      input_MineSmall_Chemicals,
      output_MineSmall_Ore,
      budget_MineSmall,
      TMineBlock ) do
      begin
        Beauty := -100;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGISmallMine, 'Mine', vidFacility_PGISmallMine, TFacility ) do
      begin
        xSize := 3;
        ySize := 3;
        Level := 12;
        FacId := FID_Mine;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGISmallMineConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The mine is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGISmallMine])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIIndustrialFacilities;      
        TechnologyKind := tidLicence_Mines;
        MinistryId := nidMinistry_HeavyIndustry;
        Register( tidClassFamily_Facilities );
      end;

    // Mine
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIMineConstr,
      cost_Mine,
      [45, 40, 15],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaMineBlock.Create(
      tidBlock_PGIMine,
      [input_Mine_ExecutivesWorkForce, input_Mine_ProfessionalWorkForce, input_Mine_Workers],
      input_Mine_LegalServices,
      input_Mine_ComputerServices,
      input_Mine_Chemicals,
      output_Mine_Ore,
      budget_Mine,
      TMineBlock ) do
      begin
        Beauty := -100;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGIMine, 'Large Mine', vidFacility_PGIMine, TFacility ) do
      begin
        xSize := 3;
        ySize := 3;
        Level := 12;
        FacId := FID_Mine;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIMineConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The mine is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIMine])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIIndustrialFacilities;
        TechnologyKind := tidInventionKind_LargeMines;
        MinistryId := nidMinistry_HeavyIndustry;
        Register( tidClassFamily_Facilities );
      end;

    // Small Chemical Mine
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGISmallChemMineConstr,
      cost_MineSmall,
      [45, 40, 15],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaChemMineBlock.Create(
      tidBlock_PGISmallChemMine,
      [input_MineSmall_ExecutivesWorkForce, input_MineSmall_ProfessionalWorkForce, input_MineSmall_Workers],
      input_MineSmall_LegalServices,
      input_MineSmall_ComputerServices,
      input_MineSmall_Chemicals,
      output_MineSmall_Ore,
      budget_MineSmall,
      TChemMineBlock ) do
      begin
        Beauty := -100;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGISmallChemMine, 'Chemical Mine', vidFacility_PGISmallChemMine, TFacility ) do
      begin
        xSize := 3;
        ySize := 3;
        Level := 12;
        FacId := FID_ChemicalMine;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGISmallChemMineConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The mine is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGISmallChemMine])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIIndustrialFacilities;
        TechnologyKind := tidLicence_ChemMines;
        MinistryId := nidMinistry_HeavyIndustry;
        Register( tidClassFamily_Facilities );
      end;

    // Chemical Mine
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIChemMineConstr,
      cost_Mine,
      [45, 40, 15],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaChemMineBlock.Create(
      tidBlock_PGIChemMine,
      [input_Mine_ExecutivesWorkForce, input_Mine_ProfessionalWorkForce, input_Mine_Workers],
      input_Mine_LegalServices,
      input_Mine_ComputerServices,
      input_Mine_Chemicals,
      output_Mine_Ore,
      budget_Mine,
      TChemMineBlock ) do
      begin
        Beauty := -100;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGIChemMine, 'Large Chemical Mine', vidFacility_PGIChemMine, TFacility ) do
      begin
        xSize := 3;
        ySize := 3;
        Level := 12;
        FacId := FID_ChemicalMine;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIChemMineConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The mine is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIChemMine])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIIndustrialFacilities;
        TechnologyKind := tidInventionKind_LargeChemMines;
        MinistryId := nidMinistry_HeavyIndustry;
        Register( tidClassFamily_Facilities );
      end;

    // Small Silicon Mine
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGISmallSiliconMineConstr,
      cost_MineSmall,
      [45, 40, 15],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaSiliconMineBlock.Create(
      tidBlock_PGISmallSiliconMine,
      [input_MineSmall_ExecutivesWorkForce, input_MineSmall_ProfessionalWorkForce, input_MineSmall_Workers],
      input_MineSmall_LegalServices,
      input_MineSmall_ComputerServices,
      input_MineSmall_Chemicals,
      output_MineSmall_Ore,
      budget_MineSmall,
      TSiliconMineBlock ) do
      begin
        Beauty := -100;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGISmallSiliconMine, 'Silicon Mine', vidFacility_PGISmallSiliconMine, TFacility ) do
      begin
        xSize := 3;
        ySize := 3;
        Level := 12;
        FacId := FID_SiliconMine;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGISmallSiliconMineConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The mine is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGISmallSiliconMine])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIIndustrialFacilities;
        TechnologyKind := tidLicence_SiliconMines;
        MinistryId := nidMinistry_HeavyIndustry;
        Register( tidClassFamily_Facilities );
      end;

    // Silicon Mine
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGISiliconMineConstr,
      cost_Mine,
      [45, 40, 15],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaSiliconMineBlock.Create(
      tidBlock_PGISiliconMine,
      [input_Mine_ExecutivesWorkForce, input_Mine_ProfessionalWorkForce, input_Mine_Workers],
      input_Mine_LegalServices,
      input_Mine_ComputerServices,
      input_Mine_Chemicals,
      output_Mine_Ore,
      budget_Mine,
      TSiliconMineBlock ) do
      begin
        Beauty := -100;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGISiliconMine, 'Large Silicon Mine', vidFacility_PGISiliconMine, TFacility ) do
      begin
        xSize := 3;
        ySize := 3;
        Level := 12;
        FacId := FID_SiliconMine;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGISiliconMineConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The mine is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGISiliconMine])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIIndustrialFacilities;
        TechnologyKind := tidInventionKind_LargeSiliconMines;
        MinistryId := nidMinistry_HeavyIndustry;
        Register( tidClassFamily_Facilities );
      end;

    // Small Stone Mine
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGISmallStoneMineConstr,
      cost_MineSmall,
      [45, 40, 15],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaStoneMineBlock.Create(
      tidBlock_PGISmallStoneMine,
      [input_MineSmall_ExecutivesWorkForce, input_MineSmall_ProfessionalWorkForce, input_MineSmall_Workers],
      input_MineSmall_LegalServices,
      input_MineSmall_ComputerServices,
      input_MineSmall_Chemicals,
      output_MineSmall_Ore,
      budget_MineSmall,
      TStoneMineBlock ) do
      begin
        Beauty := -100;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGISmallStoneMine, 'Stone Mine', vidFacility_PGISmallStoneMine, TFacility ) do
      begin
        xSize := 3;
        ySize := 3;
        Level := 12;
        FacId := FID_StoneMine;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGISmallStoneMineConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The mine is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGISmallStoneMine])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIIndustrialFacilities;
        TechnologyKind := tidLicence_StoneMines;
        MinistryId := nidMinistry_HeavyIndustry;
        Register( tidClassFamily_Facilities );
      end;

    // Stone Mine
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIStoneMineConstr,
      cost_Mine,
      [45, 40, 15],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaStoneMineBlock.Create(
      tidBlock_PGIStoneMine,
      [input_Mine_ExecutivesWorkForce, input_Mine_ProfessionalWorkForce, input_Mine_Workers],
      input_Mine_LegalServices,
      input_Mine_ComputerServices,
      input_Mine_Chemicals,
      output_Mine_Ore,
      budget_Mine,
      TStoneMineBlock ) do
      begin
        Beauty := -100;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGIStoneMine, 'Large Stone Mine', vidFacility_PGIStoneMine, TFacility ) do
      begin
        xSize := 3;
        ySize := 3;
        Level := 12;
        FacId := FID_StoneMine;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIStoneMineConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The mine is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIStoneMine])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIIndustrialFacilities;
        TechnologyKind := tidInventionKind_LargeStoneMines;
        MinistryId := nidMinistry_HeavyIndustry;
        Register( tidClassFamily_Facilities );
      end;

    // Small Coal Mine
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGISmallCoalMineConstr,
      cost_MineSmall,
      [45, 40, 15],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaCoalMineBlock.Create(
      tidBlock_PGISmallCoalMine,
      [input_MineSmall_ExecutivesWorkForce, input_MineSmall_ProfessionalWorkForce, input_MineSmall_Workers],
      input_MineSmall_LegalServices,
      input_MineSmall_ComputerServices,
      input_MineSmall_Chemicals,
      output_MineSmall_Ore,
      budget_MineSmall,
      TCoalMineBlock ) do
      begin
        Beauty := -100;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGISmallCoalMine, 'Coal Mine', vidFacility_PGISmallCoalMine, TFacility ) do
      begin
        xSize := 3;
        ySize := 3;
        Level := 12;
        FacId := FID_CoalMine;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGISmallCoalMineConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The mine is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGISmallCoalMine])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIIndustrialFacilities;
        TechnologyKind := tidLicence_CoalMines;
        MinistryId := nidMinistry_HeavyIndustry;
        Register( tidClassFamily_Facilities );
      end;

    // Coal Mine
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGICoalMineConstr,
      cost_Mine,
      [45, 40, 15],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaCoalMineBlock.Create(
      tidBlock_PGICoalMine,
      [input_Mine_ExecutivesWorkForce, input_Mine_ProfessionalWorkForce, input_Mine_Workers],
      input_Mine_LegalServices,
      input_Mine_ComputerServices,
      input_Mine_Chemicals,
      output_Mine_Ore,
      budget_Mine,
      TCoalMineBlock ) do
      begin
        Beauty := -100;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGICoalMine, 'Large Coal Mine', vidFacility_PGICoalMine, TFacility ) do
      begin
        xSize := 3;
        ySize := 3;
        Level := 12;
        FacId := FID_CoalMine;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGICoalMineConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The mine is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGICoalMine])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIIndustrialFacilities;
        TechnologyKind := tidInventionKind_LargeCoalMines;
        MinistryId := nidMinistry_HeavyIndustry;
        Register( tidClassFamily_Facilities );
      end;

    // Small Food Processor
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGISmallFoodProcConstr,
      cost_FoodProcessingPlantSmall,
      [40, 40, 20],
      40,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFoodProcessorBlock.Create(
      tidBlock_PGISmallFoodProc,
      [input_FoodProcessingPlantSmall_ExecutivesWorkForce, input_FoodProcessingPlantSmall_ProfessionalWorkForce, input_FoodProcessingPlantSmall_Workers],
      input_FoodProcessingPlantSmall_FreshFood,
      input_FoodProcessingPlantSmall_Chemicals,
      input_FoodProcessingPlantSmall_LegalServices,
      input_FoodProcessingPlantSmall_ComputerServices,
      output_FoodProcessingPlantSmall_ProcessedFood,
      budget_FoodProcessingPlantSmall,
      TFoodProcessorBlock ) do
      begin
        Beauty := -50;
      //BeautyStrength := 1;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_PGISmallFoodProc, 'Food Processor', vidFacility_PGISmallFoodProc, TFacility ) do
      begin
        xSize := 3;
        ySize := 3;
        Level := 30;
        FacId := FID_FoodProc;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGISmallFoodProcConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGISmallFoodProc])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIIndustrialFacilities;
        TechnologyKind := tidLicence_FoodProc;
        MinistryId := nidMinistry_Agriculture;
        Register( tidClassFamily_Facilities );
      end;

    // Food Processor
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIFoodProcConstr,
      cost_FoodProcessingPlant,
      [40, 40, 20],
      40,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFoodProcessorBlock.Create(
      tidBlock_PGIFoodProc,
      [input_FoodProcessingPlant_ExecutivesWorkForce, input_FoodProcessingPlant_ProfessionalWorkForce, input_FoodProcessingPlant_Workers],
      input_FoodProcessingPlant_FreshFood,
      input_FoodProcessingPlant_Chemicals,
      input_FoodProcessingPlant_LegalServices,
      input_FoodProcessingPlant_ComputerServices,
      output_FoodProcessingPlant_ProcessedFood,
      budget_FoodProcessingPlant,
      TFoodProcessorBlock ) do
      begin
        Beauty := -50;
      //BeautyStrength := 1;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_PGIFoodProc, 'Large Food Processor', vidFacility_PGIFoodProc, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 30;
        FacId := FID_FoodProc;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIFoodProcConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIFoodProc])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIIndustrialFacilities;
        TechnologyKind := tidInventionKind_LargeFoodProc;
        MinistryId := nidMinistry_Agriculture;
        Register( tidClassFamily_Facilities );
      end;

    // Small Metal industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGISmallMetalConstr,
      cost_MetalurgicSmall,
      [40, 50, 10],
      55,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaMetalIndustryBlock.Create(
      tidBlock_PGISmallMetal,
      [0{input_MetalurgicSmall_ExecutivesWorkForce}, input_MetalurgicSmall_ProfessionalWorkForce, input_MetalurgicSmall_Workers],
      input_MetalurgicSmall_Ore,
      input_MetalurgicSmall_Chemicals,
      input_MetalurgicSmall_LegalServices,
      input_MetalurgicSmall_ComputerServices,
      output_MetalurgicSmall_Metal,
      budget_MetalurgicSmall,
      TMetalIndustryBlock ) do
      begin
        Beauty := -100;
      //BeautyStrength := 1;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_PGISmallMetal, 'Metallurgic Industry', vidFacility_PGISmallMetal, TFacility ) do
      begin
        xSize := 4;
        ySize := 4;
        Level := 30;
        FacId := FID_Metal;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGISmallMetalConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGISmallMetal])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIIndustrialFacilities;
        TechnologyKind := tidLicence_Metallurgy;
        MinistryId := nidMinistry_HeavyIndustry;
        Register( tidClassFamily_Facilities );
      end;

    // Metal industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIMetalConstr,
      cost_Metalurgic,
      [40, 50, 10],
      55,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaMetalIndustryBlock.Create(
      tidBlock_PGIMetal,
      [input_Metalurgic_ExecutivesWorkForce, input_Metalurgic_ProfessionalWorkForce, input_Metalurgic_Workers],
      input_Metalurgic_Ore,
      input_Metalurgic_Chemicals,
      input_Metalurgic_LegalServices,
      input_Metalurgic_ComputerServices,
      output_Metalurgic_Metal,
      budget_Metalurgic,
      TMetalIndustryBlock ) do
      begin
        Beauty := -100;
      //BeautyStrength := 1;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_PGIMetal, 'Large Metallurgic Industry', vidFacility_PGIMetal, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 30;
        FacId := FID_Metal;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIMetalConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIMetal])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIIndustrialFacilities;
        TechnologyKind := tidInventionKind_LargeMetallurgy;
        MinistryId := nidMinistry_HeavyIndustry;
        Register( tidClassFamily_Facilities );
      end;

    // Small Textile industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGISmallTextileConstr,
      cost_TextileSmall,
      [45, 40, 15],
      40,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaTextilIndustryBlock.Create(
      tidBlock_PGISmallTextile,
      [input_TextileSmall_ExecutivesWorkForce, input_TextileSmall_ProfessionalWorkForce, input_TextileSmall_Workers],
      input_TextileSmall_OrganicMaterials,
      input_TextileSmall_Chemicals,
      input_TextileSmall_ComputerServices,
      input_TextileSmall_LegalServices,
      output_TextileSmall_FabricsandThreads,
      budget_TextileSmall,
      TTextilIndustryBlock ) do
      begin
        Beauty := -20;
      //BeautyStrength := 0.5;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_PGISmallTextile, 'Textile Industry', vidFacility_PGISmallTextile, TFacility ) do
      begin
        xSize := 4;
        ySize := 4;
        Level := 20;
        FacId := FID_Textile;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGISmallTextileConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGISmallTextile])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIIndustrialFacilities;
        TechnologyKind := tidLicence_Textile;
        MinistryId := nidMinistry_LightIndustry;
        Register( tidClassFamily_Facilities );
      end;

    // Textile industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGITextileConstr,
      cost_Textile,
      [45, 40, 15],
      40,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaTextilIndustryBlock.Create(
      tidBlock_PGITextile,
      [input_Textile_ExecutivesWorkForce, input_Textile_ProfessionalWorkForce, input_Textile_Workers],
      input_Textile_OrganicMaterials,
      input_Textile_Chemicals,
      input_Textile_ComputerServices,
      input_Textile_LegalServices,
      output_Textile_FabricsandThreads,
      budget_Textile,
      TTextilIndustryBlock ) do
      begin
        Beauty := -20;
      //BeautyStrength := 0.5;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_PGITextile, 'Large Textile Industry', vidFacility_PGITextile, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 20;
        FacId := FID_Textile;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGITextileConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGITextile])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIIndustrialFacilities;
        TechnologyKind := tidInventionKind_LargeTextile;
        MinistryId := nidMinistry_LightIndustry;
        Register( tidClassFamily_Facilities );
      end;

    // Small Clothings industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGISmallClothingsConstr,
      cost_ClothingSmall,
      [50, 40, 10],
      30,
      TBlockUnderConstruction) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaClothingsIndustryBlock.Create(
      tidBlock_PGISmallClothings,
      [input_ClothingSmall_ExecutivesWorkForce, input_ClothingSmall_ProfessionalWorkForce, input_ClothingSmall_Workers],
      input_ClothingSmall_FabricsandThreads,
      input_ClothingSmall_OrganicMaterials,
      input_ClothingSmall_Chemicals,
      input_ClothingSmall_ComputerServices,
      input_ClothingSmall_LegalServices,
      output_ClothingSmall_Cheapclothing,
      budget_ClothingSmall,
      TClothingsIndustryBlock ) do
      begin
        Beauty := -20;
      //BeautyStrength := 0.5;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_PGISmallClothings, 'Clothes Industry', vidFacility_PGISmallClothings, TFacility ) do
      begin
        xSize := 3;
        ySize := 3;
        Level := 40;
        FacId := FID_Clothes;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGISmallClothingsConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGISmallClothings])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIIndustrialFacilities;
        TechnologyKind := tidLicence_Clothing;
        MinistryId := nidMinistry_LightIndustry;
        Register( tidClassFamily_Facilities );
      end;

    // Clothings industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIClothingsConstr,
      cost_Clothing,
      [50, 40, 10],
      30,
      TBlockUnderConstruction) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaClothingsIndustryBlock.Create(
      tidBlock_PGIClothings,
      [input_Clothing_ExecutivesWorkForce, input_Clothing_ProfessionalWorkForce, input_Clothing_Workers],
      input_Clothing_FabricsandThreads,
      input_Clothing_OrganicMaterials,
      input_Clothing_Chemicals,
      input_Clothing_ComputerServices,
      input_Clothing_LegalServices,
      output_Clothing_Cheapclothing,
      budget_Clothing,
      TClothingsIndustryBlock ) do
      begin
        Beauty := -20;
      //BeautyStrength := 0.5;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_PGIClothings, 'Large Clothes Industry', vidFacility_PGIClothings, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 40;
        FacId := FID_Clothes;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIClothingsConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIClothings])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIIndustrialFacilities;
        TechnologyKind := tidInventionKind_LargeClothes;
        MinistryId := nidMinistry_LightIndustry;
        Register( tidClassFamily_Facilities );
      end;

    // Construction industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIConstructionConstr,
      cost_Construction,
      [45, 40, 15],
      40,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaConstructionIndustryBlock.Create(
      tidBlock_PGIConstruction,
      [input_Construction_ExecutivesWorkForce, input_Construction_ProfessionalWorkForce, input_Construction_Workers],
      input_Construction_Ore,
      input_Construction_Metal,
      input_Construction_Chemicals,
      input_Construction_Timber,
      input_Construction_LegalServices,
      input_Construction_ComputerServices,
      output_Construction_Construction,
      budget_Construction,
      TConstructionIndustryBlock ) do
      begin
        Beauty := -50;
      //BeautyStrength := 0.5;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_PGIConstruction, 'Construction Industry', vidFacility_PGIConstruction, TFacility ) do
      begin
        xSize := 4;
        ySize := 4;
        Level := 20;
        FacId := FID_Construction;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIConstructionConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIConstruction])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIIndustrialFacilities;
        TechnologyKind := tidLicence_Construction;
        MinistryId := nidMinistry_HeavyIndustry;
        Register( tidClassFamily_Facilities );
      end;

    // Small Electronic industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGISmallElectronicConstr,
      cost_ElectronicIndustrySmall,
      [40, 40, 20],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaElectronicIndustryBlock.Create(
      tidBlock_PGISmallElectronic,
      [input_ElectronicIndustrySmall_ExecutivesWorkForce, input_ElectronicIndustrySmall_ProfessionalWorkForce, input_ElectronicIndustrySmall_Workers],
      input_ElectronicIndustrySmall_Metal/2,
      input_ElectronicIndustrySmall_Metal/2,
      input_ElectronicIndustrySmall_Chemicals,
      input_ElectronicIndustrySmall_ComputerServices,
      input_ElectronicIndustrySmall_LegalServices,
      output_ElectronicIndustrySmall_Electroniccomponents,
      budget_ElectronicIndustrySmall,
      TElectronicIndustryBlock ) do
      begin
        Beauty := -40;
      //BeautyStrength := 0.5;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_PGISmallElectronic, 'Electronic Industry', vidFacility_PGISmallElectronic, TFacility ) do
      begin
        xSize := 3;
        ySize := 3;
        Level := 20;
        FacId := FID_ElectComp;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires Electronic materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGISmallElectronicConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGISmallElectronic])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIIndustrialFacilities;
        TechnologyKind := tidLicence_Electronics;
        MinistryId := nidMinistry_LightIndustry;
        Register( tidClassFamily_Facilities );
      end;

    // Electronic industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIElectronicConstr,
      cost_ElectronicIndustry,
      [40, 40, 20],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaElectronicIndustryBlock.Create(
      tidBlock_PGIElectronic,
      [input_ElectronicIndustry_ExecutivesWorkForce, input_ElectronicIndustry_ProfessionalWorkForce, input_ElectronicIndustry_Workers],
      input_ElectronicIndustry_Metal/2,
      input_ElectronicIndustry_Metal/2,
      input_ElectronicIndustry_Chemicals,
      input_ElectronicIndustry_ComputerServices,
      input_ElectronicIndustry_LegalServices,
      output_ElectronicIndustry_Electroniccomponents,
      budget_ElectronicIndustry,
      TElectronicIndustryBlock ) do
      begin
        Beauty := -40;
      //BeautyStrength := 0.5;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_PGIElectronic, 'Large Electronic Industry', vidFacility_PGIElectronic, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 20;
        FacId := FID_ElectComp;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires Electronic materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIElectronicConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIElectronic])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIIndustrialFacilities;
        TechnologyKind := tidInventionKind_LargeElectComp;
        MinistryId := nidMinistry_LightIndustry;
        Register( tidClassFamily_Facilities );
      end;

    // Heavy industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIHeavyConstr,
      cost_HeavyIndustry,
      [45, 40, 15],
      60,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaHeavyIndustryBlock.Create(
      tidBlock_PGIHeavy,
      [input_HeavyIndustry_ExecutivesWorkForce, input_HeavyIndustry_ProfessionalWorkForce, input_HeavyIndustry_Workers],
      input_HeavyIndustry_Metal,
      input_HeavyIndustry_Chemicals,
      input_HeavyIndustry_ElectronicComponents,
      input_HeavyIndustry_ComputerServices,
      input_HeavyIndustry_LegalServices,
      output_HeavyIndustry_Machinery,
      budget_HeavyIndustry,
      THeavyIndustryBlock ) do
      begin
        Beauty := -50;
      //BeautyStrength := 0.5;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_PGIHeavy, 'Machinery Industry', vidFacility_PGIHeavy, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 20;
        FacId := FID_Heavy;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires Heavy materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIHeavyConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIHeavy])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIIndustrialFacilities;
        TechnologyKind := tidLicence_Heavy;
        MinistryId := nidMinistry_HeavyIndustry;
        Register( tidClassFamily_Facilities );
      end;

    // Car industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGICarIndustryConstr,
      cost_CarIndustry,
      [40, 45, 15],
      55,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaCarIndustryBlock.Create(
      tidBlock_PGICarIndustry,
      [input_CarIndustry_ExecutivesWorkForce, input_CarIndustry_ProfessionalWorkForce, input_CarIndustry_Workers],
      input_CarIndustry_Metal,
      input_CarIndustry_FabricsAndThreads,
      input_CarIndustry_ElectronicComponents,
      input_CarIndustry_Chemicals,
      input_CarIndustry_Plastics,
      input_CarIndustry_ComputerServices,
      input_CarIndustry_LegalServices,
      output_CarIndustry_NiceCars,
      budget_CarIndustry,
      TCarIndustryBlock ) do
      begin
        Beauty := -40;
      //BeautyStrength := 0.5;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_PGICarIndustry, 'Car Factory', vidFacility_PGICarIndustry, TFacility ) do
      begin
        xSize := 4;
        ySize := 4;
        Level := 40;
        FacId := FID_Car;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires Heavy materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGICarIndustryConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGICarIndustry])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIIndustrialFacilities;
        TechnologyKind := tidLicence_Cars;
        MinistryId := nidMinistry_HeavyIndustry;
        Register( tidClassFamily_Facilities );
      end;

    // Small HHA industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGISmallHHAIndustryConstr,
      cost_HouseHoldingAppliancesSmall,
      [45, 40, 15],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaHouseHoldingAppliancesBlock.Create(
      tidBlock_PGISmallHHAIndustry,
      [input_HouseHoldingAppliancesSmall_ExecutivesWorkForce, input_HouseHoldingAppliancesSmall_ProfessionalWorkForce, input_HouseHoldingAppliancesSmall_Workers],
      input_HouseHoldingAppliancesSmall_Metal,
      input_HouseHoldingAppliancesSmall_ElectronicComponents,
      input_HouseHoldingAppliancesSmall_Chemicals,
      input_HouseHoldingAppliancesSmall_Plastics,
      input_HouseHoldingAppliancesSmall_ComputerServices,
      input_HouseHoldingAppliancesSmall_LegalServices,
      output_HouseHoldingAppliancesSmall_HouseHoldingAppliances,
      budget_HouseHoldingAppliancesSmall,
      THouseHoldingAppliancesBlock ) do
      begin
        Beauty := -40;
      //BeautyStrength := 0.5;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_PGISmallHHAIndustry, 'Appliances Factory', vidFacility_PGISmallHHAIndustry, TFacility ) do
      begin
        xSize := 3;
        ySize := 3;
        Level := 40;
        FacId := FID_Household;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires Heavy materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGISmallHHAIndustryConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGISmallHHAIndustry])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIIndustrialFacilities;
        TechnologyKind := tidLicence_HHA;
        MinistryId := nidMinistry_LightIndustry;
        Register( tidClassFamily_Facilities );
      end;

    // HHA industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIHHAIndustryConstr,
      cost_HouseHoldingAppliances,
      [45, 40, 15],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaHouseHoldingAppliancesBlock.Create(
      tidBlock_PGIHHAIndustry,
      [input_HouseHoldingAppliances_ExecutivesWorkForce, input_HouseHoldingAppliances_ProfessionalWorkForce, input_HouseHoldingAppliances_Workers],
      input_HouseHoldingAppliances_Metal,
      input_HouseHoldingAppliances_ElectronicComponents,
      input_HouseHoldingAppliances_Chemicals,
      input_HouseHoldingAppliances_Plastics,
      input_HouseHoldingAppliances_ComputerServices,
      input_HouseHoldingAppliances_LegalServices,
      output_HouseHoldingAppliances_HouseHoldingAppliances,
      budget_HouseHoldingAppliances,
      THouseHoldingAppliancesBlock ) do
      begin
        Beauty := -40;
      //BeautyStrength := 0.5;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_PGIHHAIndustry, 'Large Appliances Factory', vidFacility_PGIHHAIndustry, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 40;
        FacId := FID_Household;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires Heavy materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIHHAIndustryConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIHHAIndustry])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIIndustrialFacilities;
        TechnologyKind := tidInventionKind_LargeHHA;
        MinistryId := nidMinistry_LightIndustry;
        Register( tidClassFamily_Facilities );
      end;

    // Business Machines
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIBusinessMachineConstr,
      cost_BusinessMachines,
      [40, 40, 20],
      40,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );                                  
      end;                                                
    with TMetaBusinessMachinesBlock.Create(
      tidBlock_PGIBusinessMachine,
      [input_BusinessMachines_ExecutivesWorkForce, input_BusinessMachines_ProfessionalWorkForce, input_BusinessMachines_Workers],
      input_BusinessMachines_Metal,
      input_BusinessMachines_Chemicals,
      input_BusinessMachines_Plastics,
      input_BusinessMachines_ElectronicComponents,
      input_BusinessMachines_ComputerServices,
      input_BusinessMachines_LegalServices,
      output_BusinessMachines_BusinessMachines,              
      budget_BusinessMachines,
      TBusinessMachinesBlock ) do
      begin
        Beauty := -40;
      //BeautyStrength := 0.5;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_PGIBusinessMachine, 'Business Machines Industry', vidFacility_PGIBusinessMachine, TFacility ) do
      begin
        xSize := 4;
        ySize := 4;
        Level := 40;
        FacId := FID_BusMach;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires Heavy materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIBusinessMachineConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIBusinessMachine])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIIndustrialFacilities;
        TechnologyKind := tidLicence_BMIndustry;
        MinistryId := nidMinistry_LightIndustry;
        Register( tidClassFamily_Facilities );
      end;

  end;

procedure RegisterStores;
  begin
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIFoodStoreConstr,
      70000,
      [90, 0, 10],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFoodStoreBlock.Create(
      tidBlock_PGIFoodStore,
      [0, 0, 10],
      25, // people buying per hour
      24, // people buying per hour
      FairPrice,
      [0.9, 0.9, 0.9],
      110,
      TFoodStoreBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_PGIFoodStore, 'Food Store', vidFacility_PGIFoodStore, TFacility ) do
      begin
        xSize := 1;
        ySize := 1;
        Level := 100;
        FacId := FID_FoodStore;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIFoodStoreConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Food Store is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIFoodStore])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIServiceFacilities;
        TechnologyKind := tidInventionKind_ServiceFacilities;
        DepOnTech := false; // make available even without HQs
        MinistryId := nidMinistry_Agriculture;
        Register( tidClassFamily_Facilities );
      end;

    {// Clothes store
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIClothesStoreConstr,
      50000,
      [90, 0, 10],
      15,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaClothesShopBlock.Create(
      tidBlock_PGIClothesStore,
      [2, 5, 20],
      10,
      FairPrice,
      [0.6, 0.6, 0.7],
      TClothesShopBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_PGIClothesStore, 'Clothes Store', vidFacility_PGIClothesStore, TFacility ) do
      begin
        xSize := 1;
        ySize := 1;
        Level := 1000;
        Desc := '';
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIClothesStoreConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Shop is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIClothesStore])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIServiceFacilities;
        Technology := nidInvention_PGIDistributedDirection;
        Register( tidClassFamily_Facilities );
      end;

    // HHA store
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIHHAStoreConstr,
      50000,
      [90, 0, 10],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaHHAStoreBlock.Create(
      tidBlock_PGIHHAStore,
      [2, 5, 20],
      5,
      FairPrice,
      [0.8, 0.8, 0.9],
      THHAStoreBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_PGIHHAStore, 'House Holding Appliances Store', vidFacility_PGIHHAStore, TFacility ) do
      begin
        xSize := 1;
        ySize := 1;
        Level := 1000;
        Desc := '';
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabHHAStoreConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Shop is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabHHAStore])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabIllusions;
        Technology := nidInvention_MoabBasicIllusions;
        Register( tidClassFamily_Facilities );
      end;
}
    // Car Store
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGICarStoreConstr,
      500000,
      [90, 0, 10],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaCarShopBlock.Create(
      tidBlock_PGICarStore,
      [0, 1, 10],
      1/(24*7),
      2/(24*7),
      1,
      FairPrice,
      [0.8, 0.9, 1],
      140,
      TCarShopBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_PGICarStore, 'Car Store', vidFacility_PGICarStore, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 100;
        FacId := FID_CarStore;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGICarStoreConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Shop is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGICarStore])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIServiceFacilities;
        TechnologyKind := tidInventionKind_ServiceFacilities;
        DepOnTech := false; // make available even without HQs
        MinistryId := nidMinistry_HeavyIndustry;
        Register( tidClassFamily_Facilities );
      end;

    // Drug Store
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIDrugStoreConstr,
      70000,
      [90, 0, 10],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaDrugStoreBlock.Create(
      tidBlock_PGIDrugStore,
      [0, 5, 3],
      5,
      FairPrice,
      [0.8, 0.9, 1],
      140,
      TDrugStoreBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_PGIDrugStore, 'Drug Store', vidFacility_PGIDrugStore, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 100;
        FacId := FID_DrugStore;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIDrugStoreConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Shop is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIDrugStore])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIServiceFacilities;
        TechnologyKind := tidInventionKind_ServiceFacilities;
        DepOnTech := false; // make available even without HQs
        MinistryId := nidMinistry_LightIndustry;
        Register( tidClassFamily_Facilities );
      end;

    // Supermarket A
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGISupermarketConstrA,
      2000000,
      [80, 0, 20],
      45,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaSuperMarketBlock.Create(
      tidBlock_PGISupermarketA,
      [0, 1, 50],
      2/(24*7),
      1/(24*7),
      30,
      32,
      18,
      10,
      FairPrice,
      [0.9, 0.9, 1],
      200,
      TSuperMarketBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_PGISupermarketA, 'Mall', vidFacility_PGISupermarketA, TFacility ) do
      begin
        xSize := 4;
        ySize := 4;
        Level := 60;
        FacId := FID_Supermarket;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGISupermarketConstrA])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Supermarket is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGISupermarketA])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIServiceFacilities;
        TechnologyKind := tidInventionKind_Supermarkets;
        MinistryId := nidMinistry_Commerce;
        Register( tidClassFamily_Facilities );
      end;

    // Supermarket B
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGISupermarketConstrB,
      1500000,
      [80, 0, 20],
      40,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaSuperMarketBlock.Create(
      tidBlock_PGISupermarketB,
      [0, 1, 30],
      2/(3*24*7),
      1/(3*24*7),
      18,
      20,
      10,
      5,
      FairPrice,
      [0.9, 0.9, 0.9],
      200,
      TSuperMarketBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_PGISupermarketB, 'Supermarket', vidFacility_PGISupermarketB, TFacility ) do
      begin
        xSize := 3;
        ySize := 3;
        Level := 100;
        FacId := FID_Supermarket;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGISupermarketConstrB])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Supermarket is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGISupermarketB])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIServiceFacilities;
        TechnologyKind := tidInventionKind_Supermarkets;
        MinistryId := nidMinistry_Commerce;
        Register( tidClassFamily_Facilities );
      end;

    // Supermarket C
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGISupermarketConstrC,
      1000000,
      [80, 0, 20],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaSuperMarketBlock.Create(
      tidBlock_PGISupermarketC,
      [0, 1, 10],
      3/(5*24*7),
      2/(5*24*7),
      10,
      10,
      8,
      6,
      FairPrice,
      [0.7, 0.8, 0.9],
      200,
      TSuperMarketBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_PGISupermarketC, 'Mart', vidFacility_PGISupermarketC, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 100;
        FacId := FID_Supermarket;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGISupermarketConstrC])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Supermarket is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGISupermarketC])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIServiceFacilities;
        TechnologyKind := tidInventionKind_ServiceFacilities;
        DepOnTech := false; // make available even without HQs
        MinistryId := nidMinistry_Commerce;
        Register( tidClassFamily_Facilities );
      end;

    // Bar
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIBarConstr,
      150000,
      [90, 0, 10],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaBarBlock.Create(
      tidBlock_PGIBar,
      [0, 0, 15],
      20,
      FairPrice,
      [0.8, 0.8, 0.8],
      100,
      TBarBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_PGIBar, 'Bar', vidFacility_PGIBar, TFacility ) do
      begin
        xSize := 1;
        ySize := 1;
        Level := 100;
        FacId := FID_Bar;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIBarConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Bar is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIBar])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIServiceFacilities;
        TechnologyKind := tidInventionKind_Bars;
        Register( tidClassFamily_Facilities );
      end;

    // Restaurant
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIRestaurantConstr,
      400000,
      [90, 0, 10],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaRestaurantBlock.Create(
      tidBlock_PGIRestaurant,
      [0, 1, 25],
      25,
      FairPrice,
      [0.9, 0.8, 0.7],
      140,
      TRestaurantBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_PGIRestaurant, 'Restaurant', vidFacility_PGIRestaurant, TFacility ) do
      begin
        xSize := 1;
        ySize := 1;
        Level := 1000;
        FacId := FID_Restaurant;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIRestaurantConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Restaurant is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIRestaurant])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIServiceFacilities;
        TechnologyKind := tidInventionKind_Restaurants;
        Register( tidClassFamily_Facilities );
      end;

    // MovieA
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIMovieAConstr,
      2000000,
      [90, 0, 10],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaMovieBlock.Create(
      tidBlock_PGIMovieA,
      [0, 0, 8],
      80,
      FairPrice,
      [0.9, 0.9, 0.9],
      100,
      TMovieBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_PGIMovieA, 'Delmar''s Movie Theater', vidFacility_PGIMovieA, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 100;
        FacId := FID_Movie;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIMovieAConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Movie is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIMovieA])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIServiceFacilities;
        TechnologyKind := tidInventionKind_MovieTheaters;
        Register( tidClassFamily_Facilities );
      end;

    // MovieB
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIMovieBConstr,
      2500000,
      [90, 0, 10],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaMovieBlock.Create(
      tidBlock_PGIMovieB,
      [0, 0, 8],
      90,
      FairPrice,
      [1, 1, 1],
      100,
      TMovieBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_PGIMovieB, 'Baltimore''s Movie Theater', vidFacility_PGIMovieB, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 100;
        FacId := FID_Movie;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIMovieBConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Movie is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIMovieB])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIServiceFacilities;
        TechnologyKind := tidInventionKind_MovieTheaters;
        Register( tidClassFamily_Facilities );
      end;

  end;

procedure RegisterHeadquarters;
  begin
    // General
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIGeneralHeadquarterConstr,
      4000000,
      [60, 0, 40],
      10,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaMainHeadquarter.Create(
      tidBlock_PGIGeneralHeadquarter,
      [130, 100, 50],
      tidInventionKind_Direction,
      TMainHeadquarter ) do
      begin
        Beauty := 100;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_PGIGeneralHeadquarter, 'Company Headquarters', vidFacility_PGIGeneralHeadquarter, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 5000;
        FacId := FID_MainHeadquarter;
        Options := Options - [mfcGenerateName, mfcInTown];
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIGeneralHeadquarterConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The facility is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidFacility_PGIGeneralHeadquarter])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIDistributedDirection;
        Register( tidClassFamily_Facilities );
      end;

    // General Standalone
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIGeneralHeadquarterConstr + 'STA',
      4000000,
      [60, 0, 40],
      10,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaMainHeadquarter.Create(
      tidBlock_PGIGeneralHeadquarter + 'STA',
      [15, 7, 1],
      tidInventionKind_Direction,
      TMainHeadquarter ) do
      begin
        Beauty     := 100;
        Standalone := true;
        VisualStages := 5;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_PGIGeneralHeadquarter + 'STA', 'Company Headquarters', 601, TFacility ) do
      begin
        xSize := 3;
        ySize := 3;
        Level := 5000;
        FacId := FID_MainHeadquarter;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIGeneralHeadquarterConstr + 'STA'])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The facility is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidFacility_PGIGeneralHeadquarter + 'STA'])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIDistributedDirection;
        UniquenessMask := $00000001;
        Register( tidClassFamily_Facilities );
      end;

    // Industries
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIIndHeadquarterConstr,
      1500000,
      [60, 0, 40],
      10,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaHeadquarterBlock.Create(
      tidBlock_PGIIndHeadquarter,
      [100, 50, 20],
      tidInventionKind_IndustrialFacilities,
      THeadquarterBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_PGIIndHeadquarter, 'Industry Headquarters', vidFacility_PGIIndHeadquarter, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 5000;
        Desc  := '';
        FacId := FID_IndHeadquarter;
        Options := Options - [mfcGenerateName, mfcInTown];
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIIndHeadquarterConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The facility is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidFacility_PGIIndHeadquarter])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIDistributedDirection;
        TechnologyKind := tidInventionKind_Direction;
        Register( tidClassFamily_Facilities );
      end;

    // Markets
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIServiceHeadquarterConstr,
      1500000,
      [60, 0, 40],
      10,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaHeadquarterBlock.Create(
      tidBlock_PGIServiceHeadquarter,
      [100, 50, 20],
      tidInventionKind_ServiceFacilities,
      THeadquarterBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_PGIServiceHeadquarter, 'Commerce Headquarters', vidFacility_PGIServiceHeadquarter, TFacility ) do
      begin
        Desc  := '';
        XSize := 2;
        YSize := 2;
        Level := 5000;
        FacId := FID_CommHeadquarter;
        Options := Options - [mfcGenerateName, mfcInTown];
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIServiceHeadquarterConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The facility is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidFacility_PGIServiceHeadquarter])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIDistributedDirection;
        TechnologyKind := tidInventionKind_Direction;
        Register( tidClassFamily_Facilities );
      end;

    // Residential & Business Headquarter
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIResHeadquarterConstr,
      1500000,
      [60, 0, 40],
      10,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaHeadquarterBlock.Create(
      tidBlock_PGIResHeadquarter,
      [100, 50, 20],
      tidInventionKind_OfficeAndResidentials,
      THeadquarterBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_PGIResHeadquarter, 'Real Estates Headquarters', vidFacility_PGIResHeadquarter, TFacility ) do
      begin
        Desc  := '';
        XSize := 2;
        YSize := 2;
        Level := 5000;
        FacId := FID_OffcHeadquarter;
        Options := Options - [mfcGenerateName, mfcInTown];
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIResHeadquarterConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The facility is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidFacility_PGIResHeadquarter])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIDistributedDirection;
        TechnologyKind := tidInventionKind_Direction;
        Register( tidClassFamily_Facilities );
      end;

    // Public Facilities
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIPubHeadquarterConstr,
      1500000,
      [100, 50, 20],
      10,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaPublicAffairsHeadquarter.Create(
      tidBlock_PGIPubHeadquarter,
      [33, 7, 12],
      tidInventionKind_PublicFacilities,
      maxHQAdv,
      TPublicAffairsHeadquarter ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_PGIPubHeadquarter, 'Civic Affairs Headquarters', vidFacility_PGIPubHeadquarter, TFacility ) do
      begin
        Desc  := '';
        XSize := 2;
        YSize := 2;
        Level := 5000;
        FacId := FID_PubHeadquarter;
        Options := Options - [mfcGenerateName, mfcInTown];
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'RequiPub construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIPubHeadquarterConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The facility is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidFacility_PGIPubHeadquarter])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIDistributedDirection;
        TechnologyKind := tidInventionKind_Direction;
        Register( tidClassFamily_Facilities );
      end;

  end;

procedure RegisterSpecialFacilities;
  begin
    // Computing Industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGIComputingIndustryConstr,
      cost_ComputingIndustry,
      [70, 0, 30],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaSoftwareBlock.Create(
      tidBlock_PGIComputingIndustry,
      [input_ComputingIndustry_ExecutivesWorkForce, input_ComputingIndustry_ProfessionalWorkForce, input_ComputingIndustry_Workers],
      input_ComputingIndustry_LegalServices,
      output_ComputingIndustry_ComputerServices,
      budget_ComputingIndustry,
      TSoftwareBlock ) do
      begin
        Beauty := 100;
      //BeautyStrength := 0.1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGIComputingIndustry, 'Software Firm', vidFacility_PGIComputingIndustry, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 40;
        FacId := FID_SoftwareFirm;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIComputingIndustryConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Computing Industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGIComputingIndustry])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIBusinessFacilities;
        TechnologyKind := tidInventionKind_Software;
        Register( tidClassFamily_Facilities );
      end;

    // Legal Services
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGILegalServicesConstr,
      cost_LegalServices,
      [80, 0, 20],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaLegalServiceBlock.Create(
      tidBlock_PGILegalServices,
      [input_LegalServices_ExecutivesWorkForce, input_LegalServices_ProfessionalWorkForce, input_LegalServices_Workers],
      input_LegalServices_ComputerServices,
      output_LegalServices_LegalServices,
      budget_LegalServices,
      TSoftwareBlock ) do
      begin
        Beauty := 100;
      //BeautyStrength := 0.1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGILegalServices, 'Lawyers Firm', vidFacility_PGILegalServices, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 100;
        FacId := FID_LawyerFirm;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGILegalServicesConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Firm is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGILegalServices])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGIBusinessFacilities;
        TechnologyKind := tidInventionKind_LegalServices;
        Register( tidClassFamily_Facilities );
      end;

    // TV Station
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGITVStationConstr,
      30000000,
      [70, 0, 30],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaBroadcaster.Create(
      tidBlock_PGITVStation,                           
      tidBroadcast_TV,
      200,
      [5, 50, 35],                                       
      accIdx_TV_Supplies,
      accIdx_TV_Products,
      accIdx_TV_Salaries,
      TBroadcaster ) do
      begin
        Beauty := 200;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGITVStation, 'TV', vidFacility_PGITVStation, TFacility ) do
      begin
        xSize := 3;
        ySize := 3;
        Level := 60;
        FacId := FID_TVStation;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGITVStationConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Station is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGITVStation])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGISpecial;
        TechnologyKind := tidInventionKind_Television;
        Register( tidClassFamily_Facilities );
      end;

    // TV Antenna
    with TMetaBlockUnderConstruction.Create(
      tidBlock_PGITVAntennaConstr,
      500000,
      [90, 0, 10],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaAntenna.Create(
      tidBlock_PGITVAntenna,
      [0, 0, 4],
      accIdx_TV_Supplies,
      accIdx_TV_Products,
      accIdx_TV_Salaries,
      TAntenna ) do
      begin
        BroadcastId := tidBroadcast_TV;
        Beauty      := -100;
        Power       := 50;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_PGITVAntenna, 'TV Antenna', vidFacility_PGITVAntenna, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 50;
        FacId := FID_TVAntena;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGITVAntennaConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Antena is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_PGITVAntenna])));
        ClusterName := tidClusterName_PGI;
        FacilityKind := tidFacilityKind_PGISpecial;
        TechnologyKind := tidInventionKind_Television;
        Register( tidClassFamily_Facilities );
      end;
  end;

procedure RegisterPackFacilities;
  begin
    RegisterHeadquarters;
    RegisterIndustries;
    RegisterStores;
    RegisterResidentials;
    RegisterOffices;
    RegisterPublicFacilities;
    RegisterSpecialFacilities;
  end;

function ModelExtensionId : string; export;
  begin
    result := 'PGIPack1';
  end;
    
function GetDependances : string; export;
  begin
    result := 'GeneralPack1';
  end;
    
procedure RegisterModelExtension; export;
  begin
    SimMLS.LoadMLS;
    InitVisualClasses;
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
    


