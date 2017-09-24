{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W+,X+,Y-,Z1}
{$MINSTACKSIZE $00004000}
{$MAXSTACKSIZE $00100000}
{$IMAGEBASE $00400000}
{$APPTYPE GUI}

{$DEFINE USELogs}

library MarikoPack1;

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
  FluidConsts in 'FluidConsts.pas',
  PublicFacility in '..\..\Kernel\PublicFacility.pas',
  ServiceBlock in '..\..\StdBlocks\ServiceBlock.pas',
  Restaurant in '..\..\StdBlocks\Restaurant.pas',
  Computing in '..\..\StdBlocks\Computing.pas',
  LegalServices in '..\..\StdBlocks\LegalServices.pas',
  Standards in '..\Standards.pas',
  CacheCommon in '..\..\Cache\CacheCommon.pas',
  MarikoConst in 'MarikoConst.pas',
  Land in '..\..\Land\Land.pas',
  OfficeBlock in '..\..\StdBlocks\OfficeBlock.pas',
  NewsServerInterfaces in '..\..\News Server\NewsServerInterfaces.pas',
  Environmental in '..\..\StdBlocks\Environmental.pas',
  TransportInterfaces in '..\..\Transport\TransportInterfaces.pas',
  Transport in '..\..\Transport\Transport.pas',
  MatrixLayer in '..\..\Transport\MatrixLayer.pas',
  VisualClassManager in '..\..\Class Packer\VisualClassManager.pas',
  Broadcast in '..\..\StdBlocks\Broadcast.pas',
  StdBroadcast in '..\..\StdBlocks\StdBroadcast.pas',
  StdAccounts in '..\..\StdBlocks\StdAccounts.pas',
  FacIds in '..\FacIds.pas',
  TownPolitics in '..\..\Kernel\TownPolitics.pas',
  Logs in '..\..\Logs\Logs.pas',
  ModelServerCache in '..\..\Cache\ModelServerCache.pas',
  Inventions in '..\..\Inventions\Inventions.pas',
  PlasticInd in '..\..\StdBlocks\PlasticInd.pas',
  CommonFacs in '..\CommonFacs.pas',
  RankProtocol in '..\..\Protocol\RankProtocol.pas',
  GenIdd in '..\..\Utils\Serial\GenIdd.pas',
  ChemMine in '..\..\StdBlocks\ChemMine.pas',
  CoalMine in '..\..\StdBlocks\CoalMine.pas',
  SiliconMine in '..\..\StdBlocks\SiliconMine.pas',
  StoneMine in '..\..\StdBlocks\StoneMine.pas',
  SimMLS in '..\..\Kernel\SimMLS.pas',
  ComputerStore in '..\..\StdBlocks\ComputerStore.pas',
  CDPlant in '..\..\StdBlocks\CDPlant.pas',
  CDStore in '..\..\StdBlocks\CDStore.pas';

procedure RegisterFacilityKinds;
  begin
    with TFacilityKind.Create( tidFacilityKind_MarikoDistributedDirection ) do
      begin
        Name        := 'Headquarters';
        SuperType   := tidSuperFacKind_Headquarter;
        ClusterName := tidClusterName_Mariko;
        Role        := rolBuyer;
        Register( tidClassFamily_FacilityKinds );
      end;
    with TFacilityKind.Create( tidFacilityKind_MarikoFarms ) do
      begin
        Name        := 'Farms';
        SuperType   := tidSuperFacKind_Farm;
        ClusterName := tidClusterName_Mariko;
        Technology  := tidInventionKind_IndustrialFacilities;
        Role        := rolProducer;
        Register( tidClassFamily_FacilityKinds );
      end;
    with TFacilityKind.Create( tidFacilityKind_MarikoIndustrialFacilities ) do
      begin
        Name        := 'Factories';
        SuperType   := tidSuperFacKind_Industry;
        ClusterName := tidClusterName_Mariko;
        Technology  := tidInventionKind_IndustrialFacilities;
        Role        := rolProducer;
        Register( tidClassFamily_FacilityKinds );
      end;
    with TFacilityKind.Create( tidFacilityKind_MarikoResidentials ) do
      begin
        Name        := 'Residentials';
        SuperType   := tidSuperFacKind_Residential;
        ClusterName := tidClusterName_Mariko;
        Register( tidClassFamily_FacilityKinds );
      end;
    with TFacilityKind.Create( tidFacilityKind_MarikoServiceFacilities ) do
      begin
        Name        := 'Commerce';
        SuperType   := tidSuperFacKind_Service;
        ClusterName := tidClusterName_Mariko;
        Technology  := tidInventionKind_ServiceFacilities;
        Role        := rolBuyer;
        Register( tidClassFamily_FacilityKinds );
      end;
    with TFacilityKind.Create( tidFacilityKind_MarikoPublicFacilities ) do
      begin
        Name        := 'Public';
        SuperType   := tidSuperFacKind_Public;
        ClusterName := tidClusterName_Mariko;
        Technology  := tidInventionKind_PublicFacilities;
        Register( tidClassFamily_FacilityKinds );
      end;
    with TFacilityKind.Create( tidFacilityKind_MarikoBusinessFacilities ) do
      begin
        Name        := 'Offices';
        SuperType   := tidSuperFacKind_Business;
        ClusterName := tidClusterName_Mariko;
        Technology  := tidInventionKind_OfficeAndResidentials;
        Register( tidClassFamily_FacilityKinds );
      end;
    with TFacilityKind.Create( tidFacilityKind_MarikoSpecial ) do
      begin
        Name        := 'Special';
        SuperType   := tidSuperFacKind_Special;
        ClusterName := tidClusterName_Mariko;
        Role        := rolProducer;
        Register( tidClassFamily_FacilityKinds );
      end;
  end;

type
  TMarikoCluster =
    class( TCluster )
      public
        function NameNewspaper( TownName : string ) : string; override;
    end;

  function TMarikoCluster.NameNewspaper( TownName : string ) : string;
    begin
      result := TownName + ' Globe';
    end;

procedure RegisterClusterFacilities;
  begin
    TMarikoCluster.Create( tidClusterName_Mariko ).Register( tidClassFamily_Clusters );
    RegisterTownHall( tidClusterName_Mariko, tidFacility_MarikoTownHall, vidFacility_MarikoTownHall, 6, 6, 250, TPoliticalTownHall );
    RegisterTradeCenter( tidClusterName_Mariko, tidFacility_MarikoTradeCenter, vidFacility_MarikoTradeCenter, 3, 3 );
  end;

procedure RegisterOffices;
  begin
    // Office A
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MKOOfficeBuildingConstrA,
      6000000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaOfficeBlock.Create(
      tidBlock_MKOOfficeBuildingA,
      40,
      TOfficeBlock ) do
      begin
        Efficiency := 0.75;
        Beauty := 70;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MKOOfficeBuildingA, 'Yamamoto', vidFacility_MKOOfficeBuildingA, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        FacId := FID_Office;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MKOOfficeBuildingConstrA])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MKOOfficeBuildingA])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoBusinessFacilities;
        TechnologyKind := tidInventionKind_Offices;
        MinistryId := nidMinistry_Commerce;
        Register( tidClassFamily_Facilities );
      end;

    // Office B
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MKOOfficeBuildingConstrB,
      10000000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaOfficeBlock.Create(
      tidBlock_MKOOfficeBuildingB,
      70,
      TOfficeBlock ) do
      begin
        Efficiency := 0.9;
        Beauty := 80;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MKOOfficeBuildingB, 'Tanakuta', vidFacility_MKOOfficeBuildingB, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        FacId := FID_Office;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MKOOfficeBuildingConstrB])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MKOOfficeBuildingB])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoBusinessFacilities;
        TechnologyKind := tidInventionKind_Offices;
        MinistryId := nidMinistry_Commerce;
        Register( tidClassFamily_Facilities );
      end;

    // Office C
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MKOOfficeBuildingConstrC,
      30000000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaOfficeBlock.Create(
      tidBlock_MKOOfficeBuildingC,
      150,
      TOfficeBlock ) do
      begin
        Efficiency := 0.60;
        Beauty := 45;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MKOOfficeBuildingC, 'Sato Scraper', vidFacility_MKOOfficeBuildingC, TFacility ) do
      begin
        XSize := 1;
        YSize := 1;
        Level := 120;
        FacId := FID_Office;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MKOOfficeBuildingConstrC])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MKOOfficeBuildingC])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoBusinessFacilities;
        TechnologyKind := tidInventionKind_Offices;
        MinistryId := nidMinistry_Commerce;
        Register( tidClassFamily_Facilities );
      end;

    // Office D
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MKOOfficeBuildingConstrD,
      20000000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaOfficeBlock.Create(
      tidBlock_MKOOfficeBuildingD,
      120,
      TOfficeBlock ) do
      begin
        Efficiency := 0.50;
        Beauty := 45;
        //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MKOOfficeBuildingD, 'Kunawa Scraper', vidFacility_MKOOfficeBuildingD, TFacility ) do
      begin
        XSize := 1;
        YSize := 1;
        Level := 120;
        FacId := FID_Office;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MKOOfficeBuildingConstrD])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MKOOfficeBuildingD])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoBusinessFacilities;
        TechnologyKind := tidInventionKind_Offices;
        MinistryId := nidMinistry_Commerce;
        Register( tidClassFamily_Facilities );
      end;

    // Office E
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MKOOfficeBuildingConstrE,
      10000000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaOfficeBlock.Create(
      tidBlock_MKOOfficeBuildingE,
      80,
      TOfficeBlock ) do
      begin
        Efficiency := 0.75;
        Beauty := 70;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MKOOfficeBuildingE, 'Koto-ku', vidFacility_MKOOfficeBuildingE, TFacility ) do
      begin
        XSize := 3;
        YSize := 3;
        Level := 120;
        FacId := FID_Office;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MKOOfficeBuildingConstrE])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MKOOfficeBuildingE])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoBusinessFacilities;
        TechnologyKind := tidInventionKind_Offices;
        MinistryId := nidMinistry_Commerce;
        Register( tidClassFamily_Facilities );
      end;

    // Office F
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MKOOfficeBuildingConstrF,
      5000000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaOfficeBlock.Create(
      tidBlock_MKOOfficeBuildingF,
      50,
      TOfficeBlock ) do
      begin
        Efficiency := 0.75;
        Beauty := 60;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MKOOfficeBuildingF, 'Small Koto', vidFacility_MKOOfficeBuildingF, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        FacId := FID_Office;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MKOOfficeBuildingConstrF])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MKOOfficeBuildingF])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoBusinessFacilities;
        TechnologyKind := tidInventionKind_Offices;
        MinistryId := nidMinistry_Commerce;
        Register( tidClassFamily_Facilities );
      end;

  end;

procedure RegisterResidentials;
  begin
    // Low cost High Class buildings
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoLoCostHighClassConstr,
      40000,
      [100, 0, 0],
      7,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_MarikoLoCostHighClass,
      pkHigh,
      20,
      TPopulatedBlock ) do
      begin
        Beauty := -200;
        ModifyPrice := false;
        LowCost := true;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MarikoLoCostHighClass, 'Low-cost HighClass buildings', vidFacility_MarikoLoCostHighClass, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        FacId := FID_hiClassLoCost;
        Options := Options - [mfcGenerateName, mfcInTown];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoLoCostHighClassConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoLoCostHighClass])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // Low cost MiddleClass buildings
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoLoCostMiddleClassConstr,
      200000,
      [100, 0, 0],
      7,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_MarikoLoCostMiddleClass,
      pkMiddle,
      30,
      TPopulatedBlock ) do
      begin
        Beauty := -200;
        ModifyPrice := false;
        LowCost := true;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MarikoLoCostMiddleClass, 'Low-cost MiddleClass buildings', vidFacility_MarikoLoCostMiddleClass, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        FacId := FID_midClassLoCost;
        Options := Options - [mfcGenerateName, mfcInTown];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoLoCostMiddleClassConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoLoCostMiddleClass])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // Low cost LowClass buildings
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoLoCostLowClassConstr,
      100000,
      [100, 0, 0],
      10,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_MarikoLoCostLowClass,
      pkLow,
      50,
      TPopulatedBlock ) do
      begin
        Beauty := -200;
        ModifyPrice := false;
        LowCost := true;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MarikoLoCostLowClass, 'Low-cost LowClass buildings', vidFacility_MarikoLoCostLowClass, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        FacId := FID_lowClassLoCost;
        Options := Options - [mfcGenerateName, mfcInTown];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoLoCostLowClassConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoLoCostLowClass])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // HighClass Building A
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoHighClassBuildingConstrA,
      1000000,
      [100, 0, 0],
      25,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_MarikoHighClassBuildingA,
      pkHigh,
      50,
      TPopulatedBlock ) do
      begin
        Efficiency := 0.75;
        Beauty := 70;
      //BeautyStrength := 0.1;
        VisualStages := 2;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MarikoHighClassBuildingA, 'Katana House', vidFacility_MarikoHighClassBuildingA, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 120;
        FacId := FID_hiClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoHighClassBuildingConstrA])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoHighClassBuildingA])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // HighClass Building B
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoHighClassBuildingConstrB,
      1000000,
      [100, 0, 0],
      40,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_MarikoHighClassBuildingB,
      pkHigh,
      50,
      TPopulatedBlock ) do
      begin
        Efficiency := 0.75;
        Beauty := 70;
      //BeautyStrength := 0.1;
        VisualStages := 2;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MarikoHighClassBuildingB, 'Sohei House', vidFacility_MarikoHighClassBuildingB, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 120;
        FacId := FID_hiClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoHighClassBuildingConstrB])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoHighClassBuildingB])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // HighClass Building C
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoHighClassBuildingConstrC,
      1500000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_MarikoHighClassBuildingC,
      pkHigh,
      75,
      TPopulatedBlock ) do
      begin
        Efficiency := 0.75;
        Beauty := 70;
      //BeautyStrength := 0.1;
        VisualStages := 2;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MarikoHighClassBuildingC, 'Tarakawa House', vidFacility_MarikoHighClassBuildingC, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 120;
        FacId := FID_hiClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoHighClassBuildingConstrC])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoHighClassBuildingC])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // Middle Class Building A
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoMiddleClassBuildingConstrA,
      4900000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_MarikoMiddleClassbuildingA,
      pkMiddle,
      250,
      TPopulatedBlock ) do
      begin
        Efficiency := 0.75;
        Beauty := 70;
      //BeautyStrength := 0.1;
        VisualStages := 2;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MarikoMiddleClassbuildingA, 'Somsei-Taj Rooms', vidFacility_MarikoMiddleClassbuildingA, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 120;
        FacId := FID_midClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoMiddleClassbuildingConstrA])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoMiddleClassbuildingA])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // Middle Class Building B
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoMiddleClassbuildingConstrB,
      5000000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_MarikoMiddleClassbuildingB,
      pkMiddle,
      240,
      TPopulatedBlock ) do
      begin
        Efficiency := 0.8;
        Beauty := 70;
      //BeautyStrength := 0.1;
        VisualStages := 2;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MarikoMiddleClassbuildingB, 'Marakas Rooms', vidFacility_MarikoMiddleClassbuildingB, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 120;
        FacId := FID_midClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoMiddleClassbuildingConstrB])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoMiddleClassbuildingB])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // Middle Class Building C
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoMiddleClassbuildingConstrC,
      4000000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_MarikoMiddleClassBuildingC,
      pkMiddle,
      200,
      TPopulatedBlock ) do
      begin
        Efficiency := 0.8;
        Beauty := 70;
      //BeautyStrength := 0.1;
        VisualStages := 2;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MarikoMiddleClassBuildingC, 'Yohito Rooms', vidFacility_MarikoMiddleClassBuildingC, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 120;
        FacId := FID_midClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoMiddleClassbuildingConstrC])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoMiddleClassBuildingC])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // Middle Class Building D
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoMiddleClassbuildingConstrD,
      6000000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_MarikoMiddleClassBuildingD,
      pkMiddle,
      280,
      TPopulatedBlock ) do
      begin
        Efficiency := 0.8;
        Beauty := 70;
      //BeautyStrength := 0.1;
        VisualStages := 2;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MarikoMiddleClassBuildingD, 'Hirijoto-Manko Rooms', vidFacility_MarikoMiddleClassBuildingD, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 120;
        FacId := FID_midClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoMiddleClassbuildingConstrD])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoMiddleClassBuildingD])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // Middle Class Building E
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoMiddleClassbuildingConstrE,
      4000000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_MarikoMiddleClassBuildingE,
      pkMiddle,
      200,
      TPopulatedBlock ) do
      begin
        Efficiency := 0.81;
        Beauty := 75;
      //BeautyStrength := 0.1;
        VisualStages := 2;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MarikoMiddleClassBuildingE, 'Blue Rooms', vidFacility_MarikoMiddleClassBuildingE, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 120;
        FacId := FID_midClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoMiddleClassbuildingConstrE])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoMiddleClassBuildingE])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // Middle Class Building G
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoMiddleClassbuildingConstrG,
      6500000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_MarikoMiddleClassBuildingG,
      pkMiddle,
      290,
      TPopulatedBlock ) do
      begin
        Efficiency := 0.90;
        Beauty := 75;
      //BeautyStrength := 0.1;
        VisualStages := 2;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MarikoMiddleClassBuildingG, 'Round Mariko', vidFacility_MarikoMiddleClassBuildingG, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 120;
        FacId := FID_midClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoMiddleClassbuildingConstrG])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoMiddleClassBuildingG])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;


    // Low Class Building A
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoLowClassbuildingConstrA,
      5000000,
      [100, 0, 0],
      40,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_MarikoLowClassbuildingA,
      pkLow,
      450,
      TPopulatedBlock ) do
      begin
        Efficiency := 0.8;
        Beauty := 50;
      //BeautyStrength := 0.1;
        VisualStages := 2;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MarikoLowClassbuildingA, 'Katana Building', vidFacility_MarikoLowClassbuildingA, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 120;
        FacId := FID_lowClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoLowClassbuildingConstrA])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoLowClassbuildingA])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // Low Class Building B
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoLowClassbuildingConstrB,
      8000000,
      [100, 0, 0],
      40,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_MarikoLowClassbuildingB,
      pkLow,
      800,
      TPopulatedBlock ) do
      begin
        Efficiency := 0.7;
        Beauty := 50;
      //BeautyStrength := 0.1;
        VisualStages := 2;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MarikoLowClassbuildingB, 'Nikotori Building', vidFacility_MarikoLowClassbuildingB, TFacility ) do
      begin
        xSize := 3;
        ySize := 3;
        Level := 120;
        FacId := FID_lowClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoLowClassbuildingConstrB])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoLowClassbuildingB])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // Low Class Building C
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoLowClassbuildingConstrC,
      5000000,
      [100, 0, 0],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_MarikoLowClassBuildingC,
      pkLow,
      400,
      TPopulatedBlock ) do
      begin
        Efficiency := 0.7;
        Beauty := 50;
      //BeautyStrength := 0.1;
        VisualStages := 2;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MarikoLowClassBuildingC, 'Hazobito Building', vidFacility_MarikoLowClassBuildingC, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 120;
        FacId := FID_lowClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoLowClassbuildingConstrC])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoLowClassBuildingC])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // Low Class Building D
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoLowClassbuildingConstrD,
      7000000,
      [100, 0, 0],
      40,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_MarikoLowClassBuildingD,
      pkLow,
      750,
      TPopulatedBlock ) do
      begin
        Efficiency := 0.45;
        Beauty := 25;
      //BeautyStrength := 0.1;
        VisualStages := 2;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MarikoLowClassBuildingD, 'Hisogari Building', vidFacility_MarikoLowClassBuildingD, TFacility ) do
      begin
        xSize := 3;
        ySize := 3;
        Level := 120;
        FacId := FID_lowClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoLowClassbuildingConstrD])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoLowClassBuildingD])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // Low Class Building E
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoLowClassbuildingConstrE,
      5000000,
      [100, 0, 0],
      40,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_MarikoLowClassbuildingE,
      pkLow,
      450,
      TPopulatedBlock ) do
      begin
        Efficiency := 0.81;
        Beauty := 50;
      //BeautyStrength := 0.1;
        VisualStages := 2;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MarikoLowClassbuildingE, 'Taramagushi', vidFacility_MarikoLowClassbuildingE, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 120;
        FacId := FID_lowClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoLowClassbuildingConstrE])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoLowClassbuildingE])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

  end;

procedure RegisterPublicFacilities;
  begin
    // Hospital
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoHospitalConstr,
      10000000,
      [90, 5, 5],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPublicFacility.Create(
      tidBlock_MarikoHospital,
      [20, 50, 10],
      [PFInfoDef(tidPublicFac_Health, 40000)],
      TPublicFacility ) do
      begin
        MaintCost := 500;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MarikoHospital, 'Hospital', vidFacility_MarikoHospital, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        SlotCount := 0;
        FacId := FID_Hospital;
        DemoLapse := 1;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoHospitalConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoHospital])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoPublicFacilities;
        TechnologyKind := tidInventionKind_PublicFacilities;
        Register( tidClassFamily_Facilities );
      end;

    // School
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoSchoolConstr,
      5000000, //5*80000,
      [99, 0, 1],
      25,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPublicFacility.Create(
      tidBlock_MarikoSchool,
      [1, 20, 12],
      [PFInfoDef( tidPublicFac_School, 20000 )],
      TPublicFacility ) do
      begin
        MaintCost := 500;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MarikoSchool, 'School', vidFacility_MarikoSchool, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 120;
        SlotCount := 0;
        FacId := FID_School;
        DemoLapse := 1;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoSchoolConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoSchool])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoPublicFacilities;
        TechnologyKind := tidInventionKind_PublicFacilities;
        Register( tidClassFamily_Facilities );
      end;

    // Police
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoPoliceConstr,
      5000000,
      [90, 5, 5],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPublicFacility.Create(
      tidBlock_MarikoPolice,
      [0, 2, 80],
      [PFInfoDef(tidPublicFac_Police, 5000),
      PFInfoDef(tidPublicFac_Fire, 10000)],
      TPublicFacility ) do
      begin
        MaintCost := 500;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MarikoPolice, 'Police', vidFacility_MarikoPolice, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 120;
        SlotCount := 0;
        FacId := FID_Police;
        DemoLapse := 1;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoPoliceConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoPolice])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoPublicFacilities;
        TechnologyKind := tidInventionKind_PublicFacilities;
        Register( tidClassFamily_Facilities );
      end;

    {// Fire
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoFireConstr,
      1000000,
      [90, 5, 5],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPublicFacility.Create(
      tidBlock_MarikoFire,
      [0, 1, 30],
      [PFInfoDef( tidPublicFac_Fire, 5000 )],
      TPublicFacility ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MarikoFire, 'Fire', vidFacility_MarikoFire, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 120;
        SlotCount := 0;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoFireConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoFire])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoPublicFacilities;
        Technology := nidInvention_PublicFacilities;
        TechnologyKind := tidInventionKind_PublicFacilities;
        Register( tidClassFamily_Facilities );
      end;}

    // Small park
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MKOSmallParkConstr,
      1000000,
      [100, 0, 0],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaEnvironmentalBlock.Create(
      tidBlock_MKOSmallPark,
      TEnvironmentalBlock ) do
      begin
        RegisterModifier( tidEnvironment_Beauty, 300, 10 );
        RegisterModifier( tidEnvironment_Pollution, -300, 10 );
        MaintCost := 100;
        DissabledStop := true;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MKOSmallPark, 'Small Park', vidFacility_MKOSmallPark, TFacility ) do
      begin
        Reqs := 'Requires first research at Public Affairs Headquarters.';
        xSize := 3;
        ySize := 3;
        Level := 200;
        FacId := FID_Park;
        DemoLapse := 1;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MKOSmallParkConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Park Completed', 'Park is done', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MKOSmallPark])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoPublicFacilities;
        TechnologyKind := tidInventionKind_PublicFacilities;
        Register( tidClassFamily_Facilities );
      end;

    // Medium park
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MKOMediumParkConstr,
      2000000,
      [100, 0, 0],
      22,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaEnvironmentalBlock.Create(
      tidBlock_MKOMediumPark,
      TEnvironmentalBlock ) do
      begin
        RegisterModifier( tidEnvironment_Beauty, 400, 10 );
        RegisterModifier( tidEnvironment_Pollution, -400, 10 );
        MaintCost := 250;
        DissabledStop := true;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MKOMediumPark, 'Park', vidFacility_MKOMediumPark, TFacility ) do
      begin
        XSize := 5;
        YSize := 5;
        Level := 200;
        FacId := FID_Park;
        DemoLapse := 1;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MKOMediumParkConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Park Completed', 'Building is done', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MKOMediumPark])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoPublicFacilities;
        TechnologyKind := tidInventionKind_PublicFacilities;
        Register( tidClassFamily_Facilities );
      end;

    // Central park
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MKOCentralParkConstr,
      3000000,
      [100, 0, 0],
      22,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaEnvironmentalBlock.Create(
      tidBlock_MKOCentralPark,
      TEnvironmentalBlock ) do
      begin
        RegisterModifier( tidEnvironment_Beauty, 500, 10 );
        RegisterModifier( tidEnvironment_Pollution, -500, 10 );
        MaintCost := 400;
        DissabledStop := true;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MKOCentralPark, 'Big Park', vidFacility_MKOCentralPark, TFacility ) do
      begin
        XSize := 7;
        YSize := 7;
        Level := 200;
        FacId := FID_Park;
        DemoLapse := 1;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MKOCentralParkConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Park Completed', 'Building is done', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MKOCentralPark])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoPublicFacilities;
        TechnologyKind := tidInventionKind_PublicFacilities;
        Register( tidClassFamily_Facilities );
      end;

    // Statue of Liberty
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoLibertyConstr,
      costLiberty,
      [100, 0, 0],
      50,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaEnvironmentalBlock.Create(
      tidBlock_MarikoLiberty,
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
    with TMetaFacility.Create( tidFacility_MarikoLiberty, 'Statue of Liberty', vidFacility_MarikoLiberty, TFacility ) do
      begin
        XSize := 4;
        YSize := 4;
        Level := 200;
        FacId := FID_LuxuryFac;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoLibertyConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Park Completed', 'Building is done', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoLiberty])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoPublicFacilities;
        TechnologyKind := tidInventionKind_Monuments;
        Register( tidClassFamily_Facilities );
      end;

    // IFEL Tower
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoTowerConstr,
      costLiberty,
      [100, 0, 0],
      50,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaEnvironmentalBlock.Create(
      tidBlock_MarikoTower,
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
    with TMetaFacility.Create( tidFacility_MarikoTower, 'IFEL Tower', vidFacility_MarikoTower, TFacility ) do
      begin
        XSize := 4;
        YSize := 4;
        Level := 200;
        FacId := FID_LuxuryFac;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoTowerConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Park Completed', 'Building is done', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MarikoTower])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoPublicFacilities;
        TechnologyKind := tidInventionKind_Monuments;
        Register( tidClassFamily_Facilities );
      end;

    // Common Public services
    CopyCommonFacilities( tidBlock_SRVCOMMON, tidClusterName_Mariko, tidFacilityKind_MarikoPublicFacilities, tidInventionKind_PublicFacilities );
    // Common Special
    CopyCommonFacilities( tidBlock_SPECIALCOMMON, tidClusterName_Mariko, tidFacilityKind_MarikoSpecial, '' );
    // Common Warehouses
    CopyCommonFacilities(tidBlock_WHCOMMON, tidClusterName_Mariko, tidFacilityKind_MarikoIndustrialFacilities, '');
  end;

procedure RegisterIndustries;
  begin
    // Farm Small
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoSmallFarmConstr,
      cost_FarmSmall,
      [80, 20, 0],
      15,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFarmBlock.Create(
      tidBlock_MarikoSmallFarm,
      [0,//{input_FarmSmall_ExecutivesWorkForce}
      input_FarmSmall_ProfessionalWorkForce,
      input_FarmSmall_Workers],
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
    with TMetaFacility.Create( tidFacility_MarikoSmallFarm, 'Small Farm', vidFacility_MarikoSmallFarm, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 20;
        FacId := FID_Farm;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoSmallFarmConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The farm is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoSmallFarm])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoFarms;
        TechnologyKind := tidLicence_Farms;
        Register( tidClassFamily_Facilities );
      end;

    // Farm
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoFarmConstr,
      cost_Farm,
      [80, 15, 5],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFarmBlock.Create(
      tidBlock_MarikoFarm,
      [input_Farm_ExecutivesWorkForce, input_Farm_ProfessionalWorkForce, input_Farm_Workers],
      input_Farm_Chemicals,
      input_Farm_LegalServices,
      input_Farm_ComputerServices,
      output_Farm_FreshFood,
      output_Farm_OrganicMaterials,
      budget_Farm,
      TFarmBlock ) do
      begin
        Beauty := -50;
        DesignEffic := 1.1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MarikoFarm, 'Farm', vidFacility_MarikoFarm, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 20;
        FacId := FID_Farm;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoFarmConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Farm is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoFarm])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoFarms;
        TechnologyKind := tidInventionKind_LargeFarms;
        Register( tidClassFamily_Facilities );
      end;

    // Small Chemical plant
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoSmallChemicalConstr,
      cost_ChemicalPlantSmall,
      [50, 40, 10],
      50,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaChemicalBlock.Create(
      tidBlock_MarikoSmallChemical,
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
    with TMetaFacility.Create( tidFacility_MarikoSmallChemical, 'Small Chemical Plant', vidFacility_MarikoSmallChemical, TFacility ) do
      begin
        xSize := 4;
        ySize := 4;
        Level := 15;
        FacId := FID_Chemical;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoSmallChemicalConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoSmallChemical])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoIndustrialFacilities;
        TechnologyKind := tidLicence_Chemical;
        Register( tidClassFamily_Facilities );
      end;

    // Chemical plant
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoChemicalConstr,
      cost_ChemicalPlant,
      [50, 40, 10],
      50,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaChemicalBlock.Create(
      tidBlock_MarikoChemical,
      [input_ChemicalPlant_ExecutivesWorkForce, input_ChemicalPlant_ProfessionalWorkForce, input_ChemicalPlant_Workers],
      input_ChemicalPlant_LegalServices,
      input_ChemicalPlant_ComputerServices,
      input_ChemicalPlant_Ore,
      output_ChemicalPlant_Chemicals,
      budget_ChemicalPlant,
      TChemicalBlock ) do
      begin
        Beauty := -40;
        DesignEffic := 1.1;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MarikoChemical, 'Chemical Plant', vidFacility_MarikoChemical, TFacility ) do
      begin
        xSize := 4;
        ySize := 4;
        Level := 15;
        FacId := FID_Chemical;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoChemicalConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoChemical])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoIndustrialFacilities;
        TechnologyKind := tidInventionKind_LargeChemical;
        Register( tidClassFamily_Facilities );
      end;

    // Small Mine
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoSmallMineConstr,
      cost_MineSmall,
      [45, 40, 15],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaMineBlock.Create(
      tidBlock_MarikoSmallMine,
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
    with TMetaFacility.Create( tidFacility_MarikoSmallMine, 'Small Mine', vidFacility_MarikoSmallMine, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 12;
        FacId := FID_Mine;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoSmallMineConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The mine is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoSmallMine])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoIndustrialFacilities;
        TechnologyKind := tidLicence_Mines;
        Register( tidClassFamily_Facilities );
      end;

    // Mine
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoMineConstr,
      cost_Mine,
      [45, 40, 15],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaMineBlock.Create(
      tidBlock_MarikoMine,
      [input_Mine_ExecutivesWorkForce, input_Mine_ProfessionalWorkForce, input_Mine_Workers],
      input_Mine_LegalServices,
      input_Mine_ComputerServices,
      input_Mine_Chemicals,
      output_Mine_Ore,
      budget_Mine,
      TMineBlock ) do
      begin
        Beauty := -100;
        DesignEffic := 1.1;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MarikoMine, 'Mine', vidFacility_MarikoMine, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 12;
        FacId := FID_Mine;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoMineConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The mine is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoMine])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoIndustrialFacilities;
        TechnologyKind := tidInventionKind_LargeMines;
        Register( tidClassFamily_Facilities );
      end;

    // Small Chemical Mine
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoSmallChemMineConstr,
      cost_MineSmall,
      [45, 40, 15],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaChemMineBlock.Create(
      tidBlock_MarikoSmallChemMine,
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
    with TMetaFacility.Create( tidFacility_MarikoSmallChemMine, 'Small Chemical Mine', vidFacility_MarikoSmallChemMine, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 12;
        FacId := FID_ChemicalMine;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoSmallChemMineConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The mine is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoSmallChemMine])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoIndustrialFacilities;
        TechnologyKind := tidLicence_ChemMines;
        Register( tidClassFamily_Facilities );
      end;

    // Chemical Mine
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoChemMineConstr,
      cost_Mine,
      [45, 40, 15],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaChemMineBlock.Create(
      tidBlock_MarikoChemMine,
      [input_Mine_ExecutivesWorkForce, input_Mine_ProfessionalWorkForce, input_Mine_Workers],
      input_Mine_LegalServices,
      input_Mine_ComputerServices,
      input_Mine_Chemicals,
      output_Mine_Ore,
      budget_Mine,
      TChemMineBlock ) do
      begin
        Beauty := -100;
        DesignEffic := 1.1;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MarikoChemMine, 'Chemical Mine', vidFacility_MarikoChemMine, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 12;
        FacId := FID_ChemicalMine;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoChemMineConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The mine is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoChemMine])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoIndustrialFacilities;
        TechnologyKind := tidInventionKind_LargeChemMines;
        Register( tidClassFamily_Facilities );
      end;

    // Small Silicon Mine
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoSmallSiliconMineConstr,
      cost_MineSmall,
      [45, 40, 15],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaSiliconMineBlock.Create(
      tidBlock_MarikoSmallSiliconMine,
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
    with TMetaFacility.Create( tidFacility_MarikoSmallSiliconMine, 'Small Silicon Mine', vidFacility_MarikoSmallSiliconMine, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 12;
        FacId := FID_SiliconMine;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoSmallSiliconMineConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The mine is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoSmallSiliconMine])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoIndustrialFacilities;
        TechnologyKind := tidLicence_SiliconMines;
        Register( tidClassFamily_Facilities );
      end;

    // Silicon Mine
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoSiliconMineConstr,
      cost_Mine,
      [45, 40, 15],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaSiliconMineBlock.Create(
      tidBlock_MarikoSiliconMine,
      [input_Mine_ExecutivesWorkForce, input_Mine_ProfessionalWorkForce, input_Mine_Workers],
      input_Mine_LegalServices,
      input_Mine_ComputerServices,
      input_Mine_Chemicals,
      output_Mine_Ore,
      budget_Mine,
      TChemMineBlock ) do
      begin
        Beauty := -100;
        DesignEffic := 1.1;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MarikoSiliconMine, 'Silicon Mine', vidFacility_MarikoSiliconMine, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 12;
        FacId := FID_SiliconMine;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoSiliconMineConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The mine is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoSiliconMine])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoIndustrialFacilities;
        TechnologyKind := tidInventionKind_LargeSiliconMines;
        Register( tidClassFamily_Facilities );
      end;

    // Small Stone Mine
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoSmallStoneMineConstr,
      cost_MineSmall,
      [45, 40, 15],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaStoneMineBlock.Create(
      tidBlock_MarikoSmallStoneMine,
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
    with TMetaFacility.Create( tidFacility_MarikoSmallStoneMine, 'Small Stone Mine', vidFacility_MarikoSmallStoneMine, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 12;
        FacId := FID_StoneMine;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoSmallStoneMineConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The mine is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoSmallStoneMine])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoIndustrialFacilities;
        TechnologyKind := tidLicence_StoneMines;
        Register( tidClassFamily_Facilities );
      end;

    // Stone Mine
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoStoneMineConstr,
      cost_Mine,
      [45, 40, 15],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaStoneMineBlock.Create(
      tidBlock_MarikoStoneMine,
      [input_Mine_ExecutivesWorkForce, input_Mine_ProfessionalWorkForce, input_Mine_Workers],
      input_Mine_LegalServices,
      input_Mine_ComputerServices,
      input_Mine_Chemicals,
      output_Mine_Ore,
      budget_Mine,
      TChemMineBlock ) do
      begin
        Beauty := -100;
        DesignEffic := 1.1;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MarikoStoneMine, 'Stone Mine', vidFacility_MarikoStoneMine, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 12;
        FacId := FID_StoneMine;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoStoneMineConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The mine is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoStoneMine])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoIndustrialFacilities;
        TechnologyKind := tidInventionKind_LargeStoneMines;
        Register( tidClassFamily_Facilities );
      end;

    // Small Coal Mine
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoSmallCoalMineConstr,
      cost_MineSmall,
      [45, 40, 15],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaCoalMineBlock.Create(
      tidBlock_MarikoSmallCoalMine,
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
    with TMetaFacility.Create( tidFacility_MarikoSmallCoalMine, 'Small Coal Mine', vidFacility_MarikoSmallCoalMine, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 12;
        FacId := FID_CoalMine;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoSmallCoalMineConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The mine is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoSmallCoalMine])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoIndustrialFacilities;
        TechnologyKind := tidLicence_CoalMines;
        Register( tidClassFamily_Facilities );
      end;

    // Coal Mine
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoCoalMineConstr,
      cost_Mine,
      [45, 40, 15],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaCoalMineBlock.Create(
      tidBlock_MarikoCoalMine,
      [input_Mine_ExecutivesWorkForce, input_Mine_ProfessionalWorkForce, input_Mine_Workers],
      input_Mine_LegalServices,
      input_Mine_ComputerServices,
      input_Mine_Chemicals,
      output_Mine_Ore,
      budget_Mine,
      TChemMineBlock ) do
      begin
        Beauty := -100;
        DesignEffic := 1.1;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MarikoCoalMine, 'Coal Mine', vidFacility_MarikoCoalMine, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 12;
        FacId := FID_CoalMine;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoCoalMineConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The mine is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoCoalMine])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoIndustrialFacilities;
        TechnologyKind := tidInventionKind_LargeCoalMines;
        Register( tidClassFamily_Facilities );
      end;

    // Small Food Processor
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoSmallFoodProcConstr,
      cost_FoodProcessingPlantSmall,
      [40, 40, 20],
      40,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFoodProcessorBlock.Create(
      tidBlock_MarikoSmallFoodProc,
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
    with TMetaFacility.Create( tidFacility_MarikoSmallFoodProc, 'Small Food Processor', vidFacility_MarikoSmallFoodProc, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 30;
        FacId := FID_FoodProc;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoSmallFoodProcConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoSmallFoodProc])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoIndustrialFacilities;
        TechnologyKind := tidLicence_FoodProc;
        Register( tidClassFamily_Facilities );
      end;

    // Food Processor
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoFoodProcConstr,
      cost_FoodProcessingPlant,
      [40, 40, 20],
      40,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFoodProcessorBlock.Create(
      tidBlock_MarikoFoodProc,
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
        DesignEffic := 1.1;
      //BeautyStrength := 1;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MarikoFoodProc, 'Food Processor', vidFacility_MarikoFoodProc, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 30;
        FacId := FID_FoodProc;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoFoodProcConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoFoodProc])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoIndustrialFacilities;
        TechnologyKind := tidInventionKind_LargeFoodProc;
        Register( tidClassFamily_Facilities );
      end;

    // Small Metal industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoSmallMetalConstr,
      cost_MetalurgicSmall,
      [40, 50, 10],
      55,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaMetalIndustryBlock.Create(
      tidBlock_MarikoSmallMetal,
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
    with TMetaFacility.Create( tidFacility_MarikoSmallMetal, 'Small Metal Industry', vidFacility_MarikoSmallMetal, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 30;
        FacId := FID_Metal;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoSmallMetalConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoSmallMetal])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoIndustrialFacilities;
        TechnologyKind := tidLicence_Metallurgy;
        Register( tidClassFamily_Facilities );
      end;

    // Metal industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoMetalConstr,
      cost_Metalurgic,
      [40, 50, 10],
      55,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaMetalIndustryBlock.Create(
      tidBlock_MarikoMetal,
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
        DesignEffic := 1.1;
      //BeautyStrength := 1;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MarikoMetal, 'Metal Industry', vidFacility_MarikoMetal, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 30;
        FacId := FID_Metal;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoMetalConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoMetal])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoIndustrialFacilities;
        TechnologyKind := tidInventionKind_LargeMetallurgy;
        Register( tidClassFamily_Facilities );
      end;

    // Plastic industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoPlasticConstr,
      cost_Plastic,
      [40, 50, 10],
      55,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaPlasticIndustryBlock.Create(
      tidBlock_MarikoPlastic,
      [input_Plastic_ExecutivesWorkForce, input_Plastic_ProfessionalWorkForce, input_Plastic_Workers],
      input_Plastic_Oil,
      input_Plastic_Chemicals,
      input_Plastic_LegalServices,
      input_Plastic_ComputerServices,
      output_Plastic_Plastic,
      budget_Plastic,
      TPlasticIndustryBlock ) do
      begin
        Beauty := -100;
        DesignEffic := 1.1;
      //BeautyStrength := 1;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MarikoPlastic, 'Plastic Industry', vidFacility_MarikoPlastic, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 30;
        FacId := FID_Plastics;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoPlasticConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoPlastic])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoIndustrialFacilities;
        TechnologyKind := tidLicence_Plastics;
        Register( tidClassFamily_Facilities );
      end;

    // CD industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoCDConstr,
      35000000,
      [40, 50, 10],
      55,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaCDBlock.Create(
      tidBlock_MarikoCD,
      [1, 6, 100],
      100*0.1,
      100*0.1,
      100*0.1, // Plastic
      100*0.1, // Printed Material
      100*1,   // CD
      0,
      TCDBlock ) do
      begin
        Beauty := -100;
        DesignEffic := 1.1;
      //BeautyStrength := 1;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MarikoCD, 'CD Plant', vidFacility_MarikoCD, TFacility ) do
      begin
        xSize := 4;
        ySize := 4;
        Level := 30;
        FacId := FID_CDPlant;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoCDConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoCD])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoIndustrialFacilities;
        TechnologyKind := tidInventionKind_CDPlant;
        Register( tidClassFamily_Facilities );
      end;

    // Small Textile industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoSmallTextileConstr,
      cost_TextileSmall,
      [45, 40, 15],
      40,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaTextilIndustryBlock.Create(
      tidBlock_MarikoSmallTextile,
      [input_TextileSmall_ExecutivesWorkForce, input_TextileSmall_ProfessionalWorkForce, input_TextileSmall_Workers],
      input_TextileSmall_OrganicMaterials,
      input_TextileSmall_Chemicals,
      0,//input_TextileSmall_ComputerServices,
      0,//input_TextileSmall_LegalServices,
      output_TextileSmall_FabricsandThreads,
      budget_TextileSmall,
      TTextilIndustryBlock ) do
      begin
        Beauty := -20;
      //BeautyStrength := 0.5;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MarikoSmallTextile, 'Small Textile Industry', vidFacility_MarikoSmallTextile, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 20;
        FacId := FID_Textile;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoSmallTextileConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoSmallTextile])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoIndustrialFacilities;
        TechnologyKind := tidLicence_Textile;
        Register( tidClassFamily_Facilities );
      end;

    // Textile industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoTextileConstr,
      cost_Textile,
      [45, 40, 15],
      40,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaTextilIndustryBlock.Create(
      tidBlock_MarikoTextile,
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
        DesignEffic := 1.1;
      //BeautyStrength := 0.5;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MarikoTextile, 'Textile Industry', vidFacility_MarikoTextile, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 20;
        FacId := FID_Textile;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoTextileConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoTextile])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoIndustrialFacilities;
        TechnologyKind := tidInventionKind_LargeTextile;
        Register( tidClassFamily_Facilities );
      end;

    // Small Clothings industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoSmallClothingsConstr,
      cost_ClothingSmall,
      [50, 40, 10],
      30,
      TBlockUnderConstruction) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaClothingsIndustryBlock.Create(
      tidBlock_MarikoSmallClothings,
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
    with TMetaFacility.Create( tidFacility_MarikoSmallClothings, 'Small Clothing Industry', vidFacility_MarikoSmallClothings, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 40;
        FacId := FID_Clothes;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoSmallClothingsConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoSmallClothings])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoIndustrialFacilities;
        TechnologyKind := tidLicence_Clothing;
        Register( tidClassFamily_Facilities );
      end;

    // Clothings industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoClothingsConstr,
      cost_Clothing,
      [50, 40, 10],
      30,
      TBlockUnderConstruction) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaClothingsIndustryBlock.Create(
      tidBlock_MarikoClothings,
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
        DesignEffic := 1.1;
      //BeautyStrength := 0.5;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MarikoClothings, 'Clothing Industry', vidFacility_MarikoClothings, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 40;
        FacId := FID_Clothes;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoClothingsConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoClothings])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoIndustrialFacilities;
        TechnologyKind := tidInventionKind_LargeClothes;
        Register( tidClassFamily_Facilities );
      end;

    // Construction industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoConstructionConstr,
      cost_Construction,
      [45, 40, 15],
      40,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaConstructionIndustryBlock.Create(
      tidBlock_MarikoConstruction,
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
        DesignEffic := 1.1;
      //BeautyStrength := 0.5;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MarikoConstruction, 'Construction Industry', vidFacility_MarikoConstruction, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 20;
        FacId := FID_Construction;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoConstructionConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoConstruction])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoIndustrialFacilities;
        TechnologyKind := tidLicence_Construction;
        Register( tidClassFamily_Facilities );
      end;

    // Small Electronic industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoSmallElectronicConstr,
      cost_ElectronicIndustrySmall,
      [40, 40, 20],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaElectronicIndustryBlock.Create(
      tidBlock_MarikoSmallElectronic,
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
        DesignEffic := 1.1;
      //BeautyStrength := 0.5;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MarikoSmallElectronic, 'Small Electronic Industry', vidFacility_MarikoSmallElectronic, TFacility ) do
      begin
        xSize := 4;
        ySize := 4;
        Level := 20;
        FacId := FID_ElectComp;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires Electronic materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoSmallElectronicConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoSmallElectronic])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoIndustrialFacilities;
        TechnologyKind := tidLicence_Electronics;
        Register( tidClassFamily_Facilities );
      end;

    // Electronic industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoElectronicConstr,
      cost_ElectronicIndustry,
      [40, 40, 20],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaElectronicIndustryBlock.Create(
      tidBlock_MarikoElectronic,
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
        DesignEffic := 1.15;
      //BeautyStrength := 0.5;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MarikoElectronic, 'Electronic Industry', vidFacility_MarikoElectronic, TFacility ) do
      begin
        xSize := 4;
        ySize := 4;
        Level := 20;
        FacId := FID_ElectComp;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires Electronic materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoElectronicConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoElectronic])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoIndustrialFacilities;
        TechnologyKind := tidInventionKind_LargeElectComp;
        Register( tidClassFamily_Facilities );
      end;

    // Heavy industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoHeavyConstr,
      cost_HeavyIndustry,
      [45, 40, 15],
      60,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaHeavyIndustryBlock.Create(
      tidBlock_MarikoHeavy,
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
        DesignEffic := 1.1;
      //BeautyStrength := 0.5;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MarikoHeavy, 'Machinery Industry', vidFacility_MarikoHeavy, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 20;
        FacId := FID_Heavy;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires Heavy materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoHeavyConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoHeavy])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoIndustrialFacilities;
        TechnologyKind := tidLicence_Heavy;
        Register( tidClassFamily_Facilities );
      end;

    // Car industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoCarIndustryConstr,
      cost_CarIndustry,
      [40, 45, 15],
      55,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaCarIndustryBlock.Create(
      tidBlock_MarikoCarIndustry,
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
        DesignEffic := 1.1;
        //BeautyStrength := 0.5;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MarikoCarIndustry, 'Car Industry', vidFacility_MarikoCarIndustry, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 40;
        FacId := FID_Car;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires Heavy materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoCarIndustryConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoCarIndustry])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoIndustrialFacilities;
        TechnologyKind := tidLicence_Cars;
        Register( tidClassFamily_Facilities );
      end;

    // Small HHA industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoSmallHHAIndustryConstr,
      cost_HouseHoldingAppliancesSmall,
      [45, 40, 15],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaHouseHoldingAppliancesBlock.Create(
      tidBlock_MarikoSmallHHAIndustry,
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
    with TMetaFacility.Create( tidFacility_MarikoSmallHHAIndustry, 'Small Household Appliances', vidFacility_MarikoSmallHHAIndustry, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 40;
        FacId := FID_Household;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires Heavy materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoSmallHHAIndustryConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoSmallHHAIndustry])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoIndustrialFacilities;
        TechnologyKind := tidLicence_HHA;
        Register( tidClassFamily_Facilities );
      end;

    // HHA industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoHHAIndustryConstr,
      cost_HouseHoldingAppliances,
      [45, 40, 15],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaHouseHoldingAppliancesBlock.Create(
      tidBlock_MarikoHHAIndustry,
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
        DesignEffic := 1.1;
      //BeautyStrength := 0.5;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MarikoHHAIndustry, 'Household Appliances', vidFacility_MarikoHHAIndustry, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 40;
        FacId := FID_Household;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires Heavy materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoHHAIndustryConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoHHAIndustry])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoIndustrialFacilities;
        TechnologyKind := tidInventionKind_LargeHHA;
        Register( tidClassFamily_Facilities );
      end;

    // Business Machines
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoBusinessMachineConstr,
      cost_BusinessMachines,
      [40, 40, 20],
      40,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaBusinessMachinesBlock.Create(
      tidBlock_MarikoBusinessMachine,
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
        DesignEffic := 1.1;
        //BeautyStrength := 0.5;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MarikoBusinessMachine, 'Business Machines', vidFacility_MarikoBusinessMachine, TFacility ) do
      begin
        xSize := 4;
        ySize := 4;
        Level := 40;
        FacId := FID_BusMach;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires Heavy materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoBusinessMachineConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoBusinessMachine])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoIndustrialFacilities;
        TechnologyKind := tidLicence_BMIndustry;
        Register( tidClassFamily_Facilities );
      end;
  end;

procedure RegisterStores;
  begin
    // Food Store
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoFoodStoreConstr,
      50000,
      [90, 0, 10],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFoodStoreBlock.Create(
      tidBlock_MarikoFoodStore,
      [0, 0, 10],
      25, // people buying per hour
      24, // people buying per hour
      FairPrice,
      [0.9, 0.9, 0.9],
      120,
      TFoodStoreBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MarikoFoodStore, 'Food Store', vidFacility_MarikoFoodStore, TFacility ) do
      begin
        xSize := 1;
        ySize := 1;
        Level := 100;
        FacId := FID_FoodStore;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoFoodStoreConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Food Store is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoFoodStore])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoServiceFacilities;
        TechnologyKind := tidInventionKind_ServiceFacilities;
        DepOnTech := false; // make available even without HQs
        Register( tidClassFamily_Facilities );
      end;

    // Clothes store
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoClothesStoreConstr,
      60000,
      [90, 0, 10],
      15,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaClothesShopBlock.Create(
      tidBlock_MarikoClothesStore,
      [0, 1, 10],
      10,
      FairPrice,
      [0.6, 0.6, 0.7],
      130,
      TClothesShopBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MarikoClothesStore, 'Clothes Store', vidFacility_MarikoClothesStore, TFacility ) do
      begin
        xSize := 1;
        ySize := 1;
        Level := 1000;
        FacId := FID_ClotheStore;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoClothesStoreConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Shop is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoClothesStore])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoServiceFacilities;
        TechnologyKind := tidInventionKind_ServiceFacilities;
        DepOnTech := false; // make available even without HQs
        Register( tidClassFamily_Facilities );
      end;

    // HHA store
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoHHAStoreConstr,
      60000,
      [90, 0, 10],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaHHAStoreBlock.Create(
      tidBlock_MarikoHHAStore,
      [0, 1, 10],
      5,
      FairPrice,
      [0.8, 0.8, 0.9],
      120,
      THHAStoreBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MarikoHHAStore, 'Appliances Store', vidFacility_MarikoHHAStore, TFacility ) do
      begin
        xSize := 1;
        ySize := 1;
        Level := 120;
        FacId := FID_HHAStore;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoHHAStoreConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Shop is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoHHAStore])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoServiceFacilities;
        TechnologyKind := tidInventionKind_ServiceFacilities;
        DepOnTech := false; // make available even without HQs
        Register( tidClassFamily_Facilities );
      end;

    // Computer store
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoComputerStoreConstr,
      2500000,
      [90, 0, 10],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaComputerStoreBlock.Create(
      tidBlock_MarikoComputerStore,
      [0, 3, 10],
      1,
      FairPrice,
      [0.8, 0.8, 0.9],
      120,
      TComputerStoreBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MarikoComputerStore, 'Computer Store', vidFacility_MarikoComputerStore, TFacility ) do
      begin
        xSize := 1;
        ySize := 1;
        Level := 120;
        FacId := FID_CompStore;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoComputerStoreConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Shop is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoComputerStore])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoServiceFacilities;
        TechnologyKind := tidInventionKind_ServiceFacilities;
        DepOnTech := false; // make available even without HQs
        Register( tidClassFamily_Facilities );
      end;

    // Car Store
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoCarStoreConstr,
      500000,
      [90, 0, 10],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaCarShopBlock.Create(
      tidBlock_MarikoCarStore,
      [0, 2, 10],
      1/(24*7),
      2/(24*7),
      1,
      FairPrice,
      [0.9, 0.9, 0.9],
      150,
      TCarShopBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MarikoCarStore, 'Car Store', vidFacility_MarikoCarStore, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 120;
        FacId := FID_CarStore;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoCarStoreConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Shop is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoCarStore])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoServiceFacilities;
        TechnologyKind := tidInventionKind_ServiceFacilities;
        DepOnTech := false; // make available even without HQs
        Register( tidClassFamily_Facilities );
      end;

    // CD store
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoCDStoreConstr,
      1000000,
      [90, 0, 10],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaCDStoreBlock.Create(
      tidBlock_MarikoCDStore,
      [0, 2, 10],
      15,
      FairPrice,
      [0.95, 0.95, 0.7],
      100,
      TCDStoreBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MarikoCDStore, 'CD Store', vidFacility_MarikoCDStore, TFacility ) do
      begin
        xSize := 1;
        ySize := 1;
        Level := 1000;
        FacId := FID_CDStore;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoCDStoreConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Shop is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoCDStore])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoServiceFacilities;
        TechnologyKind := tidInventionKind_ServiceFacilities;
        DepOnTech := false; // make available even without HQs
        Register( tidClassFamily_Facilities );
      end;

    // Supermarket A
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoSupermarketConstrA,
      1500000,
      [80, 0, 20],
      45,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaSuperMarketBlock.Create(
      tidBlock_MarikoSupermarketA,
      [0, 2, 20],
      2/(24*7),
      1/(24*7),
      30,
      32,
      18,
      10,
      FairPrice,
      [1, 0.9, 0.9],
      200,
      TSuperMarketBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MarikoSupermarketA, 'Takuri Market', vidFacility_MarikoSupermarketA, TFacility ) do
      begin
        xSize := 4;
        ySize := 4;
        Level := 120;
        FacId := FID_Supermarket;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoSupermarketConstrA])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Supermarket is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoSupermarketA])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoServiceFacilities;
        TechnologyKind := tidInventionKind_Supermarkets;
        Register( tidClassFamily_Facilities );
      end;

    // Supermarket B
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoSupermarketConstrB,
      800000,
      [80, 0, 20],
      40,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaSuperMarketBlock.Create(
      tidBlock_MarikoSupermarketB,
      [0, 2, 15],
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
    with TMetaFacility.Create( tidFacility_MarikoSupermarketB, 'Tamakura Market', vidFacility_MarikoSupermarketB, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 120;
        FacId := FID_Supermarket;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoSupermarketConstrB])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Supermarket is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoSupermarketB])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoServiceFacilities;
        TechnologyKind := tidInventionKind_Supermarkets;
        Register( tidClassFamily_Facilities );
      end;

    // Supermarket C
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoSupermarketConstrC,
      500000,
      [80, 0, 20],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaSuperMarketBlock.Create(
      tidBlock_MarikoSupermarketC,
      [0, 1, 12],
      3/(5*24*7),
      2/(5*24*7),
      10,
      10,
      8,
      6,
      FairPrice,
      [0.9, 0.9, 0.9],
      200,
      TSuperMarketBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MarikoSupermarketC, 'Satoi Market', vidFacility_MarikoSupermarketC, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 120;
        FacId := FID_Supermarket;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoSupermarketConstrC])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Supermarket is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoSupermarketC])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoServiceFacilities;
        TechnologyKind := tidInventionKind_ServiceFacilities;
        DepOnTech := false; // make available even without HQs
        Register( tidClassFamily_Facilities );
      end;

    {// Bar
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoBarConstr,
      200000,
      [90, 0, 10],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaBarBlock.Create(
      tidBlock_MarikoBar,
      [1, 2, 15],
      20,
      FairPrice,
      [0.8, 0.8, 0.8],
      TBarBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MarikoBar, 'Bar', vidFacility_MarikoBar, TFacility ) do
      begin
        xSize := 1;
        ySize := 1;
        Level := 100;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoBarConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Bar is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoBar])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoServiceFacilities;
        Technology := nidInvention_ServiceFacilities;
        TechnologyKind := tidInventionKind_ServiceFacilities;
        Register( tidClassFamily_Facilities );
      end;}

    {// Restaurant
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoRestaurantConstr,
      400000,
      [90, 0, 10],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaRestaurantBlock.Create(
      tidBlock_MarikoRestaurant,
      [2, 5, 25],
      25,
      FairPrice,
      [0.9, 0.8, 0.7],
      TRestaurantBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MarikoRestaurant, 'Restaurant', vidFacility_MarikoRestaurant, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 1000;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoRestaurantConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Restaurant is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoRestaurant])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoServiceFacilities;
        Technology := nidInvention_ServiceFacilities;
        TechnologyKind := tidInventionKind_ServiceFacilities;
        Register( tidClassFamily_Facilities );
      end;}

  end;

procedure RegisterHeadquarters;
  begin
    // General
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoGeneralHeadquarterConstr,
      4000000,
      [60, 0, 40],
      10,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaMainHeadquarter.Create(
      tidBlock_MarikoGeneralHeadquarter,
      [30, 50, 10],
      tidInventionKind_Direction,
      TMainHeadquarter ) do
      begin
        Beauty := 100;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MarikoGeneralHeadquarter, 'Company Headquarters', vidFacility_MarikoGeneralHeadquarter, TFacility ) do
      begin
        xSize := 3;
        ySize := 3;
        Level := 5000;
        FacId := FID_MainHeadquarter;
        Options := Options - [mfcGenerateName, mfcInTown];
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoGeneralHeadquarterConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The facility is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidFacility_MarikoGeneralHeadquarter])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoDistributedDirection;
        Register( tidClassFamily_Facilities );
      end;

    // General Standalone
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoGeneralHeadquarterConstr + 'STA',
      4000000,
      [60, 0, 40],
      10,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaMainHeadquarter.Create(
      tidBlock_MarikoGeneralHeadquarter + 'STA',
      [15, 7, 1],
      tidInventionKind_Direction,
      TMainHeadquarter ) do
      begin
        Beauty     := 100;
        Standalone := true;
        VisualStages := 5;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MarikoGeneralHeadquarter + 'STA', 'Company Headquarters', 701, TFacility ) do
      begin
        xSize := 3;
        ySize := 3;
        Level := 5000;
        FacId := FID_MainHeadquarter;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoGeneralHeadquarterConstr + 'STA'])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The facility is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidFacility_MarikoGeneralHeadquarter + 'STA'])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoDistributedDirection;
        UniquenessMask := $00000001;
        Register( tidClassFamily_Facilities );
      end;

    // Industries
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoIndHeadquarterConstr,
      1500000,
      [60, 0, 40],
      10,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaHeadquarterBlock.Create(
      tidBlock_MarikoIndHeadquarter,
      [30, 25, 10],
      tidInventionKind_IndustrialFacilities,
      THeadquarterBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MarikoIndHeadquarter, 'Industries Headquarters', vidFacility_MarikoIndHeadquarter, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 5000;
        FacId := FID_IndHeadquarter;
        Options := Options - [mfcGenerateName, mfcInTown];
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoIndHeadquarterConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The facility is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidFacility_MarikoIndHeadquarter])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoDistributedDirection;
        TechnologyKind := tidInventionKind_Direction;
        Register( tidClassFamily_Facilities );
      end;

    // Markets
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoServiceHeadquarterConstr,
      1500000,
      [60, 0, 40],
      10,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaHeadquarterBlock.Create(
      tidBlock_MarikoServiceHeadquarter,
      [30, 25, 10],
      tidInventionKind_ServiceFacilities,
      //nidInvention_ServiceFacilities,
      THeadquarterBlock ) do
      begin
        //RegisterInvention( nidInvention_BigMarkets );
        //RegisterInvention( nidInvention_Bars );
        //RegisterInvention( nidInvention_Restaurants );
        //RegisterInvention( nidInvention_Movies );
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MarikoServiceHeadquarter, 'Commerce Headquarters', vidFacility_MarikoServiceHeadquarter, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 5000;
        FacId := FID_CommHeadquarter;
        Options := Options - [mfcGenerateName, mfcInTown];
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoServiceHeadquarterConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The facility is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidFacility_MarikoServiceHeadquarter])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoDistributedDirection;
        TechnologyKind := tidInventionKind_Direction;
        Register( tidClassFamily_Facilities );
      end;

    // Residential Headquarter
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoResHeadquarterConstr,
      1500000,
      [60, 0, 40],
      10,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaHeadquarterBlock.Create(
      tidBlock_MarikoResHeadquarter,
      [33, 7, 12],
      tidInventionKind_OfficeAndResidentials,
      //nidInvention_OfficeAndResidentials,
      THeadquarterBlock ) do
      begin
        //RegisterInvention(nidInvention_OfficeBuildings);
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MarikoResHeadquarter, 'Real Estates Headquarters', vidFacility_MarikoResHeadquarter, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 5000;
        FacId := FID_OffcHeadquarter;
        Options := Options - [mfcGenerateName, mfcInTown];
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoResHeadquarterConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The facility is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidFacility_MarikoResHeadquarter])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoDistributedDirection;
        TechnologyKind := tidInventionKind_Direction;
        Register( tidClassFamily_Facilities );
      end;

    // Public Facilities
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoPubHeadquarterConstr,
      1500000,
      [60, 0, 40],
      10,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaPublicAffairsHeadquarter.Create(
      tidBlock_MarikoPubHeadquarter,
      [33, 7, 12],
      tidInventionKind_PublicFacilities,
      //nidInvention_PublicFacilities,
      maxHQAdv,
      TPublicAffairsHeadquarter ) do
      begin
        //RegisterInvention( nidInvention_Monuments );
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MarikoPubHeadquarter, 'Civic Affairs Headquarters', vidFacility_MarikoPubHeadquarter, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 5000;
        FacId := FID_PubHeadquarter;
        Options := Options - [mfcGenerateName, mfcInTown];
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'RequiPub construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoPubHeadquarterConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The facility is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidFacility_MarikoPubHeadquarter])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoDistributedDirection;
        TechnologyKind := tidInventionKind_Direction;
        Register( tidClassFamily_Facilities );
      end;

  end;

procedure RegisterSpecialFacilities;
  begin
    {// Computing Industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoComputingIndustryConstr,
      cost_ComputingIndustry,
      [70, 0, 30],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaSoftwareBlock.Create(
      tidBlock_MarikoComputingIndustry,
      [input_ComputingIndustry_ExecutivesWorkForce, input_ComputingIndustry_ProfessionalWorkForce, input_ComputingIndustry_Workers],
      input_ComputingIndustry_LegalServices,
      output_ComputerServices,
      budget_ComputingIndustry,
      TSoftwareBlock ) do
      begin
        Beauty := 100;
      //BeautyStrength := 0.1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MarikoComputingIndustry, 'Software Firm', vidFacility_MarikoComputingIndustry, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 40;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoComputingIndustryConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Computing Industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoComputingIndustry])));
        ClusterName    := tidClusterName_Mariko;
        FacilityKind   := tidFacilityKind_MarikoIndustrialFacilities;
        Technology := nidInvention_IndustrialFacilities;
        TechnologyKind := tidInventionKind_IndustrialFacilities;
        Register( tidClassFamily_Facilities );
      end;

    // Legal Services
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoLegalServicesConstr,
      cost_LegalServices,
      [80, 0, 20],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaLegalServiceBlock.Create(
      tidBlock_MarikoLegalServices,
      [input_LegalServices_ExecutivesWorkForce, input_LegalServices_ProfessionalWorkForce, input_LegalServices_Workers],
      input_LegalServices_ComputerServices,
      output_LegalServices,
      budget_LegalServices,
      TSoftwareBlock ) do
      begin
        Beauty := 100;
      //BeautyStrength := 0.1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MarikoLegalServices, 'Lawyer Firm', vidFacility_MarikoLegalServices, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 100;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoLegalServicesConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Firm is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoLegalServices])));
        ClusterName    := tidClusterName_Mariko;
        FacilityKind   := tidFacilityKind_MarikoIndustrialFacilities;
        Technology := nidInvention_IndustrialFacilities;
        TechnologyKind := tidInventionKind_IndustrialFacilities;
        Register( tidClassFamily_Facilities );
      end;}

    // TV Station
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoTVStationConstr,
      30000000,
      [70, 0, 30],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaBroadcaster.Create(
      tidBlock_MarikoTVStation,
      tidBroadcast_TV,
      200,
      [4, 45, 35],
      accIdx_TV_Supplies,
      accIdx_TV_Products,
      accIdx_TV_Salaries,
      TBroadcaster ) do
      begin
        Beauty := 200;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MarikoTVStation, 'TV', vidFacility_MarikoTVStation, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 60;
        FacId := FID_TVStation;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoTVStationConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Firm is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoTVStation])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoSpecial;
        TechnologyKind := tidInventionKind_Television;
        Register( tidClassFamily_Facilities );
      end;

    // TV Antenna
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MarikoTVAntennaConstr,
      500000,
      [90, 0, 10],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaAntenna.Create(
      tidBlock_MarikoTVAntenna,
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
    with TMetaFacility.Create( tidFacility_MarikoTVAntenna, 'TV Antenna', vidFacility_MarikoTVAntenna, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 50;
        FacId := FID_TVAntena;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoTVAntennaConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Firm is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MarikoTVAntenna])));
        ClusterName := tidClusterName_Mariko;
        FacilityKind := tidFacilityKind_MarikoSpecial;
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
    result := 'MarikoPack1';
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


