{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y-,Z1}
{$MINSTACKSIZE $00004000}
{$MAXSTACKSIZE $00100000}
{$IMAGEBASE $00400000}
{$APPTYPE GUI}
library MoabPack1;

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
  MoabConst in 'MoabConst.pas',
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
  ServiceBlock in '..\..\StdBlocks\ServiceBlock.pas',
  Restaurant in '..\..\StdBlocks\Restaurant.pas',
  Circuits in '..\..\Circuits\Circuits.pas',
  PublicFacility in '..\..\Kernel\PublicFacility.pas',
  Computing in '..\..\StdBlocks\Computing.pas',
  LegalServices in '..\..\StdBlocks\LegalServices.pas',
  CacheCommon in '..\..\Cache\CacheCommon.pas',
  Standards in '..\Standards.pas',
  Land in '..\..\Land\Land.pas',
  OfficeBlock in '..\..\StdBlocks\OfficeBlock.pas',
  NewsServerInterfaces in '..\..\News Server\NewsServerInterfaces.pas',
  Environmental in '..\..\StdBlocks\Environmental.pas',
  Movie in '..\..\StdBlocks\Movie.pas',
  TransportInterfaces in '..\..\Transport\TransportInterfaces.pas',
  Transport in '..\..\Transport\Transport.pas',
  MatrixLayer in '..\..\Transport\MatrixLayer.pas',
  VisualClassManager in '..\..\Class Packer\VisualClassManager.pas',
  Broadcast in '..\..\StdBlocks\Broadcast.pas',
  StdAccounts in '..\..\StdBlocks\StdAccounts.pas',
  StdBroadcast in '..\..\StdBlocks\StdBroadcast.pas',
  FacIds in '..\FacIds.pas',
  TownPolitics in '..\..\Kernel\TownPolitics.pas',
  Logs in '..\..\Logs\Logs.pas',
  ModelServerCache in '..\..\Cache\ModelServerCache.pas',
  Inventions in '..\..\Inventions\Inventions.pas',
  OilRig in '..\..\StdBlocks\OilRig.pas',
  Refinery in '..\..\StdBlocks\Refinery.pas',
  GasStation in '..\..\StdBlocks\GasStation.pas',
  RankProtocol in '..\..\Protocol\RankProtocol.pas',
  GenIdd in '..\..\Utils\Serial\GenIdd.pas',
  CommonFacs in '..\CommonFacs.pas',
  ChemMine in '..\..\StdBlocks\ChemMine.pas',
  SiliconMine in '..\..\StdBlocks\SiliconMine.pas',
  StoneMine in '..\..\StdBlocks\StoneMine.pas',
  CoalMine in '..\..\StdBlocks\CoalMine.pas',
  SimMLS in '..\..\Kernel\SimMLS.pas',
  FuneralParlors in '..\..\StdBlocks\FuneralParlors.pas';

procedure RegisterFacilityKinds;
  begin
    with TFacilityKind.Create( tidFacilityKind_MoabSacred ) do
      begin
        Name        := 'Headquarters';
        SuperType   := tidSuperFacKind_Headquarter;
        ClusterName := tidClusterName_Moab;
        Role        := rolBuyer;
        Register( tidClassFamily_FacilityKinds );
      end;
    with TFacilityKind.Create( tidFacilityKind_MoabIndustrial ) do
      begin
        Name        := 'Industries';
        SuperType   := tidSuperFacKind_Industry;
        ClusterName := tidClusterName_Moab;
        Technology  := tidInventionKind_IndustrialFacilities;
        Role        := rolProducer;
        Register( tidClassFamily_FacilityKinds );
      end;
    with TFacilityKind.Create( tidFacilityKind_MoabResidentials ) do
      begin
        Name        := 'Residentials';
        SuperType   := tidSuperFacKind_Residential;
        ClusterName := tidClusterName_Moab;
        Register( tidClassFamily_FacilityKinds );
      end;
    with TFacilityKind.Create( tidFacilityKind_MoabIllusions ) do
      begin
        Name        := 'Commerce';
        SuperType   := tidSuperFacKind_Service;
        ClusterName := tidClusterName_Moab;
        Technology  := tidInventionKind_ServiceFacilities;
        Role        := rolBuyer;
        Register( tidClassFamily_FacilityKinds );
      end;
    with TFacilityKind.Create( tidFacilityKind_MoabCorrectionals ) do
      begin
        Name        := 'Public';
        SuperType   := tidSuperFacKind_Public;
        ClusterName := tidClusterName_Moab;
        Technology  := tidInventionKind_PublicFacilities;
        Register( tidClassFamily_FacilityKinds );
      end;
    with TFacilityKind.Create( tidFacilityKind_MoabBusinessFacilities ) do
      begin
        Name        := 'Offices';
        SuperType   := tidSuperFacKind_Business;
        ClusterName := tidClusterName_Moab;
        Technology  := tidInventionKind_OfficeAndResidentials;
        Register( tidClassFamily_FacilityKinds );
      end;
    with TFacilityKind.Create( tidFacilityKind_MoabSpecial ) do
      begin
        Name        := 'Special';
        SuperType   := tidSuperFacKind_Special;
        ClusterName := tidClusterName_Moab;
        Role        := rolProducer;
        Register( tidClassFamily_FacilityKinds );
      end;
  end;

type
  TMoabCluster =
    class( TCluster )
      public
        function NameNewspaper( TownName : string ) : string; override;
    end;

  function TMoabCluster.NameNewspaper( TownName : string ) : string;
    begin
      result := TownName + ' Herald';
    end;

procedure RegisterClusterFacilities;
  begin
    TMoabCluster.Create( tidClusterName_Moab ).Register( tidClassFamily_Clusters );
    RegisterTownHall( tidClusterName_Moab, tidFacility_MoabTownHall, vidFacility_MoabTownHall, 7, 7, 200, TPoliticalTownHall );
    RegisterTradeCenter( tidClusterName_Moab, tidFacility_MoabTradeCenter, vidFacility_MoabTradeCenter, 2, 2 );
  end;

procedure RegisterResidentials;
  begin
    // Low cost High Class domes
    with TMetaBlockUnderConstruction.Create(
      tidBlock_LoCostKnightsConstr,
      40000,
      [100, 0, 0],
      7,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_LoCostKnights,
      pkHigh,
      20,
      TPopulatedBlock ) do
      begin
        Beauty := -200;
        ModifyPrice := false;
        LowCost := true;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_LoCostKnights, 'Generic high class buildings', vidFacility_LoCostKnights, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        FacId := FID_hiClassLoCost;
        Options := Options - [mfcGenerateName, mfcInTown];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_LoCostKnightsConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_LoCostKnights])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // Low cost MiddleClass domes
    with TMetaBlockUnderConstruction.Create(
      tidBlock_LoCostNursesConstr,
      200000,
      [100, 0, 0],
      7,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_LoCostNurses,
      pkMiddle,
      20,
      TPopulatedBlock ) do
      begin
        Beauty := -200;
        ModifyPrice := false;
        LowCost := true;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_LoCostNurses, 'Generic middle class buildings', vidFacility_LoCostNurses, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        FacId := FID_midClassLoCost;
        Options := Options - [mfcGenerateName, mfcInTown];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_LoCostNursesConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_LoCostNurses])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // Low cost LowClass domes
    with TMetaBlockUnderConstruction.Create(
      tidBlock_LoCostBeingsConstr,
      100000,
      [100, 0, 0],
      10,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_LoCostBeings,
      pkLow,
      40,
      TPopulatedBlock ) do
      begin
        Beauty := -200;
        ModifyPrice := false;
        LowCost := true;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_LoCostBeings, 'Generic low class buildings', vidFacility_LoCostBeings, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        FacId := FID_lowClassLoCost;
        Options := Options - [mfcGenerateName, mfcInTown];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_LoCostBeingsConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_LoCostBeings])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // Knights Building A
    with TMetaBlockUnderConstruction.Create(
      tidBlock_KnightsBuildingConstrA,
      1000000,
      [100, 0, 0],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_KnightsBuildingA,
      pkHigh,
      22,
      TPopulatedBlock ) do
      begin
        Beauty := 60;
      //BeautyStrength := 0.1;
        VisualStages := 2;
        VisualStages := 2;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_KnightsBuildingA, 'Zharot Palace', vidFacility_KnightsBuildingA, TFacility ) do
      begin
        XSize := 3;
        YSize := 3;
        Level := 120;
        FacId := FID_hiClass;
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_KnightsBuildingConstrA])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_KnightsBuildingA])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // Knights Building B
    with TMetaBlockUnderConstruction.Create(
      tidBlock_KnightsBuildingConstrB,
      1200000,
      [100, 0, 0],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_KnightsBuildingB,
      pkHigh,
      27,
      TPopulatedBlock ) do
      begin
        Beauty := 60;
      //BeautyStrength := 0.1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_KnightsBuildingB, 'Black Palace', vidFacility_KnightsBuildingB, TFacility ) do
      begin
        XSize := 3;
        YSize := 3;
        Level := 120;
        FacId := FID_hiClass;
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_KnightsBuildingConstrB])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_KnightsBuildingB])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // Knights Building C
    with TMetaBlockUnderConstruction.Create(
      tidBlock_KnightsBuildingConstrC,
      2800000,
      [100, 0, 0],
      25,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_KnightsBuildingC,
      pkHigh,
      77,
      TPopulatedBlock ) do
      begin
        Beauty := 60;
      //BeautyStrength := 0.1;
        VisualStages := 2;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_KnightsBuildingC, 'Knights Dome', vidFacility_KnightsBuildingC, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        FacId := FID_hiClass;
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_KnightsBuildingConstrC])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_KnightsBuildingC])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // Knight building D
    with TMetaBlockUnderConstruction.Create(
      tidBlock_KnightsBuildingConstrD,
      500000,
      [100, 0, 0],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_KnightsBuildingD,
      pkHigh,
      23,
      TPopulatedBlock ) do
      begin
        Efficiency := 0.8;
        Beauty := 60;
      //BeautyStrength := 0.1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_KnightsBuildingD, 'Knights Bungalow', vidFacility_KnightsBuildingD, TFacility ) do
      begin
        XSize := 3;
        YSize := 3;
        Level := 120;
        FacId := FID_hiClass;
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_KnightsBuildingConstrD])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_KnightsBuildingD])));
        ClusterName  := tidClusterName_Moab;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        FacilityKind := tidFacilityKind_MoabResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // Nurse building A
    with TMetaBlockUnderConstruction.Create(
      tidBlock_NurseDomeConstrA,
      3000000,
      [100, 0, 0],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_NurseDomeA,
      pkMiddle,
      133,
      TPopulatedBlock ) do
      begin
        Efficiency := 0.7;
        Beauty := 20;
      //BeautyStrength := 0.1;
        VisualStages := 2;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_NurseDomeA, 'Nurse Dome', vidFacility_NurseDomeA, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        FacId := FID_midClass;
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_NurseDomeConstrA])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Dome Completed', 'Dome is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_NurseDomeA])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // Nurse building B
    with TMetaBlockUnderConstruction.Create(
      tidBlock_NurseDomeConstrB,
      10000000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_NurseDomeB,
      pkMiddle,
      333,
      TPopulatedBlock ) do
      begin
        Efficiency := 0.75;
        Beauty := 70;
      //BeautyStrength := 0.1;
        VisualStages := 2;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_NurseDomeB, 'Nurse Zoo', vidFacility_NurseDomeB, TFacility ) do
      begin
        XSize := 5;
        YSize := 5;
        Level := 120;
        FacId := FID_midClass;
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_NurseDomeConstrB])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Dome Completed', 'Dome is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_NurseDomeB])));
        ClusterName  := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // Nurse dome C
    with TMetaBlockUnderConstruction.Create(
      tidBlock_NurseDomeConstrC,
      300000,
      [100, 0, 0],
      10,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_NurseDomeC,
      pkMiddle,
      33,
      TPopulatedBlock ) do
      begin
        Beauty := 60;
      //BeautyStrength := 0.1;
        Efficiency := 0.6;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_NurseDomeC, 'Nurse Rooms', vidFacility_NurseDomeC, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        FacId := FID_midClass;
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_NurseDomeConstrC])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_NurseDomeC])));
        ClusterName  := tidClusterName_Moab;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        FacilityKind := tidFacilityKind_MoabResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // Nurse domes D
    with TMetaBlockUnderConstruction.Create(
      tidBlock_NurseDomeConstrD,
      350000,
      [100, 0, 0],
      10,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_NurseDomeD,
      pkMiddle,
      99,
      TPopulatedBlock ) do
      begin
        Beauty := 30;
      //BeautyStrength := 0.1;
        Efficiency := 0.57;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_NurseDomeD, 'Nurse Fraternity', vidFacility_NurseDomeD, TFacility ) do
      begin
        xSize := 3;
        ySize := 3;
        Level := 120;
        FacId := FID_midClass;
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_NurseDomeConstrD])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_NurseDomeD])));
        ClusterName := tidClusterName_Moab;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        FacilityKind := tidFacilityKind_MoabResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // Beings dome A
    with TMetaBlockUnderConstruction.Create(
      tidBlock_BeingsDomeConstrA,
      3000000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create( tidBlock_BeingsDomeA, pkLow, 200, TPopulatedBlock ) do
      begin
        Beauty := 20;
      //BeautyStrength := 0.1;
        VisualStages := 2;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_BeingsDomeA, 'Guruh Dome', vidFacility_BeingsDomeA, TFacility ) do
      begin
        XSize := 3;
        YSize := 3;
        Level := 120;
        FacId := FID_lowClass;
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_BeingsDomeConstrA])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Dome Completed', 'Dome is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_BeingsDomeA])));
        ClusterName  := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // Beings dome B
    with TMetaBlockUnderConstruction.Create(
      tidBlock_BeingsDomeConstrB,
      2550000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create( tidBlock_BeingsDomeB, pkLow, 170, TPopulatedBlock ) do
      begin
        Beauty := 20;
      //BeautyStrength := 0.1;
        VisualStages := 2;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_BeingsDomeB, 'Lothar Dome', vidFacility_BeingsDomeB, TFacility ) do
      begin
        XSize := 3;
        YSize := 3;
        Level := 120;
        FacId := FID_lowClass;
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_BeingsDomeConstrB])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Dome Completed', 'Dome is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_BeingsDomeB])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // Beings dome C
    with TMetaBlockUnderConstruction.Create(
      tidBlock_BeingsDomeConstrC,
      4000000,
      [100, 0, 0],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create( tidBlock_BeingsDomeC, pkLow, 300, TPopulatedBlock ) do
      begin
        Beauty := 20;
      //BeautyStrength := 0.1;
        VisualStages := 2;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_BeingsDomeC, 'Black Bowl', vidFacility_BeingsDomeC, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        FacId := FID_lowClass;
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_BeingsDomeConstrC])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Dome Completed', 'Dome is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_BeingsDomeC])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

    // Beings dome D
    with TMetaBlockUnderConstruction.Create(
      tidBlock_BeingsDomeConstrD,
      5000000,
      [100, 0, 0],
      40,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create( tidBlock_BeingsDomeD, pkLow, 350, TPopulatedBlock ) do
      begin
        Beauty := 20;
      //BeautyStrength := 0.1;
        VisualStages := 2;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_BeingsDomeD, 'Souls Dome', vidFacility_BeingsDomeD, TFacility ) do
      begin
        XSize := 4;
        YSize := 4;
        Level := 120;
        FacId := FID_lowClass;
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_BeingsDomeConstrD])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Dome Completed', 'Dome is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_BeingsDomeD])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabResidentials;
        TechnologyKind := tidInventionKind_OfficeAndResidentials;
        MinistryId := nidMinistry_Housing;
        Register( tidClassFamily_Facilities );
      end;

  end;

procedure RegisterPublicFacilities;
  begin
    // Big correctional
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabBigCorrectionalConstr,
      12500000,
      [90, 5, 5],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPublicFacility.Create(
      tidBlock_MoabBigCorrectional,
      [2, 30, 80],
      [PFInfoDef(tidPublicFac_Police, 1250), // 1250
       PFInfoDef(tidPublicFac_Health, 10000),
       PFInfoDef(tidPublicFac_Fire, 2500),   // 2500
       PFInfoDef(tidPublicFac_School, 5000)], // 2,500,000
      TPublicFacility ) do
      begin
        MaintCost := 1000; // >>>
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MoabBigCorrectional, 'Moab Correctional', vidFacility_MoabBigCorrectional, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        SlotCount := 0;
        FacId := FID_Hospital; // >>>>>>>
        DemoLapse := 1;
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MoabBigCorrectionalConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Dome Completed', 'Dome is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MoabBigCorrectional])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabCorrectionals;
        TechnologyKind := tidInventionKind_PublicFacilities;
        MinistryId := nidMinistry_Defense;
        Register( tidClassFamily_Facilities );
      end;

    // Small park
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabSmallParkConstr,
      1000000,
      [100, 0, 0],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaEnvironmentalBlock.Create(
      tidBlock_MoabSmallPark,
      TEnvironmentalBlock ) do
      begin
        RegisterModifier( tidEnvironment_Beauty, 300, 10 );
        RegisterModifier( tidEnvironment_Pollution, -300, 10 );
        MaintCost := 100;
        DissabledStop := true;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MoabSmallPark, 'Small Park', vidFacility_MoabSmallPark, TFacility ) do
      begin
        xSize := 3;
        ySize := 3;
        Level := 200;
        FacId := FID_Park;
        DemoLapse := 1;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MoabSmallParkConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Park Completed', 'Park is done', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MoabSmallPark])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabCorrectionals;
        TechnologyKind := tidInventionKind_PublicFacilities;
        Register( tidClassFamily_Facilities );
      end;

    // Medium park
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabMediumParkConstr,
      2000000,
      [100, 0, 0],
      22,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaEnvironmentalBlock.Create(
      tidBlock_MoabMediumPark,
      TEnvironmentalBlock ) do
      begin
        RegisterModifier( tidEnvironment_Beauty, 400, 10 );
        RegisterModifier( tidEnvironment_Pollution, -400, 10 );
        MaintCost := 250;
        DissabledStop := true;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MoabMediumPark, 'Park', vidFacility_MoabMediumPark, TFacility ) do
      begin
        XSize := 5;
        YSize := 5;
        Level := 200;
        FacId := FID_Park;
        DemoLapse := 1;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MoabMediumParkConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Park Completed', 'Building is done', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MoabMediumPark])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabCorrectionals;
        TechnologyKind := tidInventionKind_PublicFacilities;
        Register( tidClassFamily_Facilities );
      end;

    // Central park
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabCentralParkConstr,
      3000000,
      [100, 0, 0],
      22,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaEnvironmentalBlock.Create(
      tidBlock_MoabCentralPark,
      TEnvironmentalBlock ) do
      begin
        RegisterModifier( tidEnvironment_Beauty, 500, 10 );
        RegisterModifier( tidEnvironment_Pollution, -500, 10 );
        MaintCost := 400;
        DissabledStop := true;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MoabCentralPark, 'Big Park', vidFacility_MoabCentralPark, TFacility ) do
      begin
        XSize := 7;
        YSize := 7;
        Level := 200;
        FacId := FID_Park;
        DemoLapse := 1;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MoabCentralParkConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Park Completed', 'Building is done', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MoabCentralPark])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabCorrectionals;
        TechnologyKind := tidInventionKind_PublicFacilities;
        Register( tidClassFamily_Facilities );
      end;

    // Statue of Liberty
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabLibertyConstr,
      costLiberty,
      [100, 0, 0],
      50,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaEnvironmentalBlock.Create(
      tidBlock_MoabLiberty,
      TEnvironmentalBlock ) do
      begin
        Prestige := prestLiberty;
        RegisterModifier( tidEnvironment_Beauty, 500, 1 );
        MinColDist := 5;
        ColIsSameComp := false;
        MaintCost := 1000;
        DissabledStop := true;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MoabLiberty, 'Statue of Liberty', vidFacility_MoabLiberty, TFacility ) do
      begin
        XSize := 4;
        YSize := 4;
        Level := 200;
        FacId := FID_LuxuryFac;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MoabLibertyConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Park Completed', 'Building is done', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MoabLiberty])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabCorrectionals;
        TechnologyKind := tidInventionKind_Monuments;
        Register( tidClassFamily_Facilities );
      end;

    // IFEL Tower
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabTowerConstr,
      costIFELTower,
      [100, 0, 0],
      50,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaEnvironmentalBlock.Create(
      tidBlock_MoabTower,
      TEnvironmentalBlock ) do
      begin
        Prestige := prestIFELTower;
        RegisterModifier( tidEnvironment_Beauty, 500, 1 );
        MinColDist := 5;
        ColIsSameComp := false;
        MaintCost := 1000;
        DissabledStop := true;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MoabTower, 'IFEL Tower', vidFacility_MoabTower, TFacility ) do
      begin
        XSize := 4;
        YSize := 4;
        Level := 200;
        FacId := FID_LuxuryFac;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MoabTowerConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Park Completed', 'Building is done', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MoabTower])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabCorrectionals;
        TechnologyKind := tidInventionKind_Monuments;
        Register( tidClassFamily_Facilities );
      end;

    // Common Public services
    CopyCommonFacilities( tidBlock_SRVCOMMON, tidClusterName_Moab, tidFacilityKind_MoabCorrectionals, tidInventionKind_PublicFacilities );
    // Common Special
    CopyCommonFacilities( tidBlock_SPECIALCOMMON, tidClusterName_Moab, tidFacilityKind_MoabSpecial, '' );
    // Common Warehouses
    CopyCommonFacilities(tidBlock_WHCOMMON, tidClusterName_Moab, tidFacilityKind_MoabIndustrial, '');
  end;

procedure RegisterIndustries;
  begin
    // Farm
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabFarmConstr,
      cost_Farm,
      [80, 15, 5],
      25,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFarmBlock.Create(
      tidBlock_MoabFarm,
      [input_Farm_ExecutivesWorkForce, input_Farm_ProfessionalWorkForce, input_Farm_Workers],
      input_Farm_Chemicals,
      input_Farm_LegalServices,
      input_Farm_ComputerServices,
      output_Farm_FreshFood,
      output_Farm_OrganicMaterials,
      budget_Farm,
      TFarmBlock ) do
      begin
        VisualStages := 2;
        Beauty := -40;
      //BeautyStrength := 0.1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MoabFarm, 'Food Crater', vidFacility_MoabFarm, TFacility ) do
      begin
        xSize := 4;
        ySize := 4;
        Level := 20;
        FacId := FID_Farm;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabFarmConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The crater is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabFarm])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabIndustrial;
        TechnologyKind := tidLicence_Farms;
        Register( tidClassFamily_Facilities );
      end;

    // Chemical dome
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabChemicalConstr,
      cost_ChemicalPlant,
      [50, 40, 10],
      45,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaChemicalBlock.Create(
      tidBlock_MoabChemical,
      [input_ChemicalPlant_ExecutivesWorkForce, input_ChemicalPlant_ProfessionalWorkForce, input_ChemicalPlant_Workers],
      input_ChemicalPlant_LegalServices,
      input_ChemicalPlant_ComputerServices,
      input_ChemicalPlant_Ore,
      output_ChemicalPlant_Chemicals,
      budget_ChemicalPlant,
      TChemicalBlock ) do
      begin
        Beauty := -70;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MoabChemical, 'Chemicals Dome', vidFacility_MoabChemical, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 15;
        FacId := FID_Chemical;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabChemicalConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The dome is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabChemical])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabIndustrial;
        TechnologyKind := tidLicence_Chemical;
        Register( tidClassFamily_Facilities );
      end;

    // Mine
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabMineConstr,
      cost_Mine,
      [45, 40, 15],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaMineBlock.Create(
      tidBlock_MoabMine,
      [input_Mine_ExecutivesWorkForce, input_Mine_ProfessionalWorkForce, input_Mine_Workers],
      input_Mine_LegalServices,
      input_Mine_ComputerServices,
      input_Mine_Chemicals,
      output_Mine_Ore,
      budget_Mine,
      TMineBlock ) do
      begin
        VisualStages := 2;
        Beauty := -100;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MoabMine, 'Mine', vidFacility_MoabMine, TFacility ) do
      begin
        xSize := 6;
        ySize := 6;
        Level := 4;
        FacId := FID_Mine;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabMineConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Mine is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabMine])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabIndustrial;
        TechnologyKind := tidLicence_Mines;
        Register( tidClassFamily_Facilities );
      end;

    // Chemical Mine
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabChemMineConstr,
      cost_Mine,
      [45, 40, 15],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaChemMineBlock.Create(
      tidBlock_MoabChemMine,
      [input_Mine_ExecutivesWorkForce, input_Mine_ProfessionalWorkForce, input_Mine_Workers],
      input_Mine_LegalServices,
      input_Mine_ComputerServices,
      input_Mine_Chemicals,
      output_Mine_Ore,
      budget_Mine,
      TChemMineBlock ) do
      begin
        VisualStages := 2;
        Beauty := -100;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MoabChemMine, 'Chemical Mine', vidFacility_MoabChemMine, TFacility ) do
      begin
        xSize := 6;
        ySize := 6;
        Level := 4;
        FacId := FID_ChemicalMine;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabChemMineConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Mine is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabChemMine])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabIndustrial;
        TechnologyKind := tidLicence_ChemMines;
        Register( tidClassFamily_Facilities );
      end;

    // Silicon Mine
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabSiliconMineConstr,
      cost_Mine,
      [45, 40, 15],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaSiliconMineBlock.Create(
      tidBlock_MoabSiliconMine,
      [input_Mine_ExecutivesWorkForce, input_Mine_ProfessionalWorkForce, input_Mine_Workers],
      input_Mine_LegalServices,
      input_Mine_ComputerServices,
      input_Mine_Chemicals,
      output_Mine_Ore,
      budget_Mine,
      TSiliconMineBlock ) do
      begin
        VisualStages := 2;
        Beauty := -100;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MoabSiliconMine, 'Silicon Mine', vidFacility_MoabSiliconMine, TFacility ) do
      begin
        xSize := 6;
        ySize := 6;
        Level := 4;
        FacId := FID_SiliconMine;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabSiliconMineConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Mine is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabSiliconMine])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabIndustrial;
        TechnologyKind := tidLicence_SiliconMines;
        Register( tidClassFamily_Facilities );
      end;

    // Stone Mine
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabStoneMineConstr,
      cost_Mine,
      [45, 40, 15],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaStoneMineBlock.Create(
      tidBlock_MoabStoneMine,
      [input_Mine_ExecutivesWorkForce, input_Mine_ProfessionalWorkForce, input_Mine_Workers],
      input_Mine_LegalServices,
      input_Mine_ComputerServices,
      input_Mine_Chemicals,
      output_Mine_Ore,
      budget_Mine,
      TStoneMineBlock ) do
      begin
        VisualStages := 2;
        Beauty := -100;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MoabStoneMine, 'Stone Mine', vidFacility_MoabStoneMine, TFacility ) do
      begin
        xSize := 6;
        ySize := 6;
        Level := 4;
        FacId := FID_StoneMine;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabStoneMineConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Mine is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabStoneMine])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabIndustrial;
        TechnologyKind := tidLicence_StoneMines;
        Register( tidClassFamily_Facilities );
      end;

    // Coal Mine
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabCoalMineConstr,
      cost_Mine,
      [45, 40, 15],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaCoalMineBlock.Create(
      tidBlock_MoabCoalMine,
      [input_Mine_ExecutivesWorkForce, input_Mine_ProfessionalWorkForce, input_Mine_Workers],
      input_Mine_LegalServices,
      input_Mine_ComputerServices,
      input_Mine_Chemicals,
      output_Mine_Ore,
      budget_Mine,
      TCoalMineBlock ) do
      begin
        VisualStages := 2;
        Beauty := -100;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MoabCoalMine, 'Coal Mine', vidFacility_MoabCoalMine, TFacility ) do
      begin
        xSize := 6;
        ySize := 6;
        Level := 4;
        FacId := FID_CoalMine;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabCoalMineConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Mine is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabCoalMine])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabIndustrial;
        TechnologyKind := tidLicence_CoalMines;
        Register( tidClassFamily_Facilities );
      end;

    // OilRig
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabOilRigConstr,
      cost_OilRig,
      [45, 40, 15],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaOilRigBlock.Create(
      tidBlock_MoabOilRig,
      [input_OilRig_ExecutivesWorkForce, input_OilRig_ProfessionalWorkForce, input_OilRig_Workers],
      input_OilRig_LegalServices,
      input_OilRig_ComputerServices,
      input_OilRig_Chemicals,
      output_OilRig_Oil,
      budget_OilRig,
      TOilRigBlock ) do
      begin
        VisualStages := 2;
        Beauty := -100;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MoabOilRig, 'Oil Rig', vidFacility_MoabOilRig, TFacility ) do
      begin
        xSize := 4;
        ySize := 4;
        Level := 12;
        FacId := FID_OilRig;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabOilRigConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The OilRig is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabOilRig])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabIndustrial;
        TechnologyKind := tidLicence_OilRigs;
        Register( tidClassFamily_Facilities );
      end;

    // Food dome
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabFoodDomeConstr,
      cost_FoodProcessingPlant,
      [40, 40, 20],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFoodProcessorBlock.Create(
      tidBlock_MoabFoodDome,
      [input_FoodProcessingPlant_ExecutivesWorkForce, input_FoodProcessingPlant_ProfessionalWorkForce, input_FoodProcessingPlant_Workers],
      input_FoodProcessingPlant_FreshFood,
      input_FoodProcessingPlant_Chemicals,
      input_FoodProcessingPlant_LegalServices,
      input_FoodProcessingPlant_ComputerServices,
      output_FoodProcessingPlant_ProcessedFood,
      budget_FoodProcessingPlant,
      TFoodProcessorBlock ) do
      begin
        VisualStages := 2;
        Beauty := -100;
      //BeautyStrength := 1;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MoabFoodDome, 'Food Dome', vidFacility_MoabFoodDome, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 30;
        FacId := FID_FoodProc;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabFoodDomeConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The dome is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabFoodDome])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabIndustrial;
        TechnologyKind := tidLicence_FoodProc;
        Register( tidClassFamily_Facilities );
      end;

    // Metal industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabMetalConstr,
      cost_Metalurgic,
      [40, 50, 10],
      45,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaMetalIndustryBlock.Create(
      tidBlock_MoabMetal,
      [input_Metalurgic_ExecutivesWorkForce, input_Metalurgic_ProfessionalWorkForce, input_Metalurgic_Workers],
      input_Metalurgic_Ore,
      input_Metalurgic_Chemicals,
      input_Metalurgic_LegalServices,
      input_Metalurgic_ComputerServices,
      output_Metalurgic_Metal,
      budget_Metalurgic,
      TMetalIndustryBlock ) do
      begin
        Beauty := -120;
      //BeautyStrength := 1;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MoabMetal, 'Metals Dome', vidFacility_MoabMetal, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 30;
        FacId := FID_Metal;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabMetalConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The dome is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabMetal])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabIndustrial;
        TechnologyKind := tidLicence_Metallurgy;
        Register( tidClassFamily_Facilities );
      end;

    // Refinery industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabRefineryConstr,
      cost_Refinery,
      [40, 50, 10],
      45,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaRefineryBlock.Create(
      tidBlock_MoabRefinery,
      [input_Refinery_ExecutivesWorkForce, input_Refinery_ProfessionalWorkForce, input_Refinery_Workers],
      input_Refinery_Oil,
      input_Refinery_Chemicals,
      input_Refinery_LegalServices,
      input_Refinery_ComputerServices,
      output_Refinery_Gasoline,
      budget_Refinery,
      TRefineryBlock ) do
      begin
        Beauty := -120;
        VisualStages := 2;
      //BeautyStrength := 1;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MoabRefinery, 'Refinery Dome', vidFacility_MoabRefinery, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 30;
        FacId := FID_Refinery;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabRefineryConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The dome is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabRefinery])));
        ClusterName := tidClusterName_Moab;                                
        FacilityKind := tidFacilityKind_MoabIndustrial;
        TechnologyKind := tidLicence_Refinery;
        Register( tidClassFamily_Facilities );
      end;

    // Textile industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabTextileConstr,
      cost_Textile,
      [45, 40, 15],
      40,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaTextilIndustryBlock.Create(
      tidBlock_MoabTextile,
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
    with TMetaFacility.Create( tidFacility_MoabTextile, 'Dome of Threads', vidFacility_MoabTextile, TFacility ) do
      begin
        xSize := 4;
        ySize := 4;
        Level := 20;
        FacId := FID_Textile;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabTextileConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The dome is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabTextile])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabIndustrial;
        TechnologyKind := tidLicence_Textile;
        Register( tidClassFamily_Facilities );
      end;

    // Clothings industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabClothingsConstr,
      cost_Clothing,
      [50, 40, 10],
      25,
      TBlockUnderConstruction) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaClothingsIndustryBlock.Create(
      tidBlock_MoabClothings,
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
    with TMetaFacility.Create( tidFacility_MoabClothings, 'Dome of Clothes', vidFacility_MoabClothings, TFacility ) do
      begin
        xSize := 4;
        ySize := 4;
        Level := 40;
        FacId := FID_Clothes;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabClothingsConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabClothings])));
        ClusterName    := tidClusterName_Moab;
        FacilityKind   := tidFacilityKind_MoabIndustrial;
        TechnologyKind := tidLicence_Clothing;
        Register( tidClassFamily_Facilities );
      end;

    // Construction industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabConstructionConstr,
      cost_Construction,
      [45, 40, 15],
      40,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaConstructionIndustryBlock.Create(
      tidBlock_MoabConstruction,
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
        Beauty := -100;
      //BeautyStrength := 0.5;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MoabConstruction, 'Construction Dome', vidFacility_MoabConstruction, TFacility ) do
      begin
        xSize := 4;
        ySize := 4;
        Level := 20;
        FacId := FID_Construction;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabConstructionConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The dome is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabConstruction])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabIndustrial;
        TechnologyKind := tidLicence_Construction;
        Register( tidClassFamily_Facilities );
      end;

    // Electronic industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabElectronicConstr,
      cost_ElectronicIndustry,
      [40, 40, 20],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaElectronicIndustryBlock.Create(
      tidBlock_MoabElectronic,
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
    with TMetaFacility.Create( tidFacility_MoabElectronic, 'Dome of Chips', vidFacility_MoabElectronic, TFacility ) do
      begin
        xSize := 4;
        ySize := 4;
        Level := 20;
        FacId := FID_ElectComp;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires Electronic materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabElectronicConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The dome is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabElectronic])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabIndustrial;
        TechnologyKind := tidLicence_Electronics;
        Register( tidClassFamily_Facilities );
      end;

    // Heavy industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabHeavyConstr,
      cost_HeavyIndustry,
      [45, 40, 15],
      45,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaHeavyIndustryBlock.Create(
      tidBlock_MoabHeavy,
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
        Beauty := -100;
      //BeautyStrength := 0.5;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MoabHeavy, 'Machinery Dome', vidFacility_MoabHeavy, TFacility ) do
      begin
        xSize := 4;
        ySize := 4;
        Level := 20;
        FacId := FID_Heavy;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires Heavy materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabHeavyConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The dome is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabHeavy])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabIndustrial;
        TechnologyKind := tidLicence_Heavy;
        Register( tidClassFamily_Facilities );
      end;

    // Car industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabCarIndustryConstr,
      cost_CarIndustry,
      [40, 45, 15],
      45,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaCarIndustryBlock.Create(
      tidBlock_MoabCarIndustry,
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
    with TMetaFacility.Create( tidFacility_MoabCarIndustry, 'Cars Dome', vidFacility_MoabCarIndustry, TFacility ) do
      begin
        xSize := 6;
        ySize := 6;
        Level := 40;
        FacId := FID_Car;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires Heavy materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabCarIndustryConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The dome is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabCarIndustry])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabIndustrial;
        TechnologyKind := tidLicence_Cars;
        Register( tidClassFamily_Facilities );
      end;

    // HHA industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabHHAIndustryConstr,
      cost_HouseHoldingAppliances,
      [45, 40, 15],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaHouseHoldingAppliancesBlock.Create(
      tidBlock_MoabHHAIndustry,
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
    with TMetaFacility.Create( tidFacility_MoabHHAIndustry, 'Household Appliances Dome', vidFacility_MoabHHAIndustry, TFacility ) do
      begin
        xSize := 6;
        ySize := 6;
        Level := 40;
        FacId := FID_Household;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires Heavy materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabHHAIndustryConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The dome is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabHHAIndustry])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabIndustrial;
        TechnologyKind := tidLicence_HHA;
        Register( tidClassFamily_Facilities );
      end;

    // BusinessMachines industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabBusinessMachineConstr,
      cost_BusinessMachines,
      [40, 40, 20],
      40,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaBusinessMachinesBlock.Create(
      tidBlock_MoabBusinessMachine,
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
    with TMetaFacility.Create( tidFacility_MoabBusinessMachine, 'Business Machines Dome', vidFacility_MoabBusinessMachine, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 40;
        FacId := FID_BusMach;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires Heavy materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabBusinessMachineConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The dome is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabBusinessMachine])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabIndustrial;
        TechnologyKind := tidLicence_BMIndustry;
        Register( tidClassFamily_Facilities );
      end;

  end;

procedure RegisterStores;
  begin
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabFoodStoreConstr,
      60000,
      [90, 0, 10],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFoodStoreBlock.Create(
      tidBlock_MoabFoodStore,
      [0, 0, 13],
      25, // people buying per hour
      25, // people buying per hour
      FairPrice,
      [0.5, 0.5, 0.7],
      120,
      TFoodStoreBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MoabFoodStore, 'Food Store', vidFacility_MoabFoodStore, TFacility ) do
      begin
        xSize := 1;
        ySize := 1;
        Level := 1000;
        FacId := FID_FoodStore;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabFoodStoreConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The market is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabFoodStore])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabIllusions;
        TechnologyKind := tidInventionKind_ServiceFacilities;
        DepOnTech := false; // make available even without HQs
        MinistryId := nidMinistry_Agriculture;
        Register( tidClassFamily_Facilities );
      end;

    // Clothes store
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabClothesStoreConstr,
      70000,
      [90, 0, 10],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaClothesShopBlock.Create(
      tidBlock_MoabClothesStore,
      [0, 0, 13],
      10,
      FairPrice,
      [1, 1, 1],
      130,
      TClothesShopBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MoabClothesStore, 'Clothes Store', vidFacility_MoabClothesStore, TFacility ) do
      begin
        xSize := 1;
        ySize := 1;
        Level := 1000;
        FacId := FID_ClotheStore;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabClothesStoreConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Shop is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabClothesStore])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabIllusions;
        TechnologyKind := tidInventionKind_ServiceFacilities;
        DepOnTech := false; // make available even without HQs
        Register( tidClassFamily_Facilities );
      end;

    // HHA store
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabHHAStoreConstr,
      70000,
      [90, 0, 10],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaHHAStoreBlock.Create(
      tidBlock_MoabHHAStore,
      [0, 1, 13],
      5,
      FairPrice,
      [0.8, 0.8, 0.9],
      130,
      THHAStoreBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MoabHHAStore, 'Appliances Store', vidFacility_MoabHHAStore, TFacility ) do
      begin
        xSize := 1;
        ySize := 1;
        Level := 1000;
        FacId := FID_HHAStore;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabHHAStoreConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Shop is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabHHAStore])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabIllusions;
        TechnologyKind := tidInventionKind_ServiceFacilities;
        DepOnTech := false; // make available even without HQs
        Register( tidClassFamily_Facilities );
      end;

    // Car Store
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabCarStoreConstr,
      600000,
      [90, 0, 10],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaCarShopBlock.Create(
      tidBlock_MoabCarStore,
      [0, 2, 13],
      1/(7*24),
      1/(2*7*24),
      1,
      FairPrice,
      [0.8, 0.8, 0.8],
      150,
      TCarShopBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MoabCarStore, 'Car Store', vidFacility_MoabCarStore, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 20;
        FacId := FID_CarStore;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabCarStoreConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Shop is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabCarStore])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabIllusions;
        TechnologyKind := tidInventionKind_ServiceFacilities;
        DepOnTech := false; // make available even without HQs
        Register( tidClassFamily_Facilities );
      end;

    // Gas Station
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabGasStationConstr,
      200000,
      [90, 0, 10],
      10,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaGasStationBlock.Create(
      tidBlock_MoabGasStation,
      [0, 1, 5],
      20,
      FairPrice,
      [1, 1, 1],
      130,
      TGasStationBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MoabGasStation, 'Gas Station', vidFacility_MoabGasStation, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 20;
        FacId := FID_GasStation;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabGasStationConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Shop is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabGasStation])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabIllusions;
        TechnologyKind := tidInventionKind_ServiceFacilities;
        DepOnTech := false; // make available even without HQs
        Register( tidClassFamily_Facilities );
      end;

    // Supermarket
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabSupermarketConstr,
      1000000,
      [80, 0, 20],
      35,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaSuperMarketBlock.Create(
      tidBlock_MoabSupermarket,
      [0, 3, 21],
      2/(24*7),
      1/(24*7),
      20,
      20,
      10,
      5,
      FairPrice,
      [0.9, 0.9, 1],
      200,
      TSuperMarketBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MoabSupermarket, 'Moab Supermarket', vidFacility_MoabSupermarket, TFacility ) do
      begin
        xSize := 3;
        ySize := 3;
        Level := 20;
        FacId := FID_Supermarket;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabSupermarketConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Supermarket is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabSupermarket])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabIllusions;
        TechnologyKind := tidInventionKind_Supermarkets;
        Register( tidClassFamily_Facilities );
      end;

    // Dome of Joy
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabBarConstr,
      150000,
      [90, 0, 10],
      15,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaBarBlock.Create(
      tidBlock_MoabBar,
      [0, 0, 13],
      10,
      FairPrice,
      [0.7, 0.8, 0.8],
      100,
      TBarBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MoabBar, 'Dome of Joy', vidFacility_MoabBar, TFacility ) do
      begin
        xSize := 1;
        ySize := 1;
        Level := 1000;
        FacId := FID_Bar;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabBarConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Bar is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabBar])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabIllusions;
        TechnologyKind := tidInventionKind_Bars;
        Register( tidClassFamily_Facilities );
      end;

    {// Funerals
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabFuneralConstr,
      1000000,
      [90, 0, 10],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFuneralBlock.Create(
      tidBlock_MoabFuneral,
      [0, 1, 10],
      10,
      FairPrice,
      [0.8, 0.8, 0.8],
      100,
      TFuneralBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MoabFuneral, 'Funeral Parlor', vidFacility_MoabFuneral, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 1000;
        FacId := FID_Funeral;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabFuneralConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Funeral is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabFuneral])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabIllusions;
        //TechnologyKind := tidInventionKind_Funerals;
        Register( tidClassFamily_Facilities );
      end;}

    // Dinner Dome
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabRestaurantConstr,
      350000,
      [90, 0, 10],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaRestaurantBlock.Create(
      tidBlock_MoabRestaurant,
      [0, 1, 21],
      10,
      FairPrice,
      [0.7, 0.8, 0.8],
      150,
      TRestaurantBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MoabRestaurant, 'Dinner Dome', vidFacility_MoabRestaurant, TFacility ) do
      begin
        xSize := 1;
        ySize := 1;
        Level := 1000;
        FacId := FID_Restaurant;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabRestaurantConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Restaurant is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabRestaurant])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabIllusions;
        TechnologyKind := tidInventionKind_Restaurants;
        Register( tidClassFamily_Facilities );
      end;

    // Movie
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabMovieConstr,
      1500000,
      [100, 0, 0],
      15,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaMovieBlock.Create(
      tidBlock_MoabMovie,
      [0, 0, 8],
      100,
      FairPrice,
      [0.5, 0.5, 0.7],
      100,
      TMovieBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MoabMovie, 'Movie Theater', vidFacility_MoabMovie, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 1000;
        FacId := FID_Movie;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabMovieConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Movie is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabMovie])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabIllusions;
        TechnologyKind := tidInventionKind_MovieTheaters;
        Register( tidClassFamily_Facilities );
      end;

  end;

procedure RegisterHeadquarters;
  begin
    // General
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabGeneralHeadquarterConstr,
      1000000,
      [60, 0, 40],
      10,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaMainHeadquarter.Create( tidBlock_MoabGeneralHeadquarter,
      [77, 33, 12],
      tidInventionKind_Direction,
      //nidInvention_DistributedDirection,
      TMainHeadquarter ) do
      begin
        //RegisterInvention(nidInvention_TV);
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MoabGeneralHeadquarter, 'Moab Cathedral', vidFacility_MoabGeneralHeadquarter, TFacility ) do
      begin
        Desc  := '';
        XSize := 3;
        YSize := 3;
        Level := 5000;
        FacId := FID_MainHeadquarter;
        Options := Options - [mfcGenerateName, mfcInTown];
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabGeneralHeadquarterConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The facility is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidFacility_MoabGeneralHeadquarter])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabSacred;
        Register( tidClassFamily_Facilities );
      end;

    // General Standalone
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabGeneralHeadquarterConstr + 'STA',
      4000000,
      [60, 0, 40],
      10,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaMainHeadquarter.Create(
      tidBlock_MoabGeneralHeadquarter + 'STA',
      [15, 7, 1],
      tidInventionKind_Direction,
      TMainHeadquarter ) do
      begin
        Beauty     := 100;
        Standalone := true;
        VisualStages := 5;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MoabGeneralHeadquarter + 'STA', 'Company Headquarters', 751, TFacility ) do
      begin
        xSize := 3;
        ySize := 3;
        Level := 5000;
        FacId := FID_MainHeadquarter;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabGeneralHeadquarterConstr + 'STA'])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The facility is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidFacility_MoabGeneralHeadquarter + 'STA'])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabSacred;
        UniquenessMask := $00000001;
        Register( tidClassFamily_Facilities );
      end;

    // Tower of Industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabIndHeadquarterConstr,
      730000,
      [60, 0, 40],
      10,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaHeadquarterBlock.Create(
      tidBlock_MoabIndHeadquarter,
      [12, 33, 33],
      tidInventionKind_IndustrialFacilities,
      //nidInvention_IndustrialFacilities,
      THeadquarterBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MoabIndHeadquarter, 'Industries Tower', vidFacility_MoabIndHeadquarter, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 5000;
        FacId := FID_IndHeadquarter;
        Options := Options - [mfcGenerateName, mfcInTown];
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabIndHeadquarterConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The facility is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidFacility_MoabIndHeadquarter])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabSacred;
        TechnologyKind := tidInventionKind_Direction;
        Register( tidClassFamily_Facilities );
      end;

    // Tower of Illusions
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabIllusionHeadquarterConstr,
      200000,
      [60, 0, 40],
      10,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaHeadquarterBlock.Create(
      tidBlock_MoabIllusionHeadquarter,
      [33, 7, 12],
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
    with TMetaFacility.Create( tidFacility_MoabIllusionHeadquarter, 'Commerce Tower', vidFacility_MoabIllusionHeadquarter, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 5000;
        FacId := FID_CommHeadquarter;
        Options := Options - [mfcGenerateName, mfcInTown];
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabIllusionHeadquarterConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The facility is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidFacility_MoabIllusionHeadquarter])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabSacred;
        TechnologyKind := tidInventionKind_Direction;
        Register( tidClassFamily_Facilities );
      end;

    // Tower of land lording
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabResHeadquarterConstr,
      200000,
      [60, 0, 40],
      10,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaHeadquarterBlock.Create(
      tidBlock_MoabResHeadquarter,
      [33, 7, 12],
      tidInventionKind_OfficeAndResidentials,
      //nidInvention_OfficeAndResidentials,
      THeadquarterBlock ) do
      begin
        //RegisterInvention(nidInvention_OfficeBuildings);
        //RegisterInvention(nidInvention_LegalServices);
        //RegisterInvention(nidInvention_SoftwareFirms);
        //RegisterInvention(nidInvention_Banking);
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MoabResHeadquarter, 'Real Estates Tower', vidFacility_MoabResHeadquarter, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 5000;
        FacId := FID_OffcHeadquarter;
        Options := Options - [mfcGenerateName, mfcInTown];
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabResHeadquarterConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The facility is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidFacility_MoabResHeadquarter])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabSacred;
        TechnologyKind := tidInventionKind_Direction;
        Register( tidClassFamily_Facilities );
      end;

    // Tower of Correction
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabCorrectionHeadquarterConstr,
      200000,
      [60, 0, 40],
      10,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaPublicAffairsHeadquarter.Create(
      tidBlock_MoabCorrectionHeadquarter,
      [33, 7, 12],
      tidInventionKind_PublicFacilities,
      //nidInvention_PublicFacilities,
      maxHQAdv,
      TPublicAffairsHeadquarter ) do
      begin
        //RegisterInvention( nidInvention_Monuments );
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MoabCorrectionHeadquarter, 'Civic Affairs Tower', vidFacility_MoabCorrectionHeadquarter, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 5000;
        FacId := FID_PubHeadquarter;
        Options := Options - [mfcGenerateName, mfcInTown];
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabCorrectionHeadquarterConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The facility is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidFacility_MoabCorrectionHeadquarter])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabSacred;
        TechnologyKind := tidInventionKind_Direction;
        Register( tidClassFamily_Facilities );
      end;
  end;

procedure RegisterSpecialFacilities;
  begin
    // Computing Industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabComputingIndustryConstr,
      cost_ComputingIndustry,
      [70, 0, 30],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaSoftwareBlock.Create(
      tidBlock_MoabComputingIndustry,
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
    with TMetaFacility.Create( tidFacility_MoabComputingIndustry, 'Software Firm', vidFacility_MoabComputingIndustry, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 40;
        FacId := FID_SoftwareFirm;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabComputingIndustryConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Computing Industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabComputingIndustry])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabBusinessFacilities;
        TechnologyKind := tidInventionKind_Software;
        Register( tidClassFamily_Facilities );
      end;

    // Legal Services
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabLegalServicesConstr,
      cost_LegalServices,
      [80, 0, 20],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaLegalServiceBlock.Create(
      tidBlock_MoabLegalServices,
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
    with TMetaFacility.Create( tidFacility_MoabLegalServices, 'Lawyers Firm', vidFacility_MoabLegalServices, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 40;
        FacId := FID_LawyerFirm;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabLegalServicesConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Firm is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabLegalServices])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabBusinessFacilities;
        TechnologyKind := tidInventionKind_LegalServices;
        Register( tidClassFamily_Facilities );
      end;

    // TV Station
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabTVStationConstr,
      30000000,
      [70, 0, 30],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaBroadcaster.Create(
      tidBlock_MoabTVStation,
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
    with TMetaFacility.Create( tidFacility_MoabTVStation, 'TV', vidFacility_MoabTVStation, TFacility ) do
      begin
        xSize := 3;
        ySize := 3;
        Level := 60;
        FacId := FID_TVStation;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabTVStationConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Station is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabTVStation])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabSpecial;
        TechnologyKind := tidInventionKind_Television;
        Register( tidClassFamily_Facilities );
      end;

    // TV Antenna
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabTVAntennaConstr,
      500000,
      [90, 0, 10],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaAntenna.Create(
      tidBlock_MoabTVAntenna,
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
    with TMetaFacility.Create( tidFacility_MoabTVAntenna, 'TV Antenna', vidFacility_MoabTVAntenna, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 50;
        FacId := FID_TVAntena;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabTVAntennaConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Antena is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MoabTVAntenna])));
        ClusterName := tidClusterName_Moab;
        FacilityKind := tidFacilityKind_MoabSpecial;
        TechnologyKind := tidInventionKind_Television;
        Register( tidClassFamily_Facilities );
      end;

  end;

procedure RegisterOffices;
  begin
    // Office A
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabOfficeBuildingConstrA,
      10000000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaOfficeBlock.Create(
      tidBlock_MoabOfficeBuildingA,
      100,
      TOfficeBlock ) do
      begin
        Efficiency := 0.85;
        Beauty := 75;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MoabOfficeBuildingA, 'Moab Basilica', vidFacility_MoabOfficeBuildingA, TFacility ) do
      begin
        XSize := 3;
        YSize := 3;
        Level := 120;
        FacId := FID_Office;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MoabOfficeBuildingConstrA])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MoabOfficeBuildingA])));
        ClusterName := tidClusterName_Moab;
        TechnologyKind := tidInventionKind_Offices;
        FacilityKind := tidFacilityKind_MoabBusinessFacilities;
        MinistryId := nidMinistry_Commerce;
        Register( tidClassFamily_Facilities );
      end;

    // Office B
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MoabOfficeBuildingConstrB,
      20000000,
      [100, 0, 0],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaOfficeBlock.Create(
      tidBlock_MoabOfficeBuildingB,
      130,
      TOfficeBlock ) do
      begin
        Efficiency := 0.9;
        Beauty := 80;
      //BeautyStrength := 1;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MoabOfficeBuildingB, 'Dark Crysler', vidFacility_MoabOfficeBuildingB, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 120;
        FacId := FID_Office;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MoabOfficeBuildingConstrB])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MoabOfficeBuildingB])));
        ClusterName := tidClusterName_Moab;
        TechnologyKind := tidInventionKind_Offices;
        FacilityKind := tidFacilityKind_MoabBusinessFacilities;
        MinistryId := nidMinistry_Commerce;
        Register( tidClassFamily_Facilities );
      end;
  end;

procedure RegisterPackFacilities;
  begin
    RegisterHeadquarters;
    RegisterIndustries;
    RegisterStores;
    RegisterResidentials;
    RegisterPublicFacilities;
    RegisterSpecialFacilities;
    RegisterOffices;
  end;

function ModelExtensionId : string; export;
  begin
    result := 'MoabPack1';
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


