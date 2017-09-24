{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y-,Z1}
{$MINSTACKSIZE $00004000}
{$MAXSTACKSIZE $00100000}
{$IMAGEBASE $00400000}
{$APPTYPE GUI}
library MagnaPack1;

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
  MagnaConst in 'MagnaConst.pas',
  SimMLS in '..\..\Kernel\SimMLS.pas',
  MagnaMarket in '..\..\StdBlocks\MagnaMarket.pas',
  TycoonLevels in '..\..\Kernel\TycoonLevels.pas',
  CommonFacs in '..\CommonFacs.pas',
  MovieStudios in '..\..\StdBlocks\MovieStudios.pas';

procedure RegisterFacilityKinds;
  begin
    with TFacilityKind.Create( tidFacilityKind_MagnaSacred ) do
      begin
        Name        := 'Headquarters';
        SuperType   := tidSuperFacKind_Headquarter;
        ClusterName := tidClusterName_Magna;
        Role        := rolBuyer;
        Register( tidClassFamily_FacilityKinds );
      end;
{
    with TFacilityKind.Create( tidFacilityKind_MagnaIndustrial ) do
      begin
        Name        := 'Industries';
        SuperType   := tidSuperFacKind_Industry;
        ClusterName := tidClusterName_Magna;
        Technology  := tidInventionKind_IndustrialFacilities;
        Role        := rolProducer;
        Register( tidClassFamily_FacilityKinds );
      end;
    }
    with TFacilityKind.Create( tidFacilityKind_MagnaResidentials ) do
      begin
        Name        := 'Magna Estates';
        SuperType   := tidSuperFacKind_Residential;
        ClusterName := tidClusterName_Magna;
        Register( tidClassFamily_FacilityKinds );
      end;
    with TFacilityKind.Create( tidFacilityKind_MagnaCorrectionals ) do
      begin
        Name        := 'Public';
        SuperType   := tidSuperFacKind_Public;
        ClusterName := tidClusterName_Magna;
        Technology  := tidInventionKind_PublicFacilities;
        Register( tidClassFamily_FacilityKinds );
      end;
    with TFacilityKind.Create( tidFacilityKind_MagnaMarkets ) do
      begin
        Name        := 'Commerce';
        SuperType   := tidSuperFacKind_Service;
        ClusterName := tidClusterName_Magna;
        //Technology  := tidInventionKind_ServiceFacilities;
        Role        := rolBuyer;
        Register( tidClassFamily_FacilityKinds );
      end;
    {
    with TFacilityKind.Create( tidFacilityKind_MagnaBusinessFacilities ) do
      begin
        Name        := 'Offices';
        SuperType   := tidSuperFacKind_Business;
        ClusterName := tidClusterName_Magna;
        Technology  := tidInventionKind_OfficeAndResidentials;
        Register( tidClassFamily_FacilityKinds );
      end;
    }
    with TFacilityKind.Create( tidFacilityKind_MagnaSpecial ) do
      begin
        Name        := 'Special';
        SuperType   := tidSuperFacKind_Special;
        ClusterName := tidClusterName_Magna;
        Role        := rolProducer;
        Register( tidClassFamily_FacilityKinds );
      end;
  end;

type
  TMagnaCluster =
    class( TCluster )
      public
        function NameNewspaper( TownName : string ) : string; override;
    end;

  function TMagnaCluster.NameNewspaper( TownName : string ) : string;
    begin
      result := TownName + ' Herald';
    end;

procedure RegisterClusterFacilities;
  var
    Cluster : TMagnaCluster;
  begin
    Cluster := TMagnaCluster.Create( tidClusterName_Magna );
    Cluster.Tier := TTycoonLevel(TheClassStorage.ClassById[tidClassFamily_TycoonLevels, tidTycoonLevel_Paradigm]).Tier;
    Cluster.SpecialSeal := true;
    Cluster.Register( tidClassFamily_Clusters );
  end;

procedure RegisterResidentials;
  const
     MagnaEff      = 4; // 300%
     MagnaPrestige = 100;
  begin
    // Whirlpool
    with TMetaBlockUnderConstruction.Create(
      tidBlock_WhirlpoolConstr,
      50000,
      [80, 20, 0],
      100,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_Whirlpool,
      pkHigh,
      1500,
      TPopulatedBlock ) do
      begin
        Efficiency := MagnaEff;
        Beauty := 1000;
        CrimeResist := 1;
        PollResist  := 1;
        ModifyPrice := true;
        Prestige := MagnaPrestige;
        VisualStages := 2;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_Whirlpool, 'The Whirlpool', vidFacility_Whirlpool, TFacility ) do
      begin
        XSize := 3;
        YSize := 3;
        Level := 120;
        FacId := FID_hiClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_WhirlpoolConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_Whirlpool])));
        ClusterName := tidClusterName_Magna;
        FacilityKind := tidFacilityKind_MagnaResidentials;
        Register( tidClassFamily_Facilities );
      end;

    // Solarium
    with TMetaBlockUnderConstruction.Create(
      tidBlock_SolariumConstr,
      50000,
      [80, 20, 0],
      100,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_Solarium,
      pkHigh,
      1000,
      TPopulatedBlock ) do
      begin
        Efficiency := MagnaEff;
        Beauty := 1000;
        CrimeResist := 1;
        PollResist  := 1;
        ModifyPrice := true;
        Prestige := MagnaPrestige;
        VisualStages := 2;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_Solarium, 'Solarium', vidFacility_Solarium, TFacility ) do
      begin
        XSize := 3;
        YSize := 3;
        Level := 120;
        FacId := FID_hiClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_SolariumConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_Solarium])));
        ClusterName := tidClusterName_Magna;
        FacilityKind := tidFacilityKind_MagnaResidentials;
        Register( tidClassFamily_Facilities );
      end;

    {// Tulip
    with TMetaBlockUnderConstruction.Create(
      tidBlock_TulipConstr,
      40000000,
      [100, 0, 0],
      100,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_Tulip,
      pkHigh,
      1500,
      TPopulatedBlock ) do
      begin
        Efficiency := MagnaEff;
        Beauty := 1000;
        CrimeResist := 1;
        PollResist  := 1;
        ModifyPrice := true;
        Prestige := MagnaPrestige;
        VisualStages := 2;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_Tulip, 'Tulip', vidFacility_Tulip, TFacility ) do
      begin
        XSize := 3;
        YSize := 3;
        Level := 120;
        FacId := FID_hiClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_TulipConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_Tulip])));
        ClusterName := tidClusterName_Magna;
        FacilityKind := tidFacilityKind_MagnaResidentials;
        Register( tidClassFamily_Facilities );
      end;}

    // MayFlower
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MayFlowerConstr,
      50000,
      [80, 20, 0],
      100,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_MayFlower,
      pkMiddle,
      2000,
      TPopulatedBlock ) do
      begin
        Efficiency := MagnaEff;
        Beauty := 1000;
        CrimeResist := 1;
        PollResist  := 1;
        ModifyPrice := true;
        Prestige := MagnaPrestige;
        VisualStages := 2;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MayFlower, 'May Flower', vidFacility_MayFlower, TFacility ) do
      begin
        XSize := 3;
        YSize := 3;
        Level := 120;
        FacId := FID_midClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MayFlowerConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MayFlower])));
        ClusterName := tidClusterName_Magna;
        FacilityKind := tidFacilityKind_MagnaResidentials;
        Register( tidClassFamily_Facilities );
      end;

    // IvoryTower
    with TMetaBlockUnderConstruction.Create(
      tidBlock_IvoryTowerConstr,
      50000,
      [80, 20, 0],
      100,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_IvoryTower,
      pkMiddle,
      1700,
      TPopulatedBlock ) do
      begin
        Efficiency := MagnaEff;
        Beauty := 1000;
        CrimeResist := 1;
        PollResist  := 1;
        ModifyPrice := true;
        Prestige := MagnaPrestige;
        VisualStages := 2;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_IvoryTower, 'Ivory Tower', vidFacility_IvoryTower, TFacility ) do
      begin
        XSize := 3;
        YSize := 3;
        Level := 120;
        FacId := FID_midClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_IvoryTowerConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_IvoryTower])));
        ClusterName := tidClusterName_Magna;
        FacilityKind := tidFacilityKind_MagnaResidentials;
        Register( tidClassFamily_Facilities );
      end;

    // CloudCity
    with TMetaBlockUnderConstruction.Create(
      tidBlock_CloudCityConstr,
      50000,
      [80, 20, 0],
      100,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_CloudCity,
      pkMiddle,
      2200,
      TPopulatedBlock ) do
      begin
        Efficiency := MagnaEff;
        Beauty := 1000;
        CrimeResist := 1;
        PollResist  := 1;
        ModifyPrice := true;
        Prestige := MagnaPrestige;
        VisualStages := 2;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_CloudCity, 'Cloud City', vidFacility_CloudCity, TFacility ) do
      begin
        XSize := 3;
        YSize := 3;
        Level := 120;
        FacId := FID_midClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_CloudCityConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_CloudCity])));
        ClusterName := tidClusterName_Magna;
        FacilityKind := tidFacilityKind_MagnaResidentials;
        Register( tidClassFamily_Facilities );
      end;

    // SkyDome
    with TMetaBlockUnderConstruction.Create(
      tidBlock_SkyDomeConstr,
      50000000,
      [80, 20, 0],
      100,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_SkyDome,
      pkMiddle,
      2800,
      TPopulatedBlock ) do
      begin
        Efficiency := MagnaEff;
        Beauty := 1000;
        CrimeResist := 1;
        PollResist  := 1;
        ModifyPrice := true;
        Prestige := MagnaPrestige;
        VisualStages := 2;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_SkyDome, 'Sky Dome', vidFacility_SkyDome, TFacility ) do
      begin
        XSize := 3;
        YSize := 3;
        Level := 120;
        FacId := FID_midClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_SkyDomeConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_SkyDome])));
        ClusterName := tidClusterName_Magna;
        FacilityKind := tidFacilityKind_MagnaResidentials;
        Register( tidClassFamily_Facilities );
      end;

    // Heaven
    with TMetaBlockUnderConstruction.Create(
      tidBlock_HeavenConstr,
      50000,
      [80, 20, 0],
      100,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_Heaven,
      pkLow,
      2500,
      TPopulatedBlock ) do
      begin
        Efficiency := MagnaEff;
        Beauty := 1000;
        CrimeResist := 1;
        PollResist  := 1;
        ModifyPrice := true;
        Prestige := MagnaPrestige;
        VisualStages := 2;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_Heaven, 'Worker''s Heaven', vidFacility_Heaven, TFacility ) do
      begin
        XSize := 3;
        YSize := 3;
        Level := 120;
        FacId := FID_lowClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_HeavenConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_Heaven])));
        ClusterName := tidClusterName_Magna;
        FacilityKind := tidFacilityKind_MagnaResidentials;
        Register( tidClassFamily_Facilities );
      end;

    // Hive
    with TMetaBlockUnderConstruction.Create(
      tidBlock_HiveConstr,
      50000,
      [80, 20, 0],
      100,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_Hive,
      pkLow,
      4000,
      TPopulatedBlock ) do
      begin
        Efficiency := MagnaEff;
        Beauty := 1000;
        CrimeResist := 1;
        PollResist  := 1;
        ModifyPrice := true;
        Prestige := MagnaPrestige;
        VisualStages := 2;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_Hive, 'The Hive', vidFacility_Hive, TFacility ) do
      begin
        XSize := 3;
        YSize := 3;
        Level := 120;
        FacId := FID_lowClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_HiveConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_Hive])));
        ClusterName := tidClusterName_Magna;
        FacilityKind := tidFacilityKind_MagnaResidentials;
        Register( tidClassFamily_Facilities );
      end;

    // Octopus
    with TMetaBlockUnderConstruction.Create(
      tidBlock_OctopusConstr,
      50000,
      [80, 20, 0],
      100,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_Octopus,
      pkLow,
      3000,
      TPopulatedBlock ) do
      begin
        Efficiency := MagnaEff;
        Beauty := 1000;
        CrimeResist := 1;
        PollResist  := 1;
        ModifyPrice := true;
        Prestige := MagnaPrestige;
        VisualStages := 2;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_Octopus, 'The Octopus', vidFacility_Octopus, TFacility ) do
      begin
        XSize := 3;
        YSize := 3;
        Level := 120;
        FacId := FID_lowClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_OctopusConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_Octopus])));
        ClusterName := tidClusterName_Magna;
        FacilityKind := tidFacilityKind_MagnaResidentials;
        Register( tidClassFamily_Facilities );
      end;

    // TheSpring
    with TMetaBlockUnderConstruction.Create(
      tidBlock_TheSpringConstr,
      50000,
      [80, 20, 0],
      100,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaPopulatedBlock.Create(
      tidBlock_TheSpring,
      pkLow,
      3500,
      TPopulatedBlock ) do
      begin
        Efficiency := MagnaEff;
        Beauty := 1000;
        CrimeResist := 1;
        PollResist  := 1;
        ModifyPrice := true;
        Prestige := MagnaPrestige;
        VisualStages := 2;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_TheSpring, 'The Spring', vidFacility_TheSpring, TFacility ) do
      begin
        XSize := 3;
        YSize := 3;
        Level := 120;
        FacId := FID_lowClass;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_TheSpringConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_TheSpring])));
        ClusterName := tidClusterName_Magna;
        FacilityKind := tidFacilityKind_MagnaResidentials;
        Register( tidClassFamily_Facilities );
      end;

  end;

procedure RegisterPublicFacilities;
  begin
    // Small park
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MagnaSmallParkConstr,
      10000,
      [100, 0, 0],
      20,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaEnvironmentalBlock.Create(
      tidBlock_MagnaSmallPark,
      TEnvironmentalBlock ) do
      begin
        RegisterModifier( tidEnvironment_Beauty, 300, 10 );
        RegisterModifier( tidEnvironment_Pollution, -300, 10 );
        MaintCost := 100;
        DissabledStop := true;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MagnaSmallPark, 'Small Park', vidFacility_MagnaSmallPark, TFacility ) do
      begin
        xSize := 3;
        ySize := 3;
        Level := 200;
        FacId := FID_Park;
        DemoLapse := 1;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MagnaSmallParkConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Park Completed', 'Park is done', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MagnaSmallPark])));
        ClusterName := tidClusterName_Magna;
        FacilityKind := tidFacilityKind_MagnaCorrectionals;
        //TechnologyKind := tidInventionKind_PublicFacilities;
        Register( tidClassFamily_Facilities );
      end;

    // Medium park
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MagnaMediumParkConstr,
      20000,
      [100, 0, 0],
      22,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaEnvironmentalBlock.Create(
      tidBlock_MagnaMediumPark,
      TEnvironmentalBlock ) do
      begin
        RegisterModifier( tidEnvironment_Beauty, 400, 10 );
        RegisterModifier( tidEnvironment_Pollution, -400, 10 );
        MaintCost := 250;
        DissabledStop := true;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MagnaMediumPark, 'Park', vidFacility_MagnaMediumPark, TFacility ) do
      begin
        XSize := 5;
        YSize := 5;
        Level := 200;
        FacId := FID_Park;
        DemoLapse := 1;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MagnaMediumParkConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Park Completed', 'Building is done', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MagnaMediumPark])));
        ClusterName := tidClusterName_Magna;
        FacilityKind := tidFacilityKind_MagnaCorrectionals;
        //TechnologyKind := tidInventionKind_PublicFacilities;
        Register( tidClassFamily_Facilities );
      end;

    // Central park
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MagnaCentralParkConstr,
      20000,
      [100, 0, 0],
      22,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaEnvironmentalBlock.Create(
      tidBlock_MagnaCentralPark,
      TEnvironmentalBlock ) do
      begin
        RegisterModifier( tidEnvironment_Beauty, 500, 10 );
        RegisterModifier( tidEnvironment_Pollution, -500, 10 );
        MaintCost := 400;
        DissabledStop := true;
        Register( tidClassFamily_Blocks );
      end;
    with TMetaFacility.Create( tidFacility_MagnaCentralPark, 'Big Park', vidFacility_MagnaCentralPark, TFacility ) do
      begin
        XSize := 7;
        YSize := 7;
        Level := 200;
        FacId := FID_Park;
        DemoLapse := 1;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MagnaCentralParkConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Park Completed', 'Building is done', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MagnaCentralPark])));
        ClusterName := tidClusterName_Magna;
        FacilityKind := tidFacilityKind_MagnaCorrectionals;
        //TechnologyKind := tidInventionKind_PublicFacilities;
        Register( tidClassFamily_Facilities );
      end;

    // Statue of Liberty
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MagnaLibertyConstr,
      costLiberty,
      [100, 0, 0],
      50,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaEnvironmentalBlock.Create(
      tidBlock_MagnaLiberty,
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
    with TMetaFacility.Create( tidFacility_MagnaLiberty, 'Statue of Liberty', vidFacility_MagnaLiberty, TFacility ) do
      begin
        XSize := 4;
        YSize := 4;
        Level := 200;
        FacId := FID_LuxuryFac;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MagnaLibertyConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Park Completed', 'Building is done', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MagnaLiberty])));
        ClusterName := tidClusterName_Magna;
        FacilityKind := tidFacilityKind_MagnaCorrectionals;
        //TechnologyKind := tidInventionKind_Monuments;
        Register( tidClassFamily_Facilities );
      end;

    // IFEL Tower
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MagnaTowerConstr,
      costIFELTower,
      [100, 0, 0],
      50,
      TBlockUnderConstruction ) do
      begin
        Register( tidClassFamily_Blocks );
      end;
    with TMetaEnvironmentalBlock.Create(
      tidBlock_MagnaTower,
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
    with TMetaFacility.Create( tidFacility_MagnaTower, 'IFEL Tower', vidFacility_MagnaTower, TFacility ) do
      begin
        XSize := 4;
        YSize := 4;
        Level := 200;
        FacId := FID_LuxuryFac;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Construction, 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MagnaTowerConstr])));
        EvlStages.Insert( TEvlStage.Create( tidFacilityStage_Complete, 'Park Completed', 'Building is done', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_MagnaTower])));
        ClusterName := tidClusterName_Magna;
        FacilityKind := tidFacilityKind_MagnaCorrectionals;
        //TechnologyKind := tidInventionKind_Monuments;
        Register( tidClassFamily_Facilities );
      end;

  end;

procedure RegisterIndustries;
  begin
  end;

procedure RegisterStores;
  begin
    // Supermarket A
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MagnaSupermarketConstrA,
      25000000,
      [60, 20, 20],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaMagnaMarketBlock.Create(
      tidBlock_MagnaSupermarketA,
      [1, 6, 120],
      5/(24*7),
      4/(24*7),
      6*22,
      6*22,
      6*12,
      6*12,
      6*12,
      6*7,
      FairPrice,
      [1, 1, 1],
      200,
      TMagnaMarketBlock ) do
      begin
        Height := 5; // was 6
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MagnaSupermarketA, 'Sky Seller', vidFacility_MagnaSupermarketA, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 20;
        FacId := FID_Supermarket;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MagnaSupermarketConstrA])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Supermarket is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MagnaSupermarketA])));
        ClusterName := tidClusterName_Magna;
        FacilityKind := tidFacilityKind_MagnaMarkets;
        //TechnologyKind := tidInventionKind_SuperMarkets;
        Register( tidClassFamily_Facilities );
      end;

    // Supermarket B
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MagnaSupermarketConstrB,
      30000000,
      [60, 20, 20],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaMagnaMarketBlock.Create(
      tidBlock_MagnaSupermarketB,
      [1, 6, 120],
      5/(24*7),
      4/(24*7),
      6*22,
      6*22,
      6*12,
      6*12,
      6*12,
      6*7,
      FairPrice,
      [1, 1, 1],
      200,
      TMagnaMarketBlock ) do
      begin
        Height := 5; // was 6
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MagnaSupermarketB, 'Galaxie', vidFacility_MagnaSupermarketB, TFacility ) do
      begin
        xSize := 5;
        ySize := 5;
        Level := 20;
        FacId := FID_Supermarket;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MagnaSupermarketConstrA])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Supermarket is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MagnaSupermarketB])));
        ClusterName := tidClusterName_Magna;
        FacilityKind := tidFacilityKind_MagnaMarkets;
        //TechnologyKind := tidInventionKind_SuperMarkets;
        Register( tidClassFamily_Facilities );
      end;

  end;

procedure RegisterHeadquarters;
  begin
    // General
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MagnaResearchCenterConstr,
      1500000,
      [50, 10, 40],
      10,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaMainHeadquarter.Create(
      tidBlock_MagnaResearchCenter,
      [77, 33, 12],
      tidInventionKind_Direction,
      TMainHeadquarter ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MagnaResearchCenter, 'Magna Headquarters', vidFacility_MagnaResearchCenter, TFacility ) do
      begin
        Desc  := '';
        XSize := 3;
        YSize := 3;
        Level := 5000;
        FacId := FID_MainHeadquarter;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MagnaResearchCenterConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The facility is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MagnaResearchCenter])));
        ClusterName := tidClusterName_Magna;
        FacilityKind := tidFacilityKind_MagnaSacred;
        Register( tidClassFamily_Facilities );
      end;

{
    // Tower of Industry
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MagnaIndHeadquarterConstr,
      730000,
      [60, 0, 40],
      25,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaHeadquarterBlock.Create(
      tidBlock_MagnaIndHeadquarter,
      [12, 33, 33],
      tidInventionKind_IndustrialFacilities,
      //nidInvention_IndustrialFacilities,
      THeadquarterBlock ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MagnaIndHeadquarter, 'Industries Tower', vidFacility_MagnaIndHeadquarter, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 5000;
        FacId := FID_IndHeadquarter;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MagnaIndHeadquarterConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The facility is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidFacility_MagnaIndHeadquarter])));
        ClusterName := tidClusterName_Magna;
        FacilityKind := tidFacilityKind_MagnaSacred;
        TechnologyKind := tidInventionKind_Direction;
        Register( tidClassFamily_Facilities );
      end;

    // Tower of Illusions
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MagnaIllusionHeadquarterConstr,
      200000,
      [60, 0, 40],
      25,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaHeadquarterBlock.Create(
      tidBlock_MagnaIllusionHeadquarter,
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
    with TMetaFacility.Create( tidFacility_MagnaIllusionHeadquarter, 'Commerce Tower', vidFacility_MagnaIllusionHeadquarter, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 5000;
        FacId := FID_CommHeadquarter;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MagnaIllusionHeadquarterConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The facility is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidFacility_MagnaIllusionHeadquarter])));
        ClusterName := tidClusterName_Magna;
        FacilityKind := tidFacilityKind_MagnaSacred;
        TechnologyKind := tidInventionKind_Direction;
        Register( tidClassFamily_Facilities );
      end;

    // Tower of land lording
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MagnaResHeadquarterConstr,
      200000,
      [60, 0, 40],
      25,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaHeadquarterBlock.Create(
      tidBlock_MagnaResHeadquarter,
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
    with TMetaFacility.Create( tidFacility_MagnaResHeadquarter, 'Real Estates Tower', vidFacility_MagnaResHeadquarter, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 5000;
        FacId := FID_OffcHeadquarter;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MagnaResHeadquarterConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The facility is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidFacility_MagnaResHeadquarter])));
        ClusterName := tidClusterName_Magna;
        FacilityKind := tidFacilityKind_MagnaSacred;
        TechnologyKind := tidInventionKind_Direction;
        Register( tidClassFamily_Facilities );
      end;

    // Tower of Correction
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MagnaCorrectionHeadquarterConstr,
      200000,
      [60, 0, 40],
      25,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaPublicAffairsHeadquarter.Create(
      tidBlock_MagnaCorrectionHeadquarter,
      [33, 7, 12],
      tidInventionKind_PublicFacilities,
      //nidInvention_PublicFacilities,
      maxHQAdv,
      TPublicAffairsHeadquarter ) do
      begin
        //RegisterInvention( nidInvention_Monuments );
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MagnaCorrectionHeadquarter, 'Civic Affairs Tower', vidFacility_MagnaCorrectionHeadquarter, TFacility ) do
      begin
        XSize := 2;
        YSize := 2;
        Level := 5000;
        FacId := FID_PubHeadquarter;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MagnaCorrectionHeadquarterConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The facility is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidFacility_MagnaCorrectionHeadquarter])));
        ClusterName := tidClusterName_Magna;
        FacilityKind := tidFacilityKind_MagnaSacred;
        TechnologyKind := tidInventionKind_Direction;
        Register( tidClassFamily_Facilities );
      end;
      }
  end;

procedure RegisterSpecialFacilities;
  begin
    // General
    with TMetaBlockUnderConstruction.Create(
      tidBlock_MagnaMovieStudioConstr,
      2500000000,
      [40, 20, 40],
      30,
      TBlockUnderConstruction ) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaMovieStudios.Create(
      tidBlock_MagnaMovieStudio,
      [10, 30, 100],
      10,
      15,
      0.2,
      0.1,
      TMovieStudios) do
      begin
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( tidFacility_MagnaMovieStudio, 'Movie Studios', vidFacility_MagnaMovieStudio, TFacility ) do
      begin
        XSize := 8;
        YSize := 8;
        Level := 5000;
        FacId := FID_MovieStudios;
        //Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MagnaMovieStudioConstr])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The facility is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', tidBlock_MagnaMovieStudio])));
        ClusterName := tidClusterName_Magna;
        FacilityKind := tidFacilityKind_MagnaSpecial;
        TechnologyKind := tidInventionKind_MovieStudios;
        Register( tidClassFamily_Facilities );
      end;
  end;

procedure RegisterOffices;
  begin
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
    result := 'MagnaPack1';
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


