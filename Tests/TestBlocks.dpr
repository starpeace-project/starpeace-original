library TestBlocks;

uses
  ShareMem,
  SysUtils,
  Windows,
  ClassStorage,
  Classes,
  Population in '..\Kernel\Population.pas',
  Trade in '..\Kernel\Trade.pas',
  Construction in '..\StdBlocks\Construction.pas',
  PopulatedBlock in '..\Kernel\PopulatedBlock.pas',
  Kernel in '..\Kernel\Kernel.pas',
  WorkCenterBlock in '..\Kernel\WorkCenterBlock.pas',
  PublicFacility in '..\Kernel\PublicFacility.pas',
  Police in '..\StdBlocks\Police.pas',
  Protocol in '..\Protocol\Protocol.pas',
  Farms in '..\StdBlocks\Farms.pas',
  StdFluids in '..\StdBlocks\StdFluids.pas',
  Mine in '..\StdBlocks\Mine.pas',
  OutputEvaluators in '..\Kernel\OutputEvaluators.pas',
  Shops in '..\StdBlocks\Shops.pas',
  FoodMarket in '..\StdBlocks\FoodMarket.pas',
  Chemical in '..\StdBlocks\Chemical.pas',
  FoodProcessor in '..\StdBlocks\FoodProcessor.pas',
  EvaluatedBlock in '..\StdBlocks\EvaluatedBlock.pas',
  PolluterWorkCenter in '..\StdBlocks\PolluterWorkCenter.pas',
  MetalIndustry in '..\StdBlocks\MetalIndustry.pas',
  ConstructionIndustry in '..\StdBlocks\ConstructionIndustry.pas',
  ElectronicIndustry in '..\StdBlocks\ElectronicIndustry.pas',
  HeavyIndustry in '..\StdBlocks\HeavyIndustry.pas',
  TextilIndustry in '..\StdBlocks\TextilIndustry.pas',
  Clothings in '..\StdBlocks\Clothings.pas',
  CarIndustry in '..\StdBlocks\CarIndustry.pas',
  HouseHoldingAppliances in '..\StdBlocks\HouseHoldingAppliances.pas',
  ClothesShop in '..\StdBlocks\ClothesShop.pas',
  CarShop in '..\StdBlocks\CarShop.pas',
  BusinessMachines in '..\StdBlocks\BusinessMachines.pas',
  Headquarters in '..\Kernel\Headquarters.pas';

procedure RegisterModelExtension; export;
  begin
    FoodMarket.RegisterSurfaces;
    with TMetaBlockUnderConstruction.Create( '1x1 construction', 1, 200, 0, 0, TBlockUnderConstruction ) do
      begin
        xSize := 2;
        ySize := 2;
        Register( 'Blocks' );
      end;
    with TMetaBlockUnderConstruction.Create( '2x2 construction', 2, 400, 0, 0, TBlockUnderConstruction ) do
      begin
        xSize := 3;
        ySize := 3;
        Register( 'Blocks' );
      end;
    with TMetaBlockUnderConstruction.Create( '3x3 construction', 4, 900, 0, 0, TBlockUnderConstruction ) do
      begin
        xSize := 4;
        ySize := 4;
        Register( 'Blocks' );
      end;
    with TMetaBlockUnderConstruction.Create( '4x4 construction', 7, 1600, 4, 1000, TBlockUnderConstruction ) do
      begin
        xSize := 6;
        ySize := 6;
        Register( 'Blocks' );
      end;

    with TMetaPopulatedBlock.Create( 'HighClassBuilding', pkHigh, 50, TPopulatedBlock ) do
      begin
        xSize := 2;
        ySize := 2;
        Beauty := 40;
        BeautyStrength := 0.1;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( '0A', 'Howard''s Minitower', 100, TFacility ) do
      begin
        Desc :=
          'Designed by Howard B. Clauss, this is one of the finest buildings '+
          'in the whole Order and the Modern World. Of course, it is well suited ' +
          'for high society people. It can allocate up to 50 individuals.';
        XSize := 2;
        YSize := 2;
        Level := 120;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', '1x1 construction'])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById['Blocks', 'HighClassBuilding'])));
        ClusterName  := 'Microxoft';
        FacilityKind := 'MicroxoftResidentials';
        Register( 'Facilities' );
      end;

    with TMetaPopulatedBlock.Create( 'MiddleClassBuilding', pkMiddle, 200, TPopulatedBlock ) do
      begin
        xSize := 4;
        ySize := 4;
        Beauty := 20;
        BeautyStrength := 0.5;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( '0B', 'Paradise Residential', 110, TFacility ) do
      begin
        Desc :=
          'With about 110 capacities, Paradise Residentials are the ideal place for a '+
          'perfect life. The affordable price and its undenyable beauty makes it the best '+
          'choice for middle classes.';
        XSize := 4;
        YSize := 4;
        Level := 110;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', '3x3 construction'])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById['Blocks', 'MiddleClassBuilding'])));
        ClusterName := 'Microxoft';
        FacilityKind := 'MicroxoftResidentials';
        Register( 'Facilities' );
      end;

    with TMetaPopulatedBlock.Create( 'LowClassBuilding', pkLow, 400, TPopulatedBlock ) do
      begin
        xSize := 4;
        ySize := 4;
        Beauty := 10;
        BeautyStrength := 0.8;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( '0C', 'Sunset Apartments', 120, TFacility ) do
      begin
        Desc :=
          'The Microxoft Order really takes care of the working classes. Sunset Apartments ' +
          'can host up to 400 individuals with incredible commodities and extremely low price.';
        XSize := 4;
        YSize := 4;
        Level := 140;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', '3x3 construction'])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById['Blocks', 'LowClassBuilding'])));
        ClusterName := 'Microxoft';
        FacilityKind := 'MicroxoftResidentials';
        Register( 'Facilities' );
      end;
    with TMetaWorkCenter.Create( 'Factory', [40, 100, 500], TWorkCenter ) do
      begin
        xSize := 6;
        ySize := 6;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( '0D', 'Factory of Nothing', 130, TFacility ) do
      begin
        Desc :=
          'Designed by the Void Institute, this incledible industry '+
          'can generate actual jobs without any material production. '+
          'It is only to help testers.';
        XSize := 6;
        YSize := 6;
        Level := 14;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', '4x4 construction'])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Factory Completed', 'Factory is ready', TMetaBlock(TheClassStorage.ClassById['Blocks', 'Factory'])));
        ClusterName  := 'Microxoft';
        FacilityKind := 'MicroxoftFactories';
        TechnologyName := 'MicroxoftIndustry';
        Register( 'Facilities' );
      end;

    // Public Facilities
    with TMetaPublicFacilityInfo.Create( 'Police' ) do
      begin
        Name := 'Police Dep.';
        Importance := 100;
        Register( 'PublicFacilities' )
      end;
    with TMetaPublicFacilityInfo.Create( 'Fire' ) do
      begin
        Name := 'Fire Dep.';
        Importance := 30;
        Register( 'PublicFacilities' )
      end;
    with TMetaPublicFacilityInfo.Create( 'Health' ) do
      begin
        Name := 'Health';
        Importance := 70;
        Register( 'PublicFacilities' )
      end;
    with TMetaPublicFacilityInfo.Create( 'School' ) do
      begin
        Name := 'School';
        Importance := 80;
        Register( 'PublicFacilities' )
      end;

    with TMetaPublicFacility.Create( 'PoliceStation', TPoliceBlock ) do
      begin
        Kind     := TMetaPublicFacilityInfo(TheClassStorage.ClassById['PublicFacilities', 'Police']);
        Strength := 1000;
        XSize    := 2;
        YSize    := 2;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( 'PoliceStation', 'Police Station', 140, TFacility ) do
      begin
        Desc :=
          'To protect and serve. Microxoft Police can control about 1000 citizens.';
        XSize := 2;
        YSize := 2;
        Level := 100;
        EvlStages.Insert( TEvlStage.Create( 'Normal', 'Normal', 'Suddenly achieved', TMetaBlock(TheClassStorage.ClassById['Blocks', '1x1 construction'])));
        EvlStages.Insert( TEvlStage.Create( 'Normal', 'Normal', 'Suddenly achieved', TMetaBlock(TheClassStorage.ClassById['Blocks', 'PoliceStation'])));
        ClusterName  := 'Microxoft';
        FacilityKind := 'MicroxoftPublicFacilities';
        TechnologyName := 'MicroxoftSocial';
        Register( 'Facilities' );
      end;
    with TMetaPublicFacility.Create( 'FireStation', TPublicFacility ) do
      begin
        Kind     := TMetaPublicFacilityInfo(TheClassStorage.ClassById['PublicFacilities', 'Fire']);
        Strength := 2000;
        XSize    := 2;
        YSize    := 2;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( 'FireStation', 'Fire Station', 150, TFacility ) do
      begin
        Desc :=
          'Microxoft''s Fire Stations are the most eficient in the whole Modern '+
          'World. You will need a station like this for each 2000 citizens.';
        XSize := 2;
        YSize := 2;
        Level := 100;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', '1x1 construction'])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The facility is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', 'FireStation'])));
        ClusterName  := 'Microxoft';
        FacilityKind := 'MicroxoftPublicFacilities';
        TechnologyName := 'MicroxoftSocial';
        Register( 'Facilities' );
      end;
    with TMetaPublicFacility.Create( 'Hospital', TPublicFacility ) do
      begin
        Kind     := TMetaPublicFacilityInfo(TheClassStorage.ClassById['PublicFacilities', 'Health']);
        Strength := 2000;
        XSize    := 4;
        YSize    := 4;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( 'Hospital', 'Hospital', 160, TFacility ) do
      begin
        Desc :=
          'Where do you want to go today? Microxoft''s Hospitals are clean and efficient. '+
          'Nobody dies here unless it is Microxoft''s will. Each hospital can attend up to '+
          '2000 citizens.';
        XSize := 4;
        YSize := 4;
        Level := 100;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', '3x3 construction'])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The facility is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', 'Hospital'])));
        ClusterName  := 'Microxoft';
        FacilityKind := 'MicroxoftPublicFacilities';
        TechnologyName := 'MicroxoftSocial';
        Register( 'Facilities' );
      end;
    {
    with TMetaPublicFacility.Create( 'Clinic', TPublicFacility ) do
      begin
        Kind     := TMetaPublicFacilityInfo(TheClassStorage.ClassById['PublicFacilities', 'Health']);
        Strength := 100;
        XSize    := 2;
        YSize    := 2;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( 'Clinic', 'Clinic', 170, TFacility ) do
      begin
        Desc :=
          'Where do you want to go today? Microxoft''s Clinics are clean and efficient. '+
          'Nobody dies here unless it is Microxoft''s will. Each clinic can attend up to '+
          '100 citizens.';
        XSize := 2;
        YSize := 2;
        Level := 100;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', '2x2 construction'])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The facility is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', 'Clinic'])));
        ClusterName  := 'Microxoft';
        FacilityKind := 'MicroxoftPublicFacilities';
        Register( 'Facilities' );
      end;
    }
    with TMetaPublicFacility.Create( 'School', TPublicFacility ) do
      begin
        Kind     := TMetaPublicFacilityInfo(TheClassStorage.ClassById['PublicFacilities', 'School']);
        Strength := 500;
        XSize    := 3;
        YSize    := 3;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( 'School', 'School', 180, TFacility ) do
      begin
        Desc :=
          'Education is the key to personal success (Fates, vr20.43). Each Microxoft school '+
          'can educate the offspring of 500 citizens.';
        XSize := 3;
        YSize := 3;
        Level := 100;
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', '2x2 construction'])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The facility is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', 'School'])));
        ClusterName  := 'Microxoft';
        FacilityKind := 'MicroxoftPublicFacilities';
        TechnologyName := 'MicroxoftSocial';
        Register( 'Facilities' );
      end;

    // Farms
    with TMetaBlockUnderConstruction.Create( 'Farm construction', 1, 100, 1, 10, TBlockUnderConstruction ) do
      begin
        xSize := 6;
        ySize := 6;
        Register( 'Blocks' );
      end;
    with TMetaFarmBlock.Create(
      'MicroxoftFarm',
      0,
      10,
      0.012,
      0.04,
      200,
      100,
      1000,
      TFarmBlock ) do
      begin
        xSize := 6;
        ySize := 6;
        Beauty := -40;
        BeautyStrength := 0.1;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( 'MicroxoftFarm', 'Farm', 200, TFacility ) do
      begin
        xSize := 6;
        ySize := 6;
        Level := 20;
        Desc :=
          'I was born in a farm. (Fates, vr20.43). I saw the animals and the blue sky. Farms are important to me.';
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', 'Farm construction'])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The farm is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', 'MicroxoftFarm'])));
        ClusterName  := 'Microxoft';
        FacilityKind := 'MicroxoftFarms';
        TechnologyName := 'MicroxoftFarming';
        Register( 'Facilities' );
      end;

    // Chemical Industry
    with TMetaBlockUnderConstruction.Create( 'Chemical construction', 1, 100, 1, 10, TBlockUnderConstruction ) do
      begin
        xSize := 6;
        ySize := 6;
        Register( 'Blocks' );
      end;
    with TMetaChemicalBlock.Create( 'MicroxoftChemical', [10, 80, 150], 0.8, 1, 3000, 1000, 15500, TChemicalBlock ) do
      begin
        xSize := 6;
        ySize := 6;
        Beauty := -50;
        BeautyStrength := 1;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( 'MicroxoftChemical', 'Chemical plant', 130, TFacility ) do
      begin
        xSize := 6;
        ySize := 6;
        Level := 20;
        Desc :=
          'Chemical is one of the building blocks of moden world. (Fates, vr10.35). I think all I have, my house, my wife, my money is thaks to chemical stuffs.';
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', 'Chemical construction'])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The chemical industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', 'MicroxoftChemical'])));
        ClusterName  := 'Microxoft';
        FacilityKind := 'MicroxoftFactories';
        TechnologyName := 'MicroxoftIndustry';
        Register( 'Facilities' );
      end;

    // Mine
    with TMetaBlockUnderConstruction.Create( 'Mine construction', 1, 100, 1, 10, TBlockUnderConstruction ) do
      begin
        xSize := 6;
        ySize := 6;
        Register( 'Blocks' );
      end;
    with TMetaMineBlock.Create( 'MicroxoftMine', [5, 30, 200], 0.02, 0.6, 100, 10000, 5300, TMineBlock ) do
      begin
        xSize := 6;
        ySize := 6;
        Beauty := -100;
        BeautyStrength := 1;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( 'MicroxoftMine', 'Mine', 130, TFacility ) do
      begin
        xSize := 6;
        ySize := 6;
        Level := 20;
        Desc := 'My brother owns a mine, I envy him. (Fates, vr23.15)';
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', 'Mine construction'])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The mine is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', 'MicroxoftMine'])));
        ClusterName  := 'Microxoft';
        FacilityKind := 'MicroxoftFactories';
        TechnologyName := 'MicroxoftIndustry';
        Register( 'Facilities' );
      end;

    // Food Processor
    with TMetaBlockUnderConstruction.Create( 'FoodProcessorConstruction', 1, 100, 1, 10, TBlockUnderConstruction ) do
      begin
        xSize := 4;
        ySize := 4;
        Register( 'Blocks' );
      end;
    with TMetaFoodProcessorBlock.Create(
      'MicroxoftFoodProcessor',
      [5, 20, 300],
      100,
      85,
      0.012,
      0.4,
      200,
      0,
      TFoodProcessorBlock ) do
      begin
        xSize := 4;
        ySize := 4;
        Beauty := -10;
        BeautyStrength := 1;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( 'MicroxoftFoodProcessor', 'Food Processor', 160, TFacility ) do
      begin
        xSize := 4;
        ySize := 4;
        Level := 20;
        Desc := 'Microxoft food processors are today''s solution... (Fates, vr142.27).';
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', 'FoodProcessorConstruction'])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The factory is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', 'MicroxoftFoodProcessor'])));
        ClusterName  := 'Microxoft';
        FacilityKind := 'MicroxoftFactories';
        TechnologyName := 'MicroxoftIndustry';
        Register( 'Facilities' );
      end;

    Farms.RegisterBackup;
    Chemical.RegisterBackup;
    Mine.RegisterBackup;
    Shops.RegisterBackup;
    FoodProcessor.RegisterBackup;

   // >>>>> New stuffs <<<<<<

    // Metal industry
    with TMetaBlockUnderConstruction.Create( 'MetalIndustryConstruction', 1, 100, 1, 10, TBlockUnderConstruction ) do
      begin
        xSize := 6;
        ySize := 6;
        Register( 'Blocks' );
      end;
    with TMetaMetalIndustryBlock.Create(
      'MicroxoftMetalIndustry',
      [5, 20, 300],
      2000,
      200,
      0.029,
      0.6,
      1000,
      700,
      TMetalIndustryBlock ) do
      begin
        xSize := 6;
        ySize := 6;
        Beauty := -10;
        BeautyStrength := 1;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( 'MicroxoftMetalIndustry', 'Metalurgic Industry', 130, TFacility ) do
      begin
        xSize := 6;
        ySize := 6;
        Level := 20;
        Desc := 'Microxoft metalurgic industries are the best of this world.';
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', 'MetalIndustryConstruction'])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', 'MicroxoftMetalIndustry'])));
        ClusterName  := 'Microxoft';
        FacilityKind := 'MicroxoftFactories';
        TechnologyName := 'MicroxoftIndustry';
        Register( 'Facilities' );
      end;

    // Construction industry
    with TMetaBlockUnderConstruction.Create( 'ConstructionIndustryConstruction', 1, 100, 1, 10, TBlockUnderConstruction ) do
      begin
        xSize := 6;
        ySize := 6;
        Register( 'Blocks' );
      end;
    with TMetaConstructionIndustryBlock.Create(
      'MicroxoftConstructionIndustry',
      [5, 10, 300],
      500,
      200,
      200,
      0.6,
      0.029,
      1000,
      700,
      TConstructionIndustryBlock ) do
      begin
        xSize := 6;
        ySize := 6;
        Beauty := -40;
        BeautyStrength := 0.5;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( 'MicroxoftConstructionIndustry', 'Construction Industry', 130, TFacility ) do
      begin
        xSize := 6;
        ySize := 6;
        Level := 20;
        Desc := 'Microxoft construction industries are the best of this world.';
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', 'ConstructionIndustryConstruction'])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', 'MicroxoftConstructionIndustry'])));
        ClusterName  := 'Microxoft';
        FacilityKind := 'MicroxoftFactories';
        TechnologyName := 'MicroxoftIndustry';
        Register( 'Facilities' );
      end;

    // Electronic industry
    with TMetaBlockUnderConstruction.Create( 'ElectronicIndustryConstruction', 1, 100, 1, 10, TBlockUnderConstruction) do
      begin
        xSize := 6;
        ySize := 6;
        Register( 'Blocks' );
      end;
    with TMetaElectronicIndustryBlock.Create(
      'MicroxoftElectronicIndustry',
      [5, 50, 200],
      100,
      50,
      1,
      0.5,
      100,
      2000,
      TElectronicIndustryBlock ) do
      begin
        xSize := 6;
        ySize := 6;
        Beauty := -40;
        BeautyStrength := 0.5;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( 'MicroxoftElectronicIndustry', 'Electronic Industry', 130, TFacility ) do
      begin
        xSize := 6;
        ySize := 6;
        Level := 20;
        Desc := 'Microxoft Electronic industries are the best of this world.';
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', 'ElectronicIndustryConstruction'])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', 'MicroxoftElectronicIndustry'])));
        ClusterName  := 'Microxoft';
        FacilityKind := 'MicroxoftFactories';
        TechnologyName := 'MicroxoftIndustry';
        Register( 'Facilities' );
      end;

    // Heavy industry
    with TMetaBlockUnderConstruction.Create( 'HeavyIndustryConstruction', 1, 100, 1, 10, TBlockUnderConstruction) do
      begin
        xSize := 6;
        ySize := 6;
        Register( 'Blocks' );
      end;
    with TMetaHeavyIndustryBlock.Create(
      'MicroxoftHeavyIndustry',
      [4, 50, 200],
      1000,
      400,
      200,
      0.6,
      0.03,
      10,
      52300,
      THeavyIndustryBlock ) do
      begin
        xSize := 6;
        ySize := 6;
        Beauty := -40;
        BeautyStrength := 0.5;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( 'MicroxoftHeavyIndustry', 'Heavy Industry', 130, TFacility ) do
      begin
        xSize := 6;
        ySize := 6;
        Level := 20;
        Desc := 'Microxoft Heavy industries are the best of this world.';
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', 'HeavyIndustryConstruction'])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', 'MicroxoftHeavyIndustry'])));
        ClusterName  := 'Microxoft';
        FacilityKind := 'MicroxoftFactories';
        TechnologyName := 'MicroxoftIndustry';
        Register( 'Facilities' );
      end;

    // Textil industry
    with TMetaBlockUnderConstruction.Create( 'TextilIndustryConstruction', 1, 100, 1, 10, TBlockUnderConstruction) do
      begin
        xSize := 6;
        ySize := 6;
        Register( 'Blocks' );
      end;
    with TMetaTextilIndustryBlock.Create(
      'MicroxoftTextilIndustry',
      [4, 12, 300],
      180,
      70,
      10,
      0.6,
      0.03,
      800,
      500,
      TTextilIndustryBlock ) do
      begin
        xSize := 6;
        ySize := 6;
        Beauty := -40;
        BeautyStrength := 0.5;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( 'MicroxoftTextilIndustry', 'Textil Industry', 130, TFacility ) do
      begin
        xSize := 6;
        ySize := 6;
        Level := 20;
        Desc := 'Microxoft Textil industries are the best of this world.';
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', 'TextilIndustryConstruction'])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', 'MicroxoftTextilIndustry'])));
        ClusterName  := 'Microxoft';
        FacilityKind := 'MicroxoftFactories';
        TechnologyName := 'MicroxoftIndustry';
        Register( 'Facilities' );
      end;

    // Clothings industry
    with TMetaBlockUnderConstruction.Create( 'ClothingsIndustryConstruction', 1, 100, 1, 10, TBlockUnderConstruction) do
      begin
        xSize := 6;
        ySize := 6;
        Register( 'Blocks' );
      end;
    with TMetaClothingsIndustryBlock.Create(
      'MicroxoftClothingsIndustry',
      [5, 10, 100],
      200,
      100,
      20,
      0.6,
      0.03,
      50,
      200,
      TClothingsIndustryBlock ) do
      begin
        xSize := 6;
        ySize := 6;
        Beauty := -40;
        BeautyStrength := 0.5;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( 'MicroxoftClothingsIndustry', 'Clothings Industry', 130, TFacility ) do
      begin
        xSize := 6;
        ySize := 6;
        Level := 20;
        Desc := 'Microxoft Clothings industries are the best of this world.';
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', 'ClothingsIndustryConstruction'])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', 'MicroxoftClothingsIndustry'])));
        ClusterName  := 'Microxoft';
        FacilityKind := 'MicroxoftFactories';
        TechnologyName := 'MicroxoftIndustry';
        Register( 'Facilities' );
      end;

    // Car industry
    with TMetaBlockUnderConstruction.Create( 'CarIndustryConstruction', 1, 100, 1, 10, TBlockUnderConstruction) do
      begin
        xSize := 6;
        ySize := 6;
        Register( 'Blocks' );
      end;
    with TMetaCarIndustryBlock.Create(
      'MicroxoftCarIndustry',
      [20, 40, 200],
      1000,
      100,
      40,
      10,
      200,
      1,
      0.03,
      3,
      35600,
      TCarIndustryBlock ) do
      begin
        xSize := 6;
        ySize := 6;
        Beauty := -40;
        BeautyStrength := 0.5;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( 'MicroxoftCarIndustry', 'Car Industry', 130, TFacility ) do
      begin
        xSize := 6;
        ySize := 6;
        Level := 20;
        Desc := 'Microxoft Car industries are the best of this world.';
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', 'CarIndustryConstruction'])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', 'MicroxoftCarIndustry'])));
        ClusterName  := 'Microxoft';
        FacilityKind := 'MicroxoftFactories';
        TechnologyName := 'MicroxoftIndustry';
        Register( 'Facilities' );
      end;

    // HHA industry
    with TMetaBlockUnderConstruction.Create( 'HHAIndustryConstruction', 1, 100, 1, 10, TBlockUnderConstruction) do
      begin
        xSize := 6;
        ySize := 6;
        Register( 'Blocks' );
      end;
    with TMetaHouseHoldingAppliancesBlock.Create(
      'MicroxoftHHAIndustry',
      [20, 40, 200],
      500,
      100,
      10,
      70,
      1,
      0.8,
      70,
      0,
      THouseHoldingAppliancesBlock ) do
      begin
        xSize := 6;
        ySize := 6;
        Beauty := -40;
        BeautyStrength := 0.5;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( 'MicroxoftHHAIndustry', 'H. H. Apps. Industry', 130, TFacility ) do
      begin
        xSize := 6;
        ySize := 6;
        Level := 20;
        Desc := 'Microxoft House Holding Appliances industries are the best of this world.';
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', 'HHAIndustryConstruction'])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', 'MicroxoftHHAIndustry'])));
        ClusterName  := 'Microxoft';
        FacilityKind := 'MicroxoftFactories';
        TechnologyName := 'MicroxoftIndustry';
        Register( 'Facilities' );
      end;

    // BusinessMachines industry
    with TMetaBlockUnderConstruction.Create( 'BusinessMachinesIndustryConstruction', 1, 100, 1, 10, TBlockUnderConstruction) do
      begin
        xSize := 6;
        ySize := 6;
        Register( 'Blocks' );
      end;
    with TMetaBusinessMachinesBlock.Create(
      'MicroxoftBusinessMachinesIndustry',
      [20, 40, 200],
      100,
      70,
      250,
      4,
      0.8,
      20,
      0,
      TBusinessMachinesBlock ) do
      begin
        xSize := 6;
        ySize := 6;
        Beauty := -40;
        BeautyStrength := 0.5;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( 'MicroxoftBusinessMachinesIndustry', 'Business machines Industry', 130, TFacility ) do
      begin
        xSize := 6;
        ySize := 6;
        Level := 20;
        Desc := 'Microxoft Business Machines industries are the best of this world.';
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', 'BusinessMachinesIndustryConstruction'])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The industry is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', 'MicroxoftBusinessMachinesIndustry'])));
        ClusterName  := 'Microxoft';
        FacilityKind := 'MicroxoftFactories';
        TechnologyName := 'MicroxoftIndustry';
        Register( 'Facilities' );
      end;

    // Food Market
    with TMetaBlockUnderConstruction.Create( 'Food Market construction', 1, 100, 1, 10, TBlockUnderConstruction ) do
      begin
        xSize := 2;
        ySize := 2;
        Register( 'Blocks' );
      end;
    with TMetaFoodMarketBlock.Create( 'MicroxoftFoodMarket', [2, 5, 20], 0, 0, 0, 200, 200, TFoodMarketBlock ) do
      begin
        xSize := 2;
        ySize := 2;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( 'MicroxoftFoodMarket', 'Food Market', 150, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 20;
        Desc := 'Microxoft food market are clean and kind enought to fullfil your feelings.';
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', 'Food Market construction'])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The market is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', 'MicroxoftFoodMarket'])));
        ClusterName  := 'Microxoft';
        FacilityKind := 'MicroxoftShops';
        TechnologyName := 'MicroxoftMarkets';
        Register( 'Facilities' );
      end;

    // Clothes Shop
    with TMetaBlockUnderConstruction.Create( 'Clothes Shop construction', 1, 100, 1, 10, TBlockUnderConstruction ) do
      begin
        xSize := 2;
        ySize := 2;
        Register( 'Blocks' );
      end;
    with TMetaClothesShopBlock.Create( 'MicroxoftClothesShop', [1, 5, 20], 200, 0, 0, TClothesShopBlock ) do
      begin
        xSize := 2;
        ySize := 2;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( 'MicroxoftClothesShop', 'Clothes Shop', 150, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 20;
        Desc := 'Microxoft Clothes Shop are clean and kind enought to fullfil your feelings.';
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', 'Clothes Shop construction'])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Shop is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', 'MicroxoftClothesShop'])));
        ClusterName  := 'Microxoft';
        FacilityKind := 'MicroxoftShops';
        TechnologyName := 'MicroxoftMarkets';
        Register( 'Facilities' );
      end;

    // Car Shop
    with TMetaBlockUnderConstruction.Create( 'Car Shop construction', 1, 100, 1, 10, TBlockUnderConstruction ) do
      begin
        xSize := 2;
        ySize := 2;
        Register( 'Blocks' );
      end;
    with TMetaCarShopBlock.Create( 'MicroxoftCarShop', [1, 5, 20], 4, 0, 0, TCarShopBlock ) do
      begin
        xSize := 2;
        ySize := 2;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( 'MicroxoftCarShop', 'Car Shop', 150, TFacility ) do
      begin
        xSize := 2;
        ySize := 2;
        Level := 20;
        Desc := 'Microxoft Car Shop are clean and kind enought to fullfil your feelings.';
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', 'Car Shop construction'])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The Shop is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', 'MicroxoftCarShop'])));
        ClusterName  := 'Microxoft';
        FacilityKind := 'MicroxoftShops';
        TechnologyName := 'MicroxoftMarkets';
        Register( 'Facilities' );
      end;

    // Headquarters
    with TMetaHeadquarterBlock.Create( 'GeneralHQ', [30, 40, 20], 'MicroxoftCompanyDivisions', THeadquarterBlock ) do
      begin
        XSize    := 4;
        YSize    := 4;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( 'GeneralHQ', 'General Headquarters', 160, TFacility ) do
      begin
        Desc  := '';
        XSize := 4;
        YSize := 4;
        Level := 100;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', '3x3 construction'])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The facility is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', 'GeneralHQ'])));
        ClusterName  := 'Microxoft';
        FacilityKind := 'MicroxoftHeadquarters';
        Register( 'Facilities' );
      end;

    with TMetaHeadquarterBlock.Create( 'IndustrialHQ', [50, 50, 30], 'MicroxoftIndustry', THeadquarterBlock ) do
      begin
        XSize    := 4;
        YSize    := 4;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( 'IndustrialHQ', 'Industrial Division', 160, TFacility ) do
      begin
        Desc  := '';
        XSize := 4;
        YSize := 4;
        Level := 100;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', '3x3 construction'])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The facility is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', 'IndustrialHQ'])));
        ClusterName    := 'Microxoft';
        FacilityKind   := 'MicroxoftHeadquarters';
        TechnologyName := 'CompanyDivisions';
        Register( 'Facilities' );
      end;

    with TMetaHeadquarterBlock.Create( 'FarmingHQ', [20, 10, 10], 'MicroxoftFarming', THeadquarterBlock ) do
      begin
        XSize    := 4;
        YSize    := 4;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( 'FarmingHQ', 'Country Division', 160, TFacility ) do
      begin
        Desc  := '';
        XSize := 4;
        YSize := 4;
        Level := 100;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', '3x3 construction'])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The facility is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', 'FarmingHQ'])));
        ClusterName    := 'Microxoft';
        FacilityKind   := 'MicroxoftHeadquarters';
        TechnologyName := 'CompanyDivisions';
        Register( 'Facilities' );
      end;

    with TMetaHeadquarterBlock.Create( 'ResidentialsHQ', [15, 20, 10], 'MicroxoftLandLording', THeadquarterBlock ) do
      begin
        XSize    := 4;
        YSize    := 4;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( 'ResidentialsHQ', 'Real State Division', 160, TFacility ) do
      begin
        Desc  := '';
        XSize := 4;
        YSize := 4;
        Level := 100;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', '3x3 construction'])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The facility is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', 'ResidentialsHQ'])));
        ClusterName    := 'Microxoft';
        FacilityKind   := 'MicroxoftHeadquarters';
        TechnologyName := 'CompanyDivisions';
        Register( 'Facilities' );
      end;

    with TMetaHeadquarterBlock.Create( 'PublicFacHQ', [10, 40, 50], 'MicroxoftSocial', THeadquarterBlock ) do
      begin
        XSize    := 4;
        YSize    := 4;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( 'PublicFacHQ', 'Public Facilities Division', 160, TFacility ) do
      begin
        Desc  := '';
        XSize := 4;
        YSize := 4;
        Level := 100;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', '3x3 construction'])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The facility is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', 'PublicFacHQ'])));
        ClusterName    := 'Microxoft';
        FacilityKind   := 'MicroxoftHeadquarters';
        TechnologyName := 'CompanyDivisions';
        Register( 'Facilities' );
      end;

    with TMetaHeadquarterBlock.Create( 'StoresHQ', [25, 50, 50], 'MicroxoftMarkets', THeadquarterBlock ) do
      begin
        XSize    := 4;
        YSize    := 4;
        Register( 'Blocks' );
      end;
    with TMetaFacility.Create( 'StoresHQ', 'Stores Division', 160, TFacility ) do
      begin
        Desc  := '';
        XSize := 4;
        YSize := 4;
        Level := 100;
        Options := Options - [mfcGenerateName];
        EvlStages.Insert( TEvlStage.Create( 'Construction', 'Under Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById['Blocks', '3x3 construction'])));
        EvlStages.Insert( TEvlStage.Create( 'Complete', 'Completed', 'The facility is ready to operate', TMetaBlock(TheClassStorage.ClassById['Blocks', 'StoresHQ'])));
        ClusterName    := 'Microxoft';
        FacilityKind   := 'MicroxoftHeadquarters';
        TechnologyName := 'CompanyDivisions';
        Register( 'Facilities' );
      end;


    MetalIndustry.RegisterBackup;
    ConstructionIndustry.RegisterBackup;
    ElectronicIndustry.RegisterBackup;
    HeavyIndustry.RegisterBackup;
    TextilIndustry.RegisterBackup;
    Clothings.RegisterBackup;
    CarIndustry.RegisterBackup;
    HouseHoldingAppliances.RegisterBackup;
    BusinessMachines.RegisterBackup;
    FoodMarket.RegisterBackup;
    FoodMarket.RegisterSurfaces;
    ClothesShop.RegisterBackup;
    ClothesShop.RegisterSurfaces;
    CarShop.RegisterBackup;
    CarShop.RegisterSurfaces;
  end;

exports
  RegisterModelExtension;

{$E mdx}

begin
end.
