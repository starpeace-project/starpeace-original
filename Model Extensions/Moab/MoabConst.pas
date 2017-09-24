unit MoabConst;

interface

  uses
    Standards;

  const
    tidClusterName_Moab = 'Moab';

  const
    tidInventionKind_Rulership        = 'MoabRulership';
    tidInventionKind_Industrial       = 'MoabIndustrial';
    tidInventionKind_Market           = 'MoabMarket';
    tidInventionKind_Residential      = 'MoabResidential';
    tidInventionKind_Public           = 'MoabPublic';

  const
    nidInvention_MoabLove             = 1;
    nidInvention_MoabDynamics         = 2;
    nidInvention_MoabLandLording      = 3;
    nidInvention_MoabBasicIllusions   = 4;
    nidInvention_MoabBasicCorrections = 5;

    nidInvention_MoabOffices          = 10;

  const
    tidFacilityKind_MoabSacred             = 'MoabSacred';
    tidFacilityKind_MoabIndustrial         = 'MoabIndustrial';
    tidFacilityKind_MoabResidentials       = 'MoabResidentials';
    tidFacilityKind_MoabIllusions          = 'MoabIllusions';
    tidFacilityKind_MoabCorrectionals      = 'MoabCorrectionals';
    tidFacilityKind_MoabBusinessFacilities = tidClusterName_Moab + tidFacilityKind_BusinessFacilities;
    tidFacilityKind_MoabSpecial            = tidClusterName_Moab + tidFacilityKind_Special;

  const
    tmeInvention_VeryShort = 10;  // NOTE: Research time is in hours!
    tmeInvention_Short     = 30;
    tmeInvention_Normal    = 50;
    tmeInvention_Long      = 90;
    tmeInvention_VeryLong  = 120;

  const
    tidFacilityStage_Construction = 'Construction';
    tidFacilityStage_Complete     = 'Completed';

  const
    tidBlock_MoabTownHall       = 'MoabTownHall';
    tidFacility_MoabTownHall    = 'MoabTownHall';
    vidFacility_MoabTownHall    = 1500;

  const
    tidBlock_MoabTradeCenter       = 'MoabTradeCenter';
    tidFacility_MoabTradeCenter    = 'MoabTradeCenter';
    vidFacility_MoabTradeCenter    = 1510;

  const
    tidBlock_LoCostKnightsConstr = 'KnightsLoCostConstr';
    tidBlock_LoCostKnights       = 'KnightsLoCost';
    tidFacility_LoCostKnights    = 'KnightsLoCost';
    vidFacility_LoCostKnights    = 1461;

  const
    tidBlock_LoCostNursesConstr = 'NursesLoCostConstr';
    tidBlock_LoCostNurses       = 'NursesLoCost';
    tidFacility_LoCostNurses    = 'NursesLoCost';
    vidFacility_LoCostNurses    = 1471; //1441;

  const
    tidBlock_LoCostBeingsConstr = 'BeingsLoCostConstr';
    tidBlock_LoCostBeings       = 'BeingsLoCost';
    tidFacility_LoCostBeings    = 'BeingsLoCost';
    vidFacility_LoCostBeings    = 1481;//1451;

  const
    tidBlock_KnightsBuildingConstrA = 'KnightsBuildingConstrA';
    tidBlock_KnightsBuildingA       = 'KnightsBuildingA';
    tidFacility_KnightsBuildingA    = 'KnightsBuildingA';
    vidFacility_KnightsBuildingA    = 1361;

  const
    tidBlock_KnightsBuildingConstrB = 'KnightsBuildingConstrB';
    tidBlock_KnightsBuildingB       = 'KnightsBuildingB';
    tidFacility_KnightsBuildingB    = 'KnightsBuildingB';
    vidFacility_KnightsBuildingB    = 1371;

  const
    tidBlock_KnightsBuildingConstrC = 'KnightsBuildingConstrC';
    tidBlock_KnightsBuildingC       = 'KnightsBuildingC';
    tidFacility_KnightsBuildingC    = 'KnightsBuildingC';
    vidFacility_KnightsBuildingC    = 1381;

  const
    tidBlock_KnightsBuildingConstrD = 'KnightsBuildingConstrD';
    tidBlock_KnightsBuildingD       = 'KnightsBuildingD';
    tidFacility_KnightsBuildingD    = 'KnightsBuildingD';
    vidFacility_KnightsBuildingD    = 1451;

  const
    tidBlock_KnightsBuildingConstrE = 'KnightsBuildingConstrE';
    tidBlock_KnightsBuildingE       = 'KnightsBuildingE';
    tidFacility_KnightsBuildingE    = 'KnightsBuildingE';
    vidFacility_KnightsBuildingE    = 1451;

  const
    tidBlock_NurseDomeConstrA = 'NurseDomeConstrA';
    tidBlock_NurseDomeA       = 'NurseDomeA';
    tidFacility_NurseDomeA    = 'NurseDomeA';
    vidFacility_NurseDomeA    = 1341;

  const
    tidBlock_NurseDomeConstrB = 'NurseDomeConstrB';
    tidBlock_NurseDomeB       = 'NurseDomeB';
    tidFacility_NurseDomeB    = 'NurseDomeB';
    vidFacility_NurseDomeB    = 1351;

  const
    tidBlock_NurseDomeConstrC = 'NurseDomeConstrC';
    tidBlock_NurseDomeC       = 'NurseDomeC';
    tidFacility_NurseDomeC    = 'NurseDomeC';
    vidFacility_NurseDomeC    = 1431;

  const
    tidBlock_NurseDomeConstrD = 'NurseDomeConstrD';
    tidBlock_NurseDomeD       = 'NurseDomeD';
    tidFacility_NurseDomeD    = 'NurseDomeD';
    vidFacility_NurseDomeD    = 1441;

  const
    tidBlock_BeingsDomeConstrA = 'BeingsDomeConstrA';
    tidBlock_BeingsDomeA       = 'BeingsDomeA';
    tidFacility_BeingsDomeA    = 'BeingsDomeA';
    vidFacility_BeingsDomeA    = 1301;

  const
    tidBlock_BeingsDomeConstrB = 'BeingsDomeConstrB';
    tidBlock_BeingsDomeB       = 'BeingsDomeB';
    tidFacility_BeingsDomeB    = 'BeingsDomeB';
    vidFacility_BeingsDomeB    = 1311;

  const
    tidBlock_BeingsDomeConstrC = 'BeingsDomeConstrC';
    tidBlock_BeingsDomeC       = 'BeingsDomeC';
    tidFacility_BeingsDomeC    = 'BeingsDomeC';
    vidFacility_BeingsDomeC    = 1321;

  const
    tidBlock_BeingsDomeConstrD = 'BeingsDomeConstrD';
    tidBlock_BeingsDomeD       = 'BeingsDomeD';
    tidFacility_BeingsDomeD    = 'BeingsDomeD';
    vidFacility_BeingsDomeD    = 1331;

  const
    tidBlock_MoabFarmConstr = 'MoabFarmConstr';
    tidBlock_MoabFarm       = 'MoabFarm';
    tidFacility_MoabFarm    = 'MoabFarm';
    vidFacility_MoabFarm    = 1111;

  const
    tidBlock_MoabChemicalConstr = 'MoabChemicalConstr';
    tidBlock_MoabChemical       = 'MoabChemical';
    tidFacility_MoabChemical    = 'MoabChemical';
    vidFacility_MoabChemical    = 1161;

  const
    tidBlock_MoabMineConstr = 'MoabMineConstr';
    tidBlock_MoabMine       = 'MoabMine';
    tidFacility_MoabMine    = 'MoabMine';
    vidFacility_MoabMine    = 1121;

  const
    tidBlock_MoabChemMineConstr = 'MoabChemMineConstr';
    tidBlock_MoabChemMine       = 'MoabChemMine';
    tidFacility_MoabChemMine    = 'MoabChemMine';
    vidFacility_MoabChemMine    = 7011;

  const
    tidBlock_MoabSiliconMineConstr = 'MoabSiliconMineConstr';
    tidBlock_MoabSiliconMine       = 'MoabSiliconMine';
    tidFacility_MoabSiliconMine    = 'MoabSiliconMine';
    vidFacility_MoabSiliconMine    = 7021;

  const
    tidBlock_MoabStoneMineConstr = 'MoabStoneMineConstr';
    tidBlock_MoabStoneMine       = 'MoabStoneMine';
    tidFacility_MoabStoneMine    = 'MoabStoneMine';
    vidFacility_MoabStoneMine    = 7031;

  const
    tidBlock_MoabCoalMineConstr = 'MoabCoalMineConstr';
    tidBlock_MoabCoalMine       = 'MoabCoalMine';
    tidFacility_MoabCoalMine    = 'MoabCoalMine';
    vidFacility_MoabCoalMine    = 7041;

  const
    tidBlock_MoabOilRigConstr = 'MoabOilRigConstr';
    tidBlock_MoabOilRig       = 'MoabOilRig';
    tidFacility_MoabOilRig    = 'MoabOilRig';
    vidFacility_MoabOilRig    = 1241;

  const
    tidBlock_MoabRefineryConstr = 'MoabRefineryConstr';
    tidBlock_MoabRefinery       = 'MoabRefinery';
    tidFacility_MoabRefinery    = 'MoabRefinery';
    vidFacility_MoabRefinery    = 1251;

  const
    tidBlock_MoabFoodDomeConstr = 'MoabFoodDomeConstr';
    tidBlock_MoabFoodDome       = 'MoabFoodDome';
    tidFacility_MoabFoodDome    = 'MoabFoodDome';
    vidFacility_MoabFoodDome    = 1141;

  const
    tidBlock_MoabMetalConstr = 'MoabMetalConstr';
    tidBlock_MoabMetal       = 'MoabMetal';
    tidFacility_MoabMetal    = 'MoabMetal';
    vidFacility_MoabMetal    = 1151;

  const
    tidBlock_MoabTextileConstr = 'MoabTextileConstr';
    tidBlock_MoabTextile       = 'MoabTextile';
    tidFacility_MoabTextile    = 'MoabTextile';
    vidFacility_MoabTextile    = 1171;

  const
    tidBlock_MoabClothingsConstr = 'MoabClothingsConstr';
    tidBlock_MoabClothings       = 'MoabClothings';
    tidFacility_MoabClothings    = 'MoabClothings';
    vidFacility_MoabClothings    = 1181;

  const
    tidBlock_MoabElectronicConstr = 'MoabElectronicConstr';
    tidBlock_MoabElectronic       = 'MoabElectronic';
    tidFacility_MoabElectronic    = 'MoabElectronic';
    vidFacility_MoabElectronic    = 1131;

  const
    tidBlock_MoabConstructionConstr = 'MoabConstructionConstr';
    tidBlock_MoabConstruction       = 'MoabConstruction';
    tidFacility_MoabConstruction    = 'MoabConstruction';
    vidFacility_MoabConstruction    = 1221; // >> change latter

  const
    tidBlock_MoabHeavyConstr = 'MoabHeavyConstr';
    tidBlock_MoabHeavy       = 'MoabHeavy';
    tidFacility_MoabHeavy    = 'MoabHeavy';
    vidFacility_MoabHeavy    = 1231; // >> change latter

  const
    tidBlock_MoabCarIndustryConstr = 'MoabCarIndustryConstr';
    tidBlock_MoabCarIndustry       = 'MoabCarIndustry';
    tidFacility_MoabCarIndustry    = 'MoabCarIndustry';
    vidFacility_MoabCarIndustry    = 1191;

  const
    tidBlock_MoabHHAIndustryConstr = 'MoabHHAIndustryConstr';
    tidBlock_MoabHHAIndustry       = 'MoabHHAIndustry';
    tidFacility_MoabHHAIndustry    = 'MoabHHAIndustry';
    vidFacility_MoabHHAIndustry    = 1201;

  const
    tidBlock_MoabBusinessMachineConstr = 'MoabBusinessMachineConstr';
    tidBlock_MoabBusinessMachine       = 'MoabBusinessMachine';
    tidFacility_MoabBusinessMachine    = 'MoabBusinessMachine';
    vidFacility_MoabBusinessMachine    = 1211;

  const
    tidBlock_MoabFoodStoreConstr = 'MoabFoodStoreConstr';
    tidBlock_MoabFoodStore       = 'MoabFoodStore';
    tidFacility_MoabFoodStore    = 'MoabFoodStore';
    vidFacility_MoabFoodStore    = 1711;                             

  const
    tidBlock_MoabCarStoreConstr = 'MoabCarStoreConstr';
    tidBlock_MoabCarStore       = 'MoabCarStore';
    tidFacility_MoabCarStore    = 'MoabCarStore';
    vidFacility_MoabCarStore    = 1721;

  const
    tidBlock_MoabGasStationConstr = 'MoabGasStationConstr';
    tidBlock_MoabGasStation       = 'MoabGasStation';
    tidFacility_MoabGasStation    = 'MoabGasStation';
    vidFacility_MoabGasStation    = 1781;

  const
    tidBlock_MoabClothesStoreConstr = 'MoabClothesStoreConstr';
    tidBlock_MoabClothesStore       = 'MoabClothesStore';
    tidFacility_MoabClothesStore    = 'MoabClothesStore';
    vidFacility_MoabClothesStore    = 1731;

  const
    tidBlock_MoabSupermarketConstr = 'MoabSupermarketConstr';
    tidBlock_MoabSupermarket       = 'MoabSupermarket';
    tidFacility_MoabSupermarket    = 'MoabSupermarket';
    vidFacility_MoabSupermarket    = 1741;

  const
    tidBlock_MoabBarConstr = 'MoabBarConstr';
    tidBlock_MoabBar       = 'MoabBar';
    tidFacility_MoabBar    = 'MoabBar';
    vidFacility_MoabBar    = 1751;

  const
    tidBlock_MoabFuneralConstr = 'MoabFuneralConstr';
    tidBlock_MoabFuneral       = 'MoabFuneral';
    tidFacility_MoabFuneral    = 'MoabFuneral';
    vidFacility_MoabFuneral    = 7301;

  const
    tidBlock_MoabHHAStoreConstr = 'MoabHHAsStoreConstr';
    tidBlock_MoabHHAStore       = 'MoabHHAsStore';
    tidFacility_MoabHHAStore    = 'MoabHHAsStore';
    vidFacility_MoabHHAStore    = 1761;

  const
    tidBlock_MoabRestaurantConstr = 'MoabRestaurantConstr';
    tidBlock_MoabRestaurant       = 'MoabRestaurant';
    tidFacility_MoabRestaurant    = 'MoabRestaurant';
    vidFacility_MoabRestaurant    = 1751; // >>

  const
    tidBlock_MoabMovieConstr = 'MoabMovieConstr';
    tidBlock_MoabMovie       = 'MoabMovie';
    tidFacility_MoabMovie    = 'MoabMovie';
    vidFacility_MoabMovie    = 1771; // >>

  const
    tidBlock_MoabGeneralHeadquarterConstr = 'MoabGeneralHeadquarterConstr';
    tidBlock_MoabGeneralHeadquarter       = 'MoabGeneralHeadquarter';
    tidFacility_MoabGeneralHeadquarter    = 'MoabGeneralHeadquarter';
    vidFacility_MoabGeneralHeadquarter    = 1901;

  const
    tidBlock_MoabIndHeadquarterConstr = 'MoabIndHeadquarterConstr';
    tidBlock_MoabIndHeadquarter       = 'MoabIndHeadquarter';
    tidFacility_MoabIndHeadquarter    = 'MoabIndHeadquarter';
    vidFacility_MoabIndHeadquarter    = 1911;

  const
    tidBlock_MoabIllusionHeadquarterConstr = 'MoabIllusionHeadquarterConstr';
    tidBlock_MoabIllusionHeadquarter       = 'MoabIllusionHeadquarter';
    tidFacility_MoabIllusionHeadquarter    = 'MoabIllusionHeadquarter';
    vidFacility_MoabIllusionHeadquarter    = 1921;

  const
    tidBlock_MoabResHeadquarterConstr = 'MoabResHeadquarterConstr';
    tidBlock_MoabResHeadquarter       = 'MoabResHeadquarter';
    tidFacility_MoabResHeadquarter    = 'MoabResHeadquarter';
    vidFacility_MoabResHeadquarter    = 1931;

  const
    tidBlock_MoabCorrectionHeadquarterConstr = 'MoabCorrectionHeadquarterConstr';
    tidBlock_MoabCorrectionHeadquarter       = 'MoabCorrectionHeadquarter';
    tidFacility_MoabCorrectionHeadquarter    = 'MoabCorrectionHeadquarter';
    vidFacility_MoabCorrectionHeadquarter    = 1941;

  const
    tidBlock_MoabBigCorrectionalConstr = 'MoabBigCorrectionalConstr';
    tidBlock_MoabBigCorrectional       = 'MoabBigCorrectional';
    tidFacility_MoabBigCorrectional    = 'MoabBigCorrectional';
    vidFacility_MoabBigCorrectional    = 1021;

  const
    tidBlock_MoabSmallParkConstr = 'MoabSmallParkConstr';
    tidBlock_MoabSmallPark       = 'MoabSmallPark';
    tidFacility_MoabSmallPark    = 'MoabSmallPark';
    vidFacility_MoabSmallPark    = 2831;

  const
    tidBlock_MoabMediumParkConstr = 'MoabMediumParkConstr';
    tidBlock_MoabMediumPark       = 'MoabMediumPark';
    tidFacility_MoabMediumPark    = 'MoabMediumPark';
    vidFacility_MoabMediumPark    = 2841;

  const
    tidBlock_MoabCentralParkConstr = 'MoabCentralParkConstr';
    tidBlock_MoabCentralPark       = 'MoabCentralPark';
    tidFacility_MoabCentralPark    = 'MoabCentralPark';
    vidFacility_MoabCentralPark    = 2851;

  const
    tidBlock_MoabComputingIndustryConstr = 'MoabComputingIndustryConstr';
    tidBlock_MoabComputingIndustry       = 'MoabComputingIndustry';
    tidFacility_MoabComputingIndustry    = 'MoabComputingIndustry';
    vidFacility_MoabComputingIndustry    = 1031;

  const
    tidBlock_MoabLegalServicesConstr = 'MoabLegalServicesConstr';
    tidBlock_MoabLegalServices       = 'MoabLegalServices';
    tidFacility_MoabLegalServices    = 'MoabLegalServices';
    vidFacility_MoabLegalServices    = 1041;

  const
    tidBlock_MoabOfficeBuildingConstrA = 'MoabOfficeBuildingConstrA';
    tidBlock_MoabOfficeBuildingA       = 'MoabOfficeBuildingA';
    tidFacility_MoabOfficeBuildingA    = 'MoabOfficeBuildingA';
    vidFacility_MoabOfficeBuildingA    = 1951;

  const
    tidBlock_MoabOfficeBuildingConstrB = 'MoabOfficeBuildingConstrB';
    tidBlock_MoabOfficeBuildingB       = 'MoabOfficeBuildingB';
    tidFacility_MoabOfficeBuildingB    = 'MoabOfficeBuildingB';
    vidFacility_MoabOfficeBuildingB    = 1961;

  const
    tidBlock_MoabTVStationConstr = 'MoabTVStationConstr';
    tidBlock_MoabTVStation       = 'MoabTVStation';
    tidFacility_MoabTVStation    = 'MoabTVStation';
    vidFacility_MoabTVStation    = 1981;

  const
    tidBlock_MoabTVAntennaConstr = 'MoabTVAntennaConstr';
    tidBlock_MoabTVAntenna       = 'MoabTVAntenna';
    tidFacility_MoabTVAntenna    = 'MoabTVAntenna';
    vidFacility_MoabTVAntenna    = 1991;

  const
    tidBlock_MoabLibertyConstr = 'MoabLibertyConstr';
    tidBlock_MoabLiberty       = 'MoabLiberty';
    tidFacility_MoabLiberty    = 'MoabLiberty';
    vidFacility_MoabLiberty    = 6011;

  const
    tidBlock_MoabTowerConstr = 'MoabTowerConstr';
    tidBlock_MoabTower       = 'MoabTower';
    tidFacility_MoabTower    = 'MoabTower';
    vidFacility_MoabTower    = 6021;


implementation

end.

