unit DissidentConst;

interface

  uses
    Standards;

  const
    tidClusterName_Dissident = 'Dissidents';

  const
    tidFacilityKind_DisDistributedDirection = tidClusterName_Dissident + tidFacilityKind_DistributedDirection;
    tidFacilityKind_DisFarms                = tidClusterName_Dissident + tidFacilityKind_Farms;
    tidFacilityKind_DisIndustrialFacilities = tidClusterName_Dissident + tidFacilityKind_IndustrialFacilities;
    tidFacilityKind_DisResidentials         = tidClusterName_Dissident + tidFacilityKind_Residentials;
    tidFacilityKind_DisServiceFacilities    = tidClusterName_Dissident + tidFacilityKind_ServiceFacilities;
    tidFacilityKind_DisPublicFacilities     = tidClusterName_Dissident + tidFacilityKind_PublicFacilities;
    tidFacilityKind_DisBusinessFacilities   = tidClusterName_Dissident + tidFacilityKind_BusinessFacilities;
    tidFacilityKind_DisSpecial              = tidClusterName_Dissident + tidFacilityKind_Special;
    
  const
    tmeInvention_VeryShort = 7;  // NOTE: Research time is in hours!
    tmeInvention_Short     = 20;
    tmeInvention_Normal    = 33;
    tmeInvention_Long      = 60;
    tmeInvention_VeryLong  = 80;
    
  const
    tidBlock_DissTownHall    = tidClusterName_Dissident + 'TownHall';
    tidFacility_DissTownHall = tidClusterName_Dissident + 'TownHall';
    vidFacility_DissTownHall = 2500;
    
  const
    tidBlock_DissTradeCenter    = tidClusterName_Dissident + 'TradeCenter';
    tidFacility_DissTradeCenter = tidClusterName_Dissident + 'TradeCenter';
    vidFacility_DissTradeCenter = 2510;
    
  const
    tidBlock_DissLoCostHighClassConstr = 'DissHighClassLoCostConstr';
    tidBlock_DissLoCostHighClass       = 'DissHighClassLoCost';
    tidFacility_DissLoCostHighClass    = 'DissHighClassLoCost';
    vidFacility_DissLoCostHighClass    = 2481;
    
  const
    tidBlock_DissLoCostMiddleClassConstr = 'DissMiddleClassLoCostConstr';
    tidBlock_DissLoCostMiddleClass       = 'DissMiddleClassLoCost';
    tidFacility_DissLoCostMiddleClass    = 'DissMiddleClassLoCost';
    vidFacility_DissLoCostMiddleClass    = 2471;
    
  const
    tidBlock_DissLoCostLowClassConstr = 'DissLowClassLoCostConstr';
    tidBlock_DissLoCostLowClass       = 'DissLowClassLoCost';
    tidFacility_DissLoCostLowClass    = 'DissLowClassLoCost';
    vidFacility_DissLoCostLowClass    = 2461;
    
  const
    tidBlock_DissHighClassBuildingConstrA = 'DissHighClassBuildingConstrA';
    tidBlock_DissHighClassBuildingA       = 'DissHighClassBuildingA';
    tidFacility_DissHighClassBuildingA    = 'DissHighClassBuildingA';
    vidFacility_DissHighClassBuildingA    = 2441;
    
  const
    tidBlock_DissHighClassBuildingConstrB = 'DissHighClassBuildingConstrB';
    tidBlock_DissHighClassBuildingB       = 'DissHighClassBuildingB';
    tidFacility_DissHighClassBuildingB    = 'DissHighClassBuildingB';
    vidFacility_DissHighClassBuildingB    = 2451;
    
  const
    tidBlock_DissHighClassBuildingConstrC = 'DissHighClassBuildingConstrC';
    tidBlock_DissHighClassBuildingC       = 'DissHighClassBuildingC';
    tidFacility_DissHighClassBuildingC    = 'DissHighClassBuildingC';
    vidFacility_DissHighClassBuildingC    = 2541;
    
  const
    tidBlock_DissHighClassBuildingConstrD = 'DissHighClassBuildingConstrD';
    tidBlock_DissHighClassBuildingD       = 'DissHighClassBuildingD';
    tidFacility_DissHighClassBuildingD    = 'DissHighClassBuildingD';
    vidFacility_DissHighClassBuildingD    = 2551;
    
  const
    tidBlock_DissMiddleClassBuildingConstrA = 'DissMiddleClassBuildingConstrA';
    tidBlock_DissMiddleClassBuildingA       = 'DissMiddleClassBuildingA';
    tidFacility_DissMiddleClassBuildingA    = 'DissMiddleClassBuildingA';
    vidFacility_DissMiddleClassBuildingA    = 2421;
    
  const
    tidBlock_DissMiddleClassBuildingConstrB = 'DissMiddleClassBuildingConstrB';
    tidBlock_DissMiddleClassBuildingB       = 'DissMiddleClassBuildingB';
    tidFacility_DissMiddleClassBuildingB    = 'DissMiddleClassBuildingB';
    vidFacility_DissMiddleClassBuildingB    = 2431;
    
  const
    tidBlock_DissMiddleClassBuildingConstrC = 'DissMiddleClassBuildingConstrC';
    tidBlock_DissMiddleClassBuildingC       = 'DissMiddleClassBuildingC';
    tidFacility_DissMiddleClassBuildingC    = 'DissMiddleClassBuildingC';
    vidFacility_DissMiddleClassBuildingC    = 2561;
    
  const
    tidBlock_DissMiddleClassBuildingConstrD = 'DissMiddleClassBuildingConstrD';
    tidBlock_DissMiddleClassBuildingD       = 'DissMiddleClassBuildingD';
    tidFacility_DissMiddleClassBuildingD    = 'DissMiddleClassBuildingD';
    vidFacility_DissMiddleClassBuildingD    = 2341;
    
  const
    tidBlock_DissMiddleClassBuildingConstrE = 'DissMiddleClassBuildingConstrE';
    tidBlock_DissMiddleClassBuildingE       = 'DissMiddleClassBuildingE';
    tidFacility_DissMiddleClassBuildingE    = 'DissMiddleClassBuildingE';
    vidFacility_DissMiddleClassBuildingE    = 2351;
    
  const
    tidBlock_DissMiddleClassBuildingConstrF = 'DissMiddleClassBuildingConstrF';
    tidBlock_DissMiddleClassBuildingF       = 'DissMiddleClassBuildingF';
    tidFacility_DissMiddleClassBuildingF    = 'DissMiddleClassBuildingF';
    vidFacility_DissMiddleClassBuildingF    = 6501;


  const
    tidBlock_DissLowClassDomeConstrA = 'DissLowClassDomeConstrA';
    tidBlock_DissLowClassDomeA       = 'DissLowClassDomeA';
    tidFacility_DissLowClassDomeA    = 'DissLowClassDomeA';
    vidFacility_DissLowClassDomeA    = 2401;
    
  const
    tidBlock_DissLowClassDomeConstrB = 'DissLowClassDomeConstrB';
    tidBlock_DissLowClassDomeB       = 'DissLowClassDomeB';
    tidFacility_DissLowClassDomeB    = 'DissLowClassDomeB';
    vidFacility_DissLowClassDomeB    = 2411;
    
  const
    tidBlock_DissLowClassDomeConstrC = 'DissLowClassDomeConstrC';
    tidBlock_DissLowClassDomeC       = 'DissLowClassDomeC';
    tidFacility_DissLowClassDomeC    = 'DissLowClassDomeC';
    vidFacility_DissLowClassDomeC    = 2311;
    
  const
    tidBlock_DissLowClassDomeConstrD = 'DissLowClassDomeConstrD';
    tidBlock_DissLowClassDomeD       = 'DissLowClassDomeD';
    tidFacility_DissLowClassDomeD    = 'DissLowClassDomeD';
    vidFacility_DissLowClassDomeD    = 6511;

  const
    tidBlock_DissFarmConstr = 'DissFarmConstr';
    tidBlock_DissFarm       = 'DissFarm';
    tidFacility_DissFarm    = 'DissFarm';
    vidFacility_DissFarm    = 2111;
    
  const
    tidBlock_DissFarmSmallConstr = 'DissSmallFarmConstr';
    tidBlock_DissFarmSmall       = 'DissSmallFarm';
    tidFacility_DissFarmSmall    = 'DissSmallFarm';
    vidFacility_DissFarmSmall    = 2115;
    
  const
    tidBlock_DissMineConstr = 'DissMineConstr';
    tidBlock_DissMine       = 'DissMine';
    tidFacility_DissMine    = 'DissMine';
    vidFacility_DissMine    = 2121;

  const
    tidBlock_DissMineSmallConstr = 'DissMineSmallConstr';
    tidBlock_DissMineSmall       = 'DissMineSmall';
    tidFacility_DissMineSmall    = 'DissMineSmall';
    vidFacility_DissMineSmall    = 2125;

  const
    tidBlock_DissChemMineConstr = 'DissChemMineConstr';
    tidBlock_DissChemMine       = 'DissChemMine';
    tidFacility_DissChemMine    = 'DissChemMine';
    vidFacility_DissChemMine    = 7111;

  const
    tidBlock_DissChemMineSmallConstr = 'DissChemMineSmallConstr';
    tidBlock_DissChemMineSmall       = 'DissChemMineSmall';
    tidFacility_DissChemMineSmall    = 'DissChemMineSmall';
    vidFacility_DissChemMineSmall    = 7115;

  const
    tidBlock_DissSiliconMineConstr = 'DissSiliconMineConstr';
    tidBlock_DissSiliconMine       = 'DissSiliconMine';
    tidFacility_DissSiliconMine    = 'DissSiliconMine';
    vidFacility_DissSiliconMine    = 7121;

  const
    tidBlock_DissSiliconMineSmallConstr = 'DissSiliconMineSmallConstr';
    tidBlock_DissSiliconMineSmall       = 'DissSiliconMineSmall';
    tidFacility_DissSiliconMineSmall    = 'DissSiliconMineSmall';
    vidFacility_DissSiliconMineSmall    = 7125;

  const
    tidBlock_DissStoneMineConstr = 'DissStoneMineConstr';
    tidBlock_DissStoneMine       = 'DissStoneMine';
    tidFacility_DissStoneMine    = 'DissStoneMine';
    vidFacility_DissStoneMine    = 7131;

  const
    tidBlock_DissStoneMineSmallConstr = 'DissStoneMineSmallConstr';
    tidBlock_DissStoneMineSmall       = 'DissStoneMineSmall';
    tidFacility_DissStoneMineSmall    = 'DissStoneMineSmall';
    vidFacility_DissStoneMineSmall    = 7135;

  const
    tidBlock_DissCoalMineConstr = 'DissCoalMineConstr';
    tidBlock_DissCoalMine       = 'DissCoalMine';
    tidFacility_DissCoalMine    = 'DissCoalMine';
    vidFacility_DissCoalMine    = 7141;

  const
    tidBlock_DissCoalMineSmallConstr = 'DissCoalMineSmallConstr';
    tidBlock_DissCoalMineSmall       = 'DissCoalMineSmall';
    tidFacility_DissCoalMineSmall    = 'DissCoalMineSmall';
    vidFacility_DissCoalMineSmall    = 7145;

  const
    tidBlock_DissClothingsConstr = 'DissClothingsConstr';
    tidBlock_DissClothings       = 'DissClothings';
    tidFacility_DissClothings    = 'DissClothings';
    vidFacility_DissClothings    = 2131;

  const
    tidBlock_DissClothingsSmallConstr = 'DissClothingsSmallConstr';
    tidBlock_DissClothingsSmall       = 'DissClothingsSmall';
    tidFacility_DissClothingsSmall    = 'DissClothingsSmall';
    vidFacility_DissClothingsSmall    = 2135;
    
  const
    tidBlock_DissFoodProcConstr = 'DissFoodProcConstr';
    tidBlock_DissFoodProc       = 'DissFoodProc';
    tidFacility_DissFoodProc    = 'DissFoodProc';
    vidFacility_DissFoodProc    = 2141;

  const
    tidBlock_DissLiquorFactConstr = 'DissLiquorFactConstr';
    tidBlock_DissLiquorFact       = 'DissLiquorFact';
    tidFacility_DissLiquorFact    = 'DissLiquorFact';
    vidFacility_DissLiquorFact    = 2195;

  const
    tidBlock_DissFoodProcSmallConstr = 'DissFoodProcSmallConstr';
    tidBlock_DissFoodProcSmall       = 'DissFoodProcSmall';
    tidFacility_DissFoodProcSmall    = 'DissFoodProcSmall';
    vidFacility_DissFoodProcSmall    = 2145;

  const
    tidBlock_DissMetalConstr = 'DissMetalConstr';
    tidBlock_DissMetal       = 'DissMetal';
    tidFacility_DissMetal    = 'DissMetal';
    vidFacility_DissMetal    = 2151;

  const
    tidBlock_DissMetalSmallConstr = 'DissMetalSmallConstr';
    tidBlock_DissMetalSmall       = 'DissMetalSmall';
    tidFacility_DissMetalSmall    = 'DissMetalSmall';
    vidFacility_DissMetalSmall    = 2155;

  const
    tidBlock_DissChemicalConstr = 'DissChemicalConstr';
    tidBlock_DissChemical       = 'DissChemical';
    tidFacility_DissChemical    = 'DissChemical';
    vidFacility_DissChemical    = 2161;

  const
    tidBlock_DissChemicalSmallConstr = 'DissChemicalSmallConstr';
    tidBlock_DissChemicalSmall       = 'DissChemicalSmall';
    tidFacility_DissChemicalSmall    = 'DissChemicalSmall';
    vidFacility_DissChemicalSmall    = 2165;

  const
    tidBlock_DissTextileConstr = 'DissTextileConstr';
    tidBlock_DissTextile       = 'DissTextile';
    tidFacility_DissTextile    = 'DissTextile';
    vidFacility_DissTextile    = 2171;

  const
    tidBlock_DissTextileSmallConstr = 'DissTextileSmallConstr';
    tidBlock_DissTextileSmall       = 'DissTextileSmall';
    tidFacility_DissTextileSmall    = 'DissTextileSmall';
    vidFacility_DissTextileSmall    = 2175;

  const
    tidBlock_DissElectronicConstr = 'DissElectronicConstr';
    tidBlock_DissElectronic       = 'DissElectronic';
    tidFacility_DissElectronic    = 'DissElectronic';
    vidFacility_DissElectronic    = 2181;

  const
    tidBlock_DissElectronicSmallConstr = 'DissElectronicSmallConstr';
    tidBlock_DissElectronicSmall       = 'DissElectronicSmall';
    tidFacility_DissElectronicSmall    = 'DissElectronicSmall';
    vidFacility_DissElectronicSmall    = 2185;

  const
    tidBlock_DissCarIndustryConstr = 'DissCarIndustryConstr';
    tidBlock_DissCarIndustry       = 'DissCarIndustry';
    tidFacility_DissCarIndustry    = 'DissCarIndustry';
    vidFacility_DissCarIndustry    = 2191;

  const
    tidBlock_DissHeavyConstr = 'DissHeavyConstr';
    tidBlock_DissHeavy       = 'DissHeavy';
    tidFacility_DissHeavy    = 'DissHeavy';
    vidFacility_DissHeavy    = 2201;

  const
    tidBlock_DissConstructionConstr = 'DissConstructionConstr';
    tidBlock_DissConstruction       = 'DissConstruction';
    tidFacility_DissConstruction    = 'DissConstruction';
    vidFacility_DissConstruction    = 2211;

  const
    tidBlock_DissComputingIndustryConstr = 'DissComputingIndustryConstr';
    tidBlock_DissComputingIndustry       = 'DissComputingIndustry';
    tidFacility_DissComputingIndustry    = 'DissComputingIndustry';
    vidFacility_DissComputingIndustry    = 2221;

  const
    tidBlock_DissHHAIndustryConstr = 'DissHHAIndustryConstr';
    tidBlock_DissHHAIndustry       = 'DissHHAIndustry';
    tidFacility_DissHHAIndustry    = 'DissHHAIndustry';
    vidFacility_DissHHAIndustry    = 2231;
    
  const
    tidBlock_DissHHAIndustrySmallConstr = 'DissHHAIndustrySmallConstr';
    tidBlock_DissHHAIndustrySmall       = 'DissHHAIndustrySmall';
    tidFacility_DissHHAIndustrySmall    = 'DissHHAIndustrySmall';
    vidFacility_DissHHAIndustrySmall    = 2231;
    
  const
    tidBlock_DissToyIndustryConstr = 'DissToyIndustryConstr';
    tidBlock_DissToyIndustry       = 'DissToyIndustry';
    tidFacility_DissToyIndustry    = 'DissToyIndustry';
    vidFacility_DissToyIndustry    = 2791;

  const
    tidBlock_DissLegalServicesConstr = 'DissLegalServicesConstr';
    tidBlock_DissLegalServices       = 'DissLegalServices';
    tidFacility_DissLegalServices    = 'DissLegalServices';
    vidFacility_DissLegalServices    = 2241;
    
  const
    tidBlock_DissBusinessMachineConstr = 'DissBusinessMachineConstr';
    tidBlock_DissBusinessMachine       = 'DissBusinessMachine';
    tidFacility_DissBusinessMachine    = 'DissBusinessMachine';
    vidFacility_DissBusinessMachine    = 2251;
    
  const
    tidBlock_DissLumberMillConstr = 'DissLumberMillConstr';
    tidBlock_DissLumberMill       = 'DissLumberMill';
    tidFacility_DissLumberMill    = 'DissLumberMill';
    vidFacility_DissLumberMill    = 7201;

  const
    tidBlock_DissFurnitureIndustryConstr = 'DissFurnitureIndConstr';
    tidBlock_DissFurnitureIndustry       = 'DissFurnitureInd';
    tidFacility_DissFurnitureIndustry    = 'DissFurnitureInd';
    vidFacility_DissFurnitureIndustry    = 7251;

  const
    tidBlock_DissBankConstr = 'DissBankConstr';
    tidBlock_DissBank       = 'DissBank';
    tidFacility_DissBank    = 'DissBank';
    vidFacility_DissBank    = 2261;

  const
    tidBlock_DissFoodStoreConstr = 'DissFoodStoreConstr';
    tidBlock_DissFoodStore       = 'DissFoodStore';
    tidFacility_DissFoodStore    = 'DissFoodStore';
    vidFacility_DissFoodStore    = 2711;

  const
    tidBlock_DissCarStoreConstr = 'DissCarStoreConstr';
    tidBlock_DissCarStore       = 'DissCarStore';
    tidFacility_DissCarStore    = 'DissCarStore';
    vidFacility_DissCarStore    = 2721;

  const
    tidBlock_DissClothesStoreConstr = 'DissClothesStoreConstr';
    tidBlock_DissClothesStore       = 'DissClothesStore';
    tidFacility_DissClothesStore    = 'DissClothesStore';
    vidFacility_DissClothesStore    = 2731;

  const
    tidBlock_DissHHAStoreConstr = 'DissHHAsStoreConstr';
    tidBlock_DissHHAStore       = 'DissHHAsStore';
    tidFacility_DissHHAStore    = 'DissHHAsStore';
    vidFacility_DissHHAStore    = 2661;

  const
    tidBlock_DissToyStoreConstr = 'DissToysStoreConstr';
    tidBlock_DissToyStore       = 'DissToysStore';
    tidFacility_DissToyStore    = 'DissToysStore';
    vidFacility_DissToyStore    = 2891;

  const
    tidBlock_DissFurnitureStoreConstr = 'DissFurnituresStoreConstr';
    tidBlock_DissFurnitureStore       = 'DissFurnituresStore';
    tidFacility_DissFurnitureStore    = 'DissFurnituresStore';
    vidFacility_DissFurnitureStore    = 7261;

  const
    tidBlock_DissBookStoreConstr = 'DissBooksStoreConstr';
    tidBlock_DissBookStore       = 'DissBooksStore';
    tidFacility_DissBookStore    = 'DissBooksStore';
    vidFacility_DissBookStore    = 7271;

  const
    tidBlock_DissSupermarketConstrA = 'DissSupermarketConstrA';
    tidBlock_DissSupermarketA       = 'DissSupermarketA';
    tidFacility_DissSupermarketA    = 'DissSupermarketA';
    vidFacility_DissSupermarketA    = 2741;

  const
    tidBlock_DissSupermarketConstrB = 'DissSupermarketConstrB';
    tidBlock_DissSupermarketB       = 'DissSupermarketB';
    tidFacility_DissSupermarketB    = 'DissSupermarketB';
    vidFacility_DissSupermarketB    = 2761;

  const
    tidBlock_DissBarConstr = 'DissBarConstr';
    tidBlock_DissBar       = 'DissBar';
    tidFacility_DissBar    = 'DissBar';
    vidFacility_DissBar    = 2751;

  const
    tidBlock_DissFuneralConstr = 'DissFuneralConstr';
    tidBlock_DissFuneral       = 'DissFuneral';
    tidFacility_DissFuneral    = 'DissFuneral';
    vidFacility_DissFuneral    = 7291;

  const
    tidBlock_DissRestaurantConstr = 'DissRestaurantConstr';
    tidBlock_DissRestaurant       = 'DissRestaurant';
    tidFacility_DissRestaurant    = 'DissRestaurant';
    vidFacility_DissRestaurant    = 2771;

  const
    tidBlock_DissMovieConstr = 'DissMovieConstr';
    tidBlock_DissMovie       = 'DissMovie';
    tidFacility_DissMovie    = 'DissMovie';
    vidFacility_DissMovie    = 2781;

  const
    tidBlock_DissHospitalConstr = 'DissHospitalConstr';
    tidBlock_DissHospital       = 'DissHospital';
    tidFacility_DissHospital    = 'DissHospital';
    vidFacility_DissHospital    = 2801;

  const
    tidBlock_DissSchoolConstr = 'DissSchoolConstr';
    tidBlock_DissSchool       = 'DissSchool';
    tidFacility_DissSchool    = 'DissSchool';
    vidFacility_DissSchool    = 2811;

  const
    tidBlock_DissPoliceConstr = 'DissPoliceConstr';
    tidBlock_DissPolice       = 'DissPolice';
    tidFacility_DissPolice    = 'DissPolice';
    vidFacility_DissPolice    = 2821;
    
  const
    tidBlock_DissSmallParkConstr = 'DissSmallParkConstr';
    tidBlock_DissSmallPark       = 'DissSmallPark';
    tidFacility_DissSmallPark    = 'DissSmallPark';
    vidFacility_DissSmallPark    = 2831;
    
  const
    tidBlock_DissMediumParkConstr = 'DissMediumParkConstr';
    tidBlock_DissMediumPark       = 'DissMediumPark';
    tidFacility_DissMediumPark    = 'DissMediumPark';
    vidFacility_DissMediumPark    = 2841;

  const
    tidBlock_DissCentralParkConstr = 'DissCentralParkConstr';
    tidBlock_DissCentralPark       = 'DissCentralPark';
    tidFacility_DissCentralPark    = 'DissCentralPark';
    vidFacility_DissCentralPark    = 2851;
    
  const
    tidBlock_DissGeneralHeadquarterConstr = 'DissGeneralHeadquarterConstr';
    tidBlock_DissGeneralHeadquarter       = 'DissGeneralHeadquarter';
    tidFacility_DissGeneralHeadquarter    = 'DissGeneralHeadquarter';
    vidFacility_DissGeneralHeadquarter    = 2901;
    
  const
    tidBlock_DissIndHeadquarterConstr = 'DissIndHeadquarterConstr';
    tidBlock_DissIndHeadquarter       = 'DissIndHeadquarter';
    tidFacility_DissIndHeadquarter    = 'DissIndHeadquarter';
    vidFacility_DissIndHeadquarter    = 2911;
    
  const
    tidBlock_DissServiceHeadquarterConstr = 'DissServiceHeadquarterConstr';
    tidBlock_DissServiceHeadquarter       = 'DissServiceHeadquarter';
    tidFacility_DissServiceHeadquarter    = 'DissServiceHeadquarter';
    vidFacility_DissServiceHeadquarter    = 2921;
    
  const
    tidBlock_DissResHeadquarterConstr = 'DissResHeadquarterConstr';
    tidBlock_DissResHeadquarter       = 'DissResHeadquarter';
    tidFacility_DissResHeadquarter    = 'DissResHeadquarter';
    vidFacility_DissResHeadquarter    = 2931;
    
  const
    tidBlock_DissPubHeadquarterConstr = 'DissPubHeadquarterConstr';
    tidBlock_DissPubHeadquarter       = 'DissPubHeadquarter';
    tidFacility_DissPubHeadquarter    = 'DissPubHeadquarter';
    vidFacility_DissPubHeadquarter    = 2941;
    
  const
    tidBlock_DissOfficeBuildingConstrA = 'DissOfficeBuildingConstrA';
    tidBlock_DissOfficeBuildingA       = 'DissOfficeBuildingA';
    tidFacility_DissOfficeBuildingA    = 'DissOfficeBuildingA';
    vidFacility_DissOfficeBuildingA    = 2951;
    
  const
    tidBlock_DissOfficeBuildingConstrB = 'DissOfficeBuildingConstrB';
    tidBlock_DissOfficeBuildingB       = 'DissOfficeBuildingB';
    tidFacility_DissOfficeBuildingB    = 'DissOfficeBuildingB';
    vidFacility_DissOfficeBuildingB    = 2961;
    
    
  const
    tidBlock_DissOfficeBuildingConstrC = 'DissOfficeBuildingConstrC';
    tidBlock_DissOfficeBuildingC       = 'DissOfficeBuildingC';
    tidFacility_DissOfficeBuildingC    = 'DissOfficeBuildingC';
    vidFacility_DissOfficeBuildingC    = 2971;
    
  const
    tidBlock_DissTVStationConstr = 'DissTVStationConstr';
    tidBlock_DissTVStation       = 'DissTVStation';
    tidFacility_DissTVStation    = 'DissTVStation';
    vidFacility_DissTVStation    = 2981;

  const
    tidBlock_DissTVAntennaConstr = 'DissTVAntennaConstr';
    tidBlock_DissTVAntenna       = 'DissTVAntenna';
    tidFacility_DissTVAntenna    = 'DissTVAntenna';
    vidFacility_DissTVAntenna    = 2991;

  const
    tidBlock_DissLibertyConstr = 'DissLibertyConstr';
    tidBlock_DissLiberty       = 'DissLiberty';
    tidFacility_DissLiberty    = 'DissLiberty';
    vidFacility_DissLiberty    = 6011;

  const
    tidBlock_DissTowerConstr = 'DissTowerConstr';
    tidBlock_DissTower       = 'DissTower';
    tidFacility_DissTower    = 'DissTower';
    vidFacility_DissTower    = 6021;


implementation

end.


