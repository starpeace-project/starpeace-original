unit MarikoConst;

interface

  uses
    Standards;

  const
    tidClusterName_Mariko = 'Mariko';

  const
    tidFacilityKind_MarikoDistributedDirection = tidClusterName_Mariko + tidFacilityKind_DistributedDirection;
    tidFacilityKind_MarikoFarms                = tidClusterName_Mariko + tidFacilityKind_Farms;
    tidFacilityKind_MarikoIndustrialFacilities = tidClusterName_Mariko + tidFacilityKind_IndustrialFacilities;
    tidFacilityKind_MarikoResidentials         = tidClusterName_Mariko + tidFacilityKind_Residentials;
    tidFacilityKind_MarikoServiceFacilities    = tidClusterName_Mariko + tidFacilityKind_ServiceFacilities;
    tidFacilityKind_MarikoPublicFacilities     = tidClusterName_Mariko + tidFacilityKind_PublicFacilities;
    tidFacilityKind_MarikoBusinessFacilities   = tidClusterName_Mariko + tidFacilityKind_BusinessFacilities;
    tidFacilityKind_MarikoSpecial              = tidClusterName_Mariko + tidFacilityKind_Special;

  const
    tidBlock_MarikoTownHall    = tidClusterName_Mariko + 'TownHall';
    tidFacility_MarikoTownHall = tidClusterName_Mariko + 'TownHall';
    vidFacility_MarikoTownHall = 3500;

  const
    tidBlock_MarikoTradeCenter    = tidClusterName_Mariko + 'TradeCenter';
    tidFacility_MarikoTradeCenter = tidClusterName_Mariko + 'TradeCenter';
    vidFacility_MarikoTradeCenter = 3510;

  const
    tidBlock_MarikoLoCostHighClassConstr = 'MarikoHighClassLoCostConstr';
    tidBlock_MarikoLoCostHighClass       = 'MarikoHighClassLoCost';
    tidFacility_MarikoLoCostHighClass    = 'MarikoHighClassLoCost';
    vidFacility_MarikoLoCostHighClass    = 3451;

  const
    tidBlock_MarikoLoCostMiddleClassConstr = 'MarikoMiddleClassLoCostConstr';
    tidBlock_MarikoLoCostMiddleClass       = 'MarikoMiddleClassLoCost';
    tidFacility_MarikoLoCostMiddleClass    = 'MarikoMiddleClassLoCost';
    vidFacility_MarikoLoCostMiddleClass    = 3461;

  const
    tidBlock_MarikoLoCostLowClassConstr = 'MarikoLowClassLoCostConstr';
    tidBlock_MarikoLoCostLowClass       = 'MarikoLowClassLoCost';
    tidFacility_MarikoLoCostLowClass    = 'MarikoLowClassLoCost';
    vidFacility_MarikoLoCostLowClass    = 3471;

  const
    tidBlock_MarikoHighClassBuildingConstrA = 'MarikoHighClassBuildingConstrA';
    tidBlock_MarikoHighClassBuildingA       = 'MarikoHighClassBuildingA';
    tidFacility_MarikoHighClassBuildingA    = 'MarikoHighClassBuildingA';
    vidFacility_MarikoHighClassBuildingA    = 3301;

  const
    tidBlock_MarikoHighClassBuildingConstrB = 'MarikoHighClassBuildingConstrB';
    tidBlock_MarikoHighClassBuildingB       = 'MarikoHighClassBuildingB';
    tidFacility_MarikoHighClassBuildingB    = 'MarikoHighClassBuildingB';
    vidFacility_MarikoHighClassBuildingB    = 3311;

  const
    tidBlock_MarikoHighClassBuildingConstrC = 'MarikoHighClassBuildingConstrC';
    tidBlock_MarikoHighClassBuildingC       = 'MarikoHighClassBuildingC';
    tidFacility_MarikoHighClassBuildingC    = 'MarikoHighClassBuildingC';
    vidFacility_MarikoHighClassBuildingC    = 3321;

  const
    tidBlock_MarikoHighClassBuildingConstrD = 'MarikoHighClassBuildingConstrD';
    tidBlock_MarikoHighClassBuildingD       = 'MarikoHighClassBuildingD';
    tidFacility_MarikoHighClassBuildingD    = 'MarikoHighClassBuildingD';
    vidFacility_MarikoHighClassBuildingD    = 3331;

  const
    tidBlock_MarikoMiddleClassBuildingConstrA = 'MarikoMiddleClassBuildingConstrA';
    tidBlock_MarikoMiddleClassBuildingA       = 'MarikoMiddleClassBuildingA';
    tidFacility_MarikoMiddleClassBuildingA    = 'MarikoMiddleClassBuildingA';
    vidFacility_MarikoMiddleClassBuildingA    = 3351;

  const
    tidBlock_MarikoMiddleClassBuildingConstrB = 'MarikoMiddleClassBuildingConstrB';
    tidBlock_MarikoMiddleClassBuildingB       = 'MarikoMiddleClassBuildingB';
    tidFacility_MarikoMiddleClassBuildingB    = 'MarikoMiddleClassBuildingB';
    vidFacility_MarikoMiddleClassBuildingB    = 3361;

  const
    tidBlock_MarikoMiddleClassBuildingConstrC = 'MarikoMiddleClassBuildingConstrC';
    tidBlock_MarikoMiddleClassBuildingC       = 'MarikoMiddleClassBuildingC';
    tidFacility_MarikoMiddleClassBuildingC    = 'MarikoMiddleClassBuildingC';
    vidFacility_MarikoMiddleClassBuildingC    = 3371;

  const
    tidBlock_MarikoMiddleClassBuildingConstrD = 'MarikoMiddleClassBuildingConstrD';
    tidBlock_MarikoMiddleClassBuildingD       = 'MarikoMiddleClassBuildingD';
    tidFacility_MarikoMiddleClassBuildingD    = 'MarikoMiddleClassBuildingD';
    vidFacility_MarikoMiddleClassBuildingD    = 3381;

  const
    tidBlock_MarikoMiddleClassBuildingConstrE = 'MarikoMiddleClassBuildingConstrE';
    tidBlock_MarikoMiddleClassBuildingE       = 'MarikoMiddleClassBuildingE';
    tidFacility_MarikoMiddleClassBuildingE    = 'MarikoMiddleClassBuildingE';
    vidFacility_MarikoMiddleClassBuildingE    = 8501;

  {const
    tidBlock_MarikoMiddleClassBuildingConstrF = 'MarikoMiddleClassBuildingConstrF';
    tidBlock_MarikoMiddleClassBuildingF       = 'MarikoMiddleClassBuildingF';
    tidFacility_MarikoMiddleClassBuildingF    = 'MarikoMiddleClassBuildingF';
    vidFacility_MarikoMiddleClassBuildingF    = 8511;}

  const
    tidBlock_MarikoMiddleClassBuildingConstrG = 'MarikoMiddleClassBuildingConstrG';
    tidBlock_MarikoMiddleClassBuildingG       = 'MarikoMiddleClassBuildingG';
    tidFacility_MarikoMiddleClassBuildingG    = 'MarikoMiddleClassBuildingG';
    vidFacility_MarikoMiddleClassBuildingG    = 8521;

  const
    tidBlock_MarikoLowClassBuildingConstrA = 'MarikoLowClassBuildingConstrA';
    tidBlock_MarikoLowClassBuildingA       = 'MarikoLowClassBuildingA';
    tidFacility_MarikoLowClassBuildingA    = 'MarikoLowClassBuildingA';
    vidFacility_MarikoLowClassBuildingA    = 3401;

  const
    tidBlock_MarikoLowClassBuildingConstrB = 'MarikoLowClassBuildingConstrB';
    tidBlock_MarikoLowClassBuildingB       = 'MarikoLowClassBuildingB';
    tidFacility_MarikoLowClassBuildingB    = 'MarikoLowClassBuildingB';
    vidFacility_MarikoLowClassBuildingB    = 3411;

  const
    tidBlock_MarikoLowClassBuildingConstrC = 'MarikoLowClassBuildingConstrC';
    tidBlock_MarikoLowClassBuildingC       = 'MarikoLowClassBuildingC';
    tidFacility_MarikoLowClassBuildingC    = 'MarikoLowClassBuildingC';
    vidFacility_MarikoLowClassBuildingC    = 3421;

  const
    tidBlock_MarikoLowClassBuildingConstrD = 'MarikoLowClassBuildingConstrD';
    tidBlock_MarikoLowClassBuildingD       = 'MarikoLowClassBuildingD';
    tidFacility_MarikoLowClassBuildingD    = 'MarikoLowClassBuildingD';
    vidFacility_MarikoLowClassBuildingD    = 3431;

  const
    tidBlock_MarikoLowClassBuildingConstrE = 'MarikoLowClassBuildingConstrE';
    tidBlock_MarikoLowClassBuildingE       = 'MarikoLowClassBuildingE';
    tidFacility_MarikoLowClassBuildingE    = 'MarikoLowClassBuildingE';
    vidFacility_MarikoLowClassBuildingE    = 8511;

  const
    tidBlock_MarikoFarmConstr = 'MarikoFarmConstr';
    tidBlock_MarikoFarm       = 'MarikoFarm';
    tidFacility_MarikoFarm    = 'MarikoFarm';
    vidFacility_MarikoFarm    = 3111;

  const
    tidBlock_MarikoSmallFarmConstr = 'MarikoSmallFarmConstr';
    tidBlock_MarikoSmallFarm       = 'MarikoSmallFarm';
    tidFacility_MarikoSmallFarm    = 'MarikoSmallFarm';
    vidFacility_MarikoSmallFarm    = 3115;

  const
    tidBlock_MarikoMineConstr = 'MarikoMineConstr';
    tidBlock_MarikoMine       = 'MarikoMine';
    tidFacility_MarikoMine    = 'MarikoMine';
    vidFacility_MarikoMine    = 3121;

  const
    tidBlock_MarikoSmallMineConstr = 'MarikoSmallMineConstr';
    tidBlock_MarikoSmallMine       = 'MarikoSmallMine';
    tidFacility_MarikoSmallMine    = 'MarikoSmallMine';
    vidFacility_MarikoSmallMine    = 3125;

  const
    tidBlock_MarikoChemMineConstr = 'MarikoChemMineConstr';
    tidBlock_MarikoChemMine       = 'MarikoChemMine';
    tidFacility_MarikoChemMine    = 'MarikoChemMine';
    vidFacility_MarikoChemMine    = 7211;

  const
    tidBlock_MarikoSmallChemMineConstr = 'MarikoSmallChemMineConstr';
    tidBlock_MarikoSmallChemMine       = 'MarikoSmallChemMine';
    tidFacility_MarikoSmallChemMine    = 'MarikoSmallChemMine';
    vidFacility_MarikoSmallChemMine    = 7215;

  const
    tidBlock_MarikoSiliconMineConstr = 'MarikoSiliconMineConstr';
    tidBlock_MarikoSiliconMine       = 'MarikoSiliconMine';
    tidFacility_MarikoSiliconMine    = 'MarikoSiliconMine';
    vidFacility_MarikoSiliconMine    = 7221;

  const
    tidBlock_MarikoSmallSiliconMineConstr = 'MarikoSmallSiliconMineConstr';
    tidBlock_MarikoSmallSiliconMine       = 'MarikoSmallSiliconMine';
    tidFacility_MarikoSmallSiliconMine    = 'MarikoSmallSiliconMine';
    vidFacility_MarikoSmallSiliconMine    = 7225;

  const
    tidBlock_MarikoStoneMineConstr = 'MarikoStoneMineConstr';
    tidBlock_MarikoStoneMine       = 'MarikoStoneMine';
    tidFacility_MarikoStoneMine    = 'MarikoStoneMine';
    vidFacility_MarikoStoneMine    = 7231;

  const
    tidBlock_MarikoSmallStoneMineConstr = 'MarikoSmallStoneMineConstr';
    tidBlock_MarikoSmallStoneMine       = 'MarikoSmallStoneMine';
    tidFacility_MarikoSmallStoneMine    = 'MarikoSmallStoneMine';
    vidFacility_MarikoSmallStoneMine    = 7235;

  const
    tidBlock_MarikoCoalMineConstr = 'MarikoCoalMineConstr';
    tidBlock_MarikoCoalMine       = 'MarikoCoalMine';
    tidFacility_MarikoCoalMine    = 'MarikoCoalMine';
    vidFacility_MarikoCoalMine    = 7241;

  const
    tidBlock_MarikoSmallCoalMineConstr = 'MarikoSmallCoalMineConstr';
    tidBlock_MarikoSmallCoalMine       = 'MarikoSmallCoalMine';
    tidFacility_MarikoSmallCoalMine    = 'MarikoSmallCoalMine';
    vidFacility_MarikoSmallCoalMine    = 7245;

  const
    tidBlock_MarikoClothingsConstr = 'MarikoClothingsConstr';
    tidBlock_MarikoClothings       = 'MarikoClothings';
    tidFacility_MarikoClothings    = 'MarikoClothings';
    vidFacility_MarikoClothings    = 3131;

  const
    tidBlock_MarikoSmallClothingsConstr = 'MarikoSmallClothingsConstr';
    tidBlock_MarikoSmallClothings       = 'MarikoSmallClothings';
    tidFacility_MarikoSmallClothings    = 'MarikoSmallClothings';
    vidFacility_MarikoSmallClothings    = 3135;

  const
    tidBlock_MarikoFoodProcConstr = 'MarikoFoodProcConstr';
    tidBlock_MarikoFoodProc       = 'MarikoFoodProc';
    tidFacility_MarikoFoodProc    = 'MarikoFoodProc';
    vidFacility_MarikoFoodProc    = 3141;

  const
    tidBlock_MarikoSmallFoodProcConstr = 'MarikoSmallFoodProcConstr';
    tidBlock_MarikoSmallFoodProc       = 'MarikoSmallFoodProc';
    tidFacility_MarikoSmallFoodProc    = 'MarikoSmallFoodProc';
    vidFacility_MarikoSmallFoodProc    = 3145;

  const
    tidBlock_MarikoMetalConstr = 'MarikoMetalConstr';
    tidBlock_MarikoMetal       = 'MarikoMetal';
    tidFacility_MarikoMetal    = 'MarikoMetal';
    vidFacility_MarikoMetal    = 3151;

  const
    tidBlock_MarikoPlasticConstr = 'MarikoPlasticConstr';
    tidBlock_MarikoPlastic       = 'MarikoPlastic';
    tidFacility_MarikoPlastic    = 'MarikoPlastic';
    vidFacility_MarikoPlastic    = 3261;

  const
    tidBlock_MarikoSmallMetalConstr = 'MarikoSmallMetalConstr';
    tidBlock_MarikoSmallMetal       = 'MarikoSmallMetal';        
    tidFacility_MarikoSmallMetal    = 'MarikoSmallMetal';
    vidFacility_MarikoSmallMetal    = 3155;

  const
    tidBlock_MarikoChemicalConstr = 'MarikoChemicalConstr';
    tidBlock_MarikoChemical       = 'MarikoChemical';
    tidFacility_MarikoChemical    = 'MarikoChemical';
    vidFacility_MarikoChemical    = 3161;

  const
    tidBlock_MarikoSmallChemicalConstr = 'MarikoSmallChemicalConstr';
    tidBlock_MarikoSmallChemical       = 'MarikoSmallChemical';
    tidFacility_MarikoSmallChemical    = 'MarikoSmallChemical';
    vidFacility_MarikoSmallChemical    = 3165;

  const
    tidBlock_MarikoTextileConstr = 'MarikoTextileConstr';
    tidBlock_MarikoTextile       = 'MarikoTextile';
    tidFacility_MarikoTextile    = 'MarikoTextile';
    vidFacility_MarikoTextile    = 3171;

  const
    tidBlock_MarikoSmallTextileConstr = 'MarikoSmallTextileConstr';
    tidBlock_MarikoSmallTextile       = 'MarikoSmallTextile';
    tidFacility_MarikoSmallTextile    = 'MarikoSmallTextile';
    vidFacility_MarikoSmallTextile    = 3175;

  const
    tidBlock_MarikoElectronicConstr = 'MarikoElectronicConstr';
    tidBlock_MarikoElectronic       = 'MarikoElectronic';
    tidFacility_MarikoElectronic    = 'MarikoElectronic';
    vidFacility_MarikoElectronic    = 3181;

  const
    tidBlock_MarikoSmallElectronicConstr = 'MarikoSmallElectronicConstr';
    tidBlock_MarikoSmallElectronic       = 'MarikoSmallElectronic';
    tidFacility_MarikoSmallElectronic    = 'MarikoSmallElectronic';
    vidFacility_MarikoSmallElectronic    = 3185;

  const
    tidBlock_MarikoCarIndustryConstr = 'MarikoCarIndustryConstr';
    tidBlock_MarikoCarIndustry       = 'MarikoCarIndustry';
    tidFacility_MarikoCarIndustry    = 'MarikoCarIndustry';
    vidFacility_MarikoCarIndustry    = 3191;

  const
    tidBlock_MarikoHeavyConstr = 'MarikoHeavyConstr';
    tidBlock_MarikoHeavy       = 'MarikoHeavy';
    tidFacility_MarikoHeavy    = 'MarikoHeavy';
    vidFacility_MarikoHeavy    = 3201;

  const
    tidBlock_MarikoConstructionConstr = 'MarikoConstructionConstr';
    tidBlock_MarikoConstruction       = 'MarikoConstruction';
    tidFacility_MarikoConstruction    = 'MarikoConstruction';
    vidFacility_MarikoConstruction    = 3211;

  const
    tidBlock_MarikoComputingIndustryConstr = 'MarikoComputingIndustryConstr';
    tidBlock_MarikoComputingIndustry       = 'MarikoComputingIndustry';
    tidFacility_MarikoComputingIndustry    = 'MarikoComputingIndustry';
    vidFacility_MarikoComputingIndustry    = 3221;

  const
    tidBlock_MarikoHHAIndustryConstr = 'MarikoHHAIndustryConstr';
    tidBlock_MarikoHHAIndustry       = 'MarikoHHAIndustry';
    tidFacility_MarikoHHAIndustry    = 'MarikoHHAIndustry';
    vidFacility_MarikoHHAIndustry    = 3231;

  const
    tidBlock_MarikoSmallHHAIndustryConstr = 'MarikoSmallHHAIndustryConstr';
    tidBlock_MarikoSmallHHAIndustry       = 'MarikoSmallHHAIndustry';
    tidFacility_MarikoSmallHHAIndustry    = 'MarikoSmallHHAIndustry';
    vidFacility_MarikoSmallHHAIndustry    = 3235;

  const
    tidBlock_MarikoLegalServicesConstr = 'MarikoLegalServicesConstr';
    tidBlock_MarikoLegalServices       = 'MarikoLegalServices';
    tidFacility_MarikoLegalServices    = 'MarikoLegalServices';
    vidFacility_MarikoLegalServices    = 3241;

  const
    tidBlock_MarikoBusinessMachineConstr = 'MarikoBusinessMachineConstr';
    tidBlock_MarikoBusinessMachine       = 'MarikoBusinessMachine';
    tidFacility_MarikoBusinessMachine    = 'MarikoBusinessMachine';
    vidFacility_MarikoBusinessMachine    = 3251;

  const
    tidBlock_MarikoFoodStoreConstr = 'MarikoFoodStoreConstr';
    tidBlock_MarikoFoodStore       = 'MarikoFoodStore';
    tidFacility_MarikoFoodStore    = 'MarikoFoodStore';
    vidFacility_MarikoFoodStore    = 3601;

  const
    tidBlock_MarikoClothesStoreConstr = 'MarikoClothesStoreConstr';
    tidBlock_MarikoClothesStore       = 'MarikoClothesStore';
    tidFacility_MarikoClothesStore    = 'MarikoClothesStore';
    vidFacility_MarikoClothesStore    = 3631;

  const
    tidBlock_MarikoHHAStoreConstr = 'MarikoHHAsStoreConstr';
    tidBlock_MarikoHHAStore       = 'MarikoHHAsStore';
    tidFacility_MarikoHHAStore    = 'MarikoHHAsStore';
    vidFacility_MarikoHHAStore    = 3661;

  const
    tidBlock_MarikoComputerStoreConstr = 'MarikoComputersStoreConstr';
    tidBlock_MarikoComputerStore       = 'MarikoComputersStore';
    tidFacility_MarikoComputerStore    = 'MarikoComputersStore';
    vidFacility_MarikoComputerStore    = 7281;

  const
    tidBlock_MarikoCarStoreConstr = 'MarikoCarStoreConstr';
    tidBlock_MarikoCarStore       = 'MarikoCarStore';
    tidFacility_MarikoCarStore    = 'MarikoCarStore';
    vidFacility_MarikoCarStore    = 3691;

  const
    tidBlock_MarikoCDStoreConstr = 'MarikoCDStoreConstr';
    tidBlock_MarikoCDStore       = 'MarikoCDStore';
    tidFacility_MarikoCDStore    = 'MarikoCDStore';
    vidFacility_MarikoCDStore    = 7296;

  const
    tidBlock_MarikoSupermarketConstrA = 'MarikoSupermarketConstrA';
    tidBlock_MarikoSupermarketA       = 'MarikoSupermarketA';
    tidFacility_MarikoSupermarketA    = 'MarikoSupermarketA';
    vidFacility_MarikoSupermarketA    = 3721;

  const
    tidBlock_MarikoSupermarketConstrB = 'MarikoSupermarketConstrB';
    tidBlock_MarikoSupermarketB       = 'MarikoSupermarketB';
    tidFacility_MarikoSupermarketB    = 'MarikoSupermarketB';
    vidFacility_MarikoSupermarketB    = 3731;

  const
    tidBlock_MarikoSupermarketConstrC = 'MarikoSupermarketConstrC';
    tidBlock_MarikoSupermarketC       = 'MarikoSupermarketC';
    tidFacility_MarikoSupermarketC    = 'MarikoSupermarketC';
    vidFacility_MarikoSupermarketC    = 3741;

  const
    tidBlock_MarikoBarConstr = 'MarikoBarConstr';
    tidBlock_MarikoBar       = 'MarikoBar';
    tidFacility_MarikoBar    = 'MarikoBar';
    vidFacility_MarikoBar    = 3751;

  const
    tidBlock_MarikoRestaurantConstr = 'MarikoRestaurantConstr';
    tidBlock_MarikoRestaurant       = 'MarikoRestaurant';
    tidFacility_MarikoRestaurant    = 'MarikoRestaurant';
    vidFacility_MarikoRestaurant    = 3771;

  const
    tidBlock_MarikoHospitalConstr = 'MarikoHospitalConstr';
    tidBlock_MarikoHospital       = 'MarikoHospital';
    tidFacility_MarikoHospital    = 'MarikoHospital';
    vidFacility_MarikoHospital    = 3801;

  const
    tidBlock_MarikoSchoolConstr = 'MarikoSchoolConstr';
    tidBlock_MarikoSchool       = 'MarikoSchool';
    tidFacility_MarikoSchool    = 'MarikoSchool';
    vidFacility_MarikoSchool    = 3811;

  const
    tidBlock_MarikoPoliceConstr = 'MarikoPoliceConstr';
    tidBlock_MarikoPolice       = 'MarikoPolice';
    tidFacility_MarikoPolice    = 'MarikoPolice';
    vidFacility_MarikoPolice    = 3821;

  const
    tidBlock_MarikoFireConstr = 'MarikoFireConstr';
    tidBlock_MarikoFire       = 'MarikoFire';
    tidFacility_MarikoFire    = 'MarikoFire';
    vidFacility_MarikoFire    = 3831;

  const
    tidBlock_MKOSmallParkConstr = 'MKOSmallParkConstr';
    tidBlock_MKOSmallPark       = 'MKOSmallPark';
    tidFacility_MKOSmallPark    = 'MKOSmallPark';
    vidFacility_MKOSmallPark    = 2831;

  const
    tidBlock_MKOMediumParkConstr = 'MKOMediumParkConstr';
    tidBlock_MKOMediumPark       = 'MKOMediumPark';
    tidFacility_MKOMediumPark    = 'MKOMediumPark';
    vidFacility_MKOMediumPark    = 2841;

  const
    tidBlock_MKOCentralParkConstr = 'MKOCentralParkConstr';
    tidBlock_MKOCentralPark       = 'MKOCentralPark';
    tidFacility_MKOCentralPark    = 'MKOCentralPark';
    vidFacility_MKOCentralPark    = 2851;

  const
    tidBlock_MarikoGeneralHeadquarterConstr = 'MarikoGeneralHeadquarterConstr';
    tidBlock_MarikoGeneralHeadquarter       = 'MarikoGeneralHeadquarter';
    tidFacility_MarikoGeneralHeadquarter    = 'MarikoGeneralHeadquarter';
    vidFacility_MarikoGeneralHeadquarter    = 3901;

  const
    tidBlock_MarikoIndHeadquarterConstr = 'MarikoIndHeadquarterConstr';
    tidBlock_MarikoIndHeadquarter       = 'MarikoIndHeadquarter';
    tidFacility_MarikoIndHeadquarter    = 'MarikoIndHeadquarter';
    vidFacility_MarikoIndHeadquarter    = 3911;

  const
    tidBlock_MarikoServiceHeadquarterConstr = 'MarikoServiceHeadquarterConstr';
    tidBlock_MarikoServiceHeadquarter       = 'MarikoServiceHeadquarter';
    tidFacility_MarikoServiceHeadquarter    = 'MarikoServiceHeadquarter';
    vidFacility_MarikoServiceHeadquarter    = 3911; // 3921;

  const
    tidBlock_MarikoResHeadquarterConstr = 'MarikoResHeadquarterConstr';
    tidBlock_MarikoResHeadquarter       = 'MarikoResHeadquarter';
    tidFacility_MarikoResHeadquarter    = 'MarikoResHeadquarter';
    vidFacility_MarikoResHeadquarter    = 3911; // 3931;

  const
    tidBlock_MarikoPubHeadquarterConstr = 'MarikoPubHeadquarterConstr';
    tidBlock_MarikoPubHeadquarter       = 'MarikoPubHeadquarter';
    tidFacility_MarikoPubHeadquarter    = 'MarikoPubHeadquarter';
    vidFacility_MarikoPubHeadquarter    = 3911; // 3941;

  const
    tidBlock_MKOOfficeBuildingConstrA = 'MKOOfficeBuildingConstrA';
    tidBlock_MKOOfficeBuildingA       = 'MKOOfficeBuildingA';
    tidFacility_MKOOfficeBuildingA    = 'MKOOfficeBuildingA';
    vidFacility_MKOOfficeBuildingA    = 3951;

  const
    tidBlock_MKOOfficeBuildingConstrB = 'MKOOfficeBuildingConstrB';
    tidBlock_MKOOfficeBuildingB       = 'MKOOfficeBuildingB';
    tidFacility_MKOOfficeBuildingB    = 'MKOOfficeBuildingB';
    vidFacility_MKOOfficeBuildingB    = 3961;

  const
    tidBlock_MKOOfficeBuildingConstrC = 'MKOOfficeBuildingConstrC';
    tidBlock_MKOOfficeBuildingC       = 'MKOOfficeBuildingC';
    tidFacility_MKOOfficeBuildingC    = 'MKOOfficeBuildingC';
    vidFacility_MKOOfficeBuildingC    = 3971;

  const
    tidBlock_MKOOfficeBuildingConstrD = 'MKOOfficeBuildingConstrD';
    tidBlock_MKOOfficeBuildingD       = 'MKOOfficeBuildingD';
    tidFacility_MKOOfficeBuildingD    = 'MKOOfficeBuildingD';
    vidFacility_MKOOfficeBuildingD    = 3981;

  const
    tidBlock_MKOOfficeBuildingConstrE = 'MKOOfficeBuildingConstrE';
    tidBlock_MKOOfficeBuildingE       = 'MKOOfficeBuildingE';
    tidFacility_MKOOfficeBuildingE    = 'MKOOfficeBuildingE';
    vidFacility_MKOOfficeBuildingE    = 3991;

  const
    tidBlock_MKOOfficeBuildingConstrF = 'MKOOfficeBuildingConstrF';
    tidBlock_MKOOfficeBuildingF       = 'MKOOfficeBuildingF';
    tidFacility_MKOOfficeBuildingF    = 'MKOOfficeBuildingF';
    vidFacility_MKOOfficeBuildingF    = 8531;

  const
    tidBlock_MarikoTVStationConstr = 'MarikoTVStationConstr';
    tidBlock_MarikoTVStation       = 'MarikoTVStation';
    tidFacility_MarikoTVStation    = 'MarikoTVStation';
    vidFacility_MarikoTVStation    = 3881;

  const
    tidBlock_MarikoTVAntennaConstr = 'MarikoTVAntennaConstr';
    tidBlock_MarikoTVAntenna       = 'MarikoTVAntenna';
    tidFacility_MarikoTVAntenna    = 'MarikoTVAntenna';
    vidFacility_MarikoTVAntenna    = 3891;

  const
    tidBlock_MarikoLibertyConstr = 'MarikoLibertyConstr';
    tidBlock_MarikoLiberty       = 'MarikoLiberty';
    tidFacility_MarikoLiberty    = 'MarikoLiberty';
    vidFacility_MarikoLiberty    = 6011;

  const
    tidBlock_MarikoTowerConstr = 'MarikoTowerConstr';
    tidBlock_MarikoTower       = 'MarikoTower';
    tidFacility_MarikoTower    = 'MarikoTower';
    vidFacility_MarikoTower    = 6021;

  const
    tidBlock_MarikoCDConstr = 'MarikoCDPlantConstr';
    tidBlock_MarikoCD       = 'MarikoCDPlant';
    tidFacility_MarikoCD    = 'MarikoCDPlant';
    vidFacility_MarikoCD    = 8541;

implementation

end.

