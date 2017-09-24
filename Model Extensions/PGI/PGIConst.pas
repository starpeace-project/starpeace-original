unit PGIConst;

interface

  uses
    Standards;

  const
    tidClusterName_PGI = 'PGI';

  const
    tidFacilityKind_PGIDistributedDirection = tidClusterName_PGI + tidFacilityKind_DistributedDirection;
    tidFacilityKind_PGIFarms                = tidClusterName_PGI + tidFacilityKind_Farms;
    tidFacilityKind_PGIIndustrialFacilities = tidClusterName_PGI + tidFacilityKind_IndustrialFacilities;
    tidFacilityKind_PGIResidentials         = tidClusterName_PGI + tidFacilityKind_Residentials;
    tidFacilityKind_PGIServiceFacilities    = tidClusterName_PGI + tidFacilityKind_ServiceFacilities;
    tidFacilityKind_PGIPublicFacilities     = tidClusterName_PGI + tidFacilityKind_PublicFacilities;
    tidFacilityKind_PGIBusinessFacilities   = tidClusterName_PGI + tidFacilityKind_BusinessFacilities;
    tidFacilityKind_PGISpecial              = tidClusterName_PGI + tidFacilityKind_Special;

  const
    tidBlock_PGITownHall    = tidClusterName_PGI + 'TownHall';
    tidFacility_PGITownHall = tidClusterName_PGI + 'TownHall';
    vidFacility_PGITownHall = 4500;

  const
    tidBlock_PGITradeCenter    = tidClusterName_PGI + 'TradeCenter';
    tidFacility_PGITradeCenter = tidClusterName_PGI + 'TradeCenter';
    vidFacility_PGITradeCenter = 4510;

  const
    tidBlock_PGILoCostHighClassConstr = 'PGIHighClassLoCostConstr';
    tidBlock_PGILoCostHighClass       = 'PGIHighClassLoCost';
    tidFacility_PGILoCostHighClass    = 'PGIHighClassLoCost';
    vidFacility_PGILoCostHighClass    = 4451;

  const
    tidBlock_PGILoCostMiddleClassConstr = 'PGIMiddleClassLoCostConstr';
    tidBlock_PGILoCostMiddleClass       = 'PGIMiddleClassLoCost';
    tidFacility_PGILoCostMiddleClass    = 'PGIMiddleClassLoCost';
    vidFacility_PGILoCostMiddleClass    = 4461;

  const
    tidBlock_PGILoCostLowClassConstr = 'PGILowClassLoCostConstr';
    tidBlock_PGILoCostLowClass       = 'PGILowClassLoCost';
    tidFacility_PGILoCostLowClass    = 'PGILowClassLoCost';
    vidFacility_PGILoCostLowClass    = 4471;

  const
    tidBlock_PGIHighClassBuildingConstrA = 'PGIHighClassBuildingConstrA';
    tidBlock_PGIHighClassBuildingA       = 'PGIHighClassBuildingA';
    tidFacility_PGIHighClassBuildingA    = 'PGIHighClassBuildingA';
    vidFacility_PGIHighClassBuildingA    = 4301;

  const
    tidBlock_PGIHighClassBuildingConstrB = 'PGIHighClassBuildingConstrB';
    tidBlock_PGIHighClassBuildingB       = 'PGIHighClassBuildingB';
    tidFacility_PGIHighClassBuildingB    = 'PGIHighClassBuildingB';
    vidFacility_PGIHighClassBuildingB    = 4311;

  const
    tidBlock_PGIHighClassBuildingConstrC = 'PGIHighClassBuildingConstrC';
    tidBlock_PGIHighClassBuildingC       = 'PGIHighClassBuildingC';
    tidFacility_PGIHighClassBuildingC    = 'PGIHighClassBuildingC';
    vidFacility_PGIHighClassBuildingC    = 4321;

  const
    tidBlock_PGIHighClassBuildingConstrD = 'PGIHighClassBuildingConstrD';
    tidBlock_PGIHighClassBuildingD       = 'PGIHighClassBuildingD';
    tidFacility_PGIHighClassBuildingD    = 'PGIHighClassBuildingD';
    vidFacility_PGIHighClassBuildingD    = 4331;

  const
    tidBlock_PGIHighClassBuildingConstrE = 'PGIHighClassBuildingConstrE';
    tidBlock_PGIHighClassBuildingE       = 'PGIHighClassBuildingE';
    tidFacility_PGIHighClassBuildingE    = 'PGIHighClassBuildingE';
    vidFacility_PGIHighClassBuildingE    = 4341;

  const
    tidBlock_PGIHighClassBuildingConstrF = 'PGIHighClassBuildingConstrF';
    tidBlock_PGIHighClassBuildingF       = 'PGIHighClassBuildingF';
    tidFacility_PGIHighClassBuildingF    = 'PGIHighClassBuildingF';
    vidFacility_PGIHighClassBuildingF    = 4481;

  const
    tidBlock_PGIHighClassBuildingConstrJ = 'PGIHighClassBuildingConstrJ';
    tidBlock_PGIHighClassBuildingJ       = 'PGIHighClassBuildingJ';
    tidFacility_PGIHighClassBuildingJ    = 'PGIHighClassBuildingJ';
    vidFacility_PGIHighClassBuildingJ    = 7501;

  const
    tidBlock_PGIHighClassBuildingConstrG = 'PGIHighClassBuildingConstrG';
    tidBlock_PGIHighClassBuildingG       = 'PGIHighClassBuildingG';
    tidFacility_PGIHighClassBuildingG    = 'PGIHighClassBuildingG';
    vidFacility_PGIHighClassBuildingG    = 7511;

  const
    tidBlock_PGIHighClassBuildingConstrH = 'PGIHighClassBuildingConstrH';
    tidBlock_PGIHighClassBuildingH       = 'PGIHighClassBuildingH';
    tidFacility_PGIHighClassBuildingH    = 'PGIHighClassBuildingH';
    vidFacility_PGIHighClassBuildingH    = 7521;

  const
    tidBlock_PGIHighClassBuildingConstrI = 'PGIHighClassBuildingConstrI';
    tidBlock_PGIHighClassBuildingI       = 'PGIHighClassBuildingI';
    tidFacility_PGIHighClassBuildingI    = 'PGIHighClassBuildingI';
    vidFacility_PGIHighClassBuildingI    = 7531;

  const
    tidBlock_PGIMiddleClassBuildingConstrA = 'PGIMiddleClassBuildingConstrA';
    tidBlock_PGIMiddleClassBuildingA       = 'PGIMiddleClassBuildingA';
    tidFacility_PGIMiddleClassBuildingA    = 'PGIMiddleClassBuildingA';
    vidFacility_PGIMiddleClassBuildingA    = 4351;

  const
    tidBlock_PGIMiddleClassBuildingConstrB = 'PGIMiddleClassBuildingConstrB';
    tidBlock_PGIMiddleClassBuildingB       = 'PGIMiddleClassBuildingB';
    tidFacility_PGIMiddleClassBuildingB    = 'PGIMiddleClassBuildingB';
    vidFacility_PGIMiddleClassBuildingB    = 4361;

  const
    tidBlock_PGIMiddleClassBuildingConstrC = 'PGIMiddleClassBuildingConstrC';
    tidBlock_PGIMiddleClassBuildingC       = 'PGIMiddleClassBuildingC';
    tidFacility_PGIMiddleClassBuildingC    = 'PGIMiddleClassBuildingC';
    vidFacility_PGIMiddleClassBuildingC    = 4371;

  const
    tidBlock_PGIMiddleClassBuildingConstrD = 'PGIMiddleClassBuildingConstrD';
    tidBlock_PGIMiddleClassBuildingD       = 'PGIMiddleClassBuildingD';
    tidFacility_PGIMiddleClassBuildingD    = 'PGIMiddleClassBuildingD';
    vidFacility_PGIMiddleClassBuildingD    = 4381;

  const
    tidBlock_PGIMiddleClassBuildingConstrE = 'PGIMiddleClassBuildingConstrE';
    tidBlock_PGIMiddleClassBuildingE       = 'PGIMiddleClassBuildingE';
    tidFacility_PGIMiddleClassBuildingE    = 'PGIMiddleClassBuildingE';
    vidFacility_PGIMiddleClassBuildingE    = 4391;

  const
    tidBlock_PGILowClassBuildingConstrA = 'PGILowClassBuildingConstrA';
    tidBlock_PGILowClassBuildingA       = 'PGILowClassBuildingA';
    tidFacility_PGILowClassBuildingA    = 'PGILowClassBuildingA';
    vidFacility_PGILowClassBuildingA    = 4401;

  const
    tidBlock_PGILowClassBuildingConstrB = 'PGILowClassBuildingConstrB';
    tidBlock_PGILowClassBuildingB       = 'PGILowClassBuildingB';
    tidFacility_PGILowClassBuildingB    = 'PGILowClassBuildingB';
    vidFacility_PGILowClassBuildingB    = 4411;

  const
    tidBlock_PGILowClassBuildingConstrC = 'PGILowClassBuildingConstrC';
    tidBlock_PGILowClassBuildingC       = 'PGILowClassBuildingC';
    tidFacility_PGILowClassBuildingC    = 'PGILowClassBuildingC';
    vidFacility_PGILowClassBuildingC    = 4421;

  const
    tidBlock_PGILowClassBuildingConstrD = 'PGILowClassBuildingConstrD';
    tidBlock_PGILowClassBuildingD       = 'PGILowClassBuildingD';
    tidFacility_PGILowClassBuildingD    = 'PGILowClassBuildingD';
    vidFacility_PGILowClassBuildingD    = 4431;

  const
    tidBlock_PGILowClassBuildingConstrE = 'PGILowClassBuildingConstrE';
    tidBlock_PGILowClassBuildingE       = 'PGILowClassBuildingE';
    tidFacility_PGILowClassBuildingE    = 'PGILowClassBuildingE';
    vidFacility_PGILowClassBuildingE    = 7541;

  const
    tidBlock_PGILowClassBuildingConstrF = 'PGILowClassBuildingConstrF';
    tidBlock_PGILowClassBuildingF       = 'PGILowClassBuildingF';
    tidFacility_PGILowClassBuildingF    = 'PGILowClassBuildingF';
    vidFacility_PGILowClassBuildingF    = 7551;

  const
    tidBlock_PGIOfficeBuildingConstrA = 'PGIOfficeBuildingConstrA';
    tidBlock_PGIOfficeBuildingA       = 'PGIOfficeBuildingA';
    tidFacility_PGIOfficeBuildingA    = 'PGIOfficeBuildingA';
    vidFacility_PGIOfficeBuildingA    = 4951;

  const
    tidBlock_PGIOfficeBuildingConstrB = 'PGIOfficeBuildingConstrB';
    tidBlock_PGIOfficeBuildingB       = 'PGIOfficeBuildingB';
    tidFacility_PGIOfficeBuildingB    = 'PGIOfficeBuildingB';
    vidFacility_PGIOfficeBuildingB    = 4961;

  const
    tidBlock_PGIOfficeBuildingConstrC = 'PGIOfficeBuildingConstrC';
    tidBlock_PGIOfficeBuildingC       = 'PGIOfficeBuildingC';
    tidFacility_PGIOfficeBuildingC    = 'PGIOfficeBuildingC';
    vidFacility_PGIOfficeBuildingC    = 4971;

  const
    tidBlock_PGIOfficeBuildingConstrD = 'PGIOfficeBuildingConstrD';
    tidBlock_PGIOfficeBuildingD       = 'PGIOfficeBuildingD';
    tidFacility_PGIOfficeBuildingD    = 'PGIOfficeBuildingD';
    vidFacility_PGIOfficeBuildingD    = 7561;

  const
    tidBlock_PGIFarmConstr = 'PGIFarmConstr';
    tidBlock_PGIFarm       = 'PGIFarm';
    tidFacility_PGIFarm    = 'PGIFarm';
    vidFacility_PGIFarm    = 4111;

  const
    tidBlock_PGISmallFarmConstr = 'PGISmallFarmConstr';
    tidBlock_PGISmallFarm       = 'PGISmallFarm';
    tidFacility_PGISmallFarm    = 'PGISmallFarm';
    vidFacility_PGISmallFarm    = 4115;

  const
    tidBlock_PGIMineConstr = 'PGIMineConstr';
    tidBlock_PGIMine       = 'PGIMine';
    tidFacility_PGIMine    = 'PGIMine';
    vidFacility_PGIMine    = 4121;

  const
    tidBlock_PGISmallMineConstr = 'PGISmallMineConstr';
    tidBlock_PGISmallMine       = 'PGISmallMine';
    tidFacility_PGISmallMine    = 'PGISmallMine';
    vidFacility_PGISmallMine    = 4125;

  const
    tidBlock_PGIChemMineConstr = 'PGIChemMineConstr';
    tidBlock_PGIChemMine       = 'PGIChemMine';
    tidFacility_PGIChemMine    = 'PGIChemMine';
    vidFacility_PGIChemMine    = 7311;

  const
    tidBlock_PGISmallChemMineConstr = 'PGISmallChemMineConstr';
    tidBlock_PGISmallChemMine       = 'PGISmallChemMine';
    tidFacility_PGISmallChemMine    = 'PGISmallChemMine';
    vidFacility_PGISmallChemMine    = 7315;

  const
    tidBlock_PGISiliconMineConstr = 'PGISiliconMineConstr';
    tidBlock_PGISiliconMine       = 'PGISiliconMine';
    tidFacility_PGISiliconMine    = 'PGISiliconMine';
    vidFacility_PGISiliconMine    = 7321;

  const
    tidBlock_PGISmallSiliconMineConstr = 'PGISmallSiliconMineConstr';
    tidBlock_PGISmallSiliconMine       = 'PGISmallSiliconMine';
    tidFacility_PGISmallSiliconMine    = 'PGISmallSiliconMine';
    vidFacility_PGISmallSiliconMine    = 7325;

  const
    tidBlock_PGIStoneMineConstr = 'PGIStoneMineConstr';
    tidBlock_PGIStoneMine       = 'PGIStoneMine';
    tidFacility_PGIStoneMine    = 'PGIStoneMine';
    vidFacility_PGIStoneMine    = 7331;

  const
    tidBlock_PGISmallStoneMineConstr = 'PGISmallStoneMineConstr';
    tidBlock_PGISmallStoneMine       = 'PGISmallStoneMine';
    tidFacility_PGISmallStoneMine    = 'PGISmallStoneMine';
    vidFacility_PGISmallStoneMine    = 7335;

  const
    tidBlock_PGICoalMineConstr = 'PGICoalMineConstr';
    tidBlock_PGICoalMine       = 'PGICoalMine';
    tidFacility_PGICoalMine    = 'PGICoalMine';
    vidFacility_PGICoalMine    = 7341;

  const
    tidBlock_PGISmallCoalMineConstr = 'PGISmallCoalMineConstr';
    tidBlock_PGISmallCoalMine       = 'PGISmallCoalMine';
    tidFacility_PGISmallCoalMine    = 'PGISmallCoalMine';
    vidFacility_PGISmallCoalMine    = 7345;

  const
    tidBlock_PGIClothingsConstr = 'PGIClothingsConstr';
    tidBlock_PGIClothings       = 'PGIClothings';
    tidFacility_PGIClothings    = 'PGIClothings';
    vidFacility_PGIClothings    = 4131;

  const
    tidBlock_PGISmallClothingsConstr = 'PGISmallClothingsConstr';
    tidBlock_PGISmallClothings       = 'PGISmallClothings';
    tidFacility_PGISmallClothings    = 'PGISmallClothings';
    vidFacility_PGISmallClothings    = 4135;

  const
    tidBlock_PGIFoodProcConstr = 'PGIFoodProcConstr';
    tidBlock_PGIFoodProc       = 'PGIFoodProc';
    tidFacility_PGIFoodProc    = 'PGIFoodProc';
    vidFacility_PGIFoodProc    = 4141;

  const
    tidBlock_PGISmallFoodProcConstr = 'PGISmallFoodProcConstr';
    tidBlock_PGISmallFoodProc       = 'PGISmallFoodProc';
    tidFacility_PGISmallFoodProc    = 'PGISmallFoodProc';
    vidFacility_PGISmallFoodProc    = 4145;

  const
    tidBlock_PGIMetalConstr = 'PGIMetalConstr';
    tidBlock_PGIMetal       = 'PGIMetal';
    tidFacility_PGIMetal    = 'PGIMetal';
    vidFacility_PGIMetal    = 4151;

  const
    tidBlock_PGISmallMetalConstr = 'PGISmallMetalConstr';
    tidBlock_PGISmallMetal       = 'PGISmallMetal';
    tidFacility_PGISmallMetal    = 'PGISmallMetal';
    vidFacility_PGISmallMetal    = 4155;

  const
    tidBlock_PGIChemicalConstr = 'PGIChemicalConstr';
    tidBlock_PGIChemical       = 'PGIChemical';
    tidFacility_PGIChemical    = 'PGIChemical';
    vidFacility_PGIChemical    = 4161;

  const
    tidBlock_PGISmallChemicalConstr = 'PGISmallChemicalConstr';
    tidBlock_PGISmallChemical       = 'PGISmallChemical';
    tidFacility_PGISmallChemical    = 'PGISmallChemical';
    vidFacility_PGISmallChemical    = 4165;

  const
    tidBlock_PGIPaperConstr = 'PGIPaperConstr';
    tidBlock_PGIPaper       = 'PGIPaper';
    tidFacility_PGIPaper    = 'PGIPaper';
    vidFacility_PGIPaper    = 7571;

  const
    tidBlock_PGIPrintingConstr = 'PGIPrintingConstr';
    tidBlock_PGIPrinting       = 'PGIPrinting';
    tidFacility_PGIPrinting    = 'PGIPrinting';
    vidFacility_PGIPrinting    = 7581;  


  const
    tidBlock_PGITextileConstr = 'PGITextileConstr';
    tidBlock_PGITextile       = 'PGITextile';
    tidFacility_PGITextile    = 'PGITextile';
    vidFacility_PGITextile    = 4171;

  const
    tidBlock_PGISmallTextileConstr = 'PGISmallTextileConstr';
    tidBlock_PGISmallTextile       = 'PGISmallTextile';
    tidFacility_PGISmallTextile    = 'PGISmallTextile';
    vidFacility_PGISmallTextile    = 4175;

  const
    tidBlock_PGIElectronicConstr = 'PGIElectronicConstr';
    tidBlock_PGIElectronic       = 'PGIElectronic';
    tidFacility_PGIElectronic    = 'PGIElectronic';
    vidFacility_PGIElectronic    = 4181;

  const
    tidBlock_PGISmallElectronicConstr = 'PGISmallElectronicConstr';
    tidBlock_PGISmallElectronic       = 'PGISmallElectronic';
    tidFacility_PGISmallElectronic    = 'PGISmallElectronic';
    vidFacility_PGISmallElectronic    = 4185;

  const
    tidBlock_PGICarIndustryConstr = 'PGICarIndustryConstr';
    tidBlock_PGICarIndustry       = 'PGICarIndustry';
    tidFacility_PGICarIndustry    = 'PGICarIndustry';
    vidFacility_PGICarIndustry    = 4191;

  const
    tidBlock_PGISmallCarIndustryConstr = 'PGISmallCarIndustryConstr';
    tidBlock_PGISmallCarIndustry       = 'PGISmallCarIndustry';
    tidFacility_PGISmallCarIndustry    = 'PGISmallCarIndustry';
    vidFacility_PGISmallCarIndustry    = 4195;

  const
    tidBlock_PGIHeavyConstr = 'PGIHeavyConstr';
    tidBlock_PGIHeavy       = 'PGIHeavy';
    tidFacility_PGIHeavy    = 'PGIHeavy';
    vidFacility_PGIHeavy    = 4201;

  const
    tidBlock_PGISmallHeavyConstr = 'PGISmallHeavyConstr';
    tidBlock_PGISmallHeavy       = 'PGISmallHeavy';
    tidFacility_PGISmallHeavy    = 'PGISmallHeavy';
    vidFacility_PGISmallHeavy    = 4205;

  const
    tidBlock_PGIConstructionConstr = 'PGIConstructionConstr';
    tidBlock_PGIConstruction       = 'PGIConstruction';
    tidFacility_PGIConstruction    = 'PGIConstruction';
    vidFacility_PGIConstruction    = 4211;

  const
    tidBlock_PGIComputingIndustryConstr = 'PGIComputingIndustryConstr';
    tidBlock_PGIComputingIndustry       = 'PGIComputingIndustry';
    tidFacility_PGIComputingIndustry    = 'PGIComputingIndustry';
    vidFacility_PGIComputingIndustry    = 4221;

  const
    tidBlock_PGIHHAIndustryConstr = 'PGIHHAIndustryConstr';
    tidBlock_PGIHHAIndustry       = 'PGIHHAIndustry';
    tidFacility_PGIHHAIndustry    = 'PGIHHAIndustry';
    vidFacility_PGIHHAIndustry    = 4231;

  const
    tidBlock_PGISmallHHAIndustryConstr = 'PGISmallHHAIndustryConstr';
    tidBlock_PGISmallHHAIndustry       = 'PGISmallHHAIndustry';
    tidFacility_PGISmallHHAIndustry    = 'PGISmallHHAIndustry';
    vidFacility_PGISmallHHAIndustry    = 4235;

  const
    tidBlock_PGILegalServicesConstr = 'PGILegalServicesConstr';
    tidBlock_PGILegalServices       = 'PGILegalServices';
    tidFacility_PGILegalServices    = 'PGILegalServices';
    vidFacility_PGILegalServices    = 4241;

  const
    tidBlock_PGIBusinessMachineConstr = 'PGIBMConstr';
    tidBlock_PGIBusinessMachine       = 'PGIBM';
    tidFacility_PGIBusinessMachine    = 'PGIBM';
    vidFacility_PGIBusinessMachine    = 4251;

  const
    tidBlock_PGISmallBusinessMachineConstr = 'PGISmallBMConstr';
    tidBlock_PGISmallBusinessMachine       = 'PGISmallBM';
    tidFacility_PGISmallBusinessMachine    = 'PGISmallBM';
    vidFacility_PGISmallBusinessMachine    = 4255;

  const
    tidBlock_PGIPharmaIndustryConstr = 'PGIPharmaIndustryConstr';
    tidBlock_PGIPharmaIndustry       = 'PGIPharmaIndustry';
    tidFacility_PGIPharmaIndustry    = 'PGIPharmaIndustry';
    vidFacility_PGIPharmaIndustry    = 4271;

  const
    tidBlock_PGIFoodStoreConstr = 'PGIFoodStoreConstr';
    tidBlock_PGIFoodStore       = 'PGIFoodStore';
    tidFacility_PGIFoodStore    = 'PGIFoodStore';
    vidFacility_PGIFoodStore    = 4601;

  const
    tidBlock_PGIClothesStoreConstr = 'PGIClothesStoreConstr';
    tidBlock_PGIClothesStore       = 'PGIClothesStore';
    tidFacility_PGIClothesStore    = 'PGIClothesStore';
    vidFacility_PGIClothesStore    = 4631;

  const
    tidBlock_PGIHHAStoreConstr = 'PGIHHAsStoreConstr';
    tidBlock_PGIHHAStore       = 'PGIHHAsStore';
    tidFacility_PGIHHAStore    = 'PGIHHAsStore';
    vidFacility_PGIHHAStore    = 4661;

  const
    tidBlock_PGICarStoreConstr = 'PGICarStoreConstr';
    tidBlock_PGICarStore       = 'PGICarStore';
    tidFacility_PGICarStore    = 'PGICarStore';
    vidFacility_PGICarStore    = 4691;

  const
    tidBlock_PGIDrugStoreConstr = 'PGIDrugStoreConstr';
    tidBlock_PGIDrugStore       = 'PGIDrugStore';
    tidFacility_PGIDrugStore    = 'PGIDrugStore';
    vidFacility_PGIDrugStore    = 4701;

  const
    tidBlock_PGISupermarketConstrA = 'PGISupermarketConstrA';
    tidBlock_PGISupermarketA       = 'PGISupermarketA';
    tidFacility_PGISupermarketA    = 'PGISupermarketA';
    vidFacility_PGISupermarketA    = 4721;

  const
    tidBlock_PGISupermarketConstrB = 'PGISupermarketConstrB';
    tidBlock_PGISupermarketB       = 'PGISupermarketB';
    tidFacility_PGISupermarketB    = 'PGISupermarketB';
    vidFacility_PGISupermarketB    = 4731;

  const
    tidBlock_PGISupermarketConstrC = 'PGISupermarketConstrC';
    tidBlock_PGISupermarketC       = 'PGISupermarketC';
    tidFacility_PGISupermarketC    = 'PGISupermarketC';
    vidFacility_PGISupermarketC    = 4741;

  const
    tidBlock_PGIBarConstr = 'PGIBarConstr';
    tidBlock_PGIBar       = 'PGIBar';
    tidFacility_PGIBar    = 'PGIBar';
    vidFacility_PGIBar    = 4751;

  const
    tidBlock_PGIRestaurantConstr = 'PGIRestaurantConstr';
    tidBlock_PGIRestaurant       = 'PGIRestaurant';
    tidFacility_PGIRestaurant    = 'PGIRestaurant';
    vidFacility_PGIRestaurant    = 4771;

  const
    tidBlock_PGIMovieAConstr = 'PGIMovieAConstr';
    tidBlock_PGIMovieA       = 'PGIMovieA';
    tidFacility_PGIMovieA    = 'PGIMovieA';
    vidFacility_PGIMovieA    = 4781;

  const
    tidBlock_PGIMovieBConstr = 'PGIMovieBConstr';
    tidBlock_PGIMovieB       = 'PGIMovieB';
    tidFacility_PGIMovieB    = 'PGIMovieB';
    vidFacility_PGIMovieB    = 4791;

  const
    tidBlock_PGIHospitalConstr = 'PGIHospitalConstr';
    tidBlock_PGIHospital       = 'PGIHospital';
    tidFacility_PGIHospital    = 'PGIHospital';
    vidFacility_PGIHospital    = 4801;

  const
    tidBlock_PGISchoolConstr = 'PGISchoolConstr';
    tidBlock_PGISchool       = 'PGISchool';
    tidFacility_PGISchool    = 'PGISchool';
    vidFacility_PGISchool    = 4811;

  const
    tidBlock_PGIPoliceConstr = 'PGIPoliceConstr';
    tidBlock_PGIPolice       = 'PGIPolice';
    tidFacility_PGIPolice    = 'PGIPolice';
    vidFacility_PGIPolice    = 4821;

  const
    tidBlock_PGIFireConstr = 'PGIFireConstr';
    tidBlock_PGIFire       = 'PGIFire';
    tidFacility_PGIFire    = 'PGIFire';
    vidFacility_PGIFire    = 4831;

  const
    tidBlock_PGISmallParkConstr = 'PGISmallParkConstr';
    tidBlock_PGISmallPark       = 'PGISmallPark';
    tidFacility_PGISmallPark    = 'PGISmallPark';
    vidFacility_PGISmallPark    = 2831;

  const
    tidBlock_PGIMediumParkConstr = 'PGIMediumParkConstr';
    tidBlock_PGIMediumPark       = 'PGIMediumPark';
    tidFacility_PGIMediumPark    = 'PGIMediumPark';
    vidFacility_PGIMediumPark    = 2841;

  const
    tidBlock_PGICentralParkConstr = 'PGICentralParkConstr';
    tidBlock_PGICentralPark       = 'PGICentralPark';
    tidFacility_PGICentralPark    = 'PGICentralPark';
    vidFacility_PGICentralPark    = 2851;

  const
    tidBlock_PGIGeneralHeadquarterConstr = 'PGIGeneralHeadquarterConstr';
    tidBlock_PGIGeneralHeadquarter       = 'PGIGeneralHeadquarter';
    tidFacility_PGIGeneralHeadquarter    = 'PGIGeneralHeadquarter';
    vidFacility_PGIGeneralHeadquarter    = 4901;

  const
    tidBlock_PGIIndHeadquarterConstr = 'PGIIndHeadquarterConstr';
    tidBlock_PGIIndHeadquarter       = 'PGIIndHeadquarter';
    tidFacility_PGIIndHeadquarter    = 'PGIIndHeadquarter';
    vidFacility_PGIIndHeadquarter    = 4911;

  const
    tidBlock_PGIServiceHeadquarterConstr = 'PGIServiceHeadquarterConstr';
    tidBlock_PGIServiceHeadquarter       = 'PGIServiceHeadquarter';
    tidFacility_PGIServiceHeadquarter    = 'PGIServiceHeadquarter';
    vidFacility_PGIServiceHeadquarter    = 4911; // 4921;

  const
    tidBlock_PGIResHeadquarterConstr = 'PGIResHeadquarterConstr';
    tidBlock_PGIResHeadquarter       = 'PGIResHeadquarter';
    tidFacility_PGIResHeadquarter    = 'PGIResHeadquarter';
    vidFacility_PGIResHeadquarter    = 4911; // 4931;

  const
    tidBlock_PGIPubHeadquarterConstr = 'PGIPubHeadquarterConstr';
    tidBlock_PGIPubHeadquarter       = 'PGIPubHeadquarter';
    tidFacility_PGIPubHeadquarter    = 'PGIPubHeadquarter';
    vidFacility_PGIPubHeadquarter    = 4911; // 4941;

  const
    tidBlock_PGITVStationConstr = 'PGITVStationConstr';
    tidBlock_PGITVStation       = 'PGITVStation';
    tidFacility_PGITVStation    = 'PGITVStation';
    vidFacility_PGITVStation    = 4981;

  const
    tidBlock_PGITVAntennaConstr = 'PGITVAntennaConstr';
    tidBlock_PGITVAntenna       = 'PGITVAntenna';
    tidFacility_PGITVAntenna    = 'PGITVAntenna';
    vidFacility_PGITVAntenna    = 4991;

  const
    tidBlock_PGILibertyConstr = 'PGILibertyConstr';
    tidBlock_PGILiberty       = 'PGILiberty';
    tidFacility_PGILiberty    = 'PGILiberty';
    vidFacility_PGILiberty    = 6011;

  const
    tidBlock_PGITowerConstr = 'PGITowerConstr';
    tidBlock_PGITower       = 'PGITower';
    tidFacility_PGITower    = 'PGITower';
    vidFacility_PGITower    = 6021;


implementation

end.

