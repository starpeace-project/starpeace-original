unit FluidConsts;

interface

  const

    // Business Machines
    cost_BusinessMachines = 50399999;
    budget_BusinessMachines = 4108.4384;
    input_BusinessMachines_Metal = 25;
    input_BusinessMachines_Electroniccomponents = 45;
    input_BusinessMachines_Chemicals = 16;
    input_BusinessMachines_Plastics = 10;
    input_BusinessMachines_ExecutivesWorkForce = 1;
    input_BusinessMachines_ProfessionalWorkForce = 11;
    input_BusinessMachines_Workers = 134;
    input_BusinessMachines_ComputerServices = 1.79999995231628;
    input_BusinessMachines_LegalServices = 0.359999984502792;

    output_BusinessMachines_BusinessMachines = 9;


    // Car Industry
    cost_CarIndustry = 50399999;
    budget_CarIndustry = 1098.4389;
    input_CarIndustry_Metal = 360;
    input_CarIndustry_FabricsandThreads = 32;
    input_CarIndustry_Electroniccomponents = 14;
    input_CarIndustry_Chemicals = 18;
    input_CarIndustry_Plastics = 18;
    input_CarIndustry_ExecutivesWorkForce = 1;
    input_CarIndustry_ProfessionalWorkForce = 4;
    input_CarIndustry_Workers = 201;
    input_CarIndustry_ComputerServices = 1.79999995231628;
    input_CarIndustry_LegalServices = 0.540000021457672;

    output_CarIndustry_Nicecars = 0.540000021457672;


    // Chemical Plant
    cost_ChemicalPlant = 50399999;
    budget_ChemicalPlant = 214.4384;
    input_ChemicalPlant_Ore = 720;
    input_ChemicalPlant_ExecutivesWorkForce = 1;
    input_ChemicalPlant_ProfessionalWorkForce = 8;
    input_ChemicalPlant_Workers = 161;
    input_ChemicalPlant_ComputerServices = 0.719999969005585;
    input_ChemicalPlant_LegalServices = 0.540000021457672;

    output_ChemicalPlant_Chemicals = 90;


    // Clothing
    cost_Clothing = 20159999;
    budget_Clothing = 59.5754;
    input_Clothing_FabricsandThreads = 59;
    input_Clothing_OrganicMaterials = 14;
    input_Clothing_Chemicals = 9;
    input_Clothing_ExecutivesWorkForce = 0;
    input_Clothing_ProfessionalWorkForce = 1;
    input_Clothing_Workers = 107;
    input_Clothing_ComputerServices = 0.719999969005585;
    input_Clothing_LegalServices = 0.359999984502792;

    output_Clothing_Cheapclothing = 54;


    // Computing Industry
    cost_ComputingIndustry = 37799999;
    budget_ComputingIndustry = 1221.3288;
    input_ComputingIndustry_ExecutivesWorkForce = 3;
    input_ComputingIndustry_ProfessionalWorkForce = 17;
    input_ComputingIndustry_Workers = 13;
    input_ComputingIndustry_LegalServices = 0.719999969005585;

    output_ComputingIndustry_ComputerServices = 9;


    // Construction
    cost_Construction = 52919999;
    budget_Construction = 1311.2603;
    input_Construction_Ore = 216;
    input_Construction_Metal = 45;
    input_Construction_Chemicals = 45;
    input_Construction_Timber = 200;
    input_Construction_ExecutivesWorkForce = 0;
    input_Construction_ProfessionalWorkForce = 5;
    input_Construction_Workers = 201;
    input_Construction_ComputerServices = 0.719999969005585;
    input_Construction_LegalServices = 0.179999992251396;

    output_Construction_Construction = 36;


    // Electronic Industry
    cost_ElectronicIndustry = 45359999;
    budget_ElectronicIndustry = 0;
    input_ElectronicIndustry_Chemicals = 22;
    input_ElectronicIndustry_Metal = 36;
    input_ElectronicIndustry_ExecutivesWorkForce = 1;
    input_ElectronicIndustry_ProfessionalWorkForce = 8;
    input_ElectronicIndustry_Workers = 107;
    input_ElectronicIndustry_ComputerServices = 0.449999988079071;
    input_ElectronicIndustry_LegalServices = 0.540000021457672;

    output_ElectronicIndustry_Electroniccomponents = 46.7999992370605;


    // Farm
    cost_Farm = 12600000;
    budget_Farm = 155.3096;
    input_Farm_ExecutivesWorkForce = 0;
    input_Farm_ProfessionalWorkForce = 1;
    input_Farm_Workers = 40;
    input_Farm_Chemicals = 0.540000021457672;
    input_Farm_LegalServices = 0;
    input_Farm_ComputerServices = 0;

    output_Farm_FreshFood = 54;
    output_Farm_OrganicMaterials = 9;


    // Food Processing Plant
    cost_FoodProcessingPlant = 37799999;
    budget_FoodProcessingPlant = 23.3288;
    input_FoodProcessingPlant_FreshFood = 72;
    input_FoodProcessingPlant_Chemicals = 14;
    input_FoodProcessingPlant_ExecutivesWorkForce = 0;
    input_FoodProcessingPlant_ProfessionalWorkForce = 1;
    input_FoodProcessingPlant_Workers = 80;
    input_FoodProcessingPlant_ComputerServices = 0.359999984502792;
    input_FoodProcessingPlant_LegalServices = 0.179999992251396;

    output_FoodProcessingPlant_ProcessedFood = 72;


    // Heavy Industry
    cost_HeavyIndustry = 50399999;
    budget_HeavyIndustry = 2180.4384;
    input_HeavyIndustry_Metal = 1080;
    input_HeavyIndustry_Chemicals = 90;
    input_HeavyIndustry_ElectronicComponents = 36;
    input_HeavyIndustry_ExecutivesWorkForce = 1;
    input_HeavyIndustry_ProfessionalWorkForce = 7;
    input_HeavyIndustry_Workers = 161;
    input_HeavyIndustry_ComputerServices = 0.359999984502792;
    input_HeavyIndustry_LegalServices = 0.179999992251396;

    output_HeavyIndustry_Machinery = 1.79999995231628;


    // House Holding Appliances
    cost_HouseHoldingAppliances = 45359999;
    budget_HouseHoldingAppliances = 959.794;
    input_HouseHoldingAppliances_Metal = 60;
    input_HouseHoldingAppliances_Electroniccomponents = 45;
    input_HouseHoldingAppliances_Chemicals = 18;
    input_HouseHoldingAppliances_Plastics = 20;
    input_HouseHoldingAppliances_ExecutivesWorkForce = 1;
    input_HouseHoldingAppliances_ProfessionalWorkForce = 5;
    input_HouseHoldingAppliances_Workers = 161;
    input_HouseHoldingAppliances_ComputerServices = 0.719999969005585;
    input_HouseHoldingAppliances_LegalServices = 0.359999984502792;

    output_HouseHoldingAppliances_HouseHoldingAppliances = 21.5999984741211;


    // Legal Services
    cost_LegalServices = 30239999;
    budget_LegalServices = 688.8629;
    input_LegalServices_ExecutivesWorkForce = 3;
    input_LegalServices_ProfessionalWorkForce = 7;
    input_LegalServices_Workers = 13;
    input_LegalServices_ComputerServices = 1.79999995231628;

    output_LegalServices_LegalServices = 5.39999961853027;


    // Metalurgic
    cost_Metalurgic = 50399999;
    budget_Metalurgic = 0;
    input_Metalurgic_Ore = 432;
    input_Metalurgic_Chemicals = 32;
    input_Metalurgic_ExecutivesWorkForce = 0;
    input_Metalurgic_ProfessionalWorkForce = 3;
    input_Metalurgic_Workers = 148;
    input_Metalurgic_ComputerServices = 0.359999984502792;
    input_Metalurgic_LegalServices = 0.179999992251396;

    output_Metalurgic_Metal = 216;

    // Plastic
    cost_Plastic = 20000000;
    budget_Plastic = 0;
    input_Plastic_Oil = 600;
    input_Plastic_Chemicals = 20;
    input_Plastic_ExecutivesWorkForce = 0;
    input_Plastic_ProfessionalWorkForce = 3;
    input_Plastic_Workers = 100;
    input_Plastic_ComputerServices = 0.359999984502792;
    input_Plastic_LegalServices = 0.179999992251396;

    output_Plastic_Plastic = 216;


    // Mine
    cost_Mine = 12600000;
    budget_Mine = 53.1096;
    input_Mine_Chemicals = 14;
    input_Mine_ExecutivesWorkForce = 0;
    input_Mine_ProfessionalWorkForce = 1;
    input_Mine_Workers = 134;
    input_Mine_ComputerServices = 0.540000021457672;
    input_Mine_LegalServices = 0.179999992251396;

    output_Mine_Ore = 720;


    // Textile
    cost_Textile = 15120000;
    budget_Textile = 25;
    input_Textile_OrganicMaterials = 27;
    input_Textile_Chemicals = 9;
    input_Textile_ExecutivesWorkForce = 0;
    input_Textile_ProfessionalWorkForce = 3;
    input_Textile_Workers = 107;
    input_Textile_ComputerServices = 0.12;
    input_Textile_LegalServices = 0.1;

    output_Textile_FabricsandThreads = 90;


    // Business Machines small
    cost_BusinessMachinessmall = 12000000;
    budget_BusinessMachinessmall = 1380.5068;
    input_BusinessMachinessmall_Metal = 15;
    input_BusinessMachinessmall_Electroniccomponents = 15;
    input_BusinessMachinessmall_Chemicals = 6;
    input_BusinessMachinessmall_ExecutivesWorkForce = 1;
    input_BusinessMachinessmall_ProfessionalWorkForce = 6;
    input_BusinessMachinessmall_Workers = 77;
    input_BusinessMachinessmall_ComputerServices = 0.600000023841858;
    input_BusinessMachinessmall_LegalServices = 0.120000004768372;

    output_BusinessMachinessmall_BusinessMachines = 3;


    // Car Industry small
    cost_CarIndustrysmall = 12000000;
    budget_CarIndustrysmall = 354.5068;
    input_CarIndustrysmall_Metal = 120;
    input_CarIndustrysmall_FabricsandThreads = 11;
    input_CarIndustrysmall_Electroniccomponents = 5;
    input_CarIndustrysmall_Chemicals = 12;
    input_CarIndustrysmall_ExecutivesWorkForce = 1;
    input_CarIndustrysmall_ProfessionalWorkForce = 2;
    input_CarIndustrysmall_Workers = 116;
    input_CarIndustrysmall_ComputerServices = 0.600000023841858;
    input_CarIndustrysmall_LegalServices = 0.180000007152557;

    output_CarIndustrysmall_Nicecars = 0.180000007152557;


    // Chemical Plant small
    cost_ChemicalPlantsmall = 12000000;
    budget_ChemicalPlantsmall = 75.5069;
    input_ChemicalPlantsmall_Ore = 240;
    input_ChemicalPlantsmall_ExecutivesWorkForce = 1;
    input_ChemicalPlantsmall_ProfessionalWorkForce = 5;
    input_ChemicalPlantsmall_Workers = 93;
    input_ChemicalPlantsmall_ComputerServices = 0.240000009536743;
    input_ChemicalPlantsmall_LegalServices = 0.180000007152557;

    output_ChemicalPlantsmall_Chemicals = 30.0000019073486;


    // Clothing small
    cost_Clothingsmall = 4800000;
    budget_Clothingsmall = 8.6027;
    input_Clothingsmall_FabricsandThreads = 20;
    input_Clothingsmall_OrganicMaterials = 5;
    input_Clothingsmall_Chemicals = 3;
    input_Clothingsmall_ExecutivesWorkForce = 0;
    input_Clothingsmall_ProfessionalWorkForce = 1;
    input_Clothingsmall_Workers = 62;
    input_Clothingsmall_ComputerServices = 0.240000009536743;
    input_Clothingsmall_LegalServices = 0.120000004768372;

    output_Clothingsmall_Cheapclothing = 18;


    // Computing Industry small
    cost_ComputingIndustrysmall = 9000000;
    budget_ComputingIndustrysmall = 420.6301;
    input_ComputingIndustrysmall_ExecutivesWorkForce = 2;
    input_ComputingIndustrysmall_ProfessionalWorkForce = 10;
    input_ComputingIndustrysmall_Workers = 8;
    input_ComputingIndustrysmall_LegalServices = 0.240000009536743;

    output_ComputingIndustrysmall_ComputerServices = 3;


    // Construction small
    cost_Constructionsmall = 12600001;
    budget_Constructionsmall = 445.0822;
    input_Constructionsmall_Ore = 72;
    input_Constructionsmall_Metal = 15;
    input_Constructionsmall_Chemicals = 15;
    input_Constructionsmall_ExecutivesWorkForce = 0;
    input_Constructionsmall_ProfessionalWorkForce = 3;
    input_Constructionsmall_Workers = 116;
    input_Constructionsmall_ComputerServices = 0.240000009536743;
    input_Constructionsmall_LegalServices = 0.0600000023841858;

    output_Constructionsmall_Construction = 12;


    // Electronic Industry small
    cost_ElectronicIndustrysmall = 10800000;
    budget_ElectronicIndustrysmall = 3.3562;
    input_ElectronicIndustrysmall_Chemicals = 7;
    input_ElectronicIndustrysmall_Metal = 12;
    input_ElectronicIndustrysmall_ExecutivesWorkForce = 1;
    input_ElectronicIndustrysmall_ProfessionalWorkForce = 5;
    input_ElectronicIndustrysmall_Workers = 62;
    input_ElectronicIndustrysmall_ComputerServices = 0.150000005960464;
    input_ElectronicIndustrysmall_LegalServices = 0.180000007152557;

    output_ElectronicIndustrysmall_Electroniccomponents = 15.6000003814697;


    // Farm small
    cost_Farmsmall = 3000000;
    budget_Farmsmall = 54.2767;
    input_Farmsmall_ExecutivesWorkForce = 0;
    input_Farmsmall_ProfessionalWorkForce = 1;
    input_Farmsmall_Workers = 23;
    input_Farmsmall_Chemicals = 0.180000007152557;

    output_Farmsmall_FreshFood = 18;
    output_Farmsmall_OrganicMaterials = 3;


    // Food Processing Plant small
    cost_FoodProcessingPlantsmall = 9000000;
    budget_FoodProcessingPlantsmall = 23.6301;
    input_FoodProcessingPlantsmall_FreshFood = 24;
    input_FoodProcessingPlantsmall_Chemicals = 5;
    input_FoodProcessingPlantsmall_ExecutivesWorkForce = 0;
    input_FoodProcessingPlantsmall_ProfessionalWorkForce = 1;
    input_FoodProcessingPlantsmall_Workers = 46;
    input_FoodProcessingPlantsmall_ComputerServices = 0.120000004768372;
    input_FoodProcessingPlantsmall_LegalServices = 0.0600000023841858;

    output_FoodProcessingPlantsmall_ProcessedFood = 24;


    // Heavy Industry small
    cost_HeavyIndustrysmall = 12000000;
    budget_HeavyIndustrysmall = 733.5068;
    input_HeavyIndustrysmall_Metal = 360;
    input_HeavyIndustrysmall_Chemicals = 30;
    input_HeavyIndustrysmall_ElectronicComponents = 12;
    input_HeavyIndustrysmall_ExecutivesWorkForce = 1;
    input_HeavyIndustrysmall_ProfessionalWorkForce = 4;
    input_HeavyIndustrysmall_Workers = 93;
    input_HeavyIndustrysmall_ComputerServices = 0.120000004768372;
    input_HeavyIndustrysmall_LegalServices = 0.0600000023841858;

    output_HeavyIndustrysmall_Machinery = 0.600000023841858;


    // House Holding Appliances small
    cost_HouseHoldingAppliancessmall = 10800000;
    budget_HouseHoldingAppliancessmall = 319.3562;
    input_HouseHoldingAppliancessmall_Metal = 19;
    input_HouseHoldingAppliancessmall_Electroniccomponents = 15;
    input_HouseHoldingAppliancessmall_Chemicals = 6;
    input_HouseHoldingAppliancesSmall_Plastics = 7;
    input_HouseHoldingAppliancessmall_ExecutivesWorkForce = 1;
    input_HouseHoldingAppliancessmall_ProfessionalWorkForce = 3;
    input_HouseHoldingAppliancessmall_Workers = 93;
    input_HouseHoldingAppliancessmall_ComputerServices = 0.240000009536743;
    input_HouseHoldingAppliancessmall_LegalServices = 0.120000004768372;

    output_HouseHoldingAppliancessmall_HouseHoldingAppliances = 7.2000002861023;


    // Legal Services small
    cost_LegalServicessmall = 7200000;
    budget_LegalServicessmall = 244.9041;
    input_LegalServicessmall_ExecutivesWorkForce = 2;
    input_LegalServicessmall_ProfessionalWorkForce = 4;
    input_LegalServicessmall_Workers = 8;
    input_LegalServicessmall_ComputerServices = 0.600000023841858;

    output_LegalServicessmall_LegalServices = 1.80000007152557;


    // Metalurgic small
    cost_Metalurgicsmall = 12000000;
    budget_Metalurgicsmall = 11.5068;
    input_Metalurgicsmall_Ore = 144;
    input_Metalurgicsmall_Chemicals = 11;
    input_Metalurgicsmall_ExecutivesWorkForce = 0;
    input_Metalurgicsmall_ProfessionalWorkForce = 2;
    input_Metalurgicsmall_Workers = 85;
    input_Metalurgicsmall_ComputerServices = 0.120000004768372;
    input_Metalurgicsmall_LegalServices = 0.0600000023841858;

    output_Metalurgicsmall_Metal = 72;


    // Mine small
    cost_Minesmall = 3000000;
    budget_Minesmall = 0;
    input_Minesmall_Chemicals = 5;
    input_Minesmall_ExecutivesWorkForce = 0;
    input_Minesmall_ProfessionalWorkForce = 1;
    input_Minesmall_Workers = 77;
    input_Minesmall_ComputerServices = 0.180000007152557;
    input_Minesmall_LegalServices = 0.0600000023841858;

    output_Minesmall_Ore = 240.000015258789;


    // Textile small
    cost_Textilesmall = 3600000;
    budget_Textilesmall = 0;
    input_Textilesmall_OrganicMaterials = 9;
    input_Textilesmall_Chemicals = 3;
    input_Textilesmall_ExecutivesWorkForce = 0;
    input_Textilesmall_ProfessionalWorkForce = 2;
    input_Textilesmall_Workers = 62;

    output_Textilesmall_FabricsandThreads = 30.0000019073486;

implementation

end.
