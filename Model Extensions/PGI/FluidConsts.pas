unit FluidConsts;

interface

  const

    // Business Machines
    cost_BusinessMachines = 70000000;
    budget_BusinessMachines = 5756.2755;
    input_BusinessMachines_Metal = 22;
    input_BusinessMachines_Electroniccomponents = 62;
    input_BusinessMachines_Chemicals = 23;
    input_BusinessMachines_Plastics = 18;
    input_BusinessMachines_ExecutivesWorkForce = 2;
    input_BusinessMachines_ProfessionalWorkForce = 13;
    input_BusinessMachines_Workers = 158;
    input_BusinessMachines_ComputerServices = 2.5;
    input_BusinessMachines_LegalServices = 0.5;

    output_BusinessMachines_BusinessMachines = 12.5;


    // Car Industry
    cost_CarIndustry = 70000000;
    budget_CarIndustry = 1542.7755;
    input_CarIndustry_Metal = 500;
    input_CarIndustry_FabricsandThreads = 45;
    input_CarIndustry_Electroniccomponents = 20;
    input_CarIndustry_Chemicals = 25;
    input_CarIndustry_Plastics = 25;
    input_CarIndustry_ExecutivesWorkForce = 2;
    input_CarIndustry_ProfessionalWorkForce = 5;
    input_CarIndustry_Workers = 237;
    input_CarIndustry_ComputerServices = 2.5;
    input_CarIndustry_LegalServices = 0.75;

    output_CarIndustry_Nicecars = 0.75;


    // Chemical Plant
    cost_ChemicalPlant = 70000000;
    budget_ChemicalPlant = 333.7755;
    input_ChemicalPlant_Ore = 1000;
    input_ChemicalPlant_ExecutivesWorkForce = 2;
    input_ChemicalPlant_ProfessionalWorkForce = 9;
    input_ChemicalPlant_Workers = 190;
    input_ChemicalPlant_ComputerServices = 1;
    input_ChemicalPlant_LegalServices = 0.75;

    output_ChemicalPlant_Chemicals = 125;


    // Pharmaceutical Industry
    cost_PharmaIndustry = 20000000;
    budget_PharmaIndustry = 133;
    input_PharmaIndustry_ExecutivesWorkForce = 3;
    input_PharmaIndustry_ProfessionalWorkForce = 50;
    input_PharmaIndustry_Workers = 100;
    input_PharmaIndustry_Chemicals = 50;
    input_PharmaIndustry_Plastics  = 50;
    input_PharmaIndustry_ComputerServices = 1;
    input_PharmaIndustry_LegalServices = 0.75;

    output_PharmaIndustry_Drugs = 100;


    // Clothing
    cost_Clothing = 28000000;
    budget_Clothing = 106.9102;
    input_Clothing_FabricsandThreads = 82;
    input_Clothing_OrganicMaterials = 20;
    input_Clothing_Chemicals = 12;
    input_Clothing_ExecutivesWorkForce = 0;
    input_Clothing_ProfessionalWorkForce = 2;
    input_Clothing_Workers = 126;
    input_Clothing_ComputerServices = 1;
    input_Clothing_LegalServices = 0.5;

    output_Clothing_Cheapclothing = 75;


    // Computing Industry
    cost_ComputingIndustry = 52500000;
    budget_ComputingIndustry = 1720.4566;
    input_ComputingIndustry_ExecutivesWorkForce = 3;
    input_ComputingIndustry_ProfessionalWorkForce = 21;
    input_ComputingIndustry_Workers = 16;
    input_ComputingIndustry_LegalServices = 0.20;

    output_ComputingIndustry_ComputerServices = 12.5;


    // Construction
    cost_Construction = 73500000;
    budget_Construction = 1881.1393;
    input_Construction_Ore = 300;
    input_Construction_Metal = 62;
    input_Construction_Chemicals = 62;
    input_Construction_Timber = 200;
    input_Construction_ExecutivesWorkForce = 0;
    input_Construction_ProfessionalWorkForce = 6;
    input_Construction_Workers = 237;
    input_Construction_ComputerServices = 1;
    input_Construction_LegalServices = 0.25;

    output_Construction_Construction = 50;


    // Electronic Industry
    cost_ElectronicIndustry = 63000000;
    budget_ElectronicIndustry = 0;
    input_ElectronicIndustry_Chemicals = 30;
    input_ElectronicIndustry_Metal = 50;
    input_ElectronicIndustry_ExecutivesWorkForce = 2;
    input_ElectronicIndustry_ProfessionalWorkForce = 9;
    input_ElectronicIndustry_Workers = 126;
    input_ElectronicIndustry_ComputerServices = 0.625;
    input_ElectronicIndustry_LegalServices = 0.75;

    output_ElectronicIndustry_Electroniccomponents = 65;


    // Farm
    cost_Farm = 17500000;
    budget_Farm = 221.8189;
    input_Farm_ExecutivesWorkForce = 0;
    input_Farm_ProfessionalWorkForce = 2;
    input_Farm_Workers = 47;
    input_Farm_Chemicals = 0.75;

    output_Farm_FreshFood = 75;
    output_Farm_OrganicMaterials = 12.5;


    // Food Processing Plant
    cost_FoodProcessingPlant = 52500000;
    budget_FoodProcessingPlant = 34.9566;
    input_FoodProcessingPlant_FreshFood = 100;
    input_FoodProcessingPlant_Chemicals = 20;
    input_FoodProcessingPlant_ExecutivesWorkForce = 0;
    input_FoodProcessingPlant_ProfessionalWorkForce = 2;
    input_FoodProcessingPlant_Workers = 95;
    input_FoodProcessingPlant_ComputerServices = 0.5;
    input_FoodProcessingPlant_LegalServices = 0.25;

    output_FoodProcessingPlant_ProcessedFood = 100;


    // Heavy Industry
    cost_HeavyIndustry = 70000000;
    budget_HeavyIndustry = 3062.7755;
    input_HeavyIndustry_Metal = 1500;
    input_HeavyIndustry_Chemicals = 125;
    input_HeavyIndustry_ElectronicComponents = 50;
    input_HeavyIndustry_ExecutivesWorkForce = 2;
    input_HeavyIndustry_ProfessionalWorkForce = 8;
    input_HeavyIndustry_Workers = 190;
    input_HeavyIndustry_ComputerServices = 0.5;
    input_HeavyIndustry_LegalServices = 0.25;

    output_HeavyIndustry_Machinery = 2.5;


    // House Holding Appliances
    cost_HouseHoldingAppliances = 63000000;
    budget_HouseHoldingAppliances = 1381.5479;
    input_HouseHoldingAppliances_Metal = 85;
    input_HouseHoldingAppliances_Electroniccomponents = 62;
    input_HouseHoldingAppliances_Chemicals = 25;
    input_HouseHoldingAppliances_Plastics = 27;
    input_HouseHoldingAppliances_ExecutivesWorkForce = 2;
    input_HouseHoldingAppliances_ProfessionalWorkForce = 6;
    input_HouseHoldingAppliances_Workers = 190;
    input_HouseHoldingAppliances_ComputerServices = 1;
    input_HouseHoldingAppliances_LegalServices = 0.5;

    output_HouseHoldingAppliances_HouseHoldingAppliances = 30;


    // Legal Services
    cost_LegalServices = 42000000;
    budget_LegalServices = 977.3653;
    input_LegalServices_ExecutivesWorkForce = 3;
    input_LegalServices_ProfessionalWorkForce = 8;
    input_LegalServices_Workers = 16;
    input_LegalServices_ComputerServices = 0.7;

    output_LegalServices_LegalServices = 7.5;


    // Metalurgic
    cost_Metalurgic = 70000000;
    budget_Metalurgic = 18.7755;
    input_Metalurgic_Ore = 600;
    input_Metalurgic_Chemicals = 45;
    input_Metalurgic_ExecutivesWorkForce = 0;
    input_Metalurgic_ProfessionalWorkForce = 3;
    input_Metalurgic_Workers = 174;
    input_Metalurgic_ComputerServices = 0.5;
    input_Metalurgic_LegalServices = 0.25;

    output_Metalurgic_Metal = 300;


    // Mine
    cost_Mine = 17500000;
    budget_Mine = 88.3189;
    input_Mine_Chemicals = 20;
    input_Mine_ExecutivesWorkForce = 0;
    input_Mine_ProfessionalWorkForce = 2;
    input_Mine_Workers = 158;
    input_Mine_ComputerServices = 0.75;
    input_Mine_LegalServices = 0.25;

    output_Mine_Ore = 1000;


    // Textile
    cost_Textile = 21000000;
    budget_Textile = 62;
    input_Textile_OrganicMaterials = 38;
    input_Textile_Chemicals = 12;
    input_Textile_ExecutivesWorkForce = 0;
    input_Textile_ProfessionalWorkForce = 3;
    input_Textile_Workers = 126;
    input_Textile_ComputerServices = 0.12;
    input_Textile_LegalServices = 0.1;

    output_Textile_FabricsandThreads = 125;


    // Business Machines small
    cost_BusinessMachinessmall = 16000000;
    budget_BusinessMachinessmall = 1861.6758;
    input_BusinessMachinessmall_Metal = 20;
    input_BusinessMachinessmall_Electroniccomponents = 20;
    input_BusinessMachinessmall_Chemicals = 8;
    input_BusinessMachinessmall_ExecutivesWorkForce = 1;
    input_BusinessMachinessmall_ProfessionalWorkForce = 7;
    input_BusinessMachinessmall_Workers = 89;
    input_BusinessMachinessmall_ComputerServices = 0.800000011920929;
    input_BusinessMachinessmall_LegalServices = 0.160000011324882;

    output_BusinessMachinessmall_BusinessMachines = 4;


    // Car Industry small
    cost_CarIndustrysmall = 16000000;
    budget_CarIndustrysmall = 520.6758;
    input_CarIndustrysmall_Metal = 160;
    input_CarIndustrysmall_FabricsandThreads = 14;
    input_CarIndustrysmall_Electroniccomponents = 6;
    input_CarIndustrysmall_Chemicals = 16;
    input_CarIndustrysmall_ExecutivesWorkForce = 1;
    input_CarIndustrysmall_ProfessionalWorkForce = 3;
    input_CarIndustrysmall_Workers = 134;
    input_CarIndustrysmall_ComputerServices = 0.800000011920929;
    input_CarIndustrysmall_LegalServices = 0.240000009536743;

    output_CarIndustrysmall_Nicecars = 0.240000009536743;


    // Chemical Plant small
    cost_ChemicalPlantsmall = 16000000;
    budget_ChemicalPlantsmall = 127.6758;
    input_ChemicalPlantsmall_Ore = 320;
    input_ChemicalPlantsmall_ExecutivesWorkForce = 1;
    input_ChemicalPlantsmall_ProfessionalWorkForce = 5;
    input_ChemicalPlantsmall_Workers = 107;
    input_ChemicalPlantsmall_ComputerServices = 0.320000022649765;
    input_ChemicalPlantsmall_LegalServices = 0.240000009536743;

    output_ChemicalPlantsmall_Chemicals = 40;


    // Clothing small
    cost_Clothingsmall = 6400000;
    budget_Clothingsmall = 35.4703;
    input_Clothingsmall_FabricsandThreads = 26;
    input_Clothingsmall_OrganicMaterials = 6;
    input_Clothingsmall_Chemicals = 4;
    input_Clothingsmall_ExecutivesWorkForce = 0;
    input_Clothingsmall_ProfessionalWorkForce = 1;
    input_Clothingsmall_Workers = 72;
    input_Clothingsmall_ComputerServices = 0.320000022649765;
    input_Clothingsmall_LegalServices = 0.160000011324882;

    output_Clothingsmall_Cheapclothing = 24;


    // Computing Industry small
    cost_ComputingIndustrysmall = 12000000;
    budget_ComputingIndustrysmall = 574.5068;
    input_ComputingIndustrysmall_ExecutivesWorkForce = 2;
    input_ComputingIndustrysmall_ProfessionalWorkForce = 12;
    input_ComputingIndustrysmall_Workers = 9;
    input_ComputingIndustrysmall_LegalServices = 0.320000022649765;

    output_ComputingIndustrysmall_ComputerServices = 4;


    // Construction small
    cost_Constructionsmall = 16800000;
    budget_Constructionsmall = 614.1096;
    input_Constructionsmall_Ore = 96;
    input_Constructionsmall_Metal = 20;
    input_Constructionsmall_Chemicals = 20;
    input_Constructionsmall_ExecutivesWorkForce = 0;
    input_Constructionsmall_ProfessionalWorkForce = 4;
    input_Constructionsmall_Workers = 134;
    input_Constructionsmall_ComputerServices = 0.320000022649765;
    input_Constructionsmall_LegalServices = 0.0800000056624413;

    output_Constructionsmall_Construction = 16;


    // Electronic Industry small
    cost_ElectronicIndustrysmall = 14400000;
    budget_ElectronicIndustrysmall = 11.8083;
    input_ElectronicIndustrysmall_Chemicals = 10;
    input_ElectronicIndustrysmall_Metal = 16;
    input_ElectronicIndustrysmall_ExecutivesWorkForce = 1;
    input_ElectronicIndustrysmall_ProfessionalWorkForce = 5;
    input_ElectronicIndustrysmall_Workers = 72;
    input_ElectronicIndustrysmall_ComputerServices = 0.200000002980232;
    input_ElectronicIndustrysmall_LegalServices = 0.240000009536743;

    output_ElectronicIndustrysmall_Electroniccomponents = 20.8000011444092;


    // Farm small
    cost_Farmsmall = 4000000;
    budget_Farmsmall = 77.3689;
    input_Farmsmall_ExecutivesWorkForce = 0;
    input_Farmsmall_ProfessionalWorkForce = 1;
    input_Farmsmall_Workers = 27;
    input_Farmsmall_Chemicals = 0.240000009536743;

    output_Farmsmall_FreshFood = 24;
    output_Farmsmall_OrganicMaterials = 4;


    // Food Processing Plant small
    cost_FoodProcessingPlantsmall = 12000000;
    budget_FoodProcessingPlantsmall = 53.5068;
    input_FoodProcessingPlantsmall_FreshFood = 32;
    input_FoodProcessingPlantsmall_Chemicals = 6;
    input_FoodProcessingPlantsmall_ExecutivesWorkForce = 0;
    input_FoodProcessingPlantsmall_ProfessionalWorkForce = 1;
    input_FoodProcessingPlantsmall_Workers = 54;
    input_FoodProcessingPlantsmall_ComputerServices = 0.160000011324882;
    input_FoodProcessingPlantsmall_LegalServices = 0.0800000056624413;

    output_FoodProcessingPlantsmall_ProcessedFood = 32;


    // Heavy Industry small
    cost_HeavyIndustrysmall = 16000000;
    budget_HeavyIndustrysmall = 1003.6758;
    input_HeavyIndustrysmall_Metal = 480;
    input_HeavyIndustrysmall_Chemicals = 40;
    input_HeavyIndustrysmall_ElectronicComponents = 16;
    input_HeavyIndustrysmall_ExecutivesWorkForce = 1;
    input_HeavyIndustrysmall_ProfessionalWorkForce = 4;
    input_HeavyIndustrysmall_Workers = 107;
    input_HeavyIndustrysmall_ComputerServices = 0.160000011324882;
    input_HeavyIndustrysmall_LegalServices = 0.0800000056624413;

    output_HeavyIndustrysmall_Machinery = 0.800000011920929;


    // House Holding Appliances small
    cost_HouseHoldingAppliancessmall = 14400000;
    budget_HouseHoldingAppliancessmall = 448.8083;
    input_HouseHoldingAppliancessmall_Metal = 24;
    input_HouseHoldingAppliancessmall_Electroniccomponents = 20;
    input_HouseHoldingAppliancessmall_Chemicals = 8;
    input_HouseHoldingAppliancesSmall_Plastics = 10;
    input_HouseHoldingAppliancessmall_ExecutivesWorkForce = 1;
    input_HouseHoldingAppliancessmall_ProfessionalWorkForce = 4;
    input_HouseHoldingAppliancessmall_Workers = 107;
    input_HouseHoldingAppliancessmall_ComputerServices = 0.320000022649765;
    input_HouseHoldingAppliancessmall_LegalServices = 0.160000011324882;

    output_HouseHoldingAppliancessmall_HouseHoldingAppliances = 9.60000038146973;


    // Legal Services small
    cost_LegalServicessmall = 9600000;
    budget_LegalServicessmall = 340.2055;
    input_LegalServicessmall_ExecutivesWorkForce = 2;
    input_LegalServicessmall_ProfessionalWorkForce = 4;
    input_LegalServicessmall_Workers = 9;
    input_LegalServicessmall_ComputerServices = 0.800000011920929;

    output_LegalServicessmall_LegalServices = 2.40000009536743;


    // Metalurgic small
    cost_Metalurgicsmall = 16000000;
    budget_Metalurgicsmall = 46.6758;
    input_Metalurgicsmall_Ore = 192;
    input_Metalurgicsmall_Chemicals = 14;
    input_Metalurgicsmall_ExecutivesWorkForce = 0;
    input_Metalurgicsmall_ProfessionalWorkForce = 2;
    input_Metalurgicsmall_Workers = 98;
    input_Metalurgicsmall_ComputerServices = 0.160000011324882;
    input_Metalurgicsmall_LegalServices = 0.0800000056624413;

    output_Metalurgicsmall_Metal = 96;


    // Mine small
    cost_Minesmall = 4000000;
    budget_Minesmall = 16.1689;
    input_Minesmall_Chemicals = 6;
    input_Minesmall_ExecutivesWorkForce = 0;
    input_Minesmall_ProfessionalWorkForce = 1;
    input_Minesmall_Workers = 89;
    input_Minesmall_ComputerServices = 0.240000009536743;
    input_Minesmall_LegalServices = 0.0800000056624413;

    output_Minesmall_Ore = 320;


    // Textile small
    cost_Textilesmall = 4800000;
    budget_Textilesmall = 10;
    input_Textilesmall_OrganicMaterials = 12;
    input_Textilesmall_Chemicals = 4;
    input_Textilesmall_ExecutivesWorkForce = 0;
    input_Textilesmall_ProfessionalWorkForce = 2;
    input_Textilesmall_Workers = 72;
    input_TextileSmall_ComputerServices = 0;
    input_TextileSmall_LegalServices = 0;

    output_Textilesmall_FabricsandThreads = 40;

implementation

end.
