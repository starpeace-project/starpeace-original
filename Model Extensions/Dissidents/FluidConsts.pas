unit FluidConsts;

interface

  const

    // Business Machines
    cost_BusinessMachines = 33600001;
    budget_BusinessMachines = 2708.2922;
    input_BusinessMachines_Metal = 10;
    input_BusinessMachines_Electroniccomponents = 30;
    input_BusinessMachines_Chemicals = 10;
    input_BusinessMachines_Plastics = 10;
    input_BusinessMachines_ExecutivesWorkForce = 1;
    input_BusinessMachines_ProfessionalWorkForce = 9;
    input_BusinessMachines_Workers = 110;
    input_BusinessMachines_ComputerServices = 1.20000004768372;
    input_BusinessMachines_LegalServices = 0.240000009536743;

    output_BusinessMachines_BusinessMachines = 6;


    // Car Industry
    cost_CarIndustry = 33600001;
    budget_CarIndustry = 672.2922;
    input_CarIndustry_Metal = 240;
    input_CarIndustry_FabricsandThreads = 22;
    input_CarIndustry_Electroniccomponents = 10;
    input_CarIndustry_Chemicals = 12;
    input_CarIndustry_Plastics = 12;
    input_CarIndustry_ExecutivesWorkForce = 1;
    input_CarIndustry_ProfessionalWorkForce = 3;
    input_CarIndustry_Workers = 164;
    input_CarIndustry_ComputerServices = 1.20000004768372;
    input_CarIndustry_LegalServices = 0.360000014305115;

    output_CarIndustry_Nicecars = 0.360000014305115;


    // Chemical Plant
    cost_ChemicalPlant = 33600001;
    budget_ChemicalPlant = 109.2923;
    input_ChemicalPlant_Ore = 480;
    input_ChemicalPlant_ExecutivesWorkForce = 1;
    input_ChemicalPlant_ProfessionalWorkForce = 7;
    input_ChemicalPlant_Workers = 131;
    input_ChemicalPlant_ComputerServices = 0.480000019073486;
    input_ChemicalPlant_LegalServices = 0.360000014305115;

    output_ChemicalPlant_Chemicals = 60.0000038146973;


    // Clothing
    cost_Clothing = 13440001;
    budget_Clothing = 9.7169;
    input_Clothing_FabricsandThreads = 40;
    input_Clothing_OrganicMaterials = 10;
    input_Clothing_Chemicals = 6;
    input_Clothing_ExecutivesWorkForce = 0;
    input_Clothing_ProfessionalWorkForce = 1;
    input_Clothing_Workers = 88;
    input_Clothing_ComputerServices = 0.480000019073486;
    input_Clothing_LegalServices = 0.240000009536743;

    output_Clothing_Cheapclothing = 36;


    // Computing Industry
    cost_ComputingIndustry = 25200001;
    budget_ComputingIndustry = 801.2192;
    input_ComputingIndustry_ExecutivesWorkForce = 2;
    input_ComputingIndustry_ProfessionalWorkForce = 14;
    input_ComputingIndustry_Workers = 11;
    input_ComputingIndustry_LegalServices = 0.20;

    output_ComputingIndustry_ComputerServices = 6;


    // Construction
    cost_Construction = 35280001;
    budget_Construction = 841.5068;
    input_Construction_Ore = 144;
    input_Construction_Metal = 30;
    input_Construction_Chemicals = 30;
    input_Construction_Timber = 200;
    input_Construction_ExecutivesWorkForce = 0;
    input_Construction_ProfessionalWorkForce = 4;
    input_Construction_Workers = 164;
    input_Construction_ComputerServices = 0.480000019073486;
    input_Construction_LegalServices = 0.120000004768372;

    output_Construction_Construction = 24;


    // Electronic Industry
    cost_ElectronicIndustry = 30240001;
    budget_ElectronicIndustry = 0;
    input_ElectronicIndustry_Chemicals = 14;
    input_ElectronicIndustry_Metal = 24;
    input_ElectronicIndustry_ExecutivesWorkForce = 1;
    input_ElectronicIndustry_ProfessionalWorkForce = 7;
    input_ElectronicIndustry_Workers = 88;
    input_ElectronicIndustry_ComputerServices = 0.300000011920929;
    input_ElectronicIndustry_LegalServices = 0.360000014305115;

    output_ElectronicIndustry_Electroniccomponents = 31.2000007629395;


    // Farm
    cost_Farm = 8400000;
    budget_Farm = 95.8731;
    input_Farm_ExecutivesWorkForce = 0;
    input_Farm_ProfessionalWorkForce = 1;
    input_Farm_Workers = 33;
    input_Farm_Chemicals = 0.360000014305115;

    output_Farm_FreshFood = 36;
    output_Farm_OrganicMaterials = 6;


    // Food Processing Plant
    cost_FoodProcessingPlant = 25200001;
    budget_FoodProcessingPlant = 0;
    input_FoodProcessingPlant_FreshFood = 48;
    input_FoodProcessingPlant_Chemicals = 10;
    input_FoodProcessingPlant_ExecutivesWorkForce = 0;
    input_FoodProcessingPlant_ProfessionalWorkForce = 1;
    input_FoodProcessingPlant_Workers = 66;
    input_FoodProcessingPlant_ComputerServices = 0.240000009536743;
    input_FoodProcessingPlant_LegalServices = 0.120000004768372;

    output_FoodProcessingPlant_ProcessedFood = 48;


    // Food Processing Plant
    cost_LiquorFact = 25200001;
    budget_LiquorFact = 0;
    input_LiquorFact_FreshFood = 10;
    input_LiquorFact_Chemicals = 3;
    input_LiquorFact_ExecutivesWorkForce = 0;
    input_LiquorFact_ProfessionalWorkForce = 1;
    input_LiquorFact_Workers = 60;
    input_LiquorFact_ComputerServices = 0.240000009536743;
    input_LiquorFact_LegalServices = 0.120000004768372;

    output_LiquorFact_Liquors = 48;


    // Heavy Industry
    cost_HeavyIndustry = 33600001;
    budget_HeavyIndustry = 1425.2922;
    input_HeavyIndustry_Metal = 720;
    input_HeavyIndustry_Chemicals = 60;
    input_HeavyIndustry_ElectronicComponents = 24;
    input_HeavyIndustry_ExecutivesWorkForce = 1;
    input_HeavyIndustry_ProfessionalWorkForce = 5;
    input_HeavyIndustry_Workers = 131;
    input_HeavyIndustry_ComputerServices = 0.240000009536743;
    input_HeavyIndustry_LegalServices = 0.120000004768372;

    output_HeavyIndustry_Machinery = 1.20000004768372;


    // House Holding Appliances
    cost_HouseHoldingAppliances = 30240001;
    budget_HouseHoldingAppliances = 612.863;
    input_HouseHoldingAppliances_Metal = 40;
    input_HouseHoldingAppliances_Electroniccomponents = 30;
    input_HouseHoldingAppliances_Chemicals = 11;
    input_HouseHoldingAppliances_Plastics = 14;
    input_HouseHoldingAppliances_ExecutivesWorkForce = 1;
    input_HouseHoldingAppliances_ProfessionalWorkForce = 4;
    input_HouseHoldingAppliances_Workers = 131;
    input_HouseHoldingAppliances_ComputerServices = 0.480000019073486;
    input_HouseHoldingAppliances_LegalServices = 0.240000009536743;

    output_HouseHoldingAppliances_HouseHoldingAppliances = 14.4000005722046;

    
    // Toys
    cost_ToyIndustry = 30000000;
    budget_ToyIndustry = 3*212.863;
    input_ToyIndustry_Plastics = 3*7;
    input_ToyIndustry_Electroniccomponents = 3*4;
    input_ToyIndustry_Chemicals = 3*2;
    input_ToyIndustry_ExecutivesWorkForce = 2;
    input_ToyIndustry_ProfessionalWorkForce = 5;
    input_ToyIndustry_Workers = 120;
    input_ToyIndustry_ComputerServices = 3*0.480000019073486;
    input_ToyIndustry_LegalServices = 3*0.240000009536743;

    output_ToyIndustry_ToyIndustry = 3*16;


    // Legal Services
    cost_LegalServices = 20160001;
    budget_LegalServices = 455.5754;
    input_LegalServices_ExecutivesWorkForce = 2;
    input_LegalServices_ProfessionalWorkForce = 5;
    input_LegalServices_Workers = 11;
    input_LegalServices_ComputerServices = 0.2;

    output_LegalServices_LegalServices = 3.60000014305115;

                                                             
    // Metalurgic
    cost_Metalurgic = 33600001;
    budget_Metalurgic = 0;
    input_Metalurgic_Ore = 288;
    input_Metalurgic_Chemicals = 21;
    input_Metalurgic_ExecutivesWorkForce = 0;
    input_Metalurgic_ProfessionalWorkForce = 2;
    input_Metalurgic_Workers = 120;
    input_Metalurgic_ComputerServices = 0.200000002980232;
    input_Metalurgic_LegalServices = 0.119999997317791;

    output_Metalurgic_Metal = 144;


    // Mine
    cost_Mine = 8400000;
    budget_Mine = 0;
    input_Mine_Chemicals = 10;
    input_Mine_ExecutivesWorkForce = 0;
    input_Mine_ProfessionalWorkForce = 1;
    input_Mine_Workers = 110;
    input_Mine_ComputerServices = 0.360000014305115;
    input_Mine_LegalServices = 0.120000004768372;

    output_Mine_Ore = 480.000030517578;


    // Textile
    cost_Textile = 10080000;
    budget_Textile = 7.2877;
    input_Textile_OrganicMaterials = 18;
    input_Textile_Chemicals = 6;
    input_Textile_ExecutivesWorkForce = 0;
    input_Textile_ProfessionalWorkForce = 2;
    input_Textile_Workers = 88;
    input_Textile_ComputerServices = 0.12;
    input_Textile_LegalServices = 0.1;

    output_Textile_FabricsandThreads = 60.0000038146973;


    // Business Machines small
    cost_BusinessMachinessmall = 8000000;
    budget_BusinessMachinessmall = 901.3379;
    input_BusinessMachinessmall_Metal = 10;
    input_BusinessMachinessmall_Electroniccomponents = 10;
    input_BusinessMachinessmall_Chemicals = 4;
    input_BusinessMachinessmall_ExecutivesWorkForce = 1;
    input_BusinessMachinessmall_ProfessionalWorkForce = 5;
    input_BusinessMachinessmall_Workers = 63;
    input_BusinessMachinessmall_ComputerServices = 0.400000005960464;
    input_BusinessMachinessmall_LegalServices = 0.0800000056624413;

    output_BusinessMachinessmall_BusinessMachines = 2;


    // Car Industry small
    cost_CarIndustrysmall = 8000000;
    budget_CarIndustrysmall = 225.3379;
    input_CarIndustrysmall_Metal = 80;
    input_CarIndustrysmall_FabricsandThreads = 7;
    input_CarIndustrysmall_Electroniccomponents = 3;
    input_CarIndustrysmall_Chemicals = 8;
    input_CarIndustrysmall_ExecutivesWorkForce = 1;
    input_CarIndustrysmall_ProfessionalWorkForce = 2;
    input_CarIndustrysmall_Workers = 95;
    input_CarIndustrysmall_ComputerServices = 0.400000005960464;
    input_CarIndustrysmall_LegalServices = 0.120000004768372;

    output_CarIndustrysmall_Nicecars = 0.120000004768372;


    // Chemical Plant small
    cost_ChemicalPlantsmall = 8000000;
    budget_ChemicalPlantsmall = 30.3379;
    input_ChemicalPlantsmall_Ore = 160;
    input_ChemicalPlantsmall_ExecutivesWorkForce = 1;
    input_ChemicalPlantsmall_ProfessionalWorkForce = 4;
    input_ChemicalPlantsmall_Workers = 76;
    input_ChemicalPlantsmall_ComputerServices = 0.160000011324882;
    input_ChemicalPlantsmall_LegalServices = 0.120000004768372;

    output_ChemicalPlantsmall_Chemicals = 20;


    // Clothing small
    cost_Clothingsmall = 3200000;
    budget_Clothingsmall = 0.7352;
    input_Clothingsmall_FabricsandThreads = 13;
    input_Clothingsmall_OrganicMaterials = 3;
    input_Clothingsmall_Chemicals = 2;
    input_Clothingsmall_ExecutivesWorkForce = 0;
    input_Clothingsmall_ProfessionalWorkForce = 1;
    input_Clothingsmall_Workers = 51;
    input_Clothingsmall_ComputerServices = 0.160000011324882;
    input_Clothingsmall_LegalServices = 0.0800000056624413;

    output_Clothingsmall_Cheapclothing = 12;


    // Computing Industry small
    cost_ComputingIndustrysmall = 6000000;
    budget_ComputingIndustrysmall = 277.7534;
    input_ComputingIndustrysmall_ExecutivesWorkForce = 1;
    input_ComputingIndustrysmall_ProfessionalWorkForce = 8;
    input_ComputingIndustrysmall_Workers = 6;
    input_ComputingIndustrysmall_LegalServices = 0.16;

    output_ComputingIndustrysmall_ComputerServices = 2;


    // Construction small
    cost_Constructionsmall = 8400000;
    budget_Constructionsmall = 275.0548;
    input_Constructionsmall_Ore = 48;
    input_Constructionsmall_Metal = 10;
    input_Constructionsmall_Chemicals = 10;
    input_Constructionsmall_ExecutivesWorkForce = 0;
    input_Constructionsmall_ProfessionalWorkForce = 3;
    input_Constructionsmall_Workers = 95;
    input_Constructionsmall_ComputerServices = 0.160000011324882;
    input_Constructionsmall_LegalServices = 0.0400000028312206;

    output_Constructionsmall_Construction = 8;


    // Electronic Industry small
    cost_ElectronicIndustrysmall = 7200000;
    budget_ElectronicIndustrysmall = 0;
    input_ElectronicIndustrysmall_Chemicals = 5;
    input_ElectronicIndustrysmall_Metal = 8;
    input_ElectronicIndustrysmall_ExecutivesWorkForce = 0;
    input_ElectronicIndustrysmall_ProfessionalWorkForce = 3;
    input_ElectronicIndustrysmall_Workers = 50;
    input_ElectronicIndustrysmall_ComputerServices = 0.100000001490116;
    input_ElectronicIndustrysmall_LegalServices = 0.120000004768372;

    output_ElectronicIndustrysmall_Electroniccomponents = 10.4000005722046;


    // Farm small
    cost_Farmsmall = 2000000;
    budget_Farmsmall = 31.1845;
    input_Farmsmall_ExecutivesWorkForce = 0;
    input_Farmsmall_ProfessionalWorkForce = 1;
    input_Farmsmall_Workers = 19;
    input_Farmsmall_Chemicals = 0.120000004768372;

    output_Farmsmall_FreshFood = 12;
    output_Farmsmall_OrganicMaterials = 2;


    // Food Processing Plant small
    cost_FoodProcessingPlantsmall = 6000000;
    budget_FoodProcessingPlantsmall = 13.7534;
    input_FoodProcessingPlantsmall_FreshFood = 16;
    input_FoodProcessingPlantsmall_Chemicals = 3;
    input_FoodProcessingPlantsmall_ExecutivesWorkForce = 0;
    input_FoodProcessingPlantsmall_ProfessionalWorkForce = 1;
    input_FoodProcessingPlantsmall_Workers = 38;
    input_FoodProcessingPlantsmall_ComputerServices = 0.0800000056624413;
    input_FoodProcessingPlantsmall_LegalServices = 0.0400000028312206;

    output_FoodProcessingPlantsmall_ProcessedFood = 16;


    // Heavy Industry small
    cost_HeavyIndustrysmall = 8000000;
    budget_HeavyIndustrysmall = 470.3379;
    input_HeavyIndustrysmall_Metal = 240;
    input_HeavyIndustrysmall_Chemicals = 20;
    input_HeavyIndustrysmall_ElectronicComponents = 8;
    input_HeavyIndustrysmall_ExecutivesWorkForce = 1;
    input_HeavyIndustrysmall_ProfessionalWorkForce = 3;
    input_HeavyIndustrysmall_Workers = 76;
    input_HeavyIndustrysmall_ComputerServices = 0.0800000056624413;
    input_HeavyIndustrysmall_LegalServices = 0.0400000028312206;

    output_HeavyIndustrysmall_Machinery = 0.400000005960464;


    // House Holding Appliances small
    cost_HouseHoldingAppliancessmall = 7200000;
    budget_HouseHoldingAppliancessmall = 196.9042;
    input_HouseHoldingAppliancessmall_Metal = 10;
    input_HouseHoldingAppliancessmall_Electroniccomponents = 10;
    input_HouseHoldingAppliancessmall_Chemicals = 4;
    input_HouseHoldingAppliancessmall_Plastics = 5;
    input_HouseHoldingAppliancessmall_ExecutivesWorkForce = 1;
    input_HouseHoldingAppliancessmall_ProfessionalWorkForce = 3;
    input_HouseHoldingAppliancessmall_Workers = 76;
    input_HouseHoldingAppliancessmall_ComputerServices = 0.160000011324882;
    input_HouseHoldingAppliancessmall_LegalServices = 0.0800000056624413;

    output_HouseHoldingAppliancessmall_HouseHoldingAppliances = 4.80000019073486;


    // Legal Services small
    cost_LegalServicessmall = 4800000;
    budget_LegalServicessmall = 164.6027;
    input_LegalServicessmall_ExecutivesWorkForce = 1;
    input_LegalServicessmall_ProfessionalWorkForce = 3;
    input_LegalServicessmall_Workers = 6;
    input_LegalServicessmall_ComputerServices = 0.400000005960464;

    output_LegalServicessmall_LegalServices = 1.20000004768372;


    // Metalurgic small
    cost_Metalurgicsmall = 8000000;
    budget_Metalurgicsmall = 2.3379;
    input_Metalurgicsmall_Ore = 96;
    input_Metalurgicsmall_Chemicals = 7;
    input_Metalurgicsmall_ExecutivesWorkForce = 0;
    input_Metalurgicsmall_ProfessionalWorkForce = 1;
    input_Metalurgicsmall_Workers = 70;
    input_Metalurgicsmall_ComputerServices = 0.0800000056624413;
    input_Metalurgicsmall_LegalServices = 0.0400000028312206;

    output_Metalurgicsmall_Metal = 48;


    // Mine small
    cost_Minesmall = 2000000;
    budget_Minesmall = -12.4155;
    input_Minesmall_Chemicals = 3;
    input_Minesmall_ExecutivesWorkForce = 0;
    input_Minesmall_ProfessionalWorkForce = 1;
    input_Minesmall_Workers = 63;
    input_Minesmall_ComputerServices = 0.120000004768372;
    input_Minesmall_LegalServices = 0.0400000028312206;

    output_Minesmall_Ore = 160;


    // Textile small
    cost_Textilesmall = 2400000;
    budget_Textilesmall = 0;
    input_Textilesmall_OrganicMaterials = 5.19999980926514;
    input_Textilesmall_Chemicals = 2;
    input_Textilesmall_ExecutivesWorkForce = 0;
    input_Textilesmall_ProfessionalWorkForce = 1;
    input_Textilesmall_Workers = 50;
    input_TextileSmall_ComputerServices = 0;
    input_TextileSmall_LegalServices = 0;

    output_Textilesmall_FabricsandThreads = 20;

implementation

end.
