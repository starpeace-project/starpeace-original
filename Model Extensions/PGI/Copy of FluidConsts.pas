unit FluidConsts;

interface

  const

    // PGI Car Industry
    cost_CarIndustry = 60000000;
    budget_CarIndustry = 8052;
    input_CarIndustry_Metal = 800;
    input_CarIndustry_FabricsandThreads = 40;
    input_CarIndustry_Electroniccomponents = 15;
    input_CarIndustry_Chemicals = 60;
    input_CarIndustry_ExecutivesWorkForce = 16;
    input_CarIndustry_ProfessionalWorkForce = 42;
    input_CarIndustry_Workers = 350;
    input_CarIndustry_ComputerServices = 2;
    input_CarIndustry_LegalServices = 0.800000011920929;

    output_Nicecars = 1.79999995231628;


    // PGI Chemical Plant
    cost_ChemicalPlant = 60000000;
    budget_ChemicalPlant = 19285;
    input_ChemicalPlant_Ore = 4500;
    input_ChemicalPlant_ExecutivesWorkForce = 16;
    input_ChemicalPlant_ProfessionalWorkForce = 42;
    input_ChemicalPlant_Workers = 380;
    input_ChemicalPlant_ComputerServices = 1.60000002384186;
    input_ChemicalPlant_LegalServices = 0.800000011920929;

    output_Chemicals = 1500;


    // PGI Clothing
    cost_Clothing = 12000000;
    budget_Clothing = 0;
    input_Clothing_FabricsandThreads = 90;
    input_Clothing_OrganicMaterials = 30;
    input_Clothing_Chemicals = 10;
    input_Clothing_ExecutivesWorkForce = 2;
    input_Clothing_ProfessionalWorkForce = 24;
    input_Clothing_Workers = 100;
    input_Clothing_ComputerServices = 0.600000023841858;
    input_Clothing_LegalServices = 0.5;

    output_Cheapclothing = 100;


    // PGI Computing Industry
    cost_ComputingIndustry = 12000000;
    budget_ComputingIndustry = 376;
    input_ComputingIndustry_ExecutivesWorkForce = 4;
    input_ComputingIndustry_ProfessionalWorkForce = 42;
    input_ComputingIndustry_Workers = 8;
    input_ComputingIndustry_LegalServices = 0.800000011920929;

    output_ComputerServices = 8;


    // PGI Construction
    cost_Construction = 60000000;
    budget_Construction = 45000;
    input_Construction_Ore = 600;
    input_Construction_Metal = 250;
    input_Construction_Chemicals = 220;
    input_Construction_ExecutivesWorkForce = 8;
    input_Construction_ProfessionalWorkForce = 16;
    input_Construction_Workers = 540;
    input_Construction_ComputerServices = 1;
    input_Construction_LegalServices = 0.899999976158142;

    output_Construction = 600;


    // PGI Electronic Industry
    cost_ElectronicIndustry = 40000000;
    budget_ElectronicIndustry = 5295;
    input_ElectronicIndustry_Chemicals = 65;
    input_ElectronicIndustry_Metal = 120;
    input_ElectronicIndustry_ExecutivesWorkForce = 16;
    input_ElectronicIndustry_ProfessionalWorkForce = 42;
    input_ElectronicIndustry_Workers = 160;
    input_ElectronicIndustry_ComputerServices = 1;
    input_ElectronicIndustry_LegalServices = 0.800000011920929;

    output_Electroniccomponents = 220;


    // PGI Farm
    cost_Farm = 1000000;
    budget_Farm = 58;
    input_Farm_ExecutivesWorkForce = 0;
    input_Farm_ProfessionalWorkForce = 2;
    input_Farm_Workers = 50;
    input_Farm_Chemicals = 4;
    input_Farm_ComputerServices = 0.800000011920929;
    input_Farm_LegalServices = 0.600000023841858;

    output_FreshFood = 120;
    output_OrganicMaterials = 60;


    // PGI Food Processing Plant
    cost_FoodProcessingPlant = 30000000;
    budget_FoodProcessingPlant = 18;
    input_FoodProcessingPlant_FreshFood = 200;
    input_FoodProcessingPlant_Chemicals = 16;
    input_FoodProcessingPlant_ExecutivesWorkForce = 2;
    input_FoodProcessingPlant_ProfessionalWorkForce = 10;
    input_FoodProcessingPlant_Workers = 160;
    input_FoodProcessingPlant_ComputerServices = 1;
    input_FoodProcessingPlant_LegalServices = 0.800000011920929;

    output_ProcessedFood = 200;


    // PGI Heavy Industry
    cost_HeavyIndustry = 70000000;
    budget_HeavyIndustry = 90452;
    input_HeavyIndustry_Metal = 1200;
    input_HeavyIndustry_Chemicals = 250;
    input_HeavyIndustry_ElectronicComponents = 110;
    input_HeavyIndustry_ExecutivesWorkForce = 8;
    input_HeavyIndustry_ProfessionalWorkForce = 40;
    input_HeavyIndustry_Workers = 450;
    input_HeavyIndustry_ComputerServices = 1;
    input_HeavyIndustry_LegalServices = 0.899999976158142;

    output_Machinery = 15;


    // PGI House Holding Appliances
    cost_HouseHoldingAppliances = 50000000;
    budget_HouseHoldingAppliances = 1600;
    input_HouseHoldingAppliances_Metal = 320;
    input_HouseHoldingAppliances_Electroniccomponents = 160;
    input_HouseHoldingAppliances_Chemicals = 40;
    input_HouseHoldingAppliances_ExecutivesWorkForce = 6;
    input_HouseHoldingAppliances_ProfessionalWorkForce = 12;
    input_HouseHoldingAppliances_Workers = 340;
    input_HouseHoldingAppliances_ComputerServices = 1.20000004768372;
    input_HouseHoldingAppliances_LegalServices = 0.899999976158142;

    output_HouseHoldingAppliances = 85;


    // PGI Legal Services
    cost_LegalServices = 20000000;
    budget_LegalServices = 1615;
    input_LegalServices_ExecutivesWorkForce = 6;
    input_LegalServices_ProfessionalWorkForce = 40;
    input_LegalServices_Workers = 8;
    input_LegalServices_ComputerServices = 1.20000004768372;

    output_LegalServices = 15;


    // PGI Metalurgic
    cost_Metalurgic = 55000000;
    budget_Metalurgic = 100;
    input_Metalurgic_Ore = 5500;
    input_Metalurgic_Chemicals = 250;
    input_Metalurgic_ExecutivesWorkForce = 6;
    input_Metalurgic_ProfessionalWorkForce = 40;
    input_Metalurgic_Workers = 460;
    input_Metalurgic_ComputerServices = 1;
    input_Metalurgic_LegalServices = 0.800000011920929;

    output_Metal = 2200;


    // PGI Mine
    cost_Mine = 6000000;
    budget_Mine = 0;
    input_Mine_Chemicals = 60;
    input_Mine_ExecutivesWorkForce = 2;
    input_Mine_ProfessionalWorkForce = 10;
    input_Mine_Workers = 200;
    input_Mine_ComputerServices = 0.600000023841858;
    input_Mine_LegalServices = 0.200000002980232;

    output_Ore = 2100;


    // PGI Textile
    cost_Textile = 30000000;
    budget_Textile = 0;
    input_Textile_OrganicMaterials = 90;
    input_Textile_Chemicals = 20;
    input_Textile_ExecutivesWorkForce = 5;
    input_Textile_ProfessionalWorkForce = 25;
    input_Textile_Workers = 300;
    input_Textile_ComputerServices = 0.600000023841858;
    input_Textile_LegalServices = 0.200000002980232;

    output_FabricsandThreads = 350;


    // PGI Business Machines
    cost_BusinessMachines = 45000000;
    budget_BusinessMachines = 478;
    input_BusinessMachines_Metal = 300;
    input_BusinessMachines_Electroniccomponents = 280;
    input_BusinessMachines_Chemicals = 60;
    input_BusinessMachines_ExecutivesWorkForce = 15;
    input_BusinessMachines_ProfessionalWorkForce = 40;
    input_BusinessMachines_Workers = 200;
    input_BusinessMachines_ComputerServices = 2.5;
    input_BusinessMachines_LegalServices = 0.899999976158142;

    output_BusinessMachines = 30;

implementation

end.
