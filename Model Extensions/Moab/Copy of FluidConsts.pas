unit FluidConsts;

interface

  const

    // Moab Legal Services
    cost_LegalServices = 5500000;
    budget_LegalServices = 0;
    input_LegalServices_ExecutivesWorkForce = 7;
    input_LegalServices_ProfessionalWorkForce = 33;
    input_LegalServices_Workers = 5;
    input_LegalServices_ComputerServices = 1;

    output_LegalServices = 10;


    // Moab Computing Industry
    cost_ComputingIndustry = 990000;
    budget_ComputingIndustry = 0;
    input_ComputingIndustry_ExecutivesWorkForce = 3;
    input_ComputingIndustry_ProfessionalWorkForce = 33;
    input_ComputingIndustry_Workers = 7;
    input_ComputingIndustry_LegalServices = 0.5;

    output_ComputerServices = 20;


    // Moab Business Machines
    cost_BusinessMachines = 40000000;
    budget_BusinessMachines = 0;
    input_BusinessMachines_Metal = 130;
    input_BusinessMachines_Electroniccomponents = 250;
    input_BusinessMachines_Chemicals = 40;
    input_BusinessMachines_ExecutivesWorkForce = 33;
    input_BusinessMachines_ProfessionalWorkForce = 33;
    input_BusinessMachines_Workers = 133;
    input_BusinessMachines_ComputerServices = 4;
    input_BusinessMachines_LegalServices = 0.800000011920929;

    output_BusinessMachines = 25;


    // Moab Car Industry
    cost_CarIndustry = 40000000;
    budget_CarIndustry = 0;
    input_CarIndustry_Metal = 750;
    input_CarIndustry_FabricsandThreads = 30;
    input_CarIndustry_Electroniccomponents = 10;
    input_CarIndustry_Chemicals = 50;
    input_CarIndustry_ExecutivesWorkForce = 12;
    input_CarIndustry_ProfessionalWorkForce = 33;
    input_CarIndustry_Workers = 333;
    input_CarIndustry_ComputerServices = 1;
    input_CarIndustry_LegalServices = 0.0299999993294477;

    output_Nicecars = 1;


    // Moab Chemical Plant
    cost_ChemicalPlant = 40000000;
    budget_ChemicalPlant = 5000;
    input_ChemicalPlant_Ore = 10000;
    input_ChemicalPlant_ExecutivesWorkForce = 12;
    input_ChemicalPlant_ProfessionalWorkForce = 33;
    input_ChemicalPlant_Workers = 333;
    input_ChemicalPlant_ComputerServices = 1;
    input_ChemicalPlant_LegalServices = 0.300000011920929;

    output_Chemicals = 1000;


    // Moab Clothing
    cost_Clothing = 9000000;
    budget_Clothing = 0;
    input_Clothing_FabricsandThreads = 100;
    input_Clothing_OrganicMaterials = 30;
    input_Clothing_Chemicals = 20;
    input_Clothing_ExecutivesWorkForce = 12;
    input_Clothing_ProfessionalWorkForce = 33;
    input_Clothing_Workers = 133;
    input_Clothing_ComputerServices = 0.600000023841858;
    input_Clothing_LegalServices = 0.0299999993294477;

    output_Cheapclothing = 100;


    // Moab Construction
    cost_Construction = 50000000;
    budget_Construction = 9100;
    input_Construction_Ore = 500;
    input_Construction_Metal = 200;
    input_Construction_Chemicals = 200;
    input_Construction_ExecutivesWorkForce = 7;
    input_Construction_ProfessionalWorkForce = 12;
    input_Construction_Workers = 533;
    input_Construction_ComputerServices = 0.600000023841858;
    input_Construction_LegalServices = 0.0299999993294477;

    output_Construction = 200;


    // Moab Electronic Industry
    cost_ElectronicIndustry = 30000000;
    budget_ElectronicIndustry = 5790;
    input_ElectronicIndustry_Chemicals = 50;
    input_ElectronicIndustry_Metal = 100;
    input_ElectronicIndustry_ExecutivesWorkForce = 12;
    input_ElectronicIndustry_ProfessionalWorkForce = 33;
    input_ElectronicIndustry_Workers = 133;
    input_ElectronicIndustry_ComputerServices = 0.5;
    input_ElectronicIndustry_LegalServices = 0.0299999993294477;

    output_Electroniccomponents = 200;


    // Moab Farm
    cost_Farm = 4000000;
    budget_Farm = 1500;
    input_Farm_Chemicals = 2;
    input_Farm_ComputerServices = 0.0410000011324883;
    input_Farm_LegalServices = 0.0120000001043081;

    output_FreshFood = 200;
    output_OrganicMaterials = 100;


    // Moab Food Processing Plant
    cost_FoodProcessingPlant = 20000000;
    budget_FoodProcessingPlant = 200;
    input_FoodProcessingPlant_FreshFood = 400;
    input_FoodProcessingPlant_Chemicals = 40;
    input_FoodProcessingPlant_ExecutivesWorkForce = 7;
    input_FoodProcessingPlant_ProfessionalWorkForce = 7;
    input_FoodProcessingPlant_Workers = 233;
    input_FoodProcessingPlant_ComputerServices = 0.400000005960464;
    input_FoodProcessingPlant_LegalServices = 0.0120000001043081;

    output_ProcessedFood = 300;


    // Moab Heavy Industry
    cost_HeavyIndustry = 50000000;
    budget_HeavyIndustry = 52000;
    input_HeavyIndustry_Metal = 1000;
    input_HeavyIndustry_Chemicals = 400;
    input_HeavyIndustry_ElectronicComponents = 100;
    input_HeavyIndustry_ExecutivesWorkForce = 7;
    input_HeavyIndustry_ProfessionalWorkForce = 12;
    input_HeavyIndustry_Workers = 433;
    input_HeavyIndustry_ComputerServices = 0.600000023841858;
    input_HeavyIndustry_LegalServices = 0.0299999993294477;

    output_Machinery = 10;


    // Moab House Holding Appliances
    cost_HouseHoldingAppliances = 30000000;
    budget_HouseHoldingAppliances = 1200;
    input_HouseHoldingAppliances_Metal = 300;
    input_HouseHoldingAppliances_Electroniccomponents = 150;
    input_HouseHoldingAppliances_Chemicals = 30;
    input_HouseHoldingAppliances_ExecutivesWorkForce = 7;
    input_HouseHoldingAppliances_ProfessionalWorkForce = 12;
    input_HouseHoldingAppliances_Workers = 433;
    input_HouseHoldingAppliances_ComputerServices = 1;
    input_HouseHoldingAppliances_LegalServices = 0.800000011920929;

    output_HouseHoldingAppliances = 80;


    // Moab Metalurgic
    cost_Metalurgic = 40000000;
    budget_Metalurgic = 1100;
    input_Metalurgic_Ore = 4000;
    input_Metalurgic_Chemicals = 300;
    input_Metalurgic_ExecutivesWorkForce = 7;
    input_Metalurgic_ProfessionalWorkForce = 12;
    input_Metalurgic_Workers = 433;
    input_Metalurgic_ComputerServices = 0.600000023841858;
    input_Metalurgic_LegalServices = 0.300000011920929;

    output_Metal = 2000;


    // Moab Mine
    cost_Mine = 10000000;
    budget_Mine = 1700;
    input_Mine_Chemicals = 310;
    input_Mine_ExecutivesWorkForce = 7;
    input_Mine_ProfessionalWorkForce = 12;
    input_Mine_Workers = 133;
    input_Mine_ComputerServices = 0.600000023841858;
    input_Mine_LegalServices = 0.300000011920929;

    output_Ore = 10000;


    // Moab Textile
    cost_Textile = 14000000;
    budget_Textile = 300;
    input_Textile_OrganicMaterials = 100;
    input_Textile_Chemicals = 20;
    input_Textile_ExecutivesWorkForce = 12;
    input_Textile_ProfessionalWorkForce = 33;
    input_Textile_Workers = 433;
    input_Textile_ComputerServices = 0.600000023841858;
    input_Textile_LegalServices = 0.300000011920929;

    output_FabricsandThreads = 600;

implementation

end.
