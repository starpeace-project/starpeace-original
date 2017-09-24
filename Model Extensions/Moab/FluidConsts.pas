unit FluidConsts;

interface

  const

    // Business Machines
    cost_BusinessMachines = 42000000;
    budget_BusinessMachines = 3426.9694;
    input_BusinessMachines_Metal = 18;
    input_BusinessMachines_Electroniccomponents = 38;
    input_BusinessMachines_Chemicals = 13;
    input_BusinessMachines_Plastics = 10;
    input_BusinessMachines_ExecutivesWorkForce = 1;
    input_BusinessMachines_ProfessionalWorkForce = 10;
    input_BusinessMachines_Workers = 122;
    input_BusinessMachines_ComputerServices = 1.5;
    input_BusinessMachines_LegalServices = 0.300000011920929;

    output_BusinessMachines_BusinessMachines = 7.5;


    // Car Industry
    cost_CarIndustry = 42000000;
    budget_CarIndustry = 919.4694;
    input_CarIndustry_Metal = 300;
    input_CarIndustry_FabricsandThreads = 27;
    input_CarIndustry_Electroniccomponents = 12;
    input_CarIndustry_Chemicals = 25;
    input_CarIndustry_Plastics = 25;
    input_CarIndustry_ExecutivesWorkForce = 1;
    input_CarIndustry_ProfessionalWorkForce = 4;
    input_CarIndustry_Workers = 184;
    input_CarIndustry_ComputerServices = 1.5;
    input_CarIndustry_LegalServices = 0.450000017881393;

    output_CarIndustry_Nicecars = 0.450000017881393;


    // Chemical Plant
    cost_ChemicalPlant = 42000000;
    budget_ChemicalPlant = 200.4694;
    input_ChemicalPlant_Ore = 600;
    input_ChemicalPlant_ExecutivesWorkForce = 1;
    input_ChemicalPlant_ProfessionalWorkForce = 7;
    input_ChemicalPlant_Workers = 147;
    input_ChemicalPlant_ComputerServices = 0.600000023841858;
    input_ChemicalPlant_LegalServices = 0.450000017881393;

    output_ChemicalPlant_Chemicals = 75;


    // Clothing
    cost_Clothing = 16800000;
    budget_Clothing = 35.1878;
    input_Clothing_FabricsandThreads = 50;
    input_Clothing_OrganicMaterials = 12;
    input_Clothing_Chemicals = 8;
    input_Clothing_ExecutivesWorkForce = 0;
    input_Clothing_ProfessionalWorkForce = 1;
    input_Clothing_Workers = 98;
    input_Clothing_ComputerServices = 0.600000023841858;
    input_Clothing_LegalServices = 0.300000011920929;

    output_Clothing_Cheapclothing = 45;


    // Computing Industry
    cost_ComputingIndustry = 31500000;
    budget_ComputingIndustry = 1042.477;
    input_ComputingIndustry_ExecutivesWorkForce = 2;
    input_ComputingIndustry_ProfessionalWorkForce = 16;
    input_ComputingIndustry_Workers = 12;
    input_ComputingIndustry_LegalServices = 0.20;

    output_ComputingIndustry_ComputerServices = 7.5;


    // Construction
    cost_Construction = 44100000;
    budget_Construction = 1098.3678;
    input_Construction_Ore = 180;
    input_Construction_Metal = 38;
    input_Construction_Chemicals = 38;
    input_Construction_Timber = 200;
    input_Construction_ExecutivesWorkForce = 0;
    input_Construction_ProfessionalWorkForce = 5;
    input_Construction_Workers = 184;
    input_Construction_ComputerServices = 0.600000023841858;
    input_Construction_LegalServices = 0.150000005960464;

    output_Construction_Construction = 30;


    // Electronic Industry
    cost_ElectronicIndustry = 37800000;
    budget_ElectronicIndustry = 0;
    input_ElectronicIndustry_Chemicals = 18;
    input_ElectronicIndustry_Metal = 30;
    input_ElectronicIndustry_ExecutivesWorkForce = 1;
    input_ElectronicIndustry_ProfessionalWorkForce = 7;
    input_ElectronicIndustry_Workers = 98;
    input_ElectronicIndustry_ComputerServices = 0.375;
    input_ElectronicIndustry_LegalServices = 0.450000017881393;

    output_ElectronicIndustry_Electroniccomponents = 39;


    // Farm
    cost_Farm = 10500000;
    budget_Farm = 134.4923;
    input_Farm_ExecutivesWorkForce = 0;
    input_Farm_ProfessionalWorkForce = 1;
    input_Farm_Workers = 37;
    input_Farm_Chemicals = 0.450000017881393;
    input_Farm_LegalServices = 0;
    input_Farm_ComputerServices = 0;

    output_Farm_FreshFood = 45;
    output_Farm_OrganicMaterials = 7.5;


    // Food Processing Plant
    cost_FoodProcessingPlant = 31500000;
    budget_FoodProcessingPlant = 33.977;
    input_FoodProcessingPlant_FreshFood = 60;
    input_FoodProcessingPlant_Chemicals = 12;
    input_FoodProcessingPlant_ExecutivesWorkForce = 0;
    input_FoodProcessingPlant_ProfessionalWorkForce = 1;
    input_FoodProcessingPlant_Workers = 73;
    input_FoodProcessingPlant_ComputerServices = 0.300000011920929;
    input_FoodProcessingPlant_LegalServices = 0.150000005960464;

    output_FoodProcessingPlant_ProcessedFood = 60;


    // Heavy Industry
    cost_HeavyIndustry = 42000000;
    budget_HeavyIndustry = 1839.4694;
    input_HeavyIndustry_Metal = 900;
    input_HeavyIndustry_Chemicals = 75;
    input_HeavyIndustry_ElectronicComponents = 30;
    input_HeavyIndustry_ExecutivesWorkForce = 1;
    input_HeavyIndustry_ProfessionalWorkForce = 6;
    input_HeavyIndustry_Workers = 147;
    input_HeavyIndustry_ComputerServices = 0.300000011920929;
    input_HeavyIndustry_LegalServices = 0.150000005960464;

    output_HeavyIndustry_Machinery = 1.5;


    // House Holding Appliances
    cost_HouseHoldingAppliances = 37800000;
    budget_HouseHoldingAppliances = 802.1724;
    input_HouseHoldingAppliances_Metal = 49;
    input_HouseHoldingAppliances_Electroniccomponents = 38;
    input_HouseHoldingAppliances_Chemicals = 15;
    input_HouseHoldingAppliances_Plastics = 17;
    input_HouseHoldingAppliances_ExecutivesWorkForce = 1;
    input_HouseHoldingAppliances_ProfessionalWorkForce = 5;
    input_HouseHoldingAppliances_Workers = 147;
    input_HouseHoldingAppliances_ComputerServices = 0.600000023841858;
    input_HouseHoldingAppliances_LegalServices = 0.300000011920929;

    output_HouseHoldingAppliances_HouseHoldingAppliances = 18;

    // Legal Services
    cost_LegalServices = 25200000;
    budget_LegalServices = 599.7816;
    input_LegalServices_ExecutivesWorkForce = 2;
    input_LegalServices_ProfessionalWorkForce = 6;
    input_LegalServices_Workers = 12;
    input_LegalServices_ComputerServices = 0.7;

    output_LegalServices_LegalServices = 4.5;


    // Metalurgic
    cost_Metalurgic = 42000000;
    budget_Metalurgic = 17.4694;
    input_Metalurgic_Ore = 360;
    input_Metalurgic_Chemicals = 27;
    input_Metalurgic_ExecutivesWorkForce = 0;
    input_Metalurgic_ProfessionalWorkForce = 2;
    input_Metalurgic_Workers = 135;
    input_Metalurgic_ComputerServices = 0.300000011920929;
    input_Metalurgic_LegalServices = 0.150000005960464;

    output_Metalurgic_Metal = 180;

    // Refinery
    RefineryFact = 1;
    cost_Refinery = RefineryFact*35000000;
    budget_Refinery = RefineryFact*0;
    input_Refinery_Oil = RefineryFact*3000;
    input_Refinery_Chemicals = RefineryFact*10;
    input_Refinery_ExecutivesWorkForce = RefineryFact*0;
    input_Refinery_ProfessionalWorkForce = RefineryFact*2;
    input_Refinery_Workers = RefineryFact*100;
    input_Refinery_ComputerServices = RefineryFact*0.300000011920929;
    input_Refinery_LegalServices = RefineryFact*0.150000005960464;

    output_Refinery_Gasoline = RefineryFact*3000;

    // Mine
    cost_Mine = 20500000;
    budget_Mine = 35.9923;
    input_Mine_Chemicals = 12;
    input_Mine_ExecutivesWorkForce = 0;
    input_Mine_ProfessionalWorkForce = 1;
    input_Mine_Workers = 122;
    input_Mine_ComputerServices = 0.450000017881393;
    input_Mine_LegalServices = 0.150000005960464;

    output_Mine_Ore = 600;

    // OilRig
    OilRigFact = 1;
    cost_OilRig = OilRigFact*10500000;
    budget_OilRig = OilRigFact*0;
    input_OilRig_Chemicals = OilRigFact*2;
    input_OilRig_ExecutivesWorkForce = OilRigFact*0;
    input_OilRig_ProfessionalWorkForce = OilRigFact*1;
    input_OilRig_Workers = OilRigFact*25;
    input_OilRig_ComputerServices = OilRigFact*0.450000017881393;
    input_OilRig_LegalServices = OilRigFact*0.150000005960464;

    output_OilRig_Oil = OilRigFact*1500;


    // Textile
    cost_Textile = 12600000;
    budget_Textile = 10;
    input_Textile_OrganicMaterials = 22;
    input_Textile_Chemicals = 8;
    input_Textile_ExecutivesWorkForce = 0;
    input_Textile_ProfessionalWorkForce = 2;
    input_Textile_Workers = 98;
    input_Textile_ComputerServices = 0.12;
    input_Textile_LegalServices = 0.1;

    output_Textile_FabricsandThreads = 75;

implementation

end.
