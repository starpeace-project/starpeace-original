unit CommonTasks;

interface

  const
    tidTask_MainHeadquarter   = 'MainHq';
    tidTask_OffcHeadquarter   = 'OfficesHq';
    tidTask_IndHeadquarter    = 'IndustriesHq';
    tidTask_CommHeadquarter   = 'CommerceHq';
    tidTask_PublHeadquarter   = 'PublicHq';

    tidTask_SelectProduct     = 'SelectProduct';
    tidTask_AnyProduct        = 'AnyProduct';
    tidTask_BuildGenWarehouse = 'BuildGenWH';
    tidTask_BuildChmWarehouse = 'BuildChmWH';
    tidTask_AnySore           = 'AnyStore';
    tidTask_BuildStore        = 'BuildStore';
    tidTask_BuildMart         = 'BuildMart';
    tidTask_BuildPharmacy     = 'BuildPharmacy';
    tidTask_BuildToyStore     = 'BuildToyStore';
    tidTask_BuildHHAStore     = 'BuildHAAStore';
    tidTask_BuildFurnitureStore = 'BuildFurnitureStore';
    tidTask_BuildClotheStore  = 'BuildClotheStore';
    tidTask_BuildGasStation   = 'BuildGasStation';
    tidTask_BuildCDStore      = 'BuildCDStore';
    tidTask_MakeProfit        = 'MakeProfit';
    tidTask_ResearchSomething = 'RshSomething';
    tidTask_BuyAds            = 'BuyAds';
    tidTask_CloneAds          = 'CloneAds';
    tidTask_SellToAll         = 'SellToAll';
    tidTask_UpgradeIndustry   = 'UpgradeIndustry';
    tidTask_ManuallyConnect   = 'ManuallyConnect';

    tidTask_BuildPharmaInd    = 'BuildPharmaInd';
    tidTask_BuildToyInd       = 'BuildToyInd';
    tidTask_BuildHHAInd       = 'BuildHHAInd';
    tidTask_BuildFurnitureInd = 'BuildFurnitureInd';
    tidTask_BuildClotheInd    = 'BuildClotheInd';
    tidTask_BuildRefinery     = 'BuildRefinery';
    tidTask_BuildCDPlant      = 'BuildCDPlant';
    tidTask_HireSupplies      = 'HireSupplies';
    tidTask_OfferSupplies     = 'OfferSupplies';
    tidTask_AskLoan           = 'AskLoan';

    tidTask_BuildLCResidential= 'BuildLCResidential';


  procedure RegisterTasks;
  procedure AssembleTutorialBranch(Id, RootId, Cluster, WhID, InputName, StoreID, ResearchID, IndustryID, MainHQID, TutorialID : string; theAccountId, CommerceId, IndId : integer; TaskWeight: single);

implementation

  uses
    Kernel, Standards, Protocol, Tasks, InformativeTask, ClusterTask, HeadquarterTasks,
    ResidentialTasks, BuildFacilitiesTask, ChoiceTask, MakeProfitTask, ResearchTask,
    BuildWarehouse, LoanMoneyTask, TaskUtils, FacIds, FoodStore, ClothesShop, HHAStore, MathUtils,
    BasicAccounts, HireOfferTask, BuyAdsTask, CloneAdsTask, SellAllTask, UpgradeFacTask, ManuallyConnectTask;


  procedure RegisterTasks;
    begin

      // Main Headquarter
      with TMetaHeadquarterTask.Create(
        tidTask_MainHeadquarter,
        'Main Headquarter',
        '',               // No description
        '',               // This is a Template
        1,                // Once every 4 virtual-day
        TBuildMainHeadquarterTask) do
        begin
          StageCount    := 1;
          //Technology    := 'DistributedDirection'; //tidInventionKind_Direction
          FacId         := FID_MainHeadquarter;
          BlockClass    := 'TMainHeadquarter';
          Priority      := tprHigh;
          KindId        := 'MainHq';
          NotTitle      := 'Build the Company Headquarter';
          ChoiceMethod  := fcmFirst;
          Register(tidClassFamily_Tasks);
        end;

      // Select Product Task
      with TMetaTask.Create(
        tidTask_SelectProduct,
        'Select Product',
        '', // No description
        '', // This is a Template
        1,
        TRandomTask) do
        begin
          Priority := tprHigh - 1;
          Register(tidClassFamily_Tasks);
        end;

      // Build Mega Warehouse
      with TMetaBuildWarehouseTask.Create(
        tidTask_BuildGenWarehouse,
        'Build Warehouse',
        '',               // No description
        '',               // This is a Template
        1,
        TBuildWarehouseTask) do
        begin
          StageCount   := 1; // ??????
          BlockClass   := 'TMegaStorage'; // >> Change
          Priority     := tprNormal;
          KindId       := 'BuildFacility';
          NotTitle     := 'Build a General Warehouse';
          Register(tidClassFamily_Tasks);
        end;

      // Build Chemical Warehouse
      with TMetaBuildWarehouseTask.Create(
        tidTask_BuildChmWarehouse,
        'Build Warehouse',
        '',               // No description
        '',               // This is a Template
        1,
        TBuildWarehouseTask) do
        begin
          StageCount   := 1; // ??????
          FacId        := FID_ChemicalWarehouse;
          BlockClass   := 'TChemicalStorage';
          Priority     := tprNormal;
          KindId       := 'BuildFacility';
          NotTitle     := 'Build a Chemical Warehouse';
          ChoiceMethod := fcmFirst;
          Register(tidClassFamily_Tasks);
        end;

      // Commerce Headquarter
      with TMetaHeadquarterTask.Create(
        tidTask_CommHeadquarter,
        'Commerce Headquarter',
        '',               // No description
        '',               // This is a Template
        1,
        TBuildHeadquarterTask) do
        begin
          StageCount    := 1;
          Technology    := 'BasicCommerce'; //tidInventionKind_ServiceFacilities;
          FacId         := FID_CommHeadquarter;
          Priority      := tprNormal - 1;
          KindId        := 'BuildFacility';
          NotTitle      := 'Build the Commerce Headquarters';
          ChoiceMethod  := fcmFirst;
          Register(tidClassFamily_Tasks);
        end;

      // Industry Headquarter
      with TMetaHeadquarterTask.Create(
        tidTask_IndHeadquarter,
        'Industries Headquarter',
        '',               // No description
        '',               // This is a Template
        1,
        TBuildHeadquarterTask) do
        begin
          StageCount    := 1;
          Technology    := 'Industries'; //tidInventionKind_IndustrialFacilities;
          FacId         := FID_IndHeadquarter;
          Priority      := tprNormal - 4;
          KindId        := 'BuildFacility';
          NotTitle      := 'Build the Industry Headquarters';
          ChoiceMethod  := fcmFirst;
          Register(tidClassFamily_Tasks);
        end;

      // Build Mart
      with TMetaBuildFacilitiesTask.Create(
        tidTask_BuildMart,
        'Build a Mart',
        '',
        '',
        1,
        TBuildFacilitiesTask) do
        begin
          FacId        := FID_Supermarket;
          BlockClass   := 'TSuperMarketBlock';
          Priority     := tprNormal - 2;
          KindId       := 'BuildFacility';
          NotTitle     := 'Build A Household Appliance Store';
          ChoiceMethod := fcmCheaper;
          Register(tidClassFamily_Tasks);
        end;

      // Build Pharmacy
      with TMetaBuildFacilitiesTask.Create(
        tidTask_BuildPharmacy,
        'Build a Pharmacy',
        '',
        '',
        1,
        TBuildFacilitiesTask) do
        begin
          FacId        := FID_DrugStore;
          BlockClass   := 'TDrugStoreBlock';
          Priority     := tprNormal - 2;
          KindId       := 'BuildFacility';
          NotTitle     := 'Build a Pharmacy';
          ChoiceMethod := fcmCheaper;
          Register(tidClassFamily_Tasks);
        end;

      // Build Toy Store
      with TMetaBuildFacilitiesTask.Create(
        tidTask_BuildToyStore,
        'Build a Toy Store',
        '',
        '',
        1,
        TBuildFacilitiesTask) do
        begin
          FacId        := FID_ToyStore;
          BlockClass   := 'TToyStoreBlock';
          Priority     := tprNormal - 2;
          KindId       := 'BuildFacility';
          NotTitle     := 'Build a Toy Store';
          ChoiceMethod := fcmCheaper;
          Register(tidClassFamily_Tasks);
        end;

      // Build HHA Store
      with TMetaBuildFacilitiesTask.Create(
        tidTask_BuildHHAStore,
        'Build a HHA Store',
        '',
        '',
        1,
        TBuildFacilitiesTask) do
        begin
          FacId        := FID_HHAStore;
          BlockClass   := 'THHAStoreBlock';
          Priority     := tprNormal - 2;
          KindId       := 'BuildFacility';
          NotTitle     := 'Build a HHA Store';
          ChoiceMethod := fcmCheaper;
          Register(tidClassFamily_Tasks);
        end;

      // Build Furniture Store
      with TMetaBuildFacilitiesTask.Create(
        tidTask_BuildFurnitureStore,
        'Build a Furniture Store',
        '',
        '',
        1,
        TBuildFacilitiesTask) do
        begin
          FacId        := FID_Furniture;
          BlockClass   := 'TFurnitureStoreBlock';
          Priority     := tprNormal - 2;
          KindId       := 'BuildFacility';
          NotTitle     := 'Build a Furniture Store';
          ChoiceMethod := fcmCheaper;
          Register(tidClassFamily_Tasks);
        end;

      // Build Cloth Store
      with TMetaBuildFacilitiesTask.Create(
        tidTask_BuildClotheStore,
        'Build a Cloth Store',
        '',
        '',
        1,
        TBuildFacilitiesTask) do
        begin
          FacId        := FID_ClotheStore;
          BlockClass   := 'TClothesShopBlock';
          Priority     := tprNormal - 2;
          KindId       := 'BuildFacility';
          NotTitle     := 'Build a Cloth Store';
          ChoiceMethod := fcmCheaper;
          Register(tidClassFamily_Tasks);
        end;

      // Build Gas Station
      with TMetaBuildFacilitiesTask.Create(
        tidTask_BuildGasStation,
        'Build a Gas Station',
        '',
        '',
        1,
        TBuildFacilitiesTask) do
        begin
          FacId        := FID_GasStation;
          BlockClass   := 'TGasStationBlock';
          Priority     := tprNormal - 2;
          KindId       := 'BuildFacility';
          NotTitle     := 'Build a Gas Station';
          ChoiceMethod := fcmCheaper;
          Register(tidClassFamily_Tasks);
        end;

      // Build CD Store
      with TMetaBuildFacilitiesTask.Create(
        tidTask_BuildCDStore,
        'Build a CD Store',
        '',
        '',
        1,
        TBuildFacilitiesTask) do
        begin
          FacId        := FID_CDStore;
          BlockClass   := 'TCDStoreBlock';
          Priority     := tprNormal - 2;
          KindId       := 'BuildFacility';
          NotTitle     := 'Build a CD Store';
          ChoiceMethod := fcmCheaper;
          Register(tidClassFamily_Tasks);
        end;

      // Build Low Class Residential
      with TMetaBuildFacilitiesTask.Create(
        tidTask_BuildLCResidential,
        'Build a Low Class Residential',
        '',
        '',
        1,
        TBuildFacilitiesTask) do
        begin
          FacId        := FID_lowClass;
          BlockClass   := 'TPopulatedBlock';
          Priority     := tprNormal - 2;
          KindId       := 'BuildFacility';
          NotTitle     := 'Build a Low Class Residential Store';
          ChoiceMethod := fcmCheaper;
          Register(tidClassFamily_Tasks);
        end;

      // Buy Ads
      with TMetaBuyAdsTask.Create(
        tidTask_BuyAds,
        'Buy Advertisement',
        '',
        '',
        1,
        TBuyAdsTask) do
        begin
          Priority := tprNormal - 3;
          KindId   := 'BuyAds'; // >> Change later when the page is done..
          NotTitle := 'Buy Ads for one of your Store';
          Register(tidClassFamily_Tasks);
        end;

      // Clone Ads
      with TMetaCloneAdsTask.Create(
        tidTask_CloneAds,
        'Clone Advertisement Setting',
        '',
        '',
        1,
        TCloneAdsTask) do
        begin
          Priority := tprNormal - 3;
          KindId   := 'CloneAds'; // >> Change later when the page is done..
          NotTitle := 'Clone the Ads Setting in All Your Stores';
          Register(tidClassFamily_Tasks);
        end;

      // Any Product
      with TMetaTask.Create(
        tidTask_AnyProduct,
        'Product',
        '',   // No description
        '',   // None
        1,
        TSuperTask) do
        begin
          Priority := tprNormal;
          Register(tidClassFamily_Tasks);
        end;

      // Make Profit
      with TMetaMakeProfitTask.Create(
        tidTask_MakeProfit,
        'Make Profit',
        '',
        '',
        1,
        TMakeProfitTask) do
        begin
          Priority := tprNormal - 3;
          KindId   := 'GrowMoney'; // >> Change later when the page is done..
          NotTitle := 'Make Money with Your Stores';
          Register(tidClassFamily_Tasks);
        end;

      // Research Something
      with TMetaResearchTask.Create(
        tidTask_ResearchSomething,
        'Research Something',
        '',               // No description
        '',               // This is a Template
        1,
        TResearchTask) do
        begin
          StageCount    := 1;
          Priority      := tprNormal - 4;
          KindId        := 'Research';
          NotTitle      := 'Research';
          Register(tidClassFamily_Tasks);
        end;

      // Build Pharma Industry
      with TMetaBuildFacilitiesTask.Create(
        tidTask_BuildPharmaInd,
        'Build a Pharmaceutic Industry',
        '',
        '',
        1,
        TBuildFacilitiesTask) do
        begin
          FacId        := FID_Pharmaceutics;
          BlockClass   := 'TPharmaIndustryBlock';
          Priority     := tprNormal - 5;
          KindId       := 'BuildFacility';
          NotTitle     := 'Build a Pharmaceutic Industry';
          ChoiceMethod := fcmCheaper;
          Register(tidClassFamily_Tasks);
        end;

      // Build Toy Industry
      with TMetaBuildFacilitiesTask.Create(
        tidTask_BuildToyInd,
        'Build a Toy Factory',
        '',
        '',
        1,
        TBuildFacilitiesTask) do
        begin
          FacId        := FID_Toys;
          BlockClass   := 'TToysBlock';
          Priority     := tprNormal - 5;
          KindId       := 'BuildFacility';
          NotTitle     := 'Build a Toy Factory';
          ChoiceMethod := fcmCheaper;
          Register(tidClassFamily_Tasks);
        end;

      // Build HHA Industry
      with TMetaBuildFacilitiesTask.Create(
        tidTask_BuildHHAInd,
        'Build a Toy Factory',
        '',
        '',
        1,
        TBuildFacilitiesTask) do
        begin
          FacId        := FID_Household;
          BlockClass   := 'THouseHoldingAppliancesBlock';
          Priority     := tprNormal - 5;
          KindId       := 'BuildFacility';
          NotTitle     := 'Build a Household Appliances Factory';
          ChoiceMethod := fcmCheaper;
          Register(tidClassFamily_Tasks);
        end;

      // Build Furniture Industry
      with TMetaBuildFacilitiesTask.Create(
        tidTask_BuildFurnitureInd,
        'Build a Toy Factory',
        '',
        '',
        1,
        TBuildFacilitiesTask) do
        begin
          FacId        := FID_FurnitureInd;
          BlockClass   := 'TFurnitureIndustryBlock';
          Priority     := tprNormal - 5;
          KindId       := 'BuildFacility';
          NotTitle     := 'Build a Furniture Factory';
          ChoiceMethod := fcmCheaper;
          Register(tidClassFamily_Tasks);
        end;

      // Build Clothes Industry
      with TMetaBuildFacilitiesTask.Create(
        tidTask_BuildClotheInd,
        'Build a Cloth Industry',
        '',
        '',
        1,
        TBuildFacilitiesTask) do
        begin
          FacId        := FID_Clothes;
          BlockClass   := 'TClothingsIndustryBlock';
          Priority     := tprNormal - 5;
          KindId       := 'BuildFacility';
          NotTitle     := 'Build a Cloth Industry';
          ChoiceMethod := fcmCheaper;
          Register(tidClassFamily_Tasks);
        end;

      // Build Refinery
      with TMetaBuildFacilitiesTask.Create(
        tidTask_BuildRefinery,
        'Build a Refinery',
        '',
        '',
        1,
        TBuildFacilitiesTask) do
        begin
          FacId        := FID_Refinery;
          BlockClass   := 'TRefineryBlock';
          Priority     := tprNormal - 5;
          KindId       := 'BuildFacility';
          NotTitle     := 'Build a Refinery';
          ChoiceMethod := fcmCheaper;
          Register(tidClassFamily_Tasks);
        end;

      // Build CD Plant
      with TMetaBuildFacilitiesTask.Create(
        tidTask_BuildCDPlant,
        'Build a CD Plant',
        '',
        '',
        1,
        TBuildFacilitiesTask) do
        begin
          FacId        := FID_CDPlant;
          BlockClass   := 'TCDBlock';
          Priority     := tprNormal - 5;
          KindId       := 'BuildFacility';
          NotTitle     := 'Build a Refinery';
          ChoiceMethod := fcmCheaper;
          Register(tidClassFamily_Tasks);
        end;

      // Sell To All
      with TMetaSellAllTask.Create(
        tidTask_SellToAll,
        'Sell To All Your Store',
        '',
        '',
        1,
        TSellAllTask) do
        begin
          Priority := tprNormal - 6;
          KindId   := 'SellToAll'; // >> Change later when the page is done..
          NotTitle := 'Sell To All Your Store';
          Register(tidClassFamily_Tasks);
        end;

      // Offer stuff
      with TMetaHireOfferTask.Create(
        tidTask_OfferSupplies,
        'Offer Products',
        '',
        '',
        1,
        THireOfferTask) do
        begin
          Priority := tprNormal - 8;
          Hire := false;
          KindId       := 'OfferProducts';
          NotTitle     := 'Offer Products';
          Register(tidClassFamily_Tasks);
        end;

      // Hire stuff
      with TMetaHireOfferTask.Create(
        tidTask_HireSupplies,
        'Hire Suppliers',
        '',
        '',
        1,
        THireOfferTask) do
        begin
          Priority := tprNormal - 8;
          Hire := true;
          KindId       := 'HireSuppliers';
          NotTitle     := 'Hire Suppliers';
          Register(tidClassFamily_Tasks);
        end;

      with TMetaLoanMoneyTask.Create(
        tidTask_AskLoan,
        'Ask Loan',
        '',
        '',
        1,
        TLoanMoneyTask) do
        begin
          Priority := tprNormal - 8;
          Loan     := 100000000;
          KindId   := 'AskLoan';
          NotTitle := 'Ask for a loan';
          Register(tidClassFamily_Tasks);
        end;

      // Upgrade Industry
      with TMetaUpgradeFacTask.Create(
        tidTask_UpgradeIndustry,
        'Upgrade Your Industry',
        '',
        '',
        1,
        TUpgradeFacTask) do
        begin
          Priority := tprNormal - 9;
          KindId   := 'UpgradeFac'; // >> Change later when the page is done..
          NotTitle := 'Upgrade Your Industry';
          Register(tidClassFamily_Tasks);
        end;

      // Connect Manually
      with TMetaManuallyConnectTask.Create(
        tidTask_ManuallyConnect,
        'Connect Your Industry to Your Warehouse',
        '',
        '',
        1,
        TManuallyConnectTask) do
        begin
          Priority := tprNormal - 9;
          KindId   := 'ManuallyConnect'; // >> Change later when the page is done..
          NotTitle := 'Connect Your Industry to Your Warehouse';
          Register(tidClassFamily_Tasks);
        end;

    end;

  procedure AssembleTutorialBranch(Id, RootId, Cluster, WhID, InputName, StoreID, ResearchID, IndustryID, MainHQID, TutorialID : string; theAccountId, CommerceId, IndId : integer; TaskWeight: single);
    var
      selID : string;
    begin
      selID := Cluster + '_' + ID;

      // Product ID
      with TMetaTask.Create(
        tidTask_AnyProduct,
        selID,
        RootID) do
        begin
          Weight := TaskWeight;
          Register(tidClassFamily_Tasks);
        end;

      {// Build General Warehouse (inporter)
      with TMetaBuildWarehouseTask.Create(
        WhID,
        selID + 'ImpWH',
        selID) do
        begin
          Priority    := tprNormal;
          Kind        := whkImport;
          SelectInput := InputName;
          Register(tidClassFamily_Tasks);
        end;

      // Commerce Headquarter
      with TMetaHeadquarterTask.Create(
        tidTask_CommHeadquarter,
        selID + 'CommHQ',
        selID) do
        begin
          Priority := tprNormal - 1;
          Register(tidClassFamily_Tasks);
        end;   }

      // Build Store
      with TMetaBuildFacilitiesTask.Create(
        StoreID,
        selID + StoreID,
        selID) do
        begin
          Priority := tprNormal - 2;
          Register(tidClassFamily_Tasks);
        end;

      // Build Store
      with TMetaBuildFacilitiesTask.Create(
        StoreID,
        selID + StoreID + '9',
        selID) do
        begin
          Priority := tprNormal - 3;
          FacCount := 9;
          Register(tidClassFamily_Tasks);
        end;

      // Build General Warehouse (importer)
      with TMetaBuildWarehouseTask.Create(
        WhID,
        selID + 'ImpWH',
        selID) do
        begin
          Priority    := tprNormal - 4;
          FacId       := FID_MegaWarehouseImp;
          Kind        := whkImport;
          SelectInput := InputName;
          ChoiceMethod := fcmFirst;
          Register(tidClassFamily_Tasks);
        end;

      // Click the sell to all your store button
      with TMetaSellAllTask.Create(
        tidTask_SellToAll,
        selID + 'SellToAll',
        selID) do
        begin
          Goods := CommerceId;
          Priority   := tprNormal - 5;
          FacId      := FID_MegaWarehouseImp;
          Register(tidClassFamily_Tasks);
        end;

      // Hire products for your WH
      with TMetaHireOfferTask.Create(
        tidTask_HireSupplies,
        selID + 'HireWH',
        selID) do
        begin
          FacId    := FID_MegaWarehouseImp;
          Goods    := CommerceId;
          Priority := tprNormal - 6;
          Hire     := true;
          Register(tidClassFamily_Tasks);
        end;

     // Ask for a loan
      with TMetaLoanMoneyTask.Create(
        tidTask_AskLoan,
        selID + 'AskLoan',
        selID) do
        begin
          Loan := 200000000;
          Priority  := tprNormal - 7;
          Register(tidClassFamily_Tasks);
        end;

      // Main Headquarter
      with TMetaHeadquarterTask.Create(
        tidTask_MainHeadquarter,
        selID + MainHQID,
        selID) do
        begin
          Priority := tprNormal - 8;
          Register(tidClassFamily_Tasks);
        end;

      // Ask Advertisement in one of your stores
      with TMetaBuyAdsTask.Create(
        tidTask_BuyAds,
        selID + 'BuyAds',
        selID) do
        begin
          Priority   := tprNormal - 9;
          Register(tidClassFamily_Tasks);
        end;

      // Clone Advertisement in all your stores
      with TMetaCloneAdsTask.Create(
        tidTask_CloneAds,
        selID + 'CloneAds',
        selID) do
        begin
          Priority   := tprNormal - 10;
          FacId      := CommerceId;
          Register(tidClassFamily_Tasks);
        end;

      // Wait until commerce make $x profit
      with TMetaMakeProfitTask.Create(
        tidTask_MakeProfit,
        selID + 'MakeProfit',
        selID) do
        begin
          Goal      := 1000000;
          AccountId := accIdx_Commerce;
          Priority  := tprNormal - 11;
          Register(tidClassFamily_Tasks);
        end;

      // Research Something
      with TMetaResearchTask.Create(
        tidTask_ResearchSomething,
        selID + ResearchID,
        selID) do
        begin
          Technology := ResearchID;
          StoreId    := IndId;
          Priority   := tprNormal - 12;
          Register(tidClassFamily_Tasks);
        end;

      // Build Industry
      with TMetaBuildFacilitiesTask.Create(
        IndustryID,
        selID + IndustryID,
        selID) do
        begin
          Priority := tprNormal - 13;
          Register(tidClassFamily_Tasks);
        end;

      {// Connect your Industry to your Warehouse
      with TMetaManuallyConnectTask.Create(
        tidTask_ManuallyConnect,
        selID + 'ManuallyConnect',
        selID) do
        begin
          Priority   := tprNormal - 14;
          ToFacId    := FID_MegaWarehouseImp;
          FromFacId  := IndId;
          Register(tidClassFamily_Tasks);
        end;}

      {// Hire cheaper suppliers for your Industry
      with TMetaHireOfferTask.Create(
        tidTask_HireSupplies,
        selID + 'HireInd',
        selID) do
        begin
          Priority := tprNormal - 15;
          Hire := true;
          Register(tidClassFamily_Tasks);
        end;}

      // Build General Warehouse (exporter)
      with TMetaBuildWarehouseTask.Create(
        WhID,
        selID + 'ExpWH',
        selID) do
        begin
          Priority    := tprNormal - 16;
          FacId       := FID_MegaWarehouseExp;
          Kind        := whkExport;
          SelectInput := InputName;
          ChoiceMethod := fcmFirst;
          Register(tidClassFamily_Tasks);
        end;

      // Offer/Hire stuff
      with TMetaHireOfferTask.Create(
        tidTask_OfferSupplies,
        selID + 'OfferWH',
        selID) do
        begin
          FacId       := FID_MegaWarehouseExp;
          Goods       := CommerceId;
          Priority    := tprNormal - 17;
          Hire        := false;
          Register(tidClassFamily_Tasks);
        end;

      {// Another Loan, only if needed
      with TMetaLoanMoneyTask.Create(
        tidTask_AskLoan,
        selID + 'AskLoan2',
        selID) do
        begin
          Loan := 250000000;
          Priority  := tprNormal - 17;
          Register(tidClassFamily_Tasks);
        end;}

      {// Upgrade Industry, if necessary
      with TMetaUpgradeFacTask.Create(
        tidTask_UpgradeIndustry,
        selID + 'UpgradeIndustry',
        selID) do
        begin
          Priority   := tprNormal - 15;
          FacId      := IndId;
          Register(tidClassFamily_Tasks);
        end;}

      {// Build Residential
       with TMetaBuildFacilitiesTask.Create(
        tidTask_BuildLCResidential,
        selID + tidTask_BuildLCResidential,
        selID) do
        begin
          Priority := tprNormal - 18;
          Register(tidClassFamily_Tasks);
        end;}

    end;

end.
