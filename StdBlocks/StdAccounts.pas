unit StdAccounts;

interface

  // Industries account Ids
  const
    // Farms
    accIdx_Farms             = 100;
    accIdx_Farms_Supplies    = 101;
    accIdx_Farms_Products    = 102;
    accIdx_Farms_Salaries    = 103;
    accIdx_Farms_Maintenance = 104;

    // BusinessMachines
    accIdx_BusinessMachines             = 110;
    accIdx_BusinessMachines_Supplies    = 111;
    accIdx_BusinessMachines_Products    = 112;
    accIdx_BusinessMachines_Salaries    = 113;
    accIdx_BusinessMachines_Maintenance = 114;

    // CarIndustry
    accIdx_CarIndustry             = 120;
    accIdx_CarIndustry_Supplies    = 121;
    accIdx_CarIndustry_Products    = 122;
    accIdx_CarIndustry_Salaries    = 123;
    accIdx_CarIndustry_Maintenance = 124;

    // Chemical
    accIdx_Chemical             = 130;
    accIdx_Chemical_Supplies    = 131;
    accIdx_Chemical_Products    = 132;
    accIdx_Chemical_Salaries    = 133;
    accIdx_Chemical_Maintenance = 134;

    // ClotheIndustry
    accIdx_ClotheIndustry             = 140;
    accIdx_ClotheIndustry_Supplies    = 141;
    accIdx_ClotheIndustry_Products    = 142;
    accIdx_ClotheIndustry_Salaries    = 143;
    accIdx_ClotheIndustry_Maintenance = 144;

    // ConstIndustry
    accIdx_ConstIndustry             = 150;
    accIdx_ConstIndustry_Supplies    = 151;
    accIdx_ConstIndustry_Products    = 152;
    accIdx_ConstIndustry_Salaries    = 153;
    accIdx_ConstIndustry_Maintenance = 154;

    // ElectIndustry
    accIdx_ElectIndustry             = 160;
    accIdx_ElectIndustry_Supplies    = 161;
    accIdx_ElectIndustry_Products    = 162;
    accIdx_ElectIndustry_Salaries    = 163;
    accIdx_ElectIndustry_Maintenance = 164;

    // FoodProc
    accIdx_FoodProc             = 170;
    accIdx_FoodProc_Supplies    = 171;
    accIdx_FoodProc_Products    = 172;
    accIdx_FoodProc_Salaries    = 173;
    accIdx_FoodProc_Maintenance = 174;

    // HeavyIndustry
    accIdx_HeavyIndustry             = 180;
    accIdx_HeavyIndustry_Supplies    = 181;
    accIdx_HeavyIndustry_Products    = 182;
    accIdx_HeavyIndustry_Salaries    = 183;
    accIdx_HeavyIndustry_Maintenance = 184;

    // HHAIndustry
    accIdx_HHAIndustry             = 190;
    accIdx_HHAIndustry_Supplies    = 191;
    accIdx_HHAIndustry_Products    = 192;
    accIdx_HHAIndustry_Salaries    = 193;
    accIdx_HHAIndustry_Maintenance = 194;

    // ToyIndustry
    accIdx_ToyIndustry             = 195;
    accIdx_ToyIndustry_Supplies    = 196;
    accIdx_ToyIndustry_Products    = 197;
    accIdx_ToyIndustry_Salaries    = 198;
    accIdx_ToyIndustry_Maintenance = 199;

    // MetalIndustry
    accIdx_MetalIndustry             = 200;
    accIdx_MetalIndustry_Supplies    = 201;
    accIdx_MetalIndustry_Products    = 202;
    accIdx_MetalIndustry_Salaries    = 203;
    accIdx_MetalIndustry_Maintenance = 204;

    // Mine
    accIdx_Mine             = 210;
    accIdx_Mine_Supplies    = 211;
    accIdx_Mine_Products    = 212;
    accIdx_Mine_Salaries    = 213;
    accIdx_Mine_Maintenance = 214;

    // OilRig
    accIdx_OilRig             = 215;
    accIdx_OilRig_Supplies    = 216;
    accIdx_OilRig_Products    = 217;
    accIdx_OilRig_Salaries    = 218;
    accIdx_OilRig_Maintenance = 219;

    // TextileIndustry
    accIdx_TextileIndustry             = 220;
    accIdx_TextileIndustry_Supplies    = 221;
    accIdx_TextileIndustry_Products    = 222;
    accIdx_TextileIndustry_Salaries    = 223;
    accIdx_TextileIndustry_Maintenance = 224;

    // Refinery
    accIdx_Refinery             = 225;
    accIdx_Refinery_Supplies    = 226;
    accIdx_Refinery_Products    = 227;
    accIdx_Refinery_Salaries    = 228;
    accIdx_Refinery_Maintenance = 229;

    // Computing
    accIdx_Computing             = 230;
    accIdx_Computing_Supplies    = 231;
    accIdx_Computing_Products    = 232;
    accIdx_Computing_Salaries    = 233;
    accIdx_Computing_Maintenance = 234;

    // PlasticIndustry
    accIdx_PlasticIndustry             = 235;
    accIdx_PlasticIndustry_Supplies    = 236;
    accIdx_PlasticIndustry_Products    = 237;
    accIdx_PlasticIndustry_Salaries    = 238;
    accIdx_PlasticIndustry_Maintenance = 239;

    // LegalServices
    accIdx_LegalServices             = 240;
    accIdx_LegalServices_Supplies    = 241;
    accIdx_LegalServices_Products    = 242;
    accIdx_LegalServices_Salaries    = 243;
    accIdx_LegalServices_Maintenance = 244;

    // PharmaIndustry
    accIdx_PharmaIndustry             = 400;
    accIdx_PharmaIndustry_Supplies    = 401;
    accIdx_PharmaIndustry_Products    = 402;
    accIdx_PharmaIndustry_Salaries    = 403;
    accIdx_PharmaIndustry_Maintenance = 404;

    // LiquorFact
    accIdx_LiquorFact             = 410;
    accIdx_LiquorFact_Supplies    = 411;
    accIdx_LiquorFact_Products    = 412;
    accIdx_LiquorFact_Salaries    = 413;
    accIdx_LiquorFact_Maintenance = 414;

    // LumberMill
    accIdx_LumberMill             = 420;
    accIdx_LumberMill_Supplies    = 421;
    accIdx_LumberMill_Products    = 422;
    accIdx_LumberMill_Salaries    = 423;
    accIdx_LumberMill_Maintenance = 424;

    // FurnitureIndustry
    accIdx_FurnitureIndustry             = 425;
    accIdx_FurnitureIndustry_Supplies    = 426;
    accIdx_FurnitureIndustry_Products    = 427;
    accIdx_FurnitureIndustry_Salaries    = 428;
    accIdx_FurnitureIndustry_Maintenance = 429;

    // MovieStudios
    accIdx_MovieStudios             = 435;
    accIdx_MovieStudios_Supplies    = 436;
    accIdx_MovieStudios_Products    = 437;
    accIdx_MovieStudios_Salaries    = 438;
    accIdx_MovieStudios_Maintenance = 439;

    // PaperIndustry
    accIdx_PaperIndustry             = 440;
    accIdx_PaperIndustry_Supplies    = 441;
    accIdx_PaperIndustry_Products    = 442;
    accIdx_PaperIndustry_Salaries    = 443;
    accIdx_PaperIndustry_Maintenance = 444;

    // PrintingPlant
    accIdx_PrintingPlant             = 450;
    accIdx_PrintingPlant_Supplies    = 451;
    accIdx_PrintingPlant_Products    = 452;
    accIdx_PrintingPlant_Salaries    = 453;
    accIdx_PrintingPlant_Maintenance = 454;

    // CDPlant
    accIdx_CDPlant                   = 460;
    accIdx_CDPlant_Supplies          = 461;
    accIdx_CDPlant_Products          = 462;
    accIdx_CDPlant_Salaries          = 463;
    accIdx_CDPlant_Maintenance       = 464;
    
  // Service blocks account Ids
  const
    // Car
    accIdx_CarStore           = 250;
    accIdx_CarStore_Supplies  = 251;
    accIdx_CarStore_Salaries  = 252;
    accIdx_CarStore_Sales     = 253;

    // Clothes Shop
    accIdx_ClothesStore          = 260;
    accIdx_ClothesStore_Supplies = 261;
    accIdx_ClothesStore_Salaries = 262;
    accIdx_ClothesStore_Sales    = 263;

    // FoodStore
    accIdx_FoodStore           = 270;
    accIdx_FoodStore_Supplies  = 271;
    accIdx_FoodStore_Salaries  = 272;
    accIdx_FoodStore_Sales     = 273;

    // Bar
    accIdx_Bar           = 280;
    accIdx_Bar_Supplies  = 291;
    accIdx_Bar_Salaries  = 292;
    accIdx_Bar_Sales     = 293;

    // Funeral Store
    accIdx_FuneralParlor           = 295;
    accIdx_FuneralParlor_Supplies  = 296;
    accIdx_FuneralParlor_Salaries  = 297;
    accIdx_FuneralParlor_Sales     = 298;

    // HHA Store
    accIdx_HHAStore           = 300;
    accIdx_HHAStore_Supplies  = 301;
    accIdx_HHAStore_Salaries  = 302;
    accIdx_HHAStore_Sales     = 303;

    // Toy Store
    accIdx_ToyStore           = 305;
    accIdx_ToyStore_Supplies  = 306;
    accIdx_ToyStore_Salaries  = 307;
    accIdx_ToyStore_Sales     = 308;

    // Super Market
    accIdx_SuperMarket           = 310;
    accIdx_SuperMarket_Supplies  = 311;
    accIdx_SuperMarket_Salaries  = 312;
    accIdx_SuperMarket_Sales     = 313;

    // Restaurant
    accIdx_Restaurant           = 320;
    accIdx_Restaurant_Supplies  = 321;
    accIdx_Restaurant_Salaries  = 322;
    accIdx_Restaurant_Sales     = 323;

    // Movie
    accIdx_Movie                = 330;
    accIdx_Movie_Supplies       = 331;
    accIdx_Movie_Salaries       = 332;
    accIdx_Movie_Sales          = 333;

    // Furniture Store
    accIdx_FurnitureStore           = 340;
    accIdx_FurnitureStore_Supplies  = 341;
    accIdx_FurnitureStore_Salaries  = 342;
    accIdx_FurnitureStore_Sales     = 343;

    // Book Store
    accIdx_BookStore           = 345;
    accIdx_BookStore_Supplies  = 346;
    accIdx_BookStore_Salaries  = 347;
    accIdx_BookStore_Sales     = 348;

    // Drug Store
    accIdx_DrugStore           = 500;
    accIdx_DrugStore_Supplies  = 501;
    accIdx_DrugStore_Salaries  = 502;
    accIdx_DrugStore_Sales     = 503;

    // Gas Station
    accIdx_GasStation           = 505;
    accIdx_GasStation_Supplies  = 506;
    accIdx_GasStation_Salaries  = 507;
    accIdx_GasStation_Sales     = 508;

    // Computer Store
    accIdx_ComputerStore          = 510;
    accIdx_ComputerStore_Supplies = 511;
    accIdx_ComputerStore_Salaries = 512;
    accIdx_ComputerStore_Sales    = 513;

    // CD Store
    accIdx_CDStore           = 520;
    accIdx_CDStore_Supplies  = 521;
    accIdx_CDStore_Salaries  = 522;
    accIdx_CDStore_Sales     = 523;

    
  const
    // Warehouses
    accIdx_Warehouses             = 350;
    accIdx_Warehouses_Supplies    = 351;
    accIdx_Warehouses_Products    = 352;
    accIdx_Warehouses_Salaries    = 353;
    accIdx_Warehouses_Maintenance = 354;

  // Media
  const
    // TV                                                                        
    accIdx_TV             = 360;
    accIdx_TV_Supplies    = 361;
    accIdx_TV_Products    = 362;
    accIdx_TV_Salaries    = 363;
    accIdx_TV_Maintenance = 364;


  procedure RegisterAccounts;

implementation

  uses
    Accounts, BasicAccounts;

  procedure RegisterAccounts;
    begin
      // Farms
      with TMetaAccount.Create(
        accIdx_Farms,
        accIdx_Industries,
        'Farms',
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Farms_Products,
        accIdx_Farms,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Farms_Supplies,
        accIdx_Farms,
        'Cost of raw materials',  // Cost of raw materials // Cost of goods sold // Sales revenue
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Farms_Salaries,
        accIdx_Farms,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Farms_Maintenance,
        accIdx_Farms,
        'Operating costs',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

    // BusinessMachines
      with TMetaAccount.Create(
        accIdx_BusinessMachines,
        accIdx_Industries,
        'Business Machines',
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_BusinessMachines_Products,
        accIdx_BusinessMachines,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_BusinessMachines_Supplies,
        accIdx_BusinessMachines,
        'Cost of raw materials',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_BusinessMachines_Salaries,
        accIdx_BusinessMachines,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_BusinessMachines_Maintenance,
        accIdx_BusinessMachines,
        'Operating costs',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // CarIndustry
      with TMetaAccount.Create(
        accIdx_CarIndustry,
        accIdx_Industries,
        'Car industries',
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_CarIndustry_Products,
        accIdx_CarIndustry,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_CarIndustry_Supplies,
        accIdx_CarIndustry,
        'Cost of raw materials',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_CarIndustry_Salaries,
        accIdx_CarIndustry,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_CarIndustry_Maintenance,
        accIdx_CarIndustry,
        'Operating costs',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // Chemical
      with TMetaAccount.Create(
        accIdx_Chemical,
        accIdx_Industries,
        'Chemical plants',
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Chemical_Products,
        accIdx_Chemical,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Chemical_Supplies,
        accIdx_Chemical,
        'Cost of raw materials',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Chemical_Salaries,
        accIdx_Chemical,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Chemical_Maintenance,
        accIdx_Chemical,
        'Operating costs',                                
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;                                            

      // ClotheIndustry
      with TMetaAccount.Create(
        accIdx_ClotheIndustry,
        accIdx_Industries,
        'Clothes',
        '',
        TAccount ) do
        begin                                                   
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_ClotheIndustry_Products,
        accIdx_ClotheIndustry,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_ClotheIndustry_Supplies,
        accIdx_ClotheIndustry,
        'Cost of raw materials',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_ClotheIndustry_Salaries,
        accIdx_ClotheIndustry,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_ClotheIndustry_Maintenance,
        accIdx_ClotheIndustry,
        'Operating costs',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // ConstIndustry
      with TMetaAccount.Create(
        accIdx_ConstIndustry,
        accIdx_Industries,
        'Construction',
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_ConstIndustry_Products,
        accIdx_ConstIndustry,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_ConstIndustry_Supplies,
        accIdx_ConstIndustry,
        'Cost of raw materials',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_ConstIndustry_Salaries,
        accIdx_ConstIndustry,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_ConstIndustry_Maintenance,
        accIdx_ConstIndustry,
        'Operating costs',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // ElectIndustry
      with TMetaAccount.Create(
        accIdx_ElectIndustry,
        accIdx_Industries,
        'Electronic',
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_ElectIndustry_Products,
        accIdx_ElectIndustry,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_ElectIndustry_Supplies,
        accIdx_ElectIndustry,
        'Cost of raw materials',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_ElectIndustry_Salaries,
        accIdx_ElectIndustry,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_ElectIndustry_Maintenance,
        accIdx_ElectIndustry,
        'Operating costs',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // FoodProc
      with TMetaAccount.Create(
        accIdx_FoodProc,
        accIdx_Industries,
        'Food Processing',
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_FoodProc_Products,
        accIdx_FoodProc,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_FoodProc_Supplies,
        accIdx_FoodProc,
        'Cost of raw materials',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_FoodProc_Salaries,
        accIdx_FoodProc,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_FoodProc_Maintenance,
        accIdx_FoodProc,
        'Operating costs',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // LiquorFact
      with TMetaAccount.Create(
        accIdx_LiquorFact,
        accIdx_Industries,
        'Liquor Production',
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_LiquorFact_Products,
        accIdx_LiquorFact,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_LiquorFact_Supplies,
        accIdx_LiquorFact,
        'Cost of raw materials',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_LiquorFact_Salaries,
        accIdx_LiquorFact,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_LiquorFact_Maintenance,
        accIdx_LiquorFact,
        'Operating costs',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // LumberMill
      with TMetaAccount.Create(
        accIdx_LumberMill,
        accIdx_Industries,
        'Lumber Mills',
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_LumberMill_Products,
        accIdx_LumberMill,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_LumberMill_Supplies,
        accIdx_LumberMill,
        'Cost of raw materials',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_LumberMill_Salaries,
        accIdx_LumberMill,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_LumberMill_Maintenance,
        accIdx_LumberMill,
        'Operating costs',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // FurnitureIndustry
      with TMetaAccount.Create(
        accIdx_FurnitureIndustry,
        accIdx_Industries,
        'Furniture',
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_FurnitureIndustry_Products,
        accIdx_FurnitureIndustry,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_FurnitureIndustry_Supplies,
        accIdx_FurnitureIndustry,
        'Cost of raw materials',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_FurnitureIndustry_Salaries,
        accIdx_FurnitureIndustry,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_FurnitureIndustry_Maintenance,
        accIdx_FurnitureIndustry,
        'Operating costs',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // PaperIndustry
      with TMetaAccount.Create(
        accIdx_PaperIndustry,
        accIdx_Industries,
        'Paper',
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_PaperIndustry_Products,
        accIdx_PaperIndustry,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_PaperIndustry_Supplies,
        accIdx_PaperIndustry,
        'Cost of raw materials',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_PaperIndustry_Salaries,
        accIdx_PaperIndustry,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_PaperIndustry_Maintenance,
        accIdx_PaperIndustry,
        'Operating costs',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // PrintingPlant
      with TMetaAccount.Create(
        accIdx_PrintingPlant,
        accIdx_Industries,
        'Books',
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_PrintingPlant_Products,
        accIdx_PrintingPlant,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_PrintingPlant_Supplies,
        accIdx_PrintingPlant,
        'Cost of raw materials',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_PrintingPlant_Salaries,
        accIdx_PrintingPlant,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_PrintingPlant_Maintenance,
        accIdx_PrintingPlant,
        'Operating costs',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // CDPlant
      with TMetaAccount.Create(
        accIdx_CDPlant,
        accIdx_Industries,
        'CD',
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_CDPlant_Products,
        accIdx_CDPlant,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_CDPlant_Supplies,
        accIdx_CDPlant,
        'Cost of raw materials',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_CDPlant_Salaries,
        accIdx_CDPlant,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_CDPlant_Maintenance,
        accIdx_CDPlant,
        'Operating costs',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // MovieStudios
      with TMetaAccount.Create(
        accIdx_MovieStudios,
        accIdx_Industries,
        'Movie Studios',
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_MovieStudios_Products,
        accIdx_MovieStudios,
        'Licences revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_MovieStudios_Supplies,
        accIdx_MovieStudios,
        'Cost of materials',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_MovieStudios_Salaries,
        accIdx_MovieStudios,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_MovieStudios_Maintenance,
        accIdx_MovieStudios,
        'Production costs',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // HeavyIndustry
      with TMetaAccount.Create(
        accIdx_HeavyIndustry,
        accIdx_Industries,
        'Heavy industry',
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_HeavyIndustry_Products,
        accIdx_HeavyIndustry,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_HeavyIndustry_Supplies,
        accIdx_HeavyIndustry,
        'Cost of raw materials',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_HeavyIndustry_Salaries,
        accIdx_HeavyIndustry,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_HeavyIndustry_Maintenance,
        accIdx_HeavyIndustry,
        'Operating costs',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // HHAIndustry
      with TMetaAccount.Create(
        accIdx_HHAIndustry,
        accIdx_Industries,
        'Household',
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_HHAIndustry_Products,
        accIdx_HHAIndustry,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_HHAIndustry_Supplies,
        accIdx_HHAIndustry,
        'Cost of raw materials',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_HHAIndustry_Salaries,
        accIdx_HHAIndustry,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_HHAIndustry_Maintenance,
        accIdx_HHAIndustry,
        'Operating costs',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // MetalIndustry
      with TMetaAccount.Create(
        accIdx_MetalIndustry,
        accIdx_Industries,
        'Metallurgic industry',
        '',
        TAccount ) do
        begin
          Rankeable := true;
          Taxable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_MetalIndustry_Products,
        accIdx_MetalIndustry,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_MetalIndustry_Supplies,
        accIdx_MetalIndustry,
        'Cost of raw materials',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_MetalIndustry_Salaries,
        accIdx_MetalIndustry,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_MetalIndustry_Maintenance,
        accIdx_MetalIndustry,
        'Operating costs',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // PlasticIndustry
      with TMetaAccount.Create(
        accIdx_PlasticIndustry,
        accIdx_Industries,
        'Plastics',
        '',
        TAccount ) do
        begin
          Rankeable := true;
          Taxable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_PlasticIndustry_Products,
        accIdx_PlasticIndustry,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_PlasticIndustry_Supplies,
        accIdx_PlasticIndustry,
        'Cost of raw materials',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_PlasticIndustry_Salaries,
        accIdx_PlasticIndustry,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_PlasticIndustry_Maintenance,
        accIdx_PlasticIndustry,
        'Operating costs',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // Mine
      with TMetaAccount.Create(
        accIdx_Mine,
        accIdx_Industries,
        'Mines',
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Mine_Products,
        accIdx_Mine,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Mine_Supplies,
        accIdx_Mine,
        'Cost of raw materials',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Mine_Salaries,
        accIdx_Mine,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Mine_Maintenance,
        accIdx_Mine,
        'Operating costs',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // OilRig
      with TMetaAccount.Create(
        accIdx_OilRig,
        accIdx_Industries,
        'Oil rigs',
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_OilRig_Products,
        accIdx_OilRig,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_OilRig_Supplies,
        accIdx_OilRig,
        'Cost of raw materials',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_OilRig_Salaries,
        accIdx_OilRig,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_OilRig_Maintenance,
        accIdx_OilRig,
        'Operating costs',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // Refinery
      with TMetaAccount.Create(
        accIdx_Refinery,
        accIdx_Industries,
        'Refineries',
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Refinery_Products,
        accIdx_Refinery,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Refinery_Supplies,
        accIdx_Refinery,
        'Cost of raw materials',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Refinery_Salaries,
        accIdx_Refinery,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Refinery_Maintenance,
        accIdx_Refinery,
        'Operating costs',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // Textile Industry
      with TMetaAccount.Create(
        accIdx_TextileIndustry,
        accIdx_Industries,
        'Textile industry',
        '',
        TAccount ) do
        begin
          Rankeable := true;
          Taxable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_TextileIndustry_Products,
        accIdx_TextileIndustry,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_TextileIndustry_Supplies,
        accIdx_TextileIndustry,
        'Cost of raw materials',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_TextileIndustry_Salaries,
        accIdx_TextileIndustry,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_TextileIndustry_Maintenance,
        accIdx_TextileIndustry,
        'Operating costs',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // Computing
      with TMetaAccount.Create(
        accIdx_Computing,
        accIdx_Firms,
        'Software',
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Computing_Products,
        accIdx_Computing,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Computing_Supplies,
        accIdx_Computing,
        'Cost of services',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Computing_Salaries,
        accIdx_Computing,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Computing_Maintenance,
        accIdx_Computing,
        'Operating costs',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // Legal Services
      with TMetaAccount.Create(
        accIdx_LegalServices,
        accIdx_Firms,
        'Legal Services',
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_LegalServices_Products,
        accIdx_LegalServices,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_LegalServices_Supplies,
        accIdx_LegalServices,
        'Cost of services',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_LegalServices_Salaries,
        accIdx_LegalServices,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_LegalServices_Maintenance,
        accIdx_LegalServices,
        'Operating costs',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // Pharmaceutics
      with TMetaAccount.Create(
        accIdx_PharmaIndustry,
        accIdx_Industries,
        'Pharmaceutics',
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_PharmaIndustry_Products,
        accIdx_PharmaIndustry,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_PharmaIndustry_Supplies,
        accIdx_PharmaIndustry,
        'Cost of raw materials',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_PharmaIndustry_Salaries,
        accIdx_PharmaIndustry,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_PharmaIndustry_Maintenance,
        accIdx_PharmaIndustry,
        'Operating costs',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // Toys
      with TMetaAccount.Create(
        accIdx_ToyIndustry,
        accIdx_Industries,
        'Toys',
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_ToyIndustry_Products,
        accIdx_ToyIndustry,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_ToyIndustry_Supplies,
        accIdx_ToyIndustry,
        'Cost of raw materials',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_ToyIndustry_Salaries,
        accIdx_ToyIndustry,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_ToyIndustry_Maintenance,
        accIdx_ToyIndustry,
        'Operating costs',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // Warehouses
      with TMetaAccount.Create(                       
        accIdx_Warehouses,
        accIdx_Special,
        'Warehouses',
        '',
        TAccount ) do
        begin
          Rankeable := true;
          Taxable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Warehouses_Products,
        accIdx_Warehouses,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Warehouses_Supplies,
        accIdx_Warehouses,
        'Cost of goods sold',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Warehouses_Salaries,
        accIdx_Warehouses,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Warehouses_Maintenance,
        accIdx_Warehouses,
        'Operating costs',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;


     // Cars
      with TMetaAccount.Create(
        accIdx_CarStore,
        accIdx_Commerce,
        'Cars',
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_CarStore_Sales,
        accIdx_CarStore,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_CarStore_Supplies,
        accIdx_CarStore,
        'Cost of goods sold',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_CarStore_Salaries,
        accIdx_CarStore,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // Clothes Shop
      with TMetaAccount.Create(
        accIdx_ClothesStore,
        accIdx_Commerce,
        'Clothes',
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_ClothesStore_Sales,
        accIdx_ClothesStore,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_ClothesStore_Supplies,
        accIdx_ClothesStore,
        'Cost of goods sold',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_ClothesStore_Salaries,
        accIdx_ClothesStore,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

     // FoodStore
      with TMetaAccount.Create(
        accIdx_FoodStore,
        accIdx_Commerce,
        'Food',
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_FoodStore_Sales,
        accIdx_FoodStore,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_FoodStore_Supplies,
        accIdx_FoodStore,
        'Cost of goods sold',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_FoodStore_Salaries,
        accIdx_FoodStore,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // Bar
      with TMetaAccount.Create(
        accIdx_Bar,
        accIdx_Commerce,
        'Bars',
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Bar_Sales,
        accIdx_Bar,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Bar_Supplies,
        accIdx_Bar,
        'Cost of goods sold',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Bar_Salaries,
        accIdx_Bar,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // Funeral
      with TMetaAccount.Create(
        accIdx_FuneralParlor,
        accIdx_Commerce,
        'Funerals',
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_FuneralParlor_Sales,
        accIdx_FuneralParlor,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_FuneralParlor_Supplies,
        accIdx_FuneralParlor,
        'Cost of funeral',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_FuneralParlor_Salaries,
        accIdx_FuneralParlor,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // HHA Store
      with TMetaAccount.Create(
        accIdx_HHAStore,
        accIdx_Commerce,
        'Household Appliances',                   
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_HHAStore_Sales,
        accIdx_HHAStore,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_HHAStore_Supplies,
        accIdx_HHAStore,
        'Cost of goods sold',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_HHAStore_Salaries,
        accIdx_HHAStore,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // Toy Store
      with TMetaAccount.Create(
        accIdx_ToyStore,
        accIdx_Commerce,
        'Toys',
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_ToyStore_Sales,
        accIdx_ToyStore,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_ToyStore_Supplies,
        accIdx_ToyStore,
        'Cost of goods sold',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_ToyStore_Salaries,
        accIdx_ToyStore,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // Furniture Store
      with TMetaAccount.Create(
        accIdx_FurnitureStore,
        accIdx_Commerce,
        'Furniture',
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_FurnitureStore_Sales,
        accIdx_FurnitureStore,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_FurnitureStore_Supplies,
        accIdx_FurnitureStore,
        'Cost of goods sold',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_FurnitureStore_Salaries,
        accIdx_FurnitureStore,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // Book Store
      with TMetaAccount.Create(
        accIdx_BookStore,
        accIdx_Commerce,
        'Books',
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_BookStore_Sales,
        accIdx_BookStore,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_BookStore_Supplies,
        accIdx_BookStore,
        'Cost of goods sold',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_BookStore_Salaries,
        accIdx_BookStore,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // CD Store
      with TMetaAccount.Create(
        accIdx_CDStore,
        accIdx_Commerce,
        'CDs',
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_CDStore_Sales,
        accIdx_CDStore,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_CDStore_Supplies,
        accIdx_CDStore,
        'Cost of goods sold',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_CDStore_Salaries,
        accIdx_CDStore,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // Computer Store
      with TMetaAccount.Create(
        accIdx_ComputerStore,
        accIdx_Commerce,
        'Computers',                      
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_ComputerStore_Sales,
        accIdx_ComputerStore,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_ComputerStore_Supplies,
        accIdx_ComputerStore,
        'Cost of goods sold',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_ComputerStore_Salaries,
        accIdx_ComputerStore,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

     // Super Market
      with TMetaAccount.Create(
        accIdx_SuperMarket,
        accIdx_Commerce,
        'Supermarkets',
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_SuperMarket_Sales,
        accIdx_SuperMarket,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_SuperMarket_Supplies,
        accIdx_SuperMarket,
        'Cost of goods sold',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_SuperMarket_Salaries,
        accIdx_SuperMarket,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

    // Restaurant
      with TMetaAccount.Create(
        accIdx_Restaurant,
        accIdx_Commerce,
        'Restaurants',
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Restaurant_Sales,
        accIdx_Restaurant,
        'Sales revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Restaurant_Supplies,
        accIdx_Restaurant,
        'Cost of goods sold',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Restaurant_Salaries,
        accIdx_Restaurant,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // Movie
      with TMetaAccount.Create(
        accIdx_Movie,
        accIdx_Commerce,
        'Movie Theaters',
        '',
        TAccount ) do
        begin
          Rankeable := true;
          Taxable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Movie_Sales,
        accIdx_Movie,
        'Movie revenues',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Movie_Supplies,
        accIdx_Movie,
        'Cost of movie licenses',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Movie_Salaries,                                  
        accIdx_Movie,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // Drug store
      with TMetaAccount.Create(
        accIdx_DrugStore,
        accIdx_Commerce,
        'Drug Stores',
        '',
        TAccount ) do
        begin
          Rankeable := true;
          Taxable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_DrugStore_Sales,
        accIdx_DrugStore,
        'Sales revenues',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_DrugStore_Supplies,
        accIdx_DrugStore,
        'Cost of goods sold',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_DrugStore_Salaries,
        accIdx_DrugStore,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // Gas station
      with TMetaAccount.Create(
        accIdx_GasStation,
        accIdx_Commerce,
        'Gas Stations',
        '',
        TAccount ) do
        begin
          Rankeable := true;
          Taxable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_GasStation_Sales,
        accIdx_GasStation,
        'Revenues',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_GasStation_Supplies,
        accIdx_GasStation,
        'Cost of goods sold',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_GasStation_Salaries,
        accIdx_GasStation,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // TV
      with TMetaAccount.Create(
        accIdx_TV,
        accIdx_Special,
        'TV',
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_TV_Products,
        accIdx_TV,
        'Advertisement revenue',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_TV_Supplies,
        accIdx_TV,
        'Production costs',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_TV_Salaries,
        accIdx_TV,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_TV_Maintenance,
        accIdx_TV,
        'Operating costs',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

    end;

end.


