unit Standards;

interface

  // Ministeries
  const
    nidMinistry_Health        = 1;
    nidMinistry_Education     = 2;
    nidMinistry_Defense       = 3;
    nidMinistry_Agriculture   = 4;
    nidMinistry_LightIndustry = 5;
    nidMinistry_HeavyIndustry = 6;
    nidMinistry_Commerce      = 7;
    nidMinistry_Housing       = 8;

  // Ministry Headquarters
  const
    tidBlock_MinistryHeadquarterConstr = 'MinistryHeadquarterConstr';
    tidBlock_MinistryHeadquarter       = 'MinistryHeadquarter';
    tidFacility_MinistryHeadquarter    = 'MinistryHeadquarter';
    vidFacility_MinistryHeadquarter    = 171;

  // Inventions
  const
    // Direction
    tidInventionKind_Direction             = 'Direction';

    // Basic Facilities
    tidInventionKind_Farming               = 'Farming';
    tidInventionKind_IndustrialFacilities  = 'Industries';
    tidInventionKind_OfficeAndResidentials = 'Residentials';
    tidInventionKind_ServiceFacilities     = 'ServiceFacilities';
    tidInventionKind_PublicFacilities      = 'PublicFacilities';
    tidInventionKind_BusinessFacilities    = 'BusinessFacilities';

    // TV
    tidInventionKind_Television            = 'TV';

    // Business
    tidInventionKind_Software              = 'Software';
    tidInventionKind_LegalServices         = 'LegalServices';
    tidInventionKind_Offices               = 'Offices';
    tidInventionKind_Banking               = 'Banking';

    // Fancy
    tidInventionKind_Monuments             = 'Monuments';

    // Large Industries
    tidInventionKind_LargeFarms            = 'LargeFarms';
    tidInventionKind_LargeChemical         = 'LargeChemical';
    tidInventionKind_LargeMines            = 'LargeMines';
    tidInventionKind_LargeChemMines        = 'LargeChemMines';
    tidInventionKind_LargeSiliconMines     = 'LargeSiliconMines';
    tidInventionKind_LargeStoneMines       = 'LargeStoneMines';
    tidInventionKind_LargeCoalMines        = 'LargeCoalMines';
    tidInventionKind_Plastics              = 'Plastics';
    tidInventionKind_LargeFoodProc         = 'LargeFoodProc';
    tidInventionKind_LargeTextile          = 'LargeTextile';
    tidInventionKind_LargeClothes          = 'LargeClothes';
    tidInventionKind_LargeElectComp        = 'LargeElectComp';
    tidInventionKind_LargeMetallurgy       = 'LargeMetallurgy';
    tidInventionKind_Construction          = 'Construction';
    tidInventionKind_HeavyIndustry         = 'HeavyIndustry';
    tidInventionKind_CarIndustry           = 'Cars';
    tidInventionKind_LargeHHA              = 'LargeHHA';
    tidInventionKind_BMIndustry            = 'BMIndustry';
    tidInventionKind_PaperIndustry         = 'Paper';
    tidInventionKind_PrintingPlant         = 'Printing';
    tidInventionKind_CDPlant               = 'CD';

    // Commerce
    tidInventionKind_SuperMarkets          = 'Supermarkets';
    tidInventionKind_Bars                  = 'Bars';
    tidInventionKind_Restaurants           = 'Restaurants';
    tidInventionKind_MovieTheaters         = 'Movies';
    tidInventionKind_Funerals              = 'Funerals';

    // Illegal Business
    tidInventionKind_Agencies              = 'Agencies';

    // Movie Studios
    tidInventionKind_MovieStudios          = 'MovieStudios';

  const
    tidLicence_Farms                       = 'Farms';
    tidLicence_FoodProc                    = 'FoodProc';
    tidLicence_Chemical                    = 'Chemical';
    tidLicence_Cars                        = 'Cars';
    tidLicence_Construction                = 'Construction';
    tidLicence_Electronics                 = 'Electronics';
    tidLicence_HHA                         = 'HHA';
    tidLicence_Metallurgy                  = 'Metallurgy';
    tidLicence_Mines                       = 'Mines';
    tidLicence_ChemMines                   = 'ChemMines';
    tidLicence_SiliconMines                = 'SiliconMines';
    tidLicence_StoneMines                  = 'StoneMines';
    tidLicence_CoalMines                   = 'CoalMines';
    tidLicence_Textile                     = 'Textile';
    tidLicence_Clothing                    = 'Clothes';
    tidLicence_Heavy                       = 'HeavyInd';
    tidLicence_BMIndustry                  = 'BMIndustry';
    tidLicence_Toys                        = 'ToyIndustry';
    tidLicence_PharmaIndustry              = 'PharmaIndustry';
    tidLicence_OilRigs                     = 'OilRigs';
    tidLicence_Refinery                    = 'Refinery';
    tidLicence_Plastics                    = 'Plastics';
    tidLicence_Liquors                     = 'Liquors';
    tidLicence_LumberMills                 = 'LumberMills';
    tidLicence_FurnitureInd                = 'FurnitureProd';

  const
    // Main Headquarter
    nidInvention_DistributedDirection      = 1;
    nidInvention_BasicDirectionTechnics    = 2;
    nidInvention_AdvancedDirectionTechnics = 3;

    // Basic
    nidInvention_Farming                   = 1;
    nidInvention_IndustrialFacilities      = 1;
    nidInvention_OfficeAndResidentials     = 1;
    nidInvention_ServiceFacilities         = 1;
    nidInvention_PublicFacilities          = 1;

    // Industry
    nidInvention_BigFarms              = 10;
    nidInvention_EtClimate             = 11;
    nidInvention_WaterPumps            = 12;
    nidInvention_WaterControl          = 13;
    nidInvention_SyntheticGround       = 14;
    nidInvention_AutoIrrigation        = 15;
    nidInvention_InducedPhotosynthesis = 16;
    nidInvention_SyntheticAnimalFood   = 17;
    nidInvention_BiogenicFeeding       = 18;
    nidInvention_AdvancedGenetics      = 19;
    nidInvention_EtGenetics            = 140;
    nidInvention_ColonialMutantPlants  = 141;
    nidInvention_VegetalMusic          = 142;
    nidInvention_SkinlessPigs          = 143;
    nidInvention_MiceRecycling         = 144;
    nidInvention_SunlightCorrection    = 145;
    nidInvention_PlagueControlSystem   = 146;
    nidInvention_PersonalHygieneCtrl   = 147;

    nidInvention_BigChemicalPlant      = 20;

    nidInvention_BigFoodProc           = 30;
    nidInvention_RecurrentFoodProd     = 31;
    nidInvention_SolarRefrigeration    = 32;
    nidInvention_WaterApplications     = 33;
    nidInvention_SiliconApplications   = 34;
    nidInvention_FoodQualityControl    = 35;
    nidInvention_BrumeSyndromeCorr     = 36;
    nidInvention_ROOGCorrection        = 37;
    nidInvention_BasicFoodRefinement   = 38;
    nidInvention_AdvancedFoodRef       = 39;
    nidInvention_OrganicChemicals      = 150;
    nidInvention_RadiactiveEnlargement = 151;

    nidInvention_BigMines              = 40;
    nidInvention_BigMetalurgy          = 50;

    nidInvention_BigTextile            = 60;
    nidInvention_NATOpolyester         = 61;
    nidInvention_InverseKnitting       = 62;
    nidInvention_MutantCaterpillars    = 63;
    nidInvention_SyntheticThreads      = 64;

    nidInvention_BigClothesIndustry    = 70;
    nidInvention_BigElectronicIndustry = 80;
    nidInvention_BigHHA                = 90;
    nidInvention_ConstructionIndustry  = 100;
    nidInvention_CarIndustry           = 110;
    nidInvention_BMIndustry            = 120;
    nidInvention_HeavyIndustry         = 130;

    // Commerce
    nidInvention_BigMarkets            = 10;
    nidInvention_Bars                  = 11;
    nidInvention_Restaurants           = 12;
    nidInvention_Movies                = 13;

    // Residential and Offices
    nidInvention_OfficeBuildings       = 10;
    nidInvention_SoftwareFirms         = 20;
    nidInvention_LegalServices         = 30;
    nidInvention_TV                    = 40;
    nidInvention_Banking               = 50;

    // Public
    nidInvention_Monuments = 2;

  const
    tidPublicFac_Police    = 'Police';
    tidPublicFac_Health    = 'Health';
    tidPublicFac_Fire      = 'Fire';
    tidPublicFac_School    = 'School';
    tidPublicFac_Beauty    = 'Beauty';
    tidPublicFac_Disposal  = 'Disposal';
    tidPublicFac_College   = 'College';
    tidPublicFac_Museum    = 'Museum';
    tidPublicFac_Sport     = 'Sport';
    tidPublicFac_Jail      = 'Jail';
    tidPublicFac_Cementery = 'Cementery';

  const
    tidBlock_SRVCOMMON = 'SRVCOMMON_';
    tidBlock_SRVILLEGALCOMMON = 'SRVILLEGALCOMMON_';
    tidBlock_SPECIALCOMMON = 'SPECIALCOMMON_';
    tidBlock_WHCOMMON = 'WHCOMMON'; 

  const
    tidFacilityKind_DistributedDirection = 'DirectionFacilities';
    tidFacilityKind_Farms                = 'Farms';
    tidFacilityKind_IndustrialFacilities = 'IndustrialFacilities';
    tidFacilityKind_Residentials         = 'ResidentialFacilities';
    tidFacilityKind_ServiceFacilities    = 'ServiceFacilities';
    tidFacilityKind_PublicFacilities     = 'PublicFacilities';
    tidFacilityKind_BusinessFacilities   = 'BusinessFacilities';
    tidFacilityKind_Special              = 'Special';
    tidFacilityKind_Warehouse            = 'UWWarehouses';

  const
    tidFacilityStage_Construction = 'Construction';
    tidFacilityStage_Complete     = 'Completed';

  const
    tmeInvention_VeryShort = 10;  // NOTE: Research time is in hours!
    tmeInvention_Short     = 20;
    tmeInvention_Normal    = 50;
    tmeInvention_Long      = 90;
    tmeInvention_VeryLong  = 120;

  const
    maxHQAdv = 2400;

  procedure CopyCommonFacilities( Family, ClusterName, FacKind, TechKind : string );

implementation                                                   

  uses                                               
    Kernel, ClassStorage, Collection;

  procedure CopyCommonFacilities( Family, ClusterName, FacKind, TechKind : string );
    var
      facCount  : integer;
      facI      : integer;
      MF, newMF : TMetaFacility;
      NewFacs   : TCollection;
    begin
      NewFacs  := TCollection.Create( 0, rkUse );
      facCount := TheClassStorage.ClassCount[tidClassFamily_Facilities];
      for facI := 0 to pred(facCount) do
        begin
          MF := TMetaFacility(TheClassStorage.ClassByIdx[tidClassFamily_Facilities, facI]);
          if pos( Family, MF.Id ) = 1
            then
              begin
                newMF := CMetaFacility(MF.ClassType).CopyFrom( MF, ClusterName + MF.Id );
                newMF.ClusterName    := ClusterName;
                newMF.FacilityKind   := FacKind;
                newMF.TechnologyKind := TechKind;
                newMF.Technology     := MF.Technology;
                NewFacs.Insert( newMF );
              end;
        end;
      for facI := 0 to pred(NewFacs.Count) do
        TMetaFacility(NewFacs[facI]).Register( tidClassFamily_Facilities );
      NewFacs.Free;
    end;


end.
