unit FacIds;

interface
  uses
    Variants;
    
  type
    TFacId      = integer;
    PFacIdArray = ^TFacIdArray;
    TFacIdArray = array[0..0] of TFacId;

  const
    FID_None = 0;
    
    // Headquarters
    FID_MainHeadquarter  = 10;
    FID_OffcHeadquarter  = 12;
    FID_IndHeadquarter   = 11;
    FID_CommHeadquarter  = 13;
    FID_PubHeadquarter   = 14;

    // Residentials
    FID_hiClassLoCost  = 20;
    FID_midClassLoCost = 21;
    FID_lowClassLoCost = 22;

    FID_hiClass        = 25;
    FID_midClass       = 26;
    FID_lowClass       = 27;

    // Office
    FID_Office = 30;

    // Industries
    FID_Farm          = 40;
    FID_FoodProc      = 42;
    FID_Chemical      = 44;
    FID_Mine          = 46;
    FID_Textile       = 48;
    FID_Metal         = 50;
    FID_ElectComp     = 52;
    FID_Clothes       = 54;
    FID_Household     = 56;
    FID_BusMach       = 58;
    FID_Heavy         = 60;
    FID_Car           = 62;
    FID_Construction  = 64;
    FID_MovieStudios  = 65;
    FID_Pharmaceutics = 66;
    FID_Plastics      = 68;
    FID_Toys          = 69;
    FID_OilRig        = 200;
    FID_Refinery      = 201;
    FID_LiquorFact    = 202;
    FID_ChemicalMine  = 203;
    FID_SiliconMine   = 204;
    FID_StoneMine     = 205;
    FID_CoalMine      = 206;
    FID_LumberMill    = 207;
    FID_FurnitureInd  = 208;
    FID_PaperInd      = 209;
    FID_PrintingPlant = 210;
    FID_CDPlant       = 211;

    // Commerce
    FID_FoodStore   = 70;
    FID_ClotheStore = 71;
    FID_HHAStore    = 72;
    FID_BMStore     = 73;
    FID_CarStore    = 74;
    FID_Supermarket = 75;
    FID_Bar         = 76;
    FID_Restaurant  = 77;
    FID_Movie       = 78;
    FID_DrugStore   = 79;
    FID_ToyStore    = 90;
    FID_GasStation  = 91;
    FID_Furniture   = 92;
    FID_BookStore   = 93;
    FID_CompStore   = 94;
    FID_Funeral     = 95;
    FID_CDStore     = 96;

    // Business
    FID_SoftwareFirm = 80;
    FID_LawyerFirm   = 81;

    // Public Facilities
    FID_Hospital     = 100;
    FID_School       = 101;
    FID_Police       = 102;
    FID_FireStation  = 103;
    FID_Correctional = 104;
    FID_Park         = 105;
    FID_Disposal     = 106;
    FID_Amusement    = 107;
    FID_Jail         = 108;
    FID_Agency       = 109;

    // Special
    FID_Bank      = 110;
    FID_TVStation = 111;
    FID_TVAntena  = 112;

    // Warehouses
    FID_ColdWarehouse     = 120;
    FID_ChemicalWarehouse = 121;
    FID_GeneralWarehouse  = 122;
    FID_FabricsWarehouse  = 123;
    FID_OreWarehouse      = 124;
    FID_MegaWarehouseImp  = 125;
    FID_MegaWarehouseExp  = 126;

    // Luxury Facilities
    FID_LuxuryFac = 140;


  function NewFacIds(var Dest : PFacIdArray; const Source : array of TFacId) : integer ;
  function Contains(Ids : array of TFacId; Id : TFacId) : boolean;

implementation

  function NewFacIds(var Dest : PFacIdArray; const Source : array of TFacId) : integer ;
    begin
      result := high(Source) - low(Source) + 1;
      if result > 0
        then
          begin
            GetMem(Dest, result*sizeof(Dest[0]));
            move(Source, Dest^, result*sizeof(Dest[0]));
          end
        else Dest := nil;
    end;

  function Contains(Ids : array of TFacId; Id : TFacId) : boolean;
    var
      i : integer;
    begin
      i := low(Ids);
      while (i <= high(Ids)) and (Ids[i] <> Id) do
        inc(i);
      result := i <= high(Ids);
    end;

end.
