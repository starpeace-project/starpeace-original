unit StdFluids;

interface

  const
    tidFluid_ConstructionForce      = 'Construction';
    tidFluid_Machinery              = 'Machinery';
    tidFluid_FreshFood              = 'FreshFood';
    tidFluid_OrganicMat             = 'OrganicMat';
    tidFluid_Chemicals              = 'Chemicals';
    tidFluid_LegalServ              = 'LegalServ';
    tidFluid_CompServ               = 'CompServ';
    tidFluid_Ore                    = 'Ore';
    tidFluid_OreChems               = 'OreChems';
    tidFluid_OreSilicon             = 'OreSilicon';
    tidFluid_OreStone               = 'OreStone';
    tidFluid_OreCoal                = 'OreCoal';
    tidFluid_ElabFood               = 'ElabFood';
    tidFluid_Metals                 = 'Metals';
    tidFluid_Plastics               = 'Plastics';
    tidFluid_Drugs                  = 'Drugs';
    tidFluid_ElectComp              = 'ElectComp';
    tidFluid_BusinessMachines       = 'BusinessMachines';
    tidFluid_Cars                   = 'Cars';
    tidFluid_FabThreads             = 'FabricThreads';
    tidFluid_Clothes                = 'Clothes';
    tidFluid_HouseHoldingAppliances = 'HouseHoldingAppliances';
    tidFluid_Toys                   = 'Toys';
    tidFluid_Oil                    = 'Oil';
    tidFluid_Gasoline               = 'Gasoline';
    tidFluid_Liquors                = 'Liquors';
    tidFluid_Advertisement          = 'Advertisement';
    tidFluid_Films                  = 'Films';
    tidFluid_Timber                 = 'Timber';
    tidFluid_Furniture              = 'Furniture';
    tidFluid_Books                  = 'Books';
    tidFluid_Paper                  = 'Paper';
    tidFluid_CDs                    = 'CDs';
    tidFluid_PrintedMaterial        = 'PrintedMaterial';

  const
    unid_None          = 0;
    unid_Advertisement = 1;
    unid_CompServ      = 2;
    unid_LegalServ     = 3;

  const
    tidGate_ConstructionForce      = tidFluid_ConstructionForce;
    tidGate_Machinery              = tidFluid_Machinery;
    tidGate_FreshFood              = tidFluid_FreshFood;
    tidGate_OrganicMat             = tidFluid_OrganicMat;
    tidGate_Chemicals              = tidFluid_Chemicals;
    tidGate_LegalServ              = tidFluid_LegalServ;
    tidGate_CompServ               = tidFluid_CompServ;
    tidGate_Ore                    = tidFluid_Ore;
    tidGate_ElabFood               = tidFluid_ElabFood;
    tidGate_Metals                 = tidFluid_Metals;
    tidGate_Plastics               = tidFluid_Plastics;
    tidGate_Gasoline               = tidFluid_Gasoline;
    tidGate_Oil                    = tidFluid_Oil;
    tidGate_OreChems               = tidFluid_OreChems;
    tidGate_OreSilicon             = tidFluid_OreSilicon;
    tidGate_OreStone               = tidFluid_OreStone;
    tidGate_OreCoal                = tidFluid_OreCoal;
    tidGate_Drugs                  = tidFluid_Drugs;
    tidGate_ElectComp              = tidFluid_ElectComp;
    tidGate_BusinessMachines       = tidFluid_BusinessMachines;
    tidGate_Cars                   = tidFluid_Cars;
    tidGate_FabThreads             = tidFluid_FabThreads;
    tidGate_Clothes                = tidFluid_Clothes;
    tidGate_HouseHoldingAppliances = tidFluid_HouseHoldingAppliances;
    tidGate_Toys                   = tidFluid_Toys;
    tidGate_Liquors                = tidFluid_Liquors;
    tidGate_Advertisement          = tidFluid_Advertisement;
    tidGate_Films                  = tidFluid_Films;
    tidGate_Timber                 = tidFluid_Timber;
    tidGate_Furniture              = tidFluid_Furniture;
    tidGate_Books                  = tidFluid_Books;
    tidGate_Paper                  = tidFluid_Paper;
    tidGate_CDs                    = tidFluid_CDs;
    tidGate_PrintedMaterial        = tidFluid_PrintedMaterial;

  procedure RegisterMetaFluids;

implementation

  uses
    Kernel, MediaGates;

  // RegisterMetaFluids

  procedure RegisterMetaFluids;
    begin
      with TMetaFluid.Create(
        tidFluid_ConstructionForce,
        'Construction Force',
        'Construction Force',
        'tons',
        'tons/day',
        24,
        0.001,
        1000,
        100 ) do
        begin
          Options := Options + [mfTradeable, mfImportable, mfConstruction];
          StorageVol := 500000;
          Register( 'Fluids' );
        end;
      with TMetaFluid.Create(
        tidFluid_Machinery,
        'Machinery',
        'Machinery',
        'machines',
        'machines/day',
        24,
        0.0005,
        400,
        8000 ) do
        begin
          Options := Options + [mfTradeable, mfImportable, mfConstruction];
          StorageVol := 5000;
          Register( 'Fluids' );
        end;
      with TMetaFluid.Create(
        tidFluid_FreshFood,
        'Fresh Food',
        'Fresh food is produced mainly by farms. It englobes vegetables, grains and meat.',
        'kg',
        'kg/day',
        24,
        0.0008,
        1,
        4 ) do
        begin
          Options := Options + [mfStorable, mfTradeable];
          StorageVol := 5000000;
          Register( 'Fluids' );
        end;
      with TMetaFluid.Create(
        tidFluid_OrganicMat,
        'Organic Materials',
        'Vegetal fibers and other subproducts of farms.',
        'kg',
        'kg/day',
        24,
        0.0002,
        1,
        10 ) do
        begin
          Options := Options + [mfTradeable, mfImportable, mfStorable];
          StorageVol := 1000000;
          Register( 'Fluids' );
        end;
      with TMetaFluid.Create(
        tidFluid_Chemicals,
        'Chemicals',
        'All kind of chemical products.',
        'kg',
        'kg/day',
        24,
        0.0005,
        1,
        20 ) do
        begin
          Options := Options + [mfTradeable, mfImportable, mfStorable];
          StorageVol := 100000;
          Register( 'Fluids' );
        end;
      with TMetaFluid.Create(
        tidFluid_LegalServ,
        'Legal Services',
        'Legal counceling services. This will make a facility more competitive.',
        'hours',
        'hours/week',
        7*24,
        0.0001,
        80,
        250 ) do
        begin
          Options  := Options + [mfTradeable, mfCompanyFluid, mfImportable];
          UNId     := unid_LegalServ;
          CnxLimit := 500;
          StorageVol := 0;
          Register( 'Fluids' );
        end;
      with TMetaFluid.Create(
        tidFluid_CompServ,
        'Computer Services',
        'Information management, process automatization.',
        'hours',
        'hours/week',
        7*24,
        0.0001,
        80,
        200 ) do
        begin
          Options  := Options + [mfTradeable, mfCompanyFluid, mfImportable];
          UNId     := unid_CompServ;
          CnxLimit := 500;
          StorageVol := 0;
          Register( 'Fluids' );
        end;
      with TMetaFluid.Create(
        tidFluid_Ore,
        'Ore',
        'All kind of mineral products not ready yet for industrials applications.',
        'kg',
        'kg/day',
        24,
        0.0005,
        1,
        1 ) do
        begin
          Options := Options + [mfImportable, mfTradeable];
          StorageVol := 100000;
          Register( 'Fluids' );
        end;
      with TMetaFluid.Create(
        tidFluid_OreChems,
        'Raw Chemicals',
        'Raw chemicals. Have to be processed to obtain usable chemical products.',
        'kg',
        'kg/day',
        24,
        0.0005,
        1,
        1 ) do
        begin
          Options := Options + [mfImportable, mfTradeable];
          StorageVol := 100000;
          Register( 'Fluids' );
        end;
      with TMetaFluid.Create(
        tidFluid_OreSilicon,
        'Silicon',
        'Silicon used at electronic component factories.',
        'kg',
        'kg/day',
        24,
        0.0005,
        1,
        1 ) do
        begin
          Options := Options + [mfImportable, mfTradeable];
          StorageVol := 200000;
          Register( 'Fluids' );
        end;
      with TMetaFluid.Create(
        tidFluid_OreStone,
        'Stone',
        'Stone is used to produce construction materials.',
        'kg',
        'kg/day',
        24,
        0.0005,
        1,
        1 ) do
        begin
          Options := Options + [mfImportable, mfTradeable];
          StorageVol := 200000;
          Register( 'Fluids' );
        end;
      with TMetaFluid.Create(
        tidFluid_OreCoal,
        'Coal',
        'Coal is required by metal plants.',
        'kg',
        'kg/day',
        24,
        0.0005,
        1,
        1 ) do
        begin
          Options := Options + [mfImportable, mfTradeable];
          StorageVol := 200000;
          Register( 'Fluids' );
        end;
      with TMetaFluid.Create(
        tidFluid_ElabFood,
        'Processed Food',
        'All kind of elaborated food ready for consuming.',
        'items',
        'items/day',
        24,
        0.0001,
        1,
        15 ) do
        begin
          Options := Options + [mfTradeable, mfImportable, mfStorable];
          StorageVol := 2500000;
          Register( 'Fluids' );
        end;
      with TMetaFluid.Create(
        tidFluid_Metals,
        'Metals',
        'All kind of metals ready for industrial applications.',
        'kg',
        'kg/day',
        24,
        0.0005,
        1,
        8 ) do
        begin
          Options := Options + [mfTradeable, mfImportable, mfStorable];
          StorageVol := 25000;
          Register( 'Fluids' );
        end;
      with TMetaFluid.Create(
        tidFluid_Plastics,
        'Plastics',
        'All kind of Plastics ready for industrial applications.',
        'kg',
        'kg/day',
        24,
        0.0001,
        1,
        5 ) do
        begin
          Options := Options + [mfTradeable, mfImportable, mfStorable];
          StorageVol := 20000;
          Register( 'Fluids' );
        end;
      with TMetaFluid.Create(
        tidFluid_Drugs,
        'Pharmaceutics',
        'Pharmaceutics.',
        'kg',
        'kg/day',
        24,
        0.0004,
        1,
        40 ) do
        begin
          Options := Options + [mfTradeable, mfImportable, mfStorable];
          StorageVol := 10000;
          Register( 'Fluids' );
        end;
      with TMetaFluid.Create(
        tidFluid_ElectComp,
        'Electronic Components',
        'All kind of metals ready for industrial applications.',
        'items',
        'items/day',
        24,
        0.0002,
        1,
        50 ) do
        begin
          Options := Options + [mfTradeable, mfImportable, mfStorable];
          StorageVol := 10000;
          Register( 'Fluids' );
        end;
      with TMetaFluid.Create(
        tidFluid_BusinessMachines,
        'Business Machines',
        'Business machines such as computers, printers, photocopiers, ect.',
        'items',
        'items/day',
        24,
        0.001,
        4,
        800 ) do
        begin
          Options := Options + [mfTradeable, mfStorable, mfImportable, mfConstruction];
          StorageVol := 10000;
          Register( 'Fluids' );
        end;
      with TMetaFluid.Create(
        tidFluid_HouseHoldingAppliances,
        'Household appliances',
        'Any domestic device, refrigerators, washing machines, air conditioners, etc.',
        'items',
        'items/day',
        24,
        0.0005,
        10,
        110 ) do
        begin
          Options := Options + [mfTradeable, mfImportable, mfStorable];
          StorageVol := 10000;
          Register( 'Fluids' );
        end;
      with TMetaFluid.Create(
        tidFluid_Oil,
        'Crude Oil',
        'Oil.',
        'lt',
        'lt/day',
        24,
        0.0002,
        10,
        0.2 ) do
        begin
          Options := Options + [mfTradeable, mfStorable, mfImportable];
          StorageVol := 100000;
          Register( 'Fluids' );
        end;
      with TMetaFluid.Create(
        tidFluid_Gasoline,
        'Gasoline',
        'Gasoline.',
        'lt',
        'lt/day',
        24,
        0.0002,
        10,
        0.7 ) do
        begin
          Options := Options + [mfTradeable, mfStorable, mfImportable];
          StorageVol := 100000;
          Register( 'Fluids' );
        end;
      with TMetaFluid.Create(
        tidFluid_Toys,
        'Toys',
        'Toys.',
        'items',
        'items/day',
        24,
        0.0005,
        10,
        32 ) do
        begin
          Options := Options + [mfTradeable, mfStorable, mfImportable];
          StorageVol := 10000;
          Register( 'Fluids' );
        end;
      with TMetaFluid.Create(
        tidFluid_Cars,
        'Cars',
        'Cars.',
        'cars',
        'cars/week',
        24*7,
        0.001, // ??????
        800,
        7000 ) do
        begin
          Options := Options + [mfTradeable, mfImportable, mfStorable];
          StorageVol := 200;
          Register( 'Fluids' );
        end;
      with TMetaFluid.Create(
        tidFluid_FabThreads,
        'Fabrics and Threads',
        'Fabric and thread ready for looming.',
        'meters',
        'meters/day',
        24,
        0.0002,
        2,
        6 ) do
        begin
          Options := Options + [mfTradeable, mfImportable, mfStorable];
          StorageVol := 10000;
          Register( 'Fluids' );
        end;
      with TMetaFluid.Create(
        tidFluid_Clothes,
        'Clothes',
        'Stuff to wear on.',
        'items',
        'items/day',
        24,
        0.0001,
        1,
        25 ) do
        begin
          Options := Options + [mfTradeable, mfImportable, mfStorable];
          StorageVol := 10000;
          Register( 'Fluids' );
        end;
      with TMetaFluid.Create(
        tidFluid_Liquors,
        'Liquors',
        'Any kind of booze',
        'items',
        'items/day',
        24,
        0.0002,
        1,
        5 ) do
        begin
          Options := Options + [mfTradeable, mfStorable, mfImportable];
          StorageVol := 2500000;
          Register( 'Fluids' );
        end;
      with TMetaFluid.Create(
        tidFluid_Advertisement,
        'Advertisement',
        'Advertisement',
        'hits',
        'hits/day',
        24,
        0,
        0,
        0.5 ) do
        begin
          Options  := Options + [mfTradeable, mfImportable, mfCompanyFluid];
          UNId     := unid_Advertisement;
          CnxLimit := 500;
          StorageVol := 0;
          Register( 'Fluids' );
        end;
      with TMetaFluid.Create(
        tidFluid_Films,
        'Films',
        'Movies',
        'Films',
        'Films/day',
        1, // 24
        0.00002,
        1,
        50) do
        begin
          Options  := Options + [mfTradeable, mfStorable, mfImportable];
          CnxLimit := 500;
          StorageVol := 0;
          Register( 'Fluids' );
        end;
      with TMetaFluid.Create(
        tidFluid_Timber,
        'Timber',
        'Any kind of timber',
        'kg',
        'kg/day',
        24,
        0.0005,
        700,
        2 ) do
        begin
          Options := Options + [mfTradeable, mfImportable, mfStorable];
          StorageVol := 500000;
          Register( 'Fluids' );
        end;
      with TMetaFluid.Create(
        tidFluid_Furniture,
        'Furniture',
        'Furniture for home',
        'items',
        'items/day',
        24,
        0.0005,
        700,
        100 ) do
        begin
          Options := Options + [mfTradeable, mfImportable, mfStorable];
          StorageVol := 15000;
          Register( 'Fluids' );
        end;
      with TMetaFluid.Create(
        tidFluid_Books,
        'Books',
        'Books in general',
        'books',
        'books/day',
        24,
        0.0001,
        700,
        15 ) do
        begin
          Options := Options + [mfTradeable, mfStorable, mfImportable];
          StorageVol := 20000;
          Register( 'Fluids' );
        end;
      with TMetaFluid.Create(
        tidFluid_Paper,     // id of the fluid
        'Paper',            // name
        'Paper in general', // desc
        'kg',               // unit nme
        'kg/day',           // fluid name/time
        24,                 // number of hours the above means
        0.0005,             // transp cost
        700,                // ??
        15 ) do             // unit cost
        begin
          Options := Options + [mfTradeable, mfImportable, mfStorable];
          StorageVol := 250000;
          Register( 'Fluids' );
        end;
      with TMetaFluid.Create(
        tidFluid_CDs,          // id of the fluid
        'Compact Discs',       // name
        'Music Compact Discs', // desc
        'CD',                  // unit nme
        'CD/day',              // fluid name/time
        24,                    // number of hours the above means
        0.0001,                // transp cost
        700,                   // ??
        15 ) do                // unit cost
        begin
          Options := Options + [mfTradeable, mfStorable, mfImportable];
          StorageVol := 25000;
          Register( 'Fluids' );
        end;
      with TMetaFluid.Create(
        tidFluid_PrintedMaterial,     // id of the fluid
        'Printed Material',           // name
        'Brochures, Flyers, etc.',    // desc
        'kg',                         // unit nme
        'kg/day',                     // fluid name/time
        24,                           // number of hours the above means
        0.0005,                       // transp cost
        700,                          // ??
        25 ) do                       // unit cost
        begin
          Options := Options + [mfTradeable, mfStorable, mfImportable];
          StorageVol := 25000;
          Register( 'Fluids' );
        end;

    end;

end.

