unit CloneOptions;

interface

  const
    // Block [0-7]
    cloneOption_SameTown     = $00000001;
    cloneOption_SameCompany  = $00000002;
    cloneOption_Suppliers    = $00000004;
    cloneOption_Clients      = $00000008;

    // Work Center [8-15]
    cloneOption_Salaries     = $00000100;

    // Eval Blocks [16-23]
    cloneOption_OutputPrices = $00010000;

    // Office & Residentials
    cloneOption_Rent         = $00010000;
    cloneOption_Maint        = $00020000;

    // Stores
    cloneOption_SrvPrices    = $00010000;
    cloneOption_Ads          = $00020000;



implementation

end.
 