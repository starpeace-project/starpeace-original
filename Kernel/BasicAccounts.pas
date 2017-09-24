unit BasicAccounts;

interface

  const
    accIdx_None             = -1;
    accIdx_Master           = 0;
    accIdx_Roads            = 1;
    accIdx_Commerce         = 3;
    accIdx_Industries       = 4;
    accIdx_Public           = 5;
    accIdx_Firms            = 7;
    accIdx_RoadConstruction = 8;
    accIdx_RoadDemolition   = 9;
    accIdx_RoadMaintenance  = 90;
    accIdx_Construction     = 11;
    accIdx_Loans            = 12;
    accIdx_TransfersIn      = 13;
    accIdx_TransfersOut     = 14;

    // Residentials
    accIdx_Residentials             = 2;
    accIdx_Residentials_Rents       = 15;
    accIdx_Residentials_Maintenance = 16;
    accIdx_Residentials_Repairs     = 17;

    // Offices sub-accounts
    accIdx_Offices             = 6;
    accIdx_Offices_Rents       = 18;
    accIdx_Offices_Maintenance = 19;
    accIdx_Offices_Repairs     = 20;

    // Public Facilities
    accIdx_PF_Salaries        = 22;
    accIdx_PublicBudget       = 23;
    accIdx_PF_PublicOperation = 24;

    // Research Centers
    accIdx_ResearchCenter           = 30;
    accIdx_ResearchCenter_Research  = 31;
    accIdx_ResearchCenter_Salaries  = 32;
    accIdx_ResearchCenter_Equipment = 33;
    accIdx_ResearchCenter_Supplies  = 34;
    accIdx_ResearchCenter_ImpCosts  = 35;

    // General
    accIdx_RentalProperties = 40;
    accIdx_Special          = 41;
    accIdx_Compensations    = 42;

    // Banks
    accIdx_Bank              = 45;
    accIdx_Bank_LoanIncome   = 46;
    accIdx_Bank_IssuedLoans  = 47;
    accIdx_Bank_Loans        = 48;
    accIdx_Bank_LoanPayments = 49;
    accIdx_Bank_Salaries     = 50;

  procedure RegisterAccounts;

implementation

  uses
    Accounts;

  procedure RegisterAccounts;
    begin
      with TMetaAccount.Create(
        accIdx_Master,
        accIdx_None,
        'Net Profit (losses)',
        '',
        TAccount ) do
        begin
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Construction,
        accIdx_Master,
        'Construction',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Roads,
        accIdx_Construction,
        'Roads',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_RentalProperties,
        accIdx_Master,
        'Rental properties',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Residentials,
        accIdx_RentalProperties,
        'Residentials',
        '',
        TAccount ) do
        begin
          Taxable := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Commerce,
        accIdx_Master,
        'Commerce',
        '',
        TAccount ) do
        begin
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Industries,
        accIdx_Master,
        'Industries',
        '',
        TAccount ) do
        begin
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Offices,
        accIdx_RentalProperties,
        'Offices',
        '',
        TAccount ) do
        begin
          Taxable   := true;
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Firms,
        accIdx_Master,
        'Firms',
        '',
        TAccount ) do
        begin
          Rankeable := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_RoadConstruction,
        accIdx_Roads,
        'Road Construction',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_RoadDemolition,
        accIdx_Roads,
        'Road Demolition',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_RoadMaintenance,
        accIdx_Roads,
        'Road Maintenance',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Loans,
        accIdx_Master,
        'Loans',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_TransfersIn,
        accIdx_Master,
        'Money Recieved',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_TransfersOut,
        accIdx_Master,
        'Money Sent',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Special,
        accIdx_Master,
        'Special',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_PublicBudget,
        accIdx_Special,
        'State Budget',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Public,
        accIdx_Special,
        'Public Facilities',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Compensations,
        accIdx_Special,
        'Compensations',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // Residential sub-accounts
      with TMetaAccount.Create(
        accIdx_Residentials_Rents,
        accIdx_Residentials,
        'Rents',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Residentials_Maintenance,
        accIdx_Residentials,
        'Maintenance',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Residentials_Repairs,
        accIdx_Residentials,
        'Repairs',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // Offices sub-accounts
      with TMetaAccount.Create(
        accIdx_Offices_Rents,
        accIdx_Offices,
        'Rents',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Offices_Maintenance,
        accIdx_Offices,
        'Maintenance',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Offices_Repairs,
        accIdx_Offices,
        'Repairs',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // Public Facilities
      with TMetaAccount.Create(
        accIdx_PF_Salaries,
        accIdx_Public,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // Public Facilities Operating costs
      with TMetaAccount.Create(
        accIdx_PF_PublicOperation,
        accIdx_Public,
        'Operating costs',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

      // Research Centers
      with TMetaAccount.Create(
        accIdx_ResearchCenter,
        accIdx_Special,
        'Headquarters',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_ResearchCenter_Research,
        accIdx_ResearchCenter,
        'Researchs',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_ResearchCenter_Salaries,
        accIdx_ResearchCenter,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_ResearchCenter_Equipment,
        accIdx_ResearchCenter,
        'Equipment',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_ResearchCenter_Supplies,
        accIdx_ResearchCenter,
        'Services',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_ResearchCenter_ImpCosts,
        accIdx_ResearchCenter,
        'Implementation Costs',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Bank,
        accIdx_Special,
        'Bank',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Bank_LoanIncome,
        accIdx_Bank,
        'Debtors payments',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Bank_IssuedLoans,
        accIdx_Bank,
        'Issued loans',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Bank_Loans,
        accIdx_Bank,
        'Loan',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Bank_LoanPayments,
        accIdx_Bank,
        'Loan payments',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_Bank_Salaries,
        accIdx_Bank,
        'Salaries',
        '',
        TAccount ) do
        begin
          Register( tidClassFamily_Accounts );
        end;

    end;

    
end.
