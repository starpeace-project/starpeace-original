unit StdTaxes;

interface

  uses
    Taxes, BasicTaxes;

  procedure RegisterTaxes;
  procedure RegisterTaxesToAccounts;

implementation

  uses
    Kernel, ClassStorage, Accounts, BasicAccounts, SysUtils, Collection;

  procedure RegisterTaxesToAccounts;
    var
      count : integer;
      i     : integer;
      MA    : TMetaAccount;
      ToTax : TCollection;
    begin
      try
        count := TheClassStorage.ClassCount[tidClassFamily_Accounts];
      except
        count := 0;
      end;
      ToTax := TCollection.Create( count, rkUse );
      try
        for i := 0 to pred(count) do
          try
            MA := TMetaAccount(TheClassStorage.ClassByIdx[tidClassFamily_Accounts, i]);
            if MA.Taxable
              then ToTax.Insert( MA );
          except
            raise Exception.Create( IntToStr(i) );
          end;
        for i := 0 to pred(ToTax.Count) do
          begin
            MA := TMetaAccount(ToTax[i]);
            with TMetaTaxToAccount.Create( MA.Id, MA.AccId, TTaxToAccount ) do
              begin
                Register( tidClassFamily_Taxes );
              end;
          end;
      finally
        ToTax.Free;
      end;
    end;

  procedure RegisterTaxes;
    begin
      with TMetaAccount.Create(
        accIdx_Taxes,
        accIdx_Special,
        'Taxes',
        '',
        TAccount ) do
        begin
          TaxAccount := true;
          Register( tidClassFamily_Accounts );
        end;
      with TMetaAccount.Create(
        accIdx_LandTax,
        accIdx_Taxes,
        'Land Tax',
        '',
        TAccount ) do
        begin
          TaxAccount := true;
          Register( tidClassFamily_Accounts );
        end;
    end;

end.

