unit TycoonLevels;

interface

  uses
    MetaInstances, Kernel, Languages;

  const
    tidTycoonLevel_Apprentice   = 'Apprentice';
    tidTycoonLevel_Entrepreneur = 'Entrepreneur';
    tidTycoonLevel_Tycoon       = 'Tycoon';
    tidTycoonLevel_Master       = 'Master';
    tidTycoonLevel_Paradigm     = 'Paradigm';
    tidTycoonLevel_Legend       = 'Legend';
    tidTycoonLevel_OverLegend   = 'BeyondLegend';

  type
    TApprenticeLevel =
      class( TTycoonLevel )
        public
          function AdvanceTycoon( Tycoon : TTycoon; out Reason : TMultiString ) : boolean; override;
      end;

    TEntrepreneurLevel =
      class( TTycoonLevel )
        public
          function AdvanceTycoon( Tycoon : TTycoon; out Reason : TMultiString ) : boolean; override;
      end;

    TMagnateLevel =
      class( TTycoonLevel )
        public
          function AdvanceTycoon( Tycoon : TTycoon; out Reason : TMultiString ) : boolean; override;
      end;

    TMasterLevel =
      class( TTycoonLevel )
        public
          function AdvanceTycoon( Tycoon : TTycoon; out Reason : TMultiString ) : boolean; override;
      end;

    TParadigmLevel =
      class( TTycoonLevel )
        public
          function AdvanceTycoon( Tycoon : TTycoon; out Reason : TMultiString ) : boolean; override;
      end;

    TLegendLevel =
      class( TTycoonLevel )
        public
          function AdvanceTycoon( Tycoon : TTycoon; out Reason : TMultiString ) : boolean; override;
      end;

    TOverLegendLevel =
      class( TTycoonLevel )        
        public
          function AdvanceTycoon( Tycoon : TTycoon; out Reason : TMultiString ) : boolean; override;
          function LoseLevel( Tycoon : TTycoon; year: word; out Reason : TMultiString ) : boolean;
      end;

  type
    TOverLegendParam =
      record
        OverLegendFee : currency;
        OverLegendProfit : currency;
        OverLegendPrestige : integer;
        OverLegendFacLimit : integer;
        OverLegendProfitPerFac : currency;
      end;

  const
    OneMillion      = 1000*1000.0;
    OneBillion      = 1000*OneMillion;

    EntrepreneurFee = 100*OneMillion;
    TycoonFee       = 500*OneMillion;
    MasterFee       = 2*OneBillion;
    ParadigmFee     = 20*OneBillion;
    LegendFee       = 40*OneBillion;

    EntrepreneurProfit = 1000.0;
    TycoonProfit       = 5000.0;
    MasterProfit       = 50000.0;
    ParadigmProfit     = 100000.0;
    LegendProfit       = 500000.0;

    MasterPrestige   = 2500;
    ParadigmPrestige = 5000;
    LegendPrestige   = 15000;

  procedure RegisterLevels;
  function CalcOverLegenParam(Level : integer) : TOverLegendParam;
  function DecToRom(Dec: LongInt): String;

  function CreateNextLevel(Level : integer) : string;

implementation

  uses
    Accounts, BasicAccounts, BasicTaxes, SysUtils, MathUtils, SimHints, TownPolitics,
    WorldPolitics, ClassStorage;

  function xGetProfits( Tycoon : TTycoon ) : TMoney;
    var
      hours : single;
    begin
      hours := Tycoon.WorldLocator.GetHoursADay;
      if TheGlobalConfigHandler.GetConfigParm('LifeAfterLegend', '0') = '1'
      then
        begin
          if (Copy(Tycoon.Level.Id, 0, length(tidTycoonLevel_OverLegend)) <> tidTycoonLevel_OverLegend) and (Tycoon.Accounts.AccountArray[accIdx_ResearchCenter_Research] <> nil)
          then
            result :=
              (Tycoon.Accounts.MasterAccount.Value
               - Tycoon.Accounts.AccountArray[accIdx_Construction].Value
               - Tycoon.Accounts.AccountArray[accIdx_Taxes].Value
               - Tycoon.Accounts.AccountArray[accIdx_TransfersIn].Value
               - Tycoon.Accounts.AccountArray[accIdx_TransfersOut].Value
               - Tycoon.Accounts.AccountArray[accIdx_Bank].Value)/(365*hours)
          else
            result :=
              (Tycoon.Accounts.MasterAccount.Value
               - Tycoon.Accounts.AccountArray[accIdx_Construction].Value
               - Tycoon.Accounts.AccountArray[accIdx_Taxes].Value
               - Tycoon.Accounts.AccountArray[accIdx_TransfersIn].Value
               - Tycoon.Accounts.AccountArray[accIdx_TransfersOut].Value
               - Tycoon.Accounts.AccountArray[accIdx_Bank].Value
               - Tycoon.Accounts.AccountArray[accIdx_ResearchCenter_Research].Value)/(365*hours);
        end
      else
        result :=
          (Tycoon.Accounts.MasterAccount.Value
           - Tycoon.Accounts.AccountArray[accIdx_Construction].Value
           - Tycoon.Accounts.AccountArray[accIdx_Taxes].Value
           - Tycoon.Accounts.AccountArray[accIdx_TransfersIn].Value
           - Tycoon.Accounts.AccountArray[accIdx_TransfersOut].Value
           - Tycoon.Accounts.AccountArray[accIdx_Bank].Value)/(365*hours);
    end;

  function TApprenticeLevel.AdvanceTycoon( Tycoon : TTycoon; out Reason : TMultiString ) : boolean;
    begin
      Reason := nil;
      result := false;
    end;

  function TEntrepreneurLevel.AdvanceTycoon( Tycoon : TTycoon; out Reason : TMultiString ) : boolean;
    begin
      if Tycoon.Budget - Tycoon.LoanAmount > EntrepreneurFee
        then
          if Tycoon.YearProfitPerHour > EntrepreneurProfit
            then
              begin
                Tycoon.GenMoney( -EntrepreneurFee, accIdx_TransfersOut );
                result := true;
                Reason := nil;
              end
            else
              begin
                result := false;
                Reason := InstantiateMultiString( mtidNotEnoughProfit, [EntrepreneurProfit, Tycoon.YearProfitPerHour] );
              end
        else
          begin
            result := false;
            Reason := InstantiateMultiString( mtidNotEnoughMoney, [EntrepreneurFee] );
          end;
    end;

  function TMagnateLevel.AdvanceTycoon( Tycoon : TTycoon; out Reason : TMultiString ) : boolean;
    begin
      if Tycoon.Budget - Tycoon.LoanAmount > TycoonFee
        then
          if Tycoon.YearProfitPerHour > EntrepreneurProfit
            then
              begin
                Tycoon.GenMoney( -TycoonFee, accIdx_TransfersOut );
                result := true;
                Reason := nil;
              end
            else
              begin
                result := false;
                Reason := InstantiateMultiString( mtidNotEnoughProfit, [TycoonProfit, Tycoon.YearProfitPerHour] );
              end
        else
          begin
            result := false;
            Reason := InstantiateMultiString( mtidNotEnoughMoney, [TycoonFee] );
          end;
    end;

  function TMasterLevel.AdvanceTycoon( Tycoon : TTycoon; out Reason : TMultiString ) : boolean;
    begin
      if Tycoon.Budget - Tycoon.LoanAmount > MasterFee
        then
          if Tycoon.YearProfitPerHour > MasterProfit
            then
              if Tycoon.Prestige >= MasterPrestige
                then
                  if true //Tycoon.CountItemsInCurriculum( currId_Mayor ) >= 1
                    then
                      begin
                        Tycoon.GenMoney( -MasterFee, accIdx_TransfersOut );
                        result := true;
                        Reason := nil;
                      end
                    else
                      begin
                        result := false;
                        Reason := InstantiateMultiString( mtidMustHaveBeenMayor, [0] );
                      end
                else
                  begin
                    result := false;
                    Reason := InstantiateMultiString( mtidPrestigeRequired, [MasterPrestige] );
                  end
            else
              begin
                result := false;
                Reason := InstantiateMultiString( mtidNotEnoughProfit, [MasterProfit, Tycoon.YearProfitPerHour] );
              end
        else
          begin
            result := false;                               
            Reason := InstantiateMultiString( mtidNotEnoughMoney, [MasterFee] );
          end;
    end;

  function TParadigmLevel.AdvanceTycoon( Tycoon : TTycoon; out Reason : TMultiString ) : boolean;
    begin
      if Tycoon.Budget - Tycoon.LoanAmount > ParadigmFee
        then
          if Tycoon.YearProfitPerHour > ParadigmProfit
            then
              if Tycoon.Prestige >= ParadigmPrestige
                then
                  begin
                    Tycoon.GenMoney( -ParadigmFee, accIdx_TransfersOut );
                    result := true;
                    Reason := nil;
                  end
                else
                  begin
                    result := false;
                    Reason := InstantiateMultiString( mtidPrestigeRequired, [ParadigmPrestige] );
                  end
            else
              begin
                result := false;
                Reason := InstantiateMultiString( mtidNotEnoughProfit, [ParadigmProfit, Tycoon.YearProfitPerHour] );
              end
        else
          begin
            result := false;
            Reason := InstantiateMultiString( mtidNotEnoughMoney, [ParadigmFee] );
          end;
    end;

  function TLegendLevel.AdvanceTycoon( Tycoon : TTycoon; out Reason : TMultiString ) : boolean;
    begin
      if Tycoon.Budget - Tycoon.LoanAmount > LegendFee
        then
          if Tycoon.YearProfitPerHour > LegendProfit
            then                                                 
              if Tycoon.Prestige >= LegendPrestige                        
                then
                  if true //Tycoon.CountItemsInCurriculum( currId_President ) >= 1
                    then
                      begin
                        Tycoon.GenMoney( -LegendFee, accIdx_TransfersOut );
                        result := true;
                        Reason := nil;
                      end
                    else
                      begin
                        result := false;
                        Reason := InstantiateMultiString( mtidMustHaveBeenPresident, [0] );
                      end
                else
                  begin
                    result := false;
                    Reason := InstantiateMultiString( mtidPrestigeRequired, [LegendPrestige] );
                  end
            else
              begin
                result := false;
                Reason := InstantiateMultiString( mtidNotEnoughProfit, [LegendProfit, Tycoon.YearProfitPerHour] );
              end
        else
          begin
            result := false;
            Reason := InstantiateMultiString( mtidNotEnoughMoney, [LegendFee] );
          end;
    end;

  function TOverLegendLevel.AdvanceTycoon( Tycoon : TTycoon; out Reason : TMultiString ) : boolean;
    var
      OLParam : TOverLegendParam;
      Profit : TMoney;
      Prestige : single;
    begin
      OLParam := CalcOverLegenParam(Tier);
      if Tycoon.Budget - Tycoon.LoanAmount > OLParam.OverLegendFee
        then
          begin
            Profit := Tycoon.YearProfitPerHour;
            Prestige := Tycoon.Prestige;
            if Profit > OLParam.OverLegendProfit
              then
                if Prestige >= LegendPrestige                        
                  then
                    if true //Tycoon.CountItemsInCurriculum( currId_President ) >= 1
                      then
                        begin
                          Tycoon.GenMoney( -OLParam.OverLegendFee, accIdx_TransfersOut );
                          result := true;
                          
                          Reason := nil;
                        end
                      else
                        begin
                          result := false;
                          Reason := InstantiateMultiString( mtidMustHaveBeenPresident, [0] );
                        end
                  else
                    begin
                      result := false;
                      Reason := InstantiateMultiString( mtidPrestigeRequired, [LegendPrestige] );
                    end
              else
                begin
                  result := false;
                  Reason := InstantiateMultiString( mtidNotEnoughProfit, [OLParam.OverLegendProfit, Tycoon.YearProfitPerHour] );
                end;
          end  
        else
          begin
            result := false;
            Reason := InstantiateMultiString( mtidNotEnoughMoney, [OLParam.OverLegendFee] );
          end;
    end;

    function TOverLegendLevel.LoseLevel( Tycoon : TTycoon; year: word; out Reason : TMultiString ) : boolean;
      var
        PrecLevel : string;
        OLParam : TOverLegendParam;
      begin
        PrecLevel := Tycoon.Level.Name;
        OLParam := CalcOverLegenParam(Tier);
        if Tycoon.YearProfitPerHour > OLParam.OverLegendProfit
          then                                                 
            if Tycoon.Prestige >= OLParam.OverLegendPrestige                        
              then
                begin
                  result := False;
                  Reason := nil;
                end
              else
                begin
                  Tycoon.Level := TTycoonLevel(TheClassStorage.ClassById[tidClassFamily_TycoonLevels, Tycoon.Level.PrevLevelID]);
                  result := true;
                  Reason := InstantiateMultiString( mtidNotEnoughPrestigeLostLevel, [PrecLevel, year, OLParam.OverLegendPrestige] );
                end
          else
            begin
              Tycoon.Level := TTycoonLevel(TheClassStorage.ClassById[tidClassFamily_TycoonLevels, Tycoon.Level.PrevLevelID]);
              result := true;
              Reason := InstantiateMultiString( mtidNotEnoughProfitLostLevel, [PrecLevel, year, OLParam.OverLegendProfit, Tycoon.YearProfitPerHour] );
            end
      end;

  // Generate Over Legend Parameters

  function CalcOverLegenParam(Level : integer) : TOverLegendParam;
    var
      j, x : integer;
      tot : int64;
    begin
      Result.OverLegendFee := (20*(Level-4)+20)*OneBillion;
      Result.OverLegendProfit := 500000+((Level-4)*200000);
      x := Level - 5;
      if TheGlobalConfigHandler.GetConfigParm('Legend.facLimit', '0') <> '501'
        then
          begin
            tot := 0;
            for j := 1 to x do
              tot := tot + (((Trunc((j)/2))*400));
            Result.OverLegendProfitPerFac := tot + 700;
          end
        else
          begin
            tot := 0;
            for j := 1 to x do
              tot := tot + (((Trunc((j)/2))*400));
            Result.OverLegendProfitPerFac := 2*(tot + 700);
          end;
      tot := 0;
      for j := 1 to x do
        tot := tot + (((Trunc((j)/2))*400));
      Result.OverLegendPrestige := (tot + 1200);
      if TheGlobalConfigHandler.GetConfigParm('Legend.facLimit', '0') <> '501'
        then
          begin
            tot := 0;
            for j := 7 to (Level + 1) do
              tot := tot + (Round(int(j/(4.5))) + 2) * 50;
            Result.OverLegendFacLimit := tot + 1200;
          end
        else
          begin
            tot := 0;
            for j := 7 to (Level +1) do
              tot := tot + (Round(int(j/4.5)/2) + 2) * 50;
            Result.OverLegendFacLimit := tot + 650;
          end;
      Result.OverLegendProfit := Round((Result.OverLegendFacLimit * Result.OverLegendProfitPerFac));
    end;

  function CreateNextLevel(Level : integer): string;
    var
      Limits : TOverLegendParam;
    begin
      with TOverLegendLevel.Create(tidTycoonLevel_OverLegend + IntToStr(Level-5), tidTycoonLevel_OverLegend + IntToStr(Level -4), Level) do
        begin
          Name := 'Beyond Legend ' + DecToRom(Level - 5);
          Limits := CalcOverLegenParam(Level);
          PrestigeBoost := Limits.OverLegendPrestige;
          FacLimit := Limits.OverLegendFacLimit;
          PercOfResearchSubs := 0;
          MoneyBackOnDemolish := 0;
          Fee := Limits.OverLegendFee;
          HourIncome := Limits.OverLegendProfit;
          Description :=
            IntToStr(Limits.OverLegendPrestige) + ' prestige points. ' +
            'Cannot buy from Trade Centers. ' +
            'No priorites, no protection from the IFEL. ' +
            'Can build up to %s facilities. ';
          Condition :=
            'You have to pay ' + FormatMoney(Limits.OverLegendFee) + ' to the IFEL. ' +
            'You must have an average profit ratio superior to ' + FormatMoney(Limits.OverLegendProfit) + '/h. ' +
            'Prestige greater than ' + IntToStr(Limits.OverLegendPrestige) + '. ' +
            ' ';
          Register( tidClassFamily_TycoonLevels );
          RetrieveDynamicTexts;
          Result := tidTycoonLevel_OverLegend + IntToStr(Level-5);
        end;
    end;

  function DecToRom(Dec: LongInt): String;
    const
      Nums : Array[1..13] of Integer =
        (1, 4, 5, 9, 10, 40, 50, 90, 100,
          400, 500, 900, 1000);
      RomanNums:  Array[1..13] of string =
        ('I', 'IV', 'V', 'IX', 'X', 'XL',
          'L', 'XC', 'C', 'CD', 'D', 'CM', 'M');
    var
      i: Integer;
    begin
      Result := '';
      for i := 13 downto 1 do
        while (Dec >= Nums[i]) do
        begin
          Dec := Dec - Nums[i];
          Result  := Result + RomanNums[i];
        end;
    end;
  
  // RegisterLevels

  procedure RegisterLevels;
    var
      Limits : TOverLegendParam;
    begin
      if TheGlobalConfigHandler.GetConfigParm('LifeAfterLegend', '0') = '1'
      then
        begin
          with TOverLegendLevel.Create(tidTycoonLevel_OverLegend + '1', '', 6) do
            begin
              Limits := CalcOverLegenParam(6);
              Name := 'Beyond Legend ' + DecToRom(1);
              PrestigeBoost := Limits.OverLegendPrestige;
              FacLimit := Limits.OverLegendFacLimit;
              PercOfResearchSubs := 0;
              MoneyBackOnDemolish := 0;
              Fee := Limits.OverLegendFee;
              HourIncome := Limits.OverLegendProfit;
              PrevLevelID := tidTycoonLevel_Legend;
              Description :=
                IntToStr(Limits.OverLegendPrestige) + ' prestige points. ' +
                'Cannot buy from Trade Centers. ' +
                'No priorites, no protection from the IFEL. ' +
                'Can build up to %s facilities. ';
              Condition :=
                'You have to pay ' + FormatMoney(Limits.OverLegendFee) + ' to the IFEL. ' +
                'You must have an average profit ratio superior to ' + FormatMoney(Limits.OverLegendProfit) + '/h. ' +
                'Prestige greater than ' + IntToStr(Limits.OverLegendPrestige) + '. ' +
                ' ';
              Register( tidClassFamily_TycoonLevels );
          end;
          with TLegendLevel.Create( tidTycoonLevel_Legend, tidTycoonLevel_OverLegend + '1', 5 ) do
            begin
              Name := 'Legend';
              PrestigeBoost := 800;
              //FacLimit := 2500;
              //FacLimit := high(FacLimit);
              FacLimit := StrToInt(TheGlobalConfigHandler.GetConfigParm('Legend.facLimit', '10000'));
              PercOfResearchSubs := 0;
              MoneyBackOnDemolish := 0;
              Description :=
                '800 prestige points. ' +
                'Have access to Tier 5 technologies. ' +
                'Cannot buy from Trade Centers. ' +
                'No priorites, no protection from the IFEL. ' +
                'Can build up to %s facilities. ';
              Condition :=
                'You have to pay ' + FormatMoney(LegendFee) + ' to the IFEL. ' +
                'You must have an average profit ratio superior to ' + FormatMoney(LegendProfit) + '/h. ' +
                'Prestige greater than ' + IntToStr(LegendPrestige) + '. ' +
                ' ';
              Register( tidClassFamily_TycoonLevels );
            end;
          end
        else
          begin
            with TLegendLevel.Create( tidTycoonLevel_Legend, '', 5 ) do
              begin
                Name := 'Legend';
                PrestigeBoost := 800;
                //FacLimit := 2500;
                //FacLimit := high(FacLimit);
                FacLimit := StrToInt(TheGlobalConfigHandler.GetConfigParm('Legend.facLimit', '10000'));
                PercOfResearchSubs := 0;
                MoneyBackOnDemolish := 0;
                Description :=
                  '800 prestige points. ' +
                  'Have access to Tier 5 technologies. ' +
                  'Cannot buy from Trade Centers. ' +
                  'No priorites, no protection from the IFEL. ' +
                  'Can build up to %s facilities. ';
                Condition :=
                  'You have to pay ' + FormatMoney(LegendFee) + ' to the IFEL. ' +
                  'You must have an average profit ratio superior to ' + FormatMoney(LegendProfit) + '/h. ' +
                  'Prestige greater than ' + IntToStr(LegendPrestige) + '. ' +
                  ' ';
                Register( tidClassFamily_TycoonLevels );
              end
          end;
      with TParadigmLevel.Create( tidTycoonLevel_Paradigm, tidTycoonLevel_Legend, 4 ) do
        begin
          Name := 'Paradigm';
          PrestigeBoost := 400;
          //FacLimit := 1000;
          FacLimit := StrToInt(TheGlobalConfigHandler.GetConfigParm('Paradigm.facLimit', '1000'));
          PercOfResearchSubs := 0;
          MoneyBackOnDemolish := 0;
          Description :=
            '400 prestige points. ' +
            'Have access to Tier 4 technologies. ' +
            'Cannot buy from Trade Centers. ' +
            'No priorites, no protection from the IFEL. ' +
            'Can build up to %s facilities. ';
          Condition :=
            'You have to pay ' + FormatMoney(ParadigmFee) + ' to the IFEL. ' +
            'Average profit ratio superior to ' + FormatMoney(ParadigmProfit) + '/h. ' +
            'Prestige greater than ' + IntToStr(ParadigmPrestige) + '. ';
          Register( tidClassFamily_TycoonLevels );
        end;
      with TMasterLevel.Create( tidTycoonLevel_Master, tidTycoonLevel_Paradigm, 3 ) do
        begin
          Name := 'Master';
          PrestigeBoost := 200;
          //FacLimit := 800;
          FacLimit := StrToInt(TheGlobalConfigHandler.GetConfigParm('Master.facLimit', '800'));
          PercOfResearchSubs := 0;
          MoneyBackOnDemolish := 0;
          Description :=
            '200 prestige points. ' +
            'Have access to Tier 3 technologies. ' +
            'Cannot buy from Trade Centers. ' +
            'No priorites, no protection from the IFEL. ' +
            'Can build up to %s facilities. ';
          Condition :=
            'You have to pay ' + FormatMoney(MasterFee) + ' to the IFEL. ' +
            'Average profit ratio superior to ' + FormatMoney(MasterProfit) + '/h ' +
            'Prestige greater than ' + IntToStr(MasterPrestige) + '. ' +
            ' ';
          Register( tidClassFamily_TycoonLevels );
        end;
      with TMagnateLevel.Create( tidTycoonLevel_Tycoon, tidTycoonLevel_Master, 2 ) do
        begin
          Name := 'Tycoon';
          PrestigeBoost := 100;
          //FacLimit := 400;
          FacLimit := StrToInt(TheGlobalConfigHandler.GetConfigParm('Tycoon.facLimit', '400'));
          PercOfResearchSubs := 0;
          MoneyBackOnDemolish := 0;
          Description :=
            '100 prestige points. ' +
            'Have access to Tier 2 technologies. ' +
            'Cannot buy from Trade Centers. ' +
            'No priorites, no protection from the IFEL. ' +
            'Can build up to %s facilities. ';
          Condition :=
            'You have to pay ' + FormatMoney(TycoonFee) + ' to the IFEL. ' +
            'You must have an average profit ratio superior to ' + FormatMoney(TycoonProfit) + '/h';
          Register( tidClassFamily_TycoonLevels );
        end;
      with TEntrepreneurLevel.Create( tidTycoonLevel_Entrepreneur, tidTycoonLevel_Tycoon, 1 ) do
        begin
          Name := 'Entrepreneur';
          PrestigeBoost := 50;
          //FacLimit := 150;
          FacLimit := StrToInt(TheGlobalConfigHandler.GetConfigParm('Entrepreneur.facLimit', '150'));
          PercOfResearchSubs := 0.3;
          Priority := cprNewbieB;
          MoneyBackOnDemolish := 2/3;
          Description :=
            'Still is protected by the IFEL, has certain priorities in supplier ' +
            'and client lists. 2/3 of the cost of a building is returned if the building is ' +
            'demolished. Can build up to %s facilities. ' +
            'Can buy from Trade Centers. ' +
            IntToStr(round(100*PercOfResearchSubs)) + '% of research expenditures is returned at the end of the year. ';
          Condition :=
            'You have to pay the original debt to the IFEL ($100,000,000). ' +
            'You must have an average profit ratio superior to ' + FormatMoney(EntrepreneurProfit) + '/h';
          Register( tidClassFamily_TycoonLevels );
        end;
      with TApprenticeLevel.Create( tidTycoonLevel_Apprentice, tidTycoonLevel_Entrepreneur, 0 ) do
        begin
          Name := 'Apprentice';
          PrestigeBoost := 25;
          //FacLimit := 50;
          FacLimit := StrToInt(TheGlobalConfigHandler.GetConfigParm('Apprentice.facLimit', '50'));
          PercOfResearchSubs := 0.5;
          Priority := cprNewbieA;
          MoneyBackOnDemolish := 1;
          Description :=
            'Still learning the economy. Protected by the IFEL, has priority in supplier ' +
            'and client lists. The cost of a building is returned if the building is ' +
            'demolished. Can build up to %s facilities. ' +
            'Can buy from Trade Centers. ' +
            IntToStr(round(100*PercOfResearchSubs)) + '% of research expenditures is returned at the end of the year. ';
          Register( tidClassFamily_TycoonLevels );
        end;
    end;

end.



