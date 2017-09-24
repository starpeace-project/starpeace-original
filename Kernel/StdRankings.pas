unit StdRankings;

interface

  procedure RegisterRankings;

implementation

  uses
    SysUtils, ClassStorage, Accounts, Rankings, BasicRankings, Languages;

  const
    mtidNTARanking        : TRegMultiString = nil;
    mtidPrestigeRanking   : TRegMultiString = nil;
    mtidWealthRanking     : TRegMultiString = nil;
    mtidTournamentRanking : TRegMultiString = nil;

  procedure RegisterRankings;

    function GetRankeableSuperAccount( MetaAccount : TMetaAccount ) : TMetaAccount;
      begin
        if MetaAccount.MasterAccount = nil
          then result := nil
          else
            if MetaAccount.MasterAccount.Rankeable
              then result := MetaAccount.MasterAccount
              else result := GetRankeableSuperAccount( MetaAccount.MasterAccount );
      end;

    var
      i, count     : integer;
      SuperAccount : TMetaAccount;
      MetaAccount  : TMetaAccount;
      AccRanking   : TAccountRanking;
    begin
      // Register Overall ranking
      TOverallRanking.Create( 'NTA', '', InstantiateMultiString( mtidNTARanking, [0] ), 100 ).Register( tidClassFamily_Rankings );

      // Register Prestige ranking
      TPrestigeRanking.Create( 'Prestige', 'NTA', InstantiateMultiString( mtidPrestigeRanking, [0] ), 100 ).Register( tidClassFamily_Rankings );

      // Register Wealth ranking
      TWealthRanking.Create( 'Wealth', 'NTA', InstantiateMultiString( mtidWealthRanking, [0] ), 100 ).Register( tidClassFamily_Rankings );

      // Register Account-based rankings
      count := TheClassStorage.ClassCount[tidClassFamily_Accounts];
      for i := 0 to pred(count) do
        begin
          MetaAccount := TMetaAccount(TheClassStorage.ClassByIdx[tidClassFamily_Accounts, i]);
          if MetaAccount.Rankeable
            then
              begin
                SuperAccount := GetRankeableSuperAccount( MetaAccount );
                if SuperAccount <> nil
                  then AccRanking := TAccountRanking.Create( MetaAccount.Id, SuperAccount.Id, CloneMultiString( MetaAccount.Name_MLS ), 100, MetaAccount.AccId )
                  else AccRanking := TAccountRanking.Create( MetaAccount.Id, 'NTA', CloneMultiString( MetaAccount.Name_MLS ), 100, MetaAccount.AccId );
                AccRanking.Register( tidClassFamily_Rankings );
              end;
        end;
     if StrToInt(TheGlobalConfigHandler.GetConfigParm('TornamentLength', '0')) > 0
       then TContestRanking.Create('aTournament', '', InstantiateMultiString(mtidTournamentRanking, [0] ), 100 ).Register(tidClassFamily_Rankings);
    end;

initialization

  mtidNTARanking        := TRegMultiString.Create( 'mtidNTARanking', 'NTA' );
  mtidPrestigeRanking   := TRegMultiString.Create( 'mtidPrestigeRanking', 'Prestige' );
  mtidWealthRanking     := TRegMultiString.Create( 'mtidWealthRanking', 'Wealth' );
  mtidTournamentRanking := TRegMultiString.Create( 'mtidTournamentRanking', 'Tournament' );

end.
