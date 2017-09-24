unit BasicRankings;

interface

  uses
    Classes, Rankings, Accounts, CacheAgent, Languages;

  type
    TAccountRanking =
      class( TRanking )
        public
          constructor Create( anId, aSuperRanking : string; aName : TMultiString; Max : integer; AccountId : TAccountId );
        private
          fAccountId : TAccountId;
        public
          function  CompareObjects( const Obj1, Obj2 : TObject; const Context ) : integer; override;
          procedure CacheItem( index : integer; const Obj : TObject; Cache : TObjectCache ); override;
          function  IsRankeable( const Obj : TObject ) : boolean; override;
          procedure Serialize( Prefix : string; List : TStringList ); override;
          procedure SerializeItem( Prefix : string; index : integer; const Obj : TObject; List : TStringList ); override;
      end;

    TPrestigeRanking =
      class( TRanking )
        public
          function  CompareObjects( const Obj1, Obj2 : TObject; const Context ) : integer; override;
          procedure CacheItem( index : integer; const Obj : TObject; Cache : TObjectCache ); override;
          function  IsRankeable( const Obj : TObject ) : boolean; override;
          procedure Serialize( Prefix : string; List : TStringList ); override;
          procedure SerializeItem( Prefix : string; index : integer; const Obj : TObject; List : TStringList ); override;
      end;

    TWealthRanking =
      class( TRanking )
        public
          function  CompareObjects( const Obj1, Obj2 : TObject; const Context ) : integer; override;
          procedure CacheItem( index : integer; const Obj : TObject; Cache : TObjectCache ); override;
          function  IsRankeable( const Obj : TObject ) : boolean; override;
          procedure Serialize( Prefix : string; List : TStringList ); override;
          procedure SerializeItem( Prefix : string; index : integer; const Obj : TObject; List : TStringList ); override;
      end;

    TOverallRanking =
      class( TRanking )
        public
          function  CompareObjects( const Obj1, Obj2 : TObject; const Context ) : integer; override;
          procedure CacheItem( index : integer; const Obj : TObject; Cache : TObjectCache ); override;
          function  IsRankeable( const Obj : TObject ) : boolean; override;
          procedure Serialize( Prefix : string; List : TStringList ); override;
          procedure SerializeItem( Prefix : string; index : integer; const Obj : TObject; List : TStringList ); override;
          class function IsOverall : boolean; override;
      end;

    TContestRanking =
      class( TRanking )
        private
          function  CalcContestPoints(Obj : TObject) : integer;
        public
          function  CompareObjects( const Obj1, Obj2 : TObject; const Context ) : integer; override;
          procedure CacheItem( index : integer; const Obj : TObject; Cache : TObjectCache ); override;
          function  IsRankeable( const Obj : TObject ) : boolean; override;
          procedure Serialize( Prefix : string; List : TStringList ); override;
          procedure SerializeItem( Prefix : string; index : integer; const Obj : TObject; List : TStringList ); override;
          class function IsOverall : boolean; override;
          class function Serializable : boolean; override;
      end;

implementation

  uses
    Kernel, MathUtils, SysUtils, RankProtocol;


  // TAccountRanking

  constructor TAccountRanking.Create( anId, aSuperRanking : string; aName : TMultiString; Max : integer; AccountId : TAccountId );
    begin
      inherited Create( anId, aSuperRanking, aName, Max );
      fAccountId := AccountId;
    end;

  function TAccountRanking.CompareObjects( const Obj1, Obj2 : TObject; const Context ) : integer;
    var
      Dealer1 : TMoneyDealer absolute Obj1;
      Dealer2 : TMoneyDealer absolute Obj2;
      acc1    : TMoney;
      acc2    : TMoney;
    begin
      try
        acc1 := Dealer1.Accounts.AccountArray[fAccountId].Value;
        acc2 := Dealer2.Accounts.AccountArray[fAccountId].Value;
        if acc1 > acc2
          then result := -1
          else
            if acc1 < acc2
              then result := 1
              else result := 0;
      except
        result := 0;
      end;
    end;

  procedure TAccountRanking.CacheItem( index : integer; const Obj : TObject; Cache : TObjectCache );
    var
      Tycoon : TTycoon absolute Obj;
    begin
      try
        Cache.WriteString( 'Name' + IntToStr(index), Tycoon.Name );
        Cache.WriteString( 'Value' + IntToStr(index), FormatMoney(Tycoon.Accounts.AccountArray[fAccountId].Value) );
      except
      end;
    end;

  function TAccountRanking.IsRankeable( const Obj : TObject ) : boolean;
    var
      Tycoon : TTycoon absolute Obj;
    begin
      result := Tycoon.Accounts.AccountArray[fAccountId].Value > 0;
    end;

  procedure TAccountRanking.Serialize( Prefix : string; List : TStringList );
    begin
      inherited;
      List.Values[Prefix + tidRankings_RankType] := '1';
    end;

  procedure TAccountRanking.SerializeItem( Prefix : string; index : integer; const Obj : TObject; List : TStringList );
    var
      Tycoon : TTycoon absolute Obj;
    begin
      inherited;
      try
        List.Values[Prefix + tidRankings_MemberName] := Tycoon.Name;
        List.Values[Prefix + tidRankings_MemberValue] := CurrToStr(Tycoon.Accounts.AccountArray[fAccountId].Value);
      except
      end;
    end;
    

  // TPrestigeRanking

  function TPrestigeRanking.CompareObjects( const Obj1, Obj2 : TObject; const Context ) : integer;
    var
      Tycoon1 : TTycoon absolute Obj1;
      Tycoon2 : TTycoon absolute Obj2;
    begin
      if Tycoon1.Prestige > Tycoon2.Prestige
        then result := -1
        else
          if Tycoon1.Prestige < Tycoon2.Prestige
            then result := 1
            else result := 0;
    end;

  procedure TPrestigeRanking.CacheItem( index : integer; const Obj : TObject; Cache : TObjectCache );
    var
      Tycoon : TTycoon absolute Obj;
    begin
      try
        Cache.WriteString( 'Name' + IntToStr(index), Tycoon.Name );
        Cache.WriteString( 'Value' + IntToStr(index), IntToStr(round(Tycoon.Prestige)) );
      except
      end;
    end;

  function TPrestigeRanking.IsRankeable( const Obj : TObject ) : boolean;
    var
      Tycoon : TTycoon absolute Obj;
    begin
      result := Tycoon.Prestige > 0;
    end;

  procedure TPrestigeRanking.Serialize( Prefix : string; List : TStringList );
    begin
      inherited;
      List.Values[Prefix + tidRankings_RankType] := '2';
    end;

  procedure TPrestigeRanking.SerializeItem( Prefix : string; index : integer; const Obj : TObject; List : TStringList );
    var
      Tycoon : TTycoon absolute Obj;
    begin
      inherited;
      try
        List.Values[Prefix + tidRankings_MemberName] := Tycoon.Name;
        List.Values[Prefix + tidRankings_MemberValue] := IntToStr(round(Tycoon.Prestige));
      except
      end;
    end;


  // TWealthRanking

  function TWealthRanking.CompareObjects( const Obj1, Obj2 : TObject; const Context ) : integer;
    var
      Tycoon1 : TTycoon absolute Obj1;
      Tycoon2 : TTycoon absolute Obj2;
    begin
      if Tycoon1.Budget > Tycoon2.Budget
        then result := -1
        else
          if Tycoon1.Budget < Tycoon2.Budget
            then result := 1
            else result := 0;
    end;

  procedure TWealthRanking.CacheItem( index : integer; const Obj : TObject; Cache : TObjectCache );
    var
      Tycoon : TTycoon absolute Obj;
    begin
      try
        Cache.WriteString( 'Name' + IntToStr(index), Tycoon.Name );
        Cache.WriteString( 'Value' + IntToStr(index), FormatMoney(Tycoon.Budget) );
      except
      end;
    end;

  function TWealthRanking.IsRankeable( const Obj : TObject ) : boolean;
    var
      Tycoon : TTycoon absolute Obj;
    begin
      result := Tycoon.Budget > InitialBudget;
    end;

  procedure TWealthRanking.Serialize( Prefix : string; List : TStringList );
    begin
      inherited;
      List.Values[Prefix + tidRankings_RankType] := '3';
    end;

  procedure TWealthRanking.SerializeItem( Prefix : string; index : integer; const Obj : TObject; List : TStringList );
    var
      Tycoon : TTycoon absolute Obj;
    begin
      inherited;
      try
        List.Values[Prefix + tidRankings_MemberName] := Tycoon.Name;
        List.Values[Prefix + tidRankings_MemberValue] := CurrToStr(Tycoon.Budget);
      except
      end;
    end;


  // TOverallRanking

  function TOverallRanking.CompareObjects( const Obj1, Obj2 : TObject; const Context ) : integer;
    var
      Tycoon1 : TTycoon absolute Obj1;
      Tycoon2 : TTycoon absolute Obj2;
    begin
      if Tycoon1.RankingAvg > Tycoon2.RankingAvg
        then result := -1
        else
          if Tycoon1.RankingAvg < Tycoon2.RankingAvg
            then result := 1
            else result := 0;
    end;

  procedure TOverallRanking.CacheItem( index : integer; const Obj : TObject; Cache : TObjectCache );
    var
      Tycoon : TTycoon absolute Obj;
    begin
      try
        Cache.WriteString( 'Name' + IntToStr(index), Tycoon.Name );
        Cache.WriteString( 'Value' + IntToStr(index), IntToStr(round(Tycoon.RankingAvg)) );
      except
      end;
    end;

  function TOverallRanking.IsRankeable( const Obj : TObject ) : boolean;
    var
      Tycoon : TTycoon absolute Obj;
    begin
      result := true;
    end;

  procedure TOverallRanking.Serialize( Prefix : string; List : TStringList );
    begin
      inherited;
      List.Values[Prefix + tidRankings_RankType] := '0';
    end;

  procedure TOverallRanking.SerializeItem( Prefix : string; index : integer; const Obj : TObject; List : TStringList );
    var
      Tycoon : TTycoon absolute Obj;
    begin
      inherited;
      try
        List.Values[Prefix + tidRankings_MemberName] := Tycoon.Name;
        List.Values[Prefix + tidRankings_MemberValue] := IntToStr(round(Tycoon.RankingAvg));
      except
      end;
    end;

  class function TOverallRanking.IsOverall : boolean;
    begin
      result := true;
    end;


  // TContestRanking

  function TContestRanking.CalcContestPoints(Obj : TObject) : integer;
    var
      Tycoon : TTycoon absolute Obj;
      aux    : string;
    begin
      result := 0;
      aux := Tycoon.Cookie['rkPts'];
      if aux <> ''
        then inc(result, StrToInt(aux));
      aux := Tycoon.Cookie['lvPts'];
      if aux <> ''
        then inc(result, StrToInt(aux));
      aux := Tycoon.Cookie['bnkPts'];
      if aux <> ''
        then inc(result, StrToInt(aux));
    end;

  function TContestRanking.CompareObjects( const Obj1, Obj2 : TObject; const Context ) : integer;
    var
      Tycoon1 : TTycoon absolute Obj1;
      Tycoon2 : TTycoon absolute Obj2;
      pts1    : integer;
      pts2    : integer;
    begin
      pts1 := CalcContestPoints(Obj1);
      pts2 := CalcContestPoints(Obj2);
      if pts1 > pts2
        then result := -1
        else
          if pts1 < pts2
            then result := 1
            else result := 0;
    end;

  procedure TContestRanking.CacheItem( index : integer; const Obj : TObject; Cache : TObjectCache );
    var
      Tycoon : TTycoon absolute Obj;
    begin
      try
        Cache.WriteString( 'Name' + IntToStr(index), Tycoon.Name );
        Cache.WriteString( 'Value' + IntToStr(index), IntToStr(CalcContestPoints(Obj)));
      except
      end;
    end;

  function TContestRanking.IsRankeable( const Obj : TObject ) : boolean;
    var
      Tycoon : TTycoon absolute Obj;
    begin
      result := not Tycoon.IsRole and (Tycoon.Companies.Count > 0); //and (CalcContestPoints(Obj) > -1000);
    end;

  procedure TContestRanking.Serialize( Prefix : string; List : TStringList );
    begin
      inherited;
      List.Values[Prefix + tidRankings_RankType] := '10';
    end;

  procedure TContestRanking.SerializeItem( Prefix : string; index : integer; const Obj : TObject; List : TStringList );
    var
      Tycoon : TTycoon absolute Obj;
    begin
      inherited;
      try
        List.Values[Prefix + tidRankings_MemberName] := Tycoon.Name;
        List.Values[Prefix + tidRankings_MemberValue] := IntToStr(CalcContestPoints(Obj));
      except
      end;
    end;

  class function TContestRanking.IsOverall : boolean;
    begin
      result := true;
    end;

  class function TContestRanking.Serializable : boolean;
    begin
      result := false;
    end;

end.

