unit GMList;

interface

  uses
    Collection;

  type
    TGMConnectOptions = (GMCO_HIGHPRIORITY, GMCO_NORMALPRIORITY, GMCO_IGNORE);

    TGameMasterListInfo =
      class
        public
          constructor Create( aName : string; anOptions : TGMConnectOptions );
        private
          fName    : string;
          fOptions : TGMConnectOptions;
        public
          property Name    : string            read fName;
          property Options : TGMConnectOptions read fOptions write fOptions;
      end;

    TGameMasterList =
      class
        public
          constructor Create;
          destructor  Destroy; override;
        public
          function  AddGameMaster( aName : string; anOptions : TGMConnectOptions; out Idx : integer ) : TGameMasterListInfo;
          procedure DeleteGameMaster( aName : string );
          function  MoveGameMasterUp( GameMaster : TGameMasterListInfo ) : boolean;
          function  MoveGameMasterDown( GameMaster : TGameMasterListInfo ) : boolean;
          procedure ChangeGameMasterProperties( GameMaster : TGameMasterListInfo; NewOptions : TGMConnectOptions; out NewIdx : integer );
        public
          procedure LoadFromString( const aStr : string );
          procedure SaveToString( var aStr : string );
        private
          fGameMasters : TSortedCollection;
          function getCount : integer;
          function getItemByIdx( Idx : integer ) : TGameMasterListInfo;
          function getItemByName( Name : string ) : TGameMasterListInfo;
        public
          property Count : integer read getCount;
          property Item[Idx : integer] : TGameMasterListInfo read getItemByIdx;
          property ItemByName[Name : string] : TGameMasterListInfo read getItemByName;
        private
          function Compare( Item1, Item2 : TObject) : integer;
      end;


implementation

  uses
    sysutils, classes;

  // TGameMasterListInfo

  constructor TGameMasterListInfo.Create( aName : string; anOptions : TGMConnectOptions );
    begin
      inherited Create;
      fName    := aName;
      fOptions := anOptions;
    end;

  // TGameMasterList

  constructor TGameMasterList.Create;
    begin
      inherited Create;
      fGameMasters := TSortedCollection.Create( 20, rkBelonguer, Compare );
    end;

  destructor TGameMasterList.Destroy;
    begin
      fGameMasters.Free;
      inherited;
    end;

  function TGameMasterList.AddGameMaster( aName : string; anOptions : TGMConnectOptions; out Idx : integer ) : TGameMasterListInfo;
    begin
      result := getItemByName( aName );
      if result = nil
        then
          begin
            result := TGameMasterListInfo.Create( aName, anOptions );
            Idx    := fGameMasters.InsertPos( result );
            fGameMasters.AtInsert( Idx, result );
          end;
    end;

  procedure TGameMasterList.DeleteGameMaster( aName : string );
    begin
      fGameMasters.Delete( getItemByName( aName ));
    end;

  function TGameMasterList.MoveGameMasterUp( GameMaster : TGameMasterListInfo ) : boolean;
    var
      Idx : integer;
    begin
      result := false;
      Idx    := fGameMasters.IndexOf( GameMaster );
      if (Idx > 0) and (TGameMasterListInfo(fGameMasters[pred(Idx)]).Options = GameMaster.Options)
        then
          begin
            result := true;
            fGameMasters.Exchange( Idx, pred(Idx) );
          end;
    end;

  function TGameMasterList.MoveGameMasterDown( GameMaster : TGameMasterListInfo ) : boolean;
    var
      Idx : integer;
    begin
      result := false;
      Idx    := fGameMasters.IndexOf( GameMaster );
      if (Idx <> -1) and (Idx < pred(fGameMasters.Count) ) and (TGameMasterListInfo(fGameMasters[succ(Idx)]).Options = GameMaster.Options)
        then
          begin
            result := true;
            fGameMasters.Exchange( Idx, succ(Idx) );
          end;
    end;

  procedure TGameMasterList.ChangeGameMasterProperties( GameMaster : TGameMasterListInfo; NewOptions : TGMConnectOptions; out NewIdx : integer );
    begin
      if GameMaster.Options <> NewOptions
        then
          begin
            fGameMasters.Extract( GameMaster );
            GameMaster.Options := NewOptions;
            NewIdx             := fGameMasters.InsertPos( GameMaster );
            fGameMasters.AtInsert( NewIdx, GameMaster );
          end
        else NewIdx := fGameMasters.IndexOf( GameMaster );

    end;

  procedure TGameMasterList.LoadFromString( const aStr : string );
    var
      StrList : TStringList;
      GMCount : integer;
      Name    : string;
      Opt     : TGMConnectOptions;
      i       : integer;
    begin
      try
        StrList := TStringList.Create;
        try
          StrList.Text := aStr;
          try
            GMCount := StrToInt( StrList.Values['Count'] );
          except
            GMCount := 0;
          end;
          fGameMasters.DeleteAll;
          for i := 0 to pred(GMCount) do
            begin
              try
                Name := StrList.Values['GMName' + IntToStr(i)];
                try
                  Opt  := TGMConnectOptions( StrToInt( StrList.Values['GMOptions' + IntToStr(i)]));
                except
                  Opt := GMCO_NORMALPRIORITY;
                end;
                fGameMasters.Insert( TGameMasterListInfo.Create( Name, Opt ) );
              except
              end;
            end;
        finally
          StrList.Free;
        end;
      except
      end;
    end;

  procedure TGameMasterList.SaveToString( var aStr : string );
    var
      StrList : TStringList;
      i       : integer;
    begin
      StrList := TStringList.Create;
      try
        StrList.Values['Count'] := IntToStr(fGameMasters.Count);
        for i := 0 to pred(fGameMasters.Count) do
          begin
            StrList.Values['GMName' + IntToStr(i)]    := TGameMasterListInfo(fGameMasters[i]).Name;
            StrList.Values['GMOptions' + IntToStr(i)] := IntToStr(integer(TGameMasterListInfo(fGameMasters[i]).Options));
          end;
        aStr := StrList.Text;
      finally
        StrList.Free;
      end;
    end;

  function TGameMasterList.getCount : integer;
    begin
      result := fGameMasters.Count;
    end;

  function TGameMasterList.getItemByIdx( Idx : integer ) : TGameMasterListInfo;
    begin
      result := TGameMasterListInfo(fGameMasters[Idx]);
    end;

  function TGameMasterList.getItemByName( Name : string ) : TGameMasterListInfo;
    var
      found : boolean;
      i     : integer;
    begin
      found := false;
      i     := 0;
      while (i < fGameMasters.Count) and not found do
        begin
          found := TGameMasterListInfo(fGameMasters[i]).Name = Name;
          inc( i );
        end;
      if found
        then result := TGameMasterListInfo(fGameMasters[pred(i)])
        else result := nil;
    end;

  function TGameMasterList.Compare( Item1, Item2 : TObject) : integer;
    begin
      if TGameMasterListInfo(Item1).Options > TGameMasterListInfo(Item2).Options
        then result := 1
        else
          if TGameMasterListInfo(Item1).Options < TGameMasterListInfo(Item2).Options
            then result := -1
            else result := 0;
    end;

end.
