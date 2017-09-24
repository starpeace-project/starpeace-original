unit RankingsDaemon;

interface

  uses
    DirServerSession, Daemons;

  function CreateDaemon(const DSAddr : string; DSPort : integer) : IDaemon;

implementation

  uses
    Windows, Classes, SysUtils, SyncObjs, Inifiles, RankProtocol, Forms, Clipbrd,
    MathUtils, Logs, DirectoryServerProtocol;

  const
    cLogId = 'Rankings Daemon';

  type
    TUserHash = 'A'..'Z';
    
  type
    TRanking =
      class
        public
          constructor Create(const Id, Name, Super : string; RankType : integer);
          destructor  Destroy; override;
        public
          function  AddRankedPlayer(const Name, Value : string) : integer;
          procedure Sort;
          procedure Generate(const Session : IDirServerSession; Run : integer);
        private
          fId            : string;
          fName          : string;
          fSuperId       : string;
          fSuper         : TRanking;
          fType          : integer;
          fRankedPlayers : TStringList;
          function GetKeyName : string;
      end;

  type
    TRankingValue =
      class
        private
          constructor Create(const Name, Value : string); virtual;
        private
          function  ValueAsStr : string; virtual; abstract;
          procedure Increment(const Value : string); virtual; abstract;
          function  Compare(Value : TRankingValue) : integer; virtual; abstract;
        private
          fName : string;
      end;

    CRankingValue = class of TRankingValue;

  type
    TIntegerValue =
      class(TRankingValue)
        private
          constructor Create(const Name, Value : string); override;
        private
          function  ValueAsStr : string; override;
          procedure Increment(const Value : string); override;
          function  Compare(Value : TRankingValue) : integer; override;
        private
          fValue : integer;
      end;

    CIntegerValue = class of TIntegerValue;

  type
    TCurrencyValue =
      class(TRankingValue)
        private
          constructor Create(const Name, Value : string); override;
        private
          function  ValueAsStr : string; override;
          procedure Increment(const Value : string); override;
          function  Compare(Value : TRankingValue) : integer; override;
        private
          fValue : currency;
      end;

    CCurrencyValue = class of TCurrencyValue;

  type
    TITAValue =
      class(TRankingValue)
        private
          constructor Create(const Name, Value : string); override;
        private
          function  ValueAsStr : string; override;
          procedure Increment(const Value : string); override;
          function  Compare(Value : TRankingValue) : integer; override;
        private
          fValue : integer;
          fCount : integer;
      end;

    CITAValue = class of TITAValue;

  const
    cRankingTypeClasses : array [1..4] of CRankingValue = (TCurrencyValue, TIntegerValue, TCurrencyValue, TITAValue);

  type
    TPlayerCurriculum =
      class
        private
          constructor Create(const PlayerName : string);
          destructor  Destroy; override;
        private
          procedure AddRanking(const Name : string; Position : integer; const Value : string);
          procedure WriteToDataBase(const Session : IDirServerSession; Run : integer);
        private
          fPlayerName   : string;
          fRankingCount : integer;
          fRankings     : TStringList;
      end;

  type
    TRankingsDaemon =
      class(TBasicDaemon)
        private // IDaemon
          function  GetName : string;                      override;
          function  GetDescription : string;               override;
        private
          procedure Execute;                               override;
          function  GetLogId : string;                     override;
        private
          procedure GenerateGlobalRankings;
        private
          fRun : integer;
      end;

  function CreateDaemon(const DSAddr : string; DSPort : integer) : IDaemon;
    begin
      Result := TRankingsDaemon.Create(DSAddr, DSPort);
    end;

  function CompareRankingValues(Item1, Item2 : pointer) : integer;
    begin
      Result := TRankingValue(Item1).Compare(TRankingValue(Item2));
    end;

  function GenerateValidRankKey(const id : string) : string;
    begin
      if (id[1] in ['A'..'Z']) or (id[1] in ['a'..'z'])
        then Result := id
        else Result := 'R' + id;
    end;

  constructor TRanking.Create(const Id, Name, Super : string; RankType : integer);
    begin
      inherited Create;
      fId := Id;
      fName := Name;
      fSuperId := Super;
      fType := RankType;
      fRankedPlayers := TStringList.Create;
    end;

  destructor TRanking.Destroy;
    var
      i : integer;
    begin
      for i := 0 to pred(fRankedPlayers.Count) do
        TRankingValue(fRankedPlayers.Objects[i]).Free;
      fRankedPlayers.Free;
      inherited;
    end;

  function TRanking.AddRankedPlayer(const Name, Value : string) : integer;
    var
      idx : integer;
    begin
      idx := fRankedPlayers.IndexOf(Name);
      if idx <> -1
        then TRankingValue(fRankedPlayers.Objects[idx]).Increment(Value) // player already ranked
        else idx := fRankedPlayers.AddObject(Name, cRankingTypeClasses[fType].Create('Value', Value));
      Result := idx;
    end;

  procedure TRanking.Sort;
    type
      TCompareFun = function (Item1, Item2 : pointer) : integer;

    procedure QuickSort(SortList : TStringList; L, R: Integer; CompareFun : TCompareFun);
      var
        i, j : integer;
        P    : pointer;
      begin
        repeat
          i := L;
          j := R;
          P := SortList.Objects[(L + R) shr 1];
          repeat
            while CompareFun(SortList.Objects[i], P) < 0 do inc(i);
            while CompareFun(SortList.Objects[j], P) > 0 do dec(j);
            if i <= j
              then
                begin
                  SortList.Exchange(i, j);
                  inc(i);
                  dec(j);
                end;
          until i > j;
          if L < j
            then QuickSort(SortList, L, j, CompareFun);
          L := I;
        until i >= R;
      end;

    begin
      if fRankedPlayers.Count > 0
        then QuickSort(fRankedPlayers, 0, pred(fRankedPlayers.Count), CompareRankingValues);
    end;

  procedure TRanking.Generate(const Session : IDirServerSession; Run : integer);
    const
      cPrestigeRankingId = 'Prestige';
      cWealthRankingId   = 'Wealth';
      cITARankingId      = 'ITA';
    var
      basekey          : string;
      validkey         : string;
      i                : integer;
      playername       : string;
      playerkey        : string;
      userhash         : TUserHash;
      value            : string;
      PrintableRanking : TStringList;
    begin
      Log(cLogId, 'Generating ranking ' + fName);
      basekey := 'Root/Rankings/' + GetKeyName;
      if Session.CreateFullPathKey(basekey, true) and Session.SetCurrentKey(basekey)
        then
          begin
            if Session.ValueExists('Current')
              then
                begin
                  validkey := Session.ReadString('Current');
                  if validkey <> ''
                    then
                      if validkey[length(validkey)] = '1'
                        then validkey[length(validkey)] := '2'
                        else validkey[length(validkey)] := '1'
                    else validkey := 'K1';
                end
              else validkey := 'K1';
            Session.WriteString('Name', fName);
            PrintableRanking := TStringList.Create;
            try
              for i := pred(fRankedPlayers.Count) downto 0 do
                begin
                  playername := fRankedPlayers[i];
                  if playername <> ''
                    then
                      begin
                        playerkey := GetAliasId(playername);
                        value := TRankingValue(fRankedPlayers.Objects[i]).ValueAsStr;
                        PrintableRanking.Add(playername + '=' + value);
                        userhash := playerkey[1];
                        if (fId = cPrestigeRankingId) or (fId = cWealthRankingId) or (fId = cITARankingId)
                          then
                            if Session.SetCurrentKey('Root/Users/' + userhash + '/' + playerkey)
                              then
                                if Session.CreateFullPathKey('Root/Users/' + userhash + '/' + playerkey + '/' + fId, true) and
                                   Session.SetCurrentKey('Root/Users/' + userhash + '/' + playerkey + '/' + fId)
                                  then
                                    begin
                                      Session.WriteString('Value', value);
                                      Session.WriteInteger('Run', Run);
                                    end
                                  else Log(cLogId, 'Unable to set or create ' + 'Root/Users/' + userhash + '/' + playerkey + '/' + fId + ' key')
                              else Log(cLogId, 'Unable to set ' + 'Root/Users/' + userhash + '/' + playerkey + ' key');
                      end;
                end;
              if Session.SetCurrentKey(basekey)
                then
                  begin
                    Session.WriteString(validkey, PrintableRanking.Text);
                    {$IFDEF EXTRALogs}
                    Log(fName, PrintableRanking.Text);
                    {$ENDIF}
                    Session.WriteString('Current', validkey);
                  end
                else Log(cLogId, 'Unable to set key ' + basekey);
            finally
              PrintableRanking.Free;
            end;
          end
        else Log(cLogId, 'Unable to find, create or set ' + basekey + ' key');
    end;

  function TRanking.GetKeyName : string;
    var
      prefix : string;
      Super  : TRanking;
    begin
      prefix := '';
      Super := fSuper;
      while Super <> nil do
        begin
          prefix := GenerateValidRankKey(Super.fId) + '/' + prefix;
          Super := Super.fSuper;
        end;
      Result := prefix + GenerateValidRankKey(fId);
    end;

  // TRankingValue

  constructor TRankingValue.Create(const Name, Value : string);
    begin
      inherited Create;
      fName := Name;
    end;

  // TIntegerValue

  constructor TIntegerValue.Create(const Name, Value : string);
    var
      dbl : double;
    begin
      inherited Create(Name, Value);
      dbl := StrToFloat(Value);
      fValue := round(dbl);
    end;

  function TIntegerValue.ValueAsStr : string;
    begin
      Result := IntToStr(fValue);
    end;

  procedure TIntegerValue.Increment(const Value : string);
    var
      newvalue : integer;
    begin
      newvalue := StrToInt(Value);
      inc(fValue, newvalue);
    end;

  function TIntegerValue.Compare(Value : TRankingValue) : integer;
    begin
      Result := fValue - TIntegerValue(Value).fValue;
    end;

  // TCurrencyValue

  constructor TCurrencyValue.Create(const Name, Value : string);
    begin
      inherited Create(Name, Value);
      fValue := StrToCurr(Value);
    end;

  function TCurrencyValue.ValueAsStr : string;
    begin
      Result := FormatMoney(fValue);
    end;

  procedure TCurrencyValue.Increment(const Value : string);
    var
      newvalue : currency;
    begin
      newvalue := StrToCurr(Value);
      fValue := fValue + newvalue;
    end;

  function TCurrencyValue.Compare(Value : TRankingValue) : integer;
    begin
      if fValue < TCurrencyValue(Value).fValue
        then Result := -1
        else
          if fValue = TCurrencyValue(Value).fValue
            then Result := 0
            else Result := 1;
    end;

  // TITAValue

  constructor TITAValue.Create(const Name, Value : string);
    begin
      inherited Create(Name, Value);
      fValue := StrToInt(Value);
      fCount := 1;
    end;

  function TITAValue.ValueAsStr : string;
    var
      value : integer;
    begin
      value := round(fValue/fCount);
      Result := IntToStr(value);
    end;

  procedure TITAValue.Increment(const Value : string);
    const
      cITAConstant = 10000;
    var
      newvalue : integer;
    begin
      newvalue := cITAConstant - StrToInt(Value);
      inc(fValue, newvalue);
      inc(fCount);
    end;

  function TITAValue.Compare(Value : TRankingValue) : integer;
    begin
      if fValue < TITAValue(Value).fValue
        then Result := -1
        else
          if fValue = TITAValue(Value).fValue
            then Result := 0
            else Result := 1;
    end;

  const
    cDefaultPeriod = 60*60*1000;

  // TPlayerCurriculum

  constructor TPlayerCurriculum.Create(const PlayerName : string);
    begin
      inherited Create;
      fPlayerName := PlayerName;
      fRankings := TStringList.Create;
    end;

  destructor TPlayerCurriculum.Destroy;
    begin
      fRankings.Free;
      inherited;
    end;

  procedure TPlayerCurriculum.AddRanking(const Name : string; Position : integer; const Value : string);
    begin
      fRankings.Add('Name' + IntToStr(fRankingCount) + '=' + Name);
      fRankings.Add('Position' + IntToStr(fRankingCount) + '=' + IntToStr(Position));
      fRankings.Add('Value' + IntToStr(fRankingCount) + '=' + Value);
      inc(fRankingCount);
    end;

  procedure TPlayerCurriculum.WriteToDataBase(const Session : IDirServerSession; Run : integer);
    var
      playerkey : string;
      userhash  : TUserHash;
    begin
      playerkey := GetAliasId(fPlayerName);
      userhash := playerkey[1];
      if Session.SetCurrentKey('Root/Users/' + userhash + '/' + playerkey)
        then
          begin
            fRankings.Insert(0, 'Run=' + IntToStr(Run));
            fRankings.Insert(1, 'Count=' + IntToStr(fRankingCount));
            Session.WriteString('Rankings', fRankings.Text);
            {$IFDEF EXTRALogs}
            Log('Player Curricula', fRankings.Text);
            {$ENDIF}
          end
        else Log(cLogId, 'Unable to set ' + 'Root/Users/' + userhash + '/' + playerkey + ' key');
    end;

  // TRankingsDaemon

  function TRankingsDaemon.GetName : string;
    begin
      Result := cLogId;
    end;

  function TRankingsDaemon.GetDescription : string;
    begin
      Result := 'Rankings creator daemon';
    end;

  procedure TRankingsDaemon.Execute;
    begin
      inherited;
      GenerateGlobalRankings;
      inc(fRun);
    end;

  function TRankingsDaemon.GetLogId : string;
    begin
      Result := cLogId;
    end;

  procedure TRankingsDaemon.GenerateGlobalRankings;
    var
      basekey         : widestring;
      key             : widestring;
      WorldRankings   : TStringList;
      Areas           : TStringList;
      Worlds          : TStringList;
      i, j, k, l      : integer;
      rankcount       : integer;
      membercount     : integer;
      CurRanking      : TRanking;
      Rankings        : TList;
      UnParented      : TList;
      ITARanking      : TRanking;
      incrankings     : integer;
      idx             : integer;
      id              : string;
      name            : string;
      _type           : string;
      super           : string;
      rankstructure   : string;
      PlayerCurricula : TList;
      CurCurriculum   : TPlayerCurriculum;

    function GetRankingObject(const id, name, super : string; ranktype : integer) : TRanking;
      var
        i : integer;
      begin
        Result := nil;
        i := 0;
        while (Result = nil) and (i < Rankings.Count) do
          if TRanking(Rankings[i]).fId = id
            then Result := Rankings[i]
            else inc(i);
        if Result = nil
          then
            begin
              Result := TRanking.Create(id, name, super, ranktype);
              Rankings.Add(Result);
            end;
        // find super now
        i := 0;
        while (Result.fSuper = nil) and (i < Rankings.Count) do
          if TRanking(Rankings[i]).fId = Result.fSuperId
            then Result.fSuper := Rankings[i]
            else inc(i);
        if Result.fSuper = nil // super not found, add to unresolved supers list
          then Unparented.Add(Result);
        // now check if new ranking is the parent of anybody
        i := 0;
        while i < Unparented.Count do
          begin
            if TRanking(Unparented[i]).fSuperId = Result.fId
              then
                begin
                  TRanking(Unparented[i]).fSuper := Result;
                  Unparented[i] := nil;
                end;
            inc(i);
          end;
        Unparented.Pack;
      end;

    function GenerateRankingStructure : string;
      var
        level       : integer;
        Ranking     : TRanking;
        RankStrList : TStringList;

      procedure GenerateRanking(Ranking : TRanking; idx, level : integer);
        var
          i        : integer;
          nodepath : string;
          Super    : TRanking;
        begin
          nodepath := '';
          Super := Ranking.fSuper;
          while Super <> nil do
            begin
              nodepath := GenerateValidRankKey(Super.fId) + '/' + nodepath;
              Super := Super.fSuper;
            end;
          if nodepath <> ''
            then nodepath := nodepath + GenerateValidRankKey(Ranking.fId)
            else nodepath := GenerateValidRankKey(Ranking.fId);
          RankStrList.Add(nodepath);
          RankStrList.Add(Ranking.fName);
          RankStrList.Add(IntToStr(level));
          // now write the children
          Rankings[idx] := nil;
          for i := 0 to pred(Rankings.Count) do
            if (Rankings[i] <> nil) and (TRanking(Rankings[i]).fSuper = Ranking)
              then GenerateRanking(Rankings[i], i, level + 1);
          Ranking.Free;
        end;

      begin
        Result := '';
        RankStrList := TStringList.Create;
        try
          while Rankings.Count > 0 do
            begin
              Ranking := Rankings[0];
              while Ranking.fSuper <> nil do
                Ranking := Ranking.fSuper;
              level := 0;
              GenerateRanking(Ranking, Rankings.IndexOf(Ranking), level);
              Rankings.Pack;
            end;
          Result := RankStrList.Text;
        finally
          //RankStrList.SaveToFile('RankingStructure.txt');
          RankStrList.Free;
        end;
      end;

    function GetPlayerCurriculum(const PlayerName : string) : TPlayerCurriculum;
      var
        i     : integer;
        found : boolean;
      begin
        i := 0;
        found := false;
        while (i < PlayerCurricula.Count) and not found do
          begin
            found := TPlayerCurriculum(PlayerCurricula[i]).fPlayerName = PlayerName;
            if not found
              then inc(i);
          end;
        if not found
          then
            begin
              Result := TPlayerCurriculum.Create(PlayerName);
              PlayerCurricula.Add(Result);
            end
          else Result := PlayerCurricula[i];
      end;

    begin
      try
        Log(cLogId, 'Starting rankings generation at ' + DateTimeToStr(Now));
        Areas := TStringList.Create;
        try
          Worlds := TStringList.Create;
          try
            WorldRankings := TStringList.Create;
            try
              Rankings := TList.Create;
              try
                Unparented := TList.Create;
                try
                  basekey := 'Root/Areas';
                  if fSession.SetCurrentKey(basekey)
                    then
                      begin
                        Areas.Text := fSession.GetKeyNames;
                        for i := 0 to pred(Areas.Count) do
                          begin
                            key := basekey + '/' + Areas[i] + '/Worlds';
                            if fSession.SetCurrentKey(key)
                              then
                                begin
                                  Worlds.Text := fSession.GetKeyNames;
                                  for j := 0 to pred(Worlds.Count) do
                                    begin
                                      key := key + '/' + Worlds[j] + '/Model/Rankings';
                                      if fSession.SetCurrentKey(key)
                                        then
                                          begin
                                            if fSession.ValueExists('Ranking')
                                              then
                                                with WorldRankings do
                                                  begin
                                                    Text := fSession.ReadString('Ranking');
                                                    Log('Read rankings', 'Rankings read from ' + Areas[i] + ', ' + Worlds[j] + #10#13 + Text);
                                                    try
                                                      if (Text <> '') and (IndexOf(tidTerminator_EndOfRanking) <> -1)
                                                        then
                                                          begin
                                                            rankcount := StrToInt(Values[tidRankings_RankCount]);
                                                            for k := 0 to pred(rankcount) do
                                                              begin
                                                                id := Values[IntToStr(k) + tidRankings_RankId];
                                                                name := Values[IntToStr(k) + tidRankings_RankName];
                                                                _type := Values[IntToStr(k) + tidRankings_RankType];
                                                                super := Values[IntToStr(k) + tidRankings_RankSuper];
                                                                if (id <> '') and (id <> 'NTA') and (name <> '') and (_type <> '') and (_type <> '0') and (super <> '')
                                                                  then
                                                                    try
                                                                      CurRanking := GetRankingObject(Values[IntToStr(k) + tidRankings_RankId], Values[IntToStr(k) + tidRankings_RankName],
                                                                                                     Values[IntToStr(k) + tidRankings_RankSuper], StrToInt(Values[IntToStr(k) + tidRankings_RankType]));
                                                                      membercount := StrToInt(Values[IntToStr(k) + tidRankings_RankMemberCount]);
                                                                      for l := 0 to pred(membercount) do
                                                                        CurRanking.AddRankedPlayer(Values[IntToStr(k) + tidRankings_Member + IntToStr(l) + tidRankings_MemberName], Values[IntToStr(k) + tidRankings_Member + IntToStr(l) + tidRankings_MemberValue]);
                                                                    except
                                                                      on e : Exception do
                                                                        Log(cLogId, 'Exception "' + e.Message + '" generated while processing ranking ' + name + ' of world ' + Worlds[j] + ' in area ' + Areas[i] + ' at ' + DateTimeToStr(Now));
                                                                      else
                                                                        Log(cLogId, 'Unknown exception generated while processing ranking ' + name + ' of world ' + Worlds[j] + ' in area ' + Areas[i] + ' at ' + DateTimeToStr(Now));
                                                                    end;
                                                              end;
                                                          end
                                                        else Log(cLogId, 'Malformed or unexistent ranking in world ' + Worlds[j] + ' of area ' + Areas[i]);
                                                    except
                                                      on e : Exception do
                                                        Log(cLogId, 'Exception "' + e.Message + '" generated while processing rankings of world ' + Worlds[j] + ' in area ' + Areas[i] + ' at ' + DateTimeToStr(Now));
                                                      else
                                                        Log(cLogId, 'Unknown exception generated while processing rankings of world ' + Worlds[j] + ' in area ' + Areas[i] + ' at ' + DateTimeToStr(Now));
                                                    end;
                                                  end;
                                          end
                                        else Log(cLogId, 'Key ' + key + ' not found, or unable to be set');
                                      WorldRankings.Text := '';
                                      key := basekey + '/' + Areas[i] + '/Worlds';
                                    end;
                                end
                              else Log(cLogId, 'Key ' + key + ' not found, or unable to be set');
                          end;
                        Log(cLogId, IntToStr(Rankings.Count) + ' rankings found. Now sorting');
                        for i := 0 to pred(Rankings.Count) do
                          TRanking(Rankings[i]).Sort;
                        Log(cLogId, 'Creating ITA ranking');
                        incrankings := 0;
                        ITARanking := TRanking.Create('ITA', 'ITA', '', 4);
                        for i := 0 to pred(Rankings.Count) do
                          with TRanking(Rankings[i]) do
                            if fRankedPlayers.Count > 0
                              then
                                begin
                                  inc(incrankings);
                                  for j := 0 to pred(fRankedPlayers.Count) do
                                    begin
                                      idx := ITARanking.AddRankedPlayer(fRankedPlayers[j], IntToStr(fRankedPlayers.Count - j));
                                      if TITAValue(ITARanking.fRankedPlayers.Objects[idx]).fCount = 1
                                        then
                                          for k := 0 to pred(i) do
                                            if TRanking(Rankings[k]).fRankedPlayers.Count > 0
                                              then TRankingValue(ITARanking.fRankedPlayers.Objects[idx]).Increment(IntToStr(TRanking(Rankings[k]).fRankedPlayers.Count + 1))
                                    end;
                                  for j := 0 to pred(ITARanking.fRankedPlayers.Count) do
                                    if TITAValue(ITARanking.fRankedPlayers.Objects[j]).fCount < incrankings
                                      then TRankingValue(ITARanking.fRankedPlayers.Objects[j]).Increment(IntToStr(fRankedPlayers.Count + 1));
                                end;
                        Log(cLogId, 'Sorting ITA ranking');
                        ITARanking.Sort;
                        Rankings.Insert(0, ITARanking);
                        Log(cLogId, 'ITA ranking sorted. Now gathering user curricula');
                        PlayerCurricula := TList.Create;
                        try
                          try
                            for i := 0 to pred(Rankings.Count) do
                              begin
                                CurRanking := Rankings[i];
                                with CurRanking do
                                  for j := pred(fRankedPlayers.Count) downto 0 do
                                    begin
                                      CurCurriculum := GetPlayerCurriculum(fRankedPlayers[j]);
                                      CurCurriculum.AddRanking(CurRanking.fName, fRankedPlayers.Count - j, TRankingValue(fRankedPlayers.Objects[j]).ValueAsStr);
                                    end;
                              end;
                          except
                            on e : Exception do
                              Log(cLogId, 'Exception "' + e.Message + '" generated while generating user curricula at ' + DateTimeToStr(Now));
                          end;
                          try
                            ITARanking.Generate(fSession, fRun);
                            Log(cLogId, 'ITA ranking generated. Generating other rankings');
                            for i := 1 to pred(Rankings.Count) do
                              begin
                                CurRanking := Rankings[i];
                                if CurRanking.fSuper = nil
                                  then CurRanking.fSuper := ITARanking;
                                CurRanking.Generate(fSession, fRun);
                              end;
                          except
                            on e : Exception do
                              Log(cLogId, 'Exception "' + e.Message + '" generated while writing rankings at ' + DateTimeToStr(Now));
                            else
                              Log(cLogId, 'Unknown exception generated while while writing rankings at ' + DateTimeToStr(Now));
                          end;
                          Log(cLogId, 'Rankings generated. Now generating structure and version information');
                          if fSession.SetCurrentKey('Root/Rankings')
                            then
                              begin
                                rankstructure := GenerateRankingStructure;
                                fSession.WriteString('Structure', rankstructure);
                                fSession.WriteInteger('Run', fRun);
                                fSession.WriteString('DefaultRankingId', 'ITA');
                                Log(cLogId, 'Structure and version information generated');
                              end
                            else Log(cLogId, 'Unable to set ' + 'Root/Rankings' + ' key in order to generate structure and version information');
                          Log(cLogId, 'Generating player curricula');
                          try
                            for i := 0 to pred(PlayerCurricula.Count) do
                              TPlayerCurriculum(PlayerCurricula[i]).WriteToDataBase(fSession, fRun);
                            Log(cLogId, 'Player curricula generated at ' + DateTimeToStr(Now));
                          except
                            on e : Exception do
                              Log(cLogId, 'Exception "' + e.Message + '" generated while writing player curricula at ' + DateTimeToStr(Now));
                          end;
                          for i := 0 to pred(PlayerCurricula.Count) do
                            TPlayerCurriculum(PlayerCurricula[i]).Free;
                        finally
                          PlayerCurricula.Free;
                        end;
                      end
                    else Log(cLogId, 'Key ' + basekey + ' not found, or unable to be set');
                finally
                  Unparented.Free;
                end;
                for i := 0 to pred(Rankings.Count) do
                  TRanking(Rankings[i]).Free;
              finally
                Rankings.Free;
              end;
            finally
              WorldRankings.Free;
            end;
          finally
            Worlds.Free;
          end;
        finally
          Areas.Free;
        end;
      Log(cLogId, 'Rankings succesfully generated at ' + DateTimeToStr(Now));
      except
        on e : Exception do
          Log(cLogId, 'Exception "' + e.Message + '" generated while generating rankings at ' + DateTimeToStr(Now));
        else
          Log(cLogId, 'Unknown exception generated while while generating rankings at ' + DateTimeToStr(Now));
      end;
    end;

end.
