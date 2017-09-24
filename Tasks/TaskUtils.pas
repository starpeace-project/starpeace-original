unit TaskUtils;

interface

  uses
    Classes, Collection, Kernel, FacIds, Population, Tasks, BuildFacilitiesTask;

  const
    tidConstBlockClassName = 'TBlockUnderConstruction';

  function FindFacility(FacId : TFacId; Company : TCompany; var Facility : TFacility) : boolean;
  function FindFacilityInTycoon(FacId : TFacId; Tycoon : TTycoon; var Company : TCompany; var Facility : TFacility) : boolean;
  function GetTypicalFacilities(MetaTask : TMetaBuildFacilitiesTask) : TStringList;
  function ConstructionConnected(Block : TBlock) : boolean;
  function TownHasFacilities(Town : TTown; Facs : array of TFacId) : boolean;
  function FindFacilityNotBelongingTo(xPos, yPos : integer; Town : TTown; Tycoon : TTycoon; Facs : array of TFacId) : TFacility;
  function FindSupplierFor(Input : TInput; var Suggestion : array of TPoint) : integer;

implementation

  uses
    SysUtils, ClassStorage, ClassStorageInt, StdFluids, MathUtils, EvaluatedBlock;

  // Utilitary Functions

  function FindFacility(FacId : TFacId; Company : TCompany; var Facility : TFacility) : boolean;
    var
      i   : integer;
      cnt : integer;
    begin
      Facility := nil;
      cnt := Company.Facilities.Count;
      Company.Facilities.Lock;
      try
        i := 0;
        while (i < cnt) and (TFacility(Company.Facilities[i]).MetaFacility.FacID <> FacId) do
          inc(i);
        if i < cnt
          then Facility := TFacility(Company.Facilities[i])
          else Facility := nil;
        result := Facility <> nil;
      finally
        Company.Facilities.Unlock;
      end;
    end;

  function FindFacilityInTycoon(FacId : TFacId; Tycoon : TTycoon; var Company : TCompany; var Facility : TFacility) : boolean;
    var
      i   : integer;
      cnt : integer;
    begin
      Company  := nil;
      Facility := nil;
      cnt := Tycoon.AllCompaniesCount;
      i   := 0;
      while (i < cnt) and (Facility = nil) do
        begin
          Company := Tycoon.AllCompanies[i];
          FindFacility(FacId, Company, Facility);
          inc(i);
        end;
      result := Facility <> nil;
    end;

  function GetTypicalFacilities(MetaTask : TMetaBuildFacilitiesTask) : TStringList;
    var
      CltCnt : integer;
      FacCnt : integer;
      i      : integer;
      c      : integer;
      Fac    : TMetaFacility;
      ClsStg : TClassStorage;
      idx    : integer;
    begin
      result := TStringList.Create;
      // Add the cluster to the list
      ClsStg := ClassStorage.TheClassStorage;
      CltCnt := ClsStg.ClassCount[tidClassFamily_Clusters];
      for c := 0 to pred(CltCnt) do
        result.AddObject(TCluster(ClsStg.ClassByIdx[tidClassFamily_Clusters, c]).Id, nil);
      // Add the meta facilities to the list
      FacCnt := ClsStg.ClassCount[tidClassFamily_Facilities];
      i      := 0;
      while i < FacCnt do
        begin
          Fac := TMetaFacility(ClsStg.ClassByIdx[tidClassFamily_Facilities, i]);
          if MetaTask.IsEquivalent(Fac.FacId)
            then
              begin
                idx := result.IndexOf(uppercase(Fac.ClusterName));
                if (idx <> -1)
                  then
                    if result.Objects[idx] = nil
                      then result.Objects[idx] := Fac
                      else
                        if TMetaFacility(result.Objects[idx]).Price > Fac.Price
                          then result.Objects[idx] := Fac;
              end;
          inc(i);
        end;
    end;

  function InputConnected(Block : TBlock; InputName : string) : boolean;
    var
      Input : TInput;
      CCnt  : integer;
      i     : integer;
    begin
      Input := Block.InputsByName[InputName];
      if Input = nil
        then result := true
        else
          begin
            CCnt := Input.ConnectionCount;
            i    := 0;
            while (i < CCnt) and ((Input.ExtraConnectionInfo[i] = nil) or not Input.ExtraConnectionInfo[i].Connected) do
              inc(i);
            result := i < CCnt;
          end;
    end;

  function ConstructionConnected(Block : TBlock) : boolean;
    begin
      result := InputConnected(Block, tidFluid_ConstructionForce) and
                InputConnected(Block, tidFluid_Machinery) and
                InputConnected(Block, tidFluid_BusinessMachines);
    end;

  function TownHasFacilities(Town : TTown; Facs : array of TFacId) : boolean;

    var
      Facilities : TLockableCollection;
      i          : integer;
      count      : integer;
      size       : integer;
      found      : integer;
      lIdx       : integer;
      hIdx       : integer;

    function CheckFacility(Facility : TFacility) : boolean;
      var
        k : integer;
      begin
        if (Town <> Facility.Town) or (Facility.MetaFacility.FacId = 0)
          then result := false
          else
            begin
              k := lIdx;
              while (k <= hIdx) and (Facility.MetaFacility.FacId <> Facs[k]) do
                inc(k);
              if k <= hIdx
                then
                  begin
                    Facs[k] := 0;
                    inc(found);
                  end;
              result := found = size;
            end;
      end;

    begin
      Facilities := TInhabitedTown(Town).World.Facilities;
      lIdx       := low(Facs);
      hIdx       := high(Facs);
      size       := hIdx - lIdx + 1;
      found      := 0;
      Facilities.Lock;
      try
        i := 0;
        count := Facilities.Count;
        while (i < count) and not CheckFacility(TFacility(Facilities[i])) do
          inc(i);
        result := i < count;
      finally
        Facilities.UnLock;
      end;
    end;

  function FindFacilityNotBelongingTo(xPos, yPos : integer; Town : TTown; Tycoon : TTycoon; Facs : array of TFacId) : TFacility;

    var
      Facilities : TLockableCollection;
      i          : integer;
      count      : integer;
      dist       : integer;

    function MatchFacility(Fac : TFacility) : boolean;
      var
        k : integer;
        d : integer;
      begin
        if (Fac.Company <> nil) and (Fac.Company.Owner <> Tycoon)
          then
            begin
              k := low(Facs);
              while (k <= high(Facs)) and (Fac.MetaFacility.FacId <> Facs[k])do
                inc(k);
              if k <= high(Facs)
                then
                  begin
                    d := MathUtils.Dist(xPos, yPos, Fac.xPos, Fac.yPos);
                    if (dist = 0) or (dist < d)
                      then
                        begin
                          result := true;
                          dist   := d;
                        end
                      else result := false
                  end
                else result := false;
            end
          else result := false;
      end;

    begin
      dist       := 0;
      Facilities := TInhabitedTown(Town).World.Facilities;
      Facilities.Lock;
      try
        i     := 0;
        count := Facilities.Count;
        while (i < count) and not MatchFacility(TFacility(Facilities[i])) do
          inc(i);
        if i < count
          then result := TFacility(Facilities[i])
          else result := nil;
      finally
        Facilities.UnLock;
      end;
    end;

  function FacilityProduces(Fac : TFacility; Input : TInput) : boolean;
    var
      Block : TBlock;
      cnt   : integer;
      i     : integer;
      Msg   : TBlockOverloadedMsg;
    begin
      Block := Fac.CurrBlock;
      cnt   := Block.OutputCount;
      i     := 0;
      while (i < cnt) and (Block.Outputs[i].MetaOutput.MetaFluid <> Input.MetaInput.MetaFluid) do
        inc(i);
      if i < cnt
        then
          begin
            Msg.Gate := Input.MetaInput;
            Block.Dispatch(Msg);
            result := Msg.Result;
          end
        else result := false;
    end;

  function FindSupplierFor(Input : TInput; var Suggestion : array of TPoint) : integer;

    var
      Town       : TTown;
      Facilities : TLockableCollection;
      Fac        : TFacility;
      i          : integer;
      count      : integer;
      dist       : integer;
      xpos       : integer;
      ypos       : integer;
      sgcnt      : integer;

    procedure InsertFacility(Fac : TFacility);
      begin
        if (Suggestion[0].x <> 0) or (Suggestion[0].y <> 0)
          then
            begin
              if MathUtils.Dist(Suggestion[0].x, Suggestion[0].y, xPos, ypos) > dist
                then
                  begin
                    Move(Suggestion[0], Suggestion[1], (sgcnt - 1)*sizeof(Suggestion[0]));
                    Suggestion[0].x := Fac.xPos;
                    Suggestion[0].y := Fac.yPos;
                  end;
            end
          else
            begin
              Suggestion[0].x := Fac.xPos;
              Suggestion[0].y := Fac.yPos;
            end
      end;

    begin
      Town       := Input.Block.Facility.Town;
      sgcnt      := high(Suggestion) - low(Suggestion) + 1;
      result     := 0;
      xpos       := Input.Block.xPos;
      ypos       := Input.Block.yPos;
      dist       := 0;
      Facilities := TInhabitedTown(Town).World.Facilities;
      Facilities.Lock;
      try
        i     := 0;
        count := Facilities.Count;
        while i < count do
          begin
            Fac   := TFacility(Facilities[i]);
            if (Fac.Town = Town) and FacilityProduces(Fac, Input)
              then
                begin
                  dist := MathUtils.Dist(xpos, ypos, Fac.xPos, Fac.yPos);
                  InsertFacility(Fac);
                  result := min(sgcnt, result + 1);
                end;
            inc(i);
          end;
      finally
        Facilities.UnLock;
      end;
    end;

end.

