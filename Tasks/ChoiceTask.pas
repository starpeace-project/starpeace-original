unit ChoiceTask;

interface

  uses
    Windows, Classes, Tasks, Kernel, Collection, BuildFacilitiesTask, FacIds,
    Persistent, CacheAgent, BackupInterfaces;

  const
    stgContractSupplier = 3;
    stgConnectSupplier  = 4;

  const
    MaxSuggestions = 3;

  const
    tidBuiltFacility          = 'BuiltFac';
    tidCookie_ConnectFacility = 'ConnFac';
    tidCookie_FacX            = 'FacX';
    tidCookie_FacY            = 'FacY';
    tidCookie_ConnToFacX      = 'CnnToX';
    tidCookie_ConnToFacY      = 'CnnToY';
    tidCookie_ConnToMetaFac   = 'CnnMetaFac';

  type
    TMetaChoice =
      class
        public
          constructor Create(aServiceId : string; TheIds : array of TFacId);
          destructor  Destroy; override;
        private
          fCookies    : TStringList;
          fServiceId  : string;
          fFacIds     : PFacIdArray;
          fCount      : integer;
        public
          procedure SetCookies(CookieList : TStringList);
        public
          property Cookies    : TStringList read fCookies;
          property ServiceId  : string      read fServiceId;
          property Ids        : PFacIdArray read fFacIds;
          property FacIdCount : integer     read fCount;
      end;

    TMetaChoiceTask =
      class(TMetaTask)
        public
          constructor Create(anId, aName, aDesc, aSuperTaskId : string; aPeriod : integer; aTaskClass : CTask);
          destructor  Destroy; override;
        private
          fMetaChoices : TCollection;
        public
          function AddChoice(Choice : TMetaChoice) : TMetaChoice;
        private
          function GetChoiceCount : integer;
          function GetChoice(index : integer) : TMetaChoice;
        public
          property ChoiceCount : integer read GetChoiceCount;
          property Choices[index : integer] : TMetaChoice read GetChoice;
      end;

    TChoiceTask =
      class(TAtomicTask)
        protected
          procedure DefineTarget; override;
          procedure Activate;     override;
      end;

    TMetaCookieBasedFacilityTask =
      class(TMetaBuildFacilitiesTask)
        private
          fCookieName  : string;
          fCookieValue : string;
          fReqFacs     : PFacIdArray;
          fReqCount    : integer;
        public
          procedure RequireFacilities(TheReqFacs : array of TFacId);
        public
          property CookieName  : string      read fCookieName  write fCookieName;
          property CookieValue : string      read fCookieValue write fCookieValue;
          property ReqFacs     : PFacIdArray read fReqFacs;
          property ReqCount    : integer     read fReqCount;
      end;

    TCookieBasedFacilityTask =
      class(TBuildFacilitiesTask)
        protected
          class function GetPriority(MetaTask : TMetaTask; SuperTask : TTask; Context : ITaskContext) : integer; override;
      end;

    TCookieBasedBuildStoreTask =
      class(TCookieBasedFacilityTask)
      end;

    TSuggestionArray = array[0..MaxSuggestions - 1] of TPoint;

    TConnectionInfo =
      class(TPersistent)
        public
          constructor Create(anInput : TInput);
        private
          fSuggestions : TSuggestionArray;
          fInput       : TInput;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    TCookieBasedBuildIndustryTask =
      class(TCookieBasedFacilityTask)
        protected
          procedure Initialize; override;
          procedure Finalize; override;
        private
          fInputs : TCollection;
        private
          procedure GetTroubleInputs;
          function  ConnectedInput(Input : TInput) : boolean;
          function  NeedsSuppliers : boolean;
          function  NeedsConnection : boolean;
          function  VerifyConnections : boolean;
        public
          function  Execute : TTaskResult; override;
        private
          function  CollectSuggestions(var Suggestions : array of TPoint) : integer;
        public
          procedure StoreToCache(Prefix : string; Cache : TObjectCache); override;
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        protected
          procedure FacilityRemoved(Fac : TFacility); override;
      end;

  procedure RegisterBackup;

implementation

  uses
    SysUtils, ServiceInfo, Population, MathUtils, TaskUtils;

  type
    PChoiceIndex = ^TChoiceIndex;
    TChoiceIndex = array[0..0] of integer;

  // TMetaChoice

  constructor TMetaChoice.Create(aServiceId : string; TheIds : array of TFacId);
    begin
      inherited Create;
      fCookies := TStringList.Create;
      fServiceId  := aServiceId;
      if TheIds[0] = FID_None
        then fCount := 0
        else fCount := FacIds.NewFacIds(fFacIds, TheIds);
    end;

  destructor TMetaChoice.Destroy;
    begin
      fCookies.Free;
      if fFacIds <> nil
       then FreeMem(fFacIds);
      inherited;
    end;

  procedure TMetaChoice.SetCookies(CookieList : TStringList);
    var
      i : integer;
    begin
      for i := 0 to pred(fCookies.Count) do
        CookieList.Add(fCookies[i]);
    end;

  // TMetaChoiceTask

  constructor TMetaChoiceTask.Create(anId, aName, aDesc, aSuperTaskId : string; aPeriod : integer; aTaskClass : CTask);
    begin
      inherited Create(anId, aName, aDesc, aSuperTaskId, aPeriod, aTaskClass);
      fMetaChoices := TCollection.Create(0, rkBelonguer);
    end;

  destructor TMetaChoiceTask.Destroy;
    begin
      fMetaChoices.Free;
      inherited;
    end;

  function TMetaChoiceTask.AddChoice(Choice : TMetaChoice) : TMetaChoice;
    begin
      fMetaChoices.Insert(Choice);
      result := Choice;
    end;

  function TMetaChoiceTask.GetChoiceCount : integer;
    begin
      result := fMetaChoices.Count;
    end;

  function TMetaChoiceTask.GetChoice(index : integer) : TMetaChoice;
    begin
      result := TMetaChoice(fMetaChoices[index]);
    end;

  // TChoiceTask

  procedure TChoiceTask.DefineTarget;
    var
      Indexes  : PChoiceIndex;
      Ratios   : PChoiceIndex;
      count    : integer;
      i, j     : integer;
      TownHall : TTownHall;
      Choice   : TMetaChoice;
      Service  : TServiceInfo;

    procedure Swap(var v1, v2 : integer);
      var
        t : integer;
      begin
        t  := v1;
        v1 := v2;
        v2 := t;
      end;

    function PossibleChoice(idx : integer) : boolean;
      begin
        Choice := TMetaChoiceTask(MetaTask).Choices[idx];
        result := (Choice.FacIdCount = 0) or TaskUtils.TownHasFacilities(TTown(Context.getContext(tcIdx_Town)), Slice(Choice.Ids^, Choice.FacIdCount));
      end;

    begin
      count := TMetaChoiceTask(MetaTask).ChoiceCount;
      GetMem(Indexes, count*sizeof(Indexes[0]));
      GetMem(Ratios, count*sizeof(Ratios[0]));
      try
        FillChar(Indexes^, count*sizeof(Indexes[0]), 0);
        TownHall := TTownHall(TInhabitedTown(Context.getContext(tcIdx_Town)).TownHall.CurrBlock);
        // Get the ratios
        for i := 0 to pred(count) do
          begin
            Choice  := TMetaChoiceTask(MetaTask).Choices[i];
            Service := TServiceInfo(TownHall.ServiceInfoById[Choice.ServiceId]);
            if Service <> nil
              then Ratios[i] := min(100, round(100*Service.Ratio)) + intcond(Service.Capacity > 0, 30, 0)
              else Ratios[i] := 100;
            Indexes[i] := i;
          end;
        // Sort the indexes usising bubble sort
        for i := 0 to pred(pred(count)) do
          for j := succ(i) to pred(count) do
            if Ratios[i] > Ratios[j]
              then
                begin
                  Swap(Ratios[i], Ratios[j]);
                  Swap(Indexes[i], Indexes[j]);
                end;
        // Get the best choice
        i := 0;
        while (i < count) and not PossibleChoice(Indexes[i]) do
          inc(i);
      if i < count
        then TMetaChoiceTask(MetaTask).Choices[Indexes[i]].SetCookies(SuperTask.CookieList);
      finally
        FreeMem(Indexes);
        FreeMem(Ratios);
      end;
    end;

  procedure TChoiceTask.Activate;
    begin
      SuperTask.SubTaskFinalize;
    end;


  // TMetaCookieBasedFacilityTask

  procedure TMetaCookieBasedFacilityTask.RequireFacilities(TheReqFacs : array of TFacId);
    begin
      fReqCount := FacIds.NewFacIds(fReqFacs, TheReqFacs);
    end;

  // TCookieBasedFacilityTask

  class function TCookieBasedFacilityTask.GetPriority(MetaTask : TMetaTask; SuperTask : TTask; Context : ITaskContext) : integer;
    begin
      result := inherited GetPriority(MetaTask, SuperTask, Context);
      if (result <> tprIgnoreTask) and (SuperTask.Cookies[TMetaCookieBasedFacilityTask(MetaTask).CookieName] <> TMetaCookieBasedFacilityTask(MetaTask).CookieValue)
        then result := tprIgnoreTask;
    end;


  // TConnectionInfo

  constructor TConnectionInfo.Create(anInput : TInput);
    begin
      inherited Create;
      fInput := anInput;
    end;

  procedure TConnectionInfo.LoadFromBackup(Reader : IBackupReader);
    begin
      Reader.ReadObject('Input', fInput, nil);
      Reader.ReadBuffer('Suggs', fSuggestions, nil, sizeof(fSuggestions));
    end;

  procedure TConnectionInfo.StoreToBackup(Writer : IBackupWriter);
    begin
      Writer.WriteObjectRef('Input', fInput);
      Writer.WriteBuffer('Suggs', fSuggestions, sizeof(fSuggestions));
    end;


  // TCookieBasedBuildIndustryTask

  procedure TCookieBasedBuildIndustryTask.Initialize;
    begin
      fInputs := TCollection.Create(0, rkBelonguer);
    end;

  procedure TCookieBasedBuildIndustryTask.Finalize;
    begin
      fInputs.Free;
    end;

  function TCookieBasedBuildIndustryTask.NeedsSuppliers : boolean;
    var
      i     : integer;
      cnt   : integer;
      Input : TInput;
    begin
      i       := 0;
      cnt     := fInputs.Count;
      result  := false;
      while (i < cnt) and not result do
        begin
          Input  := TConnectionInfo(fInputs[i]).fInput;
          result := Input.ConnectionCount = 0;
          inc(i);
        end;
    end;

  procedure TCookieBasedBuildIndustryTask.GetTroubleInputs;
    var
      Block  : TBlock;
      i      : integer;
      cnt    : integer;
      Input  : TInput;
      CnInfo : TConnectionInfo;
    begin
      fInputs.DeleteAll;
      Block := Facs[0].CurrBlock;
      cnt   := Block.InputCount;
      for i := 0 to pred(cnt) do
        begin
          Input := Block.Inputs[i];
          if (mfTradeable in Input.MetaInput.MetaFluid.Options) and (Input.MetaInput.Level = mglBasic) and ((Input.ConnectionCount = 0) or not ConnectedInput(Input))
            then
              begin
                CnInfo := TConnectionInfo.Create(Input);
                TaskUtils.FindSupplierFor(Input, CnInfo.fSuggestions);
                fInputs.Insert(CnInfo);
              end;
        end;
    end;

  function TCookieBasedBuildIndustryTask.ConnectedInput(Input : TInput) : boolean;
    var
      i   : integer;
      cnt : integer;
    begin
      cnt := Input.ConnectionCount;
      i   := 0;
      while (i < cnt) and ((Input.ExtraconnectionInfo[i] = nil) or Input.ExtraconnectionInfo[i].Connected) do
        inc(i);
      result := i = cnt;
    end;

  function TCookieBasedBuildIndustryTask.NeedsConnection : boolean;
    var
      i : integer;
    begin
      i := 0;
      while (i < fInputs.Count) and ConnectedInput(TConnectionInfo(fInputs[i]).fInput) do
        inc(i);
      result := i < fInputs.Count;
    end;

  function TCookieBasedBuildIndustryTask.VerifyConnections : boolean;
    var
      i : integer;
    begin
      i := 0;
      while i < fInputs.Count do
        if ConnectedInput(TConnectionInfo(fInputs[i]).fInput)
          then fInputs.AtDelete(i)
          else inc(i);
      result := fInputs.Count > 0;
    end;

  function TCookieBasedBuildIndustryTask.Execute : TTaskResult;
    begin
      result := inherited Execute;
      case Stage of
        stgWait :
          if result = trFinished
            then
              begin
                if fInputs = nil
                  then fInputs := TCollection.Create(0, rkBelonguer);
                if fInputs.Count = 0
                  then GetTroubleInputs;
                if NeedsSuppliers
                  then
                    begin
                      result := trContinue;
                      Stage  := stgContractSupplier;
                      NotifyTycoon('');
                    end
                  else
                    if VerifyConnections
                      then
                        begin
                          result := trContinue;
                          Stage  := stgConnectSupplier;
                          NotifyTycoon('');
                        end;
              end;
        stgContractSupplier :
          if NeedsSuppliers
            then result := trContinue
            else
              begin
                if VerifyConnections
                  then
                    begin
                      result := trContinue;
                      Stage  := stgConnectSupplier;
                      NotifyTycoon('');
                    end                                
                  else result := trFinished;
              end;
        stgConnectSupplier :
          if NeedsConnection
            then result := trContinue
            else result := trFinished;
      end;
    end;

  function TCookieBasedBuildIndustryTask.CollectSuggestions(var Suggestions : array of TPoint) : integer;
    var
      i     : integer;
      count : integer;

    procedure AddSuggestions(Suggs : array of TPoint);
      var
        k : integer;
        j : integer;
      begin
        for k := low(Suggs) to high(Suggs) do
          if (Suggs[k].x <> 0) or (Suggs[k].y <> 0)
            then
              begin
                j := 0;
                while (j < count) and ((Suggs[k].x <> Suggestions[j].x) or (Suggs[k].y <> Suggestions[j].y))do
                  inc(j);
                if j = count 
                  then
                    begin
                      Suggestions[count] := Suggs[k];
                      inc(count);
                    end;
              end;
      end;

    begin
      count := 0;
      for i := 0 to pred(fInputs.Count) do
        AddSuggestions(TConnectionInfo(fInputs[i]).fSuggestions);
      result := count;
    end;

  procedure TCookieBasedBuildIndustryTask.StoreToCache(Prefix : string; Cache : TObjectCache);
    var
      Tycoon : TTycoon;
      Input  : TInput;
      Suggs  : array[0..25] of TPoint;
      SgCnt  : integer;
      ActCnt : integer;
      i      : integer;
      j      : integer;
      Fac    : TFacility;
      iStr   : string;
    begin
      inherited;
      Tycoon := TTycoon(Context.getContext(tcIdx_Tycoon));
      case Stage of
        stgContractSupplier :
          begin
            for i := 0 to pred(fInputs.Count) do
              Cache.WriteString(Prefix + 'toCnnInp' + IntToStr(i), TConnectionInfo(fInputs[i]).fInput.MetaInput.MetaFluid.Name_MLS.Values[Tycoon.Language]);
            ActCnt := 0;
            SgCnt  := CollectSuggestions(Suggs);
            Cache.WriteInteger(Prefix + 'toCnnCount', SgCnt);
            for i := 0 to pred(SgCnt) do
              begin
                Fac := Tycoon.WorldLocator.FacilityAt(Suggs[i].x, Suggs[i].y);
                if Fac <> nil
                  then
                    begin
                      iStr := IntToStr(ActCnt);
                      inc(ActCnt);
                      Cache.WriteString(Prefix + 'toCnnName' + iStr, Fac.Name);
                      Cache.WriteInteger(Prefix + 'toCnnX' + iStr, Fac.xPos);
                      Cache.WriteInteger(Prefix + 'toCnnY' + iStr, Fac.yPos);
                      Cache.WriteInteger(Prefix + 'toCnnVID' + iStr, Fac.MetaFacility.VisualClass);
                    end;
              end;
          end;
        stgConnectSupplier :
          begin
            ActCnt := 0;
            for i := 0 to pred(fInputs.Count) do
              begin
                Input := TConnectionInfo(fInputs[i]).fInput;
                if not ConnectedInput(Input)
                  then
                    for j := 0 to pred(Input.ConnectionCount) do
                      if (Input.ExtraConnectionInfo[j] <> nil) and not Input.ExtraConnectionInfo[j].Connected
                        then
                          with Input.Connections[j].Block do
                            begin
                              iStr := IntToStr(ActCnt);
                              Cache.WriteString(Prefix + 'toCnnName' + iStr, Facility.Name);
                              Cache.WriteInteger(Prefix + 'toCnnX' + iStr, Facility.xPos);
                              Cache.WriteInteger(Prefix + 'toCnnY' + iStr, Facility.yPos);
                              Cache.WriteInteger(Prefix + 'toCnnVID' + iStr, Facility.MetaFacility.VisualClass);
                              Inc(ActCnt);
                            end;
                Cache.WriteInteger(Prefix + 'toCnnCount', ActCnt);
              end;
          end;
      end;
    end;

  procedure TCookieBasedBuildIndustryTask.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      Reader.ReadObject('Inputs', fInputs, nil);
    end;

  procedure TCookieBasedBuildIndustryTask.StoreToBackup ( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteLooseObject('Inputs', fInputs);
    end;

  procedure TCookieBasedBuildIndustryTask.FacilityRemoved(Fac : TFacility);
    begin
      inherited;
      fInputs.DeleteAll;
    end;


  // RegisterBackup

  procedure RegisterBackup;
    begin
      RegisterClass(TChoiceTask);
      RegisterClass(TCookieBasedFacilityTask);
      RegisterClass(TCookieBasedBuildStoreTask);
      RegisterClass(TCookieBasedBuildIndustryTask);
      RegisterClass(TConnectionInfo);
    end;

end.


