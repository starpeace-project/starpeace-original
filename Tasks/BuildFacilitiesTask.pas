unit BuildFacilitiesTask;

interface

  uses
    Classes, Tasks, Kernel, Tutorial, CacheAgent, FacIds, BackupInterfaces;

  const
    stgZero    = 0;
    stgConnect = 1;
    stgWait    = 2;

  const
    ciConnected    = 0;
    ciDisconnected = 1;

  type
    TFacilityChoiceMethod = (fcmFirst, fcmCheaper, fcmRandom);

  type
    TMetaBuildFacilitiesTask =
      class(TMetaTask)
        public
          constructor Create(anId, aName, aDesc, aSuperTaskId : string; aPeriod : integer; aTaskClass : CTask); overload;
          constructor Create(srcTaskId, newId, newSuperTaskId : string); overload;
          destructor  Destroy; override;
        private
          fBlockClass   : string;
          fFacId        : TFacId;
          fTypicalFacs  : TStringList;
          fEqivIds      : PFacIdArray;
          fEqivCount    : integer;
          fChoiceMethod : TFacilityChoiceMethod;
          fFacCount     : integer;
        public
          function  IsEquivalent(Id : TFacId) : boolean;
          procedure EquivalentIds(Ids : array of TFacId);
        private
          procedure GetTypicalFacilities;
          function  GetTypicalFac(cluster : string) : TMetaFacility;
          procedure SetChoiceMethod(aMethod : TFacilityChoiceMethod);
        public
          property BlockClass   : string                read fBlockClass   write fBlockClass;
          property FacId        : integer               read fFacId        write fFacId;
          property FacCount     : integer               read fFacCount     write fFacCount;
          property ChoiceMethod : TFacilityChoiceMethod read fChoiceMethod write SetChoiceMethod;
          property TypicalFac[cluster : string] : TMetaFacility read GetTypicalFac;
      end;

    TCnntInfo      = byte;
    TFacilityArray = array[0..10] of TFacility;
    PFacilityArray = ^TFacilityArray;
    TCnntInfoArray = array[0..10] of TCnntInfo;
    PCnntInfoArray = ^TCnntInfoArray;

    TBuildFacilitiesTask =
      class(TAtomicTask)
        public
          constructor Create(aMetaTask : TMetaTask; aSuperTask : TSuperTask; aContext : ITaskContext); override;
          destructor  Destroy; override;
        protected
          fMetaFacility : TMetaFacility;
          fReqCount     : byte;
          fCount        : byte;
          fEnded        : byte;
          fFacs         : PFacilityArray;
          fCnntInfo     : PCnntInfoArray;
        public
          function  GetProgress : integer; override;
          procedure StoreToCache(Prefix : string; Cache : TObjectCache); override;
          procedure LoadFromBackup(Reader : IBackupReader); override;
          procedure StoreToBackup (Writer : IBackupWriter); override;
          function  AllConnected : boolean;
        protected
          procedure FacilityRemoved(Fac : TFacility); virtual;
        public
          function Execute : TTaskResult; override;
        private
          procedure MsgNewBlock(var Msg : TMsgNewBlock); message msgKernel_NewBlock;
          procedure MsgDelFacility(var Msg : TMsgFacilityDeleted); message msgKernel_FacilityDeleted;
          procedure SetReqCount(count : byte);
        protected
          property ReqCount : byte read fReqCount write SetReqCount;
        protected
          function  GetTargetBlock : string; virtual;
          procedure AddFacility(Fac : TFacility; Connected : boolean);
          function  FacilityIndex(Fac : TFacility) : integer;
          function  DelFacility(Fac : TFacility) : boolean;
          procedure FacilityBuilt(Fac : TFacility); virtual;
        protected
          property Facs : PFacilityArray read fFacs;
      end;

  procedure RegisterBackup;

implementation

  uses
    SysUtils, TaskUtils, Collection, Population, ModelServerCache, ClassStorage,
    ClassStorageInt, StdFluids, MathUtils, Languages;

  // TMetaBuildFacilitiesTask

  constructor TMetaBuildFacilitiesTask.Create(anId, aName, aDesc, aSuperTaskId : string; aPeriod : integer; aTaskClass : CTask);
    begin
      inherited;
      fTypicalFacs := TStringList.Create;
    end;

  constructor TMetaBuildFacilitiesTask.Create(srcTaskId, newId, newSuperTaskId : string);
    begin
      inherited Create(srcTaskId, newId, newSuperTaskId);
      fBlockClass   := TMetaBuildFacilitiesTask(Source).BlockClass;
      fFacId        := TMetaBuildFacilitiesTask(Source).FacId;
      fFacCount     := 1;
      fTypicalFacs  := TStringList.Create;
      fTypicalFacs.Assign(TMetaBuildFacilitiesTask(Source).fTypicalFacs);
      fEqivCount := TMetaBuildFacilitiesTask(Source).fEqivCount;
      if fEqivCount > 0
        then
          begin
            ReallocMem(fEqivIds, fEqivCount*sizeof(fEqivIds[0]));
            System.move(TMetaBuildFacilitiesTask(Source).fEqivIds, fEqivIds, fEqivCount*sizeof(fEqivIds[0]));
          end
        else fEqivIds := nil;
      fChoiceMethod := TMetaBuildFacilitiesTask(Source).fChoiceMethod;
    end;

  destructor TMetaBuildFacilitiesTask.Destroy;
    var
      i : integer;
    begin
      for i := 0 to pred(fTypicalFacs.Count) do
        fTypicalFacs.Objects[i].Free;
      fTypicalFacs.Free;
      if fEqivIds <> nil
        then FreeMem(fEqivIds);
      inherited;
    end;

  function TMetaBuildFacilitiesTask.IsEquivalent(Id : TFacId) : boolean;
    var
      i : integer;
    begin
      if Id = fFacId
        then result := true
        else
          begin
            i := 0;
            while (i < fEqivCount) and (fEqivIds[i] <> Id) do
              inc(i);
            result := i < fEqivCount;
          end;
    end;

  procedure TMetaBuildFacilitiesTask.EquivalentIds(Ids : array of TFacId);
    begin
      fEqivCount := FacIds.NewFacIds(fEqivIds, Ids);
    end;

  procedure TMetaBuildFacilitiesTask.GetTypicalFacilities;
    var
      CltCnt : integer;
      FacCnt : integer;
      i      : integer;
      c      : integer;
      Fac    : TMetaFacility;
      ClsStg : TClassStorage;
      idx    : integer;
      Facs   : TCollection;
    begin
      fTypicalFacs.Clear;
      // Add the cluster to the list
      ClsStg := ClassStorage.TheClassStorage;
      CltCnt := ClsStg.ClassCount[tidClassFamily_Clusters];
      for c := 0 to pred(CltCnt) do
        fTypicalFacs.AddObject(TCluster(ClsStg.ClassByIdx[tidClassFamily_Clusters, c]).Id, nil);
      // Add the meta facilities to the list
      FacCnt := ClsStg.ClassCount[tidClassFamily_Facilities];
      i      := 0;
      while i < FacCnt do
        begin
          Fac := TMetaFacility(ClsStg.ClassByIdx[tidClassFamily_Facilities, i]);
          if IsEquivalent(Fac.FacId)
            then
              begin
                idx := fTypicalFacs.IndexOf(Fac.ClusterName);
                if (idx <> -1)
                  then
                    if fTypicalFacs.Objects[idx] = nil
                      then
                        begin
                          Facs := TCollection.Create(0, rkUse);
                          fTypicalFacs.Objects[idx] := Facs;
                          Facs.Insert(Fac);
                        end
                      else
                        begin
                          Facs := TCollection(fTypicalFacs.Objects[idx]);
                          case fChoiceMethod of
                            fcmCheaper :
                              if TMetaFacility(Facs[0]).Price > Fac.Price
                                then Facs[0] := Fac;
                            fcmRandom  :
                              Facs.Insert(Fac);
                          end;
                        end;
              end;
          inc(i);
        end;
    end;

  procedure TMetaBuildFacilitiesTask.SetChoiceMethod(aMethod : TFacilityChoiceMethod);
    begin
      fChoiceMethod := aMethod;
      GetTypicalFacilities;
    end;

  function TMetaBuildFacilitiesTask.GetTypicalFac(cluster : string) : TMetaFacility;
    var
      idx  : integer;
      Facs : TCollection;
    begin
      if fTypicalFacs <> nil
        then
          begin
            idx := fTypicalFacs.IndexOf(cluster);
            if idx <> -1
              then
                begin
                  Facs := TCollection(fTypicalFacs.Objects[idx]);
                  if (Facs <> nil) and (Facs.Count > 0)
                    then
                      case fChoiceMethod of
                        fcmFirst, fcmCheaper :
                          result := TMetaFacility(Facs[0]);
                        fcmRandom :
                          result := TMetaFacility(Facs[random(Facs.Count)]);
                        else
                          result := nil;
                      end
                    else result := nil;
                end
              else result := nil;
          end
        else result := nil;
    end;

  // TBuildFacilitiesTask

  constructor TBuildFacilitiesTask.Create(aMetaTask : TMetaTask; aSuperTask : TSuperTask; aContext : ITaskContext);
    var
      Company : TCompany;
    begin
      inherited Create(aMetaTask, aSuperTask, aContext);
      ReqCount      := TMetaBuildFacilitiesTask(MetaTask).FacCount;
      Company       := TCompany(Context.getContext(tcIdx_Company));
      fMetaFacility := TMetaBuildFacilitiesTask(MetaTask).TypicalFac[Company.Cluster.Id];
    end;

  destructor TBuildFacilitiesTask.Destroy;
    begin
      if fFacs <> nil
        then FreeMem(fFacs);
      if fCnntInfo <> nil
        then FreeMem(fCnntInfo);
      inherited;
    end;

  function TBuildFacilitiesTask.GetProgress : integer;
    begin
      if fReqCount = 0
        then result := round(100*fEnded/fReqCount)
        else result := 100;
    end;

  procedure TBuildFacilitiesTask.StoreToCache(Prefix : string; Cache : TObjectCache);

    procedure CacheNotConnectedFacs;
      var
        i    : integer;
        iStr : string;
        cnt  : integer;
      begin
        cnt := 0;
        if fFacs <> nil
          then
            for i := 0 to pred(fCount) do
              if fCnntInfo[i] = ciDisconnected
                then
                  try
                    try
                      iStr := IntToStr(cnt);
                      Cache.WriteString(Prefix + 'toCnnName' + iStr, fFacs[i].Name);
                      Cache.WriteString(Prefix + 'toCnnX'    + iStr, IntToStr(fFacs[i].xPos));
                      Cache.WriteString(Prefix + 'toCnnY'    + iStr, IntToStr(fFacs[i].yPos));
                    finally
                      inc(cnt);
                    end;
                  except
                  end;
        Cache.WriteInteger(Prefix + 'toCnnCount', cnt);
      end;

    begin
      inherited;
      if fMetaFacility <> nil
        then
          begin
            Cache.WriteInteger(Prefix + 'FacId', fMetaFacility.FacId);
            Cache.WriteInteger(Prefix + 'FacVisualId', fMetaFacility.VisualClass + fMetaFacility.TypicalStage.Index);
            Cache.WriteString(Prefix  + 'FacName', fMetaFacility.Name);
            StoreMultiStringToCache( Prefix + 'FacName', fMetaFacility.Name_MLS, Cache );
            Cache.WriteString(Prefix  + 'FacKindName', fMetaFacility.Kind.Name);
            Cache.WriteString(Prefix  + 'FacKindId', fMetaFacility.Kind.Id);
            Cache.WriteString(Prefix  + 'FacPlural', fMetaFacility.PluralName);
            Cache.WriteInteger(Prefix + 'FacZone', fMetaFacility.ZoneType);
            Cache.WriteInteger(Prefix + 'ReqCount', fReqCount);
            Cache.WriteInteger(Prefix + 'Count', fCount);
          end;
      if fFacs <> nil
        then CacheNotConnectedFacs;
    end;

  procedure TBuildFacilitiesTask.LoadFromBackup(Reader : IBackupReader);
    var
      i    : integer;
      aux  : string;
    begin
      inherited;
      aux := Reader.ReadString('FacId', '');
      if aux <> ''
        then fMetaFacility := TMetaFacility(TheClassStorage.ClassById[tidClassFamily_Facilities, aux])
        else fMetaFacility := nil;
      ReqCount := Reader.ReadInteger('ReqCount', 1);
      fCount   := Reader.ReadInteger('Count', 0);
      fEnded   := Reader.ReadInteger('Ended', 0);
      for i := 0 to pred(fReqCount) do
        begin
          aux := IntToStr(i);
          Reader.ReadObject('toConn' + aux, fFacs[i], nil);
          fCnntInfo[i] := byte(Reader.ReadByte('cnntInfo' + aux, ciConnected));
        end;
    end;

  procedure TBuildFacilitiesTask.StoreToBackup (Writer : IBackupWriter);
    var
      i    : integer;
      iStr : string;
    begin
      inherited;
      Writer.WriteString('FacId', fMetaFacility.Id);
      Writer.WriteInteger('ReqCount', fReqCount);
      Writer.WriteInteger('Count', fCount);
      Writer.WriteInteger('Ended', fEnded);
      for i := 0 to pred(fReqCount) do
        begin
          iStr := IntToStr(i);
          Writer.WriteObjectRef('toConn' + iStr, fFacs[i]);
          Writer.WriteByte('cnntInfo' + iStr, fCnntInfo[i]);
        end;
    end;

  function TBuildFacilitiesTask.AllConnected : boolean;
    var
      i : integer;
    begin
      i := 0;
      while (i < fReqCount) and ((fCnntInfo[i] = ciConnected) or TaskUtils.ConstructionConnected(fFacs[i].CurrBlock)) do
        begin
          fCnntInfo[i] := ciConnected;
          inc(i);
        end;
      result := i = fReqCount;
    end;

  procedure TBuildFacilitiesTask.FacilityRemoved(Fac : TFacility);
    begin
    end;

  function TBuildFacilitiesTask.Execute : TTaskResult;      
    begin
      if fEnded < fReqCount
        then
          begin
            if (Stage = stgConnect) and (fCount = fReqCount) and AllConnected
              then
                begin
                  if MetaTask.StageCount >= stgWait
                    then
                      begin
                        Stage := stgWait;
                        UpdateObjectCache(Context.getContext(tcIdx_Tycoon), -1, -1);
                      end;
                end;
            result := trContinue;
          end
        else result := trFinished;
    end;

  procedure TBuildFacilitiesTask.MsgNewBlock(var Msg : TMsgNewBlock);
    var
      Company : TCompany;

    function SomeOneToConnect : boolean;
      var
        i : integer;
      begin
        i := 0;
        while (i < fReqCount) and (fCnntInfo[i] = ciConnected) do
          inc(i);
        result := i < fReqCount;
      end;

    begin
      Company := TCompany(Context.getContext(tcIdx_Company));
      if (Msg.Block <> nil) and
         (Msg.Block.Facility.MetaFacility.FacId = fMetaFacility.FacId) and
         (Msg.Block.Facility.Company = Company)
         then
           if Msg.Block.ClassName = tidConstBlockClassName
             then
               begin
                 if fCount < fReqCount // :P Stupid Bug! 
                   then
                     begin
                       AddFacility(Msg.Block.Facility, TaskUtils.ConstructionConnected(Msg.Block));
                       if fCount = fReqCount
                         then
                           begin
                             if SomeOneToConnect
                               then
                                 begin
                                   Stage := stgConnect;
                                   NotifyTycoon('');
                                 end
                               else
                                 if MetaTask.StageCount >= stgWait
                                   then
                                     begin
                                       Stage := stgWait;
                                       UpdateObjectCache(Context.getContext(tcIdx_Tycoon), -1, -1);
                                     end;
                           end;
                     end;
               end
             else
               if (fEnded < fReqCount) and (Msg.Block.ClassName = GetTargetBlock)
                 then
                   begin
                     inc(fEnded);
                     FacilityBuilt(Msg.Block.Facility);
                     if fEnded < fReqCount
                       then NotifyTycoon('');
                   end;
    end;

  procedure TBuildFacilitiesTask.MsgDelFacility(var Msg : TMsgFacilityDeleted);
    var
      Company : TCompany;
    begin
      Company := TCompany(Context.getContext(tcIdx_Company));
      if (Msg.Facility <> nil) and
         (Msg.Facility.MetaFacility.FacId = fMetaFacility.FacId) and
         (Msg.Facility.Company = Company)
        then
          begin
            if DelFacility(Msg.Facility)
              then
                begin
                  if (fEnded > 0) and (Msg.Facility.CurrBlock.ClassName = GetTargetBlock)
                    then dec(fEnded);
                  Stage := 0;
                  NotifyTycoon('');
                  FacilityRemoved(Msg.Facility);
                end;
          end;
    end;

  procedure TBuildFacilitiesTask.SetReqCount(count : byte);
    begin
      if fFacs <> nil
        then FreeMem(fFacs);
      if fCnntInfo <> nil
        then FreeMem(fCnntInfo);
      fReqCount := count;
      if count > 0
        then
          begin
            GetMem(fFacs, count*sizeof(fFacs[0]));
            FillChar(fFacs^, count*sizeof(fFacs[0]), 0);
            GetMem(fCnntInfo, count*sizeof(fCnntInfo[0]));
            FillChar(fCnntInfo^, count*sizeof(fCnntInfo[0]), ciConnected);
          end
        else
          begin
            fFacs := nil;
            fCnntInfo := nil;
          end;
    end;

  function TBuildFacilitiesTask.GetTargetBlock : string;
    begin
      result := TMetaBuildFacilitiesTask(MetaTask).BlockClass;
    end;

  procedure TBuildFacilitiesTask.AddFacility(Fac : TFacility; Connected : boolean);
    begin
      if Connected
        then fCnntInfo[fCount] := ciConnected
        else fCnntInfo[fCount] := ciDisconnected;
      fFacs[fCount] := Fac;
      inc(fCount);
    end;

  function TBuildFacilitiesTask.FacilityIndex(Fac : TFacility) : integer;
    begin
      result := pred(fCount);
      while (result >= 0) and (fFacs[result] <> Fac) do
        dec(result);
    end;

  function TBuildFacilitiesTask.DelFacility(Fac : TFacility) : boolean;
    var
      idx : integer;
      cnt : integer;
    begin
      idx := FacilityIndex(Fac);
      if idx >= 0
        then
          begin
            cnt := fCount - Idx - 1;
            if cnt > 0
              then
                begin
                  move(fFacs[idx + 1], fFacs[idx], cnt*sizeof(fFacs[0]));
                  move(fCnntInfo[idx + 1], fCnntInfo[idx], cnt*sizeof(fCnntInfo[0]));
                end;
            dec(fCount);
            result := true;
          end
        else result := false;
    end;


  procedure TBuildFacilitiesTask.FacilityBuilt(Fac : TFacility);
    begin
    end;

  // RegisterBackup

  procedure RegisterBackup;
    begin
      RegisterClass( TBuildFacilitiesTask );
    end;

end.
