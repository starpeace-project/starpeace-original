unit ResearchCenter;

interface

  uses
    Protocol, WorkCenterBlock, Kernel, Collection, Classes, CacheAgent,
    BackupInterfaces, Accounts, Inventions;

  type
    TCollection = Collection.TCollection;

  const
    tidURL_SpecialMailMessages = 'Visual/Voyager/Mail/SpecialMessages/';
    tidMailMsgURL_NoMoneyRes   = tidURL_SpecialMailMessages + 'MsgNoMoneyForResearch.asp';

  type
    TMetaResearchCenter =
      class( TMetaWorkCenter )
        public
          constructor Create( anId           : string;
                              aCapacities    : array of TFluidValue;
                              aInventionKind : string;
                              aBlockClass    : CBlock );
          destructor  Destroy; override;
        private
          fInventionKind : string;
          fInventions    : TCollection;
          fStandalone    : boolean;
        private
          function GetMinimalWorkForce( kind : TPeopleKind ) : TFluidValue;
        public
          property InventionKind : string read fInventionKind;
          property MinimalWorkForce[kind : TPeopleKind] : TFluidValue read GetMinimalWorkForce;
          property Standalone : boolean read fStandalone write fStandalone;
        public
          procedure RegisterInvention(Invention : TInvention);
          procedure RegisterInventions(Inventions : array of TInvention);
        private
          function GetInventionCount : integer;
          function GetInvention( index : integer ) : TInvention;
        public
          property InventionCount : integer read GetInventionCount;
          property Inventions[index : integer] : TInvention read GetInvention;
        public
          function FindInvention(id : string) : TInvention;
      end;

    TDevelopingResearch =
      class
        public
          constructor Create( anInv : TInvention; aPrior : byte );
        private
          fInvention : TInvention;
          fPriority  : byte;
      end;

    TResearchCenter =
      class( TFinanciatedWorkCenter )
        public
          constructor Create( aMetaBlock : TMetaBlock; aFacility : TFacility ); override;
          destructor  Destroy; override;
        public
          procedure Stop; override;
        private
          procedure StartResearch;
          procedure InsertResearch(DevRes : TDevelopingResearch);
          function  CanResearch(InventionId : TInventionNumId) : boolean;
        published
          procedure ResearchInvention(Invention : TInvention; Priority : integer);
          procedure RestoreResearch(InventionId : string; Priority : integer);
          procedure QueueResearch(InventionId : string; Priority : integer);
          procedure CancelResearch(InventionId : string);
          procedure RDOQueueResearch(InventionId : widestring; Priority : integer);
          procedure RDOCancelResearch(InventionId : widestring);
          function  RDOGetInvProps(InventionId : widestring) : olevariant;
          function  RDOGetInvPropsByLang(InventionId, lang : widestring) : olevariant;
          function  RDOGetInvDesc(InventionId : widestring) : olevariant;
          function  RDOGetInvDescEx(InventionId, LangId : widestring) : olevariant;
        protected
          fCurrResearch     : TDevelopingResearch;
          fProgress         : single;
          fDirCoverage      : word;
          fDevResearches    : TCollection;
          fWFError          : single;
          fCompanyDirection : TCompanyDirection;
        protected
          function Evaluate : TEvaluationResult; override;
          function GetVisualClassId  : TVisualClassId; override;
          function GetUpgradeCost : TMoney; override;
          function GetActualMaxUpgrade : integer; override;
        public
          procedure AutoConnect( loaded : boolean ); override;
        published
          function GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string; override;
        public
          procedure BlockLoaded; override;
          procedure StoreToCache  ( Cache  : TObjectCache  ); override;
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        private
          function GetResearch( index : integer ) : TDevelopingResearch;
          function GetResearchById( InventionId : TInventionNumId ) : TDevelopingResearch;
        public
          property Researches[index : integer] : TDevelopingResearch read GetResearch;
          property ResearchById[InventionId : TInventionNumId] : TDevelopingResearch read GetResearchById;
        private
          procedure CheckResearches;
        protected
          function  RenderCloneMenu(lang : string) : string; override;
      end;

  const
    PeopleToStrength             : array[TPeopleKind] of single = (60, 30, 6);
    StandaloneHQPeopleToStrength : array[TPeopleKind] of single = (200, 100, 50);

  const
    WorkforceOpenRatio  = 1;
    WorkforceCloseRatio = 1;
    BasicWorkforce      = 0.1;

  procedure RegisterBackup;

implementation

  uses
    ClassStorage, SysUtils, MathUtils, Population, SimHints, BasicAccounts, Logs,
    Languages, ModelServerCache, CloneOptions;

  const
    WFErrorDelta = 0.1;

  // TMetaResearchCenter

  constructor TMetaResearchCenter.Create( anId : string; aCapacities : array of TFluidValue; aInventionKind : string; aBlockClass : CBlock );
    begin
      inherited Create( anId, aCapacities, accIdx_ResearchCenter_Supplies, accIdx_None, accIdx_ResearchCenter_Salaries, aBlockClass );
      fInventionKind := aInventionKind;
      fInventions    := TCollection.Create(0, rkUse);
      MaxUpgrade     := StrToInt(TheGlobalConfigHandler.GetConfigParm('MaxRCenterUpgrade', '20'));
    end;

  destructor TMetaResearchCenter.Destroy;
    begin
      fInventions.Free;
      inherited;
    end;

  function TMetaResearchCenter.GetMinimalWorkForce( kind : TPeopleKind ) : TFluidValue;
    begin
      result := BasicWorkForce*Capacity[kind];
    end;

  procedure TMetaResearchCenter.RegisterInvention(Invention : TInvention);
    begin
      fInventions.Insert(Invention);
      if fInventionKind <> ''
        then fInventionKind := Invention.Kind;
    end;

  procedure TMetaResearchCenter.RegisterInventions(Inventions : array of TInvention);
    var
      i : integer;
    begin
      for i := low(Inventions) to high(Inventions) do
        RegisterInvention( Inventions[i] );
    end;

  function TMetaResearchCenter.GetInventionCount : integer;
    begin
      result := fInventions.Count;
    end;

  function TMetaResearchCenter.GetInvention( index : integer ) : TInvention;
    begin
      result := TInvention(fInventions[index]);
    end;

  function TMetaResearchCenter.FindInvention(id : string) : TInvention;
    var
      i : integer;
    begin
      i := pred(fInventions.Count);
      while (i >= 0) and (Inventions[i].Id <> id) do
        dec(i);
      if i >= 0
        then result := Inventions[i]
        else result := nil;
    end;

  // TDevelopingResearch

  constructor TDevelopingResearch.Create( anInv : TInvention; aPrior : byte );
    begin
      inherited Create;
      fInvention := anInv;
      fPriority  := aPrior;
    end;

  // TResearchCenter

  constructor TResearchCenter.Create( aMetaBlock : TMetaBlock; aFacility : TFacility );
    begin
      inherited;
      fDevResearches := TCollection.Create( 0, rkBelonguer );
    end;

  destructor TResearchCenter.Destroy;
    begin
      fDevResearches.Free;
      inherited;
    end;

  procedure TResearchCenter.Stop;
    begin
      inherited;
      fDirCoverage := 0;
    end;

  procedure TResearchCenter.StartResearch;
    var
      WorldLocator : IWorldLocator;
      URL          : string;
      resName      : string;
      lang         : string; 
      reason       : string;
    begin
      try
        Facility.Lock;
        try
          fCurrResearch.Free;
          if fDevResearches.Count > 0
            then
              begin
                fCurrResearch := TDevelopingResearch(fDevResearches.AtExtract(0));
                if not fCurrResearch.fInvention.Enabled(Facility.Company)
                  then StartResearch
                  else
                    begin
                      // This is to avoid the start of a research if you don't have the money
                      if Facility.Company.Owner.Budget < (fCurrResearch.fInvention.Price +  fCurrResearch.fInvention.GetFeePrice(Facility.Company))
                      then
                        begin
                          if Facility.Company.Owner.Budget > fCurrResearch.fInvention.Price
                          then
                            reason := 'license'
                          else
                            reason := 'research';
                          Facility.UpdateCache(true);
                          Facility.StatusChanged(fchStructure);
                          WorldLocator := Facility.Town.WorldLocator;
                          lang := Facility.Company.Owner.Language;
                          resName := fCurrResearch.fInvention.Name_MLS.Values[lang];
                          URL := WorldLocator.GetWorldURL + '/' + lang + '/' + tidMailMsgURL_NoMoneyRes + '?Company=' + Facility.Company.Name + '&InvName='+ resName + '&TycName=' + Facility.Company.Owner.Name + '&Reason=' + reason;
                          WorldLocator.SendEmail('mailer@Global' + WorldLocator.GetWorldName + '.net', Facility.Company.Owner.Name + '@' + WorldLocator.GetWorldName + '.net', 'No Money for ' + resName + '!', URL); //TheModelServer.TheWorld.SendNoMoneyForResearchMsg(Facility.Company.Owner.Name, Facility.Company.Name, fCurrResearch.fInvention.Name, Facility.Company.Owner.Language);
                          fCurrResearch := nil;
                        end;
                    end;
              end
            else fCurrResearch := nil;
        finally
          fProgress := 0;
          Facility.Unlock;
        end;
      except
      end;
    end;

  procedure TResearchCenter.InsertResearch( DevRes : TDevelopingResearch );
    var
      i   : integer;
      cnt : integer;
    begin
      Facility.Lock;
      try
        cnt := fDevResearches.Count;
        i   := 0;
        while (i < cnt) and (Researches[i].fPriority <= DevRes.fPriority) do
          inc(i);
        fDevResearches.AtInsert(i, DevRes);
      finally
        Facility.Unlock;
      end;
    end;

  function TResearchCenter.CanResearch(InventionId : TInventionNumId) : boolean;
    begin
      Facility.Lock;
      try
        result := (ResearchById[InventionId] = nil) and ((fCurrResearch = nil) or (fCurrResearch.fInvention.NumId <> InventionId));
      finally
        Facility.Unlock;
      end;
    end;

  procedure TResearchCenter.ResearchInvention(Invention : TInvention; Priority : integer);
    begin
      InsertResearch(TDevelopingResearch.Create(Invention, Priority));
      if fCurrResearch = nil
        then StartResearch;
    end;

  procedure TResearchCenter.RestoreResearch(InventionId : string; Priority : integer);
    var
      Inv : TInvention;
    begin
      Inv := GetInventionById(InventionId);
      if Inv <> nil
        then
          if fCurrResearch = nil
            then fCurrResearch := TDevelopingResearch.Create(Inv, Priority)
            else InsertResearch(TDevelopingResearch.Create(Inv, Priority));
    end;

  procedure TResearchCenter.QueueResearch(InventionId : string; Priority : integer );
    var
      Inv : TInvention;
    begin
      try
        Inv := GetInventionById(InventionId);
        if (Inv <> nil) and CanResearch(Inv.NumId) and not Facility.Company.HasInvention[Inv.NumId] and Inv.Enabled(Facility.Company)
          then
            if Inv.Time > 0
              then
                begin
                  ResearchInvention(Inv, Priority);
                  Facility.UpdateCache(true);
                end
              else
                if Facility.Budget >= Inv.Price + Inv.GetFeePrice(Facility.Company)
                  then
                    begin
                      BlockGenMoney(-Inv.Price, accIdx_ResearchCenter_Research);
                      Facility.Company.DeclareInvention(Inv);
                      Facility.UpdateCache(true);
                      Facility.StatusChanged(fchStructure);
                    end;
      except
      end;
    end;

  procedure TResearchCenter.CancelResearch(InventionId : string);
    var
      Inv : TInvention;
      Dev : TDevelopingResearch;
    begin
      try
        Inv := GetInventionById(InventionId);
        if Inv <> nil
          then
            begin
              if (fCurrResearch <> nil) and (fCurrResearch.fInvention.NumId = Inv.NumId)
                then
                  begin
                    // Return the money spent upto now
                    BlockGenMoney((fProgress/100)*Inv.Price, accIdx_ResearchCenter_Research); // accIdx_Compensations
                    StartResearch;
                  end
                else
                  begin
                    Dev := ResearchById[Inv.NumId];
                    if Dev <> nil
                      then
                        begin
                          Facility.Lock;
                          try
                            fDevResearches.Delete(Dev)
                          finally
                            Facility.Unlock;
                          end
                        end
                      else
                        if Facility.Company.RetireInvention(Inv)
                          then CheckResearches;
                  end;
              Facility.UpdateCache(false);
              Facility.StatusChanged(fchStructure);
            end;
      except
      end;
    end;

  procedure TResearchCenter.RDOQueueResearch(InventionId : widestring; Priority : integer);
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Queue Research: ' + InventionId + ', ' + IntToStr(Priority) );
      try
        if Facility.CheckOpAuthenticity
          then QueueResearch(InventionId, Priority);
        Logs.Log( tidLog_Survival,  'OK!');
      except
        Logs.Log( tidLog_Survival,  'Error!');
      end;
    end;

  procedure TResearchCenter.RDOCancelResearch(InventionId : widestring);
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Cancel Research: ' + InventionId );
      try
        if Facility.CheckOpAuthenticity
          then CancelResearch(InventionId);
        Logs.Log( tidLog_Survival,  'OK!');
      except
        Logs.Log( tidLog_Survival,  'Error!');
      end;
    end;

  function TResearchCenter.RDOGetInvProps(InventionId : widestring) : olevariant;
    var
      Inv : TInvention;
      lid : string;
    begin
      try
        Inv := TMetaResearchCenter(MetaBlock).FindInvention(InventionId);
        if Inv <> nil
          then
            begin
              if (Facility.Company <> nil) and (Facility.Company.Owner <> nil)
                then lid := Facility.Company.Owner.Language
                else lid := langDefault;
              result := Inv.GetClientProps(Facility.Company, lid) // >> MLS Alarm!
            end
          else result := '';
      except
        result := '';
      end;
    end;

  function TResearchCenter.RDOGetInvPropsByLang(InventionId, lang : widestring) : olevariant;
    var
      Inv : TInvention;
      lid : string;
    begin
      try
        Inv := TMetaResearchCenter(MetaBlock).FindInvention(InventionId);
        if Inv <> nil
          then
            begin
              lid := lang;
              if Facility.Company <> nil
                then result := Inv.GetClientProps(Facility.Company, lid)
                else result := '';
            end
          else result := '';
      except
        result := '';
      end;
    end;

  function TResearchCenter.RDOGetInvDesc(InventionId : widestring) : olevariant;
    var
      Inv : TInvention;
    begin
      try
        Inv := TMetaResearchCenter(MetaBlock).FindInvention(InventionId);
        if Inv <> nil
          then result := Inv.GetFullDesc( langDefault ) // >> MLS Alarm!
          else result := '';
      except
        result := '';
      end;
    end;

  function TResearchCenter.RDOGetInvDescEx(InventionId, LangId : widestring) : olevariant;
    var
      Inv : TInvention;
    begin
      try
        Inv := TMetaResearchCenter(MetaBlock).FindInvention(InventionId);
        if (Inv <> nil) and (LangId <> '')
          then result := Inv.GetFullDesc( LangId)
          else result := '';
      except
        result := '';
      end;
    end;

{
  function TResearchCenter.RDOGetInvHist(InventionId : widestring) : olevariant;
    var
      InvRec : TInventionRecord;
    begin
      try
        if Facility.Company <> nil
          then
            begin
              //InvRec := Facility.Company.FindInventionRecord(InventionId);
              if InvRec <> nil
                then result := InvRec.Invention.Desc // >> FIX!!
                else result := '';
            end
          else result := '';
      except
        result := '';
      end;
    end;
}

  function TResearchCenter.Evaluate : TEvaluationResult;

    function ComputeDirectionStrength : integer;
      var
        kind     : TPeopleKind;
        strength : single;
      begin
        strength := 0;
        for kind := low(kind) to high(kind) do
          if TMetaResearchCenter(MetaBlock).Standalone and (Workers[kind].Q > 0)
            then strength := strength + StandaloneHQPeopleToStrength[kind]*sqrt(Workers[kind].Q) //sqrt(sqrt(Workers[kind].Q))
            else strength := strength + PeopleToStrength[kind]*Workers[kind].Q;
        result := round(strength);
      end;

    procedure AdjustWorkforce;
      var
        SupDirectionSup  : single;
        MB               : TMetaResearchCenter;
        kind             : TPeopleKind;
        NewStrength      : integer;
        lastSupport      : single;
        ugrLevel         : byte;
      begin
        MB := TMetaResearchCenter(MetaBlock);
        if fCompanyDirection = nil
          then fCompanyDirection := Facility.Company.Directions[MB.InventionKind];
        ugrLevel := max(1, UpgradeLevel);
        lastSupport := fCompanyDirection.Support;
        if not Facility.CriticalTrouble
          then
            if fCompanyDirection.Demand > 0
              then
                begin
                  if (lastSupport >= 1) and (lastSupport < 2)
                    then fWFError := 0
                    else fWFError := realmax(-2, realmin(2, fWFError + 1.5 - realmin(2, lastSupport)));

                  for kind := low(kind) to high(kind) do
                    if TMetaResearchCenter(MetaBlock).Capacity[kind] > 0
                      then
                        begin
                          if WorkersMax[kind].Q = 0
                            then WorkersMax[kind].Q := MB.MinimalWorkForce[kind]
                            else WorkersMax[kind].Q := realmin(ugrLevel*TMetaResearchCenter(MetaBlock).Capacity[kind], realmax(ugrLevel*MB.MinimalWorkForce[kind], WorkersMax[kind].Q + WFErrorDelta*fWFError));
                        end;
                end
              else
                for kind := low(kind) to high(kind) do
                  if fCurrResearch <> nil
                    then WorkersMax[kind].Q := MB.MinimalWorkForce[kind]
                    else WorkersMax[kind].Q := SmartRound(0.5*MB.MinimalWorkForce[kind]);

        if Facility.HasTechnology
          then
            begin
              if Facility.CompanyDir <> nil
                then SupDirectionSup := sqrt(realmin(1, Facility.CompanyDir.Support))
                else SupDirectionSup := 1;
              NewStrength               := ComputeDirectionStrength;
              fCompanyDirection.Strength := fCompanyDirection.Strength + NewStrength*SupDirectionSup;
              fDirCoverage              := round(100*fCompanyDirection.Support); //min(100, round(100*CompanyDirection.Support))
            end
          else
            begin
              SupDirectionSup := 0;
              fDirCoverage    := 0;
            end;

        if SupDirectionSup >= 0.9
          then Facility.ClearTrouble(facNeedCompSupport)
          else Facility.ReportTrouble(facNeedCompSupport);

      end;

    procedure StandaloneAdjustWorkforce;
      var
        MB       : TMetaResearchCenter;
        kind     : TPeopleKind;
        ugrLevel : byte;
      begin
        MB := TMetaResearchCenter(MetaBlock);
        if fCompanyDirection = nil
          then fCompanyDirection := Facility.Company.Directions[MB.InventionKind];
        ugrLevel := max(1, UpgradeLevel);
        for kind := low(kind) to high(kind) do
          WorkersMax[kind].Q := ugrLevel*sqr(ugrLevel)*MB.Capacity[kind];
        if Facility.HasTechnology
          then
            begin
              fCompanyDirection.Strength := fCompanyDirection.Strength + ComputeDirectionStrength;
              fDirCoverage               := round(100*fCompanyDirection.Support);
            end
          else fDirCoverage := 0;
      end;

    var
      kind         : TPeopleKind;
      hourday      : single;
      dtCorrection : single;
      capacity     : single;
      cnt          : integer;
      ToPay        : TMoney;
      extraPrg     : double;
    begin
      result := inherited Evaluate;
      if TMetaResearchCenter(MetaBlock).Standalone
        then StandaloneAdjustWorkforce
        else AdjustWorkForce;
      if not Facility.CriticalTrouble
        then
          if fCurrResearch <> nil
            then
              begin
                capacity := 0;
                cnt      := 0;
                for kind := low(kind) to high(kind) do
                  if TMetaResearchCenter(MetaBlock).Capacity[kind] > 0
                    then
                      begin
                        capacity := capacity + Workers[kind].Q/WorkersMax[kind].Q;
                        inc( cnt );
                      end;
                if cnt > 0
                  then capacity := capacity/cnt
                  else capacity := 1;
                hourday := Facility.Town.WorldLocator.GetHoursADay;
                if hourday <> 0
                  then dtCorrection := realmin(5, 24/hourday)
                  else dtCorrection := 1;
                capacity  := dtCorrection*realmin(capacity, fDirCoverage/100);
                fProgress := fProgress + capacity*dt; // >> You can pay more than what you are suppose to...
                ToPay := fCurrResearch.fInvention.Price*capacity*dt/fCurrResearch.fInvention.Time;
                BlockGenMoney( -ToPay, accIdx_ResearchCenter_Research );
                if fProgress >= fCurrResearch.fInvention.Time
                  then
                    begin
                      extraPrg := fProgress - fCurrResearch.fInvention.Time;
                      BlockGenMoney(fCurrResearch.fInvention.Price*extraPrg/fCurrResearch.fInvention.Time, accIdx_ResearchCenter_Research);
                      Facility.Company.DeclareInvention(fCurrResearch.fInvention);
                      StartResearch;
                      ModelServerCache.UnCacheObject(self, noKind, noInfo); // Facility.UpdateCache(false); // >>
                      Facility.StatusChanged(fchStructure);
                    end;
              end
            else
              begin
                StartResearch;
                if fCurrResearch <> nil
                  then Facility.UpdateCache(true);
              end;                                               
    end;

  function TResearchCenter.GetUpgradeCost : TMoney;
    begin
      if (Facility <> nil) and (Facility.Company <> nil)
        then
          begin
            if fCompanyDirection = nil
              then fCompanyDirection := Facility.Company.Directions[TMetaResearchCenter(MetaBlock).InventionKind];
            if fCompanyDirection.Support < 1.5
              then
                begin
                  result := inherited GetUpgradeCost;
                  result := sqr(sqr(UpgradeLevel))*result;
                end
              else result := -1;
          end
        else result := inherited GetUpgradeCost;
    end;                                                                           

  function TResearchCenter.GetActualMaxUpgrade : integer;
    begin
      if (Facility <> nil) and (Facility.Company <> nil)
        then
          begin
            if fCompanyDirection = nil
              then fCompanyDirection := Facility.Company.Directions[TMetaResearchCenter(MetaBlock).InventionKind];
            if (fCompanyDirection.Support < 1.5) and (WorkForceRatioAvg > 0.90)
              then result := UpgradeLevel + 1
              else result := UpgradeLevel;
          end
        else result := inherited GetActualMaxUpgrade;
    end;
    
  function TResearchCenter.GetVisualClassId : TVisualClassId;
    begin
      result := min( UpgradeLevel - 1, MetaBlock.VisualStages - 1 );
    end;

  procedure TResearchCenter.AutoConnect( loaded : boolean );
    var
      kind    : TPeopleKind;
      WFInput : TInput;
      MB      : TMetaResearchCenter;
      MinWF   : TFluidValue;
    begin
      inherited;
      if not loaded
        then
          begin
            MB := TMetaResearchCenter(MetaBlock);
            for kind := low(kind) to high(kind) do
              begin
                WFInput := InputsByName[PeopleKindPrefix[kind] + tidGate_WorkForceIn];
                if WFInput <> nil
                  then
                    begin
                      MinWF := MB.MinimalWorkForce[kind];
                      WFInput.ActualMaxFluid.Q := MinWF;
                      WorkersMax[kind].Q := MinWF;
                    end;
              end;
          end;
    end;

  function TResearchCenter.GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string;
    begin
      result := inherited GetStatusText( kind, ToTycoon );
      case kind of
        sttMain :
          if fCurrResearch <> nil
            then
              result :=
                result +
                SimHints.GetHintText( mtidResearchMain.Values[ToTycoon.Language], [round(100*fProgress/fCurrResearch.fInvention.Time)] );
                {
                IntToStr(round(100*fProgress/fCurrResearch.fInvention.Time)) +
                '% research completed.';
                }
        sttSecondary :
          begin
            if fCurrResearch <> nil
              then                      
                result :=
                  result +
                  SimHints.GetHintText( mtidResearchSec.Values[ToTycoon.Language], [fCurrResearch.fInvention.Name, FormatMoney(fCurrResearch.fInvention.Price)] );
                  {
                  'Researching "' + fCurrResearch.fInvention.Name + '". ' +
                  'Total cost: ' + IntToStr(round(fCurrResearch.fInvention.Price));
                  }
            result := result + ' ' +
              SimHints.GetHintText( mtidCompSupported.Values[ToTycoon.Language], [fDirCoverage] );
              // ' Company supported at ' + IntToStr(fDirCoverage) + '%.';
          end;
        sttHint :
          case Facility.AccessLevelOf( ToTycoon ) of
            acsFull, acsModerate :
              if not Facility.CriticalTrouble
                then
                  if not Facility.HasTechnology
                    then result := GetHintText(mtidEvalBlockNeedTechnology.Values[ToTycoon.Language], [Facility.MetaFacility.Technology.Name])
                    else
                      if Facility.Trouble and facNeedCompSupport <> 0
                        then result := GetHintText( mtidNeedsCompSupport.Values[ToTycoon.Language], [0] )
                        else
                          if fCurrResearch <> nil
                            then
                              if 100*fProgress/fCurrResearch.fInvention.Time < 5
                                then result := GetHintText( mtidGeneralHQResearch.Values[ToTycoon.Language], [0] )
                                else result := GetHintText( mtidHQResearch.Values[ToTycoon.Language], [ToTycoon.Name] )
                            else result := GetHintText( mtidHQIdle.Values[ToTycoon.Language], [0] )
                else GetHintText(mtidVisitWebSite.Values[ToTycoon.Language], [0]);
          end;
      end
    end;

  procedure TResearchCenter.BlockLoaded;
    begin
      inherited;
      fCompanyDirection := Facility.Company.Directions[TMetaResearchCenter(MetaBlock).InventionKind]; // >> Optimizable
    end;

  procedure TResearchCenter.StoreToCache( Cache : TObjectCache );
    var
      avlCount  : array[0..10] of integer;
      devCount  : array[0..10] of integer;
      hasCount  : array[0..10] of integer;
      cat       : integer;
      catStr    : string;
      maxCat    : integer;
      Invention : TInvention;
      InvRec    : TInventionRecord;
      MetaRC    : TMetaResearchCenter;
      i         : integer;
      iStr      : string;
    begin
      inherited;
      FillChar(avlCount, sizeof(avlCount), 0); //avlCount  := 0;
      FillChar(devCount, sizeof(devCount), 0); //devCount  := 0;
      FillChar(hasCount, sizeof(hasCount), 0); ///hasCount  := 0;
      MetaRC := TMetaResearchCenter(MetaBlock);
      Cache.WriteString('RsKind', MetaRC.fInventionKind);
      maxCat := 0;
      for i := 0 to pred(MetaRC.InventionCount) do
        begin
          Invention := MetaRC.Inventions[i];
          cat    := Invention.CatPos;
          catStr := IntToStr(cat);
          maxCat := max(maxCat, cat);
          if (fCurrResearch <> nil) and ((Invention = fCurrResearch.fInvention) or (ResearchById[Invention.NumId] <> nil))
            then
              begin
                Invention.StoreToCache(Cache, 'dev' + catStr, devCount[cat]);
                inc(devCount[cat]);
              end
            else
              if Facility.Company.HasInvention[Invention.NumId]
                then
                  begin
                    Invention.StoreToCache(Cache, 'has' + catStr, hasCount[cat]);
                    InvRec := Facility.Company.FindInventionRecord(Invention);
                    if InvRec <> nil
                      then Cache.WriteString('has' + catStr + 'RsCost' + IntToStr(hasCount[cat]), FormatMoney(InvRec.TotalCost - InvRec.Subsidy));
                    inc(hasCount[cat]);
                  end
                else
                  begin
                    Invention.StoreToCache(Cache, 'avl' + catStr, avlCount[cat]);
                    Cache.WriteBoolean('avl' + catStr + 'RsEnabled' + IntToStr(avlCount[cat]), Invention.Enabled(Facility.Company));
                    inc(avlCount[cat]);
                  end;
        end;
      Cache.WriteInteger('CatCount', maxCat);
      for i := 0 to maxCat do
        begin
          iStr := IntToStr(i);
          Cache.WriteInteger('devCount' + iStr, devCount[i]);
          Cache.WriteInteger('hasCount' + iStr, hasCount[i]);
          Cache.WriteInteger('avlCount' + iStr, avlCount[i]);
        end;
    end;

  procedure TResearchCenter.LoadFromBackup( Reader : IBackupReader );
    var
      InventionId : string;
      i           : integer;
      theId       : string;
      prior       : integer;
    begin
      inherited;
      fDevResearches := TCollection.Create(0, rkBelonguer);
      if Reader.ReadBoolean('IsRes', false)
        then
          begin
            InventionId := Reader.ReadString('curResId', '');
            RestoreResearch(InventionId, 0);
            for i := 0 to pred(Reader.ReadInteger('rsCount', 0)) do
              begin
                theId := Reader.ReadString('Id' + IntToStr(i), '');
                prior := Reader.ReadByte('Pr' + IntToStr(i), 0);
                RestoreResearch(theId, prior);
              end;
          end;
      fProgress := Reader.ReadSingle('Progress', 0);
    end;

  procedure TResearchCenter.StoreToBackup( Writer : IBackupWriter );
    var
      i   : integer;
      aux : string;
    begin
      inherited;
      Writer.WriteBoolean('IsRes', fCurrResearch <> nil);
      if fCurrResearch <> nil
        then
          begin
            Writer.WriteString('curResId', fCurrResearch.fInvention.Id);
            Writer.WriteInteger('rsCount', fDevResearches.Count);
            for i := 0 to pred(fDevResearches.Count) do
              with Researches[i] do
                begin
                  aux := 'Id' + IntToStr(i);
                  Writer.WriteString(aux, fInvention.Id);
                  aux := 'Pr' + IntToStr(i);
                  Writer.WriteByte(aux, fPriority);
                end;
          end;
      Writer.WriteSingle('Progress', fProgress);
      aux := '';
    end;

  function TResearchCenter.GetResearch(index : integer) : TDevelopingResearch;
    begin
      result := TDevelopingResearch(fDevResearches[index]);
    end;

  function TResearchCenter.GetResearchById( InventionId : TInventionNumId ) : TDevelopingResearch;
    var
      i : integer;
    begin
      Facility.Lock;
      try
        i := pred(fDevResearches.Count);
        while (i >= 0) and (Researches[i].fInvention.NumId <> InventionId) do
          dec(i);
        if i >= 0
          then result := Researches[i]
          else result := nil;
      finally
        Facility.Unlock;
      end;
    end;

  procedure TResearchCenter.CheckResearches;
    var
      i : integer;
    begin
      Facility.Lock;
      try
        for i := pred(fDevResearches.Count) downto 0 do
          begin
            if not Researches[i].fInvention.Enabled(Facility.Company)
              then fDevResearches.AtDelete(i);
          end;
        if (fCurrResearch <> nil) and not fCurrResearch.fInvention.Enabled(Facility.Company)
          then
            begin
              BlockGenMoney((fProgress/100)*fCurrResearch.fInvention.Price, accIdx_ResearchCenter_Research); // accIdx_Compensations
              StartResearch;
            end;
      finally
        Facility.Unlock;
      end;
    end;

  function TResearchCenter.RenderCloneMenu(lang : string) : string;
    var
      aux : string;
    begin
      aux := mtidResCenterCloneMenu.Values[lang];
      result := inherited RenderCloneMenu(lang);
      if aux <> ''
        then result := result + Format(aux, [cloneOption_Suppliers]); // FIX MLS 'Suppliers|%d|'
    end;

  // RegisterBackup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass( TResearchCenter );
    end;


end.



