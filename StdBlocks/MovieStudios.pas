unit MovieStudios;

interface

  uses
    ClassStorageInt, Kernel, Population, Surfaces, BackupInterfaces, Protocol, CacheAgent,
    Accounts, WorkCenterBlock, Inventions, Languages, MediaGates;

  const
    tidInventionClass_MovieStudios = 'StudioReseach';

  const
    DefaultBudget  = 5*1000*1000; // 5 M
    DefaultTime    = 365; // days
    FilmMaxLife    = 4*365;
    MinFilmQuality = 5;

  const
    flgAutoRelease = $01;
    flgAutoProduce = $02;

  type
    TMetaMovieStudios =
      class(TMetaWorkCenter)
        public
          constructor Create(anId           : string;
                             aCapacities    : array of TFluidValue;
                             //aSupplyAccount : TAccountId;
                             //aProdAccount   : TAccountId;
                             //aSalaryAccount : TAccountId;
                             aChemMax       : TFluidValue;
                             aPlasticMax    : TFluidValue;
                             aCompServMax   : TFluidValue;
                             aLegalServMax  : TFluidValue;
                             aBlockClass    : CBlock);
      end;

    TFilmProject =
      class
        private
          fName        : string;
          fTotalBudget : TMoney;
          //fSpentBudget : TMoney;
          fPlanDays    : integer;
          fFilmDays    : integer;
          fAvgQuality  : single;
          fHours       : single;
          fAutoRelease : boolean;
          fAutoProduce : boolean;
        public
          function Done : boolean;
      end;

    TFilm =
      class
        fName        : string;
        fRelQuality  : single;
        fQuality     : single;
        fDaysOut     : integer;
        fExpositions : TFluidValue;
      end;

    TMovieStudios =
      class(TFinanciatedWorkCenter, IMediaBlock)
        public
          destructor Destroy; override;
        private
          fChemicals : TInputData;
          fPlastics  : TInputData;
          fFilms     : TOutputData;
        private
          //function  GenerateNextName : string;
          function  EvalRandomQualityFactors : integer;
          procedure RealeaseTheFilm;
        public
          procedure AutoConnect(loaded : boolean); override;
          procedure EndOfPeriod(PeriodType : TPeriodType; PeriodCount : integer); override;
          function  GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string; override;
        private
          fChemInput   : TInput;
          fPlastInput  : TInput;
          fFilmsOutput : TMediaOutput;
        protected
          function Evaluate : TEvaluationResult; override;
        public
          procedure LoadFromBackup(Reader : IBackupReader); override;
          procedure StoreToBackup (Writer : IBackupWriter); override;
        protected
          procedure Deleted; override;
        private
          fExperience  : integer;
          fFilmProject : TFilmProject;
          fReleaseFilm : TFilm;
        private
          function  StudioEfficiency(WFEffic : single) : single;
          function  StudioQuality(WFEffic : single) : single;
          procedure GetInventionsEffect(var QEffect : single; var KEffect : integer; DsgnK : TPercent);
          function  EfficiencyToBudget(Effic : single) : single;
          function  BudgetByHour : TMoney;
          procedure PlanInputs(ratio : single);
          procedure PlanCompanyInputs(ratio : single);
          function  BasicCost : TMoney;
        published
          procedure RDOLaunchMovie(theName : widestring; budget : double; months : integer; AutoInfo : word); //procedure RDOLaunchMovie(theName : widestring; budget : double; months : integer; AutoRelease : WordBool);
          procedure RDOCancelMovie(useless : integer);
          procedure RDOReleaseMovie(useless : integer);
          procedure RDOAutoProduce(value : WordBool);
        public
          procedure StoreToCache( Cache : TObjectCache ); override;
        private
          function  QueryInterface(const IID: TGUID; out Obj): hresult; stdcall;
          function  _AddRef  : integer; stdcall;
          function  _Release : integer; stdcall;
          function  getCurrentTitle : string;
          procedure reportSales(value : TFluidValue);
      end;

    TMovieStudioInvention =
      class(TWorkCenterInvention)
        public
          constructor Load(xmlObj : OleVariant); override;
        private
          fEffic    : integer;
          fQuality  : integer;
        public
          property Effic   : integer read fEffic;
          property Quality : integer read fQuality;
        public
          function GetClientProps(Company : TObject; LangId : TLanguageId) : string; override;
      end;

  procedure RegisterInventionClass;
  procedure RegisterBackup;


implementation

  uses
    MetaInstances, SysUtils, ClassStorage, MathUtils, PyramidalModifier, Classes, SimHints,
    Construction, StdAccounts, StdFluids, Logs, Standards, ModelServerCache,
    MediaNameGenerator;

  // TMetaMovieStudios

  constructor TMetaMovieStudios.Create(anId           : string;
                                            aCapacities    : array of TFluidValue;
                                            //aSupplyAccount : TAccountId;
                                            //aProdAccount   : TAccountId;
                                            //aSalaryAccount : TAccountId;
                                            aChemMax       : TFluidValue;
                                            aPlasticMax    : TFluidValue;
                                            aCompServMax   : TFluidValue;
                                            aLegalServMax  : TFluidValue;
                                            aBlockClass    : CBlock);
    var
      Sample : TMovieStudios;
    begin
      inherited Create(anId,
        aCapacities,
        accIdx_MovieStudios_Supplies,
        accIdx_MovieStudios_Products,
        accIdx_MovieStudios_Salaries,
        aBlockClass);

      Sample := nil;

      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Chemicals,
          inputZero,
          InputData(aChemMax, 100),
          InputData(0, 0),
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_Chemicals]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fChemicals),
          Sample.Offset(Sample.fChemicals)));
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Plastics,
          inputZero,
          InputData(aPlasticMax, 100),
          InputData(0, 0),
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_Plastics]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fPlastics),
          Sample.Offset(Sample.fPlastics)));

      // Company Inputs
      RegisterCompanyInput(tidFluid_CompServ, aCompServMax, false);
      RegisterCompanyInput(tidFluid_LegalServ, aLegalServMax, false);

      // Outputs
      MetaOutputs.Insert(
        TMetaOutput.Create(
          tidGate_Films,
          FluidData(qIlimited, 100),
          TMediaOutput, //TPullOutput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_Films]),
          5,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fFilms),
          Sample.Offset(Sample.fFilms)));
    end;


  // TFilmProject

  function TFilmProject.Done : boolean;
    begin
      result := fFilmDays >= fPlanDays;
    end;


  // TMovieStudios

  destructor TMovieStudios.Destroy;
    begin
      inherited;
    end;

  {function TMovieStudios.GenerateNextName : string;
    var
      p, c : integer;
      aux  : string;
      part : integer;
    begin
      aux := trim(fFilmProject.fName);
      p   := length(aux);
      c   := 0;
      while (p > 0) and (aux[p] in ['0'..'9']) do
        begin
          dec(p);
          inc(c);
        end;
      if c > 0
        then
          begin
            part := StrToInt(copy(aux, p + 1, c));
            result := copy(aux, 1, p) + IntToStr(part + 1);
          end
        else result := aux + ' 2';
    end;}

  function TMovieStudios.EvalRandomQualityFactors : integer;

    const
      DefaultTimeRisk = 10;

    function TimeRisk(months : integer) : integer;
      begin
        if months <= 12
          then result := DefaultTimeRisk + 2*(12 - months)
          else
            if months <= 18
              then result := DefaultTimeRisk
              else result := min(2, DefaultTimeRisk + 2*(months - 18));
      end;

    function BudgetFactor(budget : TMoney) : single;
      begin
        if budget > 0
          then result := DefaultBudget/budget
          else result := 1; // >> should not happen
      end;

    function RunTheRisk(value : integer) : integer;
      var
        cards : array[0..99] of integer;
        i     : integer;
        idx1  : integer;
        idx2  : integer;
        aux   : integer;
      begin
        fillchar(cards, sizeof(cards), 0);
        // risk factor 1
        for i := 0 to pred(value) do
          cards[i] := 1;
        // risk factor 2
        for i := value to min(99, value + (value div 2)) do
          cards[i] := 2;
        // put the price
        idx1 := 59; // el loco
        cards[idx1] := 3;
        // deal the cards
        for i := 1 to 500 do
          begin
            idx1 := random(100);
            idx2 := random(100);
            aux  := cards[idx1];
            cards[idx1] := cards[idx2];
            cards[idx2] := aux;
          end;
        // ta, ta, tan!
        result := cards[random(100)];
      end;

    var
      risk : integer;

    begin
      result := 0;
      risk := round(TimeRisk(fFilmProject.fPlanDays div 30)*BudgetFactor(fFilmProject.fTotalBudget));
      case RunTheRisk(risk) of
        1: result := -10; // DO'H!!
        2: result := -5;  // BOO!!
        3: result := 10;  // JACK POT
      end;
    end;

  procedure TMovieStudios.RealeaseTheFilm;
    var
      AutoInfo : word;
      newName  : string;
      lang     : integer;
    begin
      Facility.Lock;
      try
        fReleaseFilm.Free;
        fReleaseFilm := TFilm.Create;
        fReleaseFilm.fName       := fFilmProject.fName;
        fReleaseFilm.fRelQuality := fFilmProject.fAvgQuality + EvalRandomQualityFactors + realmin(5, fExperience/4);
        fReleaseFilm.fQuality    := realmin(150, fReleaseFilm.fRelQuality);
        fReleaseFilm.fDaysOut    := max(0, fFilmProject.fFilmDays - fFilmProject.fPlanDays);
        inc(fExperience);
        if fFilmProject.fAutoProduce
          then
            begin
              if fFilmProject.fAutoRelease
                then AutoInfo := flgAutoProduce or flgAutoRelease
                else AutoInfo := flgAutoProduce;
              if Facility.Company <> nil
                then Lang := StrToInt(Facility.Company.Owner.Language)
                else Lang := 0;
              newName := MediaNameGenerator.GenerateName(Lang, random(4));
              RDOLaunchMovie(newName, fFilmProject.fTotalBudget, fFilmProject.fPlanDays div 30, AutoInfo);
            end
          else
            begin
              fFilmProject.Free;
              fFilmProject := nil;
            end;
        Facility.Town.MapRefresh.RefeshFacility(Facility, fchStructure);
      finally
        Facility.Unlock;
      end;
    end;

  procedure TMovieStudios.AutoConnect(loaded : boolean);
    begin
      inherited;
      fChemInput   := InputsByName[tidFluid_Chemicals];
      fPlastInput  := InputsByName[tidFluid_Plastics];
      fFilmsOutput := TMediaOutput(OutputsByName[tidFluid_Films]);
    end;

  procedure TMovieStudios.EndOfPeriod(PeriodType : TPeriodType; PeriodCount : integer);
    var
      TotalPop  : integer;
      TimeSlope : double;
      ExpSlope  : double;
    begin
      inherited;
      case PeriodType of
        perDay :
          begin
            // On going project
            if not Facility.CriticalTrouble and (fFilmProject <> nil) and not fFilmProject.Done and Facility.HasTechnology
              then
                begin
                  inc(fFilmProject.fFilmDays);
                  if fFilmProject.Done and fFilmProject.fAutoRelease
                    then RealeaseTheFilm;
                end
              else
                if (fFilmProject <> nil) and fFilmProject.Done
                  then inc(fFilmProject.fFilmDays);
            // The film on release
            if fReleaseFilm <> nil
              then
                begin
                  // Adjust the quality
                  inc(fReleaseFilm.fDaysOut);
                  if (Facility.Town <> nil) and (Facility.Town.WorldLocator <> nil)
                    then
                      begin
                        TotalPop  := Facility.Town.WorldLocator.GetTotalPopulation*2; // People will see it twice!!
                        TimeSlope := 50*fReleaseFilm.fDaysOut/FilmMaxLife;
                        if TotalPop > 0
                          then ExpSlope := 50*fReleaseFilm.fExpositions/TotalPop
                          else ExpSlope := 0;
                        fReleaseFilm.fQuality := realmax(1, fReleaseFilm.fRelQuality - TimeSlope - ExpSlope);
                        // Drop movie if too old and too bad..
                        if (fReleaseFilm.fQuality <= MinFilmQuality) and (fReleaseFilm.fDaysOut > 2*365)
                          then
                            begin
                              fReleaseFilm.Free;
                              fReleaseFilm := nil;
                            end;
                      end;
                end;
          end;
      end
    end;

  function TMovieStudios.Evaluate : TEvaluationResult;
    var
      InvEffic : single;
      InvQlt   : integer;
      StdEffic : single;
      StdQlt   : single;
      WFEffic  : single;
      theDt    : single;
      FixedBdg : TMoney;
      Budget   : TMoney;
    begin
      result := inherited Evaluate;
      if not Facility.CriticalTrouble
        then
          begin
            theDt   := dt;
            WFEffic := WorkforceEfficiency;
            // Ongoing film
            if (fFilmProject <> nil) and not fFilmProject.Done and Facility.HasTechnology
              then
                begin
                  GetInventionsEffect(InvEffic, InvQlt, 40); // 50% of quality if no inventions done
                  StdEffic := InvEffic*StudioEfficiency(WFEffic);
                  StdQlt   := 0.4*StudioQuality(WFEffic) + 0.6*InvQlt;

                  fFilmProject.fAvgQuality := (fFilmProject.fAvgQuality*fFilmProject.fHours + StdQlt*theDt)/(fFilmProject.fHours + theDt);
                  fFilmProject.fHours      := fFilmProject.fHours + theDt;

                  FixedBdg := BasicCost;
                  Budget   := EfficiencyToBudget(StdEffic)*BudgetByHour*theDt;
                  if FixedBdg/2 < Budget
                    then BlockGenMoney(-(Budget - FixedBdg/2), accIdx_MovieStudios_Maintenance);

                  HireWorkForce(1);
                  PlanInputs(1);
                  PlanCompanyInputs(1);
                end
               else
                 begin
                   HireWorkForce(0.5);
                   PlanInputs(0);
                   PlanCompanyInputs(0.2);
                 end;

            // Released Film
            if fReleaseFilm <> nil
              then
                begin
                  fFilms.Q := qIlimited;
                  fFilms.K := min(100, SmartRound(fReleaseFilm.fQuality));
                end
              else
                begin
                  fFilms.Q := 0;
                  fFilms.K := 0;
                end;
          end
        else
          begin
            fFilms.Q := 0;
            fFilms.K := 0;
          end;
    end;

  procedure TMovieStudios.LoadFromBackup(Reader : IBackupReader);
    var
      Autoinfo : byte;
    begin
      inherited;
      fExperience := Reader.ReadInteger('Exp', 0);
      if Reader.ReadBoolean('HasPrj', false)
        then
          begin
            fFilmProject := TFilmProject.Create;
            fFilmProject.fName := Reader.ReadString('PrjName', 'Unknown');
            fFilmProject.fTotalBudget := Reader.ReadCurrency('TotalBudget', DefaultBudget);
            fFilmProject.fPlanDays := Reader.ReadInteger('PlanDays', DefaultTime);
            fFilmProject.fFilmDays := Reader.ReadInteger('FilmDays', 0);
            fFilmProject.fAvgQuality := Reader.ReadSingle('AvgQlt', 50);
            fFilmProject.fHours := Reader.ReadSingle('Hours', 0);
            Autoinfo := Reader.ReadByte('', 0); // fFilmProject.fAutoRelease := Reader.ReadBoolean('AutoRel', false);
            fFilmProject.fAutoRelease := AutoInfo and flgAutoRelease <> 0;
            fFilmProject.fAutoProduce := AutoInfo and flgAutoProduce <> 0;
          end
        else fFilmProject := nil;
      if Reader.ReadBoolean('HasRel', false)
        then
          begin
            fReleaseFilm := TFilm.Create;
            fReleaseFilm.fName := Reader.ReadString('RelName', 'Unknown');
            fReleaseFilm.fRelQuality := Reader.ReadSingle('RelQlt', 50);
            fReleaseFilm.fQuality    := fReleaseFilm.fRelQuality;
            fReleaseFilm.fDaysOut := Reader.ReadInteger('DaysOut', 0);
            fReleaseFilm.fExpositions := Reader.ReadSingle('TotExp', 0);
          end;
    end;

  procedure TMovieStudios.StoreToBackup(Writer : IBackupWriter);
    var
      AutoInfo : byte;
    begin
      inherited;
      Writer.WriteInteger('Exp', fExperience);
      Writer.WriteBoolean('HasPrj', fFilmProject <> nil);
      if fFilmProject <> nil
        then
          begin
            Writer.WriteString('PrjName', fFilmProject.fName);
            Writer.WriteCurrency('TotalBudget', fFilmProject.fTotalBudget);
            Writer.WriteInteger('PlanDays', fFilmProject.fPlanDays);
            Writer.WriteInteger('FilmDays', fFilmProject.fFilmDays);
            Writer.WriteSingle('AvgQlt', fFilmProject.fAvgQuality);
            Writer.WriteSingle('Hours', fFilmProject.fHours);
            if fFilmProject.fAutoRelease
              then AutoInfo := flgAutoRelease
              else AutoInfo := 0;
            if fFilmProject.fAutoProduce
              then AutoInfo := AutoInfo or flgAutoProduce;
            Writer.WriteByte('AutoRel', AutoInfo); //Writer.WriteBoolean('AutoRel', fFilmProject.fAutoRelease);
          end;
      Writer.WriteBoolean('HasRel', fReleaseFilm <> nil);
      if fReleaseFilm <> nil
        then
          begin
            Writer.WriteString('RelName', fReleaseFilm.fName);
            Writer.WriteSingle('RelQlt', fReleaseFilm.fRelQuality);
            Writer.WriteInteger('RelDays', fReleaseFilm.fDaysOut);
            Writer.WriteSingle('TotExp', fReleaseFilm.fExpositions);
          end;
    end;

  procedure TMovieStudios.Deleted;
    begin
      fFilmProject.Free;
      fFilmProject := nil;
      fReleaseFilm.Free;
      fFilmProject := nil;
      inherited;
    end;

  function TMovieStudios.StudioEfficiency(WFEffic : single) : single;
    var
      theDt      : single;
      PlastEffic : single;
      ChemEffic  : single;
      SofSEffic  : single;
      LegSEffic  : single;
    begin
      theDt := dt;
      // Plastics
      if fPlastInput.ActualMaxFluid.Q > 0
        then PlastEffic := fPlastInput.FluidData.Q/(theDt*fPlastInput.ActualMaxFluid.Q)
        else PlastEffic := 1;
      // Chemicals
      if fChemInput.ActualMaxFluid.Q > 0
        then ChemEffic := fChemInput.FluidData.Q/(theDt*fChemInput.ActualMaxFluid.Q)
        else ChemEffic := 1;
      // Sofware & Legals
      SofSEffic := realmin(1, CompanyInputs[0].Q/CompanyInputs[0].Max);
      LegSEffic := realmin(1, CompanyInputs[1].Q/CompanyInputs[1].Max);
      // Total
      result := realmin(1, WFEffic*(0.25*PlastEffic + 0.15*ChemEffic + 0.35*SofSEffic + 0.25*LegSEffic));
    end;

  function TMovieStudios.StudioQuality(WFEffic : single) : single;
    var
      PlastK : single;
      ChemK  : single;
      SofSK  : single;
      LegSK  : single;
    begin
      // Plastics
      if fPlastInput.ActualMaxFluid.Q > 0
        then PlastK := fPlastInput.FluidData.K
        else PlastK := 100;
      // Chemicals
      if fChemInput.ActualMaxFluid.Q > 0
        then ChemK := fChemInput.FluidData.K
        else ChemK := 100;
      // Sofware & Legals
      SofSK := CompanyInputs[0].K;
      LegSK := CompanyInputs[1].K;
      // Total
      result := WFEffic*(0.25*PlastK + 0.15*ChemK + 0.35*SofSK + 0.25*LegSK);
    end;

  procedure TMovieStudios.GetInventionsEffect(var QEffect : single; var KEffect : integer; DsgnK : TPercent);
    var
      i         : integer;
      QltPts    : integer;
      QltSum    : integer;
      Effic     : integer;
      Invention : TMovieStudioInvention;
      DsgnPts   : single;
    begin
      QltPts := 0;
      QltSum := 0;
      Effic  := 0;
      for i := 0 to pred(MetaBlock.Inventions.Count) do
        begin
          Invention := TMovieStudioInvention(MetaBlock.Inventions[i]);
          if Invention.Quality > 0
            then inc(QltSum, Invention.Quality);
          if Facility.Company.HasInvention[Invention.NumId]
            then
              begin
                QltPts := QltPts + Invention.Quality;
                Effic  := Effic  + Invention.Effic;
              end;
        end;
      DsgnPts := DsgnK*QltSum/100;
      if QltSum > 0
        then KEffect := min(150, round(100*(DsgnPts + QltPts)/QltSum))
        else KEffect := DsgnK;
      QEffect := 1 + Effic/100;
    end;

  function TMovieStudios.EfficiencyToBudget(Effic : single) : single;
    begin
      result := 2 - sqrt(realmax(0, realmin(2, Effic)));
    end;

  function TMovieStudios.BudgetByHour : TMoney;
    begin
      if fFilmProject <> nil
        then result := fFilmProject.fTotalBudget/(24*fFilmProject.fPlanDays)
        else result := 0;
    end;

  procedure TMovieStudios.PlanInputs(ratio : single);
    begin
      fChemInput.ActualMaxFluid.Q  := ratio*fChemInput.MetaInput.MaxFluid.Q;
      fPlastInput.ActualMaxFluid.Q := ratio*fPlastInput.MetaInput.MaxFluid.Q;
    end;

  procedure TMovieStudios.PlanCompanyInputs(ratio : single);
    var
      UpgLevel : single;
      i        : integer;
    begin
      UpgLevel := realmax(1, UpgradeLevel);
      for i := 0 to pred(CompanyInputCount) do
        with CompanyInputs[i]^ do
          Max := UpgLevel*ratio*TMetaCompanyInput(MetaBlock.CompanyInputs[i]).Max;
    end;

  function TMovieStudios.BasicCost : TMoney;
    var
      i    : integer;
      kind : TPeopleKind;
    begin
      result := 0;
      for i := 0 to pred(InputCount) do
        result := result + Inputs[i].LastCost;
      for kind := low(kind) to high(kind) do
        result := result + WorkForceCost(kind);
    end;

  procedure TMovieStudios.RDOLaunchMovie(theName : widestring; budget : double; months : integer; AutoInfo : word); //procedure RDOLaunchMovie(theName : widestring; budget : double; months : integer; AutoRelease : WordBool);
    var
      finalName : string;
      Lang      : integer;
    begin
      try
        Facility.Lock;
        if Facility.CheckOpAuthenticity
          then
            try
              fFilmProject.Free;
              fFilmProject := TFilmProject.Create;
              if Facility.Company <> nil
                then Lang := StrToInt(Facility.Company.Owner.Language)
                else Lang := 0;
              if trim(theName) = ''
                then finalName := MediaNameGenerator.GenerateName(Lang, random(4))
                else finalName := trim(theName);
              fFilmProject.fName := finalName;
              fFilmProject.fTotalBudget := realmax(0.5*DefaultBudget, budget);
              fFilmProject.fPlanDays := max(6, months)*30 + months div 2;
              fFilmProject.fFilmDays := 1;
              fFilmProject.fAutoRelease := AutoInfo and flgAutoRelease <> 0;
              fFilmProject.fAutoProduce := AutoInfo and flgAutoProduce <> 0;
              Facility.Town.MapRefresh.RefeshFacility(Facility, fchStructure);
            finally
              Facility.Unlock;
            end;
      except
      end;
    end;

  procedure TMovieStudios.RDOCancelMovie(useless : integer);
    begin
      try
        Facility.Lock;
        if Facility.CheckOpAuthenticity
          then
            try
              fFilmProject.Free;
              fFilmProject := nil;
              Facility.Town.MapRefresh.RefeshFacility(Facility, fchStructure);
            finally
              Facility.Unlock;
            end;
      except
      end;
    end;

  procedure TMovieStudios.RDOReleaseMovie(useless : integer);
    begin
      try
        Facility.Lock;
        try
          if Facility.CheckOpAuthenticity and (fFilmProject <> nil) and fFilmProject.Done
            then RealeaseTheFilm;
        finally
          Facility.Unlock;
        end;
      except
      end;
    end;

  procedure TMovieStudios.RDOAutoProduce(value : WordBool);
    var
      invCache : boolean;
    begin
      try
        invCache := false;
        Facility.Lock;
        try
          if Facility.CheckOpAuthenticity and (fFilmProject <> nil) and (fFilmProject.fAutoProduce <> value)
            then
              begin
                fFilmProject.fAutoProduce := value;
                invCache := true;
              end;
        finally
          Facility.Unlock;
        end;
        if invCache
          then ModelServerCache.InvalidateCache(Facility, false);
      except
      end;
    end;

  procedure TMovieStudios.StoreToCache(Cache : TObjectCache);
    begin
      inherited;
      if fFilmProject <> nil
        then
          begin
            Cache.WriteString('InProd', 'YES');
            Cache.WriteString('FilmName', fFilmProject.fName);
            Cache.WriteInteger('FilmTime', fFilmProject.fPlanDays div 30);
            Cache.WriteString('FilmBudget', FormatMoney(fFilmProject.fTotalBudget));
            if fFilmProject.fAutoRelease
              then Cache.WriteString('AutoRel', 'YES')
              else Cache.WriteString('AutoRel', 'NO');
            if fFilmProject.fAutoProduce
              then Cache.WriteString('AutoProd', 'YES')
              else Cache.WriteString('AutoProd', 'NO');
            if fFilmProject.Done
              then Cache.WriteString('FilmDone', 'YES')
              else Cache.WriteString('FilmDone', 'NO');
          end;
    end;

  function TMovieStudios.GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string;
    var
      auxPerc : byte;
    begin
      result := inherited GetStatusText(kind, ToTycoon);
      case kind of
        sttMain :
          begin
          end;
        sttSecondary :
          begin
            if fFilmProject <> nil
              then
                begin
                  auxPerc := round(100*fFilmProject.fFilmDays/fFilmProject.fPlanDays);
                  result := result + ' ' +
                    GetHintText(mtidFilmProject.Values[ToTycoon.Language],
                      [
                      fFilmProject.fName,
                      fFilmProject.fFilmDays,
                      fFilmProject.fPlanDays,
                      auxPerc
                      ]);
                end;
            if fReleaseFilm <> nil
              then
                begin
                  auxPerc := round(fReleaseFilm.fQuality);
                  result  := result + ' ' + GetHintText(mtidLicensingFilm.Values[ToTycoon.Language], [fReleaseFilm.fName, auxPerc]);
                end;
            {// this is temporary
            if fReleaseFilm <> nil
              then result := result + ' (' + IntToStr(round(fReleaseFilm.fExpositions)) + ')';}
          end;
        sttHint:
          begin
          end;
      end;
    end;

  function TMovieStudios.QueryInterface(const IID: TGUID; out Obj): hresult; stdcall;
    const
      E_NOINTERFACE = HResult($80004002);
    begin
      if GetInterface(IID, Obj)
        then Result := 0
        else Result := E_NOINTERFACE;
    end;

  function TMovieStudios._AddRef  : integer; stdcall;
    begin
      result := 1;
    end;

  function TMovieStudios._Release : integer; stdcall;
    begin
      result := 1;
    end;

  function TMovieStudios.getCurrentTitle : string;
    begin
      Facility.Lock;
      try
        if fReleaseFilm <> nil
          then result := fReleaseFilm.fName
          else result := '';
      finally
        Facility.Unlock;
      end;
    end;

  procedure TMovieStudios.reportSales(value : TFluidValue);
    begin
      Facility.Lock;
      try
        if fReleaseFilm <> nil
          then fReleaseFilm.fExpositions := fReleaseFilm.fExpositions + value;
      finally
        Facility.Unlock;
      end;
    end;


  // TMovieStudioInvention

  constructor TMovieStudioInvention.Load(xmlObj : OleVariant);
    var
      Aux : OleVariant;
    begin
      inherited Load(xmlObj);
      Aux      := xmlObj.children.item(tidInvElement_Props, Unassigned);
      fQuality := GetProperty(Aux, tidInvAttr_Q);
      fEffic   := GetProperty(Aux, tidInvAttr_Effic);
    end;

  function TMovieStudioInvention.GetClientProps(Company : TObject; LangId : TLanguageId) : string;
    begin
      result := inherited GetClientProps(Company, LangId);
      if fEffic <> 0
        then result := result + SimHints.GetHintText(mtidInvEff.Values[LangId], [FormatDelta(fEffic)]) + LineBreak;
      if Quality <> 0
        then result := result + SimHints.GetHintText(mtidInvQ.Values[LangId], [FormatDelta(Quality)]) + LineBreak;
    end;

  // RegisterInventionClass

  procedure RegisterInventionClass;
    begin
      TheClassStorage.RegisterClass(
        tidClassFamily_InvClasses,
        tidInventionClass_MovieStudios,
        TInventionClass.Create(TMovieStudioInvention));
    end;


  // RegisterBackup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass(TMovieStudios);
    end;

initialization

  Randomize;

end.
