unit EvaluatedBlock;

interface

  uses
    ClassStorageInt, Protocol, Kernel, WorkCenterBlock, Surfaces, OutputEvaluators, Accounts,
    BackupInterfaces, Inventions, Languages, Variants;

  const
    msgBlockOverloaded     = 10000;
    msgBlockOperationRatio = 10001;

  const
    tidIndustriesInvention = 'Industries';

  type
    TBlockOverloadedMsg =
      record
        Cmd    : word;
        Gate   : TMetaGate;
        Perc   : TPercent;
        Result : boolean;
      end;

    TMetaEvaluatedBlock =
      class(TMetaWorkCenter)
        public
          constructor Create(anId                : string;
                             aCapacities         : array of TFluidValue;
                             aSupplyAccount      : TAccountId;
                             aProdAccount        : TAccountId;
                             aSalaryAccount      : TAccountId;
                             aMantainanceAccount : TAccountId;
                             aBlockClass         : CBlock);
          destructor Destroy; override;
        private
          fMantainanceAccount : TAccountId;
          fMetaEvaluatorPool  : TMetaEvaluatorPool;
          fDesignProfPerc     : TPercent;
          fNetProfit          : TMoney;
          fCargoKind          : TCargoKind;
          fWeatherEnvelope    : integer;
          fLimitWeatherCond   : single;
        private
          function GetNetProfit : TMoney;
          function GetOperationCost : TMoney;
        protected
          property MetaEvaluatorPool  : TMetaEvaluatorPool read fMetaEvaluatorPool;
          property DesignProfPerc     : TPercent           read fDesignProfPerc write fDesignProfPerc;
          property NetProfit          : TMoney             read GetNetProfit;
          property OperationCost      : TMoney             read GetOperationCost;
          property WeatherEnvelope    : integer            read fWeatherEnvelope  write fWeatherEnvelope;
          property LimitWeatherCond   : single             read fLimitWeatherCond write fLimitWeatherCond;
        private
          function  GetDesignEffic : single;
          procedure SetDesignEffic(value : single);
          function  GetDesignK : TPercent;
          procedure SetDesignK (value : TPercent);
        public
          property MantainanceAccount : TAccountId read fMantainanceAccount;
          property CargoKind          : TCargoKind read fCargoKind write fCargoKind;
          property DesignEffic        : single     read GetDesignEffic write SetDesignEffic;
          property DesignK            : TPercent   read GetDesignK write SetDesignK;
        public
          procedure Register( ClassFamily : TClassFamilyId );
          procedure EvaluateTexts; override;
      end;

    TEvaluatedBlock =
      class(TFinanciatedWorkCenter)
        protected
          constructor Create(aMetaBlock : TMetaBlock; aFacility : TFacility); override;
        public
          destructor Destroy; override;
        protected
          function Evaluate : TEvaluationResult; override;
        public
          procedure AutoConnect( loaded : boolean ); override;
        private
          fEvaluatorPool  : TEvaluatorPool;
          fBeautyModifier : TSurfaceModifier;
        protected
          function GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string; override;
        public
          procedure LoadFromBackup(Reader : IBackupReader); override;
          procedure StoreToBackup (Writer : IBackupWriter); override;
          procedure Stop;                                   override;
          procedure CopySettingsFrom(Block : TBlock; Options : integer); override;
          function  RenderCloneMenu(lang : string) : string; override;
        private
          procedure msgOverloaded(var Msg : TBlockOverloadedMsg); message msgBlockOverloaded;
          procedure msgGetOperationRatio(var Msg : TBlockOverloadedMsg); message msgBlockOperationRatio;
        protected
          function Producing : boolean;
          function GetVisualClassId  : TVisualClassId; override;
        public
          property EvaluatorPool  : TEvaluatorPool   read fEvaluatorPool;
          property BeautyModifier : TSurfaceModifier read fBeautyModifier;
        public
          procedure GetInventionsEffect(var QEffect : TInventionEfficiencyEffect; var KEffect : integer; DsgnK : TPercent);
          function  GetWeatherConditions : single;
          function  AdverseWeatherConditions : boolean;
        protected
          procedure Deleted; override;
        protected
          procedure RecalculateInventionsEffect; override;
      end;

    TIndustryInvention =
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

implementation

  uses
    StdFluids, ClassStorage, PyramidalModifier, Classes, SysUtils,
    SimHints, MathUtils, Collection, MetaInstances, Logs, CloneOptions;


  // TMetaEvaluatedBlock

  constructor TMetaEvaluatedBlock.Create(anId                : string;
                                         aCapacities         : array of TFluidValue;
                                         aSupplyAccount      : TAccountId;
                                         aProdAccount        : TAccountId;
                                         aSalaryAccount      : TAccountId;
                                         aMantainanceAccount : TAccountId;
                                         aBlockClass         : CBlock);
    begin
      inherited Create(anId, aCapacities, aSupplyAccount, aProdAccount, aSalaryAccount, aBlockClass);
      fMetaEvaluatorPool  := TMetaEvaluatorPool.Create;
      fMantainanceAccount := aMantainanceAccount;
      fDesignProfPerc     := 45; // If operation ratio is over this % and having Input, Effic, Price = 100% it produces incomes > 0
      HasPeopleCargo      := true;
      CargoKind           := carLight;
      MinCompSup          := 0.75;
      fWeatherEnvelope    := noIndex;
      fLimitWeatherCond   := 0;
      MaxUpgrade          := 100;
    end;

  destructor TMetaEvaluatedBlock.Destroy;
    begin
      fMetaEvaluatorPool.Free;
      inherited;
    end;

  function TMetaEvaluatedBlock.GetNetProfit : TMoney;

    function GetInputCost : TMoney;
      var
        i         : integer;
        MetaInput : TMetaInput;
      begin
        result := 0;
        for i := 0 to pred(MetaInputs.Count) do
          begin
            MetaInput := TMetaInput(MetaInputs[i]);
            if not (mfWorkForce in MetaInput.MetaFluid.Options)
              then result := result + MetaInput.MetaFluid.MarketPrice*MetaInput.MaxFluid.Q;
          end;
      end;

    var
      nPrf : TMoney;
      iPrf : TMoney;
      wPrf : TMoney;

    begin
      if fNetProfit = 0
        then
          begin
            nPrf := fMetaEvaluatorPool.NetProfit;
            iPrf := GetInputCost;
            wPrf := WorkForceCost;
            fNetProfit := nPrf - iPrf - wPrf;
          end;
      result := fNetProfit;
    end;

  function TMetaEvaluatedBlock.GetOperationCost : TMoney;
    begin
      result := (fDesignProfPerc/100)*NetProfit;
    end;

  function TMetaEvaluatedBlock.GetDesignEffic : single;
    begin
      result := MetaEvaluatorPool[0].DesignEffic;
    end;

  procedure TMetaEvaluatedBlock.SetDesignEffic(value : single);
    var
      i : integer;
    begin
      for i := 0 to pred(MetaEvaluatorPool.EvaluatorCount) do
        MetaEvaluatorPool[i].DesignEffic := value;
    end;

  function TMetaEvaluatedBlock.GetDesignK : TPercent;
    begin
      result := MetaEvaluatorPool[0].DesignMaxK;
    end;

  procedure TMetaEvaluatedBlock.SetDesignK(value : TPercent);
    var
      i : integer;
    begin
      for i := 0 to pred(MetaEvaluatorPool.EvaluatorCount) do
        MetaEvaluatorPool[i].DesignMaxK := value;
    end;

  procedure TMetaEvaluatedBlock.Register( ClassFamily : TClassFamilyId );
    begin
      MetaEvaluatorPool.EnlargeInputs(EnlargeFactor);
      inherited Register( ClassFamily );
    end;

  procedure TMetaEvaluatedBlock.EvaluateTexts;
    var
      i, j, k : integer;
      Outp    : string;
      Inp     : string;
      Inputs  : TStringList;                              
      lang    : string;
      Desc    : string;
    begin
      for k := 0 to pred(LangList.Count) do
        begin
          lang    := LangList[k];
          Outp    := SimHints.GetHintText( mtidDescFactoryHead.Values[lang], [0] ) + ' ';
          Inputs  := TStringList.Create;
          Desc    := '';
          for i := 0 to pred(MetaEvaluatorPool.EvaluatorCount) do
            with MetaEvaluatorPool.MetaEvaluators[i] do
              begin
                Outp := Outp + SimHints.GetHintText( mtidDescFactoryHeadN.Values[lang], [MetaOutput.MetaFluid.FormatValue( MetaOutput.MaxFluid.Q, lang ), lowercase(MetaOutput.MetaFluid.Name_MLS.Values[lang])] );
                if i < pred(MetaEvaluatorPool.EvaluatorCount)
                  then Outp := Outp + ', ';
                for j := 0 to pred(InputInfoList.Count) do
                  with TMetaInputInfo(InputInfoList[j]) do
                    if (pos( MetaInput.MetaFluid.Name_MLS.Values[lang], Inp ) = 0) and (InputKind = ikBasic)
                      then Inputs.Add( SimHints.GetHintText( mtidDescFactoryReqN.Values[lang], [lowercase(MetaInput.MetaFluid.Name_MLS.Values[lang])] ));
              end;
          if Desc <> ''
            then Desc := Desc + ' ';
          if Inputs.Count > 0
            then
              begin
                Inp := SimHints.GetHintText( mtidDescFactoryReq.Values[lang], [0] ) + ' ';
                for i := 0 to pred(Inputs.Count) do
                  begin
                    Inp := Inp + Inputs[i];
                    if i < Inputs.Count - 1
                      then
                        if i < Inputs.Count - 2
                          then Inp := Inp + ', '                     
                          else Inp := Inp + ' ' + mtidAND.Values[lang] + ' ';  // >> MLS2
                  end;
                Desc := Outp + '. ' + Inp + '.'
              end
            else Desc := Outp + '.';                                    
          Inputs.Free;
          if Desc_MLS.Values[lang] <> ''
            then Desc_MLS.Values[lang] := Desc_MLS.Values[lang] + ' ';
          Desc_MLS.Values[lang] := Desc_MLS.Values[lang] + Desc;
        end;
    end; 


  // TEvaluatedBlock

  constructor TEvaluatedBlock.Create(aMetaBlock : TMetaBlock; aFacility : TFacility);
    begin
      inherited;
      fEvaluatorPool := TMetaEvaluatedBlock(aMetaBlock).fMetaEvaluatorPool.Instantiate(self);
    end;

  destructor TEvaluatedBlock.Destroy;
    begin                                               
      fEvaluatorPool.Free;
      fBeautyModifier.Delete;
      inherited;
    end;

  function TEvaluatedBlock.Evaluate : TEvaluationResult;
    var
      WFEffic  : single;
      DsgnProf : TPercent;
      OprRatio : TPercent;
      DsgnCost : TMoney;
      WthrCond : single;
    begin
      result := inherited Evaluate;
      if not Facility.CriticalTrouble
        then
          try
            // Workforce Effic
            if Facility.Trouble and facNeedsWorkForce <> 0
              then WFEffic := 0
              else WFEffic := WorkForceEfficiency;
            // Weather conditions
            WthrCond := GetWeatherConditions;
            // Evaluate
            result := fEvaluatorPool.Evaluate(WFEffic, WthrCond <= TMetaEvaluatedBlock(MetaBlock).LimitWeatherCond);
            // Charge variable cost
            OprRatio := fEvaluatorPool.OperationRatio;
            if OprRatio > 0
              then
                begin
                  DsgnProf := TMetaEvaluatedBlock(MetaBlock).fDesignProfPerc;
                  DsgnCost := realmax(0, TMetaEvaluatedBlock(MetaBlock).OperationCost);
                  if OprRatio < DsgnProf
                    then BlockGenMoney(-DsgnCost*dt, TMetaEvaluatedBlock(MetaBlock).MantainanceAccount)
                    else BlockGenMoney(-DsgnCost*dt*(100 - OprRatio)/(100 - DsgnProf), TMetaEvaluatedBlock(MetaBlock).MantainanceAccount);
                end;
            // Hire Workforce
            if WthrCond <= TMetaEvaluatedBlock(MetaBlock).LimitWeatherCond
              then HireWorkForce(0)
              else HireWorkForce(1);
            // Set cargo values
            SetCargoValue( TMetaEvaluatedBlock(MetaBlock).CargoKind, OprRatio/10 );
            // Inc the complex facilities
            if Facility.CompanyDir <> nil
              then Facility.CompanyDir.ComplexFacs := Facility.CompanyDir.ComplexFacs + 1;
          except
            result := evrError;
          end
        else SetCargoValue( TMetaEvaluatedBlock(MetaBlock).CargoKind, 0 );
    end;

  procedure TEvaluatedBlock.AutoConnect( loaded : boolean );
    begin
      inherited;
      fBeautyModifier :=
        TPyramidalModifier.Create(
          tidEnvironment_Beauty,
          Point(xOrigin, yOrigin),
          MetaBlock.Beauty,
          MetaBlock.BeautyStrength );
    end;

  function TEvaluatedBlock.GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string;
    var
      OutputAbstract : string;
      k              : TPeopleKind;
      i              : integer;
    begin
      result := inherited GetStatusText( kind, ToTycoon );
      case kind of
        sttMain :
          begin
            if not Facility.CriticalTrouble
              then
                if not AdverseWeatherConditions
                  then
                    if Facility.Trouble and facNeedsWorkForce = 0
                      then
                        begin
                          OutputAbstract := '';
                          for i := 0 to pred(fEvaluatorPool.EvaluatorCount) do
                            begin
                              OutputAbstract := OutputAbstract + fEvaluatorPool.Evaluators[i].FormatOutput(Self, kind, fEvaluatorPool.EvaluatorCount > 1, ToTycoon);
                              if i < pred(fEvaluatorPool.EvaluatorCount)
                                then OutputAbstract := OutputAbstract + LineBreak;
                            end;
                          result := result + OutputAbstract;
                        end
                      else result := result +
                             SimHints.GetHintText( mtidHiringWorkForce.Values[ToTycoon.Language], [round(100*WorkForceEfficiency)] );
                             //'Hiring workforce at ' + IntToStr(round(100*WorkForceEfficiency)) + '%.';
          end;
        sttSecondary :
          begin
            if not Facility.CriticalTrouble // (Facility.Trouble = facNoTrouble) or (Facility.Trouble = facInsuficientInput)
              then
                if AdverseWeatherConditions
                  then result := SimHints.GetHintText( mtidStoppedDueWeather.Values[ToTycoon.Language], [0] )//'Stopped because of weather conditions.'
                  else
                    if Facility.Trouble and facNeedsWorkForce = 0
                      then
                        begin
                          OutputAbstract := fEvaluatorPool.GetSecondaryTextHeader(ToTycoon.Language);
                          for i := 0 to pred(fEvaluatorPool.EvaluatorCount) do
                            begin
                              OutputAbstract :=
                                OutputAbstract +
                                fEvaluatorPool.Evaluators[i].FormatOutput(Self, kind, true, ToTycoon) + '. ';
                            end;
                          result := result + OutputAbstract;
                        end
                      else
                        if not AdverseWeatherConditions
                          then
                            with TMetaWorkCenter(MetaBlock) do
                              for k := low(k) to high(k) do
                                if Capacity[k] > 0
                                  then
                                    begin
                                      if WorkersMax[k].Q > 0
                                        then
                                          begin
                                            result := result +
                                              SimHints.GetHintText(
                                                mtidHiringWorkForceSec.Values[ToTycoon.Language],
                                                [
                                                mtidWorkforceKindName[k].Values[ToTycoon.Language],
                                                round(Workers[k].Q),
                                                round(WorkersMax[k].Q)
                                                ] );
                                          end;
                                    end;
            result := Format(mtidUpgradeLevel.Values[ToTycoon.Language], [UpgradeLevel]) + '  ' + result;
          end;
        sttHint :
          if not Facility.CriticalTrouble
            then
              case Facility.AccessLevelOf( ToTycoon ) of
                acsFull, acsModerate :
                  begin
                    if not Facility.HasTechnology
                      then result := GetHintText(mtidEvalBlockNeedTechnology.Values[ToTycoon.Language], [Facility.MetaFacility.Technology.Name])
                      else
                        if AdverseWeatherConditions
                          then result := GetHintText(mtidEvalBlockBadWeatherCond.Values[ToTycoon.Language], [0])
                          else
                            if Facility.Trouble and facNeedCompSupport <> 0
                              then result := GetHintText(mtidNeedsCompSupport.Values[ToTycoon.Language], [0])
                              else
                                if Facility.Trouble and facNeedsWorkForce <> 0
                                  then result := GetHintText(mtidBlockNeedsWorkForce.Values[ToTycoon.Language], [mtidPeopleKindName[WFRequired].Values[ToTycoon.Language]])
                                  else result := fEvaluatorPool.GetHint(ToTycoon);
                  end;
              end;
      end;
    end;

  procedure TEvaluatedBlock.LoadFromBackup(Reader : IBackupReader);
    begin
      inherited;
      fEvaluatorPool := TMetaEvaluatedBlock(MetaBlock).fMetaEvaluatorPool.Instantiate(self);
      fEvaluatorPool.LoadFromBackup(Reader);
    end;

  procedure TEvaluatedBlock.StoreToBackup(Writer : IBackupWriter);
    begin
      inherited;
      fEvaluatorPool.StoreToBackup(Writer);
    end;

  procedure TEvaluatedBlock.Stop;
    var
      i : integer;
    begin
      inherited;
      for i := 0 to pred(fEvaluatorPool.EvaluatorCount) do
        with fEvaluatorPool.Evaluators[i] do
          begin
            BiasRatio := 0;
            SupplyRatio := 0;
            DonotProduce(Self);
          end;
    end;

  procedure TEvaluatedBlock.CopySettingsFrom(Block : TBlock; Options : integer); 
    var
      i      : integer;
      inpIdx : integer;
    begin
      inherited;
      if ObjectIs(ClassName, Block)
        then
          try
            if (Block.Facility.MetaFacility.FacId = Facility.MetaFacility.FacId) and (Options and cloneOption_OutputPrices <> 0)
              then
                for i := 0 to pred(EvaluatorPool.EvaluatorCount) do
                  begin
                    inpIdx := EvaluatorPool.Evaluators[i].MetaEvaluator.MetaOutput.Index;
                    Outputs[inpIdx].PricePerc := Block.Outputs[inpIdx].PricePerc;
                  end;
          except
            Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' Error in TEvaluatedBlock.CopySettingsFrom.');
          end;
    end;

  function TEvaluatedBlock.RenderCloneMenu(lang : string) : string;
    var
      aux : string;
    begin
      aux := mtidEvalBlockCloneMenu.Values[lang];
      result := inherited RenderCloneMenu(lang);
      if aux <> ''
        then result := result + Format(aux, [cloneOption_OutputPrices, cloneOption_Suppliers, cloneOption_Clients]); // FIX MLS 'Price|%d|Suppliers|%d|Clients|%d|'
    end;

  procedure TEvaluatedBlock.msgOverloaded(var Msg : TBlockOverloadedMsg);
    begin
      msgGetOperationRatio(Msg);
      Msg.Result := Msg.Perc < 100;
    end;

  procedure TEvaluatedBlock.msgGetOperationRatio(var Msg : TBlockOverloadedMsg);
    var
      i, cnt : integer;
    begin
      i   := 0;
      cnt := fEvaluatorPool.EvaluatorCount;
      while (i < cnt) and (fEvaluatorPool.Evaluators[i].MetaEvaluator.MetaOutput.MetaFluid <> Msg.Gate.MetaFluid) do
        inc(i);
      Msg.Perc := fEvaluatorPool.Evaluators[i].OperationRatio;
    end;

  function TEvaluatedBlock.Producing : boolean;
    var
      i, cnt : integer;
    begin
      i   := 0;
      cnt := fEvaluatorPool.EvaluatorCount;
      while (i < cnt) and (fEvaluatorPool.Evaluators[i].OperationRatio = 0) do
        inc(i);
      result := i < cnt;
    end;

  function TEvaluatedBlock.GetVisualClassId : TVisualClassId;
    begin
      case MetaBlock.VisualStages of
        2 :
          if Facility.CriticalTrouble or not Producing
            then result := 0
            else result := 1;
        else result := 0;
      end;
    end;

  procedure TEvaluatedBlock.GetInventionsEffect(var QEffect : TInventionEfficiencyEffect; var KEffect : integer; DsgnK : TPercent);
    var
      i         : integer;
      QltPts    : integer;
      QltSum    : integer;
      Effic     : integer;
      Invention : TIndustryInvention;
      DsgnPts   : single;
    begin
      QltPts := 0;
      QltSum := 0;
      Effic  := 0;
      for i := 0 to pred(MetaBlock.Inventions.Count) do
        begin
          Invention := TIndustryInvention(MetaBlock.Inventions[i]);
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
        else KEffect := 0;
      {
      if QltSum > 0
        then KEffect := min(100, round(100*Quality/QltSum))
        else KEffect := 0;
      }
      QEffect := 1 + Effic/100;
    end;

  function TEvaluatedBlock.GetWeatherConditions : single;
    begin
      with TMetaEvaluatedBlock(MetaBlock) do
        if WeatherEnvelope <> noIndex
          then result := WeatherEnvelopValue[WeatherEnvelope]
          else result := 1;
    end;

  function TEvaluatedBlock.AdverseWeatherConditions : boolean;
    begin
      result := GetWeatherConditions <= TMetaEvaluatedBlock(MetaBlock).LimitWeatherCond;
    end;

  procedure TEvaluatedBlock.Deleted;
    begin
      fBeautyModifier.Delete;
      fBeautyModifier := nil;
      inherited;
    end;

  procedure TEvaluatedBlock.RecalculateInventionsEffect;
    var
      i  : integer;
      OE : TOutputEvaluator;
    begin
      for i := 0 to pred(fEvaluatorPool.EvaluatorCount) do
        begin
          OE := fEvaluatorPool.Evaluators[i];
          OE.RecalculateInventionsEffect(self);
        end;
    end;


  // TIndustryInvention

  constructor TIndustryInvention.Load(xmlObj : OleVariant);
    var
      Aux : OleVariant;
    begin
      inherited Load(xmlObj);
      Aux      := xmlObj.children.item(tidInvElement_Props, Unassigned);
      fQuality := GetProperty(Aux, tidInvAttr_Q);
      fEffic   := GetProperty(Aux, tidInvAttr_Effic);
    end;

  function TIndustryInvention.GetClientProps(Company : TObject; LangId : TLanguageId) : string;
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
        tidIndustriesInvention,
        TInventionClass.Create(TIndustryInvention));
    end;

end.


