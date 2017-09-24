unit Tutorial;

interface

  uses
    SysUtils, Tasks, BackupInterfaces, CacheAgent;

  const
    tidTask_Tutorial             = 'Tutorial';
    tidTask_TutorialWelcome      = 'Welcome';
    tidTask_WhoYouAre            = 'WhoYouAre';
    tidTask_YourProfile          = 'YourProfile';
    tidTask_TheSpider            = 'TheSpider';

    tidTask_IndustryAndStore     = 'Industry&Store';
    tidChoiceIndustry            = 'ChoiceIndustry';
    tidTask_BuildFarm            = 'bFarm';
    tidTask_BuildFoodProc        = 'bFoodProc';
    tidTask_BuildClothesIndustry = 'bClothes';
    tidTask_BuildHHAIndustry     = 'bHousehold';
    tidTask_BuildFoodSore        = 'bFoodStore';
    tidTask_BuildClothesSore     = 'bClothesStore';
    tidTask_BuildHHASore         = 'bHHAStore';
    tidTask_GrowMoney            = 'GrowMoney';
    tidTask_TutorialFarewell     = 'Farewell';
    tidTask_CloseTutor           = 'CloseTutor';

  const
    MaxProfTurns = 50;

  type
    TTutorialTask =
      class(TSuperTask)
        public
          constructor Create(aMetaTask : TMetaTask; aSuperTask : TSuperTask; aContext : ITaskContext); override;
        private
          fActive    : boolean;
          fDone      : boolean;
          fNotTycoon : boolean;
        public
          property NotTycoon : boolean read fNotTycoon write fNotTycoon;
        public
          function  GetActive : boolean;        override;
          procedure SetActive(Value : boolean); override;
          procedure LoadFromBackup(Reader : IBackupReader); override;
          procedure StoreToBackup (Writer : IBackupWriter); override;
          procedure StoreToCache(Prefix : string; Cache : TObjectCache); override;
          function  GetTaskNumber : integer; override;
          function  GetNotTycoon : boolean; override;
          procedure SetNotTycoon(value : boolean); override;
        private
          procedure PickCompanyAndTown;
        public
          function  Execute : TTaskResult; override;
      end;

    TMetaBuildIndustryAndStore =
      class(TMetaTask)
        private
          fProdCookieName  : string;
          fProdCookieValue : string;
        public
          property ProdCookieName  : string read fProdCookieName  write fProdCookieName;
          property ProdCookieValue : string read fProdCookieValue write fProdCookieValue;
      end;

    TBuildIndustryAndStore =
      class(TTask)
        protected
          class procedure SetCompletionCookies(MetaTask : TMetaTask; SuperTask : TTask; canceled : boolean); override;
      end;

    TGrowMoneyTask =
      class(TAtomicTask)
        private
          fProfTurns : byte;
        public
          function Execute : TTaskResult; override;
      end;

  procedure RegisterTasks;
  procedure RegisterBackup;

implementation

  uses
    Kernel, Standards, Protocol, InformativeTask, ClusterTask, HeadquarterTasks,
    ResidentialTasks, BuildFacilitiesTask, BuildWarehouse, ChoiceTask, TaskUtils,
    FacIds, FoodStore, ClothesShop, HHAStore, MathUtils, CommonTasks, MakeProfitTask,
    ResearchTask, HireOfferTask, LoanMoneyTask, PGITutorial, DissidentTutorial,
    MoabTutorial, MarikoTutorial, WhoYouAre, YourProfile, TheSpider, BuyAdsTask,
    CloneAdsTask, SellAllTask, UpgradeFacTask, ManuallyConnectTask;

  // TTutorialTask

  constructor TTutorialTask.Create(aMetaTask : TMetaTask; aSuperTask : TSuperTask; aContext : ITaskContext);
    begin
      inherited;
      fActive := true;
    end;

  function TTutorialTask.GetActive : boolean;
    begin
      result := fActive;
    end;

  procedure TTutorialTask.SetActive(Value : boolean);
    begin
      fActive := Value;
    end;

  procedure TTutorialTask.LoadFromBackup(Reader : IBackupReader);
    begin
      inherited;
      fActive := Reader.ReadBoolean('Active', true);
      fDone   := Reader.ReadBoolean('Done', false);
    end;

  procedure TTutorialTask.StoreToBackup (Writer : IBackupWriter);
    begin
      inherited;
      Writer.WriteBoolean('Active', fActive);
      Writer.WriteBoolean('Done', fDone);
    end;

  procedure TTutorialTask.StoreToCache(Prefix : string; Cache : TObjectCache);
    begin
      inherited;
      Cache.WriteBoolean('Done', fDone);
    end;

  function TTutorialTask.GetTaskNumber : integer;
    begin
      result := 0;
    end;

  function TTutorialTask.GetNotTycoon : boolean;
    begin
      result := fNotTycoon;
    end;

  procedure TTutorialTask.SetNotTycoon(value : boolean);
    begin
      fNotTycoon := value;
    end;

  procedure TTutorialTask.PickCompanyAndTown;
    var
      Tycoon  : TTycoon;
      Company : TCompany;
      HqFac   : TFacility;
      i       : integer;
    begin
      Tycoon := TTycoon(Context.getContext(tcIdx_Tycoon));
      if (Tycoon <> nil) and (Tycoon.AllCompaniesCount > 0)
        then
          begin
            Company := TCompany(Context.getContext(tcIdx_Company));
            i := 0;
            while (Company = nil) and (i < Tycoon.AllCompaniesCount) do
              begin
                if (UpperCase(Tycoon.AllCompanies[i].Cluster.Id) <> 'UW')
                  then Company := Tycoon.AllCompanies[i];
                inc(i);
              end;
            if Company <> nil
              then
                begin
                  Context.setContext(tcIdx_Company, Company);
                  if TaskUtils.FindFacility(FID_MainHeadquarter, Company, HqFac)
                    then Context.setContext(tcIdx_Town, HqFac.Town);
                end;
          end;
    end;

  function TTutorialTask.Execute : TTaskResult;
    var
      Company : TCompany;
      Town    : TTown;
    begin
      if not fDone
        then
          begin
            Company := TCompany(Context.getContext(tcIdx_Company));
            Town    := TTown(Context.getContext(tcIdx_Town));
            if (Company = nil) or (Town = nil)
              then PickCompanyAndTown;
            if Company <> nil
              then result := inherited Execute
              else result := trContinue;
            fDone := result = trFinished;
          end
        else result := trFinished;
    end;


  // TBuildIndustryAndStore

  class procedure TBuildIndustryAndStore.SetCompletionCookies(MetaTask : TMetaTask; SuperTask : TTask; canceled : boolean);
    begin
      inherited;
      if SuperTask <> nil
        then SuperTask.AddCookie(TMetaBuildIndustryAndStore(MetaTask).ProdCookieName, TMetaBuildIndustryAndStore(MetaTask).ProdCookieValue);
    end;


  // TGrowMoneyTask

  function TGrowMoneyTask.Execute : TTaskResult;
    var
      Tycoon : TTycoon;
    begin
      Tycoon := TTycoon(Context.getContext(tcIdx_Tycoon));
      if Tycoon.NetProfit > 0
        then fProfTurns := max(MaxProfTurns, fProfTurns + 1)
        else fProfTurns := min(0, fProfTurns - 1);
      {
      if fProfTurns = MaxProfTurns
        then result := trFinished
        else result := trContinue;
      // >> REMOVE!!!!
      }
      result := trFinished;
    end;

  // Register Tasks

  procedure RegisterTasks;
    begin
      // Tutorial
      with TMetaTask.Create(
        tidTask_Tutorial,
        'Tutorial',
        '',   // No description
        '',   // None
        10,
        TTutorialTask) do
        begin
          Register(tidClassFamily_Tasks);
        end;

      // Tutorial Welcome
      with TMetaTask.Create(
        tidTask_TutorialWelcome,
        'Tutorial Welcome',
        '',           // No description
        tidTask_Tutorial,
        6,
        TInformativeTask) do
        begin
          NotTitle :=
            'Welcome to LEGACY Online Tutorial. In case you close this window ' +
            'you can access this tutorial page from the TUTORIAL button ' +
            'located in the PROFILE page.';
          Priority   := tprHighest;
          KindId     := 'Welcome';
          StageCount := 2;
          Register(tidClassFamily_Tasks);
        end;

      // Who You Are
      with TMetaWhoYouAreTask.Create(
        tidTask_WhoYouAre,
        'Who You Are',
        '',               // No description
        tidTask_Tutorial, // This is a Template
        1,                // Once every 4 virtual-day
        TWhoYouAreTask) do
        begin
          StageCount    := 3;
          Priority      := tprHighest - 1;
          KindId        := 'WhoYouAre';
          NotTitle      := 'Introduction to the Tutorial';
          Register(tidClassFamily_Tasks);
        end;

      // Your Profile
      with TMetaYourProfile.Create(
        tidTask_YourProfile,
        'Who You Are',
        '',               // No description
        tidTask_Tutorial, // This is a Template
        1,                // Once every 4 virtual-day
        TYourProfileTask) do
        begin
          StageCount    := 3;
          Priority      := tprHighest - 2;
          KindId        := 'YourProfile';
          NotTitle      := 'All About Your Account';
          Register(tidClassFamily_Tasks);
        end;

      // The Spider
      with TMetaYourProfile.Create(
        tidTask_TheSpider,
        'The Spider',
        '',               // No description
        tidTask_Tutorial, // This is a Template
        1,                // Once every 4 virtual-day
        TTheSpiderTask) do
        begin
          StageCount    := 3;
          Priority      := tprHighest - 3;
          KindId        := 'TheSpider';
          NotTitle      := 'World Atlas';
          Register(tidClassFamily_Tasks);
        end;

      CommonTasks.RegisterTasks;
      PGITutorial.RegisterTasks;
      DissidentTutorial.RegisterTasks;
      MoabTutorial.RegisterTasks;
      MarikoTutorial.RegisterTasks;

      // Tutorial Farewell
      with TMetaTask.Create(
        tidTask_TutorialFarewell,
        'Tutorial Farewell',
        '',           // No description
        tidTask_Tutorial,
        1,
        TInformativeTask) do
        begin
          NotTitle :=
            'Welcome to LEGACY Online Tutorial. In case you close this window ' +
            'you can access this tutorial page from the TUTORIAL button ' +
            'located in the PROFILE page.';
          Priority   := tprLow - 1;
          KindId     := 'Farewell';
          StageCount := 1;
          Register(tidClassFamily_Tasks);
        end;

      // Tutorial Farewell
      with TMetaTask.Create(
        tidTask_CloseTutor,
        'Close Tutor',
        '',           // No description
        tidTask_Tutorial,
        1,
        TInformativeTask) do
        begin
          NotTitle   := 'Closing Tutor';
          NotOptions := nopTutorial_OFF;
          Priority   := tprLow - 2;
          KindId     := 'Farewell';
          StageCount := 0;
          Register(tidClassFamily_Tasks);
        end;

    end;

  procedure RegisterBackup;
    begin
      RegisterClass(TTutorialTask);
      RegisterClass(TBuildIndustryAndStore);
      RegisterClass(TGrowMoneyTask);
      InformativeTask.RegisterBackup;
      HeadquarterTasks.RegisterBackup;
      ResidentialTasks.RegisterBackup;
      BuildFacilitiesTask.RegisterBackup;
      ChoiceTask.RegisterBackup;
      ClusterTask.RegisterBackup;
      MakeProfitTask.RegisterBackup;
      ResearchTask.RegisterBackup;
      BuildWarehouse.RegisterBackup;
      HireOfferTask.RegisterBackup;
      LoanMoneyTask.RegisterBackup;
      WhoYouAre.RegisterBackup;
      YourProfile.RegisterBackup;
      TheSpider.RegisterBackup;
      BuyAdsTask.RegisterBackup;
      CloneAdsTask.RegisterBackup;
      SellAllTask.RegisterBackup;
      UpgradeFacTask.RegisterBackup;
      ManuallyConnectTask.RegisterBackup
    end;


end.

