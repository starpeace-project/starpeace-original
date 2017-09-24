unit IllegalKernel;

interface

  uses
    ShareMem, ClassStorageInt, classes, Collection, extctrls, Graphics, comobj,
    Kernel, World;

  const
    MaxOptions = 5;
    MaxRoles = 10;
    MaxWays = 2;     
    MaxCharges = 8;

  type
    TAttrValue = single;
    TCoordinate =
      record
        X   : Longint;
        Y   : Longint;
      end;

  type

    // Metainstances

    TIllegalMetaInstance = class;
    TMetaAttribute       = class;
    TMetaModifier        = class;
    TMetaMission         = class;
    TMetaRole            = class;
    TMetaHistoryItem     = class;
    TAttributeModifier   = class;
    TMissionRequirement  = class;
    //TRoleRequirement     = class;

    // Instances

    TAttribute     = class;
    TModifier      = class;
    TMission       = class;
    TRole          = class;
    THistoryItem   = class;
    TCriminal      = class;
    TTeam          = class;
    TLeader        = class;
    TIllegalSystem = class;

    // Metaclasses

    CAttribute     = class of TAttribute;
    CModifier      = class of TModifier;
    CMission       = class of TMission;
    CRole          = class of TRole;
    CHistoryItem   = class of THistoryItem;
    CCriminal      = class of TCriminal;
    CTeam          = class of TTeam;
    CLeader        = class of TLeader;
    CIllegalSystem = class of TIllegalSystem;

    //MetaIstances Declarations

    TIllegalMetaInstance =
      class
        public
          constructor Create( anId : TClassId );
        private
          fId     : TClassId;
          fFamily : TClassFamilyId;
        public
          property Id     : TClassId       read fId;
          property Family : TClassFamilyId read fFamily;
        public
          procedure Register( ClassFamily : TClassFamilyId );
      end;

    TMetaAttribute =
      class( TIllegalMetaInstance )
        public
          constructor Create( anId     : TClassID;
                              aName    : widestring;
                              aDesc    : widestring;
                              aDefault : TAttrValue );
        private
          fName    : widestring;
          fDesc    : widestring;
          fDefault : TAttrValue;
        public
          property Name    : widestring read fName;
          property Desc    : widestring read fDesc;
          property Default : TAttrValue read fDefault;
      end;

      TMetaModifier =
      class( TIllegalMetaInstance )
        public
          constructor Create( anId     : TClassID;
                              aName    : widestring;
                              aDesc    : widestring );
          destructor Destroy; override;
        private
          fName      : widestring;
          fDesc      : widestring;
          fValue     : single;
          fModifiers : TCollection;
        public
          property Name      : widestring read fName;
          property Desc      : widestring read fDesc;
          property Value     : single read fValue write fValue;
          property Modifiers : TCollection read fModifiers write fModifiers;
        public
          procedure AddModifier( AttributeModifier : TAttributeModifier );
      end;

      TAttributeModifier =
      class( TIllegalMetaInstance )
        public
          constructor Create( anId          : TClassID;
                              aName         : widestring;
                              aDesc         : widestring;
                              anAttribute   : TMetaAttribute;
                              aCost         : single;
                              aTime         : LongInt;
                              aValue        : integer;
                              aRequirement  : integer;
                              aDifficulty   : integer);
        private
          fName         : widestring;
          fDesc         : widestring;
          fOwner        : TMetaModifier;
          fAttribute    : TMetaAttribute;
          fCost         : single;
          fTime         : LongInt;
          fValue        : integer;
          fDifficulty   : integer;
          fRequirement  : integer;
          fPrevModifier : TCollection;
        public
          property Name         : widestring read fName;
          property Desc         : widestring read fDesc;
          property Cost         : single read fCost;
          property Time         : LongInt read fTime;
          property Owner        : TMetaModifier read fOwner write fOwner;
          property Attribute    : TMetaAttribute read fAttribute;
          property Difficulty   : integer read fDifficulty;
          property value        : LongInt read fValue;
          property Requirement  : integer read fRequirement;
          property PrevModifier : TCollection read fPrevModifier;
        public
          procedure AddPrevModifier(PrevModifier : TAttributeModifier);
      end;

      TMetaMission =
      class( TIllegalMetaInstance )
        public
          constructor Create( anId          : TClassID;
                              aName         : widestring;
                              aDesc         : widestring;
                              aMissType     : widestring;
                              aMissionClass : CMission );
          destructor Destroy; override;
        private
          fName             : widestring;
          fDesc             : widestring;
          fMissionType      : widestring;
          fMissionClass     : CMission;
        public
          property Name : widestring read fName;
          property Desc : widestring read fDesc;
          property MissionType  : widestring read fMissionType;
          property MissionClass : CMission read fMissionClass;
        public
          function Instantiate( Owner : TTeam ) : TMission;
      end;

      TMissionRequirement =
      class( TIllegalMetaInstance )
        public
          constructor Create( anId     : TClassID;
                              aName    : widestring;
                              aDesc    : widestring );
        private
          fName         : widestring;
          fDesc         : widestring;
          fAttribute    : TMetaAttribute;
        public
          property Name      : widestring read fName;
          property Desc      : widestring read fDesc;
          property Attribute : TMetaAttribute read fAttribute;
      end;

      TMetaRole =
      class( TIllegalMetaInstance )
        public
          constructor Create( anId             : TClassID;
                              aName            : widestring;
                              aDesc            : widestring );
          destructor Destroy; override;
        private
          fName         : widestring;
          fDesc         : widestring;
          fRequirements : TCollection;
        public
          property Name         : widestring read fName;
          property Desc         : widestring read fDesc;
          property Requirements : TCollection read fRequirements;
        public
          procedure AddRequirement( RoleRequirement : TMetaAttribute );
      end;

      {TRoleRequirement =
      class( TIllegalMetaInstance )
        public
          constructor Create( anId     : TClassID;
                              aName    : widestring;
                              aDesc    : widestring );
        private
          fName         : widestring;
          fDesc         : widestring;
          fAttribute    : TMetaAttribute;
        public
          property Name      : widestring read fName;
          property Desc      : widestring read fDesc;
          property Attribute : TMetaAttribute read fAttribute;
      end;}

      TMetaHistoryItem =
      class( TIllegalMetaInstance )
        public
          constructor Create( anId         : TClassID;
                              aName        : widestring;
                              aDesc        : widestring;
                              aTypeOfOwner : widestring );
          destructor Destroy; override;
        private
          fName         : widestring;
          fDesc         : widestring;
          fTypeOfOwner  : widestring;
        public
          property Name         : widestring read fName;
          property Desc         : widestring read fDesc;
          property TypeOfOwner  : widestring read fTypeOfOwner;
      end;

      //Instances Declarations

      TAttribute     =
      class
        public
          constructor Create( aOwner : TCriminal; aMetaAttribute : TMetaAttribute; aValue : single ); virtual;
        private
          fValue          : single;
          fOwner          : TCriminal;
          fMetaAttribute  : TMetaAttribute;
          fInTraining     : boolean;
        public
          property Value        : single read fValue write fValue;
          property Owner        : TCriminal read fOwner write fOwner;
          property MetaAttribute: TMetaAttribute read fMetaAttribute write fMetaAttribute;
          property InTraining   : boolean read fInTraining write fInTraining;
      end;

      TModifier =
      class
        public
          constructor Create( aValue : single );
        private
          fValue         : single;
        public
          property Value : single read fValue;
      end;

      TMissionInfos =
        record
          Id          : widestring;
          MissionType : widestring;
          Duration    : integer;
          descr       : widestring;
        end;

      TMissionOptionInfo =
        record
          Id            : widestring;
          way           : widestring;
          Roles         : array[0..(MaxRoles - 1)] of widestring;
          Descriptions  : array[0..(MaxRoles - 1)] of widestring;
          Skills        : array[0..(MaxRoles - 1)] of widestring;
          SkillValues   : array[0..(MaxRoles - 1)] of single;
          Sub           : boolean;
          ParentOptionId: widestring;
          Objective     : widestring;
          ObjCoord      : TCoordinate;
          Duration      : integer;
          Cost          : single;
          Profit        : single;
        end;

      TCriminalCharge =
        record
          Name          : widestring;
          PreTrialTime  : integer;
          TrialTime     : integer;
          LawyersHours  : integer;
          MinBribe      : single;
          MinJailTime   : integer;
          MaxJailTime   : integer;
        end;

      TRolesInMission =
        record
          Name       : array[1..8] of widestring;
          Role       : array[1..8] of widestring;
        end;

      TParametersInMission =
        record
          Name       : array[1..(MaxOptions -1)] of widestring;
          Value      : array[1..(MaxOptions -1)] of boolean;
          Way        : array[1..(MaxOptions -1)] of widestring;
        end;

      TCrimForTask =
        record
          Name     : widestring;
          SkillVal : single;
        end;

      TNameStatusSkill =
        record
          Name : widestring;
          Status : widestring;
          Skill : single;
        end;

      TMission =
      class
        protected
          constructor Create( aOwner : TTeam; aMetaMission : TMetaMission ); virtual;
        public
          destructor Destroy; override;
        private
          fOwner        : TTeam;
          fMetaMission  : TMetaMission;
          fRoles        : TRolesInMission;
          fParameters   : TParametersInMission;
          fObjective    : TCoordinate;
          fStartingTime : integer;
          fChange       : widestring;
          fMissionTime  : integer;
          fCost         : single;
          fProfit       : single;
          fReport       : TStringList;
          fNextDiff     : single;
          fReady        : boolean;
          fCharges      : TStringList;
        public
          property Owner : TTeam read fOwner write fOwner;
          property Roles : TRolesInMission read fRoles write fRoles;
          property Parameters : TParametersInMission read fParameters write fParameters;
          property MetaMission : TMetaMission read fMetaMission write fMetaMission;
          property Objective : TCoordinate read fObjective write fObjective;
          property StartingTime : integer read fStartingTime write fStartingTime;
          property Change : widestring read fChange write fChange;
          property MissionTime : integer read fMissionTime write fMissionTime;
          property Cost : single read fCost write fCost;
          property Profit : single read fProfit write fProfit;
          property Report : TStringList read fReport write fReport;
          property NextDiff : single read fNextDiff write fNextDiff;
          property Ready : boolean read fReady write fReady;
          property Charges : TStringList read fCharges write fCharges;
        public
          class function OptionData( index : integer ) : TMissionOptionInfo; virtual;
        public
          procedure StartMission; virtual;
          procedure ProceedWithMission; virtual;
          procedure EndMission; virtual;
          function GetRightCriminalForTask(Role : widestring; skill : widestring) : TCrimForTask;
          function GetTeamMemberNameAndStatus(Name : widestring; skillName : widestring): TNameStatusSkill;
          function CheckTeamStatus : boolean;
      end;

      TRole =
      class
        public
          constructor Create( aOwner : TTeam; aMetaRole : TMetaRole); virtual;
          destructor Destroy; override;
        private
          fCriminal     : TCriminal;
          fOwner        : TTeam;
          fMetaRole     : TMetaRole;
        public
          property Criminal : TCriminal read fCriminal write fCriminal;
          property Owner : TTeam read fOwner write fOwner;
          property MetaRole : TMetaRole read fMetaRole write fMetaRole;
      end;

      THistoryItem   =
      class
        public
          //constructor Create( aTeamOwner : TTeam; aCriminalOwner : TCriminal; aLeaderOwner  : TLeader; aMetaHistoryItem : TMetaHistoryItem; aDate : TDateTime ); virtual;
          constructor Create( aTeamOwner : TTeam; aCriminalOwner : TCriminal; aLeaderOwner  : TLeader; aMetaHistoryItem : TMetaHistoryItem; aDate : integer; aEvent : widestring; aParam : widestring ); virtual;
        private
          //fDate         : TDateTime;
          fDate             : integer;
          fEvent            : widestring;
          fParam            : widestring;
          fCriminalOwner    : TCriminal;
          fTeamOwner        : TTeam;
          fLeaderOwner      : TLeader;
          fMetaHistoryItem  : TMetaHistoryItem;
        public
          //property Date        : TDateTime read fDate write fDate;
          property Date            : integer read fDate write fDate;
          property Event           : widestring read fEvent write fEvent;
          property Param           : widestring read fParam write fParam;
          property CriminalOwner   : TCriminal read fCriminalOwner write fCriminalOwner;
          property LeaderOwner     : TLeader read fLeaderOwner write fLeaderOwner;
          property TeamOwner       : TTeam read fTeamOwner write fTeamOwner;
          property MetaHistoryItem : TMetaHistoryItem read fMetaHistoryItem write fMetaHistoryItem;
      end;

      TCriminal      =
      class
        public
            constructor Create( aOwner : TIllegalSystem; aName : widestring; aPicture : widestring); virtual;
            destructor Destroy; override;
          private
            fName            : widestring;
            fAge             : integer;
            fBirthday        : integer;
            fOwner           : TIllegalSystem;
            fLeaderName      : widestring;
            fTeamName        : widestring;
            fAttributes      : TCollection;
            fHistoryItems    : TLockableCollection;
            fPicture         : widestring;
            fState           : widestring;
            fSalary          : single;
            fSalaryPerc      : integer;
            fTrainingStage   : LongInt;
            fTrainingClass   : TAttributeModifier;
            fChange          : boolean;
            fCrimPolRecord   : TStringList;
            fCrimPenRecord   : TStringList;
            fWarrant         : single;
            fJailTime        : integer;
            fTrialDay        : integer;
            fTrialLength     : integer;
            fLawyersHours    : integer;
          public
            property Picture      : widestring read fPicture write fPicture;
            property Name         : widestring read fName write fName;
            property Age          : integer read fAge write fAge;
            property Birthday     : integer read fBirthday;
            property LeaderName   : widestring read fLeaderName write fLeaderName;
            property TeamName     : widestring read fTeamName write fTeamName;
            property Attributes   : TCollection read fAttributes;
            property HistoryItems : TLockableCollection read fHistoryItems;
            property Owner        : TIllegalSystem read fOwner write fOwner;
            property State        : widestring read fState write fState;
            property Salary       : single read fSalary write fSalary;
            property SalaryPerc   : integer read fSalaryPerc write fSalaryPerc;
            property TrainingStage: LongInt read fTrainingStage write fTrainingStage;
            property TrainingClass: TAttributeModifier read fTrainingClass write fTrainingClass;
            property Change       : boolean read fChange write fChange;
            property CrimPolRecord: TStringList read fCrimPolRecord write fCrimPolRecord;
            property CrimPenRecord: TStringList read fCrimPenRecord write fCrimPenRecord;
            property Warrant      : single read fWarrant write fWarrant;
            property JailTime     : integer read fJailTime write fJailTime;
            property TrialDay     : integer read fTrialDay write fTrialDay;
            property LawyersHours : integer read fLawyersHours write fLawyersHours;
            property TrialLength  : integer read fTrialLength write fTrialLength;
          public
            procedure AddAttribute( Attribute : TAttribute );
            procedure AddHistoryItem( HistoryItem : THistoryItem );
            procedure FinishedTraining(AttClass : TAttributeModifier);
            function CalculateSalary: single;
            procedure StopTraining;
            procedure Train(Training : widestring);
            procedure Act;
            procedure CreateAttributes;
      end;

      TTeam          =
      class
        public
          constructor Create( aOwner : TLeader; aName : widestring ); virtual;
          destructor Destroy; override;
        private
          fName         : widestring;
          fOwner        : TLeader;
          fHeadquarter  : TCoordinate;
          fMission      : TMission;
          fRoles        : TLockableCollection;
          fHistoryItems : TLockableCollection;
        public
          property Name         : widestring read fName write fName;
          property Owner        : TLeader read fOwner write fOwner;
          property Headquarter  : TCoordinate read fHeadquarter write fHeadquarter;
          property Mission      : TMission read fMission write fMission;
          property Roles        : TLockableCollection read fRoles;
          property HistoryItems : TLockableCollection read fHistoryItems;
        public
          procedure AddRole( Role : TRole );
          procedure AddHistoryItem( HistoryItem : THistoryItem);
          procedure RoleChange(CriminalName : widestring; NewRoleName : widestring);
          procedure Hire(LeaderName : widestring; CriminalName : widestring; SalaryPerc : integer);
          function CalcSalary : single;
          function CalcAvPerc : single;
          function CalcTrainingCost : single;
          procedure AdjustLoyaltyAndReputation;
          procedure Act;
      end;

      TLeader        =
      class
        public
          constructor Create( aOwner : TIllegalSystem; aName : widestring; aPicture : widestring); virtual;
          destructor Destroy; override;
        private
          fOwner       : TIllegalSystem;
          fName        : widestring;
          fMoney       : single;
          fExpenses    : single;
          fReputation  : LongInt;
          fHistoryItems: TLockableCollection;
          fTeams       : TLockableCollection;
          fPicture     : widestring;
        public
          property Picture      : widestring read fPicture write fPicture;
          property Name         : widestring read fName write fName;
          property Money        : single read fMoney write fMoney;
          property Reputation   : LongInt read fReputation write fReputation;
          property Expenses     : single read fExpenses write fExpenses;
          property Owner        : TIllegalSystem read fOwner write fOwner;
          property HistoryItems : TLockableCollection read fHistoryItems;
          property Teams        : TLockableCollection read fTeams;
        public
          procedure AddTeam( Team : TTeam );
          procedure DismTeam(Team : TTeam);
          procedure AddHistoryItem( HistoryItem : THistoryItem);
          function CalcExpenses : single;
          procedure Act;
      end;

      TIllegalSystem =
      class( TWorldExtension )
        public
          constructor Create;
          destructor Destroy; override;
        private
          fCriminals        : TLockableCollection;
          fLeaders          : TLockableCollection;
        public
          Charges          : array[1..MaxCharges] of TCriminalCharge;
        public
          property Criminals    : TLockableCollection read fCriminals;
          property Leaders      : TLockableCollection read fLeaders;
        published
          function RDOGetCriminalNames: widestring;
          function RDOGetMissionReport(LeaderName: widestring; TeamName : widestring): olevariant;
          procedure RDOCreateCriminal(Name : widestring; Picture : widestring);
          procedure RDOCreateLeader(Name : widestring; Picture : widestring);
          function RDOCreateTeam(LeaderName : widestring; Name : widestring): olevariant;
          procedure RDOChangeRole(LeaderName : widestring; TeamName : widestring; CriminalName : widestring; NewRoleName : widestring);
          function RDOFindLeader(LeaderName : widestring): olevariant;
          function RDOFindTeam(LeaderName : widestring; TeamName : widestring): olevariant;
          function RDOFindCriminal(CriminalName : widestring): olevariant;
          procedure RDOHireCriminal(LeaderName : widestring; TeamName : widestring; CriminalName : widestring; SalaryPerc : integer);
          procedure RDOFireCriminal(LeaderName : widestring; TeamName : widestring; CriminalName : widestring);
          procedure RDODismissTeam(LeaderName : widestring; TeamName : widestring);
          procedure RDOChangeCriminalState(CriminalName : widestring; NewState : widestring);
          procedure RDOChangeTeam(LeaderName : widestring; OldTeamName : widestring; NewTeamName : widestring; CriminalName : widestring);
          procedure RDOCriminalShowed(CrimName : widestring);
          procedure RDOStopCriminalTraining(CriminalName : widestring);
          procedure RDOCriminalTraining(CriminalName : widestring; TrainingName : widestring);
          procedure RDOAssignHeadquarter(LeaderName : widestring; TeamName : widestring; CoordinateX : integer; CoordinateY : integer);
          function RDOCalculateSalary(LeaderName : widestring; TeamName : widestring) : olevariant;
          function RDOCalculatePercentage(LeaderName : widestring; TeamName : widestring) : olevariant;
          function RDOCalculateTrainingCost(LeaderName : widestring; TeamName : widestring) : olevariant;
          function RDOCalculateLeaderExpenses(LeaderName : widestring) : olevariant;
          procedure RDOChangeCriminalSalary(LeaderName : widestring; CriminalName : widestring; SalaryP : integer);
          function RDORecoveryHistoryItem(LeaderName : widestring; TeamName : widestring; CriminalName : widestring): olevariant;
          function RDORecoveryMissionInfos(MissionName : widestring): olevariant;
          function RDORecoveryMissionOption(MissionName : widestring; OptionIndex : integer): olevariant;
          function RDORecoveryCriminalPendingRecord(CriminalName : widestring) : olevariant;
          function RDORecoveryCriminalPoliceRecord(CriminalName : widestring) : olevariant;
          function RDORecoveryCriminalJailDetails(CriminalName : widestring) : olevariant;
          procedure RDOAssignMission(LeaderName : widestring; TeamName : widestring; MissionName : widestring; RolesList : widestring; ParametersList : widestring; ObjectiveX : integer; ObjectiveY : integer; Cost : widestring);
          procedure RDODeassignMission(LeaderName : widestring; TeamName : widestring; Cancel : widestring; MissionName : widestring);
          procedure RDOStartMission(LeaderName : widestring; TeamName : widestring; MissionName : widestring);
        public
          procedure AddCriminal( Criminal : TCriminal );
          procedure AddLeader( Leader : TLeader );
          procedure Act;
          function CriminalWrapper(Criminal : TCriminal): TStringList;
          function TeamWrapper(Team : TTeam): TStringList;
          function LeaderWrapper(Leader : TLeader): TStringList;
          function HistoryItemsWrapper(HistoryItems : TLockableCollection) : TStringList;
          procedure Start;
          procedure CreateRole(Owner : TTeam; MetaRole : TMetaRole);
          function FindLeaderInSystem(LeaderName : widestring): TLeader;
          function FindTeamInSystem(LeaderName : widestring; TeamName : widestring): TTeam;
          function FindCriminalInSystem(CriminalName : widestring): TCriminal;
          procedure ChangeLeaderMoney(LeaderName : widestring; Money : single);
          procedure GiveChargesToCriminal(CriminalName : widestring; Charges : TStringList);
          procedure SetTrialDay(CriminalName : widestring);
          procedure Trial(CriminalName : widestring; LegalHours : integer);
          procedure TrialOutcome(CriminalName : widestring);
          function UseSkill(SkillValue : single; Difficulty : single): integer;
          procedure PropagateTheBeat;
        // World Extension
        public
          function  GetId : string; override;
          procedure EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer ); override;
          procedure Loaded( aWorld : TWorld ); override;
        protected
          procedure CompanyCreated ( Company  : TCompany );  override;
          procedure TycoonCreated  ( Tycoon   : TTycoon );   override;
          procedure FacilityCreated( Facility : TFacility ); override;
          procedure CompanyDeleted ( Company  : TCompany );  override;
          procedure TycoonDeleted  ( Tycoon   : TTycoon );   override;
          procedure FacilityDeleted( Facility : TFacility ); override;
      end;

  var
    ClassId    : TClassID;
    IllSystem  : TIllegalSystem;
    Time       : integer;

  const
    tidRDOHook_IB = 'IB';

implementation

  uses
   ClassStorage, Forms, Windows, SysUtils;

    //MetaIstances Procedures

    constructor TIllegalMetaInstance.Create(anID : TClassID);
      begin
        fId := anId;
      end;

    procedure TIllegalMetaInstance.Register( ClassFamily : TClassFamilyId );
      begin
        fFamily := ClassFamily;
        TheClassStorage.RegisterClass( fFamily, fId, self );
      end;

    constructor TMetaAttribute.Create( anId     : TClassID; aName    : widestring; aDesc    : widestring; aDefault : TAttrValue );
      begin
        inherited Create(anID);
        anID := ClassID;
        fName := aName;
        fDesc := aDesc;
        fDefault := aDefault;
      end;

    constructor TMetaModifier.Create;
      begin
        inherited Create(anID);
        fName := aName;
        fDesc := aDesc;
        fModifiers := TLockableCollection.Create(0, rkBelonguer);
      end;

    destructor TMetaModifier.Destroy;
      begin
        fModifiers.Destroy;
      end;

    procedure TMetaModifier.AddModifier( AttributeModifier : TAttributeModifier );
      begin
        fModifiers.Insert( AttributeModifier );
      end;

    constructor TMetaMission.Create( anId          : TClassID;
                                     aName         : widestring;
                                     aDesc         : widestring;
                                     aMissType     : widestring;
                                     aMissionClass : CMission );
      begin
        inherited Create( anId );
        fName            := aName;
        fDesc            := aDesc;
        fMissionType     := aMissType;
        fMissionClass    := aMissionClass;
      end;

    destructor TMetaMission.Destroy;
      begin
        inherited Destroy;;
      end;

    function TMetaMission.Instantiate( Owner : TTeam ) : TMission;
      begin
        result := fMissionClass.Create( Owner, self );
      end;

    constructor TMetaRole.Create;
      begin
        inherited Create(anID);
        fName := aName;
        fDesc := aDesc;
        fRequirements := TCollection.Create(0, rkBelonguer);
      end;

    destructor TMetaRole.Destroy;
      begin
        fRequirements.Destroy;
      end;

    procedure TMetaRole.AddRequirement( RoleRequirement : TMetaAttribute );
      begin
        fRequirements.Insert ( RoleRequirement );
      end;

    constructor TMetaHistoryItem.Create;
      begin
        inherited Create(anID);
        fName := aName;
        fDesc := aDesc;
        fTypeOfOwner := aTypeOfOwner;
      end;

    destructor TMetaHistoryItem.Destroy;
      begin
        inherited;
      end;

    constructor TAttributeModifier.Create;
      begin
        inherited Create(anID);
        fName := aName;
        fDesc := aDesc;
        fOwner := TMetaModifier(TheClassStorage.ClassById['Modifier', 'Modifier1:SkillsModifier']);
        fAttribute := anAttribute;
        fCost := aCost;
        fValue := aValue;
        fDifficulty := aDifficulty;
        fTime := aTime;
        fRequirement := aRequirement;
        fPrevModifier := TLockableCollection.Create(0, rkBelonguer);
        TMetaModifier(TheClassStorage.ClassById['Modifier', 'Modifier1:SkillsModifier']).AddModifier(self);
      end;

    procedure TAttributeModifier.AddPrevModifier(PrevModifier : TAttributeModifier);
      begin
        fPrevModifier.Insert( PrevModifier );
      end;

    constructor TMissionRequirement.Create;
      begin
        inherited Create(anID);
        fName := aName;
        fDesc := aDesc;
        fAttribute := Attribute;
      end;

    {constructor TRoleRequirement.Create;
      begin
        inherited Create(anID);
        fName := aName;
        fDesc := aDesc;
        fAttribute := Attribute;
      end;}

    //Instances Procedures

    constructor TAttribute.Create;
      begin
        inherited Create;
        fOwner := aOwner;
        fValue := aValue;
        fMetaAttribute := aMetaAttribute;
        fInTraining := False;
      end;

    constructor TModifier.Create;
      begin
        inherited Create;
        fValue := aValue;
      end;

    constructor TMission.Create;
      begin
        inherited Create;
        fOwner := aOwner;
        fMetaMission := aMetaMission;
        fChange := 'no';
        fReport := TStringList.Create;
        fMissionTime := 0;
        fReady := False;
        fCharges := TStringList.Create;
      end;

    destructor TMission.Destroy;
      begin
        inherited Destroy;
      end;

    class function TMission.OptionData( index : integer ) : TMissionOptionInfo;
      var
        i : integer;
      begin
        result.Id    := '';
        result.way   := '';
        for i := 0 to (MaxRoles - 1) do
          begin
            result.Roles[i] := '';
            result.SkillValues[i] := 0;
            result.Descriptions[i] := '';
            result.Skills[i] := '';
          end;
        result.Sub := False;
        result.ParentOptionId := '';
        result.Objective := '';
        result.ObjCoord.X := 0;
        result.ObjCoord.Y := 0;
      end;

    function TMission.GetRightCriminalForTask(Role : widestring; skill : widestring) : TCrimForTask;
      var
        i : integer;
        j : integer;
        k : integer;
        l : integer;
        m : integer;
        criminal   : TCriminal;
        criminal2  : TCriminal;
        crimSpare  : array[1..8] of widestring;
        CrimName   : widestring;
        BackupName : widestring;
        SkillValue  : single;
        SkillBackup : single;
        CrimChosen  : TCrimForTask;
      begin
        CrimName := '';
        SkillValue := 0;
        SkillBackup := 0;
        for i := 1 to 8 do
          begin
            if Roles.Name[i] <> ''
            then
              begin
                criminal := IllSystem.FindCriminalInSystem(Roles.Name[i]);
                if Role = Roles.Role[i]
                then
                  begin
                    if criminal.State <> 'InTeam: InMission: Dead'
                    then
                      begin
                        for l := 0 to (criminal.Attributes.Count - 1) do
                          if TAttribute(criminal.Attributes[l]).MetaAttribute.Name = skill
                          then
                            begin
                              CrimName := Roles.Name[i];
                              SkillValue := TAttribute(criminal.Attributes[l]).Value;
                            end;
                      end;
                  end
                else
                  begin
                    for k := 0 to (TheClassStorage.ClassCount['Role'] - 1) do
                      if StrLComp(PChar(Roles.Role[i]), PChar(TMetarole(TheClassStorage.ClassByIdx['Role', k]).Name), StrLen(PChar(Roles.Role[i]))) = 0
                      then
                        if (TMetaRole(TheClassStorage.ClassByIdx['Role', k]).Name + '(Backup)') = Roles.Role[i]
                        then
                          begin
                            if criminal.State <> 'InTeam: InMission: Dead'
                            then
                              begin
                                for l := 0 to (criminal.Attributes.Count - 1) do
                                  if TAttribute(criminal.Attributes[l]).MetaAttribute.Name = skill
                                  then
                                    if criminal.State = 'InTeam: InMission: Wounded'
                                    then
                                      begin
                                        if (TAttribute(criminal.Attributes[l]).Value / 2) > SkillValue
                                        then
                                          begin
                                            SkillValue := (TAttribute(criminal.Attributes[l]).Value / 2);
                                            BackupName := criminal.Name;
                                          end;
                                      end
                                    else
                                      begin
                                        if (TAttribute(criminal.Attributes[l]).Value) > SkillValue
                                        then
                                          begin
                                            SkillValue := (TAttribute(criminal.Attributes[l]).Value / 2);
                                            BackupName := criminal.Name;
                                          end;
                                      end;
                              end;
                          end
                        else
                          crimSpare[i] := Roles.Name[i];
                  end;
              end;
          end;
        if crimName <> ''
        then
          begin
            if IllSystem.FindCriminalInSystem(crimName).State = 'InTeam: InMission: Wounded'
            then
              begin
                if (SkillValue / 2) >= SkillBackup
                then
                  begin
                    CrimChosen.Name := CrimName;
                    CrimChosen.SkillVal := (SkillValue / 2)
                  end
                else
                  begin
                    CrimChosen.Name := BackupName;
                    CrimChosen.SkillVal := SkillBackup;
                  end;
              end
            else
              begin
                CrimChosen.Name := CrimName;
                CrimChosen.SkillVal := SkillValue
              end;
          end
        else
          if BackupName <> ''
          then
            begin
              if IllSystem.FindCriminalInSystem(BackupName).State = 'InTeam: InMission: Wounded'
              then
                begin
                  CrimChosen.Name := BackupName;
                  CrimChosen.SkillVal := (SkillBackup / 2)
                end
              else
                begin
                  CrimChosen.Name := BackupName;
                  CrimChosen.SkillVal := SkillBackup;
                end;
            end
          else
            begin
              for j := 1 to 8 do
              begin
                if crimSpare[j] <> ''
                then
                  begin
                    criminal2 := IllSystem.FindCriminalInSystem(crimSpare[j]);
                    for m := 0 to (criminal2.Attributes.Count - 1) do
                      if TAttribute(criminal2.Attributes[m]).MetaAttribute.Name = skill
                      then
                        begin
                          if criminal2.State = 'InTeam: InMission: Wounded'
                          then
                            begin
                              if (TAttribute(criminal.Attributes[m]).Value / 2) > CrimChosen.SkillVal
                              then
                                begin
                                  CrimChosen.Name := criminal2.Name;
                                  CrimChosen.SkillVal := (TAttribute(criminal.Attributes[m]).Value / 2);
                                end;
                            end
                          else
                            begin
                              if (TAttribute(criminal.Attributes[m]).Value) > CrimChosen.SkillVal
                              then
                                begin
                                  CrimChosen.Name := criminal2.Name;
                                  CrimChosen.SkillVal := (TAttribute(criminal.Attributes[m]).Value);
                                end;
                            end;
                        end;
                  end;
              end;
            end;
        result := CrimChosen;
      end;

    function TMission.GetTeamMemberNameAndStatus(Name : widestring; skillName : widestring): TNameStatusSkill;
      var
        i : integer;
        j : integer;
        k : integer;
        NumRole : integer;
        Chosen  : integer;
        Right   : boolean;
        Crim : TNameStatusSkill;
      begin
        if Name <> ''
        then
          begin
            for i := 0 to (IllSystem.Criminals.Count - 1) do
              if TCriminal(IllSystem.Criminals[i]).Name = Name
              then
                begin
                  Crim.Status := TCriminal(IllSystem.Criminals[i]).State;
                  Crim.Name := Name
                end;
          end
        else
          begin
            for j := 1 to 8 do
              if Roles.Name[j] <> ''
              then
                NumRole := j;
            Right := False;
            i := 0;
            Chosen := Random(NumRole - 1) + 1;
            while Right = False do
              begin
                if TCriminal(IllSystem.Criminals[i]).Name = Roles.Name[Chosen]
                then
                  begin
                    if TCriminal(IllSystem.Criminals[i]).State <> 'InTeam: InMission: Dead'
                    then
                      begin
                        Crim.Name := Roles.Name[Chosen];
                        Crim.Status := TCriminal(IllSystem.Criminals[i]).State;
                        if skillName = ''
                        then
                          begin
                            for k := 0 to (TheClassStorage.ClassCount['Attribute'] - 1) do
                              if TMetaAttribute(TheClassStorage.ClassByIdx['Attribute' , k]).Name = skillName
                              then
                                Crim.Skill := TAttribute(TCriminal(IllSystem.Criminals[i]).Attributes[k]).Value;
                          end
                        else
                          Crim.Skill := 0;
                        Right := True;
                      end;
                  end;
                i := i + 1
              end;
          end;
        Result := Crim;
      end;

    function TMission.CheckTeamStatus: boolean;
      var
        i : integer;
        life : boolean;
      begin
        life := False;
        for i := 1 to 8 do
          if Roles.Name[i] <> ''
          then
            begin
              if IllSystem.FindCriminalInSystem(Roles.Name[i]).State <> 'InTeam: InMission: Dead'
              then
                life := True;
            end;
        Result := life;
      end;

    procedure TMission.StartMission;
      begin
        //
      end;

    procedure TMission.ProceedWithMission;
      begin
        //
      end;

    procedure TMission.EndMission;
      begin
        //
      end;

    constructor TRole.Create;
      begin
        inherited Create;
        fOwner := aOwner;
        fMetaRole := aMetaRole;
      end;

    destructor TRole.Destroy;
      begin
      end;

    constructor THistoryItem.Create;
      begin
        inherited Create;
        if aCriminalOwner <> nil
        then
          fCriminalOwner := aCriminalOwner;
        if aTeamOwner <> nil
        then
          fTeamOwner := aTeamOwner;
        if aLeaderOwner <> nil
        then
          fLeaderOwner := aLeaderOwner;
        fMetaHistoryItem := aMetaHistoryItem;
        fDate := aDate;
        fEvent := aEvent;
        fParam := aParam;
      end;

    constructor TCriminal.Create;
      begin
        inherited Create;
        fOwner := aOwner;
        fName := aName;
        fAge := 15 + Random(20);
        fPicture := aPicture;
        fState := 'OnTheMarket';
        fTrainingStage := 0;
        fChange := False;
        fSalaryPerc := 100;
        fAttributes := TCollection.Create(0, rkBelonguer);
        fHistoryItems := TLockableCollection.Create(0, rkBelonguer);
        fBirthday := Random(364) + 1;
        fCrimPolRecord := TStringList.Create;
        fCrimPenRecord := TStringList.Create;
        fWarrant := 0;
        fJailTime := 0;
        fTrialDay := 0;
        fTrialLength := 0;
        fLeaderName := '';
        fTeamName := '';
      end;

    procedure TCriminal.CreateAttributes;
      var
        j : integer;
        k : integer;
        l : integer;
        m : integer;
        n : integer;
        p : integer;
        q : integer;
        AttrLean     : TAttribute;
        value        : single;
        trained      : widestring;
        ClassBySkill : TCollection;
        TrainItems   : TCollection;
        good         : boolean;
        already      : boolean;
        found        : boolean;
        foundAll     : boolean;
      begin
        AttrLean := TAttribute.Create(self, TMetaAttribute(TheClassStorage.ClassById['Attribute', '11-Skill:Driving']), 0);
        AddAttribute(AttrLean);
        TrainItems := TCollection.Create(0, rkBelonguer);
        for p := 1 to (TheClassStorage.ClassCount['Attribute'] - 1) do
          begin
            if TMetaAttribute(TheClassStorage.ClassByIdx['Attribute', p]).Default = 0
              then
                value := Random(100)
              else
                value := TMetaAttribute(TheClassStorage.ClassByIdx['Attribute', p]).Default;
              AttrLean := TAttribute.Create(self, TMetaAttribute(TheClassStorage.ClassByIdx['Attribute', p]), value);
              AddAttribute(AttrLean);
          end;
        for j := 1 to ((Age - 15) * 2) do
          begin
            ClassBySkill := TCollection.Create(0, rkBelonguer);
            TAttribute(Attributes[0]).Value := TAttribute(Attributes[0]).Value + Random(3);
            trained := 'no';
            case Random(8) of
              0:
                begin
                  for l := 0 to (TheClassStorage.ClassCount['AttributeModifier'] - 1) do
                    if TAttributeModifier(TheClassStorage.ClassByIdx['AttributeModifier', l]).Attribute.Name = 'Driving'
                    then
                      ClassBySkill.Insert(TheClassStorage.ClassByIdx['AttributeModifier', l]);
                  {for o := 0 to (HistoryItems.Count - 1) do
                    if THistoryItem(HistoryItems[o]).MetaHistoryItem.Name = 'Training'
                    then
                      TrainItems.Insert(HistoryItems[o]);}
                  k := 0;
                end;
              1..8:
                begin
                  trained := 'yes';
                end;
            end;
            while trained = 'no' do
              begin
                good := True;
                already := false;
                foundAll := True;
                for n := 0 to (TrainItems.Count - 1) do
                  begin
                    if TAttributeModifier(ClassBySkill[k]).Name = THistoryItem(TrainItems[n]).Param
                    then
                      already := True;
                  end;
                for m := 0 to (TAttributeModifier(ClassBySkill[k]).PrevModifier.Count - 1) do
                  begin
                    found := false;
                    for q := 0 to (TrainItems.Count - 1) do
                      begin
                        if TAttributeModifier(TAttributeModifier(ClassBySkill[k]).PrevModifier[m]).Name = THistoryItem(TrainItems[q]).Param
                        then
                          found := True;
                      end;
                    if found = False
                    then
                      foundAll := False;
                  end;
                if foundAll = False
                then
                  good := False;
                if good = True
                then
                  if already = False
                  then
                    begin
                      trained := 'yes';
                      FinishedTraining(TAttributeModifier(ClassBySkill[k]));
                      TrainItems.Insert(HistoryItems[HistoryItems.Count - 1]);
                    end;
                k := k + 1;
                if k > (ClassBySkill.Count - 1)
                then
                  trained := 'yes'
              end;
          end;
        Salary := CalculateSalary;
      end;

    function TCriminal.CalculateSalary: single;
      var
        i        : integer;
        Average  : single;
        OutSkills: integer;
      begin
        Average := 0;
        OutSkills := 0;
        for i := 0 to (fAttributes.Count - 3) do
          begin
            Average := Average + TAttribute(fAttributes[i]).Value;
            if TAttribute(fAttributes[i]).Value >= 70
            then
              OutSkills := OutSkills + 1;
            if TAttribute(fAttributes[i]).Value >= 90
            then
              OutSkills := OutSkills + 1;
          end;
          Result := ((((Average / (fAttributes.Count - 2)) * 100) + (OutSkills * 1440))/ 720)  * SalaryPerc / 100;
      end;

    procedure TCriminal.StopTraining;
      var
        i : integer;
      begin
        TrainingClass := nil;
        for i := 0 to (Attributes.Count - 1) do
          TAttribute(Attributes[i]).InTraining := False;
      end;

    procedure TCriminal.Train(Training : widestring);
      var
        i : integer;
        j : integer;
      begin
        for j := 0 to (TheClassStorage.ClassCount['AttributeModifier'] - 1) do
          if Training = TAttributeModifier(TheClassStorage.ClassByIdx['AttributeModifier', j]).Name
            then
              begin
                TrainingClass := TAttributeModifier(TheClassStorage.ClassByIdx['AttributeModifier', j]);
                for i := 0 to (Attributes.Count - 1) do
                  if TAttribute(Attributes[i]).MetaAttribute.Name = TAttributeModifier(TheClassStorage.ClassByIdx['AttributeModifier', j]).Attribute.Name
                  then
                    TAttribute(Attributes[i]).InTraining := True;
              end;
        TrainingStage := 0;
      end;

    procedure TCriminal.Act;
      begin
        if Round(Time / 24) = Birthday
        then
          if Frac(Time / 24) = 0
          then
            begin
              Age := Age + 1;
              Change := True;
            end;
        if fState = 'InJail'
        then
          if Time = TrialDay
          then
            begin
              IllSystem.Trial(Name, LawyersHours);
              Change := True;
            end;
        if fState = 'InTeam: Training'
        then
          begin
            if TrainingStage = TrainingClass.Time
            then
              begin
                Change := True;
                TrainingStage := 0;
                State := 'InTeam: StandBy';
                FinishedTraining(TrainingClass);
                StopTraining;
              end
            else
              TrainingStage := TrainingStage + 1;
          end;
      end;

    procedure TCriminal.FinishedTraining(AttClass : TAttributeModifier);
      var
        i : integer;
        j : integer;
        learnResult : integer;
        gain        : integer;
        skillValue  : single;
      begin
        for j := 0 to (Attributes.Count - 1) do
          if TAttribute(Attributes[j]).MetaAttribute.Name = 'Learning'
          then
            skillValue := TAttribute(Attributes[j]).Value;
        for i := 0 to (Attributes.Count - 1) do
          if AttClass.Attribute.Name = TAttribute(Attributes[i]).MetaAttribute.Name
          then
            begin
              learnResult := IllSystem.UseSkill(skillValue, AttClass.Difficulty);
              case learnResult of
                1 :
                  begin
                    gain := AttClass.value + Round(AttClass.value / 5);
                    TAttribute(Attributes[i]).Value := TAttribute(Attributes[i]).Value + gain;
                  end;
                2 :
                  begin
                    gain := AttClass.value;
                    TAttribute(Attributes[i]).Value := TAttribute(Attributes[i]).Value + gain;
                  end;
                3 :
                  begin
                    gain := AttClass.value - Round(AttClass.value / 5);
                    TAttribute(Attributes[i]).Value := TAttribute(Attributes[i]).Value + gain;
                  end;
                4 :
                  begin
                    gain := AttClass.value - Round(AttClass.value / 3);
                    TAttribute(Attributes[i]).Value := TAttribute(Attributes[i]).Value + gain;
                  end;
              end;
              HistoryItems.Lock;
              try
                AddHistoryItem(THistoryItem.Create(nil, self, nil, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Criminal11: Training']), Time, 'Finished Class: ' + AttClass.Name + ' Points Gained: ' + IntToStr(gain) + ' of ' + IntToStr(AttClass.value), AttClass.Name));
              finally
                HistoryItems.Unlock;
              end;
            end;
        StopTraining;
      end;

    destructor TCriminal.Destroy;
      begin
        fAttributes.Free;
        fHistoryItems.Free;
        inherited;
      end;

    procedure TCriminal.AddAttribute( Attribute : TAttribute );
      begin
        fAttributes.Insert ( Attribute );
      end;

    procedure TCriminal.AddHistoryItem( HistoryItem : THistoryItem );
      begin
        fHistoryItems.Insert ( HistoryItem );
      end;

    constructor TTeam.Create;
      begin
        inherited Create;
        fOwner := aOwner;
        fName := aName;
        fRoles := TLockableCollection.Create(0, rkBelonguer);
        fHistoryItems := TLockableCollection.Create(0, rkBelonguer);
        fHeadquarter.X := 0;
        fHeadquarter.Y := 0;
      end;

    destructor TTeam.Destroy;
      begin
        fMission.Free;
        fRoles.Free;
        fHistoryItems.Free;
        inherited;
      end;

    procedure TTeam.AddRole( Role : TRole );
      begin
        fRoles.Insert ( Role );
      end;

    procedure TTeam.AddHistoryItem( HistoryItem : THistoryItem );
      begin
        fHistoryItems.Insert ( HistoryItem );
      end;

    procedure TTeam.RoleChange(CriminalName : widestring; NewRoleName : widestring);
      var
        i : integer;
        j : integer;
        changed : widestring;
      begin
        changed := 'no';
        i := 0;
        Roles.Lock;
        try
          while changed = 'no' do
            begin
              if TRole(Roles[i]).Criminal.Name = CriminalName
              then
                if NewRoleName = 'EraseRole'
                then
                  begin
                    TRole(Roles[i]).Criminal.CalculateSalary;
                    TRole(Roles[i]).Criminal.LeaderName := '';
                    TRole(Roles[i]).Criminal.TeamName := '';
                    Roles.AtDelete(i);
                    changed := 'yes';
                  end
                else
                  begin
                    for j := 0 to (TheClassStorage.ClassCount['Role'] - 1) do
                      if TMetaRole(TheClassStorage.ClassByIdx['Role', j]).Name = NewRoleName
                      then
                        begin
                          TRole(Roles[i]).MetaRole := TMetaRole(TheClassStorage.ClassByIdx['Role', j]);
                          changed := 'yes';
                        end;
                  end;
              if i = Roles.Count
              then
                changed := 'yes';
              i := i + 1;
            end;
        finally
          Roles.Unlock;
        end;
      end;

    procedure TTeam.Hire(LeaderName : widestring; CriminalName: widestring; SalaryPerc : integer);
      begin
        IllSystem.CreateRole(self, TMetaRole(TheClassStorage.ClassByIdx['Role', 0]));
        TRole(Roles[self.Roles.Count - 1]).Criminal := IllSystem.FindCriminalInSystem(CriminalName);
        IllSystem.RDOChangeCriminalSalary(LeaderName, CriminalName, SalaryPerc);
        TCriminal(TRole(Roles[self.Roles.Count - 1]).Criminal).State := 'InTeam: StandBy';
        TCriminal(TRole(Roles[self.Roles.Count - 1]).Criminal).TeamName := Name;
        TCriminal(TRole(Roles[self.Roles.Count - 1]).Criminal).LeaderName := LeaderName;
      end;

    function TTeam.CalcSalary : single;
      var
        i : integer;
        total : single;
      begin
        total := 0;
        Roles.Lock;
        try
          for i := 0 to (Roles.Count - 1) do
            begin
              total := total + TRole(Roles[i]).Criminal.Salary;
              if TRole(Roles[i]).Criminal.State = 'InTeam: Training'
              then
                total := total + (TAttributeModifier(TRole(Roles[i]).Criminal.TrainingClass).Cost / TAttributeModifier(TRole(Roles[i]).Criminal.TrainingClass).Time)
            end;
        finally
          Roles.Unlock;
        end;
        Result := total;
      end;

    function TTeam.CalcAvPerc : single;
      var
        k : integer;
        PercAverage : single;
      begin
        PercAverage := 0;
        Roles.Lock;
        try
          if Roles.Count <> 0
          then
            begin
              for k := 0 to (Roles.Count - 1) do
                PercAverage := PercAverage + TCriminal(TRole(Roles[k]).Criminal).SalaryPerc;
              PercAverage := PercAverage / Roles.Count;
            end;
        finally
          Roles.Unlock;
        end;
        Result := PercAverage;
      end;

    function TTeam.CalcTrainingCost : single;
      var
        i : integer;
        total : single;
      begin
        total := 0;
        Roles.Lock;
        try
          if Roles.Count <> 0
          then
            for i := 0 to (Roles.Count - 1) do
              if TRole(Roles[i]).Criminal.State = 'InTeam: Training'
              then
                total := total + (TRole(Roles[i]).Criminal.TrainingClass.Cost / TRole(Roles[i]).Criminal.TrainingClass.Time);
        finally
          Roles.Unlock;
        end;
        Result := total;
      end;

    procedure TTeam.AdjustLoyaltyAndReputation;
        var
          i : integer;
          j : integer;
          criminal : TCriminal;
          PercAverage : integer;
        begin
          if (Time mod 30) = 0
          then
            begin
              Roles.Lock;
              try
                if Roles.Count <> 0
                then
                  begin
                    PercAverage := Round(CalcAvPerc);
                    for i := 0 to (Roles.Count - 1) do
                      begin
                        criminal := TCriminal(TRole(Roles[i]).Criminal);
                        for j := 0 to (criminal.Attributes.Count - 1) do
                          if TMetaAttribute(TAttribute(criminal.Attributes[j]).MetaAttribute).Name = 'Loyalty'
                          then
                            begin
                              TAttribute(criminal.Attributes[j]).Value := TAttribute(criminal.Attributes[j]).Value + Round((criminal.SalaryPerc - PercAverage)/10);
                              criminal.Change := True;
                            end;
                        Owner.Reputation := Owner.Reputation + Round((criminal.SalaryPerc - PercAverage)/20);
                      end;
                  end;
               finally
                Roles.Unlock;
              end;
            end;
        end;

    procedure TTeam.Act;
      begin
        AdjustLoyaltyAndReputation;
        if Mission <> nil
        then
          if Mission.Ready = True
          then
            begin
              Mission.ProceedWithMission;
              Mission.MissionTime := Mission.MissionTime + 1;
            end;
      end;

    constructor TLeader.Create;
      begin
        inherited Create;
        fTeams := TLockableCollection.Create(0, rkBelonguer);
        fHistoryItems := TLockableCollection.Create(0, rkBelonguer); 
        fOwner := aOwner;
        fPicture := aPicture;
        fName := aName;
        fMoney := 10000000;
        fExpenses := 0;
        fReputation := 0;
      end;

    destructor TLeader.Destroy;
      begin
        fTeams.Destroy;
        inherited;
      end;

    procedure TLeader.AddTeam( Team : TTeam );
      begin
        fTeams.Insert ( Team );
      end;

    procedure TLeader.AddHistoryItem( HistoryItem : THistoryItem );
      begin
        fHistoryItems.Insert ( HistoryItem );
      end;

    procedure TLeader.DismTeam(Team : TTeam);
      var
        i : integer;
        j : integer;
        dismissed : widestring;
      begin
        for i := 7 downto 0 do
          begin
            Team.Roles.Lock;
            try
              if i < (Team.Roles.Count)
              then
                begin
                  IllSystem.RDOFireCriminal(self.Name, Team.Name, TRole(Team.Roles[i]).Criminal.Name)
                end;
            finally
              Team.Roles.Unlock;
            end;
          end;
        j := 0;
        dismissed := 'no';
        Teams.Lock;
        try
          while dismissed = 'no' do
            begin
              if TTeam(Teams[j]).Name = Team.Name
              then
                begin
                  self.Teams.AtDelete(j);
                  dismissed := 'yes';
                end;
              j := j+1
            end;
        finally
          Teams.Unlock;
        end;
      end;

    function TLeader.CalcExpenses : single;
      var
        i        : integer;
        expenses : single;
      begin
         expenses := 0;
         Teams.Lock;
         try
           for i := 0 to (Teams.Count - 1) do
             expenses := expenses + StrToFloat(IllSystem.RDOCalculateSalary(Name, TTeam(Teams[i]).Name));
         finally
           Teams.Unlock;
         end;
        Result := expenses;
      end;

    procedure TLeader.Act;
      begin
        Expenses := CalcExpenses;
        Money := Money - Expenses;
      end;

    constructor TIllegalSystem.Create;
      begin
        inherited;
        fCriminals := TLockableCollection.Create(0, rkBelonguer);
        fLeaders := TLockableCollection.Create(0, rkBelonguer);
      end;

    destructor TIllegalSystem.Destroy;
      begin
        fCriminals.Free;
        fLeaders.Free;
        inherited;
      end;

    procedure TIllegalSystem.AddCriminal( Criminal : TCriminal );
      begin
        fCriminals.Insert ( Criminal );
      end;

    procedure TIllegalSystem.AddLeader( Leader : TLeader );
      begin
        fLeaders.Insert ( Leader );
      end;

    procedure TIllegalSystem.Act;
      begin

      end;

    function TIllegalSystem.RDOGetCriminalNames: widestring;
      var
        i : integer;
        NameList : TStringList;
      begin
        NameList := TStringList.Create;
        IllSystem.Criminals.Lock;
        try
          for i := 0 to (IllSystem.Criminals.Count - 1) do
            NameList.Values[IntToStr(i)] := TCriminal(IllSystem.Criminals[i]).Name;
          Result := NameList.Text;
        finally
          IllSystem.Criminals.Unlock;
        end;
      end;

    function TIllegalSystem.CriminalWrapper(Criminal : TCriminal): TStringList;
      var
        i : integer;
        List : TStringList;
      begin
        List := TStringList.Create;
        List.Values['Name']           := Criminal.Name;
        List.Values['Picture']        := Criminal.Picture;
        List.Values['Name']           := Criminal.Name;
        List.Values['Age']            := IntToStr(Criminal.Age);
        List.Values['State']          := Criminal.State;
        List.Values['Salary']         := FloatToStr(Criminal.Salary);
        List.Values['SalaryPerc']     := IntToStr(Criminal.SalaryPerc);
        List.Values['TrainingStage']  := IntToStr(Criminal.TrainingStage);
        List.Values['Birthday']       := IntToStr(Criminal.Birthday);
        List.Values['Warrant']        := FloatToStr(Criminal.Warrant);
        List.Values['JailTime']       := IntToStr(Criminal.JailTime);
        List.Values['TrialDay']       := IntToStr(Criminal.TrialDay);
        List.Values['LawyersHours']   := IntToStr(Criminal.LawyersHours);
        List.Values['TrialLength']   := IntToStr(Criminal.TrialLength);
        if Criminal.TrainingClass <> nil
        then
          List.Values['TrainingClass']  := Criminal.TrainingClass.Name;
        List.Values['AttrInTraining'] := '';
        if Criminal.Change = True
        then
          List.Values['Change'] := 'yes'
        else
          List.Values['Change'] := 'no';
        for i := 0 to (Criminal.Attributes.Count - 1) do
          begin
            List.Values[IntToStr(i)] := FloatToStr(TAttribute(Criminal.Attributes[i]).Value);
            if TAttribute(Criminal.Attributes[i]).InTraining = True
            then
              List.Values['AttrInTraining'] := TAttribute(Criminal.Attributes[i]).MetaAttribute.Name;
          end;
        Result := List;
      end;

    function TIllegalSystem.TeamWrapper(Team : TTeam): TStringList;
      var
        i : integer;
        j : integer;
        k : integer;
        List : TStringList;
      begin
        List := TStringList.Create;
        List.Values['Name'] := Team.Name;
        List.Values['HeadquarterX'] := IntToStr(Team.Headquarter.X);
        List.Values['HeadquarterY'] := IntToStr(Team.Headquarter.Y);
        if Team.Mission <> nil
        then
          begin
            List.Values['MissionName'] := Team.Mission.MetaMission.Name;
            List.Values['ObjX'] := IntToStr(Team.Mission.Objective.X);
            List.Values['ObjY'] := IntToStr(Team.Mission.Objective.Y);
            List.Values['Change'] := Team.Mission.Change;
            List.Values['Cost'] := FloatToStr(Team.Mission.Cost);
            List.Values['Profit'] := FloatToStr(Team.Mission.Profit);
            List.Values['Time'] := IntToStr(Team.Mission.MissionTime);
            if Team.Mission.Ready = True
            then
              List.Values['Ready'] := 'True'
            else
              List.Values['Ready'] := 'False';
            for j := 1 to (MaxOptions - 1) do
              begin
                List.Values['Parameter' + IntToStr(j)] := Team.Mission.Parameters.Name[j];
                if Team.Mission.Parameters.Value[j] = True
                then
                  List.Values['ParameterValue' + IntToStr(j)] := 'yes'
                else
                  List.Values['ParameterValue' + IntToStr(j)] := 'no';
                List.Values['ParameterWay' + IntToStr(j)] := Team.Mission.Parameters.Way[j];
              end;
            for k := 1 to 8 do
              begin
                List.Values['CrimInMission' + IntToStr(k)] := Team.Mission.Roles.Name[k];
                List.Values['RoleInMissionName' + IntToStr(k)] := Team.Mission.Roles.Role[k];
              end;
          end
        else
          List.Values['MissionName'] := '';
        Team.Roles.Lock;
        try
          List.Values['NumCrim'] := IntToStr(Team.Roles.Count);
          for i := 0 to (Team.Roles.Count - 1) do
            begin
              List.Values['Role' + IntToStr(i)] := TRole(Team.Roles[i]).MetaRole.Name;
              List.Values['Criminal' + IntToStr(i)] := TRole(Team.Roles[i]).Criminal.Name;
            end;
        finally
          Team.Roles.Unlock;
        end;
        Result := List;
      end;

    function TIllegalSystem.RDOGetMissionReport(LeaderName: widestring; TeamName : widestring): olevariant;
      begin
        Result := FindTeamInSystem(LeaderName, TeamName).Mission.Report.Text;
      end;

    function TIllegalSystem.LeaderWrapper(Leader : TLeader): TStringList;
      var
        List : TStringList;
      begin
        List := TStringList.Create;
        List.Values['Name'] := Leader.Name;
        List.Values['Picture'] := Leader.Picture;
        List.Values['Money'] := FloatToStr(Leader.Money);
        List.Values['Expenses'] := FloatToStr(Leader.Expenses);
        List.Values['Reputation'] := IntToStr(Leader.Reputation);
        Result := List;
      end;

    function TIllegalSystem.HistoryItemsWrapper(HistoryItems : TLockableCollection) : TStringList;
      var
        i : integer;
        List : TStringList;
      begin
        HistoryItems.Lock;
        List := TStringList.Create;
        try
          begin
            for i := 0 to (HistoryItems.Count - 1) do
              begin
                List.Values['Item' + IntToStr(i) + '-' + 'Date'] := IntToStr(THistoryItem(HistoryItems[i]).Date);
                List.Values['Item' + IntToStr(i) + '-' + 'Event'] := THistoryItem(HistoryItems[i]).Event;
                List.Values['Item' + IntToStr(i) + '-' + 'Param'] := THistoryItem(HistoryItems[i]).Param;
                List.Values['Item' + IntToStr(i) + '-' + 'Name'] := THistoryItem(HistoryItems[i]).MetaHistoryItem.Name;
                List.Values['Item' + IntToStr(i) + '-' + 'Desc'] := THistoryItem(HistoryItems[i]).MetaHistoryItem.Desc;
                if THistoryItem(HistoryItems[i]).MetaHistoryItem.Name = 'MissionCompletedByATeam'
                then
                  List.Values['Item' + IntToStr(i) + '-' + 'Mission?'] := 'yes';
                if THistoryItem(HistoryItems[i]).MetaHistoryItem.Name = 'AccomplishedMission'
                then
                  List.Values['Item' + IntToStr(i) + '-' + 'Mission?'] := 'yes';
                if THistoryItem(HistoryItems[i]).MetaHistoryItem.Name = 'MissionAccomplished'
                then
                  List.Values['Item' + IntToStr(i) + '-' + 'Mission?'] := 'yes';
              end;
            List.Values['ItemNumber'] := IntToStr(HistoryItems.Count - 1);
            end;
        finally
          HistoryItems.Unlock;
        end;
        Result := List;
      end;

    // Running Code

    procedure TIllegalSystem.Start;
      begin
        Randomize;
      end;

    // Create Leader and store him in the system

    procedure TIllegalSystem.RDOCreateLeader(Name : widestring; Picture : widestring);
      var
        LeaderLean : TLeader;
      begin
        LeaderLean := TLeader.Create(IllSystem, Name, Picture);
        LeaderLean.AddHistoryItem(THistoryItem.Create(nil, nil, LeaderLean, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Leader11: LeaderBorn']), Time, Name + ' becomes a criminal boss', ''));
        IllSystem.AddLeader(LeaderLean);
      end;

    // Create Criminal and store him in the system

    procedure TIllegalSystem.RDOCreateCriminal(Name : widestring; Picture : widestring);
      var
        CriminalLean : TCriminal;
      begin
        CriminalLean := TCriminal.Create(IllSystem, Name, Picture);
        IllSystem.AddCriminal(CriminalLean);
        TCriminal(IllSystem.Criminals[IllSystem.Criminals.Count - 1]).CreateAttributes;
      end;

    function TIllegalSystem.RDOCreateTeam(LeaderName : widestring; Name : widestring): olevariant;
      var
        j : integer;
        TeamLean : TTeam;
        leader : TLeader;
        already : widestring;
      begin
        already := 'no';
        leader := FindLeaderinSystem(LeaderName);
        Leader.Teams.Lock;
        try
          for j := 0 to (Leader.Teams.Count - 1) do
            if Name = TTeam(Leader.Teams.Items[j]).Name
            then
              already := 'yes';
        finally
          Leader.Teams.Unlock;
        end;
        if already = 'no'
        then
          begin
            TeamLean := TTeam.Create(FindLeaderinSystem(LeaderName), Name);
            TeamLean.AddHistoryItem(THistoryItem.Create(TeamLean, nil, nil, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Team11: TeamCreation']), Time, 'The team is created by ' + LeaderName , ''));
            leader.AddTeam(TeamLean);
            leader.AddHistoryItem(THistoryItem.Create(nil, nil, leader, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Leader12: TeamCreated']), Time, 'Founded team ' + Name, ''));
            Result := 'yes';
          end
        else
          Result := 'no';
      end;

     procedure TIllegalSystem.CreateRole(Owner : TTeam; MetaRole : TMetaRole);
      var
        RoleLean : TRole;
      begin
        RoleLean := TRole.Create(Owner, MetaRole);
        Owner.AddRole(RoleLean);
      end;

      // Change Role in a Given Team

      function TIllegalSystem.FindLeaderinSystem(LeaderName : widestring): TLeader;
        var
          i : integer;
          found : widestring;
          leader : TLeader;
        begin
          i := 0;
          found := 'no';
          IllSystem.Leaders.Lock;
          try
            while found = 'no' do
              begin
                if TLeader(IllSystem.Leaders[i]).Name = LeaderName
                then
                  begin
                    leader := TLeader(IllSystem.Leaders[i]);
                    found := 'yes';
                  end;
                i := i + 1;
                if (i = (IllSystem.Leaders.Count)) and (found = 'no')
                then
                  begin
                    found := 'yes';
                    leader := nil;
                  end;
              end;
          finally
            IllSystem.Leaders.Unlock;
          end;
          Result := leader;
        end;

      function TIllegalSystem.RDOFindLeader(LeaderName : widestring): olevariant;
        var
          Leader : TLeader;
        begin
          Leader := FindLeaderInSystem(LeaderName);
          if Leader <> nil
          then
            Result := LeaderWrapper(Leader).Text
          else
            Result := '';
        end;

      function TIllegalSystem.FindTeamInSystem(LeaderName : widestring; TeamName : widestring): TTeam;
        var
          i : integer;
          Leader : TLeader;
          found : widestring;
        begin
          Leader := FindLeaderinSystem(LeaderName);
          i := 0;
          found := 'no';
          Leader.Teams.Lock;
          try
            while found = 'no' do
              begin
                if TTeam(Leader.Teams[i]).Name = TeamName
                then
                  begin
                    Result := TTeam(Leader.Teams[i]);
                    found := 'yes';
                  end;
                if i = (Leader.Teams.Count - 1)
                  then
                    found := 'yes';
                  i := i + 1;
              end;
          finally
            Leader.Teams.Unlock;
          end;
        end;

      function TIllegalSystem.RDOFindTeam(LeaderName : widestring; TeamName : widestring): olevariant;
        var
          Team : TTeam;
        begin
          Team := FindTeamInSystem(LeaderName, TeamName);
          Result := TeamWrapper(Team).Text;
        end;

      function TIllegalSystem.FindCriminalInSystem(CriminalName : widestring): TCriminal;
        var
          i : integer;
        begin
          IllSystem.Criminals.Lock;
          try
            for i := 0 to (IllSystem.Criminals.Count - 1) do
              if TCriminal(IllSystem.Criminals[i]).Name = CriminalName
              then
                Result := TCriminal(IllSystem.Criminals[i]);
          finally
            IllSystem.Criminals.Unlock;
          end;
        end;

      function TIllegalSystem.RDOFindCriminal(CriminalName : widestring): olevariant;
        var
          Criminal : TCriminal;
        begin
          Criminal := FindCriminalInSystem(CriminalName);
          Result := CriminalWrapper(Criminal).Text;
        end;

      procedure TIllegalSystem.RDOChangeRole(LeaderName : widestring; TeamName : widestring; CriminalName : widestring; NewRoleName : widestring);
        begin
          FindTeamInSystem(LeaderName, TeamName).RoleChange(CriminalName, NewRoleName);
        end;

      procedure TIllegalSystem.RDOHireCriminal(LeaderName : widestring; TeamName : widestring; CriminalName : widestring; SalaryPerc : integer);
        var
          crim : TCriminal;
          team : TTeam;
          leader : TLeader;
        begin
          FindTeamInSystem(LeaderName, TeamName).Hire(LeaderName, CriminalName, SalaryPerc);
          crim := FindCriminalInSystem(CriminalName);
          crim.AddHistoryItem(THistoryItem.Create(nil, crim, nil, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Criminal12: Hire']), Time, 'Hired by ' + LeaderName + ' in team ' + TeamName, ''));
          team := FindTeamInSystem(LeaderName, TeamName);
          team.AddHistoryItem(THistoryItem.Create(team, nil, nil, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Team12: HiredCriminal']), Time, CriminalName + ' has been hired in the team', ''));
          leader := FindLeaderinSystem(LeaderName);
          leader.AddHistoryItem(THistoryItem.Create(nil, nil, leader, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Leader14: CriminalHired']), Time, 'Hired ' + CriminalName + ' in team ' + TeamName, ''));
        end;

      procedure TIllegalSystem.RDOFireCriminal(LeaderName : widestring; TeamName : widestring; CriminalName : widestring);
        var
          crim : TCriminal;
          team : TTeam;
          leader : Tleader;
        begin
          RDOChangeRole(LeaderName, TeamName, CriminalName, 'EraseRole');
          crim := FindCriminalInSystem(CriminalName);
          crim.AddHistoryItem(THistoryItem.Create(nil, crim, nil, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Criminal13: Fire']), Time, 'Fired by ' + LeaderName + ' from team ' + TeamName, ''));
          team := FindTeamInSystem(LeaderName, TeamName);
          team.AddHistoryItem(THistoryItem.Create(team, nil, nil, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Team13: FiredCriminal']), Time, CriminalName + ' has been fired from the team', ''));
          leader := FindLeaderinSystem(LeaderName);
          leader.AddHistoryItem(THistoryItem.Create(nil, nil, leader, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Leader14: CriminalHired']), Time, 'Fired ' + CriminalName + ' from team ' + TeamName, ''));
        end;

      procedure TIllegalSystem.RDODismissTeam(LeaderName : widestring; TeamName : widestring);
        var
          leader : TLeader;
        begin
          leader := FindLeaderinSystem(LeaderName);
          leader.DismTeam(FindTeamInSystem(LeaderName, TeamName));
          leader.AddHistoryItem(THistoryItem.Create(nil, nil, leader, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Leader13: TeamDismissed']), Time, 'Dismissed team ' + TeamName, ''));
        end;

      procedure TIllegalSystem.RDOChangeCriminalState(CriminalName : widestring; NewState : widestring);
        var
          crim : TCriminal;
        begin
          crim := FindCriminalInSystem(CriminalName);
          crim.State := NewState;
          crim.Change := True;
        end;

      procedure TIllegalSystem.RDOChangeTeam(LeaderName : widestring; OldTeamName : widestring; NewTeamName : widestring; CriminalName : widestring);
        var
          crim : TCriminal;
          team : TTeam;
          leader : TLeader;
        begin
          RDOChangeRole(LeaderName, OldTeamName, CriminalName, 'EraseRole');
          FindTeamInSystem(LeaderName, NewTeamName).Hire(LeaderName, CriminalName, 100);
          crim := FindCriminalInSystem(CriminalName);
          crim.AddHistoryItem(THistoryItem.Create(nil, crim, nil, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Criminal14: ChangeTeam']), Time, 'Moved from team ' + OldTeamName + ' to team ' + NewTeamName, ''));
          team := FindTeamInSystem(LeaderName, OldTeamName);
          team.AddHistoryItem(THistoryItem.Create(team, nil, nil, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Team14: CriminalChangeTeam']), Time, CriminalName + ' has left the team to join team ' + NewTeamName, ''));
          team := FindTeamInSystem(LeaderName, NewTeamName);
          team.AddHistoryItem(THistoryItem.Create(team, nil, nil, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Team14: CriminalChangeTeam']), Time, CriminalName + ' has joined the team coming from team ' + OldTeamName, ''));
          leader := FindLeaderinSystem(LeaderName);
          leader.AddHistoryItem(THistoryItem.Create(nil, nil, leader, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Leader16: ChangeTeamToCriminal']), Time, 'Moved ' + CriminalName + ' from team ' + OldTeamName + ' to team ' + NewTeamName, ''));
        end;

      procedure TIllegalSystem.RDOCriminalShowed(CrimName : widestring);
        begin
          FindCriminalInSystem(CrimName).Change := False;
        end;

      procedure TIllegalSystem.RDOStopCriminalTraining(CriminalName : widestring);
        begin
          FindCriminalInSystem(CriminalName).StopTraining;
          RDOChangeCriminalState(CriminalName, 'InTeam: StandBy');
        end;

      procedure TIllegalSystem.RDOCriminalTraining(CriminalName : widestring; TrainingName : widestring);
        begin
          FindCriminalInSystem(CriminalName).Train(TrainingName);
          RDOChangeCriminalState(CriminalName, 'InTeam: Training');
        end;

      procedure TIllegalSystem.RDOAssignHeadquarter(LeaderName : widestring; TeamName : widestring; CoordinateX : integer; CoordinateY : integer);
        var
          Coord : TCoordinate;
        begin
          Coord.X := CoordinateX;
          Coord.Y := CoordinateY;
          FindTeamInSystem(LeaderName, TeamName).Headquarter := Coord;
        end;

      function TIllegalSystem.RDOCalculateSalary(LeaderName : widestring; TeamName : widestring) : olevariant;
        begin
          Result := FindTeamInSystem(LeaderName, TeamName).CalcSalary;
        end;

      function TIllegalSystem.RDOCalculateTrainingCost(LeaderName : widestring; TeamName : widestring) : olevariant;
        begin
          Result := FindTeamInSystem(LeaderName, TeamName).CalcTrainingCost;
        end;

      function TIllegalSystem.RDOCalculatePercentage(LeaderName : widestring; TeamName : widestring) : olevariant;
        begin
          Result := FindTeamInSystem(LeaderName, TeamName).CalcAvPerc;
        end;

      function TIllegalSystem.RDOCalculateLeaderExpenses(LeaderName : widestring) : olevariant;
        begin
          Result := FindLeaderinSystem(LeaderName).CalcExpenses;
        end;

      procedure TIllegalSystem.ChangeLeaderMoney(LeaderName : widestring; Money : single);
        begin
          FindLeaderinSystem(LeaderName).Money := FindLeaderinSystem(LeaderName).Money - Money;
        end;

      procedure TIllegalSystem.RDOChangeCriminalSalary(LeaderName : widestring; CriminalName : widestring; SalaryP : integer);
        var
          i : integer;
          oldSalary : single;
          criminal : TCriminal;
        begin
          if SalaryP = 0
          then
            SalaryP := 1;
          criminal := FindCriminalInSystem(CriminalName);
          oldSalary := criminal.Salary;
          criminal.Salary := (oldSalary / criminal.SalaryPerc * 100) * SalaryP / 100;
          for i := 0 to (criminal.Attributes.Count - 1) do
            if TMetaAttribute(TAttribute(criminal.Attributes[i]).MetaAttribute).Name = 'Loyalty'
            then
              TAttribute(criminal.Attributes[i]).Value := (TAttribute(criminal.Attributes[i]).Value - Round((criminal.SalaryPerc - 100)/10)) + Round((SalaryP - 100)/10);
          criminal.SalaryPerc := SalaryP;
          criminal.Change := True;
        end;

      function TIllegalSystem.RDORecoveryCriminalPendingRecord(CriminalName : widestring) : olevariant;
        var
          Crim : TCriminal;
        begin
          Crim := FindCriminalInSystem(CriminalName);
          Result := Crim.CrimPenRecord.Text;
        end;

      function TIllegalSystem.RDORecoveryCriminalPoliceRecord(CriminalName : widestring) : olevariant;
        var
          Crim : TCriminal;
        begin
          Crim := FindCriminalInSystem(CriminalName);
          Result := Crim.CrimPolRecord.Text;
        end;

      function TIllegalSystem.RDORecoveryCriminalJailDetails(CriminalName : widestring) : olevariant;
        var
          Details : TStringList;
          Crim : TCriminal;
        begin
          Crim := FindCriminalInSystem(CriminalName);
          Details := TStringList.Create;
          Details.Values['JailTime'] := IntToStr(Crim.JailTime);
          Details.Values['TrialDay'] := IntToStr(Crim.TrialDay);
          Details.Values['LawyersHours'] := IntToStr(Crim.LawyersHours);
          Details.Values['TrialLength'] := IntToStr(Crim.TrialLength);
          Result := Details.Text + Crim.CrimPolRecord.Text;
        end;

      function TIllegalSystem.RDORecoveryHistoryItem(LeaderName : widestring; TeamName : widestring; CriminalName : widestring): olevariant;
        begin
          if LeaderName <> ''
          then
            Result := HistoryItemsWrapper(FindLeaderInSystem(LeaderName).HistoryItems).Text;
          if TeamName <> ''
          then
            Result := HistoryItemsWrapper(FindTeamInSystem(LeaderName, TeamName).HistoryItems).Text;
          if CriminalName <> ''
          then
            Result := HistoryItemsWrapper(FindCriminalInSystem(CriminalName).HistoryItems).Text;
        end;

      function TIllegalSystem.RDORecoveryMissionInfos(MissionName : widestring): olevariant;
        var
          i     : integer;
          Info  : TStringList;
        begin
          Info := TStringList.Create;
          for i := 0 to (TheClassStorage.ClassCount['Mission'] - 1) do
            if TMetaMission(TheClassStorage.ClassByIdx['Mission', i]).Name = MissionName
            then
              begin
                Info.Values['descr'] := TMetaMission(TheClassStorage.ClassByIdx['Mission', i]).Desc;
                Info.Values['MissionType'] := TMetaMission(TheClassStorage.ClassByIdx['Mission', i]).MissionType;
                Info.Values['Id'] := TMetaMission(TheClassStorage.ClassByIdx['Mission', i]).Name;
              end;
          Result := Info.Text;
        end;

      function TIllegalSystem.RDORecoveryMissionOption(MissionName : widestring; OptionIndex : integer): olevariant;
        var
          i    : integer;
          j    : integer;
          Info : TStringList;
        begin
          Info := TStringList.Create;
          for i := 0 to (TheClassStorage.ClassCount['Mission'] - 1) do
            if TMetaMission(TheClassStorage.ClassByIdx['Mission', i]).Name = MissionName
            then
              begin
                Info.Values['Id'] := TMetaMission(TheClassStorage.ClassByIdx['Mission', i]).MissionClass.OptionData(OptionIndex).Id;
                Info.Values['Way'] := TMetaMission(TheClassStorage.ClassByIdx['Mission', i]).MissionClass.OptionData(OptionIndex).way;
                Info.Values['ParentOptionId'] := TMetaMission(TheClassStorage.ClassByIdx['Mission', i]).MissionClass.OptionData(OptionIndex).ParentOptionId;
                Info.Values['Objective'] := TMetaMission(TheClassStorage.ClassByIdx['Mission', i]).MissionClass.OptionData(OptionIndex).Objective;
                Info.Values['ObjCoorX'] := IntToStr(TMetaMission(TheClassStorage.ClassByIdx['Mission', i]).MissionClass.OptionData(OptionIndex).ObjCoord.X);
                Info.Values['ObjCoorY'] := IntToStr(TMetaMission(TheClassStorage.ClassByIdx['Mission', i]).MissionClass.OptionData(OptionIndex).ObjCoord.Y);
                Info.Values['Duration'] := IntToStr(TMetaMission(TheClassStorage.ClassByIdx['Mission', i]).MissionClass.OptionData(OptionIndex).Duration);
                Info.Values['Cost'] := FloatToStr(TMetaMission(TheClassStorage.ClassByIdx['Mission', i]).MissionClass.OptionData(OptionIndex).Cost);
                Info.Values['Profit'] := FloatToStr(TMetaMission(TheClassStorage.ClassByIdx['Mission', i]).MissionClass.OptionData(OptionIndex).Profit);
                if TMetaMission(TheClassStorage.ClassByIdx['Mission', i]).MissionClass.OptionData(OptionIndex).Sub
                then
                  Info.Values['Sub'] := 'yes'
                else
                  Info.Values['Sub'] := 'no';
                for j := 0 to (MaxRoles - 1) do
                  begin
                    Info.Values['Role' + IntToStr(j)] := TMetaMission(TheClassStorage.ClassByIdx['Mission', i]).MissionClass.OptionData(OptionIndex).Roles[j];
                    Info.Values['Desc' + IntToStr(j)] := TMetaMission(TheClassStorage.ClassByIdx['Mission', i]).MissionClass.OptionData(OptionIndex).Descriptions[j];
                    Info.Values['Skill' + IntToStr(j)] := TMetaMission(TheClassStorage.ClassByIdx['Mission', i]).MissionClass.OptionData(OptionIndex).Skills[j];
                    Info.Values['SkillValue' + IntToStr(j)] := FloatToStr(TMetaMission(TheClassStorage.ClassByIdx['Mission', i]).MissionClass.OptionData(OptionIndex).SkillValues[j]);
                  end;
              end;
          result := Info.Text;
        end;

      procedure TIllegalSystem.RDOAssignMission(LeaderName : widestring; TeamName : widestring; MissionName : widestring; RolesList : widestring; ParametersList : widestring; ObjectiveX : integer; ObjectiveY : integer; Cost : widestring);
        var
          i    : integer;
          j    : integer;
          k    : integer;
          l    : integer;
          team : TTeam;
          criminal : TCriminal;
          Leader : TLeader;
          Roles : TStringList;
          Parameters : TStringList;
          RolesToAss : TRolesInMission;
          ParamsToAss : TParametersInMission;
          CoordToAss : TCoordinate;
        begin
          Roles := TStringList.Create;
          Roles.Text := RolesList;
          Parameters := TStringList.Create;
          Parameters.Text := ParametersList;
          team := FindTeamInSystem(LeaderName, TeamName);
          for i := 0 to (TheClassStorage.ClassCount['Mission'] - 1) do
            if TMetaMission(TheClassStorage.ClassByIdx['Mission', i]).Name = MissionName
            then
              begin
                team.Mission := TMetaMission(TheClassStorage.ClassByIdx['Mission', i]).Instantiate(team);
                for k := 1 to 8 do
                  begin
                    RolesToAss.Name[k] := Roles.Values['Name' + IntToStr(k)];
                    RolesToAss.Role[k] := Roles.Values['Role' + IntToStr(k)];
                    team.Mission.Roles := RolesToAss;
                  end;
                for l := 1 to (MaxOptions - 1) do
                  begin
                    ParamsToAss.Name[l] := Parameters.Values['Name' + IntToStr(l)];
                    if Parameters.Values['Value' + IntToStr(l)] = 'True'
                    then
                      ParamsToAss.Value[l] := True
                    else
                      ParamsToAss.Value[l] := False;
                    ParamsToAss.Way[l] := Parameters.Values['Way' + IntToStr(l)];
                    team.Mission.Parameters := ParamsToAss;
                  end;
                CoordToAss.X := ObjectiveX;
                CoordToAss.Y := ObjectiveY;
                team.Mission.Objective := CoordToAss;
              end;
          team.AddHistoryItem(THistoryItem.Create(team, nil, nil, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Team17: MissionAssigned']), Time, 'Assigned the mission ' + MissionName + ' by ' + LeaderName, ''));
          for j := 1 to 8 do
            if team.Mission.Roles.Name[j] <> ''
            then
              begin
                criminal := FindCriminalInSystem(team.Mission.Roles.Name[j]);
                RDOChangeCriminalState(team.Mission.Roles.Name[j], 'InTeam: InMission');
                criminal.AddHistoryItem(THistoryItem.Create(nil, criminal, nil, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Criminal17: AssignedMission']), Time, 'Assigned the mission ' + MissionName + ' by ' + LeaderName + ' in team ' + TeamName, ''));
              end;
          Leader := FindLeaderinSystem(LeaderName);
          if team.Mission.MetaMission.MissionType = 'One-shot'
          then
            begin
              Leader.Money := Leader.Money - StrToFloat(Cost);
              team.Mission.Cost := StrToFloat(Cost);
            end;
          Leader.AddHistoryItem(THistoryItem.Create(nil, nil, Leader, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Leader17: AssignMission']), Time, 'Assigned the mission ' + MissionName + ' to team ' + TeamName, ''));
        end;

      procedure TIllegalSystem.RDOStartMission(LeaderName : widestring; TeamName : widestring; MissionName : widestring);
        begin
          FindTeamInSystem(LeaderName, TeamName).Mission.StartMission;
        end;

      procedure TIllegalSystem.RDODeassignMission(LeaderName : widestring; TeamName : widestring; Cancel : widestring; MissionName : widestring);
        var
          i : integer;
          team : TTeam;
          criminal : TCriminal;
          Leader : TLeader;
        begin
          Leader := FindLeaderinSystem(LeaderName);
          team := FindTeamInSystem(LeaderName, TeamName);
          if Cancel = 'yes'
          then
            begin
              team.AddHistoryItem(THistoryItem.Create(team, nil, nil, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Team18: MissionDeassigned']), Time, 'Deassigned the mission ' + MissionName + '  by ' + LeaderName, ''));
              Leader.AddHistoryItem(THistoryItem.Create(nil, nil, Leader, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Leader18: DeassignMission']), Time, 'Deassigned the mission ' + MissionName + ' to team ' + TeamName, ''));
            end
          else
            begin
              Leader.Money := Leader.Money + team.Mission.Profit;
              team.AddHistoryItem(THistoryItem.Create(team, nil, nil, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Team16: MissionAccomplished']), Time, 'The mission ' + MissionName + ', assigned by ' + LeaderName + ', has been accomplished by the team', team.Mission.Report.text));
              Leader.AddHistoryItem(THistoryItem.Create(nil, nil, Leader, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Leader19: MissionCompletedByATeam']), Time, 'The mission ' + MissionName + ' assigned to team ' + TeamName + ' has been accomplished', team.Mission.Report.text));
            end;
          for i := 1 to 8 do
            if team.Mission.Roles.Name[i] <> ''
            then
              begin
                criminal := FindCriminalInSystem(team.Mission.Roles.Name[i]);
                if Cancel = 'yes'
                then
                  begin
                    criminal.AddHistoryItem(THistoryItem.Create(nil, criminal, nil, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Criminal18: DeassignedMission']), Time, 'Deassigned the mission  by ' + LeaderName + ' in team ' + TeamName, ''));
                  end
                else
                  begin
                    criminal.AddHistoryItem(THistoryItem.Create(nil, criminal, nil, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Criminal16: AccomplishedMission']), Time, 'The mission ' + MissionName + ', assigned by ' + LeaderName + ', has been accomplished by the criminal in team ' + TeamName, team.Mission.Report.text));
                  end;
                if criminal.State = 'InTeam: InMission'
                then
                  begin
                    IllSystem.RDOChangeCriminalState(team.Mission.Roles.Name[i], 'InTeam: StandBy');
                  end;
                if criminal.State = 'InTeam: InMission: Wounded'
                then
                  begin
                    IllSystem.RDOChangeCriminalState(team.Mission.Roles.Name[i], 'InTeam: Wounded');
                  end;
                if criminal.State = 'InTeam: InMission: Dead'
                then
                  begin
                    IllSystem.RDOChangeCriminalState(team.Mission.Roles.Name[i], 'InTeam: Dead');
                  end;
              end;
          team.Mission := nil;
        end;

      procedure TIllegalSystem.GiveChargesToCriminal(CriminalName : widestring; Charges : TStringList);
        var
          j : integer;
          k : integer;
          l : integer;
          team : TTeam;
          criminal : TCriminal;
        begin
          k := 0;
          criminal := FindCriminalInSystem(CriminalName);
          team := FindTeamInSystem(criminal.LeaderName, criminal.TeamName);
          for j := 1 to (Charges.Count) do
          if Charges.Values['Charge' + IntToStr(j)] <> ''
          then
            for l := 1 to (Charges.Count) do
              if Charges.Values['Offender' + IntToStr(j) + IntToStr(l)] <> ''
              then
                if Charges.Values['Offender' + IntToStr(j) + IntToStr(l)] = criminal.Name
                then
                  begin
                    k := k + 1;
                    if Charges.Values['PoliceDiff' + IntToStr(j)] <> '0'
                    then
                      begin
                        criminal.CrimPenRecord.Values['Charge' + IntToStr(k)] := team.Mission.Charges.Values['Charge' + IntToStr(j)];
                        criminal.CrimPenRecord.Values['Mission'+ IntToStr(k)] := team.Mission.MetaMission.Name;
                        criminal.CrimPenRecord.Values['Date'+ IntToStr(k)] := IntToStr(team.Mission.StartingTime);
                        criminal.CrimPenRecord.Values['Leader' + IntToStr(k)] := criminal.LeaderName;
                        criminal.CrimPenRecord.Values['Team' + IntToStr(k)] := criminal.TeamName;
                      end
                    else
                      begin
                        criminal.CrimPolRecord.Values['Charge' + IntToStr(k)] := team.Mission.Charges.Values['Charge' + IntToStr(j)];
                        criminal.CrimPolRecord.Values['Mission'+ IntToStr(k)] := team.Mission.MetaMission.Name;
                        criminal.CrimPolRecord.Values['Date'+ IntToStr(k)] := IntToStr(team.Mission.StartingTime);
                        criminal.CrimPolRecord.Values['Leader' + IntToStr(k)] := criminal.LeaderName;
                        criminal.CrimPolRecord.Values['Team' + IntToStr(k)] := criminal.TeamName;
                      end
                  end;
        end;


      procedure TIllegalSystem.SetTrialDay(CriminalName : widestring);
        var
          i : integer;
          crim : TCriminal;
        begin
          crim := FindCriminalInSystem(CriminalName);
          if crim.TrialDay = 0
          then
            crim.TrialDay := Time;
          for i := 1 to (crim.CrimPolRecord.Count) do
            begin
              if crim.CrimPolRecord.Values['Charge' + IntToStr(i)] = 'InformaticCrime'
              then
                if (crim.TrialDay - Time) < 300
                then
                  crim.TrialDay := Time + 300;
              if crim.CrimPolRecord.Values['Charge' + IntToStr(i)] = 'BankRobbery'
              then
                if (crim.TrialDay - Time) < 500
                then
                  crim.TrialDay := Time + 500;
              if crim.CrimPolRecord.Values['Charge' + IntToStr(i)] = 'BankRobberyWithSafeWreckage'
              then
                if (crim.TrialDay - Time) < 700
                then
                  crim.TrialDay := Time + 700;
              if crim.CrimPolRecord.Values['Charge' + IntToStr(i)] = 'BankRobberyWithSafeHacking'
              then
                if (crim.TrialDay - Time) < 700
                then
                  crim.TrialDay := Time + 700;
              if crim.CrimPolRecord.Values['Charge' + IntToStr(i)] = 'Murder'
              then
                if (crim.TrialDay - Time) < 1200
                then
                  crim.TrialDay := Time + 1200;
              if crim.CrimPolRecord.Values['Charge' + IntToStr(i)] = 'AttemptedMurder'
              then
                if (crim.TrialDay - Time) < 1000
                then
                  crim.TrialDay := Time + 1000;
              if crim.CrimPolRecord.Values['Charge' + IntToStr(i)] = 'Assault'
              then
                if (crim.TrialDay - Time) < 200
                then
                  crim.TrialDay := Time + 200;
              if crim.CrimPolRecord.Values['Charge' + IntToStr(i)] = 'AggravatedAssault'
              then
                if (crim.TrialDay - Time) < 400
                then
                  crim.TrialDay := Time + 400;
            end;
        end;

      procedure TIllegalSystem.Trial(CriminalName : widestring; LegalHours : integer);
        var
          i : integer;
          crim : TCriminal;
        begin
          crim := FindCriminalInSystem(CriminalName);
          crim.TrialLength := 0;
          for i := 1 to (crim.CrimPolRecord.Count) do
            begin
              if crim.CrimPolRecord.Values['Charge' + IntToStr(i)] = 'InformaticCrime'
              then
                if crim.TrialLength < 30
                then
                  crim.TrialLength := 30;
              if crim.CrimPolRecord.Values['Charge' + IntToStr(i)] = 'BankRobbery'
              then
                if crim.TrialLength < 50
                then
                  crim.TrialLength := 50;
              if crim.CrimPolRecord.Values['Charge' + IntToStr(i)] = 'BankRobberyWithSafeWreckage'
              then
                if crim.TrialLength < 70
                then
                  crim.TrialLength := 70;
              if crim.CrimPolRecord.Values['Charge' + IntToStr(i)] = 'BankRobberyWithSafeHacking'
              then
                if crim.TrialLength < 70
                then
                  crim.TrialLength := 70;
              if crim.CrimPolRecord.Values['Charge' + IntToStr(i)] = 'Murder'
              then
                if crim.TrialLength < 120
                then
                  crim.TrialLength := 120;
              if crim.CrimPolRecord.Values['Charge' + IntToStr(i)] = 'AttemptedMurder'
              then
                if crim.TrialLength < 100
                then
                  crim.TrialLength := 100;
              if crim.CrimPolRecord.Values['Charge' + IntToStr(i)] = 'Assault'
              then
                if crim.TrialLength < 20
                then
                  crim.TrialLength := 20;
              if crim.CrimPolRecord.Values['Charge' + IntToStr(i)] = 'AggravatedAssault'
              then
                if crim.TrialLength < 40
                then
                  crim.TrialLength := 40;
            end;
        end;

      procedure TIllegalSystem.TrialOutcome(CriminalName : widestring);
        var
          i : integer;
          crim : TCriminal;
        begin
          crim := FindCriminalInSystem(CriminalName);
          crim.TrialLength := 0;
          for i := 0 to (crim.CrimPolRecord.Count - 1) do
            begin
              if crim.CrimPolRecord.Values['Charge' + IntToStr(i)] = 'InformaticCrime'
              then
                if crim.TrialLength < 30
                then
                  crim.TrialLength := 30;
              if crim.CrimPolRecord.Values['Charge' + IntToStr(i)] = 'BankRobbery'
              then
                if crim.TrialLength < 50
                then
                  crim.TrialLength := 50;
              if crim.CrimPolRecord.Values['Charge' + IntToStr(i)] = 'BankRobberyWithSafeWreckage'
              then
                if crim.TrialLength < 70
                then
                  crim.TrialLength := 70;
              if crim.CrimPolRecord.Values['Charge' + IntToStr(i)] = 'BankRobberyWithSafeHacking'
              then
                if crim.TrialLength < 70
                then
                  crim.TrialLength := 70;
              if crim.CrimPolRecord.Values['Charge' + IntToStr(i)] = 'Murder'
              then
                if crim.TrialLength < 120
                then
                  crim.TrialLength := 120;
              if crim.CrimPolRecord.Values['Charge' + IntToStr(i)] = 'AttemptedMurder'
              then
                if crim.TrialLength < 100
                then
                  crim.TrialLength := 100;
              if crim.CrimPolRecord.Values['Charge' + IntToStr(i)] = 'Assault'
              then
                if crim.TrialLength < 20
                then
                  crim.TrialLength := 20;
              if crim.CrimPolRecord.Values['Charge' + IntToStr(i)] = 'AggravatedAssault'
              then
                if crim.TrialLength < 40
                then
                  crim.TrialLength := 40;
            end;
        end;

      function TIllegalSystem.UseSkill(SkillValue : single; Difficulty : single): integer;
        var
          success    : single;
          roll       : integer;
        begin
          success := 60 + skillValue - Difficulty;
          roll := Random(100);
          if roll < (success / 3)
          then
            Result := 1;
          if (roll <= (success)) and (roll >= (success / 3))
          then
            Result := 2;
          if (roll > (success)) and (roll <= (success + ((100 - success) / 3 * 2)))
          then
            Result := 3;
          if roll > (success + ((100 - success) / 3 * 2))
          then
            Result := 4;
        end;

      procedure TIllegalSystem.PropagateTheBeat;
        var
          i : integer;
          j : integer;
          k : integer;
        begin
          IllSystem.Leaders.Lock;
          try
            for i := 0 to (IllSystem.Leaders.Count - 1) do
              begin
                TLeader(IllSystem.Leaders[i]).Act;
                TLeader(IllSystem.Leaders[i]).Teams.Lock;
                try
                  for j := 0 to (TLeader(IllSystem.Leaders[i]).Teams.Count - 1) do
                    TTeam(TLeader(IllSystem.Leaders[i]).Teams[j]).Act;
                finally
                  TLeader(IllSystem.Leaders[i]).Teams.Unlock;
                end;
              end;
          finally
            IllSystem.Leaders.Unlock;
          end;
          IllSystem.Criminals.Lock;
          try
            for k := 0 to (IllSystem.Criminals.Count - 1) do
              TCriminal(IllSystem.Criminals[k]).Act;
          finally
            IllSystem.Criminals.Unlock;
          end;
        end;

      function  TIllegalSystem.GetId : string;
        begin
          result := tidRDOHook_IB;
        end;

      procedure TIllegalSystem.EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer );
        begin
          case PeriodType of
            perHour :
              while PeriodCount > 0 do
                begin
                  PropagateTheBeat;
                  dec( PeriodCount );
                end;
          end;
        end;

      procedure TIllegalSystem.Loaded( aWorld : TWorld );
        begin
          // Do stuff here after everything is loaded (only if necessary)
        end;

      procedure TIllegalSystem.CompanyCreated( Company  : TCompany );
        begin
        end;

      procedure TIllegalSystem.TycoonCreated( Tycoon : TTycoon );
        begin
        end;

      procedure TIllegalSystem.FacilityCreated( Facility : TFacility );
        begin
        end;

      procedure TIllegalSystem.CompanyDeleted( Company : TCompany );
        begin
        end;

      procedure TIllegalSystem.TycoonDeleted( Tycoon : TTycoon );
        begin
        end;

      procedure TIllegalSystem.FacilityDeleted( Facility : TFacility );
        begin
        end;

end.


