unit IllegalKernel;

interface

  uses                       
    ShareMem, ClassStorageInt, classes, Collection, extctrls, Graphics, comobj,
    Kernel, World, Windows, BackupInterfaces, MetaCrime, CrimeProtocol;

  type
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

      TMetaServerMission =
        class( TMetaMission )
          public
            constructor Create( anId          : TClassID;
                                aName         : widestring;
                                aDesc         : widestring;
                                aMissType     : widestring;
                                aMissionClass : CMission );
          private
            fMissionClass : CMission;
          public
            property MissionClass : CMission read fMissionClass;
          public
            function Instantiate( Owner : TTeam ) : TMission;
        end;

      //Instances Declarations

      TAttribute =
      class
        public
          constructor Create( aOwner : TCriminal; aMetaAttribute : TMetaAttribute; aValue : single ); virtual;
        private
          fValue          : single;
          fOwner          : TCriminal;
          fMetaAttribute  : TMetaAttribute;
          fInTraining     : boolean;
        public
          property Value         : single         read fValue         write fValue;
          property Owner         : TCriminal      read fOwner         write fOwner;
          property MetaAttribute : TMetaAttribute read fMetaAttribute write fMetaAttribute;
          property InTraining    : boolean        read fInTraining    write fInTraining;
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

      TTargetType =
        (ttpCoordinates, ttpRoad, ttpCriminal, ttpTeam, ttpPlayer, ttpLeader, ttpCity, ttpFacility, ttpCompany, ttpFacilitiesFromPlayer, ttpFacilitiesFromPlayerInRadius, ttpFacilitiesFromCompany, ttpFacilitiesFromCompanyInRadius, ttpFacilitiesInRadius);

      TCriminalState =
        (tcsNoState, tcsOnTheMarket, tcsOnTheMarketHidden, tcsInTeamStandBy, tcsInTeamTraining, tcsInTeamInMission, tcsInTeamInMissionDead, tcsInJail, tcsHiddenByPolice, tcsInTeamDead, tcsInTeamInJail);

      TMissionOptionInfo =
        record
          Id            : widestring;
          way           : widestring;
          compulsory    : boolean;
          Roles         : array[0..(MaxRoles - 1)] of widestring;
          Descriptions  : array[0..(MaxRoles - 1)] of widestring;
          Skills        : array[0..(MaxRoles - 1)] of widestring;
          SkillValues   : array[0..(MaxRoles - 1)] of single;
          Sub           : boolean;
          ParentOptionId: widestring;
          Objective     : widestring;
          ObjCoord      : TCoordinate;
          Target        : TTargetType;
          MaxRadius     : integer;
          MaxDistance   : integer;
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
          Result     : array[1..8] of widestring;
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
          Status : TCriminalState;
          Skill : single;
        end;

      TMission =
      class
        protected
          constructor Create( aOwner : TTeam; aMetaMission : TMetaServerMission ); virtual;
        public
          destructor Destroy; override;
        private
          fOwner        : TTeam;
          fMetaMission  : TMetaServerMission;
          fRoles        : TRolesInMission;
          fParameters   : TParametersInMission;
          fTargetDescr  : string;
          fTargetCoord  : TCoordinate;
          fTargetName   : string;
          fTargetType   : TTargetType;
          fTargetFacType: string;
          fTargetPlayer : string;
          fRadius       : integer;
          fStartingTime : integer;
          fChange       : widestring;
          fMissionTime  : integer;
          fCost         : single;
          fProfit       : single;
          fReport       : TStringList;
          fNextDiff     : single;
          fReady        : boolean;
          fCharges      : TStringList;
          fExperience   : TStringList;
        public
          property Owner : TTeam read fOwner write fOwner;
          property Roles : TRolesInMission read fRoles write fRoles;
          property Parameters : TParametersInMission read fParameters write fParameters;
          property MetaMission : TMetaServerMission read fMetaMission write fMetaMission;
          property TargetDescr : string read fTargetDescr write fTargetDescr;
          property TargetCoord : TCoordinate read fTargetCoord write fTargetCoord;
          property TargetName : string read fTargetName write fTargetName;
          property TargetType : TTargetType read fTargetType write fTargetType;
          property TargetFacType : string read fTargetFacType write fTargetFacType;
          property TargetPlayer : string read fTargetPlayer write fTargetPlayer;
          property Radius : integer read fRadius write fRadius;
          property StartingTime : integer read fStartingTime write fStartingTime;
          property Change : widestring read fChange write fChange;
          property MissionTime : integer read fMissionTime write fMissionTime;
          property Cost : single read fCost write fCost;
          property Profit : single read fProfit write fProfit;
          property Report : TStringList read fReport write fReport;
          property NextDiff : single read fNextDiff write fNextDiff;
          property Ready : boolean read fReady write fReady;
          property Charges : TStringList read fCharges write fCharges;
          property Experience : TStringList read fExperience write fExperience;
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
          constructor Create( aTeamOwner : TTeam; aCriminalOwner : TCriminal; aLeaderOwner  : TLeader; aMetaHistoryItem : TMetaHistoryItem; aDate : integer; aEvent : widestring; aParam1 : widestring; aParam2 : widestring ); virtual;
        private
          //fDate         : TDateTime;
          fDate             : integer;
          fEvent            : widestring;
          fParam1           : widestring;
          fParam2           : widestring;
          fCriminalOwner    : TCriminal;
          fTeamOwner        : TTeam;
          fLeaderOwner      : TLeader;
          fMetaHistoryItem  : TMetaHistoryItem;
        public
          //property Date        : TDateTime read fDate write fDate;
          property Date            : integer read fDate write fDate;
          property Event           : widestring read fEvent write fEvent;
          property Param1          : widestring read fParam1 write fParam1;
          property Param2          : widestring read fParam2 write fParam2;
          property CriminalOwner   : TCriminal read fCriminalOwner write fCriminalOwner;
          property LeaderOwner     : TLeader read fLeaderOwner write fLeaderOwner;
          property TeamOwner       : TTeam read fTeamOwner write fTeamOwner;
          property MetaHistoryItem : TMetaHistoryItem read fMetaHistoryItem write fMetaHistoryItem;
      end;

      TCharge        =
      class
        public
          constructor Create( aOwner : TCriminal; aMetaCharge : TMetaCharge; aMission : widestring; aDate : integer; aLeaderName : widestring; aTeamName : widestring; aLeaderCharge : widestring); virtual;
        private
          fOwner           : TCriminal;
          fMetaCharge      : TMetaCharge;
          fMission         : widestring;
          fDate            : integer;
          fLeaderName      : widestring;
          fTeamName        : widestring;
          fLeaderCharge    : widestring;
        public
          property MetaCharge  : TMetaCharge read fMetaCharge write fMetaCharge;
          property Owner       : TCriminal  read fOwner write fOwner;
          property Mission     : widestring read fMission write fMission;
          property Date        : integer    read fDate   write fDate;
          property LeaderName  : widestring read fLeaderName write fLeaderName;
          property TeamName    : widestring read fTeamName write fTeamName;
          property LeaderCharge: widestring read fLeaderCharge write fLeaderCharge;
        end;

      TLeaderCharge        =
      class
        public
          constructor Create( aOwner : TLeader; aMetaCharge : TMetaCharge; aMission : widestring; aDate : integer); virtual;
        private
          fOwner           : TLeader;
          fMetaCharge      : TMetaCharge;
          fMission         : widestring;
          fDate            : integer;
        public
          property MetaCharge  : TMetaCharge read fMetaCharge write fMetaCharge;
          property Owner       : TLeader  read fOwner write fOwner;
          property Mission     : widestring read fMission write fMission;
          property Date        : integer    read fDate   write fDate;
        end;

      TCriminal      =
      class
        public
          constructor Create( aOwner : TIllegalSystem; aName : widestring; aMetaCriminal : TMetaCriminal); virtual;
          destructor Destroy; override;
        private
          fMetaCriminal    : TMetaCriminal;
          fName            : widestring;
          fAge             : integer;
          fBirthday        : integer;
          fHealth          : integer;
          fOwner           : TIllegalSystem;
          fLeaderName      : widestring;
          fTeamName        : widestring;
          fAttributes      : TCollection;
          fHistoryItems    : TLockableCollection;
          fPicture         : widestring;
          fState           : TCriminalState;
          fSalary          : single;
          fSalaryPerc      : integer;
          fTrainingStage   : LongInt;
          fTrainingClass   : TAttributeModifier;
          fChange          : boolean;
          fLocation        : TCoordinate;
          fCrimPolRecord   : TLockableCollection;
          fCrimPenRecord   : TLockableCollection;
          fWarrant         : single;
          fJailTime        : integer;
          fTrialDay        : integer;
          fTrialEndDay     : integer;
          fLawyersHours    : integer;
          fBribing         : single;
        public
          property MetaCriminal : TMetaCriminal read fMetaCriminal write fMetaCriminal;
          property Picture      : widestring read fPicture write fPicture;
          property Name         : widestring read fName write fName;
          property Age          : integer read fAge write fAge;
          property Birthday     : integer read fBirthday;
          property Health       : integer read fHealth write fHealth;
          property LeaderName   : widestring read fLeaderName write fLeaderName;
          property TeamName     : widestring read fTeamName write fTeamName;
          property Attributes   : TCollection read fAttributes;
          property HistoryItems : TLockableCollection read fHistoryItems;
          property Owner        : TIllegalSystem read fOwner write fOwner;
          property State        : TCriminalState read fState write fState;
          property Salary       : single read fSalary write fSalary;
          property SalaryPerc   : integer read fSalaryPerc write fSalaryPerc;
          property TrainingStage: LongInt read fTrainingStage write fTrainingStage;
          property TrainingClass: TAttributeModifier read fTrainingClass write fTrainingClass;
          property Change       : boolean read fChange write fChange;
          property Location     : TCoordinate read fLocation write fLocation;
          property CrimPolRecord: TLockableCollection read fCrimPolRecord;
          property CrimPenRecord: TLockableCollection read fCrimPenRecord;
          property Warrant      : single read fWarrant write fWarrant;
          property JailTime     : integer read fJailTime write fJailTime;
          property TrialDay     : integer read fTrialDay write fTrialDay;
          property LawyersHours : integer read fLawyersHours write fLawyersHours;
          property TrialEndDay  : integer read fTrialEndDay write fTrialEndDay;
          property Bribing      : single read fBribing write fBribing;
        public
          procedure AddAttribute( Attribute : TAttribute );
          procedure AddHistoryItem( HistoryItem : THistoryItem );
          procedure AddCrimPolRecord( CrimPolRecord : TCharge );
          procedure AddCrimPenRecord( CrimPenRecord : TCharge );
          procedure FinishedTraining(AttClass : TAttributeModifier);
          procedure OutOfJail;
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
          fLawyersHours : integer;
          fBribing      : single;
        public
          property Name         : widestring read fName write fName;
          property Owner        : TLeader read fOwner write fOwner;
          property Headquarter  : TCoordinate read fHeadquarter write fHeadquarter;
          property Mission      : TMission read fMission write fMission;
          property Roles        : TLockableCollection read fRoles;
          property HistoryItems : TLockableCollection read fHistoryItems;
          property LawyersHours : integer read fLawyersHours write fLawyersHours;
          property Bribing      : single read fBribing write fBribing;
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

      TLeader =
      class
        public
          constructor Create( aOwner : TIllegalSystem; aName, aRealName : widestring; aPicture : widestring); virtual;
          destructor Destroy; override;
        private
          fOwner       : TIllegalSystem;
          fName        : widestring;
          fRealName    : widestring;
          fMoney       : single;
          fExpenses    : single;
          fReputation  : LongInt;
          fHistoryItems: TLockableCollection;
          fTeams       : TLockableCollection;
          fPicture     : widestring;
          fCharges     : TLockableCollection;
          fNameList    : TStringList;
          fNLChange    : integer;
          fWarrant     : single;
          fJailTime    : integer;
          fTrialDay    : integer;
          fTrialEndDay : integer;
          fLawyersHours: integer;
          fBribing     : single;
        public
          property Picture      : widestring read fPicture write fPicture;
          property Name         : widestring read fName write fName;
          property RealName     : widestring read fRealName write fRealName;
          property Money        : single read fMoney write fMoney;
          property Reputation   : LongInt read fReputation write fReputation;
          property Expenses     : single read fExpenses write fExpenses;
          property Owner        : TIllegalSystem read fOwner write fOwner;
          property HistoryItems : TLockableCollection read fHistoryItems;
          property Teams        : TLockableCollection read fTeams;
          property Charges      : TLockableCollection read fCharges;
          property NameList     : TStringList read fNameList write fNameList;
          property NLChange     : integer read fNLChange write fNLChange;
          property Warrant      : single read fWarrant write fWarrant;
          property JailTime     : integer read fJailTime write fJailTime;
          property TrialDay     : integer read fTrialDay write fTrialDay;
          property LawyersHours : integer read fLawyersHours write fLawyersHours;
          property TrialEndDay  : integer read fTrialEndDay write fTrialEndDay;
          property Bribing      : single read fBribing write fBribing;
        public
          procedure AddTeam( Team : TTeam );
          procedure DismTeam( Team : TTeam );
          procedure AddHistoryItem( HistoryItem : THistoryItem );
          procedure AddCharge( Charge : TLeaderCharge );
          function  CalcExpenses : single;
          procedure ChangeLevel;
          procedure Act;
          function  FindTeam( name : widestring ) : TTeam;
      end;

      {$M+}
      TIllegalSystem =
      class( TWorldExtension )
        public
          constructor Create;
          destructor Destroy; override;
        private
          fTime             : integer;
          fCriminals        : TLockableCollection;
          fLeaders          : TLockableCollection;
          fBeat             : TNotifyEvent;
        public
          Charges          : array[1..MaxCharges] of TCriminalCharge;
          FirstMNameList   : TStringList;
          FirstFNameList   : TStringList;
          LastNameList     : TStringList;
          CrimListChange   : integer;
        public
          property Criminals    : TLockableCollection read fCriminals;
          property Leaders      : TLockableCollection read fLeaders;
          property Time         : integer read fTime write fTime;
        published
          function  RDOGetCriminalNames(LeaderName : widestring): olevariant;
          function  RDOGetMissionReport(LeaderName: widestring; TeamName : widestring) : olevariant;
          function RDOCreateLeader(Name, RealName : widestring; Picture : widestring) : olevariant;
          function  RDOCreateTeam(LeaderName : widestring; Name : widestring) : olevariant;
          function RDOChangeRole(LeaderName : widestring; TeamName : widestring; CriminalName : widestring; NewRoleName : widestring): olevariant;
          function  RDOFindLeaderName(RealName : widestring) : olevariant;
          function  RDOFindLeader(LeaderName : widestring) : olevariant;
          function  RDOGetTeams(LeaderName : widestring) : olevariant;
          function  RDOFindTeam(LeaderName : widestring; TeamName : widestring) : olevariant;
          function  RDOFindCriminal(CriminalName : widestring) : olevariant;
          function RDOHireCriminal(LeaderName : widestring; TeamName : widestring; CriminalName : widestring; SalaryPerc : integer) : olevariant;
          function RDOFireCriminal(LeaderName : widestring; TeamName : widestring; CriminalName : widestring) : olevariant;
          function RDODismissTeam(LeaderName : widestring; TeamName : widestring) : olevariant;
          function RDOChangeCriminalState(CriminalName : widestring; NewState : integer) : olevariant;
          function RDOChangeTeam(LeaderName : widestring; OldTeamName : widestring; NewTeamName : widestring; CriminalName : widestring) : olevariant;
          function RDOCriminalShowed(CrimName : widestring) : olevariant;
          function RDOMissionChangeShowed(LeaderName: widestring; TeamName : widestring) : olevariant;
          function RDOStopCriminalTraining(CriminalName : widestring) : olevariant;
          function RDOCriminalTraining(CriminalName : widestring; TrainingName : widestring) : olevariant;
          function  RDOGetCriminalTrainingInfo(CriminalName : widestring) : olevariant;
          function RDOAssignHeadquarter(LeaderName : widestring; TeamName : widestring; CoordinateX : integer; CoordinateY : integer) : olevariant;
          function  RDOCalculateSalary(LeaderName : widestring; TeamName : widestring) : olevariant;
          function  RDOCalculatePercentage(LeaderName : widestring; TeamName : widestring) : olevariant;
          function  RDOCalculateTrainingCost(LeaderName : widestring; TeamName : widestring) : olevariant;
          function  RDOCalculateLeaderExpenses(LeaderName : widestring) : olevariant;
          function RDOChangeCriminalSalary(LeaderName : widestring; CriminalName : widestring; SalaryP : integer) : olevariant;
          function  RDORecoveryHistoryItem(LeaderName : widestring; TeamName : widestring; CriminalName : widestring) : olevariant;
          function  RDORecoveryMissionInfos(MissionName : widestring) : olevariant;
          function  RDORecoveryMissionOption(MissionName : widestring; OptionIndex : integer) : olevariant;
          function  RDORecoveryCriminalPendingRecord(CriminalName : widestring) : olevariant;
          function  RDORecoveryCriminalPoliceRecord(CriminalName : widestring) : olevariant;
          function  RDORecoveryCriminalJailDetails(CriminalName : widestring) : olevariant;
          function RDOAssignMission(LeaderName : widestring; TeamName : widestring; MissionName : widestring; RolesList : widestring; ParametersList : widestring; Objective : widestring; ObjectiveX : integer; ObjectiveY : integer; TargetType : integer; TargetName : widestring; Radius : integer; TargetPlayer : widestring; TargetFacType: widestring; Cost : widestring) : olevariant;
          function RDODeassignMission(LeaderName : widestring; TeamName : widestring; Cancel : widestring; MissionName : widestring) : olevariant;
          function RDOStartMission(LeaderName : widestring; TeamName : widestring; MissionName : widestring) : olevariant;
          function RDOGetTime : olevariant;
          function RDOCheckCriminalList(LeaderName : widestring) : olevariant;
          function RDOChangeReputation(LeaderName : widestring; NewRep : integer) : olevariant;
        published
          function RDOChangeJailParameters(LeaderName : widestring; CrimName : widestring; TeamName : widestring; LawyersHours : widestring; Bribing : widestring) : olevariant;
          function RDOGetLawyersHoursCost : olevariant;
          function RDOGetCityList : olevariant;
          function RDOGetCriminalListByLeader(LeaderName : widestring) : olevariant;
          function RDOGetCompanyList(LeaderName : widestring) : olevariant;
          function RDOGetPlayerList : olevariant;
          function RDOGetTeamListByLeader(LeaderName : widestring) : olevariant;
          function RDOGetLeaderList : olevariant;
          function RDOGetFacilityTypeList : olevariant;
        public
          procedure HandleCriminalsDisponibility;
          procedure PrepareCriminalNames(LeaderName : widestring);
          procedure CreateCriminal(Name : widestring; MetaCriminal : TMetaCriminal);
          procedure AddCriminal( Criminal : TCriminal );
          procedure AddLeader( Leader : TLeader );
          procedure Act;
          function CriminalWrapper(Criminal : TCriminal): TStringList;
          function TeamWrapper(Team : TTeam): TStringList;
          function LeaderWrapper(Leader : TLeader): TStringList;
          function HistoryItemsWrapper(HistoryItems : TLockableCollection) : TStringList;
          function ChargesWrapper(Charges : TLockableCollection) : TStringList;
          procedure Start;
          procedure CreateRole(Owner : TTeam; MetaRole : TMetaRole);
          function FindLeaderInSystem(LeaderName : widestring): TLeader;
          function FindTeamInSystem(LeaderName : widestring; TeamName : widestring): TTeam;
          function FindCriminalInSystem(CriminalName : widestring): TCriminal;
          procedure CriminalQuit(LeaderName : widestring; TeamName : widestring; CriminalName : widestring);
          procedure ChangeLeaderMoney(LeaderName : widestring; Money : single);
          procedure GiveChargesToCriminal(CriminalName : widestring);
          procedure GiveChargesToLeader(LeaderName : widestring; MissionName : widestring; Date : integer; Charges : TStringList);
          procedure CriminalArrested(CriminalName : widestring);
          function GetPrisonLocation(CriminalLocation : TCoordinate) : TCoordinate;
          function GetHospitalLocation(CriminalLocation : TCoordinate) : TCoordinate;
          function GetHiddenLocation : TCoordinate;
          procedure SetTrialDay(CriminalName : widestring);
          procedure TrialEnd(CriminalName : widestring);
          procedure TrialOutcome(CriminalName : widestring);
          procedure CriminalConfession(CriminalName : widestring);
          procedure SetLeaderTrialDay(LeaderName : widestring; speed : integer);
          procedure LeaderOnTrial(LeaderName : widestring);
          procedure LeaderTrialOutcome(LeaderName : widestring);
          function GetLeaderLevel(Reputation : integer): integer;
          function UseSkill(SkillValue : single; Difficulty : single): integer;
          function GenerateCriminalName(sex : string): string;
          function CriminalWounded(CrimName : string; WoundLevel : integer) : integer;
          procedure PropagateTheBeat;
        published
          property Beat : TNotifyEvent read fBeat write fBeat;
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
        // Backup
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;
      {$M-}

  var
    ClassId    : TClassID;
    IllSystem  : TIllegalSystem;

  const
    tidRDOHook_IB = 'IB';
    TargetType : array[0..11] of string = ('ttpCoordinates', 'ttpRoad', 'ttpCriminal', 'ttpPlayer', 'ttpLeader', 'ttpCity', 'ttpFacility', 'ttpCompany', 'ttpFacilitiesFromPlayer', 'ttpFacilitiesFromPlayerInRadius', 'ttpFacilitiesFromCompanyInRadius', 'ttpFacilitiesInRadius');


  procedure RegisterBackup;

implementation

  uses
    Forms, SysUtils, BackupObjects, ClassStorage;

    constructor TMetaServerMission.Create( anId          : TClassID;
                                     aName         : widestring;
                                     aDesc         : widestring;
                                     aMissType     : widestring;
                                     aMissionClass : CMission );
      begin
        inherited Create( anId, aName, aDesc, aMissType );
        fMissionClass := aMissionClass;
      end;

    function TMetaServerMission.Instantiate( Owner : TTeam ) : TMission;
      begin
        result := fMissionClass.Create( Owner, self );
      end;

      
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
        fExperience := TStringList.Create;
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
        result.compulsory := False;
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
        k : integer;
        l : integer;
        criminal   : TCriminal;
        CrimName   : widestring;
        SkillValue  : single;
        CrimChosen  : TCrimForTask;
        s1, s2: string;
        RoleToFill : string;
      begin
        CrimName := '';
        SkillValue := 0;
        for i := 1 to 8 do
          begin
            if Roles.Name[i] <> ''
            then
              begin
                if Role = Roles.Role[i]
                then
                  begin
                    criminal := IllSystem.FindCriminalInSystem(Roles.Name[i]);
                    for l := 0 to (criminal.Attributes.Count - 1) do
                      if TAttribute(criminal.Attributes[l]).MetaAttribute.Name = skill
                      then
                        case criminal.Health of
                          0..30: ;
                          31..50:
                            begin
                              CrimName := Roles.Name[i];
                              SkillValue := Round(TAttribute(criminal.Attributes[l]).Value / 2);
                            end;
                          51..70:
                            begin
                              CrimName := Roles.Name[i];
                              SkillValue := Round((TAttribute(criminal.Attributes[l]).Value / 3) * 2);
                            end;
                        else
                          begin
                            CrimName := Roles.Name[i];
                            SkillValue := TAttribute(criminal.Attributes[l]).Value;
                          end;
                        end;
                  end;
              end;
          end;
        if CrimName = ''
        then
          begin
            for k := 0 to (TheClassStorage.ClassCount['Role'] - 1) do
              begin
                s1 := Role;
                s2 := TMetarole(TheClassStorage.ClassByIdx['Role', k]).Name;
                if StrLComp(PChar(s1), PChar(s2), StrLen(PChar(s1))) = 0
                then
                  RoleToFill := TMetarole(TheClassStorage.ClassByIdx['Role', k]).Name;
              end;
            for i := 1 to 8 do
              begin
                if Roles.Name[i] <> ''
                then
                  begin
                    if (RoleToFill + '(Backup)') = Roles.Role[i]
                    then
                      begin
                        criminal := IllSystem.FindCriminalInSystem(Roles.Name[i]);
                        for l := 0 to (criminal.Attributes.Count - 1) do
                          if TAttribute(criminal.Attributes[l]).MetaAttribute.Name = skill
                          then
                            case criminal.Health of
                              0..30: ;
                              31..50:
                                begin
                                  CrimName := Roles.Name[i];
                                  SkillValue := Round(TAttribute(criminal.Attributes[l]).Value / 2);
                                end;
                              51..70:
                                begin
                                  CrimName := Roles.Name[i];
                                  SkillValue := Round((TAttribute(criminal.Attributes[l]).Value / 3) * 2);
                                end;
                            else
                              begin
                                CrimName := Roles.Name[i];
                                SkillValue := TAttribute(criminal.Attributes[l]).Value;
                              end;
                            end;
                      end;
              end;
          end;
        if CrimName = ''
        then
          for i := 1 to 8 do
            if Roles.Name[i] <> ''
            then
              begin
                criminal := IllSystem.FindCriminalInSystem(Roles.Name[i]);
                for l := 0 to (criminal.Attributes.Count - 1) do
                  if TAttribute(criminal.Attributes[l]).MetaAttribute.Name = skill
                  then
                    case criminal.Health of
                      0..30: ;
                      31..50:
                        begin
                          if Round(TAttribute(criminal.Attributes[l]).Value / 2) > SkillValue
                          then
                            begin
                              CrimName := Roles.Name[i];
                              SkillValue := Round(TAttribute(criminal.Attributes[l]).Value / 2);
                            end;
                        end;
                      51..70:
                        begin
                          if Round((TAttribute(criminal.Attributes[l]).Value / 3) * 2) > SkillValue
                          then
                            begin
                              CrimName := Roles.Name[i];
                              SkillValue := Round((TAttribute(criminal.Attributes[l]).Value / 3) * 2);
                            end;
                        end;
                    else
                      begin
                        CrimName := Roles.Name[i];
                        SkillValue := TAttribute(criminal.Attributes[l]).Value;
                      end;
                    end;
              end;
          end;

         {       for k := 0 to (TheClassStorage.ClassCount['Role'] - 1) do
                  begin
                    s1 := Roles.Role[i];
                    s2 := TMetarole(TheClassStorage.ClassByIdx['Role', k]).Name;
                    if StrLComp(PChar(s1), PChar(s2), StrLen(PChar(s1))) = 0
                    then
                      if (TMetaRole(TheClassStorage.ClassByIdx['Role', k]).Name + '(Backup)') =
                      then
                        begin


                if Role = (Roles.Role[i]
                then
                  begin
                    criminal := IllSystem.FindCriminalInSystem(Roles.Name[i]);
                    case criminal.Health of
                      0..30: ;
                      31..50:
                        begin
                          CrimName := Roles.Name[i];
                          SkillValue := Round(TAttribute(criminal.Attributes[l]).Value / 2);
                        end;
                      51..70:
                        begin
                          CrimName := Roles.Name[i];
                          SkillValue := Round((TAttribute(criminal.Attributes[l]).Value / 3) * 2);
                        end;
                      else
                        begin
                          CrimName := Roles.Name[i];
                          SkillValue := TAttribute(criminal.Attributes[l]).Value;
                        end;
                      end;
                  end;
              end;
          end;
                    then
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
                      begin
                        s1 := Roles.Role[i];
                        s2 := TMetarole(TheClassStorage.ClassByIdx['Role', k]).Name;
                        if StrLComp(PChar(s1), PChar(s2), StrLen(PChar(s1))) = 0
                        then
                          if (TMetaRole(TheClassStorage.ClassByIdx['Role', k]).Name + '(Backup)') = Roles.Role[i]
                          then
                            begin
                              if criminal.State <> TCriminalState(6)
                              then
                                begin
                                  for l := 0 to (criminal.Attributes.Count - 1) do
                                    if TAttribute(criminal.Attributes[l]).MetaAttribute.Name = skill
                                    then
                                      if criminal.State = TCriminalState(5)
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
          end;
        if crimName <> ''
        then
          begin
            if IllSystem.FindCriminalInSystem(crimName).State = TCriminalState(5)
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
              if IllSystem.FindCriminalInSystem(BackupName).State = TCriminalState(6)
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
                          if criminal2.State = TCriminalState(6)
                          then
                            begin
                              if (TAttribute(criminal2.Attributes[m]).Value / 2) > CrimChosen.SkillVal
                              then
                                begin
                                  CrimChosen.Name := criminal2.Name;
                                  CrimChosen.SkillVal := (TAttribute(criminal2.Attributes[m]).Value / 2);
                                end;
                            end
                          else
                            begin
                              if (TAttribute(criminal2.Attributes[m]).Value) > CrimChosen.SkillVal
                              then
                                begin
                                  CrimChosen.Name := criminal2.Name;
                                  CrimChosen.SkillVal := (TAttribute(criminal2.Attributes[m]).Value);
                                end;
                            end;
                        end;
                  end;
              end;
            end;}
        CrimChosen.Name := CrimName;
        CrimChosen.SkillVal := SkillValue;
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
            NumRole := 1;
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
                    if TCriminal(IllSystem.Criminals[i]).State <> TCriminalState(6)
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
              if IllSystem.FindCriminalInSystem(Roles.Name[i]).State <> TCriminalState(6)
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
        fParam1 := aParam1;
        fParam2 := aParam2;
      end;

    constructor TCharge.Create(aOwner : TCriminal; aMetaCharge : TMetaCharge; aMission : widestring; aDate : integer; aLeaderName : widestring; aTeamName : widestring; aLeaderCharge : widestring);
      begin
        inherited Create;
        fOwner := aOwner;
        fMetaCharge := aMetaCharge;
        fMission := aMission;
        fDate := aDate;
        fLeaderName := aLeaderName;
        fTeamName := aTeamName;
        fLeaderCharge := aLeaderCharge;
      end;

    constructor TLeaderCharge.Create( aOwner : TLeader; aMetaCharge : TMetaCharge; aMission : widestring; aDate : integer);
      begin
        inherited Create;
        fOwner := aOwner;
        fMetaCharge := aMetaCharge;
        fMission := aMission;
        fDate := aDate;
      end;

    constructor TCriminal.Create;
      begin
        inherited Create;
        fOwner := aOwner;
        fMetaCriminal := aMetaCriminal;
        fName := aName;
        fAge := (aMetaCriminal.Age - 3) + Random(6);
        fHealth := 100;
        fPicture := aMetaCriminal.Photo;
        fState := TCriminalState(1);
        fTrainingStage := 0;
        fChange := False;
        fSalaryPerc := 100;
        fAttributes := TCollection.Create(0, rkBelonguer);
        fHistoryItems := TLockableCollection.Create(0, rkBelonguer);
        fBirthday := Random(364) + 1;
        fLocation.X := 0;
        fLocation.Y := 0;
        fCrimPolRecord := TLockableCollection.Create(0, rkBelonguer);
        fCrimPenRecord := TLockableCollection.Create(0, rkBelonguer);
        fWarrant := 0;
        fJailTime := 0;
        fTrialDay := 0;
        fTrialEndDay := 0;
        fLawyersHours := 0;
        fBribing := 0;
        fLeaderName := '';
        fTeamName := '';
      end;

    procedure TCriminal.CreateAttributes;
      var
        i : integer;
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
        TrainNumber  : integer;
      begin
        AttrLean := TAttribute.Create(self, TMetaAttribute(TheClassStorage.ClassById['Attribute', '11-Skill:Driving']), 0);
        AddAttribute(AttrLean);
        TrainItems := TCollection.Create(0, rkBelonguer);
        for p := 1 to (TheClassStorage.ClassCount['Attribute'] - 1) do
          begin
            value := TMetaAttribute(TheClassStorage.ClassByIdx['Attribute', p]).Default;
            AttrLean := TAttribute.Create(self, TMetaAttribute(TheClassStorage.ClassByIdx['Attribute', p]), value);
            AddAttribute(AttrLean);
          end;
        for i := 0 to (TheClassStorage.ClassCount['Attribute'] - 1) do
          begin
            if TMetaAttribute(TheClassStorage.ClassByIdx['Attribute', i]).Name = 'Stability'
            then
              TAttribute(Attributes[i]).Value := (StrToInt(MetaCriminal.SkillSet.Values['Stability']) - 5 + Random(9));
            if TMetaAttribute(TheClassStorage.ClassByIdx['Attribute', i]).Name = 'Learning'
            then
              TAttribute(Attributes[i]).Value := TAttribute(Attributes[i]).Value + Random(80);
            TrainNumber := 0;
            if MetaCriminal.SkillSet.Values[TMetaAttribute(TheClassStorage.ClassByIdx['Attribute', i]).Name] = 'Low'
            then
              TrainNumber := 2;
            if MetaCriminal.SkillSet.Values[TMetaAttribute(TheClassStorage.ClassByIdx['Attribute', i]).Name] = 'Middle'
            then
              TrainNumber := 4;
            if MetaCriminal.SkillSet.Values[TMetaAttribute(TheClassStorage.ClassByIdx['Attribute', i]).Name] = 'High'
            then
              TrainNumber := 6;
            if TMetaAttribute(TheClassStorage.ClassByIdx['Attribute', i]).Name = 'Driving'
            then
              for j := 1 to TrainNumber do
                begin
                  ClassBySkill := TCollection.Create(0, rkBelonguer);
                  trained := 'no';
                  k := 0;
                  for l := 0 to (TheClassStorage.ClassCount['AttributeModifier'] - 1) do
                    if TAttributeModifier(TheClassStorage.ClassByIdx['AttributeModifier', l]).Attribute.Name = TMetaAttribute(TheClassStorage.ClassByIdx['Attribute', i]).Name
                    then
                      ClassBySkill.Insert(TheClassStorage.ClassByIdx['AttributeModifier', l]);
                  while trained = 'no' do
                    begin
                      good := True;
                      already := false;
                      foundAll := True;
                      for n := 0 to (TrainItems.Count - 1) do
                        begin
                          if TAttributeModifier(ClassBySkill[k]).Name = THistoryItem(TrainItems[n]).Param1
                          then
                            already := True;
                        end;
                      for m := 0 to (TAttributeModifier(ClassBySkill[k]).PrevModifier.Count - 1) do
                        begin
                          found := false;
                          for q := 0 to (TrainItems.Count - 1) do
                            begin
                              if TAttributeModifier(TAttributeModifier(ClassBySkill[k]).PrevModifier[m]).Name = THistoryItem(TrainItems[q]).Param1
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
                end
            else
              TAttribute(Attributes[i]).Value := TAttribute(Attributes[i]).Value + (TrainNumber * 10);
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
          Result := ((((Average / (fAttributes.Count - 3)) * 100) + (OutSkills * 14400))/ 720)  * SalaryPerc / 100;
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
      var
        lname : string;
      begin
        if Round(IllSystem.Time / 24) = Birthday
        then
          if Frac(IllSystem.Time / 24) = 0
          then
            begin
              Age := Age + 1;
              Change := True;
            end;
        if fState = TCriminalState(7)
        then
          begin
            if TrialDay <> 0
            then
              if IllSystem.Time = TrialDay
              then
                begin
                  TrialDay := 0;
                  IllSystem.CriminalConfession(Name);
                  Change := True;
                end;
            if TrialEndDay <> 0
            then
              if IllSystem.Time = TrialEndDay
              then
                begin
                  TrialEndDay := 0;
                  IllSystem.TrialOutcome(Name);
                  Change := True;
                end;
            if JailTime <> 0
            then
              if JailTime = IllSystem.Time
              then
                begin
                  OutOfJail;
                  JailTime := 0;
                  Change := True;
                end;
          end;
        if fState = TCriminalState(8)
        then
          begin
            if IllSystem.Time = TrialEndDay
              then
                begin
                  lname := LeaderName;
                  IllSystem.CriminalQuit(LeaderName, TeamName, Name);
                  IllSystem.RDOChangeCriminalState(Name, 2);
                  LeaderName := lname;
                  TrialEndDay := 0;
                  Change := True;
                end;
          end;
        if fState = TCriminalState(4)
        then
          begin
            if TrainingStage = TrainingClass.Time
            then
              begin
                Change := True;
                TrainingStage := 0;
                State := TCriminalState(3);
                FinishedTraining(TrainingClass);
                StopTraining;
              end
            else
              TrainingStage := TrainingStage + 1;
          end;
      end;

    procedure TCriminal.OutOfJail;
      begin
        if LeaderName <> ''
        then
          begin
            IllSystem.RDOChangeCriminalState(Name, 3);
            Location := IllSystem.FindTeamInSystem(LeaderName, TeamName).Headquarter;
          end
        else
          IllSystem.RDOChangeCriminalState(Name, 1);
      end;

    procedure TCriminal.FinishedTraining(AttClass : TAttributeModifier);
      var
        i : integer;
        j : integer;
        learnResult : integer;
        gain        : integer;
        skillValue  : single;
      begin
        gain := 0;
        skillValue := 0;
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
                AddHistoryItem(THistoryItem.Create(nil, self, nil, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Criminal11: Training']), IllSystem.Time, 'Finished Class: ' + AttClass.Name + ' Points Gained: ' + IntToStr(gain) + ' of ' + IntToStr(AttClass.value), AttClass.Name, ''));
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

    procedure TCriminal.AddCrimPolRecord( CrimPolRecord : TCharge );
      begin
        fCrimPolRecord.Insert ( CrimPolRecord );
      end;

    procedure TCriminal.AddCrimPenRecord( CrimPenRecord : TCharge );
      begin
        fCrimPenRecord.Insert ( CrimPenRecord );
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
        fLawyersHours := 0;
        fBribing := 0;
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
        TCriminal(TRole(Roles[self.Roles.Count - 1]).Criminal).State := TCriminalState(3);
        TCriminal(TRole(Roles[self.Roles.Count - 1]).Criminal).TeamName := Name;
        TCriminal(TRole(Roles[self.Roles.Count - 1]).Criminal).LeaderName := LeaderName;
        TCriminal(TRole(Roles[self.Roles.Count - 1]).Criminal).Bribing := Bribing;
        TCriminal(TRole(Roles[self.Roles.Count - 1]).Criminal).LawyersHours := LawyersHours;
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
              if TRole(Roles[i]).Criminal.State = TCriminalState(4)
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
              if TRole(Roles[i]).Criminal.State = TCriminalState(4)
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
          if (IllSystem.Time  mod 30) = 0
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
              Mission.Change := 'yes';
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
        fRealName := aRealName;
        fMoney := 10000000;
        fExpenses := 0;
        fReputation := 0;
        fCharges := TLockableCollection.Create(0, rkBelonguer);
        fNameList := TStringList.Create;
        fNLChange := 0;
        fWarrant := 0;
        fJailTime := 0;
        fTrialDay := 0;
        fTrialEndDay := 0;
        fLawyersHours := 0;
        fBribing := 0;
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

    procedure TLeader.AddCharge( Charge : TLeaderCharge );
      begin
        fCharges.Insert ( Charge );
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

    procedure TLeader.ChangeLevel;
      begin
        IllSystem.HandleCriminalsDisponibility;
        IllSystem.PrepareCriminalNames(Name);
        NLChange := 1;
      end;

    procedure TLeader.Act;
      begin
        if TrialDay <> 0
        then
          if TrialDay = IllSystem.Time
          then
            begin
              TrialDay := 0;
              IllSystem.LeaderOnTrial(Name);
            end;
        if TrialEndDay <> 0
        then
          if TrialEndDay = IllSystem.Time
          then
            begin
              TrialEndDay := 0;
              IllSystem.LeaderTrialOutcome(Name);
            end;
        if IllSystem.CrimListChange = 1
        then
          IllSystem.PrepareCriminalNames(Name);
        Expenses := CalcExpenses;
        Money := Money - Expenses;
      end;

    function TLeader.FindTeam( name : widestring ) : TTeam;
      var
        i : integer;
      begin
        Teams.Lock;
        try
          i := 0;
          while (i < Teams.Count) and (TTeam(Teams[i]).Name <> name) do
            inc( i );
          if i < Teams.Count
            then result := TTeam(Teams[i])
            else result := nil;
        finally
          Teams.Unlock;
        end;
      end;

    constructor TIllegalSystem.Create;
      begin
        inherited;
        fCriminals := TLockableCollection.Create(0, rkBelonguer);
        fLeaders := TLockableCollection.Create(0, rkBelonguer);
        FirstMNameList := TStringList.Create;
        FirstFNameList := TStringList.Create;
        LastNameList := TStringList.Create;
        CrimListChange := 0;
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

    function TIllegalSystem.RDOGetCriminalNames(LeaderName : widestring): olevariant;
      var
        leader : TLeader;
      begin
        try
          leader := FindLeaderInSystem(LeaderName);
          Result := leader.NameList.Text;
          leader.NLChange := 0;
        except
          Result := CRIME_ERROR_Unknown;
        end;
      end;

    procedure TIllegalSystem.PrepareCriminalNames(LeaderName : widestring);
      var
        i : integer;
        j : integer;
        k : integer;
        l : integer;
        m : integer;
        n : integer;
        o : integer;
        p : integer;
        leader : TLeader;
        NameList : TStringList;
        CrimLevel : integer;
        MetaNameList : TStringList;
        MetaCrimList : TStringList;
        MetaCrimName : string;
        MetaNumb     : integer;
        already : boolean;
        disp    : boolean;
        TotMetaCrim : integer;
        MaxCrimAvalaible : integer;
      begin
        leader := FindLeaderInSystem(LeaderName);
        NameList := TStringList.Create;
        CrimLevel := IllSystem.GetLeaderLevel(leader.Reputation);
        l := 1;
        m := 0;
        MetaCrimList := TStringList.Create;
        for j := 0 to (TheClassStorage.ClassCount['Criminal'] - 1) do
          if TMetaCriminal(TheClassStorage.ClassByIdx['Criminal', j]).Level = CrimLevel
          then
            begin
              already := False;
              leader.Teams.Lock;
              try
                if leader.Teams.Count <> 0
                then
                  for n := 0 to (leader.Teams.Count - 1) do
                    begin
                      TTeam(leader.Teams[n]).Roles.Lock;
                      try
                        for o := 0 to (TTeam(leader.Teams[n]).Roles.Count - 1) do
                          if TMetaCriminal(TheClassStorage.ClassByIdx['Criminal', j]).Name = TCriminal(TRole(TTeam(leader.Teams[n]).Roles[o]).Criminal).MetaCriminal.Name
                          then
                            already := True;
                      finally
                        TTeam(leader.Teams[n]).Roles.Unlock;
                      end;
                    end;
              finally
                leader.Teams.Unlock;
              end;
              disp := False;
              if already = False
              then
                begin
                  IllSystem.Criminals.Lock;
                  try
                    for p := 0 to (IllSystem.Criminals.Count - 1) do
                      if TCriminal(IllSystem.Criminals[p]).State = TCriminalState(1)
                      then
                        if TCriminal(IllSystem.Criminals[p]).Health = 100
                        then
                          if TCriminal(IllSystem.Criminals[p]).MetaCriminal.Name = TMetaCriminal(TheClassStorage.ClassByIdx['Criminal', j]).Name
                          then
                            disp := true;
                  finally
                    IllSystem.Criminals.Unlock;
                  end;
                  if disp = True
                  then
                    begin
                      m := m + 1;
                      MetaCrimList.Values[IntToStr(m)] := TMetaCriminal(TheClassStorage.ClassByIdx['Criminal', j]).Name;
                    end;
                end;
            end;
        TotMetaCrim := MetaCrimList.Count;
        if TotMetaCrim < 8
        then
          MaxCrimAvalaible := TotMetaCrim
        else
          MaxCrimAvalaible := 8;
        MetaNameList := TStringList.Create;
        while l <= MaxCrimAvalaible do
          begin
            MetaNumb := 0;
            while MetaNumb = 0 do
              begin
                MetaNumb := (Random(m) + 1);
                if MetaCrimList.Values[IntToStr(MetaNumb)] = ''
                then
                  MetaNumb := 0;
              end;
            MetaCrimName := MetaCrimList.Values[IntToStr(MetaNumb)];
            MetaCrimList.Values[IntToStr(MetaNumb)] := '';
            MetaNameList.Clear;
            k := 0;
            IllSystem.Criminals.Lock;
            try
              for i := 0 to (IllSystem.Criminals.Count - 1) do
                if TCriminal(IllSystem.Criminals[i]).MetaCriminal.Name = MetaCrimName
                then
                  begin
                    if TCriminal(IllSystem.Criminals[i]).State = TCriminalState(1)
                    then
                      if TCriminal(IllSystem.Criminals[i]).Health = 100
                        then
                          begin
                            k := k + 1;
                            MetaNameList.Values[IntToStr(k)] := TCriminal(IllSystem.Criminals[i]).Name;
                          end;
                    if TCriminal(IllSystem.Criminals[i]).State = TCriminalState(2)
                    then
                      if TCriminal(IllSystem.Criminals[i]).LeaderName <> LeaderName
                      then
                        begin
                          k := k + 1;
                          MetaNameList.Values[IntToStr(k)] := TCriminal(IllSystem.Criminals[i]).Name;
                        end;
                  end;
            finally
              IllSystem.Criminals.Unlock;
            end;
            if MetaNameList.Count <> 0
            then
              begin
                NameList.Values[IntToStr(l)] := MetaNameList.Values[IntToStr(Random(MetaNameList.Count) + 1)];
                l := l + 1;
              end;
          end;
        if (CrimLevel - 1) > 0
        then
          begin
            m := 0;
            MetaCrimList.Clear;
            for j := 0 to (TheClassStorage.ClassCount['Criminal'] - 1) do
              if TMetaCriminal(TheClassStorage.ClassByIdx['Criminal', j]).Level = (CrimLevel - 1)
              then
                begin
                  already := False;
                  leader.Teams.Lock;
                  try
                    if leader.Teams.Count <> 0
                    then
                      for n := 0 to (leader.Teams.Count - 1) do
                        begin
                          TTeam(leader.Teams[n]).Roles.Lock;
                          try
                            for o := 0 to (TTeam(leader.Teams[n]).Roles.Count - 1) do
                              if TMetaCriminal(TheClassStorage.ClassByIdx['Criminal', j]).Name = TCriminal(TRole(TTeam(leader.Teams[n]).Roles[o]).Criminal).MetaCriminal.Name
                              then
                                already := True;
                          finally
                            TTeam(leader.Teams[n]).Roles.Unlock;
                          end;
                        end;
                  finally
                    leader.Teams.Unlock;
                  end;
                  disp := False;
                  if already = False
                  then
                    begin
                      IllSystem.Criminals.Lock;
                      try
                        for p := 0 to (IllSystem.Criminals.Count - 1) do
                          if TCriminal(IllSystem.Criminals[p]).State = TCriminalState(1)
                          then
                            if TCriminal(IllSystem.Criminals[p]).Health = 100
                            then
                              if TCriminal(IllSystem.Criminals[p]).MetaCriminal.Name = TMetaCriminal(TheClassStorage.ClassByIdx['Criminal', j]).Name
                              then
                                disp := true;
                      finally
                        IllSystem.Criminals.Unlock;
                      end;
                      if disp = True
                      then
                        begin
                          m := m + 1;
                          MetaCrimList.Values[IntToStr(m)] := TMetaCriminal(TheClassStorage.ClassByIdx['Criminal', j]).Name;
                        end;
                    end;
                end;
            TotMetaCrim := MetaCrimList.Count;
            if TotMetaCrim < 4
            then
              begin
                MaxCrimAvalaible := l + TotMetaCrim - 1;
              end
            else
              MaxCrimAvalaible := l + 3;
            while l <= MaxCrimAvalaible do
              begin
                MetaNumb := 0;
                while MetaNumb = 0 do
                  begin
                    MetaNumb := (Random(m - 1) + 1);
                    if MetaCrimList.Values[IntToStr(MetaNumb)] = ''
                    then
                      MetaNumb := 0;
                  end;
                MetaCrimName := MetaCrimList.Values[IntToStr(MetaNumb)];
                MetaCrimList.Values[IntToStr(MetaNumb)] := '';
                MetaNameList.Clear;
                k := 0;
                IllSystem.Criminals.Lock;
                try
                  for i := 0 to (IllSystem.Criminals.Count - 1) do
                    if TCriminal(IllSystem.Criminals[i]).MetaCriminal.Name = MetaCrimName
                    then
                      begin
                        if TCriminal(IllSystem.Criminals[i]).State = TCriminalState(1)
                        then
                          if TCriminal(IllSystem.Criminals[i]).Health = 100
                            then
                              begin
                                k := k + 1;
                                MetaNameList.Values[IntToStr(k)] := TCriminal(IllSystem.Criminals[i]).Name;
                              end;
                        if TCriminal(IllSystem.Criminals[i]).State = TCriminalState(2)
                        then
                          if TCriminal(IllSystem.Criminals[i]).LeaderName <> LeaderName
                          then
                            begin
                              k := k + 1;
                              MetaNameList.Values[IntToStr(k)] := TCriminal(IllSystem.Criminals[i]).Name;
                            end;
                      end;
                finally
                  IllSystem.Criminals.Unlock;
                end;
                if MetaNameList.Count <> 0
                then
                  begin
                    NameList.Values[IntToStr(l)] := MetaNameList.Values[IntToStr(Random(MetaNameList.Count) + 1)];
                    l := l + 1;
                  end;
              end;
          end;
        if (CrimLevel - 2) > 0
        then
          begin
            m := 0;
            MetaCrimList.Clear;
            for j := 0 to (TheClassStorage.ClassCount['Criminal'] - 1) do
              if TMetaCriminal(TheClassStorage.ClassByIdx['Criminal', j]).Level = (CrimLevel - 2)
              then
                begin
                  already := False;
                  leader.Teams.Lock;
                  try
                    if leader.Teams.Count <> 0
                    then
                      for n := 0 to (leader.Teams.Count - 1) do
                        begin
                          TTeam(leader.Teams[n]).Roles.Lock;
                          try
                            for o := 0 to (TTeam(leader.Teams[n]).Roles.Count - 1) do
                              if TMetaCriminal(TheClassStorage.ClassByIdx['Criminal', j]).Name = TCriminal(TRole(TTeam(leader.Teams[n]).Roles[o]).Criminal).MetaCriminal.Name
                              then
                                already := True;
                          finally
                            TTeam(leader.Teams[n]).Roles.Unlock;
                          end;
                        end;
                  finally
                    leader.Teams.Unlock;
                  end;
                  disp := False;
                  if already = False
                  then
                    begin
                      IllSystem.Criminals.Lock;
                      try
                        for p := 0 to (IllSystem.Criminals.Count - 1) do
                          if TCriminal(IllSystem.Criminals[p]).State = TCriminalState(1)
                          then
                            if TCriminal(IllSystem.Criminals[p]).Health = 100
                            then
                              if TCriminal(IllSystem.Criminals[p]).MetaCriminal.Name = TMetaCriminal(TheClassStorage.ClassByIdx['Criminal', j]).Name
                              then
                                disp := true;
                      finally
                        IllSystem.Criminals.Unlock;
                      end;
                      if disp = True
                      then
                        begin
                          m := m + 1;
                          MetaCrimList.Values[IntToStr(m)] := TMetaCriminal(TheClassStorage.ClassByIdx['Criminal', j]).Name;
                        end;
                    end;
                end;
            TotMetaCrim := MetaCrimList.Count;
            if TotMetaCrim < 2
            then
              begin
                MaxCrimAvalaible := l + TotMetaCrim - 1;
              end
            else
              MaxCrimAvalaible := l + 1;
            while l <= MaxCrimAvalaible do
              begin
                MetaNumb := 0;
                while MetaNumb = 0 do
                  begin
                    MetaNumb := (Random(m - 1) + 1);
                    if MetaCrimList.Values[IntToStr(MetaNumb)] = ''
                    then
                      MetaNumb := 0;
                  end;
                MetaCrimName := MetaCrimList.Values[IntToStr(MetaNumb)];
                MetaCrimList.Values[IntToStr(MetaNumb)] := '';
                MetaNameList.Clear;
                k := 0;
                IllSystem.Criminals.Lock;
                try
                  for i := 0 to (IllSystem.Criminals.Count - 1) do
                    if TCriminal(IllSystem.Criminals[i]).MetaCriminal.Name = MetaCrimName
                    then
                      begin
                        if TCriminal(IllSystem.Criminals[i]).State = TCriminalState(1)
                        then
                          if TCriminal(IllSystem.Criminals[i]).Health = 100
                            then
                              begin
                                k := k + 1;
                                MetaNameList.Values[IntToStr(k)] := TCriminal(IllSystem.Criminals[i]).Name;
                              end;
                        if TCriminal(IllSystem.Criminals[i]).State = TCriminalState(2)
                        then
                          if TCriminal(IllSystem.Criminals[i]).LeaderName <> LeaderName
                          then
                            begin
                              k := k + 1;
                              MetaNameList.Values[IntToStr(k)] := TCriminal(IllSystem.Criminals[i]).Name;
                            end;
                      end;
                finally
                  IllSystem.Criminals.Unlock;
                end;
                if MetaNameList.Count <> 0
                then
                  begin
                    NameList.Values[IntToStr(l)] := MetaNameList.Values[IntToStr(Random(MetaNameList.Count) + 1)];
                    l := l + 1;
                  end;
              end;
          end;
        if (CrimLevel - 3) > 0
        then
          begin
            m := 0;
            MetaCrimList.Clear;
            for j := 0 to (TheClassStorage.ClassCount['Criminal'] - 1) do
              if TMetaCriminal(TheClassStorage.ClassByIdx['Criminal', j]).Level = (CrimLevel - 3)
              then
                begin
                  already := False;
                  leader.Teams.Lock;
                  try
                    if leader.Teams.Count <> 0
                    then
                      for n := 0 to (leader.Teams.Count - 1) do
                        begin
                          TTeam(leader.Teams[n]).Roles.Lock;
                          try
                            for o := 0 to (TTeam(leader.Teams[n]).Roles.Count - 1) do
                              if TMetaCriminal(TheClassStorage.ClassByIdx['Criminal', j]).Name = TCriminal(TRole(TTeam(leader.Teams[n]).Roles[o]).Criminal).MetaCriminal.Name
                              then
                                already := True;
                          finally
                            TTeam(leader.Teams[n]).Roles.Unlock;
                          end;
                        end;
                  finally
                    leader.Teams.Unlock;
                  end;
                  disp := False;
                  if already = False
                  then
                    begin
                      IllSystem.Criminals.Lock;
                      try
                        for p := 0 to (IllSystem.Criminals.Count - 1) do
                          if TCriminal(IllSystem.Criminals[p]).State = TCriminalState(1)
                          then
                            if TCriminal(IllSystem.Criminals[p]).Health = 100
                            then
                              if TCriminal(IllSystem.Criminals[p]).MetaCriminal.Name = TMetaCriminal(TheClassStorage.ClassByIdx['Criminal', j]).Name
                              then
                                disp := true;
                      finally
                        IllSystem.Criminals.Unlock;
                      end;
                      if disp = True
                      then
                        begin
                          m := m + 1;
                          MetaCrimList.Values[IntToStr(m)] := TMetaCriminal(TheClassStorage.ClassByIdx['Criminal', j]).Name;
                        end;
                    end;
                end;
            TotMetaCrim := MetaCrimList.Count;
            if TotMetaCrim < 2
            then
              begin
                MaxCrimAvalaible := l + TotMetaCrim - 1;
              end
            else
              MaxCrimAvalaible := l + 1;
            while l <= MaxCrimAvalaible do
              begin
                MetaNumb := 0;
                while MetaNumb = 0 do
                  begin
                    MetaNumb := (Random(m - 1) + 1);
                    if MetaCrimList.Values[IntToStr(MetaNumb)] = ''
                    then
                      MetaNumb := 0;
                  end;
                MetaCrimName := MetaCrimList.Values[IntToStr(MetaNumb)];
                MetaCrimList.Values[IntToStr(MetaNumb)] := '';
                MetaNameList.Clear;
                k := 0;
                IllSystem.Criminals.Lock;
                try
                  for i := 0 to (IllSystem.Criminals.Count - 1) do
                    if TCriminal(IllSystem.Criminals[i]).MetaCriminal.Name = MetaCrimName
                    then
                      begin
                        if TCriminal(IllSystem.Criminals[i]).State = TCriminalState(1)
                        then
                          if TCriminal(IllSystem.Criminals[i]).Health = 100
                            then
                              begin
                                k := k + 1;
                                MetaNameList.Values[IntToStr(k)] := TCriminal(IllSystem.Criminals[i]).Name;
                              end;
                        if TCriminal(IllSystem.Criminals[i]).State = TCriminalState(2)
                        then
                          if TCriminal(IllSystem.Criminals[i]).LeaderName <> LeaderName
                          then
                            begin
                              k := k + 1;
                              MetaNameList.Values[IntToStr(k)] := TCriminal(IllSystem.Criminals[i]).Name;
                            end;
                      end;
                finally
                  IllSystem.Criminals.Unlock;
                end;
                if MetaNameList.Count <> 0
                then
                  begin
                    NameList.Values[IntToStr(l)] := MetaNameList.Values[IntToStr(Random(MetaNameList.Count) + 1)];
                    l := l + 1;
                  end;
              end;
          end;
        if (CrimLevel - 4) > 0
        then
          begin
            m := 0;
            MetaCrimList.Clear;
            for j := 0 to (TheClassStorage.ClassCount['Criminal'] - 1) do
              if TMetaCriminal(TheClassStorage.ClassByIdx['Criminal', j]).Level = (CrimLevel - 4)
              then
                begin
                  already := False;
                  leader.Teams.Lock;
                  try
                    if leader.Teams.Count <> 0
                    then
                      for n := 0 to (leader.Teams.Count - 1) do
                        begin
                          TTeam(leader.Teams[n]).Roles.Lock;
                          try
                            for o := 0 to (TTeam(leader.Teams[n]).Roles.Count - 1) do
                              if TMetaCriminal(TheClassStorage.ClassByIdx['Criminal', j]).Name = TCriminal(TRole(TTeam(leader.Teams[n]).Roles[o]).Criminal).MetaCriminal.Name
                              then
                                already := True;
                          finally
                            TTeam(leader.Teams[n]).Roles.Unlock;
                          end;
                        end;
                  finally
                    leader.Teams.Unlock;
                  end;
                  disp := False;
                  if already = False
                  then
                    begin
                      IllSystem.Criminals.Lock;
                      try
                        for p := 0 to (IllSystem.Criminals.Count - 1) do
                          if TCriminal(IllSystem.Criminals[p]).State = TCriminalState(1)
                          then
                            if TCriminal(IllSystem.Criminals[p]).Health = 100
                            then
                              if TCriminal(IllSystem.Criminals[p]).MetaCriminal.Name = TMetaCriminal(TheClassStorage.ClassByIdx['Criminal', j]).Name
                              then
                                disp := true;
                      finally
                        IllSystem.Criminals.Unlock;
                      end;
                      if disp = True
                      then
                        begin
                          m := m + 1;
                          MetaCrimList.Values[IntToStr(m)] := TMetaCriminal(TheClassStorage.ClassByIdx['Criminal', j]).Name;
                        end;
                    end;
                end;
            TotMetaCrim := MetaCrimList.Count;
            if TotMetaCrim < 2
            then
              begin
                MaxCrimAvalaible := l + TotMetaCrim - 1;
              end
            else
              MaxCrimAvalaible := l + 1;
            while l <= MaxCrimAvalaible do
              begin
                MetaNumb := 0;
                while MetaNumb = 0 do
                  begin
                    MetaNumb := (Random(m - 1) + 1);
                    if MetaCrimList.Values[IntToStr(MetaNumb)] = ''
                    then
                      MetaNumb := 0;
                  end;
                MetaCrimName := MetaCrimList.Values[IntToStr(MetaNumb)];
                MetaCrimList.Values[IntToStr(MetaNumb)] := '';
                MetaNameList.Clear;
                k := 0;
                IllSystem.Criminals.Lock;
                try
                  for i := 0 to (IllSystem.Criminals.Count - 1) do
                    if TCriminal(IllSystem.Criminals[i]).MetaCriminal.Name = MetaCrimName
                    then
                      begin
                        if TCriminal(IllSystem.Criminals[i]).State = TCriminalState(1)
                        then
                          if TCriminal(IllSystem.Criminals[i]).Health = 100
                          then
                            begin
                              k := k + 1;
                              MetaNameList.Values[IntToStr(k)] := TCriminal(IllSystem.Criminals[i]).Name;
                            end;
                        if TCriminal(IllSystem.Criminals[i]).State = TCriminalState(2)
                        then
                          if TCriminal(IllSystem.Criminals[i]).LeaderName <> LeaderName
                          then
                            begin
                              k := k + 1;
                              MetaNameList.Values[IntToStr(k)] := TCriminal(IllSystem.Criminals[i]).Name;
                            end;
                      end;
                finally
                  IllSystem.Criminals.Unlock;
                end;
                if MetaNameList.Count <> 0
                then
                  begin
                    NameList.Values[IntToStr(l)] := MetaNameList.Values[IntToStr(Random(MetaNameList.Count) + 1)];
                    l := l + 1;
                  end;
              end;
          end;
        leader.NameList := NameList;
        CrimListChange := 0;
        leader.NLChange := 1;
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
        List.Values['Health']         := IntToStr(Criminal.Health);
        List.Values['State']          := IntToStr(integer(Criminal.State));
        List.Values['Salary']         := FloatToStr(Criminal.Salary);
        List.Values['SalaryPerc']     := IntToStr(Criminal.SalaryPerc);
        List.Values['TrainingStage']  := IntToStr(Criminal.TrainingStage);
        List.Values['Birthday']       := IntToStr(Criminal.Birthday);
        List.Values['Warrant']        := FloatToStr(Criminal.Warrant);
        List.Values['JailTime']       := IntToStr(Criminal.JailTime);
        List.Values['TrialDay']       := IntToStr(Criminal.TrialDay);
        List.Values['LawyersHours']   := IntToStr(Criminal.LawyersHours);
        List.Values['Bribing']        := FloatToStr(Criminal.Bribing);
        List.Values['TrialEndDay']    := IntToStr(Criminal.TrialEndDay);
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
        List.Values['Bribing'] := FloatToStr(Team.Bribing);
        List.Values['LawyersHours'] := IntToStr(Team.LawyersHours);
        if Team.Mission <> nil
        then
          begin
            List.Values['MissionName'] := Team.Mission.MetaMission.Name;
            List.Values['Objective'] := Team.Mission.TargetDescr;
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

    function TIllegalSystem.RDOGetMissionReport(LeaderName: widestring; TeamName : widestring) : olevariant;
      begin
        try
          Result := FindTeamInSystem(LeaderName, TeamName).Mission.Report.Text;
        except
          Result := CRIME_ERROR_Unknown;
        end;
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
        List := TStringList.Create;
        HistoryItems.Lock;
        try
          begin
            for i := 0 to (HistoryItems.Count - 1) do
              begin
                List.Values['Item' + IntToStr(i) + '-' + 'Date'] := IntToStr(THistoryItem(HistoryItems[i]).Date);
                List.Values['Item' + IntToStr(i) + '-' + 'Event'] := THistoryItem(HistoryItems[i]).Event;
                List.Values['Item' + IntToStr(i) + '-' + 'Param'] := THistoryItem(HistoryItems[i]).Param1;
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

    function TIllegalSystem.ChargesWrapper(Charges : TLockableCollection) : TStringList;
      var
        i : integer;
        List : TStringList;
      begin
        List := TStringList.Create;
        Charges.Lock;
        try
          begin
            for i := 0 to (Charges.Count - 1) do
              begin
                List.Values['Charge' + IntToStr(i + 1)] := TCharge(Charges[i]).MetaCharge.Name;
                List.Values['Mission' + IntToStr(i + 1)] := TCharge(Charges[i]).Mission;
                List.Values['Date' + IntToStr(i + 1)] := IntToStr(TCharge(Charges[i]).Date);
                List.Values['LeaderName' + IntToStr(i + 1)] := TCharge(Charges[i]).LeaderName;
                List.Values['MissionName' + IntToStr(i + 1)] := TCharge(Charges[i]).TeamName;
              end;
            List.Values['ItemNumber'] := IntToStr(Charges.Count - 1);
          end;
        finally
          Charges.Unlock;
        end;
        Result := List;
      end;

    // Running Code

    procedure TIllegalSystem.Start;
      begin
        Randomize;
      end;

    // Create Leader and store him in the system

    function TIllegalSystem.RDOCreateLeader(Name : widestring; RealName : widestring; Picture : widestring): olevariant;
      var
        LeaderLean : TLeader;
      begin
        try
          LeaderLean := TLeader.Create(IllSystem, Name, RealName, Picture);
          LeaderLean.AddHistoryItem(THistoryItem.Create(nil, nil, LeaderLean, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Leader11: LeaderBorn']), IllSystem.Time, Name + ' becomes a criminal boss', '', ''));
          IllSystem.AddLeader(LeaderLean);
          HandleCriminalsDisponibility;
          PrepareCriminalNames(Name);
          Result := CRIME_NOERROR;
        except
          Result := CRIME_ERROR_Unknown;
        end;
      end;

    // Create Criminal and store him in the system

    procedure TIllegalSystem.CreateCriminal(Name : widestring; MetaCriminal : TMetaCriminal);
      var
        i : integer;
        CriminalLean : TCriminal;
      begin
        CriminalLean := TCriminal.Create(IllSystem, Name, MetaCriminal);
        IllSystem.AddCriminal(CriminalLean);
        IllSystem.Criminals.Lock;
        try
          for i := 0 to (IllSystem.Criminals.Count - 1) do
            if CriminalLean.Name = TCriminal(IllSystem.Criminals[i]).Name
            then
              TCriminal(IllSystem.Criminals[i]).CreateAttributes;
        finally
          IllSystem.Criminals.Unlock;
        end;
        //TCriminal(IllSystem.Criminals[IllSystem.Criminals.Count - 1]).CreateAttributes;
      end;

    function TIllegalSystem.RDOCreateTeam(LeaderName : widestring; Name : widestring) : olevariant;
      var
        MaxTeams : integer;
        i        : integer;
        Team     : TTeam;
        Leader   : TLeader;
      begin
        {
        try
          already := 'no';
          leader := FindLeaderinSystem(LeaderName);
          case IllSystem.GetLeaderLevel(leader.Reputation) of
            1 : MaxTeams := 2;
            2 : MaxTeams := 4;
            3 : MaxTeams := 8;
            4 : MaxTeams := 16;
          else
            MaxTeams := -1;
          end;
          Leader.Teams.Lock;
          try
            if ((Leader.Teams.Count >= MaxTeams) and (MaxTeams <> - 1))
            then
              begin
                already := 'na';
              end
            else
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
              TeamLean.AddHistoryItem(THistoryItem.Create(TeamLean, nil, nil, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Team11: TeamCreation']), IllSystem.Time, 'The team is created by ' + LeaderName , '', ''));
              leader.AddTeam(TeamLean);
              leader.AddHistoryItem(THistoryItem.Create(nil, nil, leader, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Leader12: TeamCreated']), IllSystem.Time, 'Founded team ' + Name, '', ''));
              Result := 1;
            end;
          if already = 'yes'
          then
            Result := 2;
          if already = 'na'
          then
            Result := 3;
        except
          Result := CRIME_ERROR_Unknown;
        end;
        }
        try
          Leader := FindLeaderinSystem(LeaderName);
          case IllSystem.GetLeaderLevel(leader.Reputation) of
            1 : MaxTeams := 2;
            2 : MaxTeams := 4;
            3 : MaxTeams := 8;
            4 : MaxTeams := 16;
          else
            MaxTeams := -1;
          end;
          if Leader.Teams.Count <= MaxTeams
            then
              begin
                fLeaders.Lock;
                try
                  i := 0;
                  while (i < fLeaders.Count) and (TLeader(fLeaders[i]).FindTeam( name ) = nil) do
                    inc( i );
                  if i >= fLeaders.Count
                    then
                      begin
                        Team := TTeam.Create(FindLeaderinSystem(LeaderName), Name);
                        Team.AddHistoryItem(THistoryItem.Create(Team, nil, nil, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Team11: TeamCreation']), IllSystem.Time, 'The team is created by ' + LeaderName , '', ''));
                        Leader.AddTeam(Team);
                        Leader.AddHistoryItem(THistoryItem.Create(nil, nil, leader, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Leader12: TeamCreated']), IllSystem.Time, 'Founded team ' + Name, '', ''));
                        result := CRIME_NOERROR;
                      end
                    else result := CRIME_ERROR_WrongTeamName
                finally
                  fLeaders.Unlock;
                end;
              end
            else result := CRIME_ERROR_MaxReached;
        except
          result := CRIME_ERROR_Unknown;
        end;
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
          leader := nil;
          i := 0;
          found := 'no';
          IllSystem.Leaders.Lock;
          try
            while found = 'no' do
              begin
                if IllSystem.Leaders.Count <> 0
                then
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
                  end
                else
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

      function TIllegalSystem.RDOFindLeaderName(RealName : widestring) : olevariant;
        var
          i : integer;
        begin
          try
            Leaders.Lock;
            try
              i := 0;
              RealName := uppercase(RealName);
              while (i < Leaders.Count) and (uppercase(TLeader(Leaders[i]).RealName) <> RealName) do
                inc( i );
              if i < Leaders.Count
                then result := TLeader(Leaders[i]).Name
                else result := '';
            finally
              Leaders.Unlock;
            end;
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      function TIllegalSystem.RDOFindLeader(LeaderName : widestring) : olevariant;
        var
          Leader : TLeader;
        begin
          try
            Leader := FindLeaderInSystem(LeaderName);
            if Leader <> nil
            then
              Result := LeaderWrapper(Leader).Text
            else
              Result := '';
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      function TIllegalSystem.FindTeamInSystem(LeaderName : widestring; TeamName : widestring): TTeam;
        var
          i : integer;
          Leader : TLeader;
          found : widestring;
        begin
          result := nil;
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
                    begin
                      found := 'yes';
                    end;
                  i := i + 1;
              end;
          finally
            Leader.Teams.Unlock;
          end;
        end;

      function TIllegalSystem.RDOGetTeams(LeaderName : widestring) : olevariant;
        var
          Leader : TLeader;
          List   : TStringList;
          i      : integer;
        begin
          try
            Leader := FindLeaderInSystem(LeaderName);
            if Leader <> nil
              then
                begin
                  List := TStringList.Create;
                  try
                    Leader.Teams.Lock;
                    try
                      for i := 0 to pred(Leader.Teams.Count) do
                        List.Add( TTeam(Leader.Teams[i]).Name );
                    finally
                      Leader.Teams.Unlock;
                    end;
                    result := List.Text;
                  finally
                    List.Free;
                  end;
                end
              else result := '';
          except
            Result := CRIME_ERROR_WrongLeaderName;
          end;
        end;

      function TIllegalSystem.RDOFindTeam(LeaderName : widestring; TeamName : widestring) : olevariant;
        var
          Team : TTeam;
        begin
          try
            Team := FindTeamInSystem(LeaderName, TeamName);
            Result := TeamWrapper(Team).Text;
          except
            Result := CRIME_ERROR_WrongLeaderName;
          end;
        end;

      function TIllegalSystem.FindCriminalInSystem(CriminalName : widestring): TCriminal;
        var
          i : integer;
        begin
          result := nil;
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

      function TIllegalSystem.RDOFindCriminal(CriminalName : widestring) : olevariant;
        var
          Criminal : TCriminal;
        begin
          try
            Criminal := FindCriminalInSystem(CriminalName);
            Result   := CriminalWrapper(Criminal).Text;
          except
            Result := CRIME_ERROR_WrongCriminalName;
          end;
        end;

      function TIllegalSystem.RDOChangeRole(LeaderName : widestring; TeamName : widestring; CriminalName : widestring; NewRoleName : widestring): olevariant;
        begin
          try
            FindTeamInSystem(LeaderName, TeamName).RoleChange(CriminalName, NewRoleName);
            Result := CRIME_NOERROR;
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      function TIllegalSystem.RDOHireCriminal(LeaderName : widestring; TeamName : widestring; CriminalName : widestring; SalaryPerc : integer) : olevariant;
        var
          i : integer;
          k : integer;
          crim : TCriminal;
          team : TTeam;
          leader : TLeader;
          CrimLean : TStringList;
        begin
          try
            FindTeamInSystem(LeaderName, TeamName).Hire(LeaderName, CriminalName, SalaryPerc);
            leader := FindLeaderinSystem(LeaderName);
            for i := (leader.NameList.Count - 1) downto 0 do
              if leader.NameList.Values[IntToStr(i + 1)] = CriminalName
              then
                begin
                  leader.NameList.Delete(i);
                  CrimLean := TStringList.Create;
                  CrimLean.Text := leader.NameList.Text;
                  leader.NameList.Clear;
                  for k := 0 to (CrimLean.Count -1) do
                    leader.NameList.Values[IntToStr(k+1)] := CrimLean.Values[CrimLean.Names[k]];
                end;
            leader.NLChange := 1;
            crim := FindCriminalInSystem(CriminalName);
            crim.AddHistoryItem(THistoryItem.Create(nil, crim, nil, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Criminal12: Hire']), IllSystem.Time, 'Hired by ' + LeaderName + ' in team ' + TeamName, '', ''));
            team := FindTeamInSystem(LeaderName, TeamName);
            team.AddHistoryItem(THistoryItem.Create(team, nil, nil, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Team12: HiredCriminal']), IllSystem.Time, CriminalName + ' has been hired in the team', '', ''));
            leader.AddHistoryItem(THistoryItem.Create(nil, nil, leader, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Leader14: CriminalHired']), IllSystem.Time, 'Hired ' + CriminalName + ' in team ' + TeamName, '', ''));
            Result := CRIME_NOERROR;
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      procedure TIllegalSystem.CriminalQuit(LeaderName : widestring; TeamName : widestring; CriminalName : widestring);
        var
          i : integer;
          crim : TCriminal;
          team : TTeam;
          leader : Tleader;
        begin
          RDOChangeRole(LeaderName, TeamName, CriminalName, 'EraseRole');
          crim := FindCriminalInSystem(CriminalName);
          crim.LeaderName := '';
          crim.TeamName := '';
          crim.Bribing := 0;
          crim.LawyersHours := 0;
          for i := 0 to (crim.Attributes.Count - 1) do
            if TAttribute(crim.Attributes[i]).MetaAttribute.Name = 'Loyalty'
            then
              TAttribute(crim.Attributes[i]).Value := 0;
          crim.AddHistoryItem(THistoryItem.Create(nil, crim, nil, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Criminal13: Fire']), IllSystem.Time, 'Fired by ' + LeaderName + ' from team ' + TeamName, LeaderName, FloatToStr(0)));
          team := FindTeamInSystem(LeaderName, TeamName);
          team.AddHistoryItem(THistoryItem.Create(team, nil, nil, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Team13: FiredCriminal']), IllSystem.Time, CriminalName + ' has been fired from the team', '', ''));
          leader := FindLeaderinSystem(LeaderName);
          leader.AddHistoryItem(THistoryItem.Create(nil, nil, leader, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Leader14: CriminalHired']), IllSystem.Time, 'Fired ' + CriminalName + ' from team ' + TeamName, '', FloatToStr(0)));
        end;

      function TIllegalSystem.RDOFireCriminal(LeaderName : widestring; TeamName : widestring; CriminalName : widestring): olevariant;
        var
          i : integer;
          crim : TCriminal;
          team : TTeam;
          leader : Tleader;
          loyalty : single;
        begin
          try
            RDOChangeRole(LeaderName, TeamName, CriminalName, 'EraseRole');
            crim := FindCriminalInSystem(CriminalName);
            crim.LeaderName := '';
            crim.TeamName := '';
            crim.Bribing := 0;
            crim.LawyersHours := 0;
            loyalty := 0;
            for i := 0 to (crim.Attributes.Count - 1) do
              if TAttribute(crim.Attributes[i]).MetaAttribute.Name = 'Loyalty'
              then
                begin
                  if TAttribute(crim.Attributes[i]).Value > 30
                  then
                    TAttribute(crim.Attributes[i]).Value := TAttribute(crim.Attributes[i]).Value - 30
                  else
                    TAttribute(crim.Attributes[i]).Value := 0;
                  loyalty := TAttribute(crim.Attributes[i]).Value;
                end;
            crim.AddHistoryItem(THistoryItem.Create(nil, crim, nil, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Criminal13: Fire']), IllSystem.Time, 'Fired by ' + LeaderName + ' from team ' + TeamName, LeaderName, FloatToStr(loyalty)));
            team := FindTeamInSystem(LeaderName, TeamName);
            team.AddHistoryItem(THistoryItem.Create(team, nil, nil, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Team13: FiredCriminal']), IllSystem.Time, CriminalName + ' has been fired from the team', '', ''));
            leader := FindLeaderinSystem(LeaderName);
            leader.AddHistoryItem(THistoryItem.Create(nil, nil, leader, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Leader14: CriminalHired']), IllSystem.Time, 'Fired ' + CriminalName + ' from team ' + TeamName, '', FloatToStr(loyalty)));
            Result := CRIME_NOERROR;
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      function TIllegalSystem.RDODismissTeam(LeaderName : widestring; TeamName : widestring) : olevariant;
        var
          leader : TLeader;
        begin
          try
            leader := FindLeaderinSystem(LeaderName);
            leader.DismTeam(FindTeamInSystem(LeaderName, TeamName));
            leader.AddHistoryItem(THistoryItem.Create(nil, nil, leader, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Leader13: TeamDismissed']), IllSystem.Time, 'Dismissed team ' + TeamName, '', ''));
            Result := CRIME_NOERROR;
          except
            Result := CRIME_ERROR_WrongTeamName;
          end;
        end;

      function TIllegalSystem.RDOChangeCriminalState(CriminalName : widestring; NewState : integer): olevariant;
        var
          crim : TCriminal;
        begin
          try
            crim := FindCriminalInSystem(CriminalName);
            crim.State := TCriminalState(NewState);
            crim.Change := True;
            Result := CRIME_NOERROR;
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      function TIllegalSystem.RDOChangeTeam(LeaderName : widestring; OldTeamName : widestring; NewTeamName : widestring; CriminalName : widestring): olevariant;
        var
          crim : TCriminal;
          team : TTeam;
          leader : TLeader;
        begin
          try
            RDOChangeRole(LeaderName, OldTeamName, CriminalName, 'EraseRole');
            FindTeamInSystem(LeaderName, NewTeamName).Hire(LeaderName, CriminalName, 100);
            crim := FindCriminalInSystem(CriminalName);
            crim.AddHistoryItem(THistoryItem.Create(nil, crim, nil, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Criminal14: ChangeTeam']), IllSystem.Time, 'Moved from team ' + OldTeamName + ' to team ' + NewTeamName, '', ''));
            team := FindTeamInSystem(LeaderName, OldTeamName);
            team.AddHistoryItem(THistoryItem.Create(team, nil, nil, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Team14: CriminalChangeTeam']), IllSystem.Time, CriminalName + ' has left the team to join team ' + NewTeamName, '', ''));
            team := FindTeamInSystem(LeaderName, NewTeamName);
            team.AddHistoryItem(THistoryItem.Create(team, nil, nil, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Team14: CriminalChangeTeam']), IllSystem.Time, CriminalName + ' has joined the team coming from team ' + OldTeamName, '', ''));
            leader := FindLeaderinSystem(LeaderName);
            leader.AddHistoryItem(THistoryItem.Create(nil, nil, leader, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Leader16: ChangeTeamToCriminal']), IllSystem.Time, 'Moved ' + CriminalName + ' from team ' + OldTeamName + ' to team ' + NewTeamName, '', ''));
            Result := CRIME_NOERROR;
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      function TIllegalSystem.RDOCriminalShowed(CrimName : widestring): olevariant;
        begin
          try
            FindCriminalInSystem(CrimName).Change := False;
            Result := CRIME_NOERROR;
          except
            result := CRIME_ERROR_Unknown;
          end;
        end;

      function TIllegalSystem.RDOMissionChangeShowed(LeaderName: widestring; TeamName : widestring): olevariant;
        begin
          try
            FindTeamInSystem(LeaderName, TeamName).Mission.Change := 'no';
            Result := CRIME_NOERROR;
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      function TIllegalSystem.RDOStopCriminalTraining(CriminalName : widestring): olevariant;
        begin
          try
            FindCriminalInSystem(CriminalName).StopTraining;
            RDOChangeCriminalState(CriminalName, 3);
            Result := CRIME_NOERROR;
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      function TIllegalSystem.RDOCriminalTraining(CriminalName : widestring; TrainingName : widestring): olevariant;
        begin
          try
            FindCriminalInSystem(CriminalName).Train(TrainingName);
            RDOChangeCriminalState(CriminalName, 4);
            Result := CRIME_NOERROR;
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      function TIllegalSystem.RDOGetCriminalTrainingInfo(CriminalName : widestring) : olevariant;
        var
          Criminal : TCriminal;
          List     : TStringList;
          //count    : integer;
          i        : integer;
          HI       : THistoryItem;
          //MA       : TMetaAttribute;
          //MAM      : TAttributeModifier;
        begin
          try
            Criminal := FindCriminalInSystem( CriminalName );
            List := TStringList.Create;
            try
              Criminal.HistoryItems.Lock;
              try
                //count := 0;
                for i := 0 to pred(Criminal.HistoryItems.Count) do
                  begin
                    HI := THistoryItem(Criminal.HistoryItems[i]);
                    if HI.MetaHistoryItem.Name = 'Training'
                      then List.Add( HI.Param1 );
                  end;
              finally
                Criminal.HistoryItems.Unlock;
              end;
              result := List.Text;
            finally
              List.Free;
            end;
          except
            result := '';
          end;
        end;

      function TIllegalSystem.RDOAssignHeadquarter(LeaderName : widestring; TeamName : widestring; CoordinateX : integer; CoordinateY : integer): olevariant;
        var
          i : integer;
          team : TTeam;
          Coord : TCoordinate;
        begin
          try
            Coord.X := CoordinateX;
            Coord.Y := CoordinateY;
            team := FindTeamInSystem(LeaderName, TeamName);
            team.Headquarter := Coord;
            team.Roles.Lock;
            try
              for i := 0 to (team.Roles.Count - 1) do
                TRole(team.Roles[i]).Criminal.Location := Coord;
            finally
              team.Roles.Unlock;
            end;
            Result := CRIME_NOERROR;
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      function TIllegalSystem.RDOCalculateSalary(LeaderName : widestring; TeamName : widestring) : olevariant;
        begin
          try
            Result := FindTeamInSystem(LeaderName, TeamName).CalcSalary;
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      function TIllegalSystem.RDOCalculateTrainingCost(LeaderName : widestring; TeamName : widestring) : olevariant;
        begin
          try
            Result := FindTeamInSystem(LeaderName, TeamName).CalcTrainingCost;
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      function TIllegalSystem.RDOCalculatePercentage(LeaderName : widestring; TeamName : widestring) : olevariant;
        begin
          try
            Result := FindTeamInSystem(LeaderName, TeamName).CalcAvPerc;
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      function TIllegalSystem.RDOCalculateLeaderExpenses(LeaderName : widestring) : olevariant;
        begin
          try
            Result := FindLeaderinSystem(LeaderName).CalcExpenses;
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      procedure TIllegalSystem.ChangeLeaderMoney(LeaderName : widestring; Money : single);
        begin
          FindLeaderinSystem(LeaderName).Money := FindLeaderinSystem(LeaderName).Money - Money;
        end;

      function TIllegalSystem.RDOChangeCriminalSalary(LeaderName : widestring; CriminalName : widestring; SalaryP : integer) : olevariant;
        var
          i : integer;
          oldSalary : single;
          criminal : TCriminal;
        begin
          try
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
            Result := CRIME_NOERROR;
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      function TIllegalSystem.RDORecoveryCriminalPendingRecord(CriminalName : widestring) : olevariant;
        var
          Crim : TCriminal;
        begin
          try
            Crim := FindCriminalInSystem(CriminalName);
            Result := ChargesWrapper(Crim.CrimPenRecord).Text;
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      function TIllegalSystem.RDORecoveryCriminalPoliceRecord(CriminalName : widestring) : olevariant;
        var
          Crim : TCriminal;
        begin
          try
            Crim := FindCriminalInSystem(CriminalName);
            Result := ChargesWrapper(Crim.CrimPolRecord).Text;
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      function TIllegalSystem.RDORecoveryCriminalJailDetails(CriminalName : widestring) : olevariant;
        var
          Details : TStringList;
          Crim : TCriminal;
        begin
          try
            Crim := FindCriminalInSystem(CriminalName);
            Details := TStringList.Create;
            Details.Values['JailTime'] := IntToStr(Crim.JailTime);
            Details.Values['TrialDay'] := IntToStr(Crim.TrialDay);
            Details.Values['LawyersHours'] := IntToStr(Crim.LawyersHours);
            Details.Values['TrialEndDay'] := IntToStr(Crim.TrialEndDay);
            Result := Details.Text + ChargesWrapper(Crim.CrimPolRecord).Text;
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      function TIllegalSystem.RDORecoveryHistoryItem(LeaderName : widestring; TeamName : widestring; CriminalName : widestring) : olevariant;
        begin
          try
            if LeaderName <> ''
            then
              Result := HistoryItemsWrapper(FindLeaderInSystem(LeaderName).HistoryItems).Text;
            if TeamName <> ''
            then
              Result := HistoryItemsWrapper(FindTeamInSystem(LeaderName, TeamName).HistoryItems).Text;
            if CriminalName <> ''
            then
              Result := HistoryItemsWrapper(FindCriminalInSystem(CriminalName).HistoryItems).Text;
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      function TIllegalSystem.RDORecoveryMissionInfos(MissionName : widestring) : olevariant;
        var
          i     : integer;
          Info  : TStringList;
        begin
          try
            Info := TStringList.Create;
            for i := 0 to (TheClassStorage.ClassCount['Mission'] - 1) do
              if TMetaServerMission(TheClassStorage.ClassByIdx['Mission', i]).Name = MissionName
              then
                begin
                  Info.Values['descr'] := TMetaServerMission(TheClassStorage.ClassByIdx['Mission', i]).Desc;
                  Info.Values['MissionType'] := TMetaServerMission(TheClassStorage.ClassByIdx['Mission', i]).MissionType;
                  Info.Values['Id'] := TMetaServerMission(TheClassStorage.ClassByIdx['Mission', i]).Name;
                end;
            Result := Info.Text;
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      function TIllegalSystem.RDORecoveryMissionOption(MissionName : widestring; OptionIndex : integer) : olevariant;
        var
          i    : integer;
          j    : integer;
          Info : TStringList;
        begin
          try
            Info := TStringList.Create;
            for i := 0 to (TheClassStorage.ClassCount['Mission'] - 1) do
              if TMetaServerMission(TheClassStorage.ClassByIdx['Mission', i]).Name = MissionName
              then
                begin
                  Info.Values['Id'] := TMetaServerMission(TheClassStorage.ClassByIdx['Mission', i]).MissionClass.OptionData(OptionIndex).Id;
                  Info.Values['Way'] := TMetaServerMission(TheClassStorage.ClassByIdx['Mission', i]).MissionClass.OptionData(OptionIndex).way;
                  if TMetaServerMission(TheClassStorage.ClassByIdx['Mission', i]).MissionClass.OptionData(OptionIndex).compulsory
                  then
                    Info.Values['Compulsory'] := 'yes'
                  else
                    Info.Values['Compulsory'] := 'no';
                  Info.Values['ParentOptionId'] := TMetaServerMission(TheClassStorage.ClassByIdx['Mission', i]).MissionClass.OptionData(OptionIndex).ParentOptionId;
                  Info.Values['Target'] := IntToStr(integer(TMetaServerMission(TheClassStorage.ClassByIdx['Mission', i]).MissionClass.OptionData(OptionIndex).Target));
                  Info.Values['Objective'] := TMetaServerMission(TheClassStorage.ClassByIdx['Mission', i]).MissionClass.OptionData(OptionIndex).Objective;
                  Info.Values['ObjCoorX'] := IntToStr(TMetaServerMission(TheClassStorage.ClassByIdx['Mission', i]).MissionClass.OptionData(OptionIndex).ObjCoord.X);
                  Info.Values['ObjCoorY'] := IntToStr(TMetaServerMission(TheClassStorage.ClassByIdx['Mission', i]).MissionClass.OptionData(OptionIndex).ObjCoord.Y);
                  Info.Values['MaxDistance'] := IntToStr(TMetaServerMission(TheClassStorage.ClassByIdx['Mission', i]).MissionClass.OptionData(OptionIndex).MaxDistance);
                  Info.Values['MaxRadius'] := IntToStr(TMetaServerMission(TheClassStorage.ClassByIdx['Mission', i]).MissionClass.OptionData(OptionIndex).MaxRadius);
                  Info.Values['Duration'] := IntToStr(TMetaServerMission(TheClassStorage.ClassByIdx['Mission', i]).MissionClass.OptionData(OptionIndex).Duration);
                  Info.Values['Cost'] := FloatToStr(TMetaServerMission(TheClassStorage.ClassByIdx['Mission', i]).MissionClass.OptionData(OptionIndex).Cost);
                  Info.Values['Profit'] := FloatToStr(TMetaServerMission(TheClassStorage.ClassByIdx['Mission', i]).MissionClass.OptionData(OptionIndex).Profit);
                  if TMetaServerMission(TheClassStorage.ClassByIdx['Mission', i]).MissionClass.OptionData(OptionIndex).Sub
                  then
                    Info.Values['Sub'] := 'yes'
                  else
                    Info.Values['Sub'] := 'no';
                  for j := 0 to (MaxRoles - 1) do
                    begin
                      Info.Values['Role' + IntToStr(j)] := TMetaServerMission(TheClassStorage.ClassByIdx['Mission', i]).MissionClass.OptionData(OptionIndex).Roles[j];
                      Info.Values['Desc' + IntToStr(j)] := TMetaServerMission(TheClassStorage.ClassByIdx['Mission', i]).MissionClass.OptionData(OptionIndex).Descriptions[j];
                      Info.Values['Skill' + IntToStr(j)] := TMetaServerMission(TheClassStorage.ClassByIdx['Mission', i]).MissionClass.OptionData(OptionIndex).Skills[j];
                      Info.Values['SkillValue' + IntToStr(j)] := FloatToStr(TMetaServerMission(TheClassStorage.ClassByIdx['Mission', i]).MissionClass.OptionData(OptionIndex).SkillValues[j]);
                    end;
                end;
            result := Info.Text;
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      function TIllegalSystem.RDOAssignMission(LeaderName : widestring; TeamName : widestring; MissionName : widestring; RolesList : widestring; ParametersList : widestring; Objective : widestring; ObjectiveX : integer; ObjectiveY : integer; TargetType : integer; TargetName : widestring; Radius : integer; TargetPlayer : widestring; TargetFacType: widestring; Cost : widestring) : olevariant;
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
          try
            Roles := TStringList.Create;
            Roles.Text := RolesList;
            Parameters := TStringList.Create;
            Parameters.Text := ParametersList;
            team := FindTeamInSystem(LeaderName, TeamName);
            for i := 0 to (TheClassStorage.ClassCount['Mission'] - 1) do
              if TMetaServerMission(TheClassStorage.ClassByIdx['Mission', i]).Name = MissionName
              then
                begin
                  team.Mission := TMetaServerMission(TheClassStorage.ClassByIdx['Mission', i]).Instantiate(team);
                  for k := 1 to 8 do
                    begin
                      RolesToAss.Name[k] := Roles.Values['Name' + IntToStr(k)];
                      RolesToAss.Role[k] := Roles.Values['Role' + IntToStr(k)];
                      RolesToAss.Result[k] := 'OK';
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
                  team.Mission.TargetDescr := Objective;
                  team.Mission.TargetCoord := CoordToAss;
                  team.Mission.TargetType := TTargetType(TargetType);
                  team.Mission.TargetName := TargetName;
                  team.Mission.Radius := Radius;
                  team.Mission.TargetPlayer := TargetPlayer;
                  team.Mission.TargetFacType := TargetFacType;
                end;
            team.AddHistoryItem(THistoryItem.Create(team, nil, nil, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Team17: MissionAssigned']), IllSystem.Time, 'Assigned the mission ' + MissionName + ' by ' + LeaderName, '', ''));
            for j := 1 to 8 do
              if team.Mission.Roles.Name[j] <> ''
              then
                begin
                  criminal := FindCriminalInSystem(team.Mission.Roles.Name[j]);
                  RDOChangeCriminalState(team.Mission.Roles.Name[j], 5);
                  criminal.AddHistoryItem(THistoryItem.Create(nil, criminal, nil, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Criminal17: AssignedMission']), IllSystem.Time, 'Assigned the mission ' + MissionName + ' by ' + LeaderName + ' in team ' + TeamName, '', ''));
                end;
            Leader := FindLeaderinSystem(LeaderName);
            if team.Mission.MetaMission.MissionType = 'One-shot'
            then
              begin
                Leader.Money := Leader.Money - StrToFloat(Cost);
                team.Mission.Cost := StrToFloat(Cost);
              end;
            Leader.AddHistoryItem(THistoryItem.Create(nil, nil, Leader, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Leader17: AssignMission']), IllSystem.Time, 'Assigned the mission ' + MissionName + ' to team ' + TeamName, '', ''));
            Result := CRIME_NOERROR;
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      function TIllegalSystem.RDOStartMission(LeaderName : widestring; TeamName : widestring; MissionName : widestring) : olevariant;
        begin
          try
            FindTeamInSystem(LeaderName, TeamName).Mission.StartMission;
            Result := CRIME_NOERROR;
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      function TIllegalSystem.RDODeassignMission(LeaderName : widestring; TeamName : widestring; Cancel : widestring; MissionName : widestring) : olevariant;
        var
          i : integer;
          team : TTeam;
          criminal : TCriminal;
          Leader : TLeader;
        begin
          try
            Leader := FindLeaderinSystem(LeaderName);
            team := FindTeamInSystem(LeaderName, TeamName);
            if Cancel = 'yes'
            then
              begin
                team.AddHistoryItem(THistoryItem.Create(team, nil, nil, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Team18: MissionDeassigned']), IllSystem.Time, 'Deassigned the mission ' + MissionName + '  by ' + LeaderName, '', ''));
                Leader.AddHistoryItem(THistoryItem.Create(nil, nil, Leader, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Leader18: DeassignMission']), IllSystem.Time, 'Deassigned the mission ' + MissionName + ' to team ' + TeamName, '', ''));
              end
            else
              begin
                Leader.Money := Leader.Money + team.Mission.Profit;
                team.AddHistoryItem(THistoryItem.Create(team, nil, nil, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Team16: MissionAccomplished']), IllSystem.Time, 'The mission ' + MissionName + ', assigned by ' + LeaderName + ', has been accomplished by the team', team.Mission.Report.text, ''));
                Leader.AddHistoryItem(THistoryItem.Create(nil, nil, Leader, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Leader19: MissionCompletedByATeam']), IllSystem.Time, 'The mission ' + MissionName + ' assigned to team ' + TeamName + ' has been accomplished', team.Mission.Report.text, ''));
              end;
            for i := 1 to 8 do
              if team.Mission.Roles.Name[i] <> ''
              then
                begin
                  criminal := FindCriminalInSystem(team.Mission.Roles.Name[i]);
                  if Cancel = 'yes'
                  then
                    begin
                      criminal.AddHistoryItem(THistoryItem.Create(nil, criminal, nil, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Criminal18: DeassignedMission']), IllSystem.Time, 'Deassigned the mission  by ' + LeaderName + ' in team ' + TeamName, '', ''));
                    end
                  else
                    begin
                      criminal.AddHistoryItem(THistoryItem.Create(nil, criminal, nil, TMetaHistoryItem(TheClassStorage.ClassById['HistoryItem', 'Criminal16: AccomplishedMission']), IllSystem.Time, 'The mission ' + MissionName + ', assigned by ' + LeaderName + ', has been accomplished by the criminal in team ' + TeamName, team.Mission.Report.text, team.Mission.Charges.Text));
                    end;
                  if criminal.State = TCriminalState(5)
                  then
                    begin
                      IllSystem.RDOChangeCriminalState(team.Mission.Roles.Name[i], 3);
                    end;
                  if criminal.State = TCriminalState(6)
                  then
                    begin
                      IllSystem.RDOChangeCriminalState(team.Mission.Roles.Name[i], 9);
                    end;
                end;
            team.Mission := nil;
            Result := CRIME_NOERROR;
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      procedure TIllegalSystem.GiveChargesToCriminal(CriminalName : widestring);
        var
          i : integer;
          j : integer;
          l : integer;
          team : TTeam;
          criminal : TCriminal;
          ChargeLean : TCharge;
        begin
          criminal := FindCriminalInSystem(CriminalName);
          team := FindTeamInSystem(criminal.LeaderName, criminal.TeamName);
          for j := 1 to (team.Mission.Charges.Count) do
          if team.Mission.Charges.Values['Charge' + IntToStr(j)] <> ''
          then
            for l := 1 to (team.Mission.Charges.Count) do
              if team.Mission.Charges.Values['Offender' + IntToStr(j) + IntToStr(l)] <> ''
              then
                if team.Mission.Charges.Values['Offender' + IntToStr(j) + IntToStr(l)] = criminal.Name
                then
                  begin
                    if (criminal.State = TCriminalState(7))
                    then
                      begin
                        for i := 0 to (TheClassStorage.ClassCount['Charge'] - 1) do
                          if TMetaCharge(TheClassStorage.ClassByIdx['Charge', i]).Name = team.Mission.Charges.Values['Charge' + IntToStr(j)]
                          then
                            begin
                              ChargeLean := TCharge.Create(criminal, TMetaCharge(TheClassStorage.ClassByIdx['Charge', i]), team.Mission.MetaMission.Name, team.Mission.StartingTime, criminal.LeaderName, criminal.TeamName, team.Mission.Charges.Values['LeaderCharge' + IntToStr(j)]);
                              criminal.AddCrimPolRecord(ChargeLean);
                            end;
                      end
                    else
                      begin
                        for i := 0 to (TheClassStorage.ClassCount['Charge'] - 1) do
                          if TMetaCharge(TheClassStorage.ClassByIdx['Charge', i]).Name = team.Mission.Charges.Values['Charge' + IntToStr(j)]
                          then
                            begin
                              ChargeLean := TCharge.Create(criminal, TMetaCharge(TheClassStorage.ClassByIdx['Charge', i]), team.Mission.MetaMission.Name, team.Mission.StartingTime, criminal.LeaderName, criminal.TeamName, team.Mission.Charges.Values['LeaderCharge' + IntToStr(j)]);
                              criminal.AddCrimPenRecord(ChargeLean);
                            end;
                      end;
                  end;
        end;

      procedure TIllegalSystem.GiveChargesToLeader(LeaderName : widestring; MissionName : widestring; Date : integer; Charges : TStringList);
        var
          i : integer;
          j : integer;
          leader : TLeader;
          ChargeLean : TLeaderCharge;
        begin
          leader := FindLeaderInSystem(LeaderName);
          for i := 1 to Charges.Count do
            begin
              for j := 0 to (TheClassStorage.ClassCount['Charge'] - 1) do
                if TMetaCharge(TheClassStorage.ClassByIdx['Charge', j]).Name = Charges.Values['LeaderCharge' + IntToStr(i)]
                then
                  begin
                    ChargeLean := TLeaderCharge.Create(leader, TMetaCharge(TheClassStorage.ClassByIdx['Charge', j]), MissionName, Date);
                    leader.AddCharge(ChargeLean);
                    leader.Warrant := leader.Warrant + TMetaCharge(TheClassStorage.ClassByIdx['Charge', j]).Warrant;
                  end;
            end;
        end;

      procedure TIllegalSystem.CriminalArrested(CriminalName : widestring);
        var
          criminal : TCriminal;
        begin
          criminal := FindCriminalInSystem(CriminalName);
          if criminal.State = TCriminalState(5)
            then
              begin
                IllSystem.RDOChangeCriminalState(CriminalName, 7);
                IllSystem.GiveChargesToCriminal(CriminalName);
                criminal.Location := IllSystem.GetPrisonLocation(IllSystem.FindTeamInSystem(criminal.LeaderName, criminal.TeamName).Mission.TargetCoord);
                IllSystem.SetTrialDay(CriminalName);
              end;
        end;

      function TIllegalSystem.GetPrisonLocation(CriminalLocation : TCoordinate) : TCoordinate;
        begin
          Result := CriminalLocation;
        end;

      function TIllegalSystem.GetHospitalLocation(CriminalLocation : TCoordinate) : TCoordinate;
        begin
          Result := CriminalLocation;
        end;

      function TIllegalSystem.GetHiddenLocation : TCoordinate;
        var
          coord : TCoordinate;
        begin
          coord.X := 100;
          coord.Y := 100;
          Result := coord;
        end;

      procedure TIllegalSystem.SetTrialDay(CriminalName : widestring);
        var
          i : integer;
          j : integer;
          crim : TCriminal;
        begin
          crim := FindCriminalInSystem(CriminalName);
          if crim.TrialDay = 0
          then
            crim.TrialDay := IllSystem.Time;
          crim.CrimPolRecord.Lock;
          try
            for i := 0 to (crim.CrimPolRecord.Count - 1) do
              for j := 0 to (TheClassStorage.ClassCount['Charge'] - 1) do
                begin
                  if TCharge(crim.CrimPolRecord[i]).MetaCharge.Name = TMetacharge(TheClassStorage.ClassByIdx['Charge', j]).Name
                  then
                    if crim.TrialDay < IllSystem.Time +  TMetacharge(TheClassStorage.ClassByIdx['Charge', j]).PreTrialTime
                    then
                      crim.TrialDay := IllSystem.Time + TMetacharge(TheClassStorage.ClassByIdx['Charge', j]).PreTrialTime;
                end;
          finally
            crim.CrimPolRecord.Unlock;
          end;
        end;

      procedure TIllegalSystem.TrialEnd(CriminalName : widestring);
        var
          i : integer;
          j : integer;
          crim : TCriminal;
        begin
          crim := FindCriminalInSystem(CriminalName);
          crim.CrimPolRecord.Lock;
          try
            for i := 0 to (crim.CrimPolRecord.Count - 1) do
              for j := 0 to (TheClassStorage.ClassCount['Charges'] - 1) do
                begin
                  if TCharge(crim.CrimPolRecord[i]).MetaCharge.Name = TMetacharge(TheClassStorage.ClassByIdx['Charge', j]).Name
                  then
                    if crim.TrialEndDay < IllSystem.Time + TMetacharge(TheClassStorage.ClassByIdx['Charge', j]).TrialTime
                    then
                      crim.TrialEndDay := IllSystem.Time + TMetacharge(TheClassStorage.ClassByIdx['Charge', j]).TrialTime;
                end;
          finally
            crim.CrimPolRecord.Unlock;
          end;
        end;

      procedure TIllegalSystem.CriminalConfession(CriminalName : widestring);
        var
          i : integer;
          j : integer;
          k : integer;
          l : integer;
          m : integer;
          n : integer;
          o : integer;
          crim : TCriminal;
          MaxTime : integer;
          TotalLawyersHours : integer;
          TotalBribing : single;
          ConfPerc : integer;
          leaderNamesList : TStringList;
          already : boolean;
          confessed : boolean;
          lastDate : integer;
          loyalty : single;
        begin
          crim := FindCriminalInSystem(CriminalName);
          MaxTime := 0;
          TotalLawyersHours := 0;
          TotalBribing := 0;
          loyalty := 0;
          leaderNamesList := TStringList.Create;
          n := 0;
          crim.CrimPolRecord.Lock;
          try
            for l := 0 to (crim.CrimPolRecord.Count - 1) do
              begin
                already := False;
                m := 0;
                while m < leaderNamesList.Count do
                  begin
                    if leaderNamesList.Values[IntToStr(m)] = TCharge(crim.CrimPolRecord[l]).LeaderName
                    then
                      already := True;
                    m := m + 1;
                  end;
                if already = False
                then
                  begin
                    leaderNamesList.Values[IntToStr(n)];
                    n := n + 1
                  end;
              end;
          finally
            crim.CrimPolRecord.Unlock;
          end;
          confessed := False;
          for l := 0 to (leaderNamesList.Count - 1) do
            begin
              if confessed = False
              then
                begin
                  crim.CrimPolRecord.Lock;
                  try
                    for i := 0 to (crim.CrimPolRecord.Count - 1) do
                      if leaderNamesList.Values[IntToStr(l)] = TCharge(crim.CrimPolRecord[i]).LeaderName
                      then
                        for j := 0 to (TheClassStorage.ClassCount['Charges'] - 1) do
                          if TCharge(crim.CrimPolRecord[i]).MetaCharge.Name = TMetacharge(TheClassStorage.ClassByIdx['Charge', j]).Name
                          then
                            begin
                              if MaxTime <> -1
                              then
                                MaxTime := MaxTime + TMetacharge(TheClassStorage.ClassByIdx['Charge', j]).MaxJailTime
                              else
                                MaxTime := -1;
                              TotalLawyersHours := TotalLawyersHours + TMetacharge(TheClassStorage.ClassByIdx['Charge', j]).MinLawyersHours;
                              TotalBribing := TotalBribing + TMetacharge(TheClassStorage.ClassByIdx['Charge', j]).MinBribe;
                            end;
                  finally
                    crim.CrimPolRecord.Unlock;
                  end;
                  case MaxTime of
                    -1             : ConfPerc := 80;
                    0..17520       : ConfPerc := 0;
                    17521..43800   : ConfPerc := 20;
                    43801..87600   : ConfPerc := 40;
                    87601..175200  : ConfPerc := 60;
                  else
                    ConfPerc := 80;
                  end;
                  case Round(100 * crim.LawyersHours / TotalLawyersHours) of
                    0              : ConfPerc := Round(ConfPerc * 1.5);
                    1..50          : ConfPerc := ConfPerc;
                    51..100        : ConfPerc := Round(ConfPerc /2);
                    101..200       : ConfPerc := Round(ConfPerc /3);
                  else
                    ConfPerc := Round(ConfPerc /4);
                  end;
                  case Round(100 * crim.Bribing / TotalBribing) of
                    0..99          : ConfPerc := ConfPerc;
                    100..150       : ConfPerc := Round(ConfPerc / 2);
                    151..199       : ConfPerc := Round(ConfPerc / 3);
                  else
                    ConfPerc := Round(ConfPerc / 4);
                  end;
                  for k := 0 to (crim.Attributes.Count - 1) do
                    if TAttribute(crim.Attributes[k]).MetaAttribute.Name = 'Stability'
                    then
                      case UseSkill(TAttribute(crim.Attributes[k]).Value, 50) of
                        1 : ConfPerc := ConfPerc - 20;
                        2 : ConfPerc := ConfPerc;
                        3 : ConfPerc := ConfPerc + 10;
                        4 : ConfPerc := ConfPerc + 30;
                      end;
                  if crim.LeaderName = leaderNamesList.Values[IntToStr(l)]
                  then
                    begin
                      for k := 0 to (crim.Attributes.Count - 1) do
                        if TAttribute(crim.Attributes[k]).MetaAttribute.Name = 'Loyalty'
                        then
                          loyalty := TAttribute(crim.Attributes[k]).Value;
                    end
                  else
                    begin
                      lastDate := 0;
                      for m := 0 to (crim.HistoryItems.Count - 1) do
                        begin
                          if ((THistoryItem(crim.HistoryItems[m]).MetaHistoryItem.Name = 'Fire') or (THistoryItem(crim.HistoryItems[m]).MetaHistoryItem.Name = 'GivenToAnotherLeader'))
                          then
                            if THistoryItem(crim.HistoryItems[m]).Param1 = leaderNamesList.Values[IntToStr(l)]
                            then
                              if THistoryItem(crim.HistoryItems[m]).Date > lastDate
                              then
                                begin
                                  lastDate := THistoryItem(crim.HistoryItems[m]).Date;
                                  loyalty := StrToFloat(THistoryItem(crim.HistoryItems[m]).Param2);
                                end;
                        end;
                    end;
                  case UseSkill(loyalty, ConfPerc) of
                    1 : TrialEnd(crim.Name);
                    2 : TrialEnd(crim.Name);
                    3 :
                      begin
                        SetLeaderTrialDay(leaderNamesList.Values[IntToStr(l)], 1);
                        confessed := True;
                        IllSystem.RDOChangeCriminalState(crim.Name, 8);
                        crim.Location := IllSystem.GetHiddenLocation;
                        crim.CrimPolRecord.Lock;
                        try
                          for o := (crim.CrimPolRecord.Count - 1) downto 0 do
                            if TCharge(crim.CrimPolRecord[o]).LeaderName = leaderNamesList.Values[IntToStr(l)]
                            then
                              crim.CrimPolRecord.AtDelete(o);
                        finally
                          crim.CrimPolRecord.Unlock;
                        end;
                      end;
                    4 :
                      begin
                        SetLeaderTrialDay(leaderNamesList.Values[IntToStr(l)], 2);
                        confessed := True;
                        IllSystem.RDOChangeCriminalState(crim.Name, 8);
                        crim.Location := IllSystem.GetHiddenLocation;
                        crim.CrimPolRecord.Lock;
                        try
                          for o := (crim.CrimPolRecord.Count - 1) downto 0 do
                            if TCharge(crim.CrimPolRecord[o]).LeaderName = leaderNamesList.Values[IntToStr(l)]
                            then
                              crim.CrimPolRecord.AtDelete(o);
                        finally
                          crim.CrimPolRecord.Unlock;
                        end;
                      end;
                  end;
                end;
            end;
        end;

      procedure TIllegalSystem.LeaderOnTrial(LeaderName : widestring);
        var
          i : integer;
          j : integer;
          leader : TLeader;
        begin
          leader := FindLeaderInSystem(LeaderName);
          leader.Charges.Lock;
          try
            for i := 0 to (leader.Charges.Count - 1) do
              for j := 0 to (TheClassStorage.ClassCount['Charges'] - 1) do
                begin
                  if TLeaderCharge(leader.Charges[i]).MetaCharge.Name = TMetacharge(TheClassStorage.ClassByIdx['Charge', j]).Name
                  then
                    if leader.TrialEndDay < IllSystem.Time + TMetacharge(TheClassStorage.ClassByIdx['Charge', j]).TrialTime
                    then
                      leader.TrialEndDay := IllSystem.Time + TMetacharge(TheClassStorage.ClassByIdx['Charge', j]).TrialTime;
                end;
          finally
            leader.Charges.Unlock;
          end;
        end;

      procedure TIllegalSystem.SetLeaderTrialDay(LeaderName : widestring; speed : integer);
        var
          i : integer;
          j : integer;
          leader : TLeader;
        begin
          leader := FindLeaderInSystem(LeaderName);
          if leader.TrialDay = 0
          then
            leader.TrialDay := IllSystem.Time;
          leader.Charges.Lock;
          try
            for i := 0 to (leader.Charges.Count - 1) do
              for j := 0 to (TheClassStorage.ClassCount['Charges'] - 1) do
                begin
                  if TLeaderCharge(leader.Charges[i]).MetaCharge.Name = TMetacharge(TheClassStorage.ClassByIdx['Charge', j]).Name
                  then
                    if speed = 1
                    then
                      if leader.TrialDay < IllSystem.Time + TMetacharge(TheClassStorage.ClassByIdx['Charge', j]).PreTrialTime
                      then
                        leader.TrialDay := IllSystem.Time + (TMetacharge(TheClassStorage.ClassByIdx['Charge', j]).PreTrialTime * 2);
                    if speed = 2
                    then
                      if leader.TrialDay < IllSystem.Time + TMetacharge(TheClassStorage.ClassByIdx['Charge', j]).PreTrialTime
                      then
                        leader.TrialDay := IllSystem.Time + TMetacharge(TheClassStorage.ClassByIdx['Charge', j]).PreTrialTime;
                end;
          finally
            leader.Charges.Unlock;
          end;
        end;

      procedure TIllegalSystem.LeaderTrialOutcome(LeaderName : widestring);
        var
          i : integer;
          j : integer;
          leader : TLeader;
          MaxTime : integer;
          //MinTime : integer;
          //TotalLawyersHours : integer;
          TotalBribing : single;
          //JailTime : integer;
        begin
          leader := FindLeaderInSystem(LeaderName);
          MaxTime := 0;
          //TotalLawyersHours := 0;
          TotalBribing := 0;
          leader.Charges.Lock;
          try
            for i := 0 to (leader.Charges.Count - 1) do
              for j := 0 to (TheClassStorage.ClassCount['Charges'] - 1) do
                if TCharge(leader.Charges[i]).MetaCharge.Name = TMetacharge(TheClassStorage.ClassByIdx['Charge', j]).Name
                then
                  begin
                    if MaxTime <> -1
                    then
                      MaxTime := MaxTime + TMetacharge(TheClassStorage.ClassByIdx['Charge', j]).MaxJailTime
                    else
                      MaxTime := -1;
                    //MinTime := MinTime + TMetacharge(TheClassStorage.ClassByIdx['Charge', j]).MinJailTime;
                    //TotalLawyersHours := TotalLawyersHours + TMetacharge(TheClassStorage.ClassByIdx['Charge', j]).MinLawyersHours;
                    TotalBribing := TotalBribing + TMetacharge(TheClassStorage.ClassByIdx['Charge', j]).MinBribe;
                  end;
          finally
            leader.Charges.Unlock;
          end;
        end;

      procedure TIllegalSystem.TrialOutcome(CriminalName : widestring);
        var
          i : integer;
          j : integer;
          crim : TCriminal;
          MaxTime : integer;
          MinTime : integer;
          TotalLawyersHours : integer;
          TotalBribing : single;
          JailTime : integer;
          FreePerc : integer;
        begin
          crim := FindCriminalInSystem(CriminalName);
          MaxTime := 0;
          MinTime := 0;
          FreePerc := 0;
          TotalLawyersHours := 0;
          TotalBribing := 0;
          crim.TrialEndDay := 0;
          crim.CrimPolRecord.Lock;
          try
            for i := 0 to (crim.CrimPolRecord.Count - 1) do
              for j := 0 to (TheClassStorage.ClassCount['Charges'] - 1) do
                if TCharge(crim.CrimPolRecord[i]).MetaCharge.Name = TMetacharge(TheClassStorage.ClassByIdx['Charge', j]).Name
                then
                  begin
                    if MaxTime <> -1
                    then
                      MaxTime := MaxTime + TMetacharge(TheClassStorage.ClassByIdx['Charge', j]).MaxJailTime
                    else
                      MaxTime := -1;
                    MinTime := MinTime + TMetacharge(TheClassStorage.ClassByIdx['Charge', j]).MinJailTime;
                    TotalLawyersHours := TotalLawyersHours + TMetacharge(TheClassStorage.ClassByIdx['Charge', j]).MinLawyersHours;
                    TotalBribing := TotalBribing + TMetacharge(TheClassStorage.ClassByIdx['Charge', j]).MinBribe;
                  end;
          finally
            crim.CrimPolRecord.Unlock;
          end;
          case Round(100 * crim.LawyersHours / TotalLawyersHours) of
            0 : JailTime := MaxTime;
            1..49 :
              begin
                case MaxTime of
                  -1             : JailTime := MaxTime;
                  0..17520       : JailTime := (Round((MaxTime - MinTime)/2) + MinTime);
                  17521..43800   : JailTime := ((Round((MaxTime - MinTime)/3)*2) + MinTime);
                  43801..87600   : JailTime := MaxTime;
                  87601..175200  : JailTime := MaxTime;
                else
                  JailTime := MaxTime;
                end;
              end;
            50..99 :
              begin
                case MaxTime of
                  -1             : JailTime := MaxTime;
                  0..17520       : JailTime := MinTime;
                  17521..43800   : JailTime := (Round((MaxTime - MinTime)/2) + MinTime);
                  43801..87600   : JailTime := ((Round((MaxTime - MinTime)/3)*2) + MinTime);
                  87601..175200  : JailTime := MaxTime;
                else
                  JailTime := MaxTime;
                end;
              end;
            100..199 :
              begin
                case MaxTime of
                  -1 :
                    begin
                      if MinTime < 350400
                      then
                        JailTime := ((Round((350400 - MinTime)/3)*2) + MinTime)
                      else
                        JailTime := MinTime;
                    end;
                  0..17520 :
                    begin
                      JailTime := MinTime;
                      FreePerc := 30;
                    end;
                  17521..43800   : JailTime := MinTime;
                  43801..87600   : JailTime := (Round((MaxTime - MinTime)/3) + MinTime);
                  87601..175200  : JailTime := (Round((MaxTime - MinTime)/2) + MinTime);
                else
                  JailTime := (Round((MaxTime - MinTime)/2) + MinTime);
                end;
              end;
          else
            begin
              case MaxTime of
                -1             :
                  begin
                    if MinTime < 350400
                      then
                        JailTime := ((Round((350400 - MinTime)/2)) + MinTime)
                      else
                        JailTime := MinTime;
                  end;
                0..17520 :
                  begin
                    JailTime := MinTime;
                    FreePerc := 50;
                  end;
                17521..43800 :
                  begin
                    JailTime := MinTime;
                    FreePerc := 30;
                  end;
                43801..87600   : JailTime := MinTime;
                87601..175200  : JailTime := (Round((MaxTime - MinTime)/3) + MinTime);
              else
                JailTime := (Round((MaxTime - MinTime)/3) + MinTime);
              end;
            end;
          end;
          case Round(100 * crim.Bribing / TotalBribing) of
            0..99 : JailTime := JailTime;
            100..150 :
              begin
                case JailTime of
                  -1 :
                    begin
                      if MinTime < 350400
                      then
                        JailTime := 350400
                      else
                        JailTime := MinTime;
                      FreePerc := FreePerc + 5;
                    end;
                  0..17520 :
                    begin
                      JailTime := MinTime;
                      FreePerc := FreePerc + 30;
                    end;
                  17521..43800 :
                    begin
                      JailTime := MinTime;
                      FreePerc := FreePerc + 25;
                    end;
                  43801..87600 :
                    begin
                      JailTime := (Round((JailTime - MinTime)/2) + MinTime);
                      FreePerc := FreePerc + 20;
                    end;
                  87601..175200 :
                    begin
                      JailTime := ((Round((JailTime - MinTime)/3)*2) + MinTime);
                      FreePerc := FreePerc + 15;
                    end;
                else
                  begin
                    JailTime := ((Round((JailTime - MinTime)/4)*3) + MinTime);
                    FreePerc := FreePerc + 10;
                  end;
                end;
              end;
            151..199 :
              begin
                case JailTime of
                  -1 :
                    begin
                      if MinTime < 350400
                      then
                        JailTime := ((Round((350400 - MinTime)/2)) + MinTime)
                      else
                        JailTime := MinTime;
                      FreePerc := FreePerc + 15;
                    end;
                  0..17520 :
                    begin
                      JailTime := MinTime;
                      FreePerc := FreePerc + 40;
                    end;
                  17521..43800 :
                    begin
                      JailTime := MinTime;
                      FreePerc := FreePerc + 35;
                    end;
                  43801..87600 :
                    begin
                      JailTime := MinTime;
                      FreePerc := FreePerc + 30;
                    end;
                  87601..175200 :
                    begin
                      JailTime := (Round((JailTime - MinTime)/2) + MinTime);
                      FreePerc := FreePerc + 25;
                    end;
                else
                  begin
                    JailTime := ((Round((JailTime - MinTime)/3)*2) + MinTime);
                    FreePerc := FreePerc + 20;
                  end;
                end;
              end;
          else
            begin
              case JailTime of
                -1 :
                  begin
                    if MinTime < 350400
                      then
                        JailTime := ((Round((350400 - MinTime)/3)*2) + MinTime)
                      else
                        JailTime := MinTime;
                    FreePerc := FreePerc + 30;
                  end;
                0..17520 :
                  begin
                    JailTime := MinTime;
                    FreePerc := FreePerc + 55;
                  end;
                17521..43800 :
                  begin
                    JailTime := MinTime;
                    FreePerc := FreePerc + 50;
                  end;
                43801..87600 :
                  begin
                    JailTime := MinTime;
                    FreePerc := FreePerc + 45;
                  end;
                87601..175200 :
                  begin
                    JailTime := (Round((JailTime - MinTime)/3) + MinTime);
                    FreePerc := FreePerc + 40;
                  end;
              else
                begin
                  JailTime := (Round((JailTime - MinTime)/2) + MinTime);
                  FreePerc := FreePerc + 35;
                end;
              end;
            end;
          end;
          if (Random(100) + 1) <= FreePerc
          then
            crim.JailTime := 0
          else
            if crim.JailTime = 0
            then
              crim.JailTime := IllSystem.Time + JailTime
            else
              crim.JailTime := crim.JailTime + JailTime;
          crim.CrimPolRecord.Lock;
          try
            crim.CrimPolRecord.DeleteAll;
          finally
            crim.CrimPolRecord.Unlock;
          end;
        end;

      function TIllegalSystem.UseSkill(SkillValue : single; Difficulty : single): integer;
        var
          success    : single;
          roll       : integer;
        begin
          success := 60 + skillValue - Difficulty;
          roll := Random(100);
          Result := 0;
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

      procedure TIllegalSystem.HandleCriminalsDisponibility;
        var
          i : integer;
          j : integer;
          k : integer;
          l : integer;
          m : integer;
          n : integer;
          LeadersPerLevel : array[1..5] of integer;
          CrimNumbPerLevel : array[1..5] of integer;
          CrimNeededPerLevel : array[1..5] of integer;
          GotMetaCrim : boolean;
          MetaCrimNumber : integer;
          MetaCrimNumbPerLevel : array[1..5] of integer;
        begin
          for n := 1 to 5 do
            begin
              LeadersPerLevel[n] := 0;
              CrimNumbPerLevel[n] := 0;
              MetaCrimNumbPerLevel[n] := 0;
              CrimNeededPerLevel[n] := 0
            end;
          IllSystem.Leaders.Lock;
          try
            for i := 0 to (IllSystem.Leaders.Count - 1) do
              begin
                case IllSystem.GetLeaderLevel(TLeader(IllSystem.Leaders[i]).Reputation) of
                  1 : LeadersPerLevel[1] := LeadersPerLevel[1] + 1;
                  2 : LeadersPerLevel[2] := LeadersPerLevel[2] + 1;
                  3 : LeadersPerLevel[3] := LeadersPerLevel[3] + 1;
                  4 : LeadersPerLevel[4] := LeadersPerLevel[4] + 1;
                else
                  LeadersPerLevel[5] := LeadersPerLevel[5] + 1;
                end;
              end;
          finally
            IllSystem.Leaders.Unlock;
          end;
          for n := 1 to 5 do
            begin
              if LeadersPerLevel[n] <> 0 then
                begin
                  IllSystem.Criminals.Lock;
                  try
                    for j := 0 to (IllSystem.Criminals.Count - 1) do
                      if TCriminal(IllSystem.Criminals[j]).State = TCriminalState(1)
                      then
                        if TCriminal(IllSystem.Criminals[j]).MetaCriminal.Level = n
                        then
                          CrimNumbPerLevel[n] := CrimNumbPerLevel[n] + 1;
                  finally
                    IllSystem.Criminals.Unlock;
                  end;
                end;
            end;
          for n := 1 to 5 do
            begin
              for j := 0 to (TheClassStorage.ClassCount['Criminal'] - 1) do
                if TMetaCriminal(TheClassStorage.ClassByIdx['Criminal', j]).Level = n
                then
                  MetaCrimNumbPerLevel[n] := MetaCrimNumbPerLevel[n] + 1;
            end;
          for n := 1 to 5 do
            begin
              CrimNeededPerLevel[n] := (LeadersPerLevel[n] * 15);
              if (n - 1) > 0
              then
                CrimNeededPerLevel[n - 1] := CrimNeededPerLevel[n - 1] + (LeadersPerLevel[n] * 5);
              if (n - 2) > 0
              then
                CrimNeededPerLevel[n - 2] := CrimNeededPerLevel[n - 2] + (LeadersPerLevel[n] * 2);
              if (n - 3) > 0
              then
                CrimNeededPerLevel[n - 3] := CrimNeededPerLevel[n - 3] + (LeadersPerLevel[n] * 2);
              if (n - 4) > 0
              then
                CrimNeededPerLevel[n - 4] := CrimNeededPerLevel[n - 4] + (LeadersPerLevel[n] * 2);
            end;
          for n := 1 to 5 do
            begin
              for k := (CrimNumbPerLevel[n] + 1) to CrimNeededPerLevel[n] do
                begin
                  l := -1;
                  m := 1;
                  MetaCrimNumber := Random(MetaCrimNumbPerLevel[n]) + 1;
                  GotMetaCrim := False;
                  while GotMetaCrim = False do
                    begin
                      l := l + 1;
                      if TMetaCriminal(TheClassStorage.ClassByIdx['Criminal', l]).Level = n
                      then
                        begin
                          if m = MetaCrimNumber
                          then
                            GotMetaCrim := True;
                          m := m + 1;
                        end;
                    end;
                  CreateCriminal(GenerateCriminalName(TMetaCriminal(TheClassStorage.ClassByIdx['Criminal', l]).Sex), TMetaCriminal(TheClassStorage.ClassByIdx['Criminal', l]));
                end;
            end;
        end;

      function TIllegalSystem.RDOCheckCriminalList(LeaderName : widestring) : olevariant;
        var
          leader : TLeader;
        begin
          try
            leader := FindLeaderInSystem(LeaderName);
            Result := leader.NLChange;
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      function TIllegalSystem.GetLeaderLevel(Reputation : integer): integer;
        begin
          case Reputation of
            0..200 : Result := 1;
            201..400 : Result := 2;
            401..800 : Result := 3;
            801..1600 : Result := 4;
          else
            Result := 5;
          end;
        end;

      function TIllegalSystem.GenerateCriminalName(sex : string) : string;
        var
          i : integer;
          j : integer;
          Name : string;
          Good : boolean;
        begin
          Good := False;
          while Good = False do
            begin
              if sex = 'Male'
              then
                begin
                  i := Random(IllSystem.FirstMNameList.Count) + 1;
                  Name := IllSystem.FirstMNameList.Values['m' + IntToStr(i)];
                end
              else
                begin
                  i := Random(IllSystem.FirstFNameList.Count) + 1;
                  Name := IllSystem.FirstFNameList.Values['f' + IntToStr(i)];
                end;
              i := Random(IllSystem.LastNameList.Count) + 1;
              Name := Name + ' ' + IllSystem.LastNameList.Values[IntToStr(i)];
              Good := True;
              IllSystem.Criminals.Lock;
              try
                for j := 0 to (IllSystem.Criminals.Count - 1) do
                  if TCriminal(IllSystem.Criminals[j]).Name = Name
                  then
                    Good := False;
              finally
                IllSystem.Criminals.Unlock;
              end;
            end;
          Result := Name;
        end;

      function TIllegalSystem.RDOGetCityList : olevariant;
        var
          CityList : TStringList;
        begin
          try
            CityList := TStringList.Create;
            CityList.Values['1'] := 'Esperanza';
            CityList.Values['2'] := 'New Venice';
            CityList.Values['3'] := 'Ottawa';
            CityList.Values['4'] := 'Santa Clara';
            CityList.Values['5'] := 'Milan';
            Result := CityList.Text;
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      function TIllegalSystem.RDOGetLeaderList : olevariant;
        var
          i : integer;
          LeaderList : TStringList;
        begin
          try
            LeaderList := TStringList.Create;
            IllSystem.Leaders.Lock;
            try
              for i := 0 to (IllSystem.Leaders.Count - 1) do
                LeaderList.Values[IntToStr(i + 1)] := TLeader(IllSystem.Leaders[i]).Name;
            finally
              IllSystem.Leaders.Unlock;
            end;
            Result := LeaderList.Text;
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      function TIllegalSystem.RDOGetCriminalListByLeader(LeaderName : widestring) : olevariant;
        var
          i : integer;
          j : integer;
          CriminalList : TStringList;
        begin
          try
            CriminalList := TStringList.Create;
            if LeaderName = ''
            then
              begin
                IllSystem.Criminals.Lock;
                try
                  for i := 0 to (IllSystem.Criminals.Count - 1) do
                    CriminalList.Values[IntToStr(i + 1)] := TCriminal(IllSystem.Criminals[i]).Name;
                finally
                  IllSystem.Criminals.Unlock;
                end;
              end
            else
              begin
                j := 1;
                IllSystem.Criminals.Lock;
                try
                  for i := 0 to (IllSystem.Criminals.Count - 1) do
                    if TCriminal(IllSystem.Criminals[i]).LeaderName = LeaderName
                    then
                      begin
                        CriminalList.Values[IntToStr(j)] := TCriminal(IllSystem.Criminals[i]).Name;
                        j := j + 1;
                      end;
                finally
                  IllSystem.Criminals.Unlock;
                end;
              end;
            Result := CriminalList.Text;
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      function TIllegalSystem.RDOGetTeamListByLeader(LeaderName : widestring) : olevariant;
        var
          i : integer;
          j : integer;
          TeamList : TStringList;
        begin
          try
            TeamList := TStringList.Create;
            IllSystem.Leaders.Lock;
            try
              for i := 0 to (IllSystem.Leaders.Count - 1) do
                if TLeader(IllSystem.Leaders[i]).Name = LeaderName
                then
                  begin
                    TLeader(IllSystem.Leaders[i]).Teams.Lock;
                    try
                      for j := 0 to (TLeader(IllSystem.Leaders[i]).Teams.Count - 1) do
                        TeamList.Values[IntToStr(j + 1)] := TTeam(TLeader(IllSystem.Leaders[i]).Teams[j]).Name;
                    finally
                      TLeader(IllSystem.Leaders[i]).Teams.Unlock;
                    end;
                  end;
            finally
              IllSystem.Leaders.Unlock;
            end;
            Result := TeamList.Text;
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      function TIllegalSystem.RDOGetCompanyList(LeaderName : widestring) : olevariant;
        var
          CompanyList : TStringList;
        begin
          try
            CompanyList := TStringList.Create;
            CompanyList.Values['1'] := 'Cult Foundation';
            CompanyList.Values['2'] := 'Cult Insurrectionists United';
            CompanyList.Values['3'] := 'Cult Warehouse';
            CompanyList.Values['4'] := 'Darkside Inc.';
            CompanyList.Values['5'] := 'New Age Cult';
            Result := CompanyList.Text;
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      function TIllegalSystem.RDOGetPlayerList : olevariant;
        var
          TycoonList : TStringList;
        begin
          try
            TycoonList := TStringList.Create;
            TycoonList.Values['1'] := 'Steve';
            TycoonList.Values['2'] := 'Aluas';
            TycoonList.Values['3'] := 'Marco';
            TycoonList.Values['4'] := 'Pepe';
            TycoonList.Values['5'] := 'Cepero';
            Result := TycoonList.Text;
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      function TIllegalSystem.RDOGetFacilityTypeList : olevariant;
        var
          FacilityList : TStringList;
        begin
          try
            FacilityList := TStringList.Create;
            FacilityList.Values['1'] := 'Bar';
            FacilityList.Values['2'] := 'Bank';
            FacilityList.Values['3'] := 'Liquor Plant';
            FacilityList.Values['4'] := 'Large Farm';
            FacilityList.Values['5'] := 'Movie Theatre';
            Result := FacilityList.Text;
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      function TIllegalSystem.RDOChangeReputation(LeaderName : widestring; NewRep : integer) : olevariant;
        var
          leader : TLeader;
          PrevLevel : integer;
        begin
          try
            leader := FindLeaderInSystem(LeaderName);
            PrevLevel := GetLeaderLevel(leader.Reputation);
            leader.Reputation := NewRep;
            if PrevLevel <> GetLeaderLevel(leader.Reputation)
            then
              leader.ChangeLevel;
            Result := CRIME_NOERROR
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      function TIllegalSystem.RDOChangeJailParameters(LeaderName : widestring; CrimName : widestring; TeamName : widestring; LawyersHours : widestring; Bribing : widestring) : olevariant;
        var
          i : integer;
          team : TTeam;
        begin
          if TeamName <> ''
          then
            begin
              team := FindTeamInSystem(LeaderName, TeamName);
              team.Bribing := StrToFloat(Bribing);
              team.LawyersHours := StrToInt(LawyersHours);
            end;
          try
            IllSystem.Criminals.Lock;
            try
              for i := 0 to (IllSystem.Criminals.Count - 1) do
                begin
                  if CrimName <> ''
                  then
                    begin
                      if TCriminal(IllSystem.Criminals[i]).Name = CrimName
                      then
                        begin
                          TCriminal(IllSystem.Criminals[i]).LawyersHours := StrToInt(LawyersHours);
                          TCriminal(IllSystem.Criminals[i]).Bribing := StrToFloat(Bribing);
                        end;
                    end
                  else
                    begin
                      if TCriminal(IllSystem.Criminals[i]).LeaderName = LeaderName
                      then
                        begin
                          if TeamName <> ''
                          then
                            begin
                              if TCriminal(IllSystem.Criminals[i]).TeamName = TeamName
                              then
                                begin
                                  TCriminal(IllSystem.Criminals[i]).LawyersHours := StrToInt(LawyersHours);
                                  TCriminal(IllSystem.Criminals[i]).Bribing := StrToFloat(Bribing);
                                end;
                            end
                          else
                            begin
                              TCriminal(IllSystem.Criminals[i]).LawyersHours := StrToInt(LawyersHours);
                              TCriminal(IllSystem.Criminals[i]).Bribing := StrToFloat(Bribing);
                            end;
                        end;
                    end;
                end;
            finally
              IllSystem.Criminals.Unlock;
            end;
            Result := 'OK';
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      function TIllegalSystem.RDOGetLawyersHoursCost : olevariant;
        begin
          try
            Result := '400'
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      function TIllegalSystem.CriminalWounded(CrimName : string; WoundLevel : integer) : integer;
        var
          crim : TCriminal;
          val : integer;
        begin
          crim := FindCriminalInSystem(CrimName);
          val := (Random(25) + 1);
          case WoundLevel of
            1:
              if crim.Health - val > 0
              then
                crim.Health := crim.Health - val
              else
                begin
                  crim.Health := 0;
                  RDOChangeCriminalState(crim.Name, 6);
                end;
            2:
              if crim.Health - (val + 20) > 0
              then
                crim.Health := crim.Health - val
              else
                begin
                  crim.Health := 0;
                  RDOChangeCriminalState(crim.Name, 6);
                end;
            3:
              if crim.Health - (val + 45) > 0
              then
                crim.Health := crim.Health - val
              else
                begin
                  crim.Health := 0;
                  RDOChangeCriminalState(crim.Name, 6);
                end;
            4:
              if crim.Health - (val + 65) > 0
              then
                crim.Health := crim.Health - val
              else
                begin
                  crim.Health := 0;
                  RDOChangeCriminalState(crim.Name, 6);
                end;
            5:
              if crim.Health - (val + 85) > 0
              then
                crim.Health := crim.Health - val
              else
                begin
                  crim.Health := 0;
                  RDOChangeCriminalState(crim.Name, 6);
                end;
          end;
          Result := crim.Health;
        end;

      procedure TIllegalSystem.PropagateTheBeat;
        var
          i : integer;
          j : integer;
          k : integer;
        begin
          IllSystem.Time := IllSystem.Time + 1;
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
          inherited;
          {
          case PeriodType of
            perHour :
              while PeriodCount > 0 do
                begin
                  PropagateTheBeat;
                  dec( PeriodCount );
                end;
            perMonth :
              while PeriodCount > 0 do
                begin
                  HandleCriminalsDisponibility;
                  CrimListChange := 1;
                  dec( PeriodCount );
                end;
          end;
          }
        end;

      function TIllegalSystem.RDOGetTime : olevariant;
        begin
          try
            Result := IllSystem.Time;
          except
            Result := CRIME_ERROR_Unknown;
          end;
        end;

      procedure TIllegalSystem.Loaded( aWorld : TWorld );
        begin
          inherited;
          // Do stuff here after everything is loaded (only if necessary)
        end;

      procedure TIllegalSystem.CompanyCreated( Company  : TCompany );
        begin
          inherited;
        end;

      procedure TIllegalSystem.TycoonCreated( Tycoon : TTycoon );
        begin
          inherited;
        end;

      procedure TIllegalSystem.FacilityCreated( Facility : TFacility );
        begin
          inherited;
        end;

      procedure TIllegalSystem.CompanyDeleted( Company : TCompany );
        begin
          inherited;
        end;

      procedure TIllegalSystem.TycoonDeleted( Tycoon : TTycoon );
        begin
          inherited;
        end;

      procedure TIllegalSystem.FacilityDeleted( Facility : TFacility );
        begin
          inherited;
        end;

      procedure TIllegalSystem.LoadFromBackup( Reader : IBackupReader );
        begin
          inherited;
        end;

      procedure TIllegalSystem.StoreToBackup( Writer : IBackupWriter );
        begin
          inherited;
        end;

     // Backup

     procedure RegisterBackup;
       begin
         RegisterClass( TIllegalSystem );
       end;

end.


