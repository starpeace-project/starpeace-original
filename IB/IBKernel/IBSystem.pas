unit IBSystem;

interface

  uses
    Classes, Collection, InterfaceCollection, Persistent, BackupInterfaces,
    MapStringToObject, SyncObjs, ClassStorageInt, MetaInstances, ClassStorage;

  const
    tidMetaMissionFamily  = 'tidMetaMissionFamily';
    tidMetaActionFamily   = 'tidMetaActionFamily';
    tidMetaHistoryFamily  = 'tidMetaHistoryFamily';
    tidMetaTrainingFamily = 'tidMetaTrainingFamily';
    cidModelServerAction  = 'cidModelServerAction';

  const
    SKILL_COUNT = 10;

  const
    MAXTEAM_MEMBERS = 8;

  const
    AVAILABLE_CRIMINALS = 10;

  const
    HISTREQ_TYCOON       = 0;
    HISTREQ_CRIMINAL     = 1;
    HISTREQ_DATE         = 2;
    HISTREQ_MISSION      = 3;
    HISTREQ_TRAINING     = 4;
    HISTREQ_TTEAM        = 5;

    MAX_HISTREQUIREMENTS = 6;

  const
    FREQ_HOUR  = 1;
    FREQ_DAY   = 24;
    FREQ_WEEK  = 7*FREQ_DAY;
    FREQ_MONTH = 31*FREQ_DAY;
    FREQ_YEAR  = 365*FREQ_DAY;

  const
    ERR_SUCCEDEED           = 0;
    ERR_TEAMINMISSION       = 1;
    ERR_TOOMANYMEMBERS      = 2;
    ERR_UNKNOWN             = 3;
    ERR_NOMISSION           = 4;
    ERR_TYCOONALREADYLOGGED = 5;
    ERR_NOSUCHTYCOON        = 6;
    ERR_TYCOONNOTLOGGED     = 7;
    ERR_TYCOONALREADYEXISTS = 8;
    ERR_TEAMALREADYEXISTS   = 9;
    ERR_CRIMINALHIRED       = 10;
    ERR_TEAMNOTLOCATED      = 11;
    ERR_NOSUCHTEAM          = 12;
    ERR_NOSUCHCRIMINAL      = 13;
    ERR_NOSUCHMISSION       = 14;
    ERR_TEAMNOTREADY        = 15;
    ERR_UNABLETOASSIGNROLES = 16;
    ERR_INVALIDSTATUSCHANGE = 17;

    ERR_MAXERROR            = 50;

  const
    TEAMSTATE_READY     = 0;
    TEAMSTATE_UNLOCATED = 1;
    TEAMSTATE_INMISSION = 2;

  const
    CRIMSTATUS_ONTHEMARKET = 1;
    CRIMSTATUS_READY       = 2;
    CRIMSTATUS_INMISSION   = 3;
    CRIMSTATUS_CAPTURED    = 4;
    CRIMSTATUS_DEAD        = 5;
    CRIMSTATUS_TRAINING    = 6;

  const
    MAX_ROLES  = 9;
    MAX_SKILLS = 10;

  const
    ROLE_LEADER    = 0;
    ROLE_DRIVER    = 1;
    ROLE_GORILLA   = 2;
    ROLE_ARTIFICER = 3;
    ROLE_STALKER   = 4;
    ROLE_HACKER    = 5;
    ROLE_DOCTOR    = 6;
    ROLE_SNIPER    = 7;
    ROLE_FALSIFIER = 8;

  const
    SKILL_LEADERSHIP = 0;
    SKILL_DRIVING    = 1;
    SKILL_BRAWLING   = 2;
    SKILL_FIREARMS   = 3;
    SKILL_STALKING   = 4;
    SKILL_COMPUTER   = 5;
    SKILL_DEMOLITION = 6;
    SKILL_STEALTH    = 7;
    SKILL_MEDICINE   = 8;
    SKILL_FORGERY    = 9;

  const // RDOGetCriminalList
    EXPERT_PREFIX = 'e_';
    ROOKIE_PREFIX = 'r_';

  const // Training States
    TRAININGSTATE_TRAINING = 1;
    TRAININGSTATE_PAUSED   = 2;

  type
    TCriminalSex = ( csMale, csFemale );
    TErrorCode   = integer;

  type
    TLocationInformation =
      packed record
        Population     : integer;
        Education      : integer;
        PoliceCoverage : integer;
      end;

    TRoleInfo =
      packed record
        role  : integer;
        skill : integer;
      end;

    TRoles = array[0..MAXTEAM_MEMBERS] of TRoleInfo;

  type
    TCriminal        = class;
    TCriminalTycoon  = class;
    TSkillCollection = class;
    TTeam            = class;
    TMission         = class;
    TCriminalMarket  = class;
    TAction          = class;
    CAction          = class of TAction;
    THistoryItem     = class;
    TTraining        = class;

    TSimulationObject =
      class(TPersistent)
        public
          procedure Act( Frequency : integer ); virtual; abstract;
      end;

    TSimObjectInfo =
      class(TPersistent)
        public
          constructor Create( aSimObject : TSimulationObject; aFreq : integer );
        public
          procedure Simulate;
        private
          fSimObject : TSimulationObject;
          fFreq      : integer;
          fCurrTick  : integer;
        public
          property SimObject : TSimulationObject read fSimObject;
          property Frequency : integer           read fFreq;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    TLocalCriminalMarket =
      class(TSimulationObject)
        public
          constructor Create( aLocation : string; aCriminalMarket : TCriminalMarket );
          destructor  Destroy; override;
        public
          procedure getCriminalList( ExpCriminals : TCollection; RookieCriminals : TCollection );
          function  extractCriminal( CriminalId : integer ) : TCriminal;
          procedure addExpertCriminal( Criminal : TCriminal );
        public
          procedure Act( Frequency : integer ); override;
        private
          fLocation       : string;
          fExperts        : TCollection;
          fRookies        : TCollection;
          fCriminalMarket : TCriminalMarket;
        private
          function GenerateCriminal : TCriminal;
          function findCriminal( CriminalId : integer; where : TCollection ) : TCriminal;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    TCriminalMarket =
      class(TSimulationObject)
        public
          constructor Create;
          destructor  Destroy; override;
        public
          procedure InitTexts;
        public
          procedure AddLocation( aLocation : string );
          procedure getCriminalList( Location : string; ExpCriminals : TCollection; RookieCriminals : TCollection );
          function  HireCriminal( Location : string; CriminalId : integer ) : TCriminal;
          procedure FireCriminal( Location : string; Criminal : TCriminal );
          function  GenerateId : integer;
        private
          fLocations   : TMapStringToObject;
          fLastId      : integer;
        private // Criminal Generation
          fMaleNames      : TStringList;
          fFemaleNames    : TStringList;
          fLastNames      : TStringList;
          fMalePictures   : TStringList;
          fFemalePictures : TStringList;
        public
          property MaleNames      : TStringList read fMaleNames;
          property FemaleNames    : TStringList read fFemaleNames;
          property LastNames      : TStringList read fLastNames;
          property MalePictures   : TStringList read fMalePictures;
          property FemalePictures : TStringList read fFemalePictures;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        public
          procedure Act( Frequency : integer ); override;
      end;

    TIBClientView =
      class
        public
          constructor Create( aCriminalTycoon : TCriminalTycoon );
        public
          procedure Report( aReport : string ); virtual; abstract;
        protected
          fTycoon : TCriminalTycoon;
      end;

    // data passed to the actions

    TInternalActionData =
      record
        Criminals     : array[0..MAXTEAM_MEMBERS - 1] of TCriminal;
        CriminalCount : integer;
        Team          : TTeam;
      end;

    TModelServerActionData =
      record
        Id        : integer;
        Intensity : single;
        Radius    : integer;
      end;

    TMetaAction =
      class(TMetaInstance)
        public
          constructor Create( id : string; actionClass : CAction; isInternal : boolean );
        public
          function Instantiate( const Info ) : TAction; virtual;
        private
          fInternal    : boolean;
          fActionClass : CAction;
        public
          property Internal : boolean read fInternal;
      end;

    TAction =
      class(TPersistent)
        public
          constructor Create( aMetaAction : TMetaAction );
        public
          procedure Execute( const Info; var outcome : single ); virtual; abstract;
          procedure Init( const Info ); virtual; abstract;
        protected
          fMetaAction : TMetaAction;
        public
          property MetaAction : TMetaAction read fMetaAction;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    TRequierements   = array[0..MAX_HISTREQUIREMENTS] of integer;
    TMetaHistoryItem =
      class(TMetaInstance)
        public
          constructor Create( anId : string );
        public
          procedure AddRequierement( Req : integer );
          function  Requires( Req : integer ) : boolean;
          function  Instantiate : THistoryItem;
        private
          //fId               : string;
          fRequirementCount : integer;
          fRequirements     : TRequierements;
        public
          //property Id               : string         read fId;
          property RequirementCount : integer        read fRequirementCount;
          property Requirements     : TRequierements read fRequirements;
      end;

    THistoryItem =
      class(TPersistent)
        public
          constructor Create( aMetaHistoryItem : TMetaHistoryItem );
          destructor  Destroy; override;
        public
          procedure addRequierement( Req : integer; data : string );
          function  serialize : string;
        protected
          fMetaClass : TMetaHistoryItem;
          fData      : TStringList;
        public
          property MetaClass : TMetaHistoryItem read fMetaClass;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    TTownInfo =
      class(TPersistent)
        public
          constructor Create( aName : string );
        private
          fName : string;
        public
          property Name : string read fName;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    TIBSystem =
      class(TPersistent)
        public
          constructor Create;
          destructor  Destroy; override;
        public
          procedure InitMissions;
        published
          // LogIn procedure
          function  RDOLogCriminalIn( TycoonName : widestring ) : olevariant;
          procedure RDOLogCriminalOff( TycoonName : widestring );
          function  RDOCreateCriminalId( TycoonName : widestring; CriminalId : widestring ) : olevariant;
          // Tycoon Information
          function  RDOGetTycoonInfo( TycoonId : integer ): olevariant;
          function  RDOGetTeamInfo( TycoonId : integer; TeamName : widestring ): olevariant;
          function  RDOCreateTeam( TycoonId : integer; TeamName : widestring ) : olevariant;
          function  RDOSetTeamHeadQuarter( TycoonId : integer; TeamName : widestring; x, y : integer; Location : WideString ) : olevariant;
          // Criminals
          function  RDOHireCriminal( TycoonId : integer; CriminalId : integer; TeamName : widestring ): olevariant;
          function  RDOFireCriminal( TycoonId : integer; TeamName : widestring; CriminalId : integer ): olevariant;
          function  RDOGetCriminalList( TycoonId : integer; TeamName : widestring ): olevariant;
          // Mission
          function  RDOStartMission( TycoonId : integer; TeamName : widestring; MissionId : widestring; MissionInfo : widestring ): olevariant;
          function  RDOAbortMission( TycoonId : integer; TeamName : widestring ): olevariant;
        public
          procedure Act;
          procedure HandleEvent( event : integer; var data );
          procedure getLocationInformation( Location : string; var Info : TLocationInformation );
          procedure addLocation( Location : string );
        public // Simulation Objects
          procedure AddSimulationObject( aSimObject : TSimulationObject; Freq : integer );
          procedure DeleteSimulationObject( aSimObject : TSimulationObject; Freq : integer );
        public
          procedure AddCriminalHistoryItem( Criminal : TCriminal; Id : string; Date : string; Team : TTeam; Mission : TMission; Training : TTraining );
          procedure AddTycoonHistoryItem( Tycoon : TCriminalTycoon; Id : string; Date : string; Team : TTeam; aCriminal : TCriminal );
          procedure AddTeamHistoryItem( Team : TTeam; Id : string; Date : string; Tycoon : TCriminalTycoon; Mission : TMission; aCriminal : TCriminal );
        private
          procedure Simulate;
          function  CreateClientView( Tycoon : TCriminalTycoon ) : TIBClientView;
        private
          fCriminalTycoons : TMapStringToObject;
          fAgencies        : TLockableCollection;
          fSimObjects      : TLockableCollection;
          fCriminalMarket  : TCriminalMarket;
          fClientViews     : TLockableCollection;
          fLocations       : TLockableCollection;
          //fDataFolder      : string;
        private
          function getLocation( name : string ) : TTownInfo;
        private
          procedure SerializeCriminal( Prefix : string; Criminal : TCriminal; where : TStringList; idx : integer );
        private // Critical Sections
          fSimulationSection : TCriticalSection;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    TCriminalTycoon =
      class(TSimulationObject)
        public
          constructor Create( aIBSystem : TIBSystem; aCriminalName : string; aTycoonName : string );
          destructor  Destroy; override;
        public
          procedure addHistoryItem( Item : THistoryItem );
          function  getTeamByName( aName : string ) : TTeam;
        public
          procedure Act( Frequency : integer ); override;
        protected
          fCriminalName : string;
          fTycoonName   : string;
          fTeams        : TLockableCollection;
          fLogged       : boolean;
          fClientView   : TIBClientView;
          fHistory      : TCollection;
        public
          property TycoonName   : string read fTycoonName;
          property CriminalName : string read fCriminalName;
          property Logged       : boolean read fLogged write fLogged;
          property Teams        : TLockableCollection read fTeams;
          property ClientView   : TIBClientView read fClientView write fClientView;
          property History      : TCollection read fHistory;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    TMetaMission =
      class(TMetaInstance)
        public
          function Instantiate( MissionInfo : string; Owner : TTeam; out Mission : TMission ) : TErrorCode; virtual; abstract;
        protected
          fName : string;
        public
          property Name : string read fName;
      end;

    TMission =
      class(TSimulationObject)
        public
          constructor Create( aOwner : TTeam; aMetaMission : TMetaMission ); virtual;
        public
          procedure StartMission;       virtual;
          procedure ProceedWithMission; virtual;
          procedure EndMission;         virtual;
          procedure Abort;              virtual;
          procedure Escaped;            virtual;
        public
          procedure Act( Frequency : integer ); override;
        protected
          fOwner       : TTeam;
          fAbort       : boolean;
          fEscaped     : boolean;
          fMetaMission : TMetaMission;
        public
          property Owner       : TTeam        read fOwner;
          property MetaMission : TMetaMission read fMetaMission;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    TTeam =
      class(TSimulationObject)
        public
          constructor Create( aName : string; aOwner : TCriminalTycoon );
          destructor  Destroy; override;
        public
          function  AddCriminal( aCriminal : TCriminal ) : TErrorCode;
          function  DeleteCriminal( aCriminal : TCriminal ) : TErrorCode;
          function  FindCriminal( aName : string ) : TCriminal;
          function  getCriminalById( CriminalId : integer ) : TCriminal;
          function  AssignMission( aMission : TMission; MissionInfo : string ) : TErrorCode;
          function  getCriminalForTask( role, skill : integer; assdRoleCount : integer; const assdRoles : TRoles ) : TCriminal;
          function  setHeadquarter( x, y : integer; aLocation : string ) : TErrorCode;
          procedure addHistoryItem( HistoryItem : THistoryItem );
        public
          procedure Act( Frequency : integer ); override;
        protected
          fName      : string;
          fOwner     : TCriminalTycoon;
          fCriminals : TLockableCollection;
          fLocation  : TTownInfo;
          fState     : integer;
          fMission   : TMission;
          fHQX       : integer;
          fHQY       : integer;
          fHistory   : TCollection;
        protected
          procedure Moved;
        public
          property Owner     : TCriminalTycoon read fOwner;
          property Name      : string read fName;
          property Criminals : TLockableCollection read fCriminals;
          property Location  : TTownInfo read fLocation;
          property State     : integer read fState;
          property Mission   : TMission read fMission;
          property HQX       : integer read fHQX;
          property HQY       : integer read fHQY;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    TCriminal =
      class(TSimulationObject)
        public
          constructor Create( aName : string; anAge : integer; aSex : TCriminalSex; aLocation : string; aPictureId : string; anId : integer );
          destructor  Destroy; override;
        public
          procedure addHistoryItem( Item : THistoryItem );
          function  setStatus( anewStatus : integer; var info ) : TErrorCode;
          function  CheckStability( Skill : integer; PsiFactor : single ) : single;
        public
          procedure Act( Frequency : integer ); override;
        protected
          fName      : string;
          fAge       : integer;
          fSex       : TCriminalSex;
          fSkills    : TSkillCollection;
          fPictureId : string;
          fTeam      : TTeam;
          fId        : integer;
          fRole      : integer;
          // status
          fStatus    : integer;
          fStability : single;
          // History
          fHistory   : TCollection;
        protected
          function getLocation : TTownInfo;
        public
          property Name      : string read fName;
          property Id        : integer read fId;
          property Age       : integer read fAge;
          property Sex       : TCriminalSex read fSex;
          property Skills    : TSkillCollection read fSkills;
          property Team      : TTeam read fTeam;
          property Status    : integer read fStatus;
          property PictureId : string read fPictureId;
          property Role      : integer read fRole;
          property Location  : TTownInfo read getLocation;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    TSkill      = single;
    TSkillArray = array[0..SKILL_COUNT] of TSkill;

    TSkillCollection =
      class(TPersistent)
        public
          constructor Create;
        private
          fSkillCount : integer;
          fSkills     : TSkillArray;
        public
          property Count  : integer read fSkillCount write fSkillCount;
          property Skills : TSkillArray read fSkills write fSkills;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    // Trainings

    TMetaTraining =
      class(TMetaInstance)
        public
          procedure TrainingComplete( percent : integer; Trainee : TObject ); virtual; abstract;
        private
          fId        : string;
          fDuration  : integer; // in days
          fFrequency : integer;
          fName      : string;
        public
          property Id        : string  read fId;
          property Duration  : integer read fDuration;
          property Frequency : integer read fFrequency;
          property Name      : string  read fName;
      end;

    TTraining =
      class(TSimulationObject)
        public
          constructor Create( aMetaTraining : TMetaTraining; aTrainee : TObject );
        public
          procedure Act( Frequency : integer ); override;
        public
          procedure Abort;  virtual;
          procedure Pause;  virtual;
          procedure Resume; virtual;
        protected
          fState        : integer;
          fTrainee      : TObject;
          fMetaTraining : TMetaTraining;
          fPercent      : integer;
          fTimeElapsed  : integer;
          fFrequency    : integer;
          procedure Finished; virtual;
        public
          property Frequency    : integer       read fFrequency;
          property MetaTraining : TMetaTraining read fMetaTraining;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

  var
    theIBSystem         : TIBSystem;
    theMetaClassStorage : TClassStorage;

implementation

  uses
    Windows, SysUtils, MissionEngine, TestClientView, Registry;

  const
    IB_KEY = 'Software\Oceanus\Five\ModelServer\';

  var
    IBDataFolder : string = '';

  function getIBDataFolder : string;
    var
      Reg : TRegistry;
    begin
      if IBDataFolder = ''
        then
          begin
            Reg := TRegistry.Create;
            Reg.RootKey := HKEY_LOCAL_MACHINE;
            if Reg.OpenKey(IB_KEY, false)
              then IBDataFolder := Reg.ReadString('IBData');
          end;
      result := IBDataFolder;
    end;


  // TSimObjectInfo

  constructor TSimObjectInfo.Create( aSimObject : TSimulationObject; aFreq : integer );
    begin
      inherited Create;
      fSimObject := aSimObject;
      fFreq      := aFreq;
      fCurrTick  := aFreq;
    end;

  procedure TSimObjectInfo.Simulate;
    begin
      dec( fCurrTick );
      if fCurrTick = 0
        then
          begin
            fCurrTick := fFreq;
            fSimObject.Act( fFreq );
          end;
    end;

  procedure TSimObjectInfo.LoadFromBackup(Reader : IBackupReader);
    begin
      inherited;
      Reader.ReadObject('SimObj', fSimObject, nil);
      fFreq := Reader.ReadInteger('Freq', 1);
      fCurrTick := Reader.ReadInteger('Tick', 1);
    end;

  procedure TSimObjectInfo.StoreToBackup(Writer : IBackupWriter);
    begin
      inherited;
      Writer.WriteObjectRef('SimObj', fSimObject);
      Writer.WriteInteger('Freq', fFreq);
      Writer.WriteInteger('Tick', fCurrTick);
    end;

  // TLocalCriminalMarket

  constructor TLocalCriminalMarket.Create( aLocation : string; aCriminalMarket : TCriminalMarket );
    begin
      inherited Create;
      fLocation       := aLocation;
      fExperts        := TCollection.Create( 10, rkBelonguer );
      fRookies        := TCollection.Create( 10, rkBelonguer );
      fCriminalMarket := aCriminalMarket;
    end;

  destructor TLocalCriminalMarket.Destroy;
    begin
      fExperts.Free;
      fRookies.Free;
      inherited;
    end;

  procedure TLocalCriminalMarket.getCriminalList( ExpCriminals : TCollection; RookieCriminals : TCollection );
    var
      i    : integer;
      Crim : TCriminal;
    begin
      try
        for i := 0 to pred(fExperts.Count) do
          ExpCriminals.Insert( fExperts[i] );

        for i := 0 to pred(fRookies.Count) do
          RookieCriminals.Insert( fRookies[i] );

        if fRookies.Count < AVAILABLE_CRIMINALS
          then
            begin
              for i := fRookies.Count to AVAILABLE_CRIMINALS do
                begin
                  Crim := GenerateCriminal();
                  fRookies.Insert( Crim );
                  RookieCriminals.Insert( Crim );
                end;
            end;
      except
      end;
    end;

  function TLocalCriminalMarket.extractCriminal( CriminalId : integer ) : TCriminal;
    begin
      try
        result := findCriminal( CriminalId, fExperts );
        if result <> nil
          then fExperts.Extract( result )
          else
            begin
              result := findCriminal( CriminalId, fRookies );
              if result <> nil
                then fRookies.Extract( result )
            end;
      except
        result := nil;
      end;
    end;

  procedure TLocalCriminalMarket.addExpertCriminal( Criminal : TCriminal );
    begin
      try
        if findCriminal( Criminal.Id, fExperts ) = nil
          then fExperts.Insert( Criminal );
      except
      end;
    end;

  procedure TLocalCriminalMarket.Act( Frequency : integer );
    begin
    end;

  function TLocalCriminalMarket.GenerateCriminal : TCriminal;
    const
      GENPARAM_COUNT = 5;

    const
      GEN_POPULATION     = 0;
      GEN_EDUCATION      = 1;
      GEN_POLICECOVERAGE = 2;
      GEN_MALE           = 3;
      GEN_FEMALE         = 4;

    type
      TSkillGenInfo =
        packed record
          MinValue   : integer;
          MaxValue   : integer;
          Importance : array[0..pred(GENPARAM_COUNT)] of integer;
        end;

    const
      GENCRIMINAL_INFO : array[0..pred(SKILL_COUNT)] of TSkillGenInfo =
        (
          //                                     POP  EDU  POL  MALE FMALE
          (MinValue:0;  MaxValue:40;  Importance:( 20,  40,   0,   40,  20 )), // SKILL_LEADERSHIP
          (MinValue:50; MaxValue:70;  Importance:( 40,   0,  40,   10,  10 )), // SKILL_DRIVING
          (MinValue:0;  MaxValue:70;  Importance:(  0,   0,   0,  100,   0 )), // SKILL_BRAWLING
          (MinValue:0;  MaxValue:70;  Importance:( 60,  30,  10,    0,   0 )), // SKILL_FIREARMS
          (MinValue:0;  MaxValue:50;  Importance:( 30,  50,  20,    0,   0 )), // SKILL_STALKING
          (MinValue:0;  MaxValue:60;  Importance:( 20,  80,   0,    0,   0 )), // SKILL_COMPUTER
          (MinValue:0;  MaxValue:50;  Importance:( 50,  20,   0,   30,   0 )), // SKILL_DEMOLITION
          (MinValue:0;  MaxValue:100; Importance:( 20,   0,  20,   20,  40 )), // SKILL_STEALTH
          (MinValue:0;  MaxValue:80;  Importance:( 20,  80,   0,    0,   0 )), // SKILL_MEDICINE
          (MinValue:0;  MaxValue:50;  Importance:(  0, 100,   0,    0,   0 ))  // SKILL_FORGERY
        );

      GENPARAMS_VALUE : array[0..pred(GENPARAM_COUNT)] of integer =
        // POP   EDUCATION  POLICE   MALE  FEMALE
        ( 100000,    100,      100,     1,   1 );

    function percent( value, maxValue : integer ) : integer;
      begin
        result := round( (value*100)/maxValue );
        if result > 100
          then result := 100;
      end;

    function applypercent( perc : integer; maxValue : integer ) : integer;
      begin
        result := round( (perc*maxValue)/100 );
      end;

    function ParamToPercent( Param : integer; Value : integer ): integer;
      begin
        result := percent( Value, GENPARAMS_VALUE[Param] );
      end;

    function GenerateSkill( Criminal : TCriminal; Info : TLocationInformation; Skill : integer ) : single;
      var
        p    : integer;
        diff : integer;
      begin
        p    := GENCRIMINAL_INFO[Skill].MinValue;
        diff := GENCRIMINAL_INFO[Skill].MaxValue - GENCRIMINAL_INFO[Skill].MinValue;

        p := p + random( applypercent( applypercent( ParamToPercent(GEN_POPULATION, Info.Population), GENCRIMINAL_INFO[Skill].Importance[GEN_POPULATION] ), diff ));
        p := p + random( applypercent( applypercent( ParamToPercent(GEN_EDUCATION, Info.Education), GENCRIMINAL_INFO[Skill].Importance[GEN_POPULATION] ), diff ));
        p := p + random( applypercent( applypercent( ParamToPercent(GEN_POLICECOVERAGE, Info.PoliceCoverage), GENCRIMINAL_INFO[Skill].Importance[GEN_POPULATION] ), diff ));
        p := p + random( applypercent( applypercent( ParamToPercent(GEN_MALE, integer(Criminal.Sex = csMale)), GENCRIMINAL_INFO[Skill].Importance[GEN_POPULATION] ), diff ));
        p := p + random( applypercent( applypercent( ParamToPercent(GEN_FEMALE, integer(Criminal.Sex = csFemale)), GENCRIMINAL_INFO[Skill].Importance[GEN_POPULATION] ), diff ));

        result := p/100;
      end;

    const
      MALE_PROB       = 0.75;
      MALE_MINAGE     = 18;
      MALE_AGERANGE   = 65 - MALE_MINAGE;
      FEMALE_MINAGE   = 20;
      FEMALE_AGERANGE = 50 - FEMALE_MINAGE;

    function GenerateName( Sex : TCriminalSex ) : string;
      begin
        case Sex of
          csMale   : result := fCriminalMarket.MaleNames[random(fCriminalMarket.MaleNames.Count)];
          csFemale : result := fCriminalMarket.FemaleNames[random(fCriminalMarket.FemaleNames.Count)];
        end;

        result := result + ' ' + fCriminalMarket.LastNames[random(fCriminalMarket.LastNames.Count)];
      end;

    function GenerateAge( Sex : TCriminalSex ) : integer;
      begin
        result := 25; // to avoid the compiler warning

        case Sex of
          csMale   : result := MALE_MINAGE   + random(MALE_AGERANGE);
          csFemale : result := FEMALE_MINAGE + random(FEMALE_AGERANGE);
        end;
      end;

    function GeneratePicture( Sex : TCriminalSex ) : string;
      begin
        result := ''; // same
        case Sex of
          csMale   : result := fCriminalMarket.MalePictures[random(fCriminalMarket.MalePictures.Count)];
          csFemale : result := fCriminalMarket.FemalePictures[random(fCriminalMarket.FemalePictures.Count)];
        end;
      end;

    var
      aName      : string;
      anAge      : integer;
      aSex       : TCriminalSex;
      aLocation  : string;
      aPictureId : string;
      anId       : integer;
      i          : integer;
      LocInfo    : TLocationInformation;
    begin
      try
        if random < MALE_PROB
          then aSex := csMale
          else aSex := csFemale;

        aLocation  := fLocation;
        anId       := fCriminalMarket.GenerateId;

        aName      := GenerateName( aSex );
        anAge      := GenerateAge( aSex );
        aPictureId := GeneratePicture( aSex );

        result  := TCriminal.Create( aName, anAge, aSex, aLocation, aPictureId, anId );

        theIBSystem.getLocationInformation( aLocation, LocInfo ); //>> make this a function
        for i := 0 to pred(SKILL_COUNT) do
          result.Skills.fSkills[i] := GenerateSkill( result, LocInfo, i );
      except
        result := nil;
      end;
    end;

  function TLocalCriminalMarket.findCriminal( CriminalId : integer; where : TCollection ) : TCriminal;
    var
      i     : integer;
      found : boolean;
    begin
      i     := 0;
      found := false;
      while (i < where.Count) and not found do
        begin
          found := TCriminal(where[i]).Id = CriminalId;
          if not found
            then inc( i );
        end;
      if found
        then result := TCriminal(where[i])
        else result := nil;
    end;

  procedure TLocalCriminalMarket.LoadFromBackup(Reader : IBackupReader);
    begin
      inherited;
      fLocation := Reader.ReadString('Loc', '');
      Reader.ReadObject('Exps', fExperts, nil);
      if fExperts = nil
        then fExperts := TCollection.Create(10, rkBelonguer);
      Reader.ReadObject('Rookies', fRookies, nil);
      if fRookies = nil
        then fRookies := TCollection.Create(10, rkBelonguer);
      Reader.ReadObject('CrMrk', fCriminalMarket, nil);
    end;

  procedure TLocalCriminalMarket.StoreToBackup(Writer : IBackupWriter);
    begin
      inherited;
      Writer.WriteString('Loc', fLocation);
      Writer.WriteLooseObject('Exps', fExperts);
      Writer.WriteLooseObject('Rookies', fRookies);
      Writer.WriteObjectRef('CrMrk', fCriminalMarket);
    end;


  // TCriminalMarket

  constructor TCriminalMarket.Create;
    begin
      inherited Create;
      fLocations := TMapStringToObject.Create( mmOwn );
      fLastId    := 0;
      InitTexts;
    end;

  destructor TCriminalMarket.Destroy;
    begin
      fLocations.Free;
      inherited;
    end;

  procedure TCriminalMarket.InitTexts;
    const
      MALENAME_FILE      = 'males.txt';
      FEMALENAME_FILE    = 'females.txt';
      LASTNAME_FILE      = 'lastname.txt';
      MALEPICTURE_FILE   = 'malepics.txt';
      FEMALEPICTURE_FILE = 'fempics.txt';

    var
      MaleNamesFile     : string;
      FemaleNameFile    : string;
      LastNameFile      : string;
      MalePictureFile   : string;
      FemalePictureFile : string;
      DataFolder        : string;

    begin
      DataFolder := getIBDataFolder + 'Names\';

      fMaleNames      := TStringList.Create;
      fFemaleNames    := TStringList.Create;
      fLastNames      := TStringList.Create;
      fMalePictures   := TStringList.Create;
      fFemalePictures := TStringList.Create;

      MaleNamesFile     := DataFolder + MALENAME_FILE;
      FemaleNameFile    := DataFolder + FEMALENAME_FILE;
      LastNameFile      := DataFolder + LASTNAME_FILE;
      MalePictureFile   := DataFolder + MALEPICTURE_FILE;
      FemalePictureFile := DataFolder + FEMALEPICTURE_FILE;

      fMaleNames.LoadFromFile( MaleNamesFile );
      fFemaleNames.LoadFromFile( FemaleNameFile );
      fLastNames.LoadFromFile( LastNameFile );
      fMalePictures.LoadFromFile( MalePictureFile );
      fFemalePictures.LoadFromFile( FemalePictureFile );
    end;

  procedure TCriminalMarket.AddLocation( aLocation : string );
    begin
      try
        if fLocations.Items[aLocation] = nil
          then fLocations.Items[aLocation] := TLocalCriminalMarket.Create( aLocation, self );
      except
      end;
    end;

  procedure TCriminalMarket.getCriminalList( Location : string; ExpCriminals : TCollection; RookieCriminals : TCollection );
    var
      LocalMarket : TLocalCriminalMarket;
    begin
      try
        LocalMarket := TLocalCriminalMarket(fLocations.Items[Location]);
        if LocalMarket <> nil
          then LocalMarket.getCriminalList( ExpCriminals, RookieCriminals );
      except
      end;
    end;

  function TCriminalMarket.HireCriminal( Location : string; CriminalId : integer ) : TCriminal;
    var
      LocalMarket : TLocalCriminalMarket;
    begin
      try
        LocalMarket := TLocalCriminalMarket(fLocations.Items[Location]);
        if LocalMarket <> nil
          then result := LocalMarket.extractCriminal( CriminalId )
          else result := nil;
      except
        result := nil;
      end;
    end;

  procedure TCriminalMarket.FireCriminal( Location : string; Criminal : TCriminal );
    var
      LocalMarket : TLocalCriminalMarket;
    begin
      try
        LocalMarket := TLocalCriminalMarket(fLocations.Items[Location]);
        if LocalMarket <> nil
          then
            begin
              Criminal.setStatus( CRIMSTATUS_ONTHEMARKET, self );
              LocalMarket.addExpertCriminal( Criminal );
            end;
        //>> notify
      except
      end;
    end;

  function TCriminalMarket.GenerateId : integer;
    begin
      result := fLastId;
      inc( fLastId );
    end;

  procedure TCriminalMarket.LoadFromBackup(Reader : IBackupReader);
    begin
      inherited;
      Reader.ReadObject('Locations', fLocations, nil);
      fLastId := Reader.ReadInteger('LstId', 0);
      InitTexts;
    end;

  procedure TCriminalMarket.StoreToBackup(Writer : IBackupWriter);
    begin
      inherited;
      Writer.WriteObject('Locations', fLocations);
      Writer.WriteInteger('LstId', fLastId);
    end;

  procedure TCriminalMarket.Act(Frequency : integer);
    begin
    end;

  // TIBClientView

  constructor TIBClientView.Create( aCriminalTycoon : TCriminalTycoon );
    begin
      inherited Create;
      fTycoon := aCriminalTycoon;
    end;

  // TMetaAction

  constructor TMetaAction.Create( id : string; actionClass : CAction; isInternal : boolean );
    begin
      inherited Create(id);
      fInternal    := isInternal;
      fActionClass := actionClass;
    end;

  function TMetaAction.Instantiate( const Info ) : TAction;
    begin
      result := fActionClass.Create( self );
      result.Init( Info );
    end;

  // TAction

  constructor TAction.Create( aMetaAction : TMetaAction );
    begin
      inherited Create;
      fMetaAction := aMetaAction;
    end;

  procedure TAction.LoadFromBackup(Reader : IBackupReader);
    var
      aux : string;
    begin
      inherited;
      aux := Reader.ReadString('MetaAct', '');
      fMetaAction := TMetaAction(TheClassStorage.ClassById[tidMetaActionFamily, aux]);
    end;

  procedure TAction.StoreToBackup(Writer : IBackupWriter);
    begin
      inherited;
      Writer.WriteString('MetaAct', fMetaAction.Id);
    end;


  // TMetaHistoryItem

  constructor TMetaHistoryItem.Create( anId : string );
    begin
      inherited Create(anId);
      //fId := anId;
    end;

  procedure TMetaHistoryItem.AddRequierement( Req : integer );
    begin
      fRequirements[fRequirementCount] := Req;
      inc( fRequirementCount );
    end;

  function TMetaHistoryItem.Requires( Req : integer ) : boolean;
    var
      i     : integer;
      found : boolean;
    begin
      i     := 0;
      found := false;

      while (i < fRequirementCount) and not found do
        begin
          found := fRequirements[i] = Req;
          inc( i );
        end;

      result := found;
    end;

  function TMetaHistoryItem.Instantiate : THistoryItem;
    begin
      result := THistoryItem.Create( self );
    end;

  // THistoryItem

  constructor THistoryItem.Create( aMetaHistoryItem : TMetaHistoryItem );
    begin
      inherited Create;
      fMetaClass := aMetaHistoryItem;
      fData      := TStringList.Create;
    end;

  destructor THistoryItem.Destroy;
    begin
      fData.Free;
      inherited;
    end;

  procedure THistoryItem.addRequierement( Req : integer; data : string );
    const
      REQUIEREMENT_TO_STR : array[0..pred(MAX_HISTREQUIREMENTS)] of string =
                              (
                                'TYCOON',
                                'CRIMINAL',
                                'DATE',
                                'MISSION',
                                'TRAINING',
                                'TTEAM'
                              );

    begin
      if fMetaClass.Requires( Req )
        then fData.Values[REQUIEREMENT_TO_STR[Req]] := data;
    end;

  function THistoryItem.serialize : string;
    begin
      result := fData.Text;
    end;

  procedure THistoryItem.LoadFromBackup(Reader : IBackupReader);
    var
      aux : string;
    begin
      inherited;
      aux := Reader.ReadString('MetaItem', '');
      fMetaClass := TMetaHistoryItem(TheClassStorage.ClassById[tidMetaHistoryFamily, aux]);
      Reader.ReadObject('Data', fData, nil);
    end;

  procedure THistoryItem.StoreToBackup(Writer : IBackupWriter);
    begin
      inherited;
      Writer.WriteString('MetaItem', fMetaClass.Id);
      Writer.WriteObject('Data', fData);
    end;


  // TTownInfo

  constructor TTownInfo.Create( aName : string );
    begin
      inherited Create;
      fName := aName;
    end;

  procedure TTownInfo.LoadFromBackup(Reader : IBackupReader);
    begin
      inherited;
      fName := Reader.ReadString('Name', '');
    end;

  procedure TTownInfo.StoreToBackup(Writer : IBackupWriter);
    begin
      inherited;
      Writer.WriteString('Name', fName);
    end;


  // TIBSystem

  constructor TIBSystem.Create;
    begin
      inherited Create;
      fCriminalTycoons   := TMapStringToObject.Create( mmOwn );
      fAgencies          := TLockableCollection.Create( 10, rkBelonguer );
      fSimObjects        := TLockableCollection.Create( 10, rkBelonguer );
      fClientViews       := TLockableCollection.Create( 10, rkBelonguer );
      fLocations         := TLockableCollection.Create( 10, rkBelonguer );
      fCriminalMarket    := TCriminalMarket.Create;
      fSimulationSection := TCriticalSection.Create;
      InitMissions;
    end;

  destructor TIBSystem.Destroy;
    begin
      fCriminalTycoons.Free;
      fAgencies.Free;
      fSimObjects.Free;
      fClientViews.Free;
      fLocations.Free;
      fCriminalMarket.Free;
      fSimulationSection.Free;

      DestroyMissionLoader;
      inherited;
    end;

  procedure TIBSystem.InitMissions;
    begin
      InitMissionLoader;
      TheMetaMissionLoader.CheckDirectory( getIBDataFolder + '\Missions' );
    end;

  function TIBSystem.RDOLogCriminalIn( TycoonName : widestring ) : olevariant;
    var
      Tycoon : TCriminalTycoon;
    begin
      try
        fSimulationSection.Enter;
        try
          Tycoon := TCriminalTycoon(fCriminalTycoons.Items[TycoonName]);
          if Tycoon <> nil
            then
              begin
                if not Tycoon.Logged
                  then
                    begin
                      Tycoon.Logged := true;

                      if (integer(Tycoon) < ERR_MAXERROR)
                        then
                          begin
                            Tycoon.Free;
                            result := ERR_UNKNOWN;
                          end;

                      if Tycoon.ClientView <> nil
                        then Tycoon.ClientView.Free;

                      Tycoon.ClientView := CreateClientView( Tycoon );

                      result := integer(Tycoon);
                    end
                  else result := ERR_TYCOONALREADYLOGGED;
              end
            else result := ERR_NOSUCHTYCOON;
        finally
          fSimulationSection.Leave;
        end;
      except
        result := ERR_UNKNOWN;
      end
    end;

  procedure TIBSystem.RDOLogCriminalOff( TycoonName : widestring );
    var
      Tycoon : TCriminalTycoon;
    begin
      try
        fSimulationSection.Enter;
        try
          Tycoon := TCriminalTycoon(fCriminalTycoons.Items[TycoonName]);
          if Tycoon <> nil
            then
              begin
                if Tycoon.Logged
                  then
                    begin
                      Tycoon.ClientView.Free;
                      Tycoon.ClientView := nil;
                      Tycoon.Logged     := false;
                    end;
              end;
        finally
          fSimulationSection.Leave;
        end;
      except
      end
    end;

  function TIBSystem.RDOCreateCriminalId( TycoonName : widestring; CriminalId : widestring ) : olevariant;
    var
      Tycoon : TCriminalTycoon;
    begin
      try
        fSimulationSection.Enter;
        try
          Tycoon := TCriminalTycoon(fCriminalTycoons.Items[TycoonName]);
          if Tycoon = nil
            then
              begin
                Tycoon := TCriminalTycoon.Create( self, CriminalId, TycoonName );
                fCriminalTycoons.Items[TycoonName] := Tycoon;
                result := RDOLogCriminalIn( TycoonName );
              end
            else result := ERR_TYCOONALREADYEXISTS;
        finally
          fSimulationSection.Leave;
        end;
      except
        result := ERR_UNKNOWN;
      end
    end;

  function TIBSystem.RDOGetTycoonInfo( TycoonId : integer ): olevariant;
    var
      Tycoon : TCriminalTycoon;
      Props  : TStringList;
      i      : integer;
    begin
      try
        fSimulationSection.Enter;
        try
          Tycoon := TCriminalTycoon(TycoonId);
          if Tycoon.Logged
            then
              begin
                Props := TStringList.Create;
                try
                  //>> todo: gather all information needed (level, earnings, etc)

                  Props.Values['CriminalName'] := Tycoon.CriminalName;
                  //Teams
                  for i := 0 to pred(Tycoon.Teams.Count) do
                    Props.Values['Team' + IntToStr(i)] := TTeam(Tycoon.Teams[i]).Name;

                  result := Props.Text;
                finally
                  Props.Free;
                end;
              end
            else result := '';
        finally
          fSimulationSection.Leave;
        end;
      except
        result := '';
      end
    end;

  function TIBSystem.RDOGetTeamInfo( TycoonId : integer; TeamName : widestring ): olevariant;
    var
      Tycoon : TCriminalTycoon;
      Team   : TTeam;
      Props  : TStringList;
      i      : integer;
    begin
      try
        fSimulationSection.Enter;
        try
          Tycoon := TCriminalTycoon(TycoonId);
          if Tycoon.Logged
            then
              begin
                Props := TStringList.Create;
                try
                  Team := Tycoon.getTeamByName( TeamName );
                  if Team <> nil
                    then
                      begin
                        //>> send other info like status

                        // Criminals info
                        for i := 0 to pred(Team.Criminals.Count) do
                          SerializeCriminal( '', TCriminal(Team.Criminals[i]), Props, i );

                        result := Props.Text;
                      end
                    else result := '';
                finally
                  Props.Free;
                end;
              end
            else result := '';
        finally
          fSimulationSection.Leave;
        end;
      except
        result := '';
      end
    end;

  function TIBSystem.RDOCreateTeam( TycoonId : integer; TeamName : widestring ) : olevariant;
    var
      Tycoon  : TCriminalTycoon;
      NewTeam : TTeam;
    begin
      try
        fSimulationSection.Enter;
        try
          Tycoon := TCriminalTycoon(TycoonId);
          if Tycoon.Logged
            then
              begin
                if Tycoon.getTeamByName(TeamName) = nil
                  then
                    begin
                      NewTeam := TTeam.Create( TeamName, Tycoon );
                      Tycoon.Teams.Insert( NewTeam );

                      result  := ERR_SUCCEDEED;
                    end
                  else result := ERR_TEAMALREADYEXISTS;
              end
            else result := ERR_TYCOONNOTLOGGED;
        finally
          fSimulationSection.Leave;
        end;
      except
        result := ERR_UNKNOWN;
      end
    end;

  function TIBSystem.RDOSetTeamHeadQuarter( TycoonId : integer; TeamName : widestring; x, y : integer; Location : WideString ) : olevariant;
    var
      Tycoon  : TCriminalTycoon;
      Team    : TTeam;
    begin
      try
        fSimulationSection.Enter;
        try
          Tycoon := TCriminalTycoon(TycoonId);
          if Tycoon.Logged
            then
              begin
                Team := Tycoon.getTeamByName(TeamName);
                if Team <> nil
                  then result  := Team.setHeadquarter( x, y, Location )
                  else result := ERR_NOSUCHTEAM;
              end
            else result := ERR_TYCOONNOTLOGGED;
        finally
          fSimulationSection.Leave;
        end;
      except
        result := ERR_UNKNOWN;
      end
    end;

  function TIBSystem.RDOHireCriminal( TycoonId : integer; CriminalId : integer; TeamName : widestring ): olevariant;
    var
      Tycoon   : TCriminalTycoon;
      Criminal : TCriminal;
      Team     : TTeam;
    begin
      try
        fSimulationSection.Enter;
        try
          Tycoon := TCriminalTycoon(TycoonId);
          if Tycoon.Logged
            then
              begin
                Team := Tycoon.getTeamByName( TeamName );
                if Team <> nil
                  then
                    begin
                      if Team.Location <> nil
                        then
                          begin
                            Criminal := fCriminalMarket.HireCriminal( Team.Location.Name, CriminalId );
                            if Criminal <> nil
                              then
                                begin
                                  if Criminal.setStatus( CRIMSTATUS_READY, Team ) = ERR_SUCCEDEED
                                    then
                                      begin
                                        result := Team.AddCriminal( Criminal );
                                      end
                                    else result := ERR_UNKNOWN;
                                end
                              else result := ERR_CRIMINALHIRED;
                          end
                        else result := ERR_TEAMNOTLOCATED;
                    end
                  else result := ERR_NOSUCHTEAM;
              end
            else result := ERR_TYCOONNOTLOGGED;
        finally
          fSimulationSection.Leave;
        end;
      except
        result := ERR_UNKNOWN;
      end
    end;

  function TIBSystem.RDOFireCriminal( TycoonId : integer; TeamName : widestring; CriminalId : integer ): olevariant;
    var
      Tycoon   : TCriminalTycoon;
      Criminal : TCriminal;
      Team     : TTeam;
    begin
      try
        fSimulationSection.Enter;
        try
          Tycoon := TCriminalTycoon(TycoonId);
          if Tycoon.Logged
            then
              begin
                Team := Tycoon.getTeamByName( TeamName );
                if Team <> nil
                  then
                    begin
                      if Team.Location <> nil
                        then
                          begin
                            Criminal := Team.getCriminalById( CriminalId );
                            if Criminal <> nil
                              then
                                begin
                                  fCriminalMarket.FireCriminal( Team.Location.Name, Criminal );
                                  result := ERR_SUCCEDEED;
                                end
                              else result := ERR_NOSUCHCRIMINAL;
                          end
                        else result := ERR_TEAMNOTLOCATED;
                    end
                  else result := ERR_NOSUCHTEAM;
              end
            else result := ERR_TYCOONNOTLOGGED;
        finally
          fSimulationSection.Leave;
        end;
      except
        result := ERR_UNKNOWN;
      end
    end;

  function TIBSystem.RDOGetCriminalList( TycoonId : integer; TeamName : widestring ): olevariant;
    var
      Tycoon          : TCriminalTycoon;
      Team            : TTeam;
      ExpCriminals    : TCollection;
      RookieCriminals : TCollection;
      i               : integer;
      Criminals       : TStringList;
    begin
      try
        fSimulationSection.Enter;
        try
          Tycoon := TCriminalTycoon(TycoonId);
          if Tycoon.Logged
            then
              begin
                Team := Tycoon.getTeamByName( TeamName );
                if Team <> nil
                  then
                    begin
                      if Team.Location <> nil
                        then
                          begin
                            ExpCriminals    := TCollection.Create( 10, rkUse );
                            RookieCriminals := TCollection.Create( 10, rkUse );
                            Criminals       := TStringList.Create;
                            try
                              fCriminalMarket.getCriminalList( Team.Location.Name, ExpCriminals, RookieCriminals );

                              for i := 0 to pred(ExpCriminals.Count) do
                                SerializeCriminal( EXPERT_PREFIX, TCriminal(ExpCriminals[i]), Criminals, i );

                              for i := 0 to pred(RookieCriminals.Count) do
                                SerializeCriminal( ROOKIE_PREFIX, TCriminal(RookieCriminals[i]), Criminals, i );

                              result := Criminals.Text;
                            finally
                              ExpCriminals.Free;
                              RookieCriminals.Free;
                              Criminals.Free;
                            end;
                          end
                        else result := '';
                    end
                  else result := '';
              end
            else result := '';
        finally
          fSimulationSection.Leave;
        end;
      except
        result := '';
      end
    end;

  function TIBSystem.RDOStartMission( TycoonId : integer; TeamName : widestring; MissionId : widestring; MissionInfo : widestring ): olevariant;
    var
      Tycoon      : TCriminalTycoon;
      Team        : TTeam;
      MetaMission : TMetaMission;
      Mission     : TMission;
      res         : TErrorCode;
    begin
      try
        fSimulationSection.Enter;
        try
          Tycoon := TCriminalTycoon(TycoonId);
          if Tycoon.Logged
            then
              begin
                Team := Tycoon.getTeamByName( TeamName );
                if Team <> nil
                  then
                    begin
                      if Team.State = TEAMSTATE_READY
                        then
                          begin
                            MetaMission := TMetaMission(theMetaClassStorage.ClassById[tidMetaMissionFamily, MissionId]);
                            if MetaMission <> nil
                              then
                                begin
                                  res := MetaMission.Instantiate( MissionInfo, Team, Mission );
                                  if res = ERR_SUCCEDEED
                                    then
                                      begin
                                        result := Team.AssignMission( Mission, MissionInfo );
                                        if result <> ERR_SUCCEDEED
                                          then Mission.Free
                                          else
                                            begin
                                              theIBSystem.AddSimulationObject( Team, FREQ_HOUR );
                                            end;
                                      end
                                    else
                                      begin
                                        Mission.Free;
                                        result := res;
                                      end;
                                end
                              else result := ERR_NOSUCHMISSION;
                          end
                        else result := ERR_TEAMNOTREADY;
                    end
                  else result := ERR_NOSUCHTEAM;
              end
            else result := ERR_TYCOONNOTLOGGED;
        finally
          fSimulationSection.Leave;
        end;
      except
        result := ERR_UNKNOWN;
      end
    end;

  function TIBSystem.RDOAbortMission( TycoonId : integer; TeamName : widestring ): olevariant;
    var
      Tycoon : TCriminalTycoon;
      Team   : TTeam;
    begin
      try
        fSimulationSection.Enter;
        try
          Tycoon := TCriminalTycoon(TycoonId);
          if Tycoon.Logged
            then
              begin
                Team := Tycoon.getTeamByName( TeamName );
                if Team <> nil
                  then
                    begin
                      if Team.State = TEAMSTATE_INMISSION
                        then
                          begin
                            Team.Mission.Abort;
                            result := ERR_SUCCEDEED;
                          end
                        else result := ERR_TEAMNOTREADY;
                    end
                  else result := ERR_NOSUCHTEAM;
              end
            else result := ERR_TYCOONNOTLOGGED;
        finally
          fSimulationSection.Leave;
        end;
      except
        result := ERR_UNKNOWN;
      end
    end;

  procedure TIBSystem.Act;
    begin
      fSimulationSection.Enter;
      try
        Simulate;
      finally
        fSimulationSection.Leave;
      end;
    end;

  procedure TIBSystem.HandleEvent( event : integer; var data );
    begin
    end;

  procedure TIBSystem.getLocationInformation( Location : string; var Info : TLocationInformation );
    begin
      //>> todo: use the modelserver
      Info.Population     := 1000 + random( 700000 );
      Info.Education      := 10 + random( 90 );
      Info.PoliceCoverage := 10 + random( 90 );
    end;

  procedure TIBSystem.addLocation( Location : string );
    begin
      fCriminalMarket.AddLocation( Location );
    end;

  procedure TIBSystem.AddSimulationObject( aSimObject : TSimulationObject; Freq : integer );
    begin
      try
        fSimObjects.Lock;
        try
          fSimObjects.Insert( TSimObjectInfo.Create( aSimObject, Freq ) );
        finally
          fSimObjects.Unlock;
        end;
      except
      end;
    end;

  procedure TIBSystem.DeleteSimulationObject( aSimObject : TSimulationObject; Freq : integer );
    var
      i     : integer;
      found : boolean;
    begin
      try
        fSimObjects.Lock;
        try
          i     := 0;
          found := false;

          while (i < fSimObjects.Count) and not found do
            begin
              if (TSimObjectInfo(fSimObjects[i]).Frequency = Freq) and (TSimObjectInfo(fSimObjects[i]).SimObject = aSimObject)
                then
                  begin
                    fSimObjects.AtDelete( i );
                    found := true;
                  end;
              inc( i );
            end;
        finally
          fSimObjects.Unlock;
        end;
      except
      end;
    end;

  procedure TIBSystem.AddCriminalHistoryItem( Criminal : TCriminal; Id : string; Date : string; Team : TTeam; Mission : TMission; Training : TTraining );
    var
      MetaHistoryItem : TMetaHistoryItem;
      HistoryItem     : THistoryItem;
    begin
      MetaHistoryItem := TMetaHistoryItem(theMetaClassStorage.ClassById[tidMetaHistoryFamily, Id]);
      if MetaHistoryItem <> nil
        then
          begin
            HistoryItem  := MetaHistoryItem.Instantiate;
            HistoryItem.addRequierement( HISTREQ_DATE, Date );
            if Mission <> nil
              then HistoryItem.addRequierement( HISTREQ_MISSION, Mission.MetaMission.Name );

            if Training <> nil
              then HistoryItem.addRequierement( HISTREQ_TRAINING, Training.MetaTraining.Name );

            if Team <> nil
              then HistoryItem.addRequierement( HISTREQ_TTEAM, Team.Name );

            Criminal.addHistoryItem( HistoryItem );
          end;
    end;

  procedure TIBSystem.AddTycoonHistoryItem( Tycoon : TCriminalTycoon; Id : string; Date : string; Team : TTeam; aCriminal : TCriminal );
    var
      MetaHistoryItem : TMetaHistoryItem;
      HistoryItem     : THistoryItem;
    begin
      MetaHistoryItem := TMetaHistoryItem(theMetaClassStorage.ClassById[tidMetaHistoryFamily, Id]);
      if MetaHistoryItem <> nil
        then
          begin
            HistoryItem  := MetaHistoryItem.Instantiate;
            HistoryItem.addRequierement( HISTREQ_DATE, Date );

            if Team <> nil
              then HistoryItem.addRequierement( HISTREQ_TTEAM, Team.Name );

            if aCriminal <> nil
              then HistoryItem.addRequierement( HISTREQ_CRIMINAL, aCriminal.Name );

            Tycoon.addHistoryItem( HistoryItem );
          end;
    end;

  procedure TIBSystem.AddTeamHistoryItem( Team : TTeam; Id : string; Date : string; Tycoon : TCriminalTycoon; Mission : TMission; aCriminal : TCriminal );
    var
      MetaHistoryItem : TMetaHistoryItem;
      HistoryItem     : THistoryItem;
    begin
      MetaHistoryItem := TMetaHistoryItem(theMetaClassStorage.ClassById[tidMetaHistoryFamily, Id]);
      if MetaHistoryItem <> nil
        then
          begin
            HistoryItem  := MetaHistoryItem.Instantiate;
            HistoryItem.addRequierement( HISTREQ_DATE, Date );

            if Tycoon <> nil
              then HistoryItem.addRequierement( HISTREQ_TYCOON, Tycoon.CriminalName );

            if aCriminal <> nil
              then HistoryItem.addRequierement( HISTREQ_CRIMINAL, aCriminal.Name );

            if Mission <> nil
              then HistoryItem.addRequierement( HISTREQ_CRIMINAL, Mission.MetaMission.Name );

            Team.addHistoryItem( HistoryItem );
          end;
    end;

  procedure TIBSystem.Simulate;
    var
      i : integer;
    begin
      try
        fSimObjects.Lock;
        try
          for i := pred(fSimObjects.Count) downto 0 do
            TSimObjectInfo(fSimObjects[i]).Simulate;
        finally
          fSimObjects.Unlock;
        end;
      except
      end;
    end;

  function TIBSystem.CreateClientView( Tycoon : TCriminalTycoon ) : TIBClientView;
    begin
      result := TTestClientView.Create( Tycoon );
    end;

  function TIBSystem.getLocation( name : string ) : TTownInfo;
    var
      found : boolean;
      i     : integer;
    begin
      found := false;
      i     := 0;

      while (i < fLocations.Count) and not found do
        begin
          found := CompareText( TTownInfo(fLocations[i]).Name, name ) = 0;
          inc( i );
        end;

      if found
        then result := TTownInfo(fLocations[pred(i)])
        else result := nil;
    end;

  procedure TIBSystem.SerializeCriminal( Prefix : string; Criminal : TCriminal; where : TStringList; idx : integer );
    var
      i : integer;
    begin
      with Criminal, where do
        begin
          Values[Prefix + 'Name'    + IntToStr(idx)] := Name;
          Values[Prefix + 'Sex'     + IntToStr(idx)] := IntToStr(integer(Sex));
          Values[Prefix + 'Picture' + IntToStr(idx)] := PictureId;
          Values[Prefix + 'Status'  + IntToStr(idx)] := IntToStr(Status);
          Values[Prefix + 'Id'      + IntToStr(idx)] := IntToStr(Id);
          for i := 0 to pred(SKILL_COUNT) do
            Values[Prefix + 'Skill' + IntToStr(i) + '_' + IntToStr(idx)] := FloatToStr(Skills.Skills[i]);
        end;
    end;

  procedure TIBSystem.LoadFromBackup(Reader : IBackupReader);
    begin
      inherited;
      Reader.ReadObject('Criminals', fCriminalTycoons, nil);
      Reader.ReadObject('Agencies', fAgencies, nil);
      Reader.ReadObject('SimObjs', fSimObjects, nil);
      Reader.ReadObject('CriminalMarket', fCriminalMarket, nil);
      fClientViews := TLockableCollection.Create(0, rkBelonguer);
      InitMissions;
    end;

  procedure TIBSystem.StoreToBackup(Writer : IBackupWriter);
    begin
      inherited;
      Writer.WriteObject('Criminals', fCriminalTycoons);
      Writer.WriteObject('Agencies', fAgencies);
      Writer.WriteObject('SimObjs', fSimObjects);
      Writer.WriteObject('CriminalMarket', fCriminalMarket);
    end;


  // TCriminalTycoon

  constructor TCriminalTycoon.Create( aIBSystem : TIBSystem; aCriminalName : string; aTycoonName : string );
    begin
      inherited Create;
      fCriminalName := aCriminalName;
      fTycoonName   := aTycoonName;
      fTeams        := TLockableCollection.Create( 5, rkBelonguer );
      fHistory      := TCollection.Create( 10, rkBelonguer );
    end;

  destructor TCriminalTycoon.Destroy;
    begin
      fTeams.Free;
      fHistory.Free;
      inherited;
    end;

  procedure TCriminalTycoon.addHistoryItem( Item : THistoryItem );
    begin
      fHistory.Insert( Item );
    end;

  function TCriminalTycoon.getTeamByName( aName : string ) : TTeam;
    var
      i     : integer;
      found : boolean;
    begin
      i     := 0;
      found := false;
      while (i < fTeams.Count) and not found do
        begin
          found := CompareText( TTeam(fTeams[i]).Name, aName ) = 0;
          if not found
            then inc( i );
        end;
      if found
        then result := TTeam(fTeams[i])
        else result := nil;
    end;

  procedure TCriminalTycoon.Act( Frequency : integer );
    begin
    end;

  procedure TCriminalTycoon.LoadFromBackup(Reader : IBackupReader);
    begin
      inherited;
      fCriminalName := Reader.ReadString('Name', '');
      fTycoonName   := Reader.ReadString('Tycoon', '');
      Reader.ReadObject('Teams', fTeams, nil);
      Reader.ReadObject('History', fHistory, nil);
    end;

  procedure TCriminalTycoon.StoreToBackup(Writer : IBackupWriter);
    begin
      inherited;
      Writer.WriteString('Name', fCriminalName);
      Writer.WriteString('Tycoon', fTycoonName);
      Writer.WriteObject('Teams', fTeams);
      Writer.WriteLooseObject('History', fHistory);
    end;


  // TMission

  constructor TMission.Create( aOwner : TTeam; aMetaMission : TMetaMission );
    begin
      inherited Create;
      fOwner       := aOwner;
      fMetaMission := aMetaMission;
    end;

  procedure TMission.StartMission;
    begin
    end;

  procedure TMission.ProceedWithMission;
    begin
    end;

  procedure TMission.EndMission;
    var
      i        : integer;
      Criminal : TCriminal;
    begin
      for i := 0 to pred(fOwner.Criminals.Count) do
        begin
          Criminal := TCriminal(fOwner.Criminals[i]);
          case Criminal.Status of
            CRIMSTATUS_INMISSION :
              Criminal.setStatus( CRIMSTATUS_READY, fOwner );
            //>> check when captured or dead and start trials etc... also add experience, charges, and history items
          end;
        end;

      //>> todo: change criminal status, apply charges, etc
      theIBSystem.DeleteSimulationObject( self, FREQ_HOUR );
    end;

  procedure TMission.Abort;
    begin
      fAbort := true;
    end;

  procedure TMission.Escaped;
    begin
      fEscaped := true;
    end;

  procedure TMission.Act( Frequency : integer );
    begin
    end;

  procedure TMission.LoadFromBackup(Reader : IBackupReader);
    var
      aux : string;
    begin
      inherited;
      aux := Reader.ReadString('MetaMission', '');
      fMetaMission := TMetaMission(TheClassStorage.ClassById[tidMetaMissionFamily, aux]);
      Reader.ReadObject('Owner', fOwner, nil);
      fAbort := Reader.ReadBoolean('Aborted', false);
      fEscaped := Reader.ReadBoolean('Escaped', false);
    end;

  procedure TMission.StoreToBackup(Writer : IBackupWriter);
    begin
      inherited;
      Writer.WriteString('MetaMission', fMetaMission.Id);
      Writer.WriteObject('Owner', fOwner);
      Writer.WriteBoolean('Aborted', fAbort);
      Writer.WriteBoolean('Escaped', fEscaped);
    end;


  // TTeam

  constructor TTeam.Create( aName : string; aOwner : TCriminalTycoon );
    begin
      inherited Create;
      fName      := aName;
      fOwner     := aOwner;
      fCriminals := TLockableCollection.Create( 4, rkBelonguer );
      fState     := TEAMSTATE_UNLOCATED;
      fMission   := nil;
      fHistory   := TCollection.Create( 10, rkBelonguer );
    end;

  destructor TTeam.Destroy;
    begin
      fCriminals.Free;
      fHistory.Free;
      inherited;
    end;

  function TTeam.AddCriminal( aCriminal : TCriminal ) : TErrorCode;
    begin
      try
        if fCriminals.Count < MAXTEAM_MEMBERS
          then
            begin
              fCriminals.Insert( aCriminal );
              result := ERR_SUCCEDEED;
              //>> notify
            end
          else result := ERR_TOOMANYMEMBERS;
      except
        result := ERR_UNKNOWN;
      end;
    end;

  function TTeam.DeleteCriminal( aCriminal : TCriminal ) : TErrorCode;
    begin
      try
        fCriminals.Extract( aCriminal );
        result := ERR_SUCCEDEED;
      except
        result := ERR_UNKNOWN;
      end;
    end;

  function TTeam.FindCriminal( aName : string ) : TCriminal;
    var
      i     : integer;
      found : boolean;
    begin
      try
        fCriminals.Lock;
        try
          i     := 0;
          found := false;
          while (i < fCriminals.Count) and not found do
            begin
              found := CompareText( TCriminal(fCriminals[i]).Name, aName ) = 0;
              if not found
                then inc( i );
            end;
          if found
            then result := TCriminal(fCriminals[i])
            else result := nil;
        finally
          fCriminals.Unlock;
        end;
      except
        result := nil;
      end;
    end;

  function TTeam.getCriminalById( CriminalId : integer ) : TCriminal;
    var
      i     : integer;
      found : boolean;
    begin
      try
        fCriminals.Lock;
        try
          i     := 0;
          found := false;
          while (i < fCriminals.Count) and not found do
            begin
              found := TCriminal(fCriminals[i]).Id = CriminalId;
              if not found
                then inc( i );
            end;
          if found
            then result := TCriminal(fCriminals[i])
            else result := nil;
        finally
          fCriminals.Unlock;
        end;
      except
        result := nil;
      end;
    end;

  function TTeam.AssignMission( aMission : TMission; MissionInfo : string ) : TErrorCode;
    var
      Props : TStringList;

    function CheckAndAssignRoles : boolean;
      var
        i        : integer;
        CrimId   : integer;
        Role     : integer;
        Criminal : TCriminal;
        Error    : boolean;
      begin
        try
          i     := 0;
          Error := false;
          while (Props.IndexOfName('Role' + IntToStr(i)) <> -1) or Error do
            begin
              CrimId := StrToInt( Props.Values['Criminal' + IntToStr(i)]);
              Role   := StrToInt( Props.Values['Role' + IntToStr(i)] );

              Criminal := getCriminalById( CrimId );
              if (Criminal <> nil)
                then Error := Criminal.setStatus( CRIMSTATUS_INMISSION, Role ) <> ERR_SUCCEDEED
                else Error := true;

              inc( i );
            end;

          result := not Error;
        except
          result := false;
        end;
      end;
    begin
      try
        if fMission = nil
          then
            begin
              Props := TStringList.Create;
              try
                Props.Text := MissionInfo;
                if CheckAndAssignRoles
                  then
                    begin
                      fMission   := aMission;
                      fMission.StartMission;
                      fState     := TEAMSTATE_INMISSION;
                      result     := ERR_SUCCEDEED;
                    end
                  else result := ERR_UNABLETOASSIGNROLES;
              finally
                Props.Free;
              end;
            end
          else result := ERR_TEAMINMISSION;
      except
        result := ERR_UNKNOWN;
      end;
    end;

  function TTeam.getCriminalForTask( role, skill : integer; assdRoleCount : integer; const assdRoles : TRoles ) : TCriminal;
    function FindAdequateCriminal : TCriminal;
      var
        i     : integer;
        found : boolean;
        Crim  : TCriminal;
      begin
        i     := 0;
        found := false;
        Crim  := nil;

        while (i < fCriminals.Count) and not found do
          begin
            Crim  := TCriminal(fCriminals[i]);
            found := (Crim.Role = role) and (Crim.Status = CRIMSTATUS_INMISSION);
            if not found
              then inc( i );
          end;

        if found
          then result := Crim
          else result := nil;
      end;

    function FindBestCriminalAvailable : TCriminal;
      function RoleNotAssigned( Criminal : TCriminal ): boolean;
        var
          i     : integer;
          found : boolean;
        begin
          i     := 0;
          found := false;
          while (i < assdRoleCount) and not found do
            begin
              found := assdRoles[i].role = Criminal.Role;
              inc( i );
            end;
          result := not found;
        end;

      var
        i         : integer;
        Crim      : TCriminal;
        Crims     : array[0..MAXTEAM_MEMBERS] of TCriminal;
        CrimCount : integer;
        maxskill  : single;
      begin
        // build a list of idle criminals
        CrimCount := 0;
        for i := 0 to pred(fCriminals.Count) do
          begin
            Crim := TCriminal(fCriminals[i]);
            if (Crim.Status = CRIMSTATUS_INMISSION) and RoleNotAssigned( Crim )
              then
                begin
                  Crims[CrimCount] := Crim;
                  inc( CrimCount );
                end;
          end;

        // Find the one with better skills

        result   := nil;
        maxskill := -100000;

        for i := 0 to pred(CrimCount) do
          if Crims[i].Skills.Skills[skill] > maxskill
            then
              begin
                maxskill := Crims[i].Skills.Skills[skill];
                result   := Crims[i];
              end;
      end;

    begin
      try
        result := FindAdequateCriminal;
        if result = nil
          then result := FindBestCriminalAvailable;
      except
        result := nil;
      end;
    end;

  function TTeam.setHeadquarter( x, y : integer; aLocation : string ) : TErrorCode;
    procedure AssigHQ;
      begin
        fHQX      := x;
        fHQY      := y;
        fLocation := theIBSystem.getLocation( aLocation );
      end;

    begin
      result := ERR_SUCCEDEED;
      case fState of
        TEAMSTATE_UNLOCATED :
          begin
            fState := TEAMSTATE_READY;
            AssigHQ;
            //>> notify event
          end;
        TEAMSTATE_READY :
          begin
            Moved;
            AssigHQ;
          end;
        TEAMSTATE_INMISSION : result := ERR_TEAMINMISSION;
      end;
    end;

  procedure TTeam.addHistoryItem( HistoryItem : THistoryItem );
    begin
      fHistory.Insert( HistoryItem );
    end;

  procedure TTeam.Act( Frequency : integer );
    begin
      case fState of
        TEAMSTATE_INMISSION :
          fMission.ProceedWithMission;
      end;
    end;

  procedure TTeam.Moved;
    begin
      //>> todo: simulation of Acc risk and notification
    end;

  procedure TTeam.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      fName := Reader.ReadString('Name', '');
      Reader.ReadObject('Owner', fOwner, nil);
      Reader.ReadObject('Criminals', fCriminals, nil);
      fState := Reader.ReadInteger('State', TEAMSTATE_UNLOCATED);
      Reader.ReadObject('Mission', fMission, nil);
      fHQX := Reader.ReadInteger('HQX', 0);
      fHQY := Reader.ReadInteger('HQY', 0);

      Reader.ReadObject('Location', fLocation, nil);
      Reader.ReadObject('History', fHistory, nil);
    end;

  procedure TTeam.StoreToBackup ( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteString('Name', fName);
      Writer.WriteObjectRef('Owner', fOwner);
      Writer.WriteLooseObject('Criminals', fCriminals);
      Writer.WriteInteger('State', fState);
      Writer.WriteObject('Mission', fMission);
      Writer.WriteInteger('HQX', fHQX);
      Writer.WriteInteger('HQY', fHQY);

      Writer.WriteObjectRef('Location', fLocation);
      Writer.WriteLooseObject('History', fHistory);
    end;


  // TCriminal

  constructor TCriminal.Create( aName : string; anAge : integer; aSex : TCriminalSex; aLocation : string; aPictureId : string; anId : integer );
    begin
      inherited Create;
      fName      := aName;
      fId        := anId;
      fAge       := anAge;
      fSex       := aSex;
      fSkills    := TSkillCollection.Create;
      fPictureId := aPictureId;
      fStatus    := CRIMSTATUS_ONTHEMARKET;
      fHistory   := TCollection.Create( 10, rkBelonguer );
    end;

  destructor TCriminal.Destroy;
    begin
      fSkills.Free;
      fHistory.Free;
      inherited;
    end;

  procedure TCriminal.addHistoryItem( Item : THistoryItem );
    begin
      fHistory.Insert( Item );
    end;

  function TCriminal.setStatus( anewStatus : integer; var info ) : TErrorCode;
    var
      aRole : integer absolute info;
      aTeam : TTeam absolute info;
    begin
      result  := ERR_SUCCEDEED;
      case fStatus of
        CRIMSTATUS_ONTHEMARKET :
          case anewStatus of
            CRIMSTATUS_READY :
              begin
                fStatus := anewStatus;
                fTeam   := aTeam;
              end;
            else result := ERR_INVALIDSTATUSCHANGE;
          end;

        CRIMSTATUS_READY :
          case anewStatus of
            CRIMSTATUS_INMISSION :
              begin
                fStatus := anewStatus;
                fRole   := aRole;
              end;

            CRIMSTATUS_ONTHEMARKET :
              begin
                fStatus := anewStatus;
                fTeam   := nil;
              end;
            else result := ERR_INVALIDSTATUSCHANGE;
          end;

        CRIMSTATUS_INMISSION :
          case anewStatus of
            CRIMSTATUS_READY :
              begin
                fStatus := anewStatus;
              end;

            CRIMSTATUS_ONTHEMARKET :
              begin
                fStatus := anewStatus;
                fTeam   := nil;
              end;
              CRIMSTATUS_CAPTURED :
                begin
                  fStatus := anewStatus;
                end;

              CRIMSTATUS_DEAD :
                begin
                  fStatus := anewStatus;
                end;
            else result := ERR_INVALIDSTATUSCHANGE;
          end;
      end;

    end;

  function TCriminal.CheckStability( Skill : integer; PsiFactor : single ) : single;
    const
      STAB_BOUNDS : array[0..pred(SKILL_COUNT)] of single =
                      (
                        0.5,  //SKILL_LEADERSHIP
                        0.2,  //SKILL_DRIVING
                        0.05, //SKILL_BRAWLING
                        0.4,  //SKILL_FIREARMS
                        0.7,  //SKILL_STALKING
                        0.5,  //SKILL_COMPUTER
                        0.2,  //SKILL_DEMOLITION
                        0.5,  //SKILL_STEALTH
                        0.8,  //SKILL_MEDICINE
                        0.5   //SKILL_FORGERY
                      );

    const
      STABLEVEL_FRAGILE = 0;
      STABLEVEL_LUNATIC = 1;
      STABLEVEL_SOLID   = 2;

      FRAGILE_BOUND     = 0.5;
      LUNATIC_BOUND     = 0.8;
      SOLID_BOUND       = 1.0;

    function StabToLevel : integer;
      begin
        if fStability < FRAGILE_BOUND
          then result := STABLEVEL_FRAGILE
          else
            if fStability < LUNATIC_BOUND
              then result := STABLEVEL_LUNATIC
              else result := STABLEVEL_SOLID;
      end;

    function CalcFragileStability( aPsiFactor : single ) : single;
      begin
        result := (((fStability/FRAGILE_BOUND) - 7*PsiFactor - 1)*STAB_BOUNDS[Skill])/8;
      end;

    function CalcLunaticStability( aPsiFactor : single ) : single;
      var
        NoiseAmplitude : single;
        n              : single;
      begin
        n              := STAB_BOUNDS[Skill];
        NoiseAmplitude := ((n/16 - n)*(fStability - FRAGILE_BOUND))/(LUNATIC_BOUND - FRAGILE_BOUND) + n;

        // NoiseAmplitude is the maximum possible noise for the PsiFactor

        NoiseAmplitude := NoiseAmplitude*PsiFactor; //apply PsiFactor percent (PsiFactor = 0 means no noise)

        result := NoiseAmplitude*(random*2 - 1);
      end;

    function CalcSolidStability( aPsiFactor : single ) : single;
      begin
        result := ln(fStability)/ln(10);
      end;

    begin
      case StabToLevel of
        STABLEVEL_FRAGILE : result := CalcFragileStability( PsiFactor );
        STABLEVEL_LUNATIC : result := CalcLunaticStability( PsiFactor );
        STABLEVEL_SOLID   : result := CalcSolidStability( PsiFactor );
        else result := 0;
      end;

      if result > STAB_BOUNDS[Skill]
        then result := STAB_BOUNDS[Skill]
        else
          if result < -STAB_BOUNDS[Skill]
            then result := -STAB_BOUNDS[Skill];
    end;

  procedure TCriminal.Act( Frequency : integer );
    begin
    end;

  function TCriminal.getLocation : TTownInfo;
    begin
      if fTeam <> nil
        then result := fTeam.Location
        else result := nil;
    end;

  procedure TCriminal.LoadFromBackup(Reader : IBackupReader);
    begin
      inherited;
      fName := Reader.ReadString('Name', '');
      fAge  := Reader.ReadInteger('Age', 25);
      fSex  := TCriminalSex(Reader.ReadByte('Sex', byte(csMale)));
      Reader.ReadObject('Skills', fSkills, nil);
      fPictureId := Reader.ReadString('PicId', '');
      Reader.ReadObject('Team', fTeam, nil);
      fId := Reader.ReadInteger('Id', 0);
      fRole := Reader.ReadInteger('Role', 0);
      // status
      fStatus := Reader.ReadInteger('Status', 0);
      fStability := Reader.ReadSingle('Stability', 0);
      // History
      Reader.ReadObject('History', fHistory, nil);
    end;

  procedure TCriminal.StoreToBackup(Writer : IBackupWriter);
    begin
      inherited;
      Writer.WriteString('Name', fName);
      Writer.WriteInteger('Age', fAge);
      Writer.WriteByte('Sex', byte(fSex));
      Writer.WriteLooseObject('Skills', fSkills);
      Writer.WriteString('PicId', fPictureId);
      Writer.WriteObjectRef('Team', fTeam);
      Writer.WriteInteger('Id', fId);
      Writer.WriteInteger('Role', fRole);
      // status
      Writer.WriteInteger('Status', fStatus);
      Writer.WriteSingle('Stability', fStability);
      // History
      Writer.WriteLooseObject('History', fHistory);
    end;


  // TSkillCollection

  constructor TSkillCollection.Create;
    begin
      inherited;
      fSkillCount := SKILL_COUNT;
    end;

  procedure TSkillCollection.LoadFromBackup(Reader : IBackupReader);
    var
      i : integer;
    begin
      inherited;
      fSkillCount := Reader.ReadInteger('Count', SKILL_COUNT);
      for i := 0 to pred(fSkillCount) do
        fSkills[i] := Reader.ReadSingle('', 0);
    end;

  procedure TSkillCollection.StoreToBackup(Writer : IBackupWriter);
    var
      i : integer;
    begin
      inherited;
      Writer.WriteInteger('Count', fSkillCount);
      for i := 0 to pred(fSkillCount) do
        Writer.WriteSingle(IntToStr(i), fSkills[i]);
    end;


  // TTraining

  constructor TTraining.Create( aMetaTraining : TMetaTraining; aTrainee : TObject );
    begin
      inherited Create;
      fState        := TRAININGSTATE_TRAINING;
      fMetaTraining := aMetaTraining;
      fPercent      := 0;
      fTimeElapsed  := 0;
      fTrainee      := aTrainee;
      fFrequency    := fMetaTraining.Frequency;
    end;

  procedure TTraining.Act( Frequency : integer );
    begin
      case fState of
        TRAININGSTATE_TRAINING :
          begin
            inc( fTimeElapsed );
            fPercent := trunc((fTimeElapsed/fMetaTraining.Duration)*100);

            if fPercent >= 100
              then
                begin
                  fMetaTraining.TrainingComplete( fPercent, fTrainee );
                end;
          end;

        TRAININGSTATE_PAUSED :
          begin
          end;
      end;
    end;

  procedure TTraining.Abort;
    begin
      Finished;
    end;

  procedure TTraining.Pause;
    begin
      fState := TRAININGSTATE_PAUSED;
    end;

  procedure TTraining.Resume;
    begin
      fState := TRAININGSTATE_TRAINING;
    end;

  procedure TTraining.Finished;
    begin
      fMetaTraining.TrainingComplete( fPercent, fTrainee );
      theIBSystem.DeleteSimulationObject( self, fFrequency );
    end;

  procedure TTraining.LoadFromBackup(Reader : IBackupReader);
    var
      aux : string;
    begin
      inherited;
      aux := Reader.ReadString('Meta', '');
      fMetaTraining := TMetaTraining(TheClassStorage.ClassById[tidMetaTrainingFamily, aux]);
      fState        := Reader.ReadInteger('State', 0);
      Reader.ReadObject('Trainee', fTrainee, nil);
      fPercent      := Reader.ReadInteger('Perc', 0);
      fTimeElapsed  := Reader.ReadInteger('TimeElapsed', 0);
      fFrequency    := Reader.ReadInteger('Frequency', 0);
    end;

  procedure TTraining.StoreToBackup(Writer : IBackupWriter);
    begin
      inherited;
      Writer.WriteString('Meta', fMetaTraining.Id);
      Writer.WriteInteger('State', fState);
      Writer.WriteObjectRef('Trainee', fTrainee);
      Writer.WriteInteger('Perc', fPercent);
      Writer.WriteInteger('TimeElapsed', fTimeElapsed);
      Writer.WriteInteger('Frequency', fFrequency);
    end;

end.


