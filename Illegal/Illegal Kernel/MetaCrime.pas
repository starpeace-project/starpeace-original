unit MetaCrime;

interface

  uses
    ShareMem, ClassStorageInt, classes, Collection;

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
                              aMissType     : widestring );
          destructor Destroy; override;
        private
          fName             : widestring;
          fDesc             : widestring;
          fMissionType      : widestring;
        public
          property Name : widestring read fName;
          property Desc : widestring read fDesc;
          property MissionType : widestring read fMissionType;
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

      TMetaCriminal =
      class( TIllegalMetaInstance )
        public
          constructor Create( anId              : TClassId;
                              aName             : widestring;
                              aPhoto            : widestring;
                              aLevel            : integer;
                              anAge             : integer;
                              aSkillSet         : TStringList;
                              aSex              : string );
          destructor Destroy; override;
        private
          fName             : widestring;
          fPhoto            : widestring;
          fLevel            : integer;
          fSkillSet         : TStringList;
          fAge              : integer;
          fSex              : string;
        public
          property Name     : widestring read fName;
          property Photo    : widestring read fPhoto;
          property Level    : integer read fLevel;
          property SkillSet : TStringList read fSkillSet;
          property Age      : integer read fAge;
          property Sex      : string read fSex;
        end;

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

      TMetaCharge =
      class( TIllegalMetaInstance )
        public
          constructor Create( anId             : TClassID;
                              aName            : widestring;
                              aPreTrialTime    : integer;
                              aTrialTime       : integer;
                              aMinLawyersHours : integer;
                              aMinBribe        : integer;
                              aMinJailTime     : integer;
                              aMaxJailTime     : integer;
                              aWarrant         : single );
          destructor Destroy; override;
        private
          fName            : widestring;
          fPreTrialTime    : integer;
          fTrialTime       : integer;
          fMinLawyersHours : integer;
          fMinBribe        : integer;
          fMinJailTime     : integer;
          fMaxJailTime     : integer;
          fWarrant         : single;
        public
          property Name            : widestring read fName;
          property PreTrialTime    : integer read fPreTrialTime;
          property TrialTime       : integer read fTrialTime;
          property MinLawyersHours : integer read fMinLawyersHours;
          property MinBribe        : integer read fMinBribe;
          property MinJailTime     : integer read fMinJailTime;
          property MaxJailTime     : integer read fMaxJailTime;
          property Warrant         : single read fWarrant;
      end;



      //Instances Declarations

      

  var
    ClassId    : TClassID;
    Time       : integer;

  const
    tidRDOHook_IB = 'IB';

implementation

  uses
   ClassStorage;

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
        inherited;
      end;

    procedure TMetaModifier.AddModifier( AttributeModifier : TAttributeModifier );
      begin
        fModifiers.Insert( AttributeModifier );
      end;

    constructor TMetaMission.Create( anId          : TClassID;
                                     aName         : widestring;
                                     aDesc         : widestring;
                                     aMissType     : widestring );
      begin
        inherited Create( anId );
        fName            := aName;
        fDesc            := aDesc;
        fMissionType     := aMissType;
      end;

    destructor TMetaMission.Destroy;
      begin
        inherited
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
        inherited;
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

    constructor TMetaCriminal.Create( anId : TClassId; aName  : widestring; aPhoto : widestring; aLevel : integer; anAge : integer; aSkillSet : TStringList; aSex : string );
      begin
        inherited Create(anID);
        fName := aName;
        fPhoto := aPhoto;
        fLevel := aLevel;
        fSkillSet := TStringList.Create;
        fSkillSet := aSkillSet;
        fAge := anAge;
        fSex := aSex;
      end;

    destructor TMetaCriminal.Destroy;
      begin
        inherited;
      end;

    constructor TMetaCharge.Create( anId : TClassID;aName : widestring; aPreTrialTime : integer; aTrialTime : integer; aMinLawyersHours : integer; aMinBribe : integer; aMinJailTime : integer; aMaxJailTime : integer; aWarrant : single);
      begin
        inherited Create(anID);
        fName            := aName;
        fPreTrialTime    := aPreTrialTime;
        fTrialTime       := aTrialTime;
        fMinLawyersHours := aMinLawyersHours;
        fMinBribe        := aMinBribe;
        fMinJailTime     := aMinJailTime;
        fMaxJailTime     := aMaxJailTime;
        fWarrant         := aWarrant;
      end;

    destructor TMetaCharge.Destroy;
      begin
        inherited;
      end;

end.


