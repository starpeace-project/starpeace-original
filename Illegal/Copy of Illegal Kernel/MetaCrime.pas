unit MetaCrime;

interface

  uses
    ShareMem, ClassStorageInt, classes, Collection, extctrls, Graphics, comobj;

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

    TMission = class;

    // Metaclasses

    CMission       = class of TMission;

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
      end;

  var
    ClassId    : TClassID;
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


end.


