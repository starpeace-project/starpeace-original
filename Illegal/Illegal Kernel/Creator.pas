unit Creator;

interface

uses
  ClassStorageInt, Collection, IllegalKernel, MetaCrime;

  procedure CreateEverything; forward;

var
  IBSystem : olevariant;

implementation

uses
  SysUtils, ClassStorage, WinSockRDOConnection, RDOServer, RDOInterfaces, RDOObjectProxy, Classes;

    function CreateIllSystemProxy : olevariant;
      var
        IBCnx : IRDOConnectionInit;
      begin
        IBCnx        := TWinSockRDOConnection.Create;
        IBCnx.Server := 'denise';
        IBCnx.Port   := 7001;
        if IBCnx.Connect( 10000 )
          then
            begin
              result := TRDOObjectProxy.Create as IDispatch;
              result.SetConnection( IBCnx );
              result.BindTo( 'IB' );
              result.WaitForAnswer := true;
              result.TimeOut := 1200000;
            end
          else raise Exception.Create( 'Could not connect to Model Server!' );
      end;

    procedure CreateEverything;
      var
        AttrLean    : TMetaAttribute;
        ModLean     : TMetaModifier;
        MisLean     : TMetaMission;
        RoleLean    : TMetaRole;
        AttrModLean : TAttributeModifier;
        HistItemLean: TMetaHistoryItem;
        CrimLean     : TMetaCriminal;
        CrimSkills   : TStringList;
        //ChargeLean  : TCriminalCharge;
      begin

        InitTheClassStorage;

        // Create and Register Metaistances                                

        // Create and Register Attributes

        classId := '11-Skill:Driving';
        AttrLean := TMetaAttribute.Create(classID, 'Driving', 'Driving Skill', 0);
        AttrLean.Register('Attribute');
        classId := '12-Skill:FireArms';
        AttrLean := TMetaAttribute.Create(classID, 'Firearms', 'FireArms Skill', 0);
        AttrLean.Register('Attribute');
        classId := '13-Skill:Computer';
        AttrLean := TMetaAttribute.Create(classID, 'Computer', 'Computer Skill', 0);
        AttrLean.Register('Attribute');
        classId := '14-Skill:Leadership';
        AttrLean := TMetaAttribute.Create(classID, 'Leadership', 'Leadership Skill', 0);
        AttrLean.Register('Attribute');
        classId := '15-Skill:Brawling';
        AttrLean := TMetaAttribute.Create(classID, 'Brawling', 'Brawling Skill', 0);
        AttrLean.Register('Attribute');
        classId := '16-Skill:Demolition';
        AttrLean := TMetaAttribute.Create(classID, 'Demolition', 'Demolition and Fire Setting Skill', 0);
        AttrLean.Register('Attribute');
        classId := '17-Skill:Stealth';
        AttrLean := TMetaAttribute.Create(classID, 'Stealth', 'Stealth and Disguise Skill', 0);
        AttrLean.Register('Attribute');
        classId := '18-Skill:Medicine';
        AttrLean := TMetaAttribute.Create(classID, 'Medicine', 'Cure and Treat Skill', 0);
        AttrLean.Register('Attribute');
        classId := '19-Skill:Forgery';
        AttrLean := TMetaAttribute.Create(classID, 'Forgery', 'Skill of Producing Fake Document and Money', 0);
        AttrLean.Register('Attribute');

        // Create and Register Factors

        classId := '21-Factor:Loyalty';
        AttrLean := TMetaAttribute.Create(classID, 'Loyalty', 'How much the criminal is loyal to his boss', 20);
        AttrLean.Register('Attribute');
        classId := '22-Factor:Stability';
        AttrLean := TMetaAttribute.Create(classID, 'Stability', 'An indication of the mental sanity of the criminal', 0);
        AttrLean.Register('Attribute');
        classId := '23-Factor:Intelligence';
        AttrLean := TMetaAttribute.Create(classID, 'Learning', 'An indication of how well the criminal learn', 0);
        AttrLean.Register('Attribute');

        // Create and Register Modifier

        classId := 'Modifier1:SkillsModifier';
        ModLean := TMetaModifier.Create(classID, 'SkillsModifier', 'Used to modify skills');
        ModLean.Register('Modifier');

        // Create and Register Attribute Modifiers

        classId := 'Driving-11';
        AttrModLean := TAttributeModifier.Create(classID, 'Driving Licence ', 'Get the basic driving licence', TMetaAttribute(TheClassStorage.ClassById['Attribute', '11-Skill:Driving']), 500, 30, 15, 0, 20);
        AttrModLean.Register('AttributeModifier');
        classId := 'Driving-12';
        AttrModLean := TAttributeModifier.Create(classID, 'Driving Non-Automatic Cars ', 'Learn how to drive with Standard Gears', TMetaAttribute(TheClassStorage.ClassById['Attribute', '11-Skill:Driving']), 300, 10, 10, 15, 30);
        AttrModLean.AddPrevModifier(TAttributeModifier(TheClassStorage.ClassById['AttributeModifier', 'Driving-11']));
        AttrModLean.Register('AttributeModifier');
        classId := 'Driving-13';
        AttrModLean := TAttributeModifier.Create(classID, 'Streets Knowledge ', 'Deep knowledge of the streets of the city your taking the class in', TMetaAttribute(TheClassStorage.ClassById['Attribute', '11-Skill:Driving']), 200, 20, 10, 15, 30);
        AttrModLean.AddPrevModifier(TAttributeModifier(TheClassStorage.ClassById['AttributeModifier', 'Driving-11']));
        AttrModLean.Register('AttributeModifier');
        classId := 'Driving-14';
        AttrModLean := TAttributeModifier.Create(classID, 'High Speed Driving ', 'Learn how to drive at very high speeds', TMetaAttribute(TheClassStorage.ClassById['Attribute', '11-Skill:Driving']), 1500, 40, 10, 20, 40);
        AttrModLean.AddPrevModifier(TAttributeModifier(TheClassStorage.ClassById['AttributeModifier', 'Driving-11']));
        AttrModLean.Register('AttributeModifier');
        classId := 'Driving-15';
        AttrModLean := TAttributeModifier.Create(classID, 'Acrobatic Driving ', 'Perform Special Maneuvers', TMetaAttribute(TheClassStorage.ClassById['Attribute', '11-Skill:Driving']), 2000, 60, 10, 35, 45);
        AttrModLean.AddPrevModifier(TAttributeModifier(TheClassStorage.ClassById['AttributeModifier', 'Driving-11']));
        AttrModLean.AddPrevModifier(TAttributeModifier(TheClassStorage.ClassById['AttributeModifier', 'Driving-14']));
        AttrModLean.Register('AttributeModifier');
        classId := 'Driving-16';
        AttrModLean := TAttributeModifier.Create(classID, 'Reverse Driving ', 'Drive backwards or forward without any differnce', TMetaAttribute(TheClassStorage.ClassById['Attribute', '11-Skill:Driving']), 500, 30, 5, 30, 35);
        AttrModLean.AddPrevModifier(TAttributeModifier(TheClassStorage.ClassById['AttributeModifier', 'Driving-11']));
        AttrModLean.AddPrevModifier(TAttributeModifier(TheClassStorage.ClassById['AttributeModifier', 'Driving-12']));
        AttrModLean.Register('AttributeModifier');
        classId := 'Driving-17';
        AttrModLean := TAttributeModifier.Create(classID, 'Jumps and Two-Wheels ', 'Learn the two most difficult mneuvers', TMetaAttribute(TheClassStorage.ClassById['Attribute', '11-Skill:Driving']), 3000, 80, 10, 55, 60);
        AttrModLean.AddPrevModifier(TAttributeModifier(TheClassStorage.ClassById['AttributeModifier', 'Driving-11']));
        AttrModLean.AddPrevModifier(TAttributeModifier(TheClassStorage.ClassById['AttributeModifier', 'Driving-12']));
        AttrModLean.AddPrevModifier(TAttributeModifier(TheClassStorage.ClassById['AttributeModifier', 'Driving-14']));
        AttrModLean.AddPrevModifier(TAttributeModifier(TheClassStorage.ClassById['AttributeModifier', 'Driving-15']));
        AttrModLean.Register('AttributeModifier');
        classId := 'Driving-18';
        AttrModLean := TAttributeModifier.Create(classID, 'Roadblock Bashing ', 'Pass through any police roadblock', TMetaAttribute(TheClassStorage.ClassById['Attribute', '11-Skill:Driving']), 800, 25, 5, 45, 40);
        AttrModLean.AddPrevModifier(TAttributeModifier(TheClassStorage.ClassById['AttributeModifier', 'Driving-11']));
        AttrModLean.AddPrevModifier(TAttributeModifier(TheClassStorage.ClassById['AttributeModifier', 'Driving-14']));
        AttrModLean.Register('AttributeModifier');
        classId := 'Driving-19';
        AttrModLean := TAttributeModifier.Create(classID, 'Flat Tires Driving ', 'When one or more of your tires blows-up', TMetaAttribute(TheClassStorage.ClassById['Attribute', '11-Skill:Driving']), 1200, 20, 5, 40, 45);
        AttrModLean.AddPrevModifier(TAttributeModifier(TheClassStorage.ClassById['AttributeModifier', 'Driving-11']));
        AttrModLean.AddPrevModifier(TAttributeModifier(TheClassStorage.ClassById['AttributeModifier', 'Driving-12']));
        AttrModLean.AddPrevModifier(TAttributeModifier(TheClassStorage.ClassById['AttributeModifier', 'Driving-15']));
        AttrModLean.Register('AttributeModifier');
        classId := 'Driving-20';
        AttrModLean := TAttributeModifier.Create(classID, 'Under Fire Driving ', 'Driving in the middle of flying bullets', TMetaAttribute(TheClassStorage.ClassById['Attribute', '11-Skill:Driving']), 2000, 50, 10, 45, 40);
        AttrModLean.AddPrevModifier(TAttributeModifier(TheClassStorage.ClassById['AttributeModifier', 'Driving-11']));
        AttrModLean.AddPrevModifier(TAttributeModifier(TheClassStorage.ClassById['AttributeModifier', 'Driving-14']));
        AttrModLean.AddPrevModifier(TAttributeModifier(TheClassStorage.ClassById['AttributeModifier', 'Driving-15']));
        AttrModLean.Register('AttributeModifier');
        classId := 'Driving-21';
        AttrModLean := TAttributeModifier.Create(classID, 'Matching The Right Car ', 'Learn how to choose the right car for a given mission', TMetaAttribute(TheClassStorage.ClassById['Attribute', '11-Skill:Driving']), 800, 60, 10, 30, 35);
        AttrModLean.AddPrevModifier(TAttributeModifier(TheClassStorage.ClassById['AttributeModifier', 'Driving-11']));
        AttrModLean.AddPrevModifier(TAttributeModifier(TheClassStorage.ClassById['AttributeModifier', 'Driving-13']));
        AttrModLean.Register('AttributeModifier');

        // Create and Register History Items

        //Criminals

        classId := 'Criminal11: Training';
        HistItemLean := TMetaHistoryItem.Create(classId, 'Training', 'Every time the criminal completes a training', 'Criminal');
        HistItemLean.Register('HistoryItem');
        classId := 'Criminal12: Hire';
        HistItemLean := TMetaHistoryItem.Create(classId, 'Hire', 'Every time the criminal is hired', 'Criminal');
        HistItemLean.Register('HistoryItem');
        classId := 'Criminal13: Fire';
        HistItemLean := TMetaHistoryItem.Create(classId, 'Fire', 'Every time the criminal is fired', 'Criminal');
        HistItemLean.Register('HistoryItem');
        classId := 'Criminal14: ChangeTeam';
        HistItemLean := TMetaHistoryItem.Create(classId, 'ChangeTeam', 'Every time the criminal changes team', 'Criminal');
        HistItemLean.Register('HistoryItem');
        classId := 'Criminal15: StartedMission';
        HistItemLean := TMetaHistoryItem.Create(classId, 'StartedMission', 'Every time the criminal starts an on-going mission', 'Criminal');
        HistItemLean.Register('HistoryItem');
        classId := 'Criminal16: AccomplishedMission';
        HistItemLean := TMetaHistoryItem.Create(classId, 'AccomplishedMission', 'Every time the criminal accomplishes a mission', 'Criminal');
        HistItemLean.Register('HistoryItem');
        classId := 'Criminal17: AssignedMission';
        HistItemLean := TMetaHistoryItem.Create(classId, 'AssignedMission', 'Every time the criminal is given a mission', 'Criminal');
        HistItemLean.Register('HistoryItem');
        classId := 'Criminal18: DeassignedMission';
        HistItemLean := TMetaHistoryItem.Create(classId, 'DeassignedMission', 'Every time the criminal is deassigned a mission', 'Criminal');
        HistItemLean.Register('HistoryItem');

        //Teams

        classId := 'Team11: TeamCreation';
        HistItemLean := TMetaHistoryItem.Create(classId, 'TeamCreation', 'When the team is born', 'Team');
        HistItemLean.Register('HistoryItem');
        classId := 'Team12: HiredCriminal';
        HistItemLean := TMetaHistoryItem.Create(classId, 'HiredCriminal', 'Every time a criminal is hired by the team', 'Team');
        HistItemLean.Register('HistoryItem');
        classId := 'Team13: FiredCriminal';
        HistItemLean := TMetaHistoryItem.Create(classId, 'FiredCriminal', 'Every time a criminal is fired from the team', 'Team');
        HistItemLean.Register('HistoryItem');
        classId := 'Team14: CriminalChangeTeam';
        HistItemLean := TMetaHistoryItem.Create(classId, 'CriminalChangeTeam', 'Every time a criminal leave or join the team either to go in another team or coming from another team', 'Team');
        HistItemLean.Register('HistoryItem');
        classId := 'Team15: MissionStarted';
        HistItemLean := TMetaHistoryItem.Create(classId, 'MissionStarted', 'Every time the team starts a mission', 'Team');
        HistItemLean.Register('HistoryItem');
        classId := 'Team16: MissionAccomplished';
        HistItemLean := TMetaHistoryItem.Create(classId, 'MissionAccomplished', 'Every time the team accomplishes a mission', 'Team');
        HistItemLean.Register('HistoryItem');
        classId := 'Team17: MissionAssigned';
        HistItemLean := TMetaHistoryItem.Create(classId, 'MissionAssigned', 'Every time a leader assigns a mission to the team', 'Team');
        HistItemLean.Register('HistoryItem');
        classId := 'Team18: MissionDeassigned';
        HistItemLean := TMetaHistoryItem.Create(classId, 'MissionDeassigned', 'Every time a leader deassigns a mission to the team', 'Team');
        HistItemLean.Register('HistoryItem');

        //Leaders

        classId := 'Leader11: LeaderBorn';
        HistItemLean := TMetaHistoryItem.Create(classId, 'LeaderBorn', 'When the player alter-ego is created', 'Leader');
        HistItemLean.Register('HistoryItem');
        classId := 'Leader12: TeamCreated';
        HistItemLean := TMetaHistoryItem.Create(classId, 'TeamCreated', 'Every time the leader creates a team', 'Leader');
        HistItemLean.Register('HistoryItem');
        classId := 'Leader13: TeamDismissed';
        HistItemLean := TMetaHistoryItem.Create(classId, 'TeamDismissed', 'Every time the leader dismisses a team', 'Leader');
        HistItemLean.Register('HistoryItem');
        classId := 'Leader14: CriminalHired';
        HistItemLean := TMetaHistoryItem.Create(classId, 'CriminalHired', 'Every time the leader hire a criminal in a team', 'Leader');
        HistItemLean.Register('HistoryItem');
        classId := 'Leader15: CriminalFired';
        HistItemLean := TMetaHistoryItem.Create(classId, 'CriminalFired', 'Every time the leader fire a criminal from a team', 'Leader');
        HistItemLean.Register('HistoryItem');
        classId := 'Leader16: ChangeTeamToCriminal';
        HistItemLean := TMetaHistoryItem.Create(classId, 'ChangeTeamToCriminal', 'Every time the leader moves a criminal from a team to another', 'Leader');
        HistItemLean.Register('HistoryItem');
        classId := 'Leader17: AssignMission';
        HistItemLean := TMetaHistoryItem.Create(classId, 'AssignMission', 'Every time the leader assigns a mission to a team', 'Leader');
        HistItemLean.Register('HistoryItem');
        classId := 'Leader18: DeassignMission';
        HistItemLean := TMetaHistoryItem.Create(classId, 'DeassignMission', 'Every time the leader deassigns a mission to a team', 'Leader');
        HistItemLean.Register('HistoryItem');
        classId := 'Leader19: MissionCompletedByATeam';
        HistItemLean := TMetaHistoryItem.Create(classId, 'MissionCompletedByATeam', 'Every time a mission assigned to a team has been accomplished', 'Leader');
        HistItemLean.Register('HistoryItem');

        // Create and Register Mission

        classId := 'Mission1:BankRobbery';
        MisLean := TMetaMission.Create(classID, 'Bank Robbery', 'This is the typical bank robbery. The team will enter a bank during its business hours and collect all the money from the counters and the clients and the vault, if you choose so.', 'One-shot');
        MisLean.Register('Mission');
        classId := 'Mission2:GamblingPlace';
        MisLean := TMetaMission.Create(classID, 'Gambling Place', 'The team will run a illegal gambling place', 'On-Going');
        MisLean.Register('Mission');
        classId := 'Mission3:DestroyRoad';
        MisLean := TMetaMission.Create(classID, 'Destroy Road', 'The team will destroy a square of road', 'One-Shot');
        MisLean.Register('Mission');

        // Create and Register Roles

        classId := 'Role0:Unassigned';
        RoleLean := TMetaRole.Create(classId, '', 'No precise role assigned');
        RoleLean.Register('Role');

        classId := 'Role1:Leader';
        RoleLean := TMetaRole.Create(classId, 'Leader', 'The leader of the team');
        RoleLean.AddRequirement(TMetaAttribute(TheClassStorage.ClassById['Attribute', '14-Skill:Leadership']));
        RoleLean.Register('Role');

        classId := 'Role2:Driver';
        RoleLean := TMetaRole.Create(classId, 'Driver', 'Drives your way to or away your destination');
        RoleLean.AddRequirement(TMetaAttribute(TheClassStorage.ClassById['Attribute', '11-Skill:Driving']));
        RoleLean.Register('Role');

        classId := 'Role3:Hacker';
        RoleLean := TMetaRole.Create(classId, 'Hacker', 'Computer Wizard');
        RoleLean.AddRequirement(TMetaAttribute(TheClassStorage.ClassById['Attribute', '13-Skill:Computer']));
        RoleLean.AddRequirement(TMetaAttribute(TheClassStorage.ClassById['Attribute', '19-Skill:Forgery']));
        RoleLean.Register('Role');

        classId := 'Role4:Gorilla';
        RoleLean := TMetaRole.Create(classId, 'Gorilla', 'He is big and knows how to beat you up');
        RoleLean.AddRequirement(TMetaAttribute(TheClassStorage.ClassById['Attribute', '15-Skill:Brawling']));
        RoleLean.AddRequirement(TMetaAttribute(TheClassStorage.ClassById['Attribute', '12-Skill:Firearms']));
        RoleLean.Register('Role');

        classId := 'Role5:Doctor';
        RoleLean := TMetaRole.Create(classId, 'Doctor', 'Self-explanatory');
        RoleLean.AddRequirement(TMetaAttribute(TheClassStorage.ClassById['Attribute', '18-Skill:Medicine']));
        RoleLean.Register('Role');

        classId := 'Role6:Sniper';
        RoleLean := TMetaRole.Create(classId, 'Sniper', 'Bulls-eye shooter');
        RoleLean.AddRequirement(TMetaAttribute(TheClassStorage.ClassById['Attribute', '12-Skill:Firearms']));
        RoleLean.AddRequirement(TMetaAttribute(TheClassStorage.ClassById['Attribute', '17-Skill:Stealth']));
        RoleLean.Register('Role');

        classId := 'Role7:Falsifier';
        RoleLean := TMetaRole.Create(classId, 'Falsifier', 'Can reproduce everything you need');
        RoleLean.AddRequirement(TMetaAttribute(TheClassStorage.ClassById['Attribute', '19-Skill:Forgery']));
        RoleLean.AddRequirement(TMetaAttribute(TheClassStorage.ClassById['Attribute', '13-Skill:Computer']));
        RoleLean.Register('Role');

        classId := 'Role8:Stalker';
        RoleLean := TMetaRole.Create(classId, 'Stalker', 'Silent but deadly');
        RoleLean.AddRequirement(TMetaAttribute(TheClassStorage.ClassById['Attribute', '17-Skill:Stealth']));
        RoleLean.AddRequirement(TMetaAttribute(TheClassStorage.ClassById['Attribute', '15-Skill:Brawling']));
        RoleLean.Register('Role');

        classId := 'Role9:Artificer';
        RoleLean := TMetaRole.Create(classId, 'Artificer', 'Able to use dynamite and set arsons');
        RoleLean.AddRequirement(TMetaAttribute(TheClassStorage.ClassById['Attribute', '16-Skill:Demolition']));
        RoleLean.Register('Role');

        // Create Istances

        // Create Illegal System

        IBSystem := CreateIllSystemProxy;

        // Create and Load Leaders

        {IBSystem.RDOCreateLeader('Usama Bin Laden', 'C:\five\source\illegal\LeadersPicture\UsamaBinLaden.bmp');
        IBSystem.RDOCreateLeader('James Bulger', 'C:\five\source\illegal\LeadersPicture\JamesBulger.bmp');}

        // Create and Load Criminals

        //Create and Register MetaCriminal

        classId := 'Criminal1';
        CrimSkills := TStringList.Create;
        CrimSkills.Values['Driving'] := 'High';
        CrimSkills.Values['FireArms'] := 'Middle';
        CrimSkills.Values['Computer'] := 'No';
        CrimSkills.Values['Ladership'] := 'High';
        CrimSkills.Values['Brawling'] := 'Middle';
        CrimSkills.Values['Demolition'] := 'No';
        CrimSkills.Values['Stealth'] := 'Middle';
        CrimSkills.Values['Medicine'] := 'No';
        CrimSkills.Values['Forgery'] := 'No';
        CrimSkills.Values['Stability'] := '80';
        CrimLean := TMetaCriminal.Create(classId, 'Steve McQueen', 'C:\five\source\illegal\CriminalsPicture\SteveMcQueen.bmp', 5, 35, CrimSkills, 'Male');
        CrimLean.Register('Criminal');

        classId := 'Criminal2';
        CrimSkills := TStringList.Create;
        CrimSkills.Values['Driving'] := 'Low';
        CrimSkills.Values['FireArms'] := 'No';
        CrimSkills.Values['Computer'] := 'No';
        CrimSkills.Values['Ladership'] := 'No';
        CrimSkills.Values['Brawling'] := 'Low';
        CrimSkills.Values['Demolition'] := 'No';
        CrimSkills.Values['Stealth'] := 'No';
        CrimSkills.Values['Medicine'] := 'No';
        CrimSkills.Values['Forgery'] := 'No';
        CrimSkills.Values['Stability'] := '30';
        CrimLean := TMetaCriminal.Create(classId, 'Joe Pena', 'C:\five\source\illegal\CriminalsPicture\PETER MEDELARO.bmp', 1, 17, CrimSkills, 'Male');
        CrimLean.Register('Criminal');

        classId := 'Criminal3';
        CrimSkills := TStringList.Create;
        CrimSkills.Values['Driving'] := 'Middle';
        CrimSkills.Values['FireArms'] := 'No';
        CrimSkills.Values['Computer'] := 'No';
        CrimSkills.Values['Ladership'] := 'No';
        CrimSkills.Values['Brawling'] := 'No';
        CrimSkills.Values['Demolition'] := 'No';
        CrimSkills.Values['Stealth'] := 'High';
        CrimSkills.Values['Medicine'] := 'No';
        CrimSkills.Values['Forgery'] := 'No';
        CrimSkills.Values['Stability'] := '20';
        CrimLean := TMetaCriminal.Create(classId, 'Jane Linux', 'C:\five\source\illegal\CriminalsPicture\ANNA ELIZABETH YOUNG.bmp', 1, 21, CrimSkills, 'Female');
        CrimLean.Register('Criminal');

        classId := 'Criminal4';
        CrimSkills := TStringList.Create;
        CrimSkills.Values['Driving'] := 'Low';
        CrimSkills.Values['FireArms'] := 'High';
        CrimSkills.Values['Computer'] := 'Middle';
        CrimSkills.Values['Ladership'] := 'High';
        CrimSkills.Values['Brawling'] := 'No';
        CrimSkills.Values['Demolition'] := 'No';
        CrimSkills.Values['Stealth'] := 'Low';
        CrimSkills.Values['Medicine'] := 'No';
        CrimSkills.Values['Forgery'] := 'No';
        CrimSkills.Values['Stability'] := '70';
        CrimLean := TMetaCriminal.Create(classId, 'Kaiser Sose', 'C:\five\source\illegal\CriminalsPicture\Kaiser Sose.bmp', 4, 32, CrimSkills, 'Male');
        CrimLean.Register('Criminal');

        classId := 'Criminal5';
        CrimSkills := TStringList.Create;
        CrimSkills.Values['Driving'] := 'No';
        CrimSkills.Values['FireArms'] := 'Middle';
        CrimSkills.Values['Computer'] := 'No';
        CrimSkills.Values['Ladership'] := 'No';
        CrimSkills.Values['Brawling'] := 'Middle';
        CrimSkills.Values['Demolition'] := 'No';
        CrimSkills.Values['Stealth'] := 'High';
        CrimSkills.Values['Medicine'] := 'Low';
        CrimSkills.Values['Forgery'] := 'No';
        CrimSkills.Values['Stability'] := '50';
        CrimLean := TMetaCriminal.Create(classId, 'Mata Hari', 'C:\five\source\illegal\CriminalsPicture\MATA HARI.bmp', 2, 25, CrimSkills, 'Female');
        CrimLean.Register('Criminal');

        classId := 'Criminal6';
        CrimSkills := TStringList.Create;
        CrimSkills.Values['Driving'] := 'No';
        CrimSkills.Values['FireArms'] := 'No';
        CrimSkills.Values['Computer'] := 'No';
        CrimSkills.Values['Ladership'] := 'No';
        CrimSkills.Values['Brawling'] := 'No';
        CrimSkills.Values['Demolition'] := 'Middle';
        CrimSkills.Values['Stealth'] := 'No';
        CrimSkills.Values['Medicine'] := 'No';
        CrimSkills.Values['Forgery'] := 'No';
        CrimSkills.Values['Stability'] := '70';
        CrimLean := TMetaCriminal.Create(classId, 'Steven Pidity', 'C:\five\source\illegal\CriminalsPicture\BRUCE MACKINNON.bmp', 1, 16, CrimSkills, 'Male');
        CrimLean.Register('Criminal');

        classId := 'Criminal7';
        CrimSkills := TStringList.Create;
        CrimSkills.Values['Driving'] := 'No';
        CrimSkills.Values['FireArms'] := 'High';
        CrimSkills.Values['Computer'] := 'No';
        CrimSkills.Values['Ladership'] := 'Middle';
        CrimSkills.Values['Brawling'] := 'High';
        CrimSkills.Values['Demolition'] := 'Middle';
        CrimSkills.Values['Stealth'] := 'Low';
        CrimSkills.Values['Medicine'] := 'Low';
        CrimSkills.Values['Forgery'] := 'No';
        CrimSkills.Values['Stability'] := '40';
        CrimLean := TMetaCriminal.Create(classId, 'Billy The Kid', 'C:\five\source\illegal\CriminalsPicture\BILLY THE KID.bmp', 3, 19, CrimSkills, 'Male');
        CrimLean.Register('Criminal');

        classId := 'Criminal8';
        CrimSkills := TStringList.Create;
        CrimSkills.Values['Driving'] := 'Middle';
        CrimSkills.Values['FireArms'] := 'Low';
        CrimSkills.Values['Computer'] := 'No';
        CrimSkills.Values['Ladership'] := 'No';
        CrimSkills.Values['Brawling'] := 'No';
        CrimSkills.Values['Demolition'] := 'No';
        CrimSkills.Values['Stealth'] := 'No';
        CrimSkills.Values['Medicine'] := 'No';
        CrimSkills.Values['Forgery'] := 'No';
        CrimSkills.Values['Stability'] := '80';
        CrimLean := TMetaCriminal.Create(classId, 'Hope Less', 'C:\five\source\illegal\CriminalsPicture\Jane Brown.bmp', 1, 35, CrimSkills, 'Female');
        CrimLean.Register('Criminal');

        classId := 'Criminal9';
        CrimSkills := TStringList.Create;
        CrimSkills.Values['Driving'] := 'No';
        CrimSkills.Values['FireArms'] := 'High';
        CrimSkills.Values['Computer'] := 'No';
        CrimSkills.Values['Ladership'] := 'No';
        CrimSkills.Values['Brawling'] := 'Middle';
        CrimSkills.Values['Demolition'] := 'High';
        CrimSkills.Values['Stealth'] := 'No';
        CrimSkills.Values['Medicine'] := 'No';
        CrimSkills.Values['Forgery'] := 'Low';
        CrimSkills.Values['Stability'] := '10';
        CrimLean := TMetaCriminal.Create(classId, 'Crazy Joe', 'C:\five\source\illegal\CriminalsPicture\Red Ronnie.bmp', 1, 21, CrimSkills, 'Male');
        CrimLean.Register('Criminal');

        classId := 'Criminal0';
        CrimSkills := TStringList.Create;
        CrimSkills.Values['Driving'] := 'No';
        CrimSkills.Values['FireArms'] := 'High';
        CrimSkills.Values['Computer'] := 'No';
        CrimSkills.Values['Ladership'] := 'No';
        CrimSkills.Values['Brawling'] := 'No';
        CrimSkills.Values['Demolition'] := 'Middle';
        CrimSkills.Values['Stealth'] := 'No';
        CrimSkills.Values['Medicine'] := 'No';
        CrimSkills.Values['Forgery'] := 'No';
        CrimSkills.Values['Stability'] := '40';
        CrimLean := TMetaCriminal.Create(classId, 'Stupid Bill', 'C:\five\source\illegal\CriminalsPicture\DOUGLAS W. MANGINO.bmp', 1, 22, CrimSkills, 'Male');
        CrimLean.Register('Criminal');

        classId := 'Criminal11';
        CrimSkills := TStringList.Create;
        CrimSkills.Values['Driving'] := 'Low';
        CrimSkills.Values['FireArms'] := 'No';
        CrimSkills.Values['Computer'] := 'No';
        CrimSkills.Values['Ladership'] := 'No';
        CrimSkills.Values['Brawling'] := 'No';
        CrimSkills.Values['Demolition'] := 'No';
        CrimSkills.Values['Stealth'] := 'Middle';
        CrimSkills.Values['Medicine'] := 'No';
        CrimSkills.Values['Forgery'] := 'No';
        CrimSkills.Values['Stability'] := '60';
        CrimLean := TMetaCriminal.Create(classId, 'Idiot Ben', 'C:\five\source\illegal\CriminalsPicture\Bill Black.bmp', 1, 23, CrimSkills, 'Male');
        CrimLean.Register('Criminal');

        classId := 'Criminal12';
        CrimSkills := TStringList.Create;
        CrimSkills.Values['Driving'] := 'Low';
        CrimSkills.Values['FireArms'] := 'No';
        CrimSkills.Values['Computer'] := 'No';
        CrimSkills.Values['Ladership'] := 'No';
        CrimSkills.Values['Brawling'] := 'No';
        CrimSkills.Values['Demolition'] := 'Middle';
        CrimSkills.Values['Stealth'] := 'No';
        CrimSkills.Values['Medicine'] := 'No';
        CrimSkills.Values['Forgery'] := 'No';
        CrimSkills.Values['Stability'] := '40';
        CrimLean := TMetaCriminal.Create(classId, 'Homer Simpsons', 'C:\five\source\illegal\CriminalsPicture\Homer Simpsons.bmp', 1, 45, CrimSkills, 'Male');
        CrimLean.Register('Criminal');

        // Create Records

        {
        // Criminal Charges

        ChargeLean.Name         := 'InformaticCrime';
        ChargeLean.PreTrialTime := 300;
        ChargeLean.TrialTime    := 30;
        ChargeLean.LawyersHours := 100;
        ChargeLean.MinBribe     := 5000;
        ChargeLean.MinJailTime  := 3000;
        ChargeLean.MaxJailTime  := 30000;
        IBSystem.Charges[1]    := ChargeLean;

        ChargeLean.Name         := 'BankRobbery';
        ChargeLean.PreTrialTime := 500;
        ChargeLean.TrialTime    := 50;
        ChargeLean.LawyersHours := 500;
        ChargeLean.MinBribe     := 100000;
        ChargeLean.MinJailTime  := 10000;
        ChargeLean.MaxJailTime  := 50000;
        IBSystem.Charges[2]    := ChargeLean;

        ChargeLean.Name         := 'BankRobberyWithSafeWreckage';
        ChargeLean.PreTrialTime := 700;
        ChargeLean.TrialTime    := 70;
        ChargeLean.LawyersHours := 600;
        ChargeLean.MinBribe     := 120000;
        ChargeLean.MinJailTime  := 12000;
        ChargeLean.MaxJailTime  := 60000;
        IBSystem.Charges[3]    := ChargeLean;

        ChargeLean.Name         := 'BankRobberyWithSafeHacking';
        ChargeLean.PreTrialTime := 700;
        ChargeLean.TrialTime    := 70;
        ChargeLean.LawyersHours := 600;
        ChargeLean.MinBribe     := 120000;
        ChargeLean.MinJailTime  := 12000;
        ChargeLean.MaxJailTime  := 60000;
        IBSystem.Charges[4]    := ChargeLean;

        ChargeLean.Name         := 'Murder';
        ChargeLean.PreTrialTime := 1200;
        ChargeLean.TrialTime    := 120;
        ChargeLean.LawyersHours := 1000;
        ChargeLean.MinBribe     := 200000;
        ChargeLean.MinJailTime  := 20000;
        ChargeLean.MaxJailTime  := -1;
        IBSystem.Charges[5]    := ChargeLean;

        ChargeLean.Name         := 'AttemptedMurder';
        ChargeLean.PreTrialTime := 1000;
        ChargeLean.TrialTime    := 100;
        ChargeLean.LawyersHours := 800;
        ChargeLean.MinBribe     := 150000;
        ChargeLean.MinJailTime  := 12000;
        ChargeLean.MaxJailTime  := 80000;
        IBSystem.Charges[6]    := ChargeLean;

        ChargeLean.Name         := 'Assault';
        ChargeLean.PreTrialTime := 200;
        ChargeLean.TrialTime    := 20;
        ChargeLean.LawyersHours := 80;
        ChargeLean.MinBribe     := 10000;
        ChargeLean.MinJailTime  := 2000;
        ChargeLean.MaxJailTime  := 10000;
        IBSystem.Charges[7]    := ChargeLean;

        ChargeLean.Name         := 'AggravatedAssault';
        ChargeLean.PreTrialTime := 400;
        ChargeLean.TrialTime    := 40;
        ChargeLean.LawyersHours := 150;
        ChargeLean.MinBribe     := 20000;
        ChargeLean.MinJailTime  := 5000;
        ChargeLean.MaxJailTime  := 20000;
        IBSystem.Charges[8]    := ChargeLean;
        }

      end;

end.





                           




                           




                           




                           




                           




