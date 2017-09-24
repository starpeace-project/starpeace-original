library ib;

uses
  ShareMem,
  SysUtils,
  Classes,
  World in '..\..\Kernel\World.pas',
  Kernel in '..\..\Kernel\Kernel.pas',
  IllegalKernel in '..\..\Illegal\Illegal Kernel\IllegalKernel.pas',
  ClassStorageInt in '..\..\Class Storage\ClassStorageInt.pas',
  ClassStorage in '..\..\Kernel\ClassStorage.pas',
  Inventions in '..\..\Inventions\Inventions.pas',
  MetaCrime in '..\..\Illegal\Illegal Kernel\MetaCrime.pas',
  CrimeProtocol in '..\..\Illegal\Illegal Kernel\CrimeProtocol.pas',
  RoadDestroy in '..\..\Illegal\Illegal Kernel\RoadDestroy.pas',
  Circuits in '..\..\Circuits\Circuits.pas',
  Logs in '..\..\Logs\Logs.pas',
  Protocol in '..\..\Protocol\Protocol.pas',
  TransportInterfaces in '..\..\Transport\TransportInterfaces.pas',
  VisualClassManager in '..\..\Class Packer\VisualClassManager.pas',
  FacIds in '..\FacIds.pas',
  RankProtocol in '..\..\Protocol\RankProtocol.pas',
  NewsServerInterfaces in '..\..\News Server\NewsServerInterfaces.pas',
  Transport in '..\..\Transport\Transport.pas',
  Standards in '..\Standards.pas';

function ModelExtensionId : string; export;
  begin
    result := 'IB';
  end;

function GetDependances : string; export;
  begin
    result := 'GeneralPack1';
  end;

procedure RegisterModelExtension;             
  //var
    //AttrLean     : TMetaAttribute;
    //ModLean      : TMetaModifier;
    //MisLean      : TMetaMission;
    //RoleLean     : TMetaRole;
    //AttrModLean  : TAttributeModifier;
    //HistItemLean : TMetaHistoryItem;
    //CrimLean     : TMetaCriminal;
    //CrimSkills   : TStringList;
    //ChargeLean   : TMetaCharge;
  begin
    IllegalKernel.RegisterBackup;
    (*
    // Create and Register Attributes

    classId := '11-Skill:Driving';
    AttrLean := TMetaAttribute.Create(classID, 'Driving', 'Driving Skill', 0);
    AttrLean.Register('Attribute');
    classId := '12-Skill:FireArms';
    AttrLean := TMetaAttribute.Create(classID, 'Firearms', 'FireArms Skill', 10);
    AttrLean.Register('Attribute');
    classId := '13-Skill:Computer';
    AttrLean := TMetaAttribute.Create(classID, 'Computer', 'Computer Skill', 0);
    AttrLean.Register('Attribute');
    classId := '14-Skill:Leadership';
    AttrLean := TMetaAttribute.Create(classID, 'Leadership', 'Leadership Skill', 0);
    AttrLean.Register('Attribute');
    classId := '15-Skill:Brawling';
    AttrLean := TMetaAttribute.Create(classID, 'Brawling', 'Brawling Skill', 30);
    AttrLean.Register('Attribute');
    classId := '16-Skill:Demolition';
    AttrLean := TMetaAttribute.Create(classID, 'Demolition', 'Demolition and Fire Setting Skill', 0);
    AttrLean.Register('Attribute');
    classId := '17-Skill:Stealth';
    AttrLean := TMetaAttribute.Create(classID, 'Stealth', 'Stealth and Disguise Skill', 10);
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
    classId := '23-Factor:Learning';
    AttrLean := TMetaAttribute.Create(classID, 'Learning', 'An indication of how well the criminal learn', 20);
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
    classId := 'Criminal19: GivenToAnotherLeader';
    HistItemLean := TMetaHistoryItem.Create(classId, 'GivenToAnotherLeader', 'Every time the criminal is traded to another leader', 'Criminal');
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
    MisLean := TMetaServerMission.Create(classID, 'Bank Robbery', 'This is the typical bank robbery. The team will enter a bank during its business hours and collect all the money from the counters and the clients and the vault, if you choose so.', 'One-shot', TMission);
    MisLean.Register('Mission');
    classId := 'Mission2:GamblingPlace';
    MisLean := TMetaServerMission.Create(classID, 'Gambling Place', 'The team will run a illegal gambling place', 'On-Going', TMission);
    MisLean.Register('Mission');
    classId := 'Mission3:DestroyRoad';
    MisLean := TMetaServerMission.Create(classID, 'Destroy Road', 'The team will destroy a square of road', 'One-Shot', TRoadDestroy);
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

    //Create and Register Criminal Charges

    classId := 'Charge1:InformaticCrime';
    ChargeLean := TMetaCharge.Create(classID, 'InformaticCrime', 300, 30, 100, 5000, 4380, 26280, 10000 );
    ChargeLean.Register('Charge');

    classId := 'Charge2:BankRobbery';
    ChargeLean := TMetaCharge.Create(classID, 'BankRobbery', 500, 50, 500, 100000, 26280, 131400, 100000 );
    ChargeLean.Register('Charge');

    classId := 'Charge3:BankRobberyWithSafeWreckage';
    ChargeLean := TMetaCharge.Create(classID, 'BankRobberyWithSafeWreckage', 700, 70, 600, 120000, 43800, 175200, 120000 );
    ChargeLean.Register('Charge');

    classId := 'Charge4:BankRobberyWithSafeHacking';
    ChargeLean := TMetaCharge.Create(classID, 'BankRobberyWithSafeHacking', 700, 70, 600, 120000, 43800, 175200, 120000 );
    ChargeLean.Register('Charge');

    classId := 'Charge5:Murder2';
    ChargeLean := TMetaCharge.Create(classID, 'Murder2', 1200, 120, 1000, 200000, 43800, -1, 200000 );
    ChargeLean.Register('Charge');

    classId := 'Charge6:AttemptedMurder';
    ChargeLean := TMetaCharge.Create(classID, 'AttemptedMurder', 1000, 100, 800, 150000, 26280, 87600, 100000 );
    ChargeLean.Register('Charge');

    classId := 'Charge7:Assault';
    ChargeLean := TMetaCharge.Create(classID, 'Assault', 200, 20, 80, 10000, 4380, 8760, 20000 );
    ChargeLean.Register('Charge');

    classId := 'Charge8:AggravatedAssault';
    ChargeLean := TMetaCharge.Create(classID, 'AggravatedAssault', 400, 40, 150, 20000, 8760, 17520, 30000 );
    ChargeLean.Register('Charge');

    classId := 'Charge9:VandalizationWithExplosives';
    ChargeLean := TMetaCharge.Create(classID, 'VandalizationWithExplosives', 300, 30, 120, 10000, 8760, 17520, 20000 );
    ChargeLean.Register('Charge');

    classId := 'Charge9:Vandalization';
    ChargeLean := TMetaCharge.Create(classID, 'Vandalization', 200, 25, 100, 10000, 4380, 17520, 15000 );
    ChargeLean.Register('Charge');

    //Create and Register MetaCriminal

    classId := 'Criminal1';
    CrimSkills := TStringList.Create;
    CrimSkills.Values['Driving'] := 'High';
    CrimSkills.Values['FireArms'] := 'Middle';
    CrimSkills.Values['Computer'] := 'No';
    CrimSkills.Values['Leadership'] := 'High';
    CrimSkills.Values['Brawling'] := 'Middle';
    CrimSkills.Values['Demolition'] := 'No';
    CrimSkills.Values['Stealth'] := 'Middle';
    CrimSkills.Values['Medicine'] := 'No';
    CrimSkills.Values['Forgery'] := 'No';
    CrimSkills.Values['Stability'] := '80';
    CrimLean := TMetaCriminal.Create(classId, 'Steve McQueen', 'SteveMcQueen', 5, 35, CrimSkills, 'Male');
    CrimLean.Register('Criminal');

    classId := 'Criminal2';
    CrimSkills := TStringList.Create;
    CrimSkills.Values['Driving'] := 'Low';
    CrimSkills.Values['FireArms'] := 'No';
    CrimSkills.Values['Computer'] := 'No';
    CrimSkills.Values['Leadership'] := 'No';
    CrimSkills.Values['Brawling'] := 'Low';
    CrimSkills.Values['Demolition'] := 'No';
    CrimSkills.Values['Stealth'] := 'No';
    CrimSkills.Values['Medicine'] := 'No';
    CrimSkills.Values['Forgery'] := 'No';
    CrimSkills.Values['Stability'] := '30';
    CrimLean := TMetaCriminal.Create(classId, 'Joe Pena', 'PETER MEDELARO', 1, 17, CrimSkills, 'Male');
    CrimLean.Register('Criminal');

    classId := 'Criminal3';
    CrimSkills := TStringList.Create;
    CrimSkills.Values['Driving'] := 'Middle';
    CrimSkills.Values['FireArms'] := 'No';
    CrimSkills.Values['Computer'] := 'No';
    CrimSkills.Values['Leadership'] := 'No';
    CrimSkills.Values['Brawling'] := 'No';
    CrimSkills.Values['Demolition'] := 'No';
    CrimSkills.Values['Stealth'] := 'High';
    CrimSkills.Values['Medicine'] := 'No';
    CrimSkills.Values['Forgery'] := 'No';
    CrimSkills.Values['Stability'] := '20';
    CrimLean := TMetaCriminal.Create(classId, 'Jane Linux', 'ANNA ELIZABETH YOUNG', 1, 21, CrimSkills, 'Female');
    CrimLean.Register('Criminal');

    classId := 'Criminal4';
    CrimSkills := TStringList.Create;
    CrimSkills.Values['Driving'] := 'Low';
    CrimSkills.Values['FireArms'] := 'High';
    CrimSkills.Values['Computer'] := 'Middle';
    CrimSkills.Values['Leadership'] := 'High';
    CrimSkills.Values['Brawling'] := 'No';
    CrimSkills.Values['Demolition'] := 'No';
    CrimSkills.Values['Stealth'] := 'Low';
    CrimSkills.Values['Medicine'] := 'No';
    CrimSkills.Values['Forgery'] := 'No';
    CrimSkills.Values['Stability'] := '70';
    CrimLean := TMetaCriminal.Create(classId, 'Kaiser Sose', 'Kaiser Sose', 4, 32, CrimSkills, 'Male');
    CrimLean.Register('Criminal');

    classId := 'Criminal5';
    CrimSkills := TStringList.Create;
    CrimSkills.Values['Driving'] := 'No';
    CrimSkills.Values['FireArms'] := 'Middle';
    CrimSkills.Values['Computer'] := 'No';
    CrimSkills.Values['Leadership'] := 'No';
    CrimSkills.Values['Brawling'] := 'Middle';
    CrimSkills.Values['Demolition'] := 'No';
    CrimSkills.Values['Stealth'] := 'High';
    CrimSkills.Values['Medicine'] := 'Low';
    CrimSkills.Values['Forgery'] := 'No';
    CrimSkills.Values['Stability'] := '50';
    CrimLean := TMetaCriminal.Create(classId, 'Mata Hari', 'MATA HARI', 2, 25, CrimSkills, 'Female');
    CrimLean.Register('Criminal');

    classId := 'Criminal6';
    CrimSkills := TStringList.Create;
    CrimSkills.Values['Driving'] := 'No';
    CrimSkills.Values['FireArms'] := 'No';
    CrimSkills.Values['Computer'] := 'No';
    CrimSkills.Values['Leadership'] := 'No';
    CrimSkills.Values['Brawling'] := 'No';
    CrimSkills.Values['Demolition'] := 'Middle';
    CrimSkills.Values['Stealth'] := 'No';
    CrimSkills.Values['Medicine'] := 'No';
    CrimSkills.Values['Forgery'] := 'No';
    CrimSkills.Values['Stability'] := '70';
    CrimLean := TMetaCriminal.Create(classId, 'Steven Pidity', 'BRUCE MACKINNON', 1, 16, CrimSkills, 'Male');
    CrimLean.Register('Criminal');

    classId := 'Criminal7';
    CrimSkills := TStringList.Create;
    CrimSkills.Values['Driving'] := 'No';
    CrimSkills.Values['FireArms'] := 'High';
    CrimSkills.Values['Computer'] := 'No';
    CrimSkills.Values['Leadership'] := 'Middle';
    CrimSkills.Values['Brawling'] := 'High';
    CrimSkills.Values['Demolition'] := 'Middle';
    CrimSkills.Values['Stealth'] := 'Low';
    CrimSkills.Values['Medicine'] := 'Low';
    CrimSkills.Values['Forgery'] := 'No';
    CrimSkills.Values['Stability'] := '40';
    CrimLean := TMetaCriminal.Create(classId, 'Billy The Kid', 'BILLY THE KID', 3, 19, CrimSkills, 'Male');
    CrimLean.Register('Criminal');

    classId := 'Criminal8';
    CrimSkills := TStringList.Create;
    CrimSkills.Values['Driving'] := 'Middle';
    CrimSkills.Values['FireArms'] := 'Low';
    CrimSkills.Values['Computer'] := 'No';
    CrimSkills.Values['Leadership'] := 'No';
    CrimSkills.Values['Brawling'] := 'No';
    CrimSkills.Values['Demolition'] := 'No';
    CrimSkills.Values['Stealth'] := 'No';
    CrimSkills.Values['Medicine'] := 'No';
    CrimSkills.Values['Forgery'] := 'No';
    CrimSkills.Values['Stability'] := '80';
    CrimLean := TMetaCriminal.Create(classId, 'Hope Less', 'Jane Brown', 1, 35, CrimSkills, 'Female');
    CrimLean.Register('Criminal');

    classId := 'Criminal9';
    CrimSkills := TStringList.Create;
    CrimSkills.Values['Driving'] := 'No';
    CrimSkills.Values['FireArms'] := 'High';
    CrimSkills.Values['Computer'] := 'No';
    CrimSkills.Values['Leadership'] := 'No';
    CrimSkills.Values['Brawling'] := 'Middle';
    CrimSkills.Values['Demolition'] := 'High';
    CrimSkills.Values['Stealth'] := 'No';
    CrimSkills.Values['Medicine'] := 'No';
    CrimSkills.Values['Forgery'] := 'Low';
    CrimSkills.Values['Stability'] := '10';
    CrimLean := TMetaCriminal.Create(classId, 'Crazy Joe', 'Red Ronnie', 1, 21, CrimSkills, 'Male');
    CrimLean.Register('Criminal');

    classId := 'Criminal0';
    CrimSkills := TStringList.Create;
    CrimSkills.Values['Driving'] := 'No';
    CrimSkills.Values['FireArms'] := 'High';
    CrimSkills.Values['Computer'] := 'No';
    CrimSkills.Values['Leadership'] := 'No';
    CrimSkills.Values['Brawling'] := 'No';
    CrimSkills.Values['Demolition'] := 'Middle';
    CrimSkills.Values['Stealth'] := 'No';
    CrimSkills.Values['Medicine'] := 'No';
    CrimSkills.Values['Forgery'] := 'No';
    CrimSkills.Values['Stability'] := '40';
    CrimLean := TMetaCriminal.Create(classId, 'Stupid Bill', 'DOUGLAS W. MANGINO', 1, 22, CrimSkills, 'Male');
    CrimLean.Register('Criminal');

    classId := 'Criminal11';
    CrimSkills := TStringList.Create;
    CrimSkills.Values['Driving'] := 'Low';
    CrimSkills.Values['FireArms'] := 'No';
    CrimSkills.Values['Computer'] := 'No';
    CrimSkills.Values['Leadership'] := 'No';
    CrimSkills.Values['Brawling'] := 'No';
    CrimSkills.Values['Demolition'] := 'No';
    CrimSkills.Values['Stealth'] := 'Middle';
    CrimSkills.Values['Medicine'] := 'No';
    CrimSkills.Values['Forgery'] := 'No';
    CrimSkills.Values['Stability'] := '60';
    CrimLean := TMetaCriminal.Create(classId, 'Idiot Ben', 'Bill Black', 1, 23, CrimSkills, 'Male');
    CrimLean.Register('Criminal');

    classId := 'Criminal12';
    CrimSkills := TStringList.Create;
    CrimSkills.Values['Driving'] := 'Low';
    CrimSkills.Values['FireArms'] := 'No';
    CrimSkills.Values['Computer'] := 'No';
    CrimSkills.Values['Leadership'] := 'No';
    CrimSkills.Values['Brawling'] := 'No';
    CrimSkills.Values['Demolition'] := 'Middle';
    CrimSkills.Values['Stealth'] := 'No';
    CrimSkills.Values['Medicine'] := 'No';
    CrimSkills.Values['Forgery'] := 'No';
    CrimSkills.Values['Stability'] := '40';
    CrimLean := TMetaCriminal.Create(classId, 'Homer Simpsons', 'Homer Simpsons', 1, 45, CrimSkills, 'Male');
    CrimLean.Register('Criminal');

    //Load Names

   *)

  end;



procedure RegisterWorldExtension( World : TWorld; WorldLoaded : boolean );
  begin
    // Create Illegal System
    IllSystem := TIllegalSystem.Create;
    World.RegisterWorldExtension( IllSystem );
    IllSystem.Start;

    (*
    IllSystem.FirstMNameList.Values['m1'] := 'Ace';
    IllSystem.FirstFNameList.Values['f1'] := 'Adrianne';
    IllSystem.FirstFNameList.Values['f2'] := 'Amethyst';
    IllSystem.FirstFNameList.Values['f3'] := 'Anise';
    IllSystem.FirstFNameList.Values['f4'] := 'Ann';
    IllSystem.FirstFNameList.Values['f5'] := 'Anne';
    IllSystem.FirstFNameList.Values['f6'] := 'April';
    IllSystem.FirstMNameList.Values['m2'] := 'August';
    IllSystem.FirstFNameList.Values['f7'] := 'Brandi';
    IllSystem.FirstMNameList.Values['m3'] := 'Brent';
    IllSystem.FirstMNameList.Values['m4'] := 'Bud';
    IllSystem.FirstFNameList.Values['f8'] := 'Carol';
    IllSystem.FirstFNameList.Values['f9'] := 'Cassia';
    IllSystem.FirstFNameList.Values['f10'] := 'Charity';
    IllSystem.FirstFNameList.Values['f11'] := 'Chastity';
    IllSystem.FirstFNameList.Values['f12'] := 'Cherish';
    IllSystem.FirstFNameList.Values['f13'] := 'Cherry';
    IllSystem.FirstFNameList.Values['f14'] := 'Christine';
    IllSystem.FirstFNameList.Values['f15'] := 'Clare';
    IllSystem.FirstFNameList.Values['f16'] := 'Coral';
    IllSystem.FirstFNameList.Values['f17'] := 'Crystal';
    IllSystem.FirstFNameList.Values['f18'] := 'Dahlia';
    IllSystem.FirstFNameList.Values['f19'] := 'Daisy';
    IllSystem.FirstMNameList.Values['m5'] := 'Dalton';
    IllSystem.FirstMNameList.Values['m6'] := 'Daniel';
    IllSystem.FirstMNameList.Values['m7'] := 'Darwin';
    IllSystem.FirstFNameList.Values['f20'] := 'Dawn';
    IllSystem.FirstMNameList.Values['m8'] := 'Derrick';
    IllSystem.FirstFNameList.Values['f21'] := 'Destiny';
    IllSystem.FirstMNameList.Values['m9'] := 'Earnest';
    IllSystem.FirstFNameList.Values['f22'] := 'Ebony';
    IllSystem.FirstFNameList.Values['f23'] := 'Ellen';
    IllSystem.FirstMNameList.Values['m10'] := 'Emery';
    IllSystem.FirstFNameList.Values['f24'] := 'Faith';
    IllSystem.FirstFNameList.Values['f25'] := 'Fawn';
    IllSystem.FirstFNameList.Values['f26'] := 'Fern';
    IllSystem.FirstMNameList.Values['m11'] := 'Fox';
    IllSystem.FirstMNameList.Values['m12'] := 'Frederick';
    IllSystem.FirstFNameList.Values['f27'] := 'Gale';
    IllSystem.FirstMNameList.Values['m13'] := 'Garnet';
    IllSystem.FirstFNameList.Values['f28'] := 'Gay';
    IllSystem.FirstFNameList.Values['f29'] := 'Georgina';
    IllSystem.FirstFNameList.Values['f30'] := 'Gillian';
    IllSystem.FirstFNameList.Values['f31'] := 'Ginger';
    IllSystem.FirstFNameList.Values['f32'] := 'Goldie';
    IllSystem.FirstFNameList.Values['f33'] := 'Grace';
    IllSystem.FirstMNameList.Values['m14'] := 'Graham';
    IllSystem.FirstMNameList.Values['m15'] := 'Griffin';
    IllSystem.FirstFNameList.Values['f34'] := 'Hazel';
    IllSystem.FirstMNameList.Values['m16'] := 'Heron';
    IllSystem.FirstFNameList.Values['f37'] := 'Holly';
    IllSystem.FirstFNameList.Values['f38'] := 'Hope';
    IllSystem.FirstMNameList.Values['m17'] := 'Hunter';
    IllSystem.FirstFNameList.Values['f39'] := 'Hyacinth';
    IllSystem.FirstFNameList.Values['f40'] := 'Iris';
    IllSystem.FirstFNameList.Values['f41'] := 'Ivy';
    IllSystem.FirstFNameList.Values['f42'] := 'Jade';
    IllSystem.FirstMNameList.Values['m18'] := 'James';
    IllSystem.FirstFNameList.Values['f43'] := 'Jewel';
    IllSystem.FirstMNameList.Values['m19'] := 'John';
    IllSystem.FirstFNameList.Values['f44'] := 'Joy';
    IllSystem.FirstFNameList.Values['f45'] := 'June';
    IllSystem.FirstFNameList.Values['f46'] := 'Kathleen';
    IllSystem.FirstMNameList.Values['m20'] := 'Kenyon';
    IllSystem.FirstFNameList.Values['f47'] := 'Kiara';
    IllSystem.FirstFNameList.Values['f48'] := 'Kolour';
    IllSystem.FirstMNameList.Values['m21'] := 'Ladislas';
    IllSystem.FirstMNameList.Values['m22'] := 'Lake';
    IllSystem.FirstFNameList.Values['f49'] := 'Laurel';
    IllSystem.FirstFNameList.Values['f50'] := 'Lily';
    IllSystem.FirstFNameList.Values['f51'] := 'Linden';
    IllSystem.FirstFNameList.Values['f52'] := 'Love';
    IllSystem.FirstMNameList.Values['m23'] := 'Mark';
    IllSystem.FirstFNameList.Values['f53'] := 'May';
    IllSystem.FirstFNameList.Values['f54'] := 'Meadow';
    IllSystem.FirstFNameList.Values['f55'] := 'Melody';
    IllSystem.FirstFNameList.Values['f56'] := 'Mercy';
    IllSystem.FirstFNameList.Values['f57'] := 'Merry';
    IllSystem.FirstMNameList.Values['m24'] := 'Milton';
    IllSystem.FirstFNameList.Values['f58'] := 'Misty';
    IllSystem.FirstFNameList.Values['f59'] := 'Modesty';
    IllSystem.FirstFNameList.Values['f60'] := 'Mor';
    IllSystem.FirstFNameList.Values['f61'] := 'Myrtle';
    IllSystem.FirstFNameList.Values['f62'] := 'Nadia';
    IllSystem.FirstFNameList.Values['f63'] := 'Panda';
    IllSystem.FirstFNameList.Values['f64'] := 'Patience';
    IllSystem.FirstFNameList.Values['f65'] := 'Pearl';
    IllSystem.FirstFNameList.Values['f66'] := 'Poppy';
    IllSystem.FirstFNameList.Values['f67'] := 'Prudence';
    IllSystem.FirstFNameList.Values['f68'] := 'Rain';
    IllSystem.FirstFNameList.Values['f69'] := 'Rose';
    IllSystem.FirstMNameList.Values['m25'] := 'Royal';
    IllSystem.FirstFNameList.Values['f70'] := 'Savannah';
    IllSystem.FirstFNameList.Values['f71'] := 'Scarlet';
    IllSystem.FirstFNameList.Values['f72'] := 'Serenity';
    IllSystem.FirstFNameList.Values['f73'] := 'Spring';
    IllSystem.FirstFNameList.Values['f74'] := 'Star';
    IllSystem.FirstFNameList.Values['f75'] := 'Summer';
    IllSystem.FirstFNameList.Values['f76'] := 'Sunshine';
    IllSystem.FirstFNameList.Values['f77'] := 'Tansy';
    IllSystem.FirstFNameList.Values['f78'] := 'Tawny';
    IllSystem.FirstFNameList.Values['f79'] := 'Temperance';
    IllSystem.FirstFNameList.Values['f80'] := 'Tempest';
    IllSystem.FirstMNameList.Values['m26'] := 'Tiger';
    IllSystem.FirstFNameList.Values['f81'] := 'Trinity';
    IllSystem.FirstFNameList.Values['f82'] := 'Twyla';
    IllSystem.FirstFNameList.Values['f83'] := 'Vanessa';
    IllSystem.FirstFNameList.Values['f84'] := 'Verity';
    IllSystem.FirstFNameList.Values['f85'] := 'Violet';
    IllSystem.FirstFNameList.Values['f86'] := 'Willow';
    IllSystem.FirstMNameList.Values['m27'] := 'Wilson';
    IllSystem.FirstMNameList.Values['m28'] := 'Wolf';

    IllSystem.LastNameList.Values['1'] := 'Appleby';
    IllSystem.LastNameList.Values['2'] := 'Argyle';
    IllSystem.LastNameList.Values['3'] := 'BarkHouse';
    IllSystem.LastNameList.Values['4'] := 'Bannon';
    IllSystem.LastNameList.Values['5'] := 'Chapman';
    IllSystem.LastNameList.Values['6'] := 'Cormack';
    IllSystem.LastNameList.Values['7'] := 'Donovan';
    IllSystem.LastNameList.Values['8'] := 'Drury';
    IllSystem.LastNameList.Values['9'] := 'Egan';
    IllSystem.LastNameList.Values['10'] := 'Elliot';
    IllSystem.LastNameList.Values['11'] := 'Fairbanks';
    IllSystem.LastNameList.Values['12'] := 'Fuller';
    IllSystem.LastNameList.Values['13'] := 'Goff';
    IllSystem.LastNameList.Values['14'] := 'Gonzales';
    IllSystem.LastNameList.Values['15'] := 'Higgins';
    IllSystem.LastNameList.Values['16'] := 'Holden';
    IllSystem.LastNameList.Values['17'] := 'Jagger';
    IllSystem.LastNameList.Values['18'] := 'Jarvis';
    IllSystem.LastNameList.Values['19'] := 'Ingram';
    IllSystem.LastNameList.Values['20'] := 'Kerr';
    IllSystem.LastNameList.Values['21'] := 'Kinsey';
    IllSystem.LastNameList.Values['22'] := 'Leary';
    IllSystem.LastNameList.Values['23'] := 'Leonard';
    IllSystem.LastNameList.Values['24'] := 'Morris';
    IllSystem.LastNameList.Values['25'] := 'Moyse';
    IllSystem.LastNameList.Values['26'] := 'O''Shea';
    IllSystem.LastNameList.Values['27'] := 'Richmond';
    IllSystem.LastNameList.Values['28'] := 'Salinger';
    IllSystem.LastNameList.Values['29'] := 'Travis';
    IllSystem.LastNameList.Values['30'] := 'William';
  *)

    if not WorldLoaded
      then
        begin
        end
      else
        begin
          // create system here from scratch (a new world was started)
          // IllSystem := TIllegalSystem.Create;
        end;
  end;

exports
  ModelExtensionId,
  GetDependances,
  RegisterModelExtension,
  RegisterWorldExtension;

{$E mdx}

begin
end.


