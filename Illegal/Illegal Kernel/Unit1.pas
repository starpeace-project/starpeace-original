unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, IllegalKernel, Creator,
  StdCtrls, ExtCtrls, ExtDlgs, ClassStorage, Menus, Collection, Mask,
  ComCtrls, Grids, MetaCrime;

  const
    MaxOptions = 5;
    MaxRoles = 10;
    MaxWays = 2;
    MaxCharges = 8;

  // Move to common unit later!!
  type
    TAttrValue = single;
    TCoordinate =
      record
        X   : Longint;
        Y   : Longint;
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
        (tcsOnTheMarket, tcsOnTheMarketHidden, tcsInTeamStandBy, tcsInTeamTraining, tcsInTeamInMission, tcsInTeamInMissionDead, tcsInJail, tcsHiddenByPolice, tcsInTeamDead, tcsInTeamInJail);

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
        MaxDistance   : integer;
        MaxRadius     : integer;
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


type
  TForm1 = class(TForm)
    StartButton: TButton;
    btnCreateLeader: TButton;
    gbCreateLeader: TGroupBox;
    Label1: TLabel;
    edLeaderName: TEdit;
    btnLeaderOk: TButton;
    btnLeaderImage: TButton;
    Panel1: TPanel;
    Image1: TImage;
    gbLeader: TGroupBox;
    Label2: TLabel;
    Image2: TImage;
    btnLeaderChosen: TButton;
    cbLeaders: TComboBox;
    edLeader: TEdit;
    Label3: TLabel;
    lblTeamNumber: TLabel;
    GroupBox1: TGroupBox;
    Criminal1: TPanel;
    Criminal2: TPanel;
    Image3: TImage;
    Image4: TImage;
    OpenPictureDialog1: TOpenPictureDialog;
    lblCrim1Name: TLabel;
    lblCrim2Name: TLabel;
    Criminal3: TPanel;
    Image5: TImage;
    lblCrim3Name: TLabel;
    Criminal4: TPanel;
    Image6: TImage;
    lblCrim4Name: TLabel;
    Criminal5: TPanel;
    Image7: TImage;
    lblCrim5Name: TLabel;
    Criminal6: TPanel;
    Image8: TImage;
    lblCrim6Name: TLabel;
    Criminal8: TPanel;
    Image10: TImage;
    lblCrim8Name: TLabel;
    Criminal7: TPanel;
    Image9: TImage;
    lblCrim7Name: TLabel;
    Label4: TLabel;
    lblSkillName1: TLabel;
    lblSkillName2: TLabel;
    lblSkillName3: TLabel;
    lblSkillName4: TLabel;
    lblSkillName5: TLabel;
    lblSkillName6: TLabel;
    lblSkill1: TLabel;
    lblSkill2: TLabel;
    lblSkill3: TLabel;
    lblSkill4: TLabel;
    lblSkill5: TLabel;
    lblSkill6: TLabel;
    GroupBox2: TGroupBox;
    Image11: TImage;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    lblCrimTName1: TLabel;
    Label24: TLabel;
    cbRoles1: TComboBox;
    btnCreateNewTeam: TButton;
    GroupBox3: TGroupBox;
    Label25: TLabel;
    eTeamName: TEdit;
    btnTeam: TButton;
    Image12: TImage;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    cbRoles2: TComboBox;
    Label23: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Image13: TImage;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    cbRoles3: TComboBox;
    Label38: TLabel;
    Label39: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Image14: TImage;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label50: TLabel;
    cbRoles4: TComboBox;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label55: TLabel;
    Label56: TLabel;
    Label57: TLabel;
    lblCrimTName2: TLabel;
    lblCrimTName4: TLabel;
    Image15: TImage;
    Label61: TLabel;
    Label62: TLabel;
    Label63: TLabel;
    Label64: TLabel;
    Label65: TLabel;
    Label66: TLabel;
    Label67: TLabel;
    cbRoles5: TComboBox;
    Label68: TLabel;
    Label69: TLabel;
    Label70: TLabel;
    Label71: TLabel;
    Label72: TLabel;
    Label73: TLabel;
    lblCrimTName5: TLabel;
    Label75: TLabel;
    Label76: TLabel;
    Label77: TLabel;
    Label78: TLabel;
    Label79: TLabel;
    Label80: TLabel;
    cbRoles6: TComboBox;
    Label82: TLabel;
    Label83: TLabel;
    Label84: TLabel;
    Label85: TLabel;
    Label86: TLabel;
    Label87: TLabel;
    Image16: TImage;
    lblCrimTName6: TLabel;
    Label88: TLabel;
    Label89: TLabel;
    Label90: TLabel;
    Label91: TLabel;
    Label92: TLabel;
    Label93: TLabel;
    Label94: TLabel;
    cbRoles7: TComboBox;
    Label95: TLabel;
    Label96: TLabel;
    Label97: TLabel;
    Label98: TLabel;
    Label99: TLabel;
    Label100: TLabel;
    Image17: TImage;
    lblCrimTName7: TLabel;
    Label102: TLabel;
    cbRoles8: TComboBox;
    Image18: TImage;
    Label116: TLabel;
    lblCrimTName8: TLabel;
    lblCrimTName3: TLabel;
    btnCancel: TButton;
    PopupMenu1: TPopupMenu;
    Choice1: TMenuItem;
    Choice2: TMenuItem;
    Choice3: TMenuItem;
    cbTeams: TComboBox;
    Label58: TLabel;
    btnDismissTeam: TButton;
    btnCancelTeam: TButton;
    Image19: TImage;
    Image20: TImage;
    Image21: TImage;
    Image22: TImage;
    Image23: TImage;
    Image24: TImage;
    Image25: TImage;
    Image26: TImage;
    GroupBox4: TGroupBox;
    btnHeadquarter: TButton;
    Label59: TLabel;
    Label60: TLabel;
    eHeadX: TEdit;
    eHeadY: TEdit;
    Label74: TLabel;
    lblTime: TLabel;
    Timer1: TTimer;
    Label81: TLabel;
    lblFactorName1: TLabel;
    lblFactorName2: TLabel;
    lblFactor1: TLabel;
    lblFactor2: TLabel;
    Label101: TLabel;
    Label115: TLabel;
    Label117: TLabel;
    Label118: TLabel;
    Label123: TLabel;
    Label124: TLabel;
    Label125: TLabel;
    Label126: TLabel;
    Label127: TLabel;
    Label128: TLabel;
    Label129: TLabel;
    Label130: TLabel;
    Label131: TLabel;
    Label132: TLabel;
    Label133: TLabel;
    Label134: TLabel;
    Label135: TLabel;
    Label136: TLabel;
    Label137: TLabel;
    Label138: TLabel;
    Label139: TLabel;
    Label140: TLabel;
    Label141: TLabel;
    Label142: TLabel;
    Label147: TLabel;
    Label148: TLabel;
    Label149: TLabel;
    Label150: TLabel;
    Label151: TLabel;
    Label152: TLabel;
    Label153: TLabel;
    Label154: TLabel;
    Label119: TLabel;
    lblSalary: TLabel;
    Label120: TLabel;
    Label121: TLabel;
    Label122: TLabel;
    Label143: TLabel;
    Label144: TLabel;
    Label145: TLabel;
    Label146: TLabel;
    Label155: TLabel;
    Label156: TLabel;
    Label157: TLabel;
    Label158: TLabel;
    Label159: TLabel;
    Label160: TLabel;
    Label161: TLabel;
    Label162: TLabel;
    Label163: TLabel;
    GroupBox5: TGroupBox;
    lblTotalSalary: TLabel;
    Label165: TLabel;
    lblExpenses: TLabel;
    Label166: TLabel;
    Label167: TLabel;
    Label168: TLabel;
    lblMoney: TLabel;
    Label170: TLabel;
    Label169: TLabel;
    Label171: TLabel;
    Label172: TLabel;
    Label173: TLabel;
    Label174: TLabel;
    Label175: TLabel;
    Label176: TLabel;
    Label177: TLabel;
    Label178: TLabel;
    Label179: TLabel;
    Label180: TLabel;
    Label181: TLabel;
    Label182: TLabel;
    Label183: TLabel;
    Label185: TLabel;
    Label186: TLabel;
    Label187: TLabel;
    Label188: TLabel;
    Label189: TLabel;
    Label190: TLabel;
    Label191: TLabel;
    Label192: TLabel;
    Label193: TLabel;
    Label194: TLabel;
    Label195: TLabel;
    Label196: TLabel;
    Label164: TLabel;
    Label197: TLabel;
    Label198: TLabel;
    Label199: TLabel;
    Label200: TLabel;
    Label201: TLabel;
    Label202: TLabel;
    Label203: TLabel;
    Label204: TLabel;
    Label205: TLabel;
    Label206: TLabel;
    Label207: TLabel;
    Label208: TLabel;
    Label209: TLabel;
    Label210: TLabel;
    Label211: TLabel;
    Label212: TLabel;
    Label213: TLabel;
    Label214: TLabel;
    Label215: TLabel;
    Label216: TLabel;
    Label217: TLabel;
    Label218: TLabel;
    Label219: TLabel;
    Label220: TLabel;
    Label221: TLabel;
    Label222: TLabel;
    Label223: TLabel;
    Label224: TLabel;
    Label225: TLabel;
    Label226: TLabel;
    Label103: TLabel;
    Label104: TLabel;
    Label105: TLabel;
    Label106: TLabel;
    Label107: TLabel;
    Label108: TLabel;
    Label109: TLabel;
    Label110: TLabel;
    Label111: TLabel;
    Label112: TLabel;
    Label113: TLabel;
    Label114: TLabel;
    Label184: TLabel;
    Label227: TLabel;
    Label228: TLabel;
    Label229: TLabel;
    Label230: TLabel;
    Label231: TLabel;
    Label232: TLabel;
    lblSkillName7: TLabel;
    lblSkillName8: TLabel;
    lblSkillName9: TLabel;
    lblSkill7: TLabel;
    lblSkill8: TLabel;
    lblSkill9: TLabel;
    lblFactor3: TLabel;
    lblFactorName3: TLabel;
    TrackBar1: TTrackBar;
    Label233: TLabel;
    Label234: TLabel;
    Label235: TLabel;
    lblReputation: TLabel;
    Label237: TLabel;
    Label236: TLabel;
    Label238: TLabel;
    lblAvarPerc: TLabel;
    tbSalary1: TTrackBar;
    tbSalary2: TTrackBar;
    tbSalary3: TTrackBar;
    tbSalary4: TTrackBar;
    tbSalary5: TTrackBar;
    tbSalary6: TTrackBar;
    tbSalary7: TTrackBar;
    tbSalary8: TTrackBar;
    Choice4: TMenuItem;
    lblCrim1Age: TLabel;
    lblCrim3Age: TLabel;
    lblCrim5Age: TLabel;
    lblCrim7Age: TLabel;
    lblCrim2Age: TLabel;
    lblCrim4Age: TLabel;
    lblCrim6Age: TLabel;
    lblCrim8Age: TLabel;
    lblCrimTAge1: TLabel;
    lblCrimTAge2: TLabel;
    lblCrimTAge3: TLabel;
    lblCrimTAge4: TLabel;
    lblCrimTAge5: TLabel;
    lblCrimTAge6: TLabel;
    lblCrimTAge7: TLabel;
    lblCrimTAge8: TLabel;
    GroupBox6: TGroupBox;
    lblTotalTraining: TLabel;
    Label240: TLabel;
    Label241: TLabel;
    Label239: TLabel;
    cbTeamHistory: TComboBox;
    Label242: TLabel;
    cbLeaderHistory: TComboBox;
    GroupBox7: TGroupBox;
    Label243: TLabel;
    cbMissions: TComboBox;
    Label244: TLabel;
    Label245: TLabel;
    MissionReqGrid: TStringGrid;
    mMissionDescr: TMemo;
    MissionOption1: TCheckBox;
    MissionOption2: TCheckBox;
    MissionOption3: TCheckBox;
    MissionOption4: TCheckBox;
    MissionWays1: TComboBox;
    btnStartMission: TButton;
    btnAcceptMission: TButton;
    MissionWays2: TComboBox;
    MissionWays3: TComboBox;
    MissionWays4: TComboBox;
    btnResearch: TButton;
    Label246: TLabel;
    lblRoleDescription: TLabel;
    btnCancelMission: TButton;
    btnObjective: TButton;
    GroupBox8: TGroupBox;
    lblObjX: TLabel;
    lblObjY: TLabel;
    btnAssignObj: TButton;
    eObjX: TEdit;
    eObjY: TEdit;
    lblObjective: TLabel;
    Label250: TLabel;
    btnObjCancel: TButton;
    lblMissionCost: TLabel;
    Label251: TLabel;
    lblMissionProfit: TLabel;
    Label252: TLabel;
    Label249: TLabel;
    Label253: TLabel;
    lblMissionDuration: TLabel;
    Label255: TLabel;
    Label256: TLabel;
    GroupBox9: TGroupBox;
    Label254: TLabel;
    lblRepObjective: TLabel;
    Label258: TLabel;
    lblRepCost: TLabel;
    Label260: TLabel;
    lblRepProfit: TLabel;
    Label262: TLabel;
    Label263: TLabel;
    Label264: TLabel;
    lblRepDuration: TLabel;
    Label266: TLabel;
    Label267: TLabel;
    mReport: TMemo;
    btnMissionOk: TButton;
    Choice5: TMenuItem;
    Choice6: TMenuItem;
    choice7: TMenuItem;
    btnHire: TButton;
    lblObjTarget: TLabel;
    cbObjTarget: TComboBox;
    lblObjRadius: TLabel;
    cbObjPlayer: TComboBox;
    lblObjPlayer: TLabel;
    lblObjFacilities: TLabel;
    cbObjFacilities: TComboBox;
    cbObjRadius: TComboBox;
    Label248: TLabel;
    lblMaxDistance: TLabel;
    cbObjX: TComboBox;
    cbObjY: TComboBox;
    Label257: TLabel;
    lblTotalCriminals: TLabel;
    eReputation: TEdit;
    btnReputation: TButton;
    btnChooseLeader: TButton;
    btnJail: TButton;
    GroupBox10: TGroupBox;
    Label247: TLabel;
    Label259: TLabel;
    lblJailLHours: TLabel;
    Label265: TLabel;
    Label268: TLabel;
    Label269: TLabel;
    Label270: TLabel;
    eJailLHours: TEdit;
    eJailBribe: TEdit;
    cbJailCrim: TComboBox;
    cbJailTeam: TComboBox;
    btnJailCancel: TButton;
    btnJailOK: TButton;
    lblHealthName1: TLabel;
    lblHealthName8: TLabel;
    lblHealthName7: TLabel;
    lblHealthName6: TLabel;
    lblHealthName5: TLabel;
    lblHealthName4: TLabel;
    lblHealthName3: TLabel;
    lblHealthName2: TLabel;
    lblHealth1: TLabel;
    lblHealth2: TLabel;
    lblHealth3: TLabel;
    lblHealth4: TLabel;
    lblHealth5: TLabel;
    lblHealth6: TLabel;
    lblHealth7: TLabel;
    lblHealth8: TLabel;
    procedure StartButtonClick(Sender: TObject);
    procedure btnLeaderImageClick(Sender: TObject);
    procedure btnLeaderOkClick(Sender: TObject);
    procedure btnCreateLeaderClick(Sender: TObject);
    procedure btnLeaderChosenClick(Sender: TObject);
    procedure cbLeadersChange(Sender: TObject);
    procedure DisplayCriminalsOnMarket;
    procedure btnChooseLeaderClick(Sender: TObject);
    procedure CriminalClicked(Sender: TObject);
    procedure btnTeamClick(Sender: TObject);
    procedure btnCreateNewTeamClick(Sender: TObject);
    procedure edLeaderNameChange(Sender: TObject);
    procedure btnHireClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure Image2DblClick(Sender: TObject);
    procedure RolesChange(Sender: TObject);
    procedure ColorRoleSkills(position : integer);
    procedure cbTeamsChange(Sender: TObject);
    procedure eTeamNameChange(Sender: TObject);
    procedure Choice1Click(Sender: TObject);
    procedure RightClickOnPicture(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnDismissTeamClick(Sender: TObject);
    procedure btnCancelTeamClick(Sender: TObject);
    procedure ChangeTeamClick(Sender: TObject);
    procedure Training(Sender: TObject);
    procedure StopTraining(Sender: TObject);
    procedure CustomPopUpMenu(CriminalState : string; Index : integer);
    procedure btnHeadquarterClick(Sender: TObject);
    procedure eHeadXChange(Sender: TObject);
    procedure eHeadYChange(Sender: TObject);
    procedure GiveTheBeat(Sender : TObject);
    procedure ShowCriminal(CrimName : string; Position : integer);
    procedure TrackBar1Change(Sender: TObject);
    procedure SalaryChange(Sender: TObject);
    procedure cbLeaderHistoryChange(Sender: TObject);
    procedure cbMissionsChange(Sender: TObject);
    procedure MissionOptionClicked(Sender: TObject);
    procedure MissionWaysChanged(Sender: TObject);
    procedure MissionReqGridSelectCell(Sender: TObject; Col, Row: Integer;
      var CanSelect: Boolean);
    procedure btnAcceptMissionClick(Sender: TObject);
    procedure btnObjCancelClick(Sender: TObject);
    procedure btnAssignObjClick(Sender: TObject);
    procedure ObjFieldsChange(Sender: TObject);
    procedure ListCriminalByLeader;
    procedure btnObjectiveClick(Sender: TObject);
    procedure btnStartMissionClick(Sender: TObject);
    procedure btnMissionOkClick(Sender: TObject);
    procedure ShowMissionBox(TeamList : TStringList);
    procedure btnCancelMissionClick(Sender: TObject);
    procedure eReputationChange(Sender: TObject);
    procedure btnReputationClick(Sender: TObject);
    procedure btnJailClick(Sender: TObject);
    procedure btnJailCancelClick(Sender: TObject);
    procedure btnJailOKClick(Sender: TObject);
    procedure JailFieldsChange(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1               : TForm1;
  CriminalInTeamName  : string;
  Timer               : TTimer;
  TeamLabels             : array[1..8] of TLabel;
  TeamAgeLabels          : array[1..8] of TLabel;
  SkillLabels            : array[1..12] of TLabel;
  SkillNameLabels        : array[1..12] of Tlabel;
  TeamSkillLabels        : array[0..11, 0..7] of TLabel;
  TeamSkillNameLabels    : array[0..11, 0..7] of TLabel;
  TeamImages             : array[1..8] of TImage;
  StatusImages           : array[1..8] of TImage;
  CriminalImages         : array[1..8] of TImage;
  CriminalPanels         : array[1..8] of TPanel;
  CriminalLabels         : array[1..8] of TLabel;
  CriminalAges           : array[1..8] of TLabel;
  TeamRolesCombos        : array[1..8] of TComboBox;
  PopUpMenuItems         : array[1..7] of TMenuItem;
  Salaries               : array[1..8] of TLabel;
  Health                 : array[1..8] of TLabel;
  HealthName             : array[1..8] of TLabel;
  SalaryBars             : array[1..8] of TTrackBar;
  MissionOptions         : array[1..4] of TCheckBox;
  MissionWays            : array[1..4] of TComboBox;
  MisStoredOptions       : array[0..(MaxOptions - 1)] of TMissionOptionInfo;
  Skills                 : TCollection;
  SkillModifiers         : TCollection;
  TeamRoles              : TCollection;
  Criminals              : TStringList;
  TeamsInMenu            : TLockableCollection;
  SkillsInMenu           : TCollection;
  SkillModifiersInMenu   : TCollection;
  readyToShowMission     : boolean;

implementation

{$R *.DFM}

function CheckRoomInTeam: string; forward;
procedure CleanTeamSheet; forward;
procedure CleanCriminal( number : integer); forward;
procedure PutCriminalOnTheMarket(CriminalName : string); forward;
procedure GetCriminalOutOfTeam; forward;
function CalculateTotalSalary(TeamToCalcName : string): single; forward;
function CalculateAvarPerc(TeamToCalcName : string): single; forward;
function CalculateTotalTrainingCost(TeamToCalcName : string): single; forward;
procedure FillTeamHistoryItems(TeamName : string); forward;
procedure FillLeaderHistoryItems(LeaderName : string); forward;
procedure CalculateExpenses; forward;
procedure FillMissionGrid; forward;
procedure CheckRolesForMission; forward;
procedure FillGridWithOption(OptionName : string; WayName : string); forward;
procedure CleanMissionSheet; forward;
procedure CleanMissionGrid; forward;

procedure TForm1.StartButtonClick(Sender: TObject);
  var
    i : integer;
    j : integer;
    k : integer;
    l : integer;
    m : integer;
    n : integer;
    o : integer;
    p : integer;
    q : integer;
  begin
    CreateEverything;
    //Time := 0;
    readyToShowMission := True;
    Timer1.Enabled := True;
    StartButton.Enabled := False;
    {
    for i := 0 to (IllSystem.Leaders.Count - 1) do
      begin
        cbLeaders.Items.Add(TLeader(IllSystem.Leaders.Items[i]).Name);
      end;
    }
    btnCreateLeader.Enabled := True;
    btnChooseLeader.Enabled := True;
    TeamLabels[1] := lblCrimTName1;
    TeamLabels[2] := lblCrimTName2;
    TeamLabels[3] := lblCrimTName3;
    TeamLabels[4] := lblCrimTName4;
    TeamLabels[5] := lblCrimTName5;
    TeamLabels[6] := lblCrimTName6;
    TeamLabels[7] := lblCrimTName7;
    TeamLabels[8] := lblCrimTName8;
    TeamAgeLabels[1] := lblCrimTAge1;
    TeamAgeLabels[2] := lblCrimTAge2;
    TeamAgeLabels[3] := lblCrimTAge3;
    TeamAgeLabels[4] := lblCrimTAge4;
    TeamAgeLabels[5] := lblCrimTAge5;
    TeamAgeLabels[6] := lblCrimTAge6;
    TeamAgeLabels[7] := lblCrimTAge7;
    TeamAgeLabels[8] := lblCrimTAge8;
    SkillLabels[1] := lblSkill1;
    SkillLabels[2] := lblSkill2;
    SkillLabels[3] := lblSkill3;
    SkillLabels[4] := lblSkill4;
    SkillLabels[5] := lblSkill5;
    SkillLabels[6] := lblSkill6;
    SkillLabels[7] := lblSkill7;
    SkillLabels[8] := lblSkill8;
    SkillLabels[9] := lblSkill9;
    SkillLabels[10] := lblFactor1;
    SkillLabels[11] := lblFactor2;
    SkillLabels[12] := lblFactor3;
    SkillNameLabels[1] := lblSkillName1;
    SkillNameLabels[2] := lblSkillName2;
    SkillNameLabels[3] := lblSkillName3;
    SkillNameLabels[4] := lblSkillName4;
    SkillNameLabels[5] := lblSkillName5;
    SkillNameLabels[6] := lblSkillName6;
    SkillNameLabels[7] := lblSkillName7;
    SkillNameLabels[8] := lblSkillName8;
    SkillNameLabels[9] := lblSkillName9;
    SkillNameLabels[10] := lblFactorName1;
    SkillNameLabels[11] := lblFactorName2;
    SkillNameLabels[12] := lblFactorName3;
    TeamSkillLabels[0, 0] := label17;
    TeamSkillLabels[1, 0] := label18;
    TeamSkillLabels[2, 0] := label19;
    TeamSkillLabels[3, 0] := label20;
    TeamSkillLabels[4, 0] := label21;
    TeamSkillLabels[5, 0] := label22;
    TeamSkillLabels[6, 0] := label188;
    TeamSkillLabels[7, 0] := label189;
    TeamSkillLabels[8, 0] := label190;
    TeamSkillLabels[9, 0] := label117;
    TeamSkillLabels[10, 0] := label118;
    TeamSkillLabels[11, 0] := label169;
    TeamSkillLabels[0, 1] := label26;
    TeamSkillLabels[1, 1] := label27;
    TeamSkillLabels[2, 1] := label28;
    TeamSkillLabels[3, 1] := label29;
    TeamSkillLabels[4, 1] := label30;
    TeamSkillLabels[5, 1] := label31;
    TeamSkillLabels[6, 1] := label194;
    TeamSkillLabels[7, 1] := label195;
    TeamSkillLabels[8, 1] := label196;
    TeamSkillLabels[9, 1] := label125;
    TeamSkillLabels[10, 1] := label126;
    TeamSkillLabels[11, 1] := label172;
    TeamSkillLabels[0, 2] := label39;
    TeamSkillLabels[1, 2] := label40;
    TeamSkillLabels[2, 2] := label41;
    TeamSkillLabels[3, 2] := label42;
    TeamSkillLabels[4, 2] := label43;
    TeamSkillLabels[5, 2] := label44;
    TeamSkillLabels[6, 2] := label197;
    TeamSkillLabels[7, 2] := label198;
    TeamSkillLabels[8, 2] := label199;
    TeamSkillLabels[9, 2] := label129;
    TeamSkillLabels[10, 2] := label130;
    TeamSkillLabels[11, 2] := label174;
    TeamSkillLabels[0, 3] := label52;
    TeamSkillLabels[1, 3] := label53;
    TeamSkillLabels[2, 3] := label54;
    TeamSkillLabels[3, 3] := label55;
    TeamSkillLabels[4, 3] := label56;
    TeamSkillLabels[5, 3] := label57;
    TeamSkillLabels[6, 3] := label206;
    TeamSkillLabels[7, 3] := label207;
    TeamSkillLabels[8, 3] := label208;
    TeamSkillLabels[9, 3] := label133;
    TeamSkillLabels[10, 3] := label134;
    TeamSkillLabels[11, 3] := label176;
    TeamSkillLabels[0, 4] := label68;
    TeamSkillLabels[1, 4] := label69;
    TeamSkillLabels[2, 4] := label70;
    TeamSkillLabels[3, 4] := label71;
    TeamSkillLabels[4, 4] := label72;
    TeamSkillLabels[5, 4] := label73;
    TeamSkillLabels[6, 4] := label212;
    TeamSkillLabels[7, 4] := label213;
    TeamSkillLabels[8, 4] := label214;
    TeamSkillLabels[9, 4] := label137;
    TeamSkillLabels[10, 4] := label138;
    TeamSkillLabels[11, 4] := label178;
    TeamSkillLabels[0, 5] := label82;
    TeamSkillLabels[1, 5] := label83;
    TeamSkillLabels[2, 5] := label84;
    TeamSkillLabels[3, 5] := label85;
    TeamSkillLabels[4, 5] := label86;
    TeamSkillLabels[5, 5] := label87;
    TeamSkillLabels[6, 5] := label218;
    TeamSkillLabels[7, 5] := label219;
    TeamSkillLabels[8, 5] := label220;
    TeamSkillLabels[9, 5] := label141;
    TeamSkillLabels[10, 5] := label142;
    TeamSkillLabels[11, 5] := label180;
    TeamSkillLabels[0, 6] := label95;
    TeamSkillLabels[1, 6] := label96;
    TeamSkillLabels[2, 6] := label97;
    TeamSkillLabels[3, 6] := label98;
    TeamSkillLabels[4, 6] := label99;
    TeamSkillLabels[5, 6] := label100;
    TeamSkillLabels[6, 6] := label224;
    TeamSkillLabels[7, 6] := label225;
    TeamSkillLabels[8, 6] := label226;
    TeamSkillLabels[9, 6] := label149;
    TeamSkillLabels[10, 6] := label150;
    TeamSkillLabels[11, 6] := label182;
    TeamSkillLabels[0, 7] := label109;
    TeamSkillLabels[1, 7] := label110;
    TeamSkillLabels[2, 7] := label111;
    TeamSkillLabels[3, 7] := label112;
    TeamSkillLabels[4, 7] := label113;
    TeamSkillLabels[5, 7] := label114;
    TeamSkillLabels[6, 7] := label230;
    TeamSkillLabels[7, 7] := label231;
    TeamSkillLabels[8, 7] := label232;
    TeamSkillLabels[9, 7] := label153;
    TeamSkillLabels[10, 7] := label154;
    TeamSkillLabels[11, 7] := label184;
    TeamSkillNameLabels[0, 0] := label11;
    TeamSkillNameLabels[1, 0] := label12;
    TeamSkillNameLabels[2, 0] := label13;
    TeamSkillNameLabels[3, 0] := label14;
    TeamSkillNameLabels[4, 0] := label15;
    TeamSkillNameLabels[5, 0] := label16;
    TeamSkillNameLabels[6, 0] := label185;
    TeamSkillNameLabels[7, 0] := label186;
    TeamSkillNameLabels[8, 0] := label187;
    TeamSkillNameLabels[9, 0] := label101;
    TeamSkillNameLabels[10, 0] := label115;
    TeamSkillNameLabels[11, 0] := label164;
    TeamSkillNameLabels[0, 1] := label5;
    TeamSkillNameLabels[1, 1] := label6;
    TeamSkillNameLabels[2, 1] := label7;
    TeamSkillNameLabels[3, 1] := label8;
    TeamSkillNameLabels[4, 1] := label9;
    TeamSkillNameLabels[5, 1] := label10;
    TeamSkillNameLabels[6, 1] := label191;
    TeamSkillNameLabels[7, 1] := label192;
    TeamSkillNameLabels[8, 1] := label193;
    TeamSkillNameLabels[9, 1] := label123;
    TeamSkillNameLabels[10, 1] := label124;
    TeamSkillNameLabels[11, 1] := label171;
    TeamSkillNameLabels[0, 2] := label32;
    TeamSkillNameLabels[1, 2] := label33;
    TeamSkillNameLabels[2, 2] := label34;
    TeamSkillNameLabels[3, 2] := label35;
    TeamSkillNameLabels[4, 2] := label36;
    TeamSkillNameLabels[5, 2] := label37;
    TeamSkillNameLabels[6, 2] := label200;
    TeamSkillNameLabels[7, 2] := label201;
    TeamSkillNameLabels[8, 2] := label202;
    TeamSkillNameLabels[9, 2] := label127;
    TeamSkillNameLabels[10, 2] := label128;
    TeamSkillNameLabels[11, 2] := label173;
    TeamSkillNameLabels[0, 3] := label45;
    TeamSkillNameLabels[1, 3] := label46;
    TeamSkillNameLabels[2, 3] := label47;
    TeamSkillNameLabels[3, 3] := label48;
    TeamSkillNameLabels[4, 3] := label49;
    TeamSkillNameLabels[5, 3] := label50;
    TeamSkillNameLabels[6, 3] := label203;
    TeamSkillNameLabels[7, 3] := label204;
    TeamSkillNameLabels[8, 3] := label205;
    TeamSkillNameLabels[9, 3] := label131;
    TeamSkillNameLabels[10, 3] := label132;
    TeamSkillNameLabels[11, 3] := label175;
    TeamSkillNameLabels[0, 4] := label61;
    TeamSkillNameLabels[1, 4] := label62;
    TeamSkillNameLabels[2, 4] := label63;
    TeamSkillNameLabels[3, 4] := label64;
    TeamSkillNameLabels[4, 4] := label65;
    TeamSkillNameLabels[5, 4] := label66;
    TeamSkillNameLabels[6, 4] := label209;
    TeamSkillNameLabels[7, 4] := label210;
    TeamSkillNameLabels[8, 4] := label211;
    TeamSkillNameLabels[9, 4] := label135;
    TeamSkillNameLabels[10, 4] := label136;
    TeamSkillNameLabels[11, 4] := label177;
    TeamSkillNameLabels[0, 5] := label75;
    TeamSkillNameLabels[1, 5] := label76;
    TeamSkillNameLabels[2, 5] := label77;
    TeamSkillNameLabels[3, 5] := label78;
    TeamSkillNameLabels[4, 5] := label79;
    TeamSkillNameLabels[5, 5] := label80;
    TeamSkillNameLabels[6, 5] := label215;
    TeamSkillNameLabels[7, 5] := label216;
    TeamSkillNameLabels[8, 5] := label217;
    TeamSkillNameLabels[9, 5] := label139;
    TeamSkillNameLabels[10, 5] := label140;
    TeamSkillNameLabels[11, 5] := label179;
    TeamSkillNameLabels[0, 6] := label89;
    TeamSkillNameLabels[1, 6] := label90;
    TeamSkillNameLabels[2, 6] := label91;
    TeamSkillNameLabels[3, 6] := label92;
    TeamSkillNameLabels[4, 6] := label93;
    TeamSkillNameLabels[5, 6] := label94;
    TeamSkillNameLabels[6, 6] := label221;
    TeamSkillNameLabels[7, 6] := label222;
    TeamSkillNameLabels[8, 6] := label223;
    TeamSkillNameLabels[9, 6] := label147;
    TeamSkillNameLabels[10, 6] := label148;
    TeamSkillNameLabels[11, 6] := label181;
    TeamSkillNameLabels[0, 7] := label103;
    TeamSkillNameLabels[1, 7] := label104;
    TeamSkillNameLabels[2, 7] := label105;
    TeamSkillNameLabels[3, 7] := label106;
    TeamSkillNameLabels[4, 7] := label107;
    TeamSkillNameLabels[5, 7] := label108;
    TeamSkillNameLabels[6, 7] := label227;
    TeamSkillNameLabels[7, 7] := label228;
    TeamSkillNameLabels[8, 7] := label229;
    TeamSkillNameLabels[9, 7] := label151;
    TeamSkillNameLabels[10, 7] := label152;
    TeamSkillNameLabels[11, 7] := label183;
    Salaries[1] := label120;
    Salaries[2] := label122;
    Salaries[3] := label158;
    Salaries[4] := label159;
    Salaries[5] := label160;
    Salaries[6] := label161;
    Salaries[7] := label162;
    Salaries[8] := label163;
    SalaryBars[1] := tbSalary1;
    SalaryBars[2] := tbSalary2;
    SalaryBars[3] := tbSalary3;
    SalaryBars[4] := tbSalary4;
    SalaryBars[5] := tbSalary5;
    SalaryBars[6] := tbSalary6;
    SalaryBars[7] := tbSalary7;
    SalaryBars[8] := tbSalary8;
    Health[1] := lblHealth1;
    Health[2] := lblHealth2;
    Health[3] := lblHealth3;
    Health[4] := lblHealth4;
    Health[5] := lblHealth5;
    Health[6] := lblHealth6;
    Health[7] := lblHealth7;
    Health[8] := lblHealth8;
    HealthName[1] := lblHealthName1;
    HealthName[2] := lblHealthName2;
    HealthName[3] := lblHealthName3;
    HealthName[4] := lblHealthName4;
    HealthName[5] := lblHealthName5;
    HealthName[6] := lblHealthName6;
    HealthName[7] := lblHealthName7;
    HealthName[8] := lblHealthName8;
    TeamImages[1] := image11;
    TeamImages[2] := image12;
    TeamImages[3] := image13;
    TeamImages[4] := image14;
    TeamImages[5] := image15;
    TeamImages[6] := image16;
    TeamImages[7] := image17;
    TeamImages[8] := image18;
    StatusImages[1] := image19;
    StatusImages[2] := image20;
    StatusImages[3] := image21;
    StatusImages[4] := image22;
    StatusImages[5] := image23;
    StatusImages[6] := image24;
    StatusImages[7] := image25;
    StatusImages[8] := image26;
    CriminalImages[1] := image3;
    CriminalImages[2] := image4;
    CriminalImages[3] := image5;
    CriminalImages[4] := image6;
    CriminalImages[5] := image7;
    CriminalImages[6] := image8;
    CriminalImages[7] := image9;
    CriminalImages[8] := image10;
    CriminalPanels[1] := Criminal1;
    CriminalPanels[2] := Criminal2;
    CriminalPanels[3] := Criminal3;
    CriminalPanels[4] := Criminal4;
    CriminalPanels[5] := Criminal5;
    CriminalPanels[6] := Criminal6;
    CriminalPanels[7] := Criminal7;
    CriminalPanels[8] := Criminal8;
    CriminalLabels[1] := lblCrim1Name;
    CriminalLabels[2] := lblCrim2Name;
    CriminalLabels[3] := lblCrim3Name;
    CriminalLabels[4] := lblCrim4Name;
    CriminalLabels[5] := lblCrim5Name;
    CriminalLabels[6] := lblCrim6Name;
    CriminalLabels[7] := lblCrim7Name;
    CriminalLabels[8] := lblCrim8Name;
    CriminalAges[1] := lblCrim1Age;
    CriminalAges[2] := lblCrim2Age;
    CriminalAges[3] := lblCrim3Age;
    CriminalAges[4] := lblCrim4Age;
    CriminalAges[5] := lblCrim5Age;
    CriminalAges[6] := lblCrim6Age;
    CriminalAges[7] := lblCrim7Age;
    CriminalAges[8] := lblCrim8Age;
    TeamRolesCombos[1] := cbRoles1;
    TeamRolesCombos[2] := cbRoles2;
    TeamRolesCombos[3] := cbRoles3;
    TeamRolesCombos[4] := cbRoles4;
    TeamRolesCombos[5] := cbRoles5;
    TeamRolesCombos[6] := cbRoles6;
    TeamRolesCombos[7] := cbRoles7;
    TeamRolesCombos[8] := cbRoles8;
    MissionOptions[1] := MissionOption1;
    MissionOptions[2] := MissionOption2;
    MissionOptions[3] := MissionOption3;
    MissionOptions[4] := MissionOption4;
    MissionWays[1] := MissionWays1;
    MissionWays[2] := MissionWays2;
    MissionWays[3] := MissionWays3;
    MissionWays[4] := MissionWays4;
    PopUpMenuItems[1] := choice1;
    PopUpMenuItems[2] := choice2;
    PopUpMenuItems[3] := choice3;
    PopUpMenuItems[4] := choice4;
    PopUpMenuItems[5] := choice5;
    PopUpMenuItems[6] := choice6;
    PopUpMenuItems[7] := choice7;
    TrackBar1.SetTick(100);
    TrackBar1.Position := 100;
    TeamsInMenu := TLockableCollection.Create(0, rkBelonguer);
    Skills := TCollection.Create(0, rkBelonguer);
    SkillModifiers := TCollection.Create(0, rkBelonguer);
    TeamRoles := TCollection.Create(0, rkBelonguer);
    //Criminals := TLockableCollection.Create(0, rkBelonguer);
    SkillsInMenu := TCollection.Create(0, rkBelonguer);
    SkillModifiersInMenu := TCollection.Create(0, rkBelonguer);
    for k := 0 to (TheClassStorage.ClassCount['Attribute'] - 1) do
      begin
        Skills.Insert(TMetaAttribute(TheClassStorage.ClassByIdx['Attribute', k]));
        SkillNameLabels[k+1].Caption := (TMetaAttribute(Skills[k]).Name + ':');
        SkillsInMenu.Insert(TMenuItem.Create(PopUpMenu1));
        TMenuItem(SkillsInMenu[k]).Caption := (TMetaAttribute(Skills[k]).Name);
        for l := 0 to 7 do
          begin
            TeamSkillNameLabels[k, l].Caption := (TMetaAttribute(Skills[k]).Name + ':');
          end;
      end;
    for o := 0 to (TheClassStorage.ClassCount['AttributeModifier'] - 1) do
      begin
        SkillModifiers.Insert(TAttributeModifier(TheClassStorage.ClassByIdx['AttributeModifier', o]));
        SkillModifiersInMenu.Insert(TMenuItem.Create(PopUpMenu1));
        TMenuItem(SkillModifiersInMenu[o]).OnClick := Training;
      end;
    for j := 0 to (TheClassStorage.ClassCount['Role'] - 1) do
      begin
        TeamRoles.Insert(TMetaRole(TheClassStorage.ClassByIdx['Role', j]));
        for m := 1 to 8 do
          begin
            TeamRolesCombos[m].Items.Add(TMetaRole(TeamRoles[j]).Name);
            TeamRolesCombos[m].Enabled := False;
          end;
      end;
    cbMissions.Items.Add('');
    for q := 0 to (TheClassStorage.ClassCount['Mission'] - 1) do
      cbMissions.Items.Add(TMetaMission(TheClassStorage.ClassByIdx['Mission', q]).Name);
  end;

procedure TForm1.btnLeaderImageClick(Sender: TObject);
  begin
    OpenPictureDialog1.InitialDir := 'C:\five\source\illegal\LeadersPicture\';
    OpenPictureDialog1.Execute;
    if OpenPictureDialog1.FileName <> ''
    then
      Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
  end;

procedure TForm1.btnLeaderOkClick(Sender: TObject);
  var
    i : integer;
    already : string;
    leader : TStringList;
    leaderList : TStringList;
  begin
    already := 'no';
    leader := TStringList.Create;
    leader.Text := IBSystem.RDOFindLeader(widestring(edLeaderName.Text));
    if leader.Values['Name'] <> ''
    then
      Application.MessageBox( PChar('There is already a Leader called "' + edLeaderName.Text + '". Please choose another name.'), PChar('Error'), MB_OK or MB_ICONERROR )
    else
      begin
        if IBSystem.RDOCreateLeader(widestring(edLeaderName.Text), widestring(edLeaderName.Text), widestring(OpenPictureDialog1.FileName)) <> 1
        then
          begin
            leaderList := TStringList.Create;
            leaderList.Text := IBSystem.RDOGetLeaderList;
            cbLeaders.Clear;
            for i := 0 to (leaderList.Count - 1) do
              cbLeaders.Items.Add(leaderList.Values[IntToStr(i + 1)]);
            gbCreateLeader.Visible := False;
            gbCreateLeader.Height := 25;
            gbCreateLeader.Width := 33;
            gbCreateLeader.Top := 8;
            gbCreateLeader.Left := 688;
            btnCreateLeader.Enabled := True;
            gbLeader.Visible := True;
            cbLeaders.ItemIndex := cbLeaders.Items.Count - 1;
            cbLeadersChange(Self);
            btnLeaderChosenClick(Self);
          end;
      end;
  end;

procedure TForm1.btnCreateLeaderClick(Sender: TObject);
  begin
    edLeaderName.Text := '';
    Image1.Picture.LoadFromFile('C:\five\source\illegal\LeadersPicture\PictureBlank.bmp');
    gbCreateLeader.Visible := True;
    btnCreateLeader.Enabled := False;
    gbCreateLeader.Height := 177;
    gbCreateLeader.Width := 209;
    gbCreateLeader.Top := 104;
    gbCreateLeader.Left := 688;
    GroupBox7.Visible := False;
  end;

procedure TForm1.btnLeaderChosenClick(Sender: TObject);
  var
    i : integer;
    LeaderList : TStringList;
    TeamList : TStringList;
    Back : olevariant;
  begin
    btnLeaderChosen.Visible := False;
    btnCreateNewTeam.Enabled := True;
    edLeader.Text := cbLeaders.Text;
    cbLeaders.Visible := False;
    edLeader.Visible := True;
    btnLeaderChosen.Visible := False;
    btnJail.Visible := True;
    btnDismissTeam.Visible := True;
    LeaderList := TStringList.Create;
    LeaderList.text := IBSystem.RDOFindLeader(widestring(cbLeaders.Text));
    lblMoney.Caption := FloatToStr(Round(StrToFloat(LeaderList.Values['Money'])));
    lblReputation.Caption := LeaderList.Values['Reputation'];
    eReputation.Text := LeaderList.Values['Reputation'];
    CalculateExpenses;
    FillLeaderHistoryItems(cbLeaders.Text);
    cbLeaderHistory.Enabled := True;
    Back := IBSystem.RDOGetCriminalNames(widestring(LeaderList.Values['Name']));
    if VarType(Back) = varInteger
      then
        Application.MessageBox( PChar('Error getting criminals on the market. Code: ' + IntToStr(Back)), PChar('Error'), MB_OK or MB_ICONERROR )
      else
        begin
          Criminals := TStringList.Create;
          Criminals.Text := Back;
          DisplayCriminalsOnMarket;
        end;
    cbTeams.Clear;
    Back := IBSystem.RDOGetTeamListByLeader(widestring(LeaderList.Values['Name']));
    if VarType(Back) = varInteger
      then
        Application.MessageBox( PChar('Unknown Error. Code: ' + IntToStr(Back)), PChar('Error'), MB_OK or MB_ICONERROR )
      else
        begin
          TeamList := TStringList.Create;
          TeamList.Text := Back;
          if TeamList.Count = 0
          then
            cbTeams.Enabled := False
          else
            begin
              cbTeams.Enabled := True;
              for i := 0 to (TeamList.Count - 1) do
                cbTeams.Items.Add(TeamList.Values[IntToStr(i + 1)]);
            end;
        end;
  end;

procedure TForm1.DisplayCriminalsOnMarket;
  var
    m : integer;
    n : integer;
    CriminalToShow : TStringList;
  begin
    CriminalToShow := TStringList.Create;
    for n := 1 to 8 do
      begin
        if Criminals.Values[IntToStr(n)] <> ''
        then
          begin
            CriminalToShow.Text := IBSystem.RDOFindCriminal(widestring(Criminals.Values[IntToStr(n)]));
            CriminalLabels[n].Caption := CriminalToShow.Values['Name'];
            CriminalImages[n].Picture.LoadFromFile(CriminalToShow.Values['Picture']);
            CriminalAges[n].Caption := '- ' + CriminalToShow.Values['Age'];
          end
        else
          begin
            CriminalImages[n].Picture.LoadFromFile('C:\five\source\illegal\CriminalsPicture\NoCriminals.bmp');
            CriminalLabels[n].Caption := '';
            CriminalAges[n].Caption := '';
          end;
      end;
    for m := 1 to (Skills.Count) do
      begin
        skillLabels[m].Caption := '';
      end;
    lblSalary.Caption := '';
    TrackBar1.Position := 100;
    TrackBar1.Enabled := False;
    TrackBar1.Hint := IntToStr(TrackBar1.Position);
    TrackBar1.ShowHint := False;
    for n := 0 to 8 do
      begin
        CriminalPanels[n].Color := clBtnFace;
      end;
  end;

procedure TForm1.cbLeadersChange(Sender: TObject);
  var
    LeaderList : TStringList;
  begin
    if cbLeaders.Text <> ''
    then
      begin
        LeaderList := TStringList.Create;
        LeaderList.Text := IBSystem.RDOFindLeader(widestring(cbLeaders.Text));
        if LeaderList.Values['Picture'] = ''
        then
          image2.Picture.LoadFromFile('C:\five\source\illegal\LeadersPicture\NoPicture.bmp')
        else
          image2.Picture.LoadFromFile(LeaderList.Values['Picture']);
        btnLeaderChosen.Enabled := True;
      end
    else
      btnLeaderChosen.Enabled := False;
  end;

procedure TForm1.btnChooseLeaderClick(Sender: TObject);
  var
    i : integer;
    leaderList : TStringList;
  begin
    gbLeader.Visible := true;
    btnLeaderChosen.Visible := True;
    btnCreateNewTeam.Enabled := False;
    cbLeaders.Text := '';
    edLeader.Text := '';
    cbLeaders.Visible := True;
    edLeader.Visible := False;
    btnJail.Visible := False;
    btnDismissTeam.Visible := True;
    CleanTeamSheet;
    GroupBox2.Visible := False;
    leaderList := TStringList.Create;
    leaderList.Text := IBSystem.RDOGetLeaderList;
    lblTeamNumber.Caption := '0';
    cbTeams.Clear;
    cbLeaders.Clear;
    for i := 0 to (leaderList.Count - 1) do
      cbLeaders.Items.Add(leaderList.Values[IntToStr(i + 1)]);
  end;

procedure TForm1.CriminalClicked(Sender: TObject);
  var
    i     : integer;
    j     : integer;
    SenderIndex   : integer;
    criminal : TStringList;
  begin
    criminal := TStringList.Create;
    SenderIndex := 0;
    for j := 1 to 8 do
      begin
        if TImage(Sender).Name = CriminalImages[j].Name
        then
          SenderIndex := j;
      end;
    TrackBar1.Position := 100;
    TrackBar1.Enabled := True;
    TrackBar1.Hint := IntToStr(TrackBar1.Position);
    TrackBar1.ShowHint := True;
    if CriminalLabels[SenderIndex].Caption <> ''
    then
      begin
        for i := 0 to 8 do
          begin
            CriminalPanels[i].Color := clBtnFace;
          end;
        CriminalPanels[SenderIndex].Color := clWhite;
        if CheckRoomInTeam = 'yes'
        then
          btnHire.Enabled := True
        else
          btnHire.Enabled := False;
        criminal.Text := IBSystem.RDOFindCriminal(widestring(CriminalLabels[SenderIndex].Caption));
        for j := 1 to (Skills.Count) do
          begin
            skillLabels[j].Caption := criminal.Values[IntToStr(j-1)];
          end;
        lblSalary.Caption := IntToStr(Round(StrToFloat(criminal.Values['Salary'])));
      end;
  end;

procedure TForm1.btnTeamClick(Sender: TObject);
  var
    j       : integer;
    Res : integer;
  begin
    Res := IBSystem.RDOCreateTeam(widestring(cbLeaders.Text), widestring(eTeamName.Text));
    if Res = 2
    then
      Application.MessageBox( PChar('You already have a team named "' + eTeamName.Text + '"! Please choose another name.'), PChar('Error'), MB_OK or MB_ICONERROR )
    else
      begin
        if Res = 3
        then
          Application.MessageBox( PChar('You already have the maximum number of teams allowed to your level!'), PChar('Error'), MB_OK or MB_ICONERROR )
        else
          begin
            lblTeamNumber.Caption := IntToStr(StrToInt(lblTeamNumber.Caption) + 1);
            cbTeams.Items.Add(eTeamName.Text);
            cbTeams.Enabled := True;
            cbTeams.ItemIndex := cbTeams.Items.Count - 1;
            GroupBox2.Caption := 'Team : ' + cbTeams.Text;
            btnJail.Enabled := True;
            CleanTeamSheet;
            CleanMissionSheet;
            cbMissions.ItemIndex := -1;
            cbMissions.Enabled := True;
            CleanMissionGrid;
            cbMissions.ItemIndex := 0;
            GroupBox2.Visible := True;
            GroupBox7.Visible := True;
            GroupBox3.Visible := False;
            GroupBox3.Height := 31;
            GroupBox3.Width := 33;
            GroupBox3.Top := 50;
            GroupBox3.Left := 688;
            btnDismissTeam.Enabled := True;
            btnCreateNewTeam.Enabled := True;
            FillTeamHistoryItems(cbTeams.Text);
            FillLeaderHistoryItems(cbLeaders.Text);
            for j := 1 to 8 do
              begin
                if CriminalPanels[j].Color = clWhite
                then
                  begin
                    if CheckRoomInTeam = 'yes'
                    then
                      btnHire.Enabled := True
                    else
                      btnHire.Enabled := False;
                  end;
              end;
          end;
      end;
  end;

procedure TForm1.btnCreateNewTeamClick(Sender: TObject);
  begin
    GroupBox2.Visible := False;
    GroupBox7.Visible := False;
    GroupBox9.Visible := False;
    GroupBox3.Visible := True;
    GroupBox3.Height := 81;
    GroupBox3.Width := 249;
    GroupBox3.Top := 378;
    GroupBox3.Left := 688;
    eTeamName.Text := '';
    eTeamName.SetFocus;
    cbTeams.Enabled := False;
    cbTeams.ItemIndex := -1;
    btnDismissTeam.Enabled := False;
    btnCreateNewTeam.Enabled := False;
    btnJail.Enabled := False;
  end;

procedure TForm1.edLeaderNameChange(Sender: TObject);
  begin
    If edLeaderName.Text <> ''
    then
      btnLeaderOk.Enabled := True
    else
      btnLeaderOk.Enabled := False;
  end;

procedure TForm1.btnHireClick(Sender: TObject);
  var
    i : integer;
    j : integer;
    k : integer;
    m : integer;
    n : integer;
    o : integer;
    hired : string;
    Crim : TStringList;
    CrimLean : TStringList;
  begin
    btnHire.Enabled := False;
    Crim := TStringList.Create;
    for i := 1 to 8 do
      if CriminalPanels[i].Color = clWhite
      then
        Crim.Text := IBSystem.RDOFindCriminal(widestring(CriminalLabels[i].Caption));
    hired := 'no';
    i := 0;
    while hired = 'no' do
      begin
        i := i+1;
          if TeamLabels[i].Caption = ''
          then
            begin
              TeamLabels[i].Caption := Crim.Values['Name'];
              TeamAgeLabels[i].Caption := '- Age:' + Crim.Values['Age'];
              TeamAgeLabels[i].Hint := 'Birthday : ' + Crim.Values['Birthday'];
              TeamRolesCombos[i].Enabled := True;
              for j := 0 to (Skills.Count - 1) do
                begin
                  if TeamSkillNameLabels[j, i-1].Caption = 'Loyalty:'
                    then
                      begin
                        TeamSkillLabels[j, i-1].Caption := FloatToStr(Round(StrToFloat(Crim.Values[IntToStr(j)]) + ((TrackBar1.Position - 100)/10)));
                      end
                    else
                      begin
                        TeamSkillLabels[j, i-1].Caption := Crim.Values[IntToStr(j)];
                      end;
                end;
              TeamImages[i].Picture.LoadFromFile(Crim.Values['Picture']);
              SalaryBars[i].Enabled := True;
              SalaryBars[i].Position := TrackBar1.Position;
              SalaryBars[i].Hint := IntToStr(SalaryBars[i].Position);
              SalaryBars[i].ShowHint := True;
              if IBSystem.RDOHireCriminal(widestring(cbLeaders.Text), widestring(cbTeams.Text), widestring(Crim.Values['Name']), (TrackBar1.Position)) <> 1
              then
                begin
                  Salaries[i].Caption := IntToStr(Round(StrToFloat(Crim.Values['Salary'])));
                  for o := (Criminals.Count - 1) downto 0 do
                    begin
                      if StrLComp(PChar(Crim.Values['Name']), PChar(Criminals.Values[Criminals.Names[o]]), StrLen(PChar(Criminals.Values[Criminals.Names[o]]))) = 0
                      then
                        if StrLen(PChar(Crim.Values['Name'])) = StrLen(PChar(Criminals.Values[Criminals.Names[o]]))
                        then
                          begin
                            Criminals.Delete(o);
                            CrimLean := TStringList.Create;
                            CrimLean.Text := Criminals.Text;
                            Criminals.Clear;
                            for k := 0 to (CrimLean.Count -1) do
                              Criminals.Values[IntToStr(k+1)] := CrimLean.Values[CrimLean.Names[k]];
                          end;
                    end;
                  DisplayCriminalsOnMarket;
                  lblSalary.Caption := '';
                  TrackBar1.Position := 100;
                  TrackBar1.Enabled := False;
                  TrackBar1.Hint := IntToStr(TrackBar1.Position);
                  TrackBar1.ShowHint := False;
                  hired := 'yes';
                  for m := 1 to (Skills.Count) do
                    begin
                      skillLabels[m].Caption := '';
                    end;
                  lblSalary.Caption := '';
                  TrackBar1.Position := 100;
                  TrackBar1.Enabled := False;
                  TrackBar1.Hint := IntToStr(TrackBar1.Position);
                  TrackBar1.ShowHint := False;
                end;
              {newOne := 'no';
              o := 0;
              NewCrim := TStringList.Create;
              while newOne = 'no' do
                begin
                  NewCrim.Text := IBSystem.RDOFindCriminal(widestring(Criminals.Values[IntToStr(o)]));
                  if NewCrim.Values['State'] = 'OnTheMarket'
                  then
                    begin
                      newOne := 'yes';
                      for p := 1 to 8 do
                      begin
                        if NewCrim.Values['Name'] = CriminalLabels[p].Caption
                        then
                          newOne := 'no';
                      end;
                    end;
                    o := o + 1;
                    if o = (Criminals.Count)
                    then
                      begin
                        if newOne = 'no'
                        then
                          begin
                            newOne := 'yes';
                            o := o + 1;
                          end;
                      end;
                end;
              for k := 1 to 8 do
                begin
                  if Crim.Values['Name'] = CriminalLabels[k].Caption
                  then
                    begin
                      if o <= Criminals.Count
                      then
                        begin
                          CriminalLabels[k].Caption := NewCrim.Values['Name'];
                          CriminalImages[k].Picture.LoadFromFile(NewCrim.Values['Picture']);
                          CriminalAges[k].Caption := '- ' + NewCrim.Values['Age'];
                        end
                      else
                        begin
                          CriminalLabels[k].Caption := '';
                          CriminalImages[k].Picture.LoadFromFile('C:\five\source\illegal\CriminalsPicture\NoCriminals.bmp');
                          CriminalAges[k].Caption := '';
                        end;
                      lblSalary.Caption := '';
                      TrackBar1.Position := 100;
                      TrackBar1.Enabled := False;
                      TrackBar1.Hint := IntToStr(TrackBar1.Position);
                      TrackBar1.ShowHint := False;
                    end;
                end;}
            end;
        for n := 0 to 8 do
          begin
            CriminalPanels[n].Color := clBtnFace;
          end;
      end;
    lblTotalSalary.Caption := FloatToStr(Round(CalculateTotalSalary(cbTeams.Text)));
    lblAvarPerc.Caption := FloatToStr(Round(CalculateAvarPerc(cbTeams.Text)));
    FillTeamHistoryItems(cbTeams.Text);
    FillLeaderHistoryItems(cbLeaders.Text);
    CalculateExpenses;
  end;

procedure TForm1.btnCancelClick(Sender: TObject);
  begin
    gbCreateLeader.Visible := False;
    gbCreateLeader.Height := 25;
    gbCreateLeader.Width := 33;
    gbCreateLeader.Top := 8;
    gbCreateLeader.Left := 688;
    btnChooseLeader.Enabled := True;
    btnCreateLeader.Enabled := True;
  end;

procedure TForm1.Image2DblClick(Sender: TObject);
  var
    LeaderList : TStringList;
  begin
    if edLeader.Visible = True
    then
      begin
        LeaderList := TStringList.Create;
        LeaderList.text := IBSystem.RDOFindLeader(widestring(edLeaderName.Text));
        OpenPictureDialog1.InitialDir := 'C:\five\source\illegal\LeadersPicture\';
        OpenPictureDialog1.Execute;
        if OpenPictureDialog1.FileName <> ''
        then
          begin
            Image2.Picture.LoadFromFile(OpenPictureDialog1.FileName);
            LeaderList.Values['Picture'] := OpenPictureDialog1.FileName;
          end;
      end;
  end;

function CheckRoomInTeam: string;
  var
    i           : integer;
    j           : integer;
    space       : string;
    selected    : string;
  begin
    if Form1.GroupBox2.Visible = True
    then
      begin
        space := 'no';
        j := 0;
        selected := 'no';
        for i := 1 to 8 do
          begin
            if TeamLabels[i].Caption = ''
            then
              space := 'yes'
          end;
        while selected = 'no' do
          begin
            j := j+1;
            if CriminalPanels[j].Color = clWhite
            then
              selected := 'yes';
            if j = 9
            then
              selected := 'yes';
          end;
        if j = 9
        then
          space := 'no';
      end
    else
      space := 'no';
    Result := space;
  end;

procedure TForm1.RolesChange(Sender: TObject);
  var
    i : integer;
    j : integer;
    SenderIndex : integer;
    a,b,c : string;
  begin
    SenderIndex := 0;
    for j := 1 to 8 do
      begin
        if TComboBox(Sender).Name = TeamRolesCombos[j].Name
        then
          SenderIndex := j;
      end;
    if TeamRolesCombos[SenderIndex].Text <> ''
    then
      begin
        for i := 1 to (TeamRoles.Count - 1) do
          begin
            a := TeamRolesCombos[SenderIndex].Text;
            b := TMetaRole(TeamRoles.Items[i]).Name;
            c := TMetaRole(TeamRoles.Items[i]).Name;
            if StrLComp(PChar(a), PChar(b), StrLen(PChar(c))) = 0
            then
              if IBSystem.RDOChangeRole(widestring(cbLeaders.Text), widestring(cbTeams.Text), widestring(TeamLabels[SenderIndex].Caption), widestring(TMetaRole(TeamRoles.Items[i]).Name)) = 1
              then
                Application.MessageBox( PChar('Couldn''t change Role.'), PChar('Error'), MB_OK or MB_ICONERROR );
          end;
      end
    else
      if IBSystem.RDOChangeRole(widestring(cbLeaders.Text), widestring(cbTeams.Text), widestring(TeamLabels[SenderIndex].Caption), widestring('')) = 1
      then
        Application.MessageBox( PChar('Couldn''t change Role.'), PChar('Error'), MB_OK or MB_ICONERROR );
    ColorRoleSkills(SenderIndex);
    CheckRolesForMission;
  end;

procedure TForm1.ColorRoleSkills(position : integer);
  var
    i : integer;
    k : integer;
  begin
    TeamRolesCombos[position].Hint := '';
    if TeamRolesCombos[position].Text <> ''
    then
      begin
        for i := 1 to (TeamRoles.Count - 1) do
          if StrLComp(PChar(TeamRolesCombos[position].Text), PChar(TMetaRole(TeamRoles[i]).Name), StrLen(PChar(TMetaRole(TeamRoles[i]).Name))) = 0
          then
            begin
              TeamRolesCombos[position].Hint := TMetaRole(TeamRoles[i]).Desc;
              for k := 0 to (Skills.Count - 1) do
                begin
                  TeamSkillNameLabels[k, position-1].Font.Color := clBlack;
                  if TeamSkillNameLabels[k, position - 1].Color = clBtnFace
                    then
                      TeamSkillNameLabels[k, position-1].Hint := '';
                  if TMetaAttribute(Skills[k]).Name = TMetaAttribute(TMetaRole(TeamRoles[i]).Requirements[0]).Name
                  then
                    begin
                      TeamSkillNameLabels[k, position - 1].Font.Color := clBlue;
                      if TeamSkillNameLabels[k, position - 1].Color = clBtnFace
                        then
                          TeamSkillNameLabels[k, position - 1].Hint := 'Primary Skill';
                    end;
                  if TMetaRole(TeamRoles[i]).Requirements.Count = 2
                  then
                    if TMetaAttribute(Skills[k]) = TMetaAttribute(TMetaRole(TeamRoles[i]).Requirements[1])
                    then
                      begin
                        TeamSkillNameLabels[k, position - 1].Font.Color := clAqua;
                        if TeamSkillNameLabels[k, position - 1].Color = clBtnFace
                        then
                          TeamSkillNameLabels[k, position - 1].Hint := 'Secondary Skill';
                      end;
                end;
            end;
      end
    else
      for k := 0 to (Skills.Count - 1) do
        TeamSkillNameLabels[k, position-1].Font.Color := clBlack;
  end;

procedure TForm1.cbTeamsChange(Sender: TObject);
  var
    i : integer;
    j : integer;
    k : integer;
    l : integer;
    m : integer;
    TeamToShow : TStringList;
  begin
    readyToShowMission := False;
    CleanTeamSheet;
    TeamToShow := TStringList.Create;
    TeamToShow.Text := IBSystem.RDOFindTeam(widestring(cbLeaders.Text), widestring(cbTeams.Text));
    GroupBox2.Caption := 'Team : ' + TeamToShow.Values['Name'];
    for j := 1 to StrToInt(TeamToShow.Values['NumCrim']) do
      begin
        TeamRolesCombos[j].Enabled := True;
        TeamRolesCombos[j].ItemIndex := TeamRolesCombos[j].Items.IndexOf(TeamToShow.Values['Role'+ IntToStr(j - 1)]);
        ShowCriminal(TeamToShow.Values['Criminal'+ IntToStr(j - 1)], j);
      end;
    if CheckRoomInTeam = 'yes'
        then
          btnHire.Enabled := True
        else
          btnHire.Enabled := False;
    if cbTeams.Text = ''
        then
          begin
            btnDismissTeam.Enabled := False;
            GroupBox2.Visible := False;
            GroupBox7.Visible := False;
          end
        else
          begin
            btnDismissTeam.Enabled := True;
            GroupBox2.Visible := True;
            GroupBox7.Visible := True;
          end;
    FillTeamHistoryItems(cbTeams.Text);
    eHeadX.Text := TeamToShow.Values['HeadquarterX'];
    eHeadY.Text := TeamToShow.Values['HeadquarterY'];
    lblTotalSalary.Caption := FloatToStr(Round(CalculateTotalSalary(TeamToShow.Values['Name'])));
    lblAvarPerc.Caption := FloatToStr(Round(CalculateAvarPerc(TeamToShow.Values['Name'])));
    lblTotalTraining.Caption := FloatToStr(Round(CalculateTotalTrainingCost(TeamToShow.Values['Name'])));
    if StrToInt(lblTotalTraining.Caption) < 1
      then
        GroupBox6.Visible := False
      else
        GroupBox6.Visible := True;
    CleanMissionSheet;
    CleanMissionGrid;
    if TeamToShow.Values['MissionName'] <> ''
    then
      begin
        btnDismissTeam.Enabled := False;
        cbMissions.ItemIndex := cbMissions.Items.IndexOf(TeamToShow.Values['MissionName']);
        cbMissionsChange(cbMissions);
        for i := 1 to MaxOptions do
          if TeamToShow.Values['ParameterValue' + IntToStr(i)] = 'yes'
          then
            begin
              MissionOptions[i].Checked := True;
              if TeamToShow.Values['ParameterWay' + IntToStr(i)] <> ''
              then
                begin
                  MissionWays[i].Visible := True;
                  MissionWays[i].ItemIndex := MissionWays[i].Items.IndexOf(TeamToShow.Values['ParameterWay' + IntToStr(i)]);
                end;
            end;
        readyToShowMission := True;
        FillMissionGrid;
        for k := 1 to 8 do
          for l := 1 to 8 do
            begin
              if TeamToShow.Values['CrimInMission' + IntToStr(k)] = TeamLabels[l].Caption
              then
                begin
                  TeamRolesCombos[l].ItemIndex := TeamRolesCombos[l].Items.IndexOf(TeamToShow.Values['RoleInMissionName' + IntToStr(k)]);
                  TeamRolesCombos[l].Enabled := False;
                end;
              if TeamLabels[l].Caption = ''
              then
                TeamRolesCombos[l].Enabled := False;
            end;
        eObjX.Text := TeamToShow.Values['ObjX'];
        eObjY.Text := TeamToShow.Values['ObjY'];
        lblObjective.Caption := TeamToShow.Values['Objective'];
        cbMissions.Enabled := False;
        for m := 1 to (MaxOptions - 1) do
          begin
            MissionOptions[m].Enabled := False;
            MissionWays[m].Enabled := False;
          end;
        if Form1.btnMissionOk.Enabled = True
        then
          begin
            Form1.GroupBox7.Visible := False;
            Form1.GroupBox9.Visible := True;
            ShowMissionBox(TeamToShow);
          end
        else
          begin
            Form1.GroupBox7.Visible := True;
            Form1.GroupBox9.Visible := False;
          end;
        btnObjective.Enabled := False;
        Form1.btnAcceptMission.Visible := False;
        Form1.btnResearch.visible := True;
        Form1.btnStartMission.Visible := True;
        Form1.btnCancelMission.Visible := True;
      end
    else
      begin
        cbMissions.ItemIndex := -1;
        cbMissions.Enabled := True;
        btnDismissTeam.Enabled := True;
        readyToShowMission := True;
      end;
  end;

procedure TForm1.ShowCriminal(CrimName : string; Position : integer);
  var
    i : integer;
    k : integer;
    m : integer;
    crimList : TStringList;
  begin
    crimList := TStringList.Create;
    crimList.Text := IBSystem.RDOFindCriminal(widestring(CrimName));
    TeamLabels[Position].Caption := crimList.Values['Name'];
    TeamAgeLabels[Position].Caption := '- Age:' + crimList.Values['Age'];
    TeamImages[Position].Picture.LoadFromFile(crimList.Values['Picture']);
    for k := 1 to (Skills.Count) do
      begin
        TeamSkillLabels[k-1, Position-1].Caption := crimList.Values[IntToStr(k - 1)];
        TeamSkillNameLabels[k-1, Position-1].Color := clBtnFace;
        TeamSkillNameLabels[k-1, Position-1].Hint := '';
      end;
    StatusImages[Position].Picture := nil;
    Salaries[Position].Caption := IntToStr(Round(StrToFloat(crimList.Values['Salary'])));
    SalaryBars[Position].Position := StrToInt(crimList.Values['SalaryPerc']);
    SalaryBars[Position].Enabled := True;
    SalaryBars[Position].ShowHint := True;
    ColorRoleSkills(Position);
    if crimList.Values['State'] = IntToStr(4)
    then
      begin
        StatusImages[Position].Picture.LoadFromFile('C:\five\source\illegal\UtilityPictures\Training.bmp');
        for m := 0 to (Skills.Count - 1) do
          begin
            if crimList.Values['AttrInTraining'] = TMetaAttribute(Skills[m]).Name
            then
              begin
                TeamSkillNameLabels[m, Position-1].Color := clRed;
                for i := 0 to (SkillModifiers.Count - 1) do
                  if crimList.Values['TrainingClass'] = TAttributeModifier(SkillModifiers[i]).Name
                    then
                      TeamSkillNameLabels[m, Position-1].Hint := TAttributeModifier(SkillModifiers[i]).Name + ', Difficulty: ' + IntToStr(TAttributeModifier(SkillModifiers[i]).Difficulty) + ', Av. Gain: ' + IntToStr(TAttributeModifier(SkillModifiers[i]).value) + ' - Finishing Time: ' + IntToStr(TAttributeModifier(SkillModifiers[i]).Time + IBSystem.RDOGetTime) + ', Cost /h: ' + IntToStr(Round(TAttributeModifier(SkillModifiers[i]).Cost / TAttributeModifier(SkillModifiers[i]).Time));
              end;
          end;
      end;
    if crimList.Values['State'] = IntToStr(3)
    then
      begin
        TeamRolesCombos[Position].Enabled := True;
        StatusImages[Position].Picture := nil;
      end;
    if crimList.Values['State'] = IntToStr(5)
    then
      begin
        StatusImages[Position].Picture.LoadFromFile('C:\five\source\illegal\UtilityPictures\Mission.bmp');
      end;
    if crimList.Values['Health'] <> '100'
    then
      begin
        TeamRolesCombos[Position].Enabled := False;
        TeamRolesCombos[Position].Visible := False;
        TeamRolesCombos[Position].ItemIndex := -1;
        HealthName[Position].Visible := True;
        Health[Position].Visible := True;
        Health[Position].Caption := crimList.Values['Health'];
        StatusImages[Position].Picture.LoadFromFile('C:\five\source\illegal\UtilityPictures\Wounded.bmp');
      end
    else
      begin
        TeamRolesCombos[Position].Enabled := True;
        TeamRolesCombos[Position].Visible := True;
        HealthName[Position].Visible := False;
        Health[Position].Visible := False;
        Health[Position].Caption := crimList.Values['Health'];
      end;
    if crimList.Values['State'] = IntToStr(6)
    then
      begin
        StatusImages[Position].Picture.LoadFromFile('C:\five\source\illegal\UtilityPictures\MissionDead.bmp');
      end;
    if crimList.Values['State'] = IntToStr(9)
    then
      begin
        TeamRolesCombos[Position].Enabled := False;
        TeamRolesCombos[Position].ItemIndex := -1;
        StatusImages[Position].Picture.LoadFromFile('C:\five\source\illegal\UtilityPictures\Dead.bmp');
      end;
    if crimList.Values['State'] = IntToStr(10)
    then
      begin
        TeamRolesCombos[Position].Enabled := False;
        TeamRolesCombos[Position].ItemIndex := -1;
        StatusImages[Position].Picture.LoadFromFile('C:\five\source\illegal\UtilityPictures\Jail.bmp');
      end;
    if crimList.Values['State'] = IntToStr(7)
    then
      begin
        StatusImages[Position].Picture.LoadFromFile('C:\five\source\illegal\UtilityPictures\Jail.bmp');
      end;
  end;

procedure CleanTeamSheet;
  var
    i : integer;
  begin
    for i := 1 to 8 do
      begin
        CleanCriminal(i);
      end;
    Form1.eHeadX.Text := '0';
    Form1.eHeadY.Text := '0';
    Form1.lblTotalSalary.Caption := '0';
    Form1.lblTotalTraining.Caption := '0';
    Form1.GroupBox6.Visible := False;
  end;

procedure CleanCriminal( number : integer);
  var
    j : integer;
  begin
    TeamLabels[number].Caption := '';
    TeamAgeLabels[number].Caption := '';
    TeamImages[number].Picture := nil;
    StatusImages[number].Picture := nil;
    SalaryBars[number].Enabled := False;
    SalaryBars[number].Position := 0;
    SalaryBars[number].ShowHint := False;
    TeamRolesCombos[number].ItemIndex := -1;
    TeamRolesCombos[number].Enabled := False;
    for j := 1 to Skills.Count do
      begin
        TeamSkillLabels[j-1, number - 1].Caption := '';
        TeamSkillNameLabels[j-1, number - 1].Font.Color := clBlack;
        TeamSkillNameLabels[j-1, number - 1].Color := clBtnFace;
        TeamSkillNameLabels[j-1, number - 1].Hint := '';
      end;
    Salaries[number].Caption := '0';
  end;

procedure TForm1.eTeamNameChange(Sender: TObject);
  begin
    if eTeamName.Text <> ''
    then
      btnTeam.Enabled := True
    else
      btnTeam.Enabled := False;
  end;

procedure TForm1.Choice1Click(Sender: TObject);
  begin
    GetCriminalOutOfTeam;
    CalculateExpenses;
    if IBSystem.RDOChangeCriminalState(widestring(CriminalInTeamName), 1) = 1
    then
      Application.MessageBox( PChar('Couldn''t change criminal stete.'), PChar('Error'), MB_OK or MB_ICONERROR );
    PutCriminalOnTheMarket(CriminalInTeamName);
    FillTeamHistoryItems(cbTeams.Text);
    FillLeaderHistoryItems(cbLeaders.Text);
  end;

procedure GetCriminalOutOfTeam;
  var
    i : integer;
  begin
    for i := 1 to 8 do
      If CriminalInTeamName = TeamLabels[i].Caption
      then
        CleanCriminal(i);
    if IBSystem.RDOFireCriminal(widestring(Form1.cbLeaders.Text), widestring(Form1.cbTeams.Text), widestring(CriminalInTeamName)) = 1
    then
      Application.MessageBox( PChar('Couldn''t fire criminal.'), PChar('Error'), MB_OK or MB_ICONERROR );
    Form1.lblTotalSalary.Caption := FloatToStr(Round(CalculateTotalSalary(Form1.cbTeams.Text)));
    Form1.lblAvarPerc.Caption := FloatToStr(Round(CalculateAvarPerc(Form1.cbTeams.Text)));
  end;

procedure TForm1.RightClickOnPicture(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  var
    j     : integer;
    SenderIndex : integer;
    crimList : TStringList;
  begin
    SenderIndex := 0;
    for j := 1 to 8 do
      begin
        if TImage(Sender).Name = TeamImages[j].Name
        then
          SenderIndex := j;
        if TImage(Sender).Name = StatusImages[j].Name
        then
          SenderIndex := j;
      end;
    if TeamLabels[SenderIndex].Caption <> ''
    then
      begin
        CriminalInTeamName := TeamLabels[SenderIndex].Caption;
        crimList := TStringList.Create;
        crimList.Text := IBSystem.RDOFindCriminal(widestring(CriminalInTeamName));
        CustomPopUpMenu(crimList.Values['State'], SenderIndex);
      end
    else
      begin
        CustomPopUpMenu('NoCriminal', 0);
      end;
  end;

procedure PutCriminalOnTheMarket(CriminalName : string);
  begin
    Form1.DisplayCriminalsOnMarket;
  end;
  {var
    i : integer;
    FreeSpot : integer;
    crimList : TStringList;
  begin
    i := 0;
    FreeSpot := 0;
    while FreeSpot = 0 do
    begin
      i := i+1;
      if CriminalLabels[i].Caption = ''
      then
        FreeSpot := i;
      if i > 8
      then
        FreeSpot := -1;
    end;
    if FreeSpot <> -1
    then
      begin
        CriminalLabels[FreeSpot].Caption := CriminalName;
        crimList := TStringList.Create;
        crimList.Text := IBSystem.RDOFindCriminal(widestring(CriminalName));
        CriminalImages[FreeSpot].Picture.LoadFromFile(crimList.Values['Picture']);
        CriminalAges[FreeSpot].Caption := '- ' + crimList.Values['Age'];
      end;
  end;}

procedure TForm1.btnDismissTeamClick(Sender: TObject);
  var
    i : integer;
  begin
    btnDismissTeam.Enabled := False;
    if IBSystem.RDODismissTeam(widestring(cbLeaders.Text), widestring(cbTeams.Text)) = 1
    then
      Application.MessageBox( PChar('Couldn''t dismiss team.'), PChar('Error'), MB_OK or MB_ICONERROR );
    for i := 1 to 8 do
      if TeamLabels[i].Caption <> ''
      then
        PutCriminalOnTheMarket(TeamLabels[i].Caption);
    CleanTeamSheet;
    GroupBox2.Visible := False;
    GroupBox7.Visible := False;
    btnHire.Enabled := False;
    cbTeams.Items.Delete(cbTeams.ItemIndex);
    cbTeams.ItemIndex := -1;
    lblTeamNumber.Caption := IntToStr(StrToInt(lblTeamNumber.Caption) - 1);
    FillLeaderHistoryItems(cbLeaders.Text);
  end;

procedure TForm1.btnCancelTeamClick(Sender: TObject);
  begin
    GroupBox3.Visible := False;
    GroupBox3.Height := 31;
    GroupBox3.Width := 33;
    GroupBox3.Top := 50;
    GroupBox3.Left := 688;
    cbTeams.Enabled := True;
    btnCreateNewTeam.Enabled := True;
    btnJail.Enabled := True;
  end;

procedure TForm1.ChangeTeamClick(Sender: TObject);
  var
    i          : integer;
  begin
    if IBSystem.RDOChangeTeam(widestring(cbLeaders.Text), widestring(cbTeams.Text), widestring(TMenuItem(Sender).Caption), widestring(CriminalInTeamName)) <> 1
    then
      begin
        for i := 1 to 8 do
          If CriminalInTeamName = TeamLabels[i].Caption
          then
            CleanCriminal(i);
        lblTotalSalary.Caption := FloatToStr(Round(CalculateTotalSalary(cbTeams.Text)));
        lblAvarPerc.Caption := FloatToStr(Round(CalculateAvarPerc(cbTeams.Text)));
        FillTeamHistoryItems(cbTeams.Text);
        FillLeaderHistoryItems(cbLeaders.Text);
      end
    else
      Application.MessageBox( PChar('Couldn''t change criminal team.'), PChar('Error'), MB_OK or MB_ICONERROR );
  end;

procedure TForm1.StopTraining(Sender: TObject);
  var
    i : integer;
    j : integer;
  begin
    if IBSystem.RDOStopCriminalTraining(widestring(CriminalInTeamName)) = 1
    then
      Application.MessageBox( PChar('Couldn''t stop criminal training.'), PChar('Error'), MB_OK or MB_ICONERROR );
    lblTotalTraining.Caption := FloatToStr(Round(CalculateTotalTrainingCost(cbTeams.Text)));
    if StrToInt(lblTotalTraining.Caption) < 1
      then
        GroupBox6.Visible := False
      else
        GroupBox6.Visible := True;
    for i := 1 to 8 do
      if CriminalInTeamName = TeamLabels[i].Caption
      then
        begin
          StatusImages[i].Picture := nil;
          for j := 0 to (Skills.Count - 1) do
            begin
              TeamSkillNameLabels[j, i-1].Color := clBtnFace;
              TeamSkillNameLabels[j, i-1].Hint := '';
            end;
        end;
  end;

procedure TForm1.Training(Sender: TObject);
  var
    i : integer;
    k : integer;
    skill : string;
  begin
    if IBSystem.RDOCriminalTraining(widestring(CriminalInTeamName), widestring(TAttributeModifier(SkillModifiers[StrToInt(TMenuItem(Sender).Hint)]).Name)) <> 1
    then
      begin
        lblTotalTraining.Caption := FloatToStr(Round(CalculateTotalTrainingCost(cbTeams.Text)));
        if StrToInt(lblTotalTraining.Caption) < 1
          then
            GroupBox6.Visible := False
          else
            GroupBox6.Visible := True;
        skill := TMenuItem(Sender).Parent.Caption;
        for i := 1 to 8 do
          begin
            if CriminalInTeamName = TeamLabels[i].Caption
            then
              begin
                StatusImages[i].Picture.LoadFromFile('C:\five\source\illegal\UtilityPictures\Training.bmp');
                for k := 0 to (Skills.Count- 1) do
                begin
                  if TeamSkillNameLabels[k, i-1].Caption = skill + ':'
                  then
                    begin
                      TeamSkillNameLabels[k, i-1].Color := clRed;
                      TeamSkillNameLabels[k, i-1].Hint :=  TAttributeModifier(SkillModifiers[StrToInt(TMenuItem(Sender).Hint)]).Name + ', Difficulty: ' + IntToStr(TAttributeModifier(SkillModifiers[StrToInt(TMenuItem(Sender).Hint)]).Difficulty) + ', Av. Gain: ' + IntToStr(TAttributeModifier(SkillModifiers[StrToInt(TMenuItem(Sender).Hint)]).value) + ' - Finishing Time: ' + IntToStr(TAttributeModifier(SkillModifiers[StrToInt(TMenuItem(Sender).Hint)]).Time + IBSystem.RDOGetTime) + ', Cost /h: ' + IntToStr(Round(TAttributeModifier(SkillModifiers[StrToInt(TMenuItem(Sender).Hint)]).Cost / TAttributeModifier(SkillModifiers[StrToInt(TMenuItem(Sender).Hint)]).Time));
                    end;
                end;
              end;
          end;
      end
    else
      Application.MessageBox( PChar('Couldn''t start criminal training.'), PChar('Error'), MB_OK or MB_ICONERROR );
  end;

procedure TForm1.CustomPopUpMenu(CriminalState : string; Index : integer);
  var
    i : integer;
    j : integer;
    k : integer;
    l : integer;
    m : integer;
    n : integer;
    o : integer;
    p : integer;
    q : integer;
    r : integer;
    s : integer;
    t : integer;
    u : integer;
    v : integer;
    x : integer;
    MenuItem     : TMenuItem;
    MenuHistItem : TMenuItem;
    MenuPenRec   : TMenuItem;
    MenuCrimRec  : TMenuItem;
    MenuJail     : TMenuItem;
    HistoryList  : TStringList;
    CrimPenRecord: TStringList;
    CrimPolRecord: TStringList;
    JailDetails  : TStringList;
    already      : boolean;
    foundAll     : boolean;
    found        : boolean;
    PrevReq      : string;
    Team         : TStringList;
  begin
    // Cleaning
    for n := 1 to PopupMenu1.Items.Count do
      for l := (PopUpMenuItems[n].Count) downto 1 do
        begin
          for q := PopUpMenuItems[n].Items[l-1].Count downto 1 do
            PopUpMenuItems[n].Items[l-1].Delete(q-1);
          PopUpMenuItems[n].Delete(l-1);
        end;
    PopUpMenuItems[7].Visible := False;
    TeamsInMenu.DeleteAll;
    // Preparing Change Team Option
    for o := 0 to (cbTeams.Items.Count - 1) do
      begin
        MenuItem := TMenuItem.Create(PopUpMenu1);
        MenuItem.Caption := cbTeams.Items[o];
        TeamsInMenu.Insert(MenuItem);
        MenuItem.OnClick := ChangeTeamClick;
        PopUpMenuItems[3].Add(MenuItem);
      end;
    Team := TStringList.Create;
    for p := 0 to (TeamsInMenu.Count - 1) do
      begin
        Team.Text := IBSystem.RDOFindTeam(widestring(cbLeaders.Text), widestring(TMenuItem(TeamsInMenu[p]).Caption));
        if (TMenuItem(TeamsInMenu[p]).Caption = cbTeams.Text) or (StrToInt(Team.Values['NumCrim']) = 8)
        then
          begin
            TMenuItem(TeamsInMenu[p]).Enabled := False;
            if TMenuItem(TeamsInMenu[p]).Caption <> cbTeams.Text
            then
              TMenuItem(TeamsInMenu[p]).Caption := TMenuItem(TeamsInMenu[p]).Caption + ' (Team Full)';
          end
        else
          begin
            TMenuItem(TeamsInMenu[p]).Enabled := True;
            if Team.Values['State'] = 'InMission'
            then
              begin
                TMenuItem(TeamsInMenu[p]).Enabled := False;
                TMenuItem(TeamsInMenu[p]).Caption := TMenuItem(TeamsInMenu[p]).Caption + ' (In Mission)';
              end
            else
              TMenuItem(TeamsInMenu[p]).Enabled := True;
          end;
      end;
    // Preparing History Option
    HistoryList := TStringList.Create;
    if CriminalState <> 'NoCriminal'
    then
      begin
        PopUpMenuItems[4].Enabled := True;
        HistoryList.Text := IBSystem.RDORecoveryHistoryItem(widestring(''), widestring(''), widestring(TeamLabels[Index].Caption));
        for r := 0 to StrToInt(HistoryList.Values['ItemNumber']) do
          begin
            MenuHistItem := TMenuItem.Create(PopupMenu1);
            MenuHistItem.Caption := 'Date: ' + HistoryList.Values['Item' + IntToStr(r) + '-' + 'Date'] + ' - ' + HistoryList.Values['Item' + IntToStr(r) + '-' + 'Event'];
            PopUpMenuItems[4].Add(MenuHistItem);
          end;
      end;
    // Prepairing Training Option
    if CriminalState = IntToStr(3)
    then
      begin
        for j := 1 to 4 do
          PopUpMenuItems[j].Enabled := True;
        PopUpMenuItems[2].Caption := 'Training';
        PopUpMenuItems[2].OnClick := nil;
        for k := 0 to (skills.Count - 4) do
          begin
            PopUpMenuItems[2].Add(TMenuItem(SkillsInMenu[k]));
            for m := 0 to (SkillModifiers.Count - 1) do
              begin
                if TMenuItem(SkillsInMenu[k]).Caption = TAttributeModifier(SkillModifiers[m]).Attribute.Name
                then
                  begin
                    PrevReq := ' - Prev. Training(s) Req.: ';
                    already := False;
                    foundAll := True;
                    for s := 0 to StrToInt(HistoryList.Values['ItemNumber']) do
                      begin
                        if TAttributeModifier(SkillModifiers[m]).Name = HistoryList.Values['Item' + IntToStr(s) + '-' + 'Param']
                        then
                          already := True;
                      end;
                    for t := 0 to (TAttributeModifier(SkillModifiers[m]).PrevModifier.Count - 1) do
                      begin
                        found := false;
                        for u := 0 to StrToInt(HistoryList.Values['ItemNumber']) do
                          begin
                            if TAttributeModifier(TAttributeModifier(SkillModifiers[m]).PrevModifier[t]).Name = HistoryList.Values['Item' + IntToStr(u) + '-' + 'Param']
                            then
                              found := True;
                          end;
                        if found = False
                        then
                          begin
                            foundAll := False;
                            PrevReq := PrevReq + TAttributeModifier(TAttributeModifier(SkillModifiers[m]).PrevModifier[t]).Name + ' ';
                          end;
                      end;
                    if already = False
                    then
                      begin
                        TMenuItem(SkillsInMenu[k]).Add(TMenuItem(SkillModifiersInMenu[m]));
                        TMenuItem(SkillModifiersInMenu[m]).Caption := (TAttributeModifier(SkillModifiers[m]).Name)  + ' - Cost: ' + FloatToStr(TAttributeModifier(SkillModifiers[m]).Cost) + '$' + ', Time: ' + IntToStr(TAttributeModifier(SkillModifiers[m]).Time) + ', Difficulty: ' + IntToStr(TAttributeModifier(SkillModifiers[m]).Difficulty) + ', Av. Gain: ' + IntToStr(TAttributeModifier(SkillModifiers[m]).value);
                        TMenuItem(SkillModifiersInMenu[m]).Hint := IntToStr(m);
                        if foundAll = False
                        then
                          begin
                            TMenuItem(SkillModifiersInMenu[m]).Enabled := False;
                            TMenuItem(SkillModifiersInMenu[m]).Caption := TMenuItem(SkillModifiersInMenu[m]).Caption + PrevReq;
                          end
                        else
                          if TAttributeModifier(SkillModifiers[m]).Requirement <= StrToInt(TeamSkillLabels[k, Index - 1].Caption)
                          then
                            begin
                              TMenuItem(SkillModifiersInMenu[m]).Enabled := True;
                            end
                          else
                            begin
                              TMenuItem(SkillModifiersInMenu[m]).Enabled := False;
                              TMenuItem(SkillModifiersInMenu[m]).Caption := TMenuItem(SkillModifiersInMenu[m]).Caption + ' - Min. Skill Value Req.: ' + IntToStr(TAttributeModifier(SkillModifiers[m]).Requirement);
                            end;
                      end;
                  end;
              end;
          end;
      end;
    if CriminalState = IntToStr(4)
    then
      begin
        PopUpMenuItems[1].Enabled := False;
        PopUpMenuItems[2].Caption := 'Stop Training';
        PopUpMenuItems[2].OnClick := StopTraining;
        PopUpMenuItems[2].Enabled := True;
        PopUpMenuItems[3].Enabled := False;
      end;
    if CriminalState = IntToStr(5)
    then
      begin
        PopUpMenuItems[1].Enabled := False;
        PopUpMenuItems[2].Caption := 'No Training During Missions';
        PopUpMenuItems[2].OnClick := nil;
        PopUpMenuItems[2].Enabled := False;
        PopUpMenuItems[3].Enabled := False;
      end;
    // Prepare Pending Criminal Record
    if CriminalState <> 'NoCriminal'
    then
      begin
        CrimPenRecord := TStringList.Create;
        CrimPenRecord.Text := IBSystem.RDORecoveryCriminalPendingRecord(widestring(TeamLabels[Index].Caption));
        if CrimPenRecord.Count <> 0
        then
          begin
            PopUpMenuItems[6].Enabled := True;
            PopUpMenuItems[6].Caption := 'Pending Crimes';
            for v := 1 to (CrimPenRecord.Count) do
              if CrimPenRecord.Values['Charge' + IntToStr(v)] <> ''
              then
                begin
                  MenuPenRec := TMenuItem.Create(PopupMenu1);
                  MenuCrimRec.Caption := CrimPenRecord.Values['Charge' + IntToStr(v)] + ' - Mission: ' + CrimPenRecord.Values['Mission'+ IntToStr(v)] + ' - Date: ' + CrimPenRecord.Values['Date'+ IntToStr(v)] + ' - Leader: ' + CrimPenRecord.Values['Leader' + IntToStr(v)] + ' - Team: ' + CrimPenRecord.Values['Team' + IntToStr(v)];
                  PopUpMenuItems[6].Add(MenuPenRec);
                end;
          end
        else
          begin
            PopUpMenuItems[6].Enabled := False;
            PopUpMenuItems[6].Caption := 'No Pending Crimes'
          end;
      end;
    // Prepare Police Criminal Record
    if CriminalState <> 'NoCriminal'
    then
      begin
        CrimPolRecord := TStringList.Create;
        CrimPolRecord.Text := IBSystem.RDORecoveryCriminalPoliceRecord(widestring(TeamLabels[Index].Caption));
        if CrimPolRecord.Count <> 0
        then
          begin
            PopUpMenuItems[5].Enabled := True;
            PopUpMenuItems[5].Caption := 'Criminal Records';
            for x := 0 to (CrimPolRecord.Count - 1) do
              if CrimPolRecord.Values['Charge' + IntToStr(x)] <> ''
              then
                begin
                  MenuCrimRec := TMenuItem.Create(PopupMenu1);
                  MenuCrimRec.Caption := CrimPolRecord.Values['Charge' + IntToStr(x)] + ' - Mission: ' + CrimPolRecord.Values['Mission'+ IntToStr(x)] + ' - Date: ' + CrimPolRecord.Values['Date'+ IntToStr(x)] + ' - Leader: ' + CrimPolRecord.Values['Leader' + IntToStr(x)] + ' - Team: ' + CrimPolRecord.Values['Team' + IntToStr(x)];
                  PopUpMenuItems[5].Add(MenuCrimRec);
                end;
          end
        else
          begin
            PopUpMenuItems[5].Enabled := False;
            PopUpMenuItems[5].Caption := 'No Criminal Records'
          end;
      end;
    // Prepare Jail Details
    if CriminalState <> 'NoCriminal'
    then
      if CriminalState = IntToStr(7)
      then
        begin
          JailDetails := TStringList.Create;
          JailDetails.Text := IBSystem.RDORecoveryCriminalJailDetails(widestring(TeamLabels[Index].Caption));
          if JailDetails.Values['TrialDay'] <> '0'
          then
            begin
              if JailDetails.Values['TrialLength'] = '0'
              then
                begin
                  PopUpMenuItems[7].Visible := True;
                  PopUpMenuItems[7].Enabled := True;
                  PopUpMenuItems[7].Caption := 'Day of Trial: ' + JailDetails.Values['TrialDay'] + ' - Lawyers'' Hours: ' + JailDetails.Values['LawyersHours'];
                  PopUpMenuItems[5].Caption := 'Trial''s Charges';
                end
              else
                begin
                  PopUpMenuItems[7].Visible := True;
                  PopUpMenuItems[7].Enabled := True;
                  PopUpMenuItems[7].Caption := 'Trial Day N. ' + IntToStr(StrToInt(JailDetails.Values['TrialDay']) + StrToInt(JailDetails.Values['TrialEndDay']) - Time) + ' of ' + JailDetails.Values['TrialEndDay'] + ' - Lawyers'' Hours: ' + JailDetails.Values['LawyersHours'];
                end
            end
          else
            begin
              PopUpMenuItems[7].Visible := True;
              PopUpMenuItems[7].Enabled := True;
              PopUpMenuItems[7].Caption := 'Jail Time: ' + JailDetails.Values['JailTime'];
            end;
        end;
    // Disabled Everything if NoCriminal
    if CriminalState = 'NoCriminal'
    then
      for i := 1 to 6 do
          PopUpMenuItems[i].Enabled := False;
  end;

procedure TForm1.btnHeadquarterClick(Sender: TObject);
  begin
    if IBSystem.RDOAssignHeadquarter(widestring(cbLeaders.Text), widestring(cbTeams.Text), StrToInt(eHeadX.Text), StrToInt(eHeadY.Text)) <> 1
    then
      begin
        btnHeadquarter.Enabled := False;
        CheckRolesForMission;
        ObjFieldsChange(Self);
      end
    else
      Application.MessageBox( PChar('Couldn''t assaign headquarter.'), PChar('Error'), MB_OK or MB_ICONERROR );
  end;

procedure TForm1.eHeadXChange(Sender: TObject);
  begin
    if (StrToInt(eHeadX.Text) > 0) and (StrToInt(eHeadY.Text) > 0)
    then
      btnHeadquarter.Enabled := True
    else
      btnHeadquarter.Enabled := False;
  end;

procedure TForm1.eHeadYChange(Sender: TObject);
  begin
    if (StrToInt(eHeadX.Text) > 0) and (StrToInt(eHeadY.Text) > 0)
    then
      btnHeadquarter.Enabled := True
    else
      btnHeadquarter.Enabled := False;
  end;

function CalculateTotalSalary(TeamToCalcName : string): single;
  var
    total : single;
  begin
    total := IBSystem.RDOCalculateSalary(widestring(Form1.cbLeaders.Text), widestring(TeamToCalcName));
    Result := total;
  end;

function CalculateAvarPerc(TeamToCalcName : string): single;
  var
    total : single;
  begin
    total := IBSystem.RDOCalculatePercentage(widestring(Form1.cbLeaders.Text), widestring(TeamToCalcName));
    Result := total;
  end;

function CalculateTotalTrainingCost(TeamToCalcName : string): single;
  var
    total : single;
  begin
    total := IBSystem.RDOCalculateTrainingCost(widestring(Form1.cbLeaders.Text), widestring(TeamToCalcName));
    Result := total;
  end;

procedure CalculateExpenses;
  begin
    form1.lblExpenses.Caption := FloatToStr(Round(StrToFloat(IBSystem.RDOCalculateLeaderExpenses(widestring(Form1.cbLeaders.Text)))));
  end;

procedure FillTeamHistoryItems(TeamName : string);
  var
    i : integer;
    HistoryList : TStringList;
  begin
    HistoryList := TStringList.Create;
    HistoryList.Text := IBSystem.RDORecoveryHistoryItem(widestring(Form1.cbLeaders.Text), widestring(TeamName), widestring(''));
    Form1.cbTeamHistory.Items.Clear;
    for i := 0 to StrToInt(HistoryList.Values['ItemNumber']) do
      if HistoryList.Values['Item' + IntToStr(i) + '-' + 'Mission?'] = 'yes'
      then
        begin
          Form1.cbTeamHistory.Items.Add('Date: ' + HistoryList.Values['Item' + IntToStr(i) + '-' + 'Date'] + ' - Mission:' + HistoryList.Values['Item' + IntToStr(i) + '-' + 'Event']);
        end
      else
        Form1.cbTeamHistory.Items.Add('Date: ' + HistoryList.Values['Item' + IntToStr(i) + '-' + 'Date'] + ' - Mission:' + HistoryList.Values['Item' + IntToStr(i) + '-' + 'Event']);
  end;

procedure FillLeaderHistoryItems(LeaderName : string);
  var
    i : integer;
    HistoryList : TStringList;
  begin
    HistoryList := TStringList.Create;
    HistoryList.Text := IBSystem.RDORecoveryHistoryItem(widestring(LeaderName), widestring(''), widestring(''));
    Form1.cbLeaderHistory.Items.Clear;
    for i := 0 to StrToInt(HistoryList.Values['ItemNumber']) do
      Form1.cbLeaderHistory.Items.Add('Date: ' + HistoryList.Values['Item' + IntToStr(i) + '-' + 'Date'] + ' - ' + HistoryList.Values['Item' + IntToStr(i) + '-' + 'Event']);
  end;

procedure TForm1.TrackBar1Change(Sender: TObject);
  var
    i : integer;
    j : integer;
    crimList : TStringList;
  begin
    crimList := TStringList.Create;
    for j := 1 to 8 do
      if CriminalPanels[j].Color = clWhite
      then
        begin
          crimList.Text := IBSystem.RDOFindCriminal(widestring(CriminalLabels[j].Caption));
        end;
    lblSalary.Caption := FloatToStr(Round((TrackBar1.Position * StrToFloat(crimList.Values['Salary']) / 100)));
    for i := 0 to (Skills.Count - 1) do
      if TMetaAttribute(Skills[i]).Name = 'Loyalty'
      then
        lblFactor1.Caption := FloatToStr(Round(StrToFloat(crimList.Values[IntToStr(i)]) + Round((TrackBar1.Position - 100)/10)));
  TrackBar1.Hint := IntToStr(TrackBar1.Position);
  TrackBar1.ShowHint := True;
  end;

procedure TForm1.SalaryChange(Sender: TObject);
  var
    i : integer;
    j : integer;
    SenderIndex : integer;
    crimList : TStringList;
  begin
    SenderIndex := 0;
    for j := 1 to 8 do
      begin
        if TTrackBar(Sender).Name = SalaryBars[j].Name
        then
          SenderIndex := j;
      end;
    if IBSystem.RDOChangeCriminalSalary(widestring(cbLeaders.Text), widestring(TeamLabels[SenderIndex].Caption), SalaryBars[SenderIndex].Position) = 1
    then
      Application.MessageBox( PChar('Couldn''t change criminal salary.'), PChar('Error'), MB_OK or MB_ICONERROR );
    crimList := TStringList.Create;
    crimList.Text := IBSystem.RDOFindCriminal(widestring(TeamLabels[SenderIndex].Caption));
    Salaries[SenderIndex].Caption := IntToStr(Round(StrToFloat(crimList.Values['Salary'])));
    for i := 0 to (Skills.Count - 1) do
      if TMetaAttribute(Skills[i]).Name = 'Loyalty'
      then
        TeamSkillLabels[i, SenderIndex - 1].Caption := FloatToStr(Round(StrToFloat(crimList.Values[IntToStr(i)])));
    lblTotalSalary.Caption := FloatToStr(Round(CalculateTotalSalary(cbTeams.Text)));
    lblAvarPerc.Caption := FloatToStr(Round(CalculateAvarPerc(cbTeams.Text)));
    CalculateExpenses;
    SalaryBars[SenderIndex].Hint := IntToStr(SalaryBars[SenderIndex].Position);
    SalaryBars[SenderIndex].ShowHint := True;
  end;

procedure TForm1.cbLeaderHistoryChange(Sender: TObject);
  begin
    cbLeaderHistory.Hint := cbLeaderHistory.Text;
  end;

procedure CleanMissionSheet;
  var
    i : integer;
    j : integer;
    m : integer;
    prevRole : string;
  begin
    for m := 1 to 4 do
      begin
        MissionOptions[m].Caption := '';
        MissionOptions[m].Visible := False;
        MissionOptions[m].Checked := False;
        MissionWays[m].Clear;
        MissionWays[m].Visible := False;
        MissionOptions[m].Enabled := True;
        MissionWays[m].Enabled := True;
      end;
    Form1.mMissionDescr.Clear;
    Form1.GroupBox7.Caption := 'Choose Mission';
    Form1.eObjX.Text := '0';
    Form1.eObjY.Text := '0';
    Form1.lblObjective.Caption := '';
    Form1.btnObjective.Enabled := False;
    Form1.btnAcceptMission.Visible := True;
    Form1.btnAcceptMission.Enabled := False;
    Form1.btnResearch.visible := False;
    Form1.btnStartMission.Visible := False;
    Form1.btnCancelMission.Visible := False;
    for i := 1 to 8 do
      begin
        prevRole := TeamRolesCombos[i].Text;
        TeamRolesCombos[i].Clear;
        for j := 0 to (TeamRoles.Count - 1) do
          begin
            TeamRolesCombos[i].Items.Add(TMetarole(TeamRoles[j]).Name);
            if TeamRolesCombos[i].Items.Strings[j] = prevRole
            then
              TeamRolesCombos[i].ItemIndex := TeamRolesCombos[i].Items.IndexOf(prevRole);
          end;
      end;

  end;

procedure CleanMissionGrid;
  var
    j : integer;
    k : integer;
  begin
    Form1.lblRoleDescription.Caption := '';
    Form1.MissionReqGrid.RowCount := MaxRoles + 1;
    for j := 0 to (Form1.MissionReqGrid.RowCount - 1) do
      for k:= 0 to (Form1.MissionReqGrid.ColCount - 1) do
        Form1.MissionReqGrid.Cells[k, j] := '';
    Form1.lblMissionCost.Caption := '';
    Form1.lblMissionProfit.Caption := '';
  end;

procedure TForm1.cbMissionsChange(Sender: TObject);
  var
    i : integer;
    j : integer;
    k : integer;
    l : integer;
    already : boolean;
    lastOpt : integer;
    Info : TStringList;
    OptionInfos : TStringList;
    OptionToStore : TMissionOptionInfo;
  begin
    CleanMissionSheet;
    CleanMissionGrid;
    if cbMissions.Text <> ''
    then
      begin
        Info := TStringList.Create;
        Info.Text := IBSystem.RDORecoveryMissionInfos(widestring(cbMissions.Text));
        mMissionDescr.Lines.Add(Info.Values['descr']);
        mMissionDescr.Lines.Delete(1);
        GroupBox7.Caption := 'Mission: ' + Info.Values['Id'] + ' - Type: ' + Info.Values['MissionType'];
        btnObjective.Enabled := True;
        lastOpt := 0;
        for i := 0 to (MaxOptions - 1) do
          begin
            OptionInfos := TStringList.Create;
            OptionInfos.Text := IBSystem.RDORecoveryMissionOption(widestring(cbMissions.Text), i);
            if i = 0
            then
              lblObjective.Caption := OptionInfos.Values['Objective'];
            if OptionInfos.Values['Id'] <> ''
            then
              begin
                OptionToStore.Id    := OptionInfos.Values['Id'];
                OptionToStore.way   := OptionInfos.Values['Way'];
                for j := 0 to (MaxRoles - 1) do
                  begin
                    OptionToStore.Roles[j] := OptionInfos.Values['Role' + IntToStr(j)];
                    OptionToStore.SkillValues[j] := StrToFloat(OptionInfos.Values['SkillValue' + IntToStr(j)]);
                    OptionToStore.Descriptions[j] := OptionInfos.Values['Desc' + IntToStr(j)];
                    OptionToStore.Skills[j] := OptionInfos.Values['Skill' + IntToStr(j)];
                  end;
                if Info.Values['Sub'] = 'yes'
                then
                  OptionToStore.Sub := True
                else
                  OptionToStore.Sub := False;
                if Info.Values['Compulsory'] = 'yes'
                then
                  OptionToStore.compulsory := True
                else
                  OptionToStore.compulsory := False;
                OptionToStore.ParentOptionId := OptionInfos.Values['ParentOptionId'];
                OptionToStore.Objective := OptionInfos.Values['Objective'];
                OptionToStore.Target := TTargetType(StrToInt(OptionInfos.Values['Target']));
                OptionToStore.MaxDistance := StrToInt(OptionInfos.Values['MaxDistance']);
                OptionToStore.MaxRadius := StrToInt(OptionInfos.Values['MaxRadius']);
                OptionToStore.ObjCoord.X := StrToInt(OptionInfos.Values['ObjCoorX']);
                OptionToStore.ObjCoord.Y := StrToInt(OptionInfos.Values['ObjCoorY']);
                OptionToStore.Duration := StrToInt(OptionInfos.Values['Duration']);
                OptionToStore.Cost := StrToFloat(OptionInfos.Values['Cost']);
                OptionToStore.Profit := StrToFloat(OptionInfos.Values['Profit']);
                MisStoredOptions[i] := OptionToStore;
                if OptionInfos.Values['Id'] <> 'NoOption'
                then
                  begin
                    if OptionInfos.Values['Sub'] = 'no'
                    then
                      begin
                        already := False;
                        for k := 0 to 4 do
                          if MissionOptions[k].Caption = OptionInfos.Values['Id']
                          then
                            already := True;
                        if already = False
                        then
                          begin
                            lastOpt := lastOpt + 1;
                            MissionOptions[lastOpt].Caption := OptionInfos.Values['Id'];
                            MissionOptions[lastOpt].Visible := True;
                            if OptionInfos.Values['Compulsory'] = 'yes'
                            then
                              begin
                                MissionOptions[lastOpt].Checked := True;
                                MissionWays[lastOpt].Visible := True;
                                MissionOptions[lastOpt].Enabled := False;
                              end
                            else
                              begin
                                MissionOptions[lastOpt].Checked := False;
                                MissionOptions[lastOpt].Enabled := True;
                              end;
                            if OptionInfos.Values['Way'] <> ''
                            then
                              begin
                                MissionWays[lastOpt].Items.Add(OptionInfos.Values['Way']);
                                MissionWays[lastOpt].ItemIndex := 0;
                              end;
                          end
                        else
                          begin
                            for l := 0 to 4 do
                              if MissionOptions[l].Caption = OptionInfos.Values['Id']
                              then
                                MissionWays[lastOpt].Items.Add(OptionInfos.Values['Way']);
                          end;
                      end;
                  end;
              end;
          end;
        FillMissionGrid;
    end;
  end;

procedure FillMissionGrid;
  var
    i : integer;
    l : integer;
    m : integer;
    n : integer;
    o : integer;
    p : integer;
    q : integer;
    r : integer;
    s : integer;
    u : integer;
    totalRows : integer;
    ok : boolean;
    ok2 : boolean;
    PrevRoles : array[1..8] of string;
    backupRolePos : integer;
    metaRole : string;
  begin
    totalRows := 0;
    CleanMissionGrid;
    Form1.MissionReqGrid.Cells[0, 0] := 'Roles';
    Form1.MissionReqGrid.Cells[1, 0] := 'Skill';
    Form1.MissionReqGrid.Cells[2, 0] := 'Difficulty';
    if Form1.cbMissions.Text <> ''
    then
      begin
        FillGridWithOption('NoOption', '');
        for i := 1 to 4 do
          if MissionOptions[i].Caption <> ''
          then
            if MissionOptions[i].Checked = True
            then
              FillGridWithOption(MissionOptions[i].Caption, MissionWays[i].Text);
        for l := 0 to (Form1.MissionReqGrid.RowCount - 1) do
          if Form1.MissionReqGrid.Cells[0, l] <> ''
          then
            totalRows := l + 1;
        if totalRows = 1
        then
          Form1.MissionReqGrid.RowCount := 2
        else
          Form1.MissionReqGrid.RowCount := totalRows;
        for p := 1 to 8 do
          begin
            PrevRoles[p] := TeamRolesCombos[p].Text;
            TeamRolesCombos[p].Clear;
            for q:= 0 to (TeamRoles.Count - 1) do
              TeamRolesCombos[p].Items.Add(TMetaRole(TeamRoles[q]).Name);
          end;
        for m := 1 to TotalRows do
          begin
            ok := False;
            backupRolePos := -2;
            for n := 0 to (TeamRolesCombos[1].Items.Count - 1) do
              begin
                if Form1.MissionReqGrid.Cells[0, m] = TeamRolesCombos[1].Items[n]
                  then
                    begin
                      ok := True;
                      backupRolePos := n;
                    end;
              end;
            if ok = True
            then
              if TeamRolesCombos[1].Items[backupRolePos] <> ''
              then
                for o := 1 to 8 do
                  begin
                    TeamRolesCombos[o].Items.Insert((backupRolePos + 1), Form1.MissionReqGrid.Cells[0, m] + '(Backup)');
                  end;
            if ok = False
            then
              begin
                for u := 1 to (TeamRoles.Count - 1) do
                  if StrLComp(PChar(Form1.MissionReqGrid.Cells[0, m]), PChar(TMetaRole(TeamRoles[u]).Name), StrLen(PChar(TMetaRole(TeamRoles[u]).Name))) = 0
                  then
                    metaRole := TMetaRole(TeamRoles[u]).Name;
                for s := 1 to (TeamRolesCombos[1].Items.Count - 1) do
                  if StrLComp(PChar(TeamRolesCombos[1].Items[s]), PChar(metaRole), StrLen(PChar(metaRole))) = 0
                  then
                    backupRolePos := s;
                for o := 1 to 8 do
                  begin
                    TeamRolesCombos[o].Items.Insert((backupRolePos), Form1.MissionReqGrid.Cells[0, m]);
                    if TeamRolesCombos[o].Items[backupRolePos + 1] = metaRole
                    then
                      TeamRolesCombos[o].Items[backupRolePos + 1] := TeamRolesCombos[o].Items[backupRolePos + 1] + '(Backup)';
                  end;
              end;
          end;
        for r := 1 to 8 do
          begin
            ok2 := False;
            for s := 0 to (TeamRolesCombos[r].Items.Count - 1) do
              if TeamRolesCombos[r].Items[s] = PrevRoles[r]
              then
                ok2 := True;
            if ok2 = True
            then
              TeamRolesCombos[r].ItemIndex := TeamRolesCombos[r].Items.IndexOf(PrevRoles[r])
            else
              begin
                if IBSystem.RDOChangeRole(widestring(Form1.cbLeaders.Text), widestring(Form1.cbTeams.Text), widestring(TeamLabels[r].Caption), widestring('')) = 1
                then
                  Application.MessageBox( PChar('Couldn''t change criminal role.'), PChar('Error'), MB_OK or MB_ICONERROR );
                TeamRolesCombos[r].ItemIndex := 0;
              end;
          end;
        CheckRolesForMission;
    end;
  end;

procedure CheckRolesForMission;
  var
    m : integer;
    n : integer;
    ok : boolean;
    ok2 : boolean;
  begin
    ok2 := True;
    if Form1.cbMissions.Text <> ''
    then
      begin
        for m := 1 to (Form1.MissionReqGrid.RowCount - 1) do
          begin
            ok := false;
            for n := 1 to 8 do
              //if TeamRolesCombos[n].Text <> ''
              //then
                if Form1.MissionReqGrid.Cells[0, m] = TeamRolesCombos[n].Text
                then
                  begin
                    if ok = true
                    then
                      ok := False
                    else
                      ok := true;
                  end;
            if ok <> True
            then
              ok2 := false
          end;
      end;
    if ok2 = True
    then
      begin
        if (StrToInt(Form1.eObjY.Text) > 0) and (StrToInt(Form1.eObjY.Text) > 0)
        then
          if (StrToInt(Form1.eHeadX.Text) > 0) and (StrToInt(Form1.eHeadY.Text) > 0)
          then
            Form1.btnAcceptMission.Enabled := True;
      end
    else
      Form1.btnAcceptMission.Enabled := False;
  end;

procedure FillGridWithOption(OptionName : string; WayName : string);
  var
    i : integer;
    j : integer;
    k : integer;
    l : integer;
    m : integer;
    n : integer;
    o : integer;
    lastRole : integer;
    sameLine : integer;
    strToBuild : string;
  begin
    lastRole := -1;
    l := 0;
    while lastRole = -1 do
      begin
        l := l + 1;
        if Form1.MissionReqGrid.Cells[0, l] = ''
        then
          lastRole := l - 1;
      end;
    for i := 0 to (MaxOptions - 1) do
      if ((MisStoredOptions[i].Id = OptionName) and (MisStoredOptions[i].way = WayName))
        then
          begin
            if Form1.lblMissionCost.Caption = ''
            then
              begin
                Form1.lblMissionCost.Caption := FloatToStr(MisStoredOptions[i].Cost);
                Form1.lblMissionProfit.Caption := FloatToStr(MisStoredOptions[i].Profit);
                Form1.lblMissionDuration.Caption := IntToStr(MisStoredOptions[i].Duration);
              end
            else
              begin
                Form1.lblMissionCost.Caption := FloatToStr(Round(StrToFloat(Form1.lblMissionCost.Caption) + MisStoredOptions[i].Cost));
                Form1.lblMissionProfit.Caption := FloatToStr(Round(StrToFloat(Form1.lblMissionProfit.Caption) + MisStoredOptions[i].Profit));
                Form1.lblMissionDuration.Caption := IntToStr(StrToInt(Form1.lblMissionDuration.Caption) + MisStoredOptions[i].Duration);
              end;
            for j := 0 to (MaxRoles - 1) do
             if MisStoredOptions[i].Roles[j] <> ''
             then
               begin
                 sameLine := 0;
                 for k := 1 to (lastRole + 1) do
                   if ((Form1.MissionReqGrid.Cells[0, k] = MisStoredOptions[i].Roles[j]) and (Form1.MissionReqGrid.Cells[1, k] = MisStoredOptions[i].Skills[j]))
                   then
                     begin
                       sameLine := k;
                     end;
                 if sameLine <> 0
                 then
                   begin
                     strToBuild := '';
                     for m := 0 to 4 do
                       if m = 0
                       then
                         for n := 0 to (MaxRoles - 1) do
                           begin
                             if ((MisStoredOptions[m].Roles[n] = MisStoredOptions[i].Roles[j]) and (MisStoredOptions[m].Skills[n] = MisStoredOptions[i].Skills[j]))
                             then
                               if n = j
                               then
                                 if strToBuild = ''
                                 then
                                   strToBuild := FloatToStr(MisStoredOptions[i].SkillValues[j])
                                 else
                                   strToBuild := strToBuild + ', ' + FloatToStr(MisStoredOptions[i].SkillValues[j])
                                 //Form1.MissionReqGrid.Cells[2, sameLine] := Form1.MissionReqGrid.Cells[2, sameLine] + ', ' + FloatToStr(MisStoredOptions[i].SkillValues[j])
                               else
                                 if strToBuild = ''
                                 then
                                   strToBuild := FloatToStr(MisStoredOptions[m].SkillValues[n])
                                 else
                                   strToBuild := strToBuild + ', ' + FloatToStr(MisStoredOptions[m].SkillValues[n]);
                           end
                       else
                         if MissionOptions[m].checked
                         then
                           if MissionOptions[m].Caption <> OptionName
                           then
                             begin
                               for o := 0 to (MaxOptions - 1) do
                                 if ((MisStoredOptions[o].Id = OptionName) and (MisStoredOptions[o].way = WayName))
                                   then
                                     for n := 0 to (MaxRoles - 1) do
                                       if ((MisStoredOptions[o].Roles[n] = MisStoredOptions[i].Roles[j]) and (MisStoredOptions[o].Skills[n] = MisStoredOptions[i].Skills[j]))
                                       then
                                         if n = j
                                         then
                                           if strToBuild = ''
                                           then
                                             strToBuild := FloatToStr(MisStoredOptions[i].SkillValues[j])
                                           else
                                             strToBuild := strToBuild + ', ' + FloatToStr(MisStoredOptions[i].SkillValues[j])
                                           //Form1.MissionReqGrid.Cells[2, sameLine] := Form1.MissionReqGrid.Cells[2, sameLine] + ', ' + FloatToStr(MisStoredOptions[i].SkillValues[j])
                                         else
                                           if strToBuild = ''
                                           then
                                             strToBuild := FloatToStr(MisStoredOptions[o].SkillValues[n])
                                           else
                                             strToBuild := strToBuild + ', ' + FloatToStr(MisStoredOptions[o].SkillValues[n]);
                             end;
                               //Form1.MissionReqGrid.Cells[2, sameLine] := Form1.MissionReqGrid.Cells[2, sameLine] + ', ' + FloatToStr(MisStoredOptions[m].SkillValues[n]);
                     Form1.MissionReqGrid.Cells[2, sameLine] := strToBuild;
                   end
                 else
                   begin
                     lastRole := lastRole + 1;
                     Form1.MissionReqGrid.Cells[0, lastRole] := MisStoredOptions[i].Roles[j];
                     Form1.MissionReqGrid.Cells[1, lastRole] := MisStoredOptions[i].Skills[j];
                     Form1.MissionReqGrid.Cells[2, lastRole] := FloatToStr(MisStoredOptions[i].SkillValues[j]);
                   end;
               end;
          end;
  end;

procedure TForm1.MissionOptionClicked(Sender: TObject);
  var
    j : integer;
    SenderIndex : integer;
  begin
    if readyToShowMission = True
    then
      begin
        SenderIndex := 0;
        for j := 1 to 4 do
          begin
            if TWinControl(Sender).Name = MissionOptions[j].Name
            then
              SenderIndex := j;
          end;
        if MissionWays[SenderIndex].Items.Count <> 0
        then
          MissionWays[SenderIndex].Visible := MissionOptions[SenderIndex].Checked;
        FillMissionGrid;
      end;
  end;

procedure TForm1.MissionWaysChanged(Sender: TObject);
  begin
    FillMissionGrid;
  end;

procedure TForm1.MissionReqGridSelectCell(Sender: TObject; Col,
  Row: Integer; var CanSelect: Boolean);
  var
    i : integer;
    j : integer;
    k : integer;
    RoleName : string;
  begin
    RoleName := MissionReqGrid.Cells[Col, Row];
    for i := 0 to 4 do
      if i = 0
      then
        begin
          for j := 0 to (MaxRoles - 1) do
            if MisStoredOptions[i].Roles[j] = RoleName
            then
              lblRoleDescription.Caption := MisStoredOptions[i].Descriptions[j]
        end
      else
        for k := 1 to (MaxOptions - 1) do
          if MissionOptions[i].Checked = True
          then
            if ((MisStoredOptions[k].Id = MissionOptions[i].Caption) and (MisStoredOptions[k].way = MissionWays[i].Text))
            then
              for j := 0 to (MaxRoles - 1) do
                if MisStoredOptions[k].Roles[j] = RoleName
                then
                  if MisStoredOptions[k].Descriptions[j] <> ''
                  then
                    lblRoleDescription.Caption := MisStoredOptions[k].Descriptions[j]
  end;

procedure TForm1.btnAcceptMissionClick(Sender: TObject);
  var
    i : integer;
    j : integer;
    k : integer;
    RolesToPass : TStringList;
    Parameters : TStringList;
  begin
    RolesToPass := TStringList.Create;
    k := 0;
    for i := 1 to 8 do
      begin
        if TeamRolesCombos[i].Text <> ''
        then
          begin
            k := k + 1;
            RolesToPass.Values['Name' + IntToStr(k)] := TeamLabels[i].Caption;
            RolesToPass.Values['Role' + IntToStr(k)] := TeamRolesCombos[i].Text;
            TeamRolesCombos[i].Enabled := False;
            StatusImages[i].Picture.LoadFromFile('C:\five\source\illegal\UtilityPictures\Mission.bmp');
          end;
      end;
    Parameters := TStringList.Create;
    for j := 1 to (MaxOptions - 1) do
      begin
        if MissionOptions[j].Checked = True
        then
          if MissionWays[j].Visible = True
          then
            begin
              Parameters.Values['Name' + IntToStr(j)] := MissionOptions[j].Caption;
              Parameters.Values['Value' + IntToStr(j)] := 'True';
              Parameters.Values['Way' + IntToStr(j)] := MissionWays[j].Text;
            end
          else
            begin
              Parameters.Values['Name' + IntToStr(j)] := MissionOptions[j].Caption;
              Parameters.Values['Value' + IntToStr(j)] := 'True';
              Parameters.Values['Way' + IntToStr(j)] := '';
            end
        else
          begin
            Parameters.Values['Name' + IntToStr(j)] := MissionOptions[j].Caption;
            Parameters.Values['Value' + IntToStr(j)] := 'False';
            Parameters.Values['Way' + IntToStr(j)] := '';
          end;
        MissionOptions[j].Enabled := False;
        MissionWays[j].Enabled := False;
      end;
    if cbObjTarget.Text <> ''
    then
      begin
        if IBSystem.RDOAssignMission(widestring(edLeader.Text), widestring(cbTeams.Text), widestring(cbMissions.Text), widestring(RolesToPass.Text), widestring(Parameters.Text), widestring(lblObjective.Caption), StrToInt(eObjX.Text), StrToInt(eObjY.Text), MisStoredOptions[0].Target, widestring(cbObjTarget.Text), StrToInt(cbObjRadius.Text), widestring(cbObjPlayer.Text), widestring(cbObjFacilities.Text), widestring(lblMissionCost.Caption)) = 1
        then
          begin
            Application.MessageBox( PChar('Couldn''t assign mission.'), PChar('Error'), MB_OK or MB_ICONERROR );
          end;
      end
    else
      if IBSystem.RDOAssignMission(widestring(edLeader.Text), widestring(cbTeams.Text), widestring(cbMissions.Text), widestring(RolesToPass.Text), widestring(Parameters.Text), widestring(lblObjective.Caption), StrToInt(eObjX.Text), StrToInt(eObjY.Text), integer(MisStoredOptions[0].Target), widestring(cbObjTarget.Text), 0, widestring(cbObjPlayer.Text), widestring(cbObjFacilities.Text), widestring(lblMissionCost.Caption)) = 1
      then
        Application.MessageBox( PChar('Couldn''t assign mission.'), PChar('Error'), MB_OK or MB_ICONERROR );
    FillTeamHistoryItems(cbTeams.Text);
    FillLeaderHistoryItems(edLeader.Text);
    cbMissions.Enabled := False;
    btnAcceptMission.Visible := False;
    btnResearch.visible := True;
    btnStartMission.Visible := True;
    btnCancelMission.Visible := True;
    btnObjective.Enabled := False;
    btnDismissTeam.Enabled := True;
  end;

procedure TForm1.btnObjCancelClick(Sender: TObject);
  begin
    GroupBox8.Visible := False;
    mMissionDescr.Visible := True;
    Label244.Visible := True;
    btnObjective.Enabled := True;
  end;

procedure TForm1.btnAssignObjClick(Sender: TObject);
  begin
    if eObjX.Text <> ''
    then
      begin
        cbObjX.Items.Add(eObjX.Text);
        cbObjX.Text := eObjX.Text;
      end
    else
      eObjX.Text := cbObjX.Text;
    if eObjY.Text <> ''
    then
      begin
        cbObjY.Items.Add(eObjY.Text);
        cbObjY.Text := eObjY.Text;
      end
    else
      eObjY.Text := cbObjY.Text;
    if MisStoredOptions[0].Target = ttpCoordinates
    then
      begin
        lblObjective.Caption := 'Point of the map: ' + eObjX.Text + ', ' + eObjy.Text;
      end;
    if MisStoredOptions[0].Target = ttpRoad
    then
      begin
        lblObjective.Caption := 'Road: ' + eObjX.Text + ', ' + eObjy.Text;
      end;
    if MisStoredOptions[0].Target = ttpCriminal
    then
      begin
        if cbObjPlayer.Text = ''
        then
          lblObjective.Caption := 'Criminal:' + cbObjTarget.Text
        else
          lblObjective.Caption := 'Criminal:' + cbObjTarget.Text + ' minion of ' + cbObjPlayer.Text;
      end;
    if MisStoredOptions[0].Target = ttpTeam
    then
      begin
        lblObjective.Caption := cbObjTarget.Text + ' team of ' + cbObjPlayer.Text;
      end;
    if MisStoredOptions[0].Target = ttpPlayer
    then
      begin
        lblObjective.Caption := 'Player: ' + cbObjPlayer.Text;
      end;
    if MisStoredOptions[0].Target = ttpLeader
    then
      begin
        lblObjective.Caption := 'Boss: ' + cbObjPlayer.Text;
      end;
    if MisStoredOptions[0].Target = ttpCity
    then
      begin
        lblObjective.Caption := 'City:' + cbObjTarget.Text;
      end;
    if MisStoredOptions[0].Target = ttpFacility
    then
      begin
        lblObjective.Caption := MisStoredOptions[0].Objective + '- Pos.: ' +  eObjX.Text + ', ' + eObjy.Text;
      end;
    if MisStoredOptions[0].Target = ttpCompany
    then
      begin
        lblObjective.Caption := cbObjTarget.Text + ' company of ' + cbObjPlayer.Text;
      end;
    if MisStoredOptions[0].Target = ttpFacilitiesFromPlayer
    then
      begin
        lblObjective.Caption := 'All the ' + cbObjFacilities.Text + 's of ' + cbObjPlayer.Text;
      end;
    if MisStoredOptions[0].Target = ttpFacilitiesFromPlayerInRadius
    then
      begin
        lblObjective.Caption := 'All the ' + cbObjFacilities.Text + 's of ' + cbObjPlayer.Text + ' in a radius of ' + cbObjRadius.Text;
      end;
    if MisStoredOptions[0].Target = ttpFacilitiesFromCompanyInRadius
    then
      begin
        lblObjective.Caption := 'All the ' + cbObjFacilities.Text + 's of the company ' + cbObjTarget.Text + ' of ' + cbObjPlayer.Text + ' in a radius of ' + cbObjRadius.Text;
      end;
    if MisStoredOptions[0].Target = ttpFacilitiesFromCompany
    then
      begin
        lblObjective.Caption := 'All the ' + cbObjFacilities.Text + 's of the company ' + cbObjTarget.Text + ' of ' + cbObjPlayer.Text;
      end;
    if MisStoredOptions[0].Target = ttpFacilitiesInRadius
    then
      begin
        lblObjective.Caption := 'All the ' + cbObjFacilities.Text + 's in a radius of ' + cbObjRadius.Text;
      end;
    GroupBox8.Visible := False;
    mMissionDescr.Visible := True;
    Label244.Visible := True;
    btnObjective.Enabled := True;
    cbMissions.Enabled := True;
    CheckRolesForMission;
  end;

procedure TForm1.ObjFieldsChange(Sender: TObject);
  var
    i : integer;
    OK : boolean;
    Back : olevariant;
    List : TStringList;
  begin
    OK := True;
    if eHeadX.Text < '0'
    then
      OK := False;
    if eHeadY.Text < '0'
    then
      OK := False;
    if MisStoredOptions[0].Target = ttpCoordinates
    then
      begin
        if MisStoredOptions[0].MaxDistance <> 0
        then
          begin
            if cbObjX.Text = ''
            then
              OK := False;
            if cbObjY.Text = ''
            then
              OK := False;
          end
        else
          begin
            if (StrToInt(eObjX.Text) <= 0)
            then
              OK := False;
            if (StrToInt(eObjX.Text) <= 0)
            then
              OK := False;
          end;
      end;
    if MisStoredOptions[0].Target = ttpRoad
    then
      begin
        if MisStoredOptions[0].MaxDistance <> 0
        then
          begin
            if cbObjX.Text = ''
            then
              OK := False;
            if cbObjY.Text = ''
            then
              OK := False;
          end
        else
          begin
            if (StrToInt(eObjX.Text) <= 0)
            then
              OK := False;
            if (StrToInt(eObjX.Text) <= 0)
            then
              OK := False;
          end;
      end;
    if MisStoredOptions[0].Target = ttpCriminal
    then
      begin
        if Sender = cbObjPlayer
        then
          ListCriminalByLeader;
        if cbObjTarget.Text = ''
        then
          OK := False;
      end;
    if MisStoredOptions[0].Target = ttpTeam
    then
      begin
        if Sender = cbObjPlayer
        then
          begin
            List := TStringList.Create;
            Back := IBSystem.RDOGetTeamListByLeaderList;
            if varType(Back) = varInteger
              then
                Application.MessageBox( PChar('Unknown Error'), PChar('Error'), MB_OK or MB_ICONERROR )
              else
                begin
                  List.Text := Back.Text;
                  cbObjTarget.Clear;
                  for i := 1 to List.Count do
                    cbObjTarget.Items.Add(List.Values[IntToStr(i)]);
                end;
          end;
        if cbObjTarget.Text = ''
        then
          OK := False;
        if cbObjPlayer.Text = ''
        then
          OK := False;
      end;
    if MisStoredOptions[0].Target = ttpPlayer
    then
      begin
        if cbObjPlayer.Text = ''
        then
          OK := False;
      end;
    if MisStoredOptions[0].Target = ttpLeader
    then
      begin
        if cbObjPlayer.Text = ''
        then
          OK := False;
      end;
    if MisStoredOptions[0].Target = ttpCity
    then
      begin
        if cbObjTarget.Text = ''
        then
          OK := False;
      end;
    if MisStoredOptions[0].Target = ttpFacility
    then
      begin
        if MisStoredOptions[0].MaxDistance <> 0
        then
          begin
            if cbObjX.Text = ''
            then
              OK := False;
            if cbObjY.Text = ''
            then
              OK := False;
          end
        else
          begin
            if (StrToInt(eObjX.Text) <= 0)
            then
              OK := False;
            if (StrToInt(eObjX.Text) <= 0)
            then
              OK := False;
          end;
      end;
    if MisStoredOptions[0].Target = ttpCompany
    then
      begin
        if Sender = cbObjPlayer
        then
          begin
            List := TStringList.Create;
            Back := IBSystem.RDOGetCompanyList(cbObjPlayer.Text);
            if varType(Back) = varInteger
              then
                Application.MessageBox( PChar('Unknown Error'), PChar('Error'), MB_OK or MB_ICONERROR )
              else
                begin
                  List.Text := Back;
                  cbObjTarget.Clear;
                  for i := 1 to List.Count do
                    cbObjTarget.Items.Add(List.Values[IntToStr(i)]);
                end;
          end;
        if cbObjTarget.Text = ''
        then
          OK := False;
        if cbObjPlayer.Text = ''
        then
          OK := False;
      end;
    if MisStoredOptions[0].Target = ttpFacilitiesFromPlayer
    then
      begin
        if Sender = cbObjPlayer
        then
          begin
            List := TStringList.Create;
            Back := IBSystem.RDOGetFacilityList(cbObjPlayer.Text);
            if varType(Back) = varInteger
              then
                Application.MessageBox( PChar('Unknown Error'), PChar('Error'), MB_OK or MB_ICONERROR )
              else
                begin
                  List.Text := Back;
                  cbObjFacilities.Clear;
                  for i := 1 to List.Count do
                    cbObjFacilities.Items.Add(List.Values[IntToStr(i)]);
                end;
          end;
        if cbObjFacilities.Text = ''
        then
          OK := False;
        if cbObjPlayer.Text = ''
        then
          OK := False;
      end;
    if MisStoredOptions[0].Target = ttpFacilitiesFromPlayerInRadius
    then
      begin
        if Sender = cbObjPlayer
        then
          begin
            List := TStringList.Create;
            Back := IBSystem.RDOGetFacilityList(cbObjPlayer.Text);
            if varType(Back) = varInteger
              then
                Application.MessageBox( PChar('Unknown Error'), PChar('Error'), MB_OK or MB_ICONERROR )
              else
                begin
                  List.Text := Back;
                  cbObjFacilities.Clear;
                  for i := 1 to List.Count do
                    cbObjFacilities.Items.Add(List.Values[IntToStr(i)]);
                end;
          end;
        if cbObjFacilities.Text = ''
        then
          OK := False;
        if cbObjPlayer.Text = ''
        then
          OK := False;
        if cbObjRadius.Text = ''
        then
          OK := False;
      end;
    if MisStoredOptions[0].Target = ttpFacilitiesFromCompany
    then
      begin
        List := TStringList.Create;
        if Sender = cbObjPlayer
        then
          begin
            List.Clear;
            Back := IBSystem.RDOGetCompanyList(cbObjPlayer.Text);
            if varType(Back) = varInteger
              then
                Application.MessageBox( PChar('Unknown Error'), PChar('Error'), MB_OK or MB_ICONERROR )
              else
                begin
                  List := TStringList.Create;
                  List.Text := Back;
                  cbObjTarget.Clear;
                  for i := 1 to List.Count do
                    cbObjTarget.Items.Add(List.Values[IntToStr(i)]);
                end;
          end;
        if Sender = cbObjTarget
        then
          begin
            List.Clear;
            Back := IBSystem.RDOGetFacilityList(cbObjPlayer.Text);
            if varType(Back) = varInteger
              then
                Application.MessageBox( PChar('Unknown Error'), PChar('Error'), MB_OK or MB_ICONERROR )
              else
                begin
                  List := TStringList.Create;
                  List.Text := Back;
                  cbObjFacilities.Clear;
                  for i := 1 to List.Count do
                    cbObjFacilities.Items.Add(List.Values[IntToStr(i)]);
                end;
          end;
        if cbObjTarget.Text = ''
        then
          OK := False;
        if cbObjFacilities.Text = ''
        then
          OK := False;
        if cbObjPlayer.Text = ''
        then
          OK := False;
      end;
    if MisStoredOptions[0].Target = ttpFacilitiesFromCompanyInRadius
    then
      begin
        List := TStringList.Create;
        if Sender = cbObjPlayer
        then
          begin
            List.Clear;
            Back := IBSystem.RDOGetCompanyList(cbObjPlayer.Text);
            if varType(Back) = varInteger
              then
                Application.MessageBox( PChar('Unknown Error'), PChar('Error'), MB_OK or MB_ICONERROR )
              else
                begin
                  List := TStringList.Create;
                  List.Text := Back;
                  cbObjTarget.Clear;
                  for i := 1 to List.Count do
                    cbObjTarget.Items.Add(List.Values[IntToStr(i)]);
                end;
          end;
        if Sender = cbObjTarget
        then
          begin
            List.Clear;
            Back := IBSystem.RDOGetFacilityList(cbObjPlayer.Text);
            if varType(Back) = varInteger
              then
                Application.MessageBox( PChar('Unknown Error'), PChar('Error'), MB_OK or MB_ICONERROR )
              else
                begin
                  List := TStringList.Create;
                  List.Text := Back;
                  cbObjFacilities.Clear;
                  for i := 1 to List.Count do
                    cbObjFacilities.Items.Add(List.Values[IntToStr(i)]);
                end;
          end;
        if cbObjTarget.Text = ''
        then
          OK := False;
        if cbObjFacilities.Text = ''
        then
          OK := False;
        if cbObjPlayer.Text = ''
        then
          OK := False;
        if cbObjRadius.Text = ''
        then
          OK := False;
      end;
    if MisStoredOptions[0].Target = ttpFacilitiesInRadius
    then
      begin
        if cbObjFacilities.Text = ''
        then
          OK := False;
        if cbObjRadius.Text = ''
        then
          OK := False;
      end;
    if OK = True
    then
      btnAssignObj.Enabled := True;
  end;

procedure TForm1.btnObjectiveClick(Sender: TObject);
  var
    i : integer;
    Back : olevariant;
    List : TStringList;
    prevX : string;
    prevY : string;
  begin
    if eObjX.Text <> '0'
    then
      prevX := eObjX.Text;
    if eObjY.Text <> '0'
    then
      prevY := eObjY.Text;
    if cbObjX.Text <> ''
    then
      prevX := cbObjX.Text;
    if cbObjY.Text <> ''
    then
      prevY := cbObjY.Text;
    cbObjTarget.Clear;
    cbObjPlayer.Clear;
    cbObjFacilities.Clear;
    cbObjRadius.Clear;
    cbObjX.Clear;
    cbObjY.Clear;
    eObjX.Text := '0';
    eObjY.Text := '0';
    lblMaxDistance.Caption := '0';
    if prevX <> ''
    then
      cbObjX.Text := prevX;
    if prevY <> ''
    then
      cbObjY.Text := prevY;
    if prevX <> '0'
    then
      eObjX.Text := prevX;
    if prevY <> '0'
    then
      eObjY.Text := prevY;
    if MisStoredOptions[0].Target = ttpCoordinates
    then
      begin
        eObjX.Visible := True;
        eObjY.Visible := True;
        cbObjX.Visible := True;
        cbObjY.Visible := True;
        lblObjX.Visible := True;
        lblObjY.Visible := True;
        lblObjTarget.Visible := False;
        cbObjTarget.Visible := False;
        cbObjPlayer.Visible := False;
        cbObjRadius.Visible := False;
        lblObjPlayer.Visible := False;
        lblObjRadius.Visible := False;
        lblObjFacilities.Visible := False;
        cbObjFacilities.Visible := False;
      end;
    if MisStoredOptions[0].Target = ttpRoad
    then
      begin
        eObjX.Visible := True;
        eObjY.Visible := True;
        cbObjX.Visible := True;
        cbObjY.Visible := True;
        lblObjX.Visible := True;
        lblObjY.Visible := True;
        lblObjTarget.Visible := False;
        cbObjTarget.Visible := False;
        cbObjPlayer.Visible := False;
        cbObjRadius.Visible := False;
        lblObjPlayer.Visible := False;
        lblObjRadius.Visible := False;
        lblObjFacilities.Visible := False;
        cbObjFacilities.Visible := False;
      end;
    if MisStoredOptions[0].Target = ttpCriminal
    then
      begin
        eObjX.Visible := False;
        eObjY.Visible := False;
        cbObjX.Visible := False;
        cbObjY.Visible := False;
        lblObjX.Visible := False;
        lblObjY.Visible := False;
        lblObjTarget.Visible := True;
        lblObjTarget.Caption := 'Criminal:';
        cbObjTarget.Visible := True;
        cbObjPlayer.Visible := False;
        cbObjRadius.Visible := False;
        lblObjPlayer.Visible := True;
        lblObjPlayer.Caption := 'Boss:';
        lblObjRadius.Visible := False;
        lblObjFacilities.Visible := False;
        cbObjFacilities.Visible := False;
        ListCriminalByLeader;
        Back := IBSystem.RDOGetLeaderList;
        if varType(Back) = varInteger
          then
            Application.MessageBox( PChar('Unknown Error'), PChar('Error'), MB_OK or MB_ICONERROR )
          else
            begin
              List := TStringList.Create;
              List.Text := Back;
              cbObjPlayer.Clear;
              for i := 1 to List.Count do
                cbObjPlayer.Items.Add(List.Values[IntToStr(i)]);
            end;
      end;
    if MisStoredOptions[0].Target = ttpTeam
    then
      begin
        eObjX.Visible := False;
        eObjY.Visible := False;
        cbObjX.Visible := False;
        cbObjY.Visible := False;
        lblObjX.Visible := False;
        lblObjY.Visible := False;
        lblObjTarget.Visible := True;
        lblObjTarget.Caption := 'Team:';
        cbObjTarget.Visible := True;
        cbObjPlayer.Visible := False;
        cbObjRadius.Visible := False;
        lblObjPlayer.Visible := True;
        lblObjPlayer.Caption := 'Boss:';
        lblObjRadius.Visible := False;
        lblObjFacilities.Visible := False;
        cbObjFacilities.Visible := False;
        Back := IBSystem.RDOGetLeaderList;
        if varType(Back) = varInteger
          then
            Application.MessageBox( PChar('Unknown Error'), PChar('Error'), MB_OK or MB_ICONERROR )
          else
            begin
              List := TStringList.Create;
              List.Text := Back;
              cbObjPlayer.Clear;
              for i := 1 to List.Count do
                cbObjPlayer.Items.Add(List.Values[IntToStr(i)]);
            end;
      end;
    if MisStoredOptions[0].Target = ttpPlayer
    then
      begin
        eObjX.Visible := False;
        eObjY.Visible := False;
        cbObjX.Visible := False;
        cbObjY.Visible := False;
        lblObjX.Visible := False;
        lblObjY.Visible := False;
        lblObjTarget.Visible := False;
        cbObjTarget.Visible := False;
        cbObjPlayer.Visible := True;
        cbObjRadius.Visible := False;
        lblObjPlayer.Visible := True;
        lblObjPlayer.Caption := 'Tycoon:';
        lblObjRadius.Visible := False;
        lblObjFacilities.Visible := False;
        cbObjFacilities.Visible := False;
        Back := IBSystem.RDOGetPlayerList;
        if varType(Back) = varInteger
          then
            Application.MessageBox( PChar('Unknown Error'), PChar('Error'), MB_OK or MB_ICONERROR )
          else
            begin
              List := TStringList.Create;
              List.Text := Back;
              cbObjPlayer.Clear;
              for i := 1 to List.Count do
                cbObjPlayer.Items.Add(List.Values[IntToStr(i)]);
            end;
      end;
    if MisStoredOptions[0].Target = ttpLeader
    then
      begin
        eObjX.Visible := False;
        eObjY.Visible := False;
        cbObjX.Visible := False;
        cbObjY.Visible := False;
        lblObjX.Visible := False;
        lblObjY.Visible := False;
        lblObjTarget.Visible := False;
        cbObjTarget.Visible := False;
        cbObjPlayer.Visible := True;
        cbObjRadius.Visible := False;
        lblObjPlayer.Visible := True;
        lblObjPlayer.Caption := 'Boss:';
        lblObjRadius.Visible := False;
        lblObjFacilities.Visible := False;
        cbObjFacilities.Visible := False;
        Back := IBSystem.RDOGetLeaderList;
        if varType(Back) = varInteger
          then
            Application.MessageBox( PChar('Unknown Error'), PChar('Error'), MB_OK or MB_ICONERROR )
          else
            begin
              List := TStringList.Create;
              List.Text := Back;
              cbObjPlayer.Clear;
              for i := 1 to List.Count do
                cbObjPlayer.Items.Add(List.Values[IntToStr(i)]);
            end;
      end;
    if MisStoredOptions[0].Target = ttpCity
    then
      begin
        eObjX.Visible := False;
        eObjY.Visible := False;
        cbObjX.Visible := False;
        cbObjY.Visible := False;
        lblObjX.Visible := False;
        lblObjY.Visible := False;
        lblObjTarget.Visible := True;
        lblObjTarget.Caption := 'City:';
        cbObjTarget.Visible := True;
        cbObjPlayer.Visible := False;
        cbObjRadius.Visible := False;
        lblObjPlayer.Visible := False;
        lblObjRadius.Visible := False;
        lblObjFacilities.Visible := False;
        cbObjFacilities.Visible := False;
        Back := IBSystem.RDOGetCityList;
        if varType(Back) = varInteger
          then
            Application.MessageBox( PChar('Unknown Error'), PChar('Error'), MB_OK or MB_ICONERROR )
          else
            begin
              List := TStringList.Create;
              List.Text := Back;
              cbObjTarget.Clear;
              for i := 1 to List.Count do
                cbObjTarget.Items.Add(List.Values[IntToStr(i)]);
            end;
      end;
    if MisStoredOptions[0].Target = ttpFacility
    then
      begin
        eObjX.Visible := True;
        eObjY.Visible := True;
        cbObjX.Visible := True;
        cbObjY.Visible := True;
        lblObjX.Visible := True;
        lblObjY.Visible := True;
        lblObjTarget.Visible := False;
        cbObjTarget.Visible := False;
        cbObjPlayer.Visible := False;
        cbObjRadius.Visible := False;
        lblObjPlayer.Visible := False;
        lblObjRadius.Visible := False;
        lblObjFacilities.Visible := False;
        cbObjFacilities.Visible := False;
      end;
    if MisStoredOptions[0].Target = ttpCompany
    then
      begin
        eObjX.Visible := False;
        eObjY.Visible := False;
        cbObjX.Visible := False;
        cbObjY.Visible := False;
        lblObjX.Visible := False;
        lblObjY.Visible := False;
        lblObjTarget.Visible := True;
        lblObjTarget.Caption := 'Company:';
        cbObjTarget.Visible := True;
        cbObjPlayer.Visible := True;
        cbObjRadius.Visible := False;
        lblObjPlayer.Visible := True;
        lblObjPlayer.Caption := 'Tycoon:';
        lblObjRadius.Visible := False;
        lblObjFacilities.Visible := False;
        cbObjFacilities.Visible := False;
        Back := IBSystem.RDOGetPlayerList;
        if varType(Back) = varInteger
          then
            Application.MessageBox( PChar('Unknown Error'), PChar('Error'), MB_OK or MB_ICONERROR )
          else
            begin
              List := TStringList.Create;
              List.Text := Back;
              cbObjPlayer.Clear;
              for i := 1 to List.Count do
                cbObjPlayer.Items.Add(List.Values[IntToStr(i)]);
            end;
      end;
    if MisStoredOptions[0].Target = ttpFacilitiesFromPlayer
    then
      begin
        eObjX.Visible := False;
        eObjY.Visible := False;
        cbObjX.Visible := False;
        cbObjY.Visible := False;
        lblObjX.Visible := False;
        lblObjY.Visible := False;
        lblObjTarget.Visible := False;
        cbObjTarget.Visible := False;
        cbObjPlayer.Visible := True;
        cbObjRadius.Visible := False;
        lblObjPlayer.Visible := True;
        lblObjPlayer.Caption := 'Tycoon:';
        lblObjRadius.Visible := False;
        lblObjFacilities.Visible := True;
        cbObjFacilities.Visible := True;
        Back := IBSystem.RDOGetPlayerList;
        if varType(Back) = varInteger
          then
            Application.MessageBox( PChar('Unknown Error'), PChar('Error'), MB_OK or MB_ICONERROR )
          else
            begin
              List := TStringList.Create;
              List.Text := Back;
              cbObjPlayer.Clear;
              for i := 1 to List.Count do
                cbObjPlayer.Items.Add(List.Values[IntToStr(i)]);
            end;
      end;
    if MisStoredOptions[0].Target = ttpFacilitiesFromPlayerInRadius
    then
      begin
        eObjX.Visible := False;
        eObjY.Visible := False;
        cbObjX.Visible := False;
        cbObjY.Visible := False;
        lblObjX.Visible := False;
        lblObjY.Visible := False;
        lblObjTarget.Visible := False;
        cbObjTarget.Visible := False;
        cbObjPlayer.Visible := True;
        cbObjRadius.Visible := True;
        lblObjPlayer.Visible := True;
        lblObjPlayer.Caption := 'Tycoon:';
        lblObjRadius.Visible := True;
        lblObjFacilities.Visible := True;
        cbObjFacilities.Visible := True;
        Back := IBSystem.RDOGetPlayerList;
        if varType(Back) = varInteger
          then
            Application.MessageBox( PChar('Unknown Error'), PChar('Error'), MB_OK or MB_ICONERROR )
          else
            begin
              List := TStringList.Create;
              List.Text := Back;
              cbObjPlayer.Clear;
              for i := 1 to List.Count do
                cbObjPlayer.Items.Add(List.Values[IntToStr(i)]);
            end;
        for i := 1 to MisStoredOptions[0].MaxRadius do
          begin
            if (StrToInt(eHeadX.Text) - MisStoredOptions[0].MaxRadius + i) > 0
            then
              if (StrToInt(eHeadY.Text) - MisStoredOptions[0].MaxRadius + i) > 0
              then
                cbObjRadius.Items.Add(IntToStr(i));
          end;
      end;
    if MisStoredOptions[0].Target = ttpFacilitiesFromCompany
    then
      begin
        eObjX.Visible := False;
        eObjY.Visible := False;
        cbObjX.Visible := False;
        cbObjY.Visible := False;
        lblObjX.Visible := False;
        lblObjY.Visible := False;
        lblObjTarget.Visible := True;
        lblObjTarget.Caption := 'Company:';
        cbObjTarget.Visible := True;
        cbObjPlayer.Visible := True;
        cbObjRadius.Visible := True;
        lblObjPlayer.Visible := True;
        lblObjPlayer.Caption := 'Tycoon:';
        lblObjRadius.Visible := True;
        lblObjFacilities.Visible := True;
        cbObjFacilities.Visible := True;
        Back := IBSystem.RDOGetPlayerList;
        if varType(Back) = varInteger
          then
            Application.MessageBox( PChar('Unknown Error'), PChar('Error'), MB_OK or MB_ICONERROR )
          else
            begin
              List := TStringList.Create;
              List.Text := Back;
              cbObjPlayer.Clear;
              for i := 1 to List.Count do
                cbObjPlayer.Items.Add(List.Values[IntToStr(i)]);
            end;
      end;
    if MisStoredOptions[0].Target = ttpFacilitiesFromCompanyInRadius
    then
      begin
        eObjX.Visible := False;
        eObjY.Visible := False;
        cbObjX.Visible := False;
        cbObjY.Visible := False;
        lblObjX.Visible := False;
        lblObjY.Visible := False;
        lblObjTarget.Visible := True;
        lblObjTarget.Caption := 'Company:';
        cbObjTarget.Visible := True;
        cbObjPlayer.Visible := True;
        cbObjRadius.Visible := True;
        lblObjPlayer.Visible := True;
        lblObjPlayer.Caption := 'Tycoon:';
        lblObjRadius.Visible := True;
        lblObjFacilities.Visible := True;
        cbObjFacilities.Visible := True;
        Back := IBSystem.RDOGetPlayerList;
        if varType(Back) = varInteger
          then
            Application.MessageBox( PChar('Unknown Error'), PChar('Error'), MB_OK or MB_ICONERROR )
          else
            begin
              List := TStringList.Create;
              List.Text := Back;
              cbObjPlayer.Clear;
              for i := 1 to List.Count do
                cbObjPlayer.Items.Add(List.Values[IntToStr(i)]);
            end;
        for i := 1 to MisStoredOptions[0].MaxRadius do
          begin
            if (StrToInt(eHeadX.Text) - MisStoredOptions[0].MaxRadius + i) > 0
            then
              if (StrToInt(eHeadY.Text) - MisStoredOptions[0].MaxRadius + i) > 0
              then
                cbObjRadius.Items.Add(IntToStr(i));
          end;
      end;
    if MisStoredOptions[0].Target = ttpFacilitiesInRadius
    then
      begin
        eObjX.Visible := False;
        eObjY.Visible := False;
        cbObjX.Visible := False;
        cbObjY.Visible := False;
        lblObjX.Visible := False;
        lblObjY.Visible := False;
        lblObjTarget.Visible := False;
        cbObjTarget.Visible := False;
        cbObjPlayer.Visible := False;
        cbObjRadius.Visible := True;
        lblObjPlayer.Visible := False;
        lblObjRadius.Visible := True;
        lblObjFacilities.Visible := True;
        cbObjFacilities.Visible := True;
        Back := IBSystem.RDOGetFacilityList;
        if varType(Back) = varInteger
          then
            Application.MessageBox( PChar('Unknown Error'), PChar('Error'), MB_OK or MB_ICONERROR )
          else
            begin
              List := TStringList.Create;
              List.Text := Back;
              cbObjFacilities.Clear;
              for i := 1 to List.Count do
                cbObjFacilities.Items.Add(List.Values[IntToStr(i)]);
            end;
        for i := 1 to MisStoredOptions[0].MaxRadius do
          begin
            if (StrToInt(eHeadX.Text) - MisStoredOptions[0].MaxRadius + i) > 0
            then
              if (StrToInt(eHeadY.Text) - MisStoredOptions[0].MaxRadius + i) > 0
              then
                cbObjRadius.Items.Add(IntToStr(i));
          end;
      end;
    if MisStoredOptions[0].MaxDistance <> 0
    then
      begin
        eObjX.Visible := False;
        eObjY.Visible := False;
        cbObjX.Visible := True;
        cbObjY.Visible := True;
        cbObjX.Items.Clear;
        cbObjY.Items.Clear;
        for i := 1 to MisStoredOptions[0].MaxDistance do
          begin
            if (StrToInt(eHeadX.Text) - MisStoredOptions[0].MaxDistance + i) > 0
            then
              cbObjX.Items.Add(IntToStr(StrToInt(eHeadX.Text) - MisStoredOptions[0].MaxDistance + i));
            if (StrToInt(eHeadY.Text) - MisStoredOptions[0].MaxDistance + i) > 0
            then
              cbObjY.Items.Add(IntToStr(StrToInt(eHeadY.Text) - MisStoredOptions[0].MaxDistance + i));
          end;
        for i := 1 to MisStoredOptions[0].MaxDistance do
          begin
            cbObjX.Items.Add(IntToStr(StrToInt(eHeadX.Text) + i));
            cbObjY.Items.Add(IntToStr(StrToInt(eHeadY.Text) + i));
          end;
        lblMaxDistance.Caption := IntToStr(MisStoredOptions[0].MaxDistance);
      end
    else
      begin
        eObjX.Visible := True;
        eObjY.Visible := True;
        cbObjX.Visible := False;
        cbObjY.Visible := False;
        lblMaxDistance.Caption := IntToStr(0);
      end;
    GroupBox8.Visible := True;
    mMissionDescr.Visible := False;
    Label244.Visible := False;
    btnObjective.Enabled := False;
    cbMissions.Enabled := False;
  end;

procedure TForm1.ListCriminalByLeader;
  var
    i : integer;
    Back : olevariant;
    List : TStringList;
  begin
    Back := IBSystem.RDOGetCriminalListByLeader(cbObjPlayer.Text);
    if varType(Back) = varInteger
      then
        Application.MessageBox( PChar('Unknown Error'), PChar('Error'), MB_OK or MB_ICONERROR )
      else
        begin
          List := TStringList.Create;
          List.Text := Back.Text;
          cbObjPlayer.Clear;
          for i := 1 to List.Count do
            cbObjTarget.Items.Add(List.Values[IntToStr(i)]);
        end;
  end;

procedure TForm1.btnStartMissionClick(Sender: TObject);
  begin
    if IBSystem.RDOStartMission(widestring(edLeader.Text), widestring(cbTeams.Text), widestring(cbMissions.Text)) <> 1
    then
      begin
        GroupBox9.Top := 72;
        GroupBox9.Left := 688;
        GroupBox9.Height := 385;
        GroupBox9.Width := 265;
        GroupBox7.Visible := False;
        GroupBox9.Visible := True;
        lblRepObjective.Caption := lblObjective.Caption;
        btnMissionOk.Enabled := True;
      end
    else
      Application.MessageBox( PChar('Couldn''t start mission.'), PChar('Error'), MB_OK or MB_ICONERROR );
  end;

procedure TForm1.btnMissionOkClick(Sender: TObject);
  var
    i : integer;
    CrimList : TStringList;
  begin
    CrimList := TStringList.Create;
    for i := 1 to 8 do
      begin
        if TeamLabels[i].Caption <> ''
        then
          begin
            CrimList.Text := IBSystem.RDOFindCriminal(widestring(TeamLabels[i].Caption));
            if CrimList.Values['State'] = IntToStr(5)
            then
              begin
                TeamRolesCombos[i].Enabled := True;
              end;
            if CrimList.Values['State'] = IntToStr(6)
            then
              begin
                TeamRolesCombos[i].Enabled := False;
                TeamRolesCombos[i].ItemIndex := -1;
              end;
            if CrimList.Values['State'] = IntToStr(10)
            then
              begin
                TeamRolesCombos[i].Enabled := False;
                TeamRolesCombos[i].ItemIndex := -1;
              end;
          end;
      end;
    if IBSystem.RDODeassignMission(widestring(edLeader.Text), widestring(cbTeams.Text), widestring('no'), widestring(cbMissions.Text)) <> 1
    then
      begin
        GroupBox7.Visible := True;
        GroupBox9.Visible := False;
        btnMissionOk.Enabled := False;
        cbMissions.ItemIndex := -1;
        cbMissions.Enabled := True;
        CleanMissionSheet;
        CleanMissionGrid;
      end
    else
      Application.MessageBox( PChar('Couldn''t deassign mission.'), PChar('Error'), MB_OK or MB_ICONERROR );
  end;

procedure TForm1.ShowMissionBox(TeamList : TStringList);
  var
    i : integer;
    Report : TStringList;
  begin
    Report := TStringList.Create;
    lblRepProfit.Caption := TeamList.Values['Profit'];
    lblRepCost.Caption := TeamList.Values['Cost'];
    lblRepDuration.Caption := TeamList.Values['Time'] + ' of ' + lblMissionDuration.Caption;
    Report.text := IBSystem.RDOGetMissionReport(widestring(edLeader.Text), widestring(cbTeams.Text));
    mReport.Clear;
    for i := 0 to Report.Count - 1 do
      mReport.Lines.Add(Report.Strings[i]);
    if lblMissionDuration.Caption = lblRepDuration.Caption
    then
      btnMissionOk.Enabled := True;
  end;

procedure TForm1.btnCancelMissionClick(Sender: TObject);
  var
    i : integer;
    CrimList : TStringList;
  begin
    CrimList := TStringList.Create;
    for i := 1 to 8 do
      begin
        if TeamLabels[i].Caption <> ''
        then
          begin
            CrimList.Text := IBSystem.RDOFindCriminal(widestring(TeamLabels[i].Caption));
            if CrimList.Values['State'] = IntToStr(5)
            then
              begin
                TeamRolesCombos[i].Enabled := True;
                StatusImages[i].Picture := nil;
              end;
          end;
      end;
    if IBSystem.RDODeassignMission(widestring(edLeader.Text), widestring(cbTeams.Text), widestring('yes'), widestring(cbMissions.Text)) <> 1
    then
      begin
        CleanMissionSheet;
        CleanMissionGrid;
        cbMissions.ItemIndex := -1;
        cbMissions.Enabled := True;
      end
    else
      Application.MessageBox( PChar('Couldn''t deassign mission.'), PChar('Error'), MB_OK or MB_ICONERROR );
  end;

procedure TForm1.eReputationChange(Sender: TObject);
  begin
    if lblReputation.Caption <> eReputation.Text
    then
      btnReputation.Enabled := True
    else
      btnReputation.Enabled := False;
  end;

procedure TForm1.btnReputationClick(Sender: TObject);
  begin
    if IBSystem.RDOChangeReputation(edLeader.Text, StrToInt(eReputation.Text)) = 1
    then
      Application.MessageBox( PChar('Couldn''t change the leader reputation.'), PChar('Error'), MB_OK or MB_ICONERROR );
  end;

// Propagate the Beat of the Game

procedure TForm1.GiveTheBeat(Sender : TObject);
  var
    k : integer;
    crimList : TStringList;
    LeaderList : TStringList;
    TeamList : TStringList;
    TotCrimList : TStringList;
    Back : olevariant;
  begin
    lblTime.Caption := IntToStr(IBSystem.RDOGetTime);
    if edLeader.Text <> ''
    then
      begin
        //IBSystem.PropagateTheBeat; // >>
        LeaderList := TStringList.Create;
        LeaderList.Text := IBSystem.RDOFindLeader(widestring(edLeader.Text));
        lblMoney.Caption := FloatToStr(Round(StrToFloat(LeaderList.Values['Money'])));
        lblExpenses.Caption := FloatToStr(Round(StrToFloat(LeaderList.Values['Expenses'])));
        lblReputation.Caption := FloatToStr(Round(StrToFloat(LeaderList.Values['Reputation'])));
        TotCrimList := TStringList.Create;
        TotCrimList.Text := IBSystem.RDOGetCriminalListByLeader('');
        lblTotalCriminals.Caption := IntToStr(TotCrimList.Count);
        if IBSystem.RDOCheckCriminalList(widestring(edLeader.Text)) = 1
        then
          begin
            Back := IBSystem.RDOGetCriminalNames(widestring(edLeader.Text));
            if VarType(Back) = varInteger
              then
                Application.MessageBox( PChar('Error getting criminals on the market. Code: ' + IntToStr(Back)), PChar('Error'), MB_OK or MB_ICONERROR )
              else
                begin
                  Criminals.Text := Back;
                  DisplayCriminalsOnMarket;
                end;
          end;
        if cbTeams.Text <> ''
        then
          begin
            if GroupBox9.Visible = True
            then
              begin
                TeamList := TStringList.Create;
                TeamList.Text := IBSystem.RDOFindTeam(widestring(edLeader.Text), widestring(cbTeams.Text));
                if TeamList.Values['Change'] = 'yes'
                then
                  begin
                    ShowMissionBox(TeamList);
                    if IBSystem.RDOMissionChangeShowed(widestring(edLeader.Text), widestring(cbTeams.Text)) = 1
                    then
                      Application.MessageBox( PChar('Couldn''t notify the showing of the mission changes.'), PChar('Error'), MB_OK or MB_ICONERROR );
                  end;
              end;
            for k := 1 to 8 do
              if TeamLabels[k].Caption <> ''
              then
                begin
                  crimList := TStringList.Create;
                  crimList.Text := IBSystem.RDOFindCriminal(widestring(TeamLabels[k].Caption));
                  if crimList.Values['Change'] = 'yes'
                  then
                    begin
                      ShowCriminal(TeamLabels[k].Caption, k);
                      if IBSystem.RDOCriminalShowed(widestring(TeamLabels[k].Caption)) = 1
                      then
                        Application.MessageBox( PChar('Couldn''t notify the showing of the criminal changes.'), PChar('Error'), MB_OK or MB_ICONERROR );
                    end;
                end;
            lblTotalTraining.Caption := FloatToStr(Round(CalculateTotalTrainingCost(cbTeams.Text)));
            if StrToInt(lblTotalTraining.Caption) < 1
              then
                GroupBox6.Visible := False
              else
                GroupBox6.Visible := True;
          end;
      end;
  end;

procedure TForm1.btnJailClick(Sender: TObject);
  var
    i : integer;
    List : TStringList;
    Back : olevariant;
  begin
    Back := IBSystem.RDOGetCriminalListByLeader(widestring(edLeader.Text));
    if varType(Back) = varInteger
      then
        Application.MessageBox( PChar('Unknown Error'), PChar('Error'), MB_OK or MB_ICONERROR )
      else
        begin
          List := TStringList.Create;
          List.Text := Back;
          cbJailCrim.Clear;
          cbJailCrim.Items.Add('');
          for i := 1 to List.Count do
            cbJailCrim.Items.Add(List.Values[IntToStr(i)]);
        end;
    cbJailTeam.Clear;
    cbJailTeam.Items.Add('');
    for i := 0 to (cbTeams.Items.Count - 1) do
      cbJailTeam.Items.Add(cbTeams.Items[i]);
    btnCreateNewTeam.Enabled := False;
    btnDismissTeam.Enabled := False;
    btnJail.Enabled := False;
    GroupBox10.Visible := True;
    GroupBox7.Visible := False;
    eJailLHours.Text := '0';
    eJailBribe.Text := '0';
  end;

procedure TForm1.btnJailCancelClick(Sender: TObject);
  begin
    btnCreateNewTeam.Enabled := True;
    btnDismissTeam.Enabled := True;
    btnJail.Enabled := True;
    GroupBox10.Visible := False;
  end;

procedure TForm1.btnJailOKClick(Sender: TObject);
  var
    Back : olevariant;
  begin
    btnCreateNewTeam.Enabled := True;
    btnDismissTeam.Enabled := True;
    btnJail.Enabled := True;
    Back := IBSystem.RDOChangeJailParameters(widestring(cbLeaders.Text), widestring(cbJailCrim.Text), widestring(cbJailTeam.Text), widestring(eJailLHours.Text), widestring(eJailBribe.Text));
    if VarType(Back) = varInteger
      then
        Application.MessageBox( PChar('Unknown Error. Code: ' + IntToStr(Back)), PChar('Error'), MB_OK or MB_ICONERROR );
    Back := IBSystem.RDOGetLawyersHoursCost;
    if VarType(Back) = varInteger
      then
        Application.MessageBox( PChar('Unknown Error. Code: ' + IntToStr(Back)), PChar('Error'), MB_OK or MB_ICONERROR )
      else
        lblJailLHours.Caption := IntToStr(StrToInt(eJailLHours.Text) * StrToInt(Back)) + ' $';
  end;

procedure TForm1.JailFieldsChange(Sender: TObject);
  var
    i : integer;
    Team : TStringList;
    criminal : TStringList;
  begin
    if cbJailCrim.Text <> ''
    then
      begin
        criminal := TStringList.Create;
        criminal.Text := IBSystem.RDOFindCriminal(widestring(cbJailCrim.Text));
        eJailLHours.Text := criminal.Values['LawyersHours'];
        eJailBribe.Text := criminal.Values['Bribing'];
      end
    else
      if cbJailTeam.Text <> ''
      then
        begin
          Team := TStringList.Create;
          Team.Text := IBSystem.RDOFindTeam(widestring(cbLeaders.Text), widestring(cbJailTeam.Text));
          cbJailCrim.Clear;
          cbJailCrim.Items.Add('');
          for i := 0 to (StrToInt(Team.Values['NumCrim']) -1) do
            cbJailCrim.Items.Add(Team.Values['Criminal' + IntToStr(i)]);
          cbJailCrim.ItemIndex := -1;
          eJailBribe.Text := Team.Values['Bribing'];
          eJailLHours.Text := Team.Values['LawyersHours'];
        end
      else
        begin
          btnJailClick(Self);
        end;
  end;















end.
