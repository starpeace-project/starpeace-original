unit CrimeMainViewer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, VisualControls,
  StdCtrls, PDTabControl, GradientBox, ExtCtrls, MarqueeCtrl, FramedButton,
  VoyagerInterfaces, VoyagerServerInterfaces, CrimeProtocol,
  InternationalizerComponent;

const
  SlotCount   = 8;
  SocketColor = $00343924;

type
  TOnTeamModified   = procedure of object;
  TOnLeaderModified = procedure of object;

type
  TCrimeMainView = class(TVisualControl)
    lbName: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lbLevel: TLabel;
    Label5: TLabel;
    lbRank: TLabel;
    lbReputation: TLabel;
    GradientBox1: TGradientBox;                         
    Tabs: TPDTabControl;
    Notebook: TNotebook;
    Slot1: TPanel;
    Panel2: TPanel;
    Status1: TMarquee;
    Label8: TLabel;
    cbTeams: TComboBox;
    btDisbandTeam: TFramedButton;
    btMission: TFramedButton;
    Panel19: TPanel;
    lbHistory: TListBox;
    Panel20: TPanel;
    btNewIdentity: TFramedButton;
    btSetHQ: TFramedButton;
    btGotoHQ: TFramedButton;
    Role1: TPanel;
    Slot2: TPanel;
    Panel5: TPanel;
    Status2: TMarquee;
    Role2: TPanel;
    TeamStatus: TMarquee;
    Slot3: TPanel;
    Panel8: TPanel;
    imgCriminal3: TImage;
    Status3: TMarquee;
    Role3: TPanel;
    Slot5: TPanel;
    Panel11: TPanel;
    imgCriminal5: TImage;
    Status5: TMarquee;
    Role5: TPanel;
    Slot6: TPanel;
    Panel14: TPanel;
    imgCriminal6: TImage;
    Status6: TMarquee;
    Role6: TPanel;
    Slot7: TPanel;
    Panel17: TPanel;
    imgCriminal7: TImage;
    Status7: TMarquee;
    Role7: TPanel;
    Slot4: TPanel;
    Panel22: TPanel;
    imgCriminal4: TImage;
    Status4: TMarquee;
    Role4: TPanel;
    Slot8: TPanel;
    Panel25: TPanel;
    imgCriminal8: TImage;
    Status8: TMarquee;
    Role8: TPanel;
    btNewTeam: TFramedButton;
    lbCost: TLabel;
    Label9: TLabel;
    Label11: TLabel;
    lbProfit: TLabel;
    Label13: TLabel;
    lbYearsOperating: TLabel;
    btViewRoster: TFramedButton;
    btTeamHistory: TFramedButton;
    Label15: TLabel;
    CloseBtn: TFramedButton;
    imgCriminal2: TImage;
    imgCriminal1: TImage;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label1: TLabel;
    Label4: TLabel;
    edAlias: TEdit;
    btCreateCriminalId: TFramedButton;
    Label6: TLabel;
    Label7: TLabel;
    pnError: TPanel;
    StatusTimer: TTimer;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure btViewRosterClick(Sender: TObject);
    procedure edAliasChange(Sender: TObject);
    procedure btCreateCriminalIdClick(Sender: TObject);
    procedure imgCriminalClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btNewTeamClick(Sender: TObject);
    procedure cbTeamsChange(Sender: TObject);
    procedure StatusTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fClientView       : IClientView;
    fMasterURLHandler : IMasterURLHandler;
    fIllSystem        : olevariant;
  private
    procedure SetIllSystem( aIllSystem : olevariant );
  public
    property ClientView       : IClientView       write fClientView;
    property MasterURLHandler : IMasterURLHandler write fMasterURLHandler;
    property IllSystem        : olevariant        write SetIllSystem;
  private
    procedure threadedGetPlayerInfo( const parms : array of const );
    procedure syncGetPlayerInfo( const parms : array of const );
    procedure syncNewPlayer( const parms : array of const );
    procedure threadedNewCrimeId( const parms : array of const );
    procedure syncNewCrimeId( const parms : array of const );
    procedure syncGetTeams( const parms : array of const );
    procedure syncNewTeam( const parms : array of const );
    procedure threadedGetTeamInfo( const parms : array of const );
    procedure syncGetTeamInfo( const parms : array of const );
  private
    fLeaderInfo     : TStringList;
    fCurrTeamInfo   : TStringList;
    fCurrMemberInfo : TStringList;
  private
    fSlots      : array[0..SlotCount - 1] of TPanel;
    fImages     : array[0..SlotCount - 1] of TImage;
    fStatus     : array[0..SlotCount - 1] of TMarquee;
    fRoles      : array[0..SlotCount - 1] of TPanel;
    fImageCache : array[0..SlotCount - 1] of string;
    fActiveSlot : integer;
  public
    procedure TeamModified;
    procedure LeaderModified;
    procedure Refresh( GoToServer : boolean );
  end;

const
  tidPath_IBImages = 'IBImages\';

const
  StateColors : array[TCriminalState] of TColor =
    ( $00778554,
      $00778554,
      $00778554,
      $00778554,
      clLime,
      clWhite,
      clRed,
      clRed,
      clRed,
      clRed,
      clRed );
  StateLabels : array[TCriminalState] of string =
    ( 'READY',
      'READY',
      'READY',
      'READY',
      'TRAINING',
      'MISSION',
      'DEAD',
      'JAILED',
      'BUSTED',
      'DEAD',
      'JAILED' );

implementation

{$R *.DFM}

  uses
    CriminalRosterViewer, CriminalViewer, Threads, Events, JPGtoBMP, NewTeamDialog;


  procedure TCrimeMainView.btViewRosterClick(Sender: TObject);
    begin
      CriminalRosterView.IllSystem        := fIllSystem;
      CriminalRosterView.ClientView       := fClientView;
      CriminalRosterView.MasterURLHandler := fMasterURLHandler;
      CriminalRosterView.Left := ClientOrigin.x + Width;
      CriminalRosterView.Top  := ClientOrigin.y + CriminalView.Height;
      CriminalRosterView.Leader := fLeaderInfo;
      CriminalRosterView.Team := fCurrTeamInfo;
      CriminalRosterView.OnTeamModified := TeamModified;
      CriminalRosterView.btHire.Enabled := fCurrMemberInfo.Count < 8;
      CriminalRosterView.Show;
    end;

  procedure TCrimeMainView.SetIllSystem( aIllSystem : olevariant );
    begin
      fIllSystem := aIllSystem;
      Fork( threadedGetPlayerInfo, priNormal, [self] );
    end;

  procedure TCrimeMainView.threadedGetPlayerInfo( const parms : array of const );
    var
      LeaderName : string;
      LeaderInfo : TStringList;
      Teams      : TStringList;
    begin
      try
        LeaderName := fIllSystem.RDOFindLeaderName( fClientView.getUserName );
        if LeaderName <> ''
          then
            begin
              LeaderInfo := TStringList.Create;
              try
                LeaderInfo.Text := fIllSystem.RDOFindLeader( LeaderName );
                Join( syncGetPlayerInfo, [LeaderInfo] );
              except
                LeaderInfo.Free;
              end;
              Teams := TStringList.Create;
              try
                try
                  Teams.Text := fIllSystem.RDOGetTeams( LeaderName );
                except
                end;
                if Teams.Count > 0
                  then Join( syncGetTeams, [Teams] )
                  else Join( syncNewTeam, [0] );
              except
                Teams.Free;
              end;
            end
          else Join( syncNewPlayer, [self] );
      except
      end;
    end;

  procedure TCrimeMainView.syncGetPlayerInfo( const parms : array of const );
    var
      List : TStringList absolute parms[0].vPointer;
    begin
      try
        fLeaderInfo.Free;
        fLeaderInfo := List;
        lbName.Caption := fLeaderInfo.Values['Name'];
        lbReputation.Caption := fLeaderInfo.Values['Reputation'];
        Notebook.PageIndex := 2;
        fStatus[0] := Status1;
        fStatus[1] := Status2;
        fStatus[2] := Status3;
        fStatus[3] := Status4;
        fStatus[4] := Status5;
        fStatus[5] := Status6;
        fStatus[6] := Status7;
        fStatus[7] := Status8;
        StatusTimer.Enabled := true;
      except
      end;
    end;

  procedure TCrimeMainView.syncNewPlayer( const parms : array of const );
    begin
      lbName.Caption := 'No Criminal ID';
      Notebook.PageIndex := 1;
    end;

  procedure TCrimeMainView.edAliasChange(Sender: TObject);
    begin
      btCreateCriminalId.Enabled := edAlias.Text <> '';
      pnError.Visible := false;
    end;

  procedure TCrimeMainView.btCreateCriminalIdClick(Sender: TObject);
    begin
      if edAlias.Text <> ''
        then Fork( threadedNewCrimeId, priNormal, [edAlias.Text] )
    end;

  procedure TCrimeMainView.threadedNewCrimeId( const parms : array of const );
    var
      name      : string absolute parms[0].vPChar;
      leader    : string;
      ErrorCode : TErrorCode;
    begin
      try
        try
          leader := fIllSystem.RDOFindLeader( name );
        except
          leader := '';
        end;
        if leader = ''
          then
            begin
              // fIllSystem.WaitForAnswer := true;
              ErrorCode := fIllSystem.RDOCreateLeader( name, fClientView.getUserName, 'NOPICTURE.JPG' );
              // fIllSystem.WaitForAnswer := false;
              Join( syncNewCrimeId, [ErrorCode] );
            end
          else Join( syncNewCrimeId, [CRIME_ERROR_Unknown] );
      except
      end;
    end;

  procedure TCrimeMainView.syncNewCrimeId( const parms : array of const );
    var
      result : integer absolute parms[0].vInteger;
    begin
      case result of
        0 :
          Fork( threadedGetPlayerInfo, priNormal, [self] );
        1 :
          begin
            pnError.Caption := 'This Alias already exists. Please choose a new one.';
            pnError.Visible := true;
          end;
        else
          begin
            pnError.Caption := 'Error: Could not create criminal ID. Please try again.';
            pnError.Visible := true;
          end;
      end;
    end;

  procedure TCrimeMainView.syncGetTeams( const parms : array of const );
    var
      List    : TStringList absolute parms[0].vPointer;
      lastIdx : integer;
    begin
      lastIdx := cbTeams.ItemIndex;
      cbTeams.Items.Assign( List );
      cbTeams.ItemIndex := lastIdx;
      if List.Count > 0
        then
          begin
            if cbTeams.ItemIndex < 0
              then cbTeams.ItemIndex := 0;
            Fork( threadedGetTeamInfo, priNormal, [List[cbTeams.ItemIndex]] );
          end;
    end;

  procedure TCrimeMainView.syncNewTeam( const parms : array of const );
    begin
      NewTeamDlg.ClientView       := fClientView;
      NewTeamDlg.MasterURLHandler := fMasterURLHandler;
      NewTeamDlg.IllSystem        := fIllSystem;
      NewTeamDlg.CriminalName     := fLeaderInfo.Values['Name'];
      if NewTeamDlg.ShowModal = mrOk
        then Fork( threadedGetPlayerInfo, priNormal, [self] ); 
    end;
    
  procedure TCrimeMainView.threadedGetTeamInfo( const parms : array of const );
    var
      name        : string absolute parms[0].vPchar;
      Info        : TStringList;
      MembersInfo : TStringList;
      Member      : TStringList;
      i           : integer;
    begin
      try
        Info := TStringList.Create;
        try
          Info.Text := fIllSystem.RDOFindTeam( lbName.Caption, name );
          MembersInfo := TStringList.Create;
          try
            for i := 0 to pred(StrToInt(Info.Values['NumCrim'])) do
              begin
                Member := TStringList.Create;
                Member.Text := fIllSystem.RDOFindCriminal( Info.Values['Criminal' + IntToStr(i)] );
                Member.Values['Role'] := Info.Values['Role' + IntToStr(i)];
                MembersInfo.AddObject( Member.Values['Name'], Member );
              end;
            Join( syncGetTeamInfo, [Info, MembersInfo] );
          except
            MembersInfo.Free;
          end;
        except
          Info.Free;
        end;
      except
      end;
    end;

  procedure TCrimeMainView.syncGetTeamInfo( const parms : array of const );
    var
      Info      : TStringList absolute parms[0].vPointer;
      i         : integer;
      cache     : string;
      filename  : string;
      CrimState : TCriminalState;
    begin
      FormShow( self );                    
      fMasterURLHandler.HandleEvent( evnAnswerPrivateCache, cache );
      try
        if fCurrMemberInfo <> nil
          then
            begin
              for i := 0 to pred(fCurrMemberInfo.Count) do
                fCurrMemberInfo.Objects[i].Free;
              fCurrMemberInfo.Free;
              fCurrMemberInfo := nil;
            end;
        fCurrTeamInfo.Free;
      except
      end;
      fCurrTeamInfo   := Info;
      fCurrMemberInfo := TStringList(parms[1].vPointer);
      for i := 0 to pred(fCurrMemberInfo.Count) do
        begin
          if i = fActiveSlot
            then fSlots[i].Color := clLime
            else fSlots[i].Color := SocketColor;
          filename := TStringList(fCurrMemberInfo.Objects[i]).Values['Picture'] + '.jpg';
          fImages[i].Visible := true;
          if filename <> fImageCache[i]
            then
              begin
                fImages[i].Picture.Bitmap := TBitmap.Create;
                fImages[i].Picture.Bitmap.PixelFormat := pf24bit;
                fImages[i].Picture.Bitmap.Width := fImages[i].Width;
                fImages[i].Picture.Bitmap.Height := fImages[i].Height;
                LoadJPGToBMP( cache + tidPath_IBImages + filename, fImages[i].Picture.Bitmap );
                //TVFilter( fImages[i].Picture.Bitmap );
                fImageCache[i] := filename;
              end;
          fRoles[i].Caption := TStringList(fCurrMemberInfo.Objects[i]).Values['Role'];
          CrimState := TCriminalState(StrToInt(TStringList(fCurrMemberInfo.Objects[i]).Values['State']));
          fStatus[i].ForeColor := StateColors[CrimState];
          fStatus[i].Caption   := StateLabels[CrimState];
        end;
      for i := fCurrMemberInfo.Count to pred(SlotCount) do
        begin
          fSlots[i].Color := SocketColor;
          fImages[i].Picture.Bitmap := nil;
          fImageCache[i] := '';
          fStatus[i].ForeColor := $00778554;
          fStatus[i].Caption   := '';
        end;
      if CriminalView.Visible and (fActiveSlot <> -1) and CriminalView.TeamMember and (fActiveSlot < fCurrMemberInfo.Count)
        then
          begin
            CriminalView.Leader     := fLeaderInfo;
            CriminalView.Team       := fCurrTeamInfo;
            CriminalView.Properties := TStringList(fCurrMemberInfo.Objects[fActiveSlot]);
          end;
      CriminalRosterView.Leader := fLeaderInfo;
      CriminalRosterView.Team := fCurrTeamInfo;
    end;

  procedure TCrimeMainView.imgCriminalClick(Sender: TObject);
    var
      idx : integer;
    begin
      idx := TComponent(Sender).Tag;
      if idx < fCurrMemberInfo.Count
        then
          begin
            if fActiveSlot <> -1
              then fSlots[fActiveSlot].Color := SocketColor;
            fSlots[idx].Color := clLime;
            fActiveSlot       := idx;
            CriminalView.ClientView       := fClientView;
            CriminalView.MasterURLHandler := fMasterURLHandler;
            CriminalView.IllSystem        := fIllSystem;
            CriminalView.TeamMember       := true;
            CriminalView.Leader           := fLeaderInfo;
            CriminalView.Team             := fCurrTeamInfo;
            CriminalView.Properties       := TStringList(fCurrMemberInfo.Objects[idx]);
            CriminalView.OnTeamModified   := TeamModified;
            CriminalView.MainView         := self;
            CriminalView.Show;
          end
        else
          begin
            CriminalRosterView.IllSystem        := fIllSystem;
            CriminalRosterView.ClientView       := fClientView;
            CriminalRosterView.MasterURLHandler := fMasterURLHandler;
            CriminalRosterView.Left := ClientOrigin.x + Width;
            CriminalRosterView.Top  := ClientOrigin.y + CriminalView.Height;
            CriminalRosterView.btHire.Enabled := true;
            CriminalRosterView.Leader := fLeaderInfo;
            CriminalRosterView.Team := fCurrTeamInfo;
            CriminalRosterView.OnTeamModified := TeamModified;
            CriminalRosterView.Show;
          end;
    end;

  procedure TCrimeMainView.TeamModified;
    begin
      Fork( threadedGetTeamInfo, priNormal, [cbTeams.Text] );
    end;

  procedure TCrimeMainView.LeaderModified;
    begin
      Fork( threadedGetPlayerInfo, priNormal, [self] );
    end;

  procedure TCrimeMainView.Refresh( GoToServer : boolean );
    begin
    end;

  procedure TCrimeMainView.FormShow(Sender: TObject);
    begin
      fSlots[0]  := Slot1;
      fSlots[1]  := Slot2;
      fSlots[2]  := Slot3;
      fSlots[3]  := Slot4;
      fSlots[4]  := Slot5;
      fSlots[5]  := Slot6;
      fSlots[6]  := Slot7;
      fSlots[7]  := Slot8;
      fImages[0] := imgCriminal1;
      fImages[1] := imgCriminal2;
      fImages[2] := imgCriminal3;
      fImages[3] := imgCriminal4;
      fImages[4] := imgCriminal5;
      fImages[5] := imgCriminal6;
      fImages[6] := imgCriminal7;
      fImages[7] := imgCriminal8;
      fRoles[0]  := Role1;
      fRoles[1]  := Role2;
      fRoles[2]  := Role3;
      fRoles[3]  := Role4;
      fRoles[4]  := Role5;
      fRoles[5]  := Role6;
      fRoles[6]  := Role7;
      fRoles[7]  := Role8;
      //fActiveSlot := -1;
    end;

  procedure TCrimeMainView.btNewTeamClick(Sender: TObject);
    begin
      syncNewTeam( [0] );
    end;

  procedure TCrimeMainView.cbTeamsChange(Sender: TObject);
    begin
      Fork( threadedGetTeamInfo, priNormal, [cbTeams.Text] );
    end;

  procedure TCrimeMainView.StatusTimerTimer(Sender: TObject);
    var
      i : integer;
    begin
      for i := 0 to pred(SlotCount) do
        fStatus[i].Tick;
    end;

  procedure TCrimeMainView.FormCreate(Sender: TObject);
    begin
      fActiveSlot := -1;
    end;

end.



