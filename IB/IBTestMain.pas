unit IBTestMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, IBSystem;

type
  TClientCriminal =
    class
      public
        procedure DeSerialize( strlist : TStringList; prefix : string; index : integer );
        function  Clone() : TClientCriminal;
      public
        Name    : string;
        Sex     : string;
        Picture : string;
        Status  : string;
        Id      : integer;
        Skills  : array[0..pred(SKILL_COUNT)] of integer;
    end;

  TClientMissionInfo =
    class
      public
        procedure Load( filename : string );
      public
        Id    : string;
        Desc  : string;
        Roles : TStringList;
    end;

  TForm1 = class(TForm)
    nbMain: TNotebook;
    Label1: TLabel;
    edTycoonName: TEdit;
    Button1: TButton;
    Label2: TLabel;
    edSPAlias: TEdit;
    Button2: TButton;
    Label3: TLabel;
    cbTeams: TComboBox;
    Label4: TLabel;
    lbCriminals: TListBox;
    Label5: TLabel;
    lbCriminalInfo: TListBox;
    btnHQ: TButton;
    btnHire: TButton;
    btnFire: TButton;
    Train: TButton;
    btnMission: TButton;
    Label6: TLabel;
    lbTeamStatus: TLabel;
    Missions: TLabel;
    cbMissions: TComboBox;
    Label7: TLabel;
    lbRole1: TLabel;
    lbRole2: TLabel;
    lbRole3: TLabel;
    lbRole4: TLabel;
    lbRole5: TLabel;
    lbRole6: TLabel;
    lbRole7: TLabel;
    lbRole8: TLabel;
    Label8: TLabel;
    cbCriminal1: TComboBox;
    cbCriminal3: TComboBox;
    cbCriminal2: TComboBox;
    cbCriminal4: TComboBox;
    cbCriminal5: TComboBox;
    cbCriminal6: TComboBox;
    cbCriminal7: TComboBox;
    cbCriminal8: TComboBox;
    Button3: TButton;
    Label9: TLabel;
    Button4: TButton;
    Label10: TLabel;
    lbOutput: TListBox;
    Button5: TButton;
    Button6: TButton;
    Label12: TLabel;
    Label14: TLabel;
    edCrimAlias: TEdit;
    lbCriminalName: TLabel;
    Button7: TButton;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure cbTeamsChange(Sender: TObject);
    procedure lbCriminalsClick(Sender: TObject);
    procedure btnMissionClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnHireClick(Sender: TObject);
    procedure btnHQClick(Sender: TObject);
    procedure btnFireClick(Sender: TObject);
    procedure cbMissionsChange(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    fTycoonId       : integer;
    fCurrentTeam    : TList;
    fMissions       : TList;
    fCurrentMission : TClientMissionInfo;
    fRoleLabels     : array[0..7] of TLabel;
    fRoleCriminals  : array[0..7] of TComboBox;
    procedure InitIBSystem;
    procedure GatherCriminalInformation;
    procedure GatherTeamInformation( teamName : string );
    procedure GatherMissionInformation;
    function  StrToRole( str : string ) : string;
    function  getCriminalFromName( name : string ) : TClientCriminal;
  end;

var
  Form1: TForm1;

implementation


  uses
    NewTeamDlg, HireCriminalDlg, NativeClassStorage, XMLFile, stringutils;

{$R *.DFM}

  // TClientCriminal

  procedure TClientCriminal.DeSerialize( strlist : TStringList; prefix : string; index : integer );
    var
      i : integer;
    begin
      Name    := strlist.Values[prefix + 'Name' + IntToStr(index)];
      Sex     := strlist.Values[Prefix + 'Sex'     + IntToStr(index)];
      Picture := strlist.Values[Prefix + 'Picture' + IntToStr(index)];
      Status  := strlist.Values[Prefix + 'Status'  + IntToStr(index)];
      Id      := StrToInt( strlist.Values[prefix + 'Id' + IntToStr(index)]);

      for i := 0 to pred(SKILL_COUNT) do
        Skills[i] := trunc( StrToFloat( strlist.Values[Prefix + 'Skill' + IntToStr(i) + '_' + IntToStr(index)])*100 );
    end;

  function TClientCriminal.Clone() : TClientCriminal;
    begin
      result         := TClientCriminal.Create;
      result.Name    := Name;
      result.Sex     := Sex;
      result.Picture := Picture;
      result.Status  := Status;
      result.Id      := Id;
      result.Skills  := Skills;
    end;

  // TClientMissionInfo

  procedure TClientMissionInfo.Load( filename : string );
    var
      xmlFile : TXMLFile;
      i       : integer;
      Info    : TXMLNode;
      _roles  : TList;
      role    : TXMLNode;
    begin
      Roles   := TStringList.Create;
      xmlFile := TXMLFile.Create;
      try
        if xmlFile.Load( filename )
          then
            begin
              Info  := xmlFile.getNodeByName( 'root/Info' );
              Id    := Info.ReadString( 'Id', '' );
              Desc  := Info.ReadString( 'Description', '' );
              Info.Free;

              _roles  := TList.Create;
              try
                xmlFile.queryNodes( '', 'Role', _roles );
                for i := 0 to pred(_roles.Count) do
                  begin
                    role := TXMLNode(_roles[i]);
                    Roles.Add( role.ValueAsString );
                  end;
              finally
                _roles.Free;
              end;
            end;
      finally
        xmlFile.Free;
      end;
    end;

  // TForm1

  procedure TForm1.InitIBSystem;
    begin
      theMetaClassStorage := TNativeClassStorage.Create;
      theIBSystem    := TIBSystem.Create; //( ExtractFilePath(Application.ExeName ) +  'Data\' );
      theIBSystem.addLocation( 'Velasco' );
      fCurrentTeam := TList.Create;
      fMissions    := TList.Create;
      GatherMissionInformation;
    end;

  procedure TForm1.GatherCriminalInformation;
    var
      str  : string;
      Info : TStringList;
      i    : integer;
    begin
       str := theIBSystem.RDOGetTycoonInfo( fTycoonId );
       if str <> ''
         then
           begin
             Info := TStringList.Create;
             try
               Info.Text := str;
               lbCriminalName.Caption := Info.Values['CriminalName'];

               // get the teams
               i := 0;
               while (Info.Values['Team' + IntToStr(i)] <> '') do
                 cbTeams.Items.Add( Info.Values['Team' + IntToStr(i)] );


               if cbTeams.Items.Count > 0
                 then cbTeams.Text := cbTeams.Items[0];
             finally
               Info.Free;
             end;
           end
         else ShowMessage( 'Error retrieving tycoon info' );
    end;

  procedure TForm1.GatherTeamInformation( teamName : string );
    var
      str  : string;
      Info : TStringList;
      i    : integer;
      crim : TClientCriminal;
    begin
      fCurrentTeam.Clear;
      str := theIBSystem.RDOGetTeamInfo( fTycoonId, teamName );
      if str <> ''
        then
          begin
             Info := TStringList.Create;
             try
               Info.Text := str;
               i := 0;
               while (Info.Values['Name' + IntToStr(i)] <> '') do
                 begin
                   crim := TClientCriminal.Create;
                   crim.DeSerialize( Info, '', i );
                   fCurrentTeam.Add( crim );
                 end;

               lbCriminals.Items.Clear;
               for i := 0 to pred(fCurrentTeam.Count) do
                 begin
                   lbCriminals.Items.Add( TClientCriminal(fCurrentTeam[i]).Name );
                 end;
             finally
               Info.Free;
             end;
          end
        else ShowMessage( 'Error retrieving team info' );
    end;

  procedure TForm1.GatherMissionInformation;
    var
      path         : string;
      FindFileData : TWIN32FindData;
      searchHandle : THandle;
      filename     : string;
      MissionInfo  : TClientMissionInfo;
      i            : integer;
    begin
      path := ExtractFilePath(Application.ExeName ) + 'Data\Missions';
      searchHandle := FindFirstFile( pchar(path + '\*.client'), FindFileData );
      if searchHandle <> INVALID_HANDLE_VALUE
        then
          try
            repeat
              if (FindFileData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0
                then
                  begin
                    filename := path + '\' + FindFileData.cFileName;
                    MissionInfo := TClientMissionInfo.Create;
                    MissionInfo.Load( filename );
                    fMissions.Add( MissionInfo );
                  end;
            until not FindNextFile( searchHandle, FindFileData );
          finally
            windows.FindClose( searchHandle );
          end;

      fRoleLabels[0]    := lbRole1;
      fRoleLabels[1]    := lbRole2;
      fRoleLabels[2]    := lbRole3;
      fRoleLabels[3]    := lbRole4;
      fRoleLabels[4]    := lbRole5;
      fRoleLabels[5]    := lbRole6;
      fRoleLabels[6]    := lbRole7;
      fRoleLabels[7]    := lbRole8;

      fRoleCriminals[0] := cbCriminal1;
      fRoleCriminals[1] := cbCriminal2;
      fRoleCriminals[2] := cbCriminal3;
      fRoleCriminals[3] := cbCriminal4;
      fRoleCriminals[4] := cbCriminal5;
      fRoleCriminals[5] := cbCriminal6;
      fRoleCriminals[6] := cbCriminal7;
      fRoleCriminals[7] := cbCriminal8;

      for i := 0 to pred(fMissions.Count) do
        cbMissions.Items.Add( TClientMissionInfo(fMissions[i]).Desc );

      for i := 0 to 7 do
        begin
          fRoleLabels[i].Visible    := false;
          fRoleCriminals[i].Visible := false;
        end;
    end;

  function TForm1.StrToRole( str : string ) : string;
    const
      ROLE_NAMES  : array[0..MAX_ROLES - 1]  of string  = ('Leader',     'Driver', 'Gorilla', 'Artificer', 'Stalker', 'Hacker', 'Doctor', 'Sniper', 'Falsifier' );
      ROLE_IDS    : array[0..MAX_ROLES - 1]  of integer = (ROLE_LEADER, ROLE_DRIVER, ROLE_GORILLA, ROLE_ARTIFICER, ROLE_STALKER, ROLE_HACKER, ROLE_DOCTOR, ROLE_SNIPER, ROLE_FALSIFIER);

    begin
      result := IntToStr(stringToId( str, ROLE_NAMES, ROLE_IDS ));
    end;

  function TForm1.getCriminalFromName( name : string ) : TClientCriminal;
    var
      i     : integer;
      found : boolean;
    begin
      if fCurrentTeam <> nil
        then
          begin
            i     := 0;
            found := false;
            while (i < fCurrentTeam.Count) and not found do
              begin
                found := TClientCriminal(fCurrentTeam[i]).Name = name;
                inc( i );
              end;

            if found
              then result := TClientCriminal(fCurrentTeam[pred(i)])
              else result := nil;
          end
        else result := nil;
    end;

  procedure TForm1.Button1Click(Sender: TObject);
    begin
      fTycoonId := theIBSystem.RDOLogCriminalIn( edTycoonName.Text );
      if fTycoonId < ERR_MAXERROR
        then
          begin
            if (fTycoonId = ERR_NOSUCHTYCOON)
              then
                begin
                  ShowMessage( 'you dont have a criminal account, create a new one' );
                  nbMain.PageIndex := 1;
                end
              else ShowMessage( 'Unknown error' );
          end
        else
          begin
            GatherCriminalInformation;
            nbMain.PageIndex := 2;
          end;
    end;

  procedure TForm1.cbTeamsChange(Sender: TObject);
    begin
      lbCriminals.Items.Clear;
      if cbTeams.Text <> ''
        then GatherTeamInformation( cbTeams.Text );
    end;

  procedure TForm1.lbCriminalsClick(Sender: TObject);
    var
      crim : TClientCriminal;
    begin
      lbCriminalInfo.Items.Clear;
      if lbCriminals.ItemIndex <> -1
        then
          begin
            crim := TClientCriminal(fCurrentTeam[lbCriminals.ItemIndex]);
            lbCriminalInfo.Items.Add( 'Sex     : ' + crim.Sex );
            lbCriminalInfo.Items.Add( 'Picture : ' + crim.Picture );
            lbCriminalInfo.Items.Add( 'Status  : ' + crim.Status );
            lbCriminalInfo.Items.Add( 'Skills:' );
            lbCriminalInfo.Items.Add( 'Leadership : ' + IntToStr( crim.Skills[SKILL_LEADERSHIP] ) + '%' );
            lbCriminalInfo.Items.Add( 'Driving    : ' + IntToStr( crim.Skills[SKILL_DRIVING] ) + '%' );
            lbCriminalInfo.Items.Add( 'Brawling   : ' + IntToStr( crim.Skills[SKILL_BRAWLING] ) + '%' );
            lbCriminalInfo.Items.Add( 'Firearms   : ' + IntToStr( crim.Skills[SKILL_FIREARMS] ) + '%' );
            lbCriminalInfo.Items.Add( 'Stalking   : ' + IntToStr( crim.Skills[SKILL_STALKING] ) + '%' );
            lbCriminalInfo.Items.Add( 'Computer   : ' + IntToStr( crim.Skills[SKILL_COMPUTER] ) + '%' );
            lbCriminalInfo.Items.Add( 'Demolition : ' + IntToStr( crim.Skills[SKILL_DEMOLITION] ) + '%' );
            lbCriminalInfo.Items.Add( 'Stealth    : ' + IntToStr( crim.Skills[SKILL_STEALTH] ) + '%' );
            lbCriminalInfo.Items.Add( 'Medicine   : ' + IntToStr( crim.Skills[SKILL_MEDICINE] ) + '%' );
            lbCriminalInfo.Items.Add( 'Forgery    : ' + IntToStr( crim.Skills[SKILL_FORGERY] ) + '%' );
          end;
    end;

  procedure TForm1.btnMissionClick(Sender: TObject);
    begin
      nbMain.PageIndex := 3;
    end;

  procedure TForm1.FormCreate(Sender: TObject);
    begin
      InitIBSystem;
    end;

  procedure TForm1.Button2Click(Sender: TObject);
    begin
      fTycoonId := theIBSystem.RDOCreateCriminalId( edSPAlias.Text, edCrimAlias.Text );
      if fTycoonId < ERR_MAXERROR
        then
          begin
            if (fTycoonId = ERR_TYCOONALREADYEXISTS)
              then
                begin
                  ShowMessage( 'that name is already in use, please choose anothe one' );
                  nbMain.PageIndex := 1;
                end
              else ShowMessage( 'Unknown error' );
          end
        else
          begin
            GatherCriminalInformation;
            nbMain.PageIndex := 2;
          end;

    end;

  procedure TForm1.Button7Click(Sender: TObject);
    var
      err : integer;
    begin
      if NewTeam.ShowModal = mrOk
        then
          begin
            if NewTeam.edTeamName.Text <> ''
              then
                begin
                  err := theIBSystem.RDOCreateTeam( fTycoonId, NewTeam.edTeamName.Text );
                  if err = ERR_SUCCEDEED
                    then
                      begin
                        cbTeams.Items.Add( NewTeam.edTeamName.Text );
                        cbTeams.Text := NewTeam.edTeamName.Text;
                      end
                    else ShowMessage( 'unknow error creating team' );
                end;
          end;
    end;

  procedure TForm1.FormShow(Sender: TObject);
    begin
      nbMain.PageIndex := 0;
    end;

procedure TForm1.btnHireClick(Sender: TObject);
  var
    err : integer;
  begin
    if cbTeams.Text <> ''
      then
        begin
          HireCriminal.theIBSystem := theIBSystem;
          HireCriminal.theTycoonId := fTycoonId;
          HireCriminal.theTeamName := cbTeams.Text;

          if HireCriminal.ShowModal = mrOK
            then
              begin
                err := theIBSystem.RDOHireCriminal( fTycoonId, HireCriminal.Selected.Id, cbTeams.Text );
                if err = ERR_SUCCEDEED
                  then
                    begin
                      lbCriminals.Items.Add( HireCriminal.Selected.Name );
                      fCurrentTeam.Add( HireCriminal.Selected.Clone );
                    end
                  else ShowMessage( 'unknown error hiring criminals' );
              end;
        end
      else ShowMessage( 'No team selected' );
  end;

  procedure TForm1.btnHQClick(Sender: TObject);
    var
      err : integer;
    begin
    if cbTeams.Text <> ''
      then
        begin
          err := theIBSystem.RDOSetTeamHeadQuarter( fTycoonId, cbTeams.Text, 20, 20, 'Velasco' );
          if err <> ERR_SUCCEDEED
            then ShowMessage( 'unknown error setting HQ' );
        end
      else ShowMessage( 'No team selected' );
    end;

  procedure TForm1.btnFireClick(Sender: TObject);
    var
      err : integer;
    begin
      if lbCriminals.ItemIndex <> -1
        then
          begin
            err := theIBSystem.RDOFireCriminal( fTycoonId, cbTeams.Text, TClientCriminal(fCurrentTeam[lbCriminals.ItemIndex]).Id );
            if err = ERR_SUCCEDEED
              then
                begin
                  fCurrentTeam.Delete( lbCriminals.ItemIndex );
                  lbCriminals.Items.Delete( lbCriminals.ItemIndex );
                  lbCriminalInfo.Clear;
                end
              else ShowMessage( 'unknown error firing this criminal' );
          end;
    end;

  procedure TForm1.cbMissionsChange(Sender: TObject);
    var
      i, j : integer;
    begin
      if cbMissions.ItemIndex <> -1
        then
          begin
            fCurrentMission := fMissions[cbMissions.ItemIndex];

            for i := 0 to pred(fCurrentMission.Roles.Count) do
              begin
                fRoleLabels[i].Caption := fCurrentMission.Roles[i];
                fRoleLabels[i].Visible := true;

                fRoleCriminals[i].Clear;
                for j := 0 to pred(fCurrentTeam.Count) do
                  fRoleCriminals[i].Items.Add( TClientCriminal(fCurrentTeam[j]).Name );
                fRoleCriminals[i].Visible := true;
              end;
          end
        else fCurrentMission := nil;
    end;

  procedure TForm1.Button3Click(Sender: TObject);
    var
      rolesassigned : boolean;
      i             : integer;
      err           : integer;
      Info          : TStringList;
    begin
     if (fCurrentTeam <> nil) and (fCurrentMission <> nil)
       then
         begin
           rolesassigned := true;
           for i := 0 to pred(fCurrentMission.Roles.Count) do
             rolesassigned := rolesassigned and (fRoleCriminals[i].Text <> '');

           if rolesassigned
             then
               begin
                 Info := TStringList.Create;
                 try
                   for i := 0 to pred(fCurrentMission.Roles.Count) do
                     begin
                       Info.Values['Role' + IntToStr(i)]       := StrToRole( fRoleLabels[i].Caption );
                       Info.Values['Criminal' + IntToStr(i)] := IntToStr(getCriminalFromName(fRoleCriminals[i].Text).Id);
                     end;

                   err := theIBSystem.RDOStartMission( fTycoonId, cbTeams.Text, fCurrentMission.Id, Info.Text );
                   if err = ERR_SUCCEDEED
                     then nbMain.PageIndex := nbMain.PageIndex + 1
                     else ShowMessage( 'unknow error starting mission' );
                 finally
                   Info.Free;
                 end;
               end;
         end
       else ShowMessage( 'Select a mission' );
    end;

  procedure TForm1.Timer1Timer(Sender: TObject);
    begin
      theIBSystem.Act;
    end;

end.
