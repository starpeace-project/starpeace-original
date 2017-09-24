unit RoadDestroy;

interface

  uses
    SysUtils, IllegalKernel, Collection, ClassStorage, MetaCrime;

  type
    TRoadDestroy =
      class( TMission )
        protected
          constructor Create( aOwner : TTeam; aMetaMission : TMetaServerMission ); override;
        private
          DestroyWith : string;
          PoliceAlerted : boolean;
        public
          class function OptionData( index : integer ) : TMissionOptionInfo; override;
        public
          procedure StartMission; override;
          procedure ResearchMission;
          procedure ProceedWithMission; override;
          procedure EndMission; override;
          procedure DriveToTheRoad(Diff: Single);
          procedure PreparationForDestruction(Diff: Single);
          procedure DestroyRoad(Diff: Single);
          procedure ControlPedestrians(Diff: Single);
          procedure PoliceArriving(Diff: Single);
          procedure ShootingWithPolice(Diff: Single);
          procedure DriveAway(Diff: Single);
          procedure CarChase(Diff: Single);
          procedure SackedIn(Diff: Single);
          procedure ArrestedWithInjuries;
          procedure Surrender;
      end;

  const
    MaxCrimes = 2;
    cRoadDestroyOptions = 3;
    cComputer1Values = 1;
    cDriving1Values = 2;

implementation

  constructor TRoadDestroy.Create( aOwner : TTeam; aMetaMission : TMetaServerMission );
    begin
      inherited;
    end;

  class function TRoadDestroy.OptionData( index : integer ) : TMissionOptionInfo;
    var
      i : integer;
      j : integer;
      k : integer;
      l : integer;
      Info : TMissionOptionInfo;
    begin
      if index < cRoadDestroyOptions
        then
          begin
            case index of
              0 :
                begin
                  Info.Id := 'NoOption';
                  Info.way := '';
                  Info.compulsory := False;
                  Info.Roles[0] := 'Leader';
                  Info.Roles[1] := 'Driver';
                  Info.Roles[2] := 'Gorilla-1';
                  for i := 3 to (MaxRoles - 1) do
                    Info.Roles[i] := '';
                  Info.Skills[0] := 'Leadership';
                  Info.Skills[1] := 'Driving';
                  Info.Skills[2] := 'Brawling';
                  for l := 3 to (MaxRoles - 1) do
                    Info.Skills[l] := '';
                  Info.SkillValues[0] := 30;
                  Info.SkillValues[1] := 20;
                  Info.SkillValues[2] := 30;
                  for j := 3 to (MaxRoles - 1) do
                    Info.SkillValues[j] := 0;
                  Info.Descriptions[0] := 'Coordinate the explosion';
                  Info.Descriptions[1] := 'Drive to and away from the road spot';
                  Info.Descriptions[2] := 'Keep away any undesired person';
                  for k := 3 to (MaxRoles - 1) do
                    Info.Descriptions[k] := '';
                  Info.Sub := False;
                  Info.ParentOptionId := '';
                  Info.Objective := 'Road';
                  Info.Target := ttpRoad;
                  Info.MaxDistance := 100;
                  Info.ObjCoord.X := 0;
                  Info.ObjCoord.Y := 0;
                  Info.MaxRadius := 0;
                  Info.Duration := 2;
                  Info.Cost := 200;
                  Info.Profit := 0;
                end;
              1 :
                begin
                  Info.Id := 'Destroy with:';
                  Info.way := 'Explosive';
                  Info.compulsory := True;
                  Info.Roles[0] := '';
                  Info.Roles[1] := '';
                  Info.Roles[2] := '';
                  Info.Roles[3] := 'Artificer';
                  for i := 4 to (MaxRoles - 1) do
                    Info.Roles[i] := '';
                  Info.Skills[0] := '';
                  Info.Skills[1] := '';
                  Info.Skills[2] := '';
                  Info.Skills[3] := 'Demolition';
                  for l := 4 to (MaxRoles - 1) do
                    Info.Skills[l] := '';
                  Info.SkillValues[0] := 0;
                  Info.SkillValues[1] := 0;
                  Info.SkillValues[2] := 0;
                  Info.SkillValues[3] := 40;
                  for j := 4 to (MaxRoles - 1) do
                    Info.SkillValues[j] := 0;
                  Info.Descriptions[0] := '';
                  Info.Descriptions[1] := '';
                  Info.Descriptions[2] := '';
                  Info.Descriptions[3] := 'Prepare and execute the explosion';
                  for k := 4 to (MaxRoles - 1) do
                    Info.Descriptions[k] := '';
                  Info.Sub := False;
                  Info.ParentOptionId := '';
                  Info.Objective := 'Road';
                  Info.Target := ttpRoad;
                  Info.MaxDistance := 100;
                  Info.ObjCoord.X := 0;
                  Info.ObjCoord.Y := 0;
                  Info.MaxRadius := 0;
                  Info.Duration := 0;
                  Info.Cost := 10000;
                  Info.Profit := 0;
                end;
              2 :
                begin
                  Info.Id := 'Destroy with:';
                  Info.way := 'Muscles';
                  Info.compulsory := True;
                  Info.Roles[0] := 'Leader';
                  Info.Roles[1] := '';
                  Info.Roles[2] := '';
                  Info.Roles[3] := '';
                  Info.Roles[4] := 'Gorilla-2';
                  Info.Roles[5] := 'Gorilla-3';
                  Info.Roles[6] := 'Gorilla-4';
                  for i := 7 to (MaxRoles - 1) do
                    Info.Roles[i] := '';
                  Info.Skills[0] := 'Leadership';
                  Info.Skills[1] := '';
                  Info.Skills[2] := '';
                  Info.Skills[3] := '';
                  Info.Skills[4] := 'Brawling';
                  Info.Skills[5] := 'Brawling';
                  Info.Skills[6] := 'Brawling';
                  for l := 7 to (MaxRoles - 1) do
                    Info.Skills[l] := '';
                  Info.SkillValues[0] := 50;
                  Info.SkillValues[1] := 0;
                  Info.SkillValues[2] := 0;
                  Info.SkillValues[3] := 0;
                  Info.SkillValues[4] := 20;
                  Info.SkillValues[5] := 20;
                  Info.SkillValues[6] := 20;
                  for j := 7 to (MaxRoles - 1) do
                    Info.SkillValues[j] := 0;
                  Info.Descriptions[0] := '';
                  Info.Descriptions[1] := '';
                  Info.Descriptions[2] := '';
                  Info.Descriptions[3] := '';
                  Info.Descriptions[4] := 'Physically destroys the road';
                  Info.Descriptions[5] := 'Physically destroys the road';
                  Info.Descriptions[6] := 'Physically destroys the road';
                  for k := 7 to (MaxRoles - 1) do
                    Info.Descriptions[k] := '';
                  Info.Sub := False;
                  Info.ParentOptionId := '';
                  Info.Objective := 'Road';
                  Info.Target := ttpRoad;
                  Info.MaxDistance := 100;
                  Info.ObjCoord.X := 0;
                  Info.ObjCoord.Y := 0;
                  Info.MaxRadius := 0;
                  Info.Duration := 2;
                  Info.Cost := 10000;
                  Info.Profit := 0;
                end;
            end;
            Result := Info;
          end
        else
          result := inherited OptionData( index );
    end;

  procedure TRoadDestroy.ResearchMission;
    begin

    end;

  procedure TRoadDestroy.StartMission;
    begin
      DestroyWith := Parameters.Way[1];
      PoliceAlerted := False;
      StartingTime := Time;
      Profit := 0;
      Change := 'yes';
      Ready := True;
    end;

  procedure TRoadDestroy.ProceedWithMission;
    begin
      if MissionTime = 0
      then
        DriveToTheRoad(20);
      if MissionTime = 1
      then
        if PoliceAlerted
        then
          PoliceArriving(NextDiff)
        else
          DestroyRoad(NextDiff);
      if MissionTime = 2
      then
        if DestroyWith = 'Explosive'
        then
          DriveAway(NextDiff);
      if MissionTime = 3
      then
        if DestroyWith <> 'Explosive'
        then
          DriveAway(NextDiff);
      if MissionTime >= 4
      then
        EndMission;
    end;

  procedure TRoadDestroy.EndMission;
    var
      i : integer;
    begin
      for i := 1 to 8 do
        if Roles.Result[i] = 'Arrested'
        then
          IllSystem.CriminalArrested(Roles.Name[i])
        else
          IllSystem.GiveChargesToCriminal(Roles.Name[i]);
      Ready := False;
      IllSystem.GiveChargesToLeader((IllSystem.FindCriminalInSystem(TRolesInMission(Roles).Name[1])).LeaderName, MetaMission.Name, StartingTime, Charges);
    end;

  procedure TRoadDestroy.DriveToTheRoad(Diff: Single);
    var
      Driver : TCrimForTask;
    begin
      Driver := GetRightCriminalForTask('Driver', 'Driving');
      case IllSystem.UseSkill(Driver.SkillVal, Diff) of
        1:
          begin
            Report.Values['DriveToTheRoad'] := Driver.Name + ' drove very fast to the road square and found an excellent spot to leave the car';
            Experience.Values['Success1'] := Driver.Name;
            Experience.Values['Skill1'] := 'Driving';
            Experience.Values['Deegre1'] := '1';
            Experience.Values['Diff1'] := FloatToStr(Diff);
            if DestroyWith = 'Explosive'
            then
              PreparationForDestruction(25)
            else
              PreparationForDestruction(45)
          end;
        2:
          begin
            Report.Values['DriveToTheRoad'] := Driver.Name + ' drove fast to the road square and found a good spot to leave the car';
            Experience.Values['Success1'] := Driver.Name;
            Experience.Values['Skill1'] := 'Driving';
            Experience.Values['Deegre1'] := '2';
            Experience.Values['Diff1'] := FloatToStr(Diff);
            if DestroyWith = 'Explosive'
            then
              PreparationForDestruction(30)
            else
              PreparationForDestruction(50)
          end;
        3:
          begin
            Report.Values['DriveToTheRoad'] := Driver.Name + ' drove to the road square but coldn''t find a good spot to leave the car';
            if DestroyWith = 'Explosive'
            then
              PreparationForDestruction(40)
            else
              PreparationForDestruction(55)
          end;
        4:
          begin
            Report.Values['DriveToTheRoad'] := Driver.Name + ' got lost and so the team arrived to the road square very late. The car was left two blocks away from the bank';
            if DestroyWith = 'Explosive'
            then
              PreparationForDestruction(50)
            else
              PreparationForDestruction(65)
          end;
      end;
    end;


  procedure TRoadDestroy.PreparationForDestruction(Diff: Single);
    var
      Leader : TCrimForTask;
    begin
      Leader := GetRightCriminalForTask('Leader', 'Leadership');
      case IllSystem.UseSkill(Leader.SkillVal, Diff) of
        1:
          begin
            Report.Values['PreparationForDestruction'] := Leader.Name + ' was able to set up the scenery for the destruction very fast and effectively';
            Experience.Values['Success2'] := Leader.Name;
            Experience.Values['Skill2'] := 'Leadership';
            Experience.Values['Deegre2'] := '1';
            Experience.Values['Diff2'] := FloatToStr(Diff);
            ControlPedestrians(20);
          end;
        2:
          begin
            Report.Values['PreparationForDestruction'] := Leader.Name + ' was able to set up the scenery for the destruction effectively';
            Experience.Values['Success2'] := Leader.Name;
            Experience.Values['Skill2'] := 'Leadership';
            Experience.Values['Deegre2'] := '2';
            Experience.Values['Diff2'] := FloatToStr(Diff);
            ControlPedestrians(30);
          end;
        3:
          begin
            Report.Values['PreparationForDestruction'] := Leader.Name + ' wasn''t able to set up the scenery for the destruction effectively';
            ControlPedestrians(40);
          end;
        4:
          begin
            Report.Values['PreparationForDestruction'] := Leader.Name + ' was disastrous at setting up the scenery for the destruction';
            ControlPedestrians(50);
          end;
      end;
    end;

  procedure TRoadDestroy.DestroyRoad(Diff: Single);
    var
      i : integer;
      j : integer;
      RolesToReass : TRolesInMission;
      crim : TCrimForTask;
      val : single;
      name1 : string;
      name2 : string;
      name3 : string;
      nameHurt : string;
      healthHurt : integer;
      rep : string;
    begin
      val := 0;
      if DestroyWith = 'Explosive'
      then
        begin
          Charges.Values['Charge1'] := 'VandalizationWithExplosives';
          Charges.Values['LeaderCharge1'] := 'VandalizationWithExplosives';
          Charges.Values['PoliceDiff1'] := IntToStr(500);
          Charges.Values['Underground1'] := IntToStr(200);
          Charges.Values['Witness1'] := IntToStr(300);
          Charges.Values['Forensic1'] := IntToStr(200);
          Charges.Values['Computer1'] := IntToStr(0);
          for i := 1 to 8 do
            if Roles.Name[i] <> ''
            then
              Charges.Values['Offender1' + IntToStr(i)] := Roles.Name[i];
          crim := GetRightCriminalForTask('Artificer', 'Demolition');
          if PoliceAlerted = False
          then
            case IllSystem.UseSkill(crim.SkillVal, Diff) of
              1:
                begin
                  Report.Values['DestroyRoad'] := crim.Name + ' made a very effective and soft explosion. None has any suspect';
                  Experience.Values['Success4'] := crim.Name;
                  Experience.Values['Skill4'] := 'Demolition';
                  Experience.Values['Deegre4'] := '1';
                  Experience.Values['Diff4'] := FloatToStr(Diff);
                  EndMission;
                end;
              2:
                begin
                  Report.Values['DestroyRoad'] := crim.Name + ' made an effective explosion. None has any suspect';
                  Experience.Values['Success4'] := crim.Name;
                  Experience.Values['Skill4'] := 'Demolition';
                  Experience.Values['Deegre4'] := '2';
                  Experience.Values['Diff4'] := FloatToStr(Diff);
                  EndMission;
                end;
              3:
                begin
                  NextDiff := 40;
                  Report.Values['DestroyRoad'] := crim.Name + ' made a very loud and souspicious explosion. A pedestrian alerted the police.';
                  PoliceAlerted := True;
                end;
              4:
                begin
                  NextDiff := 50;
                  healthHurt := IllSystem.CriminalWounded(crim.Name, 3);
                  if healthHurt = 0
                  then
                    begin
                      j := 0;
                      for i := 1 to 8 do
                        if Roles.Name[i] <> ''
                        then
                          begin
                          if Roles.Name[i] <> crim.Name
                          then
                            begin
                              j := j + 1;
                              RolesToReass.Name[j] := Roles.Name[i];
                              RolesToReass.Role[j] := Roles.Role[i];
                            end;
                          end;
                      Roles := RolesToReass;
                    end;
                  case healthHurt of
                    0: rep := 'kill himself';
                    1..20: rep := 'hurt himself really badly';
                    21..50: rep := 'hurt himself badly';
                    51..70: rep := 'hurt himself';
                  else
                    rep := 'slightly hurt himself';
                  end;
                  Report.Values['DestroyRoad'] := crim.Name + ' made a mess. The explosion was exceptionally loud and he managed to' + rep;
                  PoliceAlerted := True;
                end;
            end
          else
            case IllSystem.UseSkill(crim.SkillVal, Diff) of
              1:
                begin
                  NextDiff := 50;
                  Report.Values['DestroyRoad'] := crim.Name + ' made a very effective and soft explosion. The team was able to leave before the police arrived';
                  Experience.Values['Success4'] := crim.Name;
                  Experience.Values['Skill4'] := 'Demolition';
                  Experience.Values['Deegre4'] := '1';
                  Experience.Values['Diff4'] := FloatToStr(Diff);
                end;
              2:
                begin
                  Report.Values['DestroyRoad'] := crim.Name + ' made an effective explosion. When they were about to leave, they realized that the police had arrived on the scene';
                  Experience.Values['Success4'] := crim.Name;
                  Experience.Values['Skill4'] := 'Demolition';
                  Experience.Values['Deegre4'] := '2';
                  Experience.Values['Diff4'] := FloatToStr(Diff);
                  ShootingWithPolice(50);
                end;
              3:
                begin
                  Report.Values['DestroyRoad'] := crim.Name + ' made a very loud explosion. They were still recovering when they realized that the police had arrived on the scene';
                  ShootingWithPolice(60);
                end;
              4:
                begin
                  healthHurt := IllSystem.CriminalWounded(crim.Name, 3);
                  if healthHurt = 0
                  then
                    begin
                      j := 0;
                      for i := 1 to 8 do
                        if Roles.Name[i] <> ''
                        then
                          begin
                          if Roles.Name[i] <> crim.Name
                          then
                            begin
                              j := j + 1;
                              RolesToReass.Name[j] := Roles.Name[i];
                              RolesToReass.Role[j] := Roles.Role[i];
                            end;
                          end;
                      Roles := RolesToReass;
                    end;
                  case healthHurt of
                    0: rep := 'kill himself';
                    1..20: rep := 'hurt himself really badly';
                    21..50: rep := 'hurt himself badly';
                    51..70: rep := 'hurt himself';
                  else
                    rep := 'slightly hurt himself';
                  end;
                  Report.Values['DestroyRoad'] := crim.Name + ' made a mess. The explosion was exceptionally loud and he managed to ' + rep + '. The team was trying to realize what had gone wrong, when the police ordered them to surrender';
                  ShootingWithPolice(70);
                end;
            end;
        end
      else
        begin
          Charges.Values['Charge1'] := 'Vandalization';
          Charges.Values['LeaderCharge1'] := 'Vandalization';
          Charges.Values['PoliceDiff1'] := IntToStr(700);
          Charges.Values['Underground1'] := IntToStr(200);
          Charges.Values['Witness1'] := IntToStr(400);
          Charges.Values['Forensic1'] := IntToStr(100);
          Charges.Values['Computer1'] := IntToStr(0);
          for i := 1 to 8 do
            if Roles.Name[i] <> ''
            then
              Charges.Values['Offender1' + IntToStr(i)] := Roles.Name[i];
          for i := 2 to 4 do
            if Roles.Role[i] = ('Gorilla-' + IntToStr(i))
            then
              begin
                crim := GetRightCriminalForTask('Gorilla-' + IntToStr(i), 'Brawling');
                val := val + crim.SkillVal;
                if name1 = ''
                then
                  name1 := crim.name
                else
                  if name2 = ''
                  then
                    name2 := crim.name
                  else
                    name3 := crim.name;
              end;
              val := Round(val/3);
              if PoliceAlerted = False
              then
                case IllSystem.UseSkill(val, Diff) of
                  1:
                    begin
                      Report.Values['DestroyRoad'] := name1 + ', ' + name2 + ', ' + name3 + ' made a very effective and quick job. None has any suspect';
                      Experience.Values['Success4'] := name1;
                      Experience.Values['Skill4'] := 'Brawling';
                      Experience.Values['Deegre4'] := '1';
                      Experience.Values['Diff4'] := FloatToStr(Diff);
                      Experience.Values['Success5'] := name2;
                      Experience.Values['Skill5'] := 'Brawling';
                      Experience.Values['Deegre5'] := '1';
                      Experience.Values['Diff5'] := FloatToStr(Diff);
                      Experience.Values['Success6'] := name3;
                      Experience.Values['Skill6'] := 'Brawling';
                      Experience.Values['Deegre6'] := '1';
                      Experience.Values['Diff6'] := FloatToStr(Diff);
                      EndMission;
                    end;
                  2:
                    begin
                      Report.Values['DestroyRoad'] := name1 + ', ' + name2 + ', ' + name3 + ' made a quick job. None has any suspect';
                      Experience.Values['Success4'] := name1;
                      Experience.Values['Skill4'] := 'Brawling';
                      Experience.Values['Deegre4'] := '1';
                      Experience.Values['Diff4'] := FloatToStr(Diff);
                      Experience.Values['Success5'] := name2;
                      Experience.Values['Skill5'] := 'Brawling';
                      Experience.Values['Deegre5'] := '1';
                      Experience.Values['Diff5'] := FloatToStr(Diff);
                      Experience.Values['Success6'] := name3;
                      Experience.Values['Skill6'] := 'Brawling';
                      Experience.Values['Deegre6'] := '1';
                      Experience.Values['Diff6'] := FloatToStr(Diff);
                      EndMission;
                    end;
                  3:
                    begin
                      NextDiff := 40;
                      Report.Values['DestroyRoad'] := name1 + ', ' + name2 + ', ' + name3 + ' made a very loud job. A pedestrian was souspicious and alerted the police';
                      PoliceAlerted := True;
                    end;
                  4:
                    begin
                      NextDiff := 40;
                      case Random(2) of
                        0: nameHurt := name1;
                        1: nameHurt := name2;
                        2: nameHurt := name3;
                      end;
                      IllSystem.CriminalWounded(crim.Name, 3);
                      Report.Values['DestroyRoad'] := name1 + ', ' + name2 + ', ' + name3 + ' made a mess. ' + nameHurt + ' managed to get %s. A lot of people realized that something was wrong and alerted the police';
                      healthHurt := IllSystem.CriminalWounded(nameHurt, 1);
                      if healthHurt = 0
                      then
                        begin
                          j := 0;
                          for i := 1 to 8 do
                            if Roles.Name[i] <> ''
                            then
                              begin
                              if Roles.Name[i] <> nameHurt
                              then
                                begin
                                  j := j + 1;
                                  RolesToReass.Name[j] := Roles.Name[i];
                                  RolesToReass.Role[j] := Roles.Role[i];
                                end;
                              end;
                          Roles := RolesToReass;
                        end;
                      case healthHurt of
                        0: rep := 'killed';
                        1..20: rep := 'hurt really badly';
                        21..50: rep := 'hurt badly';
                        51..70: rep := 'hurt';
                      else
                        rep := 'slightly hurt';
                      end;
                      Report.Values['DestroyRoad'] := name1 + ', ' + name2 + ', ' + name3 + ' made a mess. ' + nameHurt + ' managed to get ' + rep + '. A lot of people realized that something was wrong and alerted the police';
                      PoliceAlerted := True;
                    end;
                end
              else
                case IllSystem.UseSkill(val, Diff) of
                  1:
                    begin
                      NextDiff := 30;
                      Report.Values['DestroyRoad'] := name1 + ', ' + name2 + ', ' + name3 + ' made a very effective and quick job. The team was able to leave before the police arrived';
                      Experience.Values['Success4'] := name1;
                      Experience.Values['Skill4'] := 'Brawling';
                      Experience.Values['Deegre4'] := '1';
                      Experience.Values['Diff4'] := FloatToStr(Diff);
                      Experience.Values['Success5'] := name2;
                      Experience.Values['Skill5'] := 'Brawling';
                      Experience.Values['Deegre5'] := '1';
                      Experience.Values['Diff5'] := FloatToStr(Diff);
                      Experience.Values['Success6'] := name3;
                      Experience.Values['Skill6'] := 'Brawling';
                      Experience.Values['Deegre6'] := '1';
                      Experience.Values['Diff6'] := FloatToStr(Diff);
                    end;
                  2:
                    begin
                      Report.Values['DestroyRoad'] := name1 + ', ' + name2 + ', ' + name3 + ' made a quick job. When they were about to leave, they realized that the police had arrived on the scene';
                      Experience.Values['Success4'] := name1;
                      Experience.Values['Skill4'] := 'Brawling';
                      Experience.Values['Deegre4'] := '1';
                      Experience.Values['Diff4'] := FloatToStr(Diff);
                      Experience.Values['Success5'] := name2;
                      Experience.Values['Skill5'] := 'Brawling';
                      Experience.Values['Deegre5'] := '1';
                      Experience.Values['Diff5'] := FloatToStr(Diff);
                      Experience.Values['Success6'] := name3;
                      Experience.Values['Skill6'] := 'Brawling';
                      Experience.Values['Deegre6'] := '1';
                      Experience.Values['Diff6'] := FloatToStr(Diff);
                      ShootingWithPolice(50);
                    end;
                  3:
                    begin
                      Report.Values['DestroyRoad'] := name1 + ', ' + name2 + ', ' + name3 + ' made a very loud job and they took a long time. While they were still finishing the job, they realized that the police had arrived on the scene';
                      ShootingWithPolice(60);
                    end;
                  4:
                    begin
                      case Random(2) of
                        0: nameHurt := name1;
                        1: nameHurt := name2;
                        2: nameHurt := name3;
                      end;
                      IllSystem.CriminalWounded(crim.Name, 3);
                      healthHurt := IllSystem.CriminalWounded(nameHurt, 1);
                      if healthHurt = 0
                      then
                        begin
                          j := 0;
                          for i := 1 to 8 do
                            if Roles.Name[i] <> ''
                            then
                              begin
                              if Roles.Name[i] <> nameHurt
                              then
                                begin
                                  j := j + 1;
                                  RolesToReass.Name[j] := Roles.Name[i];
                                  RolesToReass.Role[j] := Roles.Role[i];
                                end;
                              end;
                          Roles := RolesToReass;
                        end;
                      case healthHurt of
                        0: rep := 'killed';
                        1..20: rep := 'hurt really badly';
                        21..50: rep := 'hurt badly';
                        51..70: rep := 'hurt';
                      else
                        rep := 'slightly hurt';
                      end;
                      Report.Values['DestroyRoad'] := name1 + ', ' + name2 + ', ' + name3 + ' made a mess. ' + nameHurt + ' managed to get ' + rep + '. While they were checking on him, they heard the police telling them to surrender';
                      ShootingWithPolice(70);
                    end;
                end;
        end;
    end;

  procedure TRoadDestroy.ControlPedestrians(Diff: Single);
    var
      Gorilla : TCrimForTask;
    begin
      Gorilla := GetRightCriminalForTask('Gorilla-1', 'Brawling');
      if DestroyWith = 'Explosive'
      then
        case IllSystem.UseSkill(Gorilla.SkillVal, Diff) of
          1:
            begin
              NextDiff := 30;
              Report.Values['ControlPedestrians'] := Gorilla.Name + ' was able to stop any courious very effectively. Everything is ready for the explosion';
              Experience.Values['Success3'] := Gorilla.Name;
              Experience.Values['Skill3'] := 'Brawling';
              Experience.Values['Deegre3'] := '1';
              Experience.Values['Diff3'] := FloatToStr(Diff);
            end;
          2:
            begin
              NextDiff := 40;
              Report.Values['ControlPedestrians'] := Gorilla.Name + ' was able to stop any courious. Everything is ready for the explosion';
              Experience.Values['Success3'] := Gorilla.Name;
              Experience.Values['Skill3'] := 'Brawling';
              Experience.Values['Deegre3'] := '2';
              Experience.Values['Diff3'] := FloatToStr(Diff);
            end;
          3:
            begin
              NextDiff := 40;
              Report.Values['ControlPedestrians'] := Gorilla.Name + ' had some problem controlling the courious. One of them alerted the police';
              PoliceAlerted := True;
            end;
          4:
            begin
              NextDiff := 50;
              Report.Values['ControlPedestrians'] := Gorilla.Name + ' had a lot of problems controlling the courious. They alerted the police';
              PoliceAlerted := True;
            end;
        end
      else
        case IllSystem.UseSkill(Gorilla.SkillVal, Diff) of
          1:
            begin
              NextDiff := 10;
              Report.Values['ControlPedestrians'] := Gorilla.Name + ' was able to stop any courious very effectively. Everything is ready for demolition';
              Experience.Values['Success3'] := Gorilla.Name;
              Experience.Values['Skill3'] := 'Brawling';
              Experience.Values['Deegre3'] := '1';
              Experience.Values['Diff3'] := FloatToStr(Diff);
            end;
          2:
            begin
              NextDiff := 20;
              Report.Values['ControlPedestrians'] := Gorilla.Name + ' was able to stop any courious. Everything is ready for demolition';
              Experience.Values['Success3'] := Gorilla.Name;
              Experience.Values['Skill3'] := 'Brawling';
              Experience.Values['Deegre3'] := '2';
              Experience.Values['Diff3'] := FloatToStr(Diff);
            end;
          3:
            begin
              NextDiff := 40;
              Report.Values['ControlPedestrians'] := Gorilla.Name + ' had some problem controlling the courious. One of them alerted the police';
              PoliceAlerted := True;
            end;
          4:
            begin
              NextDiff := 50;
              Report.Values['ControlPedestrians'] := Gorilla.Name + ' had a lot of problems controlling the courious. They alerted the police';
              PoliceAlerted := True;
            end;
        end;
    end;

  procedure TRoadDestroy.PoliceArriving(Diff: Single);
    var
      Leader : TCrimForTask;
    begin
      Leader := GetRightCriminalForTask('Leader', 'Leadership');
      case IllSystem.UseSkill(Leader.SkillVal, Diff) of
        1:
          begin
            nextDiff := 20;
            Report.Values['PoliceArriving'] := Leader.Name + ' realized quickly that the task can''t be accomplished anymore with the imminent arriving of the cops. The team leave the scene';
            Experience.Values['Success7'] := Leader.Name;
            Experience.Values['Skill7'] := 'Leadership';
            Experience.Values['Deegre7'] := '1';
            Experience.Values['Diff7'] := FloatToStr(Diff);
          end;
        2:
          begin
            nextDiff := 30;
            Report.Values['PoliceArriving'] := Leader.Name + ' realized that the task can''t be accomplished anymore with the imminent arriving of the cops. The team leave the scene';
            Experience.Values['Success7'] := Leader.Name;
            Experience.Values['Skill7'] := 'Leadership';
            Experience.Values['Deegre7'] := '2';
            Experience.Values['Diff7'] := FloatToStr(Diff);
          end;
        3:
          begin
            Report.Values['PoliceArriving'] := Leader.Name + ' decided to go for the destruction anyway, even if the police was arriving';
            if DestroyWith = 'Explosive'
            then
              DestroyRoad(45)
            else
              DestroyRoad(25);
          end;
        4:
          begin
            Report.Values['PoliceArriving'] := Leader.Name + ' didn''t realize that someone had alerted the police and the team proceeded to the destruction of the road';
            if DestroyWith = 'Explosive'
            then
              DestroyRoad(55)
            else
              DestroyRoad(30);
          end;
      end;
    end;

  procedure TRoadDestroy.ShootingWithPolice(Diff: Single);
    var
      i : integer;
      j : integer;
      crim : TCrimForTask;
      rep : string;
      RolesToReass : TRolesInMission;
      hurtLev : integer;
      healthHurt : integer;
    begin
      rep := 'A shooting started. ';
      j := 0;
      for i := 1 to 8 do
        if Roles.Name[i] <> ''
        then
          begin
            crim := GetRightCriminalForTask(Roles.Role[i], 'Firearms');
            case IllSystem.UseSkill(crim.SkillVal, Diff) of
            1:
              begin
                rep := rep + crim.Name + ' was outstanding with his weapon and managed to escape without a scratch';
                if Random(2) = 0
                then
                  begin
                    rep := rep + ' and killed a policeman in the process';
                    Charges.Values['Charge2'] := 'Murder2';
                    Charges.Values['Offender21'] := crim.Name;
                    Charges.Values['PoliceDiff2'] := 'Charge1';
                  end
                else
                  begin
                    rep := rep + ' and hurt a policeman in the process';
                    Charges.Values['Charge2'] := 'AttemptedMurder';
                    Charges.Values['Offender21'] := crim.Name;
                    Charges.Values['PoliceDiff2'] := 'Charge1';
                  end;
                j := j + 1;
                RolesToReass.Name[j] := Roles.Name[i];
                RolesToReass.Role[j] := Roles.Role[i];
                RolesToReass.Result[j] := 'Escaped';
                Experience.Values['Success' + IntToStr(9 + i)] := crim.Name;
                Experience.Values['Skill' + IntToStr(9 + i)] := 'Firearms';
                Experience.Values['Deegre' + IntToStr(9 + i)] := '1';
                Experience.Values['Diff' + IntToStr(9 + i)] := FloatToStr(Diff);
              end;
            2:
              begin
                rep := rep + crim.Name + ' was good with his weapon';
                hurtLev := Random(2) + 1;
                healthHurt := IllSystem.CriminalWounded(Roles.Name[i], hurtLev);
                if healthHurt <> 0
                then
                  begin
                    rep := rep + '. ';
                    j := j + 1;
                    RolesToReass.Name[j] := Roles.Name[i];
                    RolesToReass.Role[j] := Roles.Role[i];
                    RolesToReass.Result[j] := 'Escaped';
                  end;
                case healthHurt of
                  0: rep := rep + ', unfortunately, though, was killed';
                  1..20: rep := rep + ', unfortunately, though, was hurt really badly';
                  21..50: rep := rep + ', unfortunately, though, was hurt badly';
                  51..70: rep := rep + ', unfortunately, though, was hurt';
                else
                  rep := rep + ', unfortunately, though, was slightly hurt';
                end;
                if Random(2) = 0
                then
                  begin
                    rep := rep + ' but hurt a policeman in the process and managed to escape';
                    Charges.Values['Charge2'] := 'AttemptedMurder';
                    Charges.Values['Offender21'] := crim.Name;
                    Charges.Values['PoliceDiff2'] := 'Charge1';
                  end
                else
                  rep := rep + ' but managed to escape';
                Experience.Values['Success' + IntToStr(9 + i)] := crim.Name;
                Experience.Values['Skill' + IntToStr(9 + i)] := 'Firearms';
                Experience.Values['Deegre' + IntToStr(9 + i)] := '2';
                Experience.Values['Diff' + IntToStr(9 + i)] := FloatToStr(Diff);
              end;
            3:
              begin
                rep := rep + crim.Name + ' wasn''t good with his weapon, ';
                hurtLev := Random(2) + 1;
                healthHurt := IllSystem.CriminalWounded(Roles.Name[i], hurtLev);
                if healthHurt <> 0
                then
                  begin
                    j := j + 1;
                    RolesToReass.Name[j] := Roles.Name[i];
                    RolesToReass.Role[j] := Roles.Role[i];
                    RolesToReass.Result[j] := 'Arrested';
                  end;
                case healthHurt of
                  0: rep := rep + ' and was killed';
                  1..20: rep := rep  + ' was hurt really badly and arrested ';
                  21..50: rep := rep  + ' was hurt badly and arrested ';
                  51..70: rep := rep  + ' was hurt and arrested ';
                else
                  rep := rep + ' was slightly hurt and arrested ';
                end;
                if Random(2) = 0
                then
                  begin
                    rep := rep + ' but, at least, hurt a policeman in the process';
                    Charges.Values['Charge2'] := 'AttemptedMurder';
                    Charges.Values['Offender21'] := crim.Name;
                    Charges.Values['PoliceDiff2'] := 'Charge1';
                  end;
              end;
            4:
              begin
                rep := rep + crim.Name + ' was awful in the shooting, ';
                hurtLev := Random(2) + 1;
                healthHurt := IllSystem.CriminalWounded(Roles.Name[i], hurtLev);
                if healthHurt <> 0
                then
                  begin
                    j := j + 1;
                    RolesToReass.Name[j] := Roles.Name[i];
                    RolesToReass.Role[j] := Roles.Role[i];
                    RolesToReass.Result[j] := 'Arrested';
                  end;
                case healthHurt of
                  0: rep := rep + ' and was killed';
                  1..20: rep := rep + 'was hurt really badly and arrested';
                  21..50: rep := rep + 'was hurt badly and arrested';
                  51..70: rep := rep + 'was hurt and arrested';
                else
                  rep := rep + 'was slightly hurt and arrested';
                end;
              end;
            end;
            if Roles.Name[i+1] <> ''
            then
              rep := rep + '. ';
          end;
      Roles := RolesToReass;
      Report.Values['ShootingWithPolice'] := rep;
      EndMission;
    end;

  procedure TRoadDestroy.DriveAway(Diff: Single);
    var
      Driver : TCrimForTask;
    begin
      Driver := GetRightCriminalForTask('Driver', 'Driving');
      case IllSystem.UseSkill(Driver.SkillVal, Diff) of
        1:
          begin
            Report.Values['DriveAway'] := Driver.Name + ' drove very fast and avoided any police car. The team arrived safely at the Headquarter';
            Experience.Values['Success8'] := Driver.Name;
            Experience.Values['Skill8'] := 'Driving';
            Experience.Values['Deegre8'] := '1';
            Experience.Values['Diff8'] := FloatToStr(Diff);
            EndMission;
          end;
        2:
          begin
            Report.Values['DriveAway'] := Driver.Name + ' drove fast and the team arrived safely at the Headquarter';
            Experience.Values['Success8'] := Driver.Name;
            Experience.Values['Skill8'] := 'Driving';
            Experience.Values['Deegre8'] := '2';
            Experience.Values['Diff8'] := FloatToStr(Diff);
            EndMission;
          end;
        3:
          begin
            Report.Values['DriveAway'] := Driver.Name + ' stumbled in a police car. The cops immediately started following them';
            CarChase(50);
          end;
        4:
          begin
            Report.Values['DriveAway'] := Driver.Name + ' stumbled in two police cars that were arriving on the site. The cops started a chase';
            CarChase(60);
          end;
      end;
    end;

  procedure TRoadDestroy.CarChase(Diff: Single);
    var
      i : integer;
      j : integer;
      Driver : TCrimForTask;
      hurtLev : integer;
      healthHurt : integer;
      rep : string;
      RolesToReass : TRolesInMission;
    begin
      Driver := GetRightCriminalForTask('Driver', 'Driving');
      case IllSystem.UseSkill(Driver.SkillVal, Diff) of
        1:
          begin
            Report.Values['CarChase'] := 'The team car, drived by ' + Driver.Name + ' was able to immediately flee from the cops. The team arrived at the Headquarter with nobody following them';
            Experience.Values['Success9'] := Driver.Name;
            Experience.Values['Skill9'] := 'Driving';
            Experience.Values['Deegre9'] := '1';
            Experience.Values['Diff9'] := FloatToStr(Diff);
            EndMission;
          end;
        2:
          begin
            Report.Values['CarChase'] := 'The team car, drived by ' + Driver.Name + ' was able to flee from the cops. The team arrived at the Headquarter with nobody following them';
            Experience.Values['Success9'] := Driver.Name;
            Experience.Values['Skill9'] := 'Driving';
            Experience.Values['Deegre9'] := '2';
            Experience.Values['Diff9'] := FloatToStr(Diff);
            EndMission;
          end;
        3:
          begin
            Report.Values['CarChase'] := Driver.Name + ' wasn''t able to drop the police. The chase ended with the team surrounded by the police';
            SackedIn(50);
          end;
        4:
          begin
            rep := 'In the accident ';
            j := 0;
            for i := 1 to 8 do
              if Roles.Name[i] <> ''
              then
                begin
                  hurtLev := Random(6);
                  if hurtLev <> 0
                  then
                    begin
                      healthHurt := IllSystem.CriminalWounded(Roles.Name[i], hurtLev);
                      if healthHurt <> 0
                      then
                        begin
                          j := j + 1;
                          RolesToReass.Name[j] := Roles.Name[i];
                          RolesToReass.Role[j] := Roles.Role[i];
                        end;
                      case healthHurt of
                        0: rep := rep + Roles.Name[i] + ' was killed';
                        1..20: rep := rep + Roles.Name[i] + ' was hurt really badly';
                        21..50: rep := rep + Roles.Name[i] + ' was hurt badly';
                        51..70: rep := rep + Roles.Name[i] + ' was hurt';
                      else
                        rep := rep + Roles.Name[i] + ' was slightly hurt';
                      end;
                      if Roles.Name[i+1] <> ''
                      then
                        rep := rep + ', ';
                    end
                  else
                    begin
                      j := j + 1;
                      RolesToReass.Name[j] := Roles.Name[i];
                      RolesToReass.Role[j] := Roles.Role[i];
                    end;
                end;
            Roles := RolesToReass;
            Report.Values['CarChase'] := Driver.Name + ' wasn''t able to drop the police. The chase ended with the car crashed. ' + rep + ' and the team surrounded by the police';
            SackedIn(60);
          end;
      end;
    end;

  procedure TRoadDestroy.SackedIn(Diff: Single);
    var
      Leader : TCrimForTask;
    begin
      Leader := GetRightCriminalForTask('Leader', 'Leadership');
      case IllSystem.UseSkill(Leader.SkillVal, Diff) of
        1:
          begin
            Report.Values['SackedIn'] := Leader.Name + ' took immediately the control of the situation. He deployed the team very well to face the police';
            Experience.Values['Success18'] := Leader.Name;
            Experience.Values['Skill18'] := 'Leadership';
            Experience.Values['Deegre18'] := '1';
            Experience.Values['Diff18'] := FloatToStr(Diff);
            ShootingWithPolice(60);
          end;
        2:
          begin
            Report.Values['SackedIn'] := Leader.Name + ' took control of the situation and organized the team to face the police';
            Experience.Values['Success18'] := Leader.Name;
            Experience.Values['Skill18'] := 'Leadership';
            Experience.Values['Deegre18'] := '2';
            Experience.Values['Diff18'] := FloatToStr(Diff);
            ShootingWithPolice(80);
          end;
        3:
          begin
            Report.Values['SackedIn'] := Leader.Name + ' wasn''t able to organize a resistance to the police and the team surrendered';
            Surrender;
          end;
        4:
          begin
            Report.Values['SackedIn'] := Leader.Name + ' was caught completely by surprise and the team was arrested and beated up by the police';
            ArrestedWithInjuries;
          end;
      end;
    end;

  procedure TRoadDestroy.ArrestedWithInjuries;
    var
      i : integer;
      j : integer;
      k : integer;
      rep : string;
      RolesToReass : TRolesInMission;
      hurtLev : integer;
      healthHurt : integer;
    begin
      rep := '. ';
      j := 0;
      for i := 1 to 8 do
        if Roles.Name[i] <> ''
        then
          begin
            hurtLev := Random(3);
            if hurtLev <> 0
            then
              begin
                healthHurt := IllSystem.CriminalWounded(Roles.Name[i], hurtLev);
                if healthHurt <> 0
                then
                  begin
                    j := j + 1;
                    RolesToReass.Name[j] := Roles.Name[i];
                    RolesToReass.Role[j] := Roles.Role[i];
                    RolesToReass.Result[j] := 'Arrested';
                  end;
                case healthHurt of
                  0: rep := rep + Roles.Name[i] + ' was killed';
                  1..20: rep := rep + Roles.Name[i] + ' was hurt really badly';
                  21..50: rep := rep + Roles.Name[i] + ' was hurt badly';
                  51..70: rep := rep + Roles.Name[i] + ' was hurt';
                else
                  rep := rep + Roles.Name[i] + ' was slightly hurt';
                end;
                if Roles.Name[i+1] <> ''
                then
                  rep := rep + ', ';
              end
            else
              begin
                j := j + 1;
                RolesToReass.Name[j] := Roles.Name[i];
                RolesToReass.Role[j] := Roles.Role[i];
                RolesToReass.Result[j] := 'Arrested';
              end;
          end;
      Report.Values['ArrestedWithInjuries'] := rep;
      Roles := RolesToReass;
      for k := 1 to MaxCrimes do
        Charges.Values['PoliceDiff' + IntToStr(k)] := '0';
      EndMission;
    end;

  procedure TRoadDestroy.Surrender;
    var
      i : integer;
      k : integer;
      RolesToReass : TRolesInMission;
    begin
      for k := 1 to MaxCrimes do
        Charges.Values['PoliceDiff' + IntToStr(k)] := '0';
      for i := 1 to 8 do
        if Roles.Name[i] <> ''
        then
          begin
            RolesToReass.Name[i] := Roles.Name[i];
            RolesToReass.Role[i] := Roles.Role[i];
            RolesToReass.Result[i] := 'Arrested';
          end;
      Roles := RolesToReass;
      EndMission;
    end;

end.
