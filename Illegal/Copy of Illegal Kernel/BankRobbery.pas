unit BankRobbery;

interface

  uses
    SysUtils, IllegalKernel, Collection, ClassStorage;

  type
    TBankRobbery =
      class( TMission )
        protected
          constructor Create( aOwner : TTeam; aMetaMission : TMetaMission ); override;
        private
          Hacker               : boolean;
          Vault                : string;
          AlarmState           : integer;
          AfterInTheBank       : string;
          BeforeDeadlyIncident : string;
          Hacker1Result        : integer;
          Hacker1Name          : string;
          ReadyToLeave         : string;
          NegDiff              : single;
        public
          class function OptionData( index : integer ) : TMissionOptionInfo; override;
        public
          procedure StartMission; override;
          procedure ResearchMission;
          procedure ProceedWithMission; override;
          procedure EndMission; override;
          procedure PreliminaryHacking(Diff : single);
          procedure DrivingToTheBank(Diff : single);
          procedure InTheBank(Diff : single);
          procedure DrivingAway(Diff : single);
          procedure AlarmPressed(LeadRes : integer);
          procedure OpenVaultExpl(Diff : single);
          procedure OpenVaultComp(Diff : single);
          procedure SackedIn(Diff : single);
          procedure CarChase(Diff : single);
          procedure ManageHostages(Diff : single);
          procedure ControlViolentOutburst(Diff : single);
          procedure DeadlyIncident(Diff : single);
          function FirearmTable(Diff : single): boolean;
          procedure PoliceArriving(Diff : single);
          procedure HostageStandOff(Diff : single);
          procedure NegotiationProceeding(Diff : single);
          procedure PoliceBreakIn(Diff : single);
          procedure Surrender(Diff : single);
          procedure TeamDistressedEscape(Diff : single);
          procedure ShootingCars(Diff : single);
          procedure ArrestedWithInjuries(Diff : single);
      end;

  const
    MaxCrimes = 4;
    cBankRobberyOptions = 4;
    cComputer1Values = 1;
    cDriving1Values = 2;

implementation

  constructor TBankRobbery.Create( aOwner : TTeam; aMetaMission : TMetaMission );
    begin
      inherited;
    end;

  class function TBankRobbery.OptionData( index : integer ) : TMissionOptionInfo;
    var
      i : integer;
      j : integer;
      k : integer;
      l : integer;
      Info : TMissionOptionInfo;
    begin
      if index < cBankRobberyOptions
        then
          begin
            case index of
              0 :
                begin
                  Info.Id := 'NoOption';
                  Info.way := '';
                  Info.Roles[0] := 'Leader';
                  Info.Roles[1] := 'Driver';
                  Info.Roles[2] := 'Driver';
                  for i := 3 to (MaxRoles - 1) do
                    Info.Roles[i] := '';
                  Info.Skills[0] := 'Leadership';
                  Info.Skills[1] := 'Driving';
                  Info.Skills[2] := 'Driving';
                  for l := 3 to (MaxRoles - 1) do
                    Info.Skills[l] := '';
                  Info.SkillValues[0] := 60;
                  Info.SkillValues[1] := 30;
                  Info.SkillValues[2] := 40;
                  for j := 3 to (MaxRoles - 1) do
                    Info.SkillValues[j] := 0;
                  Info.Descriptions[0] := 'Deal with the people in the bank';
                  Info.Descriptions[1] := 'Driving to and away from the bank';
                  Info.Descriptions[2] := 'Driving to and away from the bank';
                  for k := 3 to (MaxRoles - 1) do
                    Info.Descriptions[k] := '';
                  Info.Sub := False;
                  Info.ParentOptionId := '';
                  Info.Objective := 'Bank';
                  Info.ObjCoord.X := 0;
                  Info.ObjCoord.Y := 0;
                  Info.Duration := 3;
                  Info.Cost := 5000;
                  Info.Profit := 100000;
                end;
              1 :
                begin
                  Info.Id := 'Use Hacker';
                  Info.way := '';
                  Info.Roles[0] := '';
                  Info.Roles[1] := 'Driver';
                  Info.Roles[2] := '';
                  Info.Roles[3] := 'Hacker-1';
                  for i := 4 to (MaxRoles - 1) do
                    Info.Roles[i] := '';
                  Info.Skills[0] := '';
                  Info.Skills[1] := 'Driving';
                  Info.Skills[2] := '';
                  Info.Skills[3] := 'Computer';
                  for l := 4 to (MaxRoles - 1) do
                    Info.Skills[l] := '';
                  Info.SkillValues[0] := 0;
                  Info.SkillValues[1] := 35;
                  Info.SkillValues[2] := 0;
                  Info.SkillValues[3] := 80;
                  for j := 4 to (MaxRoles - 1) do
                    Info.SkillValues[j] := 0;
                  Info.Descriptions[0] := '';
                  Info.Descriptions[1] := '';
                  Info.Descriptions[2] := '';
                  Info.Descriptions[3] := 'Stop bank alarms from the Headquarter';
                  for k := 4 to (MaxRoles - 1) do
                    Info.Descriptions[k] := '';
                  Info.Sub := False;
                  Info.ParentOptionId := '';
                  Info.Objective := '';
                  Info.ObjCoord.X := 0;
                  Info.ObjCoord.Y := 0;
                  Info.Duration := 0;
                  Info.Cost := 10000;
                  Info.Profit := 0;
                end;
              2 :
                begin
                  Info.Id := 'Open Vault';
                  Info.way := 'Explosive';
                  Info.Roles[0] := 'Leader';
                  Info.Roles[1] := '';
                  Info.Roles[2] := 'Driver';
                  Info.Roles[3] := '';
                  Info.Roles[4] := 'Artificer';
                  for i := 5 to (MaxRoles - 1) do
                    Info.Roles[i] := '';
                  Info.Skills[0] := 'Leadership';
                  Info.Skills[1] := '';
                  Info.Skills[2] := 'Driving';
                  Info.Skills[3] := '';
                  Info.Skills[4] := 'Demolition';
                  for l := 5 to (MaxRoles - 1) do
                    Info.Skills[l] := '';
                  Info.SkillValues[0] := 70;
                  Info.SkillValues[1] := 0;
                  Info.SkillValues[2] := 50;
                  Info.SkillValues[3] := 0;
                  Info.SkillValues[4] := 60;
                  for j := 5 to (MaxRoles - 1) do
                    Info.SkillValues[j] := 0;
                  Info.Descriptions[0] := 'Deal with people, plan the breaking of the vault';
                  Info.Descriptions[1] := '';
                  Info.Descriptions[2] := '';
                  Info.Descriptions[3] := '';
                  Info.Descriptions[4] := 'Break vault with explosives';
                  for k := 5 to (MaxRoles - 1) do
                    Info.Descriptions[k] := '';
                  Info.Sub := False;
                  Info.ParentOptionId := '';
                  Info.Objective := '';
                  Info.ObjCoord.X := 0;
                  Info.ObjCoord.Y := 0;
                  Info.Duration := 1;
                  Info.Cost := 20000;
                  Info.Profit := 2000000;
                end;
              3 :
                begin
                  Info.Id := 'Open Vault';
                  Info.way := 'Computer';
                  Info.Roles[0] := 'Leader';
                  Info.Roles[1] := '';
                  Info.Roles[2] := 'Driver';
                  Info.Roles[3] := '';
                  Info.Roles[4] := '';
                  Info.Roles[5] := 'Hacker-2';
                  for i := 6 to (MaxRoles - 1) do
                    Info.Roles[i] := '';
                  Info.Skills[0] := 'Leadership';
                  Info.Skills[1] := '';
                  Info.Skills[2] := 'Driving';
                  Info.Skills[3] := '';
                  Info.Skills[4] := '';
                  Info.Skills[5] := 'Computer';
                  for l := 6 to (MaxRoles - 1) do
                    Info.Skills[l] := '';
                  Info.SkillValues[0] := 70;
                  Info.SkillValues[1] := 0;
                  Info.SkillValues[2] := 50;
                  Info.SkillValues[3] := 0;
                  Info.SkillValues[4] := 0;
                  Info.SkillValues[5] := 80;
                  for j := 6 to (MaxRoles - 1) do
                    Info.SkillValues[j] := 0;
                  Info.Descriptions[0] := 'Deal with people, supervise the opening of the vault';
                  Info.Descriptions[1] := '';
                  Info.Descriptions[2] := '';
                  Info.Descriptions[3] := '';
                  Info.Descriptions[4] := '';
                  Info.Descriptions[5] := 'Open vault using computers';
                  for k := 6 to (MaxRoles - 1) do
                    Info.Descriptions[k] := '';
                  Info.Sub := False;
                  Info.ParentOptionId := '';
                  Info.Objective := '';
                  Info.ObjCoord.X := 0;
                  Info.ObjCoord.Y := 0;
                  Info.Duration := 1;
                  Info.Cost := 5000;
                  Info.Profit := 2000000;
                end;
            end;
            Result := Info;
          end
        else
          result := inherited OptionData( index );
    end;

  procedure TBankRobbery.ResearchMission;
    begin

    end;

  procedure TBankRobbery.StartMission;
    begin
      Vault := Parameters.Way[2];
      Hacker := Parameters.Value[1];
      StartingTime := Time;
      Profit := 0;
      Change := 'yes';
      AlarmState := 1;
      AfterInTheBank := '';
      Hacker1Result := 0;
      Ready := True;
      NegDiff := 0;
      ReadyToLeave := 'yes';
    end;

  procedure TBankRobbery.ProceedWithMission;
    begin
      if MissionTime = 0
      then
        if Hacker
        then
          begin
            PreliminaryHacking(80)
          end
        else
          DrivingToTheBank(30);
      if MissionTime = 1
      then
        InTheBank(NextDiff);
      if MissionTime = 2
      then
        if Vault = ''
        then
          if AfterInTheBank = 'DrivingAway'
          then
            DrivingAway(NextDiff)
          else
            HostageStandOff(NextDiff);
      if MissionTime = 3
      then
        if Vault <> ''
        then
          begin
            if AfterInTheBank = 'DrivingAway'
            then
              DrivingAway(NextDiff)
            else
              HostageStandOff(NextDiff);
          end
        else
          if ReadyToLeave = 'yes'
          then
            EndMission;
      if MissionTime >= 4
      then
        if ReadyToLeave = 'yes'
          then
            EndMission;
    end;

  procedure TBankRobbery.EndMission;
    begin
      Surrender(50);
      change := 'no';
      Ready := False;
    end;

  procedure TBankRobbery.PreliminaryHacking(Diff : single);
    var
      Hacker1 : TCrimForTask;
    begin
      Hacker1 := GetRightCriminalForTask('Hacker-1', 'Computer');
      Hacker1Result := IllSystem.UseSkill(Hacker1.SkillVal, Diff);
      Hacker1Name := Hacker1.Name;
      case Hacker1Result of
        1:
          begin
            Report.Values['PreliminaryHacking'] := Hacker1.Name + ' completed the hacking of the bank computers to disconnect surveillance mechanisms when the team arrives at the bank. He thinks he nailed the system.';
            Charges.Values['Charge1'] := 'InformaticCrime';
            Charges.Values['Offender11'] := Hacker1Name;
            Charges.Values['PoliceDiff1'] := IntToStr(600);
            Charges.Values['Underground1'] := IntToStr(100);
            Charges.Values['Witness1'] := IntToStr(0);
            Charges.Values['Forensic1'] := IntToStr(0);
            Charges.Values['Computer1'] := IntToStr(600);
            DrivingToTheBank(20);
          end;
        2:
          begin
            Report.Values['PreliminaryHacking'] := Hacker1.Name + ' completed the hacking of the bank computers to disconnect surveillance mechanisms when the team arrives in the bank. He is confident about the result.';
            Charges.Values['Charge1'] := 'InformaticCrime';
            Charges.Values['Offender11'] := Hacker1Name;
            Charges.Values['PoliceDiff1'] := IntToStr(400);
            Charges.Values['Underground1'] := IntToStr(150);
            Charges.Values['Witness1'] := IntToStr(0);
            Charges.Values['Forensic1'] := IntToStr(0);
            Charges.Values['Computer1'] := IntToStr(400);
            DrivingToTheBank(35);
          end;
        3:
          begin
            Report.Values['PreliminaryHacking'] := Hacker1.Name + ' completed the hacking of the bank computers to disconnect surveillance mechanisms when the team arrives in the bank. He is not sure if he was able to get it right.';
            Charges.Values['Charge1'] := 'InformaticCrime';
            Charges.Values['Offender11'] := Hacker1Name;
            Charges.Values['PoliceDiff1'] := IntToStr(300);
            Charges.Values['Underground1'] := IntToStr(200);
            Charges.Values['Witness1'] := IntToStr(0);
            Charges.Values['Forensic1'] := IntToStr(0);
            Charges.Values['Computer1'] := IntToStr(300);
            DrivingToTheBank(45);
          end;
        4:
          begin
            Report.Values['PreliminaryHacking'] := Hacker1.Name + ' wasn''t able to complete the hacking of the bank computers. The team will not have any informatic help.';
            DrivingToTheBank(45);
          end;
      end;
    end;


  procedure TBankRobbery.DrivingToTheBank(Diff : single);
    var
      Driver : TCrimForTask;
    begin
      Driver := GetRightCriminalForTask('Driver', 'Driving');
      case IllSystem.UseSkill(Driver.SkillVal, Diff) of
        1:
          begin
            NextDiff := 45;
            Report.Values['DrivingToTheBank'] := Driver.Name + ' drove very fast to the bank and found an excellent spot to leave the car';
          end;
        2:
          begin
            NextDiff := 60;
            Report.Values['DrivingToTheBank'] := Driver.Name + ' drove fast to the bank and found a good spot to leave the car';
          end;
        3:
          begin
            NextDiff := 70;
            Report.Values['DrivingToTheBank'] := Driver.Name + ' drove to the bank but coldn''t find a good spot to leave the car';
          end;
        4:
          begin
            NextDiff := 85;
            Report.Values['DrivingToTheBank'] := Driver.Name + ' got lost and so the team arrived to the bank very late on the schedule. The car was left two blocks away from the bank';
          end;
      end;
    end;

  procedure TBankRobbery.InTheBank(Diff : single);
    var
      i : integer;
      Leader : TCrimForTask;
    begin
      Leader := GetRightCriminalForTask('Leader', 'Leadership');
      Charges.Values['Charge2'] := 'BankRobbery';
      Charges.Values['PoliceDiff2'] := IntToStr(500);
      Charges.Values['Underground2'] := IntToStr(100);
      Charges.Values['Witness2'] := IntToStr(300);
      Charges.Values['Forensic2'] := IntToStr(100);
      Charges.Values['Computer2'] := IntToStr(0);
      for i := 1 to 8 do
        if Roles.Name[i] <> ''
        then
          Charges.Values['Offender2' + IntToStr(i)] := Roles.Name[i];
      if Vault = ''
        then
          case IllSystem.UseSkill(Leader.SkillVal, Diff) of
            1:
              begin
                NextDiff := 45;
                AfterInTheBank := 'DrivingAway';
                Report.Values['InTheBank'] := Leader.Name + ' handled the scary costumers very well and was able to get all the money from the counters. None of the employers pressed the alarm';
                AlarmState := 1;
                Profit := Profit + 120000;
              end;
            2:
              begin
                NextDiff := 60;
                AfterInTheBank := 'DrivingAway';
                Report.Values['InTheBank'] := Leader.Name + ' handled the scary costumers well and was able to get a good amount of money from the counters. None of the employers pressed the alarm';
                AlarmState := 1;
                Profit := Profit + 100000;
              end;
            3:
              begin
                Report.Values['InTheBank'] := Leader.Name + ' wasn''t able to handle the costumers and the employers. Some confusion arose in the bank. He was able to get some money from the counters anyway';
                Profit := Profit + 80000;
                AlarmPressed(3);
              end;
            4:
              begin
                Report.Values['InTheBank'] := 'Due to ' + Leader.Name + ' lack of intimidation towards the costumers and employers, a big panic arose in the bank. The team lost a lot of time and couldn''t get too much money';
                Profit := Profit + 40000;
                AlarmPressed(4);
              end;
          end
        else
          if Vault = 'Explosive'
          then
            case IllSystem.UseSkill(Leader.SkillVal, Diff) of
              1:
                begin
                  Report.Values['InTheBank'] := Leader.Name + ' handled the scary costumers very well, was able to get all the money from the counters and to properly handle the preparation for the opening of the vault. None of the employers pressed the alarm';
                  AlarmState := 1;
                  Profit := Profit + 150000;
                  OpenVaultExpl(50);
                end;
              2:
                begin
                  Report.Values['InTheBank'] := Leader.Name + ' handled the scary costumers well, was able to get a good amount of money from the counters and to handle the preparation for the opening of the vault. None of the employers pressed the alarm';
                  AlarmState := 1;
                  Profit := Profit + 100000;
                  OpenVaultExpl(60);
                end;
              3:
                begin
                  Report.Values['InTheBank'] := Leader.Name + ' wasn''t able to handle the costumers and the employers. Some confusion arose in the bank. He was able to get some money anyway but made some confusion in the preparing for the explosion';
                  Profit := Profit + 80000;
                  AlarmPressed(3);
                end;
              4:
                begin
                  Report.Values['InTheBank'] := 'Due to ' + Leader.Name + ' lack of intimidation towards the costumers and employers, a big panic arose in the bank. The team lost a lot of time and couldn''t get very much money, and the preparations for the vault explosion were very sloppy and unaccurate';
                  Profit := Profit + 40000;
                  AlarmPressed(4);
                end;
            end
          else
            case IllSystem.UseSkill(Leader.SkillVal, Diff) of
              1:
                begin
                  Report.Values['InTheBank'] := Leader.Name + ' handled the scary costumers very well, was able to get all the money from the counters and to force the employers to help in the opening of the vault with the bank computers. None of them pressed the alarm';
                  AlarmState := 1;
                  Profit := Profit + 150000;
                  OpenVaultComp(50);
                end;
              2:
                begin
                  Report.Values['InTheBank'] := Leader.Name + ' handled the scary costumers well, was able to get a good amount of money from the counters and to handle the preparation for the opening of the vault. None of the employers pressed the alarm';
                  AlarmState := 1;
                  Profit := Profit + 100000;
                  OpenVaultComp(60);
                end;
              3:
                begin
                  Report.Values['InTheBank'] := Leader.Name + ' wasn''t able to handle the costumers and the employers. Some confusion arose in the bank. He was able to get some money anyway but made some confusion in the preparing for the opening of the vault with the computers';
                  Profit := Profit + 80000;
                  AlarmPressed(3);
                end;
              4:
                begin
                  Report.Values['InTheBank'] := 'Due to ' + Leader.Name + ' lack of intimidation towards the costumers and employers, a big panic arose in the bank. The team lost a lot of time and couldn''t get very much money, and the preparations for the vault opening were very unaccurate';
                  Profit := Profit + 40000;
                  AlarmPressed(4);
                end;
            end;
    end;

  procedure TBankRobbery.DrivingAway(Diff : single);
    var
      Driver : TCrimForTask;
    begin
      Driver := GetRightCriminalForTask('Driver', 'Driving');
      ReadyToLeave := 'yes';
      //case IllSystem.UseSkill(Driver.SkillVal, Diff) of

      //end;
    end;

  procedure TBankRobbery.AlarmPressed(LeadRes : integer);
    begin
      if LeadRes = 3
      then
        case Hacker1Result of
          0:
            begin
              Report.Values['AlarmPressed'] := 'In the confusion an alarm went on';
              AlarmState := 3;
              ManageHostages(50);
            end;
          1:
            begin
              Report.Values['AlarmPressed'] := 'An employer pressed the alarm, but ' + Hacker1Name + ' intercepted the call from the headquarter and shut it down';
              AlarmState := 1;
              ManageHostages(35);
            end;
          2:
            begin
              Report.Values['AlarmPressed'] := 'An employer pressed the alarm, but ' + Hacker1Name + ' intercepted the call from the headquarter and he probably shut it down';
              AlarmState := 1;
              ManageHostages(45);
            end;
          3:
            begin
              Report.Values['AlarmPressed'] := Hacker1Name + ' couldn''t stop a silent alarm pressed by a teller, but he alerted the rest of the team about the arrival of the police';
              AlarmState := 3;
              ManageHostages(60);
            end;
          4:
            begin
              AlarmState := 2;
              ManageHostages(30);
            end;
        end;
      if LeadRes = 4
      then
        case Hacker1Result of
          0:
            begin
              AlarmState := 2;
              ManageHostages(30);
            end;
          1:
            begin
              Report.Values['AlarmPressed'] := 'An employer pressed the alarm, but ' + Hacker1Name + ' intercepted the call from the headquarter and shut it down';
              AlarmState := 1;
              ManageHostages(45);
            end;
          2:
            begin
              Report.Values['AlarmPressed'] := 'In the confusion an alarm went on';
              AlarmState := 3;
              ManageHostages(50);
            end;
          3:
            begin
              AlarmState := 2;
              ManageHostages(60);
            end;
          4:
            begin
              AlarmState := 2;
              ManageHostages(60);
            end;
        end;
    end;

  procedure TBankRobbery.OpenVaultExpl(Diff : single);
    var
      i : integer;
      Artificer : TCrimForTask;
      Crim : TNameStatusSkill;
    begin
      Artificer := GetRightCriminalForTask('Artificer', 'Demolition');
      Charges.Values['Charge2'] := 'BankRobberyWithSafeWreckage';
      Charges.Values['PoliceDiff2'] := IntToStr(500);
      Charges.Values['Underground2'] := IntToStr(200);
      Charges.Values['Witness2'] := IntToStr(200);
      Charges.Values['Forensic2'] := IntToStr(300);
      Charges.Values['Computer2'] := IntToStr(0);
      for i := 1 to 8 do
        if Roles.Name[i] <> ''
        then
          Charges.Values['Offender2' + IntToStr(i + 1)] := Roles.Name[i];
      if AlarmState = 1
      then
        case IllSystem.UseSkill(Artificer.SkillVal, Diff) of
          1:
            begin
              NextDiff := 45;
              AfterInTheBank := 'DrivingAway';
              Report.Values['OpenVaultExpl'] := Artificer.Name + ' executed a perfect explosion. No damage to the content of the safe, and nobody was hurt';
              Profit := Profit + 2000000;
            end;
          2:
            begin
              NextDiff := 60;
              AfterInTheBank := 'DrivingAway';
              Report.Values['OpenVaultExpl'] := Artificer.Name + ' executed a good explosion. Slightly damage to the content of the safe, and nobody was hurt';
              Profit := Profit + 1800000;
            end;
          3:
            begin
              Crim := GetTeamMemberNameAndStatus(Artificer.Name, '');
              if Crim.Status = 'InTeam: InMission: Wounded'
                then
                  begin
                    IllSystem.RDOChangeCriminalState(Artificer.Name, 'InTeam: InMission: Dead');
                    Report.Values['OpenVaultExpl'] := Artificer.Name + ' executed a bad explosion. Big damage to the content of the safe, and, being already wounded, he managed to kill himself';
                    if CheckTeamStatus = False
                    then
                      begin
                        Report.Values['OpenVaultExpl'] := Report.Values['OpenVaultExpl'] + 'Every member of the team was dead. The mission had tragically finished';
                        Profit := 0;
                        EndMission;
                      end;
                  end
                else
                  begin
                    IllSystem.RDOChangeCriminalState(Artificer.Name, 'InTeam: InMission: Wounded');
                    Report.Values['OpenVaultExpl'] := Artificer.Name + ' executed a bad explosion. Big damage to the content of the safe, and he managed to hurt himself';
                    NextDiff := 70;
                    AfterInTheBank := 'DrivingAway';
                    Profit := Profit + 1000000;
                  end;
            end;
          4:
            begin
              IllSystem.RDOChangeCriminalState(Artificer.Name, 'InTeam: InMission: Dead');
              Report.Values['OpenVaultExpl'] := Artificer.Name + ' executed a disastrous explosion. The whole content of the safe is destroyed, and he managed to kill himself';
              if CheckTeamStatus = False
              then
                begin
                  Report.Values['OpenVaultExpl'] := Report.Values['OpenVaultExpl'] + 'Every member of the team was dead. The mission had tragically finished';
                  Profit := 0;
                  EndMission;
                end
              else
                begin
                  NextDiff := 80;
                  AfterInTheBank := 'DrivingAway';
                end;
            end;
        end;
      if AlarmState = 2
      then
        case IllSystem.UseSkill(Artificer.SkillVal, Diff) of
          1:
            begin
              NextDiff := 50;
              AfterInTheBank := 'HostageStandOff';
              Report.Values['OpenVaultExpl'] := Artificer.Name + ' executed a perfect explosion. No damage to the content of the safe, and nobody was hurt. The team was about to leave the bank, when they realize they were surrounded by the police. Someone must have pressed a silent alarm';
              Profit := Profit + 2000000;
            end;
          2:
            begin
              NextDiff := 60;
              AfterInTheBank := 'HostageStandOff';
              Report.Values['OpenVaultExpl'] := Artificer.Name + ' executed a good explosion. Slightly damage to the content of the safe, and nobody was hurt. The team was about to leave the bank, when they realize they were surrounded by the police. Someone must have pressed a silent alarm';
              Profit := Profit + 1800000;
            end;
          3:
            begin
              Crim := GetTeamMemberNameAndStatus(Artificer.Name, '');
              if Crim.Status = 'InTeam: InMission: Wounded'
                then
                  begin
                    IllSystem.RDOChangeCriminalState(Artificer.Name, 'InTeam: InMission: Dead');
                    Report.Values['OpenVaultExpl'] := Artificer.Name + ' executed a bad explosion. Big damage to the content of the safe, and, being already wounded, he managed to kill himself';
                    if CheckTeamStatus = False
                    then
                      begin
                        Report.Values['OpenVaultExpl'] := Report.Values['OpenVaultExpl'] + ' .Every member of the team was dead. The mission had tragically finished';
                        Profit := 0;
                        EndMission;
                      end;
                  end
                else
                  begin
                    IllSystem.RDOChangeCriminalState(Artificer.Name, 'InTeam: InMission: Wounded');
                    Report.Values['OpenVaultExpl'] := Artificer.Name + ' executed a bad explosion. Big damage to the content of the safe, and he managed to hurt himselfThe team was about to leave the bank, when they realize they were surrounded by the police. Someone must have pressed a silent alarm';
                    NextDiff := 70;
                    AfterInTheBank := 'HostageStandOff';
                    Profit := Profit + 1000000;
                  end;
            end;
          4:
            begin
              IllSystem.RDOChangeCriminalState(Artificer.Name, 'InTeam: InMission: Dead');
              Report.Values['OpenVaultExpl'] := Artificer.Name + ' executed a disastrous explosion. The whole content of the safe is destroyed, and he manged to kill himself. The team was about to leave the bank, when they realize they were surrounded by the police. Someone must have pressed a silent alarm';
              if CheckTeamStatus = False
              then
                begin
                  Report.Values['OpenVaultExpl'] := Report.Values['OpenVaultExpl'] + ' .Every member of the team was dead. The mission had tragically finished';
                  Profit := 0;
                  EndMission;
                end
              else
                begin
                  NextDiff := 80;
                  AfterInTheBank := 'HostageStandOff';
                end;
            end;
        end;
    end;

  procedure TBankRobbery.OpenVaultComp(Diff : single);
    var
      i : integer;
      Hacker2 : TCrimForTask;
    begin
      Hacker2 := GetRightCriminalForTask('Hacker-2', 'Computer');
      if AlarmState = 1
      then
        case IllSystem.UseSkill(Hacker2.SkillVal, Diff) of
          1:
            begin
              NextDiff := 45;
              AfterInTheBank := 'DrivingAway';
              Charges.Values['Charge2'] := 'BankRobberyWithSafeHacking';
              Charges.Values['PoliceDiff2'] := IntToStr(500);
              Charges.Values['Underground2'] := IntToStr(100);
              Charges.Values['Witness2'] := IntToStr(200);
              Charges.Values['Forensic2'] := IntToStr(100);
              Charges.Values['Computer2'] := IntToStr(200);
              for i := 1 to 8 do
                if Roles.Name[i] <> ''
                then
                  Charges.Values['Offender2' + IntToStr(i + 1)] := Roles.Name[i];
              Report.Values['OpenVaultComp'] := Hacker2.Name + ' executed a very quick opening';
              Profit := Profit + 2000000;
            end;
          2:
            begin
              NextDiff := 55;
              AfterInTheBank := 'DrivingAway';
              Charges.Values['Charge2'] := 'BankRobberyWithSafeHacking';
              Charges.Values['PoliceDiff2'] := IntToStr(500);
              Charges.Values['Underground2'] := IntToStr(100);
              Charges.Values['Witness2'] := IntToStr(200);
              Charges.Values['Forensic2'] := IntToStr(100);
              Charges.Values['Computer2'] := IntToStr(300);
              for i := 1 to 8 do
                if Roles.Name[i] <> ''
                then
                  Charges.Values['Offender2' + IntToStr(i + 1)] := Roles.Name[i];
              Report.Values['OpenVaultComp'] := Hacker2.Name + ' opened the safe with some difficulties';
              Profit := Profit + 2000000;
            end;
          3:
            begin
              Report.Values['OpenVaultComp'] := Hacker2.Name + ' was not able to open the safe. Fortunately, he realized his incapacity very soon and the team didn''t loose too much time';
              NextDiff := 65;
              AfterInTheBank := 'DrivingAway';
            end;
          4:
            begin
              Report.Values['OpenVaultComp'] := Hacker2.Name + ' was not able to open the safe. He tried for a long time and the team had a big delay';
              Charges.Values['Computer2'] := IntToStr(500);
              NextDiff := 80;
              AfterInTheBank := 'DrivingAway';
            end;
        end;
      if AlarmState = 2
      then
        case IllSystem.UseSkill(Hacker2.SkillVal, Diff) of
          1:
            begin
              NextDiff := 50;
              AfterInTheBank := 'HostageStandOff';
              Charges.Values['Charge2'] := 'BankRobberyWithSafeHacking';
              Charges.Values['PoliceDiff2'] := IntToStr(500);
              Charges.Values['Underground2'] := IntToStr(100);
              Charges.Values['Witness2'] := IntToStr(200);
              Charges.Values['Forensic2'] := IntToStr(100);
              Charges.Values['Computer2'] := IntToStr(200);
              for i := 1 to 8 do
                if Roles.Name[i] <> ''
                then
                  Charges.Values['Offender2' + IntToStr(i + 1)] := Roles.Name[i];
              Report.Values['OpenVaultComp'] := Hacker2.Name + ' executed a very quick opening. The team was about to leave the bank, when they realize they were surrounded by the police. Someone must have pressed a silent alarm';
              Profit := Profit + 2000000;
            end;
          2:
            begin
              NextDiff := 60;
              AfterInTheBank := 'HostageStandOff';
              Charges.Values['Charge2'] := 'BankRobberyWithSafeHacking';
              Charges.Values['PoliceDiff2'] := IntToStr(500);
              Charges.Values['Underground2'] := IntToStr(100);
              Charges.Values['Witness2'] := IntToStr(200);
              Charges.Values['Forensic2'] := IntToStr(100);
              Charges.Values['Computer2'] := IntToStr(300);
              for i := 1 to 8 do
                if Roles.Name[i] <> ''
                then
                  Charges.Values['Offender2' + IntToStr(i + 1)] := Roles.Name[i];
              Report.Values['OpenVaultComp'] := Hacker2.Name + ' opened the safe with some difficulties. The team was about to leave the bank, when they realize they were surrounded by the police. Someone must have pressed a silent alarm';
              Profit := Profit + 2000000;
            end;
          3:
            begin
              Report.Values['OpenVaultComp'] := Hacker2.Name + ' was not able to open the safe. Fortunately, he realized his incapacity very soon and the team didn''t loose too much time. The team was about to leave the bank, when they realize they were surrounded by the police. Someone must have pressed a silent alarm';
              NextDiff := 70;
              AfterInTheBank := 'HostageStandOff';
            end;
          4:
            begin
              Report.Values['OpenVaultComp'] := Hacker2.Name + ' was not able to open the safe. He tried for a long time and the team had a big delay. The team was about to leave the bank, when they realize they were surrounded by the police. Someone must have pressed a silent alarm';
              Charges.Values['Computer2'] := IntToStr(500);
              NextDiff := 80;
              AfterInTheBank := 'HostageStandOff';
            end;
        end;
    end;

  procedure TBankRobbery.SackedIn(Diff : single);
    begin
    end;

  procedure TBankRobbery.CarChase(Diff : single);
    begin
    end;

  procedure TBankRobbery.ManageHostages(Diff : single);
    var
      Gorilla : TCrimForTask;
    begin
      Gorilla := GetRightCriminalForTask('Gorilla', 'Brawling');
      if AlarmState = 1
      then
        if Vault = ''
        then
          case IllSystem.UseSkill(Gorilla.SkillVal, Diff) of
            1:
              begin
                Report.Values['ManageHostages'] := 'A costumer panicked. ' + Gorilla.Name + ' was able to control him and to tie it down without any problem, perfectly in schedule for leaving the bank';
                NextDiff := 40;
                AfterInTheBank := 'DrivingAway';
              end;
            2:
              begin
                Report.Values['ManageHostages'] := 'A costumer panicked. ' + Gorilla.Name + ' was able to control him but the team lost some time';
                NextDiff := 50;
                AfterInTheBank := 'DrivingAway';
              end;
            3:
              begin
                Report.Values['ManageHostages'] := 'A costumer panicked. ' + Gorilla.Name + ' became violent trying to stop him';
                Charges.Values['Charge3'] := 'Assault';
                Charges.Values['Offender31'] := Gorilla.Name;
                Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                ControlViolentOutburst(50);
              end;
            4:
              begin
                Report.Values['ManageHostages'] := 'A costumer panicked. ' + Gorilla.Name + ' became too violent trying to stop him and the costumer, fearing for his life, hit back ' + Gorilla.Name;
                Charges.Values['Charge3'] := 'Assault';
                Charges.Values['Offender31'] := Gorilla.Name;
                Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                BeforeDeadlyIncident := 'ManageHostages';
                DeadlyIncident(50);
              end;
          end
        else
          begin
            if Vault = 'Explosive'
            then
              case IllSystem.UseSkill(Gorilla.SkillVal, Diff) of
                1:
                  begin
                    Report.Values['ManageHostages'] := 'A costumer panicked. ' + Gorilla.Name + ' was able to control him and to tie it down without any problem, perfectly in schedule for the explosion of the vault';
                    OpenVaultExpl(50);
                  end;
                2:
                  begin
                    Report.Values['ManageHostages'] := 'A costumer panicked. ' + Gorilla.Name + ' was able to control him but the team lost some time for the explosion of the vault';
                    OpenVaultExpl(60);
                  end;
                3:
                  begin
                    Report.Values['ManageHostages'] := 'A costumer panicked. ' + Gorilla.Name + ' became violent trying to stop him';
                    Charges.Values['Charge3'] := 'Assault';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    ControlViolentOutburst(50);
                  end;
                4:
                  begin
                    Report.Values['ManageHostages'] := 'A costumer panicked. ' + Gorilla.Name + ' became too violent trying to stop him and the costumer, fearing for his life, hit back ' + Gorilla.Name;
                    BeforeDeadlyIncident := 'ManageHostages';
                    Charges.Values['Charge3'] := 'Assault';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    DeadlyIncident(50);
                  end;
              end
            else
              case IllSystem.UseSkill(Gorilla.SkillVal, Diff) of
                1:
                  begin
                    Report.Values['ManageHostages'] := 'A costumer panicked. ' + Gorilla.Name + ' was able to control him and to tie it down without any problem, without disturbing the computer operations to open the vault';
                    OpenVaultComp(80);
                  end;
                2:
                  begin
                    Report.Values['ManageHostages'] := 'A costumer panicked. ' + Gorilla.Name + ' was able to control him but the team lost some time for the computer operations to open the vault';
                    OpenVaultExpl(90);
                  end;
                3:
                  begin
                    Report.Values['ManageHostages'] := 'A costumer panicked. ' + Gorilla.Name + ' became violent trying to stop him';
                    Charges.Values['Charge3'] := 'Assault';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    ControlViolentOutburst(50);
                  end;
                4:
                  begin
                    Report.Values['ManageHostages'] := 'A costumer panicked. ' + Gorilla.Name + ' became too violent trying to stop him and the costumer, fearing for his life, hit back ' + Gorilla.Name;
                    BeforeDeadlyIncident := 'ManageHostages';
                    Charges.Values['Charge3'] := 'Assault';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    DeadlyIncident(50);
                  end;
              end;
          end;
      if AlarmState = 2
      then
        if Vault = ''
        then
          case IllSystem.UseSkill(Gorilla.SkillVal, Diff) of
            1:
              begin
                Report.Values['ManageHostages'] := 'A costumer panicked. ' + Gorilla.Name + ' was able to control him and to tie it down without any problem, perfectly in schedule for leaving the bank';
                DrivingAway(70);
              end;
            2:
              begin
                Report.Values['ManageHostages'] := 'A costumer panicked. ' + Gorilla.Name + ' was able to control him but the team lost some time';
                DrivingAway(80);
              end;
            3:
              begin
                Report.Values['ManageHostages'] := 'A costumer panicked. ' + Gorilla.Name + ' became violent trying to stop him';
                Charges.Values['Charge3'] := 'Assault';
                Charges.Values['Offender31'] := Gorilla.Name;
                Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                ControlViolentOutburst(50);
              end;
            4:
              begin
                Report.Values['ManageHostages'] := 'A costumer panicked. ' + Gorilla.Name + ' became too violent trying to stop him and the costumer, fearing for his life, hit back ' + Gorilla.Name;
                BeforeDeadlyIncident := 'ManageHostages';
                Charges.Values['Charge3'] := 'Assault';
                Charges.Values['Offender31'] := Gorilla.Name;
                Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                DeadlyIncident(50);
              end;
          end
        else
          begin
            if Vault = 'Explosive'
            then
              case IllSystem.UseSkill(Gorilla.SkillVal, Diff) of
                1:
                  begin
                    Report.Values['ManageHostages'] := 'A costumer panicked. ' + Gorilla.Name + ' was able to control him and to tie it down without any problem, perfectly in schedule for the explosion of the vault';
                    OpenVaultExpl(50);
                  end;
                2:
                  begin
                    Report.Values['ManageHostages'] := 'A costumer panicked. ' + Gorilla.Name + ' was able to control him but the team lost some time for the explosion of the vault';
                    OpenVaultExpl(55);
                  end;
                3:
                  begin
                    Report.Values['ManageHostages'] := 'A costumer panicked. ' + Gorilla.Name + ' became violent trying to stop him';
                    Charges.Values['Charge3'] := 'Assault';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    ControlViolentOutburst(50);
                  end;
                4:
                  begin
                    Report.Values['ManageHostages'] := 'A costumer panicked. ' + Gorilla.Name + ' became too violent trying to stop him and the costumer, fearing for his life, hit back ' + Gorilla.Name;
                    BeforeDeadlyIncident := 'ManageHostages';
                    Charges.Values['Charge3'] := 'Assault';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    DeadlyIncident(50);
                  end;
              end
            else
              case IllSystem.UseSkill(Gorilla.SkillVal, Diff) of
                1:
                  begin
                    Report.Values['ManageHostages'] := 'A costumer panicked. ' + Gorilla.Name + ' was able to control him and to tie it down without any problem, without disturbing the computer operations to open the vault';
                    OpenVaultComp(80);
                  end;
                2:
                  begin
                    Report.Values['ManageHostages'] := 'A costumer panicked. ' + Gorilla.Name + ' was able to control him but the team lost some time for the computer operations to open the vault';
                    OpenVaultExpl(90);
                  end;
                3:
                  begin
                    Report.Values['ManageHostages'] := 'A costumer panicked. ' + Gorilla.Name + ' became violent trying to stop him';
                    Charges.Values['Charge3'] := 'Assault';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    ControlViolentOutburst(50);
                  end;
                4:
                  begin
                    Report.Values['ManageHostages'] := 'A costumer panicked. ' + Gorilla.Name + ' became too violent trying to stop him and the costumer, fearing for his life, hit back ' + Gorilla.Name;
                    BeforeDeadlyIncident := 'ManageHostages';
                    Charges.Values['Charge3'] := 'Assault';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    DeadlyIncident(50);
                  end;
              end;
          end;
      if AlarmState = 3
      then
        if Vault = ''
        then
          case IllSystem.UseSkill(Gorilla.SkillVal, Diff) of
            1:
              begin
                Report.Values['ManageHostages'] := 'A costumer panicked to the sound of the alarm. ' + Gorilla.Name + ' was able to control him and to tie it down without any problem, perfectly in schedule for leaving the bank';
                PoliceArriving(40);
              end;
            2:
              begin
                Report.Values['ManageHostages'] := 'A costumer panicked to the sound of the alarm. ' + Gorilla.Name + ' was able to control him but the team lost some time';
                PoliceArriving(50);
              end;
            3:
              begin
                Report.Values['ManageHostages'] := 'A costumer panicked to the sound of the alarm. ' + Gorilla.Name + ' became violent trying to stop him';
                Charges.Values['Charge3'] := 'Assault';
                Charges.Values['Offender31'] := Gorilla.Name;
                Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                ControlViolentOutburst(50);
              end;
            4:
              begin
                Report.Values['ManageHostages'] := 'A costumer panicked to the sound of the alarm. ' + Gorilla.Name + ' became too violent trying to stop him and the costumer, fearing for his life, hit back ' + Gorilla.Name;
                BeforeDeadlyIncident := 'ManageHostages';
                Charges.Values['Charge3'] := 'Assault';
                Charges.Values['Offender31'] := Gorilla.Name;
                Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                DeadlyIncident(50);
              end;
          end
        else
          begin
            case IllSystem.UseSkill(Gorilla.SkillVal, Diff) of
              1:
                begin
                  Report.Values['ManageHostages'] := 'A costumer panicked to the sound of the alarm. ' + Gorilla.Name + ' was able to control him and to tie it down without any problem.';
                  PoliceArriving(50);
                end;
              2:
                begin
                  Report.Values['ManageHostages'] := 'A costumer panicked to the sound of the alarm. ' + Gorilla.Name + ' was able to control him but the team lost some time.';
                  PoliceArriving(60);
                end;
              3:
                begin
                  Report.Values['ManageHostages'] := 'A costumer panicked to the sound of the alarm. ' + Gorilla.Name + ' became violent trying to stop him';
                  Charges.Values['Charge3'] := 'Assault';
                  Charges.Values['Offender31'] := Gorilla.Name;
                  Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                  ControlViolentOutburst(50);
                end;
              4:
                begin
                  Report.Values['ManageHostages'] := 'A costumer panicked to the sound of the alarm. ' + Gorilla.Name + ' became too violent trying to stop him and the costumer, fearing for his life, hit back ' + Gorilla.Name;
                  BeforeDeadlyIncident := 'ManageHostages';
                  Charges.Values['Charge3'] := 'Assault';
                  Charges.Values['Offender31'] := Gorilla.Name;
                  Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                  DeadlyIncident(50);
                end;
            end;
          end;
    end;

  procedure TBankRobbery.ControlViolentOutburst(Diff : single);
    var
      Leader : TCrimForTask;
    begin
      Leader := GetRightCriminalForTask('Leader', 'Leadership');
      if AlarmState = 1
      then
        if Vault = ''
        then
          case IllSystem.UseSkill(Leader.SkillVal, Diff) of
            1:
              begin
                Report.Values['ControlViolentOutburst'] := Leader.Name + ' took control of the situation, calming everyone down. The team left the bank with a minimal time loss';
                DrivingAway(55);
              end;
            2:
              begin
                Report.Values['ControlViolentOutburst'] := Leader.Name + ' took control of the situation, calming everyone down with some difficulty. The team left the bank with a significant time loss';
                DrivingAway(65);
              end;
            3:
              begin
                Report.Values['ControlViolentOutburst'] := Leader.Name + ' tried to take control of the situation, but wasn''t able to handle it';
                BeforeDeadlyIncident := 'ControlViolentOutburst3';
                DeadlyIncident(50);
              end;
            4:
              begin
                Report.Values['ControlViolentOutburst'] := Leader.Name + ' tried to take control of the situation, but was able only to make things worse';
                BeforeDeadlyIncident := 'ControlViolentOutburst4';
                DeadlyIncident(65);
              end;
          end
        else
          begin
            if Vault = 'Explosive'
            then
              case IllSystem.UseSkill(Leader.SkillVal, Diff) of
                1:
                  begin
                    Report.Values['ControlViolentOutburst'] := Leader.Name + ' took control of the situation, calming everyone down. The team lost a minimal time for the explosion of the vault';
                    OpenVaultExpl(60);
                  end;
                2:
                  begin
                    Report.Values['ControlViolentOutburst'] := Leader.Name + ' took control of the situation, calming everyone down. The team lost some time for the explosion of the vault';
                    OpenVaultExpl(65);
                  end;
                3:
                  begin
                    Report.Values['ControlViolentOutburst'] := Leader.Name + ' tried to take control of the situation, but wasn''t able to handle it';
                    BeforeDeadlyIncident := 'ControlViolentOutburst3';
                    DeadlyIncident(50);
                  end;
                4:
                  begin
                    Report.Values['ControlViolentOutburst'] := Leader.Name + ' tried to take control of the situation, but was able only to make things worse';
                    BeforeDeadlyIncident := 'ControlViolentOutburst4';
                    DeadlyIncident(65);
                  end;
              end
            else
              case IllSystem.UseSkill(Leader.SkillVal, Diff) of
              1:
                begin
                  Report.Values['ControlViolentOutburst'] := Leader.Name + ' took control of the situation, calming everyone down. The team lost a minimal time for the opening of the vault';
                  OpenVaultComp(90);
                end;
              2:
                begin
                  Report.Values['ControlViolentOutburst'] := Leader.Name + ' took control of the situation, calming everyone down. The team lost some time for the opening of the vault';
                  OpenVaultComp(100);
                end;
              3:
                begin
                  Report.Values['ControlViolentOutburst'] := Leader.Name + ' tried to take control of the situation, but wasn''t able to handle it';
                  BeforeDeadlyIncident := 'ControlViolentOutburst3';
                  DeadlyIncident(50);
                end;
              4:
                begin
                  Report.Values['ControlViolentOutburst'] := Leader.Name + ' tried to take control of the situation, but was able only to make things worse';
                  BeforeDeadlyIncident := 'ControlViolentOutburst4';
                  DeadlyIncident(65);
                end;
              end;
          end;
      if AlarmState = 2
      then
        if Vault = ''
        then
          case IllSystem.UseSkill(Leader.SkillVal, Diff) of
            1:
              begin
                Report.Values['ControlViolentOutburst'] := Leader.Name + ' took control of the situation, calming everyone down. The team left the bank with a minimal time loss';
                DrivingAway(75);
              end;
            2:
              begin
                Report.Values['ControlViolentOutburst'] := Leader.Name + ' took control of the situation, calming everyone down with some difficulty. Someone had press a silent alrm, because the bank is surrounded by the police';
                HostageStandOff(60);
              end;
            3:
              begin
                Report.Values['ControlViolentOutburst'] := Leader.Name + ' tried to take control of the situation, but wasn''t able to handle it';
                BeforeDeadlyIncident := 'ControlViolentOutburst3';
                DeadlyIncident(50);
              end;
            4:
              begin
                Report.Values['ControlViolentOutburst'] := Leader.Name + ' tried to take control of the situation, but was able only to make things worse';
                BeforeDeadlyIncident := 'ControlViolentOutburst4';
                DeadlyIncident(65);
              end;
          end
        else
          begin
            if Vault = 'Explosive'
            then
              case IllSystem.UseSkill(Leader.SkillVal, Diff) of
                1:
                  begin
                    Report.Values['ControlViolentOutburst'] := Leader.Name + ' took control of the situation, calming everyone down. The team lost a minimal time for the explosion of the vault';
                    OpenVaultExpl(60);
                  end;
                2:
                  begin
                    Report.Values['ControlViolentOutburst'] := Leader.Name + ' took control of the situation, calming everyone down. The team lost some time for the explosion of the vault';
                    OpenVaultExpl(70);
                  end;
                3:
                  begin
                    Report.Values['ControlViolentOutburst'] := Leader.Name + ' tried to take control of the situation, but wasn''t able to handle it';
                    BeforeDeadlyIncident := 'ControlViolentOutburst3';
                    DeadlyIncident(50);
                  end;
                4:
                  begin
                    Report.Values['ControlViolentOutburst'] := Leader.Name + ' tried to take control of the situation, but was able only to make things worse';
                    BeforeDeadlyIncident := 'ControlViolentOutburst4';
                    DeadlyIncident(65);
                  end;
              end
            else
              case IllSystem.UseSkill(Leader.SkillVal, Diff) of
              1:
                begin
                  Report.Values['ControlViolentOutburst'] := Leader.Name + ' took control of the situation, calming everyone down. The team lost a minimal time for the opening of the vault';
                  OpenVaultComp(90);
                end;
              2:
                begin
                  Report.Values['ControlViolentOutburst'] := Leader.Name + ' took control of the situation, calming everyone down. The team lost some time for the opening of the vault';
                  OpenVaultComp(100);
                end;
              3:
                begin
                  Report.Values['ControlViolentOutburst'] := Leader.Name + ' tried to take control of the situation, but wasn''t able to handle it';
                  BeforeDeadlyIncident := 'ControlViolentOutburst3';
                  DeadlyIncident(50);
                end;
              4:
                begin
                  Report.Values['ControlViolentOutburst'] := Leader.Name + ' tried to take control of the situation, but was able only to make things worse';
                  BeforeDeadlyIncident := 'ControlViolentOutburst4';
                  DeadlyIncident(65);
                end;
              end;
          end;
      if AlarmState = 3
      then
        if Vault = ''
        then
          case IllSystem.UseSkill(Leader.SkillVal, Diff) of
            1:
              begin
                Report.Values['ControlViolentOutburst'] := Leader.Name + ' took control of the situation, calming everyone down. The team was about to leave the bank with a minimal time loss';
                PoliceArriving(50);
              end;
            2:
              begin
                Report.Values['ControlViolentOutburst'] := Leader.Name + ' took control of the situation, calming everyone down with some difficulty. The team was about to leave the bank with a significant time loss';
                PoliceArriving(60);
              end;
            3:
              begin
                Report.Values['ControlViolentOutburst'] := Leader.Name + ' tried to take control of the situation, but wasn''t able to handle it';
                BeforeDeadlyIncident := 'ControlViolentOutburst3';
                DeadlyIncident(50);
              end;
            4:
              begin
                Report.Values['ControlViolentOutburst'] := Leader.Name + ' tried to take control of the situation, but was able only to make things worse';
                BeforeDeadlyIncident := 'ControlViolentOutburst4';
                DeadlyIncident(65);
              end;
          end
        else
          begin
            case IllSystem.UseSkill(Leader.SkillVal, Diff) of
              1:
                begin
                  Report.Values['ControlViolentOutburst'] := Leader.Name + ' took control of the situation, calming everyone down. Because of the alarm, there was no time to open the vault';
                  PoliceArriving(55);
                end;
              2:
                begin
                  Report.Values['ControlViolentOutburst'] := Leader.Name + ' took control of the situation, calming everyone down. The team lost some time and because of the alarm, there was no time to open the vault';
                  PoliceArriving(65);
                end;
              3:
                begin
                  Report.Values['ControlViolentOutburst'] := Leader.Name + ' tried to take control of the situation, but wasn''t able to handle it';
                  BeforeDeadlyIncident := 'ControlViolentOutburst3';
                  DeadlyIncident(50);
                end;
              4:
                begin
                  Report.Values['ControlViolentOutburst'] := Leader.Name + ' tried to take control of the situation, but was able only to make things worse';
                  BeforeDeadlyIncident := 'ControlViolentOutburst4';
                  DeadlyIncident(65);
                end;
            end;
          end;
    end;

  function TBankRobbery.FirearmTable(Diff : single): boolean;
    var
      Crim : TNameStatusSkill;
      Crim2 : TNameStatusSkill;
    begin
      Crim := GetTeamMemberNameAndStatus('', 'FireArms');
      case IllSystem.UseSkill(Crim.Skill, Diff) of
        1:
          begin
            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Suddenly, in the confusion, ' + Crim.Name + ' opened fire. Another costumer is injured. ';
            Charges.Values['Charge4'] := 'AttemptedMurder';
            Charges.Values['Offender41'] := Crim.Name;
            Charges.Values['PoliceDiff4'] := Charges.Values['PoliceDiff2'];
            Result := True;
          end;
        2:
          begin
            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Suddenly, in the confusion, ' + Crim.Name + ' opened fire. Another costumer died. ';
            Charges.Values['Charge4'] := 'Murder';
            Charges.Values['Offender41'] := Crim.Name;
            Charges.Values['PoliceDiff4'] := Charges.Values['PoliceDiff2'];
            Result := True;
          end;
        3:
          begin
            Crim2 := GetTeamMemberNameAndStatus('', '');
            if Crim2.Name = Crim.Name
            then
              begin
                if Crim2.Status = 'InTeam: InMission: Wounded'
                then
                  begin
                    IllSystem.RDOChangeCriminalState(Crim2.Name, 'InTeam: InMission: Dead');
                    Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Suddenly, in the confusion, ' + Crim.Name + ' opened fire. A bullet ricocheted and killed ' + Crim.Name + ' himself. ';
                    if CheckTeamStatus = False
                    then
                      Result := False
                    else
                      Result := True;
                  end;
                if Crim2.Status = 'InTeam: InMission'
                then
                  begin
                    IllSystem.RDOChangeCriminalState(Crim2.Name, 'InTeam: InMission: Wounded');
                    Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Suddenly, in the confusion, ' + Crim.Name + ' opened fire. Another costumer died. A bullet ricocheted and injured ' + Crim.Name + ' himself. ';
                    Result := True;
                  end;
              end
            else
              begin
                if Crim2.Status = 'InTeam: InMission: Wounded'
                then
                  begin
                    IllSystem.RDOChangeCriminalState(Crim2.Name, 'InTeam: InMission: Dead');
                    Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Suddenly, in the confusion, ' + Crim.Name + ' opened fire. Another costumer died. A bullet ricocheted and killed ' + Crim2.Name + '. ';
                    if CheckTeamStatus = False
                    then
                      Result := False
                    else
                      Result := True;
                  end;
                if Crim2.Status = 'InTeam: InMission'
                then
                  begin
                    IllSystem.RDOChangeCriminalState(Crim2.Name, 'InTeam: InMission: Wounded');
                    Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Suddenly, in the confusion, ' + Crim.Name + ' opened fire. Another costumer died. A bullet ricocheted and injured ' + Crim2.Name + '. ';
                    Result := True;
                  end;
              end;
            Charges.Values['Charge4'] := 'Murder';
            Charges.Values['Offender41'] := Crim.Name;
            Charges.Values['PoliceDiff4'] := Charges.Values['PoliceDiff2'];
          end;
        4:
          begin
            Crim2 := GetTeamMemberNameAndStatus('', '');
            if Crim2.Name = Crim.Name
            then
              begin
                IllSystem.RDOChangeCriminalState(Crim2.Name, 'InTeam: InMission: Dead');
                Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Suddenly, in the confusion, ' + Crim.Name + ' opened fire. A bullet ricocheted and killed ' + Crim.Name + ' himself. ';
                if CheckTeamStatus = False
                then
                  Result := False
                else
                  Result := True;
              end
            else
              begin
                IllSystem.RDOChangeCriminalState(Crim2.Name, 'InTeam: InMission: Dead');
                Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Suddenly, in the confusion, ' + Crim.Name + ' opened fire. A bullet ricocheted and killed ' + Crim2.Name + '. ';
                Result := True;
              end;
          end;
      end;
    end;

  procedure TBankRobbery.DeadlyIncident(Diff : single);
    var
      i : integer;
      Rep : string;
      Gorilla : TCrimForTask;
      crim : TNameStatusSkill;
    begin
      Gorilla := GetRightCriminalForTask('Gorilla', 'Brawling');
      if AlarmState = 1
      then
        if Vault = ''
        then
          case IllSystem.UseSkill(Gorilla.SkillVal, Diff) of
            1:
              begin
                if BeforeDeadlyIncident = 'ControlViolentOutburst3'
                then
                  begin
                    Report.Values['DeadlyIncident'] := Gorilla.Name + ' was in the end able to control the panicked customer. The team left the bank with more delay';
                    AfterInTheBank := 'DrivingAway';
                    NextDiff := 55;
                  end;
                if BeforeDeadlyIncident = 'ManageHostages'
                then
                  begin
                    Report.Values['DeadlyIncident'] := Gorilla.Name + ' was in the end able to control the panicked customer, but injured him in the process. The team left the bank with some delay';
                    Charges.Values['Charge3'] := 'AggravatedAssault';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    AfterInTheBank := 'DrivingAway';
                    NextDiff := 50;
                  end;
                if BeforeDeadlyIncident = 'ControlViolentOutburst4'
                then
                  begin
                    Charges.Values['Charge3'] := 'AggravatedAssault';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                    Report.Values['DeadlyIncident'] := Gorilla.Name + ' was in the end able to control the panicked customer, but injured him and was hurt by him in the process. The team left the bank with more delay';
                    AfterInTheBank := 'DrivingAway';
                    NextDiff := 55;
                  end;
              end;
            2:
              begin
                if BeforeDeadlyIncident = 'ControlViolentOutburst3'
                then
                  begin
                    Report.Values['DeadlyIncident'] := Gorilla.Name + ' was in the end able to control the panicked customer, but injured him in the process. The team left the bank with much more delay';
                    Charges.Values['Charge3'] := 'AggravatedAssault';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    AfterInTheBank := 'DrivingAway';
                    NextDiff := 60;
                  end;
                if BeforeDeadlyIncident = 'ManageHostages'
                then
                  begin
                    Charges.Values['Charge3'] := 'Murder';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    IllSystem.RDOChangeCriminalState(crim.Name, 'InTeam: InMission: Wounded');
                    Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. The team left the bank with much more delay';
                    AfterInTheBank := 'DrivingAway';
                    NextDiff := 55;
                  end;
                if BeforeDeadlyIncident = 'ControlViolentOutburst4'
                then
                  begin
                    Charges.Values['Charge3'] := 'Murder';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                    Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                    if FirearmTable(Diff)
                    then
                      begin
                        Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team left the bank with much more delay';
                        AfterInTheBank := 'DrivingAway';
                        NextDiff := 60;
                      end
                    else
                      begin
                        Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                        Profit := 0;
                        EndMission;
                      end;
                  end;
              end;
            3:
              begin
                if BeforeDeadlyIncident = 'ControlViolentOutburst3'
                then
                  begin
                    Charges.Values['Charge3'] := 'Murder';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer. ';
                    if FirearmTable(Diff)
                    then
                      begin
                        Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team left the bank with incredible delay';
                        AfterInTheBank := 'DrivingAway';
                        NextDiff := 70;
                      end
                    else
                      begin
                        Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                        Profit := 0;
                        EndMission;
                      end;
                  end;
                if BeforeDeadlyIncident = 'ManageHostages'
                then
                  begin
                    Charges.Values['Charge3'] := 'Murder';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                    Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                    if FirearmTable(Diff)
                    then
                      begin
                        Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team left the bank with much more delay';
                        AfterInTheBank := 'DrivingAway';
                        NextDiff := 70;
                      end
                    else
                      begin
                        Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                        Profit := 0;
                        EndMission;
                      end;
                  end;
                if BeforeDeadlyIncident = 'ControlViolentOutburst4'
                then
                  begin
                    Charges.Values['Charge3'] := 'Murder';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                    Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                    if FirearmTable(Diff)
                    then
                      begin
                        if FirearmTable(Diff)
                        then
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team left the bank with incredible delay';
                            AfterInTheBank := 'DrivingAway';
                            NextDiff := 70;
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end
                    else
                      begin
                        Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                        Profit := 0;
                        EndMission;
                      end;
                  end;
              end;
            4:
              begin
                if BeforeDeadlyIncident = 'ControlViolentOutburst3'
                then
                  begin
                    Charges.Values['Charge3'] := 'Murder';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                    Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                    if FirearmTable(Diff)
                    then
                      begin
                        Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team left the bank with more incredible delay';
                        AfterInTheBank := 'DrivingAway';
                        NextDiff := 80;
                      end
                    else
                      begin
                        Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                        Profit := 0;
                        EndMission;
                      end;
                  end;
                if BeforeDeadlyIncident = 'ManageHostages'
                then
                  begin
                    Charges.Values['Charge3'] := 'Murder';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                    Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                    if FirearmTable(Diff)
                    then
                      begin
                        if FirearmTable(Diff)
                        then
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team left the bank with incredible delay';
                            AfterInTheBank := 'DrivingAway';
                            NextDiff := 75;
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end
                    else
                      begin
                        Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                        Profit := 0;
                        EndMission;
                      end;
                  end;
                if BeforeDeadlyIncident = 'ControlViolentOutburst4'
                then
                  begin
                    Charges.Values['Charge3'] := 'Murder';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                    Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                    if FirearmTable(Diff)
                    then
                      begin
                        if FirearmTable(Diff)
                        then
                          begin
                            if FirearmTable(Diff)
                            then
                              begin
                                Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team left the bank with more incredible delay';
                                AfterInTheBank := 'DrivingAway';
                                NextDiff := 80;
                              end
                            else
                              begin
                                Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                                Profit := 0;
                                EndMission;
                              end;
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end
                    else
                      begin
                        Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                        Profit := 0;
                        EndMission;
                      end;
                  end;
              end;
          end
        else
          begin
            if Vault = 'Explosive'
            then
              case IllSystem.UseSkill(Gorilla.SkillVal, Diff) of
                1:
                  begin
                    if BeforeDeadlyIncident = 'ControlViolentOutburst3'
                    then
                      begin
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' was in the end able to control the panicked customer. The team lost some time for the explosion of the vault';
                        OpenVaultExpl(70);
                      end;
                    if BeforeDeadlyIncident = 'ManageHostages'
                    then
                      begin
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' was in the end able to control the panicked customer, but injured him in the process. The team lost some time for the explosion of the vault';
                        Charges.Values['Charge3'] := 'AggravatedAssault';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        OpenVaultExpl(70);
                      end;
                    if BeforeDeadlyIncident = 'ControlViolentOutburst4'
                    then
                      begin
                        Charges.Values['Charge3'] := 'AggravatedAssault';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' was in the end able to control the panicked customer, but injured him and was hurt by him in the process. The team lost some time for the explosion of the vault';
                        OpenVaultExpl(70);
                      end;
                  end;
                2:
                  begin
                    if BeforeDeadlyIncident = 'ControlViolentOutburst3'
                    then
                      begin
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' was in the end able to control the panicked customer, but injured him in the process. The team lost some significant time for the explosion of the vault';
                        Charges.Values['Charge3'] := 'AggravatedAssault';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        OpenVaultExpl(75);
                      end;
                    if BeforeDeadlyIncident = 'ManageHostages'
                    then
                      begin
                        Charges.Values['Charge3'] := 'Murder';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        IllSystem.RDOChangeCriminalState(crim.Name, 'InTeam: InMission: Wounded');
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. The team lost some significant time for the explosion of the vault';
                        OpenVaultExpl(75);
                      end;
                    if BeforeDeadlyIncident = 'ControlViolentOutburst4'
                    then
                      begin
                        Charges.Values['Charge3'] := 'Murder';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                        if FirearmTable(Diff)
                        then
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team lost some significant time for the explosion of the vault';
                            OpenVaultExpl(75);
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end;
                  end;
                3:
                  begin
                    if BeforeDeadlyIncident = 'ControlViolentOutburst3'
                    then
                      begin
                        Charges.Values['Charge3'] := 'Murder';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer. ';
                        if FirearmTable(Diff)
                        then
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team lost a big amount of time for the explosion of the vault';
                            OpenVaultExpl(75);
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end;
                    if BeforeDeadlyIncident = 'ManageHostages'
                    then
                      begin
                        Charges.Values['Charge3'] := 'Murder';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                        if FirearmTable(Diff)
                        then
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team lost a big amount of time for the explosion of the vault';
                            OpenVaultExpl(75);
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end;
                    if BeforeDeadlyIncident = 'ControlViolentOutburst4'
                    then
                      begin
                        Charges.Values['Charge3'] := 'Murder';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                        if FirearmTable(Diff)
                        then
                          begin
                            if FirearmTable(Diff)
                            then
                              begin
                                Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team lost a big amount of time for the explosion of the vault';
                                OpenVaultExpl(75);
                              end
                            else
                              begin
                                Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                                Profit := 0;
                                EndMission;
                              end;
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end;
                  end;
                4:
                  begin
                    if BeforeDeadlyIncident = 'ControlViolentOutburst3'
                    then
                      begin
                        Charges.Values['Charge3'] := 'Murder';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                        if FirearmTable(Diff)
                        then
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team lost an incredible amount of time for the explosion of the vault';
                            OpenVaultExpl(80);
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end;
                    if BeforeDeadlyIncident = 'ManageHostages'
                    then
                      begin
                        Charges.Values['Charge3'] := 'Murder';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                        if FirearmTable(Diff)
                        then
                          begin
                            if FirearmTable(Diff)
                            then
                              begin
                                Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team lost an incredible amount of time for the explosion of the vault';
                                OpenVaultExpl(80);
                              end
                            else
                              begin
                                Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                                Profit := 0;
                                EndMission;
                              end;
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end;
                    if BeforeDeadlyIncident = 'ControlViolentOutburst4'
                    then
                      begin
                        Charges.Values['Charge3'] := 'Murder';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                        if FirearmTable(Diff)
                        then
                          begin
                            if FirearmTable(Diff)
                            then
                              begin
                                if FirearmTable(Diff)
                                then
                                  begin
                                    Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team lost an incredible amount of time for the explosion of the vault';
                                    OpenVaultExpl(80);
                                  end
                                else
                                  begin
                                    Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                                    Profit := 0;
                                    EndMission;
                                  end;
                              end
                            else
                              begin
                                Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                                Profit := 0;
                                EndMission;
                              end;
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end;
                  end;
              end
            else
              case IllSystem.UseSkill(Gorilla.SkillVal, Diff) of
                1:
                  begin
                    if BeforeDeadlyIncident = 'ControlViolentOutburst3'
                    then
                      begin
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' was in the end able to control the panicked customer. The team lost some time for the opening of the vault';
                        OpenVaultComp(70);
                      end;
                    if BeforeDeadlyIncident = 'ManageHostages'
                    then
                      begin
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' was in the end able to control the panicked customer, but injured him in the process. The team lost some time for the opening of the vault';
                        Charges.Values['Charge3'] := 'AggravatedAssault';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        OpenVaultComp(70);
                      end;
                    if BeforeDeadlyIncident = 'ControlViolentOutburst4'
                    then
                      begin
                        Charges.Values['Charge3'] := 'AggravatedAssault';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' was in the end able to control the panicked customer, but injured him and was hurt by him in the process. The team lost some time for the opening of the vault';
                        OpenVaultComp(70);
                      end;
                  end;
                2:
                  begin
                    if BeforeDeadlyIncident = 'ControlViolentOutburst3'
                    then
                      begin
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' was in the end able to control the panicked customer, but injured him in the process. The team lost some significant time for the opening of the vault';
                        Charges.Values['Charge3'] := 'AggravatedAssault';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        OpenVaultComp(75);
                      end;
                    if BeforeDeadlyIncident = 'ManageHostages'
                    then
                      begin
                        Charges.Values['Charge3'] := 'Murder';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        IllSystem.RDOChangeCriminalState(crim.Name, 'InTeam: InMission: Wounded');
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. The team lost some significant time for the opening of the vault';
                        OpenVaultComp(75);
                      end;
                    if BeforeDeadlyIncident = 'ControlViolentOutburst4'
                    then
                      begin
                        Charges.Values['Charge3'] := 'Murder';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                        if FirearmTable(Diff)
                        then
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team lost some significant time for the opening of the vault';
                            OpenVaultComp(75);
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end;
                  end;
                3:
                  begin
                    if BeforeDeadlyIncident = 'ControlViolentOutburst3'
                    then
                      begin
                        Charges.Values['Charge3'] := 'Murder';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer. ';
                        if FirearmTable(Diff)
                        then
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team lost a big amount of time for the opening of the vault';
                            OpenVaultComp(75);
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end;
                    if BeforeDeadlyIncident = 'ManageHostages'
                    then
                      begin
                        Charges.Values['Charge3'] := 'Murder';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                        if FirearmTable(Diff)
                        then
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team lost a big amount of time for the opening of the vault';
                            OpenVaultComp(75);
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end;
                    if BeforeDeadlyIncident = 'ControlViolentOutburst4'
                    then
                      begin
                        Charges.Values['Charge3'] := 'Murder';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                        if FirearmTable(Diff)
                        then
                          begin
                            if FirearmTable(Diff)
                            then
                              begin
                                Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team lost a big amount of time for the opening of the vault';
                                OpenVaultComp(75);
                              end
                            else
                              begin
                                Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                                Profit := 0;
                                EndMission;
                              end;
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end;
                  end;
                4:
                  begin
                    if BeforeDeadlyIncident = 'ControlViolentOutburst3'
                    then
                      begin
                        Charges.Values['Charge3'] := 'Murder';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                        if FirearmTable(Diff)
                        then
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team lost an incredible amount of time for the opening of the vault';
                            OpenVaultComp(80);
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end;
                    if BeforeDeadlyIncident = 'ManageHostages'
                    then
                      begin
                        Charges.Values['Charge3'] := 'Murder';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                        if FirearmTable(Diff)
                        then
                          begin
                            if FirearmTable(Diff)
                            then
                              begin
                                Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team lost an incredible amount of time for the opening of the vault';
                                OpenVaultComp(80);
                              end
                            else
                              begin
                                Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                                Profit := 0;
                                EndMission;
                              end;
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end;
                    if BeforeDeadlyIncident = 'ControlViolentOutburst4'
                    then
                      begin
                        Charges.Values['Charge3'] := 'Murder';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                        if FirearmTable(Diff)
                        then
                          begin
                            if FirearmTable(Diff)
                            then
                              begin
                                if FirearmTable(Diff)
                                then
                                  begin
                                    Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team lost an incredible amount of time for the opening of the vault';
                                    OpenVaultComp(80);
                                  end
                                else
                                  begin
                                    Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                                    Profit := 0;
                                    EndMission;
                                  end;
                              end
                            else
                              begin
                                Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                                Profit := 0;
                                EndMission;
                              end;
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end;
                  end;
              end;
          end;
      if AlarmState = 2
      then
        if Vault = ''
        then
          case IllSystem.UseSkill(Gorilla.SkillVal, Diff) of
            1:
              begin
                if BeforeDeadlyIncident = 'ControlViolentOutburst3'
                then
                  begin
                    Report.Values['DeadlyIncident'] := Gorilla.Name + ' was in the end able to control the panicked customer. The team was about to leave the bank, when they realize they were surrounded by the police. Someone must have pressed a silent alarm';
                    AfterInTheBank := 'HostageStandOff';
                    NextDiff := 50;
                  end;
                if BeforeDeadlyIncident = 'ManageHostages'
                then
                  begin
                    Report.Values['DeadlyIncident'] := Gorilla.Name + ' was in the end able to control the panicked customer, but injured him in the process. The team was about to leave the bank, when they realize they were surrounded by the police. Someone must have pressed a silent alarm';
                    Charges.Values['Charge3'] := 'AggravatedAssault';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    AfterInTheBank := 'HostageStandOff';
                    NextDiff := 50;
                  end;
                if BeforeDeadlyIncident = 'ControlViolentOutburst4'
                then
                  begin
                    Charges.Values['Charge3'] := 'AggravatedAssault';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                    Report.Values['DeadlyIncident'] := Gorilla.Name + ' was in the end able to control the panicked customer, but injured him and was hurt by him in the process. The team was about to leave the bank, when they realize they were surrounded by the police. Someone must have pressed a silent alarm';
                    AfterInTheBank := 'HostageStandOff';
                    NextDiff := 50;
                  end;
              end;
            2:
              begin
                if BeforeDeadlyIncident = 'ControlViolentOutburst3'
                then
                  begin
                    Report.Values['DeadlyIncident'] := Gorilla.Name + ' was in the end able to control the panicked customer, but injured him in the process. The team was about to leave the bank, when they realize they were surrounded by the police. Someone must have pressed a silent alarm';
                    Charges.Values['Charge3'] := 'AggravatedAssault';
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    Charges.Values['Offender31'] := Gorilla.Name;
                    AfterInTheBank := 'HostageStandOff';
                    NextDiff := 60;
                  end;
                if BeforeDeadlyIncident = 'ManageHostages'
                then
                  begin
                    Charges.Values['Charge3'] := 'Murder';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    IllSystem.RDOChangeCriminalState(crim.Name, 'InTeam: InMission: Wounded');
                    Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. The team was about to leave the bank, when they realize they were surrounded by the police. Someone must have pressed a silent alarm';
                    AfterInTheBank := 'HostageStandOff';
                    NextDiff := 60;
                  end;
                if BeforeDeadlyIncident = 'ControlViolentOutburst4'
                then
                  begin
                    Charges.Values['Charge3'] := 'Murder';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                    Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                    if FirearmTable(Diff)
                    then
                      begin
                        Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team was about to leave the bank, when they realize they were surrounded by the police. Someone must have pressed a silent alarm';
                        AfterInTheBank := 'HostageStandOff';
                        NextDiff := 60;
                      end
                    else
                      begin
                        Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                        Profit := 0;
                        EndMission;
                      end;
                  end;
              end;
            3:
              begin
                if BeforeDeadlyIncident = 'ControlViolentOutburst3'
                then
                  begin
                    Charges.Values['Charge3'] := 'Murder';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer. ';
                    if FirearmTable(Diff)
                    then
                      begin
                        Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team was about to leave the bank, when they realize they were surrounded by the police. Someone must have pressed a silent alarm';
                        AfterInTheBank := 'HostageStandOff';
                        NextDiff := 70;
                      end
                    else
                      begin
                        Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                        Profit := 0;
                        EndMission;
                      end;
                  end;
                if BeforeDeadlyIncident = 'ManageHostages'
                then
                  begin
                    Charges.Values['Charge3'] := 'Murder';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                    Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                    if FirearmTable(Diff)
                    then
                      begin
                        Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team was about to leave the bank, when they realize they were surrounded by the police. Someone must have pressed a silent alarm';
                        AfterInTheBank := 'HostageStandOff';
                        NextDiff := 70;
                      end
                    else
                      begin
                        Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                        Profit := 0;
                        EndMission;
                      end;
                  end;
                if BeforeDeadlyIncident = 'ControlViolentOutburst4'
                then
                  begin
                    Charges.Values['Charge3'] := 'Murder';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                    Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                    if FirearmTable(Diff)
                    then
                      begin
                        if FirearmTable(Diff)
                        then
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team was about to leave the bank, when they realize they were surrounded by the police. Someone must have pressed a silent alarm';
                            AfterInTheBank := 'HostageStandOff';
                            NextDiff := 70;
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end
                    else
                      begin
                        Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                        Profit := 0;
                        EndMission;
                      end;
                  end;
              end;
            4:
              begin
                if BeforeDeadlyIncident = 'ControlViolentOutburst3'
                then
                  begin
                    Charges.Values['Charge3'] := 'Murder';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                    Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                    if FirearmTable(Diff)
                    then
                      begin
                        Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team was about to leave the bank, when they realize they were surrounded by the police. Someone must have pressed a silent alarm';
                        AfterInTheBank := 'HostageStandOff';
                        NextDiff := 80;
                      end
                    else
                      begin
                        Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                        Profit := 0;
                        EndMission;
                      end;
                  end;
                if BeforeDeadlyIncident = 'ManageHostages'
                then
                  begin
                    Charges.Values['Charge3'] := 'Murder';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                    Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                    if FirearmTable(Diff)
                    then
                      begin
                        if FirearmTable(Diff)
                        then
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team was about to leave the bank, when they realize they were surrounded by the police. Someone must have pressed a silent alarm';
                            AfterInTheBank := 'HostageStandOff';
                            NextDiff := 80;
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end
                    else
                      begin
                        Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                        Profit := 0;
                        EndMission;
                      end;
                  end;
                if BeforeDeadlyIncident = 'ControlViolentOutburst4'
                then
                  begin
                    Charges.Values['Charge3'] := 'Murder';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                    Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                    if FirearmTable(Diff)
                    then
                      begin
                        if FirearmTable(Diff)
                        then
                          begin
                            if FirearmTable(Diff)
                            then
                              begin
                                Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team was about to leave the bank, when they realize they were surrounded by the police. Someone must have pressed a silent alarm';
                                AfterInTheBank := 'HostageStandOff';
                                NextDiff := 80;
                              end
                            else
                              begin
                                Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                                Profit := 0;
                                EndMission;
                              end;
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end
                    else
                      begin
                        Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                        Profit := 0;
                        EndMission;
                      end;
                  end;
              end;
          end
        else
          begin
            if Vault = 'Explosive'
            then
              case IllSystem.UseSkill(Gorilla.SkillVal, Diff) of
                1:
                  begin
                    if BeforeDeadlyIncident = 'ControlViolentOutburst3'
                    then
                      begin
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' was in the end able to control the panicked customer. The team lost some time for the explosion of the vault';
                        OpenVaultExpl(70);
                      end;
                    if BeforeDeadlyIncident = 'ManageHostages'
                    then
                      begin
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' was in the end able to control the panicked customer, but injured him in the process. The team lost some time for the explosion of the vault';
                        Charges.Values['Charge3'] := 'AggravatedAssault';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        OpenVaultExpl(70);
                      end;
                    if BeforeDeadlyIncident = 'ControlViolentOutburst4'
                    then
                      begin
                        Charges.Values['Charge3'] := 'AggravatedAssault';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' was in the end able to control the panicked customer, but injured him and was hurt by him in the process. The team lost some time for the explosion of the vault';
                        OpenVaultExpl(70);
                      end;
                  end;
                2:
                  begin
                    if BeforeDeadlyIncident = 'ControlViolentOutburst3'
                    then
                      begin
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' was in the end able to control the panicked customer, but injured him in the process. The team lost some significant time for the explosion of the vault';
                        Charges.Values['Charge3'] := 'AggravatedAssault';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        OpenVaultExpl(75);
                      end;
                    if BeforeDeadlyIncident = 'ManageHostages'
                    then
                      begin
                        Charges.Values['Charge3'] := 'Murder';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        IllSystem.RDOChangeCriminalState(crim.Name, 'InTeam: InMission: Wounded');
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. The team lost some significant time for the explosion of the vault';
                        OpenVaultExpl(75);
                      end;
                    if BeforeDeadlyIncident = 'ControlViolentOutburst4'
                    then
                      begin
                        Charges.Values['Charge3'] := 'Murder';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                        if FirearmTable(Diff)
                        then
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team lost some significant time for the explosion of the vault';
                            OpenVaultExpl(75);
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end;
                  end;
                3:
                  begin
                    if BeforeDeadlyIncident = 'ControlViolentOutburst3'
                    then
                      begin
                        Charges.Values['Charge3'] := 'Murder';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer. ';
                        if FirearmTable(Diff)
                        then
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team lost a big amount of time for the explosion of the vault';
                            OpenVaultExpl(75);
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end;
                    if BeforeDeadlyIncident = 'ManageHostages'
                    then
                      begin
                        Charges.Values['Charge3'] := 'Murder';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                        if FirearmTable(Diff)
                        then
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team lost a big amount of time for the explosion of the vault';
                            OpenVaultExpl(75);
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end;
                    if BeforeDeadlyIncident = 'ControlViolentOutburst4'
                    then
                      begin
                        Charges.Values['Charge3'] := 'Murder';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                        if FirearmTable(Diff)
                        then
                          begin
                            if FirearmTable(Diff)
                            then
                              begin
                                Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team lost a big amount of time for the explosion of the vault';
                                OpenVaultExpl(75);
                              end
                            else
                              begin
                                Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                                Profit := 0;
                                EndMission;
                              end;
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end;
                  end;
                4:
                  begin
                    if BeforeDeadlyIncident = 'ControlViolentOutburst3'
                    then
                      begin
                        Charges.Values['Charge3'] := 'Murder';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                        if FirearmTable(Diff)
                        then
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team lost an incredible amount of time. When they were about to open the vault, they realized they were surrounded by the police. Someone had pressed a silent alarm';
                            AfterInTheBank := 'HostageStandOff';
                            NextDiff := 80;
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end;
                    if BeforeDeadlyIncident = 'ManageHostages'
                    then
                      begin
                        Charges.Values['Charge3'] := 'Murder';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                        if FirearmTable(Diff)
                        then
                          begin
                            if FirearmTable(Diff)
                            then
                              begin
                                Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team lost an incredible amount of time. When they were about to open the vault, they realized they were surrounded by the police. Someone had pressed a silent alarm';
                                AfterInTheBank := 'HostageStandOff';
                                NextDiff := 80;
                              end
                            else
                              begin
                                Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                                Profit := 0;
                                EndMission;
                              end;
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end;
                    if BeforeDeadlyIncident = 'ControlViolentOutburst4'
                    then
                      begin
                        Charges.Values['Charge3'] := 'Murder';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                        if FirearmTable(Diff)
                        then
                          begin
                            if FirearmTable(Diff)
                            then
                              begin
                                if FirearmTable(Diff)
                                then
                                  begin
                                    Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team lost an incredible amount of time. When they were about to open the vault, they realized they were surrounded by the police. Someone had pressed a silent alarm';
                                    AfterInTheBank := 'HostageStandOff';
                                    NextDiff := 80;
                                  end
                                else
                                  begin
                                    Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                                    Profit := 0;
                                    EndMission;
                                  end;
                              end
                            else
                              begin
                                Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                                Profit := 0;
                                EndMission;
                              end;
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end;
                  end;
              end
            else
              case IllSystem.UseSkill(Gorilla.SkillVal, Diff) of
                1:
                  begin
                    if BeforeDeadlyIncident = 'ControlViolentOutburst3'
                    then
                      begin
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' was in the end able to control the panicked customer. The team lost some time for the opening of the vault';
                        OpenVaultComp(70);
                      end;
                    if BeforeDeadlyIncident = 'ManageHostages'
                    then
                      begin
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' was in the end able to control the panicked customer, but injured him in the process. The team lost some time for the opening of the vault';
                        Charges.Values['Charge3'] := 'AggravatedAssault';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        OpenVaultComp(70);
                      end;
                    if BeforeDeadlyIncident = 'ControlViolentOutburst4'
                    then
                      begin
                        Charges.Values['Charge3'] := 'AggravatedAssault';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' was in the end able to control the panicked customer, but injured him and was hurt by him in the process. The team lost some time for the opening of the vault';
                        OpenVaultComp(70);
                      end;
                  end;
                2:
                  begin
                    if BeforeDeadlyIncident = 'ControlViolentOutburst3'
                    then
                      begin
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' was in the end able to control the panicked customer, but injured him in the process. The team lost some significant time for the opening of the vault';
                        Charges.Values['Charge3'] := 'AggravatedAssault';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        OpenVaultComp(75);
                      end;
                    if BeforeDeadlyIncident = 'ManageHostages'
                    then
                      begin
                        Charges.Values['Charge3'] := 'Murder';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        IllSystem.RDOChangeCriminalState(crim.Name, 'InTeam: InMission: Wounded');
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. The team lost some significant time for the opening of the vault';
                        OpenVaultComp(75);
                      end;
                    if BeforeDeadlyIncident = 'ControlViolentOutburst4'
                    then
                      begin
                        Charges.Values['Charge3'] := 'Murder';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                        if FirearmTable(Diff)
                        then
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team lost some significant time for the opening of the vault';
                            OpenVaultComp(75);
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end;
                  end;
                3:
                  begin
                    if BeforeDeadlyIncident = 'ControlViolentOutburst3'
                    then
                      begin
                        Charges.Values['Charge3'] := 'Murder';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer. ';
                        if FirearmTable(Diff)
                        then
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team lost a big amount of time for the opening of the vault';
                            OpenVaultComp(75);
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end;
                    if BeforeDeadlyIncident = 'ManageHostages'
                    then
                      begin
                        Charges.Values['Charge3'] := 'Murder';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                        if FirearmTable(Diff)
                        then
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team lost a big amount of time for the opening of the vault';
                            OpenVaultComp(75);
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end;
                    if BeforeDeadlyIncident = 'ControlViolentOutburst4'
                    then
                      begin
                        Charges.Values['Charge3'] := 'Murder';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                        if FirearmTable(Diff)
                        then
                          begin
                            if FirearmTable(Diff)
                            then
                              begin
                                Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team lost a big amount of time for the opening of the vault';
                                OpenVaultComp(75);
                              end
                            else
                              begin
                                Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                                Profit := 0;
                                EndMission;
                              end;
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end;
                  end;
                4:
                  begin
                    if BeforeDeadlyIncident = 'ControlViolentOutburst3'
                    then
                      begin
                        Charges.Values['Charge3'] := 'Murder';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                        if FirearmTable(Diff)
                        then
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team lost an incredible amount of time. When they were about to open the vault, they realized they were surrounded by the police. Someone had pressed a silent alarm';
                            AfterInTheBank := 'HostageStandOff';
                            NextDiff := 80;
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end;
                    if BeforeDeadlyIncident = 'ManageHostages'
                    then
                      begin
                        Charges.Values['Charge3'] := 'Murder';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                        if FirearmTable(Diff)
                        then
                          begin
                            if FirearmTable(Diff)
                            then
                              begin
                                Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team lost an incredible amount of time. When they were about to open the vault, they realized they were surrounded by the police. Someone had pressed a silent alarm';
                                AfterInTheBank := 'HostageStandOff';
                                NextDiff := 80;
                              end
                            else
                              begin
                                Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                                Profit := 0;
                                EndMission;
                              end;
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end;
                    if BeforeDeadlyIncident = 'ControlViolentOutburst4'
                    then
                      begin
                        Charges.Values['Charge3'] := 'Murder';
                        Charges.Values['Offender31'] := Gorilla.Name;
                        Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                        IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                        Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                        if FirearmTable(Diff)
                        then
                          begin
                            if FirearmTable(Diff)
                            then
                              begin
                                if FirearmTable(Diff)
                                then
                                  begin
                                    Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team lost an incredible amount of time. When they were about to open the vault, they realized they were surrounded by the police. Someone had pressed a silent alarm';
                                    AfterInTheBank := 'HostageStandOff';
                                    NextDiff := 80;
                                  end
                                else
                                  begin
                                    Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                                    Profit := 0;
                                    EndMission;
                                  end;
                              end
                            else
                              begin
                                Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                                Profit := 0;
                                EndMission;
                              end;
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end;
                  end;
              end;
          end;
      if AlarmState = 3
      then
        if Vault = ''
        then
          case IllSystem.UseSkill(Gorilla.SkillVal, Diff) of
            1:
              begin
                if BeforeDeadlyIncident = 'ControlViolentOutburst3'
                then
                  begin
                    Report.Values['DeadlyIncident'] := Gorilla.Name + ' was in the end able to control the panicked customer. The team was about to leave the bank';
                    PoliceArriving(50);
                  end;
                if BeforeDeadlyIncident = 'ManageHostages'
                then
                  begin
                    Report.Values['DeadlyIncident'] := Gorilla.Name + ' was in the end able to control the panicked customer, but injured him in the process. The team was about to leave the bank';
                    Charges.Values['Charge3'] := 'AggravatedAssault';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    PoliceArriving(50);
                  end;
                if BeforeDeadlyIncident = 'ControlViolentOutburst4'
                then
                  begin
                    Charges.Values['Charge3'] := 'AggravatedAssault';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                    Report.Values['DeadlyIncident'] := Gorilla.Name + ' was in the end able to control the panicked customer, but injured him and was hurt by him in the process. The team was about to leave the bank';
                    PoliceArriving(50);
                  end;
              end;
            2:
              begin
                if BeforeDeadlyIncident = 'ControlViolentOutburst3'
                then
                  begin
                    Report.Values['DeadlyIncident'] := Gorilla.Name + ' was in the end able to control the panicked customer, but injured him in the process. The team was about to leave the bank';
                    Charges.Values['Charge3'] := 'AggravatedAssault';
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    Charges.Values['Offender31'] := Gorilla.Name;
                    PoliceArriving(60);
                  end;
                if BeforeDeadlyIncident = 'ManageHostages'
                then
                  begin
                    Charges.Values['Charge3'] := 'Murder';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    IllSystem.RDOChangeCriminalState(crim.Name, 'InTeam: InMission: Wounded');
                    Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. The team was about to leave the bank';
                    PoliceArriving(60);
                  end;
                if BeforeDeadlyIncident = 'ControlViolentOutburst4'
                then
                  begin
                    Charges.Values['Charge3'] := 'Murder';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                    Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                    if FirearmTable(Diff)
                    then
                      begin
                        Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team was about to leave the bank';
                        PoliceArriving(60);
                      end
                    else
                      begin
                        Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                        Profit := 0;
                        EndMission;
                      end;
                  end;
              end;
            3:
              begin
                if BeforeDeadlyIncident = 'ControlViolentOutburst3'
                then
                  begin
                    Charges.Values['Charge3'] := 'Murder';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer. ';
                    if FirearmTable(Diff)
                    then
                      begin
                        Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team was about to leave the bank';
                        PoliceArriving(70);
                      end
                    else
                      begin
                        Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                        Profit := 0;
                        EndMission;
                      end;
                  end;
                if BeforeDeadlyIncident = 'ManageHostages'
                then
                  begin
                    Charges.Values['Charge3'] := 'Murder';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                    Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                    if FirearmTable(Diff)
                    then
                      begin
                        Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team was about to leave the bank';
                        PoliceArriving(70);
                      end
                    else
                      begin
                        Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                        Profit := 0;
                        EndMission;
                      end;
                  end;
                if BeforeDeadlyIncident = 'ControlViolentOutburst4'
                then
                  begin
                    Charges.Values['Charge3'] := 'Murder';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                    Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                    if FirearmTable(Diff)
                    then
                      begin
                        if FirearmTable(Diff)
                        then
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team was about to leave the bank';
                            PoliceArriving(70);
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end
                    else
                      begin
                        Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                        Profit := 0;
                        EndMission;
                      end;
                  end;
              end;
            4:
              begin
                if BeforeDeadlyIncident = 'ControlViolentOutburst3'
                then
                  begin
                    Charges.Values['Charge3'] := 'Murder';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                    Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                    if FirearmTable(Diff)
                    then
                      begin
                        Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team was about to leave the bank, when they realize they were surrounded by the police';
                        AfterInTheBank := 'HostageStandOff';
                        NextDiff := 75;
                      end
                    else
                      begin
                        Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                        Profit := 0;
                        EndMission;
                      end;
                  end;
                if BeforeDeadlyIncident = 'ManageHostages'
                then
                  begin
                    Charges.Values['Charge3'] := 'Murder';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                    Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                    if FirearmTable(Diff)
                    then
                      begin
                        if FirearmTable(Diff)
                        then
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team was about to leave the bank, when they realize they were surrounded by the police';
                            AfterInTheBank := 'HostageStandOff';
                            NextDiff := 75;
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end
                    else
                      begin
                        Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                        Profit := 0;
                        EndMission;
                      end;
                  end;
                if BeforeDeadlyIncident = 'ControlViolentOutburst4'
                then
                  begin
                    Charges.Values['Charge3'] := 'Murder';
                    Charges.Values['Offender31'] := Gorilla.Name;
                    Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                    IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                    Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                    if FirearmTable(Diff)
                    then
                      begin
                        if FirearmTable(Diff)
                        then
                          begin
                            if FirearmTable(Diff)
                            then
                              begin
                                Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team was about to leave the bank, when they realize they were surrounded by the police';
                                AfterInTheBank := 'HostageStandOff';
                                NextDiff := 75;
                              end
                            else
                              begin
                                Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                                Profit := 0;
                                EndMission;
                              end;
                          end
                        else
                          begin
                            Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                            Profit := 0;
                            EndMission;
                          end;
                      end
                    else
                      begin
                        Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                        Profit := 0;
                        EndMission;
                      end;
                  end;
              end;
          end
        else
          begin
            case IllSystem.UseSkill(Gorilla.SkillVal, Diff) of
              1:
                begin
                  if BeforeDeadlyIncident = 'ControlViolentOutburst3'
                  then
                    begin
                      Report.Values['DeadlyIncident'] := Gorilla.Name + ' was in the end able to control the panicked customer. Since the alarm went on, there was no time to open the vault';
                      PoliceArriving(50);
                    end;
                  if BeforeDeadlyIncident = 'ManageHostages'
                  then
                    begin
                      Report.Values['DeadlyIncident'] := Gorilla.Name + ' was in the end able to control the panicked customer, but injured him in the process. Since the alarm went on, there was no time to open the vault';
                      Charges.Values['Charge3'] := 'AggravatedAssault';
                      Charges.Values['Offender31'] := Gorilla.Name;
                      Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                      PoliceArriving(50);
                    end;
                  if BeforeDeadlyIncident = 'ControlViolentOutburst4'
                  then
                    begin
                      Charges.Values['Charge3'] := 'AggravatedAssault';
                      Charges.Values['Offender31'] := Gorilla.Name;
                      Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                      IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                      Report.Values['DeadlyIncident'] := Gorilla.Name + ' was in the end able to control the panicked customer, but injured him and was hurt by him in the process. Since the alarm went on, there was no time to open the vault';
                      PoliceArriving(50);
                    end;
                end;
              2:
                begin
                  if BeforeDeadlyIncident = 'ControlViolentOutburst3'
                  then
                    begin
                      Report.Values['DeadlyIncident'] := Gorilla.Name + ' was in the end able to control the panicked customer, but injured him in the process. Since the alarm went on, there was no time to open the vault';
                      Charges.Values['Charge3'] := 'AggravatedAssault';
                      Charges.Values['Offender31'] := Gorilla.Name;
                      Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                      PoliceArriving(60);
                    end;
                  if BeforeDeadlyIncident = 'ManageHostages'
                  then
                    begin
                      Charges.Values['Charge3'] := 'Murder';
                      Charges.Values['Offender31'] := Gorilla.Name;
                      Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                      IllSystem.RDOChangeCriminalState(crim.Name, 'InTeam: InMission: Wounded');
                      Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. Since the alarm went on, there was no time to open the vault';
                      PoliceArriving(60);
                    end;
                  if BeforeDeadlyIncident = 'ControlViolentOutburst4'
                  then
                    begin
                      Charges.Values['Charge3'] := 'Murder';
                      Charges.Values['Offender31'] := Gorilla.Name;
                      Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                      IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                      Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                      if FirearmTable(Diff)
                      then
                        begin
                          Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Since the alarm went on, there was no time to open the vault';
                          PoliceArriving(60);
                        end
                      else
                        begin
                          Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                          Profit := 0;
                          EndMission;
                        end;
                    end;
                end;
              3:
                begin
                  if BeforeDeadlyIncident = 'ControlViolentOutburst3'
                  then
                    begin
                      Charges.Values['Charge3'] := 'Murder';
                      Charges.Values['Offender31'] := Gorilla.Name;
                      Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                      Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer. ';
                      if FirearmTable(Diff)
                      then
                        begin
                          Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Since the alarm went on, there was no time to open the vault';
                          PoliceArriving(60);
                        end
                      else
                        begin
                          Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                          Profit := 0;
                          EndMission;
                        end;
                    end;
                  if BeforeDeadlyIncident = 'ManageHostages'
                  then
                    begin
                      Charges.Values['Charge3'] := 'Murder';
                      Charges.Values['Offender31'] := Gorilla.Name;
                      Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                      IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                      Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                      if FirearmTable(Diff)
                      then
                        begin
                          Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Since the alarm went on, there was no time to open the vault';
                          PoliceArriving(60);
                        end
                      else
                        begin
                          Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                          Profit := 0;
                          EndMission;
                        end;
                    end;
                  if BeforeDeadlyIncident = 'ControlViolentOutburst4'
                  then
                    begin
                      Charges.Values['Charge3'] := 'Murder';
                      Charges.Values['Offender31'] := Gorilla.Name;
                      Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                      IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                      Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                      if FirearmTable(Diff)
                      then
                        begin
                          if FirearmTable(Diff)
                          then
                            begin
                              Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Since the alarm went on, there was no time to open the vault';
                              PoliceArriving(60);
                            end
                          else
                            begin
                              Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                              Profit := 0;
                              EndMission;
                            end;
                        end
                      else
                        begin
                          Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                          Profit := 0;
                          EndMission;
                        end;
                    end;
                end;
              4:
                begin
                  if BeforeDeadlyIncident = 'ControlViolentOutburst3'
                  then
                    begin
                      Charges.Values['Charge3'] := 'Murder';
                      Charges.Values['Offender31'] := Gorilla.Name;
                      Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                      IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                      Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                      if FirearmTable(Diff)
                      then
                        begin
                          Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team lost an incredible amount of time. The result was they were surrounded by the police even before they could consider opening the vault';
                          AfterInTheBank := 'HostageStandOff';
                          NextDiff := 80;
                        end
                      else
                        begin
                          Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                          Profit := 0;
                          EndMission;
                        end;
                    end;
                  if BeforeDeadlyIncident = 'ManageHostages'
                  then
                    begin
                      Charges.Values['Charge3'] := 'Murder';
                      Charges.Values['Offender31'] := Gorilla.Name;
                      Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                      IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                      Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                      if FirearmTable(Diff)
                      then
                        begin
                          if FirearmTable(Diff)
                          then
                            begin
                              Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team lost an incredible amount of time. The result was they were surrounded by the police even before they could consider opening the vault';
                              AfterInTheBank := 'HostageStandOff';
                              NextDiff := 80;
                            end
                          else
                            begin
                              Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                              Profit := 0;
                              EndMission;
                            end;
                        end
                      else
                        begin
                          Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                          Profit := 0;
                          EndMission;
                        end;
                    end;
                  if BeforeDeadlyIncident = 'ControlViolentOutburst4'
                  then
                    begin
                      Charges.Values['Charge3'] := 'Murder';
                      Charges.Values['Offender31'] := Gorilla.Name;
                      Charges.Values['PoliceDiff3'] := Charges.Values['PoliceDiff2'];
                      IllSystem.RDOChangeCriminalState(Gorilla.Name, 'InTeam: InMission: Wounded');
                      Report.Values['DeadlyIncident'] := Gorilla.Name + ' killed the panicked customer, but was hurt by him in the process. ';
                      if FirearmTable(Diff)
                      then
                        begin
                          if FirearmTable(Diff)
                          then
                            begin
                              if FirearmTable(Diff)
                              then
                                begin
                                  Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'The team lost an incredible amount of time. The result was they were surrounded by the police even before they could consider opening the vault';
                                  AfterInTheBank := 'HostageStandOff';
                                  NextDiff := 80;
                                end
                              else
                                begin
                                  Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                                  Profit := 0;
                                  EndMission;
                                end;
                            end
                          else
                            begin
                              Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                              Profit := 0;
                              EndMission;
                            end;
                        end
                      else
                        begin
                          Report.Values['DeadlyIncident'] := Report.Values['DeadlyIncident'] + 'Every member of the team was dead. The mission had tragically finished';
                          Profit := 0;
                          EndMission;
                        end;
                    end;
                end;
            end;
       end;

    end;

  procedure TBankRobbery.PoliceArriving(Diff : single);
    var
      Leader : TCrimForTask;
    begin
      Leader := GetRightCriminalForTask('Leader', 'Leadership');
      case IllSystem.UseSkill(Leader.SkillVal, Diff) of
        1:
          begin
            Report.Values['PoliceArriving'] := 'Under the menace of the alarm, the team prepared to leave. ' + Leader.Name + ' organized the team very well for the departure. The team left way before the arrival of the police';
            NextDiff := 55;
            AfterInTheBank := 'DrivingAway';
          end;
        2:
          begin
            Report.Values['PoliceArriving'] := 'Under the menace of the alarm, the team prepared to leave. ' + Leader.Name + ' was able to make the leave left just before the arrival of the police';
            NextDiff := 65;
            AfterInTheBank := 'DrivingAway';
          end;
        3:
          begin
            Report.Values['PoliceArriving'] := 'Under the menace of the alarm, the team prepared to leave. ' + Leader.Name + ' wasn''t able to make the team leave before the arrival of the police. The bank is surrounded';
            NextDiff := 45;
            AfterInTheBank := 'HostageStandOff';
          end;
        4:
          begin
            Report.Values['PoliceArriving'] := 'Under the menace of the alarm, the team prepared to leave. ' + Leader.Name + ' wasn''t able to make the team leave before the arrival of the police. On top of that, on their way out they were surprised by the police surrounding the bank';
            NextDiff := 55;
            AfterInTheBank := 'HostageStandOff';
          end;
      end;
    end;

  procedure TBankRobbery.HostageStandOff(Diff : single);
    var
      Leader : TCrimForTask;
    begin
      Leader := GetRightCriminalForTask('Leader', 'Leadership');
      case IllSystem.UseSkill(Leader.SkillVal, Diff) of
        1:
          begin
            Report.Values['HostageStandOff'] := Leader.Name + ' started talking with the police, threatening to kill the hostages';
            NegDiff := 60;
            NegotiationProceeding(60);
          end;
        2:
          begin
            Report.Values['HostageStandOff'] := Leader.Name + ' , after recovering from the initial surprise, started talking with the police, threatening to kill the hostages';
            NegDiff := 70;
            NegotiationProceeding(70);
          end;
        3:
          begin
            Report.Values['HostageStandOff'] := Leader.Name + ' didn''t recover in time from the surprise. The police entered the bank. A shooting started';
            PoliceBreakIn(50);
          end;
        4:
          begin
            Report.Values['HostageStandOff'] := Leader.Name + ' and the team didn''t recover in time from the surprise. The police entered the bank. A shooting started';
            PoliceBreakIn(70);
          end;
      end;
    end;

  procedure TBankRobbery.NegotiationProceeding(Diff : single);
    var
      Leader : TCrimForTask;
    begin
      Leader := GetRightCriminalForTask('Leader', 'Leadership');
      case IllSystem.UseSkill(Leader.SkillVal, Diff) of
        1:
          begin
            Report.Values['NegotiationProceeding'] := Leader.Name + ' carried on the negotiation very well, and was able to arrange for the team to leave the bank in a car with some hostages';
            DrivingAway(90);
            ReadyToLeave := 'yes';
          end;
        2:
          begin
            Report.Values['NegotiationProceeding'] := Leader.Name + ' carried on the negotiation for some time. The team was getting nervous and nervous';
            NegDiff := NegDiff + 5;
            NextDiff := NegDiff;
            AfterInTheBank := 'NegotiationProceeding';
            ReadyToLeave := 'no';
          end;
        3:
          begin
            Report.Values['NegotiationProceeding'] := Leader.Name + ' wasn''t very good at negotiating and the team surrended';
            Surrender(20);
          end;
        4:
          begin
            Report.Values['NegotiationProceeding'] := Leader.Name + ' was very bad at negotiating and the police managed to enter the bank. A shooting started';
            PoliceBreakIn(60);
          end;
      end;
    end;

  procedure TBankRobbery.PoliceBreakIn(Diff : single);
    begin
    end;

  procedure TBankRobbery.Surrender(Diff : single);
    var
      i : integer;
      j : integer;
      k : integer;
      Leader : TCrimForTask;
      crim : TNameStatusSkill;
      crim2 : TNameStatusSkill;
    begin
      Leader := GetRightCriminalForTask('Leader', 'Leadership');
      Profit := 0;
      case IllSystem.UseSkill(Leader.SkillVal, Diff) of
        1:
          begin
            Report.Values['Surrender'] := Leader.Name + ' controlled the member of the teams and was able to make everybody surrender without incidents';
          end;
        2:
          begin
            crim2 := GetTeamMemberNameAndStatus('', '');
            if Crim2.Status = 'InTeam: InMission: Wounded'
                then
                  begin
                    IllSystem.RDOChangeCriminalState(Crim2.Name, 'InTeam: InMission: Dead');
                    Report.Values['Surrender'] := Leader.Name + ' controlled the members of the teams and was able to make everybody surrender quietly, excepts for ' + crim2.Name + ', that tried to react and was shot and killed by a policeman';
                  end;
                if Crim2.Status = 'InTeam: InMission'
                then
                  begin
                    IllSystem.RDOChangeCriminalState(Crim2.Name, 'InTeam: InMission: Wounded');
                    Report.Values['Surrender'] := Leader.Name + ' controlled the member of the teams and was able to make everybody surrender without incidents, excepts for ' + crim2.Name + ', that tried to react and was shot by a policeman';
                  end;
            Report.Values['Surrender'] := Leader.Name + ' controlled the members of the teams and was able to make everybody surrender without incidents, excepts for ' + crim2.Name + ', that tried to react and was shot by a policeman';
          end;
        3:
          begin
            crim2 := GetTeamMemberNameAndStatus('', '');
            IllSystem.RDOChangeCriminalState(Crim2.Name, 'InTeam: InMission: Dead');
            Report.Values['Surrender'] := Leader.Name + ' failed to control all the members of the teams. ' + crim2.Name + ' tried to react and was shot and killed by a policeman';
          end;
        4:
          begin
            for j := 1 to 8 do
              if Roles.Name[j] <> ''
              then
                begin
                  crim := GetTeamMemberNameAndStatus(Roles.Name[j], '');
                  if crim.Status = 'InTeam: InMission'
                  then
                    IllSystem.RDOChangeCriminalState(crim.Name, 'InTeam: InMission: Wounded');
                  if crim.Status = 'InTeam: InMission: Wounded'
                  then
                    IllSystem.RDOChangeCriminalState(crim.Name, 'Dead');
                end;
            Report.Values['Surrender'] := Leader.Name + ' was very bad at controlling the team members. The police shoot all of them to arrest them';
          end;
      end;
      for k := 1 to MaxCrimes do
        Charges.Values['PoliceDiff' + IntToStr(k)] := '0';
      for i := 1 to 8 do
        if Roles.Name[i] <> ''
        then
          begin
            crim := GetTeamMemberNameAndStatus(Roles.Name[i], '');
            IllSystem.GiveChargesToCriminal(crim.Name, Charges);
            if crim.Status = 'InTeam: InMission'
            then
              begin
                IllSystem.RDOChangeCriminalState(crim.Name, 'InJail');
                IllSystem.SetTrialDay(crim.Name);
              end;
            if crim.Status = 'InTeam: InMission: Wounded'
            then
              IllSystem.RDOChangeCriminalState(crim.Name, 'InJail: Wounded');
          end;
    end;

  procedure TBankRobbery.TeamDistressedEscape(Diff : single);
    begin
    end;

  procedure TBankRobbery.ShootingCars(Diff : single);
    begin
    end;

  procedure TBankRobbery.ArrestedWithInjuries(Diff : single);
    begin
    end;


end.
