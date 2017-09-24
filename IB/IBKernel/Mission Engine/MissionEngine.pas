unit MissionEngine;

interface

  uses
    classes, Collection, IBSystem, Persistent, BackupInterfaces, XMLFile;

  type
    TMissionParameters =
      packed record
        PsiFactor      : single;
        AccRisk        : single;
        Luck           : single;
        LastExperience : integer;
        LastCharge     : integer;
      end;

    TStepParameters =
      packed record
        Skill         : single;
        Stab          : single;
        Diff          : single;
        AccRiskWeight : single;
        SkillWeight   : single;
        LuckWeight    : single;
        CriminalCount : integer;
        Criminals     : array[0..MAXTEAM_MEMBERS - 1] of TCriminal;
      end;


    TMissionResult  = (mrOutstanding, mrSuccess, mrFailure, mrDisaster, mrAbort, mrPoliceAlerted, mrPoliceArrived);
    TParamContainer = TStringList;

    TMetaSteppedMission = class;
    TSteppedMission     = class;
    TMissionStepId      = string;
    TMissionStep        = class;
    CMissionStep        = class of TMissionStep;
    TNextStep           = array[TMissionResult] of TMissionStep;

    TMetaMissionLoader =
      class
        public
          constructor Create;
          destructor  Destroy; override;
        public
          procedure RegisterStepClass( Step : string; StepClass : CMissionStep );
        public
          procedure CheckDirectory( dirName : string );
          function  getStepClass( StepName : string ): CMissionStep;
        private
          fStepClasses : TStringList;
          procedure AddMetaMissionFromFile( filename : string );
      end;

    TMetaStep =
      class
        public
          constructor Create( stepName : string; stepClass : CMissionStep; stepParams : TXMLNode; owner : TMetaSteppedMission );
          destructor  Destroy; override;
        public
          function Instantiate( Owner : TMission; MissionParams : TParamContainer ) : TMissionStep;
          function LinkOutputs( step : TMissionStep; allSteps : TCollection ) : boolean;
        private
          fStepClass         : CMissionStep;
          fId                : TMissionStepId;
          fOwner             : TMetaSteppedMission;

          fName              : string;
          fNextStep          : array[low(TMissionResult)..high(TMissionResult)] of string;
          // Difficulty
          fLocalDiff         : single;
          fStdDiff           : single;
          // weights
          fLocalDiffWeight   : single;
          fAccRiskWeight     : single;
          fOutcomeRiskWeight : single;
          fOutcomePsiWeight  : single;
          fSkillWeight       : single;
          fLuckWeight        : single;
          // outcome bounds
          fDisasterBound     : single;
          fFailureBound      : single;
          fSuccessBound      : single;
          // roles
          fRoleCount         : integer;
          fRoles             : TRoles;
          // actions
          fActions           : TCollection;
        public
          property Id : TMissionStepId read fId;
      end;

    TMetaSteppedMission =
      class(TMetaMission)
        public
          constructor Create(anId : string);
          destructor  Destroy; override;
        public
          procedure Load( filename : string );
          function  Instantiate( MissionInfo : string; Owner : TTeam; out Mission : TMission ) : TErrorCode; override;
        public
          function getMetaStep( id : string ) : TMetaStep;
        protected
          fFirstStep   : TMissionStepId;
          fSteps       : TCollection;
          fId          : string;
          fMaxAccRisk  : single;
          function  FindStepId( id : TMissionStepId ) : integer;
          function  getStepByName( MissionName : string; res : TMissionResult ) : integer;
          procedure readSteps( xmlFile : TXMLFile );
        public
          property Id : string read fId;
      end;

    TMissionStep =
      class(TPersistent)
        public
          constructor Create( Owner : TMission; aName : string; MissionParams : TParamContainer ); virtual;
          destructor  Destroy; override;
        public
          class function getDefLocalDiff : single; virtual;
          class function getDefStdDiff : single; virtual;
          class function getDefLocalDiffWeight : single; virtual;
          class function getDefAccRiskWeight : single; virtual;
          class function getDefOutcomeRiskWeight : single; virtual;
          class function getDefOutcomePsiWeight : single; virtual;
          class function getDefDisasterBound : single; virtual;
          class function getDefFailureBound : single; virtual;
          class function getDefSuccessBound : single; virtual;
        public
          procedure ExecuteActions( StepParameters : TStepParameters; var outcome : single );
          procedure Execute( time : integer; var IsOver : boolean; var StepParameters : TStepParameters; var MissionParameters : TMissionParameters ); virtual;
          procedure ProcessOutcome( res : TMissionResult; StepParameters : TStepParameters; var MissionParameters : TMissionParameters ); virtual;
          function  CalculateOutcomeValue( outcome : single ) : TMissionResult; virtual;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        protected
          fName              : string;
          fMetaStepId        : string;
          fNextStep          : TNextStep;
          fOwner             : TMission;
          // Difficulty
          fLocalDiff         : single;
          fStdDiff           : single;
          // weights
          fLocalDiffWeight   : single;
          fAccRiskWeight     : single;
          fOutcomeRiskWeight : single;
          fOutcomePsiWeight  : single;
          fSkillWeight       : single;
          fLuckWeight        : single;
          // outcome bounds
          fDisasterBound     : single;
          fFailureBound      : single;
          fSuccessBound      : single;
          // roles
          fRoleCount         : integer;
          fRoles             : TRoles;
          // report
          fReport            : TParamContainer;
          // Actions
          fActions           : TCollection;
        protected
          procedure AddExperience( var LastExperience : integer; Criminal : TCriminal; Skill : integer; degree : integer; Diff : single );
          procedure AddCharges( var LastCharge : integer; chargeName : string );
          procedure AddLeaderCharges( var LastCharge : integer; chargeName : string );
          procedure AddOffender( var LastCharge : integer; offenderName : string; Idx : integer );
        public
          property NextStep          : TNextStep read fNextStep;
          property Name              : string    read fName;
          property LocalDiff         : single    read fLocalDiff;
          property StdDiff           : single    read fStdDiff;
          property LocalDiffWeight   : single    read fLocalDiffWeight;
          property AccRiskWeight     : single    read fAccRiskWeight;
          property OutcomeRiskWeight : single    read fOutcomeRiskWeight;
          property OutcomePsiWeight  : single    read fOutcomePsiWeight;
          property DisasterBound     : single    read fDisasterBound;
          property FailureBound      : single    read fFailureBound;
          property SuccessBound      : single    read fSuccessBound;
      end;

    TSteppedMission =
      class( TMission )
        public
          constructor Create( aOwner : TTeam; aMetaMission : TMetaMission ); override;
          destructor  Destroy; override;
        public
          procedure StartMission; override;
          procedure ProceedWithMission; override;
          procedure EndMission; override;
        public
          procedure AddStep( aStep : TMissionStep );
          procedure setFirstMission( idx : integer );
          function  CalculateOutcome : single;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        protected
          fMissionTime       : integer;
          fSteps             : TCollection;
          fFirstStep         : TMissionStep;
          fCurrentStep       : TMissionStep;
          fMaxAccRisk        : single;
          fMissionParameters : TMissionParameters;
          fStepParameters    : TStepParameters;
          fLastOutcome       : single;
          fReport            : TParamContainer;
          fPoliceArrived     : boolean;
          function CheckMissionStatus( outcome : single; LastStep : TMissionStep) : TMissionStep;
        protected
          procedure FillReport; virtual;
      end;

  var
    TheMetaMissionLoader : TMetaMissionLoader;

  procedure InitMissionLoader;
  procedure DestroyMissionLoader;
  procedure RegisterStepClass( name : string; stepClass : CMissionStep );


implementation

  uses
    sysutils, windows, stdEffects, stringUtils;

  const
    ROLE_NAMES  : array[0..MAX_ROLES - 1]  of string  = ('Leader',     'Driver', 'Gorilla', 'Artificer', 'Stalker', 'Hacker', 'Doctor', 'Sniper', 'Falsifier' );
    ROLE_IDS    : array[0..MAX_ROLES - 1]  of integer = (ROLE_LEADER, ROLE_DRIVER, ROLE_GORILLA, ROLE_ARTIFICER, ROLE_STALKER, ROLE_HACKER, ROLE_DOCTOR, ROLE_SNIPER, ROLE_FALSIFIER);

    SKILL_NAMES : array[0..MAX_SKILLS - 1] of string  = ('Leadership', 'Driving', 'Brawling', 'FireArms', 'Stalking', 'Computer', 'Demolition', 'Stealth', 'Medicine', 'Forgery' );
    SKILL_IDS : array[0..MAX_SKILLS - 1]   of integer = (SKILL_LEADERSHIP, SKILL_DRIVING, SKILL_BRAWLING, SKILL_FIREARMS, SKILL_STALKING, SKILL_COMPUTER, SKILL_DEMOLITION, SKILL_STEALTH, SKILL_MEDICINE, SKILL_FORGERY );


  const
    MissionIds : array[TMissionResult] of string = ( 'OnOutstanding', 'OnSucces', 'OnFailure', 'OnDisaster', 'OnAbort', 'OnPoliceAlerted', 'OnPoliceArrive' );

  procedure WriteStringList( name : string; Writer : IBackupWriter; strList : TStringList );
    begin
      Writer.WriteString( name, strList.Text );
    end;

  function  ReadStringList( name : string; reader : IBackupReader) : TStringList;
    begin
      result      := TStringList.Create;
      result.Text := Reader.ReadString( name, '' );
    end;

  // utils

  procedure ReadParam( Params : TXMLNode; paramName : string; var param : single; default : single );
    var
      node  : TXMLNode;
    begin
      node  := Params.getChildByName( paramName );
      try
        if node <> nil
          then
            begin
              try
                param := node.ValueAsFloat;
              finally
                node.Free;
              end;
            end
          else param := default;
      except
        param := default;
      end;
    end;

  // TMetaMissionLoader

  constructor TMetaMissionLoader.Create;
    begin
      inherited;
      fStepClasses := TStringList.Create;
    end;

  destructor TMetaMissionLoader.Destroy;
    begin
      fStepClasses.Free;
      inherited;
    end;

  procedure TMetaMissionLoader.RegisterStepClass( Step : string; StepClass : CMissionStep );
    begin
      if fStepClasses.IndexOf( Step ) <> -1
        then fStepClasses.AddObject( Step, TObject(StepClass) );
    end;

  procedure TMetaMissionLoader.CheckDirectory( dirName : string );
    var
      FindFileData : TWIN32FindData;
      searchHandle : THandle;
      filename     : string;
    begin
      searchHandle := FindFirstFile( pchar(dirName + '\*.mission'), FindFileData );
      if searchHandle <> INVALID_HANDLE_VALUE
        then
          try
            repeat
              if (FindFileData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0
                then
                  begin
                    filename := dirName + '\' + FindFileData.cFileName;
                    AddMetaMissionFromFile( filename );
                  end;
            until not FindNextFile( searchHandle, FindFileData );
          finally
            windows.FindClose( searchHandle );
          end;
    end;

  function TMetaMissionLoader.getStepClass( StepName : string ): CMissionStep;
    var
      Idx : integer;
    begin
      Idx := fStepClasses.IndexOf( StepName );
      if Idx <> -1
        then result := CMissionStep(fStepClasses.Objects[Idx])
        else result := TMissionStep;
    end;

  procedure TMetaMissionLoader.AddMetaMissionFromFile( filename : string );
    var
      MetaMission : TMetaSteppedMission;
    begin
      try
        MetaMission := TMetaSteppedMission.Create(''); // >>
        MetaMission.Load( filename );
        MetaMission.Register(tidMetaMissionFamily);
        //theMetaClassStorage.RegisterClass( tidMetaMissionFamily, MetaMission.Id, MetaMission );
      except
      end;
    end;

  // TMetaStep

  constructor TMetaStep.Create( stepName : string; stepClass : CMissionStep; stepParams : TXMLNode; owner : TMetaSteppedMission );
    const
      MaxEffects = 11;
    const
      EffectNames : array[0..MaxEffects - 1] of string  = (
                                                        'Littering',
                                                        'Graffiti',
                                                        'WantonViolence',
                                                        'Noise',
                                                        'Voyeurism',
                                                        'Sabotage',
                                                        'AreaFear',
                                                        'MaintIncrease',
                                                        'UnDesirability',
                                                        'EfficHit',
                                                        'ProdQualityHit'
                                                      );

      EffectIds   : array[0..MaxEffects - 1] of integer = (
                                                        fxLittering,
                                                        fxGraffiti,
                                                        fxWantonViolence,
                                                        fxNoise,
                                                        fxVoyeurism,
                                                        fxSabotage,
                                                        fxAreaFear,
                                                        fxMaintIncrease,
                                                        fxUnDesirability,
                                                        fxEfficHit,
                                                        fxProdQualityHit
                                                      );

    var
      i          : integer;
      r          : TMissionResult;
      acdata     : TModelServerActionData;
      MetaAction : TMetaAction;
      node       : TXMLNode;
      useless    : integer;
    begin
      inherited Create;
      fName            := stepName;
      fActions         := TCollection.Create( 3, rkBelonguer );

      ReadParam( stepParams, 'StdDiff', fStdDiff, stepClass.getDefStdDiff );

      ReadParam( stepParams, 'LocalDiffWeight', fLocalDiffWeight, stepClass.getDefLocalDiffWeight );
      ReadParam( stepParams, 'AccRiskWeight', fAccRiskWeight, stepClass.getDefAccRiskWeight );
      ReadParam( stepParams, 'OutcomeRiskWeight', fOutcomeRiskWeight, stepClass.getDefOutcomeRiskWeight );
      ReadParam( stepParams, 'OutcomePsiWeight', fOutcomePsiWeight, stepClass.getDefOutcomePsiWeight );
      ReadParam( stepParams, 'LuckWeight', fLuckWeight, 0.2 );
      ReadParam( stepParams, 'SkillWeight', fSkillWeight, 0.5 );

      ReadParam( stepParams, 'DisasterBound', fDisasterBound, stepClass.getDefDisasterBound );
      ReadParam( stepParams, 'FailureBound', fFailureBound, stepClass.getDefFailureBound );
      ReadParam( stepParams, 'SuccessBound', fSuccessBound, stepClass.getDefSuccessBound );

      for r := low(TMissionResult) to high(TMissionResult) do
        begin
          fNextStep[r] := stepParams.getChildByName( MissionIds[r] ).ValueAsString;
        end;

      // read roles
      i    := 1;
      node := stepParams.getChildByName( 'Role' + IntToStr(i) );
      while node <> nil do
        begin
          fRoles[fRoleCount].role  := stringToId( node.ValueAsString, ROLE_NAMES, ROLE_IDS );
          fRoles[fRoleCount].skill := stringToId( stepParams.getChildByName( 'Skill' + IntToStr(i) ).ValueAsString, SKILL_NAMES, SKILL_IDS );

          inc( fRoleCount );
          inc( i );

          node.Free;
          node := stepParams.getChildByName( 'Role' + IntToStr(i) );
        end;

      // read actions

      // Internal actions
      i    := 1;
      node := stepParams.getChildByName( 'Action' + IntToStr(i) );
      while node <> nil do
        begin
          try
            MetaAction := TMetaAction(theMetaClassStorage.ClassById[tidMetaActionFamily, node.ValueAsString]);
            if MetaAction <> nil
              then fActions.Insert( MetaAction.Instantiate( useless ) );
          except
          end;

          node.Free;
          inc( i );
          node := stepParams.getChildByName( 'Action' + IntToStr(i) );
        end;

      // Model Server Actions

      i    := 1;
      node := stepParams.getChildByName( 'MSAction' + IntToStr(i) );
      while node <> nil do
        begin
          acdata.Id        := stringToId( node.ValueAsString, EffectNames, EffectIds );
          acdata.Intensity := stepParams.getChildByName( 'MSActionIntensity' + IntToStr(i) ).ValueAsFloat;
          acdata.Radius    := stepParams.getChildByName( 'MSActionRadius' + IntToStr(i) ).ValueAsInteger;

          try
            MetaAction := TMetaAction(theMetaClassStorage.ClassById[tidMetaActionFamily, cidModelServerAction]);
            if MetaAction <> nil
              then fActions.Insert( MetaAction.Instantiate( acdata ) );
          except
          end;

          node.Free;
          inc( i );
          node := stepParams.getChildByName( 'MSAction' + IntToStr(i) );
        end;

      fStepClass     := stepClass;
      fId            := stepName;
      fOwner         := owner;
    end;

  destructor TMetaStep.Destroy;
    begin
      inherited;
      fActions.Free;
    end;

  function TMetaStep.Instantiate( Owner : TMission; MissionParams : TParamContainer ) : TMissionStep;
    begin
      result := fStepClass.Create( Owner, fId, MissionParams );

      result.fName              := fName;
      result.fMetaStepId        := fId;
      result.fLocalDiff         := fLocalDiff;
      result.fStdDiff           := fStdDiff;
      result.fLocalDiffWeight   := fLocalDiffWeight;
      result.fSkillWeight       := fSkillWeight;
      result.fLuckWeight        := fLuckWeight;
      result.fAccRiskWeight     := fAccRiskWeight;
      result.fOutcomeRiskWeight := fOutcomeRiskWeight;
      result.fOutcomePsiWeight  := fOutcomePsiWeight;
      result.fDisasterBound     := fDisasterBound;
      result.fFailureBound      := fFailureBound;
      result.fSuccessBound      := fSuccessBound;
      result.fRoleCount         := fRoleCount;
      result.fRoles             := fRoles;
      result.fActions           := fActions;
    end;

  function TMetaStep.LinkOutputs( step : TMissionStep; allSteps : TCollection ) : boolean;
    var
      stepName : string;
      i        : TMissionResult;
      nextStep : integer;
    begin
      for i := low(TMissionResult) to high(TMissionResult) do
        begin
          stepName := fNextStep[i];
          nextStep := fOwner.getStepByName( stepName, i );
          if nextStep <> -1
            then step.fNextStep[i] := TMissionStep(allSteps.Items[nextStep]);
        end;
      result := true;
    end;

  // TMetaSteppedMission

  constructor TMetaSteppedMission.Create(anId : string);
    begin
      inherited Create(anId);
      fSteps := TCollection.Create( 10, rkBelonguer );
    end;

  destructor TMetaSteppedMission.Destroy;
    begin
      inherited;
      fSteps.Free;
    end;

  procedure TMetaSteppedMission.Load( filename : string );
    var
      xmlFile : TXMLFile;
      general : TXMLNode;
    begin
      xmlFile := TXMLFile.Create;
      try
        if xmlFile.Load( filename )
          then
            begin
              general := xmlFile.getNodeByName( '/root/General' );
              if general <> nil
                then
                  begin
                    // Read basic Params
                    fFirstStep  := general.getChildByName( 'FirstStep' ).ValueAsString;

                    fMaxAccRisk  := general.getChildByName( 'MaxAccRisk' ).ValueAsFloat;
                    fId          := general.getChildByName( 'Id' ).ValueAsString;

                    // Read all steps
                    readSteps( xmlFile );

                    general.Free;
                  end;

            end;
      finally
        xmlFile.Free;
      end;
    end;

  function TMetaSteppedMission.Instantiate( MissionInfo : string; Owner : TTeam; out Mission : TMission ) : TErrorCode;
    var
      i              : integer;
      SteppedMission : TSteppedMission;
      Props          : TStringList;
    begin
      try
        Mission        := TSteppedMission.Create( Owner, self );
        SteppedMission := TSteppedMission(Mission);
        Props          := TStringList.Create;
        try
          Props.Text := MissionInfo;

          SteppedMission.fMaxAccRisk  := fMaxAccRisk;

          // Add all steps
          for i := 0 to pred(fSteps.Count) do
            SteppedMission.AddStep( TMetaStep(fSteps.Items[i]).Instantiate( Mission, Props ));

          // Link the outputs
          for i := 0 to pred(fSteps.Count) do
            TMetaStep(fSteps.Items[i]).LinkOutputs( TMissionStep(SteppedMission.fSteps[i]), SteppedMission.fSteps );

          SteppedMission.setFirstMission( FindStepId( fFirstStep ) );
          result := ERR_SUCCEDEED;
        finally
          Props.Free;
        end;
      except
        result := ERR_UNKNOWN;
      end;
    end;

  function TMetaSteppedMission.getMetaStep( id : TMissionStepId ) : TMetaStep;
    var
      Idx : integer;
    begin
      Idx := FindStepId( id );
      if Idx <> -1
        then result := TMetaStep(fSteps[Idx])
        else result := nil;
    end;

  function TMetaSteppedMission.FindStepId( id : TMissionStepId ) : integer;
    var
      i : integer;
    begin
      i := 0;
      while (i < fSteps.Count) and (CompareText( id, TMetaStep(fSteps.Items[i]).Id ) <> 0) do
        inc( i );

      if (i < fSteps.Count)
        then result := i
        else result := -1;
    end;

  function TMetaSteppedMission.getStepByName( MissionName : string; res : TMissionResult ) : integer;
    var
      Idx : integer;
    begin
      Idx    := FindStepId( MissionName );
      result := Idx;
    end;

  procedure TMetaSteppedMission.readSteps( xmlFile : TXMLFile );
    var
      i          : integer;
      step       : string;
      stepType   : string;
      stepClass  : CMissionStep;
      steps      : TList;
      node       : TXMLNode;
    begin
      steps := TList.Create;
      try
        xmlFile.queryNodes( '', 'Property', steps );
        for i := 0 to pred(steps.Count ) do
          begin
            node := TXMLNode(steps[i]);
            try
              step      := node.ReadString( 'Name', '' );
              stepType  := node.ReadString( 'class', '' );
              stepClass := TheMetaMissionLoader.getStepClass( stepType );
              if stepClass <> nil
                then
                  begin
                    fSteps.Insert( TMetaStep.Create( step, stepClass, node, self ));
                  end;
            finally
              node.Free;
            end;
          end;
      finally
        steps.Free;
      end;
    end;

  // TMissionStep

  constructor TMissionStep.Create( Owner : TMission; aName : string; MissionParams : TParamContainer );
    begin
      inherited Create;
      fOwner         := Owner;
      fName          := aName;
      fRoleCount     := 0;
    end;

  destructor TMissionStep.Destroy;
    begin
      inherited;
    end;

  class function TMissionStep.getDefLocalDiff : single;
    begin
      result := 0;
    end;

  class function TMissionStep.getDefStdDiff : single;
    begin
      result := 0.1;
    end;

  class function TMissionStep.getDefLocalDiffWeight : single;
    begin
      result := 0.7;
    end;

  class function TMissionStep.getDefAccRiskWeight : single;
    begin
      result := 0.1;
    end;

  class function TMissionStep.getDefOutcomeRiskWeight : single;
    begin
      result := 0.5;
    end;

  class function TMissionStep.getDefOutcomePsiWeight : single;
    begin
      result := 0.3;
    end;

  class function TMissionStep.getDefDisasterBound : single;
    begin
      result := 0.25;
    end;

  class function TMissionStep.getDefFailureBound : single;
    begin
      result := 0.50;
    end;

  class function TMissionStep.getDefSuccessBound : single;
    begin
      result := 0.75;
    end;

  procedure TMissionStep.ExecuteActions( StepParameters : TStepParameters; var outcome : single );
    var
      ActionData : TInternalActionData;
      i, j       : integer;
      useless    : integer;
    begin
      for i := 0 to pred(fActions.Count) do
        begin
          if TAction(fActions[i]).MetaAction.Internal
            then
              begin
                ActionData.CriminalCount := StepParameters.CriminalCount;
                for j := 0 to pred(ActionData.CriminalCount) do
                  ActionData.Criminals[i] := StepParameters.Criminals[i];
                ActionData.Team := fOwner.Owner;
                TAction(fActions[i]).Execute( ActionData, outcome );
              end
            else TAction(fActions[i]).Execute( useless, outcome );
        end;
    end;

  procedure TMissionStep.Execute( time : integer; var IsOver : boolean; var StepParameters : TStepParameters; var MissionParameters : TMissionParameters );
    var
      StabCheck : single;
      i         : integer;
      tempskill : single;
      tempStab  : single;
    begin
      with StepParameters do
        begin
          CriminalCount := fRoleCount;
          tempskill     := 0;
          tempStab      := 0;
          SkillWeight   := fSkillWeight;
          LuckWeight    := fLuckWeight;
          for i := 0 to pred(CriminalCount) do
            begin
              Criminals[i] := fOwner.Owner.getCriminalForTask( fRoles[i].role, fRoles[i].skill, fRoleCount, fRoles );
              if Criminals[i] <> nil
                then
                  begin
                    StabCheck := Criminals[i].CheckStability( fRoles[i].skill, MissionParameters.PsiFactor );
                    tempskill := tempskill + Criminals[i].Skills.Skills[fRoles[i].skill];
                    tempStab  := tempStab + StabCheck;
                  end
                else Abort;
            end;
          if fRoleCount <> 0
            then
              begin
                Skill := ((tempskill + tempStab)/fRoleCount);
                Stab  := tempStab/fRoleCount; // we need it to calculate the Psi Importance
              end
            else
              begin
                Skill := 0;
                Stab  := 0
              end;

          Diff          := fLocalDiff*fLocalDiffWeight + fStdDiff*(1 - fLocalDiffWeight);
          AccRiskWeight := fAccRiskWeight;

          IsOver        := true; //>> check if the step takes more than the minimum time
        end;
    end;

  procedure TMissionStep.ProcessOutcome( res : TMissionResult; StepParameters : TStepParameters; var MissionParameters : TMissionParameters );
    var
      i : integer;
    begin
      case res of
        mrOutstanding:
          begin
            for i := 0 to pred(fRoleCount) do
              if StepParameters.Criminals[i] <> nil
                then AddExperience( MissionParameters.LastExperience, StepParameters.Criminals[i], fRoles[i].skill, 1, StepParameters.Diff );
          end;
        mrSuccess:
          begin
            for i := 0 to pred(fRoleCount) do
              if StepParameters.Criminals[i].Name <> ''
                then AddExperience( MissionParameters.LastExperience, StepParameters.Criminals[i], fRoles[i].skill, 1, StepParameters.Diff );
          end;
      end;
    end;

  function TMissionStep.CalculateOutcomeValue( outcome : single ) : TMissionResult;
    begin
      if outcome < fDisasterBound
        then result := mrDisaster
        else
          if outcome < fFailureBound
            then result := mrFailure
            else
              if outcome < fSuccessBound
                then result := mrSuccess
                else result := mrOutstanding;
    end;

  procedure TMissionStep.LoadFromBackup( Reader : IBackupReader );
    var
      mr       : TMissionResult;
      i        : integer;
      metaStep : TMetaStep;
    begin
      inherited;
      fName              := Reader.ReadString( 'Name', '' );
      for mr := low(mr) to high(mr) do
        Reader.ReadObject('mr' + IntToStr(ord(mr)), fNextStep[mr], nil);

      Reader.ReadObject( 'Owner', fOwner, nil );

      fLocalDiff         := Reader.ReadSingle( 'LocalDiff', 0 );
      fStdDiff           := Reader.ReadSingle( 'StdDiff', 0 );

      fLocalDiffWeight   := Reader.ReadSingle( 'LocalDiffWeight', 0 );
      fAccRiskWeight     := Reader.ReadSingle( 'AccRiskWeight', 0 );
      fOutcomeRiskWeight := Reader.ReadSingle( 'OutcomeRiskWeight', 0 );
      fOutcomePsiWeight  := Reader.ReadSingle( 'OutcomePsiWeight', 0 );
      fSkillWeight       := Reader.ReadSingle( 'SkillWeight', 0 );
      fLuckWeight        := Reader.ReadSingle( 'LuckWeight', 0 );

      fDisasterBound     := Reader.ReadSingle( 'DisasterBound', 0 );
      fFailureBound      := Reader.ReadSingle( 'FailureBound', 0 );
      fSuccessBound      := Reader.ReadSingle( 'SuccessBound', 0 );

      fRoleCount := Reader.ReadInteger( 'RoleCount', 0 );
      for i := 0 to pred(fRoleCount) do
        begin
          fRoles[i].role  := Reader.ReadInteger( 'Role'  + IntToStr(i), 0 );
          fRoles[i].skill := Reader.ReadInteger( 'Skill' + IntToStr(i), 0 );
        end;

      Reader.ReadObject( 'Report', fReport, nil );

      fMetaStepId := Reader.ReadString( 'StepId', '' );

      if fOwner <> nil
        then
          begin
            metaStep := TMetaSteppedMission(fOwner.MetaMission).getMetaStep( fMetaStepId );
            if metaStep <> nil
              then fActions := metaStep.fActions;
          end;
    end;

  procedure TMissionStep.StoreToBackup ( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteString('Name', fName);

      for mr := low(mr) to high(mr) do
        Writer.WriteObject('mr' + IntToStr(ord(mr)), fNextStep[mr]);

      Writer.WriteObject('Owner', fOwner);

      Writer.WriteSingle('LocalDiff', fLocalDiff);
      Writer.WriteSingle('StdDiff', fStdDiff);

      Writer.WriteSingle('LocalDiffWeight', fLocalDiffWeight);
      Writer.WriteSingle('AccRiskWeight', fAccRiskWeight);
      Writer.WriteSingle('OutcomeRiskWeight', fOutcomeRiskWeight);
      Writer.WriteSingle('OutcomePsiWeight', fOutcomePsiWeight);
      Writer.WriteSingle('SkillWeight', fSkillWeight);
      Writer.WriteSingle('LuckWeight', fLuckWeight);

      Writer.WriteSingle('DisasterBound', fDisasterBound);
      Writer.WriteSingle('FailureBound', fFailureBound);
      Writer.WriteSingle('SuccessBound', fSuccessBound);

      Writer.WriteInteger('RoleCount', fRoleCount);
      for i := 0 to pred(fRoleCount) do
        begin
          Writer.WriteInteger('Role'  + IntToStr(i), fRoles[i].role);
          Writer.WriteInteger('Skill' + IntToStr(i), fRoles[i].skill);
        end;

      Writer.WriteObject('Report', fReport);

      Writer.WriteString('StepId', fMetaStepId);
      {

      Reader.ReadObject( 'Report', fReport, nil );

      fMetaStepId := Reader.ReadString( 'MetaStepId', '' );

      if fOwner <> nil
        then
          begin
            metaStep := TMetaSteppedMission(fOwner.MetaMission).getMetaStep( fMetaStepId );
            if metaStep <> nil
              then fActions := metaStep.fActions;
          end;
      }
    end;

  procedure TMissionStep.AddExperience( var LastExperience : integer; Criminal : TCriminal; Skill : integer; degree : integer; Diff : single );
    begin
      //>> take care of all this experience, etc
      {
      inc( LastExperience );
      fOwner.Experience.Values['Success' + IntToStr(LastExperience)] := CriminalName;
      fOwner.Experience.Values['Skill' + IntToStr(LastExperience)] := Skill;
      fOwner.Experience.Values['Degree' + IntToStr(LastExperience)] := IntToStr(degree);
      fOwner.Experience.Values['Diff' + IntToStr(LastExperience)] := FloatToStr(Diff);
      }
    end;

  procedure TMissionStep.AddCharges( var LastCharge : integer; chargeName : string );
    begin
      {
      inc( LastCharge );
      fOwner.Charges.Values['Charge' + IntToStr(LastCharge)] := chargeName;
      }
    end;

  procedure TMissionStep.AddLeaderCharges( var LastCharge : integer; chargeName : string );
    begin
      //fOwner.Charges.Values['LeaderCharge' + IntToStr(LastCharge)] := chargeName;
    end;

  procedure TMissionStep.AddOffender( var LastCharge : integer; offenderName : string; Idx : integer );
    begin
      //fOwner.Charges.Values['Offender' + IntToStr(LastCharge) + IntToStr(idx)] := offenderName;
    end;

  // TSteppedMission

  constructor TSteppedMission.Create( aOwner : TTeam; aMetaMission : TMetaMission );
    begin
      inherited;
      fSteps        := TCollection.Create( 10, rkBelonguer );
      fReport       := TParamContainer.Create;
    end;

  destructor TSteppedMission.Destroy;
    begin
      fSteps.Free;
      fReport.Free;
      inherited;
    end;

  procedure TSteppedMission.StartMission;
    begin
      inherited;
      fCurrentStep   := fFirstStep;
      fPoliceArrived := false;
    end;

  procedure TSteppedMission.ProceedWithMission;
    var
      Done     : boolean;
      nextStep : TMissionStep;
      res      : TMissionResult;
    begin
      if (fCurrentStep <> nil)
        then
          begin
            fReport.Clear;

            fCurrentStep.fReport := fReport;
            fCurrentStep.Execute( fMissionTime, Done, fStepParameters, fMissionParameters );
            if Done
              then
                begin
                  fLastOutcome := CalculateOutcome;
                  res          := fCurrentStep.CalculateOutcomeValue( fLastOutcome );
                  fCurrentStep.ProcessOutcome( res, fStepParameters, fMissionParameters );

                  nextStep := CheckMissionStatus( fLastOutcome, fCurrentStep );
                  FillReport;
                  if nextStep = nil
                    then nextStep := fCurrentStep.NextStep[res];

                  fCurrentStep := nextStep
                end;
          end
        else
          begin
            EndMission;
          end;

      inc( fMissionTime );
    end;

  procedure TSteppedMission.EndMission;
    var
      i       : integer;
      useless : integer;
    begin
      if fPoliceArrived and not fEscaped
        then
          for i := 0 to pred(fOwner.Criminals.Count) do
            if TCriminal(fOwner.Criminals[i]).Status = CRIMSTATUS_INMISSION
              then TCriminal(fOwner.Criminals[i]).setStatus( CRIMSTATUS_CAPTURED, useless );

      inherited;
    end;

  procedure TSteppedMission.AddStep( aStep : TMissionStep );
    begin
      fSteps.Insert( aStep );
    end;

  procedure TSteppedMission.setFirstMission( idx : integer );
    begin
      if idx <> -1
        then fFirstStep := TMissionStep(fSteps.Items[idx]);
    end;

  function TSteppedMission.CalculateOutcome : single;
    var
      FinalDiff : single;
    begin
      // weight the accumulated risk with the actual difficulty of the mission
      FinalDiff := fMissionParameters.AccRisk*(fStepParameters.AccRiskWeight) + fStepParameters.Diff*(1 - fStepParameters.AccRiskWeight);

      // weight the Diff, skills and luck
      result := fStepParameters.Skill*fStepParameters.SkillWeight + fMissionParameters.Luck*fStepParameters.LuckWeight - FinalDiff*(1 - (fStepParameters.SkillWeight + fStepParameters.LuckWeight));

      // execute actions (this may affect the outcome)
      fCurrentStep.ExecuteActions( fStepParameters, result );
    end;

  procedure TSteppedMission.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
    end;

  procedure TSteppedMission.StoreToBackup ( Writer : IBackupWriter );
    begin
      inherited;
    end;

  function TSteppedMission.CheckMissionStatus( outcome : single; LastStep : TMissionStep) : TMissionStep;
    begin
      // analize and update the AccRisk and PsiFactor
      if not fAbort
        then
          begin
            with fMissionParameters do
              begin
                // very good missions (above success) decreases the accum risk as well as the psi factor
                AccRisk   := AccRisk + (LastStep.SuccessBound - outcome)*LastStep.OutcomeRiskWeight;
                PsiFactor := PsiFactor + (LastStep.SuccessBound - outcome)*LastStep.OutcomePsiWeight;

                if AccRisk > fMaxAccRisk
                  then
                    begin
                      //>> find a better way to do this
                      if (fMaxAccRisk - AccRisk) > (0.8 - random)
                        then result := LastStep.NextStep[mrPoliceArrived]
                        else result := LastStep.NextStep[mrPoliceAlerted];
                    end
                  else result := nil;
              end;
          end
        else result := LastStep.NextStep[mrAbort];
    end;

  procedure TSteppedMission.FillReport;
    const
      GrowingFactor = 4;

    var
      i          : integer;
      DiffWeight : single;
      AccRiskImp : single;
      aux        : single;
      DiffImp    : single;
      LuckImp    : single;
      SkillImp   : single;
      PsiImp     : single;
    begin
      fReport.Values['StepName'] := fCurrentStep.Name;
      fReport.Values['Psi']      := IntToStr(round( fMissionParameters.PsiFactor*100 ));
      fReport.Values['AccRisk']  := IntToStr(round( fMissionParameters.AccRisk*100 ));
      fReport.Values['Luck']     := IntToStr(round( fMissionParameters.Luck*100 ));
      fReport.Values['Skill']    := IntToStr(round( fStepParameters.Skill*100 ));
      fReport.Values['Diff']     := IntToStr(round( fStepParameters.Diff*100 ));
      fReport.Values['Outcome']  := IntToStr(round( fLastOutcome*100 ));

      with fMissionParameters, fStepParameters do
        begin
          DiffWeight := (1 - (SkillWeight + LuckWeight));
          AccRiskImp := AccRisk + AccRiskWeight       + GrowingFactor*DiffWeight;
          DiffImp    := Diff    + (1 - AccRiskWeight) + GrowingFactor*DiffWeight;
          LuckImp    := Luck    + 1                   + GrowingFactor*LuckWeight;
          SkillImp   := Skill   + 1                   + GrowingFactor*SkillWeight;
          PsiImp     := 0; //>> to fix!!

          aux        := AccRiskImp + DiffImp + LuckImp + SkillImp + PsiImp;

          AccRiskImp := (AccRiskImp*100)/aux;
          DiffImp    := (DiffImp*100)/aux;
          LuckImp    := (LuckImp*100)/aux;
          SkillImp   := (SkillImp*100)/aux;
          PsiImp     := (PsiImp*100)/aux;
        end;

      fReport.Values['PsiImp']      := IntToStr(round( PsiImp ));
      fReport.Values['AccRiskImp']  := IntToStr(round( AccRiskImp ));
      fReport.Values['LuckImp']     := IntToStr(round( DiffImp ));
      fReport.Values['SkillImp']    := IntToStr(round( LuckImp ));
      fReport.Values['DiffImp']     := IntToStr(round( SkillImp ));
      fReport.Values['OutcomeImp']  := IntToStr( 100 );


      for i := 0 to pred(fCurrentStep.fRoleCount) do
        fReport.Values['Role' + IntToStr(i)] := IntToStr( fCurrentStep.fRoles[i].role );

      Owner.Owner.ClientView.Report( fReport.Text );
    end;

  procedure InitMissionLoader;
    begin
      TheMetaMissionLoader := TMetaMissionLoader.Create;
    end;

  procedure DestroyMissionLoader;
    begin
      TheMetaMissionLoader.Free;
    end;

  procedure RegisterStepClass( name : string; stepClass : CMissionStep );
    begin
      TheMetaMissionLoader.RegisterStepClass( name, stepClass );
    end;

end.
