unit SoundTypes;

interface

  type
    ISoundTarget =
      interface
        function  GetSoundName : string;
        function  GetSoundKind : integer;
        function  GetPriority : integer;
        function  IsLooped : boolean;
        function  GetVolume : single;
        function  GetPan : single;
        function  ShouldPlayNow : boolean;
        function  IsCacheable : boolean;
        function  IsEqualTo(const SoundTarget : ISoundTarget) : boolean;
        function  GetObject : TObject;
        procedure UpdateSoundParameters;
      end;

  type
    TSoundTargetCheck = function (const Target : ISoundTarget; info : array of const) : boolean;

  type
    ISoundManager =
      interface
        procedure AddTargets(const Targets : array of ISoundTarget);
        procedure RemoveTarget(const Target : ISoundTarget);
        procedure CheckRemoveTargets(Check : TSoundTargetCheck; info : array of const);
        procedure PlayTarget(const Target : ISoundTarget);
        procedure UpdateTarget(const Target : ISoundTarget);
        procedure StopTarget(const Target : ISoundTarget);
        procedure StartCaching;
        procedure StopCaching;
        procedure Clear;
        procedure PlayCachedSounds;
        procedure Reset;
        procedure SetGlobalVolume(volume : single);
      end;

  type
    TSoundInfo =
      record
        name     : string;
        kind     : integer;
        priority : integer;
        looped   : boolean;
        volume   : single; //  0 .. 1
        pan      : single; // -1 .. 1
      end;

  const
    cLeftPan     = -1;
    cCenterPan   = 0;
    cRightPan    = 1;
    cPanDeadZone = 128;
    cMinVol      = 0.6; // these are acceptable values
    cMaxVol      = 1;
    cMaxHearDist = 50;
    cZoomVolStep = 0.25;

implementation

end.


