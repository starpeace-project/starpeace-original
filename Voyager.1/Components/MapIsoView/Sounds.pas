unit Sounds;

interface

  uses
    Classes, SoundMixer, SoundTypes, TimerTypes, ShutDown;

  type
    PTargetInfo = ^TTargetInfo;
    TTargetInfo =
      record
        hearable : boolean;
        Target   : ISoundTarget;
      end;

  type
    TSoundManager =
      class(TInterfacedObject, IShutDownTarget, ITickeable, ISoundManager)
        private
          fTargets    : TList;
          fSoundMixer : TSoundMixer;
          function  GetTargetInfo(const Target : ISoundTarget) : PTargetInfo;
        public
          constructor Create;
          destructor  Destroy; override;
        private // IShutDownTarget
          procedure OnSuspend;
          procedure OnResume;
          function  GetPriority : integer;
          procedure OnShutDown;
        private // ITickeable
          fLastUpdate   : integer;
          fEnabled      : boolean;
          fGlobalVolume : single;
          function Enabled : boolean;
          function Tick : integer;
        private // ISoundManager
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

implementation

  uses
    Windows, TimerTicker;

  const
    cSoundsTimerInterval = 120;

  constructor TSoundManager.Create;
    begin
      inherited;
      fTargets := TList.Create;
      fSoundMixer := TSoundMixer.Create;
      AttachTickeable(Self);
      ShutDown.AttachTarget(Self);
      fEnabled := true;
      fGlobalVolume := 1;
    end;

  destructor TSoundManager.Destroy;
    begin
      ShutDown.DetachTarget(Self);
      DetachTickeable(Self);
      Clear;
      fSoundMixer.Free;
      fTargets.Free;
      inherited;
    end;

  procedure TSoundManager.OnSuspend;
    begin
      {
      fEnabled := false;
      fSoundMixer.PauseSounds;
      }
    end;

  procedure TSoundManager.OnResume;
    begin
      {
      fEnabled := fTargets.Count > 0;
      fSoundMixer.ResumeSounds;
      }
    end;

  function TSoundManager.GetPriority : integer;
    begin
      Result := 100;
    end;

  procedure TSoundManager.OnShutDown;
    begin
      fEnabled := false;
      //fSoundMixer.StopAllSounds
    end;

  function TSoundManager.Enabled : boolean;
    begin
      Result := fEnabled;
    end;

  function TSoundManager.Tick;

    procedure PlaySoundsTick;
      var
        i       : integer;
        Target  : ISoundTarget;
        sndinfo : TSoundInfo;
        //sndname : string;
      begin
        for i := 0 to pred(fTargets.Count) do
          begin
            Target := PTargetInfo(fTargets[i]).Target;
            {
            sndname := Target.GetSoundName;
            if pos('dogs.wav', sndname) <> 0
              then
            }
            if Target.ShouldPlayNow
              then
                begin
                  sndinfo.name := Target.GetSoundName;
                  sndinfo.kind := Target.GetSoundKind;
                  sndinfo.priority := Target.GetPriority;
                  sndinfo.looped := Target.IsLooped;
                  sndinfo.volume := Target.GetVolume*fGlobalVolume;
                  sndinfo.pan := Target.GetPan;
                  fSoundMixer.AddSounds(sndinfo);
                end;
          end;
        fSoundMixer.PlaySounds;
      end;

    var
      CurrentTicks : integer;
      ElapsedTicks : integer;
      FrameDelay   : integer;
    begin
      CurrentTicks := GetTickCount;
      ElapsedTicks := CurrentTicks - fLastUpdate;
      FrameDelay   := cSoundsTimerInterval;
      if ElapsedTicks >= FrameDelay
        then
          begin
            PlaySoundsTick;
            fLastUpdate := CurrentTicks;
            Result := FrameDelay;
          end
        else Result := FrameDelay - ElapsedTicks;
    end;

  procedure TSoundManager.AddTargets(const Targets : array of ISoundTarget);
    var
      i          : integer;
      TargetInfo : PTargetInfo;
    begin
      for i := low(Targets) to high(Targets) do
        begin
          TargetInfo := GetTargetInfo(Targets[i]);
          if TargetInfo = nil
            then
              begin
                new(TargetInfo);
                TargetInfo.hearable := true;
                TargetInfo.Target := Targets[i];
                fTargets.Add(TargetInfo);
              end
            else
              begin
                TargetInfo.hearable := true;
                with TargetInfo.Target do
                  begin
                    UpdateSoundParameters;
                    fSoundMixer.ChangeSoundParams(GetSoundName, GetSoundKind, GetPan, GetVolume*fGlobalVolume);
                  end;
              end;
        end;
    end;

  procedure TSoundManager.RemoveTarget(const Target : ISoundTarget);
    var
      TargetInfo : PTargetInfo;
    begin
      TargetInfo := GetTargetInfo(Target);
      if TargetInfo <> nil
        then
          begin
            StopTarget(Target);
            fTargets.Remove(TargetInfo);
            dispose(TargetInfo);
          end;
    end;

  procedure TSoundManager.CheckRemoveTargets(Check : TSoundTargetCheck; info : array of const);
    var
      i   : integer;
      aux : PTargetInfo;
    begin
      for i := 0 to pred(fTargets.Count) do
        begin
          aux := PTargetInfo(fTargets[i]);
          if (aux <> nil) and Check(aux.Target, info)
            then
              begin
                fTargets[i] := nil;
                dispose(aux);
              end;
        end;
      fTargets.Pack;
    end;

  procedure TSoundManager.UpdateTarget(const Target : ISoundTarget);
    begin
      with Target do
        fSoundMixer.ChangeSoundParams(GetSoundName, GetSoundKind, GetPan, GetVolume*fGlobalVolume);
    end;

  procedure TSoundManager.StopTarget(const Target : ISoundTarget);
    begin
      with Target do
        fSoundMixer.StopPlayingSound(GetSoundName, GetSoundKind);
    end;

  procedure TSoundManager.PlayTarget(const Target :ISoundTarget);
    var
      sndinfo : TSoundInfo;
    begin
      sndinfo.name := Target.GetSoundName;
      sndinfo.kind := Target.GetSoundKind;
      sndinfo.priority := Target.GetPriority;
      sndinfo.looped := Target.IsLooped;
      sndinfo.volume := Target.GetVolume*fGlobalVolume;
      sndinfo.pan := Target.GetPan;
      fSoundMixer.AddSounds(sndinfo);
    end;

  procedure TSoundManager.StartCaching;
    var
      i          : integer;
      TargetInfo : PTargetInfo;
    begin
      for i := 0 to pred(fTargets.Count) do
        begin
          TargetInfo := PTargetInfo(fTargets[i]);
          if TargetInfo.Target.IsCacheable
            then TargetInfo.hearable := false;
        end;
    end;

  procedure TSoundManager.StopCaching;
    var
      i          : integer;
      TargetInfo : PTargetInfo;
    begin
      for i := 0 to pred(fTargets.Count) do
        begin
          TargetInfo := PTargetInfo(fTargets[i]);
          if not TargetInfo.hearable
            then
              begin
                with TargetInfo.Target do
                  fSoundMixer.StopPlayingSound(GetSoundName, GetSoundKind);
                dispose(TargetInfo);
                fTargets[i] := nil;
              end;
        end;
      fTargets.Pack;
    end;

  procedure TSoundManager.Clear;
    var
      i          : integer;
      TargetInfo : PTargetInfo;
    begin
      for i := 0 to pred(fTargets.Count) do
        begin
          TargetInfo := PTargetInfo(fTargets[i]);
          if TargetInfo <> nil
            then
              begin
                with TargetInfo.Target do
                  fSoundMixer.StopPlayingSound(GetSoundName, GetSoundKind);
                dispose(TargetInfo);
                fTargets[i] := nil;
              end;
        end;
      fTargets.Pack;
    end;

  procedure TSoundManager.PlayCachedSounds;
    begin
      fSoundMixer.PlaySounds;
    end;

  procedure TSoundManager.Reset;
    begin
      Clear;
      fSoundMixer.ClearSoundCache;
    end;

  procedure TSoundManager.SetGlobalVolume(volume : single);
    var
      i         : integer;
      SndTarget : ISoundTarget;
    begin
      fGlobalVolume := volume;
      for i := 0 to pred(fTargets.Count) do
        begin
          SndTarget := PTargetInfo(fTargets[i]).Target;
          with SndTarget do
            fSoundMixer.ChangeSoundParams(GetSoundName, GetSoundKind, GetPan, GetVolume*fGlobalVolume);
        end;
    end;

  function TSoundManager.GetTargetInfo(const Target : ISoundTarget) : PTargetInfo;
    var
      i          : integer;
      TargetInfo : PTargetInfo;
    begin
      i := 0;
      TargetInfo := nil;
      while (i < fTargets.Count) and (TargetInfo = nil) do
        begin
          TargetInfo := PTargetInfo(fTargets[i]);
          if not TargetInfo.Target.IsEqualTo(Target)
            then
              begin
                TargetInfo := nil;
                inc(i);
              end;
        end;
      Result := TargetInfo;
    end;

end.
