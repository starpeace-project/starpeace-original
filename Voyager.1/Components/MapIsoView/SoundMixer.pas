unit SoundMixer;

interface

  uses
    SoundTypes, SoundCache;

  const
    cMaxAllowedSounds = 30;

  // it does not make sense to create more than one instance of this class

  type
    TSoundMixer =
      class
        private
          fMaxAllowedSounds : integer;
          fSounds           : array [0..pred(cMaxAllowedSounds)] of TSoundInfo;
          fSoundCount       : integer;
          fSoundCache       : TSoundCache;
        public
          constructor Create;
          destructor  Destroy; override;
        public
          procedure AddSounds(const Sounds : array of TSoundInfo);
          procedure PlaySounds;
          procedure PauseSounds;
          procedure ResumeSounds;
          procedure StopPlayingSound(const Name : string; Kind : integer);
          procedure ChangeSoundParams(const Name : string; Kind : integer; Pan, Volume : single);
          procedure ClearSoundCache;
        public
          property MaxAllowedSounds : integer read fMaxAllowedSounds write fMaxAllowedSounds;
      end;

implementation

  uses
    SysUtils, SoundLib;

  var
    vMixerInstances : integer = 0;

  constructor TSoundMixer.Create;
    begin
      inherited;
      if vMixerInstances > 0
        then raise Exception.Create('Too many mixer instances');
      inc(vMixerInstances);
      fSoundCache := TSoundCache.Create;
      fMaxAllowedSounds := 30;
    end;

  destructor TSoundMixer.Destroy;
    begin
      fSoundCache.Free;
      dec(vMixerInstances);
      inherited;
    end;

  {$O-}
  procedure TSoundMixer.AddSounds(const Sounds : array of TSoundInfo);
    var
      i : integer;
      j : integer;
    begin
      for i := low(Sounds) to high(Sounds) do
        begin
          j := 0;
          while (j < fSoundCount) and (Sounds[i].priority > fSounds[j].priority) do
            inc(j);
          if j < fMaxAllowedSounds
            then
              begin
                if fSoundCount = fMaxAllowedSounds
                  then dec(fSoundCount);
                Move(fSounds[j], fSounds[j+1], (fSoundCount - j)*sizeof(fSounds[0]));
                fSounds[j] := Sounds[i];
                inc(fSoundCount);
              end;
        end;
    end;
  {$O+}

  procedure TSoundMixer.PlaySounds;
    var
      i        : integer;
      startofs : integer;
    begin
      for i := 0 to pred(fSoundCount) do
        with fSounds[i] do
          if fSoundCache.GetSound(name, ssDiskFile)
            then
              begin
                if looped
                  then startofs := random(SoundLength(name, kind))
                  else startofs := 0;
                PlaySound(name, kind, looped, pan, volume, startofs);
              end;
      fSoundCount := 0;
    end;

  procedure TSoundMixer.PauseSounds;
    begin
      PauseAllSounds;
    end;

  procedure TSoundMixer.ResumeSounds;
    begin
      ResumeAllSounds;
    end;

  procedure TSoundMixer.StopPlayingSound(const Name : string; Kind : integer);
    begin
      StopSound(Name, Kind);
    end;

  procedure TSoundMixer.ChangeSoundParams(const Name : string; Kind : integer; Pan, Volume : single);
    begin
      SetSoundPan(Name, Kind, Pan);
      SetSoundVolume(Name, Kind, Volume);
    end;

  procedure TSoundMixer.ClearSoundCache;
    begin
      fSoundCache.Clear;
    end;

end.
