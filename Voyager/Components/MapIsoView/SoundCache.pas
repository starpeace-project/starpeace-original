unit SoundCache;

interface

  const
    cMaxSoundCacheSize = 50;

  type
    TSoundSource = (ssDiskFile, ssResource);

  type
    TCachedSoundInfo =
      record
        Name  : string;
        Ticks : integer;
      end;

  type
    TSoundCache =
      class
        private
          fSoundCount : integer;
          fSounds     : array [0..pred(cMaxSoundCacheSize)] of TCachedSoundInfo;
        public
          destructor Destroy; override;
        public
          function  GetSound(const Name : string; Source : TSoundSource) : boolean;
          procedure Clear;
      end;

implementation

  uses
    Windows, SoundLib;

  destructor TSoundCache.Destroy;
    begin
      inherited;
    end;

  function TSoundCache.GetSound(const Name : string; Source : TSoundSource) : boolean;

    function FindVictim : integer;
      var
        i       : integer;
        mintick : integer;
        victim  : integer;
      begin
        victim := 0;
        mintick := fSounds[0].Ticks;
        for i := 1 to pred(fSoundCount) do
          if fSounds[i].Ticks < mintick
            then
              begin
                mintick := fSounds[i].Ticks;
                victim := i;
              end;
        UnloadSound(fSounds[victim].Name);
        Result := victim;
      end;

    var
      i      : integer;
      loaded : boolean;
    begin
      i := 0;
      loaded := false;
      while (i < fSoundCount) and not loaded do
        begin
          loaded := fSounds[i].Name = Name;
          if not loaded
            then inc(i);
        end;
      if not loaded
        then
          begin
            case Source of
              ssDiskFile:
                loaded := LoadSoundFromFile(Name);
              ssResource:
                loaded := LoadSoundFromResName(Name);
            end;
            if loaded
              then
                begin
                  if i < cMaxSoundCacheSize
                    then inc(fSoundCount)
                    else i := FindVictim;
                  fSounds[i].Name := Name;
                end;
          end;
      if loaded
        then fSounds[i].Ticks := GetTickCount;
      Result := loaded;
    end;

  procedure TSoundCache.Clear;
    begin
      fSoundCount := 0;
    end;

end.
