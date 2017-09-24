unit SoundLib;

interface

  uses
    Windows, DirectSound, SysUtils;

  const
    skClass = -1;
    skUser  = 5;

  type
    ESoundError = class(Exception);

  function  InitSoundLibrary(focus : hWnd) : boolean;
  procedure DoneSoundLibrary;
  function  GetDSoundInstance : IDirectSound;
//  function  GetLibStatus : integer;
  procedure InitDSoundEngine(Wnd : HWND);
  procedure DoneDSoundEngine;

  procedure EnableSounds;
  procedure DisableSounds;

  function  LoadSoundFromFile(const FileName : string) : boolean;
  function  LoadSoundFromModule(Handle : HModule; const ResName : string) : boolean;
  function  LoadSoundFromResName(const ResName : string) : boolean;
  procedure UnloadSound(const Name : string);
  procedure UnloadAllSounds;

  function  SoundLength(const Name : string; Kind : integer) : integer;

  procedure PlaySound(const Name : string; Kind : integer; Looped : boolean; pan, volume : single; startofs : integer);
  procedure StopSound(const Name : string; Kind : integer);
  procedure PauseSound(const Name : string; Kind : integer);
  procedure ResumeSound(const Name : string; Kind : integer);
  procedure SetSoundPan(const Name : string; Kind : integer; pan : single);
  procedure SetSoundVolume(const Name : string; Kind : integer; volume : single);
  procedure StopAllSounds;
  procedure PauseAllSounds;
  procedure ResumeAllSounds;

implementation

  uses
    Classes, ActiveX,
    MapStringToObject, WaveLow, LowStuff;

  var
    SoundMap      : TMapStringToObject = nil;
    gInstanceList : TList = nil;
    SoundsEnabled : boolean = false;

  { DirectSound DLL dirty stuff}

//  const
//    SoundLibraryPath = 'SoundLib.dll';

  type
    TReadCallback = procedure(Data : pointer; Size : DWORD; UserData : pointer); stdcall;
    TSoundBuffer = IDirectSoundBuffer;
{
  type
    TInitSoundEngine = function(Wnd : HWND) : BOOL; cdecl;
    TDoneSoundEngine = procedure; cdecl;
    TCreateSound     = function(var Buffer : TSoundBuffer; Size, Freq, Bits, Align : DWORD; Stereo : BOOL) : BOOL; cdecl;
    TFillSound       = function(Buffer : TSoundBuffer; Size : DWORD; ReadData : TReadCallback; UserData : pointer) : BOOL; cdecl;
    TStopsSound      = procedure(Buffer : TSoundBuffer); cdecl;
    TPlaysSound      = procedure(Buffer : TSoundBuffer; LoopIt, RestartIt : BOOL; Pan, Volume, StartOfs : integer); cdecl;
    TDuplicateSound  = procedure(Buffer : TSoundBuffer; var Clone : TSoundBuffer); cdecl;
    TSetsSoundPan    = procedure(Buffer : TSoundBuffer; Pan : integer); cdecl;
    TSetsSoundVolume = procedure(Buffer : TSoundBuffer; Volume : integer); cdecl;
    TGetDSound       = function : IDirectSound; cdecl;
    TGetLibStatus    = function : integer; cdecl;

  var
    hLibrary : THandle = 0;

  function LoadSoundLib(Wnd : HWND) : BOOL; cdecl; forward;

  var
    InitSoundEngine : TInitSoundEngine = LoadSoundLib;
    DoneSoundEngine : TDoneSoundEngine = nil;
    CreateSound     : TCreateSound = nil;
    FillSound       : TFillSound = nil;
    StopsSound      : TStopsSound = nil;
    PlaysSound      : TPlaysSound = nil;
    DuplicateSound  : TDuplicateSound = nil;
    SetsSoundPan    : TSetsSoundPan = nil;
    SetsSoundVolume : TSetsSoundVolume = nil;
    GetDSound       : TGetDSound = nil;
    LibStatus       : TGetLibStatus = nil;
 } //.rag
 
  function LoadSoundLib(Wnd : HWND) : BOOL; cdecl;
    begin
      {hLibrary := LoadLibrary(SoundLibraryPath);
      if hLibrary <> 0
        then
          begin
            InitSoundEngine := GetProcAddress(hLibrary, 'InitSoundEngine');
            DoneSoundEngine := GetProcAddress(hLibrary, 'DoneSoundEngine');
            CreateSound     := GetProcAddress(hLibrary, 'CreateSound');
            FillSound       := GetProcAddress(hLibrary, 'FillSound');
            StopsSound      := GetProcAddress(hLibrary, 'StopsSound');
            PlaysSound      := GetProcAddress(hLibrary, 'PlaysSound');
            DuplicateSound  := GetProcAddress(hLibrary, 'DuplicateSound');
            SetsSoundPan    := GetProcAddress(hLibrary, 'SetsSoundPan');
            SetsSoundVolume := GetProcAddress(hLibrary, 'SetsSoundVolume');
            GetDSound       := GetProcAddress(hLibrary, 'GetDSound');
            LibStatus       := GetProcAddress(hLibrary, 'GetStatus');

            Result := Assigned(InitSoundEngine) and
                      Assigned(DoneSoundEngine) and
                      Assigned(CreateSound) and
                      Assigned(FillSound) and
                      Assigned(StopsSound) and
                      Assigned(PlaysSound) and
                      Assigned(DuplicateSound) and
                      Assigned(GetDSound) and 
                      InitSoundEngine(Wnd);  
            if not Result
              then FreeLibrary(hLibrary);}//.rag
            Result := InitSoundEngine(Wnd); 
       {   end
        else
          Result := false;}//.rag
    end;

  { REAL thing }

  type
    TSoundState = (sstStopped, sstPlaying, sstPaused);

    TSoundInfo =
      class
        public
          constructor Create(aSound : IDirectSoundBuffer; const aName : string; aKind, Length : integer);
          destructor  Destroy;                          override;
        public
          function Clone(aKind : integer) : TSoundInfo;
        public
          procedure Play(aLooped : boolean; pan, volume : single; startofs : integer);
          procedure SetPan(pan : single);
          procedure SetVolume(volume : single);
          procedure Stop;
          procedure Pause;
          procedure Resume;
        private
          fState  : TSoundState;
          fLooped : boolean;
          fPan    : integer;
          fVolume : integer;
          fSound  : IDirectSoundBuffer;
          fName   : string;
          fKind   : integer;
          fLength : integer;
        public
          property Name   : string  read fName;
          property Kind   : integer read fKind;
          property Length : integer read fLength;
      end;

  const
    MaxPan    = 10000;
    MaxVolume = 10000;

  constructor TSoundInfo.Create(aSound : IDirectSoundBuffer; const aName : string; aKind, Length : integer);
    begin
      inherited Create;
      fSound  := aSound;
      fState  := sstStopped;
      fName   := aName;
      fKind   := aKind;
      fLength := Length;
    end;

  destructor TSoundInfo.Destroy;
    begin
      Stop;
      inherited;
    end;

  function TSoundInfo.Clone(aKind : integer) : TSoundInfo;
    var
      NewSound : TSoundBuffer;
    begin
      if SoundsEnabled
        then
          begin
            DuplicateSound(fSound, NewSound);
            if NewSound <> nil
              then Result := TSoundInfo.Create(NewSound, fName, aKind, fLength)
              else Result := nil;
          end
        else result := nil;
    end;

  procedure TSoundInfo.Play(aLooped : boolean; pan, volume : single; startofs : integer);
    begin
      fPan    := trunc(pan * MaxPan);
      fVolume := trunc((volume - 1) * MaxVolume);
      PlaysSound(fSound, aLooped, true, fPan, fVolume, startofs);
      fLooped := aLooped;
      fState  := sstPlaying;
    end;

  procedure TSoundInfo.SetPan(pan : single);
    begin
      fPan := trunc(pan * MaxPan);
      SetsSoundPan(fSound, fPan);
    end;

  procedure TSoundInfo.SetVolume(volume : single);
    begin
      fVolume := trunc((volume - 1) * MaxVolume);
      SetsSoundVolume(fSound, fVolume);
    end;

  procedure TSoundInfo.Stop;
    begin
      StopsSound(fSound);
      fState := sstStopped;
    end;

  procedure TSoundInfo.Pause;
    begin
      if fState = sstPlaying
        then
          begin
            StopsSound(fSound);
            if not fLooped
              then fState := sstStopped
              else fState := sstPaused;
          end;
    end;

  procedure TSoundInfo.Resume;
    begin
      if fState = sstPaused
        then
          begin
            PlaysSound(fSound, fLooped, true, fPan, fVolume, 0);
            fState := sstPlaying;
          end;
    end;


  // ListUtils

  procedure ClearList(aList : TList);
    var
      i : integer;
    begin
      if aList <> nil
        then
          begin
            for i := 0 to pred(aList.Count) do
              TObject(aList[i]).Free;
            aList.Clear;
          end;
    end;

  procedure FreeList(var aList : TList);
    begin
      ClearList(aList);
      aList.Free;
      aList := nil;
    end;

  // Sound library

  function InitSoundLibrary(focus : hWnd) : boolean;
    begin
      if InitSoundEngine(focus)
        then
          begin
            SoundMap := TMapStringToObject.Create(mmOwn);
            gInstanceList := TList.Create;
            SoundsEnabled := true;
          end
        else SoundsEnabled := false;
      Result := SoundsEnabled;
    end;

  procedure DoneSoundLibrary;
    begin
      FreeList(gInstanceList);
      SoundMap.Free;

      {if hLibrary <> 0
        then
          begin
            DoneSoundEngine;
            FreeLibrary(hLibrary);
          end;}//.rag
      DoneSoundEngine;
    end;

  function GetDSoundInstance : IDirectSound;
//    var
  //    ds : integer; //.rag
    begin
      try
(*        ds := GetLibStatus;
        if ds <> 0
          then result := IDirectSound(ds)       
          else result := nil;
        {
        if assigned(GetDSound)
          then result := GetDSound
          else result := nil;
        }
*)       result := GetDSound;
      except
        result := nil;
      end;
    end;

{  function GetLibStatus : integer;
    begin
      if assigned(LibStatus)
        then result := LibStatus
        else result := -1;
    end;
 }
  procedure InitDSoundEngine( Wnd : HWND );
    begin
      {if assigned(InitSoundEngine)
        then}//.RAG
          begin
            InitSoundEngine( Wnd );
            EnableSounds;
          end;
    end;

  procedure DoneDSoundEngine;
    begin
      {if assigned(DoneSoundEngine)
        then}//.rag
          begin
            DisableSounds;
            UnloadAllSounds;
            DoneSoundEngine;
          end
    end;

  procedure EnableSounds;
    begin
      SoundsEnabled := SoundMap <> nil;
    end;

  procedure DisableSounds;
    begin
      StopAllSounds;
      SoundsEnabled := false;
    end;

  // Read callback
  procedure ReadData(Data : pointer; Size : DWORD; UserData : pointer);  stdcall;
    var
      S : TStream absolute UserData;
    begin
      S.Read(Data^, Size);
    end;

  function ReadSoundFromStream(Stream : TStream; const Name : string; out SndLen : integer) : TSoundBuffer;
    var
      Header : TWaveHeader;
      dLen   : integer;
    begin
      try
        Stream.Read(Header, sizeof(Header));
        with Header do
          begin
            GetDataFromStream(Stream, dLen);
            if not CreateSound(Result, dLen, nSamplesPerSec, nBitsPerSample, nBlockAlign, nChannels > 1)
              then raise Exception.Create('Cannot load sound : ' + Name);
            if not FillSound(Result, dLen, ReadData, Stream)
              then
                begin
                  Result := nil;
                  raise Exception.Create('Cannot read sound data from ' + Name);
                end;
            SndLen := dLen;
          end;
      except
        Result := nil;
      end;
    end;

  function LoadSoundFromFile(const FileName : string) : boolean;
    var
      Sound  : TSoundBuffer;
      Stream : TStream;
      SndLen : integer;
    begin
      if SoundMap <> nil
        then
          if SoundMap[FileName] = nil
            then
              if FileExists(FileName)
                then
                  try
                    Stream := TFileStream.Create(FileName, fmOpenRead);
                    try
                      Sound := ReadSoundFromStream(Stream, FileName, SndLen);
                      if Sound <> nil
                        then
                          begin
                            SoundMap[FileName] := TSoundInfo.Create(Sound, FileName, skClass, SndLen);
                            Result := true;
                          end
                        else Result := false;
                    finally
                      Stream.Free;
                    end;
                  except
                    Result := false;
                  end
                else Result := false
            else Result := true
        else Result := false;
    end;

  function LoadSoundFromModule(Handle : HModule; const ResName : string) : boolean;
    var
      Sound  : TSoundBuffer;
      Stream : TStream;
      SndLen : integer;
    begin
      if SoundMap <> nil
        then
          if SoundMap[ResName] = nil
            then
              try
                Stream := TResourceStream.Create(Handle, ResName, RT_RCDATA);
                try
                  Sound := ReadSoundFromStream(Stream, ResName, SndLen);
                  if Sound <> nil
                    then
                      begin
                        SoundMap[ResName] := TSoundInfo.Create(Sound, ResName, skClass, SndLen);
                        Result := true;
                      end
                    else Result := false;
                finally
                  Stream.Free;
                end;
              except
                Result := false;
              end
            else Result := true
        else Result := false;
    end;

  function LoadSoundFromResName(const ResName : string) : boolean;
    begin
      Result := LoadSoundFromModule(hInstance, ResName);
    end;

  procedure UnloadSound(const Name : string);
    begin
      if SoundMap <> nil
        then SoundMap.Remove(Name);
    end;

  procedure UnloadAllSounds;
    begin
      StopAllSounds;
      if SoundMap <> nil
        then SoundMap.Clear;
    end;


  // Utils
  function GetExistingSoundInstance(const Name : string; Kind : integer) : TSoundInfo;
    var
      i        : integer;
      Instance : TSoundInfo;
    begin
      if gInstanceList <> nil
        then
          begin
            i := 0;
            Result := nil;
            while (i < gInstanceList.Count) and (Result = nil) do
              begin
                Instance := gInstanceList[i];
                if (Instance.Name = Name) and (Instance.Kind = Kind)
                  then Result := Instance;
                inc(i);
              end;
          end
        else
          Result := nil;
    end;

  function GetSoundInstance(const Name : string; Kind : integer) : TSoundInfo;
    var
      SoundClass : TSoundInfo;
    begin
      Result := GetExistingSoundInstance(Name, Kind);
      if Result = nil
        then
          begin
            SoundClass := TSoundInfo(SoundMap[Name]);
            if SoundClass <> nil
              then
                begin
                  Result := SoundClass.Clone(Kind);
                  if Result <> nil
                    then gInstanceList.Add(Result);
                end;
          end;
    end;

  procedure FreeSoundInstance(const Name : string; Kind : integer);
    var
      Instance : TSoundInfo;
    begin
      Instance := GetExistingSoundInstance(Name, Kind);
      if Instance <> nil
        then
          begin
            Instance.Stop;
            gInstanceList.Remove(Instance);
            Instance.Free;
          end;
    end;

  function SoundLength(const Name : string; Kind : integer) : integer;
    var
      Sound : TSoundInfo;
    begin
      Sound := GetSoundInstance(Name, Kind);
      if Sound <> nil
        then Result := Sound.Length
        else raise ESoundError.Create('Sound ' + Name + ' not loaded');
    end;

  // Sound control

  procedure PlaySound(const Name : string; Kind : integer; Looped : boolean; pan, volume : single; startofs : integer);
    var
      Sound : TSoundInfo;
    begin
      if SoundsEnabled and (Name<>'')
        then
          begin
            Sound := GetSoundInstance(Name, Kind);
            if Sound <> nil
              then Sound.Play(Looped, pan, volume, startofs)
              else raise ESoundError.Create('Sound ' + Name + ' not loaded');
          end;
    end;

  procedure StopSound(const Name : string; Kind : integer);
    begin
      if SoundsEnabled
        then FreeSoundInstance(Name, Kind);
    end;

  procedure PauseSound(const Name : string; Kind : integer);
    var
      Sound : TSoundInfo;
    begin
      if SoundsEnabled
        then
          begin
            Sound := GetSoundInstance(Name, Kind);
            if Sound <> nil
              then Sound.Pause;
          end;
    end;

  procedure ResumeSound(const Name : string; Kind : integer);
    var
      Sound : TSoundInfo;
    begin
      if SoundsEnabled
        then
          begin
            Sound := GetSoundInstance(Name, Kind);
            if Sound <> nil
              then Sound.Resume;
          end;
      end;

  procedure SetSoundPan(const Name : string; Kind : integer; pan : single);
    var
      Sound : TSoundInfo;
    begin
      if SoundsEnabled
        then
          begin
            Sound := GetSoundInstance(Name, Kind);
            if Sound <> nil
              then Sound.SetPan(pan);
          end;
    end;

  procedure SetSoundVolume(const Name : string; Kind : integer; volume : single);
    var
      Sound : TSoundInfo;
    begin
      if SoundsEnabled
        then
          begin
            Sound := GetSoundInstance(Name, Kind);
            if Sound <> nil
              then Sound.SetVolume(volume);
          end;
    end;

  procedure StopAllSounds;
    begin
      if SoundsEnabled
        then ClearList(gInstanceList);
    end;

  procedure PauseAllSounds;
    var
      i : integer;
    begin
      if SoundsEnabled and (gInstanceList <> nil)
        then
          for i := 0 to pred(gInstanceList.Count) do
            TSoundInfo(gInstanceList[i]).Pause;
    end;

  procedure ResumeAllSounds;
    var
      i : integer;
    begin
      if SoundsEnabled and (gInstanceList <> nil)
        then
          for i := 0 to pred(gInstanceList.Count) do
            TSoundInfo(gInstanceList[i]).Resume;
    end;

end.
