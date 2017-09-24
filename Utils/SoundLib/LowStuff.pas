unit LowStuff;

interface
  uses
    windows, DirectSound;
    
  type
    TReadCallback = procedure(Data : pointer; Size : DWORD; UserData : pointer); stdcall;
    
    function  InitSoundEngine(Wnd : HWND) : BOOL; cdecl;
    procedure DoneSoundEngine; cdecl;
    function  CreateSound(var Buffer : IDirectSoundBuffer; Size, Freq, Bits, Align : DWORD; Stereo : BOOL) : BOOL; cdecl;
    function  FillSound(Buffer : IDirectSoundBuffer; Size : DWORD; ReadData : TReadCallback; UserData : pointer) : BOOL; cdecl;
    procedure StopsSound(Buffer : IDirectSoundBuffer); cdecl;
    procedure PlaysSound(Buffer : IDirectSoundBuffer; LoopIt, RestartIt : BOOL; Pan, Volume, StartOfs : integer); cdecl;
    procedure DuplicateSound(Buffer : IDirectSoundBuffer; var Clone : IDirectSoundBuffer); cdecl;
    procedure SetsSoundPan(Buffer : IDirectSoundBuffer; Pan : integer); cdecl;
    procedure SetsSoundVolume(Buffer : IDirectSoundBuffer; Volume : integer); cdecl;
    function  GetDSound: IDirectSound; cdecl;
    function  GetLibStatus: integer; cdecl;

implementation
uses
  MMSystem;

var
  pDSound   : IDirectSound = nil;
  LibStatus : integer = 0;
  
function  InitSoundEngine(Wnd : HWND) : BOOL; cdecl;
  begin
    if succeeded(DirectSoundCreate(nil, pDSound, nil))
      then 
        begin
          if pDSound.SetCooperativeLevel(Wnd, DSSCL_NORMAL)=0
            then 
              begin
                LibStatus := integer(pDSound);
                result := true;
              end
            else 
              begin
                pDSound := nil;
                result := false;
              end;
        end
      else result := false;
  end;
  
procedure DoneSoundEngine; cdecl;
  begin
    pDSound    := nil;
    LibStatus := 0;
  end;

function  CreateSound(var Buffer : IDirectSoundBuffer; Size, Freq, Bits, Align : DWORD; Stereo : BOOL) : BOOL; cdecl;
  var
    dsCaps : TDSCaps;
    pcmwf  : TWaveFormatEx;
    BufferDesc: TDSBufferDesc;
  begin
    if (pDSound<>nil)
      then
        begin
          fillchar(dsCaps, sizeof(dsCaps), 0);
          dsCaps.dwSize := sizeof(dsCaps);
          pDSound.GetCaps(dsCaps);
          // Set up wave format structure.
          fillchar(pcmwf, sizeof(pcmwf), 0);
          with pcmwf do
            begin
              wFormatTag      := WAVE_FORMAT_PCM;
              if Stereo
                then nChannels := 2
                else nChannels := 1;
              nSamplesPerSec  := Freq;
              nBlockAlign     := Align;
              nAvgBytesPerSec := nSamplesPerSec * nBlockAlign;
              wBitsPerSample  := Bits;

            // Set up DSBUFFERDESC structure.
              fillchar(BufferDesc, sizeof(BufferDesc),0);
            end;
          with BufferDesc do
            begin
              dwSize := sizeof(BufferDesc);
              if Size< dsCaps.dwUnlockTransferRateHwBuffers * 120
                then dwFlags := DSBCAPS_CTRLDEFAULT
                else dwFlags := DSBCAPS_CTRLDEFAULT or DSBCAPS_LOCSOFTWARE;
              dwBufferBytes := Size; 
              lpwfxFormat   := @pcmwf;
            end;
          result := succeeded(pDSound.CreateSoundBuffer(BufferDesc, Buffer, nil));
        end
      else 
        begin
          Buffer := nil;
          result := false;
        end;
  end;

function FillSound(Buffer : IDirectSoundBuffer; Size : DWORD; ReadData : TReadCallback; UserData : pointer) : BOOL; cdecl;
  var
    pData1 : pointer;
    pData2 : pointer;
    dwData1Size : cardinal;
    dwData2Size : cardinal;
  begin
    if (Buffer<>nil) and assigned(ReadData)
      then 
        begin
          if succeeded(Buffer.Lock(0, Size, pData1, dwData1Size, pData2, dwData2Size, DSBLOCK_FROMWRITECURSOR))
            then 
              try
                if (dwData1Size > 0)
                  then ReadData(pData1, dwData1Size, UserData);

                if (dwData2Size > 0)
                  then ReadData(pData2, dwData2Size, UserData);

                Buffer.Unlock(pData1, dwData1Size, pData2, dwData2Size);
                result := true;
              except
                Buffer.Unlock(pData1, dwData1Size, pData2, dwData2Size);
                result := false;
              end
            else result := false;
        end
      else result := false;
  end;

procedure StopsSound(Buffer : IDirectSoundBuffer); cdecl;
  var
    dwStatus : cardinal;
  begin
    if (Buffer<>nil) and succeeded(Buffer.GetStatus(dwStatus)) and ((dwStatus and DSBSTATUS_PLAYING) = DSBSTATUS_PLAYING)
      then Buffer.Stop;
  end;

procedure PlaysSound(Buffer : IDirectSoundBuffer; LoopIt, RestartIt: BOOL; Pan, Volume, StartOfs : integer); cdecl;
  var
    dwStatus : cardinal;
    Loop  : integer;
  begin
    if (Buffer<>nil) and succeeded(Buffer.GetStatus(dwStatus))
      then 
        begin
          StopsSound(Buffer);
          with Buffer do
            begin
              if RestartIt
                then 
                    begin
                      SetPan(Pan);
                      SetVolume(Volume);
                      SetCurrentPosition(StartOfs);              
                    end;
              if LoopIt 
                then Loop := DSBPLAY_LOOPING
                else Loop := 0;
              Play(0, 0, Loop);
            end;
        end;
  end;

procedure DuplicateSound(Buffer : IDirectSoundBuffer; var Clone : IDirectSoundBuffer); cdecl;
  begin
    Clone := nil;
    if (pDSound<>nil) and (Buffer<>nil)
      then pDSound.DuplicateSoundBuffer(Buffer, Clone);
  end;

procedure SetsSoundPan(Buffer : IDirectSoundBuffer; Pan : integer); cdecl;
  var
    dwStatus : cardinal;
  begin
    if (Buffer<>nil) and succeeded(Buffer.GetStatus(dwStatus))
      then Buffer.SetPan(Pan);
  end;

procedure SetsSoundVolume(Buffer : IDirectSoundBuffer; Volume : integer); cdecl;
  var
    dwStatus : cardinal;
//    OK : HResult;
  begin
    if (Buffer<>nil) and succeeded(Buffer.GetStatus(dwStatus))
      then Buffer.SetVolume(Volume);  //.rag //ok := 
  end;

function  GetDSound: IDirectSound; cdecl;
  begin
    result := pDSound;
  end;

function  GetLibStatus: integer; cdecl;
  begin
    result := LibStatus;
  end;

end.
