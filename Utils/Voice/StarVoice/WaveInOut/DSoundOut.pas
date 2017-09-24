unit DSoundOut;

interface

  uses
    Windows,
    DirectSound,
    ExtTimer;

  type
    TDataProc = function(var Data; Size : integer) : integer of object;

    TDsoundOut =
      class
        public
          constructor Create;
          destructor  Destroy;                                                  override;
        public
          procedure Play;
          procedure Stop;
        public
          procedure Pause;
          procedure Resume;
        private
          fSamplingRate  : integer;
          fChannels      : integer;
          fBitsPerSample : integer;
          fBufferSize    : integer;
        private
          function MsToSamples(aMs : integer) : integer;
          function GetBlockAlign : integer;
        private
          fDSound         : IDirectSound;
          fBuffer         : IDirectSoundBuffer;
          fTimer          : TExtTimer;
          fNextWrite      : dword;
          fPlayBufferSize : dword;
          procedure CreateBuffer;
          procedure CheckBuffer;
          procedure Feed(aSize : integer);
          function  GetPlayedSize : integer;
          procedure TimerProc;
          procedure ReadData(var Data; Size : integer);
        private
          fOnData : TDataProc;
        public
          property OnData : TDataProc read fOnData write fOnData;
        public
          property SamplingRate  : integer read fSamplingRate  write fSamplingRate;
          property Channels      : integer read fChannels      write fChannels;
          property BitsPerSample : integer read fBitsPerSample write fBitsPerSample;
          property BlockAlign    : integer read GetBlockAlign;
          property BufferSize    : integer read fBufferSize    write fBufferSize;
        private
          fVolume : integer;
          procedure SetVolume(aVolume : integer);
          procedure UpdateBufferVolume;
        public
          property DirectSound : IDirectSound read fDSound write fDSound;
          property Volume : integer           read fVolume write SetVolume; // in percent, from 0 to 100
      end;

implementation

  uses
    mmSystem,
    SysUtils,
    DSoundUtils;

  const
    DefaultSamplingRate   = 8000;
    DefaultChannels       = 1;
    DefaultBitsPerSample  = 16;
    DefaultBufferSize     = 250;  // 250 ms
    DefaultEmulBufferSize = 1000; // 1000 ms

    BufferSpans = 6;

  constructor TDsoundOut.Create;
    begin
      inherited Create;
      SamplingRate  := DefaultSamplingRate;
      Channels      := DefaultChannels;
      BitsPerSample := DefaultBitsPerSample;
      BufferSize    := DefaultBufferSize;
      
      Volume := 100;

      fTimer := TExtTimer.Create(40);
      fTimer.OnTimer := TimerProc;
    end;
    
  destructor TDsoundOut.Destroy;
    begin
      Stop;
      try
        fBuffer := nil;
        fDSound := nil;
      except
      end;
      fTimer.Free;
      inherited;
    end;

  procedure TDsoundOut.Play;
    begin
      Stop;
      CreateBuffer;
      fBuffer.SetCurrentPosition(0);
      Resume;
      fTimer.Interval := BufferSize div BufferSpans;
      fTimer.Enabled := true;
    end;

  procedure TDsoundOut.Stop;
    begin
      if fBuffer <> nil
        then
          begin
            fTimer.Enabled := false;
            fBuffer.Stop;
            fBuffer := nil;
          end;
    end;

  procedure TDsoundOut.Pause;
    begin
      if fBuffer <> nil
        then fBuffer.Stop;
    end;

  procedure TDsoundOut.Resume;
    begin
      if fBuffer <> nil
        then DSoundCheck(fBuffer.Play(0, 0, DSBPLAY_LOOPING));
    end;

  function TDsoundOut.MsToSamples(aMs : integer) : integer;
    begin
      Result := MulDiv(aMs, fSamplingRate, 1000);
    end;

  function TDsoundOut.GetBlockAlign : integer;
    begin
      Result := (fBitsPerSample div 8) * fChannels;
    end;

  procedure TDsoundOut.CreateBuffer;
    var
      Desc   : TDSBUFFERDESC;
      DSCaps : TDSCAPS;
      Format : TWaveFormatEx;
    begin
      fBuffer := nil; // Destroy previous buffer

      if fDSound = nil
        then fDSound := CreateHelperDSound;

      fillchar(DSCaps, sizeof(DSCaps), 0);
      DSCaps.dwSize := sizeof(DSCaps);
      DSoundCheck(fDSound.GetCaps(DSCaps));
      if DSCaps.dwFlags and DSCAPS_EMULDRIVER <> 0
        then BufferSize := DefaultEmulBufferSize
        else BufferSize := DefaultBufferSize;

      FillPCMFormat(fSamplingRate, fBitsPerSample, fChannels, Format);
      fPlayBufferSize := MsToSamples(fBufferSize) * GetBlockAlign;

      fillchar(Desc, sizeof(Desc), 0);
      Desc.dwSize := sizeof(Desc);
      Desc.dwFlags := DSBCAPS_GLOBALFOCUS or
                      DSBCAPS_GETCURRENTPOSITION2 or
                      DSBCAPS_CTRLVOLUME;
      Desc.dwBufferBytes := fPlayBufferSize;
      Desc.lpwfxFormat   := @Format;

      DSoundCheck(fDSound.CreateSoundBuffer(Desc, fBuffer, nil));
      UpdateBufferVolume;

      fNextWrite := 0;
      Feed(fPlayBufferSize);
    end;

  procedure TDsoundOut.CheckBuffer;
    var
      Status : dword;
      hr     : hResult;
    begin
      if fBuffer <> nil
        then
          begin
            Status := 0;
            hr := fBuffer.GetStatus(Status);
            if Succeeded(hr) and (Status = DSBSTATUS_BUFFERLOST)
              then fBuffer.Restore;
          end;
    end;

  procedure TDsoundOut.Feed(aSize : integer);
    var
      FirstData  : pointer;
      SecondData : pointer;
      FirstSize  : dword;
      SecondSize : dword;
    begin 
      if (aSize > 0) and (fBuffer <> nil)
        then
          begin
            if Succeeded(fBuffer.Lock(fNextWrite, aSize, FirstData, FirstSize, SecondData, SecondSize, 0))
              then
                try
                  if FirstData <> nil
                    then ReadData(FirstData^, FirstSize);
                  if SecondData <> nil
                    then ReadData(SecondData^, SecondSize);
                  inc(fNextWrite, aSize);
                  if fNextWrite >= fPlayBufferSize
                    then dec(fNextWrite, fPlayBufferSize);
                finally
                  fBuffer.Unlock(FirstData, FirstSize, SecondData, SecondSize);
                end;
          end;
    end;

  function TDsoundOut.GetPlayedSize : integer;
    var
      WriteCursor : dword;
      PlayCursor  : dword;
    begin
      if fBuffer <> nil
        then
          begin
            fBuffer.GetCurrentPosition(@PlayCursor, @WriteCursor);
            if PlayCursor < fNextWrite
              then Result := PlayCursor + fPlayBufferSize - fNextWrite
              else Result := PlayCursor - fNextWrite;
          end
        else Result := 0;
    end;

  procedure TDsoundOut.TimerProc;
    begin
      CheckBuffer;
      Feed(GetPlayedSize);
    end;

  procedure TDsoundOut.ReadData(var Data; Size : integer);
    var
      DataBytes : TByteArray absolute Data;
      Written   : integer;
    begin
      if assigned(fOnData)
        then Written := fOnData(Data, Size)
        else Written := 0;
      fillchar(DataBytes[Written], Size - Written, 0);
    end;

  procedure TDsoundOut.SetVolume(aVolume : integer);
    begin
      fVolume := aVolume;
      UpdateBufferVolume;
    end;

  procedure TDsoundOut.UpdateBufferVolume;
    begin
      if fBuffer <> nil
        then fBuffer.SetVolume(PercentToBufferVolume(fVolume));
    end;

end.
