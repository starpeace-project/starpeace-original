unit WaveOut;

interface

  uses
    Windows, MMSystem,
    Classes;

  type
    TOnDataProc = function (var Data; Size : integer) : integer of object;

    TWavePlayer =
      class
        public
          constructor Create;
          destructor  Destroy;                                 override;
        public
          procedure Play;                                      virtual;
          procedure Stop;                                      virtual;
          procedure Wait;
        private
          fDevice        : integer;
          fSamplingRate  : integer;
          fChannels      : integer;
          fBitsPerSample : integer;
          fBufferSize    : integer;
        private
          function  GetBlockAlign : integer;
        public
          property Device        : integer read fDevice        write fDevice;
          property SamplingRate  : integer read fSamplingRate  write fSamplingRate;
          property Channels      : integer read fChannels      write fChannels;
          property BitsPerSample : integer read fBitsPerSample write fBitsPerSample;
          property BlockAlign    : integer read GetBlockAlign;
          property BufferSize    : integer read fBufferSize    write fBufferSize;
        public
          soPlayFinished : THandle;
          csFillingData  : TRTLCriticalSection;
          fAutoStop      : boolean;
          fManualStop    : boolean;
        private
          fOutHandle     : hWaveOut;
          fFirstBuffer   : TWaveHdr;
          fSecondBuffer  : TWaveHdr;
        private
          procedure StopIt;
          procedure CloseHandles;
        protected
          function  MsToSamples(aMs : integer) : integer;
          procedure FeedData(var aHeader : TWaveHdr; ForceIt : boolean);
          function  DataArrived(var Data; Size : integer) : integer;  virtual;
        private
          fOnData : TOnDataProc;
          fOnStop : TNotifyEvent;
        public
          property OnData : TOnDataProc read fOnData write fOnData;
          property OnStop : TNotifyEvent read fOnStop write fOnStop;
      end;

implementation

  uses
    WaveHdrs, MMCheck,
    SysUtils;

  const
    DefaultSamplingRate  = 8000;
    DefaultChannels      = 1;
    DefaultBitsPerSample = 16;
    DefaultBufferSize    = 250;  // 250 ms

  constructor TWavePlayer.Create;
    begin
      inherited;
      fDevice        := word(WAVE_MAPPER);
      fSamplingRate  := DefaultSamplingRate;
      fChannels      := DefaultChannels;
      fBitsPerSample := DefaultBitsPerSample;
      BufferSize     := DefaultBufferSize;
      soPlayFinished := CreateEvent(nil, false, true, nil);
      InitializeCriticalSection(csFillingData);
    end;

  destructor TWavePlayer.Destroy;
    begin
      Stop;
      fOnStop := nil;
      CloseHandle(soPlayFinished);
      DeleteCriticalSection(csFillingData);
      DeallocateBuffer(fFirstBuffer);
      DeallocateBuffer(fSecondBuffer);
      inherited;
    end;

  procedure DataArrivedProc(hdrvr : HDRVR; uMsg : UINT; dwUser : DWORD; dw1, dw2 : DWORD) stdcall;
    begin
      if (uMsg = WOM_DONE) and (dwUser <> 0)
        then
          with TWavePlayer(dwUser) do
            begin
              EnterCriticalSection(csFillingData);
              try
                FeedData(PWaveHdr(dw1)^, false);
              finally
                LeaveCriticalSection(csFillingData);
              end;
            end;
    end;

  procedure TWavePlayer.Play;
    var
      Format      : TWaveFormatEx;
      BufferBytes : integer;
    begin
      Stop;

      DeallocateBuffer(fFirstBuffer);
      DeallocateBuffer(fSecondBuffer);

      BufferBytes := MsToSamples(fBufferSize) * BlockAlign;
      AllocateBuffer(fFirstBuffer, BufferBytes);
      AllocateBuffer(fSecondBuffer, BufferBytes);

      with Format do
        begin
          wFormatTag      := WAVE_FORMAT_PCM;
          nChannels       := fChannels;
          wBitsPerSample  := fBitsPerSample;
          nSamplesPerSec  := fSamplingRate;
          nBlockAlign     := (wBitsPerSample div 8) * nChannels;
          nAvgBytesPerSec := nSamplesPerSec * nBlockAlign;
          cbSize          := 0;
        end;

      try
        TryMM(waveOutOpen(@fOutHandle, fDevice, @Format, DWORD(@DataArrivedProc), DWORD(Self), CALLBACK_FUNCTION));
        try
          TryMM(waveOutPrepareHeader(fOutHandle, @fFirstBuffer, sizeof(fFirstBuffer)));
          try
            TryMM(waveOutPrepareHeader(fOutHandle, @fSecondBuffer, sizeof(fSecondBuffer)));
            try
              ResetEvent(soPlayFinished);
              fManualStop := false;
              fAutoStop   := false;
              FeedData(fFirstBuffer, true);
              if not fAutoStop
                then FeedData(fSecondBuffer, false);
            except
              SetEvent(soPlayFinished);
              waveOutUnprepareHeader(fOutHandle, @fSecondBuffer, sizeof(fSecondBuffer));
              raise;
            end;
          except
            waveOutUnprepareHeader(fOutHandle, @fFirstBuffer, sizeof(fFirstBuffer));
            raise;
          end;
        except
          waveOutClose(fOutHandle);
          fOutHandle := 0;
          raise;
        end;
      except
        raise;
      end;
    end;

  procedure TWavePlayer.Stop;
    begin
      fManualStop := true;
      Wait;
    end;

  procedure TWavePlayer.Wait;
    begin
      WaitForSingleObject(soPlayFinished, INFINITE);
      SetEvent(soPlayFinished);
    end;

  function TWavePlayer.GetBlockAlign : integer;
    begin
      Result := (fBitsPerSample div 8) * fChannels;
    end;

  procedure TWavePlayer.StopIt;
    begin
      EnterCriticalSection(csFillingData);
      try
        CloseHandles;
        SetEvent(soPlayFinished);
        if assigned(fOnStop)
          then fOnStop(Self);
      finally
        LeaveCriticalSection(csFillingData);
      end;
    end;

  procedure TWavePlayer.CloseHandles;
    begin
      if fOutHandle <> 0
        then
          begin
            WaveOutReset(fOutHandle);

            waveOutUnprepareHeader(fOutHandle, @fSecondBuffer, sizeof(fSecondBuffer));
            waveOutUnprepareHeader(fOutHandle, @fFirstBuffer, sizeof(fFirstBuffer));

            waveOutClose(fOutHandle);
            fOutHandle := 0;
          end;
    end;

  function TWavePlayer.MsToSamples(aMs : integer) : integer;
    begin
      Result := MulDiv(aMs, fSamplingRate, 1000);
    end;

  procedure TWavePlayer.FeedData(var aHeader : TWaveHdr; ForceIt : boolean);
    var
      Fed  : dword;
      Fill : byte;
    begin
      EnterCriticalSection(csFillingData);
      try
        if not fAutoStop
          then
            with aHeader do
              begin
                Fed := DataArrived(lpData^, dwBufferLength);
                if fBitsPerSample = 8
                  then Fill := 128
                  else Fill := 0;
                fillchar(PByteArray(lpData)[Fed], dwBufferLength - Fed, Fill);

                if ForceIt or ((Fed > 0) and not fManualStop)
                  then waveOutWrite(fOutHandle, @aHeader, sizeof(aHeader))
                  else fAutoStop := true;
              end
          else StopIt;
      finally
        LeaveCriticalSection(csFillingData);
      end;
    end;

  function TWavePlayer.DataArrived(var Data; Size : integer) : integer;
    begin
      if assigned(fOnData)
        then
          try
            Result := fOnData(Data, Size)
          except
            Result := 0;
          end
        else Result := 0;
    end;

end.

