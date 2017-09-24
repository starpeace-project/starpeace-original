unit WaveIn;

interface

  uses
    Windows, MMSystem, Messages, 
    Classes;

  type
    TOnDataProc = function (var Data; Size : integer) : integer of object;

    TWaveRecorder =
      class
        public
          constructor Create;
          destructor  Destroy;                     override;
        public
          function  CanRecord : boolean;
          procedure DoRecord;
          procedure Stop;
        private
          fDevice        : dword;
          fSamplingRate  : integer;
          fChannels      : integer;
          fBitsPerSample : integer;
          fBufferSize    : integer;
        private
          function GetBlockAlign : integer;
        public
          property Device        : dword   read fDevice        write fDevice;
          property SamplingRate  : integer read fSamplingRate  write fSamplingRate;
          property Channels      : integer read fChannels      write fChannels;
          property BitsPerSample : integer read fBitsPerSample write fBitsPerSample;
          property BlockAlign    : integer read GetBlockAlign;
          property BufferSize    : integer read fBufferSize    write fBufferSize;
        private
          fSignalWnd  : hWnd;
          fAutoStop   : boolean;
          fManualStop : boolean;
        private
          fInHandle     : hWaveIn;
          fFirstBuffer  : TWaveHdr;
          fSecondBuffer : TWaveHdr;
        private
          procedure StopIt;
          procedure CloseHandles;
        public
          function MsToSamples(aMs : integer) : integer;
        protected
          procedure SignalWndProc(var Msg : TMessage);
          procedure FeedData(var aHeader : TWaveHdr);
          function  DataArrived(var Data; Size : integer) : integer;  virtual;
        private
          fOnData : TOnDataProc;
          fOnStop : TNotifyEvent;
        public
          property OnData : TOnDataProc  read fOnData write fOnData;
          property OnStop : TNotifyEvent read fOnStop write fOnStop;
      end;

implementation

  uses
    SysUtils, Forms,   
    WaveHdrs, MMCheck;

  const
    DefaultSamplingRate  = 8000;
    DefaultChannels      = 1;
    DefaultBitsPerSample = 16;
    DefaultBufferSize    = 250;  // 250 ms

  constructor TWaveRecorder.Create;
    begin
      inherited;
      fDevice        := WAVE_MAPPER;
      fSamplingRate  := DefaultSamplingRate;
      fChannels      := DefaultChannels;
      fBitsPerSample := DefaultBitsPerSample;
      fBufferSize    := DefaultBufferSize;
      fSignalWnd := AllocatehWnd(SignalWndProc);
    end;

  destructor TWaveRecorder.Destroy;
    begin
      Stop;
      fOnStop := nil;
      DeallocateBuffer(fFirstBuffer);
      DeallocateBuffer(fSecondBuffer);
      DeallocateHWND(fSignalWnd);
      inherited;
    end;

  function TWaveRecorder.CanRecord : boolean;
    var
      Format : TWaveFormatEx;
    begin
      with Format do
        begin
          wFormatTag      := WAVE_FORMAT_PCM;
          nChannels       := fChannels;
          wBitsPerSample  := fBitsPerSample;
          nSamplesPerSec  := fSamplingRate;
          nBlockAlign     := BlockAlign;
          nAvgBytesPerSec := nSamplesPerSec * nBlockAlign;
          cbSize          := 0;
        end;
      Result := waveInOpen(nil, fDevice, @Format, 0, 0, CALLBACK_NULL or WAVE_FORMAT_QUERY) = MMSYSERR_NOERROR;
    end;

  procedure TWaveRecorder.DoRecord;
    var
      Format      : TWaveFormatEx;
      BufferBytes : integer;
    begin
      Stop;

      DeallocateBuffer(fFirstBuffer);
      DeallocateBuffer(fSecondBuffer);

      with Format do
        begin
          wFormatTag      := WAVE_FORMAT_PCM;
          nChannels       := fChannels;
          wBitsPerSample  := fBitsPerSample;
          nSamplesPerSec  := fSamplingRate;
          nBlockAlign     := BlockAlign;
          nAvgBytesPerSec := nSamplesPerSec * nBlockAlign;
          cbSize          := 0;
        end;

      BufferBytes := MsToSamples(fBufferSize) * BlockAlign;
      AllocateBuffer(fFirstBuffer, BufferBytes);
      try
        AllocateBuffer(fSecondBuffer, BufferBytes);
        try
          TryMM(waveInOpen(@fInHandle, fDevice, @Format, fSignalWnd, DWORD(Self), CALLBACK_WINDOW));
          try
            TryMM(waveInPrepareHeader(fInHandle, @fFirstBuffer, sizeof(fFirstBuffer)));
            try
              TryMM(waveInPrepareHeader(fInHandle, @fSecondBuffer, sizeof(fSecondBuffer)));
              try
                TryMM(waveInAddBuffer(fInHandle, @fFirstBuffer, sizeof(fFirstBuffer)));
                TryMM(waveInAddBuffer(fInHandle, @fSecondBuffer, sizeof(fSecondBuffer)));

                fManualStop := false;
                fAutoStop   := false;
                TryMM(WaveInStart(fInHandle));
              except
                waveInUnprepareHeader(fInHandle, @fSecondBuffer, sizeof(fSecondBuffer));
                raise;
              end;
            except
              waveInUnprepareHeader(fInHandle, @fFirstBuffer, sizeof(fFirstBuffer));
              raise;
            end;
          except
            waveInClose(fInHandle);
            fInHandle := 0;
            raise;
          end;
        except
          DeallocateBuffer(fSecondBuffer);
          raise;
        end;
      except
        DeallocateBuffer(fFirstBuffer);
        raise;
      end;
    end;

  procedure TWaveRecorder.Stop;
    begin
      fManualStop := true;
      //StopIt;
      CloseHandles;
    end;

  function TWaveRecorder.GetBlockAlign : integer;
    begin
      Result := (fBitsPerSample div 8) * fChannels;
    end;

  procedure TWaveRecorder.StopIt;
    begin
      CloseHandles;
      if assigned(fOnStop)
        then fOnStop(Self);
    end;

  procedure TWaveRecorder.CloseHandles;
    begin
      if fInHandle <> 0
        then
          begin
            WaveInStop(fInHandle);
            WaveInReset(fInHandle);

            waveInUnprepareHeader(fInHandle, @fSecondBuffer, sizeof(fSecondBuffer));
            waveInUnprepareHeader(fInHandle, @fFirstBuffer, sizeof(fFirstBuffer));

            waveInClose(fInHandle);
            fInHandle := 0;
          end;
    end;

  function TWaveRecorder.MsToSamples(aMs : integer) : integer;
    begin
      Result := MulDiv(aMs, fSamplingRate, 1000);
    end;

  procedure TWaveRecorder.SignalWndProc(var Msg : TMessage);
    begin
      with Msg do
        if Msg = WIM_DATA
          then FeedData(PWaveHdr(lParam)^)
          else Result := DefWindowProc(fSignalWnd, Msg, wParam, lParam);
    end;

  procedure TWaveRecorder.FeedData(var aHeader : TWaveHdr);
    var
      Fed : dword;
    begin
      with aHeader do
        begin
          if dwBytesRecorded > 0
            then Fed := DataArrived(lpData^, dwBytesRecorded)
            else Fed := dwBytesRecorded;
          if not fAutoStop
            then
              if (Fed = dwBytesRecorded) and not fManualStop
                then waveInAddBuffer(fInHandle, @aHeader, sizeof(aHeader))
                else fAutoStop := true
            else StopIt;
        end;
    end;

  function TWaveRecorder.DataArrived(var Data; Size : integer) : integer;
    begin
      if assigned(fOnData)
        then
          try
            Result := fOnData(Data, Size);
          except
            Result := 0;
          end
        else Result := 0;
    end;

end.

