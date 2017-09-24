unit VoiceRx;

interface

  uses
    Classes,
    DirectSound,
    DSoundOut,
    CompressIntf;

  type
    TVoiceRx =
      class
        public
          constructor Create;
          destructor  Destroy;                                                  override;
        public
          procedure Start;
          procedure Stop;
        private
          fPlayer       : TDSoundOut;
          fDecompressor : IDecompressor;
          procedure SetDecompressor(aDecompressor : IDecompressor);
          function  GetDirectSound : IDirectSound;
          procedure SetDirectSound(const aDSound : IDirectSound);
        public
          property Decompressor : IDecompressor read fDecompressor  write SetDecompressor;
          property DirectSound : IDirectSound   read GetDirectSound write SetDirectSound;
        private
          fHold      : boolean;
          fHoldCount : integer;
          //fHoldTime  : integer; 
          procedure SetHold(aHold : boolean);
          function PlayerData(var Data; Size : integer) : integer;
        private
          property Hold : boolean read fHold write SetHold;
        private
 //         fTT : TStream;
      end;

implementation

  uses
    SysUtils;

  const  
    HoldTime = 16;

  const
    MinHold = 0;
    MaxHold = 8000 * 5{seconds} * sizeof(smallint);

  constructor TVoiceRx.Create;
    begin
      inherited Create;
      fPlayer := TDSoundOut.Create;
      fPlayer.OnData := PlayerData;
//.test      fTT := TFileStream.Create('c:\x.raw', fmCreate);
    end;
    
  destructor TVoiceRx.Destroy;
    begin
      Stop;

      fPlayer.Free;
//.test      fTT.Free;
      inherited;
    end;

  procedure TVoiceRx.Start;
    begin
      fPlayer.Play;
    end;
    
  procedure TVoiceRx.Stop;
    begin
      fPlayer.Stop;
    end;

  procedure TVoiceRx.SetDecompressor(aDecompressor : IDecompressor);
    var
      srate : integer;
      bits  : integer;
      ch    : integer;
    begin
      Stop;

      Assert(aDecompressor <> nil);
      fDecompressor := aDecompressor;
      fDecompressor.GetOutputType(srate, bits, ch);
      fPlayer.SamplingRate  := srate;
      fPlayer.BitsPerSample := bits;
      fPlayer.Channels      := ch;
    end;

  function TVoiceRx.GetDirectSound : IDirectSound;
    begin
      Result := fPlayer.DirectSound;
    end;

  procedure TVoiceRx.SetDirectSound(const aDSound : IDirectSound);
    begin
      fPlayer.DirectSound := aDSound;
    end;

  procedure TVoiceRx.SetHold(aHold : boolean);
    begin
      if aHold <> fHold
        then
          begin
            fHold := aHold;
            if fHold
              then fHoldCount := HoldTime
              else fHoldCount := 0;
          end;
    end;

  function TVoiceRx.PlayerData(var Data; Size : integer) : integer;
    var
      Count : integer;
      Avail : integer;
    begin
      if not Hold
        then fDecompressor.Convert(Data, Size, Count)
        else Count := 0;

      if Count < Size
        then fillchar(TByteArray(Data)[Count], Size - Count, 0);

      fDecompressor.GetAvailData(Avail);
      if Avail <= MinHold
        then Hold := true
        else
          if Avail >= MaxHold
            then Hold := false;

      if fHoldCount > 0
        then
          begin
            dec(fHoldCount);
            if fHoldCount = 1
              then Hold := false;
          end;

//.test      if Size > 0
//.test        then fTT.Write(Data, Size);

      Result := Size;
    end;

end.
