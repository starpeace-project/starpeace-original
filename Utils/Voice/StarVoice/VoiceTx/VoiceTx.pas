unit VoiceTx;

interface

  uses
    Windows, mmSystem,
    CompressIntf,
    WaveIn, WaveConvert;

  type
    TVoiceTx =
      class
        public
          constructor Create;
          destructor  Destroy;                                                  override;
        public
          procedure Start;
          procedure Stop;
        private
          fRecorder   : TWaveRecorder;
          fCompressor : ICompressor;
          procedure SetCompressor(aCompressor : ICompressor);
        private
          fConverter : TACMConverterOut;
          procedure FindRecordingFormat;
          procedure FreeConverter;
          procedure CreateConverter(const Src, Dst : TWaveFormatEx);
        public
          property Compressor : ICompressor read fCompressor write SetCompressor;
        private
          fPeak : single;
          function DoCompress(var Data; Size : integer) : integer;
          function RecorderData(var Data; Size : integer) : integer;
        public
          property Peak : single read fPeak;
      end;

implementation

  uses
    Math,
    DSoundUtils;

  constructor TVoiceTx.Create;
    begin
      inherited;
      fRecorder := TWaveRecorder.Create;
      fRecorder.BufferSize := 250; // ms
      fRecorder.OnData := RecorderData;
    end;

  destructor TVoiceTx.Destroy;
    begin
      Stop;
      fRecorder.Free;
      FreeConverter;
      inherited;
    end;

  procedure TVoiceTx.Start;
    begin
      FindRecordingFormat;
      fRecorder.DoRecord;
    end;

  procedure TVoiceTx.Stop;
    begin
      fRecorder.Stop;
    end;

  procedure TVoiceTx.SetCompressor(aCompressor : ICompressor);
    begin
      Stop;
      Assert(aCompressor <> nil);
      fCompressor := aCompressor;
    end;

  procedure TVoiceTx.FindRecordingFormat;
    type
      TRecFormat =
        record
          Rate     : integer;
          Bits     : integer;
          Channels : integer;
        end;
    const
      RecFormats : array[1..12] of TRecFormat =
        (
          (Rate : 11025; Bits : 16; Channels : 1),
          (Rate : 22050; Bits : 16; Channels : 1),
          (Rate : 44100; Bits : 16; Channels : 1),

          (Rate : 11025; Bits : 16; Channels : 2),
          (Rate : 22050; Bits : 16; Channels : 2),
          (Rate : 44100; Bits : 16; Channels : 2),

          (Rate : 11025; Bits : 8; Channels : 1),
          (Rate : 22050; Bits : 8; Channels : 1),
          (Rate : 44100; Bits : 8; Channels : 1),

          (Rate : 11025; Bits : 8; Channels : 2),
          (Rate : 22050; Bits : 8; Channels : 2),
          (Rate : 44100; Bits : 8; Channels : 2)
        );
    var
      srate : integer;
      bits  : integer;
      ch    : integer;
      Src   : TWaveFormatEx;
      Dst   : TWaveFormatEx;
      i     : integer;
    begin
      FreeConverter;

      Assert(fCompressor <> nil);

      fCompressor.GetInputType(srate, bits, ch);
      FillPCMFormat(srate, bits, ch, Dst);

      fRecorder.SamplingRate  := srate;
      fRecorder.BitsPerSample := bits;
      fRecorder.Channels      := ch;

      i := low(RecFormats);
      while (i <= high(RecFormats)) and not fRecorder.CanRecord do
        begin
          fRecorder.SamplingRate  := RecFormats[i].Rate;
          fRecorder.BitsPerSample := RecFormats[i].Bits;
          fRecorder.Channels      := RecFormats[i].Channels;
          FillPCMFormat(fRecorder.SamplingRate, fRecorder.BitsPerSample, fRecorder.Channels, Src);
          CreateConverter(Src, Dst);
          inc(i);
        end;
    end;

  procedure TVoiceTx.FreeConverter;
    begin
      fConverter.Free;
      fConverter := nil;
    end;

  procedure TVoiceTx.CreateConverter(const Src, Dst : TWaveFormatEx);
    begin
      FreeConverter;
      fConverter := TACMConverterOut.Create(Src, Dst);
      fConverter.OnWrite := DoCompress;
    end;

  function TVoiceTx.DoCompress(var Data; Size : integer) : integer;

    function IntAbs(x : integer) : integer;
      begin
        if x < 0
          then Result := -x
          else Result := x;
      end;
      
    var
      Data16M  : array[0..0] of smallint absolute Data;
      Samples  : integer;
      i        : integer;
      MaxPeak  : integer;
      AbsValue : integer;
    begin
      Assert(fCompressor <> nil);

      MaxPeak := 0;
      Samples := Size div sizeof(Data16M[0]);
      for i := 0 to pred(Samples) do
        begin
          AbsValue := IntAbs(Data16M[i]);
          MaxPeak := max(MaxPeak, AbsValue);
        end;
      fPeak := MaxPeak / 32768;

      fCompressor.Convert(Data, Size);

      Result := Size;
    end;

  function TVoiceTx.RecorderData(var Data; Size : integer) : integer;
    begin
      if fConverter <> nil
        then fConverter.Write(Data, Size)
        else DoCompress(Data, Size);
      Result := Size;
    end;

end.
