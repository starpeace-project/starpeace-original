unit StarVoice;

interface

  uses
    Windows, SysUtils,
    DirectSound,
    CompressIntf, FIFOIntf,
    VoiceTx, VoiceRx;

  type
    EVoiceChat = class(Exception);

    TVoiceChat = class;

    TOnDSoundRequest = procedure(Sender : TVoiceChat; Request : boolean) of object; 

    TVoiceChat =
      class
        public
          constructor Create(const aDSound : IDirectSound);
          destructor  Destroy;                                                  override;
        private
          fTxObject     : TVoiceTx;
          fRxObject     : TVoiceRx;
          fCompressor   : ICompressor;
          fDecompressor : IDecompressor;
        private
          fTransmit : boolean;
          fReceive  : boolean;
          fOnAir    : boolean;
          procedure SetTransmit(aTransmit : boolean);
          procedure SetReceive(aReceive : boolean);
          procedure SetOnAir(aOnAir : boolean);
          function  GetDirectSound : IDirectSound;
          procedure SetDirectSound(const aDSound : IDirectSound);
          function  GetSendFIFO : IFIFO;
          function  GetRecvFIFO : IFIFO;
        public
          property DirectSound : IDirectSound read GetDirectSound write SetDirectSound;
        public
          property SendFIFO : IFIFO read GetSendFIFO;
          property RecvFIFO : IFIFO read GetRecvFIFO;
        public
          property Transmit : boolean read fTransmit write SetTransmit;
          property Receive : boolean  read fReceive  write SetReceive;
          property OnAir : boolean    read fOnAir    write SetOnAir;
        public
          function  VUMeter : single;
          procedure ResetCompressor;
          procedure ResetDecompressor;
        private
          fOnDSoundRequest : TOnDSoundRequest;
        public
          property OnDSoundRequest : TOnDSoundRequest read fOnDSoundRequest write fOnDSoundRequest;
      end;

implementation

  uses
    mmSystem,
    Forms,
    FIFOUtils, CodecIntf;

  constructor TVoiceChat.Create(const aDSound : IDirectSound);
    begin
      inherited Create;
      if aDSound <> nil
        then
          begin
            Codecs.Initialize(ExtractFilePath(paramstr(0)));
            if Codecs.Count <= 0
              then raise EVoiceChat.Create('No codecs were found.');

            fCompressor := Codecs.OpenCompressor(0);
            fCompressor.SetOutputFIFO(CreateFIFO);
            fDecompressor := Codecs.OpenDecompressor(0);
            fDecompressor.SetInputFIFO(CreateFIFO);

            fTxObject := TVoiceTx.Create;
            fTxObject.Compressor := fCompressor;
            fRxObject := TVoiceRx.Create;
            fRxObject.Decompressor := fDecompressor;
          end
        else raise Exception.Create( 'Multimedia system is busy.' );
    end;

  destructor TVoiceChat.Destroy;
    begin
      fTxObject.Free;
      fRxObject.Free;

      fCompressor := nil;
      fDecompressor := nil;
      inherited;
    end;

  procedure TVoiceChat.SetTransmit(aTransmit : boolean);
    begin
      if aTransmit <> fTransmit
        then
          begin
            if aTransmit
              then fTxObject.Start
              else fTxObject.Stop;
            fTransmit := aTransmit;
          end;
    end;

  procedure TVoiceChat.SetReceive(aReceive : boolean);
    begin
      if aReceive <> fReceive
        then
          begin
            if aReceive
              then fRxObject.Start
              else fRxObject.Stop;
            fReceive := aReceive;
          end;
    end;

  procedure TVoiceChat.SetOnAir(aOnAir : boolean);
    begin
      if aOnAir
        then
          begin
            {$IFNDEF HALFDUPLEX} // this is just for test purposes
            try
              Transmit := true;
            except
            {$ENDIF}
              // Try half duplex, Stop & kill DSound first
              Receive := false;
              DirectSound := nil;
              if assigned(fOnDsoundRequest)
                then fOnDsoundRequest(Self, false);
              Transmit := true;
            {$IFNDEF HALFDUPLEX}
            end;
            {$ENDIF}
          end
        else
          begin
            // Stop recording & play if stopped
            Transmit := false;
            if assigned(fOnDsoundRequest) and not Receive
              then fOnDsoundRequest(Self, true);
            Receive := true;
          end;
      fOnAir := aOnAir;
    end;

  function TVoiceChat.GetDirectSound : IDirectSound;
    begin
      Result := fRxObject.DirectSound;
    end;

  procedure TVoiceChat.SetDirectSound(const aDSound : IDirectSound);
    begin
      fRxObject.DirectSound := aDSound;
    end;
    
  function TVoiceChat.GetSendFIFO : IFIFO;
    begin
      fCompressor.GetOutputFIFO(Result);
    end;
    
  function TVoiceChat.GetRecvFIFO : IFIFO;
    begin
      fDecompressor.GetInputFIFO(Result);
    end;

  function TVoiceChat.VUMeter : single;
    begin
      if fTransmit
        then Result := fTxObject.Peak
        else Result := 0;
    end;

  procedure TVoiceChat.ResetCompressor;
    begin
      fCompressor.Reset;
    end;

  procedure TVoiceChat.ResetDecompressor;
    begin
      fDecompressor.Reset;
    end;

end.
