unit MP3Reader;

interface

  uses
    Windows, mmSystem,
    DShow, // Download this (DXMedia) and all DX headers (required) from http://delphi-jedi.org/delphigraphics/jedi-index.htm
    FIFO;
    // http://homepages.borland.com/efg2lab/Library/Delphi/Graphics/Resources.htm

  type
    TMP3Reader =
      class
        public
          constructor Create(const aFileName : string; Loop : boolean );
          destructor  Destroy; override;
        private
          fStream      : IAMMultiMediaStream;
          fAudioStream : IAudioMediaStream;
          fAudioData   : IAudioData;
          fFormat      : TWaveFormatEx;
          fBuffer      : pointer;
          fSample      : IAudioStreamSample;
          fLoop        : boolean;
          fFIFO        : TFIFO;
        private
          function FeedData : boolean;
        public
          property Format : TWaveFormatEx read fFormat;
        public
          function Read(out Data; Size : integer) : integer;
      end;

implementation

  uses
    ComObj;

  constructor TMP3Reader.Create(const aFileName : string; Loop : boolean);
    var
      Audio   : IMediaStream;
      BufSize : dword;
      fname   : widestring;
      DestSize : integer;
    begin
      inherited Create;
      fLoop := Loop;
      fFIFO := TFIFO.Create;

      fStream := CoAMMultiMediaStream.Create as IAMMultiMediaStream;
      OleCheck(fStream.Initialize(STREAMTYPE_READ, AMMSF_NOGRAPHTHREAD, nil));
      OleCheck(fStream.AddMediaStream(nil, MSPID_PrimaryAudio, 0, Audio));

      fname := aFileName;

      OleCheck(fStream.OpenFile(PWideChar(fname), AMMSF_RUN));

      //OleCheck(fStream.GetMediaStream(MSPID_PrimaryAudio, Audio));
      fAudioStream := Audio as IAudioMediaStream;

      OleCheck(fAudioStream.GetFormat(fFormat));

      BufSize := fFormat.nAvgBytesPerSec div 4; // 1/4th of second
      getmem(fBuffer, BufSize);
      fAudioData := CreateCOMObject(CLSID_AMAudioData) as IAudioData;
      OleCheck(fAudioData.SetBuffer(BufSize, fBuffer, 0));
      OleCheck(fAudioData.SetFormat(fFormat));

      OleCheck(fAudioStream.CreateSample(fAudioData, 0, fSample));
    end;

  destructor TMP3Reader.Destroy;
    begin
      fSample := nil;
      fAudioData := nil;
      fAudioStream := nil;
      fStream := nil;
      freemem(fBuffer);
      fFIFO.Free;
      inherited;
    end;

  function TMP3Reader.FeedData : boolean;
    const
      MS_S_ENDOFSTREAM = $40003; // DShow header bug !!!
    var
      Res  : hResult;
      Len  : dword;
      dNul : dword;
      pNul : pointer;
    begin
      Res := fSample.Update(0, 0, nil, 0);
      if (Res = MS_S_ENDOFSTREAM) and fLoop
        then
          begin
            fStream.Seek(0);
            Res := fSample.Update(0, 0, nil, 0);
          end;
      Result := Succeeded(Res) and (Res <> MS_S_ENDOFSTREAM);
      if Result
        then
          begin
            fAudioData.GetInfo(dNul, pNul, Len);
            fFIFO.Write(fBuffer^, Len);
          end;
    end;

  function TMP3Reader.Read(out Data; Size : integer) : integer;
    begin
      while (fFIFO.Size < Size) and FeedData do;
      Result := fFIFO.Read(Data, Size);
    end;

end.

