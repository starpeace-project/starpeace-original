unit MP3Player;

interface

  uses
    Windows, Messages,
    DirectSound,
    DSoundOut,
    MP3Reader;

  type
    TMP3PlayerState = (mpsStopped, mpsPlaying, mpsPaused);

    TMP3Player = class;

    TOnStateChange = procedure(Sender : TMP3Player; PrevState, NewState : TMP3PlayerState) of object;

    TMP3Player =
      class
        public
          constructor Create;
          destructor  Destroy; override;
        public
          procedure Play;
          procedure Stop;
          procedure Pause;
        private
          fFileName      : string;
          fLoop          : boolean;
          fState         : TMP3PlayerState;
          fOnStateChange : TOnStateChange;
          function  GetVolume : integer;
          procedure SetVolume(aVolume : integer);
          function  GetDirectSound : IDirectSound;
          procedure SetDirectSound(const aDirectSound : IDirectSound);
          procedure SetState(aState : TMP3PlayerState);
        public
          property DirectSound : IDirectSound read GetDirectSound write SetDirectSound;
          property FileName : string          read fFileName      write fFileName;
          property Loop : boolean             read fLoop          write fLoop;
          property Volume : integer           read GetVolume      write SetVolume;
          property State : TMP3PlayerState    read fState;
        public
          property OnStateChange : TOnStateChange read fOnStateChange write fOnStateChange;
        private
          fPlayer    : TDsoundOut;
          fReader    : TMP3Reader;
          fLastChunk : integer;
          fAutoStop  : boolean;
          fSyncWnd   : hWnd;
          procedure  OpenStream;
          function   PlayerData(var Data; Size : integer) : integer;
          procedure  WndProc(var Msg : TMessage);
      end;

implementation

  uses
    Forms, SysUtils;

  const
    BufferMs = 1000;

    CM_STOP = WM_USER + 10;

  constructor TMP3Player.Create;
    begin
      inherited Create;
      fPlayer := TDsoundOut.Create;
      fPlayer.OnData := PlayerData;
      fPlayer.BufferSize := BufferMs;
      fSyncWnd := AllocatehWnd(WndProc);
    end;

  destructor TMP3Player.Destroy;
    begin
      fOnStateChange := nil;
      Stop;
      fPlayer.Free;
      fReader.Free;
      DeallocatehWnd(fSyncWnd);
      inherited;
    end;

  procedure TMP3Player.Play;
    begin
      case State of
        mpsStopped :
          begin
            OpenStream;
            fPlayer.Play;
            SetState(mpsPlaying);
          end;
        mpsPaused :
          begin
            fPlayer.Resume;
            SetState(mpsPlaying);
          end;
      end;
    end;

  procedure TMP3Player.Stop;
    begin
      fPlayer.Stop;
      SetState(mpsStopped);
    end;

  procedure TMP3Player.Pause;
    begin
      case State of
        mpsPlaying :
          begin
            fPlayer.Pause;
            SetState(mpsPaused);
          end;
      end;
    end;

  function TMP3Player.GetVolume : integer;
    begin
      Result := fPlayer.Volume;
    end;

  procedure TMP3Player.SetVolume(aVolume : integer);
    begin
      fPlayer.Volume := aVolume;
    end;
    
  function TMP3Player.GetDirectSound : IDirectSound;
    begin
      Result := fPlayer.DirectSound;
    end;

  procedure TMP3Player.SetDirectSound(const aDirectSound : IDirectSound);
    begin
      fPlayer.DirectSound := aDirectSound;
    end;

  procedure TMP3Player.SetState(aState : TMP3PlayerState);
    begin
      if aState <> fState
        then
          begin
            if assigned(fOnStateChange)
              then fOnStateChange(Self, fState, aState);
            fState := aState;
          end;
    end;

  procedure TMP3Player.OpenStream;
    begin
      Stop;
      
      //FreeAndNil(fReader);
      fReader.Free;
      fReader := nil;
      fReader := TMP3Reader.Create(fFileName, fLoop);

      fPlayer.SamplingRate  := fReader.Format.nSamplesPerSec;
      fPlayer.Channels      := fReader.Format.nChannels;
      fPlayer.BitsPerSample := fReader.Format.wBitsPerSample;
    end;

  function TMP3Player.PlayerData(var Data; Size : integer) : integer;
    var
      Read : integer;
    begin
      Read := fReader.Read(Data, Size);
      if Read < Size
        then
          begin
            fillchar(TByteArray(Data)[Read], Size - Read, 0);
            inc(fLastChunk, Size - Read);
            if (fLastChunk >= MulDiv(fPlayer.BufferSize, fReader.Format.nAvgBytesPerSec, 1000)) and not fAutoStop
              then
                begin
                  PostMessage(fSyncWnd, CM_STOP, 0, 0);
                  fAutoStop := true;
                end;
          end
        else
          begin
            fLastChunk := 0;
            fAutoStop  := false;
          end;
      Result := Size;
    end;

  procedure TMP3Player.WndProc(var Msg : TMessage);
    begin
      with Msg do
        if Msg = CM_STOP
          then Stop
          else Result := DefWindowProc(fSyncWnd, Msg, wParam, lParam);
    end;

end.
