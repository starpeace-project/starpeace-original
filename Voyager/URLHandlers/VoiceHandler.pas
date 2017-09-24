unit VoiceHandler;

interface

  uses
    VoyagerInterfaces, VoyagerServerInterfaces, Classes, Controls, MPlayer, ExtCtrls, StarVoice, VoicePanelViewer, VCLUtils;

  type
    TVoiceHandler =
      class( TInterfacedObject, IMetaURLHandler, IURLHandler )
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fSendTimer  : TTimer;
          fLimitTimer : TTimer;
          fVoiceObj   : TVoiceChat;
          fVoicePanel : TVoicePanel;
        // IMetaURLHandler
        private
          function getName    : string;
          function getOptions : TURLHandlerOptions;
          function getCanHandleURL( URL : TURL ) : THandlingAbility;
          function Instantiate : IURLHandler;
        // IURLHandler
        private
          function  HandleURL( URL : TURL ) : TURLHandlingResult;
          function  HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
          function  getControl : TControl;
          procedure setMasterURLHandler( const URLHandler : IMasterURLHandler );
        private
          fMasterURLHandler : IMasterURLHandler;
          fClientView       : IClientView;
        private
          procedure OnSend( Sender : TObject );
          procedure OnLimitTimer( Sender : TObject );
          procedure OnDSoundRequest( Sender : TVoiceChat; Request : boolean );
        private
          fTxId        : integer;
          fNewTx       : integer;
          fLastOnAir   : string;
          fLastTxId    : integer;
          fWasOn       : boolean;
          fLimitCnt    : integer;
          fTalkRequest : boolean;
          fIgnoreTx    : boolean;
        private
          procedure StartOfVoiceRecording;
          procedure EndOfVoiceRecording;
        private
          procedure threadedVoiceThis( const parms : array of const );
          procedure threadedRequestVoiceTx( const parms : array of const );
          procedure threadedVoiceTxOver( const parms : array of const );
          procedure threadedCancelVoiceReq( const parms : array of const );
          procedure threadedVoiceStatusChange( const parms : array of const );
          procedure syncRequestVoiceTx( const parms : array of const );
      end;

  const
    tidMetaHandler_Voice = 'VoiceHandler';

  const
    evnStartOfVoiceRecording = 3910;
    evnEndOfVoiceRecording   = 3911;
    evnDSoundFreed           = 3912;
    evnDSoundRecreated       = 3913;


implementation

  uses
    LowStuff, //.rag
    SoundLib, Events, ServerCnxEvents, Forms, Threads, SysUtils, MathUtils;

  // TVoiceHandler

  constructor TVoiceHandler.Create;
    begin
      inherited Create;
    end;

  destructor TVoiceHandler.Destroy;
    begin
      fSendTimer.Free;
      fLimitTimer.Free;
      fVoiceObj.Free;
      RemoveComponentFreeAndNil(fVoicePanel); 
      inherited;
    end;

  function TVoiceHandler.getName : string;
    begin
      result := tidMetaHandler_Voice;
    end;

  function TVoiceHandler.getOptions : TURLHandlerOptions;
    begin
      result := [hopCacheable];
    end;

  function TVoiceHandler.getCanHandleURL( URL : TURL ) : THandlingAbility;
    begin
      result := 0;
    end;

  function TVoiceHandler.Instantiate : IURLHandler;
    begin
      result := self;
    end;

  function TVoiceHandler.HandleURL( URL : TURL ) : TURLHandlingResult;
    begin
      result := urlHandled;
    end;

  function TVoiceHandler.HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
    var
      VoiceMsgInfo : TVoiceMsgInfo absolute info;
      //ErrorCode    : TErrorCode;
    begin
      result := evnHandled;
      case EventId of
        evnVoiceMsg :
          begin
            //fClientView.SayThis( '', fLastOnAir + '->' + VoiceMsgInfo.From + ', NewTx: ' + IntToStr(integer(VoiceMsgInfo.NewTx)), ErrorCode );
            if VoiceMsgInfo.NewTx
             then fIgnoreTx := false;
            if not fIgnoreTx
              then
                begin
                  if ((fLastOnAir <> VoiceMsgInfo.From) or (fLastTxId <> VoiceMsgInfo.TxId))
                    then
                      begin
                        fVoiceObj.ResetDecompressor;
                        // fClientView.SayThis( '', fLastOnAir + '->' + VoiceMsgInfo.From + '... reset was called.', ErrorCode );
                        fLastOnAir := VoiceMsgInfo.From;
                        fLastTxId  := VoiceMsgInfo.TxId;
                        fVoicePanel.Speaker.Caption := VoiceMsgInfo.From;
                      end;
                  fVoiceObj.RecvFIFO.Write( VoiceMsgInfo.buffer, VoiceMsgInfo.len );
                end;
          end;
        evnStartOfVoiceRecording :
          begin
            fTalkRequest := true;
            fVoicePanel.PendingSign.PageIndex := 1;
            Defer( threadedRequestVoiceTx, priNormal, [self] );
          end;
        evnVoiceMsgAuthorized :
          if fTalkRequest
            then StartOfVoiceRecording;
        evnEndOfVoiceRecording :
          begin
            fTalkRequest := false;
            EndOfVoiceRecording;
          end;
        evnHandlerExposed :
          begin
            fNewTx    := 1;
            fIgnoreTx := true;
            Defer( threadedVoiceStatusChange, priNormal, [1] );
          end;
        evnHandlerUnexposed :
          Defer( threadedVoiceStatusChange, priNormal, [0] );
        evnLogonStarted:
          fMasterURLHandler.HandleURL( '?' + 'frame_Id=' + tidMetaHandler_Voice + '&frame_Close=yes' );
        evnShutDown:  //.rag
          begin
            fMasterURLHandler := nil;
            fClientView       := nil;
            with fVoicePanel do
              begin
                ClientView       := nil;
                MasterURLHandler := nil;
              end;
            RemoveComponentFreeAndNil(fVoicePanel); 
          end;
      end;
    end;

  function TVoiceHandler.getControl : TControl;
    begin
      if fVoicePanel = nil
        then
          begin
            fSendTimer  := TTimer.Create( nil );
            fLimitTimer := TTimer.Create( nil );
            fSendTimer.Enabled   := false;
            fSendTimer.Interval  := 200;
            fSendTimer.OnTimer   := OnSend;
            fLimitTimer.Interval := 1000;
            fLimitTimer.Enabled  := false;
            fLimitTimer.OnTimer  := OnLimitTimer;
            fVoiceObj            := TVoiceChat.Create( SoundLib.GetDSoundInstance );
            fVoiceObj.OnDSoundRequest := OnDSoundRequest;
            fVoicePanel := TVoicePanel.Create( nil );
            fSendTimer.Enabled := true;
            fVoiceObj.OnAir := false;
            fVoicePanel.MasterURLHandler := fMasterURLHandler;
            fVoicePanel.ClientView := fClientView;
          end;
      result := fVoicePanel;
    end;

  procedure TVoiceHandler.setMasterURLHandler( const URLHandler : IMasterURLHandler );
    begin
      fMasterURLHandler := URLHandler;
      URLHandler.HandleEvent( evnAnswerClientView, fClientView );
    end;

  procedure TVoiceHandler.OnSend( Sender : TObject );
    const
      BufferSize = 1024;
    var
      len          : integer;
      Buffer       : array[0..pred(BufferSize)] of byte;
      threadBuffer : PByteArray;
    begin
      repeat
        fVoiceObj.SendFIFO.Read(Buffer, sizeof(Buffer), len);
        if len > 0
          then
            begin
              getmem( threadBuffer, len );
              move( Buffer, threadBuffer^, len );
              Defer( threadedVoiceThis, priNormal, [threadBuffer, len, fTxId, fNewTx] );
              fNewTx := 0;
            end;
      until len = 0;
      fVoiceObj.SendFIFO.GetSize( len );
      if (len = 0) and not fVoiceObj.OnAir and fWasOn
        then
          begin
            fVoiceObj.ResetCompressor;
            inc( fTxId );
            fNewTx := 1;
            fWasOn := false;
            Defer( threadedVoiceTxOver, priNormal, [self] );
          end;
      if fVoicePanel.VUGauge <> nil
        then fVoicePanel.VUGauge.Position := round(fVoiceObj.VUMeter*100);
    end;

  procedure TVoiceHandler.OnLimitTimer( Sender : TObject );
    const
      LineLimit   = 60;
      BufferLimit = 20;
    begin
      inc( fLimitCnt );
      fVoicePanel.TimeLimit.Position := (100*fLimitCnt) div LineLimit;
      if fLimitCnt = LineLimit
        then EndOfVoiceRecording;
    end;

  procedure TVoiceHandler.OnDSoundRequest( Sender : TVoiceChat; Request : boolean );
    var
      dummy : integer;
    begin
      if Request
        then
          begin
            if LowStuff.GetLibStatus = 0
              then
                begin
                  SoundLib.InitDSoundEngine( Application.MainForm.Handle );
                  fMasterURLHandler.HandleEvent( evnDSoundRecreated, dummy );
                end;
          end
        else
          begin
            SoundLib.DoneDSoundEngine;
            fMasterURLHandler.HandleEvent( evnDSoundFreed, dummy );
          end;
    end;

  procedure TVoiceHandler.StartOfVoiceRecording;
    begin
      fVoiceObj.OnAir := true;
      fWasOn          := true;
      fLimitTimer.Enabled := true;
      fVoicePanel.OnTheAirSign.PageIndex := 1;
      fVoicePanel.PendingSign.PageIndex  := 0;
      fVoicePanel.BufferGauge.Position   := 0;
      fLimitCnt := 0;
    end;

  procedure TVoiceHandler.EndOfVoiceRecording;
    begin
      fVoiceObj.OnAir := false;
      fVoicePanel.OnTheAirSign.PageIndex := 0;
      fVoicePanel.PendingSign.PageIndex  := 0;
      fVoicePanel.TimeLimit.Position     := 0;
      fVoicePanel.BufferGauge.Position   := 0;
      fLimitTimer.Enabled := false;
      fLimitCnt := 0;
      Defer( threadedCancelVoiceReq, priNormal, [self] );
    end;

  procedure TVoiceHandler.threadedVoiceThis( const parms : array of const );
    var
      Buffer    : PByteArray;
      len       : integer;
      TxId      : integer;
      NewTx     : integer;
      ErrorCode : TErrorCode;
    begin
      try
        Buffer := parms[0].vPointer;
        len    := parms[1].vInteger;
        TxId   := parms[2].vInteger;
        NewTx  := parms[3].vInteger;
        try
          fClientView.VoiceThis( Buffer^, Len, TxId, NewTx, ErrorCode );
        finally
          freemem( Buffer, len );
        end;
      except
      end;
    end;

  procedure TVoiceHandler.threadedRequestVoiceTx( const parms : array of const );
    var
      ReqIdx    : integer;
      ErrorCode : TErrorCode;
    begin
      try
        ReqIdx := fClientView.VoiceRequest( ErrorCode );
        Join( syncRequestVoiceTx, [ReqIdx] );
      except
      end;
    end;

  procedure TVoiceHandler.threadedVoiceTxOver( const parms : array of const );
    var
      ErrorCode : TErrorCode;
    begin
      try
        fClientView.VoiceTxOver( ErrorCode );
      except
      end;
    end;

  procedure TVoiceHandler.threadedCancelVoiceReq( const parms : array of const );
    var
      ErrorCode : TErrorCode;
    begin
      try
        fClientView.CancelVoiceRequest( ErrorCode );
      except
      end;
    end;

  procedure TVoiceHandler.threadedVoiceStatusChange( const parms : array of const );
    var
      ErrorCode : TErrorCode;
    begin
      try
        fClientView.VoiceStatusChanged( parms[0].vInteger, ErrorCode );
      except
      end;
    end;

  procedure TVoiceHandler.syncRequestVoiceTx( const parms : array of const );
    var
      ReqIdx : integer absolute parms[0].vInteger;
    begin
      if ReqIdx = 0
        then fMasterURLHandler.HandleEvent( evnVoiceMsgAuthorized, self );
      fVoicePanel.BufferGauge.Position := (100*min(4, ReqIdx)) div 4;
    end;

end.




