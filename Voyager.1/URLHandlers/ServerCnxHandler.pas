unit ServerCnxHandler;

interface

  uses
    VoyagerInterfaces, VoyagerServerInterfaces, Events, Protocol, Windows, Controls, WinSockRDOConnection,
    RDOServer, RDOInterfaces, Classes, Collection, Matrix, SyncObjs, GMKernel;

  const
    NetErrorsTimesOut = 2;

  const
    regKindPart = 0;
    regKindFull = 1;

  const
    ISTimeout = 20000;

  type
    TLogonError =
      ( errCannotConnect,
        errInvalidName,
        errInvalidPassword,
        errCannotCreateClientView,
        errCannotRegisterEvents,
        errRequestDenied,
        errUnknown,
        errNone );

  type
    TCachedRegion =
      class
        public
          Area    : TRect;
          Kind    : integer;
          Objects : string;
          Roads   : string;
          Rails   : string;
          Time    : TDateTime;
        public
          constructor Create( anArea : TRect; Info : string; aTime : TDateTime );
      end;

    TServerCnxHandler =
      class( TInterfacedObject, IMetaURLHandler, IURLHandler, IClientView )
        public
          constructor Create;
          destructor  Destroy; override;
        // IMetaURLHandler
        private
          function getName : string;
          function getOptions : TURLHandlerOptions;
          function getCanHandleURL( URL : TURL ) : THandlingAbility;
          function Instantiate : IURLHandler;
        // IURLHandler
        private
          function  HandleURL( URL : TURL ) : TURLHandlingResult;
          function  HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
          function  getControl : TControl;
          procedure setMasterURLHandler( URLHandler : IMasterURLHandler );
        private
          fMasterURLHandler : IMasterURLHandler;
        // IClientView
        private
          procedure SetViewedArea( x, y, dx, dy : integer; out ErrorCode : TErrorCode );
          function  ObjectsInArea( x, y, dx, dy : integer; out ErrorCode : TErrorCode ) : TObjectReport;
          function  ObjectAt( x, y : integer; out ErrorCode : TErrorCode ) : TObjId;
          function  ObjectStatusText( kind : TStatusKind; Id : TObjId; out ErrorCode : TErrorCode ) : TStatusText;
          function  ContextText : string;
          function  ObjectConnections( Id : TObjId; out ErrorCode : TErrorCode ) : TCnxReport;
          procedure FocusObject( Id : TObjId; out ErrorCode : TErrorCode );
          procedure UnfocusObject( Id : TObjId; out ErrorCode : TErrorCode );
          function  SwitchFocus( From : TObjId; toX, toY : integer; out ErrorCode : TErrorCode ) : TObjId;
          function  GetCompanyList( out ErrorCode : TErrorCode ) : TCompanyReport;
          function  NewCompany( name, cluster : string; out ErrorCode : TErrorCode ) : TCompanyInfo;
          procedure NewFacility( FacilityId : string; CompanyId : integer; x, y : integer; out ErrorCode : TErrorCode );
          function  ConnectFacilities( Facility1, Facility2 : TObjId; out ErrorCode : TErrorCode ) : string;
          function  GetUserList( out ErrorCode : TErrorCode ) : TStringList;
          function  GetChannelList( out ErrorCode : TErrorCode ) : TStringList;
          procedure SayThis( Dest, Msg : string; out ErrorCode : TErrorCode );
          procedure VoiceThis( const Buffer : array of byte; len, TxId, NewTx : integer; out ErrorCode : TErrorCode );
          function  VoiceRequest( out ErrorCode : TErrorCode ) : integer;
          procedure CancelVoiceRequest( out ErrorCode : TErrorCode );
          procedure VoiceTxOver( out ErrorCode : TErrorCode );
          procedure VoiceStatusChanged( Status : integer; out ErrorCode : TErrorCode );
          procedure CreateChannel( ChannelName, Password, aSessionApp, aSessionAppId : string; anUserLimit : integer; out ErrorCode : TErrorCode );
          procedure JoinChannel( ChannelName, Password : string; out ErrorCode : TErrorCode );
          function  GetChannelInfo( ChannelName : string; out ErrorCode : TErrorCode ) : string;
          procedure MsgCompositionChanged( State : TMsgCompositionState; out ErrorCode : TErrorCode );
          procedure Chase( UserName : string; out ErrorCode : TErrorCode );
          procedure StopChase( out ErrorCode : TErrorCode );
          procedure EnableEvents( out ErrorCode : TErrorCode );
          procedure DisableEvents( out ErrorCode : TErrorCode );
          procedure Logoff( out ErrorCode : TErrorCode );
          procedure CreateCircuitSeg( CircuitId, OwnerId, x1, y1, x2, y2, cost : integer; out ErrorCode : TErrorCode );
          procedure BreakCircuitAt( CircuitId, OwnerId, x, y : integer; out ErrorCode : TErrorCode );
          procedure WipeCircuit( CircuitId, OwnerId, x1, y1, x2, y2 : integer; out ErrorCode : TErrorCode );
          function  SegmentsInArea( CircuitId, x, y, dx, dy : integer; out ErrorCode : TErrorCode ) : TSegmentReport;
          function  GetSurface( SurfaceId : string; x, y, dx, dy : integer; out ErrorCode : TErrorCode ) : IMatrix;
          procedure DefineZone( TycoonId, ZoneId, x1, y1, x2, y2 : integer; out ErrorCode : TErrorCode );
          function  GetCookie( CookieId : string; out ErrorCode : TErrorCode ) : string;
          procedure SetCookie( CookieId, ValueId : string; out ErrorCode : TErrorCode );
          procedure CloneFacility( x, y : integer; LimitToTown, LimitToCompany : boolean );
          procedure GetNearestTownHall( x, y : integer; out xTown, yTown : integer ; out ErrorCode : TErrorCode );
          function  PickEvent : TStringList;
          function  Echo( value : integer ) : integer;
          procedure ClientAware;
          procedure ClientNotAware;
          procedure SetLanguage( langid : widestring );
          function  getUserName : string;
          function  getUserMasterName : string;
          function  getUserPassword : string;
          function  getMoney : currency;
          function  getCompanyName : string;
          function  getCompanyId  : TCompanyId;
          function  getTycoonId : TObjId;
          function  getTycoonUId : integer;
          function  getDate : TDateTime;
          function  getSeason : integer;
          function  getWorldName : string;
          function  getWorldURL : string;
          function  getWorldAbsURL : string;
          function  getWorldXSize : integer;
          function  getWorldYSize : integer;
          function  getDAAddr : string;
          function  getDAPort : integer;
          function  getDALockPort : integer;
          function  getISAddr : string;
          function  getISPort : integer;
          function  getMailAddr : string;
          function  getMailPort : integer;
          function  getMailAccount : string;
          function  getCacheAddr : string;
          function  getCachePort : integer;
          function  getSecurityId : string;
          function  getClientViewId : TObjId;
          function  getChasedUser : string;
          procedure SuplantedBy( NewClientView : IClientView );
          procedure DisposeObjectReport( var ObjectReport : TObjectReport );
          procedure DisposeSegmentReport( var SegmentReport : TSegmentReport );
          procedure DisposeCnxReport( var CnxReport : TCnxReport );
          procedure SetAutologon( active : boolean );
          procedure StoreLastConfig;
        private
          fUserName    : string;
          fMasterUserName : string;
          fPassword    : string;
          fWorldName   : string;
          fWorldArea   : string;
          fCompanyName : string;
          fCompanyId   : integer;
          fMoney       : currency;
          fDate        : TDateTime;
          fSeason      : integer;
          fWorldURL    : string;
          fWorldAbsURL : string;
          fWorldSize   : TPoint;
          fISAddr      : string;
          fISPort      : integer;
          fDAAddr      : string;
          fDAPort      : integer;
          fDSAddr      : string;
          fDSPort      : integer;
          fTycoonId    : integer;
          fTycoonUId   : integer;
          fMailAddr    : string;
          fMailPort    : integer;
          fMailAccount : string;
          fChasedUser  : string;
          fSuplanted   : boolean;
          fServerLock  : TCriticalSection;
          fLockCount   : integer;
          fEventsLock  : TCriticalSection;
          fViewCenter  : TPoint;
          fCachedRegs  : TLockableCollection;
        // Connection objects
        private
          fISCnx              : IRDOConnectionInit;
          fWSISCnx            : TWinSockRDOConnection;
          fISProxy            : OleVariant;
          fClientViewId       : TObjId;
          //fClientEventsCnx    : IRDOConnectionsServer;
          fClientEventsServer : TRDOServer;
          fClientEvents       : TObject;
        // IS Events handling
        private
          syncDirtyArea       : TLockableCollection;
          syncDirtyObject     : integer;
          syncKindOfChange    : integer;
          syncMoney           : currency;
          syncNetProfit       : currency;
          syncRanking         : integer;
          syncFacCount        : integer;
          syncFacMax          : integer;
          syncCompanionship   : string;
          syncMoveToPoint     : TPoint;
          syncFailureLevel    : integer;
          syncDate            : TDateTime;
          syncChatFrom        : string;
          syncChatText        : string;
          syncVoiceFrom       : string;
          syncTxId            : integer;
          syncNewTx           : boolean;
          syncVoiceData       : string;
          syncMsgCount        : integer;
          syncUserName        : string;
          syncUserChange      : TUserListChange;
          syncChannelName     : string;
          syncChannelPassword : string;
          syncChannelChange   : TUserListChange;
          syncMsgCompUserName : string;
          syncMsgCompState    : TMsgCompositionState;
          syncActorPoolId     : integer;
          syncActorPoolData   : TStream;
          syncNotfKind        : integer;
          syncNotfTitle       : string;
          syncNotfBody        : string;
          syncNotfOptions     : integer;
          syncExtraInfo       : string;
          syncSeason          : integer;
          fCreateEventsServerEvent : TEvent;
        private
          procedure threadedSayThis( const parms : array of const );
          procedure threadedMsgCompositionChanged( const parms : array of const );
          function  Logon( URL : string ) : TURLHandlingResult;
          procedure threadedLogon( const parms : array of const );
          procedure syncLogon( const parms : array of const );
          procedure syncCreateEventsServer( const parms : array of const );
        private
          procedure syncRefreshArea( const parms : array of const );
          procedure syncRefreshObject( const parms : array of const );
          procedure syncRefreshTycoon( const parms : array of const );
          procedure syncRefreshDate( const parms : array of const );
          procedure syncRefreshSeason( const parms : array of const );
          procedure syncEndOfPeriod( const parms : array of const );
          procedure syncTycoonRetired( const parms : array of const );
          procedure syncChatMsg( const parms : array of const );
          procedure syncVoiceMsg( const parms : array of const );
          procedure syncVoiceRequestGranted( const parms : array of const );
          procedure syncNewMail( const parms : array of const );
          procedure syncMoveTo( const parms : array of const );
          procedure syncNotifyCompanionship( const parms : array of const );
          procedure syncNotifyUserListChange( const parms : array of const );
          procedure syncNotifyChannelListChange( const parms : array of const );
          procedure syncNotifyChannelChange( const parms : array of const );
          procedure syncNotifyMsgCompositionState( const parms : array of const );
          procedure syncActorPoolModified( const parms : array of const );
          procedure syncShowNotification( const parms : array of const );
          procedure syncGameMasterMsg( const parms : array of const );
          procedure syncGMNotify( const parms : array of const );
        private
          //function ConfirmPassword( var Password : string ) : boolean;
        private
          fOffline         : boolean;
          fNetErrors       : integer;
          fReconnectThread : TThread;
        private
          procedure ReportCnxValid;
          procedure ReportCnxFailure;
          procedure ConnectionDropped;
          function  Reconnect( AccountName : string; Silent, IgnoreErrors : boolean ) : boolean;
          procedure syncReconnectChatMsg( const parms : array of const );
          procedure OnSocketDisconnect( const ClientConnection : IRDOConnection );
          //procedure OnReconnectThreadTerminate( Sender : TObject );
        private
          fCachedObjectId  : TObjId;
          fObjectTextCache : array[TStatusKind] of string;
          fTextCacheTTL    : TDateTime;
          fLastObjX        : integer;
          fLastObjY        : integer;
        private
          function  ObjectTextCacheValid( Id : TObjId ) : boolean;
          procedure CacheObjectText( Id : TObjId; Text : string );
          function  GetCachedRegion( x, y, dx, dy : integer ) : TCachedRegion;
          procedure Lock;
          procedure Unlock;
        private
          function  Offline : boolean;
        // GM stuff
        private
          function  ConnectToGameMaster( ClientId : TCustomerId; ClientInfo : widestring; GameMasters : widestring ) : OleVariant;
          function  SendGMMessage( ClientId : TCustomerId; GMId : TGameMasterId; Msg : WideString ) : OleVariant;
          procedure DisconnectUser( ClientId : TCustomerId; GMId : TGameMasterId );
      end;

  const
    tidMetaHandlerName_ServerCnx = 'CnxHandler';

  const
    htmlCnxHandlerProtocol        = 'cnx://';
    htmlAction_AutoLogon          = 'AUTOLOGON';
    htmlAction_Logon              = 'LOGON';
    htmlAction_Logoff             = 'LOGOFF';
    htmlAction_SetCompany         = 'SETCOMPANY';
    htmlAction_CreateCompany      = 'CREATECOMPANY';
    htmlParmName_ISAddr           = 'ISAddr';
    htmlParmName_ISPort           = 'ISPort';
    htmlParmName_DSAddr           = 'DSAddr';
    htmlParmName_DSPort           = 'DSPort';
    htmlParmName_UserName         = 'UserName';
    htmlParmName_Password         = 'Password';
    htmlParmName_Silent           = 'Silent';
    htmlParmName_Threaded         = 'Threaded';
    htmlParmName_IgnoreErrors     = 'IgnoreErrors';
    htmlParmName_ResultURL        = 'ResultURL';
    htmlParmName_CompanyName      = 'Name';
    htmlParmName_CompanyOwnerRole = 'OwnerRole';
    htmlParmName_CompanyId        = 'Id';

  const
    ISProxyTimeOut = 3*60*1000;
    LogoffTimeOut  = 5000;


  function ScramblePassword( const Password : string ) : string;

implementation

  uses
    URLParser, SmartThreads, SysUtils, WinSockRDOConnectionsServer, RDOObjectProxy,
    Forms, ConfirmPasswordDialog, StrUtils, ServerCnxEvents, StreamToStr, MapCompress,
    Config, Notification, CompStringsParser, Threads, ConnectingWindow, MessageBox,
    Literals, ClientMLS;

  type
    PSingleArray = ^TSingleArray;
    TSingleArray = array[0..0] of single;

  const
    ScrambleMask = $37;

  function ScramblePassword( const Password : string ) : string;
    var
      i : integer;
    begin
      result := '';
      for i := 1 to length(Password) do
        result := result + char(byte(Password[i]) xor ScrambleMask);
    end;

  type
    TTestMatrix =
      class( TInterfacedObject, IMatrix )
        private
          destructor Destroy; override;
        private
          fRows, fCols : integer;
          fItems       : PSingleArray;
        private
          function  getCols : integer;
          function  getRows : integer;
          procedure setDimensions( n, m : integer );
          function  getElement   ( i, j : integer ) : single;
          procedure setElement   ( i, j : integer; value : single );
        private
          function  CheckIndexes(i, j : integer) : boolean;
      end;

    destructor TTestMatrix.Destroy;
      begin
        if fItems <> nil
          then freemem( fItems, fRows*fCols*sizeof(single) );
        inherited;
      end;

    function TTestMatrix.getCols : integer;
      begin
        result := fCols;
      end;

    function TTestMatrix.getRows : integer;
      begin
        result := fRows;
      end;

    procedure TTestMatrix.setDimensions( n, m : integer );
      begin
        if fItems <> nil
          then freemem( fItems, fRows*fCols*sizeof(single) );
        fCols := m;
        fRows := n;
        getmem( fItems, fRows*fCols*sizeof(single) );
        fillchar( fItems^, fRows*fCols*sizeof(single), 0 );
      end;

    function TTestMatrix.getElement( i, j : integer ) : single;
      begin
        if CheckIndexes(i, j)
          then result := fItems[i*fCols + j]
          else result := 0;
      end;

    procedure TTestMatrix.setElement( i, j : integer; value : single );
      begin
        if CheckIndexes(i, j)
          then fItems[i*fCols + j] := value;
      end;

    function TTestMatrix.CheckIndexes(i, j : integer) : boolean;
      begin
        Result := (i >= 0) and (j >= 0) and (i < fRows) and (j < fCols);
      end;

  type
    TISCompanyReport =
      class( TCompanyReport )
        protected
          constructor Create( aTextualReport : string );
      end;

    constructor TISCompanyReport.Create( aTextualReport : string );
      var
        CompanyInfo : TCompanyInfo;
      begin
        inherited Create;
        repeat
          CompanyInfo := ParseCompanyInfo( aTextualReport );
          if CompanyInfo <> nil
            then CompanyList.Insert( CompanyInfo );
        until CompanyInfo = nil;
      end;

  type
    TRefreshAreaRequest =
      class
        public
          Area      : TRect;
          ExtraInfo : string;
        public
          constructor Create( anArea : TRect; anExtraInfo : string );
      end;

    constructor TRefreshAreaRequest.Create( anArea : TRect; anExtraInfo : string );
      begin
        Area      := anArea;
        ExtraInfo := anExtraInfo;
      end;

  type
    {$M+}
    TISEvents =
      class( TInterfacedObject, IRDOInterfaceServerEvents, IGMCustomer ) 
        public
          constructor Create( aServerCnxHandler : TServerCnxHandler );
        private
          fServerCnxHandler : TServerCnxHandler;
        published
          procedure InitClient( Date : TDateTime; Money : widestring; FailureLevel, TycoonId : integer );
        published
          procedure RefreshArea( x, y, dx, dy : integer; ExtraInfo : widestring );
          procedure RefreshObject( ObjId, KindOfChange : integer; ExtraInfo : widestring );
          procedure RefreshTycoon( Money, NetProfit : widestring; Ranking, FacCount, FacMax : integer );
          procedure RefreshDate( Date : TDateTime );
          procedure RefreshSeason( Season : integer );
          procedure EndOfPeriod( FailureLevel : integer );
          procedure TycoonRetired( FailureLevel : integer );
          procedure ChatMsg( From, Msg : widestring );
          procedure VoiceMsg( From, Msg : widestring; TxId, NewTx : integer );
          procedure VoiceRequestGranted( RequestId : integer );
          procedure NewMail( MsgCount : integer );
          procedure MoveTo( x, y : integer );
          procedure NotifyCompanionship( Names : widestring );
          procedure NotifyUserListChange( Name : widestring; Change : TUserListChange ); 
          procedure NotifyChannelListChange( Name, Password : widestring; Change : TUserListChange ); 
          procedure NotifyChannelChange( Name : widestring );
          procedure NotifyMsgCompositionState( Name : widestring; State : TMsgCompositionState );
          procedure ActorPoolModified( ActorPoolId : integer; Data : widestring );
          procedure ShowNotification( Kind : integer; Title, Body : widestring; Options : integer );
          procedure ModelStatusChanged( Status : integer );
          function  AnswerStatus : olevariant;
        // IGMCustomer
        published
          procedure GameMasterMsg( Msg : WideString; Info : integer );
          procedure GMNotify( notID : integer; Info : WideString );
      end;
    {$M-}

    constructor TISEvents.Create( aServerCnxHandler : TServerCnxHandler );
      begin
        inherited Create;
        fServerCnxHandler := aServerCnxHandler;
      end;

    procedure TISEvents.InitClient( Date : TDateTime; Money : widestring; FailureLevel, TycoonId : integer );
      begin
        fServerCnxHandler.fTycoonId := TycoonId;
        fServerCnxHandler.syncMoney := StrToCurr( Money );
        Join( fServerCnxHandler.syncRefreshTycoon, [0] );
        fServerCnxHandler.syncDate := Date;
        Join( fServerCnxHandler.syncRefreshDate, [0] );
        fServerCnxHandler.syncFailureLevel := FailureLevel;
        Join( fServerCnxHandler.syncEndOfPeriod, [0] );
      end;

    procedure TISEvents.RefreshArea( x, y, dx, dy : integer; ExtraInfo : widestring );
      begin
        fServerCnxHandler.syncDirtyArea.Insert( TRefreshAreaRequest.Create( Rect(x, y, x + dx, y + dy), ExtraInfo ));
        Join( fServerCnxHandler.syncRefreshArea, [0] );
      end;

    procedure TISEvents.RefreshObject( ObjId, KindOfChange : integer; ExtraInfo : widestring );
      begin
        fServerCnxHandler.syncDirtyObject  := ObjId;
        fServerCnxHandler.syncKindOfChange := KindOfChange;
        fServerCnxHandler.syncExtraInfo    := ExtraInfo;
        Join( fServerCnxHandler.syncRefreshObject, [0] );
      end;

    procedure TISEvents.RefreshTycoon( Money, NetProfit : widestring; Ranking, FacCount, FacMax : integer );
      begin
        fServerCnxHandler.syncMoney     := StrToCurr(Money);
        fServerCnxHandler.syncNetProfit := StrToCurr(NetProfit);
        fServerCnxHandler.syncRanking   := Ranking;
        fServerCnxHandler.syncFacCount  := FacCount;
        fServerCnxHandler.syncFacMax    := FacMax;
        Join( fServerCnxHandler.syncRefreshTycoon, [0] );
      end;

    procedure TISEvents.RefreshDate( Date : TDateTime );
      begin
        fServerCnxHandler.syncDate := Date;
        Join( fServerCnxHandler.syncRefreshDate, [0] );
      end;

    procedure TISEvents.RefreshSeason( Season : integer );
      begin
        fServerCnxHandler.syncSeason := Season;
        Join( fServerCnxHandler.syncRefreshSeason, [0] );
      end;

    procedure TISEvents.EndOfPeriod( FailureLevel : integer );
      begin
        fServerCnxHandler.syncFailureLevel := FailureLevel;
        Join( fServerCnxHandler.syncEndOfPeriod, [0] );
      end;

    procedure TISEvents.TycoonRetired( FailureLevel : integer );
      begin
        fServerCnxHandler.syncFailureLevel := FailureLevel;
        Join( fServerCnxHandler.syncTycoonRetired, [0] );
      end;

    procedure TISEvents.ChatMsg( From, Msg : widestring );
      begin
        fServerCnxHandler.syncChatFrom := From;
        fServerCnxHandler.syncChatText := Msg;
        //Join( fServerCnxHandler.syncChatMsg );
        Join( fServerCnxHandler.syncChatMsg, [0] );
      end;

    procedure TISEvents.VoiceMsg( From, Msg : widestring; TxId, NewTx : integer );
      begin
        fServerCnxHandler.syncVoiceFrom := From;
        fServerCnxHandler.syncTxId      := TxId;
        fServerCnxHandler.syncNewTx     := NewTx <> 0;
        fServerCnxHandler.syncVoiceData := Msg;
        Join( fServerCnxHandler.syncVoiceMsg, [0] );
      end;

    procedure TISEvents.VoiceRequestGranted( RequestId : integer );
      begin
        Join( fServerCnxHandler.syncVoiceRequestGranted, [0] );
      end;

    procedure TISEvents.NewMail( MsgCount : integer );
      begin
        fServerCnxHandler.syncMsgCount := MsgCount;
        Join( fServerCnxHandler.syncNewMail, [0] );
      end;

    procedure TISEvents.MoveTo( x, y : integer );
      begin
        fServerCnxHandler.syncMoveToPoint.x := x;
        fServerCnxHandler.syncMoveToPoint.y := y;
        Join( fServerCnxHandler.syncMoveTo, [0] );
      end;

    procedure TISEvents.NotifyCompanionship( Names : widestring );
      begin
        fServerCnxHandler.syncCompanionship := Names;
        Join( fServerCnxHandler.syncNotifyCompanionship, [0] );
      end;

    procedure TISEvents.NotifyUserListChange( Name : widestring; Change : TUserListChange );
      begin
        fServerCnxHandler.syncUserName   := Name;
        fServerCnxHandler.syncUserChange := Change;
        Join( fServerCnxHandler.syncNotifyUserListChange, [0] );
      end;

    procedure TISEvents.NotifyChannelListChange( Name, Password : widestring; Change : TUserListChange );
      begin
        fServerCnxHandler.syncChannelName     := Name;
        fServerCnxHandler.syncChannelPassword := Password;
        fServerCnxHandler.syncChannelChange   := Change;
        Join( fServerCnxHandler.syncNotifyChannelListChange, [0] );
      end;

    procedure TISEvents.NotifyChannelChange( Name : widestring );
      begin
        fServerCnxHandler.syncChannelName := Name;
        Join( fServerCnxHandler.syncNotifyChannelChange, [0] );
      end;

    procedure TISEvents.NotifyMsgCompositionState( Name : widestring; State : TMsgCompositionState );
      begin
        fServerCnxHandler.syncMsgCompUserName := Name;
        fServerCnxHandler.syncMsgCompState    := State;
        Join( fServerCnxHandler.syncNotifyMsgCompositionState, [0] );
      end;

    procedure TISEvents.ActorPoolModified( ActorPoolId : integer; Data : widestring );
      begin
        fServerCnxHandler.syncActorPoolId   := ActorPoolId;
        fServerCnxHandler.syncActorPoolData := TMemoryStream.Create;
        StringToStream( Data, fServerCnxHandler.syncActorPoolData );
        fServerCnxHandler.syncActorPoolData.Position := 0;
        Join( fServerCnxHandler.syncActorPoolModified, [0] );
      end;

    procedure TISEvents.ShowNotification( Kind : integer; Title, Body : widestring; Options : integer );
      begin
        fServerCnxHandler.syncNotfKind    := Kind;
        fServerCnxHandler.syncNotfTitle   := Title;
        fServerCnxHandler.syncNotfBody    := Body;
        fServerCnxHandler.syncNotfOptions := Options;
        Join( fServerCnxHandler.syncShowNotification, [0] );
      end;

    procedure TISEvents.ModelStatusChanged( Status : integer );
      begin
      end;

    function TISEvents.AnswerStatus : olevariant;
      begin
        result := NOERROR;
      end;

    procedure TISEvents.GameMasterMsg( Msg : WideString; Info : integer );
      begin
        Join( fServerCnxHandler.syncGameMasterMsg, [string(Msg), info] );
      end;

    procedure TISEvents.GMNotify( notID : integer; Info : WideString );
      begin
        Join( fServerCnxHandler.syncGMNotify, [notID, info] );
      end;


  // TReconnectThread

  type
    TReconnectThread =
      class( TThread )
        public
          constructor Create( aServerCnxHandler : TServerCnxHandler );
        protected
          procedure Execute; override;
        private
          fServerCnxHandler : TServerCnxHandler;
          fSuccess          : boolean;
      end;

    constructor TReconnectThread.Create( aServerCnxHandler : TServerCnxHandler );
      begin
        inherited Create( true );
        FreeOnTerminate   := true;
        fServerCnxHandler := aServerCnxHandler;
        Resume;
      end;

    procedure TReconnectThread.Execute;
      begin
        while not Terminated and not fSuccess do
          fSuccess := fServerCnxHandler.Reconnect( fServerCnxHandler.fUserName, true, true );
      end;


  const
    MaxTextCacheTTL : TDateTime = 0;
    MaxRegionTTL    : TDateTime = 0;


  // TCachedRegion

  constructor TCachedRegion.Create( anArea : TRect; Info : string; aTime : TDateTime );

    function GetNextStr( str : string; var pos : integer; StopChar : char ) : string;
      begin
        result := '';
        while (pos < length(str)) and (str[pos] <> StopChar) do
          begin
            result := result + str[pos];
            inc( pos );
          end;
        if str[pos] = StopChar
          then inc( pos );
      end;

    var
      p : integer;
    begin
      inherited Create;
      try
        p       := 1;
        Kind    := StrToInt(GetNextStr( Info, p, RepSeparator ));
        if Kind = regKindFull
          then
            begin
              Objects := GetNextStr( Info, p, RepSeparator );
              Roads   := GetNextStr( Info, p, RepSeparator );
              Rails   := GetNextStr( Info, p, RepSeparator );
              Area    := anArea;
              {
              if (Objects <> '') and (Roads <> '')
                then
                  begin
                    Area.TopLeft.x     := MapChunkSize*(anArea.TopLeft.x div MapChunkSize);
                    Area.TopLeft.y     := MapChunkSize*(anArea.TopLeft.y div MapChunkSize);
                    Area.BottomRight.x := Area.TopLeft.x + MapChunkSize;
                    Area.BottomRight.y := Area.TopLeft.y + MapChunkSize;
                  end
                else Kind := 0;
              }
            end
          else Area := anArea;
      except
        Kind := 0;
      end;
      Time := aTime;
    end;


  // TServerCnxHandler

  constructor TServerCnxHandler.Create;
    begin
      inherited Create;
      fCachedRegs   := TLockableCollection.Create( 0, rkBelonguer );
      syncDirtyArea := TLockableCollection.Create( 0, rkBelonguer );
      fServerLock   := TCriticalSection.Create;
      fEventsLock   := TCriticalSection.Create;
      fClientEvents := TISEvents.Create( self );
      fISProxy      := Unassigned;
      fCreateEventsServerEvent := TEvent.Create( nil, true, false, '' );
    end;

  destructor TServerCnxHandler.Destroy;
    begin
      if fOffline
        then
          begin
            fReconnectThread.Free;
            fReconnectThread := nil;
          end;
      fISCnx := nil;
      fISProxy := Unassigned;
      fClientEvents.Free;
      fEventsLock.Free;
      fServerLock.Free;
      syncDirtyArea.Free;
      fCachedRegs.Free;
      fCreateEventsServerEvent.Free;
      inherited;
    end;

  function TServerCnxHandler.getName : string;
    begin
      result := tidMetaHandlerName_ServerCnx;
    end;

  function TServerCnxHandler.getOptions : TURLHandlerOptions;
    begin
      result := [hopNonvisual];
    end;

  function TServerCnxHandler.getCanHandleURL( URL : TURL ) : THandlingAbility;
    begin
      result := 0;
    end;

  function TServerCnxHandler.Instantiate : IURLHandler;
    begin
      result := self;
    end;

  function TServerCnxHandler.HandleURL( URL : TURL ) : TURLHandlingResult;

    {
    var
      Silent       : boolean;
      IgnoreErrors : boolean;
    }
    (*
    procedure ShowError( Viewer : TURL; Target : string; Error : TLogonError );
      const
        htmlValue_LogonError : array[TLogonError] of string =
          ( 'ERROR_CANNOTCONNECT',
            'ERROR_INVALIDNAME',
            'ERROR_INVALIDPASSWORD',
            'ERROR_CANNOTCREATECLIENTVIEW',
            'ERROR_CANNOTREGISTEREVENTS',
            'ERROR_REQUESTDENIED',
            'ERROR_UNKNOWN' );
      var
        URL : TURL;
        Msg : string;
      begin
        if not IgnoreErrors
          then
            begin
              if Viewer <> ''
                then
                  begin
                    URL :=
                      Viewer + '?' +
                      'ResultType=ERROR&' +
                      'frame_Id=LogonView&' +
                      'frame_Class=HTMLView&' +
                      'frame_Target=' + Target + '&' +
                      'Logon=FALSE&' +
                      'frame_Maximized=Yes&' +
                      'ErrorCode=' + htmlValue_LogonError[Error];
                    fMasterURLHandler.HandleURL( URL );
                  end
                else
                  begin
                    case Error of
                      errCannotConnect :
                        Msg := 'Could not establish connection with servers.';
                      errInvalidName :
                        Msg := 'You supplied an invalid name.';
                      errInvalidPassword :
                        Msg := 'You supplied an invalid password.';
                      errCannotCreateClientView :
                        Msg :=
                          'There was an error trying to connect to server. ' +
                          'Please be sure that you have an active connection to the Internet.';
                      errCannotRegisterEvents :
                        Msg :=
                          'There was an error trying to connect to server. ' +
                          'Please be sure that you have an active connection to the Internet.';
                      errRequestDenied :
                        Msg :=
                          'There was an error trying to connect to server. ' +
                          'Your request was denied.';
                      else
                        Msg :=
                          'An unknown error has ocurred. Please report to Oceanus if this ' +
                          'happens too often.';
                    end;
                    Application.MessageBox( pchar(Msg), 'Error!', MB_ICONERROR or MB_OK );
                  end;
            end;
        raise Exception.Create( '' );
      end;
    *)

    function Logoff : TURLHandlingResult;
      var
        ErrorCode : TErrorCode;
      begin
        try
          try
            if fWSISCnx <> nil
              then
                begin
                  fWSISCnx.SetOnDisconnect( nil );
                  ErrorCode := fISProxy.Logoff;
                end;
          except
            ErrorCode := Voyager_ERROR_Unknown;
          end
        finally
          fISProxy := Unassigned;
          if fMasterURLHandler <> nil
            then fMasterURLHandler.HandleEvent( evnLogoff, ErrorCode );
          result := urlHandled;
        end
      end;

    {
    function AutoLogon : TURLHandlingResult;
      var
        ConfigHolder : IConfigHolder;
        LastUser     : string;
        CompanyInfo  : TSetCompanyInfo;
      begin
        fMasterURLHandler.HandleEvent( evnAnswerConfigHolder, ConfigHolder );
        if ConfigHolder.ReadBoolean( true, '', 'AutoLogon', false )
          then
            begin
              LastUser := ConfigHolder.ReadString( true, '', 'LastUser', '' );
              ConfigHolder.RetrieveUserList( QuickLogonDlg.UserName.Items );
              if QuickLogonDlg.UserName.Items.Count > 0
                then
                  begin
                    QuickLogonDlg.UserName.ItemIndex := QuickLogonDlg.UserName.Items.IndexOf( LastUser );
                    QuickLogonDlg.RememberPassword.Checked := ConfigHolder.ReadBoolean( true, '', 'RememberPasswords', false );
                    QuickLogonDlg.Password.Enabled := not QuickLogonDlg.RememberPassword.Checked;
                    if QuickLogonDlg.ShowModal = mrOk
                      then
                        begin
                          Application.ProcessMessages;
                          //fUserName := QuickLogonDlg.UserName.Text;
                          fUserName := ConfigHolder.ReadString( false, QuickLogonDlg.UserName.Text, 'UserName', QuickLogonDlg.UserName.Text );
                          if QuickLogonDlg.Password.Enabled
                            then fPassword := QuickLogonDlg.Password.Text
                            else fPassword := ScramblePassword( ConfigHolder.ReadString( false, QuickLogonDlg.UserName.Text, 'Password', QuickLogonDlg.Password.Text ) );
                          ConfigHolder.WriteBoolean( true, '', 'RememberPasswords', QuickLogonDlg.RememberPassword.Checked );
                          fWorldName   := ConfigHolder.ReadString ( false, QuickLogonDlg.UserName.Text, 'World', '' );
                          fCompanyName := ConfigHolder.ReadString ( false, QuickLogonDlg.UserName.Text, 'Company', '' );
                          fCompanyId   := ConfigHolder.ReadInteger( false, QuickLogonDlg.UserName.Text, 'CompanyId', 0 );
                          fISAddr      := ConfigHolder.ReadString ( false, QuickLogonDlg.UserName.Text, 'ISAddr', '' );
                          fISPort      := ConfigHolder.ReadInteger( false, QuickLogonDlg.UserName.Text, 'ISPort', 0 );
                          if Reconnect( fUserName, true, false )
                            then
                              begin
                                CompanyInfo.Name := fCompanyName;
                                CompanyInfo.Id   := fCompanyId;
                                fMasterURLHandler.HandleEvent( evnSetCompany, CompanyInfo );
                                ConfigHolder.WriteString( true, '', 'LastUser', fUserName + ' - ' + fWorldName );
                                StoreLastConfig;
                                result := urlHandled;
                              end
                            else result := AutoLogon
                        end
                      else result := urlNotHandled;
                    ConfigHolder.WriteBoolean( true, '', 'AutoLogon', not QuickLogonDlg.DisableQuickLogon.Checked );
                  end
                else result := urlNotHandled;
            end
          else result := urlNotHandled;
      end;
    }

    (*
    function Logon : TURLHandlingResult;
      var
        // LogonSuccess  : TURL;
        // LogonFailure  : TURL;
        ResultURL     : TURL;
        ResultType    : string;
        Target        : string;
        AccountStatus : TErrorCode;
        ErrorCode     : TErrorCode;
        // EventsPort    : integer;
        ClientId      : integer;
      begin
        try
          begin
            ErrorCode := 0;
            try
              fISAddr      := URLParser.GetParmValue( URL, htmlParmName_ISAddr );
              fISPort      := StrToInt(URLParser.GetParmValue( URL, htmlParmName_ISPort ));
              fUserName    := URLParser.GetParmValue( URL, htmlParmName_UserName );
              fPassword    := URLParser.GetParmValue( URL, htmlParmName_Password );
              Silent       := StrToBoolean(URLParser.GetParmValue( URL, htmlParmName_Silent ));
              IgnoreErrors := StrToBoolean(URLParser.GetParmValue( URL, htmlParmName_IgnoreErrors ));
              ResultURL    := URLParser.GetParmValue( URL, htmlParmName_ResultURL );
              Target       := URLParser.GetParmValue( URL, htmlParmName_Target );
              // LogonSuccess := URLParser.GetParmValue( URL, htmlParmName_OnLogonSuccess );
              // LogonFailure := URLParser.GetParmValue( URL, htmlParmName_OnLogonFailure );
            except
              ErrorCode := ERROR_InvalidLogonData;
              raise;
            end;
            if (fISAddr <> '') and (fISPort > 0) and (fUserName <> '')
              then
                begin
                  if not VarIsEmpty(fISProxy) and not IgnoreErrors
                    then
                      try
                        fWSISCnx.SetOnDisconnect( nil );
                        fISProxy.WaitForAnswer := true;
                        fISProxy.TimeOut       := LogoffTimeOut;
                        ErrorCode              := fISProxy.Logoff;
                        fISProxy.WaitForAnswer := false;
                      except
                      end;
                  fWSISCnx      := TWinSockRDOConnection.Create;
                  fISCnx        := fWSISCnx;
                  fISCnx.Server := fISAddr;
                  fISCnx.Port   := fISPort;
                  fISProxy      := TRDOObjectProxy.Create as IDispatch;
                  try
                    if fISCnx.Connect( 20000 )
                      then
                        begin
                          fISProxy.SetConnection( fISCnx );
                          fISProxy.BindTo( tidRDOHook_InterfaceServer );
                          fWorldName    := fISProxy.WorldName;
                          fWorldURL     := fISProxy.WorldURL;
                          fDAAddr       := fISProxy.DAAddr;
                          fDAPort       := fISProxy.DALockPort;
                          fMailAddr     := fISProxy.MailAddr;
                          fMailPort     := fISProxy.MailPort;
                          fWorldSize.x  := fISProxy.WorldXSize;
                          fWorldSize.y  := fISProxy.WorldYSize;
                          fSeason       := fISProxy.WorldSeason;
                          fISProxy.TimeOut := ISProxyTimeOut;
                          AccountStatus    := fISProxy.AccountStatus( fUserName, fPassword );
                          case AccountStatus of
                            ACCOUNT_Valid, ACCOUNT_Unexisting :
                              if (AccountStatus <> ACCOUNT_Unexisting) or ConfirmPassword( fPassword )
                                then
                                  begin
                                    fClientViewId := fISProxy.Logon( fUserName, fPassword );
                                    if fClientViewId <> 0
                                      then
                                        try
                                          begin
                                            fISProxy.BindTo( fClientViewId );
                                            // If reconecting then get rid of the old events..
                                            fClientEventsServer.Free;
                                            fClientEventsServer := TRDOServer.Create( fISCnx as IRDOServerConnection, 1, nil );
                                            {
                                            EventsPort := 9000 + fISProxy.ClientViewId;
                                            fClientEventsCnx := TWinSockRDOConnectionsServer.Create( EventsPort );
                                            fClientEventsServer := TRDOServer.Create( fClientEventsCnx as IRDOServerConnection, 1, nil );
                                            fClientEventsCnx.StartListening;
                                            }
                                            try
                                              fMailAccount := fISProxy.MailAccount;
                                              fClientEventsServer.SetCriticalSection( fEventsLock );
                                              fClientEventsServer.RegisterObject( tidRDOHook_InterfaceEvents, integer(fClientEvents) );
                                              fTycoonUId := fISProxy.TycoonId;
                                              ClientId   := fISProxy.RDOCnntId;
                                              if fISProxy.RegisterEventsById( ClientId ) = NOERROR
                                              //if fISProxy.RegisterEvents( (fISCnx as IRDOConnection).LocalAddress, {EventsPort}(fISCnx as IRDOConnection).LocalPort ) = NOERROR
                                                then
                                                  begin
                                                    if not Silent
                                                      then
                                                        begin
                                                          if AccountStatus = ACCOUNT_Unexisting
                                                            then ResultType := 'NEWACCOUNT'
                                                            else ResultType := 'NORMAL';
                                                          fMasterURLHandler.HandleURL(
                                                            ResultURL +
                                                            '?frame_Id=LogonView&'+
                                                            'frame_Class=HTMLView&'+
                                                            {'frame_Target=' + Target + '&' +}
                                                            'frame_Align=client&' + 
                                                            'ResultType=' + ResultType + '&' +
                                                            'Logon=FALSE&' +
                                                            'frame_NoBorder=True&frame_NoScrollBars=true&' +
                                                            'ClientViewId=' + IntToStr(fClientViewId) + '&' +
                                                            'WorldName=' + fWorldName + '&' +
                                                            'ISAddr=' + fISAddr + '&' +
                                                            'ISPort=' + IntToStr(fISPort) );//+ '&' +
                                                            //'UserName=' + fUserName )
                                                        end;
                                                    fWSISCnx.SetOnDisconnect( OnSocketDisconnect );
                                                  end
                                                else
                                                  begin
                                                    if not Silent
                                                      then ShowError( ResultURL, Target, errCannotRegisterEvents );
                                                  end
                                            except
                                              fClientEventsServer.Free;
                                              ErrorCode := ERROR_CannotRegisterEvents;
                                              if not Silent
                                                then ShowError( ResultURL, Target, errCannotRegisterEvents );
                                            end;
                                          end
                                        except
                                          fWSISCnx.SetOnDisconnect( nil );
                                          ErrorCode := fISProxy.Logoff;
                                          raise;
                                        end
                                      else ShowError( ResultURL, Target, errCannotCreateClientView );
                                  end
                                else raise Exception.Create( 'Password was not confirmed' );
                            ACCOUNT_InvalidName :
                              ShowError( ResultURL, Target, errInvalidName );
                            ACCOUNT_InvalidPassword :
                              ShowError( ResultURL, Target, errInvalidPassword );
                            ACCOUNT_UnknownError :
                              ShowError( ResultURL, Target, errUnknown );
                          end;
                        end
                      else ShowError( ResultURL, Target, errCannotConnect );
                  except
                    fISCnx   := nil;
                    fISProxy := Unassigned;
                    raise;
                  end;
                end
              else
                begin
                  ErrorCode := ERROR_InvalidLogonData;
                  raise Exception.Create( 'Invalid Logon data.' );
                end;
            result := urlHandled;
          end
        except
          if (ErrorCode <> 0) and not Silent
            then fMasterURLHandler.HandleEvent( evnCriticalError, ErrorCode );
          result := urlNotHandled;
        end
      end;
    *)

    function SetCompany : TURLHandlingResult;
      var
        CompanyInfo : TSetCompanyInfo;
        OwnerRole   : string;
        ErrorCode   : TErrorCode;
      begin
        try
          OwnerRole := URLParser.GetParmValue( URL, htmlParmName_CompanyOwnerRole );
          if (CompareText( OwnerRole, fUserName ) <> 0) and (OwnerRole <> '')
            then Reconnect( OwnerRole, true, true );
          EnableEvents( ErrorCode );
          fCompanyName     := URLParser.GetParmValue( URL, htmlParmName_CompanyName );
          fCompanyId       := StrToInt(URLParser.GetParmValue( URL, htmlParmName_CompanyId ));
          CompanyInfo.Name := fCompanyName;
          CompanyInfo.Id   := fCompanyId;
          CompanyInfo.World := fWorldName;
          fMasterURLHandler.HandleEvent( evnSetCompany, CompanyInfo );
          result := urlHandled;
          StoreLastConfig;
        except
          result := urlNotHandled;
        end
      end;

    function CreateCompany : TURLHandlingResult;
      begin
        result := urlHandled;
      end;

    var
      Action : string;
    begin
      Lock;
      try
        Action := URLParser.GetURLAction( URL );
        if Action = htmlAction_AutoLogon
          then result := urlNotHandled//Autologon
          else
            if Action = htmlAction_Logon
              then
                begin
                  //result := Logon
                  if StrToBoolean(URLParser.GetParmValue( URL, htmlParmName_Threaded ))
                    then
                      begin
                        ConnectingWin.Left := Screen.Width - ConnectingWin.Width;
                        ConnectingWin.Top  := Screen.Height - ConnectingWin.Height;
                        ConnectingWin.MaxTimeout := ISTimeout;
                        ConnectingWin.Display( GetLiteral('Literal319') );
                        Fork( threadedLogon, priNormal, [URL] );
                        result := urlHandled;
                      end
                    else result := Logon( URL );
                end
              else
                if Action = htmlAction_Logoff
                  then result := Logoff
                  else
                    if Action = htmlAction_CreateCompany
                      then result := CreateCompany
                      else
                        if Action = htmlAction_SetCompany
                          then result := SetCompany
                          else result := urlNotHandled;
      finally
        Unlock;
      end;
    end;

  function TServerCnxHandler.HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
    var
      ClientView : IClientView absolute info;
    begin
      result := evnHandled;
      case EventId of
        evnAnswerClientView :
          if not fSuplanted
            then ClientView := self;
        evnShutDown :
          fMasterURLHandler := nil;
        evnLogonStarted :
          begin
            if fISCnx <> nil
              then fISCnx.Disconnect;
            {
            fISCnx := nil;
            if fReconnectThread <> nil
              then fReconnectThread.Terminate;
            }
            fOffLine := true;
          end;
      end;
    end;

  function TServerCnxHandler.getControl : TControl;
    begin
      result := nil;
    end;

  procedure TServerCnxHandler.setMasterURLHandler( URLHandler : IMasterURLHandler );
    begin
      fMasterURLHandler := URLHandler;
    end;

  procedure TServerCnxHandler.SetViewedArea( x, y, dx, dy : integer; out ErrorCode : TErrorCode );
    begin
      Lock;
      try
        try
          if not VarIsEmpty(fISProxy) and not fOffline
            then
              begin
                fISProxy.SetViewedArea( x, y, dx, dy );
                fViewCenter.x := x + dx div 2;
                fViewCenter.y := y + dy div 2;
                ReportCnxValid;
                ErrorCode := NOERROR;
              end
            else ErrorCode := ERROR_Unknown;
        except
          ErrorCode := ERROR_Unknown;
          ReportCnxFailure;
        end;
      finally
        Unlock;
      end;
    end;

  function TServerCnxHandler.ObjectsInArea( x, y, dx, dy : integer; out ErrorCode : TErrorCode ) : TObjectReport;
    const
      ItemsPerObject = 5;
    var
      CachedRegion : TCachedRegion;
      ServerReport : TStringList;
      i            : integer;
      info         : byte;
    begin
      Lock;
      try
        try
          if not VarIsEmpty(fISProxy) and not fOffline
            then
              begin
                ServerReport := TStringList.Create;
                try
                  CachedRegion := GetCachedRegion( x, y, dx, dy );
                  if (CachedRegion = nil) or (CachedRegion.Kind = regKindPart)
                    then ServerReport.Text := string(fISProxy.ObjectsInArea( x, y, dx, dy ))
                    else ServerReport.Text := CachedRegion.Objects;
                  if ServerReport.Count mod ItemsPerObject = 0
                    then
                      begin
                        result.ObjectCount := ServerReport.Count div ItemsPerObject;
                        getmem( result.Objects, result.ObjectCount*sizeof(result.Objects[0]) );
                        for i := 0 to pred(result.ObjectCount) do
                          with result.Objects[i] do
                            try
                              VisualClass := StrToInt(ServerReport[ItemsPerObject*i]);
                              CompanyId   := StrToInt(ServerReport[ItemsPerObject*i + 1]);
                              info        := StrToInt(ServerReport[ItemsPerObject*i + 2]);
                              Alert       := (info and $0F) <> 0;
                              x           := StrToInt(ServerReport[ItemsPerObject*i + 3]);
                              y           := StrToInt(ServerReport[ItemsPerObject*i + 4]);
                              Level       := info shr 4;
                            except
                            end;
                      end
                    else
                      begin
                        result.ObjectCount := 0;
                        result.Objects     := nil;
                      end;
                finally
                  ServerReport.Free;
                end;
                ErrorCode := NOERROR;
              end
            else
              begin
                result.ObjectCount := 0;
                result.Objects     := nil;
                ErrorCode          := ERROR_InvalidProxy;
              end;
          ReportCnxValid;
        except
          result.ObjectCount := 0;
          result.Objects     := nil;
          ErrorCode          := ERROR_Unknown;
          ReportCnxFailure;
        end;
      finally
        Unlock;
      end;
    end;

  function TServerCnxHandler.ObjectAt( x, y : integer; out ErrorCode : TErrorCode ) : TObjId;
    begin
      Lock;
      try
        try
          if not VarIsEmpty(fISProxy) and not fOffline
            then
              begin
                result    := TObjId(fISProxy.ObjectAt( x, y ));
                ErrorCode := NOERROR;
                ReportCnxValid;
              end
            else
              begin
                result    := 0;
                ErrorCode := ERROR_Unknown;
              end
        except
          result    := 0;
          ErrorCode := ERROR_Unknown;
          ReportCnxFailure;
        end;
      finally
        Unlock;
      end;
    end;

  function TServerCnxHandler.ObjectStatusText( kind : TStatusKind; Id : TObjId; out ErrorCode : TErrorCode ) : TStatusText;
    begin
      if Id <> 0
        then
          begin
            Lock;
            try
              try
                if not VarIsEmpty(fISProxy) and not fOffline
                  then
                    begin
                      if not ObjectTextCacheValid( Id )
                        then
                          begin
                            result := TStatusText(fISProxy.AllObjectStatusText( Id, fTycoonId ));
                            ReportCnxValid;
                            CacheObjectText( Id, result );
                          end;
                      result := fObjectTextCache[kind];
                      ErrorCode := NOERROR;
                    end
                  else
                    begin
                      result    := '';
                      ErrorCode := ERROR_Unknown;
                    end;
              except
                ErrorCode := ERROR_Unknown;
                ReportCnxFailure;
              end;
            finally
              Unlock;
            end;
          end
        else result := '';
    end;

  function TServerCnxHandler.ContextText : string;
    begin
      Lock;
      try
        if not VarIsEmpty(fISProxy) and not fOffline
          then result := string(fISProxy.ContextStatusText( fViewCenter.x, fViewCenter.y ))
          else result := '';
      finally                                                
        Unlock;
      end;                    
    end;
    
  function TServerCnxHandler.ObjectConnections( Id : TObjId; out ErrorCode : TErrorCode ) : TCnxReport;
    var
      ServerReport : TStringList;
      i, j         : integer;
      k            : integer;
      ctype        : integer;
    begin
      Lock;
      try
        try
          if not VarIsEmpty(fISProxy) and not fOffline
            then
              begin
                ServerReport := TStringList.Create;
                try
                  ServerReport.Text := string(fISProxy.ObjectConnections( Id ));
                  with Result do
                    begin
                      OutputCount := StrToInt( ServerReport[0] );
                      getmem( Outputs, OutputCount*sizeof(Outputs[0]) );
                      fillchar( Outputs^, OutputCount*sizeof(Outputs[0]), 0 );
                      k := 1;
                      for i := 0 to pred(OutputCount) do
                        with Outputs[i] do
                          begin
                            ConnCount := StrToInt( ServerReport[k] );
                            inc( k );
                            getmem( Connections, ConnCount*sizeof(Connections[0]) );
                            ctype := StrToInt( ServerReport[k] );
                            inc( k );
                            if ctype = 0
                              then Kind := okPeople
                              else Kind := okNormal;
                            for j := 0 to pred(ConnCount) do
                              with Connections[j] do
                                begin
                                  x := StrToInt( ServerReport[k] );
                                  inc( k );
                                  y := StrToInt( ServerReport[k] );
                                  inc( k );
                                end;
                          end;
                      InputCount := StrToInt( ServerReport[k] );
                      getmem( Inputs, InputCount*sizeof(Inputs[0]) );
                      fillchar( Inputs^, InputCount*sizeof(Inputs[0]), 0 );
                      inc( k );
                      for i := 0 to pred(InputCount) do
                        with Inputs[i] do
                          begin
                            ConnCount := StrToInt( ServerReport[k] );
                            inc( k );
                            getmem( Connections, ConnCount*sizeof(Connections[0]) );
                            ctype := StrToInt(ServerReport[k]);
                            inc( k );
                            case ctype of
                              0:
                                Kind := ikPeople;
                              1:
                                Kind := ikBasic
                              else
                                Kind := ikNonBasic
                            end;
                            for j := 0 to pred(ConnCount) do
                              with Connections[j] do
                                begin
                                  x := StrToInt(ServerReport[k]);
                                  inc( k );
                                  y := StrToInt(ServerReport[k]);
                                  inc( k );
                                end;
                          end;
                    end;
                  ErrorCode := NOERROR;
                  ReportCnxValid;
                finally
                  ServerReport.Free;
                end;
              end
            else
              begin
                result.OutputCount := 0;
                result.Outputs     := nil;
                result.InputCount  := 0;
                result.Inputs      := nil;
                ErrorCode := ERROR_Unknown;
              end;
        except
          ErrorCode := ERROR_Unknown;
          // >> ReportCnxFailure;
        end;
      finally
        Unlock;
      end;
    end;

  procedure TServerCnxHandler.FocusObject( Id : TObjId; out ErrorCode : TErrorCode );
    begin
      Lock;
      try
        try
          if not VarIsEmpty(fISProxy) and not fOffline
            then
              begin
                fISProxy.FocusObject( Id );
                ErrorCode := NOERROR;
                ReportCnxValid;
              end
            else ErrorCode := ERROR_Unknown;
        except
          ErrorCode := ERROR_Unknown;
          ReportCnxFailure;
        end;
      finally
        Unlock;
      end;
    end;

  procedure TServerCnxHandler.UnfocusObject( Id : TObjId; out ErrorCode : TErrorCode );
    begin
      Lock;
      try
        try
          if not VarIsEmpty(fISProxy) and not fOffline
            then
              begin
                fISProxy.UnfocusObject( Id );
                ErrorCode := NOERROR;
                ReportCnxValid;
              end
            else ErrorCode := ERROR_Unknown;
        except
          ErrorCode := ERROR_Unknown;
          ReportCnxFailure;
        end;
      finally
        Unlock;
      end;
    end;

  {
  function TServerCnxHandler.SwitchFocus( From : TObjId; toX, toY : integer; out ErrorCode : TErrorCode ) : TObjId;
    begin
      Lock;
      try
        try
          if not VarIsEmpty(fISProxy) and not fOffline
            then
              begin
                result    := TObjId(fISProxy.SwitchFocus( From, toX, toY ));
                ErrorCode := NOERROR;
                ReportCnxValid;
              end
            else
              begin
                result    := 0;
                ErrorCode := ERROR_Unknown;
              end;
        except
          result    := 0;
          ErrorCode := ERROR_Unknown;
          ReportCnxFailure;
        end
      finally
        Unlock;
      end;
    end;
  }
  function TServerCnxHandler.SwitchFocus( From : TObjId; toX, toY : integer; out ErrorCode : TErrorCode ) : TObjId;
    var
      ReportTxt : string;
      p         : integer;
      StrObjId  : string;
    begin
      if (toX <> fLastObjX) or (toY <> fLastObjY)
        then
          begin
            Lock;
            try
              try
                if not VarIsEmpty(fISProxy) and not fOffline
                  then
                    begin
                      ReportTxt   := string(fISProxy.SwitchFocusEx( From, toX, toY ));
                      p           := pos( LineBreak, ReportTxt, 1 );
                      StrObjId    := copy( ReportTxt, 1, p - 1 );
                      result      := TObjId(StrToInt(StrObjId));
                      ErrorCode   := NOERROR;
                      Delete( ReportTxt, 1, length(StrObjId + LineBreak) );
                      CacheObjectText( result, ReportTxt );
                      fLastObjX := toX;
                      fLastObjY := toY;
                      ReportCnxValid;
                    end
                  else
                    begin
                      result    := 0;
                      ErrorCode := ERROR_Unknown;
                    end;
              except
                result    := 0;
                ErrorCode := ERROR_Unknown;
                ReportCnxFailure;
              end
            finally
              Unlock;
            end;
          end
        else
          begin
            result := From;
            ErrorCode := NOERROR;
          end;
    end;

  function TServerCnxHandler.GetCompanyList( out ErrorCode : TErrorCode ) : TCompanyReport;
    var
      TextualReport : string;
    begin
      Lock;
      try
        try
          if not VarIsEmpty(fISProxy) and not fOffline
            then
              begin
                TextualReport := fISProxy.GetCompanyList;
                result        := TISCompanyReport.Create( TextualReport );
                ErrorCode     := NOERROR;
              end
            else
              begin
                result    := nil;
                ErrorCode := ERROR_Unknown;
              end;
        except
          result    := nil;
          ErrorCode := ERROR_Unknown;
          ReportCnxFailure;
        end;
      finally
        Unlock;
      end;
    end;

  function TServerCnxHandler.NewCompany( name, cluster : string; out ErrorCode : TErrorCode ) : TCompanyInfo;
    var
      TextualInfo : string;
    begin
      Lock;
      try
        try
          if not VarIsEmpty(fISProxy) and not fOffline
            then
              begin
                TextualInfo := string(fISProxy.NewCompany( name, cluster ));
                result      := ParseCompanyInfo( TextualInfo );
                ErrorCode   := NOERROR;
              end
            else
              begin
                result    := nil;
                ErrorCode := ERROR_Unknown;
              end;
        except
          result    := nil;
          ErrorCode := ERROR_Unknown;
          // >> ReportCnxFailure;
        end;
      finally
        Unlock;
      end;
    end;

  procedure TServerCnxHandler.NewFacility( FacilityId : string; CompanyId : integer; x, y : integer; out ErrorCode : TErrorCode );
    begin
      Lock;
      try
        try
          if not VarIsEmpty(fISProxy) and not fOffline
            then ErrorCode := TErrorCode(fISProxy.NewFacility( FacilityId, CompanyId, x, y ))
            else ErrorCode := ERROR_Unknown;
        except
          ErrorCode := ERROR_Unknown;
          // >> ReportCnxFailure;
        end;
      finally
        Unlock;
      end;
    end;

  function TServerCnxHandler.ConnectFacilities( Facility1, Facility2 : TObjId; out ErrorCode : TErrorCode ) : string;
    begin
      Lock;
      try
        try
          if not VarIsEmpty(fISProxy) and not fOffline
            then
              begin
                result := string(fISProxy.ConnectFacilities( Facility1, Facility2 ));
                ErrorCode := NOERROR;
              end
            else ErrorCode := ERROR_Unknown;
          ReportCnxValid;
        except
          ErrorCode := ERROR_Unknown;
          // >> ReportCnxFailure;
        end;
      finally
        Unlock;
      end;
    end;

  function TServerCnxHandler.GetUserList( out ErrorCode : TErrorCode ) : TStringList;
    var
      TextualReport : string;
    begin
      Lock;
      try
        try
          if not VarIsEmpty(fISProxy) and not fOffline
            then
              begin
                TextualReport := string(fISProxy.GetUserList);
                result := TStringList.Create;
                try
                  result.Text := TextualReport;
                except
                  result.Free;
                  raise;
                end;
              end
            else
              begin
                ErrorCode := ERROR_Unknown;
                result    := nil;
              end;
        except
          ErrorCode := ERROR_Unknown;
          result    := nil;
          ReportCnxFailure;
        end;
      finally
        Unlock;
      end;
    end;

  function TServerCnxHandler.GetChannelList( out ErrorCode : TErrorCode ) : TStringList;
    var
      TextualReport : string;
    begin
      Lock;
      try
        try
          if not VarIsEmpty(fISProxy) and not fOffline
            then
              begin
                TextualReport := string(fISProxy.GetChannelList( 'ROOT' ));
                result := TStringList.Create;
                try
                  result.Text := TextualReport;
                except
                  result.Free;
                  raise;
                end;
              end
            else
              begin
                ErrorCode := ERROR_Unknown;
                result    := nil;
              end;
        except
          ErrorCode := ERROR_Unknown;
          result    := nil;
          ReportCnxFailure;
        end;
      finally
        Unlock;
      end;
    end;

  procedure TServerCnxHandler.SayThis( Dest, Msg : string; out ErrorCode : TErrorCode );
    var
      Info : TChatMsgInfo;
    begin
      if uppercase(Dest) <> uppercase(fUserName)
        then
          begin
            Fork( threadedSayThis, priHigher, [Dest, Msg] );
            ErrorCode := NOERROR;
          end
        else
          begin
            Info.From := GetLiteral('Literal320');
            Info.Msg  := Msg;
            fMasterURLHandler.HandleEvent( evnChatMsg, Info );
            ErrorCode := NOERROR;
          end;
    end; 

  procedure TServerCnxHandler.VoiceThis( const Buffer : array of byte; len, TxId, NewTx : integer; out ErrorCode : TErrorCode );

    function BufferToStr( const Buffer : array of byte; len : integer ) : string;
      var
        i : integer;
      begin
        result := '';
        for i := 0 to pred(len) do
          result := result + IntToStr(Buffer[i]) + ',';
      end;

    var
      data : string;
    begin
      try
        data := BufferToStr( Buffer, len );
        fISProxy.VoiceThis( data, TxId, NewTx );
        ErrorCode := NOERROR;
      except
        ErrorCode := ERROR_Unknown;
      end;
    end;

  function TServerCnxHandler.VoiceRequest( out ErrorCode : TErrorCode ) : integer;
    begin
      try
        result := fISProxy.VoiceRequest( 1 );
        ErrorCode := NOERROR;
      except
        result    := 1;
        ErrorCode := ERROR_Unknown;
      end;
    end;

  procedure TServerCnxHandler.CancelVoiceRequest( out ErrorCode : TErrorCode );
    begin
      try
        fISProxy.CancelVoiceRequest( 1 );
        ErrorCode := NOERROR;
      except
        ErrorCode := ERROR_Unknown;
      end;
    end;

  procedure TServerCnxHandler.VoiceTxOver( out ErrorCode : TErrorCode );
    begin
      try
        fISProxy.VoiceTxOver( 1 );
        ErrorCode := NOERROR;
      except
        ErrorCode := ERROR_Unknown;
      end;
    end;

  procedure TServerCnxHandler.VoiceStatusChanged( Status : integer; out ErrorCode : TErrorCode );
    begin
      try
        fISProxy.VoiceStatusChanged( Status );
        ErrorCode := NOERROR;
      except
        ErrorCode := ERROR_Unknown;
      end;
    end;
    
  procedure TServerCnxHandler.CreateChannel( ChannelName, Password, aSessionApp, aSessionAppId : string; anUserLimit : integer; out ErrorCode : TErrorCode );
    begin
      try
        ErrorCode := TErrorCode(fISProxy.CreateChannel( ChannelName, Password, aSessionApp, aSessionAppId, anUserLimit ));
      except                                      
        ErrorCode := ERROR_Unknown;
      end;
    end;

  procedure TServerCnxHandler.JoinChannel( ChannelName, Password : string; out ErrorCode : TErrorCode );
    begin
      try
        ErrorCode := TErrorCode(fISProxy.JoinChannel( ChannelName, Password ));
      except                         
        ErrorCode := ERROR_Unknown;  
      end;
    end;

  function TServerCnxHandler.GetChannelInfo( ChannelName : string; out ErrorCode : TErrorCode ) : string;
    begin
      try
        result := string(fISProxy.GetChannelInfo( ChannelName ));
        ErrorCode := NOERROR;
      except
        ErrorCode := ERROR_Unknown;
      end;
    end;

  procedure TServerCnxHandler.MsgCompositionChanged( State : TMsgCompositionState; out ErrorCode : TErrorCode );
    begin
      Fork( threadedMsgCompositionChanged, priHigher, [integer(State)] );
      ErrorCode := NOERROR;
    end;

  procedure TServerCnxHandler.Chase( UserName : string; out ErrorCode : TErrorCode );
    begin
      Lock;
      try
        try
          if not VarIsEmpty(fISProxy) and not fOffline
            then
              begin
                ErrorCode := TErrorCode(fISProxy.Chase( UserName ));
                if ErrorCode = NOERROR
                  then
                    begin
                      fChasedUser := UserName;
                      fMasterURLHandler.HandleEvent( evnUserChaseStarted, fChasedUser );
                    end;
                ReportCnxValid;
              end
            else ErrorCode := ERROR_Unknown;
        except
          ErrorCode := ERROR_Unknown;
          ReportCnxFailure;
        end;
      finally
        Unlock;
      end;
    end;

  procedure TServerCnxHandler.StopChase( out ErrorCode : TErrorCode );
    begin
      Lock;
      try
        try
          if not VarIsEmpty(fISProxy) and not fOffline
            then
              begin
                ErrorCode := TErrorCode(fISProxy.StopChase);
                ReportCnxValid;
              end
            else ErrorCode := ERROR_Unknown;
        except
          ErrorCode := ERROR_Unknown;
          ReportCnxFailure;
        end;
      finally
        Unlock;
        fMasterURLHandler.HandleEvent( evnUserChaseAborted, fChasedUser );
        fChasedUser := '';
      end;
    end;

  procedure TServerCnxHandler.EnableEvents( out ErrorCode : TErrorCode );
    begin
      Lock;
      try
        try
          if not VarIsEmpty(fISProxy) and not fOffline
            then
              begin
                fISProxy.WaitForAnswer := true;
                fISProxy.EnableEvents  := true;
                fISProxy.WaitForAnswer := false;
                ErrorCode := NOERROR;
                ReportCnxValid;
              end;
        except
          ErrorCode := ERROR_Unknown;
          ReportCnxFailure;
        end;
      finally
        Unlock;
      end;
    end;

  procedure TServerCnxHandler.DisableEvents( out ErrorCode : TErrorCode );
    begin
      Lock;
      try
        try
          if not VarIsEmpty(fISProxy) and not fOffline
            then
              begin
                fISProxy.EnableEvents := false;
                ErrorCode := NOERROR;
                ReportCnxValid;
              end
        except
          ErrorCode := ERROR_Unknown;
          ReportCnxFailure;
        end;
      finally
        Unlock;
      end;
    end;

  procedure TServerCnxHandler.Logoff( out ErrorCode : TErrorCode );
    begin
      Lock;
      try
        try
          if not VarIsEmpty(fISProxy) and not fOffline and (fWSISCnx <> nil)
            then
              begin
                ClientNotAware;
                fWSISCnx.SetOnDisconnect( nil );
                fISProxy.TimeOut := 5000;
                ErrorCode := TErrorCode(fISProxy.Logoff);
              end
            else ErrorCode := ERROR_Unknown;
        except
          ErrorCode := ERROR_Unknown;
        end;
      finally
        Unlock;
      end;
    end;

  procedure TServerCnxHandler.CreateCircuitSeg( CircuitId, OwnerId, x1, y1, x2, y2, cost : integer; out ErrorCode : TErrorCode );
    begin
      Lock;
      try
        try
          if not VarIsEmpty(fISProxy) and not fOffline
            then ErrorCode := TErrorCode(fISProxy.CreateCircuitSeg( CircuitId, OwnerId, x1, y1, x2, y2, cost ))
            else ErrorCode := ERROR_Unknown;
        except
          ErrorCode := ERROR_Unknown;
        end;
      finally
        Unlock;
      end;
    end;

  procedure TServerCnxHandler.BreakCircuitAt( CircuitId, OwnerId, x, y : integer; out ErrorCode : TErrorCode );
    begin
      Lock;
      try
        try
          if not VarIsEmpty(fISProxy) and not fOffline
            then ErrorCode := TErrorCode(fISProxy.BreakCircuitAt( CircuitId, OwnerId, x, y ))
            else ErrorCode := ERROR_Unknown;
        except
          ErrorCode := ERROR_Unknown;
        end;
      finally
        Unlock;
      end;
    end;

  procedure TServerCnxHandler.WipeCircuit( CircuitId, OwnerId, x1, y1, x2, y2 : integer; out ErrorCode : TErrorCode );
    begin
      Lock;
      try
        try
          if not VarIsEmpty(fISProxy) and not fOffline
            then ErrorCode := TErrorCode(fISProxy.WipeCircuit( CircuitId, OwnerId, x1, y1, x2, y2 ))
            else ErrorCode := ERROR_Unknown;
        except
          ErrorCode := ERROR_Unknown;
        end;
      finally
        Unlock;
      end;
    end;
    
  function TServerCnxHandler.SegmentsInArea( CircuitId, x, y, dx, dy : integer; out ErrorCode : TErrorCode ) : TSegmentReport;
    const
      ItemsPerObject = 10;
    var
      CachedRegion : TCachedRegion;
      ServerReport : TStringList;
      i, d         : integer;
      kind         : TCargoKind;
    begin
      Lock;
      try
        try
          if not VarIsEmpty(fISProxy) and not fOffline
            then
              begin
                ServerReport := TStringList.Create;
                try
                  CachedRegion := GetCachedRegion( x, y, dx, dy );
                  if CachedRegion <> nil
                    then
                      case CircuitId of
                        cirRoads :
                          ServerReport.Text := CachedRegion.Roads;
                        cirRailRoads :
                          ServerReport.Text := CachedRegion.Rails;
                        else
                          ServerReport.Text := fISProxy.SegmentsInArea( CircuitId, x, y, x + dx, y + dy );
                      end
                    else ServerReport.Text := fISProxy.SegmentsInArea( CircuitId, x, y, x + dx, y + dy );
                  if ServerReport.Count mod ItemsPerObject = 0
                    then
                      begin
                        result.SegmentCount := ServerReport.Count div ItemsPerObject;
                        getmem( result.Segments, result.SegmentCount*sizeof(result.Segments[0]) );
                        for i := 0 to pred(result.SegmentCount) do
                          with result.Segments[i] do
                            try
                              x1 := StrToInt(ServerReport[ItemsPerObject*i]);
                              y1 := StrToInt(ServerReport[ItemsPerObject*i + 1]);
                              x2 := StrToInt(ServerReport[ItemsPerObject*i + 2]);
                              y2 := StrToInt(ServerReport[ItemsPerObject*i + 3]);
                              d  := 4;
                              for kind := low(kind) to high(kind) do
                                begin
                                  Cargo[kind].node1 := StrToInt(ServerReport[ItemsPerObject*i + d]);
                                  inc( d );
                                  Cargo[kind].node2 := StrToInt(ServerReport[ItemsPerObject*i + d]);
                                  inc( d );
                                end;
                            except
                            end;
                      end
                    else
                      begin
                        result.SegmentCount := 0;
                        result.Segments     := nil;
                      end;
                  ReportCnxValid;
                  ErrorCode := NOERROR;
                finally
                  ServerReport.Free;
                end;
              end
            else
              begin
                ErrorCode           := ERROR_InvalidProxy;
                result.SegmentCount := 0;
                result.Segments     := nil;
              end;
        except
          result.SegmentCount := 0;
          result.Segments     := nil;
          ErrorCode           := ERROR_Unknown;
          // >> ReportCnxFailure;
        end;
      finally
        Unlock;
      end;
    end;

  function TServerCnxHandler.GetSurface( SurfaceId : string; x, y, dx, dy : integer; out ErrorCode : TErrorCode ) : IMatrix;
    var
      image : string;
    begin
      Lock;
      try
        try
          if not VarIsEmpty(fISProxy) and not fOffline
            then
              begin
                image := string(fISProxy.GetSurface( widestring(SurfaceId), x, y, x + dx, y + dy ));
                result := TTestMatrix.Create;
                if DeCompressMap( result, image )
                  then ErrorCode := NOERROR
                  else
                    begin
                      ErrorCode := ERROR_Unknown;
                      result := nil;
                    end;
              end
            else
              begin
                ErrorCode := ERROR_Unknown;
                result    := nil;
              end;
        except
          ErrorCode := ERROR_Unknown;
          result    := nil;
          // >> ReportCnxFailure;
        end;
      finally
        Unlock;
      end;
    end;

  procedure TServerCnxHandler.DefineZone( TycoonId, ZoneId, x1, y1, x2, y2 : integer; out ErrorCode : TErrorCode );
    begin
      try
        ErrorCode := TErrorCode(fISProxy.DefineZone( TycoonId, ZoneId, x1, y1, x2, y2 ));
      except
        ErrorCode := ERROR_Unknown;
      end;
    end;

  function TServerCnxHandler.GetCookie( CookieId : string; out ErrorCode : TErrorCode ) : string;
    begin
      try
        result := string(fISProxy.GetTycoonCookie( fTycoonUId, CookieId ));
        ErrorCode := NOERROR;
      except
        ErrorCode := ERROR_Unknown;
      end;
    end;

  procedure TServerCnxHandler.SetCookie( CookieId, ValueId : string; out ErrorCode : TErrorCode );
    begin
      try
        fISProxy.SetTycoonCookie( fTycoonUId, CookieId, ValueId );
        ErrorCode := NOERROR;
      except
        ErrorCode := ERROR_Unknown;
      end;
    end;

  procedure TServerCnxHandler.CloneFacility( x, y : integer; LimitToTown, LimitToCompany : boolean );
    begin
      try
        fISProxy.CloneFacility( x, y, integer(LimitToTown), integer(LimitToCompany), fTycoonUId );
      except
      end;
    end;

  procedure TServerCnxHandler.GetNearestTownHall( x, y : integer; out xTown, yTown : integer ; out ErrorCode : TErrorCode );
    var
      coords : string;
      p      : integer;
    begin
      try
        coords := fISProxy.GetNearestTownHall( x, y );
        p      := 1;
        xTown  := StrToInt(GetNextStringUpTo( coords, p, ',' ));
        yTown  := StrToInt(GetNextStringUpTo( coords, p, ',' ));
        ErrorCode := NOERROR;
      except
        ErrorCode := ERROR_Unknown;
      end;
    end;
    
  function TServerCnxHandler.PickEvent : TStringList;
    var
      text : string;
    begin
      try
        text := fISProxy.PickEvent( fTycoonUId );
        if text <> ''
          then
            begin
              result := TStringList.Create;
              result.Text := text;
            end
          else result := nil
      except
        result := nil;
      end;
    end;
    
  function TServerCnxHandler.Echo( value : integer ) : integer;
    var
      WorldName : olevariant;
    begin
      try
        WorldName := fISProxy.UserName;
        result    := value;
      except
        result := 0;
      end;
    end;
    
  procedure TServerCnxHandler.ClientAware; 
    begin
      try
        fISProxy.ClientAware;  
      except
      end;
    end;

  procedure TServerCnxHandler.ClientNotAware;
    begin
      try
        fISProxy.ClientNotAware;
      except
      end;
    end;

  procedure TServerCnxHandler.SetLanguage( langid : widestring );
    begin
      try
        fISProxy.SetLanguage( langid );
      except
      end;
    end;
    
  function TServerCnxHandler.getUserName : string;
    begin
      result := fUserName;
    end;

  function TServerCnxHandler.getUserMasterName : string;
    begin
      if fMasterUserName <> ''
        then result := fMasterUserName
        else result := getUserName;
    end;
    
  function TServerCnxHandler.getUserPassword : string;
    begin
      result := uppercase(fPassword);
    end;

  function TServerCnxHandler.getMoney : currency;
    begin
      result := fMoney;
    end;
    
  function TServerCnxHandler.getCompanyName : string;
    begin
      result := fCompanyName;
    end;

  function TServerCnxHandler.getCompanyId : TCompanyId;
    begin
      result := fCompanyId;
    end;

  function TServerCnxHandler.getTycoonId : TObjId;
    begin
      result := fTycoonId;
    end;

  function TServerCnxHandler.getTycoonUId : integer;
    begin
      result := fTycoonUId;
    end;

  function TServerCnxHandler.getDate : TDateTime;
    begin
      result := fDate;
    end;

  function TServerCnxHandler.getSeason : integer;
    begin
      result := fSeason;
    end;

  function TServerCnxHandler.getWorldName : string;
    begin
      result := fWorldName;
    end;

  function TServerCnxHandler.getWorldURL : string;
    begin
      result := fWorldURL;
    end;

  function TServerCnxHandler.getWorldAbsURL : string;
    begin
      result := fWorldAbsURL;
    end;

  function TServerCnxHandler.getWorldXSize : integer;
    begin
      result := fWorldSize.x;
    end;

  function TServerCnxHandler.getWorldYSize : integer;
    begin
      result := fWorldSize.y;
    end;

  function TServerCnxHandler.getDAAddr : string;
    begin
      result := fDAAddr;
    end;

  function TServerCnxHandler.getDAPort : integer;
    begin
      result := fDAPort;
    end;

  function TServerCnxHandler.getDALockPort : integer;
    begin
      result := fDAPort;
    end;

  function TServerCnxHandler.getISAddr : string;
    begin
      result := fISAddr;
    end;

  function TServerCnxHandler.getISPort : integer;
    begin
      result := fISPort;
    end;

  function TServerCnxHandler.getMailAddr : string;
    begin
      result := fMailAddr;
    end;

  function TServerCnxHandler.getMailPort : integer;
    begin
      result := fMailPort;
    end;

  function TServerCnxHandler.getMailAccount : string;
    begin
      result := fMailAccount;
    end;

  function TServerCnxHandler.getCacheAddr : string;
    var
      p1  : integer;
      p2  : integer;
    begin
      p1 := system.pos('http://', fWorldURL);
      if p1 = 0
        then p1 := 1
        else p1 := length('http://') + 1;
      p2 := p1;
      while (p2 <= length(fWorldURL)) and (fWorldURL[p2] <> '/') do
        inc(p2);
      result := system.copy(fWorldURL, p1, p2 - p1);
    end;

  function TServerCnxHandler.getCachePort : integer;
    begin
      result := 6000; // >>
    end;

  function TServerCnxHandler.getSecurityId : string;
    begin
      result := IntToStr(integer(fTycoonId));
    end;

  function TServerCnxHandler.getClientViewId : TObjId;
    begin
      result := fClientViewId;
    end;

  function TServerCnxHandler.getChasedUser : string;
    begin
      result := fChasedUser;
    end;

  procedure TServerCnxHandler.SuplantedBy( NewClientView : IClientView );
    begin
      fSuplanted := NewClientView <> nil;
    end;

  procedure TServerCnxHandler.DisposeObjectReport( var ObjectReport : TObjectReport );
    begin
      with ObjectReport do
        begin
          freemem( Objects, ObjectCount*sizeof(Objects[0]) );
          ObjectCount := 0;
          Objects     := nil;
        end;
    end;

  procedure TServerCnxHandler.DisposeSegmentReport( var SegmentReport : TSegmentReport );
    begin
      with SegmentReport do
        begin
          freemem( Segments, SegmentCount*sizeof(Segments[0]) );
          SegmentCount := 0;
          Segments     := nil;
        end;
    end;

  procedure TServerCnxHandler.DisposeCnxReport( var CnxReport : TCnxReport );
    var
      i : integer;
    begin
      try
        with CnxReport do
          begin
            if Outputs <> nil
              then
                begin
                  for i := 0 to pred(OutputCount) do
                    freemem( Outputs[i].Connections );
                  freemem( Outputs );
                end;
            if Inputs <> nil
              then
                begin
                  for i := 0 to pred(InputCount) do
                    freemem( Inputs[i].Connections );
                  freemem( Inputs );
                end;
          end;
      except
      end;
      fillchar( CnxReport, sizeof(CnxReport), 0 );
    end;

  procedure TServerCnxHandler.SetAutologon( active : boolean );
    var
      ConfigHolder : IConfigHolder;
    begin
      fMasterURLHandler.HandleEvent( evnAnswerConfigHolder, ConfigHolder );
      ConfigHolder.WriteBoolean( false, '', 'AutoLogon', active );
    end;

  procedure TServerCnxHandler.StoreLastConfig;
    var
      ConfigHolder : IConfigHolder;
      KeyName      : string;
    begin
      fMasterURLHandler.HandleEvent( evnAnswerConfigHolder, ConfigHolder );
      KeyName := 'QuickLogon\' + fUserName + ' - ' + fWorldName;
      ConfigHolder.WriteString ( false, '', 'LastUser', getUserName + ' - ' + fWorldName );
      ConfigHolder.WriteString ( false, KeyName, 'UserName',  fUserName );
      ConfigHolder.WriteString ( false, KeyName, 'Password',  ScramblePassword( fPassword ));
      ConfigHolder.WriteString ( false, KeyName, 'World',     fWorldName );
      ConfigHolder.WriteString ( false, KeyName, 'Area',      fWorldArea );
      ConfigHolder.WriteString ( false, KeyName, 'Company',   fCompanyName );
      ConfigHolder.WriteInteger( false, KeyName, 'CompanyId', fCompanyId );
      ConfigHolder.WriteString ( false, KeyName, 'ISAddr',    fISAddr );
      ConfigHolder.WriteInteger( false, KeyName, 'ISPort',    fISPort );
    end;

  procedure TServerCnxHandler.threadedSayThis( const parms : array of const );
    var
      Dest : string;
      Msg  : string;
    begin
      try
        Lock;
        try
          try
            if not VarIsEmpty(fISProxy) and not fOffline
              then
                begin
                  Dest := parms[0].vpchar;
                  Msg  := parms[1].vpchar;
                  fISProxy.SayThis( Dest, Msg );
                  ReportCnxValid;
                end;
          except
            ReportCnxFailure;
          end;
        finally
          Unlock;
        end;
      except
      end;
    end;

  procedure TServerCnxHandler.threadedMsgCompositionChanged( const parms : array of const );
    var
      State : Integer absolute parms[0];
    begin
      try
        Lock;
        try
          try
            if not VarIsEmpty(fISProxy) and not fOffline
              then
                begin
                  fISProxy.MsgCompositionChanged( State );
                  ReportCnxValid;
                end;
          except
            ReportCnxFailure;
          end;
        finally
          Unlock;
        end;
      except
      end;
    end;

  function TServerCnxHandler.Logon( URL : string ) : TURLHandlingResult;
    var
      ResultURL     : TURL;
      ResultType    : string;
      Target        : string;
      AccountStatus : TErrorCode;
      ErrorCode     : TErrorCode;
      ClientId      : integer;
      SuccessURL    : string;
      Silent        : boolean;
      IgnoreErrors  : boolean;
      Area          : string;

    procedure ShowError( Code : TLogonError; URL, Target : string );
      begin
        Join( syncLogon, [integer(Code), URL, Target, IgnoreErrors] );
        if Code <> errNone
          then raise Exception.Create( '' );
      end;

    begin
      ErrorCode := NOERROR;
      Silent := true;
      try
        begin
          ErrorCode := 0;
          try
            fISAddr      := URLParser.GetParmValue( URL, htmlParmName_ISAddr );
            fISPort      := StrToInt(URLParser.GetParmValue( URL, htmlParmName_ISPort ));
            fDSAddr      := URLParser.GetParmValue( URL, htmlParmName_DSAddr );
            fDSPort      := StrToInt(URLParser.GetParmValue( URL, htmlParmName_DSPort ));
            fUserName    := URLParser.GetParmValue( URL, htmlParmName_UserName );
            fPassword    := URLParser.GetParmValue( URL, htmlParmName_Password );
            Silent       := StrToBoolean(URLParser.GetParmValue( URL, htmlParmName_Silent ));
            IgnoreErrors := StrToBoolean(URLParser.GetParmValue( URL, htmlParmName_IgnoreErrors ));
            ResultURL    := URLParser.GetParmValue( URL, htmlParmName_ResultURL );
            Target       := URLParser.GetParmValue( URL, htmlParmName_Target );
            Area         := URLParser.GetParmValue( URL, 'Area' );
            if fMasterUserName = ''
              then fMasterUserName := fUserName;
            // LogonSuccess := URLParser.GetParmValue( URL, htmlParmName_OnLogonSuccess );
            // LogonFailure := URLParser.GetParmValue( URL, htmlParmName_OnLogonFailure );
          except
            ErrorCode := ERROR_InvalidLogonData;
            raise;
          end;
          if (fISAddr <> '') and (fISPort > 0) and (fUserName <> '')
            then
              begin
                if not VarIsEmpty(fISProxy) and not IgnoreErrors
                  then
                    try
                      fWSISCnx.SetOnDisconnect( nil );
                      fISProxy.WaitForAnswer := true;
                      fISProxy.TimeOut       := LogoffTimeOut;
                      ErrorCode              := fISProxy.Logoff;
                      fISProxy.WaitForAnswer := false;
                    except
                    end;
                fWSISCnx      := TWinSockRDOConnection.Create('Interface Server');
                fISCnx        := fWSISCnx;
                fISCnx.Server := fISAddr;
                fISCnx.Port   := fISPort;
                fISProxy      := TRDOObjectProxy.Create as IDispatch;
                try
                  if fISCnx.Connect( ISTimeout )
                    then
                      begin
                        fISProxy.SetConnection( fISCnx );
                        fISProxy.BindTo( tidRDOHook_InterfaceServer );

                        fWorldName    := fISProxy.WorldName;
                        if Area <> ''
                          then fWorldArea := Area
                          else fWorldArea := fISProxy.DSArea;
                        fWorldAbsURL := fISProxy.WorldURL;
                        if system.pos( fWorldAbsURL, ResultURL ) > 0
                          then system.delete( ResultURL, 1, length( fWorldAbsURL ) );
                        fWorldURL     := fWorldAbsURL + ActiveLanguage + '/';
                        fDAAddr       := fISProxy.DAAddr;
                        fDAPort       := fISProxy.DALockPort;
                        fMailAddr     := fISProxy.MailAddr;
                        fMailPort     := fISProxy.MailPort;
                        fWorldSize.x  := fISProxy.WorldXSize;
                        fWorldSize.y  := fISProxy.WorldYSize;
                        fSeason       := fISProxy.WorldSeason;
                        fISProxy.TimeOut := ISProxyTimeOut;
                        AccountStatus    := fISProxy.AccountStatus( fUserName, fPassword );
                        case AccountStatus of
                          ACCOUNT_Valid, ACCOUNT_Unexisting :
                            {
                            if (AccountStatus <> ACCOUNT_Unexisting) or ConfirmPassword( fPassword )
                              then
                            }
                                begin
                                  fClientViewId := fISProxy.Logon( fUserName, fPassword );
                                  if fClientViewId <> 0
                                    then
                                      try
                                        begin
                                          fISProxy.BindTo( fClientViewId );
                                          // If reconecting then get rid of the old events...
                                          //fClientEventsServer.Free;
                                          //fClientEventsServer := TRDOServer.Create( fISCnx as IRDOServerConnection, 1, nil );
                                          Join( syncCreateEventsServer, [fISCnx] );
                                          fCreateEventsServerEvent.WaitFor( INFINITE );
                                          fCreateEventsServerEvent.ResetEvent;
                                          try
                                            fMailAccount := fISProxy.MailAccount;
                                            //fClientEventsServer.SetCriticalSection( fEventsLock );
                                            //fClientEventsServer.RegisterObject( tidRDOHook_InterfaceEvents, integer(fClientEvents) );
                                            fTycoonUId := fISProxy.TycoonId;
                                            ClientId   := fISProxy.RDOCnntId;
                                            if fISProxy.RegisterEventsById( ClientId ) = NOERROR
                                              then
                                                begin
                                                  fISProxy.SetLanguage( ActiveLanguage );
                                                  if not Silent
                                                    then
                                                      begin
                                                        if (AccountStatus = ACCOUNT_Unexisting) or (fISProxy.GetCompanyCount = 0)
                                                          then ResultType := 'NEWACCOUNT'
                                                          else ResultType := 'NORMAL';
                                                        SuccessURL :=
                                                          fWorldURL + ResultURL +
                                                          '?frame_Id=LogonView&'+
                                                          'frame_Class=HTMLView&'+
                                                          {'frame_Target=' + Target + '&' +}
                                                          'frame_Align=client&' +
                                                          'ResultType=' + ResultType + '&' +
                                                          'Logon=FALSE&' +
                                                          'frame_NoBorder=True&frame_NoScrollBars=true&' +
                                                          'ClientViewId=' + IntToStr(fClientViewId) + '&' +
                                                          'WorldName=' + fWorldName + '&' +
                                                          'UserName=' + fUserName + '&' +
                                                          'DSAddr=' + fDSAddr + '&' +
                                                          'DSPort=' + IntToStr(fDSPort) + '&' +
                                                          'ISAddr=' + fISAddr + '&' +
                                                          'ISPort=' + IntToStr(fISPort);
                                                        ShowError( errNone, SuccessURL, Target );
                                                      end
                                                    else ShowError( errNone, '', Target );
                                                  fWSISCnx.SetOnDisconnect( OnSocketDisconnect );
                                                  fOffLine := false;
                                                end
                                              else
                                                begin
                                                  if not Silent
                                                    then ShowError( errCannotRegisterEvents, ResultURL, Target );
                                                end
                                          except
                                            fClientEventsServer.Free;
                                            ErrorCode := ERROR_CannotRegisterEvents;
                                            if not Silent
                                              then ShowError( errCannotRegisterEvents, ResultURL, Target );
                                          end;
                                        end
                                      except
                                        fWSISCnx.SetOnDisconnect( nil );
                                        ErrorCode := fISProxy.Logoff;
                                        raise;
                                      end
                                    else ShowError( errCannotCreateClientView, ResultURL, Target );
                                end;
                              //else raise Exception.Create( 'Password was not confirmed' );
                          ACCOUNT_InvalidName :
                            ShowError( errInvalidName, ResultURL, Target );
                          ACCOUNT_InvalidPassword :
                            ShowError( errInvalidPassword, ResultURL, Target );
                          ACCOUNT_UnknownError :
                            ShowError( errUnknown, ResultURL, Target );
                        end;
                      end
                    else
                      begin
                        ErrorCode := ERROR_InvalidLogonData;
                        Join( syncLogon, [integer(errCannotConnect), ResultURL, Target, IgnoreErrors] );
                      end;
                except
                  fISCnx   := nil;
                  fISProxy := Unassigned;
                  if ErrorCode = NOERROR
                    then ErrorCode := ERROR_Unknown;
                  raise;
                end;
              end
            else
              begin
                ErrorCode := ERROR_InvalidLogonData;
                raise Exception.Create( 'Invalid Logon data.' );
              end;
          if ErrorCode = NOERROR
            then result := urlHandled
            else result := urlNotHandled
        end
      except
        if (ErrorCode <> 0) and not Silent
          then ShowError( errUnknown, ResultURL, Target );//fMasterURLHandler.HandleEvent( evnCriticalError, ErrorCode );
        result := urlNotHandled;
      end
    end;

  procedure TServerCnxHandler.threadedLogon( const parms : array of const );
    var
      URL : string absolute parms[0].vPChar;
    begin
      try
        Logon( URL );
      except
      end;
    end;

  procedure TServerCnxHandler.syncLogon( const parms : array of const );

    procedure ShowError( Viewer : TURL; Target : string; Error : TLogonError );
      const
        htmlValue_LogonError : array[TLogonError] of string =
          ( 'ERROR_CANNOTCONNECT',
            'ERROR_INVALIDNAME',
            'ERROR_INVALIDPASSWORD',
            'ERROR_CANNOTCREATECLIENTVIEW',
            'ERROR_CANNOTREGISTEREVENTS',
            'ERROR_REQUESTDENIED',
            'ERROR_UNKNOWN',
            'ERROR_NONE' );
      var
        URL : TURL;
        Msg : string;
      begin
        if Viewer <> ''
          then
            begin
              URL :=
                Viewer + '?' +
                'ResultType=ERROR&' +
                'frame_Id=LogonView&' +
                'frame_Class=HTMLView&' +
                'frame_Target=' + Target + '&' +
                'Logon=FALSE&' +
                'frame_Maximized=Yes&' +
                'frame_Align=client&' +
                'ErrorCode=' + htmlValue_LogonError[Error];
              fMasterURLHandler.HandleURL( URL );
            end
          else
            begin
              case Error of
                errCannotConnect :
                  Msg := GetLiteral('Literal321');
                errInvalidName :
                  Msg := GetLiteral('Literal322');
                errInvalidPassword :
                  Msg := GetLiteral('Literal323');
                errCannotCreateClientView :
                  Msg := GetLiteral('Literal324');
                errCannotRegisterEvents :
                  Msg := GetLiteral('Literal326');
                errRequestDenied :
                  Msg := GetLiteral('Literal328');
                else
                  Msg := GetLiteral('Literal330');
              end;
              ShowMsgBox( GetLiteral('Literal332'), Msg, 0, true, false );
            end;
        fMasterURLHandler.HandleEvent( evnLogonCompleted, Error );
      end;

    var
      Error  : TLogonError;
      URL    : string;
      Target : string;
      Ignore : boolean;
    begin
      ConnectingWin.Hide;
      Error  := TLogonError(parms[0].vInteger);
      URL    := parms[1].vPChar;
      Target := parms[2].vPChar;
      Ignore := parms[3].vBoolean;
      if Error = errNone
        then
          begin
            if URL <> ''
              then fMasterURLHandler.HandleURL( URL );
            fMasterURLHandler.HandleEvent( evnLogonCompleted, Error );
          end
        else
          if not Ignore
            then ShowError( URL, Target, Error );
    end;

  procedure TServerCnxHandler.syncCreateEventsServer( const parms : array of const );
    var
      cnx : IRDOConnectionInit absolute parms[0].vPointer;
    begin
      fClientEventsServer.Free;
      fClientEventsServer := TRDOServer.Create( cnx as IRDOServerConnection, 1, nil );
      fClientEventsServer.SetCriticalSection( fEventsLock );
      fClientEventsServer.RegisterObject( tidRDOHook_InterfaceEvents, integer(fClientEvents) );
      fClientEventsServer.SetCriticalSection( fEventsLock );
      fClientEventsServer.RegisterObject( tidRDOHook_InterfaceEvents, integer(fClientEvents) );
      fCreateEventsServerEvent.SetEvent;
    end;

  procedure TServerCnxHandler.syncRefreshArea;
    var
      Info : TRefreshAreaInfo;
    begin
      if fMasterURLHandler <> nil
        then
          begin
            Info.Area := TRefreshAreaRequest(syncDirtyArea[0]).Area;
            try
              fCachedRegs.Insert( TCachedRegion.Create( Info.Area, TRefreshAreaRequest(syncDirtyArea[0]).ExtraInfo, Time ));
            except
            end;
            syncDirtyArea.AtDelete( 0 );
            fMasterURLHandler.HandleEvent( evnRefreshArea, Info );
          end;
    end;

  procedure TServerCnxHandler.syncRefreshObject;
    var
      Info : TRefreshObjectInfo;
    begin
      if fMasterURLHandler <> nil
        then
          begin
            Info.ObjId := syncDirtyObject;
            Info.KindOfChange := TFacilityChange(syncKindOfChange);
            if Info.KindOfChange = fchStatus
              then CacheObjectText( Info.ObjId, syncExtraInfo );
            fMasterURLHandler.HandleEvent( evnRefreshObject, Info );
          end;
    end;

  procedure TServerCnxHandler.syncRefreshTycoon;
    var
      Info : TRefreshTycoonInfo;
    begin
      if fMasterURLHandler <> nil
        then
          begin
            fMoney         := syncMoney;
            Info.Money     := syncMoney;
            Info.NetProfit := syncNetProfit;
            Info.Ranking   := syncRanking;
            Info.FacCount  := syncFacCount;
            Info.FacMax    := syncFacMax;
            fMasterURLHandler.HandleEvent( evnRefreshTycoon, Info );
          end;
     end;

  procedure TServerCnxHandler.syncRefreshDate;
    var
      Info : TRefreshDateInfo;
    begin
      if fMasterURLHandler <> nil
        then
          begin
            fDate     := syncDate;
            Info.Date := syncDate;
            fMasterURLHandler.HandleEvent( evnRefreshDate, Info );
          end;
    end;

  procedure TServerCnxHandler.syncRefreshSeason;
    var
      Info : TRefreshSeasonInfo;
    begin
      if fMasterURLHandler <> nil
        then
          begin
            fSeason     := syncSeason;
            Info.Season := syncSeason;
            fMasterURLHandler.HandleEvent( evnRefreshSeason, Info );
          end;
    end;

  procedure TServerCnxHandler.syncEndOfPeriod;
    var
      Info : TEndOfPeriodInfo;
    begin
      if fMasterURLHandler <> nil
        then
          begin
            Info.FailureLevel := syncFailureLevel;
            fMasterURLHandler.HandleEvent( evnEndOfPeriod, Info );
          end;
    end;

  procedure TServerCnxHandler.syncTycoonRetired;
    var
      Info : TTycoonRetiredInfo;
    begin
      if fMasterURLHandler <> nil
        then
          begin
            Info.FailureLevel := syncFailureLevel;
            fMasterURLHandler.HandleEvent( evnTycoonRetired, Info );
          end;
    end;

  procedure TServerCnxHandler.syncChatMsg;
    var
      Info : TChatMsgInfo;
    begin
      if fMasterURLHandler <> nil
        then
          begin
            Info.From := syncChatFrom;
            if uppercase(syncChatFrom) = GetLiteral('Literal333')
              then Info.Msg := TimeToStr(Now) + ' - ' + syncChatText
              else Info.Msg := syncChatText;
            fMasterURLHandler.HandleEvent( evnChatMsg, Info );
          end;
    end;

  procedure TServerCnxHandler.syncVoiceMsg;

    procedure StrToBuffer( data : string; out buffer : array of byte; out len : integer );
      var
        p : integer;
      begin
        p   := 1;
        len := 0;
        while p < length(data) do
          begin
            buffer[len] := StrToInt(GetNextStringUpTo( data, p, ',' ));
            inc( len );
          end;
      end;

    var
      Info : TVoiceMsgInfo;
    begin
      if fMasterURLHandler <> nil
        then
          begin
            Info.From  := syncVoiceFrom;
            Info.TxId  := syncTxId;
            Info.NewTx := syncNewTx;
            StrToBuffer( syncVoiceData, Info.buffer, Info.len );
            fMasterURLHandler.HandleEvent( evnVoiceMsg, Info );
          end;
    end;

  procedure TServerCnxHandler.syncVoiceRequestGranted;
    begin
      fMasterURLHandler.HandleEvent( evnVoiceMsgAuthorized, self );
    end;

  procedure TServerCnxHandler.syncNewMail;
    var
      Info : TNewMailInfo;
    begin
      if fMasterURLHandler <> nil
        then
          begin
            Info.Count := syncMsgCount;
            fMasterURLHandler.HandleEvent( evnNewMail, Info );
          end;
    end;
    
  procedure TServerCnxHandler.syncMoveTo;
    var
      Info : TMoveToInfo;
    begin
      if fMasterURLHandler <> nil
        then
          begin
            Info.Pos := syncMoveToPoint;
            fMasterURLHandler.HandleEvent( evnMoveTo, Info );
          end;
    end;

  procedure TServerCnxHandler.syncNotifyCompanionship;
    var
      Info : TNotifyCompanionshipInfo;
    begin
      if fMasterURLHandler <> nil
        then
          begin
            Info.Names := syncCompanionship;
            fMasterURLHandler.HandleEvent( evnNotifyCompanionship, Info );
          end;
    end;

  procedure TServerCnxHandler.syncNotifyUserListChange;
    var
      Info : TUserListChangeInfo;
    begin
      if fMasterURLHandler <> nil
        then
          begin
            Info.UserName := syncUserName;
            Info.Change   := syncUserChange;
            fMasterURLHandler.HandleEvent( evnUserListChanged, Info );
            if syncUserName = fChasedUser
              then fMasterURLHandler.HandleEvent( evnUserChaseAborted, syncUserName );
          end;
    end;

  procedure TServerCnxHandler.syncNotifyChannelListChange;
    var
      Info : TChannelListChangeInfo;
    begin
      if fMasterURLHandler <> nil
        then
          begin
            Info.ChannelName := syncChannelName;
            Info.Password    := syncChannelPassword;
            Info.Change      := syncChannelChange;
            fMasterURLHandler.HandleEvent( evnChannelListChanged, Info );
          end;
    end;

  procedure TServerCnxHandler.syncNotifyChannelChange;
    begin
      if fMasterURLHandler <> nil
        then fMasterURLHandler.HandleEvent( evnChannelChanged, syncChannelName );
    end;

  procedure TServerCnxHandler.syncNotifyMsgCompositionState;
    var
      Info : TMsgCompositionChangeInfo;
    begin
      if fMasterURLHandler <> nil
        then
          begin
            Info.UserName := syncMsgCompUserName;
            Info.State    := syncMsgCompState;
            fMasterURLHandler.HandleEvent( evnMsgCompositionChanged, Info );
          end;
    end;

  procedure TServerCnxHandler.syncActorPoolModified;
    var
      Info : TMsgActorPoolModifiedInfo;
    begin
      if fMasterURLHandler <> nil
        then
          begin
            Info.ActorPoolId := syncActorPoolId;
            Info.Data        := syncActorPoolData;
            fMasterURLHandler.HandleEvent( evnActorPoolModified, Info );
            Info.Data.Free;
          end;
    end;
    
  procedure TServerCnxHandler.syncShowNotification;
    var
      Notificator : INotificator;
    begin
      if fMasterURLHandler <> nil
        then
          begin
            fMasterURLHandler.HandleEvent( evnAnswerNotificator, Notificator );
            if Notificator <> nil
              then Notificator.ShowNotification( syncNotfKind, syncNotfTitle, syncNotfBody, syncNotfOptions )
              else ShowMsgBox(  syncNotfTitle, syncNotfBody, 2, true, false );
          end;
    end;

  procedure TServerCnxHandler.syncGameMasterMsg( const parms : array of const );
    var
      Msg   : string;
      //Info  : integer;
      GMMsg : TGMMsgInfo;
    begin
      Msg  := parms[0].vPchar;
      //Info := parms[1].vInteger;
      if fMasterURLHandler <> nil
        then
          begin
            GMMsg.Msg := Msg;
            fMasterURLHandler.HandleEvent( evnGMMsg, GMMsg );
          end;
    end;

  procedure TServerCnxHandler.syncGMNotify( const parms : array of const );
    var
      GMEventInfo : TGMEvent; 
    begin
      GMEventInfo.notID := parms[0].vInteger;
      GMEventInfo.info  := parms[1].vPchar;
      fMasterURLHandler.HandleEvent( evnGMEvent, GMEventInfo )
    end;

  {
  function TServerCnxHandler.ConfirmPassword( var Password : string ) : boolean;
    var
      ConfirmPasswordDlg : TConfirmPasswordDlg;
      DoConfirm          : boolean;
      Confirmed          : boolean;
    begin
      if Password <> ''
        then
          begin
            ConfirmPasswordDlg := TConfirmPasswordDlg.Create( nil );
            try
              repeat
                ConfirmPasswordDlg.Password.Text := Password;
                DoConfirm := ConfirmPasswordDlg.ShowModal = mrOk;
                Confirmed := (uppercase(ConfirmPasswordDlg.Password.Text) = uppercase(ConfirmPasswordDlg.ConfirmPassword.Text)) and (ConfirmPasswordDlg.Password.Text <> '');
                if DoConfirm and not Confirmed
                  then ShowMsgBox( GetLiteral('Literal334'), GetLiteral('Literal335'), 0, true, false );
              until not DoConfirm or Confirmed;
              Password := ConfirmPasswordDlg.Password.Text;
              result   := DoConfirm;
            finally
              ConfirmPasswordDlg.Free;
            end;
          end                       
        else result := true;
    end;
  }

  procedure TServerCnxHandler.ConnectionDropped;
    var
      Info : TChatMsgInfo;
    begin
      fOffline := true;
      Info.From := GetLiteral('Literal336');
      Info.Msg  := GetLiteral('Literal337');
      fMasterURLHandler.HandleEvent( evnChatMsg, Info );
      fReconnectThread := TReconnectThread.Create( self );
      {
      fReconnectThread.OnTerminate := OnReconnectThreadTerminate;
      fReconnectThread.Resume;
      }
    end;

  procedure TServerCnxHandler.ReportCnxValid;
    begin
      fNetErrors := 0;
    end;

  procedure TServerCnxHandler.ReportCnxFailure;
    begin
      {
      if not VarIsEmpty(fISProxy)
        then
          begin
            inc( fNetErrors );
            if fNetErrors > NetErrorsTimesOut
              then ConnectionDropped;
          end;
      }
    end;

  function TServerCnxHandler.Reconnect( AccountName : string; Silent, IgnoreErrors : boolean ) : boolean;
    var
      LogonURL    : string;
      ErrorCode   : TErrorCode;
      Info        : TChatMsgInfo;
      UserChange  : boolean;
      SilentValue : string;
      IgnoreValue : string;
    begin
      fISCnx   := nil;
      fISProxy := Unassigned;
      Lock;
      try
        try
          UserChange := AccountName <> fUserName;
          if Silent
            then SilentValue := 'yes'
            else SilentValue := 'no';
          if IgnoreErrors
            then IgnoreValue := 'yes'
            else IgnoreValue := 'no';
          LogonURL :=
            '?frame_Action=Logon&' +
            'ISAddr=' + fISAddr + '&' +
            'ISport=' + IntToStr(fISPort) + '&' +
            'DSAddr=' + fDSAddr + '&' +
            'DSPort=' + IntToStr(fDSPort) + '&' +
            'UserName=' + AccountName + '&' +
            'Password=' + fPassword + '&' +
            'Silent=' + SilentValue + '&' +
            'IgnoreErrors=' + IgnoreValue;
          if Logon( LogonURL ) = urlHandled
            then
              begin
                fOffline   := false;
                fNetErrors := 0;
                result     := true;
                EnableEvents( ErrorCode );
                Info.From := GetLiteral('Literal338');
                if UserChange
                  then Info.Msg := GetFormattedLiteral('Literal339', [AccountName])
                  else Info.Msg := GetLiteral('Literal340');
                Join( syncReconnectChatMsg, [@Info] );
              end
            else
              begin
                Info.From := GetLiteral('Literal341');
                if UserChange
                  then Info.Msg := GetFormattedLiteral('Literal342', [AccountName])
                  else Info.Msg := GetLiteral('Literal343');
                Join( syncReconnectChatMsg, [@Info] );
                result := false;
              end
        except
          result := false;
        end;
      finally
        Unlock;
      end;
    end;

  procedure TServerCnxHandler.syncReconnectChatMsg( const parms : array of const );
    var
      Info : ^TChatMsgInfo absolute parms[0].VPointer;
    begin
      fMasterURLHandler.HandleEvent( evnChatMsg, Info^ );
    end;
    
  procedure TServerCnxHandler.OnSocketDisconnect( const ClientConnection : IRDOConnection );
    begin
      ConnectionDropped;
    end;

  {
  procedure TServerCnxHandler.OnReconnectThreadTerminate(Sender : TObject);
    begin
      fReconnectThread.Free;
      fReconnectThread := nil;
    end;
  }

  function TServerCnxHandler.ObjectTextCacheValid( Id : TObjId ) : boolean;
    var
      now : TDateTime;
    begin
      now := Time;
      result := (Id = fCachedObjectId) and (now - fTextCacheTTL < MaxTextCacheTTL )
    end;

  procedure TServerCnxHandler.CacheObjectText( Id : TObjId; Text : string );
    var
      p1, p2 : integer;
      kind   : TStatusKind;
    begin
      kind := low(kind);
      p1   := 1;
      repeat
        p2 := strutils.Pos( StatusTextSeparator, Text, p1 );
        if p2 > 0
          then
            begin
              fObjectTextCache[kind] := copy( Text, p1, p2 - p1 );
              inc( kind );
              p1 := p2 + length(StatusTextSeparator);
            end;
      until (p2 = 0);
      fTextCacheTTL   := Time;
      fCachedObjectId := Id;
    end;

  function TServerCnxHandler.GetCachedRegion( x, y, dx, dy : integer ) : TCachedRegion;

    function AreaInsideReg( Reg : TCachedRegion ) : boolean;
      var
        A : TRect;
      begin
        A := Rect( x, y, x + dx, y + dy );
        result := (A.Left >= Reg.Area.Left) and (A.Right  <= Reg.Area.Right) and
                  (A.Top  >= Reg.Area.Top)  and (A.Bottom <= Reg.Area.Bottom)
      end;

    var
      i : integer;
    begin
      fCachedRegs.Lock;
      try
        // First remove outdated items
        for i := pred(fCachedRegs.Count) downto 0 do
          with TCachedRegion(fCachedRegs[i]) do
            if SysUtils.Time - Time > MaxRegionTTL
              then fCachedRegs.AtDelete( i );
        // Then look for matching item
        i := 0;
        while (i < fCachedRegs.Count) and not AreaInsideReg( TCachedRegion(fCachedRegs[i]) ) do
          inc( i );
        if i < fCachedRegs.Count
          then result := TCachedRegion(fCachedRegs[i])
          else result := nil;
      finally
        fCachedRegs.Unlock;
      end;
    end;

  procedure TServerCnxHandler.Lock;
    begin
      inc(fLockCount);
      fServerLock.Enter;
    end;

  procedure TServerCnxHandler.Unlock;
    begin
      dec(fLockCount);
      fServerLock.Leave;
    end;

  function TServerCnxHandler.Offline : boolean;
    begin
      result := fOffline;
    end;

  function TServerCnxHandler.ConnectToGameMaster( ClientId : TCustomerId; ClientInfo : widestring; GameMasters : widestring ) : OleVariant;
    begin
      if not VarIsEmpty(fISProxy) and not fOffline
        then result := fISProxy.ConnectToGameMaster( fUserName, ClientInfo, GameMasters )
        else result := '';
    end;

  function TServerCnxHandler.SendGMMessage( ClientId : TCustomerId; GMId : TGameMasterId; Msg : WideString ) : OleVariant;
    begin
      if not VarIsEmpty(fISProxy) and not fOffline
        then
          begin
            result := fISProxy.SendGMMessage( ClientId, GMId, Msg );
          end
        else result := GM_ERR_UNEXPECTED;
    end;

  procedure TServerCnxHandler.DisconnectUser( ClientId : TCustomerId; GMId : TGameMasterId );
    begin
      if not VarIsEmpty(fISProxy) and not fOffline
        then fISProxy.DisconnectUser( ClientId, GMId )
    end;


initialization

  MaxTextCacheTTL := EncodeTime( 0, 0, 1, 400 );
  MaxRegionTTL    := EncodeTime( 0, 0, 1, 400 );

end.
