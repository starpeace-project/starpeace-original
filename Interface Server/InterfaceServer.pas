unit InterfaceServer;

{ $DEFINE BUGQUEST_NoEvnSpreading}
{$DEFINE UseLogs}

interface

  uses
    Classes, ExtCtrls, Collection, SyncObjs, RDOServer, Protocol, WinInet, HostNames,
    WinSockRDOConnection, WinSockRDOConnectionsServer, RDOInterfaces, AsxCriticalSections,
    Sessions, GMKernel, ISMLS, Languages, RDOConnectionPool;

  const
    MailConnectionTimeOut = 10000;
    GMConnectionTimeOut   = 40000;
    ClientTimeOut         = 2;
    ClientsCheckRate      = 15000;   // in miliseconds
    DATimeOut             = 10*1000; // timeout = 10 sec!
    RefreshRate           = 60000;   //90000;
    ISMaxThreads          = 24;      //16;

  const
    DIR_NOERROR = 0;
    DIR_NOERROR_StillTrial = -1;

  const
    tidURL_RegisterIS = 'ServerSync/RegisterIS.asp';

  const
    DSTimeOut = 30000;

  const
    MaxDAPoolCnx = 8;

  const
    MaxDownCountAllowed = 3;

  type
    TUserId       = integer;
    TObjId        = integer;
    TClientProxy  = OleVariant;
    TObjectReport = OleVariant;

  type
    TClientViewCriticalSection = (cvcsEnableEvents, cvcsViewport, cvcsChasers, cvcsCompanionship);

  type
    {$M+}
    // Classes defined

    TClientView      = class;
    TInterfaceServer = class;
    TModelEvents     = class;

    // Metaclases defined

    CClientView      = class of TClientView;
    CInterfaceServer = class of TInterfaceServer;
    CModelEvents     = class of TModelEvents;

    TChannelStatus = (cstChatting, cstPlaying);
    TChannel =
      class
        public
          constructor Create( aName, aCreator, aPassword, aSessionApp, aSessionAppId : string; anUserLimit : integer );
          destructor  Destroy; override;
        private
          fName         : string;
          fCreator      : string;
          fPassword     : string;
          fMembers      : TLockableCollection;
          fStatus       : TChannelStatus;
          fSessionApp   : string;
          fSessionAppId : string;
          fUserLimit    : integer;
          fVoiceReqs    : TStringList;
          fLock         : TCriticalSection;
          fSysChannel   : boolean;
        public
          property Name         : string              read fName;
          property Creator      : string              read fCreator;
          property Password     : string              read fPassword;
          property Members      : TLockableCollection read fMembers;
          property Status       : TChannelStatus      read fStatus;
          property SessionApp   : string              read fSessionApp;
          property SessionAppId : string              read fSessionAppId;
          property UserLimit    : integer             read fUserLimit;
          property SysChannel   : boolean             read fSysChannel write fSysChannel;
      end;

    TClientView =
      class
        private
          constructor Create;
        public
          destructor  Destroy; override;
        private
          fUserName      : string;
          fRealName      : string;
          fPassword      : string;
          fx1, fy1       : integer;
          fx2, fy2       : integer;
          fFocused       : TLockableCollection;
          fChasers       : TLockableCollection;
          fServer        : TInterfaceServer;
          fTarget        : TClientView;
          fTycoonId      : integer;
          fTycoonProxyId : integer;
          fTycoonProxy   : OleVariant;
          fConnection    : IRDOConnectionInit;
          // fCompanyProxy  : OleVariant;
          fEnableEvents  : boolean;
          fId            : integer;
          fLastRDOError  : integer;
          fCurrChannel   : TChannel;
          fVoiceEnabled  : boolean;
          fGMId          : TGameMasterId;
          fAware         : boolean;
          fLangId        : string;
          fLongChatLines : integer;
          fAccountDesc   : integer;
          fAFK           : boolean;
        private
          function GetCompositeName : string;
        published
          property UserName      : string  read fUserName;
          property CompositeName : string  read GetCompositeName;
          property TycoonId      : integer read fTycoonId;
          property AccountDesc   : integer read fAccountDesc;
          property AFK           : boolean read fAFK write fAFK;
        published
          property x1 : integer read fx1 write fx1;
          property y1 : integer read fy1 write fy1;
          property x2 : integer read fx2 write fx2;
          property y2 : integer read fy2 write fy2;
        private
          function  GetMailAccount  : string;
          function  GetEnableEvents : boolean;
          procedure SetEnableEvents( value : boolean );
        published
          property MailAccount  : string  read GetMailAccount;
          property EnableEvents : boolean read GetEnableEvents write SetEnableEvents;
        published
          procedure SetViewedArea( x, y, dx, dy : integer );
          function  ObjectsInArea( x, y, dx, dy : integer ) : OleVariant;
          function  ObjectAt( x, y : integer ) : OleVariant;
          function  ObjectStatusText( kind : TStatusKind; Id, TycoonId : TObjId ) : OleVariant;
          function  AllObjectStatusText( Id, TycoonId : TObjId ) : OleVariant;
          function  ContextStatusText( x, y : integer ) : OleVariant;
          function  ObjectConnections( Id : TObjId ) : OleVariant;
          procedure FocusObject( Id : TObjId );
          procedure UnfocusObject( Id : TObjId );
          function  SwitchFocus( From : TObjId; toX, toY : integer ) : OleVariant;
          function  SwitchFocusEx( From : TObjId; toX, toY : integer ) : OleVariant;
          function  ConnectFacilities( Facility1, Facility2 : TObjId ) : OleVariant;
          function  CreateCircuitSeg( CircuitId, OwnerId, x1, y1, x2, y2, cost : integer ) : OleVariant;
          function  BreakCircuitAt( CircuitId, OwnerId, x, y : integer ) : OleVariant;
          function  WipeCircuit( CircuitId, OwnerId, x1, y1, x2, y2 : integer ) : OleVariant;
          function  SegmentsInArea( CircuitId, x1, y1, x2, y2 : integer ) : OleVariant;
          function  GetSurface( SurfaceId : widestring; x1, y1, x2, y2 : integer ) : OleVariant;
          function  DefineZone( TycoonId, ZoneId, x1, y1, x2, y2 : integer ) : OleVariant;
          function  GetTycoonCookie( TycoonId : integer; CookieId : widestring ) : OleVariant;
          procedure SetTycoonCookie( TycoonId : integer; CookieId, CookieValue : widestring );
          procedure CloneFacility( x, y, LimitToTown, LimitToCompany, TycoonId : integer );
          function  GetNearestTownHall( x, y : integer ) : OleVariant;
          function  PickEvent( TycoonId : integer ) : OleVariant;
          function  GetUserName : OleVariant;
          function  GetCompanyList : OleVariant;
          function  GetCompanyOwnerRole( index : integer ) : OleVariant;
          function  GetCompanyName( index : integer ) : OleVariant;
          function  GetCompanyCluster( index : integer ) : OleVariant;
          function  GetCompanyId( index : integer ) : OleVariant;
          function  GetCompanyFacilityCount( index : integer ) : OleVariant;
          function  GetCompanyProfit( index : integer ) : OleVariant;
          function  GetCompanyCount : OleVariant;
          function  NewCompany( name, cluster : widestring ) : OleVariant;
          // function  SelCompany( CompanyId : integer ) : OleVariant;
          function  NewFacility( FacilityId : widestring; CompanyId : integer; x, y : integer ) : OleVariant;
          procedure SayThis( Dest, Msg : widestring );
          procedure VoiceThis( Msg : widestring; TxId, NewTx : integer );
          function  VoiceRequest( RequestId : integer ) : OleVariant;
          procedure CancelVoiceRequest( RequestId : integer );
          procedure VoiceTxOver( RequestId : integer );
          procedure VoiceStatusChanged( Status : integer );
          procedure MsgCompositionChanged( State : TMsgCompositionState );
          function  CreateChannel( ChannelName, Password, aSessionApp, aSessionAppId : widestring; anUserLimit : integer ) : OleVariant;
          function  JoinChannel( ChannelName, Password : widestring ) : OleVariant;
          function  LaunchChannelSession( ChannelName : widestring ) : OleVariant;
          function  Chase( UserName : widestring ) : OleVariant;
          function  StopChase : OleVariant;
          function  GetUserList : OleVariant;
          function  GetChannelList( Root : widestring ) : OleVariant;
          function  GetChannelInfo( Name : widestring ) : OleVariant;
          function  ISStatus : OleVariant;
          function  ClientViewId : OleVariant;
          procedure ClientAware;
          procedure ClientNotAware;
          procedure SetLanguage( langid : widestring );
        published // Favorites
          function RDOFavoritesNewItem    ( Location : widestring; Kind : integer; Name, Info : widestring ) : OleVariant;
          function RDOFavoritesDelItem    ( Location : widestring ) : OleVariant;
          function RDOFavoritesMoveItem   ( ItemLoc : widestring; Dest : widestring ) : OleVariant;
          function RDOFavoritesRenameItem ( ItemLoc : widestring; Name : widestring ) : OleVariant;
          function RDOFavoritesGetSubItems( ItemLoc : widestring ) : OleVariant;
        private
          fClientEventsProxy : OleVariant;
          fClientConnection  : IRDOConnection;
          fConnected         : boolean; // >> Protect this field with a lock...
          fBanned            : boolean;
        private
          procedure OnDisconnect(const ClientConnection : IRDOConnection);
          procedure DoLogOff;
        private
          fClientData : TStringList;
          fIPAddr     : string;
          fEnterDate  : TDateTime;
        published
          function  RegisterEvents( ClientAddress : widestring; ClientPort : integer ) : OleVariant;
          function  RegisterEventsById( ClientId : integer ) : OleVariant;
          procedure SetClientData(data : widestring);
          function  Logoff : OleVariant;
        private
          procedure RefreshArea( x, y, dx, dy : integer; ExtraInfo : string );
          function  RefreshObject( ObjId, KindOfChange : integer ) : boolean;
          procedure RefreshTycoon;
          procedure RefreshDate( Date : TDateTime );
          procedure RefreshSeason( Season : integer );
          procedure EndOfPeriod;
          procedure TycoonRetired;
          procedure SendTickData( PoolId, TickCount : integer; TickData : widestring );
          procedure SendNotification( Kind : integer; Title, Body : string; Options : integer );
          procedure ModelStatusChanged( Status : integer );
          procedure MoveTo( x, y : integer );
          procedure ListenSystemMsg(Msg : TRegMultiString; values : array of const);
          procedure HearThis( From, Msg : string );
          procedure HearThisVoice( From, Msg : string; TxId, NewTx : integer );
          procedure VoiceRequestGranted;
          procedure ReportNewMail( MsgCount : integer );
        private
          fCompanionship : string;
          //fTimeOut       : integer;
        private
          function  FindCompanionship : string;
          procedure NotifyCompanionship;
          procedure NotifyUserListChange( Name : widestring; Change : TUserListChange );
          procedure NotifyChannelListChange( Name, Password : widestring; Change : TUserListChange );
          procedure NotifyMsgCompositionState( Name : widestring; State : TMsgCompositionState );
          procedure NotifyChannelChange( Name : widestring );
          //function  Alive : boolean;
          //procedure CheckState;
        private
          fLock : array [TClientViewCriticalSection] of TCriticalSection;
        private
          procedure Lock( CriticalSection : TClientViewCriticalSection );
          procedure Unlock( CriticalSection : TClientViewCriticalSection );
        private
          function CanonicalSquare( x, y, dx, dy : integer ) : boolean;
        // GM functs
        published
          function  ConnectToGameMaster( ClientId : TCustomerId; UserInfo, GameMasters : widestring ) : OleVariant;
          function  SendGMMessage( ClientId : TCustomerId; GMId : TGameMasterId; Msg : WideString ) : OleVariant;
          procedure DisconnectUser( ClientId : TCustomerId; GMId : TGameMasterId );
        // GM Events
        private
          procedure GameMasterMsg( Msg : WideString; Info : integer );
          procedure GMNotify( notID : integer; Info : WideString );
        public
          function  IsRole : boolean;
        private
          function GetServerBusy : boolean;
        published
          property ServerBusy : boolean read GetServerBusy;
      end;

    PMapCacheMatrix = ^TMapCacheMatrix;
    TMapCacheMatrix = array[0..0] of widestring;
    TMapCache =
      class
        public
          constructor Create( xSize, ySize : integer );
          destructor  Destroy; override;
        private
          fxSize : integer;
          fySize : integer;
          fCache : PMapCacheMatrix;
          fLock  : TAsymetrixCriticalSection;
        private
          function  GetItem( x, y : integer ) : widestring;
          procedure SetItem( x, y : integer; value : widestring );
        public
          property Items[x, y : integer] : widestring read GetItem write SetItem; default;
      end;

    TRefreshThread =
      class( TThread )
        public
          constructor Create( aServer : TInterfaceServer );
          destructor  Destroy; override;
        private
          fServer : TInterfaceServer;
          fEndEvn : TEvent;
        protected
          procedure Execute; override;
      end;

    TInterfaceServer =
      class
        public
          constructor Create( aMSAddress, MailAddr, aDSAddress, aGMAddress, aDSArea : string; aMSPort, aMSEventsPort, MailPort, aDSPort, aGMPort, aClientPort : integer; FixAddr, BanListFile : string );
          destructor  Destroy; override;
        private
          fClients     : TLockableCollection;
          fHomeChannel : TChannel;
          fModelEvents : TModelEvents;
          fLastId      : integer;
          fServerError : boolean;
          fServerBusy  : boolean;
        private
          procedure OnDADisconnect(const ClientConnection : IRDOConnection);
        protected
          procedure Lock(comm : string);
          procedure Unlock(comm : string);
          procedure LockClients(comm : string);
          procedure UnlockClients(comm : string);
        private
          function  GetDAConnection : IRDOConnectionInit;
          procedure InitDAConnectionPool;
          //procedure DropDAConnection(Proxy : OleVariant; var Connection : IRDOConnectionInit);
          function RenewWorldProxy : boolean;
        public
          procedure CheckDAConnections;
        private
          fDACnntPool   : TRDOConnectionPool;
          fDAEventsRDO  : TRDOServer;
          fDAServerConn : IRDOConnectionsServer;
          fDAClientConn : IRDOConnectionInit;
          fDAProxy      : OleVariant;
          fDAOK         : boolean;
          fServerLock   : TCriticalSection;
          fDAProxyLock  : TCriticalSection;
          fDALastTick   : integer;
        private
          function GetWorldProxy : OleVariant;
        public
          property WorldProxy : OleVariant read GetWorldProxy;
        public
          function GetNewWorldProxy(var NewConnection : IRDOConnectionInit) : OleVariant;
        private
          procedure OnDSDisconnect(const ClientConnection : IRDOConnection);
        private
          fDSClientConn : IRDOConnectionInit;
          fDSProxy      : OleVariant;
          fSession      : integer;
          fDSOK         : boolean;
        public
          property DSOK : boolean read fDSOK;
        private
          procedure OnGMDisconnect(const ClientConnection : IRDOConnection);
        private
          fGMClientConn : IRDOConnectionInit;
          fGMProxy      : OleVariant;
          fGMEventsRDO  : TRDOServer;
          //fGMServerConn : IRDOConnectionsServer;
          fGMClientId   : integer;
        public
          property GMClientConn : IRDOConnectionInit read fGMClientConn;
          property GMProxy      : OleVariant         read fGMProxy;
        private
          fClientPort        : integer;
          fClientsRDO        : TRDOServer;
          fClientsServerConn : IRDOConnectionsServer;
        private
          fWorldName     : string;
          fWorldURL      : string;
          fWorldXSize    : integer;
          fWorldYSize    : integer;
          fDAAddr        : string;
          fDAPort        : integer;
          fDSAddr        : string;
          fDSPort        : integer;
          fDSArea        : string;
          fGMAddr        : string;
          fGMPort        : integer;
          fDALockPort    : integer;
          fMailAddr      : string;
          fMailPort      : integer;
          fLocalAddr     : string;
          fMSDownCount   : integer;
          fReconnects    : integer;
        private
          function GetWorldYear       : integer;
          function GetUserCount       : integer;
          function GetVisitorCount    : integer;
          function GetWorldPopulation : integer;
          function GetWorldSeason     : integer;
          procedure SetForceCommand( command : integer );
          function GetMSDown : boolean;
          function GetMinNobility : integer;
        private
          ttlUserCount  : cardinal;
          fUserCount    : integer;
          fVisitorCount : integer;
          fMaxUserCount : integer;
          fMinNobility  : integer;
        published
          property WorldName       : string  read fWorldName;
          property WorldURL        : string  read fWorldURL;
          property WorldXSize      : integer read fWorldXSize;
          property WorldYSize      : integer read fWorldYSize;
          property WorldYear       : integer read GetWorldYear;
          property WorldPopulation : integer read GetWorldPopulation;
          property WorldSeason     : integer read GetWorldSeason;
          property UserCount       : integer read GetUserCount;
          property Clients         : TLockableCollection read fClients;
          property DAAddr          : string  read fDAAddr;
          property DAPort          : integer read fDAPort;
          property DALockPort      : integer read fDALockPort;
          property DSAddr          : string  read fDSAddr;
          property DSPort          : integer read fDSPort;
          property DSArea          : string  read fDSArea;
          property GMAddr          : string  read fGMAddr;
          property GMPort          : integer read fGMPort;
          property MailAddr        : string  read fMailAddr;
          property MailPort        : integer read fMailPort;
          property ForceCommand    : integer write SetForceCommand;
          property MSDown          : boolean read GetMSDown;
          property MinNobility     : integer read GetMinNobility;
          property ServerBusy      : boolean read fServerBusy;
        private
          function CheckUserAccount(UserName, Password : string) : boolean;
          function GetUserNobility(UserName : string) : integer;
        published
          function AccountStatus( UserName, Password : widestring ) : OleVariant;
          function Logon( UserName, Password : widestring ) : OleVariant;
          function Logoff( ClientView : TClientView ) : OleVariant;
          function GetUserList( Channel : TChannel ) : OleVariant;
          function GetChannelList : OleVariant;
          function GetChannelInfo( Name, langid : widestring ) : OleVariant;
          function GetClientView(Name : widestring) : OleVariant;
          function CanJoinWorld(Name : widestring) : OleVariant;
          function CanJoinWorldEx(Name : widestring) : OleVariant;
          procedure BanPlayer(Name : widestring);
        private
          function GetClientByName    ( Name : string ) : TClientView;
          function GetClientByTycoonId( Id : integer )  : TClientView;
        private
          fSeason : integer;
        private
          function  ObjectsInArea( x, y, dx, dy : integer ) : string;
          function  SegmentsInArea( CircuitId, x1, y1, x2, y2 : integer ) : string;
          procedure RefreshArea( x, y, dx, dy : integer );
          procedure RefreshObject( ObjId, KindOfChange : integer );
          procedure RefreshTycoons;
          procedure RefreshDate( Date : TDateTime );
          procedure RefreshSeason( Season : integer );
          procedure EndOfPeriod;
          procedure TycoonRetired( name : string );
          procedure SendTickData( PoolId, ViewerId, TickCount : integer; TickData : widestring );
          procedure SendNotification( TycoonId : integer; Kind : integer; Title, Body : string; Options : integer );
          procedure ModelStatusChanged( Status : integer );
          procedure ChatMsg( Src, CompSrc, Dest, Msg : string );
          procedure SystemMsg(dest : string; Msg : TRegMultiString; values : array of const);
          procedure VoiceMsg( Src, Msg : string; TxId, NewTx : integer );
          procedure MsgCompositionChanged( Name : string; State : TMsgCompositionState );
          procedure NotifyCompanionship;
          procedure NotifyUserListChange( Name : string; Change : TUserListChange; Channel : TChannel );
          procedure NotifyChannelListChange( Name, Password : string; Change : TUserListChange );
        private
          procedure OnMailDisconnect(const ClientConnection : IRDOConnection);
        private
          fMailConn     : IRDOConnectionInit;
          fMailServer   : OleVariant;
          fMailId       : integer;
          fMailEvents   : TRDOServer;
        public
          property MailConn   : IRDOConnectionInit read fMailConn;
          property MailServer : OleVariant         read fMailServer;
        public
          function CreateDSConnection : boolean;
        public
          procedure InitMailServer( ServerName : string; ServerPort : integer );
          procedure InitGMServer( ServerName : string; ServerPort : integer );
          procedure InitDSServer( ServerName : string; ServerPort : integer; Area : string );
        published
          procedure ReportNewMail( Account, From, Subject, MsgId : widestring );
        private
          function CountUnreadMessages( Account : widestring ) : integer;
        private
          procedure InsertInMasterIndex;
          procedure UpdateMasterIndex;
          procedure DeleteFromMasterIndex;
          procedure NotifyToURL( URL : string );
          procedure StoreInfoInDS;
        {
        private
          fSentinel : TTimer;
          fRefresh  : TTimer;
        }
        private
          fRefreshThread : TRefreshThread;
        private
          procedure OnSentinel( Sender : TObject );
          procedure OnRefresh( Sender : TObject );
        private
          fObjectCache : TMapCache;
          fRoadsCache  : TMapCache;
        private
          fChannels : TLockableCollection;
          fSessions : TSessionServer;
        private
          function  GetChannel( ChannelId : string ) : TChannel;
          procedure ClientCreatedChannel( Client : TClientView; Name, Password, aSessionApp, aSessionAppId : string; anUserLimit : integer );
          procedure ClientEnteredChannel( Client : TClientView; Channel : TChannel );
          procedure ClientLeavedChannel( Client : TClientView; Channel : TChannel );
          function  LaunchChannelSession( Client : TClientView; ChannelName : string ) : boolean;
        // To GM server InterfaceServer
        private
          function  ConnectToGameMaster( ClientId : TCustomerId; UserInfo, GameMasters : widestring ) : OleVariant;
          function  SendGMMessage( ClientId : TCustomerId; GMId : TGameMasterId; Msg : WideString ) : OleVariant;
          procedure DisconnectUser( ClientId : TCustomerId; GMId : TGameMasterId );
        // To GM clients
        published
          function  GameMasterMsg( ClientId : TCustomerId; Msg : WideString; Info : integer ) : OleVariant;
          procedure GMNotify( ClientId : TCustomerId; notID : integer; Info : WideString );
        // System Messages
        published
          //procedure RDOSystemMsg(msg : string);
          function GetConfigParm(name, def : widestring) : olevariant;
        private
          fBanList : TStringList;
        private
          procedure CreateTownChannels;
        private
          fMaintDue : boolean;
        public
          procedure ReportMaintenance(eta : TDateTime; LastDowntime : integer);
        public
          property MaintDue : boolean read fMaintDue;
      end;

    TModelEvents =
      class
        public
          constructor Create( anInterfaceServer : TInterfaceServer );
        private
          fInterfaceServer : TInterfaceServer;
        published
          procedure RefreshArea( x, y, dx, dy : integer );
          procedure RefreshObject( ObjId, KindOfChange : integer );
          procedure RefreshTycoons( useless : integer );
          procedure RefreshDate( Date : TDateTime );
          procedure RefreshSeason( Season : integer );
          procedure EndOfPeriod( useless : integer );
          procedure TycoonRetired( name : widestring );
          procedure SendTickData( PoolId, ViewerId, TickCount : integer; TickDataStr : widestring );
          procedure SendNotification( TycoonId : integer; Kind : integer; Title, Body : widestring; Options : integer );
          procedure ModelStatusChanged( Status : integer );
          procedure ReportMaintenance(eta : TDateTime; LastDowntime : integer);
      end;
    {$M-}

  procedure InitLogProxy(addr : string; port : integer);

implementation

  uses
    SmartThreads, Windows, ComObj, SysUtils, LogFile, RDOObjectProxy, MailProtocol,
    DirectoryServerProtocol, Logs;

  type
    pwidestring = ^widestring;

  procedure InitLogProxy(addr : string; port : integer);
    var
      Cnx : IRDOConnectionInit;
    begin
      Cnx := TWinSockRDOConnection.Create('LogCnx');
      Cnx.Port   := port;
      Cnx.Server := addr;
      if Cnx.Connect(10000)
        then
          begin
            LogProxy := TRDOObjectProxy.Create as IDispatch;
            LogProxy.TimeOut := 20000;
            LogProxy.SetConnection(Cnx);
            if not LogProxy.BindTo('Logs')
              then LogProxy := Unassigned;
          end
        else LogProxy := Unassigned;
    end;

  procedure Logthis(const lgName, lgMessage : string; kind : integer);
    begin
      {
      if not VarIsEmpty(LogProxy)
        then
          try
            LogProxy.Log(GetCurrentThreadId, GetTickCount, kind, lgMessage);
          except
            LogProxy := Unassigned;
            Logs.Log(lgName, TimeToStr(Now) + ' Log Proxy Down..')
          end
        else Logs.Log(lgName, IntToStr(GetCurrentThreadId) + ' ' + lgMessage + ^M^J);
      }
      //Logs.Log(lgName, TimeToStr(Now) + ' Thr: ' + IntToStr(GetCurrentThreadId) + ' ' + lgMessage);
    end;

  // TChannel

  constructor TChannel.Create( aName, aCreator, aPassword, aSessionApp, aSessionAppId : string; anUserLimit : integer );
    begin
      inherited Create;
      fName         := aName;
      fCreator      := aCreator;
      fPassword     := aPassword;
      fSessionApp   := aSessionApp;
      fSessionAppId := aSessionAppId;
      fUserLimit    := anUserLimit;
      fMembers      := TLockableCollection.Create( 0, rkUse );
      fVoiceReqs    := TStringList.Create;
      fLock         := TCriticalSection.Create;
    end;

  destructor TChannel.Destroy;
    begin
      fLock.Free;
      fVoiceReqs.Free;
      fMembers.Free;
      inherited;
    end;


  // TClientView

  constructor TClientView.Create;
    var
      i : TClientViewCriticalSection;
    begin
      inherited Create;
      for i := low(i) to high(i) do
        fLock[i] := TCriticalSection.Create;
      fFocused := TLockableCollection.Create( 0, rkUse );
      fChasers := TLockableCollection.Create( 0, rkUse );
      fLangId := '0';
      fClientData := TStringList.Create;
      fClientData.Add('CRC=0');
      fAFK := False;
    end;

  destructor TClientView.Destroy;
    var
      i : TClientViewCriticalSection;
    begin
      StopChase;
      fChasers.Free;
      fFocused.Free;
      for i := low(i) to high(i) do
        fLock[i].Free;
      if fClientConnection <> nil
        then fClientConnection.OnDisconnect := nil; // @@
      fClientData.Free;
      inherited;
    end;

  function TClientView.GetCompositeName : string;
    begin
      if fRealName = fUserName
        then result := fUserName
        else result := fUserName + ' (' + fRealName + ')';
    end;
    
  function TClientView.GetMailAccount : string;
    begin
      try
        result := fUserName + '@' + fServer.WorldName + '.net';
      except
        result := '';
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in GetMailAccount' );
        except
        end;
      end;
    end;

  function TClientView.GetEnableEvents : boolean;
    begin
      Lock( cvcsEnableEvents );
      try
        result := fEnableEvents;
      finally
        Unlock( cvcsEnableEvents );
      end;
    end;

  procedure TClientView.SetEnableEvents( value : boolean );
    begin
      Lock( cvcsEnableEvents );
      try
        fEnableEvents := value;
      finally
        Unlock( cvcsEnableEvents );
      end;
    end;

  procedure TClientView.SetViewedArea( x, y, dx, dy : integer );

    procedure UpdateChasers;
      var
        i : integer;
      begin
        fChasers.Lock;
        try
          for i := 0 to pred(fChasers.Count) do
            (fChasers[i] as TClientView).MoveTo( x + dx div 2, y + dy div 2 );
        finally
          fChasers.Unlock;
        end;
      end;

    begin
      try
        Lock( cvcsViewport );
        try
          fx1 := x;
          fy1 := y;
          fx2 := x + dx;
          fy2 := y + dy;
        finally
          Unlock( cvcsViewport );
        end;
        try
          if fServer.fDAOK
            then fServer.WorldProxy.RDOSetTycoonViewport( fTycoonId, 0, fx1, fy1, fx2, fy2 );
        except
          try
            fServer.RenewWorldProxy;
            Logs.Log('Survival', TimeToStr(Now) + ' - Error in MS SetViewedArea' );
          except
          end;
        end;
        fServer.NotifyCompanionship;
        UpdateChasers;
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in SetViewedArea' );
        except
        end;
      end;
    end;

  function TClientView.ObjectsInArea( x, y, dx, dy : integer ) : TObjectReport;
    var
      cachedValue : widestring;
    begin
      try
        if fServer.fDAOK
          then
            if CanonicalSquare( x, y, dx, dy )
              then
                begin
                  cachedValue := fServer.fObjectCache[x, y];
                  if cachedValue <> ''
                    then result := cachedValue
                    else
                      begin
                        result := widestring(fServer.WorldProxy.RDOGetObjectsInArea( x, y, dx, dy ));
                        fServer.fObjectCache[x, y] := result;
                      end;
                end
              else result := widestring(fServer.WorldProxy.RDOGetObjectsInArea( x, y, dx, dy ))
          else result := ERROR_Unknown;
      except
        fServer.RenewWorldProxy;
        if CanonicalSquare( x, y, dx, dy )
          then fServer.fObjectCache[x, y] := '';
        result := ERROR_Unknown;
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in ObjectsInArea' );
        except
        end;
      end;
    end;

  function TClientView.ObjectAt( x, y : integer ) : OleVariant;
    begin
      try
        if fServer.fDAOK
          then result := integer(fServer.WorldProxy.RDOFacilityAt( x, y ))
          else result := integer(0);
      except
        result := 0;
        try
          fServer.RenewWorldProxy;
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in ObjectsAt' );
        except
        end;
      end;
    end;

  function TClientView.ObjectStatusText( kind : TStatusKind; Id, TycoonId : TObjId ) : OleVariant;
    begin
      try
        if fServer.fDAOK
          then result := widestring(fServer.WorldProxy.RDOObjectStatusText( kind, Id, TycoonId ))
          else result := '';
      except
        fServer.RenewWorldProxy;
        result := ERROR_Unknown;
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in ObjectStatusText' );
        except
        end;
      end;
    end;

  function TClientView.AllObjectStatusText( Id, TycoonId : TObjId ) : OleVariant;
    begin
      //LogThis('Survival', '(AST.1)', 0);
      try
        if fServer.fDAOK
          then result := widestring(fServer.WorldProxy.RDOAllObjectStatusText( Id, TycoonId ))
          else result := '';
      except
        fServer.RenewWorldProxy;
        result := ERROR_Unknown;
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in AllObjectStatusText' );
        except
        end;
      end;
      //LogThis('Survival', '(AST.2)', 0);
    end;

  function TClientView.ContextStatusText( x, y : integer ) : OleVariant;
    begin
      try
        if fServer.fDAOK and not fServer.fServerBusy
          then result := widestring(fServer.WorldProxy.RDOContextStatusText( fTycoonProxyId, x, y ))
          else result := '';
      except
        fServer.RenewWorldProxy;
        result := ERROR_Unknown;
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in ContextStatusText' );
        except
        end;
      end;
    end;

  function TClientView.ObjectConnections( Id : TObjId ) : OleVariant;
    begin
      try
        if fServer.fDAOK and not fServer.fServerBusy
          then result := widestring(fServer.WorldProxy.RDOGetFacilityConnections( Id ))
          else result := '';
      except
        fServer.RenewWorldProxy;
        result := ERROR_Unknown;
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in ObjectConnections' );
        except
        end;
      end;
    end;

  procedure TClientView.FocusObject( Id : TObjId );
    begin
      try
        if fServer.fDAOK
          then
            begin
              fServer.WorldProxy.RDOFacilityGotFocus( Id );
              fFocused.Insert( TObject(Id) );
            end;
      except
        fServer.RenewWorldProxy;
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in FocusObject' );
        except
        end;
      end;
    end;

  procedure TClientView.UnfocusObject( Id : TObjId );
    begin
      try
        if fServer.fDAOK
          then
            begin
              if fFocused.IndexOf(TObject(Id)) <> noIndex
                then
                  begin
                    fFocused.Delete(TObject(Id));
                    fServer.WorldProxy.RDOFacilityLostFocus(Id);
                  end;
            end;
      except
        fServer.RenewWorldProxy;
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in UnfocusObject' );
        except
        end;
      end;
    end;

  function TClientView.SwitchFocus( From : TObjId; toX, toY : integer ) : OleVariant;
    begin
      try
        if From <> 0
          then UnFocusObject( From );
        result := ObjectAt( toX, toY );
        if result <> 0
          then FocusObject( result );
      except
        fServer.RenewWorldProxy;
        result := 0;
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in SwitchFocus' );
        except
        end;
      end;
    end;

  function TClientView.SwitchFocusEx( From : TObjId; toX, toY : integer ) : OleVariant;
    var
      ObjId  : TObjId;
      Report : string;
    begin
      try
        if fServer.fDAOK
          then
            begin
              ObjId  := TObjId(SwitchFocus( From, toX, toY ));
              Report := IntToStr(ObjId);
              Report := Report + LineBreak + string(fServer.WorldProxy.RDOAllObjectStatusText( ObjId, fTycoonProxyId ));
              result := widestring(Report);
            end
          else result := '';
      except
        fServer.RenewWorldProxy;
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in SwitchFocusEx' );
        except
        end;
      end;
    end;

  function TClientView.ConnectFacilities( Facility1, Facility2 : TObjId ) : OleVariant;
    begin
      try
        if fServer.fDAOK and not fServer.fServerBusy
          then result := widestring(fServer.WorldProxy.RDOConnectFacilities( fTycoonProxyId, Facility1, Facility2 ))
          else result := '';
      except
        fServer.RenewWorldProxy;
        result := ERROR_Unknown;
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in ConnectFacilities' );
        except
        end;
      end;
    end;

  function TClientView.CreateCircuitSeg( CircuitId, OwnerId, x1, y1, x2, y2, cost : integer ) : OleVariant;
    begin
      try
        if fServer.fDAOK and not fServer.fServerBusy
          then result := fServer.WorldProxy.RDOCreateCircuitSeg( CircuitId, OwnerId, x1, y1, x2, y2, cost )
          else result := ERROR_Unknown;
      except
        fServer.RenewWorldProxy;
        result := ERROR_Unknown;
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in CreateCircuitSeg' );
        except
        end;
      end;
    end;

  function TClientView.BreakCircuitAt( CircuitId, OwnerId, x, y : integer ) : OleVariant;
    begin
      try
        if fServer.fDAOK and not fServer.fServerBusy
          then result := fServer.WorldProxy.RDOBreakCircuitAt( CircuitId, OwnerId, x, y )
          else result := ERROR_Unknown;
      except
        fServer.RenewWorldProxy;
        result := ERROR_Unknown;
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in BreakCircuitAt' );
        except
        end;
      end;
    end;

  function TClientView.WipeCircuit( CircuitId, OwnerId, x1, y1, x2, y2 : integer ) : OleVariant;
    begin
      try
        if fServer.fDAOK and not fServer.fServerBusy
          then result := fServer.WorldProxy.RDOWipeCircuit( CircuitId, OwnerId, x1, y1, x2, y2 )
          else result := ERROR_Unknown;
      except
        fServer.RenewWorldProxy;
        result := ERROR_Unknown;
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in WipeCircuit' );
        except
        end;
      end;
    end;

  function TClientView.SegmentsInArea( CircuitId, x1, y1, x2, y2 : integer ) : OleVariant;
    var
      cachedValue : widestring;
      x, y        : integer;
    begin
      try
        if (CircuitId <> cirRoads) or not CanonicalSquare( x1, y1, x2 - x1, y2 - y1 )
          then
            begin
              try
                if fServer.fDAOK
                  then result := widestring(fServer.WorldProxy.RDOSegmentsInArea( CircuitId, x1, y1, x2, y2 ))
                  else result := '';
              except
                fServer.RenewWorldProxy;
                raise;
              end;
            end
          else
            begin
              x := (x1 + x2) div 2;
              y := (y1 + y2) div 2;
              cachedValue := fServer.fRoadsCache[x, y];
              if cachedValue <> ''
                then result := cachedValue
                else
                  if fServer.fDAOK
                    then
                      begin
                        try
                          result := widestring(fServer.WorldProxy.RDOSegmentsInArea( CircuitId, x1, y1, x2, y2 ));
                        except
                          fServer.RenewWorldProxy;
                          raise;
                        end;
                        fServer.fRoadsCache[x, y] := result;
                      end
                    else result := '';
            end;
      except
        result := ERROR_Unknown;
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in SegmentsInArea' );
        except
        end;
      end;
    end;

  function TClientView.GetSurface( SurfaceId : widestring; x1, y1, x2, y2 : integer ) : OleVariant;
    var
      tmp : widestring;
    begin
      try
        if fServer.fDAOK
          then tmp := widestring(fServer.WorldProxy.RDOGetSurface( SurfaceId, x1, y1, x2, y2 ))
          else tmp := '';
        result := tmp;
      except
        fServer.RenewWorldProxy;
        result := ERROR_Unknown;
        try
          fLastRDOError := fServer.WorldProxy.ErrorCode;
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in GetSurface' );
        except
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in GetSurface getting ErrorCode' );
        end;
      end;
    end;

  function TClientView.DefineZone( TycoonId, ZoneId, x1, y1, x2, y2 : integer ) : OleVariant;
    begin
      try
        if fServer.fDAOK and not fServer.fServerBusy
          then result := fServer.WorldProxy.RDODefineZone( TycoonId, ZoneId, x1, y1, x2, y2 )
          else result := ERROR_Unknown;
      except
        //fServer.RenewWorldProxy;
        result := ERROR_Unknown;
        try
          fLastRDOError := fServer.WorldProxy.ErrorCode;
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in DefineZone, code: ' + IntToStr(fLastRDOError));
        except
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in DefineZone Getting Error Code' );
        end;
      end;
    end;

  function TClientView.GetTycoonCookie( TycoonId : integer; CookieId : widestring ) : OleVariant;
    begin
      try
        if fServer.fDAOK and not fServer.fServerBusy
          then result := widestring(fServer.WorldProxy.RDOGetTycoonCookie( TycoonId, CookieId ))
          else result := '';
      except
        fServer.RenewWorldProxy;
        result := '';
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in GetTycoonCookie' );
        except
        end;
      end;
    end;

  procedure TClientView.SetTycoonCookie( TycoonId : integer; CookieId, CookieValue : widestring );
    begin
      try
        if fServer.fDAOK and not fServer.fServerBusy
          then fServer.WorldProxy.RDOSetTycoonCookie( TycoonId, CookieId, CookieValue );
      except
        fServer.RenewWorldProxy;
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in SetTycoonCookie' );
        except
        end;
      end;
    end;

  procedure TClientView.CloneFacility( x, y, LimitToTown, LimitToCompany, TycoonId : integer );
    begin
      try
        if fServer.fDAOK
          then fServer.WorldProxy.RDOCloneFacility( x, y, LimitToTown, LimitToCompany, TycoonId );
      except
        fServer.RenewWorldProxy;
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in CloneFacility' );
        except
        end;
      end;
    end;

  function TClientView.GetNearestTownHall( x, y : integer ) : OleVariant;
    begin
      try
        if fServer.fDAOK
          then result := widestring(fServer.WorldProxy.RDOGetNearestTownHall( x, y))
          else result := '';
      except
        fServer.RenewWorldProxy;
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in GetNearestTownHall' );
        except
        end;
      end;
    end;

  function TClientView.PickEvent( TycoonId : integer ) : OleVariant;
    begin
      try
        if fServer.fDAOK and not fServer.fServerBusy
          then result := widestring(fServer.WorldProxy.RDOPickEvent( TycoonId ))
          else result := '';
      except
        fServer.RenewWorldProxy;
        try
          result := '';
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in PickEvent' );
        except
        end;
      end;
    end;

  function TClientView.GetUserName : OleVariant;
    begin
      try
        result := widestring(fUserName);
      except
        fServer.RenewWorldProxy;
        result := ERROR_Unknown;
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in GetUserName' );
        except
        end;
      end;
    end;

  function TClientView.GetCompanyList : OleVariant;
    begin
      try
        if fServer.fDAOK
          then result := widestring(fServer.WorldProxy.RDOGetCompanyList( UserName ))
          else result := '';
      except
        fServer.RenewWorldProxy;
        result := ERROR_Unknown;
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in GetCompanyList' );
        except
        end;
      end;
    end;

  function TClientView.GetCompanyOwnerRole( index : integer ) : OleVariant;
    begin
      try
        if fServer.fDAOK
          then result := widestring(fServer.WorldProxy.RDOGetCompanyOwnerRole( UserName, index ))
          else result := '';
      except
        fServer.RenewWorldProxy;
        result := ERROR_Unknown;
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in GetCompanyOwnerRole' );
        except
        end;
      end;
    end;

  function TClientView.GetCompanyName( index : integer ) : OleVariant;
    begin
      try
        if fServer.fDAOK
          then result := widestring(fServer.WorldProxy.RDOGetCompanyName( UserName, index ))
          else result := '';
      except
        fServer.RenewWorldProxy;
        result := ERROR_Unknown;
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in GetCompanyName' );
        except
        end;
      end;
    end;

  function TClientView.GetCompanyCluster( index : integer ) : OleVariant;
    begin
      try
        if fServer.fDAOK
          then result := string(fServer.WorldProxy.RDOGetCompanyCluster( UserName, index ))
          else result := '';
      except
        fServer.RenewWorldProxy;
        result := ERROR_Unknown;
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in GetCompanyCluster' );
        except
        end;
      end;
    end;

  function TClientView.GetCompanyId( index : integer ) : OleVariant;
    begin
      try
        if fServer.fDAOK
          then result := integer(fServer.WorldProxy.RDOGetCompanyId( UserName, index ))
          else result := ERROR_Unknown;
      except
        fServer.RenewWorldProxy;
        result := ERROR_Unknown;
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in GetCompanyId' );
        except
        end;
      end;
    end;

  function TClientView.GetCompanyFacilityCount( index : integer ) : OleVariant;
    begin
      try
        if fServer.fDAOK
          then result := integer(fServer.WorldProxy.RDOGetCompanyFacilityCount( UserName, index ))
          else result := ERROR_Unknown;
      except
        fServer.RenewWorldProxy;
        result := ERROR_Unknown;
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in GetCompanyFacilityCount' );
        except
        end;
      end;
    end;

  function TClientView.GetCompanyProfit( index : integer ) : OleVariant;
    begin
      try
        if fServer.fDAOK
          then result := fServer.WorldProxy.RDOGetCompanyProfit( UserName, index )
          else result := ERROR_Unknown;
      except
        fServer.RenewWorldProxy;
        result := ERROR_Unknown;
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in GetCompanyProfit' );
        except
        end;
      end;
    end;

  function TClientView.GetCompanyCount : OleVariant;
    begin
      try
        if fServer.fDAOK
          then result := integer(fServer.WorldProxy.RDOGetCompanyCount( UserName ))
          else result := ERROR_Unknown;
      except
        fServer.RenewWorldProxy;
        result := ERROR_Unknown;
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in GetCompanyCount' );
        except
        end;
      end;
    end;

  function TClientView.NewCompany( name, cluster : widestring ) : OleVariant;
    begin
      try
        if fServer.fDAOK
          then result := widestring(fServer.WorldProxy.RDONewCompany( UserName, name, cluster ))
          else result := ERROR_Unknown;
      except
        fServer.RenewWorldProxy;
        result := ERROR_Unknown;
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in NewCompany' );
        except
        end;
      end;
    end;

  {
  function TClientView.SelCompany( CompanyId : integer ) : OleVariant;
    var
      Company : integer;
    begin
      try
        Lock;
        try
          Company := fServer.WorldProxy.RDOGetCompany( CompanyId );
          if Company <> 0
            then
              begin
                fCompanyProxy := TRDOObjectProxy.Create as IDispatch;
                fCompanyProxy.SetConnection( fServer.fDAClientConn );
                fCompanyProxy.BindTo( Company );
                result := NOERROR;
              end
            else result := ERROR_InvalidCompanyId
        finally
          Unlock;
        end;
      except
        result := ERROR_Unknown;
      end;
    end;
  }

  function TClientView.NewFacility( FacilityId : widestring; CompanyId : integer; x, y : integer ) : OleVariant;
    begin
      try
        if fServer.fDAOK and not fServer.fServerBusy
          then result := integer(fServer.WorldProxy.RDONewFacility( FacilityId, CompanyId, x, y ))
          else result := ERROR_Unknown;
      except
        fServer.RenewWorldProxy;
        result := ERROR_Unknown;
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in NewFacility' );
        except
        end;
      end;
    end;

  procedure TClientView.SayThis( Dest, Msg : widestring );
    begin
      try
        if not fBanned
          then
            begin
              if Length(Msg) > 200
                then
                  begin
                    inc(fLongChatLines);
                    Msg := copy(Msg, 1, 200);
                  end;
              fServer.ChatMsg( fUserName, CompositeName, Dest, Msg );
            end;
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in SayThis' );
        except
        end;
      end;
    end;

  procedure TClientView.VoiceThis( Msg : widestring; TxId, NewTx : integer );
    begin
      try
        fServer.VoiceMsg( CompositeName, Msg, TxId, NewTx );
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in VoiceThis' );
        except
        end;
      end;
    end;

  function TClientView.VoiceRequest( RequestId : integer ) : OleVariant;
    begin
      try
        fCurrChannel.fLock.Enter;
        try
          if fCurrChannel.fVoiceReqs.IndexOf( fUserName ) = NoIndex
            then result := fCurrChannel.fVoiceReqs.AddObject( fUserName, self );
          // SayThis( '', 'ReqIdx: ' + IntToStr(integer(result)) );
        finally
          fCurrChannel.fLock.Leave;
        end;
      except
        result := -1;
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in VoiceRequest' );
        except
        end;
      end;
    end;

  procedure TClientView.CancelVoiceRequest( RequestId : integer );
    var
      idx : integer;
    begin
      try
        fCurrChannel.fLock.Enter;
        try
          idx := fCurrChannel.fVoiceReqs.IndexOf( fUserName );
          if idx = 0
            then VoiceTxOver( RequestId )
            else fCurrChannel.fVoiceReqs.Delete( idx );
        finally
          fCurrChannel.fLock.Leave;
        end;
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in CancelVoiceRequest' );
        except
        end;
      end;
    end;

  procedure TClientView.VoiceTxOver( RequestId : integer );
    var
      CV : TClientView;
    begin
      try
        fCurrChannel.fLock.Enter;
        try
          if (fCurrChannel.fVoiceReqs.Count > 0) and (fCurrChannel.fVoiceReqs[0] = fUserName)
            then
              begin
                //SayThis( '', 'Count: ' + IntToStr(fCurrChannel.fVoiceReqs.Count) );
                fCurrChannel.fVoiceReqs.Delete( 0 );
                if fCurrChannel.fVoiceReqs.Count > 0
                  then
                    begin
                      CV := TClientView(fCurrChannel.fVoiceReqs.Objects[0]);
                      if fServer.fClients.IndexOf( CV ) <> NoIndex
                        then CV.VoiceRequestGranted;
                    end;
              end
            else Logs.Log('Survival', TimeToStr(Now) + ' - Voice request list inconsistence: ' + fUserName );
        finally
          fCurrChannel.fLock.Leave;
        end;
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in VoiceTxOver' );
        except
        end;
      end;
    end;

  procedure TClientView.VoiceStatusChanged( Status : integer );
    begin
      try
        fVoiceEnabled := not (Status = 0);
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in VoiceStatusChanged' );
        except
        end;
      end;
    end;

  procedure TClientView.MsgCompositionChanged( State : TMsgCompositionState );
    begin
      try
        if (State = mstAFK)
        then
          AFK := True
        else
          AFK := False;
        fServer.MsgCompositionChanged( UserName, State );
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in MsgCompositionChanged' );
        except
        end;
      end;
    end;

  function TClientView.CreateChannel( ChannelName, Password, aSessionApp, aSessionAppId : widestring; anUserLimit : integer ) : OleVariant;
    var
      Channel : TChannel;
    begin
      try
        Channel := fServer.GetChannel( ChannelName );
        if Channel = nil
          then
            begin
              fServer.ClientLeavedChannel( self, fCurrChannel );
              fServer.ClientCreatedChannel( self, ChannelName, Password, aSessionApp, aSessionAppId, anUserLimit );
              result := NOERROR;
            end
          else result := JoinChannel( ChannelName, Password );
      except
        result := ERROR_Unknown;
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in CreateChannel' );
        except
        end;
      end;
    end;

  function TClientView.JoinChannel( ChannelName, Password : widestring ) : OleVariant;
    var
      Channel : TChannel;
    begin
      try
        Channel := fServer.GetChannel( ChannelName );
        if fCurrChannel <> Channel
          then
            if (Channel = nil) or (uppercase(Channel.fPassword) = uppercase(Password))
              then
                if (Channel = nil) or (Channel.fUserLimit = 0) or (Channel.fMembers.Count < Channel.fUserLimit)
                  then
                    begin
                      fServer.ClientLeavedChannel( self, fCurrChannel );
                      fServer.ClientEnteredChannel( self, Channel );
                      result := NOERROR;
                    end
                  else result := ERROR_NotEnoughRoom
              else result := ERROR_InvalidPassword
          else result := NOERROR;
      except
        result := ERROR_Unknown;
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in JoinChannel' );
        except
        end;
      end
    end;

  function TClientView.LaunchChannelSession( ChannelName : widestring ) : OleVariant;
    begin
      try
        if fServer.LaunchChannelSession( self, ChannelName )
          then result := NOERROR
          else result := ERROR_Unknown;
      except
        result := ERROR_Unknown;
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in LaunchChannelSession' );
        except
        end;
      end;
    end;

  function TClientView.Chase( UserName : widestring ) : OleVariant;
    begin
      Lock( cvcsChasers );
      try
        try
          if (fTarget = nil) or (StopChase = NOERROR)
            then
              begin
                fTarget := fServer.GetClientByName( UserName );
                if (fTarget <> nil) and (fTarget <> self) and (fChasers.IndexOf( fTarget ) = NoIndex)
                  then
                    begin
                      fTarget.fChasers.Insert( self );
                      MoveTo( (fTarget.x1 + fTarget.x2) div 2 - abs(x2 - x1) div 2, (fTarget.y1 + fTarget.y2) div 2 - abs(y2 - y1) div 2 );
                      result := NOERROR;
                    end
                  else result := ERROR_InvalidUserName
              end
            else result := ERROR_Unknown;
        except
          result := ERROR_Unknown;
          try
            Logs.Log('Survival', TimeToStr(Now) + ' - Error in Chase' );
          except
          end;
        end;
      finally
        Unlock( cvcsChasers );
      end;
    end;

  function TClientView.StopChase : OleVariant;
    begin
      Lock( cvcsChasers );
      try
        try
          if fTarget <> nil
            then
              begin
                fTarget.fChasers.Delete( self );
                fTarget := nil;
                result := NOERROR;
              end
            else result := ERROR_Unknown;
        except
          result := ERROR_Unknown;
          try
            Logs.Log('Survival', TimeToStr(Now) + ' - Error in StopChase' );
          except
          end;
        end;
      finally
        Unlock( cvcsChasers );
      end;
    end;

  function TClientView.GetUserList : OleVariant;
    begin
      try
        result := fServer.GetUserList( fCurrChannel );
      except
        result := ERROR_Unknown;
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in GetUserList' );
        except
        end;
      end;
    end;

  function TClientView.GetChannelList( Root : widestring ) : OleVariant;
    begin
      try
        result := fServer.GetChannelList;
      except
        result := ERROR_Unknown;
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in GetChannelList' );
        except
        end;
      end;
    end;

  function TClientView.GetChannelInfo( Name : widestring ) : OleVariant;
    begin
      try
        result := fServer.GetChannelInfo(Name, fLangId);
      except
        result := ERROR_Unknown;
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in GetChannelInfo' );
        except
        end;
      end;
    end;

  function TClientView.ISStatus : OleVariant;
    begin
      result := NOERROR;
    end;

  function TClientView.ClientViewId : OleVariant;
    begin
      result := fId;
    end;

  procedure TClientView.ClientAware;
    begin
      if not fAware
        then
          begin
            fAware := true;
            fServer.SystemMsg('', mtidClientAware, [UserName, fServer.WorldName]); // [1]
            try
              if fServer.fDAOK
                then fServer.WorldProxy.RDOAwakeTycoon( fTycoonId );
            except
              fServer.RenewWorldProxy;
              try
                Logs.Log('Survival', TimeToStr(Now) + ' - Error in ClientAware' );
              except
              end;
            end;
          end;
    end;

  procedure TClientView.ClientNotAware;
    begin
      if fAware
        then
          try
            fAware := false;
            fServer.SystemMsg('', mtidClientUnAware, [UserName, fServer.WorldName]); // [2]
          except
            try
              Logs.Log('Survival', TimeToStr(Now) + ' - Error in ClientNotAware' );
            except
            end;
          end;
    end;

  procedure TClientView.SetLanguage( langid : widestring );
    begin
      try
        fLangId := langid;
        fTycoonProxy.Language := langid;
      except
        //fServer.DropDAConnection(fTycoonProxy, fConnection);
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in SetLanguage' );
        except
        end;
      end;
    end;

  function TClientView.RDOFavoritesNewItem( Location : widestring; Kind : integer; Name, Info : widestring ) : OleVariant;
    var
      loc : widestring;
    begin
      Lock( cvcsViewport );
      try
        if not VarIsEmpty(fTycoonProxy)
          then
            begin
              loc := Location;
              result := integer(fTycoonProxy.RDOFavoritesNewItem( loc, Kind, Name, Info ));
            end
          else result := 0;
      finally
        Unlock( cvcsViewport );
      end;
    end;

  function TClientView.RDOFavoritesDelItem( Location : widestring ) : OleVariant;
    begin
      Lock( cvcsViewport );
      try
        if not VarIsEmpty(fTycoonProxy)
          then result := integer(fTycoonProxy.RDOFavoritesDelItem( Location ))
          else result := 0;
      finally
        Unlock( cvcsViewport );
      end;
    end;

  function TClientView.RDOFavoritesMoveItem( ItemLoc : widestring; Dest : widestring ) : OleVariant;
    begin
      Lock( cvcsViewport );
      try
        if not VarIsEmpty(fTycoonProxy)
          then result := integer(fTycoonProxy.RDOFavoritesMoveItem( ItemLoc, Dest ))
          else result := 0;
      finally
        Unlock( cvcsViewport );
      end;
    end;

  function TClientView.RDOFavoritesRenameItem( ItemLoc : widestring; Name : widestring ) : OleVariant;
    begin
      Lock( cvcsViewport );
      try
        if not VarIsEmpty(fTycoonProxy)
          then result := integer(fTycoonProxy.RDOFavoritesRenameItem( ItemLoc, Name ))
          else result := 0;
      finally
        Unlock( cvcsViewport );
      end;
    end;

  function TClientView.RDOFavoritesGetSubItems( ItemLoc : widestring ) : OleVariant;
    begin
      Lock( cvcsViewport );
      try
        if not VarIsEmpty(fTycoonProxy)
          then result := widestring(fTycoonProxy.RDOFavoritesGetSubItems( ItemLoc ))
          else result := '';
      finally
        Unlock( cvcsViewport );
      end;
    end;

  procedure TClientView.OnDisconnect(const ClientConnection : IRDOConnection);
    begin
      try
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Start Disconnecting ' + UserName);
        except
          Logs.Log('Survival', TimeToStr(Now) + ' - Start Disconnecting ????');
        end;
        fConnected    := false;
        fEnableEvents := false;
        DoLogoff;
        Logs.Log('Survival', TimeToStr(Now) + ' - End Disconnecting');
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in OnDisconnect' );
        except
        end;
      end;
    end;

  function TClientView.RegisterEvents( ClientAddress : widestring; ClientPort : integer ) : OleVariant;

    procedure SendClientData;
      var
        Date         : TDateTime;
        TempMoney    : currency;
        Money        : widestring;
        FailureLevel : integer;
      begin
        if fServer.fDAOK
          then
            try
              Date      := TDateTime(fServer.WorldProxy.VirtualTime);
              TempMoney := fTycoonProxy.RDOGetBudget;
              Money     := CurrToStr(int(TempMoney));
              FailureLevel := integer(fTycoonProxy.FailureLevel);
              fClientEventsProxy.InitClient( Date, Money, FailureLevel, fTycoonProxyId );
            except
              Logs.Log('Survival', TimeToStr(Now) + ' - Error in RegisterEvents.SendClientData' );
            end;
      end;

    begin
      {
      try
        fClientConnection        := TWinSockRDOConnection.Create;
        fClientConnection.Server := ClientAddress;
        fClientConnection.Port   := ClientPort;
        fClientEventsProxy       := TRDOObjectProxy.Create as IDispatch;
        if fClientConnection.Connect( 10000 )
          then
            begin
              fClientEventsProxy.SetConnection( fClientConnection );
              fClientEventsProxy.BindTo( tidRDOHook_InterfaceEvents );
              EnableEvents := true;
              SendClientData;
              ReportNewMail( fServer.CountUnreadMessages( fUserName ));
              EnableEvents := false;
            end
          else raise Exception.Create( '' );
        result := NOERROR;
      except
        result := ERROR_CannotSetupEvents;
      end;
      }
      try
        try
          fClientConnection := fServer.fClientsServerConn.GetClientConnection( ClientAddress, ClientPort );
          fClientConnection.OnDisconnect := OnDisconnect;
          fClientEventsProxy := TRDOObjectProxy.Create as IDispatch;
          fClientEventsProxy.SetConnection( fClientConnection );
          fClientEventsProxy.BindTo( tidRDOHook_InterfaceEvents );
          EnableEvents := true;
          fConnected   := true;
          SendClientData;
          ReportNewMail( fServer.CountUnreadMessages( fUserName ));
          EnableEvents := false;
          result := NOERROR;
          if fClientConnection <> nil
            then
              begin
                Logs.Log('Survival', TimeToStr(Now) + ' ' + fRealName + '.IP = ' + fClientConnection.RemoteAddress);
                fIPAddr    := fClientConnection.RemoteAddress;
                fEnterDate := Now;
              end;
        except
          result := ERROR_CannotSetupEvents;
        end;
      finally
      end;
    end;

  function TClientView.RegisterEventsById( ClientId : integer ) : OleVariant;

    procedure SendClientData;
      var
        Date         : TDateTime;
        TempMoney    : currency;
        Money        : widestring;
        FailureLevel : integer;
      begin
        try
          if fServer.fDAOK
            then Date := TDateTime(fServer.WorldProxy.VirtualTime);
          if not VarIsEmpty(fTycoonProxy)
            then
              begin
                TempMoney := fTycoonProxy.RDOGetBudget;
                Money := CurrToStr(int(TempMoney));
                FailureLevel := integer(fTycoonProxy.FailureLevel);
              end;
          fClientEventsProxy.InitClient( Date, Money, FailureLevel, fTycoonProxyId );
        except
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in RegisterEvents.SendClientData' );
        end;
      end;

    begin
      try
        try
          fClientConnection := fServer.fClientsServerConn.GetClientConnectionById( ClientId  );
          fClientConnection.OnDisconnect := OnDisconnect;
          fClientEventsProxy := TRDOObjectProxy.Create as IDispatch;
          fClientEventsProxy.SetConnection( fClientConnection );
          fClientEventsProxy.BindTo( tidRDOHook_InterfaceEvents );
          EnableEvents := true;
          fConnected   := true;
          SendClientData;
          ReportNewMail(fServer.CountUnreadMessages(fUserName));
          EnableEvents := false;
          result := NOERROR;
          if fClientConnection <> nil
            then
              begin
                Logs.Log('Survival', TimeToStr(Now) + ' ' + fRealName + '.IP = ' + fClientConnection.RemoteAddress);
                fIPAddr    := fClientConnection.RemoteAddress;
                fEnterDate := Now;
              end;
        except
          result := ERROR_CannotSetupEvents;
        end;
      finally
      end;
    end;

  procedure TClientView.SetClientData(data : widestring);
    begin
      fClientData.Text := data;
    end;

  procedure TClientView.DoLogOff;

    procedure StopChasers;
      begin
        try
          fChasers.Lock;
          try
            while fChasers.Count > 0 do
              (fChasers[0] as TClientView).StopChase;
          finally
            fChasers.Unlock;
          end;
        except
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in TClientView.Logoff.StopChasers' );
        end;
      end;

    var
      i         : integer;
      tmpServer : TInterfaceServer;
    begin
      fConnected := false;
      try
        try
          // Log user data
          Logs.Log('Clients', fRealName + #9 + fIPAddr + #9 + TimeToStr(fEnterDate) + #9 + TimeToStr(Now) + #9 + fClientData.Values['CRC']);
          // Get rid of the notifications
          Logs.Log('Survival', '(1)');
          fClientEventsProxy := Unassigned;
          Logs.Log('Survival', '(2)');
          // Get rid of the events
          if fClientConnection <> nil
            then fClientConnection.OnDisconnect := nil;
          Logs.Log('Survival', '(3)');
          // Get rid of the connection
          fClientConnection := nil;
          Logs.Log('Survival', '(4)');
        except
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in TClientView.DoLogoff (1)' );
        end;
        Logs.Log('Survival', '(5)');
        tmpServer := fServer;
        for i := 0 to pred(fFocused.Count) do
          try
            if not VarIsEmpty(tmpServer.WorldProxy)
              then tmpServer.WorldProxy.RDOFacilityLostFocus( integer(fFocused[i]) );
          except
            //fServer.DropDAConnection(tmpServer.WorldProxy, tmpServer.fDAClientConn);
            Logs.Log('Survival', TimeToStr(Now) + ' - Error in TClientView.DoLogoff (' + IntToStr(i) + ')' );
          end;
        Logs.Log('Survival', '(6)');
        tmpServer.ClientLeavedChannel( self, fCurrChannel );
        Logs.Log('Survival', '(7)');
        StopChasers;
        Logs.Log('Survival', '(8)');
        SetViewedArea( 0, 0, 0, 0 );
        Logs.Log('Survival', '(9)');
        ClientNotAware;
        Logs.Log('Survival', '(10)');
        // Bye bye everybody!!!
        tmpServer.Logoff( self );
        Logs.Log('Survival', '(11)');
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in DoLogoff' );
        except
        end;
      end;
    end;

  function TClientView.Logoff : OleVariant;
    begin
      result := NOERROR;
    end;

  {
  procedure TClientView.RefreshArea( x, y, dx, dy : integer );
    var
      useless    : TRect;
      ObjectInfo : string;
      RoadInfo   : string;
      RailInfo   : string;
      xe, ye     : integer;
      ExtraInfo  : string;
    begin
      try
        if fConnected and fEnableEvents and not VarIsEmpty(fClientEventsProxy) and IntersectRect( useless, Bounds( x, y, dx, dy ), Rect( fx1, fy1, fx2, fy2 ))
          then
            begin
              xe := MapChunkSize*(x div MapChunkSize);
              ye := MapChunkSize*(y div MapChunkSize);
              if (x + dx < xe + MapChunkSize) and (y + dy < ye + MapChunkSize)
                then
                  begin
                    ObjectInfo := string(ObjectsInArea( xe, ye, MapChunkSize, MapChunkSize ));
                    RoadInfo   := string(SegmentsInArea( cirRoads, xe, ye, xe + MapChunkSize, ye + MapChunkSize ));
                    RailInfo   := string(SegmentsInArea( cirRailRoads, xe, ye, xe + MapChunkSize, ye + MapChunkSize ));
                    ExtraInfo := '1' + RepSeparator + ObjectInfo + RepSeparator + RoadInfo + RepSeparator + RailInfo;
                  end
                else ExtraInfo := '0';
              fClientEventsProxy.RefreshArea( x, y, dx, dy, ExtraInfo );
            end;
      except
      end;
    end;
  }

  procedure TClientView.RefreshArea( x, y, dx, dy : integer; ExtraInfo : string );
    var
      useless : TRect;
    begin
      //LogThis('Survival', '(RefreshArea.1)', 0);
      try
        if fConnected and fEnableEvents and not VarIsEmpty(fClientEventsProxy) and IntersectRect( useless, Bounds( x, y, dx, dy ), Rect( fx1, fy1, fx2, fy2 ))
          then fClientEventsProxy.RefreshArea( x, y, dx, dy, ExtraInfo );
      except
        try
          //LogThis('Survival', TimeToStr(Now) + ' - Error in RefreshArea', 0);
        except
        end;
      end;
      //LogThis('Survival', '(RefreshArea.2)', 0);
    end;

  function TClientView.RefreshObject( ObjId, KindOfChange : integer ) : boolean;
    var
      ExtraInfo : string;
    begin
      result := false;
      //LogThis('Survival', '(RefreshObject.1)', 0);
      try
        if fConnected and fEnableEvents and not VarIsEmpty(fClientEventsProxy) and (fFocused.IndexOf( TObject(ObjId) ) <> NoIndex)
          then
            begin
              result := true;
              if TFacilityChange(KindOfChange) = fchStatus
                then ExtraInfo := string(AllObjectStatusText( ObjId, fTycoonProxyId ))
                else ExtraInfo := '';
              fClientEventsProxy.RefreshObject( ObjId, KindOfChange, ExtraInfo );
            end;
      except
        try
          //LogThis('Survival', TimeToStr(Now) + ' - Error in RefreshObject', 0 );
        except
        end;
      end;
      //LogThis('Survival', '(RefreshObject.2)', 0);
    end;

  procedure TClientView.RefreshTycoon;
    var
      Money, NetProfit : widestring;
      Ranking, FacCount, FacMax : integer;
    begin
      //LogThis('Survival', '(RefreshTycoon.1)', 0);
      try  // @@
        if fConnected and fEnableEvents and not VarIsEmpty(fClientEventsProxy)
          then
            begin
              Money := FloatToStr(int(fTycoonProxy.RDOGetBudget));
              NetProfit := FloatToStr(int(fTycoonProxy.NetProfit));
              Ranking := fTycoonProxy.Ranking;
              FacCount := fTycoonProxy.FacCount;
              FacMax := fTycoonProxy.FacMax;
              fClientEventsProxy.RefreshTycoon(Money, NetProfit, Ranking, FacCount, FacMax);
            end;
      except
        on E : Exception do
          try
            Logs.Log('Survival', TimeToStr(Now) + ' - Error in RefreshTycoon: ' + E.Message);
          except
          end;
      end;
      //LogThis('Survival', '(RefreshTycoon.2)', 0);
    end;

  procedure TClientView.RefreshDate( Date : TDateTime );
    begin
      //LogThis('Survival', '(RefreshDate.1)', 0);
      try
        if fConnected and fEnableEvents and not VarIsEmpty(fClientEventsProxy)
          then fClientEventsProxy.RefreshDate( Date );
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in RefreshDate' );
        except
        end;
      end;
      //LogThis('Survival', '(RefreshDate.2)', 0);
    end;

  procedure TClientView.RefreshSeason( Season : integer );
    begin
      //LogThis('Survival', '(Season.1)', 0);
      try
        if fConnected and fEnableEvents and not VarIsEmpty(fClientEventsProxy)
          then fClientEventsProxy.RefreshSeason( Season );
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in RefreshSeason' );
        except
        end;
      end;
      //LogThis('Survival', '(Season.2)', 0);
    end;

  procedure TClientView.EndOfPeriod;
    begin
      //LogThis('Survival', '(EndOfPeriod.1)', 0);
      try
        if fConnected and fEnableEvents and not VarIsEmpty(fClientEventsProxy)
          then fClientEventsProxy.EndOfPeriod( integer(fTycoonProxy.FailureLevel) );
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in EndOfPeriod' );
        except
        end;
      end;
      //LogThis('Survival', '(EndOfPeriod.2)', 0);
    end;

  procedure TClientView.TycoonRetired;
    begin
      //LogThis('Survival', '(TycoonRetired.1)', 0);
      try
        if fConnected and fEnableEvents and not VarIsEmpty(fClientEventsProxy)
          then fClientEventsProxy.TycoonRetired( 0 );
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in TycoonRetired' );
        except
        end;
      end;
      //LogThis('Survival', '(TycoonRetired.2)', 0);
    end;

  procedure TClientView.SendTickData( PoolId, TickCount : integer; TickData : widestring );
    begin
      try
        if fConnected and fEnableEvents and not VarIsEmpty(fClientEventsProxy)
          then fClientEventsProxy.ActorPoolModified( PoolId, TickData );
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in SendTickData' );
        except
        end;
      end
    end;

  procedure TClientView.SendNotification( Kind : integer; Title, Body : string; Options : integer );
    begin
      try
        if fConnected and fEnableEvents and not VarIsEmpty(fClientEventsProxy)
          then fClientEventsProxy.ShowNotification( Kind, Title, Body, Options );
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in SendNotification' );
        except
        end;
      end
    end;

  procedure TClientView.ModelStatusChanged( Status : integer );
    begin
    end;

  procedure TClientView.MoveTo( x, y : integer );
    begin
      try
        if fConnected and fEnableEvents and not VarIsEmpty(fClientEventsProxy)
          then fClientEventsProxy.MoveTo( x, y );
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in MoveTo' );
        except
        end;
      end;
    end;

  procedure TClientView.ListenSystemMsg(Msg : TRegMultiString; values : array of const);
    begin
      try
        if fConnected and fEnableEvents and not VarIsEmpty(fClientEventsProxy)
          then fClientEventsProxy.ChatMsg(mtidSystem.Values[fLangId], Format(Msg.Values[fLangId], values));
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in ListenSystemMsg' );
        except
        end;
      end;
    end;

  procedure TClientView.HearThis( From, Msg : string );
    begin
      try
        if fConnected and fEnableEvents and not VarIsEmpty(fClientEventsProxy)
          then fClientEventsProxy.ChatMsg( From, Msg );
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in HearThis' );
        except
        end;
      end;
    end;

  procedure TClientView.HearThisVoice( From, Msg : string; TxId, NewTx : integer );
    begin
      try
        if fConnected and fEnableEvents and not VarIsEmpty(fClientEventsProxy)
          then fClientEventsProxy.VoiceMsg( From, Msg, TxId, NewTx );
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in HearThisVoice' );
        except
        end;
      end;
    end;

  procedure TClientView.VoiceRequestGranted;
    begin
      try
        if fConnected and fEnableEvents and not VarIsEmpty(fClientEventsProxy)
          then fClientEventsProxy.VoiceRequestGranted( 1 );
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in VoiceRequestGranted' );
        except
        end;
      end;
    end;

  procedure TClientView.ReportNewMail( MsgCount : integer );
    begin
      try
        if fConnected and (MsgCount > 0) and fEnableEvents and not VarIsEmpty(fClientEventsProxy)
          then fClientEventsProxy.NewMail( MsgCount );
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in ReportNewMail' );
        except
        end;
      end;
    end;

  {
  procedure TClientView.syncRegEvents;

    procedure SendClientData;
      var
        Date         : TDateTime;
        TempMoney    : currency;
        Money        : widestring;
        FailureLevel : integer;
      begin
        try
          Lock;
          try
            Date      := TDateTime(fServer.WorldProxy.VirtualTime);
            TempMoney := fTycoonProxy.RDOGetBudget;
            Money     := CurrToStr(int(TempMoney));
            FailureLevel := integer(fTycoonProxy.FailureLevel);
            fClientEventsProxy.InitClient( Date, Money, FailureLevel, fTycoonProxyId );
          finally
            Unlock;
          end;
        except
        end;
      end;

    begin
      Lock;
      try
        try
          try
            fClientConnection := fServer.fClientsServerConn.GetClientConnection( syncClientAddress, syncClientPort );
            fClientEventsProxy := TRDOObjectProxy.Create as IDispatch;
            fClientEventsProxy.SetConnection( fClientConnection );
            fClientEventsProxy.BindTo( tidRDOHook_InterfaceEvents );
            SendClientData;
            syncRegEventsResult := NOERROR;
          except
            syncRegEventsResult := ERROR_CannotSetupEvents;
          end;
        finally
          syncRegEventsDone.SetEvent;
        end;
      finally
        Unlock;
      end;
    end;
  }

  function TClientView.FindCompanionship : string;
    var
      Client   : TClientView;
      DestRect : TRect;
      i        : integer;
    begin
      Lock( cvcsViewport );
      try
        fServer.LockClients('FindCompanionship');
        try
          result := '';
          for i := 0 to pred(fServer.Clients.Count) do
            if fServer.Clients[i] <> self
              then
                begin
                  Client := TClientView(fServer.Clients[i]);
                  if IntersectRect( DestRect, Rect(fx1, fy1, fx2, fy2), Rect(Client.fx1, Client.fy1, Client.fx2, Client.fy2))
                    then
                      if result <> ''
                        then result := result + #13#10 + Client.UserName
                        else result := Client.UserName;
                end
        finally
          fServer.UnlockClients('FindCompanionship');
        end;
      finally
        Unlock( cvcsViewport );
      end;
    end;

  procedure TClientView.NotifyCompanionship;
    var
      NewCompanionship : string;
    begin
      try
        Lock( cvcsCompanionship ); // >>
        try
          try
            NewCompanionship := FindCompanionship;
            if fCompanionship <> NewCompanionship
              then
                begin
                  fCompanionship := NewCompanionship;
                  if fEnableEvents and not VarIsEmpty(fClientEventsProxy)
                    then fClientEventsProxy.NotifyCompanionship( FindCompanionship );
                end;
          except
          end;
        finally
          Unlock( cvcsCompanionship );
        end;
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in NotifyCompanionship' );
        except                                                    
        end;
      end;
    end;

  procedure TClientView.NotifyUserListChange( Name : widestring; Change : TUserListChange );
    begin
      try
        if fConnected and fEnableEvents and not VarIsEmpty(fClientEventsProxy)
          then fClientEventsProxy.NotifyUserListChange( Name, integer(Change) );
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in NotifyUserListChange' );
        except
        end;
      end;
    end;

  procedure TClientView.NotifyChannelListChange( Name, Password : widestring; Change : TUserListChange );
    begin
      try
        if fConnected and fEnableEvents and not VarIsEmpty(fClientEventsProxy)
          then fClientEventsProxy.NotifyChannelListChange( Name, Password, integer(Change) );
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in NotifyChannelListChange' );
        except
        end;
      end;
    end;

  procedure TClientView.NotifyMsgCompositionState( Name : widestring; State : TMsgCompositionState );
    begin
      try
        if fConnected and fEnableEvents and not VarIsEmpty(fClientEventsProxy)
          then fClientEventsProxy.NotifyMsgCompositionState( Name, integer(State) );
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in NotifyMsgCompositionState' );
        except
        end;
      end;
    end;

  procedure TClientView.NotifyChannelChange( Name : widestring );
    begin
      try
        if fConnected and fEnableEvents and not VarIsEmpty(fClientEventsProxy)
          then fClientEventsProxy.NotifyChannelChange( Name );
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in NotifyChannelChange' );
        except
        end;
      end;
    end;

  {function TClientView.Alive : boolean;
    begin
      result := fConnected;
    end;}

  {procedure TClientView.CheckState;
    begin
      if Alive
        then fTimeOut := 0
        else
          if fTimeOut < ClientTimeOut
            then inc( fTimeOut )
            else Logoff;
    end;}

  procedure TClientView.Lock( CriticalSection : TClientViewCriticalSection );
    begin
      fLock[CriticalSection].Enter;
    end;

  procedure TClientView.Unlock( CriticalSection : TClientViewCriticalSection );
    begin
      fLock[CriticalSection].Leave;
    end;

  function TClientView.CanonicalSquare( x, y, dx, dy : integer ) : boolean;
    begin
      result := (x mod MapChunkSize = 0) and (x mod MapChunkSize = 0) and (dx = MapChunkSize) and (dy = MapChunkSize);
    end;

  function TClientView.ConnectToGameMaster( ClientId : TCustomerId; UserInfo, GameMasters : widestring ) : OleVariant;
    var
      res : TStringList;
    begin
      result := fServer.ConnectToGameMaster( ClientId, UserInfo, GameMasters );
      res    := TStringList.Create;
      try
        res.Text := string(result);
        try
          fGMId := StrToInt( res.Values['GameMaster'] );
        except
          fGMId := INVALID_GAMEMASTER;
        end;
      finally
        res.Free;
      end;
    end;

  function TClientView.SendGMMessage( ClientId : TCustomerId; GMId : TGameMasterId; Msg : WideString ) : OleVariant;
    begin
      result := fServer.SendGMMessage( ClientId, GMId, Msg );
    end;

  procedure TClientView.DisconnectUser( ClientId : TCustomerId; GMId : TGameMasterId );
    begin
      fServer.DisconnectUser( ClientId, GMId );
    end;

  procedure TClientView.GameMasterMsg( Msg : WideString; Info : integer );
    begin
      if fConnected and fEnableEvents and not VarIsEmpty(fClientEventsProxy)
        then fClientEventsProxy.GameMasterMsg( Msg, Info );
    end;

  procedure TClientView.GMNotify( notID : integer; Info : WideString );
    begin
      if fConnected and fEnableEvents and not VarIsEmpty(fClientEventsProxy)
        then fClientEventsProxy.GMNotify( notID, Info );
    end;

  function TClientView.IsRole : boolean;
    begin
      result := fRealName <> fUserName;
    end;

  function TClientView.GetServerBusy : boolean;
    begin
      try
        result := fServer.ServerBusy;
      except
        result := false;
      end;
    end;

  // TMapCache

  constructor TMapCache.Create( xSize, ySize : integer );
    begin
      inherited Create;
      fxSize := xSize div MapChunkSize + 1;
      fySize := ySize div MapChunkSize + 1;
      getmem( fCache, fxSize*fySize*sizeof(fCache[0]) );
      initialize( fCache^, fxSize*fySize );
      fLock := TAsymetrixCriticalSection.Create;
    end;

  destructor TMapCache.Destroy;
    begin
      fLock.Destroy;
      finalize( fCache^, fxSize*fySize );
      freemem( fCache, fxSize*fySize*sizeof(fCache[0]) );
      inherited;
    end;

  function TMapCache.GetItem( x, y : integer ) : widestring;
    begin
      fLock.BeginRead( INFINITE );
      try
        result := fCache[fxSize*(y div MapChunkSize) + (x div MapChunkSize)];
      finally
        fLock.EndRead;
      end;
    end;

  procedure TMapCache.SetItem( x, y : integer; value : widestring );
    begin
      fLock.BeginWrite( INFINITE );
      try
        fCache[fxSize*(y div MapChunkSize) + (x div MapChunkSize)] := value;
      finally
        fLock.EndWrite;
      end;
    end;
    

  // TRefreshThread

  constructor TRefreshThread.Create( aServer : TInterfaceServer );
    begin
      inherited Create( true );
      fEndEvn := TEvent.Create( nil, true, false, '' );
      fServer := aServer;
      Resume;
    end;

  destructor TRefreshThread.Destroy;
    begin
      fEndEvn.Free;
      inherited;
    end;

  procedure TRefreshThread.Execute;
    begin
      while not Terminated do
        if (fEndEvn.WaitFor( RefreshRate ) = wrTimeout) and not Terminated
          then
            try
              fServer.OnSentinel( nil );
              fServer.OnRefresh( nil );
              if fServer.fServerError
                then
                  begin
                    fServer.ChatMsg( 'SYSTEM', 'SYSTEM', '', 'WARNING: Some data is corrupted on the servers.' );
                    fServer.ChatMsg( 'SYSTEM', 'SYSTEM', '', '             You might lose your changes from now on.' );
                    fServer.ChatMsg( 'SYSTEM', 'SYSTEM', '', '             We recommend you to log off.' );
                  end;
              if fServer.fServerBusy and not fServer.MaintDue
                then fServer.SystemMsg('', mtidServerBusy, ['']);
            except
            end;
    end;


  // TInterfaceServer

  constructor TInterfaceServer.Create( aMSAddress, MailAddr, aDSAddress, aGMAddress, aDSArea : string; aMSPort, aMSEventsPort, MailPort, aDSPort, aGMPort, aClientPort : integer; FixAddr, BanListFile : string );
    begin
      inherited Create;
      fServerLock  := TCriticalSection.Create;
      fDAProxyLock := TCriticalSection.Create;
      fClients     := TLockableCollection.Create( 0, rkBelonguer );
      fHomeChannel := TChannel.Create( '', '', '', '', '', 0 );
      fModelEvents := TModelEvents.Create( self );
      try
        // Clients RDO
        fClientPort := aClientPort;
        fClientsServerConn := TWinSockRDOConnectionsServer.Create( fClientPort ); // >>
        fClientsRDO := TRDOServer.Create( fClientsServerConn as IRDOServerConnection, ISMaxThreads, nil );
        fClientsRDO.RegisterObject( tidRDOHook_InterfaceServer, integer(self) );
        //fClientsRDO.SetCriticalSection( fGlobalLock );

        // DA RDO
        fDACnntPool          := TRDOConnectionPool.Create;
        fDAClientConn        := TWinSockRDOConnection.Create( 'DA' );
        fDAClientConn.Server := aMSAddress;
        fDAClientConn.Port   := aMSPort;
        fDAAddr              := aMSAddress;
        fDAPort              := aMSPort;
        fDALockPort          := fDAPort + 1;
        (fDAClientConn as IRDOConnection).OnDisconnect := OnDADisconnect;
        fDAProxy := TRDOObjectProxy.Create as IDispatch;
        if fDAClientConn.Connect( 10000 )
          then
            begin
              fDAProxy.SetConnection( fDAClientConn );
              fDAProxy.BindTo( tidRDOHook_World );
              fDAProxy.TimeOut := DATimeOut;
              fDAOK := true;
              fUserCount := fDAProxy.UserCount;
            end
          else raise Exception.Create( '' );
        //fDACnntPool.AddConnection(fDAClientConn);
        InitDAConnectionPool;
        fDAServerConn := TWinSockRDOConnectionsServer.Create( aMSEventsPort ); // >>
        fDAEventsRDO  := TRDOServer.Create( fDAServerConn as IRDOServerConnection, 1, nil );
        //fDAEventsRDO.SetCriticalSection( fGlobalLock );
        //fDAEventsRDO.RegisterObject( tidRDOHook_InterfaceEvents, integer(fModelEvents) );
        //fDAServerConn.StartListening;
        fWorldName  := fDAProxy.Name;
        fWorldURL   := fDAProxy.WorldURL;
        fMaxUserCount := fDAProxy.MaxInvestors;
        fWorldXSize := fDAProxy.xSize;
        fWorldYSize := fDAProxy.ySize;

        fLocalAddr := Trim( FixAddr );
        InsertInMasterIndex;

        // Start Server
        fDAEventsRDO.RegisterObject( tidRDOHook_InterfaceEvents, integer(fModelEvents) );
        fDAServerConn.StartListening;

        // Register to MS events
        fDAProxy.RDORegisterIS( fLocalAddr, aMSEventsPort );

        {
        fSentinel := TTimer.Create( nil );
        fRefresh  := TTimer.Create( nil );
        fSentinel.OnTimer  := OnSentinel;
        fSentinel.Interval := ClientsCheckRate;
        fRefresh.OnTimer   := OnRefresh;
        fRefresh.Interval  := RefreshRate;
        }

        // Initialize caches
        fObjectCache := TMapCache.Create( fWorldXSize, fWorldYSize );
        fRoadsCache  := TMapCache.Create( fWorldXSize, fWorldYSize );
        fRefreshThread := TRefreshThread.Create( self );

        // Initialize Channels
        fChannels := TLockableCollection.Create( 0, rkBelonguer );
        fSessions := TSessionServer.Create( aClientPort + 1 );

        InitMailServer( MailAddr, MailPort);

        InitGMServer( aGMAddress, aGMPort );

        InitDSServer( aDSAddress, aDSPort, aDSArea );

        // Init Client Server
        fClientsServerConn.StartListening;

        fSeason := 2; // Summer by default
        fMinNobility := -1;

        fBanList := TStringList.Create;

        // Create Channels
        CreateTownChannels;
        try
          //fBanList.LoadFromFile(BanListFile);
        except
        end;
      except
        fClients.Free;
        fModelEvents.Free;
        raise
      end;
    end;

  destructor TInterfaceServer.Destroy;
    begin
      fSessions.Free;
      fRefreshThread.Terminate;
      fRefreshThread.fEndEvn.SetEvent;
      fRefreshThread.WaitFor;
      fRefreshThread.Free;
      fRoadsCache.Free;
      fObjectCache.Free;
      if fDSOK and not VarIsEmpty(fDSProxy) and not VarIsNull(fDSProxy)
        then
          try
            fDSProxy.RDOEndSession;
          except
          end;
      {
      fSentinel.Free;
      fRefresh.Free;
      }
      //DeleteFromMasterIndex;
      fClients.Free;
      fDAEventsRDO.Free;
      fModelEvents.Free;
      // fDAEventsRDO.Free;
      fDAClientConn := nil;
      fDAProxy := NULL;
      fClientsRDO.Free;
      fServerLock.Free;
      fChannels.Free;
      fHomeChannel.Free;
      inherited;
    end;

  procedure TInterfaceServer.Lock(comm : string);
    begin
      LogThis('Survival', comm, 1);
      fServerLock.Enter;
      LogThis('Survival', '{', 0);
    end;

  procedure TInterfaceServer.Unlock(comm : string);
    begin
      fServerLock.Leave;
      LogThis('Survival', '}', 2);
    end;

  procedure TInterfaceServer.LockClients(comm : string);
    begin
      LogThis('Survival', comm + '.clients', 1);
      fClients.Lock;
      LogThis('Survival', '  [', 0);
    end;

  procedure TInterfaceServer.UnlockClients(comm : string);
    begin
      fClients.Unlock;
      LogThis('Survival', '  ]', 2);
    end;

  function TInterfaceServer.GetDAConnection : IRDOConnectionInit;
    var
      Cnx : IRDOConnectionInit;
    begin
      try
        InterlockedIncrement(fReconnects);
        Cnx := TWinSockRDOConnection.Create( 'DA' + IntToStr(fReconnects));
        Cnx.Server := fDAAddr;
        Cnx.Port   := fDAPort;
        (Cnx as IRDOConnection).OnDisconnect := OnDADisconnect;
        if Cnx.Connect( 10000 )
          then
            begin
              inc(fMSDownCount);
              result := Cnx as IRDOConnectionInit;
            end
          else
            begin
              if fMSDownCount > 0
                then dec(fMSDownCount);
              result := nil;
            end;
      except
        on e : Exception do
          begin
            Logs.Log('Survival', TimeToStr(Now) + ': Error: ' + e.Message);
            result := nil;
          end;
      end;
    end;

  procedure TInterfaceServer.InitDAConnectionPool;
    var
      i : integer;
    begin
      for i := 1 to pred(MaxDAPoolCnx) do
        fDACnntPool.AddConnection(GetDAConnection);
    end;

(*
  procedure TInterfaceServer.DropDAConnection(Proxy : OleVariant; var Connection : IRDOConnectionInit);
    begin
      Logs.Log('Survival', TimeToStr(Now) + ': Renewing connection. #' + IntToStr(integer(Connection)));
      Connection := GetDAConnection;
      if Connection <> nil
        then
          try
            Proxy.SetConnection(Connection);
            Proxy.TimeOut := DATimeOut;
            Logs.Log('Survival', TimeToStr(Now) + ': Renewing connection OK. #' + IntToStr(integer(Connection)));
          except
            on e : Exception do
              Logs.Log('Survival', TimeToStr(Now) + ': ERROR in Proxy.SetConnection. ' + e.Message);
          end
        else Logs.Log('Survival', TimeToStr(Now) + ': Failing to get connection.');
    end;
*)

  procedure TInterfaceServer.CheckDAConnections;
    var
      i   : integer;
      cnt : integer;
    begin
      fDACnntPool.Lock;
      try
        cnt := fDACnntPool.ConnetionCount;
        i   := 0;
        while (i < cnt) and (fDACnntPool.Connections[i].Connection <> nil) do
          inc(i);
        if i < cnt
          then fDACnntPool.SetConnection(i, GetDAConnection);
      finally
        fDACnntPool.Unlock;
      end;
    end;

  procedure TInterfaceServer.OnDADisconnect(const ClientConnection : IRDOConnection);
    begin
      Logs.Log('Survival', TimeToStr(Now) + ': Start OnDADisconnect..');
      try
        if (fDAClientConn as IRDOConnection) = ClientConnection
          then fDAOK := RenewWorldProxy;
        Logs.Log('Survival', TimeToStr(Now) + ': End OnDADisconnect..');
      except
        Logs.Log('Survival', TimeToStr(Now) + ': Error in OnDADisconnect..');
      end;
    end;

  function TInterfaceServer.GetWorldProxy : OleVariant;
    begin
      fDAProxyLock.Enter;
      try
        result := fDAProxy;
      finally
        fDAProxyLock.Leave;
      end;
    end;

  function TInterfaceServer.GetNewWorldProxy(var NewConnection : IRDOConnectionInit) : OleVariant;
    var
      Connection : IRDOConnectionInit;
    begin
      Connection := GetDAConnection; // See the disconnect thingy
      if Connection <> nil
        then
          begin
            result := TRDOObjectProxy.Create as IDispatch;
            result.SetConnection(Connection);
            result.BindTo(tidRDOHook_World);
            result.TimeOut := DATimeOut;
            NewConnection := Connection;
          end
        else
          begin
            result := Unassigned;
            NewConnection := nil;
          end;
    end;

  function TInterfaceServer.RenewWorldProxy : boolean;
    var
      Proxy : OleVariant;
      Cnx   : IRDOConnectionInit;
    begin
      try
        Logs.Log('Survival', TimeToStr(Time) + ' Begin renewing proxy' );
        fDAProxyLock.Enter;
        try
          if GetTickCount - fDALastTick > 5000
            then
              begin
                Proxy := GetNewWorldProxy(Cnx);
                if Cnx <> nil
                  then
                    begin
                      fDAProxy      := Proxy;
                      fDAClientConn := Cnx;
                      fDALastTick   := GetTickCount;
                      result        := true;
                    end
                  else result := false;
              end
            else result := true;
        finally
          fDAProxyLock.Leave;
        end;
        Logs.Log('Survival', TimeToStr(Time) + ' End renewing proxy' );
      except
        result := false;
        Logs.Log('Survival', TimeToStr(Time) + ' Error renewing proxy' );
      end;
    end;

  procedure TInterfaceServer.OnDSDisconnect(const ClientConnection : IRDOConnection);
    begin
      Logs.Log('Survival', TimeToStr(Now) + ': Directory Server connection lost.(start clearing proxy)');
      try
        fDSOK         := false;
        fDSClientConn := nil;
        fDSProxy      := Unassigned;
      except
        Logs.Log('Survival', TimeToStr(Now) + ': Direct Access connection lost.(error clearing proxy)');
      end;
    end;

  procedure TInterfaceServer.OnGMDisconnect(const ClientConnection : IRDOConnection);
    begin
      Logs.Log('Survival', TimeToStr(Now) + ': GM Server connection lost.(start clearing proxy)');
      try
        fGMClientConn := nil;
        fGMProxy      := Unassigned;
      except
        Logs.Log('Survival', TimeToStr(Now) + ': GM Access connection lost.(error clearing proxy)');
      end;
    end;

  function TInterfaceServer.GetWorldYear : integer;
    begin
      Lock('GetWorldYear.1');
      try
        try
          if not VarIsEmpty(WorldProxy)
            then result := integer(WorldProxy.CurrYear)
            else result := -1;
        except
          RenewWorldProxy;
          result := -1;
          try
            Logs.Log('Survival', TimeToStr(Now) + ' - Error in GetWorldYear' );
          except
          end;
        end;
      finally
        Unlock('GetWorldYear.3');
      end;
    end;

  function TInterfaceServer.GetWorldPopulation : integer;
    begin
      Lock('GetWorldPopulation');
      try
        try
          if not VarIsEmpty(WorldProxy)
            then result := integer(WorldProxy.TotalPop)
            else result := -1;
        except
          RenewWorldProxy; //DropDAConnection(WorldProxy, fDAClientConn);
          result := -1;
          try
            Logs.Log('Survival', TimeToStr(Now) + ' - Error in GetWorldPopulation' );
          except
          end;
        end;
      finally
        Unlock('GetWorldPopulation');
      end;
    end;

  function TInterfaceServer.GetWorldSeason : integer;
    begin
      Lock('GetWorldSeason');
      try
        try
          if not VarIsEmpty(WorldProxy) and not fServerBusy
            then fSeason := integer(WorldProxy.Season);
          result := fSeason;
        except
          RenewWorldProxy; //DropDAConnection(WorldProxy, fDAClientConn);
          result := fSeason;
          try
            Logs.Log('Survival', TimeToStr(Now) + ' - Error in GetWorldSeason' );
          except
          end;
        end;
      finally
        Unlock('GetWorldSeason');
      end;
    end;

  function TInterfaceServer.GetUserCount : integer;
    begin
      Lock('GetUserCount');
      try
        try
          if not fServerBusy and (GetTickCount - ttlUserCount > 1000) and not VarIsEmpty(WorldProxy)
            then fUserCount := integer(WorldProxy.InvestorCount);
          result := fUserCount;
        except
          RenewWorldProxy; //DropDAConnection(WorldProxy, fDAClientConn);
          result := -1;
          try
            Logs.Log('Survival', TimeToStr(Now) + ' - Error in GetWorldUserCount' );
          except
          end;
        end;
      finally
        Unlock('GetUserCount');
      end;
    end;

  function TInterfaceServer.GetVisitorCount : integer;
    begin
      Lock('GetUserCount');
      try
        try
          if not fServerBusy and (GetTickCount - ttlUserCount > 1000) and not VarIsEmpty(WorldProxy)
            then fVisitorCount := integer(WorldProxy.VisitorCount);
          result := fVisitorCount;
        except
          RenewWorldProxy; //DropDAConnection(WorldProxy, fDAClientConn);
          result := -1;
          try
            Logs.Log('Survival', TimeToStr(Now) + ' - Error in GetWorldUserCount' );
          except
          end;
        end;
      finally
        Unlock('GetUserCount');
      end;
    end;

  procedure TInterfaceServer.SetForceCommand( command : integer );
    begin
      case command of
        0 :
          begin
            try
              InitMailServer( MailAddr, MailPort );
            except
            end;
            try
              InitGMServer( GMAddr, GMPort );
            except
            end;
            try
              InitDSServer( DSAddr, DSPort, DSArea );
            except
            end;
          end;
      end;
    end;

  function TInterfaceServer.GetMSDown : boolean;
    begin
      result := fMaintDue; //fMSDownCount > MaxDownCountAllowed
    end;

  function TInterfaceServer.GetMinNobility : integer;
    var
      prm : string;
    begin
      try
        if (fMinNobility < 0) and fDAOK
          then
            begin
              prm := WorldProxy.GetConfigParm('MinNobility', '0');
              fMinNobility := StrToInt(prm);
            end;
      except
      end;
      result := fMinNobility;
    end;

  function TInterfaceServer.CheckUserAccount(UserName, Password : string) : boolean;
    var
      res : integer;
    begin
      try
        if fDSOK
          then res := fDSProxy.RDOLogonUser(UserName, Password)
          else res := 1;
        result := (res = DIR_NOERROR_StillTrial) or (res = DIR_NOERROR);
      except
        result := true;
      end;
    end;

  function TInterfaceServer.GetUserNobility(UserName : string) : integer;
    var
      key : string;
    begin
      try
        key := GetUserPath(UserName);
        if fDSOK and fDSProxy.RDOSetCurrentKey(key)
          then result := fDSProxy.RDOReadInteger('NobPoints')
          else result := 0;
      except
        result := 0;
      end;
    end;

  function TInterfaceServer.AccountStatus( UserName, Password : widestring ) : OleVariant;
    var
      PreviousClient : TClientView;
    begin
      Lock('AccountStatus');
      try
        try
          PreviousClient := GetClientByName( UserName );
          if PreviousClient <> nil
            then
              if (uppercase(Password) = uppercase(PreviousClient.fPassword))// and not PreviousClient.Alive
                then
                  begin
                    try
                      Logs.Log('Survival', TimeToStr(Now) + ' - [OJO!] Retiring the old Client View.. Infiel: ' + UserName);
                      PreviousClient.DoLogoff;
                      //LogThis('Survival', '(AccountStatus.4)', 0);
                    except
                      Logs.Log('Survival', TimeToStr(Now) + ' - Error in Retiring the old Clinet View' );
                    end;
                    result := ACCOUNT_Valid;
                  end
                else result := ACCOUNT_InvalidName
            else
              begin
                //LogThis('Survival', '(AccountStatus.5)', 0);
                try
                  if not VarIsEmpty(WorldProxy)
                    then result := integer(WorldProxy.RDOAccountStatus( UserName, Password ))
                    else result := ACCOUNT_UnknownError;
                except
                  RenewWorldProxy; //DropDAConnection(WorldProxy, fDAClientConn);
                  raise;
                end;
                //LogThis('Survival', '(AccountStatus.6)', 0);
              end;
        except
          result := ACCOUNT_UnknownError;
          try
            Logs.Log('Survival', TimeToStr(Now) + ' - Error in AccountStatus' );
          except
          end;
        end;
      finally
        Unlock('AccountStatus');
      end;
    end;

  function TInterfaceServer.Logon( UserName, Password : widestring ) : OleVariant;
    var
      ClientView     : TClientView;
      TycoonProxyId  : integer;
      Error          : TErrorCode;
      Cnx            : IRDOConnectionInit;
      key            : string;
      AccNob, AccMod : integer;
      ValidAccount   : boolean;
    begin
      Lock('Logon');
      try
        try
          if (GetClientByName( UserName ) = nil) and not VarIsEmpty(WorldProxy)
            then
              begin
                //LogThis('Survival', '(Logon.3)', 0);
                TycoonProxyId := WorldProxy.RDOGetTycoon( UserName, Password );
                //LogThis('Survival', '(Logon.4)', 0);
                if TycoonProxyId = 0
                  then
                    try
                      //LogThis('Survival', '(Logon.5)', 0);
                      ValidAccount := CheckUserAccount(UserName, Password);
                      if ValidAccount
                        then
                          begin
                            Error := WorldProxy.RDONewTycoon( UserName, Password );
                            if Error = NOERROR
                              then TycoonProxyId := WorldProxy.RDOGetTycoon( UserName, Password )
                              else TycoonProxyId := 0;
                          end;
                      //LogThis('Survival', '(Logon.6)', 0);
                    except
                      RenewWorldProxy; //DropDAConnection(WorldProxy, fDAClientConn);
                      TycoonProxyId := 0;
                      ValidAccount := false;
                    end
                  else ValidAccount := false;
                // >>
                if (TycoonProxyId <> 0)
                  then
                    begin
                      //LogThis('Survival', '(Logon.7)', 0);
                      ClientView                := TClientView.Create;
                      ClientView.fPassword      := Password;
                      ClientView.fTycoonProxyId := TycoonProxyId;
                      ClientView.fServer        := self;
                      ClientView.fCurrChannel   := fHomeChannel;
                      ClientView.fTycoonProxy   := TRDOObjectProxy.Create as IDispatch;
                      ClientView.fTycoonProxy.TimeOut := DATimeOut;
                      Cnx := fDACnntPool.GetConnection;
                      if Cnx <> nil
                        then
                          begin
                            ClientView.fTycoonProxy.SetConnection(Cnx);
                            ClientView.fConnection := Cnx;
                            ClientView.fTycoonProxy.BindTo( TycoonProxyId );
                            ClientView.fTycoonId := ClientView.fTycoonProxy.Id;
                            ClientView.fUserName := UserName; //ClientView.fTycoonProxy.Name;
                            ClientView.fRealName := ClientView.fTycoonProxy.RealName;
                            ClientView.fLangId   := ClientView.fTycoonProxy.Language;
                            if ValidAccount or ClientView.IsRole or CheckUserAccount(UserName, Password)
                              then
                                begin
                                  if fDSOK
                                    then
                                      try
                                        key := GetUserPath( UserName );
                                        if fDSProxy.RDOSetCurrentKey( key )
                                          then
                                            begin
                                              AccNob := fDSProxy.RDOReadInteger( 'NobPoints' );
                                              AccMod := fDSProxy.RDOReadInteger( 'AccModifier' );
                                              ClientView.fAccountDesc := ComposeAccDesc( AccNob, AccMod );
                                            end
                                          else ClientView.fAccountDesc := ComposeAccDesc( 0, AccMod_UnknownUser );
                                      except
                                        Logs.Log( 'Survival', TimeToStr(Now) + ' - Error in getting AccountDesc' );
                                      end;
                                  fClients.Insert( ClientView );
                                  ClientView.fId := fLastId;
                                  inc( fLastId );
                                  //LogThis('Survival', '(Logon.8)', 0);
                                  NotifyCompanionship;
                                  //LogThis('Survival', '(Logon.9)', 0);
                                  NotifyUserListChange( ComposeChatUser( UserName, ClientView.fAccountDesc, ClientView.AFK ), uchInclusion, fHomeChannel );
                                  //LogThis('Survival', '(Logon.10)', 0);
                                  result := integer(ClientView);
                                end
                              else
                                begin
                                  ClientView.Free;
                                  result := 0;
                                end;
                          end
                        else
                          begin
                            ClientView.Free;
                            result := 0;
                          end;
                    end
                  else result := 0;
              end
            else result := 0
        except
          result := 0;
          try
            Logs.Log('Survival', TimeToStr(Now) + ' - Error in Logon' );
          except
          end;
        end;
      finally
        Unlock('Logon');
      end;
    end;

  function TInterfaceServer.Logoff( ClientView : TClientView ) : OleVariant;
    var
      usrName : string;
    begin
      Lock('Logoff');
      try
        try
          if not VarIsEmpty(WorldProxy)
            then WorldProxy.RDOSleepTycoon( ClientView.fTycoonId );
        except
          RenewWorldProxy; //DropDAConnection(WorldProxy, fDAClientConn);
        end;
        //LogThis('Survival', '(Logoff.2)', 0);
        try
          if fClients.IndexOf(ClientView) <> noIndex
            then
              begin
                //LogThis('Survival', '(Logoff.3)', 0);
                fClients.Extract(ClientView);
                usrName := ClientView.UserName;
                DisconnectUser(usrName, ClientView.fGMId);
                ClientView.fClientEventsProxy := Unassigned;
                ClientView.fClientConnection  := nil;
                //LogThis('Survival', '(Logoff.4)', 0);
                NotifyUserListChange(usrName, uchExclusion, ClientView.fCurrChannel );
                //LogThis('Survival', '(Logoff.5)', 0);
                // Notify defuntion & companionship
                NotifyCompanionship;
                //LogThis('Survival', '(Logoff.6)', 0);
                try
                  //ClientView.Free;
                except
                  Logs.Log('Survival', TimeToStr(Now) + ' - Error freeing ClientView' );
                end;
              end;
          result := NOERROR;
        except
          result := ERROR_Unknown;
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in Logoff' );
        end;
        //LogThis('Survival', TimeToStr(Now) + ' - End in Logoff', 0);
      finally
        Unlock('Logoff');
      end;
    end;

  function TInterfaceServer.GetUserList( Channel : TChannel ) : OleVariant;
    var
      i    : integer;
      list : string;
    begin
      Lock('GetUserList');
      try
        try
          LockClients('GetUserList');
          list := '';
          try
            for i := 0 to pred(fClients.Count) do
              with TClientView(fClients[i]) do
                if fCurrChannel = Channel
                  then list := list + ComposeChatUser( UserName, AccountDesc, AFK ) + LineBreak;
            result := list;
          finally
            UnlockClients('GetUserList');
          end;
        except
          result := ERROR_Unknown;
          try
            Logs.Log('Survival', TimeToStr(Now) + ' - Error in Getuserlist' );
          except
          end;
        end;
      finally
        Unlock('GetUserList');
      end;
    end;

  function TInterfaceServer.GetChannelList : OleVariant;
    var
      i    : integer;
      list : string;
    begin
      Lock('GetChannelList');
      try
        try
          //LogThis('Survival', 'GetChannelList Channels.lock', 1);
          fChannels.Lock;
          //LogThis('Survival', '[', 0);
          try
            list := '';
            for i := 0 to pred(fChannels.Count) do
              with TChannel(fChannels[i]) do
                list := list + fName + LineBreak + fPassword + LineBreak;
            result := list;
          finally
            fChannels.Unlock;
            //LogThis('Survival', ']', 2);
          end;
        except
          result := ERROR_Unknown;
          try
            Logs.Log('Survival', TimeToStr(Now) + ' - Error in GetChannelList' );
          except
          end;
        end;
      finally
        Unlock('GetChannelList');
      end;
    end;

  function TInterfaceServer.GetChannelInfo( Name, langid : widestring ) : OleVariant;
    var
      Channel : TChannel;
      info    : string;
      i       : integer;
    begin
      Lock('GetChannelInfo');
      try
        try
          Channel := GetChannel( Name );
          if Channel <> nil
            then
              begin
                info := Format(mtidChannelInfo.Values[langid], [Channel.fName, Channel.fCreator]) + ' ';
                Channel.fMembers.Lock;
                try
                  if Channel.fMembers.Count = 1
                    then info := info + IntToStr(Channel.fMembers.Count) + ' ' + mtidUser.Values[langid] + ' ' + TClientView(Channel.fMembers[0]).fUserName // [4]
                    else
                      begin
                        info := info + IntToStr(Channel.fMembers.Count) + ' ' + mtidUsers.Values[langid] + ' '; // [5]
                        for i := 0 to pred(Channel.fMembers.Count) do
                          begin
                            info := info + TClientView(Channel.fMembers[i]).fUserName;
                            if i < Channel.fMembers.Count - 2
                              then info := info + ', '
                              else
                                if i < Channel.fMembers.Count - 1
                                  then info := info + ' ' + mtidAnd.Values[langid]; // [6]
                          end;
                      end;
                  info := info + '.';
                finally
                  Channel.fMembers.Unlock;
                end;
                if Channel.fPassword <> ''
                  then info := info + ' ' + mtidChanNeedsPass.Values[langid]; //info := info + ' You need a password to enter this channel.'; // [7]
                result := info;
              end
            else result := mtidNoChanInfo.Values[langid]; // result := 'There is no information associated to this object.' // [8]
        except
          result := ERROR_Unknown;
          try
            Logs.Log('Survival', TimeToStr(Now) + ' - Error in GetChannelInfo' );
          except
          end;
        end;
      finally
        Unlock('GetChannelInfo');
      end;
    end;

  function TInterfaceServer.GetClientView(Name : widestring) : OleVariant;
    var
      ClientView : TClientView;
    begin
      ClientView := GetClientByName(Name);
      result := integer(ClientView);
    end;

  function TInterfaceServer.CanJoinWorld(Name : widestring) : OleVariant;
    begin
      result := (fUserCount >= 0) and (fUserCount < fMaxUserCount);
    end;

  function TInterfaceServer.CanJoinWorldEx(Name : widestring) : OleVariant;
    var
      unob : integer;
      wnob : integer;
    begin
      if (fUserCount >= 0) and (fUserCount < fMaxUserCount)
        then
          begin
            unob := GetUserNobility(Name);
            wnob := GetMinNobility;
            if unob >= wnob
              then result := 0
              else result := wnob - unob
          end
        else result := -1;
    end;

  procedure TInterfaceServer.BanPlayer(Name : widestring);
    var
      ClientView : TClientView;
    begin
      ClientView := GetClientByName(Name);
      if ClientView <> nil
        then
          begin
            ClientView.fBanned := true;
            ClientView.fConnected := false;
          end;
    end;

  function TInterfaceServer.GetClientByName( Name : string ) : TClientView;
    var
      i : integer;
    begin
      Lock('GetChannelInfo');
      try
        LockClients('GetClientByName');
        Name := uppercase( Name );
        try
          i := 0;
          while (i < fClients.Count) and (uppercase( (fClients[i] as TClientView).UserName ) <> Name) do
            inc( i );
          if i < fClients.Count
            then result := fClients[i] as TClientView
            else result := nil;
        finally
          UnlockClients('GetClientByName');
        end;
      finally
        Unlock('GetChannelInfo');
      end;
    end;

  function TInterfaceServer.GetClientByTycoonId( Id : integer ) : TClientView;
    var
      i : integer;
    begin
      Lock('GetClientByTycoonId');
      try
        LockClients('GetClientByTycoonId');
        try
          i := 0;
          while (i < fClients.Count) and ((fClients[i] as TClientView).fTycoonId <> Id) do
            inc( i );
          if i < fClients.Count
            then result := fClients[i] as TClientView
            else result := nil;
        finally
          UnlockClients('GetClientByTycoonId');
        end;
      finally
        Unlock('GetClientByTycoonId');
      end;
    end;

  function TInterfaceServer.ObjectsInArea( x, y, dx, dy : integer ) : string;
    begin
      try
        if not VarIsEmpty(WorldProxy)
          then result := widestring(WorldProxy.RDOGetObjectsInArea( x, y, dx, dy ))
          else result := '';
      except
        RenewWorldProxy; //DropDAConnection(WorldProxy, fDAClientConn);
        result := '';
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in ObjectsInArea' );
        except
        end;
      end;
    end;

  function TInterfaceServer.SegmentsInArea( CircuitId, x1, y1, x2, y2 : integer ) : string;
    begin
      try
        if not VarIsEmpty(WorldProxy)
          then result := widestring(WorldProxy.RDOSegmentsInArea( CircuitId, x1, y1, x2, y2 ))
          else result := '';
      except
        RenewWorldProxy; //DropDAConnection(WorldProxy, fDAClientConn);
        result := '';
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in SegmentsInArea' );
        except
        end;
      end;
    end;


  {$IFNDEF BUGQUEST_NoEvnSpreading}

  procedure TInterfaceServer.RefreshArea( x, y, dx, dy : integer );

    procedure InvalidateCache( x, y, dx, dy : integer );
      var
        xi, yi : integer;
      begin
        for xi := x to x + dx do
          for yi := y to y + dy do
            if (xi >= 0) and (xi < fWorldXSize) and (yi >= 0) and (yi < fWorldYSize)
              then
                begin
                  fObjectCache[xi, yi] := '';
                  fRoadsCache[xi, yi]  := '';
                end;
      end;

    function GetExtraInfo( x, y, dx, dy : integer ) : string;
      var
        ObjectInfo : string;
        RoadInfo   : string;
        RailInfo   : string;
      begin
        ObjectInfo := string(ObjectsInArea( x, y, dx, dy ));
        RoadInfo   := string(SegmentsInArea( cirRoads, x, y, x + dx, y + dy ));
        RailInfo   := string(SegmentsInArea( cirRailRoads, x, y, x + dx, y + dy ));
        result     := '1' + RepSeparator + ObjectInfo + RepSeparator + RoadInfo + RepSeparator + RailInfo;
      end;

    var
      i         : integer;
      ExtraInfo : string;
    begin
      try
        Lock('RefreshArea');
        try
          InvalidateCache( x, y, dx, dy );
          //LogThis('Survival', '(RefreshArea.3)', 0);
          ExtraInfo := GetExtraInfo( x, y, dx, dy );
          LockClients('RefreshArea');
          try
            for i := 0 to pred(fClients.Count) do
              (fClients[i] as TClientView).RefreshArea( x, y, dx, dy, ExtraInfo );
          finally
            UnlockClients('RefreshArea');
          end;
        finally
          Unlock('RefreshArea');
        end;
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in RefreshArea' );
        except
        end;
      end;
    end;

  procedure TInterfaceServer.RefreshObject( ObjId, KindOfChange : integer );
    var
      i   : integer;
      cnt : integer;
    begin
      try
        Lock('RefreshObject');
        try
          LockClients('RefreshObject');
          try
            cnt := 0;
            for i := 0 to pred(fClients.Count) do
              if (fClients[i] as TClientView).RefreshObject( ObjId, KindOfChange )
                then inc(cnt);
          finally
            UnlockClients('RefreshObject');
          end;
          try
            if (cnt = 0) and fDAOK
              then WorldProxy.RDOFacilityLostFocus(ObjId);
          except
            RenewWorldProxy; //DropDAConnection(WorldProxy, fDAClientConn);
          end;
        finally
          Unlock('RefreshObject');
        end;
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in RefreshObject' );
        except
        end;
      end;
    end;

  procedure TInterfaceServer.RefreshTycoons;
    var
      i : integer;
    begin
      try
        Lock('RefreshTycoons');
        try
          LockClients('RefreshTycoons');
          try
            for i := 0 to pred(fClients.Count) do
              (fClients[i] as TClientView).RefreshTycoon;
          finally
            UnlockClients('RefreshTycoons');
          end;
        finally
          Unlock('RefreshTycoons');
        end;
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in RefreshTycoons' );
        except
        end;
      end;
    end;

  procedure TInterfaceServer.RefreshDate( Date : TDateTime );
    var
      i : integer;
    begin
      try
        Lock('RefreshDate');
        try
          LockClients('RefreshDate');
          try
            for i := 0 to pred(fClients.Count) do
              (fClients[i] as TClientView).RefreshDate( Date );
          finally
            UnlockClients('RefreshDate');
          end;
        finally
          Unlock('RefreshDate');
        end;
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in RefreshDate' );
        except
        end;
      end;
    end;

  procedure TInterfaceServer.RefreshSeason( Season : integer );
    var
      i : integer;
    begin
      try
        Lock('RefreshSeason');
        try
          LockClients('RefreshSeason');
          try
            fSeason := Season;
            for i := 0 to pred(fClients.Count) do
              (fClients[i] as TClientView).RefreshSeason( Season );
          finally
            UnlockClients('RefreshSeason');
          end;
        finally
          Unlock('RefreshSeason');
        end;
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in RefreshSeason');
        except
        end;
      end;
    end;

  procedure TInterfaceServer.EndOfPeriod;
    var
      i : integer;
    begin
      try
        Lock('EndOfPeriod');
        try
          LockClients('EndOfPeriod');
          try
            for i := 0 to pred(fClients.Count) do
              (fClients[i] as TClientView).EndOfPeriod;
          finally
            UnlockClients('EndOfPeriod');
          end;
        finally
          Unlock('EndOfPeriod');
        end;
      except
        try
          Logs.log('Survival', TimeToStr(Now) + ' - Error in EndOfPeriod' );
        except
        end;
      end;
    end;

  procedure TInterfaceServer.TycoonRetired( name : string );
    var
      ClientView : TClientView;
    begin
      try
        Lock('TycoonRetired');
        try
          ClientView := GetClientByName( name );
          if ClientView <> nil
            then
              begin
                ClientView.TycoonRetired;
                //ClientView.Logoff;
              end;
        finally
          Unlock('TycoonRetired');
        end;
      except
        try
          Logs.log('Survival', TimeToStr(Now) + ' - Error in TycoonRetired' );
        except
        end;
      end;
    end;

  procedure TInterfaceServer.SendTickData( PoolId, ViewerId, TickCount : integer; TickData : widestring );
    //var
      //ClientView : TClientView;
    begin
    (*
      try
        //LogThis('Survival', '(SendTickData.1)');
        Lock();
        //LogThis('Survival', '(SendTickData.2)');
        try
          ClientView := GetClientByTycoonId( ViewerId );
          if ClientView <> nil
            then ClientView.SendTickData( PoolId, TickCount, TickData );
        finally
          Unlock();
        end;
        //LogThis('Survival', '(SendTickData.3)');
      except
        try
          //LogThis('Survival', TimeToStr(Now) + ' - Error in SendTickData' );
        except
        end;
      end;
    *)
    end;

  procedure TInterfaceServer.SendNotification( TycoonId : integer; Kind : integer; Title, Body : string; Options : integer );
    var
      ClientView : TClientView;
    begin
      try
        Lock('SendNotification');
        try
          ClientView := GetClientByTycoonId( TycoonId );
          if ClientView <> nil
            then ClientView.SendNotification( Kind, Title, Body, Options );
        finally
          Unlock('SendNotification');
        end;
      except
        try
          Logs.log('Survival', TimeToStr(Now) + ' - Error in SendNotification' );
        except
        end;
      end;
    end;

  procedure TInterfaceServer.ModelStatusChanged( Status : integer );
    begin
      try
        Lock('ModelStatusChanged');
        try
          if Status and mstBusy <> 0
            then
               begin
                 fServerBusy := true;
                 SystemMsg('', mtidServerBusy, ['']); // 'Servers are busy creating backup files. Please wait.' ); // [9]
               end
            else
              if Status and mstNotBusy <> 0
                then
                  begin
                    fServerBusy := false;
                    SystemMsg('', mtidServerNotBusy, ['']); // ChatMsg( 'SYSTEM', '', 'Backup to disk completed.' ); // [10]
                  end;
          if Status and mstError <> 0
            then fServerError := true;
        finally
          Unlock('ModelStatusChanged');
        end;
      except
        try
          Logs.log('Survival', TimeToStr(Now) + ' - Error in ModelStatusChanged' );
        except
        end;
      end;
    end;

  procedure TInterfaceServer.SystemMsg(dest : string; Msg : TRegMultiString; values : array of const);
    var
      i : integer;
    begin
      try
        Lock('SystemMsg');
        try
          LockClients('SystemMsg');
          try
            for i := 0 to pred(fClients.Count) do
              with fClients[i] as TClientView do
                if (dest = '') or (fUserName = Dest)
                  then ListenSystemMsg(Msg, values);
          finally
            UnlockClients('SystemMsg');
          end;
        finally
          Unlock('SystemMsg');
        end;
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in ChatMsg' );
        except
        end;
      end;
    end;

  procedure TInterfaceServer.ChatMsg( Src, CompSrc, Dest, Msg : string );
    var
      Source : TClientView;
      i      : integer;
    begin
      try
        try
          Logs.Log( 'Chat', CompSrc + ': ' + Msg );
        except
        end;
        Lock('ChatMsg');
        try
          Source := GetClientByName( Src );
          if Source <> nil
            then
              begin
                LockClients('ChatMsg');
                try
                  for i := 0 to pred(fClients.Count) do
                    with fClients[i] as TClientView do
                      if ((Dest = '') or (pos( fUserName, Dest ) <> 0)) and ((Source = nil) or (Source.fCurrChannel = fCurrChannel))
                        then HearThis( ComposeChatUser( Source.UserName, Source.AccountDesc, Source.AFK ), Msg );
                finally
                  UnlockClients('ChatMsg');
                end;
              end;
        finally
          Unlock('ChatMsg');
        end;
      except
        try
          Logs.log('Survival', TimeToStr(Now) + ' - Error in ChatMsg' );
        except
        end;
      end;
    end;

  procedure TInterfaceServer.VoiceMsg( Src, Msg : string; TxId, NewTx : integer );
    var
      Source : TClientView;
      i      : integer;
    begin
      try
        Lock('VoiceMsg');
        try
          Source := GetClientByName( Src );
          LockClients('VoiceMsg');
          try
            for i := 0 to pred(fClients.Count) do
              with fClients[i] as TClientView do
                if (((Source = nil) or (Source.fCurrChannel = fCurrChannel)) and (Source <> fClients[i])) and fVoiceEnabled
                  then HearThisVoice( Src, Msg, TxId, NewTx );
          finally
            UnlockClients('VoiceMsg');
          end;
        finally
          Unlock('VoiceMsg');
        end;
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in VoiceMsg' );
        except
        end;
      end;
    end;

  procedure TInterfaceServer.MsgCompositionChanged( Name : string; State : TMsgCompositionState );
    var
      i : integer;
    begin
      try
        Lock('MsgCompositionChanged');
        try
          LockClients('MsgCompositionChanged');
          try
            for i := 0 to pred(fClients.Count) do
              with fClients[i] as TClientView do
                begin
                  NotifyMsgCompositionState( Name, State );
                end;
          finally
            UnlockClients('MsgCompositionChanged');
          end;
        finally
          Unlock('MsgCompositionChanged');
        end;
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in MsgCompositionChanged' );
        except
        end;
      end;
    end;

  procedure TInterfaceServer.NotifyCompanionship;
    var
      i : integer;
    begin
      try
        Lock('NotifyCompanionship');
        try
          LockClients('NotifyCompanionship');
          try
            for i := 0 to pred(fClients.Count) do
              with fClients[i] as TClientView do
                NotifyCompanionship;
          finally
            UnlockClients('NotifyCompanionship');
          end;
        finally
          Unlock('NotifyCompanionship');
        end;
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in NotifyCompanionship' );
        except
        end;
      end;
    end;

  procedure TInterfaceServer.NotifyUserListChange( Name : string; Change : TUserListChange; Channel : TChannel );
    var
      i : integer;
    begin
      try
        Lock('NotifyUserListChange');
        try
          LockClients('NotifyUserListChange');
          try
            for i := 0 to pred(fClients.Count) do
              with fClients[i] as TClientView do
                if (Name <> UserName) and (fCurrChannel = Channel)
                  then NotifyUserListChange( Name, Change );
          finally
            UnlockClients('NotifyUserListChange');
          end;
        finally
          Unlock('NotifyUserListChange');
        end;
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in NotifyUserListChanged' );
        except
        end;
      end;
    end;

  procedure TInterfaceServer.NotifyChannelListChange( Name, Password : string; Change : TUserListChange );
    var
      i : integer;
    begin
      try
        Lock('NotifyChannelListChange');
        try
          LockClients('NotifyChannelListChange');
          try
            for i := 0 to pred(fClients.Count) do
              with fClients[i] as TClientView do
                NotifyChannelListChange( Name, Password, Change );
          finally
            UnlockClients('NotifyChannelListChange');
          end;
        finally
          Unlock('NotifyChannelListChange');
        end;
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in NotifyChannelListChanged' );
        except
        end;
      end;
    end;

  procedure TInterfaceServer.OnMailDisconnect(const ClientConnection : IRDOConnection);
    begin
      Logs.Log('Survival', TimeToStr(Now) + ' Mail Server disconnected (begin clear proxy)' );
      try
        fMailConn   := nil;
        fMailServer := Unassigned;
        Logs.Log('Survival', TimeToStr(Now) + ' Mail Server disconnected (OK)' );
      except
        Logs.Log('Survival', TimeToStr(Now) + ' Mail Server disconnected (ERROR)' );
      end;
    end;

{$ELSE}

  procedure TInterfaceServer.RefreshArea( x, y, dx, dy : integer );
    begin
    end;

  procedure TInterfaceServer.RefreshObject( ObjId, KindOfChange : integer );
    begin
    end;

  procedure TInterfaceServer.RefreshTycoons;
    begin
    end;

  procedure TInterfaceServer.RefreshDate( Date : TDateTime );
    begin
    end;

  procedure TInterfaceServer.EndOfPeriod;
    begin
    end;

  procedure TInterfaceServer.TycoonRetired( name : string );
    begin
    end;

  procedure TInterfaceServer.ChatMsg( Src, Dest, Msg : string );
    begin
    end;

  procedure TInterfaceServer.MsgCompositionChanged( Name : string; State : TMsgCompositionState );
    begin
    end;

  procedure TInterfaceServer.NotifyCompanionship;
    begin
    end;

  procedure TInterfaceServer.NotifyUserListChange( Name : string; Change : TUserListChange );
    begin
    end;

{$ENDIF}

  procedure TInterfaceServer.InitMailServer(ServerName : string; ServerPort : integer);
    var
      ClientId : integer;
    begin
      fMailAddr        := ServerName;
      fMailPort        := ServerPort;
      fMailConn        := TWinSockRDOConnection.Create( 'Mail' );
      fMailConn.Server := ServerName;
      fMailConn.Port   := ServerPort;
      if fMailConn.Connect(MailConnectionTimeOut)
        then
          begin
            (fMailConn as IRDOConnection).OnDisconnect := OnMailDisconnect;
            fMailServer := TRDOObjectProxy.Create as IDispatch;
            fMailServer.SetConnection(fMailConn);
            fMailServer.BindTo(tidRDOHook_MailServer);
            fMailId := fMailServer.LogServerOn(fWorldName);
            if fMailId <> 0
              then
                begin
                  fMailServer.BindTo(fMailId);
                  fMailEvents := TRDOServer.Create(fMailConn as IRDOServerConnection, 1, nil );
                  fMailEvents.RegisterObject(tidRDOHook_MailEvents, integer(self));
                  ClientId := fMailServer.RDOCnntId;
                  if fMailServer.RegisterEventsById(ClientId) // if fMailServer.RegisterEvents((fMailConn as IRDOConnection).LocalAddress, (fMailConn as IRDOConnection).LocalPort)
                    then fMailServer.BindTo(tidRDOHook_MailServer)
                    else
                      begin
                        //raise Exception.Create( '' );
                        fMailConn := nil;
                        fMailServer := Unassigned;
                        fMailEvents.Free;
                        fMailEvents := nil;
                      end;
                end
              else
                begin
                  //raise Exception.Create( '' );
                  fMailConn := nil;
                  fMailServer := Unassigned;
                  fMailEvents.Free;
                  fMailEvents := nil;
                end;
          end
        else
          begin
            //raise Exception.Create( '' );
            fMailConn := nil;
            fMailServer := Unassigned;
            fMailEvents.Free;
            fMailEvents := nil;
          end;
    end;

  procedure TInterfaceServer.InitGMServer( ServerName : string; ServerPort : integer );
    var
      GMId : olevariant;
      ClId : integer;
    begin
      fGMAddr := ServerName;
      fGMPort := ServerPort;
      fGMClientConn        := TWinSockRDOConnection.Create( 'GM' );
      fGMClientConn.Server := ServerName;
      fGMClientConn.Port   := ServerPort;
      if fGMClientConn.Connect( GMConnectionTimeOut )
        then
          begin
            (fGMClientConn as IRDOConnection).OnDisconnect := OnGMDisconnect;
            fGMProxy := TRDOObjectProxy.Create as IDispatch;
            fGMProxy.SetConnection( fGMClientConn );
            fGMProxy.Timeout       := GMConnectionTimeOut;
            fGMProxy.WaitForAnswer := false;
            if fGMProxy.BindTo( tidRDOHook_GMServer )
              then
                begin
                  ClId := fGMProxy.RDOCnntId;
                  fGMEventsRDO := TRDOServer.Create( fGMClientConn as IRDOServerConnection, 1, nil );
                  fGMClientId := fGMProxy.RegisterInterfaceServer( integer(self), ClId, '', GMId );
                end
              else
                begin
                  fGMClientId   := 0;
                  fGMClientConn := nil;
                  fGMProxy      := Unassigned;
                end;
          end
        else
          begin
            //raise Exception.Create( 'Cannnot connect to GM server' );
            fGMClientConn := nil;
            fGMProxy      := Unassigned;
          end;
    end;

  function TInterfaceServer.CreateDSConnection : boolean;
    begin
      try
        fDSClientConn        := TWinSockRDOConnection.Create( 'DSCnx' );
        fDSClientConn.Server := fDSAddr;
        fDSClientConn.Port   := fDSPort;
        if fDSClientConn.Connect(DSTimeOut)
          then
            begin
              (fDSClientConn as IRDOConnection).OnDisconnect := OnDSDisconnect;
              fDSProxy := TRDOObjectProxy.Create as IDispatch;
              fDSProxy.SetConnection(fDSClientConn);
              fDSProxy.TimeOut := DSTimeOut;
              if fDSProxy.BindTo('DirectoryServer')
                then
                  begin
                    fSession := fDSProxy.RDOOpenSession;
                    if (fSession <> 0) and fDSProxy.BindTo(fSession)
                      then
                        begin
                          fDSProxy.WaitForAnswer := true;
                          fDSProxy.RDOSetExpires(false);
                          fDSOK  := true;
                          result := true;
                        end
                      else
                        begin
                          result   := false;
                          fDSOK    := false;
                          fDSProxy := Unassigned;
                          fDSClientConn := nil;
                        end;
                  end
                else
                  begin
                    result := false;
                    fDSOK := false;
                    fDSProxy := Unassigned;
                    fDSClientConn := nil;
                  end;
            end
          else
            begin
              result := false;
              fDSOK := false;
              fDSProxy := Unassigned;
              fDSClientConn := nil;
            end;
      except
        result := false;
        fDSOK := false;
        fDSProxy := Unassigned;
        fDSClientConn := nil;
      end;
    end;

  procedure TInterfaceServer.InitDSServer( ServerName : string; ServerPort : integer; Area : string );
    var
      key     : string;
      useless : integer;
    begin
      try
        fDSAddr := ServerName;
        fDSPort := ServerPort;
        fDSArea := Area;
        if CreateDSConnection
          then
            begin
              useless := fDSProxy.RDOSetSecurityLevel(wordbool(false));
              try
                key := 'Root/Areas/' + fDSArea + '/Worlds/' + WorldName + '/Interface';
                if fDSProxy.RDOCreateFullPathKey( key, true )
                  then
                    begin
                      if fDSProxy.RDOSetCurrentKey(key)
                        then fDSProxy.RDOWriteString('URL', fWorldURL);
                    end;
                StoreInfoInDS;
              finally
                useless := fDSProxy.RDOSetSecurityLevel(wordbool(true));
              end;
            end;
      except
      end;
    end;

  procedure TInterfaceServer.ReportNewMail(Account, From, Subject, MsgId : widestring);
    var
      ClientView : TClientView;
      i          : integer;
    begin
      try
        if Account <> ''
          then
            begin
              ClientView := GetClientByName( Account );
              if ClientView <> nil
                then
                  begin
                    ClientView.HearThis(mtidPostMan.Values[ClientView.fLangId], Format(mtidYouHaveMail.Values[ClientView.fLangId], [From, Subject])); // [12]
                    ClientView.ReportNewMail( 1 );
                  end;
            end
          else
            begin
              Lock('Report Mail');
              try
                for i := 0 to pred(fClients.Count) do
                  begin
                    ClientView := TClientView(fClients[i]);
                    ClientView.ReportNewMail(1);
                  end;
              finally
                UnLock('Report Mail');
              end;
            end;
      except
      end;
    end;

  function TInterfaceServer.CountUnreadMessages( Account : widestring ) : integer;
    begin
      Lock('CountUnreadMessages');
      try
        try
          if not VarIsEmpty(fMailServer)
            then result := fMailServer.CheckNewMail( fMailId, Account )
            else result := 0;
        except
          result := 0;
          try
            Logs.Log('Survival', TimeToStr(Now) + ' - Error in Countunreadmessages' );
          except
          end;
        end;
      finally
        Unlock('CountUnreadMessages');
      end;
    end;

  procedure TInterfaceServer.InsertInMasterIndex;
    begin
      try
        Lock('InsertInMasterIndex');
        try
          if fLocalAddr = ''
            then fLocalAddr := GetLocalAddress;
          NotifyToURL(
            fWorldURL + tidURL_RegisterIS + '?' +
            'Action=register&' +
            'WorldName=' + fWorldName + '&' +
            'ISAddr=' + fLocalAddr + '&' +
            'ISPort=' + IntToStr(fClientPort) + '&' +
            'WorldURL=' + fWorldURL + '&' +
            'Population=' + IntToStr( GetWorldPopulation ) + '&' +
            'Year=' + IntToStr( GetWorldYear ) + '&' +
            'Tycoons=' + IntToStr( GetUserCount ) + '&' +
            'Online=' + IntToStr( Clients.Count ) );
        finally
          Unlock('InsertInMasterIndex');
        end;
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in InsertInMasterIndex' );
        except
        end;
      end;
    end;

  procedure TInterfaceServer.UpdateMasterIndex;
    begin
      try
        Lock('UpdateMasterIndex');
        try
          if fLocalAddr = ''
            then fLocalAddr := GetLocalAddress;
          NotifyToURL(
            fWorldURL + tidURL_RegisterIS + '?' +
            'Action=update&' +
            'WorldName=' + fWorldName + '&' +
            'ISAddr=' + fLocalAddr + '&' +
            'ISPort=' + IntToStr(fClientPort) + '&' +
            'WorldURL=' + fWorldURL + '&' +
            'Population=' + IntToStr( GetWorldPopulation ) + '&' +
            'Year=' + IntToStr( GetWorldYear ) + '&' +
            'Tycoons=' + IntToStr( GetUserCount ) + '&' +
            'Online=' + IntToStr( Clients.Count ) );
        finally
          Unlock('UpdateMasterIndex');
        end;
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in UpdateMasterIndex' );
        except
        end;
      end;
    end;

  procedure TInterfaceServer.DeleteFromMasterIndex;
    begin
      try
        Lock('DeleteFromMasterIndex');
        try
          NotifyToURL(
            fWorldURL + tidURL_RegisterIS + '?' +
            'Action=unregister&' +
            'WorldName=' + fWorldName );
        finally
          Unlock('DeleteFromMasterIndex');
        end;
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in DeleteFromMasterIndex' );
        except
        end;
      end;
    end;

  procedure TInterfaceServer.NotifyToURL( URL : string );
    //var
      //Handle    : HINTERNET;
      //ReqHandle : HINTERNET;
    begin
      (*
      try
        Logs.Log('Survival', TimeToStr(Now) + ' - Notifying to URL' );
      except
      end;
      Handle := InternetOpen( 'FiveIS', INTERNET_OPEN_TYPE_DIRECT , nil, nil, 0 );
      if Handle <> nil
        then
          begin
            ReqHandle := InternetOpenUrl( Handle, pchar(URL), nil, 0, INTERNET_FLAG_DONT_CACHE or INTERNET_FLAG_RELOAD, 0 );
            if ReqHandle <> nil
              then InternetCloseHandle( ReqHandle );
            InternetCloseHandle( Handle );
          end;
      try
        //LogThis('Survival', TimeToStr(Now) + ' - Notifying to URL complete' );
      except
      end;
      *)
    end;

  procedure TInterfaceServer.StoreInfoInDS;
    var
      key     : string;
      useless : integer;
    begin
      Lock('StoreInfoInDS');
      try
        if fDSOK
          then
            begin
              try
                useless := fDSProxy.RDOSetSecurityLevel(wordbool(false));
                try
                  key := 'Root/Areas/' + fDSArea + '/Worlds/' + WorldName + '/General';
                  if fDSProxy.RDOCreateFullPathKey( key, true ) // ##
                    then
                      begin
                        if fDSProxy.RDOSetCurrentKey(key)
                          then
                            begin
                              fDSProxy.RDOWriteInteger( 'Population', GetWorldPopulation );
                              fDSProxy.RDOWriteInteger( 'Investors', GetUserCount );
                              fDSProxy.RDOWriteInteger( 'Visitors', GetVisitorCount );
                              fDSProxy.RDOWriteInteger( 'Online', Clients.Count );
                              fDSProxy.RDOWriteInteger( 'Date', GetWorldYear );
                            end;
                      end;
                finally
                  useless := fDSProxy.RDOSetSecurityLevel(wordbool(true));
                end;
              except
              end;

              try
                useless := fDSProxy.RDOSetSecurityLevel(wordbool(false));
                try
                  key := 'Root/Areas/' + fDSArea + '/Worlds/' + WorldName + '/Interface';
                  if fDSProxy.RDOSetCurrentKey(key)
                    then fDSProxy.RDOWriteBoolean('Running', true);
                finally
                  useless := fDSProxy.RDOSetSecurityLevel(wordbool(true));
                end;
              except
              end;
            end;
      finally
        Unlock('StoreInfoInDS');
      end;
    end;

  procedure TInterfaceServer.OnSentinel( Sender : TObject );
    //var
      //i : integer;
    begin
      {
      Lock('OnSentinel');
      try
        LockClients('OnSentinel');
        try
          for i := pred(fClients.Count) downto 0 do
            with fClients[i] as TClientView do
              try
                CheckState;
              except
              end;
        finally
          UnlockClients('OnSentinel');
        end;
      finally
        Unlock('OnSentinel');
      end;
      }
    end;

  procedure TInterfaceServer.OnRefresh( Sender : TObject );
    begin
      //UpdateMasterIndex;
      try
        if fDSOK and not fServerBusy
          then StoreInfoInDS;
      except
        Logs.Log('Survival', TimeToStr(Now) +' Error Storing Info in DS.' );
      end;
      try
        CheckDAConnections;
      except
        Logs.Log('Survival', TimeToStr(Now) +' Error Checking DA Comnnections.' );
      end;
    end;

  function TInterfaceServer.GetChannel( ChannelId : string ) : TChannel;
    var
      i : integer;
    begin
      Lock('GetChannel');
      try
        if ChannelId <> ''
          then
            begin
              i := 0;
              while (i < fChannels.Count) and (TChannel(fChannels[i]).fName <> ChannelId) do
                inc( i );
              if i < fChannels.Count
                then result := TChannel(fChannels[i])
                else result := nil;
            end
          else result := fHomeChannel;
      finally
        Unlock('GetChannel');
      end;
    end;

  procedure TInterfaceServer.ClientCreatedChannel( Client : TClientView; Name, Password, aSessionApp, aSessionAppId : string; anUserLimit : integer );
    var
      Channel : TChannel;
    begin
      Lock('ClientCreatedChannel');
      try
        Channel := GetChannel( Name );
        if Channel = nil
          then
            begin
              Channel := TChannel.Create( Name, Client.UserName, Password, aSessionApp, aSessionAppId, anUserLimit );
              fChannels.Insert( Channel );
              ClientEnteredChannel( Client, Channel );
              NotifyChannelListChange( Name, Password, uchInclusion );
            end;
      finally
        Unlock('ClientCreatedChannel');
      end;
    end;

  procedure TInterfaceServer.ClientEnteredChannel( Client : TClientView; Channel : TChannel );
    var
      i  : integer;
      CV : TClientView;
    begin
      Lock('ClientEnteredChannel');
      try
        if (Channel = nil) or (Channel.fMembers.IndexOf( Client ) = NoIndex)
          then
            begin
              Client.fCurrChannel := Channel;
              if Channel <> nil
                then
                  begin
                    //LogThis('Survival', '(ClientEnteredChannel.Channels.Lock)', 1);
                    Channel.fMembers.Lock;
                    //LogThis('Survival', '[', 0);
                    Channel.fMembers.Insert( Client );
                    try
                      for i := 0 to pred(Channel.fMembers.Count) do
                        begin
                          CV := TClientView(Channel.fMembers[i]);
                          if Channel.Name <> ''
                            then CV.HearThis(mtidSystem.Values[CV.fLangId],  Format(mtidEntersChannel.Values[CV.fLangId], [Client.UserName, Channel.fName]))  // [13]
                            else CV.HearThis(mtidSystem.Values[CV.fLangId],  Format(mtidEntersLobby.Values[CV.fLangId], [Client.UserName]));   // [14]
                        end;
                    finally
                      Channel.fMembers.Unlock;
                      //LogThis('Survival', ']', 2);
                    end;
                    Client.NotifyChannelChange( Channel.fName );
                  end
                else Client.NotifyChannelChange( '' );
              NotifyUserListChange( ComposeChatUser( Client.UserName, Client.fAccountDesc, Client.AFK ), uchInclusion, Channel );
            end;
      finally
        Unlock('ClientEnteredChannel');
      end;
    end;

  procedure TInterfaceServer.ClientLeavedChannel( Client : TClientView; Channel : TChannel );
    var
      i   : integer;
      idx : integer;
      CV  : TClientView;
    begin
      Lock('ClientLeavedChannel');
      try
        if (Channel <> nil) and (Channel.fMembers.IndexOf( Client ) <> NoIndex)
          then
            begin
             //LogThis('Survival', 'Channel.fLock.Enter', 1);
             Channel.fLock.Enter;
             //LogThis('Survival', '[', 0);
              try
                idx := Channel.fVoiceReqs.IndexOf( Client.UserName );
                if idx <> NoIndex
                  then
                    if idx = 0
                      then Client.VoiceTxOver( 0 )
                      else Channel.fVoiceReqs.Delete( idx );
              finally
                Channel.fLock.Leave;
                //LogThis('Survival', ']', 2);
              end;
              NotifyUserListChange( Client.UserName, uchExclusion, Channel );
              if Channel <> nil
                then
                  begin
                    //LogThis('Survival', 'Channel.fMembers.Lock', 1);
                    Channel.fMembers.Lock;
                    //LogThis('Survival', '[', 0);
                    try
                      for i := 0 to pred(Channel.fMembers.Count) do
                        begin
                          CV := TClientView(Channel.fMembers[i]);
                          if Channel.Name <> ''
                            then CV.HearThis(mtidSystem.Values[CV.fLangId],  Format(mtidLeftChannel.Values[CV.fLangId], [Client.UserName, Channel.fName]))  // [13]
                            else CV.HearThis(mtidSystem.Values[CV.fLangId],  Format(mtidLeftLobby.Values[CV.fLangId], [Client.UserName]));   // [14]
                        end;
                    finally
                      Channel.fMembers.Unlock;
                      //LogThis('Survival', ']', 2);
                    end;
                    //LogThis('Survival', 'Channel.fMembers.Delete.in', 0);
                    Channel.fMembers.Delete( Client );
                    //LogThis('Survival', 'Channel.fMembers.Delete.out', 0);
                    if (Channel.fMembers.Count = 0) and not Channel.SysChannel
                      then
                        begin
                          NotifyChannelListChange( Channel.fName, Channel.fPassword, uchExclusion );
                          //LogThis('Survival', 'fChannels.Delete.in', 0);
                          fChannels.Delete( Channel );
                          //LogThis('Survival', 'fChannels.Delete.out', 0);
                        end;
                  end;
            end;
      finally
        Unlock('ClientLeavedChannel');
      end;
    end;

  function TInterfaceServer.LaunchChannelSession( Client : TClientView; ChannelName : string ) : boolean;
    var
      Channel : TChannel;
      Session : TSession;
      Member  : TMember;
      i       : integer;
    begin
      Lock('LaunchChannelSession');
      try
        Channel := GetChannel( ChannelName );
        if Channel <> nil
          then
            begin
              Session := fSessions.CreateSession( Channel.fSessionAppId );
              //LogThis('Survival', 'LaunchChannelSession.fMembers.Lock', 1);
              Channel.fMembers.Lock;
              //LogThis('Survival', '[', 0);
              try
                for i := 0 to pred(Channel.fMembers.Count) do
                  begin
                    Member := TMember.Create( TClientView(Channel.fMembers[i]).fRealName, Session );
                    Session.Members.Insert( Member );
                    // >> launch file here...
                  end;
              finally
                Channel.fMembers.Unlock;
                //LogThis('Survival', ']', 2);
              end;
              result := true;
            end
          else result := false;
      finally
        Unlock('LaunchChannelSession');
      end;
    end;

  function TInterfaceServer.ConnectToGameMaster( ClientId : TCustomerId; UserInfo, GameMasters : widestring ) : OleVariant;
    begin
      try
        if fGMClientConn <> nil
          then result := fGMProxy.ConnectToGameMaster( fGMClientId, ClientId, UserInfo, GameMasters );
      except
        result := '';
      end;
    end;

  function TInterfaceServer.SendGMMessage( ClientId : TCustomerId; GMId : TGameMasterId; Msg : WideString ) : OleVariant;
    begin
      try
        if fGMClientConn <> nil
          then result := fGMProxy.SendGMMessage( fGMClientId, ClientId, GMId, Msg );
      except
        result := GM_ERR_UNEXPECTED;
      end;
    end;

  procedure TInterfaceServer.DisconnectUser( ClientId : TCustomerId; GMId : TGameMasterId );
    begin
      try
        if fGMClientConn <> nil
          then fGMProxy.DisconnectUser( fGMClientId, ClientId, GMId );
      except
      end;
    end;

  function TInterfaceServer.GameMasterMsg( ClientId : TCustomerId; Msg : WideString; Info : integer ) : OleVariant;
    var
      Customer : TClientView;
    begin
      try
        result := GM_ERR_NOERROR;
        Customer := GetClientByName( ClientId );
        if Customer <> nil
          then Customer.GameMasterMsg( Msg, Info )
          else result := GM_ERR_UNKNOWMCUST;
      except
        result := GM_ERR_UNEXPECTED;
      end;
    end;

  procedure TInterfaceServer.GMNotify( ClientId : TCustomerId; notID : integer; Info : WideString );
    var
      Customer : TClientView;
    begin
      try
        Customer := GetClientByName( ClientId );
        if Customer <> nil
          then Customer.GMNotify( notId, Info );
      except
      end;
    end;

  function TInterfaceServer.GetConfigParm(name, def : widestring) : olevariant;
    begin
      try
        Lock('GetConfigParm');
        try
          if not VarIsEmpty(WorldProxy)
            then result := WorldProxy.GetConfigParm(name, def)
            else result := def;
        finally
          Unlock('GetConfigParm');
        end;
      except
        result := def;
      end;
    end;

  procedure TInterfaceServer.CreateTownChannels;
    var
      tNames  : TStringList;
      i       : integer;
      Channel : TChannel;
      cName   : string;
    begin
      if not VarIsEmpty(WorldProxy)
        then
          begin
            tNames := TStringList.Create;
            tNames.Text := WorldProxy.GetTownNames;
            for i := 0 to pred(tNames.Count) do
              begin
                cName := tNames[i];
                Channel := TChannel.Create(cName, cName, '', '', '', 0);
                Channel.SysChannel := true;
                fChannels.Insert(Channel);
              end;
          end;
    end;

  procedure TInterfaceServer.ReportMaintenance(eta : TDateTime; LastDowntime : integer);
    var
      hh, mm, ss, ms : word;
      msg : string;
    begin
      try
        DecodeTime(eta, hh, mm, ss, ms);
        if mm > 0
          then SystemMsg('', mtidMaintDue, [mm, LastDowntime + 5])
          else
            if not fMaintDue
              then
                begin
                  // No more refreshing..
                  fRefreshThread.Terminate;
                  fRefreshThread.fEndEvn.SetEvent;
                  fRefreshThread.WaitFor;

                  fMaintDue := true;

                  // Stop MS calls
                  fDAOK := false;
                  //fServerBusy := true;

                  //fDAServerConn.StopListening;
                  Halt;
                end;
      except
        Logs.Log('Survival', TimeToStr(Now) + ' - Error in ReportMaintenance.' );
      end;
    end;

  // TModelEvents

  constructor TModelEvents.Create( anInterfaceServer : TInterfaceServer );
    begin
      inherited Create;
      fInterfaceServer := anInterfaceServer;
    end;

  procedure TModelEvents.RefreshArea( x, y, dx, dy : integer );
    begin
      try
        fInterfaceServer.RefreshArea( x, y, dx, dy );
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error refreshing area' );
        except
        end;
      end;
    end;

  procedure TModelEvents.RefreshObject( ObjId, KindOfChange : integer );
    begin
      try
        fInterfaceServer.RefreshObject( ObjId, KindOfChange );
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error refreshing object, KIND: ' + IntToStr(KindOfChange) );
        except
        end;
      end;
    end;

  procedure TModelEvents.RefreshTycoons( useless : integer );
    begin
      try
        fInterfaceServer.RefreshTycoons;
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error refreshing tycoons' );
        except
        end;
      end;
    end;

  procedure TModelEvents.RefreshDate( Date : TDateTime );
    begin
      try
        fInterfaceServer.RefreshDate( Date );
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error refreshing date' );
        except
        end;
      end;
    end;

  procedure TModelEvents.RefreshSeason( Season : integer );
    begin
      try
        fInterfaceServer.RefreshSeason( Season );
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error refreshing season' );
        except
        end;
      end;
    end;

  procedure TModelEvents.EndOfPeriod( useless : integer );
    begin
      try
        fInterfaceServer.EndOfPeriod;
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in EndOfPeriod' );
        except
        end;
      end;
    end;

  procedure TModelEvents.TycoonRetired( name : widestring );
    begin
      try
        fInterfaceServer.TycoonRetired( name );
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error retiring tycoon' );
        except
        end;
      end;
    end;

  procedure TModelEvents.SendTickData( PoolId, ViewerId, TickCount : integer; TickDataStr : widestring );
    begin
      try
        fInterfaceServer.SendTickData( PoolId, ViewerId, TickCount, TickDataStr );
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in tick data' );
        except
        end;
      end;
    end;

  procedure TModelEvents.SendNotification( TycoonId : integer; Kind : integer; Title, Body : widestring; Options : integer );
    begin
      try
        fInterfaceServer.SendNotification( TycoonId, Kind, Title, Body, Options );
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in send notification' );
        except
        end;
      end;
    end;

  procedure TModelEvents.ModelStatusChanged( Status : integer );
    begin
      try
        fInterfaceServer.ModelStatusChanged( Status );
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in ModelStatusChanged');
        except
        end;
      end;
    end;

  procedure TModelEvents.ReportMaintenance(eta : TDateTime; LastDowntime : integer);
    begin
      try
        fInterfaceServer.ReportMaintenance(eta, LastDowntime);
      except
        try
          Logs.Log('Survival', TimeToStr(Now) + ' - Error in ReportMaintenance');
        except
        end;
      end;
    end;


end.




