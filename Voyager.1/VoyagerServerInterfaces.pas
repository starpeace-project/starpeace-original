unit VoyagerServerInterfaces;

interface

  uses
    Protocol, Classes, Collection, SyncObjs, Matrix, GMKernel;

  const
    Voyager_NOERROR       = 0;
    Voyager_ERROR_Unknown = 1;


  // Object report

  type
    TObjectInfo =
      packed record
        VisualClass : TVisualClassId;
        CompanyId   : TCompanyId;
        Alert       : boolean;
        Level       : byte;
        x, y        : word;
      end;

    PObjectArray = ^TObjectArray;
    TObjectArray = array[0..0] of TObjectInfo;

    TObjectReport =
      record
        ObjectCount : integer;
        Objects     : PObjectArray;
      end;

  type
    TCargoPair =
      record
        node1 : integer;
        node2 : integer;
      end;
    TCargoArray = array[TCargoKind] of TCargoPair;

    TSegmentInfo =
      packed record
        x1, y1 : word;
        x2, y2 : word;
        Cargo  : TCargoArray;
      end;

    PSegmentArray = ^TSegmentArray;
    TSegmentArray = array[0..0] of TSegmentInfo;

    TSegmentReport =
      record
        SegmentCount : integer;
        Segments     : PSegmentArray;
      end;


  // Company report

  type
    TCompanyInfo =
      class
        public
          constructor Create( aName : string; anId : TCompanyId );
        private
          fName : string;
          fId   : TCompanyId;
        public
          property Name : string read fName;
          property Id   : TCompanyId read fId;
      end;

    TCompanyReport =
      class
        protected
          constructor Create;
          destructor  Destroy; override;
        private
          fCompanyList : TCollection;
        protected
          property CompanyList : TCollection read fCompanyList;
        private
          function GetCompanyCount : integer;
          function GetCompanyInfo( index : integer ) : TCompanyInfo;
        public
          property CompanyInfo[index : integer] : TCompanyInfo read GetCompanyInfo; default;
          property CompanyCount : integer read GetCompanyCount;
      end;

  type
    TOutputKind = (okPeople, okNormal);

    TInputKind = (ikPeople, ikBasic, ikNonBasic);

    TConnectionInfo =
      record
        x, y : integer;
        // other data expected
      end;

    PConnectionArray = ^TConnectionArray;
    TConnectionArray = array[0..0] of TConnectionInfo;

    TOutputInfo =
      record
        Kind        : TOutputKind;
        ConnCount   : integer;
        Connections : PConnectionArray;
      end;

    POutputArray = ^TOutputArray;
    TOutputArray = array[0..0] of TOutputInfo;

    TInputInfo =
      record
        Kind        : TInputKind;
        ConnCount   : integer;
        Connections : PConnectionArray;
      end;

    PInputArray = ^TInputArray;
    TInputArray = array[0..0] of TInputInfo;

    TCnxReport =
      record
        OutputCount : integer;
        Outputs     : POutputArray;
        InputCount  : integer;
        Inputs      : PInputArray;
      end;

  // Other types

  type
    TErrorCode     = integer;
    TObjId         = integer;
    TStatusText    = string;


  type
    IClientView =
      interface( IInterfaceServer )

        // IS ClientView functions
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
        procedure VoiceStatusChanged( Status : integer; out ErrorCode : TErrorCode );
        procedure VoiceTxOver( out ErrorCode : TErrorCode );
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

        // General IS functions
        function  getUserName : string;
        function  getUserMasterName : string;
        function  getUserPassword : string;
        function  getMoney : currency;
        function  getCompanyName : string;
        function  getCompanyId : TCompanyId;
        function  getTycoonId  : TObjId;
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

        // Aditional management
        procedure SuplantedBy( NewClientView : IClientView );
        procedure DisposeObjectReport ( var ObjectReport  : TObjectReport );
        procedure DisposeSegmentReport( var SegmentReport : TSegmentReport );
        procedure DisposeCnxReport( var CnxReport : TCnxReport );
        procedure SetAutologon( active : boolean );

        function Offline : boolean;

      end;


  type
    TAsyncRequestId = integer;

  const
    ridNull = 0;
    
  type
    TAsyncSetViewedAreaResponse     = procedure( RequestId : TAsyncRequestId;                          ErrorCode : TErrorCode ) of object;
    TAsyncObjectsInAreaResponse     = procedure( RequestId : TAsyncRequestId; Report : TObjectReport;  ErrorCode : TErrorCode ) of object;
    TAsyncObjectAtResponse          = procedure( RequestId : TAsyncRequestId; ObjId  : TObjId;         ErrorCode : TErrorCode ) of object;
    TAsyncObjectStatusTextResponse  = procedure( RequestId : TAsyncRequestId; Text   : TStatusText;    ErrorCode : TErrorCode ) of object;
    TAsyncObjectConnectionsResponse = procedure( RequestId : TAsyncRequestId; Report : TCnxReport;     ErrorCode : TErrorCode ) of object;
    TAsyncFocusObjectResponse       = procedure( RequestId : TAsyncRequestId;                          ErrorCode : TErrorCode ) of object;
    TAsyncUnFocusObjectResponse     = procedure( RequestId : TAsyncRequestId;                          ErrorCode : TErrorCode ) of object;
    TAsyncSwitchFocusResponse       = procedure( RequestId : TAsyncRequestId; ObjId  : TObjId;         ErrorCode : TErrorCode ) of object;
    TAsyncGetCompanyListResponse    = procedure( RequestId : TAsyncRequestId; Report : TCompanyReport; ErrorCode : TErrorCode ) of object;
    TAsyncNewCompanyResponse        = procedure( RequestId : TAsyncRequestId; Info   : TCompanyInfo;   ErrorCode : TErrorCode ) of object;
    TAsyncNewFacilityResponse       = procedure( RequestId : TAsyncRequestId;                          ErrorCode : TErrorCode ) of object;
    TAsyncGetUserListResponse       = procedure( RequestId : TAsyncRequestId; List   : TStringList;    ErrorCode : TErrorCode ) of object;
    TAsyncSayThisResponse           = procedure( RequestId : TAsyncRequestId;                          ErrorCode : TErrorCode ) of object;
    TAsyncMsgCompChangedResponse    = procedure( RequestId : TAsyncRequestId;                          ErrorCode : TErrorCode ) of object;
    TAsyncChaseResponse             = procedure( RequestId : TAsyncRequestId;                          ErrorCode : TErrorCode ) of object;
    TAsyncStopChaseResponse         = procedure( RequestId : TAsyncRequestId;                          ErrorCode : TErrorCode ) of object;
    TAsyncLogoffResponse            = procedure( RequestId : TAsyncRequestId;                          ErrorCode : TErrorCode ) of object;

  type
    IAsyncClientView =
      interface
        function SetViewedArea( x, y, dx, dy : integer; Notification : TAsyncSetViewedAreaResponse ) : TAsyncRequestId;
        function ObjectsInArea( x, y, dx, dy : integer; Notification : TAsyncObjectsInAreaResponse ) : TAsyncRequestId;
        function ObjectAt( x, y : integer; Notification : TAsyncObjectAtResponse ) : TAsyncRequestId;
        function ObjectStatusText( kind : TStatusKind; Id : TObjId; Notification : TAsyncObjectStatusTextResponse ) : TAsyncRequestId;
        function ObjectConnections( Id : TObjId; Notification : TAsyncObjectConnectionsResponse ) : TAsyncRequestId;
        function FocusObject( Id : TObjId; Notification : TAsyncFocusObjectResponse ) : TAsyncRequestId;
        function UnfocusObject( Id : TObjId; Notification : TAsyncUnfocusObjectResponse ) : TAsyncRequestId;
        function SwitchFocus( From : TObjId; toX, toY : integer; Notification : TAsyncSwitchFocusResponse ) : TAsyncRequestId;
        function GetCompanyList( Notification : TAsyncGetCompanyListResponse ) : TAsyncRequestId;
        function NewCompany( name, cluster : string; Notification : TAsyncNewCompanyResponse ) : TAsyncRequestId;
        function NewFacility( FacilityId : string; CompanyId : integer; x, y : integer; Notification : TAsyncNewFacilityResponse ) : TAsyncRequestId;
        function GetUserList( Notification : TAsyncGetUserListResponse ) : TAsyncRequestId;
        function SayThis( Dest, Msg : string; Notification : TAsyncSayThisResponse ) : TAsyncRequestId;
        function MsgCompositionChanged( State : TMsgCompositionState; Notification : TAsyncMsgCompChangedResponse ) : TAsyncRequestId;
        function Chase( UserName : string; Notification : TAsyncChaseResponse ) : TAsyncRequestId;
        function StopChase( Notification : TAsyncStopChaseResponse ) : TAsyncRequestId;
        function Logoff( Notification : TAsyncLogoffResponse ) : TAsyncRequestId;
        // Aditional stuff
        function  getClientView : IClientView;
        procedure DisposeObjectReport( var ObjectReport : TObjectReport );
        procedure DisposeSegmentReport( var SegmentReport : TSegmentReport );
        procedure DisposeCnxReport( var CnxReport : TCnxReport );
      end;


  // Utility funcs

  function ParseCompanyInfo( var text : string ) : TCompanyInfo;


implementation

  uses
    StrUtils, SysUtils;

    
  // TCompanyInfo

  constructor TCompanyInfo.Create( aName : string; anId : TCompanyId );
    begin
      inherited Create;
      fName := aName;
      fId   := anId;
    end;


  // TCompanyReport

  constructor TCompanyReport.Create;
    begin
      inherited Create;
      fCompanyList := TCollection.Create( 0, rkBelonguer );
    end;

  destructor TCompanyReport.Destroy;
    begin
      fCompanyList.Free;
      inherited;
    end;

  function TCompanyReport.GetCompanyCount : integer;
    begin
      result := fCompanyList.Count;
    end;

  function TCompanyReport.GetCompanyInfo( index : integer ) : TCompanyInfo;
    begin
      result := TCompanyInfo(fCompanyList[index]);
    end;



  // Utilities

  function ParseCompanyInfo( var text : string ) : TCompanyInfo;
    var
      p1, p2 : integer;
      name   : string;
      id     : TCompanyId;
    begin
      try
        if text[1] = '['
          then
            begin
              p1 := strutils.pos( ',', text, 1 );
              if p1 > 0
                then
                  begin
                    name := copy( text, 2, p1 - 2 );
                    p2 := strutils.pos( ']', text, p1 );
                    if p2 > 0
                      then
                        begin
                          id := StrToInt(copy( text, p1 + 1, p2 - p1 - 2 ));
                          result := TCompanyInfo.Create( name, id );
                        end
                      else result := nil
                  end
                else result := nil
            end
          else result := nil;
      except
        result := nil;
      end;
    end;

    
end.


