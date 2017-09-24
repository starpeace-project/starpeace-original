unit AsyncServerCnxHandler;

interface

  uses
    VoyagerInterfaces, VoyagerServerInterfaces, Events, Protocol, Windows, Controls,
    RDOServer, RDOInterfaces, Classes, Collection;

  type
    TAsyncServerCnxHandler =
      class( TInterfacedObject, IMetaURLHandler, IURLHandler, IAsyncClientView )
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
        // IAsyncClientView
        private
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
        private
          function  getClientView : IClientView;
          procedure DisposeObjectReport( var ObjectReport : TObjectReport );
          procedure DisposeSegmentReport( var SegmentReport : TSegmentReport );
          procedure DisposeCnxReport( var CnxReport : TCnxReport );
        private
          fClientView    : IClientView;
          fLastRequestId : TAsyncRequestId;
        private
          function GetNextRequestId : TAsyncRequestId;
      end;

  const
    tidMetaHandlerName_AsyncServerCnx = 'AsyncServerCnx';

  const
    evnBase_AsyncCnxHandler = 50;

  const
    evnAnswerAsyncClientView = evnBase_AsyncCnxHandler + 0;
    

implementation

  uses
    ServerCnxHandler, ServerCnxEvents;
    

  // TAsyncServerCnxHandler

  constructor TAsyncServerCnxHandler.Create;
    begin
      inherited Create;
    end;

  destructor TAsyncServerCnxHandler.Destroy;
    begin
      inherited;
    end;

  function TAsyncServerCnxHandler.getName : string;
    begin
      result := tidMetaHandlerName_AsyncServerCnx;
    end;

  function TAsyncServerCnxHandler.getOptions : TURLHandlerOptions;
    begin
      result := [hopNonvisual];
    end;

  function TAsyncServerCnxHandler.getCanHandleURL( URL : TURL ) : THandlingAbility;
    begin
      result := 0;
    end;

  function TAsyncServerCnxHandler.Instantiate : IURLHandler;
    begin
      result := self;
    end;

  function TAsyncServerCnxHandler.HandleURL( URL : TURL ) : TURLHandlingResult;
    begin
      result := urlHandled;
    end;

  function TAsyncServerCnxHandler.HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
    var
      AsyncClientView : IAsyncClientView absolute info;
    begin
      if EventId = evnAnswerAsyncClientView
        then
          begin
            AsyncClientView := self;
            result := evnHandled;
          end
        else result := evnNotHandled;
    end;

  function TAsyncServerCnxHandler.getControl : TControl;
    begin
      result := nil;
    end;

  procedure TAsyncServerCnxHandler.setMasterURLHandler( URLHandler : IMasterURLHandler );
    begin
      fMasterURLHandler := URLHandler;
      fMasterURLHandler.HandleEvent( evnAnswerClientView, fClientView );
    end;

  function TAsyncServerCnxHandler.SetViewedArea( x, y, dx, dy : integer; Notification : TAsyncSetViewedAreaResponse ) : TAsyncRequestId;
    var
      ErrorCode : TErrorCode;
    begin
      try
        try
          fClientView.SetViewedArea( x, y, dx, dy, ErrorCode );
        except
          ErrorCode := ERROR_Unknown;
        end;
        result := GetNextRequestId;
        if assigned(Notification)
          then Notification( result, ErrorCode );
      except
        result := ridNull;
      end;
    end;

  function TAsyncServerCnxHandler.ObjectsInArea( x, y, dx, dy : integer; Notification : TAsyncObjectsInAreaResponse ) : TAsyncRequestId;
    var
      Report    : TObjectReport;
      ErrorCode : TErrorCode;
    begin
      try
        try
          Report := fClientView.ObjectsInArea( x, y, dx, dy, ErrorCode );
        except
          Report.ObjectCount := 0; 
          ErrorCode := ERROR_Unknown;
        end;
        result := GetNextRequestId;
        if assigned(Notification)
          then Notification( result, Report, ErrorCode );
      except
        result := ridNull;
      end;
    end;

  function TAsyncServerCnxHandler.ObjectAt( x, y : integer; Notification : TAsyncObjectAtResponse ) : TAsyncRequestId;
    var
      ObjId     : TObjId;
      ErrorCode : TErrorCode;
    begin
      try
        try
          ObjId := fClientView.ObjectAt( x, y, ErrorCode );
        except
          ObjId := 0;
          ErrorCode := ERROR_Unknown;
        end;
        result := GetNextRequestId;
        if assigned(Notification)
          then Notification( result, ObjId, ErrorCode );
      except
        result := ridNull;
      end;
    end;

  function TAsyncServerCnxHandler.ObjectStatusText( kind : TStatusKind; Id : TObjId; Notification : TAsyncObjectStatusTextResponse ) : TAsyncRequestId;
    var
      Text      : TStatusText;
      ErrorCode : TErrorCode;
    begin
      try
        try
          Text := fClientView.ObjectStatusText( kind, Id, ErrorCode );
        except
          Text := '';
          ErrorCode := ERROR_Unknown;
        end;
        result := GetNextRequestId;
        if assigned(Notification)
          then Notification( result, Text, ErrorCode );
      except
        result := ridNull;
      end;
    end;

  function TAsyncServerCnxHandler.ObjectConnections( Id : TObjId; Notification : TAsyncObjectConnectionsResponse ) : TAsyncRequestId;
    var
      Report    : TCnxReport;
      ErrorCode : TErrorCode;
    begin
      try
        try
          Report := fClientView.ObjectConnections( Id, ErrorCode );
        except
          ErrorCode := ERROR_Unknown;
        end;
        Report.OutputCount := 0;
        Report.Outputs     := nil;
        Report.InputCount  := 0;
        Report.Inputs      := nil;
        result := GetNextRequestId;
        if assigned(Notification)
          then Notification( result, Report, ErrorCode );
      except
        result := ridNull;
      end;
    end;

  function TAsyncServerCnxHandler.FocusObject( Id : TObjId; Notification : TAsyncFocusObjectResponse ) : TAsyncRequestId;
    var
      ErrorCode : TErrorCode;
    begin
      try
        try
          fClientView.FocusObject( Id, ErrorCode );
        except
          ErrorCode := ERROR_Unknown;
        end;
        result := GetNextRequestId;
        if assigned(Notification)
          then Notification( result, ErrorCode );
      except
        result := ridNull;
      end;
    end;

  function TAsyncServerCnxHandler.UnfocusObject( Id : TObjId; Notification : TAsyncUnfocusObjectResponse ) : TAsyncRequestId;
    var
      ErrorCode : TErrorCode;
    begin
      try
        try
          fClientView.UnfocusObject( Id, ErrorCode );
        except
          ErrorCode := ERROR_Unknown;
        end;
        result := GetNextRequestId;
        if assigned(Notification)
          then Notification( result, ErrorCode );
      except
        result := ridNull;
      end;
    end;

  function TAsyncServerCnxHandler.SwitchFocus( From : TObjId; toX, toY : integer; Notification : TAsyncSwitchFocusResponse ) : TAsyncRequestId;
    var
      ObjId     : TObjId;
      ErrorCode : TErrorCode;
    begin
      try
        try
          ObjId := fClientView.SwitchFocus( From, toX, toY, ErrorCode );
        except
          ObjId := 0;
          ErrorCode := ERROR_Unknown;
        end;
        result := GetNextRequestId;
        if assigned(Notification)
          then Notification( result, ObjId, ErrorCode );
      except
        result := ridNull;
      end;
    end;

  function TAsyncServerCnxHandler.GetCompanyList( Notification : TAsyncGetCompanyListResponse ) : TAsyncRequestId;
    var
      Report    : TCompanyReport;
      ErrorCode : TErrorCode;
    begin
      try
        try
          Report := fClientView.GetCompanyList( ErrorCode );
        except
          Report := nil;
          ErrorCode := ERROR_Unknown;
        end;
        result := GetNextRequestId;
        if assigned(Notification)
          then Notification( result, Report, ErrorCode );
      except
        result := ridNull;
      end;
    end;

  function TAsyncServerCnxHandler.NewCompany( name, cluster : string; Notification : TAsyncNewCompanyResponse ) : TAsyncRequestId;
    var
      Info      : TCompanyInfo;
      ErrorCode : TErrorCode;
    begin
      try
        try
          Info := fClientView.NewCompany( name, cluster, ErrorCode );
        except
          Info := nil;
          ErrorCode := ERROR_Unknown;
        end;
        result := GetNextRequestId;
        if assigned(Notification)
          then Notification( result, Info, ErrorCode );
      except
        result := ridNull;
      end;
    end;

  function TAsyncServerCnxHandler.NewFacility( FacilityId : string; CompanyId : integer; x, y : integer; Notification : TAsyncNewFacilityResponse ) : TAsyncRequestId;
    var
      ErrorCode : TErrorCode;
    begin
      try
        try
          fClientView.NewFacility( FacilityId, CompanyId, x, y, ErrorCode );
        except
          ErrorCode := ERROR_Unknown;
        end;
        result := GetNextRequestId;
        if assigned(Notification)
          then Notification( result, ErrorCode );
      except
        result := ridNull;
      end;
    end;

  function TAsyncServerCnxHandler.GetUserList( Notification : TAsyncGetUserListResponse ) : TAsyncRequestId;
    var
      List      : TStringList;
      ErrorCode : TErrorCode;
    begin
      try
        try
          List := fClientView.GetUserList( ErrorCode );
        except
          List := nil;
          ErrorCode := ERROR_Unknown;
        end;
        result := GetNextRequestId;
        if assigned(Notification)
          then Notification( result, List, ErrorCode );
      except
        result := ridNull;
      end;
    end;

  function TAsyncServerCnxHandler.SayThis( Dest, Msg : string; Notification : TAsyncSayThisResponse ) : TAsyncRequestId;
    var
      ErrorCode : TErrorCode;
    begin
      try
        try
          fClientView.SayThis( Dest, Msg, ErrorCode );
        except
          ErrorCode := ERROR_Unknown;
        end;
        result := GetNextRequestId;
        if assigned(Notification)
          then Notification( result, ErrorCode );
      except
        result := ridNull;
      end;
    end;

  function TAsyncServerCnxHandler.MsgCompositionChanged( State : TMsgCompositionState; Notification : TAsyncMsgCompChangedResponse ) : TAsyncRequestId;
    var
      ErrorCode : TErrorCode;
    begin
      try
        try
          fClientView.MsgCompositionChanged( State, ErrorCode );
        except
          ErrorCode := ERROR_Unknown;
        end;
        result := GetNextRequestId;
        if assigned(Notification)
          then Notification( result, ErrorCode );
      except
        result := ridNull;
      end;
    end;

  function TAsyncServerCnxHandler.Chase( UserName : string; Notification : TAsyncChaseResponse ) : TAsyncRequestId;
    var
      ErrorCode : TErrorCode;
    begin
      try
        try
          fClientView.Chase( UserName, ErrorCode );
        except
          ErrorCode := ERROR_Unknown;
        end;
        result := GetNextRequestId;
        if assigned(Notification)
          then Notification( result, ErrorCode );
      except
        result := ridNull;
      end;
    end;

  function TAsyncServerCnxHandler.StopChase( Notification : TAsyncStopChaseResponse ) : TAsyncRequestId;
    var
      ErrorCode : TErrorCode;
    begin
      try
        try
          fClientView.StopChase( ErrorCode );
        except
          ErrorCode := ERROR_Unknown;
        end;
        result := GetNextRequestId;
        if assigned(Notification)
          then Notification( result, ErrorCode );
      except
        result := ridNull;
      end;
    end;

  function TAsyncServerCnxHandler.Logoff( Notification : TAsyncLogoffResponse ) : TAsyncRequestId;
    var
      ErrorCode : TErrorCode;
    begin
      try
        try
          fClientView.Logoff( ErrorCode );
        except
          ErrorCode := ERROR_Unknown;
        end;
        result := GetNextRequestId;
        if assigned(Notification)
          then Notification( result, ErrorCode );
      except
        result := ridNull;
      end;
    end;

  function TAsyncServerCnxHandler.GetNextRequestId : TAsyncRequestId;
    begin
      inc( fLastRequestId );
      result := fLastRequestId;
    end;

  function TAsyncServerCnxHandler.getClientView : IClientView;
    begin
      result := fClientView;
    end;

  procedure TAsyncServerCnxHandler.DisposeObjectReport( var ObjectReport : TObjectReport );
    begin
      fClientView.DisposeObjectReport( ObjectReport );
    end;

  procedure TAsyncServerCnxHandler.DisposeSegmentReport( var SegmentReport : TSegmentReport );
    begin
      fClientView.DisposeSegmentReport( SegmentReport );
    end;

  procedure TAsyncServerCnxHandler.DisposeCnxReport( var CnxReport : TCnxReport );
    begin
      fClientView.DisposeCnxReport( CnxReport );
    end;

end.
