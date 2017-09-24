unit ClientView;

interface

  uses
    Windows, Classes, Controls, GameTypes, Protocol, MapTypes, VoyagerInterfaces,
    VoyagerServerInterfaces, Matrix;

  const
    evnSetWorldInfo = 65123;  // >>>>>

  const
    cMapRows = 1000;
    cMapCols = 1000;

  type
    TSetWorldInfo =
      record
        MapImage     : TGameImage;
        BuildClasses : IBuildingClassBag;
      end;

  type
    TTestMasterUrlHandler =
      class(TInterfacedObject, IMetaURLHandler, IMasterURLHandler, IClientView)
        public
          constructor Create;
          destructor  Destroy; override;
        private // IMetaURLHandler
          function getName : string;
          function getOptions : TURLHandlerOptions;
          function getCanHandleURL(URL : TURL) : THandlingAbility;
          function Instantiate : IURLHandler;
        private // IMasterURLHandler
          function  HandleURL(URL : TURL) : TURLHandlingResult;
          function  HandleEvent(EventId : TEventId; var info) : TEventHandlingResult;
          function  getControl : TControl;
          procedure setMasterURLHandler(URLHandler : IMasterURLHandler);
          function  RegisterMetaHandler(aMetaURLHandler : IMetaURLHandler) : TMetaHandlerRegistrationResult;
          procedure RegisterDefaultHandler(Handler : string);
          procedure RegisterExclusion(Excluder, ToExclude : string; mutual : boolean);
          procedure ReportNavigation( Handler : IURLHandler; URL : TURL; localBack, localForward : boolean );
          function  getURLIsLocal(URL : TURL) : boolean;
        private // IClientView
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
          function  Echo( value : integer ) : integer;
          procedure ClientAware;
          procedure ClientNotAware;
          function  getUserName : string;
          function  getMoney : currency;
          function  getCompanyName : string;
          function  getCompanyId : TCompanyId;
          function  getTycoonId : TObjId;
          function  getTycoonUId : integer;
          function  getDate : TDateTime;
          function  getSeason : integer;
          function  getWorldName : string;
          function  getWorldURL : string;
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
          procedure DisposeObjectReport ( var ObjectReport  : TObjectReport );
          procedure DisposeSegmentReport( var SegmentReport : TSegmentReport );
          procedure DisposeCnxReport( var CnxReport : TCnxReport );
          procedure SetAutologon( active : boolean );
          function  Offline : boolean;
        private
          fFocus        : TObjId;
          fBuildings    : array[0..pred(cMapRows), 0..pred(cMapCols)] of word;
          fRoadSegs     : TStringList;
          fRailroadSegs : TStringList;
          fWorldMap     : TGameImage;
          fBuildClasses : IBuildingClassBag;
          procedure CreateBuildings;
          procedure FillBuilding(ii, jj : integer; const bclass : TBuildingClass);
      end;

implementation

  uses
    SysUtils, Land, ServerCnxEvents, URLParser;

  const
    tidMetaHandlerName_TestServer = 'TestServer';

  type
    PSingleArray = ^TSingleArray;
    TSingleArray = array[0 .. 0] of single;

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
    end;

  function TTestMatrix.getElement( i, j : integer ) : single;
    begin
      result := fItems[i*fCols + j];
    end;

  procedure TTestMatrix.setElement( i, j : integer; value : single );
    begin
      fItems[i*fCols + j] := value;
    end;

  constructor TTestMasterUrlHandler.Create;
    begin
      inherited;
      fRoadSegs := TStringList.Create;
      fRoadSegs.LoadFromFile(getWorldURL + '\roads.dat');
      fRailroadSegs := TStringList.Create;
      fRailroadSegs.LoadFromFile( getWorldURL + '\railroads.dat');
    end;

  destructor TTestMasterUrlHandler.Destroy;
    begin
      assert(RefCount = 0);
      fRoadSegs.Free;
      fRailroadSegs.Free;
      inherited
    end;

  function TTestMasterUrlHandler.getName : string;
    begin
      Result := tidMetaHandlerName_TestServer;
    end;

  function TTestMasterUrlHandler.getOptions : TURLHandlerOptions;
    begin
      Result := [hopNonvisual];
    end;

  function TTestMasterUrlHandler.getCanHandleURL(URL : TURL) : THandlingAbility;
    begin
      Result := 0;
    end;

  function TTestMasterUrlHandler.Instantiate : IURLHandler;
    begin
      Result := IMasterUrlHandler(self);
    end;

  function TTestMasterUrlHandler.HandleURL(URL : TURL) : TURLHandlingResult;
    begin
      Result := urlNotHandled;
    end;

  function TTestMasterUrlHandler.HandleEvent(EventId : TEventId; var info) : TEventHandlingResult;
    var
      ClientView : IClientView absolute info;
    begin
      case EventId of
        evnAnswerClientView :
          begin
            ClientView := self;
            Result := evnHandled;
          end;
        evnSetWorldInfo :
          with TSetWorldInfo(info) do
            begin
              fWorldMap     := MapImage;
              fBuildClasses := BuildClasses;
              CreateBuildings;
              Result := evnHandled;
            end;
        else Result := evnNotHandled;
      end;
    end;

  function TTestMasterUrlHandler.getControl : TControl;
    begin
      Result := nil;
    end;

  procedure TTestMasterUrlHandler.setMasterURLHandler(URLHandler : IMasterURLHandler);
    begin
    end;

  function TTestMasterUrlHandler.RegisterMetaHandler(aMetaURLHandler : IMetaURLHandler) : TMetaHandlerRegistrationResult;
    begin
      Result := mhrRegistered;
    end;

  procedure TTestMasterUrlHandler.RegisterDefaultHandler(Handler : string);
    begin
    end;

  procedure TTestMasterUrlHandler.RegisterExclusion(Excluder, ToExclude : string; mutual : boolean);
    begin
    end;

  procedure TTestMasterUrlHandler.ReportNavigation( Handler : IURLHandler; URL : TURL; localBack, localForward : boolean );
    begin
    end;

  function TTestMasterUrlHandler.getURLIsLocal(URL : TURL) : boolean;
    begin
      Result := URLParser.GetAnchorData( URL ).FrameId = '';
    end;

  procedure TTestMasterUrlHandler.SetViewedArea(x, y, dx, dy : integer; out ErrorCode : TErrorCode);
    begin
      ErrorCode := NOERROR;
    end;

  function TTestMasterUrlHandler.ObjectsInArea(x, y, dx, dy : integer; out ErrorCode : TErrorCode) : TObjectReport;

    function CountObjects : integer;
      var
        i, j : integer;
      begin
        Result := 0;
        for i := y to y + dy do
          for j := x to x + dx do
            if (i <= cMapRows) and (j <= cMapCols) and (fBuildings[i, j] < pred(high(fBuildings[i, j])))
              then inc(Result);
      end;

    procedure AddObjects;
      var
        i, j : integer;
        idx  : integer;
      begin
        idx := 0;
        for i := y to y + dy do
          for j := x to x + dx do
            if (i <= cMapRows) and (j <= cMapCols) and (fBuildings[i, j] < pred(high(fBuildings[i, j])))
              then
                begin
                  Result.Objects[idx].VisualClass := fBuildings[i, j];
                  Result.Objects[idx].CompanyId   := (i + j) mod 6;
                  Result.Objects[idx].x           := j;
                  Result.Objects[idx].y           := i;
                  inc(idx);
                end;
      end;

    begin
      ErrorCode := NOERROR;
      Result.ObjectCount := CountObjects;
      if Result.ObjectCount > 0
        then
          begin
            getmem(Result.Objects, Result.ObjectCount*sizeof(Result.Objects[0]));
            AddObjects;
          end
        else Result.Objects := nil;
    end;

  function TTestMasterUrlHandler.ObjectAt(x, y : integer; out ErrorCode : TErrorCode) : TObjId;
    begin
      if fBuildings[y, x] < pred(high(fBuildings[y, x]))
        then Result := x*succ(high(word)) + succ(y)
        else Result := 0;
      ErrorCode := NOERROR;
    end;

  function TTestMasterUrlHandler.ObjectStatusText(kind : TStatusKind; Id : TObjId; out ErrorCode : TErrorCode) : TStatusText;
    var
      i, j : integer;
    begin
      j := id div succ(high(word));
      i := pred(id mod succ(high(word)));
      Result := IntToStr(ord(kind)) + ': (' + IntToStr(i) + ', ' + IntToStr(j) + ')' + #13#10 + 'Visual class:' + IntToStr(fBuildings[i, j]);
      ErrorCode := NOERROR;
    end;

  function TTestMasterUrlHandler.ContextText : string;
    begin
      Result := '';
    end;

  function TTestMasterUrlHandler.ObjectConnections(Id : TObjId; out ErrorCode : TErrorCode) : TCnxReport;
    begin
      ErrorCode := NOERROR;
      fillchar(Result, sizeof(Result), 0);
    end;

  procedure TTestMasterUrlHandler.FocusObject(Id : TObjId; out ErrorCode : TErrorCode);
    begin
      fFocus := id;
      ErrorCode := NOERROR;
    end;

  procedure TTestMasterUrlHandler.UnfocusObject(Id : TObjId; out ErrorCode : TErrorCode);
    begin
      fFocus := 0;
      ErrorCode := NOERROR;
    end;

  function TTestMasterUrlHandler.SwitchFocus(From : TObjId; toX, toY : integer; out ErrorCode : TErrorCode) : TObjId;
    begin
      if fBuildings[toY, toX] < pred(high(fBuildings[toY, toX]))
        then fFocus := toX*succ(high(word)) + succ(toY)
        else fFocus := 0;
      ErrorCode := NOERROR;
      Result := fFocus;
    end;

  function TTestMasterUrlHandler.GetCompanyList(out ErrorCode : TErrorCode) : TCompanyReport;
    begin
      Result    := nil;
      ErrorCode := NOERROR;
    end;

  function TTestMasterUrlHandler.NewCompany(name, cluster : string; out ErrorCode : TErrorCode) : TCompanyInfo;
    var
      TextualInfo : string;
    begin
      TextualInfo := '';
      Result      := ParseCompanyInfo(TextualInfo);
      ErrorCode   := NOERROR;
    end;

  procedure TTestMasterUrlHandler.NewFacility(FacilityId : string; CompanyId : integer; x, y : integer; out ErrorCode : TErrorCode);
    var
      theClass : PBuildingClass;
    begin
      theClass := fBuildClasses.Get(strtoint(FacilityId));
      assert(theClass <> nil);
      FillBuilding(y, x, theClass^);
      ErrorCode := NOERROR; // >>>>>
    end;

  function TTestMasterUrlHandler.ConnectFacilities( Facility1, Facility2 : TObjId; out ErrorCode : TErrorCode ) : string;
    begin
      Result := '';
    end;

  function TTestMasterUrlHandler.GetUserList(out ErrorCode : TErrorCode) : TStringList;
    begin
      Result := TStringList.Create;
      Result.Text := 'med'^M^J'mike';
      ErrorCode := NOERROR;
    end;

  function TTestMasterUrlHandler.GetChannelList(out ErrorCode : TErrorCode) : TStringList;
    begin
      Result := TStringList.Create;
      Result.Text := '';
      ErrorCode := NOERROR;
    end;

  procedure TTestMasterUrlHandler.SayThis(Dest, Msg : string; out ErrorCode : TErrorCode);
    begin
      ErrorCode := NOERROR;
    end;

  procedure TTestMasterUrlHandler.VoiceThis( const Buffer : array of byte; len, TxId, NewTx : integer; out ErrorCode : TErrorCode );
    begin
      ErrorCode := NOERROR;
    end;

  function TTestMasterUrlHandler.VoiceRequest(out ErrorCode : TErrorCode) : integer;
    begin
      ErrorCode := NOERROR;
      Result := 0;
    end;

  procedure TTestMasterUrlHandler.CancelVoiceRequest(out ErrorCode : TErrorCode);
    begin
      ErrorCode := NOERROR;
    end;

  procedure TTestMasterUrlHandler.VoiceStatusChanged(Status : integer; out ErrorCode : TErrorCode);
    begin
      ErrorCode := NOERROR;
    end;

  procedure TTestMasterUrlHandler.VoiceTxOver(out ErrorCode : TErrorCode);
    begin
      ErrorCode := NOERROR;
    end;

  procedure TTestMasterUrlHandler.CreateChannel(ChannelName, Password, aSessionApp, aSessionAppId : string; anUserLimit : integer; out ErrorCode : TErrorCode);
    begin
      ErrorCode := NOERROR;
    end;

  procedure TTestMasterUrlHandler.JoinChannel(ChannelName, Password : string; out ErrorCode : TErrorCode);
    begin
      ErrorCode := NOERROR;
    end;

  function TTestMasterUrlHandler.GetChannelInfo( ChannelName : string; out ErrorCode : TErrorCode ) : string;
    begin
      ErrorCode := NOERROR;
      Result := '';
    end;

  procedure TTestMasterUrlHandler.MsgCompositionChanged(State : TMsgCompositionState; out ErrorCode : TErrorCode);
    begin
      ErrorCode := NOERROR;
    end;

  procedure TTestMasterUrlHandler.Chase(UserName : string; out ErrorCode : TErrorCode);
    begin
      ErrorCode := NOERROR;
    end;

  procedure TTestMasterUrlHandler.StopChase(out ErrorCode : TErrorCode);
    begin
      ErrorCode := NOERROR;
    end;

  procedure TTestMasterUrlHandler.EnableEvents(out ErrorCode : TErrorCode);
    begin
      ErrorCode := NOERROR;
    end;

  procedure TTestMasterUrlHandler.DisableEvents(out ErrorCode : TErrorCode);
    begin
      ErrorCode := NOERROR;
    end;

  procedure TTestMasterUrlHandler.Logoff(out ErrorCode : TErrorCode);
    begin
      ErrorCode := NOERROR;
    end;

  procedure TTestMasterUrlHandler.CreateCircuitSeg(CircuitId, OwnerId, x1, y1, x2, y2, cost : integer; out ErrorCode : TErrorCode);
    var
      RawSegments : TStringList;
      FileName    : string;
    begin
      ErrorCode := NOERROR;
      case CircuitId of
        cirRoads:
          begin
            RawSegments := fRoadSegs;
            FileName := '\roads.dat';
          end;
        cirRailroads:
          begin
            RawSegments := fRailroadSegs;
            FileName := '\railroads.dat';
          end
        else RawSegments := nil;
      end;
      if RawSegments <> nil
        then
          begin
            RawSegments.Add(IntToStr(x1));
            RawSegments.Add(IntToStr(y1));
            RawSegments.Add(IntToStr(x2));
            RawSegments.Add(IntToStr(y2));
            RawSegments.SaveToFile(getWorldURL + FileName);
          end;
    end;

  procedure TTestMasterUrlHandler.BreakCircuitAt(CircuitId, OwnerId, x, y : integer; out ErrorCode : TErrorCode);
    begin
      ErrorCode := NOERROR;
    end;

  procedure TTestMasterUrlHandler.WipeCircuit(CircuitId, OwnerId, x1, y1, x2, y2 : integer; out ErrorCode : TErrorCode);
    begin
      ErrorCode := NOERROR;
    end;

  function TTestMasterUrlHandler.SegmentsInArea(CircuitId, x, y, dx, dy : integer; out ErrorCode : TErrorCode) : TSegmentReport;
    var
      SegmentReport : TSegmentReport;
      SegmentIdx    : integer;
      x1, y1        : integer;
      x2, y2        : integer;
      tmp           : integer;
      RawSegments   : TStringList;
    begin
      ErrorCode := NOERROR;
      case CircuitId of
        cirRoads:
          RawSegments := fRoadSegs;
        cirRailroads:
          RawSegments := fRailroadSegs
        else
          RawSegments := nil;
      end;
      if (RawSegments <> nil) and (RawSegments.Count <> 0)
        then
          with SegmentReport do
            begin
              getmem(Segments, RawSegments.Count div 4 * sizeof(TSegmentInfo));
              SegmentCount := 0;
              for SegmentIdx := 0 to RawSegments.Count div 4 - 1 do
                begin
                  x1 := StrToInt(RawSegments[SegmentIdx * 4]);
                  y1 := StrToInt(RawSegments[SegmentIdx * 4 + 1]);
                  x2 := StrToInt(RawSegments[SegmentIdx * 4 + 2]);
                  y2 := StrToInt(RawSegments[SegmentIdx * 4 + 3]);
                  if x1 > x2
                    then
                      begin
                        tmp := x1;
                        x1 := x2;
                        x2 := tmp
                      end;
                  if y1 > y2
                    then
                      begin
                        tmp := y1;
                        y1 := y2;
                        y2 := tmp
                      end;
                  if ( x1 < x + dx ) and ( x2 >= x ) and ( y1 < y + dx ) and ( y2 >= y )
                    then
                      begin
                        Segments[SegmentCount].x1 := x1;
                        Segments[SegmentCount].y1 := y1;
                        Segments[SegmentCount].x2 := x2;
                        Segments[SegmentCount].y2 := y2;
                        inc(SegmentCount);
                      end
                end;
              ReallocMem( Segments, SegmentCount * SizeOf( TSegmentInfo ) )
            end
        else
          begin
            SegmentReport.SegmentCount := 0;
            SegmentReport.Segments := nil
          end;
      Result := SegmentReport
    end;

  var
    curgen : integer = 0;

  function SinPlusCos(i, j : integer) : single;
    begin
      Result := cos(i*Pi/5) + sin(j*Pi/5);
    end;

  function SinMultCos(i, j : integer) : single;
    begin
      Result := cos(i*Pi/5)*sin(j*Pi/5);
    end;

  function SqSinPlusSqCos(i, j : integer) : single;
    begin
      Result := sqr(cos(i*Pi/5)) + sqr(sin(j*Pi/5));
    end;

  function TTestMasterUrlHandler.GetSurface( SurfaceId : string; x, y, dx, dy : integer; out ErrorCode : TErrorCode ) : IMatrix;
    type
      TSurfaceGenFun = function(i, j : integer) : single;

    type
      TSurfaceGenData =
        record
          fun : TSurfaceGenFun;
          min : single;
          max : single;
        end;

    const
      SurfaceGenData : array [0 .. 2] of TSurfaceGenData =
        (
          (fun : SinPlusCos; min : -2; max : 2), (fun : SinMultCos; min : -1; max : 1), (fun : SqSinPlusSqCos; min : 0; max : 2)
        );

    function ScaleValue(val, min, max : single) : single;
      begin
        Result := 10*(val - min)/(max - min);
      end;

    var
      SurfaceData : IMatrix;
      i, j        : integer;
    begin
      SurfaceData := TTestMatrix.Create;
      SurfaceData.SetDimensions(dy, dx);
      with SurfaceGenData[curgen] do
        for i := 0 to pred(dy) do
          for j := 0 to pred(dx) do
            SurfaceData.SetElement(i, j, ScaleValue(fun(i, j), min, max));
      curgen := (curgen + 1) mod 3;
      ErrorCode := NOERROR;
      Result := SurfaceData;
    end;

  procedure TTestMasterUrlHandler.DefineZone( TycoonId, ZoneId, x1, y1, x2, y2 : integer; out ErrorCode : TErrorCode );
    begin
      ErrorCode := NOERROR;
    end;

  function TTestMasterUrlHandler.GetCookie( CookieId : string; out ErrorCode : TErrorCode ) : string;
    begin
      Result := '';
    end;

  procedure TTestMasterUrlHandler.SetCookie( CookieId, ValueId : string; out ErrorCode : TErrorCode );
    begin
    end;

  procedure TTestMasterUrlHandler.CloneFacility( x, y : integer; LimitToTown, LimitToCompany : boolean );
    begin
    end;

  function TTestMasterUrlHandler.Echo( value : integer ) : integer;
    begin
      Result := value;
    end;

  procedure TTestMasterUrlHandler.ClientAware;
    begin
    end;

  procedure TTestMasterUrlHandler.ClientNotAware;
    begin
    end;

  function TTestMasterUrlHandler.getUserName : string;
    begin
      Result := 'med';
    end;

  function TTestMasterUrlHandler.getMoney : currency;
    begin
      Result := 400000;
    end;

  function TTestMasterUrlHandler.getCompanyName : string;
    begin
      Result := 'Merchise';
    end;

  function TTestMasterUrlHandler.getTycoonId : TObjId;
    begin
      Result := 1;
    end;

  function TTestMasterUrlHandler.getTycoonUId : integer;
    begin
      Result := 0;
    end;

  function TTestMasterUrlHandler.getCompanyId : TCompanyId;
    begin
      Result := 1; // >>>>
    end;

  function TTestMasterUrlHandler.getDate : TDateTime;
    begin
      Result := SysUtils.Date;
    end;

  function TTestMasterUrlHandler.getSeason : integer;
    begin
      Result := random(4);
    end;

  function TTestMasterUrlHandler.getWorldName : string;
    begin
      Result := 'Zyrane';
    end;

  function TTestMasterUrlHandler.getWorldURL : string;
    begin
      Result := ExtractFilePath(ParamStr(0)) + 'Cache';
    end;

  function TTestMasterUrlHandler.getWorldXSize : integer;
    begin
      Result := cMapCols;  // >>>>
    end;

  function TTestMasterUrlHandler.getWorldYSize : integer;
    begin
      Result := cMapRows;  // >>>>
    end;

  function TTestMasterUrlHandler.getDAAddr : string;
    begin
      Result := '';
    end;

  function TTestMasterUrlHandler.getDAPort : integer;
    begin
      Result := 0;
    end;

  function TTestMasterUrlHandler.getDALockPort : integer;
    begin
      Result := 0;
    end;

  function TTestMasterUrlHandler.getISAddr: string;
    begin
      Result := '220.0.1.128';
    end;

  function TTestMasterUrlHandler.getISPort: integer;
    begin
      Result := 0;
    end;

  function TTestMasterUrlHandler.getMailAddr : string;
    begin
      Result := 'med@merchise.vcl.sld.cu';
    end;

  function TTestMasterUrlHandler.getMailPort : integer;
    begin
      Result := 0;
    end;

  function TTestMasterUrlHandler.getMailAccount : string;
    begin
      Result := 'med';
    end;

  function TTestMasterUrlHandler.getCacheAddr : string;
    begin
      Result := '';
    end;

  function TTestMasterUrlHandler.getCachePort : integer;
    begin
      Result := 0;
    end;

  function TTestMasterUrlHandler.getSecurityId : string;
    begin
      Result := '0';
    end;

  function TTestMasterUrlHandler.getClientViewId : TObjId;
    begin
      Result := 0;
    end;

  function TTestMasterUrlHandler.getChasedUser : string;
    begin
      Result := '';
    end;

  procedure TTestMasterUrlHandler.SuplantedBy(NewClientView : IClientView);
    begin
    end;

  procedure TTestMasterUrlHandler.DisposeObjectReport(var ObjectReport : TObjectReport);
    begin
      with ObjectReport do
        begin
          freemem(Objects);
          ObjectCount := 0;
          Objects     := nil;
        end;
    end;

  procedure TTestMasterUrlHandler.DisposeSegmentReport( var SegmentReport : TSegmentReport );
    begin
      with SegmentReport do
        begin
          Freemem(Segments);
          SegmentCount := 0;
          Segments     := nil;
        end;
    end;

  procedure TTestMasterUrlHandler.DisposeCnxReport( var CnxReport : TCnxReport );
    begin
    end;

  procedure TTestMasterUrlHandler.SetAutologon( active : boolean );
    begin
    end;

  function TTestMasterUrlHandler.Offline : boolean;
    begin
      Result := false;
    end;

  procedure TTestMasterUrlHandler.CreateBuildings;
    const
      cLoopCount = 1;
    var
      i, j   : integer;
      loop   : integer;
      lid    : byte;
      bclass : PBuildingClass;
      {$IFNDEF USESMALLBCLASSSET}
      maxid  : integer;
      {$ENDIF}

    function RandomBClassId : integer;
      {$IFDEF USESMALLBCLASSSET}
      const
        BClassIdSet : array [0 .. 5] of integer = (2132, 2136, 2152, 2156, 2162, 2166);
      {$ENDIF}
      begin
        {$IFDEF USESMALLBCLASSSET}
        Result := BClassIdSet[random(6)];
        {$ELSE}
        Result := random(maxid);
        {$ENDIF}
      end;

    begin
      fillchar(fBuildings, sizeof(fBuildings), $FF);
      {$IFNDEF USESMALLBCLASSSET}
      maxid := fBuildClasses.GetMaxId;
      {$ENDIF}
      for loop := 1 to cLoopCount do
        for i := 0 to high(fBuildings) - 4 do
          for j := 0 to high(fBuildings[i]) - 4 do
            begin
              lid := byte(fWorldMap.PixelAddr[j, i, 0]^);
              if (LandTypeOf(lid) = ldtCenter) and (LandClassOf(lid) <> lncZoneD)
                then
                  begin
                    bclass := fBuildClasses.Get(RandomBClassId);
                    if (bclass <> nil) and (bclass.ImagePath <> '')
                      then FillBuilding(i, j, bclass^);
                  end;
            end;
    end;

  procedure TTestMasterUrlHandler.FillBuilding(ii, jj : integer; const bclass : TBuildingClass);
    var
      i, j   : integer;
      size   : integer;
      ok     : boolean;
      land   : TLandClass;
      lid    : byte;
    begin
      size := bclass.Size;
      ok   := true;
      lid  := byte(fWorldMap.PixelAddr[jj, ii, 0]^);
      land := LandClassOf(lid);
      i := ii;
      while (i < ii + size) and ok do
        begin
          j := jj;
          while (j < jj + size) and ok do
            begin
              lid := byte(fWorldMap.PixelAddr[j, i, 0]^);
              ok  := (LandTypeOf(lid) = ldtCenter) and (LandClassOf(lid) = land);
              inc(j);
            end;
          inc(i);
        end;
      if ok
        then
          begin
            j := jj;
            while (fBuildings[ii, j] = high(fBuildings[ii, j])) and (j < jj + size) do
              inc(j);
            if j >= jj + size
              then
                begin
                  i := ii;
                  while (fBuildings[i, jj] = high(fBuildings[i, jj])) and (i < ii + size) do
                    inc(i);
                  if i >= ii + size
                    then
                      begin
                        for i := ii to pred(ii + size) do
                          for j := jj to pred(jj + size) do
                            fBuildings[i, j] := pred(high(fBuildings[i, j]));
                        fBuildings[ii, jj] := bclass.id;
                      end;
                end;
          end;
    end;

end.
