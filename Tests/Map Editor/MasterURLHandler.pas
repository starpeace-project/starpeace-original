unit MasterURLHandler;

interface

  uses
    Windows, Classes, Controls, GameTypes, Protocol, MapTypes, VoyagerInterfaces;

  type
    TSetWorldInfo =
      record
        MapImage     : TMapImage;
        BuildClasses : IBuildingClassBag;
      end;

  type
    TTestMasterUrlHandler =
      class(TInterfacedObject, IMetaURLHandler, IMasterURLHandler)
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
          procedure CreateCircuitSeg( CircuitId, OwnerId, x1, y1, x2, y2, cost : integer; out ErrorCode : TErrorCode );
          procedure BreakCircuitAt( CircuitId, OwnerId, x, y : integer; out ErrorCode : TErrorCode );
          procedure WipeCircuit( CircuitId, OwnerId, x1, y1, x2, y2 : integer; out ErrorCode : TErrorCode );
          function  SegmentsInArea( CircuitId, x, y, dx, dy : integer; out ErrorCode : TErrorCode ) : TSegmentReport;
          procedure DisposeSegmentReport( var SegmentReport : TSegmentReport );
        private
          fRoadSegs     : TSegmentReport;
          fRailroadSegs : TSegmentReport;
      end;

implementation

  uses
    SysUtils, Land, URLParser;

  const
    tidMetaHandlerName_TestServer = 'TestServer';

  constructor TTestMasterUrlHandler.Create;
    var
      rawsegs : TStringList;
      segment : string;
      coord   : string;
      i       : integer;
    begin
      inherited;
      rawsegs := TStringList.Create;
      try
        //rawsegs.LoadFromFile(getWorldURL + '\roads.' + getWorldName + '.dat');
        if rawsegs.count > 0
          then
            with fRoadSegs do
              begin
                getmem(Segments, rawsegs.Count*sizeof(Segments[0]));
                SegmentCount := rawsegs.Count;
                for i := 0 to pred(rawsegs.Count) do
                  begin
                    segment := rawsegs[i];
                    coord := copy(segment, 1, pred(Pos(' ', segment)));
                    delete(segment, 1, Pos(' ', segment));
                    Segments[i].x1 := StrToInt(coord);
                    coord := copy(segment, 1, pred(Pos(' ', segment)));
                    delete(segment, 1, Pos(' ', segment));
                    Segments[i].y1 := StrToInt(coord);
                    coord := copy(segment, 1, pred(Pos(' ', segment)));
                    delete(segment, 1, Pos(' ', segment));
                    Segments[i].x2 := StrToInt(coord);
                    coord := segment;
                    Segments[i].y2 := StrToInt(coord);
                  end;
              end
          else
            begin
              fRoadSegs.SegmentCount := 0;
              fRoadSegs.Segments := nil
            end;
        fRailroadSegs.SegmentCount := 0;
        fRailroadSegs.Segments := nil;
        //segments.LoadFromFile(getWorldURL + '\railroads.' + getWorldName + '.dat');
      finally
        rawsegs.Free;
      end;
    end;

  destructor TTestMasterUrlHandler.Destroy;
    begin
      assert(RefCount = 0);
      DisposeSegmentReport(fRoadSegs);
      DisposeSegmentReport(fRailroadSegs);
      inherited;
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
    begin
      Result := evnNotHandled;
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

  procedure TTestMasterUrlHandler.CreateCircuitSeg(CircuitId, OwnerId, x1, y1, x2, y2, cost : integer; out ErrorCode : TErrorCode);
    var
      RawSegments : TSegmentReport;
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
          end;
      end;
      if (CircuitId = cirRoads) or (CircuitId = cirRailroads)
        then
          with RawSegments do
            begin
              reallocmem(Segments, (SegmentCount + 1)*sizeof(Segments[0]));
              Segments[SegmentCount].x1 := x1;
              Segments[SegmentCount].y1 := y1;
              Segments[SegmentCount].x2 := x2;
              Segments[SegmentCount].y2 := y2;
              inc(SegmentCount);
            end;
      case CircuitId of
        cirRoads:
          fRoadSegs := RawSegments;
        cirRailroads:
          fRailroadSegs := RawSegments;
      end;
      //RawSegments.SaveToFile(getWorldURL + FileName);
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
      i             : integer;
      x1, y1        : integer;
      x2, y2        : integer;
      tmp           : integer;
      DesiredSegs   : TSegmentReport;
    begin
      ErrorCode := NOERROR;
      case CircuitId of
        cirRoads:
          DesiredSegs := fRoadSegs;
        cirRailroads:
          DesiredSegs := fRailroadSegs
        else DesiredSegs.Segments := nil;
      end;
      if (DesiredSegs.Segments <> nil) and (DesiredSegs.SegmentCount <> 0)
        then
          with SegmentReport do
            begin
              getmem(Segments, DesiredSegs.SegmentCount*sizeof(Segments[0]));
              SegmentCount := 0;
              for i := 0 to pred(DesiredSegs.SegmentCount) do
                begin
                  x1 := DesiredSegs.Segments[i].x1;
                  y1 := DesiredSegs.Segments[i].y1;
                  x2 := DesiredSegs.Segments[i].x2;
                  y2 := DesiredSegs.Segments[i].y2;
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
                  if (x1 < x + dx) and (x2 >= x) and (y1 < y + dx) and (y2 >= y)
                    then
                      begin
                        Segments[SegmentCount].x1 := x1;
                        Segments[SegmentCount].y1 := y1;
                        Segments[SegmentCount].x2 := x2;
                        Segments[SegmentCount].y2 := y2;
                        inc(SegmentCount);
                      end;
                end;
              reallocmem(Segments, SegmentCount*sizeof(Segments[0]))
            end
        else
          begin
            SegmentReport.SegmentCount := 0;
            SegmentReport.Segments := nil
          end;
      Result := SegmentReport
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

end.
