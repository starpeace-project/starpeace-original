unit CircuitsHandler;

interface

  uses
    Controls,
    Classes,
    Circuits,
    VoyagerServerInterfaces;

  type
    TSegmentsRenderer = function (const SegmentsReport : TSegmentReport; Left, Top, Width, Height : integer) : ICircuitsRendering;

  type
    TOnAreaRefreshedNotification = procedure (const Segments : TSegmentReport; const CircuitsRendering : ICircuitsRendering) of object;

  type
    ICircuitsHandler =
      interface
        procedure SetClientView(const ClientView : IClientView);
        function  RefreshArea(CircuitKind, x, y, dx, dy : integer; Renderer : TSegmentsRenderer; out Segments : TSegmentReport) : ICircuitsRendering;
        procedure AsyncRefreshArea(CircuitKind, x, y, dx, dy : integer; Renderer : TSegmentsRenderer; OnAreaRefreshed : TOnAreaRefreshedNotification);
      end;

  type
    TCircuitsHandler =
      class( TInterfacedObject, ICircuitsHandler )
        public
          constructor Create;
          destructor  Destroy; override;
        private // ICircuitsHandler
          procedure SetClientView(const ClientView : IClientView);
          function  RefreshArea(CircuitKind, x, y, dx, dy : integer; Renderer : TSegmentsRenderer; out Segments : TSegmentReport) : ICircuitsRendering;
          procedure AsyncRefreshArea(CircuitKind, x, y, dx, dy : integer; Renderer : TSegmentsRenderer; OnAreaRefreshed : TOnAreaRefreshedNotification );
        private
          fClientView : IClientView;
      end;

implementation

  uses
    SysUtils, Protocol;

  type
    TAreaCircuitsRefresherThread =
      class( TThread )
        private
          fCircuitKind     : integer;
          fX, fY, fDX, fDY : integer;
          fClientView      : IClientView;
          fRenderer        : TSegmentsRenderer;
          fOnAreaRefreshed : TOnAreaRefreshedNotification;
        public
          constructor Create(CircuitKind, x, y, dx, dy : integer; ClientView : IClientView; Renderer : TSegmentsRenderer; OnAreaRefreshed : TOnAreaRefreshedNotification);
        protected
          procedure Execute; override;
      end;

  // TAreaCircuitsRefresherThread

  constructor TAreaCircuitsRefresherThread.Create(CircuitKind, x, y, dx, dy : integer; ClientView : IClientView; Renderer : TSegmentsRenderer; OnAreaRefreshed : TOnAreaRefreshedNotification);
    begin
      inherited Create(true);
      fCircuitKind := CircuitKind;
      fX := x;
      fY := y;
      fDX := dx;
      fDY := dy;
      fClientView := ClientView;
      fRenderer := Renderer;
      fOnAreaRefreshed := OnAreaRefreshed;
      Resume;
    end;

  procedure TAreaCircuitsRefresherThread.Execute;
    var
      CircuitsRendering : ICircuitsRendering;
      SegmentReport     : TSegmentReport;
      ErrorCode         : TErrorCode;
      NewX              : integer;
      NewY              : integer;
    begin
      if fX > 0
        then NewX := fX - 1
        else NewX := 0;
      if fY > 0
        then NewY := fY - 1
        else NewY := 0;
      SegmentReport := fClientView.SegmentsInArea(fCircuitKind, NewX, NewY, fDX + 2, fDY + 2, ErrorCode);
      if ErrorCode = NOERROR
        then
          begin
            //ClipSegments( SegmentReport, NewX, NewY, NewX + fDX + 1, NewY + fDY + 1 );
            if assigned(fRenderer)
              then CircuitsRendering := fRenderer(SegmentReport, NewX, NewY, fDX + 2, fDY + 2)
              else CircuitsRendering := nil;
            if assigned(fOnAreaRefreshed)
              then fOnAreaRefreshed(SegmentReport, CircuitsRendering);
          end
        else
          if assigned(fOnAreaRefreshed)
            then fOnAreaRefreshed(SegmentReport, nil);
    end;

  // TCircuitsHandler

  constructor TCircuitsHandler.Create;
    begin
      inherited Create;
    end;

  destructor TCircuitsHandler.Destroy;
    begin
      assert(RefCount = 0);
      inherited;
    end;

  procedure TCircuitsHandler.SetClientView(const ClientView : IClientView);
    begin
      fClientView := ClientView;
    end;
    
  function TCircuitsHandler.RefreshArea(CircuitKind, x, y, dx, dy : integer; Renderer : TSegmentsRenderer; out Segments : TSegmentReport) : ICircuitsRendering;
    var
      ErrorCode : TErrorCode;
      NewX      : integer;
      NewY      : integer;
    begin
      if x > 0
        then NewX := x - 1
        else NewX := 0;
      if Y > 0
        then NewY := y - 1
        else NewY := 0;
      Segments := fClientView.SegmentsInArea(CircuitKind, NewX, NewY, dx + 2, dy + 2, ErrorCode);
      if ErrorCode = NOERROR
        then
          begin
            //ClipSegments(SegmentReport, NewX, NewY, NewX + dx + 1, NewY + dy + 1);
            if assigned(Renderer)
              then Result := Renderer(Segments, NewX, NewY, dx + 2, dy + 2)
              else Result := nil;
          end
        else Result := nil;
    end;

  procedure TCircuitsHandler.AsyncRefreshArea(CircuitKind, x, y, dx, dy : integer; Renderer : TSegmentsRenderer; OnAreaRefreshed : TOnAreaRefreshedNotification);
    begin
      TAreaCircuitsRefresherThread.Create(CircuitKind, x, y, dx, dy, fClientView, Renderer, OnAreaRefreshed);
    end;

end.
