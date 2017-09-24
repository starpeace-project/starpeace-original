unit Circuits;

interface

  uses
    Windows, VoyagerServerInterfaces;

  type
    ICircuitsRendering =
      interface
      end;

  type
    IPointsArray =
      interface
        function  GetPointCount : integer;
        function  GetPointAt(idx : integer) : TPoint;
        procedure AddPoint(const Point : TPoint);
        procedure RemovePoint( idx : integer);
        function  HasPoint(x, y : integer) : boolean;
        procedure RemoveAll;
        function  Clone : IPointsArray;
        property PointCount : integer read GetPointCount;
        property Points[idx : integer] : TPoint read GetPointAt; default;
      end;

  type
    TPointsBuffer = array [1..1] of TPoint;
    PPointsBuffer = ^TPointsBuffer;

  type
    TPointsArray =
      class(TInterfacedObject, IPointsArray)
        private
          fPoints    : PPointsBuffer;
          fAllocated : integer;
          fCount     : integer;
        private // IPointsArray
          function  GetPointCount : integer;
          function  GetPointAt(idx : integer) : TPoint;
          procedure AddPoint(const Point : TPoint);
          procedure RemovePoint(idx : integer);
          function  HasPoint(x, y : integer) : boolean;
          procedure RemoveAll;
          function  Clone : IPointsArray;
        public
          constructor Create;
          constructor CreateEmpty;
          destructor  Destroy; override;
      end;

  function  FindCircuitSegments(x1, y1, x2, y2 : integer) : TSegmentReport;
  function  FindCircuitPoints(x1, y1, x2, y2 : integer) : IPointsArray;
  procedure ClipSegments(var SegmentReport : TSegmentReport; xmin, ymin, xmax, ymax : integer);

implementation

  uses
    SysUtils;

  // TPointsArray

  const
    idxNone = 0;

  const
    AllocChunk = 10;

  constructor TPointsArray.Create;
    begin
      inherited;
      fAllocated := AllocChunk;
      getmem(fPoints, AllocChunk*sizeof(TPoint));
    end;

  constructor TPointsArray.CreateEmpty;
    begin
      inherited;
    end;

  destructor TPointsArray.Destroy;
    begin
      if fPoints<>nil
        then freemem(fPoints);
      inherited;
    end;

  function TPointsArray.GetPointCount : integer;
    begin
      Result := fCount;
    end;

  function TPointsArray.GetPointAt( idx : integer ) : TPoint;
    begin
      if (idx >= 1 ) and (idx <= fCount)
        then Result := fPoints[idx]
        else raise Exception.Create( 'Illegal point index' );
    end;

  procedure TPointsArray.AddPoint(const Point : TPoint);
    begin
      if fCount = fAllocated
        then
          begin
            inc(fAllocated, AllocChunk);
            reallocmem(fPoints, fAllocated*sizeof(Point));
          end;
      inc(fCount);
      fPoints[fCount] := Point;
    end;

  procedure TPointsArray.RemovePoint(idx : integer);
    begin
      move(fPoints^[idx + 1], fPoints^[idx], (fCount - idx)*sizeof(fPoints[1]));
      dec(fCount);
    end;

  function TPointsArray.HasPoint(x, y : integer) : boolean;
    var
      PtIdx : integer;
    begin
      PtIdx := 1;
      while (PtIdx <= fCount) and ((fPoints[PtIdx].x <> x) or (fPoints[PtIdx].y <> y)) do
        inc(PtIdx);
      if PtIdx <= fCount
        then Result := true
        else Result := false;
    end;

  procedure TPointsArray.RemoveAll;
    begin
      fCount := 0;
      if fAllocated <> AllocChunk
        then
          begin
            fAllocated := AllocChunk;
            freemem(fPoints);
            getmem(fPoints, AllocChunk*sizeof(fPoints[1]));
          end;
    end;

  function TPointsArray.Clone : IPointsArray;
    var
      Tmp : TPointsArray;
    begin
      Tmp := TPointsArray.CreateEmpty;
      Tmp.fAllocated := fAllocated;
      Tmp.fCount := fCount;
      getMem(Tmp.fPoints, Tmp.fAllocated*sizeof(fPoints[1]));
      move(fPoints^, Tmp.fPoints^, fCount*sizeof(fPoints[1]));
      Result := Tmp;
    end;

  function FindCircuitSegments(x1, y1, x2, y2 : integer) : TSegmentReport;
    var
      x         : integer;
      y         : integer;
      deltax    : integer;
      deltay    : integer;
      SegReport : TSegmentReport;
      aSegment  : TSegmentInfo;

    procedure AddSegment(SegInfo : TSegmentInfo);
      begin
        with SegReport do
          begin
            inc( SegmentCount );
            reallocmem(Segments, SegmentCount*SizeOf(TSegmentInfo));
            Segments[SegmentCount - 1] := SegInfo;
          end
      end;

    begin
      with SegReport do
        begin
          SegmentCount := 0;
          Segments := nil
        end;
      x := x1;
      y := y1;
      if x2 > x1
        then deltax := 1
        else deltax := -1;
      if y2 > y1
        then deltay := 1
        else deltay := -1;
      if abs(x2 - x1) <= abs(y2 - y1)
        then // try to approach a vertical line
          begin
            while (x <> x2) and (y <> y2) do
              begin
                aSegment.x1 := x;
                aSegment.y1 := y;
                aSegment.x2 := x + deltax;
                aSegment.y2 := y;
                AddSegment(aSegment);
                aSegment.x1 := x + deltax;
                aSegment.y1 := y;
                aSegment.x2 := x + deltax;
                aSegment.y2 := y + deltay;
                AddSegment(aSegment);
                inc(y, deltay);
                inc(x, deltax);
              end;
            if y <> y2
              then
                begin
                  aSegment.x1 := x;
                  aSegment.y1 := y;
                  aSegment.x2 := x2;
                  aSegment.y2 := y2;
                  AddSegment(aSegment);
                end
          end
        else // try to approach a horizontal line
          begin
            while (x <> x2) and (y <> y2) do
              begin
                aSegment.x1 := x;
                aSegment.y1 := y;
                aSegment.x2 := x;
                aSegment.y2 := y + deltay;
                AddSegment(aSegment);
                aSegment.x1 := x;
                aSegment.y1 := y + deltay;
                aSegment.x2 := x + deltax;
                aSegment.y2 := y + deltay;
                AddSegment(aSegment);
                inc(y, deltay);
                inc(x, deltax);
              end;
            if x <> x2
              then
                begin
                  aSegment.x1 := x;
                  aSegment.y1 := y;
                  aSegment.x2 := x2;
                  aSegment.y2 := y2;
                  AddSegment(aSegment);
                end
          end;
      Result := SegReport;
    end;

  function FindCircuitPoints(x1, y1, x2, y2 : integer) : IPointsArray;
    var
      x          : integer;
      y          : integer;
      deltax     : integer;
      deltay     : integer;
      CircPoints : IPointsArray;
      aPoint     : TPoint;
    begin
      CircPoints := TPointsArray.Create;
      x := x1;
      y := y1;
      aPoint.x := x;
      aPoint.y := y;
      CircPoints.AddPoint(aPoint);
      if x2 > x1
        then deltax := 1
        else deltax := -1;
      if y2 > y1
        then deltay := 1
        else deltay := -1;
      if abs(x2 - x1) <= abs(y2 - y1)
        then // try to approach a vertical line
          begin
            while (x <> x2) and (y <> y2) do
              begin
                aPoint.x := x + deltax;
                aPoint.y := y;
                CircPoints.AddPoint(aPoint);
                aPoint.x := x + deltax;
                aPoint.y := y + deltay;
                CircPoints.AddPoint(aPoint);
                inc(y, deltay);
                inc(x, deltax);
              end;
            if y <> y2
              then
                repeat
                  inc(y, deltay);
                  aPoint.x := x;
                  aPoint.y := y;
                  CircPoints.AddPoint(aPoint);
                until y = y2;
          end
        else // try to approach a horizontal line
          begin
            while (x <> x2) and (y <> y2) do
              begin
                aPoint.x := x;
                aPoint.y := y + deltay;
                CircPoints.AddPoint(aPoint);
                aPoint.x := x + deltax;
                aPoint.y := y + deltay;
                CircPoints.AddPoint(aPoint);
                inc(y, deltay);
                inc(x, deltax);
              end;
            if x <> x2
              then
                repeat
                  inc(x, deltax);
                  aPoint.x := x;
                  aPoint.y := y;
                  CircPoints.AddPoint(aPoint);
                until x = x2;
          end;
      Result := CircPoints;
    end;

  procedure ClipSegments(var SegmentReport : TSegmentReport; xmin, ymin, xmax, ymax : integer);

    function ClipInteger(Val, Min, Max : integer) : integer;
      begin
        if Val < Min
          then Result := Min
          else
            if Val > Max
              then Result := Max
              else Result := Val;
      end;

    var
      SegIdx : integer;
      SegCnt : integer;
    begin
      SegCnt := 0;
      with SegmentReport do
        begin
          for SegIdx := 0 to pred(SegmentCount) do
            begin
              if (((Segments[SegIdx].x1 >= xmin) or (Segments[SegIdx].x2 >= xmin)) and
                 ((Segments[SegIdx].x1 <= xmax) or (Segments[SegIdx].x2 <= xmax))) and
                 (((Segments[SegIdx].y1 >= ymin) or (Segments[SegIdx].y2 >= ymin)) and
                 ((Segments[SegIdx].y1 <= ymax) or (Segments[SegIdx].y2 <= ymax)))
                then
                  begin
                    Segments[SegCnt].x1 := ClipInteger(Segments[SegIdx].x1, xmin, xmax);
                    Segments[SegCnt].x2 := ClipInteger(Segments[SegIdx].x2, xmin, xmax);
                    Segments[SegCnt].y1 := ClipInteger(Segments[SegIdx].y1, ymin, ymax);
                    Segments[SegCnt].y2 := ClipInteger(Segments[SegIdx].y2, ymin, ymax);
                    inc(SegCnt);
                  end;
            end;
          reallocmem(Segments, SegCnt*sizeof(Segments[0]));
          SegmentCount := SegCnt;
        end;
    end;

end.
