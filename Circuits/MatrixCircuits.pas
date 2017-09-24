unit MatrixCircuits;

interface

  uses
    Collection,Circuits, LargeMatrix, Windows, BackupInterfaces, ReachMatrix;

  type
    TMatrixCircuitMap =
      class( TCircuitMap )
        private
          fMatrix   : TIntegerLargeMatrix;
          fModified : boolean;
        public
          destructor  Destroy; override;
        public
          procedure CreateSegment( x1, y1, x2, y2 : integer; OwnerId : TOwnerId; out ErrorCode : TCircuitErrorCode ); override;
          procedure BreakSegmentInPoint( x, y : integer; OwnerId : TOwnerId; out ErrorCode : TCircuitErrorCode ); override;
          procedure NearestCircuitsToArea( Area : TRect; tolerance : integer; var Circuits : TCollection; out ErrorCode : TCircuitErrorCode ); override;
          function  AreasAreConnected( Area1, Area2 : TRect; tolerance : integer; out ErrorCode : TCircuitErrorCode ) : boolean; override;
          function  AreaIsClear( Area : TRect ) : boolean; override;
        public
          procedure Render(Notify : TOnRenderRoadBlock); override;
          procedure SetSize( aXsize, aYsize : integer ); override;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        public // for debugging only
          property Matrix : TIntegerLargeMatrix read fMatrix;
        public
          function  GetReachMatrix(x1, y1, x2, y2, tolerance : integer) : TReachMatrix; override;
      end;

  procedure RegisterBackup;

implementation

  uses
    SysUtils, Logs, CircuitEquivalences;

  destructor TMatrixCircuitMap.Destroy;
    begin
      fMatrix.Free;
      inherited;
    end;

  procedure TMatrixCircuitMap.CreateSegment( x1, y1, x2, y2 : integer; OwnerId : TOwnerId; out ErrorCode : TCircuitErrorCode );
    begin
      Lock;
      try
        inherited;
        fModified := true;
      finally
        Unlock;
      end;
    end;

  procedure TMatrixCircuitMap.BreakSegmentInPoint( x, y : integer; OwnerId : TOwnerId; out ErrorCode : TCircuitErrorCode );
    begin
      Lock;
      try
        inherited;
        fModified := true;
      finally
        Unlock;
      end;
    end;

  procedure TMatrixCircuitMap.NearestCircuitsToArea( Area : TRect; tolerance : integer; var Circuits : TCollection; out ErrorCode : TCircuitErrorCode );
    var
      i, j    : integer;
      Circuit : TCircuit;
    begin
      Lock;
      try
        if false // >> fModified
          then inherited
          else
            try
              InflateRect( Area, tolerance, tolerance );
              for i := Area.Top to Area.Bottom do
                for j := Area.Left to Area.Right do
                  begin
                    Circuit := TCircuit(fMatrix[i,j]);
                    if (Circuit <> nil) and (Circuits.IndexOf( Circuit ) = NoIndex)
                      then Circuits.Insert( Circuit );
                  end;
            except
              ErrorCode := CIRCUIT_ERROR_Unknown;
            end;
      finally
        Unlock;
      end;
    end;

  function TMatrixCircuitMap.AreasAreConnected( Area1, Area2 : TRect; tolerance : integer; out ErrorCode : TCircuitErrorCode ) : boolean;
    begin
      result := inherited AreasAreConnected( Area1, Area2, tolerance, ErrorCode );
    end;

  function TMatrixCircuitMap.AreaIsClear( Area : TRect ) : boolean;
    var
      i, j : integer;
    begin
      Lock;
      try
        if false // fModified
          then result := inherited AreaIsClear( Area )
          else
            begin
                result := true;
                i      := Area.Top;
                j      := Area.Left;
                while (i <= Area.Bottom) and result do
                  begin
                    while (j <= Area.Right) and result do
                      begin
                        result := fMatrix[i,j] = 0;
                        inc( j );
                      end;
                    inc( i );
                  end;
            end;
      finally
        Unlock;
      end;
    end;

  procedure TMatrixCircuitMap.Render(Notify : TOnRenderRoadBlock);
    var
      Segs    : TCollection;
      Segment : TSegment;
      i, x, y : integer;
      Circuit : TCircuit;
      Eqs     : TEquivalences;
      itr     : integer;
    begin
      Lock;
      try
        inherited;
        try
          Segs := TCollection.Create( 0, rkUse );
          try
            Eqs := TEquivalences.Create;
            try
              itr := 0;
              repeat
                Eqs.Clear;
                fMatrix.Fill( 0 );
                FindSegsInArea( 0, 0, fMatrix.Cols, fMatrix.Rows, Segs );
                for i := 0 to pred(Segs.Count) do
                  begin
                    Segment := TSegment(Segs[i]);
                    for y := Segment.NodeA.y to Segment.NodeB.y do
                      for x := Segment.NodeA.x to Segment.NodeB.x do
                        begin
                          Circuit := TCircuit(fMatrix[y,x]);
                          if (circuit <> nil) and (circuit <> Segment.Circuit)
                            then
                              begin
                                Eqs.AddEquivalence(Circuit, Segment.Circuit);
                                //Logs.Log('Circuits', DateTimeToStr(Now) + Format(' Possible Road Bug at: %d, %d Circuits: %d(%d) and %d(%d)', [x, y, integer(Circuit), Circuit.Segments.Count, integer(Segment.Circuit), Segment.Circuit.Segments.Count]));
                              end;
                          fMatrix[y,x] := integer(Segment.Circuit);
                          Notify(x, y);
                        end;
                  end;
                  for i := 0 to pred(Eqs.Equivalences.Count) do
                    MixCircuits(TEquivalence(Eqs.Equivalences[i]).Circuits);
                  inc(itr);
                  Segs.DeleteAll;
                until (itr >= 2) or (Eqs.Equivalences.Count = 0)
              finally
                Eqs.Free;
              end;
            fModified := false;
          finally
            Segs.Free;
          end;
        except
          fModified := true;
        end;
      finally
        Unlock;
      end;
    end;

  procedure TMatrixCircuitMap.SetSize( aXsize, aYsize : integer );
    begin
      inherited;
      fMatrix := TIntegerLargeMatrix.Create( aYSize, aXsize, 10 );
    end;

  procedure TMatrixCircuitMap.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      fModified := true;
    end;

  procedure TMatrixCircuitMap.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
    end;

  function TMatrixCircuitMap.GetReachMatrix(x1, y1, x2, y2, tolerance : integer) : TReachMatrix;

    const
      minSegs = 10;
      minLen  = 20;

    function ValidCircuit(Circuit : TCircuit) : boolean;
      var
        i   : integer;
        len : integer;
      begin
        if Circuit <> nil
          then
            if Circuit.Segments.Count > minSegs
              then result := true
              else
                begin
                  len := 0;
                  for i := 0 to pred(Circuit.Segments.Count) do
                    inc(len, TSegment(Circuit.Segments[i]).Length);
                  result := len > minLen;
                end
          else result := false;
      end;

    function ReachPoint(Matrix : TReachMatrix; x, y : integer) : boolean;
      var
        i, j : integer;
      begin
        result := false;
        i := x - tolerance;
        while (i <= x + tolerance) and not result do
          begin
            j := y - tolerance;
            while (j <= y + tolerance) and not result do
              begin
                result := Matrix[i, j] <> rchNone;
                inc(j);
              end;
            inc(i);
          end;
      end;

    var
      Roads : TReachMatrix;
      x, y  : integer;
      i, j  : integer;

    begin
      Lock;
      try
        result := TReachMatrix.Create(x2 - x1 + 1, y2 - y1 + 1);
        Roads := TReachMatrix.Create(2*tolerance + x2 - x1, 2*tolerance + y2 - y1);
        try
          // browse the road matrix
          i := 0;
          for x := x1 - tolerance to x2 + tolerance do
            begin
              j := 0;
              for y := y1 - tolerance to y2 + tolerance do
                begin
                  if ValidCircuit(TCircuit(fMatrix[y, x]))
                    then Roads[i, j] := rchRoad;
                  inc(j);
                end;
              inc(i);
            end;
          // fill the resulting matrix
          for i := 0 to x2 - x1 do
            for j := 0 to y2 - y1 do
              if ReachPoint(Roads, i + tolerance, j + tolerance)
                then result[i, j] := rchRoad;
        finally
          Roads.Free;
        end;
      finally
        Unlock;
      end;
    end;

  // RegisterBackup

  procedure RegisterBackup;
    begin
      RegisterClass( TMatrixCircuitMap );
    end;

end.



