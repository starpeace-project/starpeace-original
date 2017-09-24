unit MatrixCircuits;

interface

  uses
    Collection,Circuits, LargeMatrix, Windows, BackupInterfaces;

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
          procedure Render; override;
          procedure SetSize( aXsize, aYsize : integer ); override;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        public // for debugging only
          property Matrix : TIntegerLargeMatrix read fMatrix;
      end;

  procedure RegisterBackup;

implementation

  uses
    SysUtils, Logs;

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
        if fModified
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
        if fModified
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

  procedure TMatrixCircuitMap.Render;
    var
      Segs    : TCollection;
      Segment : TSegment;
      i, x, y : integer;
      Circuit : TCircuit;
    begin
      Lock;
      try
        inherited;
        try
          Segs := TCollection.Create( 0, rkUse );
          try
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
                        then Logs.Log('Circuits', DateTimeToStr(Now) + Format(' Possible Road Bug at: %d, %d Circuits: %d(%d) and %d(%d)', [x, y, integer(Circuit), Circuit.Segments.Count, integer(Segment.Circuit), Segment.Circuit.Segments.Count]));
                      fMatrix[y,x] := integer(Segment.Circuit);
                    end;
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


  // RegisterBackup

  procedure RegisterBackup;
    begin
      RegisterClass( TMatrixCircuitMap );
    end;

end.



