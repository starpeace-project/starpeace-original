unit ReachMatrix;

interface

  const
    rchNone  = 0;
    rchRoad  = 1;
    rchReach = 2;

  type
    TReachMapArray = array[0..99] of byte;
    PReachMapArray = ^TReachMapArray;

  type
    TReachMatrix =
      class
        public
          constructor Create(rCnt, cCnt : integer);
        private
          fRows   : integer;
          fCols   : integer;
          fMatrix : PReachMapArray;
        private
          function  GetElem(i, j : integer) : byte;
          procedure SetElem(i, j : integer; value : byte);
        public
          procedure Fill(value : byte);
        public
          property Elems[i, j : integer] : byte read GetElem write SetElem; default;
      end;

implementation

  // TReachMatrix

  constructor TReachMatrix.Create(rCnt, cCnt : integer);
    begin
      fRows := rCnt;
      fCols := cCnt;
      ReallocMem(fMatrix, rCnt*cCnt*sizeof(fMatrix[0]));
      Fill(rchNone);
    end;

  function TReachMatrix.GetElem(i, j : integer) : byte;
    begin
      if (i >= 0) and (j >= 0) and (i < fRows) and (j < fCols)
        then result := fMatrix[i*fCols + j]
        else result := 0;
    end;

  procedure TReachMatrix.SetElem(i, j : integer; value : byte);
    begin
      if (i >= 0) and (j >= 0) and (i < fRows) and (j < fCols)
        then fMatrix[i*fCols + j] := value;
    end;

  procedure TReachMatrix.Fill(value : byte);
    begin
      FillChar(fMatrix^, fRows*fCols*sizeof(fMatrix[0]), value);
    end;

end.
