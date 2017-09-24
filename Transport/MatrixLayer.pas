unit MatrixLayer;

interface

  uses
    Collection, TransportInterfaces, Transport, SyncObjs;

  const
    MaxSize = 10000;

  type
    PCargoMap = ^TCargoMap;
    TCargoMap = array[0..MaxSize*MaxSize] of TCargoValue;

  // Rendering maps

  type
    TRenderMapCell =
      record
        value   : TCargoValue;
        defined : boolean;
      end;

    PRenderMap = ^TRenderMap;
    TRenderMap = array[0..MaxSize*MaxSize] of TRenderMapCell;
    
  type
    TMatrixLayer =
      class( TCargoLayer )
        protected
          constructor Create( aCargoType : TCargoType; aCargoSystem : TCargoSystem ); override;
        public
          destructor Destroy; override;
        private
          fMap  : PCargoMap;
          fLock : TCriticalSection;
        protected
          procedure Render( Quality : TRenderQuality );               override;
          function  GetCargoValue( x, y : integer ) : TCargoValue;    override;
          function  GetCargoSlope( x, y, dx, dy : integer ) : single; override;
        private
          srcMapN : PRenderMap;
          srcMapT : PRenderMap;
      end;

implementation

  uses
    MathUtils, Logs, SysUtils;

  const
    tidLog_Survival = 'Survival';


  // Map utils

  type
    TIterationOrder = (itNormal, itTransverse);
    PCargoValue     = ^TCargoValue;
    PRenderMapCell  = ^TRenderMapCell;

  function GetItem      ( i, j, xSize, ySize : integer; Matrix : PCargoMap;  Order : TIterationOrder ) : PCargoValue;    forward;
  function GetRenderItem( i, j, xSize, ySize : integer; Matrix : PRenderMap; Order : TIterationOrder ) : PRenderMapCell; forward;


  // TMatrixLayer

  constructor TMatrixLayer.Create( aCargoType : TCargoType; aCargoSystem : TCargoSystem );
    begin
      inherited;
      getmem( fMap, CargoSystem.xSize*CargoSystem.ySize*sizeof(fMap[0]) );
      getmem( srcMapN, CargoSystem.xSize*CargoSystem.ySize*sizeof(srcMapN[0]) );
      getmem( srcMapT, CargoSystem.xSize*CargoSystem.ySize*sizeof(srcMapT[0]) );
      fLock := TCriticalSection.Create;
    end;

  destructor TMatrixLayer.Destroy;
    begin
      try
        Logs.Log( 'Demolition', TimeToStr(Now) + ' - ' + ClassName );
      except
      end;
      fLock.Free;
      freemem( fMap, CargoSystem.xSize*CargoSystem.ySize*sizeof(fMap[0]) );
      freemem( srcMapN, CargoSystem.xSize*CargoSystem.ySize*sizeof(srcMapN[0]) );
      freemem( srcMapT, CargoSystem.xSize*CargoSystem.ySize*sizeof(srcMapT[0]) );
      inherited;
    end;

  procedure TMatrixLayer.Render( Quality : TRenderQuality );

    procedure InitSourceMap( Map : PRenderMap );
      var
        i, j : integer;
        cell : PRenderMapCell;
        P    : TCargoPoint;
      begin
        for i := 0 to pred(CargoSystem.ySize) do
          for j := 0 to pred(CargoSystem.xSize) do
            begin
              cell := GetRenderItem( i, j, CargoSystem.xSize, CargoSystem.ySize, Map, itNormal );
              cell.value   := 0;
              cell.defined := (i = 0) or (j = 0) or (i = pred(CargoSystem.ySize)) or (j = pred(CargoSystem.xSize));
            end;
        Points.Lock;
        try
          for i := 0 to pred(Points.Count) do
            begin
              P := TCargoPoint(Points[i]);
              cell := GetRenderItem( P.yPos, P.xPos, CargoSystem.xSize, CargoSystem.ySize, Map, itNormal );
              cell.value   := P.Value;
              cell.defined := true;
            end;
        finally
          Points.Unlock;
        end;
      end;                         

    procedure SmoothSourceMap( Map : PRenderMap );
      const
        SuperCellSize = 5;
      var
        si, sj : integer;
        i, j   : integer;
        cell   : PRenderMapCell;
        sum    : integer;
        count  : integer;
        avg    : TCargoValue;
      begin
        for si := 0 to pred(CargoSystem.ySize div SuperCellSize) do
          for sj := 0 to pred(CargoSystem.xSize div SuperCellSize) do
            begin
              sum := 0;
              count := 0;
              for i := SuperCellSize*si to SuperCellSize*si + SuperCellSize do
                for j := SuperCellSize*sj to SuperCellSize*sj + SuperCellSize do
                  begin
                    cell := GetRenderItem( i, j, CargoSystem.xSize, CargoSystem.ySize, Map, itNormal );
                    if cell.defined and not ((i = 0) or (j = 0) or (i = pred(CargoSystem.ySize)) or (j = pred(CargoSystem.xSize)))
                      then
                        begin
                          sum := sum + cell.value;
                          inc( count ); 
                        end;
                  end;
              if count > 1
                then
                  begin
                    avg := sum div count; //min( high(avg), sum );
                    for i := SuperCellSize*si to SuperCellSize*si + SuperCellSize do
                      for j := SuperCellSize*sj to SuperCellSize*sj + SuperCellSize do
                        begin
                          cell := GetRenderItem( i, j, CargoSystem.xSize, CargoSystem.ySize, Map, itNormal );
                          if cell.defined and not ((i = 0) or (j = 0) or (i = pred(CargoSystem.ySize)) or (j = pred(CargoSystem.xSize)))
                            then cell.value := avg;
                        end;
                  end;
            end;
      end;

    procedure SmoothStrings( Map : PRenderMap; Order : TIterationOrder );
      var
        lastVal : TCargoValue;
        lastPos : integer;
        xSize   : integer;
        ySize   : integer;
        i, j, k : integer;
        cell    : PRenderMapCell;
        icell   : PRenderMapCell;
        slope   : single;
        shift   : single;
      begin
        if Order = itNormal
          then
            begin
              xSize := CargoSystem.xSize;
              ySize := CargoSystem.ySize;
            end
          else
            begin
              xSize := CargoSystem.ySize;
              ySize := CargoSystem.xSize;
            end;
        for j := 1 to xSize - 2 do
          begin
            lastVal := 0;
            lastPos := 0;
            for i := 1 to pred(ySize) do
              begin
                cell := GetRenderItem( i, j, CargoSystem.xSize, CargoSystem.ySize, Map, Order );
                if cell.defined and ((lastPos <> 0) or (i <> pred(ySize)))
                  then
                    begin
                      slope := (cell.value - lastVal)/(i - lastPos);
                      shift := lastVal - slope*lastPos;
                      for k := lastPos + 1 to i - 1 do
                        begin
                          icell := GetRenderItem( k, j, CargoSystem.xSize, CargoSystem.ySize, Map, Order );
                          icell.value   := round( slope*k + shift );
                          icell.defined := true;
                        end;
                      lastVal := cell.value;
                      lastPos := i;
                    end;
              end
          end;
      end;

    procedure AverageMaps( srcMapN, srcMapT : PRenderMap; fMap : PCargoMap );
      var
        i, j               : integer;
        srcCellN, srcCellT : PRenderMapCell;
        cell               : PCargoValue;
      begin
        for i := 0 to pred(CargoSystem.ySize) do
          for j := 0 to pred(CargoSystem.xSize) do
            begin
              srcCellN := GetRenderItem( i, j, CargoSystem.xSize, CargoSystem.ySize, srcMapN, itNormal );
              srcCellT := GetRenderItem( i, j, CargoSystem.xSize, CargoSystem.ySize, srcMapT, itNormal );
              cell     := GetItem( i, j, CargoSystem.xSize, CargoSystem.ySize, fMap, itNormal );
              cell^    := (integer(srcCellN.value) + integer(srcCellT.value)) div 2;
            end;
      end;

    procedure CopyMap( srcMap : PRenderMap; fMap : PCargoMap );
      var
        i, j    : integer;
        srcCell : PRenderMapCell;
        cell    : PCargoValue;
      begin
        for i := 0 to pred(CargoSystem.ySize) do
          for j := 0 to pred(CargoSystem.xSize) do
            begin
              srcCell := GetRenderItem( i, j, CargoSystem.xSize, CargoSystem.ySize, srcMap, itNormal );
              cell    := GetItem( i, j, CargoSystem.xSize, CargoSystem.ySize, fMap, itNormal );
              cell^   := srcCell.value;
            end;
      end;

    begin
      try
        try
          InitSourceMap( srcMapN );
          // SmoothSourceMap( srcMapN );
          SmoothStrings( srcMapN, itNormal );
          SmoothStrings( srcMapN, itTransverse );
          if Quality = rqHigh
            then
              begin
                try
                  InitSourceMap( srcMapT );
                  // SmoothSourceMap( srcMapT );
                  SmoothStrings( srcMapT, itTransverse );
                  SmoothStrings( srcMapT, itNormal );
                  fLock.Enter;
                  try
                    AverageMaps( srcMapN, srcMapT, fMap );
                  finally
                    fLock.Leave;
                  end;
                finally
                end
              end
            else
              begin
                fLock.Enter;
                try
                  CopyMap( srcMapN, fMap );
                finally
                  fLock.Leave;
                end;
              end
        finally
        end;
      except
        on E : Exception do
          Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' ERROR in Cargo Surface: ' + E.Message );
      end
    end;

  function TMatrixLayer.GetCargoValue( x, y : integer ) : TCargoValue;
    begin
      fLock.Enter;
      try
        result := GetItem( y, x, CargoSystem.xSize, CargoSystem.ySize, fMap, itNormal )^;
      finally
        fLock.Leave;
      end;
    end;

  function TMatrixLayer.GetCargoSlope( x, y, dx, dy : integer ) : single;
    begin
      result := 0;
    end;


  // Util functs

  function GetItem( i, j, xSize, ySize : integer; Matrix : PCargoMap; Order : TIterationOrder ) : PCargoValue;
    begin
      if Order = itNormal
        then result := @Matrix[xSize*i + j]
        else result := @Matrix[xSize*j + i]
    end;

  function GetRenderItem( i, j, xSize, ySize : integer; Matrix : PRenderMap; Order : TIterationOrder ) : PRenderMapCell;
    begin
      if Order = itNormal
        then result := @Matrix[xSize*i + j]
        else result := @Matrix[xSize*j + i]
    end;


end.




