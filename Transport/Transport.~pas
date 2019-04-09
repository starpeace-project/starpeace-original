unit Transport;

interface

  uses
    Classes, Collection, TransportInterfaces, Matrix, Windows;

  type
    TRenderQuality = (rqLow, rqHigh);

  type
    TCargoPoint  = class;
    TCargoLayer  = class;
    TCargoSystem = class;

    CCargoPoint  = class of TCargoPoint;
    CCargoLayer  = class of TCargoLayer;
    CCargoSystem = class of TCargoSystem;

    TCargoPoint =
      class( TObject, ICargoPoint )
        public
          constructor Create( aXpos, aYpos : integer; aValue : TCargoValue );
        private
          fXpos  : integer;
          fYpos  : integer;
          fValue : TCargoValue;
        public
          property xPos  : integer     read fXpos;
          property yPos  : integer     read fYpos;
          property Value : TCargoValue read fValue write fValue;
        // ICargoPoint
        private
          function  getX : integer;
          function  getY : integer;
          function  getValue : TCargoValue;
          procedure setValue( value : TCargoValue );
        // IUnknown
        private
          function QueryInterface(const IID: TGUID; out Obj): hresult; stdcall;
          function _AddRef  : integer; stdcall;
          function _Release : integer; stdcall;
      end;

    TCargoLayer =
      class( TObject, ICargoLayer, IMatrix )
        protected
          constructor Create( aCargoType : TCargoType; aCargoSystem : TCargoSystem ); virtual;
        public
          destructor  Destroy; override;
        private
          fCargoType   : TCargoType;
          fCargoSystem : TCargoSystem;
          fPoints      : TLockableCollection;
        public
          property CargoType   : TCargoType          read fCargoType;
          property CargoSystem : TCargoSystem        read fCargoSystem;
          property Points      : TLockableCollection read fPoints;
        protected
          procedure Render( Quality : TRenderQuality );                virtual; abstract;
          function  GetCargoValue( x, y : integer ) : TCargoValue;     virtual; abstract;
          function  GetCargoSlope( x, y, dx, dy : integer ) : single;  virtual; abstract;
        public
          property Value[x, y : integer]         : TCargoValue read GetCargoValue;
          property Slope[x, y, dx, dy : integer] : single      read GetCargoSlope;
        public
          function  CreatePoint( x, y : integer ) : ICargoPoint;
          procedure SetPoint( x, y : integer; Value : TCargoValue );
          procedure DelPoint( x, y : integer );
        private
          function GetPoint( x, y : integer ) : TCargoPoint;
        // IMaxtrix
        private
          function  getCols : integer;
          function  getRows : integer;
          procedure setDimensions( n, m : integer );
          function  getElement   ( i, j : integer ) : single;
          procedure setElement   ( i, j : integer; value : single );
        // IUnknown
        private
          function QueryInterface(const IID: TGUID; out Obj): hresult; stdcall;
          function _AddRef  : integer; stdcall;
          function _Release : integer; stdcall;
      end;

    TCargoSystem =
      class( TObject, ICargoSystem )
        public
          constructor Create( aXSize, aYSize : integer );
          destructor  Destroy; override;
        private
          fXSize     : integer;
          fYSize     : integer;                          
          fLayers    : TCollection;
        public
          property xSize  : integer     read fXSize;
          property ySize  : integer     read fYSize;
          property Layers : TCollection read fLayers;
        public
          procedure Render( Quality : TRenderQuality );
          procedure AddLayer( CargoType : TCargoType; LayerType : CCargoLayer );
        private
          function FindLayer( CargoType : TCargoType ) : TCargoLayer;
        public
          property Layer[CargoType : TCargoType] : TCargoLayer read FindLayer; default;
        // ICargoSystem
        private
          function GetLayer( CargoType : TCargoType ) : ICargoLayer;
        // IUnknown
        private
          function QueryInterface(const IID: TGUID; out Obj): hresult; stdcall;
          function _AddRef  : integer; stdcall;
          function _Release : integer; stdcall;
      end;

implementation

  uses
    Logs;

  const
    tidLog_Survival = 'Survival';


  // TCargoPoint

  constructor TCargoPoint.Create( aXpos, aYpos : integer; aValue : TCargoValue );
    begin
      inherited Create;
      fXpos  := aXpos;
      fYpos  := aYpos;
      fValue := aValue;
    end;

  function TCargoPoint.getX : integer;
    begin
      result := fXpos;
    end;

  function TCargoPoint.getY : integer;
    begin
      result := fYpos;
    end;

  function TCargoPoint.getValue : TCargoValue;
    begin
      result := fValue;
    end;

  procedure TCargoPoint.setValue( value : TCargoValue );
    begin
      fValue := value;
    end;

  function TCargoPoint.QueryInterface(const IID: TGUID; out Obj): hresult; stdcall;
    begin
      pointer(Obj) := nil;
      result := E_FAIL;
    end;

  function TCargoPoint._AddRef  : integer; stdcall;
    begin
      result := 1;
    end;

  function TCargoPoint._Release : integer; stdcall;
    begin
      result := 1;
    end;


  // TCargoLayer

  constructor TCargoLayer.Create( aCargoType : TCargoType; aCargoSystem : TCargoSystem );
    begin
      inherited Create;
      fCargoType   := aCargoType;
      fCargoSystem := aCargoSystem;
      fPoints      := TLockableCollection.Create( 0, rkBelonguer );
    end;

  destructor TCargoLayer.Destroy;                 
    begin
      fPoints.Free;
      inherited;
    end;

  function TCargoLayer.CreatePoint( x, y : integer ) : ICargoPoint;
    var
      P : TCargoPoint;
    begin
      {
      P := GetPoint( x, y );
      if P = nil
        then
          begin
            P := TCargoPoint.Create( x, y, 0 );
            fPoints.Insert( P )
          end;
       }
      P := TCargoPoint.Create(x, y, 0);
      fPoints.Insert(P);
      result := P;
    end;

  procedure TCargoLayer.SetPoint( x, y : integer; Value : TCargoValue );
    var
      P : TCargoPoint;
    begin
      P := GetPoint( x, y );
      if P <> nil
        then P.Value := Value
        else fPoints.Insert( TCargoPoint.Create( x, y, Value ) );
    end;

  procedure TCargoLayer.DelPoint( x, y : integer );
    var
      P : TCargoPoint;
    begin
      P := GetPoint( x, y );
      if P <> nil
        then fPoints.Extract( P );
    end;

  function TCargoLayer.GetPoint( x, y : integer ) : TCargoPoint;
    var
      i : integer;
    begin
      fPoints.Lock;
      try
        i := 0;
        while (i < fPoints.Count) and ((TCargoPoint(fPoints[i]).xPos <> x) or (TCargoPoint(fPoints[i]).yPos <> y)) do
          inc( i );
        if i < fPoints.Count
          then result := TCargoPoint(fPoints[i])
          else result := nil;
      finally
        fPoints.Unlock;
      end;
    end;

  function TCargoLayer.getCols : integer;
    begin
      result := fCargoSystem.xSize;
    end;

  function TCargoLayer.getRows : integer;
    begin
      result := fCargoSystem.ySize;
    end;

  procedure TCargoLayer.setDimensions( n, m : integer );
    begin
    end;

  function TCargoLayer.getElement( i, j : integer ) : single;
    begin
      result := Value[j, i];
    end;

  procedure TCargoLayer.setElement( i, j : integer; value : single );
    begin
    end;

  function TCargoLayer.QueryInterface(const IID: TGUID; out Obj): hresult; stdcall;
    begin
      pointer(Obj) := nil;
      result := E_FAIL;
    end;

  function TCargoLayer._AddRef  : integer; stdcall;
    begin
      result := 1;
    end;

  function TCargoLayer._Release : integer; stdcall;
    begin
      result := 1;
    end;


  // TCargoSystem

  constructor TCargoSystem.Create( aXSize, aYSize : integer );
    begin
      inherited Create;
      fXSize  := aXSize;
      fYSize  := aYSize;
      fLayers := TCollection.Create( 0, rkBelonguer );
    end;
    
  destructor TCargoSystem.Destroy;
    begin
      fLayers.Free;
      inherited;
    end;

  procedure TCargoSystem.Render( Quality : TRenderQuality );
    var
      i : integer;
    begin
      for i := 0 to pred(fLayers.Count) do
        TCargoLayer(fLayers[i]).Render( Quality );
    end;

  procedure TCargoSystem.AddLayer( CargoType : TCargoType; LayerType : CCargoLayer );
    begin
      fLayers.Insert( LayerType.Create( CargoType, self ) );
    end;

  function TCargoSystem.FindLayer( CargoType : TCargoType ) : TCargoLayer;
    var
      i : integer;
    begin
      i := 0;
      while (i < fLayers.Count) and (TCargoLayer(fLayers[i]).CargoType <> CargoType) do
        inc( i );
      if i < fLayers.Count
        then result := TCargoLayer(fLayers[i])
        else result := nil;
    end;

  function TCargoSystem.GetLayer( CargoType : TCargoType ) : ICargoLayer;
    begin
      result := FindLayer( CargoType );
    end;

  function TCargoSystem.QueryInterface(const IID: TGUID; out Obj): hresult; stdcall;
    begin
      pointer(Obj) := nil;
      result := E_FAIL;
    end;

  function TCargoSystem._AddRef  : integer; stdcall;
    begin
      result := 1;
    end;

  function TCargoSystem._Release : integer; stdcall;
    begin
      result := 1;
    end;


end.




