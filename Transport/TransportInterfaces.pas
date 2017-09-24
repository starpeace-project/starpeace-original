unit TransportInterfaces;

interface

  type
    TCargoType  = byte;                        
    TCargoValue = shortint;

  type
    ICargoPoint =
      interface
        function  getX : integer;
        function  getY : integer;
        function  getValue : TCargoValue;
        procedure setValue( value : TCargoValue );
      end;

    ICargoLayer =
      interface
        function  CreatePoint( x, y : integer ) : ICargoPoint;
        procedure DelPoint( x, y : integer );
        procedure SetPoint( x, y : integer; Value : TCargoValue );
        function  GetCargoValue( x, y : integer ) : TCargoValue;
        function  GetCargoSlope( x, y, dx, dy : integer ) : single;  
      end;

    ICargoSystem =
      interface
        function GetLayer( CargoType : TCargoType ) : ICargoLayer;
      end;

implementation

end.
