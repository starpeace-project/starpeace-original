unit LandSurfaces;

interface

  uses
    Land, Surfaces, Variants;

  type
    TLandValueArray = array[TLandClass] of single;

  type
    TLandSurface =
      class( TSurface )
        public
          constructor Create( anId : TSurfaceId; aName : string; aValues : array of single );
        private
          fLand   : ILandInfo;
          fValues : TLandValueArray;
        public
          property Land : ILandInfo write fLand;
        protected
          function GetValue( x, y : integer ) : TSurfaceValue; override;
      end;

implementation

  constructor TLandSurface.Create( anId : TSurfaceId; aName : string; aValues : array of single );
    var
      i : TLandClass;
    begin
      inherited Create( anId, aName );
      for i := low(i) to high(i) do
        fValues[i] := aValues[ord(i)]; 
    end;

  function TLandSurface.GetValue( x, y : integer ) : TSurfaceValue;
    begin
      result := inherited GetValue( x, y );
      if fLand <> nil
        then result := result + fValues[fLand.LandClassAt( x, y )];
    end;

end.
