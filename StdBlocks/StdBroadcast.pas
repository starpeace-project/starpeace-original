unit StdBroadcast;

interface

  const
    tidBroadcast_TV      = 'TV';
    tidBroadcast_Radio   = 'Radio';
    tidEnvironment_TV    = tidBroadcast_TV;
    tidEnvironment_Radio = tidBroadcast_Radio;

  procedure RegisterSurfaces;

implementation

  uses
    Surfaces;

  procedure RegisterSurfaces;
    begin
      TSurface.Create( tidBroadcast_TV,    'TV' );
      TSurface.Create( tidBroadcast_Radio, 'Radio' );
    end;

end.
