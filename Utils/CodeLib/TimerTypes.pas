unit TimerTypes;

interface

  type
    ITickeable =
      interface
        function Enabled : boolean;
        function Tick    : integer;
      end;

  type
    ITicker =
      interface
        // private
        function  GetEnabled : boolean;
        procedure SetEnabled(which : boolean);
        // public
        function  Count : integer;
        function  GetTickeable(idx : integer) : ITickeable;
        procedure Attach(const which : ITickeable);
        procedure Detach(const which : ITickeable);
        property Enabled : boolean read GetEnabled write SetEnabled;
      end;

implementation

end.
