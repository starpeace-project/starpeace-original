unit CacheInterfaces;

interface

  type
    ICacher =
      interface
        function GetCache(Id, kind, info : integer) : string;
        function RenewCache(Agent, ObjId : string) : string;
      end;

implementation

end.
 