unit LocalCacheTypes;

interface

  type
    ILocalCacheAdviseSink =
      interface
        procedure Loaded(const url : string);
        procedure Released;
      end;

implementation

end.

