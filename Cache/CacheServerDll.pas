unit CacheServerDll;

interface

  uses
    RDOServerInterfaces;
    
  procedure InitWebServerCache( ServerConn : IRDOConnectionServer );
  procedure DoneWebServerCache;
  procedure SetCacheBasePath(const aPath : string);

implementation

  procedure InitWebServerCache( ServerConn : IRDOConnectionServer ); external 'CacheServer.dll';
  procedure DoneWebServerCache;                                      external 'CacheServer.dll';
  procedure SetCacheBasePath(const aPath : string);                  external 'CacheServer.dll';

end.
 