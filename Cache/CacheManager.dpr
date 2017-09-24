library CacheManager;

uses
  ShareMem,
  ComServ,
  CacheManager_TLB in 'CacheManager_TLB.pas',
  CachedObjectAuto in 'CachedObjectAuto.pas' {CachedObject: CoClass},
  Collection in '..\Kernel\Collection.pas',
  RDOProtocol in '..\RDO\Common\RDOProtocol.pas',
  ErrorCodes in '..\RDO\Common\ErrorCodes.pas',
  LogFile in '..\RDO\Common\LogFile.pas',
  CacheRegistryData in 'CacheRegistryData.pas',
  SpecialChars in 'SpecialChars.pas',
  CacheObjects in 'CacheObjects.pas',
  CacheObjectSpool in 'CacheObjectSpool.pas',
  CacheManagerRDO in '..\Cache Server\CacheManagerRDO.pas',
  CacheHistory in '..\Cache Server\CacheHistory.pas',
  Logs in '..\Logs\Logs.pas';

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
