program TestProxy;

uses
  Forms,
  TestProxyFrm in 'TestProxyFrm.pas' {TestCacheProxy},
  CacheProxy in '..\CacheProxy.pas',
  FiveCachedObjects in '..\FiveCachedObjects.pas',
  ObjectIndex in '..\ObjectIndex.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TTestCacheProxy, TestCacheProxy);
  Application.Run;
end.
