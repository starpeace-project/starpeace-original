program CacheObjs;

uses
  Forms,
  TestProxyFrm in 'TestProxyFrm.pas' {TestCacheProxy};

{$R *.RES}

{$R *.TLB}

begin
  Application.Initialize;
  Application.CreateForm(TTestCacheProxy, TestCacheProxy);
  Application.Run;
end.
