library FiveWebUtils;

uses
  ComServ,
  FiveWebUtils_TLB in 'FiveWebUtils_TLB.pas',
  URLTrigger in 'URLTrigger.pas' {TURLTrigger: CoClass};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
