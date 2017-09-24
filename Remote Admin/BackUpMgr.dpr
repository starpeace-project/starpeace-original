library BackUpMgr;

uses
  ComServ,
  BackUpMgr_TLB in 'BackUpMgr_TLB.pas',
  BackUpManager in 'BackUpManager.pas' {BackUpManager: CoClass};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
