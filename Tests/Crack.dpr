library Crack;

uses
  ComServ,
  Crack_TLB in 'Crack_TLB.pas',
  CrackAuto in 'CrackAuto.pas' {TestCrack: CoClass};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
