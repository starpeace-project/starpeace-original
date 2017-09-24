library xx1;

uses
  ComServ,
  xx1_TLB in 'xx1_TLB.pas',
  xx in 'xx.pas' {Pepito: CoClass};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
