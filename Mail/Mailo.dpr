library Mailo;

uses
  ComServ,
  Mailo_TLB in 'Mailo_TLB.pas',
  Unit1 in 'Unit1.pas' {MuyMilo: CoClass};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
