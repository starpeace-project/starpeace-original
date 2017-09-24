library Greed;

uses
  ComServ,
  Greed_TLB in 'Greed_TLB.pas',
  GreedAuto in 'GreedAuto.pas' {OTK: CoClass};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
