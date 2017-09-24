library GreedApi;

uses
  ComServ,
  GreedApi_TLB in 'GreedApi_TLB.pas',
  OTKAuto in 'OTKAuto.pas' {OTK: CoClass},
  rc4 in '..\Kernel\rc4.pas';

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
