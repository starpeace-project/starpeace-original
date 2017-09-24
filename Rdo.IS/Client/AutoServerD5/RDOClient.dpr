library RDOClient;

uses
  ComServ,
  RDOClient_TLB in 'RDOClient_TLB.pas',
  WinSockRDOConnection in '..\WinSockRDOConnection.pas',
  RDOObjectProxy in '..\RDOObjectProxy.pas';

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
