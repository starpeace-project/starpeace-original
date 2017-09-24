library RDOClient;

uses
  ComServ,
  RDOClient_TLB in 'RDOClient_TLB.pas',
  WinSockRDOConnection in '..\WinSockRDOConnection.pas' {CoClass: WinSockRDOConnection},
  RDOObjectProxy in '..\RDOObjectProxy.pas',
  RDOMarshalers in '..\RDOMarshalers.pas';

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
