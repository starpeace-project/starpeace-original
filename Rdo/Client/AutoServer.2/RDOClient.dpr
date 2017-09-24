library RDOClient;

uses
  ShareMem,
  ComServ,
  RDOClient_TLB in 'RDOClient_TLB.pas',
  WinSockRDOConnection in '..\WinSockRDOConnection.pas' {WinSockRDOConnection: CoClass},
  RDOObjectProxy in '..\RDOObjectProxy.pas',
  RDOMarshalers in '..\RDOMarshalers.pas',
  Logs in '..\..\..\Logs\Logs.pas';

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
