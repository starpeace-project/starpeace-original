program RDOConnectDisconnectTest;

uses
  Forms,
  ConnectDisconnectTestMain in 'ConnectDisconnectTestMain.pas' {Form1},
  RDOMarshalers in '..\..\Rdo\Client\RDOMarshalers.pas',
  RDOObjectProxy in '..\..\Rdo\Client\RDOObjectProxy.pas',
  WinSockRDOConnection in '..\..\Rdo\Client\WinsockRDOConnection.pas',
  RDOInterfaces in '..\..\Rdo\Common\RDOInterfaces.pas',
  RDOProtocol in '..\..\Rdo\Common\RDOProtocol.pas',
  SmartThreads in '..\..\Kernel\SmartThreads.pas',
  SocketComp in '..\..\Utils\Network\SocketComp.pas',
  Collection in '..\..\Kernel\Collection.pas',
  SpoolPackets in '..\..\Utils\Network\SpoolPackets.pas',
  MathUtils in '..\..\Utils\Misc\MathUtils.pas',
  RDOUtils in '..\..\Rdo\Common\RDOUtils.pas',
  ErrorCodes in '..\..\Rdo\Common\ErrorCodes.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

