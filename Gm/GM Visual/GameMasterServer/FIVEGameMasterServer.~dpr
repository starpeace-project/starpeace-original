program FIVEGameMasterServer;

{$DEFINE USELogs}
uses
  ShareMem,
  Forms,
  GMKernel in '..\..\GMKernel.pas',
  GMServer in '..\..\GMServer.pas',
  Collection in '..\..\..\Kernel\Collection.pas',
  WinSockRDOServerClientConnection in '..\..\..\Rdo\Server\WinSockRDOServerClientConnection.pas',
  RDOObjectServer in '..\..\..\Rdo\Server\RDOObjectServer.pas',
  RDOQueryServer in '..\..\..\Rdo\Server\RDOQueryServer.pas',
  RDORootServer in '..\..\..\Rdo\Server\RDORootServer.pas',
  RDOServer in '..\..\..\Rdo\Server\RDOServer.pas',
  WinSockRDOConnectionsServer in '..\..\..\Rdo\Server\WinSockRDOConnectionsServer.pas',
  RDOObjectRegistry in '..\..\..\Rdo\Server\RDOObjectRegistry.pas',
  SmartThreads in '..\..\..\Kernel\SmartThreads.pas',
  RDOUtils in '..\..\..\Rdo\Common\RDOUtils.pas',
  LogFile in '..\..\..\Rdo\Common\LogFile.pas',
  RDOInterfaces in '..\..\..\Rdo\Common\RDOInterfaces.pas',
  RDOProtocol in '..\..\..\Rdo\Common\RDOProtocol.pas',
  ErrorCodes in '..\..\..\Rdo\Common\ErrorCodes.pas',
  SocketComp in '..\..\..\Utils\Network\SocketComp.pas',
  SpoolPackets in '..\..\..\Utils\Network\SpoolPackets.pas',
  MathUtils in '..\..\..\Utils\Misc\MathUtils.pas',
  CompStringsParser in '..\..\..\Utils\Misc\CompStringsParser.pas',
  GMServerRDOMger in 'GMServerRDOMger.pas',
  RDOObjectProxy in '..\..\..\Rdo\Client\RDOObjectProxy.pas',
  RDOMarshalers in '..\..\..\Rdo\Client\RDOMarshalers.pas',
  GameMasterServerFrm in 'GameMasterServerFrm.pas' {MainForm},
  GMList in '..\GMList.pas',
  WinSockRDOConnection in '..\..\..\Rdo\Client\WinSockRDOConnection.pas',
  HostNames in '..\..\..\Utils\Network\HostNames.pas',
  TypInfo in '..\..\..\Utils\VCL\TypInfo.pas',
  Logs in '..\..\..\Logs\Logs.pas',
  Threads in '..\..\..\Utils\CodeLib\Threads.pas',
  AxlDebug in '..\..\..\Utils\CodeLib\AxlDebug.pas',
  DirectoryServerProtocol in '..\..\..\Directory Server\DirectoryServerProtocol.pas',
  GenIdd in '..\..\..\Utils\Serial\GenIdd.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
