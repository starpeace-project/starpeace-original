program ClientTest;

uses
  Forms,
  WinSockRDOConnection in '..\..\..\Rdo\Client\WinsockRDOConnection.pas',
  RDOMarshalers in '..\..\..\Rdo\Client\RDOMarshalers.pas',
  RDOObjectProxy in '..\..\..\Rdo\Client\RDOObjectProxy.pas',
  RDOAddress in '..\..\..\Rdo\Client\RDOAddress.pas',
  ErrorCodes in '..\..\..\Rdo\Common\ErrorCodes.pas',
  LogFile in '..\..\..\Rdo\Common\LogFile.pas',
  RDOInterfaces in '..\..\..\Rdo\Common\RDOInterfaces.pas',
  RDOProtocol in '..\..\..\Rdo\Common\RDOProtocol.pas',
  RDOUtils in '..\..\..\Rdo\Common\RDOUtils.pas',
  GMKernel in '..\..\GMKernel.pas',
  GMIntServer in '..\..\GMIntServer.pas',
  GMCostumer in '..\..\GMCostumer.pas',
  SmartThreads in '..\..\..\Kernel\SmartThreads.pas',
  Collection in '..\..\..\Kernel\Collection.pas',
  SpoolPackets in '..\..\..\Utils\Network\SpoolPackets.pas',
  SocketComp in '..\..\..\Utils\Network\SocketComp.pas',
  MathUtils in '..\..\..\Utils\Misc\MathUtils.pas',
  RDOServer in '..\..\..\Rdo\Server\RDOServer.pas',
  RDOObjectRegistry in '..\..\..\Rdo\Server\RDOObjectRegistry.pas',
  RDOObjectServer in '..\..\..\Rdo\Server\RDOObjectServer.pas',
  RDOTypInfo in '..\..\..\Utils\VCL\RDOTypInfo.pas',
  RDOQueryServer in '..\..\..\Rdo\Server\RDOQueryServer.pas',
  CompStringsParser in '..\..\..\Utils\Misc\CompStringsParser.pas',
  GMIServerRDOMger in 'GMIServerRDOMger.pas',
  AxlDebug in '..\..\..\Utils\CodeLib\AxlDebug.pas',
  Threads in '..\..\..\Utils\CodeLib\Threads.pas',
  ChatUtils in '..\ChatUtils.pas',
  CustomerChat in 'CustomerChat.pas' {Form3},
  ClientTestFrm in 'ClientTestFrm.pas' {GMClientMain},
  GMList in '..\GMList.pas',
  ConnectOptionsFrm in 'ConnectOptionsFrm.pas' {GMConnOptions};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TGMClientMain, GMClientMain);
  Application.CreateForm(TGMConnOptions, GMConnOptions);
  Application.Run;
end.
