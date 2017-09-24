program FiveDaemonScheduler;

uses
  ShareMem,
  Forms,
  DaemonSchedulerReport in 'DaemonSchedulerReport.pas' {DaemonSchedulerWindow},
  RDOMarshalers in '..\Rdo\Client\RDOMarshalers.pas',
  WinsockRDOConnection in '..\Rdo\Client\WinsockRDOConnection.pas',
  RDOObjectProxy in '..\Rdo\Client\RDOObjectProxy.pas',
  RDOInterfaces in '..\Rdo\Common\RDOInterfaces.pas',
  RDOUtils in '..\Rdo\Common\RDOUtils.pas',
  RDOProtocol in '..\Rdo\Common\RDOProtocol.pas',
  ErrorCodes in '..\Rdo\Common\ErrorCodes.pas',
  SmartThreads in '..\Kernel\SmartThreads.pas',
  SocketComp in '..\Utils\Network\SocketComp.pas',
  Collection in '..\Kernel\Collection.pas',
  SpoolPackets in '..\Utils\Network\SpoolPackets.pas',
  MathUtils in '..\Utils\Misc\MathUtils.pas',
  Daemons in 'Daemons.pas',
  DirServerSession in 'DirServerSession.pas',
  DaemonScheduler in 'DaemonScheduler.pas',
  Logs in '..\Logs\Logs.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TDaemonSchedulerWindow, DaemonSchedulerWindow);
  Application.Run;
end.
