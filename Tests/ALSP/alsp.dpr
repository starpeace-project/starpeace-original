program alsp;

uses
  Forms,
  alspform in 'alspform.pas' {ALSPFrm},
  RemoteAdm in '..\..\Remote Admin\RemoteAdm.pas',
  psapi in '..\..\Remote Admin\psapi.pas',
  SmartThreads in '..\..\Kernel\SmartThreads.pas',
  SocketComp in '..\..\Utils\Network\SocketComp.pas',
  Collection in '..\..\Kernel\Collection.pas',
  MathUtils in '..\..\Utils\Misc\MathUtils.pas',
  Protocol in '..\..\Protocol\Protocol.pas',
  Logs in '..\..\Logs\Logs.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TALSPFrm, ALSPFrm);
  Application.Run;
end.
