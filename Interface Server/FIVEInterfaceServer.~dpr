program FIVEInterfaceServer;

uses
  ShareMem,
  Forms,
  InterfaceServerReportForm in 'InterfaceServerReportForm.pas' {InterfaceServerReport},
  InterfaceServer in 'InterfaceServer.pas',
  Collection in '..\Kernel\Collection.pas',
  Protocol in '..\Protocol\Protocol.pas',
  FIVEInterfaceServer_TLB in 'FIVEInterfaceServer_TLB.pas',
  AsxCriticalSections in '..\Kernel\AsxCriticalSections.pas',
  Sessions in 'Sessions.pas',
  SessionInterfaces in 'SessionInterfaces.pas',
  Logs in '..\Logs\Logs.pas',
  GMKernel in '..\GM\GMKernel.pas',
  Languages in '..\Kernel\Languages.pas';

{$R *.RES}

{$R *.TLB}

begin
  Application.Initialize;
  Application.Title := 'FIVE Interface Server';
  Application.CreateForm(TInterfaceServerReport, InterfaceServerReport);
  Application.Run;
end.
