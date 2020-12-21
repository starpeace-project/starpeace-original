program FIVEMailServer;

uses
  ShareMem,
  Forms,
  SysUtils,
  MailServerReportForm in 'MailServerReportForm.pas' {MailServerReport},
  MailServer in 'MailServer.pas',
  RDOObjectProxy in '..\RDO\Client\RDOObjectProxy.pas',
  MailData in '..\Mail\MailData.pas',
  Protocol in '..\Protocol\Protocol.pas',
  MailUtils in '..\Mail\MailUtils.pas',
  Attachments in '..\Mail\Attachments.pas',
  HostNames in '..\Utils\Network\HostNames.pas',
  Logs in '..\Logs\Logs.pas';

{$R *.RES}

begin
  MailServerReportForm.AutoRun := (ParamCount > 0) and (lowercase(ParamStr(1)) = 'autorun');
  Application.Initialize;
  Application.Title := 'FIVE Mail Server';
  Application.CreateForm(TMailServerReport, MailServerReport);
  Application.Run;
end.
