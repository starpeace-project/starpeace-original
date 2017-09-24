library ServerReport;

uses
  WebBroker,
  ISAPIApp,
  ServerReportModule in 'ServerReportModule.pas' {ServerReportWebModule: TWebModule},
  GIFImage in 'gifimage.pas';

{$R *.RES}

exports
  GetExtensionVersion,
  HttpExtensionProc,
  TerminateExtension;

begin
  Application.Initialize;
  Application.CreateForm(TServerReportWebModule, ServerReportWebModule);
  Application.Run;
end.
