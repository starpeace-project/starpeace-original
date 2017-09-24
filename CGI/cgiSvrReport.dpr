program cgiSvrReport;

{$APPTYPE CONSOLE}

uses
  WebBroker,
  CGIApp,
  CGIReportModule in 'CGIReportModule.pas' {CGIWebModule: TWebModule},
  GIFImage in 'gifimage.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TCGIWebModule, CGIWebModule);
  Application.Run;
end.
