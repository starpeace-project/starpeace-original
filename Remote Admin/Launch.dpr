program Launch;

{$APPTYPE CONSOLE}

uses
  HTTPApp,
  CGIApp,
  LaunchWebModule in 'LaunchWebModule.pas' {WebModule1: TWebModule},
  Logs in '..\Logs\Logs.pas',
  RemoteAdm in 'RemoteAdm.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TWebModule1, WebModule1);
  Application.Run;
end.
