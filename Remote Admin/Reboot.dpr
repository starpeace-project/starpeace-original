program Reboot;

{$APPTYPE CONSOLE}

uses
  HTTPApp,
  CGIApp,
  RebootWebModule in 'RebootWebModule.pas' {RebWebModule: TWebModule};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TRebWebModule, RebWebModule);
  Application.Run;
end.
