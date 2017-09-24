program MSWatcher;

uses
  SvcMgr,
  SPService in 'SPService.pas' {SMSWatcher: TService},
  DoTask in 'DoTask.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TSMSWatcher, SMSWatcher);
  Application.Run;
end.
