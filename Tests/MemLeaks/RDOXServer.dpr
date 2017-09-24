program RDOXServer;

uses
  ShareMem,
  Forms,
  ServerFrm in 'ServerFrm.pas' {MainServerForm},
  SrvObject in 'SrvObject.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainServerForm, MainServerForm);
  Application.Run;
end.
