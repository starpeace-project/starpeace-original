program RDOXClient;

uses
  ShareMem,
  Forms,
  ClientFrm in 'ClientFrm.pas' {MainClientForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainClientForm, MainClientForm);
  Application.Run;
end.
