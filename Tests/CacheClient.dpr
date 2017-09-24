program CacheClient;

uses
  Forms,
  TestClientFrm in 'TestClientFrm.pas' {ClientForm};

{$R *.RES}

{$R *.TLB}

begin
  Application.Initialize;
  Application.CreateForm(TClientForm, ClientForm);
  Application.Run;
end.
