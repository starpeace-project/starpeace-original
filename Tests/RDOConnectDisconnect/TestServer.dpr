program TestServer;

uses
  ShareMem,
  Forms,
  TestServerMain in 'TestServerMain.pas' {TestServerForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TTestServerForm, TestServerForm);
  Application.Run;
end.
