program TestServer;

uses
  ShareMem,
  Forms,
  TestServerMain in 'TestServerMain.pas' {TestMyDispatch};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TTestMyDispatch, TestMyDispatch);
  Application.Run;
end.
