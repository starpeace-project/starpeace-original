program TestClient;

uses
  ShareMem,
  Forms,
  TestClientMain in 'TestClientMain.pas' {TestMyDispatch};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TTestMyDispatch, TestMyDispatch);
  Application.Run;
end.
