program TestClient1;

uses
  ShareMem,
  Forms,
  TestClientMain1 in 'TestClientMain1.pas' {TestMyDispatch};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TTestMyDispatch, TestMyDispatch);
  Application.Run;
end.
