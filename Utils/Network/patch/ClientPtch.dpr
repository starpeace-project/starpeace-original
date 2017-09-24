program ClientPtch;

uses
  Forms,
  ClientSocketFrm in 'ClientSocketFrm.pas' {Form1},
  SocketComp in '..\SocketComp.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
