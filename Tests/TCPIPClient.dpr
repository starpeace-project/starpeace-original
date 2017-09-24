program TCPIPClient;

uses
  Forms,
  ClientSocket in 'ClientSocket.pas' {ClientForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TClientForm, ClientForm);
  Application.Run;
end.
