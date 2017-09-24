program Server;

uses
  Forms,
  ServerSocket in 'ServerSocket.pas' {ServerForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TServerForm, ServerForm);
  Application.Run;
end.
