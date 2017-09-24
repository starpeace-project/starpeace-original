program SMTPTest;

uses
  Forms,
  SMTPClient in 'SMTPClient.pas' {SMTPClientForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TSMTPClientForm, SMTPClientForm);
  Application.Run;
end.
