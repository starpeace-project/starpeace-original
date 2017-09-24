program MailClient;

uses
  Forms,
  MailForm in 'MailForm.pas' {MailClientForm},
  AccForm in 'AccForm.pas' {AccountForm},
  ConnectFrm in 'ConnectFrm.pas' {ConnectForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMailClientForm, MailClientForm);
  Application.CreateForm(TAccountForm, AccountForm);
  Application.CreateForm(TConnectForm, ConnectForm);
  Application.Run;
end.
