library MailMessage;

uses
  ComServ,
  MailMessage_TLB in 'MailMessage_TLB.pas',
  MailMessageAuto in 'MailMessageAuto.pas' {FiveMessage: CoClass};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
