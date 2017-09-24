program FiveProxyCfg;

uses
  Forms,
  SocketComp in '..\Utils\Network\SocketComp.pas',
  SpoolPackets in '..\Utils\Network\SpoolPackets.pas',
  ProxyInit in '..\Utils\Network\ProxyInit.pas',
  ProxyCfgForm in '..\Utils\Network\ProxyCfgForm.pas' {ProxyCfg};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TProxyCfg, ProxyCfg);
  Application.Run;
end.
