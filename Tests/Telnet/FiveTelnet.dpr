program FiveTelnet;

uses
  Forms,
  TelnetFrm in 'TelnetFrm.pas' {Form1},
  SocketComp in '..\..\Utils\Network\SocketComp.pas',
  SpoolPackets in '..\..\Utils\Network\SpoolPackets.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
