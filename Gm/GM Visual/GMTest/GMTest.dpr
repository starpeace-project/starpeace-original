program GMTest;

uses
  Forms,
  GMTestMain in 'GMTestMain.pas' {MainTestForm},
  GMServer in '..\GMServer.pas',
  GMKernel in '..\GMKernel.pas',
  Collection in '..\..\Kernel\Collection.pas',
  GMIntServer in '..\GMIntServer.pas',
  GMCostumer in '..\GMCostumer.pas',
  GameMaster in '..\GameMaster.pas',
  CustomerChat in 'CustomerChat.pas' {ClientChat},
  ChatUtils in 'ChatUtils.pas',
  GMChat in 'GMChat.pas' {GMView},
  GMChatFrame in 'GMChatFrame.pas' {GameMasterChatFrame: TFrame};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainTestForm, MainTestForm);
  Application.Run;
end.
