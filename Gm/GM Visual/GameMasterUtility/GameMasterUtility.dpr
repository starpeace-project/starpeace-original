program GameMasterUtility;

uses
  Forms,
  GMKernel in '..\..\GMKernel.pas',
  GameMaster in '..\..\GameMaster.pas',
  Collection in '..\..\..\Kernel\Collection.pas',
  WinSockRDOConnection in '..\..\..\Rdo\Client\WinsockRDOConnection.pas',
  RDOMarshalers in '..\..\..\Rdo\Client\RDOMarshalers.pas',
  RDOObjectProxy in '..\..\..\Rdo\Client\RDOObjectProxy.pas',
  RDOAddress in '..\..\..\Rdo\Client\RDOAddress.pas',
  ErrorCodes in '..\..\..\Rdo\Common\ErrorCodes.pas',
  LogFile in '..\..\..\Rdo\Common\LogFile.pas',
  RDOInterfaces in '..\..\..\Rdo\Common\RDOInterfaces.pas',
  RDOProtocol in '..\..\..\Rdo\Common\RDOProtocol.pas',
  RDOUtils in '..\..\..\Rdo\Common\RDOUtils.pas',
  SmartThreads in '..\..\..\Kernel\SmartThreads.pas',
  SocketComp in '..\..\..\Utils\Network\SocketComp.pas',
  SpoolPackets in '..\..\..\Utils\Network\SpoolPackets.pas',
  MathUtils in '..\..\..\Utils\Misc\MathUtils.pas',
  GMURDOMger in 'GMURDOMger.pas',
  RDOObjectRegistry in '..\..\..\Rdo\Server\RDOObjectRegistry.pas',
  RDOServer in '..\..\..\Rdo\Server\RDOServer.pas',
  RDOObjectServer in '..\..\..\Rdo\Server\RDOObjectServer.pas',
  RDOQueryServer in '..\..\..\Rdo\Server\RDOQueryServer.pas',
  CompStringsParser in '..\..\..\Utils\Misc\CompStringsParser.pas',
  Threads in '..\..\..\Utils\CodeLib\Threads.pas',
  AxlDebug in '..\..\..\Utils\CodeLib\AxlDebug.pas',
  ChatUtils in '..\ChatUtils.pas',
  GameMasterLogon in 'GameMasterLogon.pas' {Form2},
  GMChat in 'GMChat.pas' {Form1},
  VisualControls in '..\..\..\Voyager\Components\VisualControls.pas',
  GMChatFrame in 'GMChatFrame.pas' {GameMasterChatFrame},
  TextShortcuts in 'TextShortcuts.pas',
  ShortcutForm in 'ShortcutForm.pas' {ShortcutCenter},
  EditShortcut in 'EditShortcut.pas' {EditShortcutForm},
  TypInfo in 'TypInfo.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TGMView, GMView);
  Application.CreateForm(TShortcutCenter, ShortcutCenter);
  Application.CreateForm(TEditShortcutForm, EditShortcutForm);
  Application.Run;
end.
