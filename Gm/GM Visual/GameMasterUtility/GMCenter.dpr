program GMCenter;

uses
  ShareMem,
  Forms,
  GMChat in 'GMChat.pas' {GMView},
  TextShortcuts in 'TextShortcuts.pas',
  Collection in '..\..\..\Kernel\Collection.pas',
  GMKernel in '..\..\GMKernel.pas',
  GameMaster in '..\..\GameMaster.pas',
  GMURDOMger in 'GMURDOMger.pas',
  RDOObjectRegistry in '..\..\..\Rdo\Server\RDOObjectRegistry.pas',
  RDOObjectServer in '..\..\..\Rdo\Server\RDOObjectServer.pas',
  RDOQueryServer in '..\..\..\Rdo\Server\RDOQueryServer.pas',
  RDOServer in '..\..\..\Rdo\Server\RDOServer.pas',
  RDOInterfaces in '..\..\..\Rdo\Common\RDOInterfaces.pas',
  RDOProtocol in '..\..\..\Rdo\Common\RDOProtocol.pas',
  SpoolPackets in '..\..\..\Utils\Network\SpoolPackets.pas',
  SocketComp in '..\..\..\Utils\Network\SocketComp.pas',
  SmartThreads in '..\..\..\Kernel\SmartThreads.pas',
  WinSockRDOConnection in '..\..\..\rdo\client\WinSockRDOConnection.pas',
  RDOUtils in '..\..\..\Rdo\Common\RDOUtils.pas',
  Logs in '..\..\..\Logs\Logs.pas',
  LogFile in '..\..\..\Rdo\Common\LogFile.pas',
  ErrorCodes in '..\..\..\Rdo\Common\ErrorCodes.pas',
  CompStringsParser in '..\..\..\Utils\Misc\CompStringsParser.pas',
  Threads in '..\..\..\Utils\CodeLib\Threads.pas',
  AxlDebug in '..\..\..\Utils\CodeLib\AxlDebug.pas',
  VoyagerInterfaces in '..\..\..\Voyager\VoyagerInterfaces.pas',
  VoyagerServerInterfaces in '..\..\..\Voyager\VoyagerServerInterfaces.pas',
  Protocol in '..\..\..\Protocol\Protocol.pas',
  StrUtils in '..\..\..\Utils\Misc\StrUtils.pas',
  Matrix in '..\..\..\Utils\Misc\Matrix.pas',
  BlockTicker in '..\..\..\Voyager\Components\BlockTicker.pas',
  MarqueeCtrl in '..\..\..\Voyager\Components\MarqueeCtrl.pas',
  MathUtils in '..\..\..\Utils\Misc\MathUtils.pas',
  InternationalizerComponent in '..\..\..\Voyager\Components\InternationalizerComponent.pas',
  VisualControls in '..\..\..\Voyager\Components\VisualControls.pas',
  FramedButton in '..\..\..\Voyager\Components\FramedButton.pas',
  ChatUtils in '..\ChatUtils.pas',
  Literals in '..\..\..\Voyager\Literals.pas',
  Config in '..\..\..\Voyager\Config.pas',
  Events in '..\..\..\Voyager\Events.pas',
  GMList in '..\GMList.pas',
  ShortcutForm in 'ShortcutForm.pas' {ShortcutCenter},
  EditShortcut in 'EditShortcut.pas' {EditShortcutForm},
  GameMasterLogon in 'GameMasterLogon.pas' {Logon};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TGMView, GMView);
  Application.CreateForm(TLogon, Logon);
  Application.CreateForm(TShortcutCenter, ShortcutCenter);
  Application.CreateForm(TEditShortcutForm, EditShortcutForm);
  Application.Run;
end.
