program ServerInstaller;

uses
  Forms,
  ActiveX,
  Synchro in '..\Utils\Synchro\Synchro.pas',
  CabUtils in '..\Utils\Synchro\CabUtils.pas',
  SyncIndex in '..\Utils\Synchro\SyncIndex.pas',
  Collection in '..\Kernel\Collection.pas',
  Threads in '..\Utils\CodeLib\Threads.pas',
  AxlDebug in '..\Utils\CodeLib\AxlDebug.pas',
  URLUtils in '..\Utils\Network\URLUtils.pas',
  MathUtils in '..\Utils\Misc\MathUtils.pas',
  LookForFolder in '..\Utils\Misc\LookForFolder.pas',
  SHDocVw in '..\Voyager\Components\WebBrowser\SHDocVw.pas',
  Mshtmhst in '..\Voyager\Components\WebBrowser\Mshtmhst.pas',
  URLMon2 in '..\Voyager\Components\WebBrowser\URLMon2.pas',
  InternetSecurityManager in '..\Voyager\Components\WebBrowser\InternetSecurityManager.pas',
  LogFile in '..\Voyager\Components\WebBrowser\LogFile.pas',
  DelphiStreamUtils in '..\Utils\Misc\DelphiStreamUtils.pas',
  CompStringsParser in '..\Utils\Misc\CompStringsParser.pas',
  MarqueeCtrl in '..\voyager\components\MarqueeCtrl.pas',
  URL2File in '..\Utils\Network\URL2File.pas',
  StrUtils in '..\Utils\Misc\StrUtils.pas',
  SmartThreads in '..\Kernel\SmartThreads.pas',
  SocketComp in '..\Utils\Network\SocketComp.pas',
  CustomWebBrowser in '..\Voyager\Components\WebBrowser\CustomWebBrowser.pas',
  SeverUpdateForm in 'SeverUpdateForm.pas' {Form1};

{$R *.RES}

begin
  BeginThreads;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
  EndThreads;
end.
