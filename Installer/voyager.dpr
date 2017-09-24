program voyager;

uses
  Forms,
  ActiveX,
  UpdateForm in 'UpdateForm.pas' {UpdateFrm},
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
  DelphiStreamUtils in '..\Utils\Misc\DelphiStreamUtils.pas',
  CompStringsParser in '..\Utils\Misc\CompStringsParser.pas',
  MarqueeCtrl in '..\voyager\components\MarqueeCtrl.pas',
  URL2File in '..\Utils\Network\URL2File.pas',
  SmartThreads in '..\Kernel\SmartThreads.pas',
  SocketComp in '..\Utils\Network\SocketComp.pas',
  PickSiteForm in 'PickSiteForm.pas' {PickSiteFrm},
  CustomWebBrowser in '..\Voyager\Components\WebBrowser\CustomWebBrowser.pas',
  ExceptHandle in '..\Utils\Debug\ExceptHandle.pas',
  ExcMagic in '..\Utils\Debug\ExcMagic.pas',
  MP3Reader in '..\Utils\MP3Play\MP3Reader.pas';

{$R *.RES}

begin
  BeginThreads;
  Application.Initialize;
  Application.CreateForm(TUpdateFrm, UpdateFrm);
  Application.CreateForm(TPickSiteFrm, PickSiteFrm);
  Application.Run;
  EndThreads;
end.
