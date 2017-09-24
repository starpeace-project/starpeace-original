program FIVECacheServer;

uses
  ShareMem,
  Forms,
  SysUtils,
  CacheCommon in '..\Cache\CacheCommon.pas',
  CachedObjectWrap in 'CachedObjectWrap.pas',
  ImageClient in 'ImageClient.pas',
  CacheServerReportForm in 'CacheServerReportForm.pas' {CacheServerReport},
  CacheHistory in 'CacheHistory.pas',
  CacheManagerRDO in 'CacheManagerRDO.pas',
  CacheObjectSpool in '..\Cache\CacheObjectSpool.pas',
  CompStringsParser in '..\Utils\Misc\CompStringsParser.pas',
  MathUtils in '..\Utils\Misc\MathUtils.pas',
  HostNames in '..\Utils\Network\HostNames.pas',
  CacheObjects in '..\Cache\CacheObjects.pas',
  InputSearchWrap in 'InputSearchWrap.pas',
  OutputSearchWrap in 'OutputSearchWrap.pas',
  WinSockRDOConnectionsServer in '..\Rdo\Server\WinSockRDOConnectionsServer.pas';

{$R *.RES}

begin
  KeepCache    := ParamCount > 0;
  AutoRunCache := (ParamCount > 0) and (UpperCase(ParamStr(1)) = 'AUTORUN');
  Application.Initialize;
  Application.Title := 'FIVE Cache Server';
  Application.CreateForm(TCacheServerReport, CacheServerReport);
  Application.Run;
end.
