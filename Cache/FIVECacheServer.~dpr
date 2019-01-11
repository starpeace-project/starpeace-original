program FiveCacheServer;

uses
  ShareMem,
  Forms,
  CacheServerReportForm in 'CacheServerReportForm.pas' {CacheServerReport},
  CachedObjectWrap in 'CachedObjectWrap.pas',
  FolderIteratorWrap in 'FolderIteratorWrap.pas',
  ImageClient in 'ImageClient.pas';

{$R *.RES}

begin
  CacheServerReportForm.DeleteCache := ParamCount = 0;
  Application.Initialize;
  Application.Title := 'FIVE Cache Server';
  Application.CreateForm(TCacheServerReport, CacheServerReport);
  Application.Run;
end.
