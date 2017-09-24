program Update;

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
  MathUtils in '..\Utils\Misc\MathUtils.pas';

{$R *.RES}

begin
  BeginThreads;
  Application.Initialize;
  Application.CreateForm(TUpdateFrm, UpdateFrm);
  Application.Run;
  EndThreads;
end.
