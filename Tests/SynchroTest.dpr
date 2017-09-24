program SynchroTest;

uses
  Forms,
  SyncTest in 'SyncTest.pas' {Form1},
  Synchro in '..\Utils\Synchro\synchro.pas',
  URLUtils in '..\Utils\Network\URLUtils.pas',
  Collection in '..\Kernel\Collection.pas',
  Threads in '..\Utils\CodeLib\Threads.pas',
  AxlDebug in '..\Utils\CodeLib\AxlDebug.pas',
  CabUtils in '..\Utils\Synchro\CabUtils.pas',
  SyncIndex in '..\Utils\Synchro\SyncIndex.pas';

{$R *.RES}

begin
  BeginThreads;
  InitSynchro( 4 );
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
  DoneSynchro;
  EndThreads;
end.
