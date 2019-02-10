program FIVENewsServer;

uses
  ShareMem,
  Forms,
  MainWindow in 'MainWindow.pas' {NewsWin},
  News in 'News.pas',
  Protocol in '..\Protocol\Protocol.pas',
  Persistent in '..\Kernel\Persistent.pas',
  BackupInterfaces in '..\Persistence\BackupInterfaces.pas',
  NewsServer in 'NewsServer.pas',
  StdReporters in 'StdReporters.pas',
  NewsRegistry in 'NewsRegistry.pas',
  GenericReporter in 'GenericReporter.pas',
  GenerateDialog in 'GenerateDialog.pas' {GenerateDlg},
  CompStringsParser in '..\Utils\Misc\CompStringsParser.pas',
  HostNames in '..\Utils\Network\HostNames.pas',
  Languages in '..\Kernel\Languages.pas',
  Logs in '..\Logs\Logs.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Five News Server';
  Application.CreateForm(TNewsWin, NewsWin);
  Application.CreateForm(TGenerateDlg, GenerateDlg);
  Application.Run;
end.
