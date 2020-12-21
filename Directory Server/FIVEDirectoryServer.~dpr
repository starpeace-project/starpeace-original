program FIVEDirectoryServer;

uses
  Forms,
  MainWindow in 'MainWindow.pas' {DirectoryWin},
  DirectoryRegistry in 'DirectoryRegistry.pas',
  DirectoryServer in 'DirectoryServer.pas',
  AutoCheck in '..\Utils\Serial\AutoCheck.pas',
  GenIdd in '..\Utils\Serial\GenIdd.pas',
  DirectoryServerProtocol in 'DirectoryServerProtocol.pas',
  CRC32 in '..\Utils\Serial\CRC32.pas',
  MemMapFile in '..\Utils\Serial\MemMapFile.pas',
  Logs in '..\Logs\Logs.pas',
  DirectoryManager in 'DirectoryManager.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TDirectoryWin, DirectoryWin);
  Application.Run;
end.
