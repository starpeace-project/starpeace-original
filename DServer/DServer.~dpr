program DServer;



uses
  ShareMem,
  Forms,
  MainWindow in 'MainWindow.pas' {DirectoryWin},
  DirectoryRegistry in 'DirectoryRegistry.pas',
  AutoCheck in '..\Utils\Serial\AutoCheck.pas',
  GenIdd in '..\Utils\Serial\GenIdd.pas',
  CRC32 in '..\Utils\Serial\CRC32.pas',
  MemMapFile in '..\Utils\Serial\MemMapFile.pas',
  DirectoryManager in 'DirectoryManager.pas',
  Logs in '..\Logs\Logs.pas',
  DirectoryServer in 'DirectoryServer.pas',
  rc4 in '..\Kernel\rc4.pas',
  DirectoryServerProtocol in 'DirectoryServerProtocol.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'DServer.exe';
  Application.CreateForm(TDirectoryWin, DirectoryWin);
  Application.Run;
end.
