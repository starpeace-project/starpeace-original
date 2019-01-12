program DServer;

{%File 'debug.txt'}

uses
  ShareMem,
  Forms,
  MainWindow in 'MainWindow.pas' {DirectoryWin},
  DirectoryRegistry in 'DirectoryRegistry.pas',
  GenIdd in '..\Utils\Serial\GenIdd.pas',
  MemMapFile in '..\Utils\Serial\MemMapFile.pas',
  DirectoryManager in 'DirectoryManager.pas',
  Logs in '..\Logs\Logs.pas',
  DirectoryServer in 'DirectoryServer.pas',
  rc4 in '..\Kernel\rc4.pas',
  DirectoryServerProtocol in 'DirectoryServerProtocol.pas',
  FileLogger in '..\Logging\FileLogger.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'DServer.exe';
  Application.CreateForm(TDirectoryWin, DirectoryWin);
  Application.Run;
end.
