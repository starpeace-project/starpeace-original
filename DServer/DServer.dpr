program DServer;

{
SPO Directory Server Build v1.0
}


uses
  ShareMem,
  Forms,
  MainWindow in 'MainWindow.pas' {DirectoryWin},
  DirectoryRegistry in 'DirectoryRegistry.pas',
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
