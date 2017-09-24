program WebBrowserTest;

  uses
    Forms,
  WebBrowserHostTest in 'WebBrowserHostTest.pas' {WebBrowserForm},
  InternetSecurityManager in 'InternetSecurityManager.pas',
  CustomWebBrowser in 'CustomWebBrowser.pas',
  LogFile in 'LogFile.pas',
  Mshtmhst in 'Mshtmhst.pas',
  SHDocVw in 'SHDocVw.pas',
  URLMon2 in 'URLMon2.pas';

  {$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TWebBrowserForm, WebBrowserForm);
  Application.Run;
end.

