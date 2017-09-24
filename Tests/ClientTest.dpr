program ClientTest;

uses
  ShareMem,
  Forms,
  clientmainwindow in 'clientmainwindow.pas' {WorldTestForm},
  FiveLogonDialog in 'FiveLogonDialog.pas' {FiveLogonDlg},
  WebBrowser in 'WebBrowser.pas',
  BackgroundForm in 'BackgroundForm.pas' {BackForm},
  NewCompanyDialog in 'NewCompanyDialog.pas' {NewCompanyDlg},
  ChaseListDialog in 'ChaseListDialog.pas' {ChaseListDlg},
  Protocol in '..\Protocol\Protocol.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TWorldTestForm, WorldTestForm);
  Application.CreateForm(TFiveLogonDlg, FiveLogonDlg);
  Application.CreateForm(TBackForm, BackForm);
  Application.CreateForm(TNewCompanyDlg, NewCompanyDlg);
  Application.CreateForm(TChaseListDlg, ChaseListDlg);
  Application.Run;
end.
