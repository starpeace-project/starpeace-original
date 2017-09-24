program IBTest;

uses
  ShareMem,
  Forms,
  IBTestMain in 'IBTestMain.pas' {Form1},
  NewTeamDlg in 'NewTeamDlg.pas' {NewTeam},
  HireCriminalDlg in 'HireCriminalDlg.pas' {HireCriminal},
  TestClientView in 'TestClientView.pas',
  Actions in 'IBKernel\Actions.pas',
  IBSystem in 'IBKernel\IBSystem.pas',
  MissionEngine in 'IBKernel\Mission Engine\MissionEngine.pas',
  XMLFile in 'Utils\XMLFile.pas',
  stringutils in 'Utils\stringutils.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TNewTeam, NewTeam);
  Application.CreateForm(THireCriminal, HireCriminal);
  Application.Run;
end.
