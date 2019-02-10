program FIVEStudy;

uses
  Forms,
  StudyWindow in 'StudyWindow.pas' {StudyWin},
  BlockStudies in 'BlockStudies.pas',
  EditWindow in 'EditWindow.pas' {EditWin},
  DepWindow in 'DepWindow.pas' {DependencyWindow},
  CloneBlockForm in 'CloneBlockForm.pas' {CloneForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'FIVE Block Study';
  Application.CreateForm(TStudyWin, StudyWin);
  Application.CreateForm(TEditWin, EditWin);
  Application.CreateForm(TCloneForm, CloneForm);
  Application.Run;
end.
