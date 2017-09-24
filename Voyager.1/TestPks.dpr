program TestPks;

uses
  Forms,
  TestFrm in 'Components\TestFrm.pas' {Form2},
  PercentEdit in 'Components\PercentEdit.pas',
  FingerTabs in 'Components\FingerTabs.pas',
  MathUtils in '..\Utils\Misc\MathUtils.pas',
  CompStringsParser in '..\Utils\Misc\CompStringsParser.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
