program MonikerTest;

uses
  Forms,
  MonMainForm in 'MonMainForm.pas' {MonikersMainForm},
  URLUtils in 'URLUtils.pas',
  DateTimeUtils in '..\..\Merchise\Utils\Misc\DateTimeUtils.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMonikersMainForm, MonikersMainForm);
  Application.Run;
end.
