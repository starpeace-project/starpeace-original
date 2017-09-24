program NewsLetter;

uses
  Forms,
  NLetterFrm in 'NLetterFrm.pas' {NLMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TNLMainForm, NLMainForm);
  Application.Run;
end.
