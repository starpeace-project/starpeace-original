program IrregularWindows;

uses
  Forms,
  IrregularWindow in 'IrregularWindow.pas' {IrregularForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TIrregularForm, IrregularForm);
  Application.Run;
end.
