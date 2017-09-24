program FIVEBlackBox;

uses
  Forms,
  MainForm in 'MainForm.pas' {BlackBoxForm},
  BlackBox in 'BlackBox.pas' {LogForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'FIVE Black Box';
  Application.CreateForm(TBlackBoxForm, BlackBoxForm);
  Application.CreateForm(TLogForm, LogForm);
  Application.Run;
end.
