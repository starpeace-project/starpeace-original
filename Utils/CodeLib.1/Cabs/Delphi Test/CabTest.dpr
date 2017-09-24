program CabTest;

uses
  Forms,
  CabMain in 'CabMain.pas' {CabMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TCabMainForm, CabMainForm);
  Application.Run;
end.
