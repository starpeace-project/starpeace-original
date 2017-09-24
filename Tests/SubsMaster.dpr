program SubsMaster;

uses
  ShareMem,
  Forms,
  SubsMasterFrm in 'SubsMasterFrm.pas' {MainForm},
  GenIdd in '..\Utils\Serial\GenIdd.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
