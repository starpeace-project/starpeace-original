program AccTrans;

uses
  ShareMem,
  Forms,
  MainFrm in 'MainFrm.pas' {MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
