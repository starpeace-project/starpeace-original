program SPGreedy;

uses
  ShareMem,
  Forms,
  MainFrm in 'MainFrm.pas' {MainForm},
  rc4 in '..\Kernel\rc4.pas',
  ASPENCRYPTLib_TLB in 'ASPENCRYPTLib_TLB.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'GreedWorks';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
