program VCLOrg;

uses
  Forms,
  VisualClassManager in 'VisualClassManager.pas',
  ClassHash in 'ClassHash.pas',
  VCLOrgFrm in 'VCLOrgFrm.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
