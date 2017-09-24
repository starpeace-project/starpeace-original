program BackUp;

uses
  ShareMem,
  Forms,
  BackupFrm in 'BackupFrm.pas' {Form1},
  Collection in '..\Kernel\Collection.pas',
  BackupObjects in 'BackupObjects.pas',
  Logs in '..\Logs\Logs.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
 