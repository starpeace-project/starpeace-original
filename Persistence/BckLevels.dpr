program BckLevels;

uses
  Forms,
  BlockLevels in 'BlockLevels.pas',
  StreamUtils in '..\Utils\Misc\StreamUtils.pas',
  DelphiStreamUtils in '..\Utils\Misc\DelphiStreamUtils.pas',
  BlkLvForm in 'BlkLvForm.pas' {Form1},
  Collection in '..\Kernel\Collection.pas',
  BackupInterfaces in 'BackupInterfaces.pas',
  BackupObjects in 'BackupObjects.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
