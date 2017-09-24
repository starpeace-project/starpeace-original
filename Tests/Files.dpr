program Files;

uses
  Forms,
  CacheProxy in '..\CacheProxy.pas',
  FilesForm in '..\FilesForm.pas' {Form1},
  FileImage in '..\FileImage.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
