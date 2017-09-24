program OtpSrh;

uses
  Forms,
  OtptSrchFrm in 'OtptSrchFrm.pas' {Form1},
  OutputSearch in '..\OutputSearch.pas',
  Collection in '..\..\Kernel\Collection.pas',
  CompStringsParser in '..\CompStringsParser.pas',
  CacheObjects in '..\CacheObjects.pas',
  SpecialChars in '..\SpecialChars.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
