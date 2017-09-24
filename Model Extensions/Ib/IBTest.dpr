program IBTest;

uses
  Forms,
  IllegalKernel in '..\..\Illegal\Illegal Kernel\IllegalKernel.pas',
  ClassStorageInt in '..\..\Class Storage\ClassStorageInt.pas',
  Collection in '..\..\Kernel\Collection.pas',
  Unit1 in '..\..\Illegal\Illegal Kernel\Unit1.pas' {Form1},
  ClassStorage in '..\..\Kernel\ClassStorage.pas',
  Creator in '..\..\Illegal\Illegal Kernel\Creator.pas',
  CrimeProtocol in '..\..\Illegal\Illegal Kernel\CrimeProtocol.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
