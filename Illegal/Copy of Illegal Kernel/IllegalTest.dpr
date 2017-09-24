program IllegalTest;

uses
  Forms,
  IllegalKernel in 'IllegalKernel.pas',
  ClassStorageInt in '..\..\Class Storage\ClassStorageInt.pas',
  Collection in '..\..\Kernel\Collection.pas',
  Unit1 in 'Unit1.pas' {Form1},
  ClassStorage in '..\..\Kernel\ClassStorage.pas',
  BankRobbery in 'BankRobbery.pas',
  Creator in 'Creator.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
