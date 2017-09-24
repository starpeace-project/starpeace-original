program SimTst1;

uses
  Forms,
  SimTst1Form in 'SimTst1Form.pas' {Form1},
  Collection in '..\Kernel\Collection.pas',
  ClassStorageInt in '..\Class Storage\ClassStorageInt.pas',
  ClassStorage in '..\Kernel\ClassStorage.pas',
  SimTst1Blks in 'SimTst1Blks.pas',
  Kernel in '..\Kernel\Kernel.pas',
  World in '..\Kernel\World.pas',
  AsxCriticalSections in '..\Kernel\AsxCriticalSections.pas';

{$R *.RES}

begin
  InitTheClassStorage;
  InitBlocks;

  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;

  DoneTheClassStorage;
end.
