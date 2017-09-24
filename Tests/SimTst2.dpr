program SimTst2;

uses
  Forms,
  SimTst2Form in 'SimTst2Form.pas' {Form1},
  SimTst2Blks in 'SimTst2Blks.pas',
  Kernel in '..\Kernel\Kernel.pas',
  Collection in '..\Kernel\Collection.pas',
  ClassStorageInt in '..\Class Storage\ClassStorageInt.pas',
  ClassStorage in '..\Kernel\ClassStorage.pas',
  Construction in '..\StdBlocks\Construction.pas',
  Productor in '..\Kernel\Productor.pas',
  Population in '..\Kernel\Population.pas',
  World in '..\Kernel\World.pas',
  AsxCriticalSections in '..\Kernel\AsxCriticalSections.pas',
  PopulatedBlock in '..\Kernel\PopulatedBlock.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
