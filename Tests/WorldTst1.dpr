program WorldTst1;

uses
  Forms,
  WorldTest1Form in 'WorldTest1Form.pas' {WorldTestForm},
  Kernel in '..\Kernel\Kernel.pas',
  Collection in '..\Kernel\Collection.pas',
  World in '..\Kernel\World.pas',
  ClassStorage in '..\Kernel\ClassStorage.pas',
  ClassStorageInt in '..\Class Storage\ClassStorageInt.pas',
  AsxCriticalSections in '..\Kernel\AsxCriticalSections.pas',
  Population in '..\Kernel\Population.pas',
  PopulatedBlock in '..\Kernel\PopulatedBlock.pas',
  WorkCenterBlock in '..\Kernel\WorkCenterBlock.pas',
  BlockView in 'BlockView.pas' {BlockViewer},
  PublicFacility in '..\Kernel\PublicFacility.pas',
  Construction in '..\StdBlocks\Construction.pas',
  Trade in '..\Kernel\Trade.pas',
  Productor in '..\Kernel\Productor.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TWorldTestForm, WorldTestForm);
  Application.CreateForm(TBlockViewer, BlockViewer);
  Application.Run;
end.
