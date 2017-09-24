library OutputSrch;

uses
  ComServ,
  OutputSrch_TLB in 'OutputSrch_TLB.pas',
  OutputSearchAuto in 'OutputSearchAuto.pas' {OutputSearch: CoClass},
  Collection in '..\Kernel\Collection.pas',
  CacheRegistryData in 'CacheRegistryData.pas',
  CacheObjects in 'CacheObjects.pas',
  ObjectIndex in '..\Kernel\ObjectIndex.pas',
  OutputSearch in 'OutputSearch.pas',
  FluidLinks in 'FluidLinks.pas',
  AutoLog in 'AutoLog.pas';

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
