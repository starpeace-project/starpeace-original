library NewsBoard;

uses
  ComServ,
  NewsBoard_TLB in 'NewsBoard_TLB.pas',
  NewsObject in 'NewsObject.pas' {NewsObject: CoClass},
  CompStringsParser in '..\Utils\Misc\CompStringsParser.pas';

exports                                           
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
