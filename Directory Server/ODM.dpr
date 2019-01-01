library ODM;

uses
  ComServ,
  ODM_TLB in 'ODM_TLB.pas',
  DirectoryManager in 'DirectoryManager.pas' {DirectoryManager: CoClass};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
