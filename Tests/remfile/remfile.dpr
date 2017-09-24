library remfile;

uses
  ComServ,
  remfile_TLB in 'remfile_TLB.pas',
  rem in 'rem.pas' {FS: CoClass};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
