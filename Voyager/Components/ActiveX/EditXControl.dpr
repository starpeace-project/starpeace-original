library EditXControl;

uses
  ComServ,
  EditXControl_TLB in 'EditXControl_TLB.pas',
  EditImpl in 'EditImpl.pas' {EditX: CoClass};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

{$E ocx}

begin
end.
