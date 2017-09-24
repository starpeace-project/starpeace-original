library TestLib;

uses
  ComServ,
  TestLib_TLB in 'TestLib_TLB.pas',
  ProgressBarImpl in '..\..\..\..\..\..\Program Files\Borland\Delphi 3\Demos\ACTIVEX\TREGSVR\ProgressBarImpl.pas' {ProgressBarX: CoClass};

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
