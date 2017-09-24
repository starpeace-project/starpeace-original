library spcrypt;

uses
  ComServ,
  spcrypt_TLB in 'spcrypt_TLB.pas',
  rc4auto in 'rc4auto.pas' {RC4: CoClass};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
