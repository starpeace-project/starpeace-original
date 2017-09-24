program authtest;
{$APPTYPE CONSOLE}
  uses
  SysUtils,
  AuthtestFrm in 'AuthtestFrm.pas' {AuthtestForm};

var
    UserName : string;
    Password : string;
    Error    : integer;
    theForm  : TAuthtestForm;

  function authUser( usr, psw : pchar ) : integer; cdecl; external 'sega_snap.dll';

begin
  initAuthFunc;
  if ParamCount >= 2
    then
      begin
        UserName := ParamStr(1);
        Password := ParamStr(2);
        Error := authUser(pchar(UserName), pchar(Password));
        WriteLn('');
        WriteLn('User: ' + UserName + ' Password: ' + Password + ' Result: ' + GetErrorText(Error));
        WriteLn('');
      end
    else
      begin
        WriteLn('Console mode: authtest.exe <user@domain> <password>');
        WriteLn('');
        AuthtestForm := TAuthtestForm.Create(nil);
        AuthtestForm.ShowModal;
        AuthtestForm.Free;
      end;
end.
