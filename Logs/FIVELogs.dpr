library FIVELogs;

uses
  ShareMem,
  SysUtils,
  Classes,
  LogDll in 'LogDll.pas';

exports
  Log,
  InitSocket;


begin
  IsMultiThread := true;
end.
