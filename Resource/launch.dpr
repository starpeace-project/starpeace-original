{$APPTYPE CONSOLE}

program launch;

uses
  Windows,
  Classes,
  ComObj;

{$R *.RES}

var
  tm : olevariant;

begin
  if paramstr(1) <> ''
    then
      begin
        tm := CreateOleObject( 'RemoteAdmin.TaskManager' );
        tm.LaunchTask( paramstr(1) );
      end;
end.


