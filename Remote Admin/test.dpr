{$APPTYPE CONSOLE}

program test;

uses
  Windows,
  Classes,
  ComObj,
  psapi in 'psapi.pas',
  RemoteAdm in 'RemoteAdm.pas';

{$R *.RES}

var
  ProgId : THandle;
  Procs  : TStringList;
  i      : integer;
  tm     : olevariant;

begin
  writeln( 'Proceding to launch program...' );
  {
  tm := CreateOleObject( 'RemoteAdmin.TaskManager' );
  tm.LaunchTask( paramstr(1) );
  }
  Procs := GetProcessList;
  for i := 0 to pred(Procs.Count) do
    writeln( '  ', Procs[i], '  ', THandle(Procs.Objects[i]) );
  {
  if StartProgam( 'E:\Work\Five\Release\Servers\FIVEInterfaceServer.exe Autorun', ProgId )
    then
      begin
        writeln( 'Program launch successfull.' );
        writeln( 'Press ENTER to shut it down...' );
        readln;
        if StopProgram( ProgId, 1000 )
          then writeln( 'Done!' )
          else writeln( 'Error!' )
      end;
  }
  {
  if StopProgram( 349, 1000 )
    then writeln( 'Done!' )
    else writeln( 'Error!' );
  }
  writeln( 'Press ENTER...' );
  readln;
end.
