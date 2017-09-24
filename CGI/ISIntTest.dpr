program ISIntTest;
{$APPTYPE CONSOLE}
uses
  SysUtils;

  function GetWorldData( const ISAddr : string; ISPort : integer; out WorldName : string; out WorldPop, UserCount, Year, OnlineUsers : integer ) : boolean; external 'ISInterface.dll';

var
  WN : string;
  WP, UC, Y, OU : integer;
  
begin
  if GetWorldData( 'lisa', 8000, WN, WP, UC, Y, OU )
    then
      begin
        writeln( 'Connected!' );
        writeln( WN, ' ', WP, ' ', UC, ' ', Y, ' ', OU );
      end
    else writeln( 'Error!' );
  readln;
end.
