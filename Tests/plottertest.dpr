{$APPTYPE CONSOLE}
program plottertest;

uses
  Plotter, Math;

procedure DoStuff;
  var
    P : TPlotter;
  begin
    P := TPlotter.Create;
    P.Plot( 0, 100, 50 );
    P.Plot( 0, 100, 20 );
    P.Plot( 0, 100, 30 );
    P.Plot( 0, -20, -20 );
    P.Plot( 0, 100, 0 );
    writeln( power( 27, 1/3 ):2:2 );
    readln;
  end;

begin
  DoStuff;
end.
