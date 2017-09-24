{$APPTYPE CONSOLE}

program MemTst1;

uses
  Collection in '..\Kernel\Collection.pas';

{$R *.RES}

var
  i : integer;
  p : pointer;

begin
  for i := 1 to 100000 do
    TCollection.Create( 10, 20, rkUse );
  readln;
end.
