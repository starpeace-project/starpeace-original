{$APPTYPE CONSOLE}

program serialtest;

uses
  GenIdd in 'GenIdd.pas';

{$R *.RES}

const
  StarPeaceSerialClass = 0.37373737;

begin
  writeln( HeavyIddCheck( '1000-0243-3353-4436', StarPeaceSerialClass ) );
  readln;
end.
