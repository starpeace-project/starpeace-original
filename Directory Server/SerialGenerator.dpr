program SerialGenerator;

{$APPTYPE CONSOLE}

uses
  Classes,
  SysUtils,
  GenIdd in '..\Utils\Serial\GenIdd.pas';

const
  ClassId = 0.1233;

procedure GenerateSerials;
  var
    Serial : string;
    count  : integer;
    start  : integer;
    i      : integer;
  begin                                        
    count := StrToInt( paramstr(1) );
    start := StrToInt( paramstr(2) );  
    for i := 1 to count do
      begin
        Serial := GenUniqueIdd( start + i, ClassId );
        if HeavyIddCheck( Serial, ClassId ) and LighIddCheck( Serial ) 
          then writeln( Serial )
          else beep;
      end;
  end;

begin
  GenerateSerials;
end.
