unit DumpUtils;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  function DumpAsHex( const Str : string ) : string;
  function DumpAsAscii( const Str : string ) : string;
  function DumpAsAnsi( const Str : string ) : string;

implementation

  uses
    StrUtils, NumUtils;

  function DumpAsHex( const Str : string ) : string;
    var
      i   : integer;
      Len : integer;
    begin
      Len := length(Str);
      Result := Spaces( Len * 3 + (Len div 8) * 2);
      for i := 0 to Len - 1 do
        Move( pchar(Hex( byte(Str[ Succ(i) ]), 2))[0], pchar(Result)[i * 3 + (i div 8) * 2], 2 );
      for i := 1 to Len div 8 do
        pchar(Result)[i * 8 * 3 + 2 * pred(i)] := '-';
    end;

  function DumpAsAscii( const Str : string ) : string;
    var
      i   : integer;
      Len : integer;
    begin
      Len := Length( Str );
      SetString( Result, pchar(Str), Len );
      for i := 1 to Len do
        if Result[i] < ' '
          then pchar(Result)[i-1] := '.';
    end;

  function DumpAsAnsi( const Str : string ) : string;
    begin
      Result := AsciiToAnsi( DumpAsAscii( Str ));
    end;

end.

