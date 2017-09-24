unit OLEUtils;

interface

  uses
    Windows, SysUtils, OLE2;
    
  type
    EBadCLSID = class(Exception);

  function CLSIDisOK( const Str : pchar) : boolean;
  function ExtIsClassId( const aExt : string ) : boolean;
  function StrToCLSID( const Str : pchar) : TCLSID;

implementation

  function CLSIDisOK( const Str : pchar) : boolean;
    const
      Pos : array[1..32] of byte =
        (1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 15, 16, 17, 18, 20,
         21, 22, 23, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36);
    var
      i : integer;
    begin
      Result := (Str[0] = '{') and (Str[37] = '}') and (StrLen(Str) = 38);
      if Result
        then
          begin
            i := Low( Pos);
            while (Str[Pos[i]] in ['0'..'9','A'..'F','a'..'f']) and (i <= High( Pos)) do
              inc(i);
            Result := (i = High( Pos) + 1);
          end;
    end;

  function ExtIsClassId( const aExt : string ) : boolean;
    begin
      if aExt <> ''
        then Result := CLSIDisOK( pchar( copy( aExt, 2, length(aExt) ) ) )
        else Result := false;

    end;

  function StrToCLSID( const Str : pchar) : TCLSID;
    var
      i : integer;
    begin
      if CLSIDisOK( Str )
        then
          with Result do
            begin
              D1 := StrToInt( '$' + Copy( Str, 2, 8));
              D2 := StrToInt( '$' + Copy( Str, 11, 4));
              D3 := StrToInt( '$' + Copy( Str, 16, 4));
              D4[0]  := StrToInt( '$' + Copy( Str, 21, 2));
              D4[1]  := StrToInt( '$' + Copy( Str, 23, 2));
              for i := 2 to 7 do
                D4[i] := StrToInt( '$' + Copy( Str, 22 + i * 2, 2));
            end
        else raise EBadCLSID.Create( 'Obtained CLSID is corrupted!!');
    end;

end.
