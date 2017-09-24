unit stringutils;

interface

  uses
    SysUtils, classes;

  procedure SymbolSeparated( str : string; symbol : char; res : TStringList );
  function stringToId( str : string; texts : array of string; Ids : array of integer ) : integer;

implementation

  procedure SymbolSeparated( str : string; symbol : char; res : TStringList );
    var
      i   : integer;
      tmp : string;
    begin
      tmp := '';
      for i := 1 to length(str) do
        begin
          if str[i] <> symbol
            then tmp := tmp + str[i]
            else
              begin
                res.Add( tmp );
                tmp := '';
              end;
        end;
      if tmp <> ''
        then res.Add( tmp );
    end;

  function stringToId( str : string; texts : array of string; Ids : array of integer ) : integer;
    var
      i : integer;
    begin
      i := 0;
      while (i < length(texts)) and (CompareText(str, texts[i]) <> 0) do
        inc( i );

      if i < length(texts)
        then result := Ids[i]
        else result := -1;
    end;



end.
 