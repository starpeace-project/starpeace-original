unit ClassesUtils;

// Copyright (c) 1998 Jorge Romero Gomez, Merchise

interface

  uses
    Classes;

  function FindStr( Strings : TStrings; const Item : string ) : integer;
  function AddStrUnique( Strings : TStrings; const Item : string ) : integer;

implementation

  uses
    SysUtils;
    
  function FindStr( Strings : TStrings; const Item : string ) : integer;
    var
      i : integer;
    begin
      i := 0;
      with Strings do
        while ( i < Count ) and ( AnsiCompareText( Strings[i], Item ) <> 0  ) do
          inc( i );
      if i < Strings.Count
        then Result := i
        else Result := -1;
    end;

  function AddStrUnique( Strings : TStrings; const Item : string ) : integer;
    begin
      if FindStr( Strings, Item ) = -1
        then Result := Strings.Add( Item )
        else Result := -1;
    end;
    
end.
