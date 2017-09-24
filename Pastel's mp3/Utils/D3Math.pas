unit D3Math;

interface

  function Min(a, b : integer) : integer;
  function Max(a, b : integer) : integer;

implementation

  function Min(a, b : integer) : integer;
    begin
      if a < b
        then Result := a
        else Result := b;
    end;

  function Max(a, b : integer) : integer;
    begin
      if a > b
        then Result := a
        else Result := b;
    end;

end.
