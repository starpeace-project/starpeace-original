unit InputSearchWrap;

interface

  function FindConnections(const Input, World, Town, Name : string; Count, X, Y, SortMode, Role : integer) : string;

implementation

  uses
    Classes, SysUtils, CacheCommon, SpecialChars, InputSearch, MathUtils;

  function FindConnections(const Input, World, Town, Name : string; Count, X, Y, SortMode, Role : integer) : string;
    var
      Search : TInputSearch;
      Link   : TInputLink;
      i      : integer;
      sep    : char;
      fcrl   : TFacilityRoleSet absolute Role; // ???
      Links  : TStringList;
    begin
      try
        sep    := BackslashChar;
        result := '';
        Search := nil;
        Links  := TStringList.Create;
        try
          Search :=
            TInputSearch.Create(
              'Worlds\' + World + '\Inputs\' + Input,
              Town,
              Name,
              Count,
              X, Y,
              SortMode,
              fcrl);
          for i := 0 to pred(Search.Result.Count) do
            begin
              Link   := Search.Result[i];
              Links.Add(IntToStr(Link.X) + sep + IntToStr(Link.Y) + sep + Link.Facility + sep + Link.Company + sep + Link.Town);
            end;
        finally
          Search.Free;
          result := Links.Text;
          Links.Free;
        end;
      except
        result := '';
      end;
    end;

end.
