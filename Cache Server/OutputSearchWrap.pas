unit OutputSearchWrap;

interface

  function FindConnections(const Output, World, Town, Name : string; Count, X, Y, SortMode, Role : Integer) : string;

implementation

  uses
    Classes, SysUtils, CacheCommon, SpecialChars, OutputSearch, MathUtils;

  function FindConnections(const Output, World, Town, Name : string; Count, X, Y, SortMode, Role : integer) : string;
    var
      Search : TOuputSearch;
      Link   : TOutputLink;
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
          try
            Search := TOuputSearch.Create('Worlds\' + World + '\Outputs\' + Output, Town, Name, Count, X, Y, SortMode, fcrl);
            for i := 0 to pred(Search.Result.Count) do
              begin
                Link   := Search.Result[i];
                if Link.K > 0
                  then Links.Add(IntToStr(Link.X) + sep + IntToStr(Link.Y) + sep + Link.Facility + sep + Link.Company + sep + Link.Town + sep + FormatMoney(Link.P) + sep + IntToStr(Link.K));
              end;
          finally
            Search.Free;
            result := Links.Text;
          end;
        finally
          Links.Free;
        end;
      except
        result := '';
      end;
    end;

end.
