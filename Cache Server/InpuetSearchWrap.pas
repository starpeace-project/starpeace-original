unit InpuetSearchWrap;

interface

  function FindConnections(const Output, World, Town, Name : string; Count, X, Y, SortMode, Role : integer) : string;

implementation

  uses
    SysUtils, CacheCommon, SpecialChars, OutputSearch, MathUtils;

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
          Search := TOuputSearch.Create('Worlds\' + World + '\Outputs\' + Output, Town, Name, Count, X, Y, SortMode, fcrl);
          for i := 0 to pred(Search.Result.Count) do
            begin
              Link   := Search.Result[i];
              Links.Add(Link.Facility + sep + Link.Company + sep + Link.Town + sep + FormatMoney(Link.P) + sep + IntToStr(Link.K));
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
