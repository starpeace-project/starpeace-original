unit OutputSearchAuto;

interface

uses
  ComObj, OutputSrch_TLB, OutputSearch;

type
  TOutputSearch = class(TAutoObject, IOutputSearch)
  protected
    function Get_Count: Integer; safecall;
    procedure Search(const Output, World, Town, Company: WideString; Count, X, Y: Integer); safecall;
    function Get_Company(row: Integer): WideString; safecall;
    function Get_K(row: Integer): Integer; safecall;
    function Get_P(row: Integer): Single; safecall;
    function Get_Town(row: Integer): WideString; safecall;
    function Get_Utility(row: Integer): WideString; safecall;
    function Get_X(row: Integer): Integer; safecall;
    function Get_Y(row: Integer): Integer; safecall;
    function Get_C(row: Integer): Single; safecall;
    function Get_SortMode: Integer; safecall;
    procedure Set_SortMode(Value: Integer); safecall;
    function Get_Role: Integer; safecall;
    procedure Set_Role(Value: Integer); safecall;
    function Get_Circuits: WideString; safecall;
    procedure Set_Circuits(const Value: WideString); safecall;
    function Get_Connected(index: Integer): WordBool; safecall;
  public
    destructor Destroy; override;
  private
    fSortMode     : integer;
    fRole         : byte;
    fCircuits     : string;
    fOutputSearch : OutputSearch.TOuputSearch;
  protected
    procedure Initialize; override;
  end;

implementation

  uses
    ComServ, CacheCommon, CacheObjects, CacheRegistryData, {AutoLog,} SysUtils;

  function TOutputSearch.Get_Count: Integer;
    begin
      if fOutputSearch <> nil
        then result := fOutputSearch.Result.Count
        else result := 0;
    end;

  procedure TOutputSearch.Search(const Output, World, Town, Company: WideString; Count, X, Y: Integer);
    begin
      //AutoLog.Log('output', DateTimeToStr(Now) + ': Start search ' + Output);
      try
        fOutputSearch.Free;
        //AutoLog.Log('output', DateTimeToStr(Now) + ': free ok.. ');
      except
        //AutoLog.Log('output', DateTimeToStr(Now) + ': free error.. ');
      end;
      try
        //AutoLog.Log('output', DateTimeToStr(Now) + ': begin create.. ');
        fOutputSearch := OutputSearch.TOuputSearch.Create('Worlds\' + World + '\Outputs\' + Output, Town, Company, Count, X, Y, fSortMode, TFacilityRoleSet(fRole));
        //AutoLog.Log('output', DateTimeToStr(Now) + ': end create.. ');
      except
        //AutoLog.Log('output', DateTimeToStr(Now) + ': error create.. ');
        fOutputSearch := nil;
      end;
      //AutoLog.Log('output', DateTimeToStr(Now) + ': end search ' + Output);
    end;

  function TOutputSearch.Get_Company(row: Integer): WideString;
    begin
      try
        if (fOutputSearch <> nil) and (row < fOutputSearch.Result.Count)
          then result := WideString(fOutputSearch.Result[row].Company)
          else result := '';
      except
        result := '';
      end;
    end;

  function TOutputSearch.Get_K(row: Integer): Integer;
    begin
      try
        if (fOutputSearch <> nil) and (row < fOutputSearch.Result.Count)
          then result := fOutputSearch.Result[row].K
          else result := 0;
      except
        result := 0;
      end;
    end;

  function TOutputSearch.Get_P(row: Integer): Single;
    begin
      try
        if (fOutputSearch <> nil) and (row < fOutputSearch.Result.Count)
          then result := fOutputSearch.Result[row].P
          else result := 0;
      except
        result := 0;
      end;
    end;

  function TOutputSearch.Get_Town(row: Integer): WideString;
    begin
      try
        if (fOutputSearch <> nil) and (row < fOutputSearch.Result.Count)
          then result := WideString(fOutputSearch.Result[row].Town)
          else result := '';
      except
        result := '';
      end;
    end;

  function TOutputSearch.Get_Utility(row: Integer): WideString;
    begin
      try
        if (fOutputSearch <> nil) and (row < fOutputSearch.Result.Count)
          then result := WideString(fOutputSearch.Result[row].Facility)
          else result := '';
      except
        result := '';
      end;
    end;

  function TOutputSearch.Get_X(row: Integer): Integer;
    begin
      try
        if (fOutputSearch <> nil) and (row < fOutputSearch.Result.Count)
          then result := fOutputSearch.Result[row].X
          else result := 0;
      except
        result := 0;
      end;
    end;

  function TOutputSearch.Get_Y(row: Integer): Integer;
    begin
      try
        if (fOutputSearch <> nil) and (row < fOutputSearch.Result.Count)
          then result := fOutputSearch.Result[row].Y
          else result := 0;
      except
        result := 0;
      end;
    end;

  destructor TOutputSearch.Destroy;
    begin
      try
        fOutputSearch.Free;
      except
      end;
      inherited;
    end;

  procedure TOutputSearch.Initialize;
    begin
    end;

  function TOutputSearch.Get_C(row: Integer): Single;
    begin
      try
        if (fOutputSearch <> nil) and (row < fOutputSearch.Result.Count)
          then result := fOutputSearch.Result.Costs[row]
          else result := 0;
      except
        result := 0;
      end;
    end;

  function TOutputSearch.Get_SortMode: Integer;
    begin
      result := fSortMode;
    end;

  procedure TOutputSearch.Set_SortMode(Value: Integer);
    begin
      fSortMode := Value;
    end;

  function TOutputSearch.Get_Role: Integer;
    begin
      result := fRole;
    end;

  procedure TOutputSearch.Set_Role(Value: Integer);
    begin
      fRole := Value;
    end;

  function TOutputSearch.Get_Circuits: WideString;
    begin
      result := fCircuits;
    end;

  procedure TOutputSearch.Set_Circuits(const Value: WideString);
    begin
      fCircuits := Value;
    end;

  function TOutputSearch.Get_Connected(index: Integer): WordBool;
    begin
      try
        if (fOutputSearch <> nil) and (index < fOutputSearch.Result.Count)
          then result := fOutputSearch.Result[index].Intercept(fCircuits)
          else result := false;
      except
        result := false;
      end;
    end;

initialization

  //AutoLog.InitLogs;

  TAutoObjectFactory.Create(ComServer, TOutputSearch, Class_OutputSearch, ciMultiInstance);

end.
