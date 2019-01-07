unit InputSearchAuto;

interface

uses
  ComObj, InputSrch_TLB, InputSearch, FluidLinks, CacheCommon, StdVcl;

type
  TInputSearch = class(TAutoObject, IInputSearch)
  protected
    function Get_Company(row: Integer): WideString; safecall;
    function Get_Count: Integer; safecall;
    function Get_Capacity(row: Integer): Integer; safecall;
    function Get_SupLevel(row: Integer): Integer; safecall;
    function Get_Town(row: Integer): WideString; safecall;
    function Get_Utility(row: Integer): WideString; safecall;
    function Get_X(row: Integer): Integer; safecall;
    function Get_Y(row: Integer): Integer; safecall;
    procedure Search(const Input, World, Town, Company: WideString; Count, X, Y: Integer); safecall;
    function Get_SortMode: Integer; safecall;
    procedure Set_SortMode(Value: Integer); safecall;
    function Get_Role: Integer; safecall;
    procedure Set_Role(Value: Integer); safecall;
    function Get_Circuits: WideString; safecall;
    function Get_Connected(row: Integer): WordBool; safecall;
    procedure Set_Circuits(const Value: WideString); safecall;
  public
    destructor Destroy; override;
  private
    fInputSearch : InputSearch.TInputSearch;
    fSortMode    : word;
    fCircuits    : string;
    fRole        : byte;
  end;

implementation

  uses
    ComServ, CacheObjects, {AutoLog,} SysUtils;

  // TInputSearch

  function TInputSearch.Get_Company(row: Integer): WideString;
    begin
      try
        if (fInputSearch <> nil) and (row < fInputSearch.Result.Count)
          then result := WideString(fInputSearch.Result[row].Company)
          else result := '';
      except
        result := '';
      end;
    end;

  function TInputSearch.Get_Count: Integer;
    begin
      try
        if fInputSearch <> nil
          then result := fInputSearch.Result.Count
          else result := 0;
      except
        result := 0;
      end;
    end;

  function TInputSearch.Get_Capacity(row: Integer): Integer;
    begin
      try
        if (fInputSearch <> nil) and (row < fInputSearch.Result.Count)
          then result := fInputSearch.Result[row].Capacity
          else result := 0;
      except
        result := 0;
      end;
    end;

  function TInputSearch.Get_SupLevel(row: Integer): Integer;
    begin
      try
        if (fInputSearch <> nil) and (row < fInputSearch.Result.Count)
          then result := fInputSearch.Result[row].SupLevel
          else result := 0;
      except
        result := 0;
      end;
    end;

  function TInputSearch.Get_Town(row: Integer): WideString;
    begin
      try
        if (fInputSearch <> nil) and (row < fInputSearch.Result.Count)
          then result := WideString(fInputSearch.Result[row].Town)
          else result := '';
      except
        result := '';
      end;
    end;

  function TInputSearch.Get_Utility(row: Integer): WideString;
    begin
      try
        if (fInputSearch <> nil) and (row < fInputSearch.Result.Count)
          then result := WideString(fInputSearch.Result[row].Facility)
          else result := '';
      except
        result := '';
      end;
    end;

  function TInputSearch.Get_X(row: Integer): Integer;
    begin
      try
        if (fInputSearch <> nil) and (row < fInputSearch.Result.Count)
          then result := fInputSearch.Result[row].X
          else result := 0;
      except
        result := 0;
      end;
    end;

  function TInputSearch.Get_Y(row: Integer): Integer;
    begin
      try
        if (fInputSearch <> nil) and (row < fInputSearch.Result.Count)
          then result := fInputSearch.Result[row].Y
          else result := 0
      except
        result := 0;
      end;
    end;

  destructor TInputSearch.Destroy;
    begin
      try
        fInputSearch.Free;
      except
      end;
      inherited;
    end;

  procedure TInputSearch.Search(const Input, World, Town, Company: WideString; Count, X, Y : Integer);
    begin
      //AutoLog.Log('input', DateTimeToStr(Now) + ': Start search ' + Input);
      try
        fInputSearch :=
          InputSearch.TInputSearch.Create(
            'Worlds\' + World + '\Inputs\' + Input,
            Town,
            Company,
            Count,
            X, Y,
            fSortMode,
            TFacilityRoleSet(fRole));
      except
        fInputSearch := nil;
      end;
      //AutoLog.Log('input', DateTimeToStr(Now) + ': End search ' + Input);
    end;

  function TInputSearch.Get_SortMode: Integer;
    begin
      result := fSortMode;
    end;

  procedure TInputSearch.Set_SortMode(Value: Integer);
    begin
      fSortMode := Value;
    end;

  function TInputSearch.Get_Role: Integer;
    begin
      result := fRole;
    end;

  procedure TInputSearch.Set_Role(Value: Integer);
    begin
      fRole := Value;
    end;

  function TInputSearch.Get_Circuits: WideString;
    begin
      result := fCircuits;
    end;

  function TInputSearch.Get_Connected(row: Integer): WordBool;
    begin
      try
        if (fInputSearch <> nil) and (row < fInputSearch.Result.Count)
          then result := fInputSearch.Result[row].Intercept(fCircuits)
          else result := false;
      except
        result := false;
      end;
    end;

  procedure TInputSearch.Set_Circuits(const Value: WideString);
    begin
      fCircuits := Value;
    end;

initialization

  //AutoLog.InitLogs;

  TAutoObjectFactory.Create(ComServer, TInputSearch, Class_InputSearch, ciMultiInstance);

end.
