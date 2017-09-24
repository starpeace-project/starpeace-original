unit InputSrch_TLB;

{ This file contains pascal declarations imported from a type library.
  This file will be written during each import or refresh of the type
  library editor.  Changes to this file will be discarded during the
  refresh process. }

{ InputSrch Library }
{ Version 1.0 }

interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, StdVCL;

const
  LIBID_InputSrch: TGUID = '{B2E5C440-71A5-11D1-A1A8-DAA499D0D834}';

const

{ Component class GUIDs }
  Class_InputSearch: TGUID = '{B2E5C442-71A5-11D1-A1A8-DAA499D0D834}';

type

{ Forward declarations: Interfaces }
  IInputSearch = interface;
  IInputSearchDisp = dispinterface;

{ Forward declarations: CoClasses }
  InputSearch = IInputSearch;

{ Dispatch interface for InputSearch Object }

  IInputSearch = interface(IDispatch)
    ['{B2E5C441-71A5-11D1-A1A8-DAA499D0D834}']
    procedure Search(const Input, World, Town, Company: WideString; Count, X, Y: Integer); safecall;
    function Get_Count: Integer; safecall;
    function Get_X(row: Integer): Integer; safecall;
    function Get_Y(row: Integer): Integer; safecall;
    function Get_Capacity(row: Integer): Integer; safecall;
    function Get_SupLevel(row: Integer): Integer; safecall;
    function Get_Town(row: Integer): WideString; safecall;
    function Get_Company(row: Integer): WideString; safecall;
    function Get_Utility(row: Integer): WideString; safecall;
    function Get_SortMode: Integer; safecall;
    procedure Set_SortMode(Value: Integer); safecall;
    function Get_Role: Integer; safecall;
    procedure Set_Role(Value: Integer); safecall;
    function Get_Circuits: WideString; safecall;
    procedure Set_Circuits(const Value: WideString); safecall;
    function Get_Connected(row: Integer): WordBool; safecall;
    property Count: Integer read Get_Count;
    property X[row: Integer]: Integer read Get_X;
    property Y[row: Integer]: Integer read Get_Y;
    property Capacity[row: Integer]: Integer read Get_Capacity;
    property SupLevel[row: Integer]: Integer read Get_SupLevel;
    property Town[row: Integer]: WideString read Get_Town;
    property Company[row: Integer]: WideString read Get_Company;
    property Utility[row: Integer]: WideString read Get_Utility;
    property SortMode: Integer read Get_SortMode write Set_SortMode;
    property Role: Integer read Get_Role write Set_Role;
    property Circuits: WideString read Get_Circuits write Set_Circuits;
    property Connected[row: Integer]: WordBool read Get_Connected;
  end;

{ DispInterface declaration for Dual Interface IInputSearch }

  IInputSearchDisp = dispinterface
    ['{B2E5C441-71A5-11D1-A1A8-DAA499D0D834}']
    procedure Search(const Input, World, Town, Company: WideString; Count, X, Y: Integer); dispid 3;
    property Count: Integer readonly dispid 1;
    property X[row: Integer]: Integer readonly dispid 2;
    property Y[row: Integer]: Integer readonly dispid 4;
    property Capacity[row: Integer]: Integer readonly dispid 5;
    property SupLevel[row: Integer]: Integer readonly dispid 6;
    property Town[row: Integer]: WideString readonly dispid 7;
    property Company[row: Integer]: WideString readonly dispid 8;
    property Utility[row: Integer]: WideString readonly dispid 9;
    property SortMode: Integer dispid 10;
    property Role: Integer dispid 12;
    property Circuits: WideString dispid 11;
    property Connected[row: Integer]: WordBool readonly dispid 13;
  end;

{ InputSearchObject }

  CoInputSearch = class
    class function Create: IInputSearch;
    class function CreateRemote(const MachineName: string): IInputSearch;
  end;



implementation

uses ComObj;

class function CoInputSearch.Create: IInputSearch;
begin
  Result := CreateComObject(Class_InputSearch) as IInputSearch;
end;

class function CoInputSearch.CreateRemote(const MachineName: string): IInputSearch;
begin
  Result := CreateRemoteComObject(MachineName, Class_InputSearch) as IInputSearch;
end;


end.
