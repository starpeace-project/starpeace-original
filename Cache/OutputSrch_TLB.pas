unit OutputSrch_TLB;

{ This file contains pascal declarations imported from a type library.
  This file will be written during each import or refresh of the type
  library editor.  Changes to this file will be discarded during the
  refresh process. }

{ OutputSrch Library }
{ Version 1.0 }

interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, StdVCL;

const
  LIBID_OutputSrch: TGUID = '{48DB8B20-6705-11D1-A1A8-955D74A3A3E2}';

const

{ Component class GUIDs }
  Class_OutputSearch: TGUID = '{48DB8B22-6705-11D1-A1A8-955D74A3A3E2}';

type

{ Forward declarations: Interfaces }
  IOutputSearch = interface;
  IOutputSearchDisp = dispinterface;

{ Forward declarations: CoClasses }
  OutputSearch = IOutputSearch;

{ Dispatch interface for OutputSearch Object }

  IOutputSearch = interface(IDispatch)
    ['{48DB8B21-6705-11D1-A1A8-955D74A3A3E2}']
    procedure Search(const Output, World, Town, Company: WideString; Count, X, Y: Integer); safecall;
    function Get_Count: Integer; safecall;
    function Get_X(row: Integer): Integer; safecall;
    function Get_Y(row: Integer): Integer; safecall;
    function Get_K(row: Integer): Integer; safecall;
    function Get_P(row: Integer): Single; safecall;
    function Get_C(row: Integer): Single; safecall;
    function Get_Town(row: Integer): WideString; safecall;
    function Get_Company(row: Integer): WideString; safecall;
    function Get_Utility(row: Integer): WideString; safecall;
    function Get_SortMode: Integer; safecall;
    procedure Set_SortMode(Value: Integer); safecall;
    function Get_Role: Integer; safecall;
    procedure Set_Role(Value: Integer); safecall;
    function Get_Circuits: WideString; safecall;
    procedure Set_Circuits(const Value: WideString); safecall;
    function Get_Connected(index: Integer): WordBool; safecall;
    property Count: Integer read Get_Count;
    property X[row: Integer]: Integer read Get_X;
    property Y[row: Integer]: Integer read Get_Y;
    property K[row: Integer]: Integer read Get_K;
    property P[row: Integer]: Single read Get_P;
    property C[row: Integer]: Single read Get_C;
    property Town[row: Integer]: WideString read Get_Town;
    property Company[row: Integer]: WideString read Get_Company;
    property Utility[row: Integer]: WideString read Get_Utility;
    property SortMode: Integer read Get_SortMode write Set_SortMode;
    property Role: Integer read Get_Role write Set_Role;
    property Circuits: WideString read Get_Circuits write Set_Circuits;
    property Connected[index: Integer]: WordBool read Get_Connected;
  end;

{ DispInterface declaration for Dual Interface IOutputSearch }

  IOutputSearchDisp = dispinterface
    ['{48DB8B21-6705-11D1-A1A8-955D74A3A3E2}']
    procedure Search(const Output, World, Town, Company: WideString; Count, X, Y: Integer); dispid 1;
    property Count: Integer readonly dispid 2;
    property X[row: Integer]: Integer readonly dispid 3;
    property Y[row: Integer]: Integer readonly dispid 4;
    property K[row: Integer]: Integer readonly dispid 5;
    property P[row: Integer]: Single readonly dispid 6;
    property C[row: Integer]: Single readonly dispid 7;
    property Town[row: Integer]: WideString readonly dispid 8;
    property Company[row: Integer]: WideString readonly dispid 9;
    property Utility[row: Integer]: WideString readonly dispid 10;
    property SortMode: Integer dispid 11;
    property Role: Integer dispid 13;
    property Circuits: WideString dispid 14;
    property Connected[index: Integer]: WordBool readonly dispid 15;
  end;

{ OutputSearchObject }

  CoOutputSearch = class
    class function Create: IOutputSearch;
    class function CreateRemote(const MachineName: string): IOutputSearch;
  end;



implementation

uses ComObj;

class function CoOutputSearch.Create: IOutputSearch;
begin
  Result := CreateComObject(Class_OutputSearch) as IOutputSearch;
end;

class function CoOutputSearch.CreateRemote(const MachineName: string): IOutputSearch;
begin
  Result := CreateRemoteComObject(MachineName, Class_OutputSearch) as IOutputSearch;
end;


end.
