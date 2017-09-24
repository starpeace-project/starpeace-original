unit FIVECacheServer_TLB;

{ This file contains pascal declarations imported from a type library.
  This file will be written during each import or refresh of the type
  library editor.  Changes to this file will be discarded during the
  refresh process. }

{ FIVECacheServer Library }
{ Version 1.0 }

interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, StdVCL;

const
  LIBID_FIVECacheServer: TGUID = '{3E83FEA1-952F-11D3-A907-004854602370}';

const

{ Component class GUIDs }
  Class_CachedObject: TGUID = '{3E83FEA3-952F-11D3-A907-004854602370}';

type

{ Forward declarations: Interfaces }
  ICachedObject = interface;
  ICachedObjectDisp = dispinterface;

{ Forward declarations: CoClasses }
  CachedObject = ICachedObject;

{ Dispatch interface for CachedObject Object }

  ICachedObject = interface(IDispatch)
    ['{3E83FEA2-952F-11D3-A907-004854602370}']
    function SetPath(const aPath: WideString): WordBool; safecall;
    function GetPath: WideString; safecall;
    function SetWorld(const Name: WideString): WordBool; safecall;
    function SetClass(const Name: WideString): WordBool; safecall;
    function SetObject(X, Y: Integer): WordBool; safecall;
    function SetObjectOfWorld(X, Y: Integer; const World: WideString): WordBool; safecall;
    function GetFolderIterator(const Folder: WideString): OleVariant; safecall;
    function ContainsFolder(const Name: WideString): WordBool; safecall;
    procedure CreateFolder(const Name: WideString); safecall;
    function Get_Recache: WordBool; safecall;
    procedure Set_Recache(Value: WordBool); safecall;
    function Properties(const Name: WideString): WideString; safecall;
    procedure KeepAlive; safecall;
    property Recache: WordBool read Get_Recache write Set_Recache;
  end;

{ DispInterface declaration for Dual Interface ICachedObject }

  ICachedObjectDisp = dispinterface
    ['{3E83FEA2-952F-11D3-A907-004854602370}']
    function SetPath(const aPath: WideString): WordBool; dispid 1;
    function GetPath: WideString; dispid 2;
    function SetWorld(const Name: WideString): WordBool; dispid 3;
    function SetClass(const Name: WideString): WordBool; dispid 4;
    function SetObject(X, Y: Integer): WordBool; dispid 5;
    function SetObjectOfWorld(X, Y: Integer; const World: WideString): WordBool; dispid 6;
    function GetFolderIterator(const Folder: WideString): OleVariant; dispid 7;
    function ContainsFolder(const Name: WideString): WordBool; dispid 8;
    procedure CreateFolder(const Name: WideString); dispid 10;
    property Recache: WordBool dispid 12;
    function Properties(const Name: WideString): WideString; dispid 13;
    procedure KeepAlive; dispid 9;
  end;

{ CachedObjectObject }

  CoCachedObject = class
    class function Create: ICachedObject;
    class function CreateRemote(const MachineName: string): ICachedObject;
  end;



implementation

uses ComObj;

class function CoCachedObject.Create: ICachedObject;
begin
  Result := CreateComObject(Class_CachedObject) as ICachedObject;
end;

class function CoCachedObject.CreateRemote(const MachineName: string): ICachedObject;
begin
  Result := CreateRemoteComObject(MachineName, Class_CachedObject) as ICachedObject;
end;


end.
