unit CacheObjs_TLB;

{ This file contains pascal declarations imported from a type library.
  This file will be written during each import or refresh of the type
  library editor.  Changes to this file will be discarded during the
  refresh process. }

{ CacheObjs Library }
{ Version 1.0 }

interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, StdVCL;

const
  LIBID_CacheObjs: TGUID = '{E2AEB740-54E5-11D1-A1A8-141207C10000}';

const

{ Component class GUIDs }
  Class_FolderIteratorAuto: TGUID = '{E2AEB744-54E5-11D1-A1A8-141207C10000}';
  Class_CacheProxyAuto: TGUID = '{670E4281-54FA-11D1-A1A8-141207C10000}';

type

{ Forward declarations: Interfaces }
  IFolderIterator = interface;
  IFolderIteratorDisp = dispinterface;
  ICacheProxy = interface;
  ICacheProxyDisp = dispinterface;

{ Forward declarations: CoClasses }
  FolderIteratorAuto = IFolderIterator;
  CacheProxyAuto = ICacheProxy;

{ Dispatch interface for FolderIterator Object }

  IFolderIterator = interface(IDispatch)
    ['{E2AEB743-54E5-11D1-A1A8-141207C10000}']
    procedure SetFolder(const aPath: WideString; Options: Integer); safecall;
    procedure Reset; safecall;
    function Next: WordBool; safecall;
    procedure SetOptions(Options: Integer); safecall;
    function Empty: WordBool; safecall;
    function Get_Current: WideString; safecall;
    function Get_FullPath: WideString; safecall;
    property Current: WideString read Get_Current;
    property FullPath: WideString read Get_FullPath;
  end;

{ DispInterface declaration for Dual Interface IFolderIterator }

  IFolderIteratorDisp = dispinterface
    ['{E2AEB743-54E5-11D1-A1A8-141207C10000}']
    procedure SetFolder(const aPath: WideString; Options: Integer); dispid 1;
    procedure Reset; dispid 2;
    function Next: WordBool; dispid 3;
    procedure SetOptions(Options: Integer); dispid 4;
    function Empty: WordBool; dispid 5;
    property Current: WideString readonly dispid 6;
    property FullPath: WideString readonly dispid 7;
  end;

{ Dispatch interface for CacheProxyAuto Object }

  ICacheProxy = interface(IDispatch)
    ['{670E4280-54FA-11D1-A1A8-141207C10000}']
    function SetPath(const aPath: WideString): WordBool; safecall;
    function GetPath: WideString; safecall;
    function SetWorld(const Name: WideString): WordBool; safecall;
    function SetClass(const Name: WideString): WordBool; safecall;
    function SetObject(X, Y: Integer): WordBool; safecall;
    function SetObjectOfWorld(X, Y: Integer; const World: WideString): WordBool; safecall;
    function GetFolderIterator(const Folder: WideString): OleVariant; safecall;
    function ContainsFolder(const Name: WideString): WordBool; safecall;
    procedure CreateObject(const aPath: WideString); safecall;
    procedure CreateFolder(const Name: WideString); safecall;
    procedure Flush; safecall;
  end;

{ DispInterface declaration for Dual Interface ICacheProxy }

  ICacheProxyDisp = dispinterface
    ['{670E4280-54FA-11D1-A1A8-141207C10000}']
    function SetPath(const aPath: WideString): WordBool; dispid 1;
    function GetPath: WideString; dispid 2;
    function SetWorld(const Name: WideString): WordBool; dispid 8;
    function SetClass(const Name: WideString): WordBool; dispid 9;
    function SetObject(X, Y: Integer): WordBool; dispid 10;
    function SetObjectOfWorld(X, Y: Integer; const World: WideString): WordBool; dispid 12;
    function GetFolderIterator(const Folder: WideString): OleVariant; dispid 3;
    function ContainsFolder(const Name: WideString): WordBool; dispid 4;
    procedure CreateObject(const aPath: WideString); dispid 6;
    procedure CreateFolder(const Name: WideString); dispid 7;
    procedure Flush; dispid 5;
  end;

{ FolderIteratorObject }

  CoFolderIteratorAuto = class
    class function Create: IFolderIterator;
    class function CreateRemote(const MachineName: string): IFolderIterator;
  end;

{ CacheProxyAutoObject }

  CoCacheProxyAuto = class
    class function Create: ICacheProxy;
    class function CreateRemote(const MachineName: string): ICacheProxy;
  end;



implementation

uses ComObj;

class function CoFolderIteratorAuto.Create: IFolderIterator;
begin
  Result := CreateComObject(Class_FolderIteratorAuto) as IFolderIterator;
end;

class function CoFolderIteratorAuto.CreateRemote(const MachineName: string): IFolderIterator;
begin
  Result := CreateRemoteComObject(MachineName, Class_FolderIteratorAuto) as IFolderIterator;
end;

class function CoCacheProxyAuto.Create: ICacheProxy;
begin
  Result := CreateComObject(Class_CacheProxyAuto) as ICacheProxy;
end;

class function CoCacheProxyAuto.CreateRemote(const MachineName: string): ICacheProxy;
begin
  Result := CreateRemoteComObject(MachineName, Class_CacheProxyAuto) as ICacheProxy;
end;


end.
