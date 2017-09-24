unit NewsFileUtils_TLB;

{ This file contains pascal declarations imported from a type library.
  This file will be written during each import or refresh of the type
  library editor.  Changes to this file will be discarded during the
  refresh process. }

{ NewsFileUtils Library }
{ Version 1.0 }

interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, StdVCL;

const
  LIBID_NewsFileUtils: TGUID = '{36349ED0-AF75-11D2-9764-008029EC1811}';

const

{ Component class GUIDs }
  Class_FolderIterator: TGUID = '{36349ED2-AF75-11D2-9764-008029EC1811}';
  Class_Tools: TGUID = '{EFE5D5F6-B332-11D2-976F-008029EC1811}';

type

{ Forward declarations: Interfaces }
  IFolderIterator = interface;
  IFolderIteratorDisp = dispinterface;
  ITools = interface;
  IToolsDisp = dispinterface;

{ Forward declarations: CoClasses }
  FolderIterator = IFolderIterator;
  Tools = ITools;

{ Dispatch interface for FolderIterator Object }

  IFolderIterator = interface(IDispatch)
    ['{36349ED1-AF75-11D2-9764-008029EC1811}']
    function FindFirst(const path: WideString): WideString; safecall;
    function FindNext: WideString; safecall;
    procedure FindClose; safecall;
    function Get_LangId: WideString; safecall;
    procedure Set_LangId(const Value: WideString); safecall;
    property LangId: WideString read Get_LangId write Set_LangId;
  end;

{ DispInterface declaration for Dual Interface IFolderIterator }

  IFolderIteratorDisp = dispinterface
    ['{36349ED1-AF75-11D2-9764-008029EC1811}']
    function FindFirst(const path: WideString): WideString; dispid 1;
    function FindNext: WideString; dispid 2;
    procedure FindClose; dispid 3;
    property LangId: WideString dispid 4;
  end;

{ Dispatch interface for Tools Object }

  ITools = interface(IDispatch)
    ['{EFE5D5F5-B332-11D2-976F-008029EC1811}']
    function EncodeNewsDate(const date: WideString): WideString; safecall;
    function DecodeNewsDate(const date: WideString): WideString; safecall;
  end;

{ DispInterface declaration for Dual Interface ITools }

  IToolsDisp = dispinterface
    ['{EFE5D5F5-B332-11D2-976F-008029EC1811}']
    function EncodeNewsDate(const date: WideString): WideString; dispid 1;
    function DecodeNewsDate(const date: WideString): WideString; dispid 2;
  end;

{ FolderIteratorObject }

  CoFolderIterator = class
    class function Create: IFolderIterator;
    class function CreateRemote(const MachineName: string): IFolderIterator;
  end;

{ ToolsObject }

  CoTools = class
    class function Create: ITools;
    class function CreateRemote(const MachineName: string): ITools;
  end;



implementation

uses ComObj;

class function CoFolderIterator.Create: IFolderIterator;
begin
  Result := CreateComObject(Class_FolderIterator) as IFolderIterator;
end;

class function CoFolderIterator.CreateRemote(const MachineName: string): IFolderIterator;
begin
  Result := CreateRemoteComObject(MachineName, Class_FolderIterator) as IFolderIterator;
end;

class function CoTools.Create: ITools;
begin
  Result := CreateComObject(Class_Tools) as ITools;
end;

class function CoTools.CreateRemote(const MachineName: string): ITools;
begin
  Result := CreateRemoteComObject(MachineName, Class_Tools) as ITools;
end;


end.
