unit FileUtils_TLB;

{ This file contains pascal declarations imported from a type library.
  This file will be written during each import or refresh of the type
  library editor.  Changes to this file will be discarded during the
  refresh process. }                                             

{ FileUtils Library }
{ Version 1.0 }

interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, StdVCL;

const
  LIBID_FileUtils: TGUID = '{36349ED0-AF75-11D2-9764-008029EC1811}';

const

{ Component class GUIDs }
  Class_FolderIterator: TGUID = '{36349ED2-AF75-11D2-9764-008029EC1811}';

type

{ Forward declarations: Interfaces }
  IFolderIterator = interface;
  IFolderIteratorDisp = dispinterface;

{ Forward declarations: CoClasses }
  FolderIterator = IFolderIterator;

{ Dispatch interface for FolderIterator Object }

  IFolderIterator = interface(IDispatch)
    ['{36349ED1-AF75-11D2-9764-008029EC1811}']
    function FindFirst(path: WideString): WideString; safecall;
    function FindNext: WideString; safecall;
    procedure FindClose; safecall;
  end;

{ DispInterface declaration for Dual Interface IFolderIterator }

  IFolderIteratorDisp = dispinterface
    ['{36349ED1-AF75-11D2-9764-008029EC1811}']
    function FindFirst(path: WideString): WideString; dispid 1;
    function FindNext: WideString; dispid 2;
    procedure FindClose; dispid 3;
  end;

{ FolderIteratorObject }

  CoFolderIterator = class
    class function Create: IFolderIterator;
    class function CreateRemote(const MachineName: string): IFolderIterator;
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


end.
