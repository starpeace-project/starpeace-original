unit xx1_TLB;

{ This file contains pascal declarations imported from a type library.
  This file will be written during each import or refresh of the type
  library editor.  Changes to this file will be discarded during the
  refresh process. }

{ xx1 Library }
{ Version 1.0 }

interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, StdVCL;

const
  LIBID_xx1: TGUID = '{DFFABB20-65D3-11D1-AB0D-008029EC1811}';

const

{ Component class GUIDs }
  Class_Pepito: TGUID = '{DFFABB22-65D3-11D1-AB0D-008029EC1811}';

type

{ Forward declarations: Interfaces }
  IPepito = interface;
  IPepitoDisp = dispinterface;

{ Forward declarations: CoClasses }
  Pepito = IPepito;

{ Dispatch interface for Pepito Object }

  IPepito = interface(IDispatch)
    ['{DFFABB21-65D3-11D1-AB0D-008029EC1811}']
    function GetName: WideString; safecall;
  end;

{ DispInterface declaration for Dual Interface IPepito }

  IPepitoDisp = dispinterface
    ['{DFFABB21-65D3-11D1-AB0D-008029EC1811}']
    function GetName: WideString; dispid 1;
  end;

{ PepitoObject }

  CoPepito = class
    class function Create: IPepito;
    class function CreateRemote(const MachineName: string): IPepito;
  end;



implementation

uses ComObj;

class function CoPepito.Create: IPepito;
begin
  Result := CreateComObject(Class_Pepito) as IPepito;
end;

class function CoPepito.CreateRemote(const MachineName: string): IPepito;
begin
  Result := CreateRemoteComObject(MachineName, Class_Pepito) as IPepito;
end;


end.
