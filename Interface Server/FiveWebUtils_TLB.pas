unit FiveWebUtils_TLB;

{ This file contains pascal declarations imported from a type library.
  This file will be written during each import or refresh of the type
  library editor.  Changes to this file will be discarded during the
  refresh process. }

{ FiveWebUtils Library }
{ Version 1.0 }

interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, StdVCL;

const
  LIBID_FiveWebUtils: TGUID = '{73C41814-C0DD-11D1-ABE2-008029EC1811}';

const

{ Component class GUIDs }
  Class_TURLTrigger: TGUID = '{73C41816-C0DD-11D1-ABE2-008029EC1811}';

type

{ Forward declarations: Interfaces }
  ITURLTrigger = interface;
  ITURLTriggerDisp = dispinterface;

{ Forward declarations: CoClasses }
  TURLTrigger = ITURLTrigger;

{ Dispatch interface for TURLTrigger Object }

  ITURLTrigger = interface(IDispatch)
    ['{73C41815-C0DD-11D1-ABE2-008029EC1811}']
    function Trig(const URL: WideString): WordBool; safecall;
  end;

{ DispInterface declaration for Dual Interface ITURLTrigger }

  ITURLTriggerDisp = dispinterface
    ['{73C41815-C0DD-11D1-ABE2-008029EC1811}']
    function Trig(const URL: WideString): WordBool; dispid 1;
  end;

{ TURLTriggerObject }

  CoTURLTrigger = class
    class function Create: ITURLTrigger;
    class function CreateRemote(const MachineName: string): ITURLTrigger;
  end;



implementation

uses ComObj;

class function CoTURLTrigger.Create: ITURLTrigger;
begin
  Result := CreateComObject(Class_TURLTrigger) as ITURLTrigger;
end;

class function CoTURLTrigger.CreateRemote(const MachineName: string): ITURLTrigger;
begin
  Result := CreateRemoteComObject(MachineName, Class_TURLTrigger) as ITURLTrigger;
end;


end.
