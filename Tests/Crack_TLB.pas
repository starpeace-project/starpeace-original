unit Crack_TLB;

{ This file contains pascal declarations imported from a type library.
  This file will be written during each import or refresh of the type
  library editor.  Changes to this file will be discarded during the
  refresh process. }

{ Crack Library }
{ Version 1.0 }

interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, StdVCL;

const
  LIBID_Crack: TGUID = '{3150CC22-6E56-11D1-AB1C-008029EC1811}';

const

{ Component class GUIDs }
  Class_TestCrack: TGUID = '{3150CC24-6E56-11D1-AB1C-008029EC1811}';

type

{ Forward declarations: Interfaces }
  ITestCrack = interface;
  ITestCrackDisp = dispinterface;

{ Forward declarations: CoClasses }
  TestCrack = ITestCrack;

{ Dispatch interface for TestCrack Object }

  ITestCrack = interface(IDispatch)
    ['{3150CC23-6E56-11D1-AB1C-008029EC1811}']
    function GetCrack: WideString; safecall;
  end;

{ DispInterface declaration for Dual Interface ITestCrack }

  ITestCrackDisp = dispinterface
    ['{3150CC23-6E56-11D1-AB1C-008029EC1811}']
    function GetCrack: WideString; dispid 1;
  end;

{ TestCrackObject }

  CoTestCrack = class
    class function Create: ITestCrack;
    class function CreateRemote(const MachineName: string): ITestCrack;
  end;



implementation

uses ComObj;

class function CoTestCrack.Create: ITestCrack;
begin
  Result := CreateComObject(Class_TestCrack) as ITestCrack;
end;

class function CoTestCrack.CreateRemote(const MachineName: string): ITestCrack;
begin
  Result := CreateRemoteComObject(MachineName, Class_TestCrack) as ITestCrack;
end;


end.
