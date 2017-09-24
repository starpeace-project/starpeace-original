unit RemoteAdmin_TLB;

{ This file contains pascal declarations imported from a type library.
  This file will be written during each import or refresh of the type
  library editor.  Changes to this file will be discarded during the
  refresh process. }

{ RemoteAdmin Library }
{ Version 1.0 }

interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, StdVCL;

const
  LIBID_RemoteAdmin: TGUID = '{B00DDE31-2732-11D3-8F76-00A0CC2C4AEF}';

const

{ Component class GUIDs }
  Class_TaskManager: TGUID = '{B00DDE33-2732-11D3-8F76-00A0CC2C4AEF}';

type

{ Forward declarations: Interfaces }
  ITaskManager = interface;
  ITaskManagerDisp = dispinterface;

{ Forward declarations: CoClasses }
  TaskManager = ITaskManager;

{ Dispatch interface for TaskManager Object }

  ITaskManager = interface(IDispatch)
    ['{B00DDE32-2732-11D3-8F76-00A0CC2C4AEF}']
    procedure EnumTasks; safecall;
    function Get_TaskCount: Integer; safecall;
    procedure Set_TaskCount(Value: Integer); safecall;
    function GetTaskName(idx: Integer): OleVariant; safecall;
    procedure StopTask(id: Integer); safecall;
    procedure KillTask(id: Integer); safecall;
    function Reboot: WordBool; safecall;
    function GetTaskId(idx: Integer): OleVariant; safecall;
    function LaunchTask(const filename: WideString): WordBool; safecall;
    property TaskCount: Integer read Get_TaskCount write Set_TaskCount;
  end;

{ DispInterface declaration for Dual Interface ITaskManager }

  ITaskManagerDisp = dispinterface
    ['{B00DDE32-2732-11D3-8F76-00A0CC2C4AEF}']
    procedure EnumTasks; dispid 1;
    property TaskCount: Integer dispid 2;
    function GetTaskName(idx: Integer): OleVariant; dispid 3;
    procedure StopTask(id: Integer); dispid 4;
    procedure KillTask(id: Integer); dispid 5;
    function Reboot: WordBool; dispid 6;
    function GetTaskId(idx: Integer): OleVariant; dispid 7;
    function LaunchTask(const filename: WideString): WordBool; dispid 8;
  end;

{ TaskManagerObject }

  CoTaskManager = class
    class function Create: ITaskManager;
    class function CreateRemote(const MachineName: string): ITaskManager;
  end;



implementation

uses ComObj;

class function CoTaskManager.Create: ITaskManager;
begin
  Result := CreateComObject(Class_TaskManager) as ITaskManager;
end;

class function CoTaskManager.CreateRemote(const MachineName: string): ITaskManager;
begin
  Result := CreateRemoteComObject(MachineName, Class_TaskManager) as ITaskManager;
end;


end.
