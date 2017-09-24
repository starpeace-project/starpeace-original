unit BackUpMgr_TLB;

{ This file contains pascal declarations imported from a type library.
  This file will be written during each import or refresh of the type
  library editor.  Changes to this file will be discarded during the
  refresh process. }

{ BackUpMgr Library }
{ Version 1.0 }

interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, StdVCL;

const
  LIBID_BackUpMgr: TGUID = '{7E819380-5C05-11D4-8C56-0048546B6CD9}';

const

{ Component class GUIDs }
  Class_BackUpManager: TGUID = '{7E819382-5C05-11D4-8C56-0048546B6CD9}';

type

{ Forward declarations: Interfaces }
  IBackUpManager = interface;
  IBackUpManagerDisp = dispinterface;

{ Forward declarations: CoClasses }
  BackUpManager = IBackUpManager;

{ Dispatch interface for BackUpManager Object }

  IBackUpManager = interface(IDispatch)
    ['{7E819381-5C05-11D4-8C56-0048546B6CD9}']
    function Get_ServersDir: WideString; safecall;
    procedure Set_ServersDir(const Value: WideString); safecall;
    procedure EnumBackups; safecall;
    function GetBackupName(i: Integer): WideString; safecall;
    function SetCurrentBackup(idx: Integer): WordBool; safecall;
    function Get_WorldName: WideString; safecall;
    procedure Set_WorldName(const Value: WideString); safecall;
    function Get_BackupCount: Integer; safecall;
    function GetBackupDate(i: Integer): WideString; safecall;
    function GetBackupSize(i: Integer): WideString; safecall;
    property ServersDir: WideString read Get_ServersDir write Set_ServersDir;
    property WorldName: WideString read Get_WorldName write Set_WorldName;
    property BackupCount: Integer read Get_BackupCount;
  end;

{ DispInterface declaration for Dual Interface IBackUpManager }

  IBackUpManagerDisp = dispinterface
    ['{7E819381-5C05-11D4-8C56-0048546B6CD9}']
    property ServersDir: WideString dispid 1;
    procedure EnumBackups; dispid 2;
    function GetBackupName(i: Integer): WideString; dispid 3;
    function SetCurrentBackup(idx: Integer): WordBool; dispid 4;
    property WorldName: WideString dispid 5;
    property BackupCount: Integer readonly dispid 6;
    function GetBackupDate(i: Integer): WideString; dispid 7;
    function GetBackupSize(i: Integer): WideString; dispid 8;
  end;

{ BackUpManagerObject }

  CoBackUpManager = class
    class function Create: IBackUpManager;
    class function CreateRemote(const MachineName: string): IBackUpManager;
  end;



implementation

uses ComObj;

class function CoBackUpManager.Create: IBackUpManager;
begin
  Result := CreateComObject(Class_BackUpManager) as IBackUpManager;
end;

class function CoBackUpManager.CreateRemote(const MachineName: string): IBackUpManager;
begin
  Result := CreateRemoteComObject(MachineName, Class_BackUpManager) as IBackUpManager;
end;


end.
