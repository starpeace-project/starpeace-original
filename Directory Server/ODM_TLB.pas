unit ODM_TLB;

{ This file contains pascal declarations imported from a type library.
  This file will be written during each import or refresh of the type
  library editor.  Changes to this file will be discarded during the
  refresh process. }

{ ODM Library }
{ Version 1.0 }

interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, StdVCL;

const
  LIBID_ODM: TGUID = '{8B08BB10-0BE8-11D4-BEA3-0080C8915F01}';

const

{ Component class GUIDs }
  Class_DirectoryManager: TGUID = '{8B08BB12-0BE8-11D4-BEA3-0080C8915F01}';

type

{ Forward declarations: Interfaces }
  IDirectoryManager = interface;
  IDirectoryManagerDisp = dispinterface;

{ Forward declarations: CoClasses }
  DirectoryManager = IDirectoryManager;

{ Dispatch interface for DirectoryManager Object }

  IDirectoryManager = interface(IDispatch)
    ['{8B08BB11-0BE8-11D4-BEA3-0080C8915F01}']
    procedure Init(const aDirName: WideString; SetSecurity: WordBool); safecall;
    function FullPathKeyExists(const FullPathKey: WideString): OleVariant; safecall;
    function KeyExists(const KeyName: WideString): OleVariant; safecall;
    function GetKeyNames: OleVariant; safecall;
    function GetValueNames: OleVariant; safecall;
    function WriteBoolean(const Name: WideString; Value: WordBool): OleVariant; safecall;
    function WriteInteger(const Name: WideString; Value: Integer): OleVariant; safecall;
    function WriteFloat(const Name: WideString; Value: Double): OleVariant; safecall;
    function WriteString(const Name, Value: WideString): OleVariant; safecall;
    function WriteDate(const Name: WideString; Value: TDateTime): OleVariant; safecall;
    function ReadBoolean(const Name: WideString): OleVariant; safecall;
    function ReadInteger(const Name: WideString): OleVariant; safecall;
    function ReadFloat(const Name: WideString): OleVariant; safecall;
    function ReadString(const Name: WideString): OleVariant; safecall;
    function ReadDate(const Name: WideString): OleVariant; safecall;
    function FullPathValueExists(const FullPathName: WideString): OleVariant; safecall;
    function ValueExists(const Name: WideString): OleVariant; safecall;
    function DeleteFullPathNode(const FullPathNode: WideString): OleVariant; safecall;
    function DeleteNode(const NodeName: WideString): OleVariant; safecall;
    function FullQuery(const aQuery: WideString): OleVariant; safecall;
    function Query(const aQuery: WideString): OleVariant; safecall;
    function CreateFullPathKey(const FullPathKey: WideString; ForcePath: WordBool): OleVariant; safecall;
    function CreateKey(const KeyName: WideString): OleVariant; safecall;
    function SetSecurityOfKey(const FullKeyName: WideString; Security: WordBool): OleVariant; safecall;
    function KeysCount: OleVariant; safecall;
    function ValuesCount: OleVariant; safecall;
    function WriteCurrency(const Name: WideString; Value: Currency): OleVariant; safecall;
    function ReadCurrency(const Name: WideString): OleVariant; safecall;
    function IntegrateValues(const RelValuePath: WideString): OleVariant; safecall;
    function QueryKey(const FullKeyName, ValueNameList: WideString): OleVariant; safecall;
    function GetCurrentKey: OleVariant; safecall;
    function SetCurrentKey(const FullPathKey: WideString): OleVariant; safecall;
    function ReadDateAsStr(const Name: WideString): OleVariant; safecall;
    function WriteDateFromStr(const Name, Value: WideString): OleVariant; safecall;
  end;

{ DispInterface declaration for Dual Interface IDirectoryManager }

  IDirectoryManagerDisp = dispinterface
    ['{8B08BB11-0BE8-11D4-BEA3-0080C8915F01}']
    procedure Init(const aDirName: WideString; SetSecurity: WordBool); dispid 1;
    function FullPathKeyExists(const FullPathKey: WideString): OleVariant; dispid 3;
    function KeyExists(const KeyName: WideString): OleVariant; dispid 4;
    function GetKeyNames: OleVariant; dispid 6;
    function GetValueNames: OleVariant; dispid 7;
    function WriteBoolean(const Name: WideString; Value: WordBool): OleVariant; dispid 8;
    function WriteInteger(const Name: WideString; Value: Integer): OleVariant; dispid 9;
    function WriteFloat(const Name: WideString; Value: Double): OleVariant; dispid 10;
    function WriteString(const Name, Value: WideString): OleVariant; dispid 11;
    function WriteDate(const Name: WideString; Value: TDateTime): OleVariant; dispid 12;
    function ReadBoolean(const Name: WideString): OleVariant; dispid 13;
    function ReadInteger(const Name: WideString): OleVariant; dispid 14;
    function ReadFloat(const Name: WideString): OleVariant; dispid 15;
    function ReadString(const Name: WideString): OleVariant; dispid 16;
    function ReadDate(const Name: WideString): OleVariant; dispid 17;
    function FullPathValueExists(const FullPathName: WideString): OleVariant; dispid 18;
    function ValueExists(const Name: WideString): OleVariant; dispid 19;
    function DeleteFullPathNode(const FullPathNode: WideString): OleVariant; dispid 20;
    function DeleteNode(const NodeName: WideString): OleVariant; dispid 21;
    function FullQuery(const aQuery: WideString): OleVariant; dispid 22;
    function Query(const aQuery: WideString): OleVariant; dispid 23;
    function CreateFullPathKey(const FullPathKey: WideString; ForcePath: WordBool): OleVariant; dispid 24;
    function CreateKey(const KeyName: WideString): OleVariant; dispid 25;
    function SetSecurityOfKey(const FullKeyName: WideString; Security: WordBool): OleVariant; dispid 26;
    function KeysCount: OleVariant; dispid 27;
    function ValuesCount: OleVariant; dispid 28;
    function WriteCurrency(const Name: WideString; Value: Currency): OleVariant; dispid 29;
    function ReadCurrency(const Name: WideString): OleVariant; dispid 30;
    function IntegrateValues(const RelValuePath: WideString): OleVariant; dispid 31;
    function QueryKey(const FullKeyName, ValueNameList: WideString): OleVariant; dispid 32;
    function GetCurrentKey: OleVariant; dispid 5;
    function SetCurrentKey(const FullPathKey: WideString): OleVariant; dispid 33;
    function ReadDateAsStr(const Name: WideString): OleVariant; dispid 2;
    function WriteDateFromStr(const Name, Value: WideString): OleVariant; dispid 34;
  end;

{ DirectoryManagerObject }

  CoDirectoryManager = class
    class function Create: IDirectoryManager;
    class function CreateRemote(const MachineName: string): IDirectoryManager;
  end;



implementation

uses ComObj;

class function CoDirectoryManager.Create: IDirectoryManager;
begin
  Result := CreateComObject(Class_DirectoryManager) as IDirectoryManager;
end;

class function CoDirectoryManager.CreateRemote(const MachineName: string): IDirectoryManager;
begin
  Result := CreateRemoteComObject(MachineName, Class_DirectoryManager) as IDirectoryManager;
end;


end.
