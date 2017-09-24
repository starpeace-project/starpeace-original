unit MailBrowser_TLB;

{ This file contains pascal declarations imported from a type library.
  This file will be written during each import or refresh of the type
  library editor.  Changes to this file will be discarded during the
  refresh process. }

{ MailBrowser Library }
{ Version 1.0 }

interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, StdVCL;

const
  LIBID_MailBrowser: TGUID = '{4F7EC360-AD33-11D1-A1A8-0080C817C099}';

const

{ Component class GUIDs }
  Class_MailBrowser: TGUID = '{4F7EC362-AD33-11D1-A1A8-0080C817C099}';

type

{ Forward declarations: Interfaces }
  IMailBrowser = interface;
  IMailBrowserDisp = dispinterface;

{ Forward declarations: CoClasses }
  MailBrowser = IMailBrowser;

{ Dispatch interface for MailBrowser Object }

  IMailBrowser = interface(IDispatch)
    ['{4F7EC361-AD33-11D1-A1A8-0080C817C099}']
    function Get_Account: WideString; safecall;
    procedure Set_Account(const Value: WideString); safecall;
    function Get_Folder: WideString; safecall;
    procedure Set_Folder(const Value: WideString); safecall;
    procedure Reset; safecall;
    function Next: WordBool; safecall;
    function DeleteMessage(const MsgPath: WideString): WordBool; safecall;
    function Get_Empty: WordBool; safecall;
    function Get_World: WideString; safecall;
    procedure Set_World(const Value: WideString); safecall;
    function Get_Header(const Name: WideString): WideString; safecall;
    function FullPath: WideString; safecall;
    property Account: WideString read Get_Account write Set_Account;
    property Folder: WideString read Get_Folder write Set_Folder;
    property Empty: WordBool read Get_Empty;
    property World: WideString read Get_World write Set_World;
    property Header[const Name: WideString]: WideString read Get_Header;
  end;

{ DispInterface declaration for Dual Interface IMailBrowser }

  IMailBrowserDisp = dispinterface
    ['{4F7EC361-AD33-11D1-A1A8-0080C817C099}']
    property Account: WideString dispid 1;
    property Folder: WideString dispid 2;
    procedure Reset; dispid 4;
    function Next: WordBool; dispid 5;
    function DeleteMessage(const MsgPath: WideString): WordBool; dispid 11;
    property Empty: WordBool readonly dispid 8;
    property World: WideString dispid 10;
    property Header[const Name: WideString]: WideString readonly dispid 6;
    function FullPath: WideString; dispid 3;
  end;

{ MailBrowserObject }

  CoMailBrowser = class
    class function Create: IMailBrowser;
    class function CreateRemote(const MachineName: string): IMailBrowser;
  end;



implementation

uses ComObj;

class function CoMailBrowser.Create: IMailBrowser;
begin
  Result := CreateComObject(Class_MailBrowser) as IMailBrowser;
end;

class function CoMailBrowser.CreateRemote(const MachineName: string): IMailBrowser;
begin
  Result := CreateRemoteComObject(MachineName, Class_MailBrowser) as IMailBrowser;
end;


end.
