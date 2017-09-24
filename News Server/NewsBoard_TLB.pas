unit NewsBoard_TLB;

{ This file contains pascal declarations imported from a type library.
  This file will be written during each import or refresh of the type
  library editor.  Changes to this file will be discarded during the
  refresh process. }

{ NewsBoard Library }
{ Version 1.0 }

interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, StdVCL;

const
  LIBID_NewsBoard: TGUID = '{DB9412F0-CEA7-11D3-B5CA-00A0CC2C4AEF}';

const

{ Component class GUIDs }
  Class_NewsObject: TGUID = '{DB9412F2-CEA7-11D3-B5CA-00A0CC2C4AEF}';

type

{ Forward declarations: Interfaces }
  INewsObject = interface;
  INewsObjectDisp = dispinterface;

{ Forward declarations: CoClasses }
  NewsObject = INewsObject;

{ Dispatch interface for NewsObject Object }

  INewsObject = interface(IDispatch)
    ['{DB9412F1-CEA7-11D3-B5CA-00A0CC2C4AEF}']
    function Author: WideString; safecall;
    function Date: WideString; safecall;
    function Body: WideString; safecall;
    function ReplyCount: Integer; safecall;
    function ReplyAuthor(index: Integer): WideString; safecall;
    function ReplyDate(index: Integer): WideString; safecall;
    function ReplySubject(index: Integer): WideString; safecall;
    function ReplyAuthorDesc(index: Integer): WideString; safecall;
    function ReplySummary(index: Integer): WideString; safecall;
    function Importance: Integer; safecall;
    function AuthorDesc: WideString; safecall;
    function Open(const path: WideString): Integer; safecall;
    function OpenReply(index: Integer): Integer; safecall;
    function ReplyPath(index: Integer): WideString; safecall;
    function NewMessage(const Parent, Author, AuthorD, Date, Subject, Body: WideString): Integer; safecall;
    function Subject: WideString; safecall;
    function FolderOnly: WordBool; safecall;
    function BodyHTML: WideString; safecall;
    function GlobalMsgCount: Integer; safecall;
    function GlobalMsgPath(index: Integer): WideString; safecall;
    procedure SetRootPath(const RootPath: WideString); safecall;
    procedure SetIndexSize(size: Integer); safecall;
    function ParentPath: WideString; safecall;
    function Summary: WideString; safecall;
    procedure SetLangId(const aLangId: WideString); safecall;
  end;

{ DispInterface declaration for Dual Interface INewsObject }

  INewsObjectDisp = dispinterface
    ['{DB9412F1-CEA7-11D3-B5CA-00A0CC2C4AEF}']
    function Author: WideString; dispid 1;
    function Date: WideString; dispid 2;
    function Body: WideString; dispid 3;
    function ReplyCount: Integer; dispid 4;
    function ReplyAuthor(index: Integer): WideString; dispid 5;
    function ReplyDate(index: Integer): WideString; dispid 6;
    function ReplySubject(index: Integer): WideString; dispid 7;
    function ReplyAuthorDesc(index: Integer): WideString; dispid 8;
    function ReplySummary(index: Integer): WideString; dispid 9;
    function Importance: Integer; dispid 10;
    function AuthorDesc: WideString; dispid 11;
    function Open(const path: WideString): Integer; dispid 12;
    function OpenReply(index: Integer): Integer; dispid 13;
    function ReplyPath(index: Integer): WideString; dispid 14;
    function NewMessage(const Parent, Author, AuthorD, Date, Subject, Body: WideString): Integer; dispid 16;
    function Subject: WideString; dispid 15;
    function FolderOnly: WordBool; dispid 17;
    function BodyHTML: WideString; dispid 18;
    function GlobalMsgCount: Integer; dispid 19;
    function GlobalMsgPath(index: Integer): WideString; dispid 20;
    procedure SetRootPath(const RootPath: WideString); dispid 21;
    procedure SetIndexSize(size: Integer); dispid 22;
    function ParentPath: WideString; dispid 23;
    function Summary: WideString; dispid 24;
    procedure SetLangId(const aLangId: WideString); dispid 25;
  end;

{ NewsObjectObject }

  CoNewsObject = class
    class function Create: INewsObject;
    class function CreateRemote(const MachineName: string): INewsObject;
  end;



implementation

uses ComObj;

class function CoNewsObject.Create: INewsObject;
begin
  Result := CreateComObject(Class_NewsObject) as INewsObject;
end;

class function CoNewsObject.CreateRemote(const MachineName: string): INewsObject;
begin
  Result := CreateRemoteComObject(MachineName, Class_NewsObject) as INewsObject;
end;


end.
