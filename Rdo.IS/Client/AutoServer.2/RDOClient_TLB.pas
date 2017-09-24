unit RDOClient_TLB;

{ This file contains pascal declarations imported from a type library.
  This file will be written during each import or refresh of the type
  library editor.  Changes to this file will be discarded during the
  refresh process. }

{ RDOClient Library }
{ Version 1.0 }

interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, StdVCL;

const
  LIBID_RDOClient: TGUID = '{58C4CB00-873A-11D1-AF26-008029E5CA8C}';

const

{ Component class GUIDs }
  Class_WinSockRDOConnection: TGUID = '{58C4CB02-873A-11D1-AF26-008029E5CA8C}';
  Class_RDOObjectProxy: TGUID = '{DBA984A1-8C05-11D1-AF26-008029E5CA8C}';

type

{ Forward declarations: Interfaces }
  IRDOConnectionInit = interface;
  IRDOConnectionInitDisp = dispinterface;
  IRDOObjectProxy = interface;
  IRDOObjectProxyDisp = dispinterface;

{ Forward declarations: CoClasses }
  WinSockRDOConnection = IRDOConnectionInit;
  RDOObjectProxy = IRDOObjectProxy;

{ Dispatch interface for WinSockRDOConnection Object }

  IRDOConnectionInit = interface(IDispatch)
    ['{58C4CB01-873A-11D1-AF26-008029E5CA8C}']
    function Get_Server: WideString; safecall;
    procedure Set_Server(const Value: WideString); safecall;
    function Get_Port: Integer; safecall;
    procedure Set_Port(Value: Integer); safecall;
    function Connect(TimeOut: Integer): WordBool; safecall;
    procedure Disconnect; safecall;
    property Server: WideString read Get_Server write Set_Server;
    property Port: Integer read Get_Port write Set_Port;
  end;

{ DispInterface declaration for Dual Interface IRDOConnectionInit }

  IRDOConnectionInitDisp = dispinterface
    ['{58C4CB01-873A-11D1-AF26-008029E5CA8C}']
    property Server: WideString dispid 1;
    property Port: Integer dispid 2;
    function Connect(TimeOut: Integer): WordBool; dispid 3;
    procedure Disconnect; dispid 4;
  end;

{ Dispatch interface for RDOObjectProxy Object }

  IRDOObjectProxy = interface(IDispatch)
    ['{DBA984A0-8C05-11D1-AF26-008029E5CA8C}']
  end;

{ DispInterface declaration for Dual Interface IRDOObjectProxy }

  IRDOObjectProxyDisp = dispinterface
    ['{DBA984A0-8C05-11D1-AF26-008029E5CA8C}']
  end;

{ WinSock RDO Connection Object }

  CoWinSockRDOConnection = class
    class function Create: IRDOConnectionInit;
    class function CreateRemote(const MachineName: string): IRDOConnectionInit;
  end;

{ RDO ObjectProxy Object }

  CoRDOObjectProxy = class
    class function Create: IRDOObjectProxy;
    class function CreateRemote(const MachineName: string): IRDOObjectProxy;
  end;



implementation

uses ComObj;

class function CoWinSockRDOConnection.Create: IRDOConnectionInit;
begin
  Result := CreateComObject(Class_WinSockRDOConnection) as IRDOConnectionInit;
end;

class function CoWinSockRDOConnection.CreateRemote(const MachineName: string): IRDOConnectionInit;
begin
  Result := CreateRemoteComObject(MachineName, Class_WinSockRDOConnection) as IRDOConnectionInit;
end;

class function CoRDOObjectProxy.Create: IRDOObjectProxy;
begin
  Result := CreateComObject(Class_RDOObjectProxy) as IRDOObjectProxy;
end;

class function CoRDOObjectProxy.CreateRemote(const MachineName: string): IRDOObjectProxy;
begin
  Result := CreateRemoteComObject(MachineName, Class_RDOObjectProxy) as IRDOObjectProxy;
end;


end.
