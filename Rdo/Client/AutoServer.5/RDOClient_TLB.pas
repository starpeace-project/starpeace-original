unit RDOClient_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : $Revision:   1.88  $
// File generated on 11/25/00 12:33:19 PM from Type Library described below.

// *************************************************************************//
// NOTE:                                                                      
// Items guarded by $IFDEF_LIVE_SERVER_AT_DESIGN_TIME are used by properties  
// which return objects that may need to be explicitly created via a function 
// call prior to any access via the property. These items have been disabled  
// in order to prevent accidental use from within the object inspector. You   
// may enable them by defining LIVE_SERVER_AT_DESIGN_TIME or by selectively   
// removing them from the $IFDEF blocks. However, such items must still be    
// programmatically created via a method of the appropriate CoClass before    
// they can be used.                                                          
// ************************************************************************ //
// Type Lib: E:\Work\Five\Source\Rdo\Client\AutoServer.5\RDOClient.tlb (1)
// IID\LCID: {58C4CB00-873A-11D1-AF26-008029E5CA8C}\0
// Helpfile: 
// DepndLst: 
//   (1) v1.0 stdole, (D:\WINNT\System32\stdole32.tlb)
//   (2) v1.0 StdVCL, (D:\WINNT\System32\STDVCL32.DLL)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, OleCtrls, StdVCL;

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  RDOClientMajorVersion = 1;
  RDOClientMinorVersion = 0;

  LIBID_RDOClient: TGUID = '{58C4CB00-873A-11D1-AF26-008029E5CA8C}';

  IID_IRDOConnectionInit: TGUID = '{58C4CB01-873A-11D1-AF26-008029E5CA8C}';
  CLASS_WinSockRDOConnection: TGUID = '{58C4CB02-873A-11D1-AF26-008029E5CA8C}';
  IID_IRDOObjectProxy: TGUID = '{DBA984A0-8C05-11D1-AF26-008029E5CA8C}';
  CLASS_RDOObjectProxy: TGUID = '{DBA984A1-8C05-11D1-AF26-008029E5CA8C}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IRDOConnectionInit = interface;
  IRDOConnectionInitDisp = dispinterface;
  IRDOObjectProxy = interface;
  IRDOObjectProxyDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  WinSockRDOConnection = IRDOConnectionInit;
  RDOObjectProxy = IRDOObjectProxy;


// *********************************************************************//
// Interface: IRDOConnectionInit
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {58C4CB01-873A-11D1-AF26-008029E5CA8C}
// *********************************************************************//
  IRDOConnectionInit = interface(IDispatch)
    ['{58C4CB01-873A-11D1-AF26-008029E5CA8C}']
    function  Get_Server: WideString; safecall;
    procedure Set_Server(const Value: WideString); safecall;
    function  Get_Port: Integer; safecall;
    procedure Set_Port(Value: Integer); safecall;
    function  Connect(TimeOut: Integer): WordBool; safecall;
    procedure Disconnect; safecall;
    property Server: WideString read Get_Server write Set_Server;
    property Port: Integer read Get_Port write Set_Port;
  end;

// *********************************************************************//
// DispIntf:  IRDOConnectionInitDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {58C4CB01-873A-11D1-AF26-008029E5CA8C}
// *********************************************************************//
  IRDOConnectionInitDisp = dispinterface
    ['{58C4CB01-873A-11D1-AF26-008029E5CA8C}']
    property Server: WideString dispid 1;
    property Port: Integer dispid 2;
    function  Connect(TimeOut: Integer): WordBool; dispid 3;
    procedure Disconnect; dispid 4;
  end;

// *********************************************************************//
// Interface: IRDOObjectProxy
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {DBA984A0-8C05-11D1-AF26-008029E5CA8C}
// *********************************************************************//
  IRDOObjectProxy = interface(IDispatch)
    ['{DBA984A0-8C05-11D1-AF26-008029E5CA8C}']
  end;

// *********************************************************************//
// DispIntf:  IRDOObjectProxyDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {DBA984A0-8C05-11D1-AF26-008029E5CA8C}
// *********************************************************************//
  IRDOObjectProxyDisp = dispinterface
    ['{DBA984A0-8C05-11D1-AF26-008029E5CA8C}']
  end;

// *********************************************************************//
// The Class CoWinSockRDOConnection provides a Create and CreateRemote method to          
// create instances of the default interface IRDOConnectionInit exposed by              
// the CoClass WinSockRDOConnection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoWinSockRDOConnection = class
    class function Create: IRDOConnectionInit;
    class function CreateRemote(const MachineName: string): IRDOConnectionInit;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TWinSockRDOConnection
// Help String      : WinSock RDO Connection Object
// Default Interface: IRDOConnectionInit
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TWinSockRDOConnectionProperties= class;
{$ENDIF}
  TWinSockRDOConnection = class(TOleServer)
  private
    FIntf:        IRDOConnectionInit;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TWinSockRDOConnectionProperties;
    function      GetServerProperties: TWinSockRDOConnectionProperties;
{$ENDIF}
    function      GetDefaultInterface: IRDOConnectionInit;
  protected
    procedure InitServerData; override;
    function  Get_Server: WideString;
    procedure Set_Server(const Value: WideString);
    function  Get_Port: Integer;
    procedure Set_Port(Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IRDOConnectionInit);
    procedure Disconnect; override;
    function  Connect1(TimeOut: Integer): WordBool;
    procedure Disconnect1;
    property  DefaultInterface: IRDOConnectionInit read GetDefaultInterface;
    property Server: WideString read Get_Server write Set_Server;
    property Port: Integer read Get_Port write Set_Port;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TWinSockRDOConnectionProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TWinSockRDOConnection
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TWinSockRDOConnectionProperties = class(TPersistent)
  private
    FServer:    TWinSockRDOConnection;
    function    GetDefaultInterface: IRDOConnectionInit;
    constructor Create(AServer: TWinSockRDOConnection);
  protected
    function  Get_Server: WideString;
    procedure Set_Server(const Value: WideString);
    function  Get_Port: Integer;
    procedure Set_Port(Value: Integer);
  public
    property DefaultInterface: IRDOConnectionInit read GetDefaultInterface;
  published
    property Server: WideString read Get_Server write Set_Server;
    property Port: Integer read Get_Port write Set_Port;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoRDOObjectProxy provides a Create and CreateRemote method to          
// create instances of the default interface IRDOObjectProxy exposed by              
// the CoClass RDOObjectProxy. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRDOObjectProxy = class
    class function Create: IRDOObjectProxy;
    class function CreateRemote(const MachineName: string): IRDOObjectProxy;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TRDOObjectProxy
// Help String      : RDO ObjectProxy Object
// Default Interface: IRDOObjectProxy
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TRDOObjectProxyProperties= class;
{$ENDIF}
  TRDOObjectProxy = class(TOleServer)
  private
    FIntf:        IRDOObjectProxy;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TRDOObjectProxyProperties;
    function      GetServerProperties: TRDOObjectProxyProperties;
{$ENDIF}
    function      GetDefaultInterface: IRDOObjectProxy;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IRDOObjectProxy);
    procedure Disconnect; override;
    property  DefaultInterface: IRDOObjectProxy read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TRDOObjectProxyProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TRDOObjectProxy
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TRDOObjectProxyProperties = class(TPersistent)
  private
    FServer:    TRDOObjectProxy;
    function    GetDefaultInterface: IRDOObjectProxy;
    constructor Create(AServer: TRDOObjectProxy);
  protected
  public
    property DefaultInterface: IRDOObjectProxy read GetDefaultInterface;
  published
  end;
{$ENDIF}


procedure Register;

implementation

uses ComObj;

class function CoWinSockRDOConnection.Create: IRDOConnectionInit;
begin
  Result := CreateComObject(CLASS_WinSockRDOConnection) as IRDOConnectionInit;
end;

class function CoWinSockRDOConnection.CreateRemote(const MachineName: string): IRDOConnectionInit;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_WinSockRDOConnection) as IRDOConnectionInit;
end;

procedure TWinSockRDOConnection.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{58C4CB02-873A-11D1-AF26-008029E5CA8C}';
    IntfIID:   '{58C4CB01-873A-11D1-AF26-008029E5CA8C}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TWinSockRDOConnection.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IRDOConnectionInit;
  end;
end;

procedure TWinSockRDOConnection.ConnectTo(svrIntf: IRDOConnectionInit);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TWinSockRDOConnection.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TWinSockRDOConnection.GetDefaultInterface: IRDOConnectionInit;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TWinSockRDOConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TWinSockRDOConnectionProperties.Create(Self);
{$ENDIF}
end;

destructor TWinSockRDOConnection.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TWinSockRDOConnection.GetServerProperties: TWinSockRDOConnectionProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function  TWinSockRDOConnection.Get_Server: WideString;
begin
  Result := DefaultInterface.Get_Server;
end;

procedure TWinSockRDOConnection.Set_Server(const Value: WideString);
begin
  DefaultInterface.Set_Server(Value);
end;

function  TWinSockRDOConnection.Get_Port: Integer;
begin
  Result := DefaultInterface.Get_Port;
end;

procedure TWinSockRDOConnection.Set_Port(Value: Integer);
begin
  DefaultInterface.Set_Port(Value);
end;

function  TWinSockRDOConnection.Connect1(TimeOut: Integer): WordBool;
begin
  Result := DefaultInterface.Connect(TimeOut);
end;

procedure TWinSockRDOConnection.Disconnect1;
begin
  DefaultInterface.Disconnect;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TWinSockRDOConnectionProperties.Create(AServer: TWinSockRDOConnection);
begin
  inherited Create;
  FServer := AServer;
end;

function TWinSockRDOConnectionProperties.GetDefaultInterface: IRDOConnectionInit;
begin
  Result := FServer.DefaultInterface;
end;

function  TWinSockRDOConnectionProperties.Get_Server: WideString;
begin
  Result := DefaultInterface.Get_Server;
end;

procedure TWinSockRDOConnectionProperties.Set_Server(const Value: WideString);
begin
  DefaultInterface.Set_Server(Value);
end;

function  TWinSockRDOConnectionProperties.Get_Port: Integer;
begin
  Result := DefaultInterface.Get_Port;
end;

procedure TWinSockRDOConnectionProperties.Set_Port(Value: Integer);
begin
  DefaultInterface.Set_Port(Value);
end;

{$ENDIF}

class function CoRDOObjectProxy.Create: IRDOObjectProxy;
begin
  Result := CreateComObject(CLASS_RDOObjectProxy) as IRDOObjectProxy;
end;

class function CoRDOObjectProxy.CreateRemote(const MachineName: string): IRDOObjectProxy;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RDOObjectProxy) as IRDOObjectProxy;
end;

procedure TRDOObjectProxy.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{DBA984A1-8C05-11D1-AF26-008029E5CA8C}';
    IntfIID:   '{DBA984A0-8C05-11D1-AF26-008029E5CA8C}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TRDOObjectProxy.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IRDOObjectProxy;
  end;
end;

procedure TRDOObjectProxy.ConnectTo(svrIntf: IRDOObjectProxy);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TRDOObjectProxy.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TRDOObjectProxy.GetDefaultInterface: IRDOObjectProxy;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TRDOObjectProxy.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TRDOObjectProxyProperties.Create(Self);
{$ENDIF}
end;

destructor TRDOObjectProxy.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TRDOObjectProxy.GetServerProperties: TRDOObjectProxyProperties;
begin
  Result := FProps;
end;
{$ENDIF}

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TRDOObjectProxyProperties.Create(AServer: TRDOObjectProxy);
begin
  inherited Create;
  FServer := AServer;
end;

function TRDOObjectProxyProperties.GetDefaultInterface: IRDOObjectProxy;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

procedure Register;
begin
  RegisterComponents('Servers',[TWinSockRDOConnection, TRDOObjectProxy]);
end;

end.
