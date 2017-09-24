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

// PASTLWTR : $Revision:   1.88.1.0.1.0  $
// File generated on 12/10/01 5:36:47 PM from Type Library described below.

// ************************************************************************ //
// Type Lib: S:\Source\Rdo\Client\AutoServer\RDOClient.tlb (1)
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

class function CoRDOObjectProxy.Create: IRDOObjectProxy;
begin
  Result := CreateComObject(CLASS_RDOObjectProxy) as IRDOObjectProxy;
end;

class function CoRDOObjectProxy.CreateRemote(const MachineName: string): IRDOObjectProxy;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RDOObjectProxy) as IRDOObjectProxy;
end;

end.
