unit Greed_TLB;

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
// File generated on 12/5/01 5:32:50 PM from Type Library described below.

// ************************************************************************ //
// Type Lib: S:\Source\crypto\Greed.tlb (1)
// IID\LCID: {825BB8D0-E9CF-11D5-9930-004854664ED7}\0
// Helpfile: 
// DepndLst: 
//   (1) v2.0 stdole, (D:\WINNT\System32\STDOLE2.TLB)
//   (2) v4.0 StdVCL, (D:\WINNT\System32\STDVCL40.DLL)
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
  GreedMajorVersion = 1;
  GreedMinorVersion = 0;

  LIBID_Greed: TGUID = '{825BB8D0-E9CF-11D5-9930-004854664ED7}';

  IID_IOTK: TGUID = '{825BB8D1-E9CF-11D5-9930-004854664ED7}';
  CLASS_OTK: TGUID = '{825BB8D3-E9CF-11D5-9930-004854664ED7}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IOTK = interface;
  IOTKDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  OTK = IOTK;


// *********************************************************************//
// Interface: IOTK
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {825BB8D1-E9CF-11D5-9930-004854664ED7}
// *********************************************************************//
  IOTK = interface(IDispatch)
    ['{825BB8D1-E9CF-11D5-9930-004854664ED7}']
    function  Connect(const addr: WideString; port: Integer): WordBool; safecall;
    function  Cypher(const text: WideString): WideString; safecall;
  end;

// *********************************************************************//
// DispIntf:  IOTKDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {825BB8D1-E9CF-11D5-9930-004854664ED7}
// *********************************************************************//
  IOTKDisp = dispinterface
    ['{825BB8D1-E9CF-11D5-9930-004854664ED7}']
    function  Connect(const addr: WideString; port: Integer): WordBool; dispid 1;
    function  Cypher(const text: WideString): WideString; dispid 2;
  end;

// *********************************************************************//
// The Class CoOTK provides a Create and CreateRemote method to          
// create instances of the default interface IOTK exposed by              
// the CoClass OTK. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoOTK = class
    class function Create: IOTK;
    class function CreateRemote(const MachineName: string): IOTK;
  end;

implementation

uses ComObj;

class function CoOTK.Create: IOTK;
begin
  Result := CreateComObject(CLASS_OTK) as IOTK;
end;

class function CoOTK.CreateRemote(const MachineName: string): IOTK;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_OTK) as IOTK;
end;

end.
