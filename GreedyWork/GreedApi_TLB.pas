unit GreedApi_TLB;

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
// File generated on 1/18/2002 11:27:05 AM from Type Library described below.

// ************************************************************************ //
// Type Lib: S:\Source\GreedyWork\GreedApi.tlb (1)
// IID\LCID: {60739967-E9A7-11D5-9930-004854664ED7}\0
// Helpfile: 
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINNT\System32\stdole2.tlb)
//   (2) v4.0 StdVCL, (C:\WINNT\System32\STDVCL40.DLL)
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
  GreedApiMajorVersion = 1;
  GreedApiMinorVersion = 0;

  LIBID_GreedApi: TGUID = '{60739967-E9A7-11D5-9930-004854664ED7}';

  IID_IOTK: TGUID = '{60739968-E9A7-11D5-9930-004854664ED7}';
  CLASS_OTK: TGUID = '{6073996A-E9A7-11D5-9930-004854664ED7}';
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
// GUID:      {60739968-E9A7-11D5-9930-004854664ED7}
// *********************************************************************//
  IOTK = interface(IDispatch)
    ['{60739968-E9A7-11D5-9930-004854664ED7}']
    function  Cypher(const text: WideString): WideString; safecall;
    function  Get_ErrorCode: Integer; safecall;
    property ErrorCode: Integer read Get_ErrorCode;
  end;

// *********************************************************************//
// DispIntf:  IOTKDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {60739968-E9A7-11D5-9930-004854664ED7}
// *********************************************************************//
  IOTKDisp = dispinterface
    ['{60739968-E9A7-11D5-9930-004854664ED7}']
    function  Cypher(const text: WideString): WideString; dispid 2;
    property ErrorCode: Integer readonly dispid 1;
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
