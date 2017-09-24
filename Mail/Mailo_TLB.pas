unit Mailo_TLB;

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
// File generated on 11/12/2002 1:37:13 PM from Type Library described below.

// ************************************************************************ //
// Type Lib: C:\Work\Five\Source\Mail\Mailo.tlb (1)
// IID\LCID: {81B826D9-07F6-4868-A178-1EBB5F7D31E3}\0
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
  MailoMajorVersion = 1;
  MailoMinorVersion = 0;

  LIBID_Mailo: TGUID = '{81B826D9-07F6-4868-A178-1EBB5F7D31E3}';

  IID_IMuyMilo: TGUID = '{B448B68A-94B6-4A6D-8E0E-9999914377B8}';
  CLASS_MuyMilo: TGUID = '{0EB6C016-DE7D-42CF-B283-69D39897122E}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IMuyMilo = interface;
  IMuyMiloDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  MuyMilo = IMuyMilo;


// *********************************************************************//
// Interface: IMuyMilo
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B448B68A-94B6-4A6D-8E0E-9999914377B8}
// *********************************************************************//
  IMuyMilo = interface(IDispatch)
    ['{B448B68A-94B6-4A6D-8E0E-9999914377B8}']
    function  Get_Name: WideString; safecall;
    property Name: WideString read Get_Name;
  end;

// *********************************************************************//
// DispIntf:  IMuyMiloDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B448B68A-94B6-4A6D-8E0E-9999914377B8}
// *********************************************************************//
  IMuyMiloDisp = dispinterface
    ['{B448B68A-94B6-4A6D-8E0E-9999914377B8}']
    property Name: WideString readonly dispid 1;
  end;

// *********************************************************************//
// The Class CoMuyMilo provides a Create and CreateRemote method to          
// create instances of the default interface IMuyMilo exposed by              
// the CoClass MuyMilo. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoMuyMilo = class
    class function Create: IMuyMilo;
    class function CreateRemote(const MachineName: string): IMuyMilo;
  end;

implementation

uses ComObj;

class function CoMuyMilo.Create: IMuyMilo;
begin
  Result := CreateComObject(CLASS_MuyMilo) as IMuyMilo;
end;

class function CoMuyMilo.CreateRemote(const MachineName: string): IMuyMilo;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_MuyMilo) as IMuyMilo;
end;

end.
