unit remfile_TLB;

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
// File generated on 3/27/01 9:03:33 AM from Type Library described below.

// ************************************************************************ //
// Type Lib: E:\Work\Five\Source\Tests\remfile\remfile.tlb (1)
// IID\LCID: {6BEE9B91-22B8-11D5-98EA-004854664ED7}\0
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
  remfileMajorVersion = 1;
  remfileMinorVersion = 0;

  LIBID_remfile: TGUID = '{6BEE9B91-22B8-11D5-98EA-004854664ED7}';

  IID_IFS: TGUID = '{6BEE9B92-22B8-11D5-98EA-004854664ED7}';
  CLASS_FS: TGUID = '{6BEE9B94-22B8-11D5-98EA-004854664ED7}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IFS = interface;
  IFSDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  FS = IFS;


// *********************************************************************//
// Interface: IFS
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6BEE9B92-22B8-11D5-98EA-004854664ED7}
// *********************************************************************//
  IFS = interface(IDispatch)
    ['{6BEE9B92-22B8-11D5-98EA-004854664ED7}']
    procedure Remove(const path: WideString); safecall;
    function  Get_LastError: Integer; safecall;
    property LastError: Integer read Get_LastError;
  end;

// *********************************************************************//
// DispIntf:  IFSDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6BEE9B92-22B8-11D5-98EA-004854664ED7}
// *********************************************************************//
  IFSDisp = dispinterface
    ['{6BEE9B92-22B8-11D5-98EA-004854664ED7}']
    procedure Remove(const path: WideString); dispid 1;
    property LastError: Integer readonly dispid 4;
  end;

// *********************************************************************//
// The Class CoFS provides a Create and CreateRemote method to          
// create instances of the default interface IFS exposed by              
// the CoClass FS. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoFS = class
    class function Create: IFS;
    class function CreateRemote(const MachineName: string): IFS;
  end;

implementation

uses ComObj;

class function CoFS.Create: IFS;
begin
  Result := CreateComObject(CLASS_FS) as IFS;
end;

class function CoFS.CreateRemote(const MachineName: string): IFS;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_FS) as IFS;
end;

end.
