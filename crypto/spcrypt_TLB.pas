unit spcrypt_TLB;

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
// File generated on 11/28/01 5:13:04 AM from Type Library described below.

// ************************************************************************ //
// Type Lib: S:\Source\crypto\spcrypt.tlb (1)
// IID\LCID: {3EA7E981-E399-11D5-992E-004854664ED7}\0
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
  spcryptMajorVersion = 1;
  spcryptMinorVersion = 0;

  LIBID_spcrypt: TGUID = '{3EA7E981-E399-11D5-992E-004854664ED7}';

  IID_IRC4: TGUID = '{3EA7E982-E399-11D5-992E-004854664ED7}';
  CLASS_RC4: TGUID = '{3EA7E984-E399-11D5-992E-004854664ED7}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IRC4 = interface;
  IRC4Disp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  RC4 = IRC4;


// *********************************************************************//
// Interface: IRC4
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3EA7E982-E399-11D5-992E-004854664ED7}
// *********************************************************************//
  IRC4 = interface(IDispatch)
    ['{3EA7E982-E399-11D5-992E-004854664ED7}']
    procedure Apply(const text: WideString; inHex: Integer; outHex: Integer); safecall;
    function  Get_Key: WideString; safecall;
    procedure Set_Key(const Value: WideString); safecall;
    function  Get_Result: WideString; safecall;
    property Key: WideString read Get_Key write Set_Key;
    property Result: WideString read Get_Result;
  end;

// *********************************************************************//
// DispIntf:  IRC4Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3EA7E982-E399-11D5-992E-004854664ED7}
// *********************************************************************//
  IRC4Disp = dispinterface
    ['{3EA7E982-E399-11D5-992E-004854664ED7}']
    procedure Apply(const text: WideString; inHex: Integer; outHex: Integer); dispid 3;
    property Key: WideString dispid 4;
    property Result: WideString readonly dispid 1;
  end;

// *********************************************************************//
// The Class CoRC4 provides a Create and CreateRemote method to          
// create instances of the default interface IRC4 exposed by              
// the CoClass RC4. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRC4 = class
    class function Create: IRC4;
    class function CreateRemote(const MachineName: string): IRC4;
  end;

implementation

uses ComObj;

class function CoRC4.Create: IRC4;
begin
  Result := CreateComObject(CLASS_RC4) as IRC4;
end;

class function CoRC4.CreateRemote(const MachineName: string): IRC4;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RC4) as IRC4;
end;

end.
