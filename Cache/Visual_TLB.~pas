unit Visual_TLB;

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
// File generated on 11/11/2002 11:59:16 AM from Type Library described below.

// ************************************************************************ //
// Type Lib: C:\Work\Five\Source\Cache\Visual.tlb (1)
// IID\LCID: {1B08E260-9DBA-11D1-A1A8-0080C817C099}\0
// Helpfile: 
// DepndLst: 
//   (1) v1.0 stdole, (C:\WINNT\System32\stdole32.tlb)
//   (2) v1.0 StdVCL, (C:\WINNT\System32\STDVCL32.DLL)
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
  VisualMajorVersion = 1;
  VisualMinorVersion = 0;

  LIBID_Visual: TGUID = '{1B08E260-9DBA-11D1-A1A8-0080C817C099}';

  IID_IVisualClass: TGUID = '{1B08E261-9DBA-11D1-A1A8-0080C817C099}';
  CLASS_VisualClass: TGUID = '{1B08E262-9DBA-11D1-A1A8-0080C817C099}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IVisualClass = interface;
  IVisualClassDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  VisualClass = IVisualClass;


// *********************************************************************//
// Interface: IVisualClass
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {1B08E261-9DBA-11D1-A1A8-0080C817C099}
// *********************************************************************//
  IVisualClass = interface(IDispatch)
    ['{1B08E261-9DBA-11D1-A1A8-0080C817C099}']
    function  Open(ClassId: Integer): WordBool; safecall;
    function  ReadString(const Section: WideString; const Name: WideString; 
                         const DefValue: WideString): WideString; safecall;
    function  ReadInteger(const Section: WideString; const Name: WideString; DefValue: Integer): Integer; safecall;
    function  ReadBool(const Section: WideString; const Name: WideString; DefValue: WordBool): WordBool; safecall;
    function  Get_Masked: WordBool; safecall;
    procedure Set_Masked(Value: WordBool); safecall;
    function  Get_UserContext: WideString; safecall;
    procedure RefreshClasses; safecall;
    function  Get_ErrorCode: Integer; safecall;
    function  Get_FilePath: WideString; safecall;
    property Masked: WordBool read Get_Masked write Set_Masked;
    property UserContext: WideString read Get_UserContext;
    property ErrorCode: Integer read Get_ErrorCode;
    property FilePath: WideString read Get_FilePath;
  end;

// *********************************************************************//
// DispIntf:  IVisualClassDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {1B08E261-9DBA-11D1-A1A8-0080C817C099}
// *********************************************************************//
  IVisualClassDisp = dispinterface
    ['{1B08E261-9DBA-11D1-A1A8-0080C817C099}']
    function  Open(ClassId: Integer): WordBool; dispid 3;
    function  ReadString(const Section: WideString; const Name: WideString; 
                         const DefValue: WideString): WideString; dispid 1;
    function  ReadInteger(const Section: WideString; const Name: WideString; DefValue: Integer): Integer; dispid 2;
    function  ReadBool(const Section: WideString; const Name: WideString; DefValue: WordBool): WordBool; dispid 4;
    property Masked: WordBool dispid 5;
    property UserContext: WideString readonly dispid 7;
    procedure RefreshClasses; dispid 6;
    property ErrorCode: Integer readonly dispid 8;
    property FilePath: WideString readonly dispid 9;
  end;

// *********************************************************************//
// The Class CoVisualClass provides a Create and CreateRemote method to          
// create instances of the default interface IVisualClass exposed by              
// the CoClass VisualClass. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoVisualClass = class
    class function Create: IVisualClass;
    class function CreateRemote(const MachineName: string): IVisualClass;
  end;

implementation

uses ComObj;

class function CoVisualClass.Create: IVisualClass;
begin
  Result := CreateComObject(CLASS_VisualClass) as IVisualClass;
end;

class function CoVisualClass.CreateRemote(const MachineName: string): IVisualClass;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_VisualClass) as IVisualClass;
end;

end.
