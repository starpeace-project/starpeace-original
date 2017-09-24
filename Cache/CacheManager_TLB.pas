unit CacheManager_TLB;

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
// File generated on 3/14/2003 2:58:24 PM from Type Library described below.

// ************************************************************************ //
// Type Lib: S:\source\Cache\CacheManager.tlb (1)
// IID\LCID: {18FC3C40-6450-11D1-A1A8-D5FDEAAE6A6F}\0
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
  CacheManagerMajorVersion = 1;
  CacheManagerMinorVersion = 0;

  LIBID_CacheManager: TGUID = '{18FC3C40-6450-11D1-A1A8-D5FDEAAE6A6F}';

  IID_ICachedObject: TGUID = '{18FC3C41-6450-11D1-A1A8-D5FDEAAE6A6F}';
  CLASS_CachedObject: TGUID = '{18FC3C42-6450-11D1-A1A8-D5FDEAAE6A6F}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  ICachedObject = interface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  CachedObject = ICachedObject;


// *********************************************************************//
// Interface: ICachedObject
// Flags:     (4368) Hidden OleAutomation Dispatchable
// GUID:      {18FC3C41-6450-11D1-A1A8-D5FDEAAE6A6F}
// *********************************************************************//
  ICachedObject = interface(IDispatch)
    ['{18FC3C41-6450-11D1-A1A8-D5FDEAAE6A6F}']
    function  SetPath(const aPath: WideString): WordBool; safecall;
    function  GetPath: WideString; safecall;
    function  SetWorld(const Name: WideString): WordBool; safecall;
    function  SetClass(const Name: WideString): WordBool; safecall;
    function  SetObject(X: Integer; Y: Integer): WordBool; safecall;
    function  SetObjectOfWorld(X: Integer; Y: Integer; const World: WideString): WordBool; safecall;
    function  GetFolderIterator(const Folder: WideString): OleVariant; safecall;
    function  ContainsFolder(const Name: WideString): WordBool; safecall;
    function  Get_Recache: WordBool; safecall;
    procedure Set_Recache(Value: WordBool); safecall;
    function  Properties(const Name: WideString): WideString; safecall;
    function  Get_Path: WideString; safecall;
    procedure Set_Path(const Value: WideString); safecall;
    function  Get_ErrorCode: Integer; safecall;
    procedure ActivateDictionary(Activate: WordBool); safecall;
    property Recache: WordBool read Get_Recache write Set_Recache;
    property Path: WideString read Get_Path write Set_Path;
    property ErrorCode: Integer read Get_ErrorCode;
  end;

// *********************************************************************//
// The Class CoCachedObject provides a Create and CreateRemote method to          
// create instances of the default interface ICachedObject exposed by              
// the CoClass CachedObject. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCachedObject = class
    class function Create: ICachedObject;
    class function CreateRemote(const MachineName: string): ICachedObject;
  end;

implementation

uses ComObj;

class function CoCachedObject.Create: ICachedObject;
begin
  Result := CreateComObject(CLASS_CachedObject) as ICachedObject;
end;

class function CoCachedObject.CreateRemote(const MachineName: string): ICachedObject;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CachedObject) as ICachedObject;
end;

end.
