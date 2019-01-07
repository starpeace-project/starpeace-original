unit FolderIterator_TLB;

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

// PASTLWTR : 1.2
// File generated on 07/01/2019 01:59:33 from Type Library described below.

// ************************************************************************  //
// Type Lib: \\vmware-host\Shared Folders\Documents\GitHub\starpeace-original\Cache\FolderIterator.tlb (1)
// LIBID: {86F691A3-66A8-11D1-A1A8-F8C645A0F340}
// LCID: 0
// Helpfile: 
// HelpString: FolderIterator Library
// DepndLst: 
//   (1) v1.0 stdole, (C:\Windows\System32\stdole32.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  FolderIteratorMajorVersion = 1;
  FolderIteratorMinorVersion = 0;

  LIBID_FolderIterator: TGUID = '{86F691A3-66A8-11D1-A1A8-F8C645A0F340}';

  IID_IDirectoryIterator: TGUID = '{86F691A5-66A8-11D1-A1A8-F8C645A0F340}';
  CLASS_DirectoryIterator: TGUID = '{86F691A4-66A8-11D1-A1A8-F8C645A0F340}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IDirectoryIterator = interface;
  IDirectoryIteratorDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  DirectoryIterator = IDirectoryIterator;


// *********************************************************************//
// Interface: IDirectoryIterator
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {86F691A5-66A8-11D1-A1A8-F8C645A0F340}
// *********************************************************************//
  IDirectoryIterator = interface(IDispatch)
    ['{86F691A5-66A8-11D1-A1A8-F8C645A0F340}']
    procedure SetFolder(const aPath: WideString; const aWildcard: WideString; Options: Integer); safecall;
    procedure Reset; safecall;
    function Next: WordBool; safecall;
    procedure SetOptions(Options: Integer); safecall;
    function Empty: WordBool; safecall;
    function Get_Current: WideString; safecall;
    function Get_FullPath: WideString; safecall;
    property Current: WideString read Get_Current;
    property FullPath: WideString read Get_FullPath;
  end;

// *********************************************************************//
// DispIntf:  IDirectoryIteratorDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {86F691A5-66A8-11D1-A1A8-F8C645A0F340}
// *********************************************************************//
  IDirectoryIteratorDisp = dispinterface
    ['{86F691A5-66A8-11D1-A1A8-F8C645A0F340}']
    procedure SetFolder(const aPath: WideString; const aWildcard: WideString; Options: Integer); dispid 1;
    procedure Reset; dispid 2;
    function Next: WordBool; dispid 3;
    procedure SetOptions(Options: Integer); dispid 4;
    function Empty: WordBool; dispid 5;
    property Current: WideString readonly dispid 6;
    property FullPath: WideString readonly dispid 7;
  end;

// *********************************************************************//
// The Class CoDirectoryIterator provides a Create and CreateRemote method to          
// create instances of the default interface IDirectoryIterator exposed by              
// the CoClass DirectoryIterator. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoDirectoryIterator = class
    class function Create: IDirectoryIterator;
    class function CreateRemote(const MachineName: string): IDirectoryIterator;
  end;

implementation

uses ComObj;

class function CoDirectoryIterator.Create: IDirectoryIterator;
begin
  Result := CreateComObject(CLASS_DirectoryIterator) as IDirectoryIterator;
end;

class function CoDirectoryIterator.CreateRemote(const MachineName: string): IDirectoryIterator;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_DirectoryIterator) as IDirectoryIterator;
end;

end.
