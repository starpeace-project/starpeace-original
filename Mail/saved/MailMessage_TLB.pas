unit MailMessage_TLB;

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
// File generated on 11/12/2002 12:03:16 PM from Type Library described below.

// ************************************************************************ //
// Type Lib: C:\Work\Five\Source\Mail\MailMessage.tlb (1)
// IID\LCID: {1A505046-B441-11D1-A1A8-0080C817C099}\0
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
  MailMessageMajorVersion = 1;
  MailMessageMinorVersion = 0;

  LIBID_MailMessage: TGUID = '{1A505046-B441-11D1-A1A8-0080C817C099}';

  IID_IFiveMessage: TGUID = '{1A505047-B441-11D1-A1A8-0080C817C099}';
  CLASS_FiveMessage: TGUID = '{1A505048-B441-11D1-A1A8-0080C817C099}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IFiveMessage = interface;
  IFiveMessageDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  FiveMessage = IFiveMessage;


// *********************************************************************//
// Interface: IFiveMessage
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {1A505047-B441-11D1-A1A8-0080C817C099}
// *********************************************************************//
  IFiveMessage = interface(IDispatch)
    ['{1A505047-B441-11D1-A1A8-0080C817C099}']
    function  Get_AttachmentCount: Integer; safecall;
    function  Get_CurrentAttachment: Integer; safecall;
    procedure Set_CurrentAttachment(Value: Integer); safecall;
    function  Get_Attachment(const prop: WideString): WideString; safecall;
    function  Get_Header(const Name: WideString): WideString; safecall;
    function  Get_Lines(index: Integer): WideString; safecall;
    function  SetMessage(const aWorld: WideString; const aAccount: WideString; 
                         const aFolder: WideString; const aMessage: WideString): WordBool; safecall;
    function  Delete: WordBool; safecall;
    function  Get_LineCount: Integer; safecall;
    property AttachmentCount: Integer read Get_AttachmentCount;
    property CurrentAttachment: Integer read Get_CurrentAttachment write Set_CurrentAttachment;
    property Attachment[const prop: WideString]: WideString read Get_Attachment;
    property Header[const Name: WideString]: WideString read Get_Header;
    property Lines[index: Integer]: WideString read Get_Lines;
    property LineCount: Integer read Get_LineCount;
  end;

// *********************************************************************//
// DispIntf:  IFiveMessageDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {1A505047-B441-11D1-A1A8-0080C817C099}
// *********************************************************************//
  IFiveMessageDisp = dispinterface
    ['{1A505047-B441-11D1-A1A8-0080C817C099}']
    property AttachmentCount: Integer readonly dispid 7;
    property CurrentAttachment: Integer dispid 8;
    property Attachment[const prop: WideString]: WideString readonly dispid 9;
    property Header[const Name: WideString]: WideString readonly dispid 1;
    property Lines[index: Integer]: WideString readonly dispid 2;
    function  SetMessage(const aWorld: WideString; const aAccount: WideString; 
                         const aFolder: WideString; const aMessage: WideString): WordBool; dispid 3;
    function  Delete: WordBool; dispid 4;
    property LineCount: Integer readonly dispid 5;
  end;

// *********************************************************************//
// The Class CoFiveMessage provides a Create and CreateRemote method to          
// create instances of the default interface IFiveMessage exposed by              
// the CoClass FiveMessage. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoFiveMessage = class
    class function Create: IFiveMessage;
    class function CreateRemote(const MachineName: string): IFiveMessage;
  end;

implementation

uses ComObj;

class function CoFiveMessage.Create: IFiveMessage;
begin
  Result := CreateComObject(CLASS_FiveMessage) as IFiveMessage;
end;

class function CoFiveMessage.CreateRemote(const MachineName: string): IFiveMessage;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_FiveMessage) as IFiveMessage;
end;

end.
