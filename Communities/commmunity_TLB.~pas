unit commmunity_TLB;

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
// File generated on 4/5/2002 2:45:57 PM from Type Library described below.

// ************************************************************************ //
// Type Lib: S:\Source\Communities\community.tlb (1)
// IID\LCID: {09DD3985-2E0C-11D5-9DDA-00A0CC2C4AE6}\0
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
  commmunityMajorVersion = 1;
  commmunityMinorVersion = 0;

  LIBID_commmunity: TGUID = '{09DD3985-2E0C-11D5-9DDA-00A0CC2C4AE6}';

  IID_IBBoard: TGUID = '{09DD3986-2E0C-11D5-9DDA-00A0CC2C4AE6}';
  CLASS_BBoard: TGUID = '{09DD3988-2E0C-11D5-9DDA-00A0CC2C4AE6}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IBBoard = interface;
  IBBoardDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  BBoard = IBBoard;


// *********************************************************************//
// Interface: IBBoard
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {09DD3986-2E0C-11D5-9DDA-00A0CC2C4AE6}
// *********************************************************************//
  IBBoard = interface(IDispatch)
    ['{09DD3986-2E0C-11D5-9DDA-00A0CC2C4AE6}']
    procedure Open(const Id: WideString; IsForum: WordBool); safecall;
    function  Count: SYSINT; safecall;
    procedure NewItem(const Author: WideString; const Title: WideString; 
                      const ParentUId: WideString; const Body: WideString; Topic: WordBool); safecall;
    function  GetAuthor(index: Integer): WideString; safecall;
    function  GetTitle(index: Integer): WideString; safecall;
    function  GetId(index: Integer): WideString; safecall;
    function  GetBody(index: Integer): WideString; safecall;
    function  GetDate(index: Integer): TDateTime; safecall;
    function  GetHTMLBody(index: Integer): WideString; safecall;
    function  GetRepliesCount(index: Integer): SYSINT; safecall;
    procedure NewPoll(const Author: WideString; const Title: WideString; 
                      const ParentUId: WideString; const Body: WideString; 
                      const PollItems: WideString); safecall;
    function  PollItemsCount: SYSINT; safecall;
    function  GetPollItem(index: Integer): WideString; safecall;
    function  GetVotes(index: Integer): SYSINT; safecall;
    procedure AddVote(const Author: WideString; const ParentUId: WideString; 
                      const Body: WideString; PollItemIdx: Integer); safecall;
    function  VotesCount: SYSINT; safecall;
    function  GetAttachment(index: Integer): WideString; safecall;
    function  Get_ThreadFolder: WideString; safecall;
    procedure NewItemEx(const Author: WideString; const Title: WideString; 
                        const AttchName: WideString; const ParentUId: WideString; 
                        const Body: WideString; Topic: WordBool); safecall;
    function  Get_UploadDir: WideString; safecall;
    property ThreadFolder: WideString read Get_ThreadFolder;
    property UploadDir: WideString read Get_UploadDir;
  end;

// *********************************************************************//
// DispIntf:  IBBoardDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {09DD3986-2E0C-11D5-9DDA-00A0CC2C4AE6}
// *********************************************************************//
  IBBoardDisp = dispinterface
    ['{09DD3986-2E0C-11D5-9DDA-00A0CC2C4AE6}']
    procedure Open(const Id: WideString; IsForum: WordBool); dispid 1;
    function  Count: SYSINT; dispid 2;
    procedure NewItem(const Author: WideString; const Title: WideString; 
                      const ParentUId: WideString; const Body: WideString; Topic: WordBool); dispid 6;
    function  GetAuthor(index: Integer): WideString; dispid 8;
    function  GetTitle(index: Integer): WideString; dispid 9;
    function  GetId(index: Integer): WideString; dispid 10;
    function  GetBody(index: Integer): WideString; dispid 3;
    function  GetDate(index: Integer): TDateTime; dispid 4;
    function  GetHTMLBody(index: Integer): WideString; dispid 5;
    function  GetRepliesCount(index: Integer): SYSINT; dispid 7;
    procedure NewPoll(const Author: WideString; const Title: WideString; 
                      const ParentUId: WideString; const Body: WideString; 
                      const PollItems: WideString); dispid 12;
    function  PollItemsCount: SYSINT; dispid 13;
    function  GetPollItem(index: Integer): WideString; dispid 15;
    function  GetVotes(index: Integer): SYSINT; dispid 16;
    procedure AddVote(const Author: WideString; const ParentUId: WideString; 
                      const Body: WideString; PollItemIdx: Integer); dispid 17;
    function  VotesCount: SYSINT; dispid 11;
    function  GetAttachment(index: Integer): WideString; dispid 19;
    property ThreadFolder: WideString readonly dispid 20;
    procedure NewItemEx(const Author: WideString; const Title: WideString; 
                        const AttchName: WideString; const ParentUId: WideString; 
                        const Body: WideString; Topic: WordBool); dispid 14;
    property UploadDir: WideString readonly dispid 18;
  end;

// *********************************************************************//
// The Class CoBBoard provides a Create and CreateRemote method to          
// create instances of the default interface IBBoard exposed by              
// the CoClass BBoard. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoBBoard = class
    class function Create: IBBoard;
    class function CreateRemote(const MachineName: string): IBBoard;
  end;

implementation

uses ComObj;

class function CoBBoard.Create: IBBoard;
begin
  Result := CreateComObject(CLASS_BBoard) as IBBoard;
end;

class function CoBBoard.CreateRemote(const MachineName: string): IBBoard;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_BBoard) as IBBoard;
end;

end.
