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

// PASTLWTR : $Revision:   1.88  $
// File generated on 9/15/00 2:26:21 PM from Type Library described below.

// *************************************************************************//
// NOTE:                                                                      
// Items guarded by $IFDEF_LIVE_SERVER_AT_DESIGN_TIME are used by properties  
// which return objects that may need to be explicitly created via a function 
// call prior to any access via the property. These items have been disabled  
// in order to prevent accidental use from within the object inspector. You   
// may enable them by defining LIVE_SERVER_AT_DESIGN_TIME or by selectively   
// removing them from the $IFDEF blocks. However, such items must still be    
// programmatically created via a method of the appropriate CoClass before    
// they can be used.                                                          
// ************************************************************************ //
// Type Lib: E:\Work\Five\Source\Cache\FolderIterator.tlb (1)
// IID\LCID: {86F691A3-66A8-11D1-A1A8-F8C645A0F340}\0
// Helpfile: 
// DepndLst: 
//   (1) v1.0 stdole, (D:\WINNT\System32\stdole32.tlb)
//   (2) v1.0 StdVCL, (D:\WINNT\System32\STDVCL32.DLL)
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
    function  Next: WordBool; safecall;
    procedure SetOptions(Options: Integer); safecall;
    function  Empty: WordBool; safecall;
    function  Get_Current: WideString; safecall;
    function  Get_FullPath: WideString; safecall;
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
    function  Next: WordBool; dispid 3;
    procedure SetOptions(Options: Integer); dispid 4;
    function  Empty: WordBool; dispid 5;
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


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TDirectoryIterator
// Help String      : 
// Default Interface: IDirectoryIterator
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TDirectoryIteratorProperties= class;
{$ENDIF}
  TDirectoryIterator = class(TOleServer)
  private
    FIntf:        IDirectoryIterator;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TDirectoryIteratorProperties;
    function      GetServerProperties: TDirectoryIteratorProperties;
{$ENDIF}
    function      GetDefaultInterface: IDirectoryIterator;
  protected
    procedure InitServerData; override;
    function  Get_Current: WideString;
    function  Get_FullPath: WideString;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IDirectoryIterator);
    procedure Disconnect; override;
    procedure SetFolder(const aPath: WideString; const aWildcard: WideString; Options: Integer);
    procedure Reset;
    function  Next: WordBool;
    procedure SetOptions(Options: Integer);
    function  Empty: WordBool;
    property  DefaultInterface: IDirectoryIterator read GetDefaultInterface;
    property Current: WideString read Get_Current;
    property FullPath: WideString read Get_FullPath;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TDirectoryIteratorProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TDirectoryIterator
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TDirectoryIteratorProperties = class(TPersistent)
  private
    FServer:    TDirectoryIterator;
    function    GetDefaultInterface: IDirectoryIterator;
    constructor Create(AServer: TDirectoryIterator);
  protected
    function  Get_Current: WideString;
    function  Get_FullPath: WideString;
  public
    property DefaultInterface: IDirectoryIterator read GetDefaultInterface;
  published
  end;
{$ENDIF}


procedure Register;

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

procedure TDirectoryIterator.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{86F691A4-66A8-11D1-A1A8-F8C645A0F340}';
    IntfIID:   '{86F691A5-66A8-11D1-A1A8-F8C645A0F340}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TDirectoryIterator.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IDirectoryIterator;
  end;
end;

procedure TDirectoryIterator.ConnectTo(svrIntf: IDirectoryIterator);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TDirectoryIterator.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TDirectoryIterator.GetDefaultInterface: IDirectoryIterator;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TDirectoryIterator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TDirectoryIteratorProperties.Create(Self);
{$ENDIF}
end;

destructor TDirectoryIterator.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TDirectoryIterator.GetServerProperties: TDirectoryIteratorProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function  TDirectoryIterator.Get_Current: WideString;
begin
  Result := DefaultInterface.Get_Current;
end;

function  TDirectoryIterator.Get_FullPath: WideString;
begin
  Result := DefaultInterface.Get_FullPath;
end;

procedure TDirectoryIterator.SetFolder(const aPath: WideString; const aWildcard: WideString; 
                                       Options: Integer);
begin
  DefaultInterface.SetFolder(aPath, aWildcard, Options);
end;

procedure TDirectoryIterator.Reset;
begin
  DefaultInterface.Reset;
end;

function  TDirectoryIterator.Next: WordBool;
begin
  Result := DefaultInterface.Next;
end;

procedure TDirectoryIterator.SetOptions(Options: Integer);
begin
  DefaultInterface.SetOptions(Options);
end;

function  TDirectoryIterator.Empty: WordBool;
begin
  Result := DefaultInterface.Empty;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TDirectoryIteratorProperties.Create(AServer: TDirectoryIterator);
begin
  inherited Create;
  FServer := AServer;
end;

function TDirectoryIteratorProperties.GetDefaultInterface: IDirectoryIterator;
begin
  Result := FServer.DefaultInterface;
end;

function  TDirectoryIteratorProperties.Get_Current: WideString;
begin
  Result := DefaultInterface.Get_Current;
end;

function  TDirectoryIteratorProperties.Get_FullPath: WideString;
begin
  Result := DefaultInterface.Get_FullPath;
end;

{$ENDIF}

procedure Register;
begin
  RegisterComponents('Servers',[TDirectoryIterator]);
end;

end.
