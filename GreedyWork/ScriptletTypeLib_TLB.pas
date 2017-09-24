unit ScriptletTypeLib_TLB;

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
// File generated on 12/4/01 1:15:11 AM from Type Library described below.

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
// Type Lib: S:\Release\Servers\wsc\crypto\scriptlet.tlb (1)
// IID\LCID: {D3589F10-E87D-11D5-992E-004854664ED7}\0
// Helpfile: 
// DepndLst: 
//   (1) v2.0 stdole, (D:\WINNT\System32\STDOLE2.TLB)
//   (2) v4.0 StdVCL, (D:\WINNT\System32\STDVCL40.DLL)
// Errors:
//   Error creating palette bitmap of (TScriptletCoClass) : Invalid GUID format
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
  ScriptletTypeLibMajorVersion = 1;
  ScriptletTypeLibMinorVersion = 0;

  LIBID_ScriptletTypeLib: TGUID = '{D3589F10-E87D-11D5-992E-004854664ED7}';

  DIID_Automation: TGUID = '{D3589F11-E87D-11D5-992E-004854664ED7}';
  CLASS_ScriptletCoClass: TGUID = '{42466F93-CE76-4C1A-BA73-D566CD4A1A47}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  Automation = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  ScriptletCoClass = Automation;


// *********************************************************************//
// DispIntf:  Automation
// Flags:     (4096) Dispatchable
// GUID:      {D3589F11-E87D-11D5-992E-004854664ED7}
// *********************************************************************//
  Automation = dispinterface
    ['{D3589F11-E87D-11D5-992E-004854664ED7}']
    function  Close(OptionalArgs: OleVariant): OleVariant; dispid 8;
    function  DecryptText(cypher_text: OleVariant; OptionalArgs: OleVariant): OleVariant; dispid 7;
    function  EncryptText(plain_text: OleVariant; OptionalArgs: OleVariant): OleVariant; dispid 6;
    function  Load(keyid: OleVariant; pvk_cert_filename: OleVariant; pwd: OleVariant; 
                   OptionalArgs: OleVariant): OleVariant; dispid 5;
    function  Generate(pbk_cert_filename: OleVariant; OptionalArgs: OleVariant): OleVariant; dispid 4;
    function  Init(key_signature: OleVariant; OptionalArgs: OleVariant): OleVariant; dispid 3;
    property Signature: OleVariant readonly dispid 2;
    property ID: OleVariant readonly dispid 1;
  end;

// *********************************************************************//
// The Class CoScriptletCoClass provides a Create and CreateRemote method to          
// create instances of the default interface Automation exposed by              
// the CoClass ScriptletCoClass. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoScriptletCoClass = class
    class function Create: Automation;
    class function CreateRemote(const MachineName: string): Automation;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TScriptletCoClass
// Help String      : 
// Default Interface: Automation
// Def. Intf. DISP? : Yes
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TScriptletCoClassProperties= class;
{$ENDIF}
  TScriptletCoClass = class(TOleServer)
  private
    FIntf:        Automation;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TScriptletCoClassProperties;
    function      GetServerProperties: TScriptletCoClassProperties;
{$ENDIF}
    function      GetDefaultInterface: Automation;
  protected
    procedure InitServerData; override;
    function  Get_Signature: OleVariant;
    function  Get_ID: OleVariant;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: Automation);
    procedure Disconnect; override;
    function  Close(OptionalArgs: OleVariant): OleVariant;
    function  DecryptText(cypher_text: OleVariant; OptionalArgs: OleVariant): OleVariant;
    function  EncryptText(plain_text: OleVariant; OptionalArgs: OleVariant): OleVariant;
    function  Load(keyid: OleVariant; pvk_cert_filename: OleVariant; pwd: OleVariant; 
                   OptionalArgs: OleVariant): OleVariant;
    function  Generate(pbk_cert_filename: OleVariant; OptionalArgs: OleVariant): OleVariant;
    function  Init(key_signature: OleVariant; OptionalArgs: OleVariant): OleVariant;
    property  DefaultInterface: Automation read GetDefaultInterface;
    property Signature: OleVariant read Get_Signature;
    property ID: OleVariant read Get_ID;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TScriptletCoClassProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TScriptletCoClass
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TScriptletCoClassProperties = class(TPersistent)
  private
    FServer:    TScriptletCoClass;
    function    GetDefaultInterface: Automation;
    constructor Create(AServer: TScriptletCoClass);
  protected
    function  Get_Signature: OleVariant;
    function  Get_ID: OleVariant;
  public
    property DefaultInterface: Automation read GetDefaultInterface;
  published
  end;
{$ENDIF}


procedure Register;

implementation

uses ComObj;

class function CoScriptletCoClass.Create: Automation;
begin
  Result := CreateComObject(CLASS_ScriptletCoClass) as Automation;
end;

class function CoScriptletCoClass.CreateRemote(const MachineName: string): Automation;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ScriptletCoClass) as Automation;
end;

procedure TScriptletCoClass.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{42466F93-CE76-4C1A-BA73-D566CD4A1A47}';
    IntfIID:   '{D3589F11-E87D-11D5-992E-004854664ED7}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TScriptletCoClass.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as Automation;
  end;
end;

procedure TScriptletCoClass.ConnectTo(svrIntf: Automation);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TScriptletCoClass.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TScriptletCoClass.GetDefaultInterface: Automation;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TScriptletCoClass.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TScriptletCoClassProperties.Create(Self);
{$ENDIF}
end;

destructor TScriptletCoClass.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TScriptletCoClass.GetServerProperties: TScriptletCoClassProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function  TScriptletCoClass.Get_Signature: OleVariant;
begin
  Result := DefaultInterface.Signature;
end;

function  TScriptletCoClass.Get_ID: OleVariant;
begin
  Result := DefaultInterface.ID;
end;

function  TScriptletCoClass.Close(OptionalArgs: OleVariant): OleVariant;
begin
  Result := DefaultInterface.Close(OptionalArgs);
end;

function  TScriptletCoClass.DecryptText(cypher_text: OleVariant; OptionalArgs: OleVariant): OleVariant;
begin
  Result := DefaultInterface.DecryptText(cypher_text, OptionalArgs);
end;

function  TScriptletCoClass.EncryptText(plain_text: OleVariant; OptionalArgs: OleVariant): OleVariant;
begin
  Result := DefaultInterface.EncryptText(plain_text, OptionalArgs);
end;

function  TScriptletCoClass.Load(keyid: OleVariant; pvk_cert_filename: OleVariant; pwd: OleVariant; 
                                 OptionalArgs: OleVariant): OleVariant;
begin
  Result := DefaultInterface.Load(keyid, pvk_cert_filename, pwd, OptionalArgs);
end;

function  TScriptletCoClass.Generate(pbk_cert_filename: OleVariant; OptionalArgs: OleVariant): OleVariant;
begin
  Result := DefaultInterface.Generate(pbk_cert_filename, OptionalArgs);
end;

function  TScriptletCoClass.Init(key_signature: OleVariant; OptionalArgs: OleVariant): OleVariant;
begin
  Result := DefaultInterface.Init(key_signature, OptionalArgs);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TScriptletCoClassProperties.Create(AServer: TScriptletCoClass);
begin
  inherited Create;
  FServer := AServer;
end;

function TScriptletCoClassProperties.GetDefaultInterface: Automation;
begin
  Result := FServer.DefaultInterface;
end;

function  TScriptletCoClassProperties.Get_Signature: OleVariant;
begin
  Result := DefaultInterface.Signature;
end;

function  TScriptletCoClassProperties.Get_ID: OleVariant;
begin
  Result := DefaultInterface.ID;
end;

{$ENDIF}

procedure Register;
begin
  RegisterComponents('ActiveX',[TScriptletCoClass]);
end;

end.
