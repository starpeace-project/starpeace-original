unit ASPEMAILLib_TLB;

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
// File generated on 12/5/01 2:17:31 PM from Type Library described below.

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
// Type Lib: D:\Program Files\Persits Software\AspEncrypt\Bin\AspEmail.dll (1)
// IID\LCID: {794D6711-F0F0-11D2-BEB0-009027438003}\0
// Helpfile: 
// DepndLst: 
//   (1) v2.0 stdole, (D:\WINNT\System32\STDOLE2.TLB)
//   (2) v4.0 StdVCL, (D:\WINNT\System32\STDVCL40.DLL)
// Errors:
//   Error creating palette bitmap of (TMailSender) : Invalid GUID format
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
  ASPEMAILLibMajorVersion = 1;
  ASPEMAILLibMinorVersion = 0;

  LIBID_ASPEMAILLib: TGUID = '{794D6711-F0F0-11D2-BEB0-009027438003}';

  IID_IMailSender: TGUID = '{794D671D-F0F0-11D2-BEB0-009027438003}';
  CLASS_MailSender: TGUID = '{794D671E-F0F0-11D2-BEB0-009027438003}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IMailSender = interface;
  IMailSenderDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  MailSender = IMailSender;


// *********************************************************************//
// Interface: IMailSender
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {794D671D-F0F0-11D2-BEB0-009027438003}
// *********************************************************************//
  IMailSender = interface(IDispatch)
    ['{794D671D-F0F0-11D2-BEB0-009027438003}']
    function  Send: WordBool; safecall;
    function  Get_IncludeErrorCode: Integer; safecall;
    procedure Set_IncludeErrorCode(pVal: Integer); safecall;
    function  Get_Host: WideString; safecall;
    procedure Set_Host(const pVal: WideString); safecall;
    function  Get_Port: Integer; safecall;
    procedure Set_Port(pVal: Integer); safecall;
    function  Get_From: WideString; safecall;
    procedure Set_From(const pVal: WideString); safecall;
    function  Get_FromName: WideString; safecall;
    procedure Set_FromName(const pVal: WideString); safecall;
    function  Get_Body: WideString; safecall;
    procedure Set_Body(const pVal: WideString); safecall;
    function  Get_Subject: WideString; safecall;
    procedure Set_Subject(const pVal: WideString); safecall;
    procedure AddAddress(const Address: WideString; Name: OleVariant); safecall;
    procedure AddReplyTo(const Address: WideString; Name: OleVariant); safecall;
    procedure AddCC(const Address: WideString; Name: OleVariant); safecall;
    procedure AddBcc(const Address: WideString; Name: OleVariant); safecall;
    procedure AddAttachment(const Path: WideString); safecall;
    procedure Reset; safecall;
    function  Get_IsHTML: Integer; safecall;
    procedure Set_IsHTML(pVal: Integer); safecall;
    function  Send2: WordBool; safecall;
    procedure AddEmbeddedImage(const Path: WideString; const ContentID: WideString); safecall;
    function  Get_AltBody: WideString; safecall;
    procedure Set_AltBody(const pVal: WideString); safecall;
    procedure AppendBodyFromFile(const Path: WideString); safecall;
    function  Get_Helo: WideString; safecall;
    procedure Set_Helo(const pVal: WideString); safecall;
    function  Get_ContentTransferEncoding: WideString; safecall;
    procedure Set_ContentTransferEncoding(const pVal: WideString); safecall;
    function  Get_Priority: Integer; safecall;
    procedure Set_Priority(pVal: Integer); safecall;
    function  Get_Expires: TDateTime; safecall;
    function  Get_CharSet: WideString; safecall;
    procedure Set_CharSet(const pVal: WideString); safecall;
    procedure ResetAll; safecall;
    procedure SendEncrypted(const pUnk: IUnknown); safecall;
    procedure AddCustomHeader(const Header: WideString); safecall;
    procedure SendSigned(const pUnk: IUnknown); safecall;
    procedure SendSignedAndEncrypted(const pSignMsg: IUnknown; const pEncryptMsg: IUnknown); safecall;
    procedure SendToQueue(Path: OleVariant); safecall;
    procedure LogonUser(const Domain: WideString; const UserID: WideString; 
                        const Password: WideString; Flags: OleVariant); safecall;
    procedure RevertToSelf; safecall;
    function  Get_Timestamp: TDateTime; safecall;
    procedure Set_Timestamp(pVal: TDateTime); safecall;
    function  EncodeHeader(const Header: WideString): WideString; safecall;
    function  Get_Username: WideString; safecall;
    procedure Set_Username(const pVal: WideString); safecall;
    function  Get_Password: WideString; safecall;
    procedure Set_Password(const pVal: WideString); safecall;
    procedure SendKeepConnected; safecall;
    procedure SendToNewsgroup(const Newsgroup: WideString); safecall;
    function  Get_Timeout: Integer; safecall;
    procedure Set_Timeout(pVal: Integer); safecall;
    property IncludeErrorCode: Integer read Get_IncludeErrorCode write Set_IncludeErrorCode;
    property Host: WideString read Get_Host write Set_Host;
    property Port: Integer read Get_Port write Set_Port;
    property From: WideString read Get_From write Set_From;
    property FromName: WideString read Get_FromName write Set_FromName;
    property Body: WideString read Get_Body write Set_Body;
    property Subject: WideString read Get_Subject write Set_Subject;
    property IsHTML: Integer read Get_IsHTML write Set_IsHTML;
    property AltBody: WideString read Get_AltBody write Set_AltBody;
    property Helo: WideString read Get_Helo write Set_Helo;
    property ContentTransferEncoding: WideString read Get_ContentTransferEncoding write Set_ContentTransferEncoding;
    property Priority: Integer read Get_Priority write Set_Priority;
    property Expires: TDateTime read Get_Expires;
    property CharSet: WideString read Get_CharSet write Set_CharSet;
    property Timestamp: TDateTime read Get_Timestamp write Set_Timestamp;
    property Username: WideString read Get_Username write Set_Username;
    property Password: WideString read Get_Password write Set_Password;
    property Timeout: Integer read Get_Timeout write Set_Timeout;
  end;

// *********************************************************************//
// DispIntf:  IMailSenderDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {794D671D-F0F0-11D2-BEB0-009027438003}
// *********************************************************************//
  IMailSenderDisp = dispinterface
    ['{794D671D-F0F0-11D2-BEB0-009027438003}']
    function  Send: WordBool; dispid 1;
    property IncludeErrorCode: Integer dispid 2;
    property Host: WideString dispid 3;
    property Port: Integer dispid 4;
    property From: WideString dispid 5;
    property FromName: WideString dispid 6;
    property Body: WideString dispid 7;
    property Subject: WideString dispid 8;
    procedure AddAddress(const Address: WideString; Name: OleVariant); dispid 9;
    procedure AddReplyTo(const Address: WideString; Name: OleVariant); dispid 10;
    procedure AddCC(const Address: WideString; Name: OleVariant); dispid 11;
    procedure AddBcc(const Address: WideString; Name: OleVariant); dispid 12;
    procedure AddAttachment(const Path: WideString); dispid 13;
    procedure Reset; dispid 14;
    property IsHTML: Integer dispid 15;
    function  Send2: WordBool; dispid 16;
    procedure AddEmbeddedImage(const Path: WideString; const ContentID: WideString); dispid 17;
    property AltBody: WideString dispid 18;
    procedure AppendBodyFromFile(const Path: WideString); dispid 19;
    property Helo: WideString dispid 20;
    property ContentTransferEncoding: WideString dispid 21;
    property Priority: Integer dispid 22;
    property Expires: TDateTime readonly dispid 23;
    property CharSet: WideString dispid 24;
    procedure ResetAll; dispid 25;
    procedure SendEncrypted(const pUnk: IUnknown); dispid 26;
    procedure AddCustomHeader(const Header: WideString); dispid 27;
    procedure SendSigned(const pUnk: IUnknown); dispid 28;
    procedure SendSignedAndEncrypted(const pSignMsg: IUnknown; const pEncryptMsg: IUnknown); dispid 29;
    procedure SendToQueue(Path: OleVariant); dispid 30;
    procedure LogonUser(const Domain: WideString; const UserID: WideString; 
                        const Password: WideString; Flags: OleVariant); dispid 31;
    procedure RevertToSelf; dispid 32;
    property Timestamp: TDateTime dispid 33;
    function  EncodeHeader(const Header: WideString): WideString; dispid 34;
    property Username: WideString dispid 35;
    property Password: WideString dispid 36;
    procedure SendKeepConnected; dispid 37;
    procedure SendToNewsgroup(const Newsgroup: WideString); dispid 38;
    property Timeout: Integer dispid 39;
  end;

// *********************************************************************//
// The Class CoMailSender provides a Create and CreateRemote method to          
// create instances of the default interface IMailSender exposed by              
// the CoClass MailSender. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoMailSender = class
    class function Create: IMailSender;
    class function CreateRemote(const MachineName: string): IMailSender;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TMailSender
// Help String      : MailSender Class
// Default Interface: IMailSender
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TMailSenderProperties= class;
{$ENDIF}
  TMailSender = class(TOleServer)
  private
    FIntf:        IMailSender;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TMailSenderProperties;
    function      GetServerProperties: TMailSenderProperties;
{$ENDIF}
    function      GetDefaultInterface: IMailSender;
  protected
    procedure InitServerData; override;
    function  Get_IncludeErrorCode: Integer;
    procedure Set_IncludeErrorCode(pVal: Integer);
    function  Get_Host: WideString;
    procedure Set_Host(const pVal: WideString);
    function  Get_Port: Integer;
    procedure Set_Port(pVal: Integer);
    function  Get_From: WideString;
    procedure Set_From(const pVal: WideString);
    function  Get_FromName: WideString;
    procedure Set_FromName(const pVal: WideString);
    function  Get_Body: WideString;
    procedure Set_Body(const pVal: WideString);
    function  Get_Subject: WideString;
    procedure Set_Subject(const pVal: WideString);
    function  Get_IsHTML: Integer;
    procedure Set_IsHTML(pVal: Integer);
    function  Get_AltBody: WideString;
    procedure Set_AltBody(const pVal: WideString);
    function  Get_Helo: WideString;
    procedure Set_Helo(const pVal: WideString);
    function  Get_ContentTransferEncoding: WideString;
    procedure Set_ContentTransferEncoding(const pVal: WideString);
    function  Get_Priority: Integer;
    procedure Set_Priority(pVal: Integer);
    function  Get_Expires: TDateTime;
    function  Get_CharSet: WideString;
    procedure Set_CharSet(const pVal: WideString);
    function  Get_Timestamp: TDateTime;
    procedure Set_Timestamp(pVal: TDateTime);
    function  Get_Username: WideString;
    procedure Set_Username(const pVal: WideString);
    function  Get_Password: WideString;
    procedure Set_Password(const pVal: WideString);
    function  Get_Timeout: Integer;
    procedure Set_Timeout(pVal: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IMailSender);
    procedure Disconnect; override;
    function  Send: WordBool;
    procedure AddAddress(const Address: WideString); overload;
    procedure AddAddress(const Address: WideString; Name: OleVariant); overload;
    procedure AddReplyTo(const Address: WideString); overload;
    procedure AddReplyTo(const Address: WideString; Name: OleVariant); overload;
    procedure AddCC(const Address: WideString); overload;
    procedure AddCC(const Address: WideString; Name: OleVariant); overload;
    procedure AddBcc(const Address: WideString); overload;
    procedure AddBcc(const Address: WideString; Name: OleVariant); overload;
    procedure AddAttachment(const Path: WideString);
    procedure Reset;
    procedure AddEmbeddedImage(const Path: WideString; const ContentID: WideString);
    procedure AppendBodyFromFile(const Path: WideString);
    procedure ResetAll;
    procedure SendEncrypted(const pUnk: IUnknown);
    procedure AddCustomHeader(const Header: WideString);
    procedure SendSigned(const pUnk: IUnknown);
    procedure SendSignedAndEncrypted(const pSignMsg: IUnknown; const pEncryptMsg: IUnknown);
    procedure SendToQueue; overload;
    procedure SendToQueue(Path: OleVariant); overload;
    procedure LogonUser(const Domain: WideString; const UserID: WideString; 
                        const Password: WideString); overload;
    procedure LogonUser(const Domain: WideString; const UserID: WideString; 
                        const Password: WideString; Flags: OleVariant); overload;
    procedure RevertToSelf;
    function  EncodeHeader(const Header: WideString): WideString;
    procedure SendToNewsgroup(const Newsgroup: WideString);
    property  DefaultInterface: IMailSender read GetDefaultInterface;
    property Expires: TDateTime read Get_Expires;
    property IncludeErrorCode: Integer read Get_IncludeErrorCode write Set_IncludeErrorCode;
    property Host: WideString read Get_Host write Set_Host;
    property Port: Integer read Get_Port write Set_Port;
    property From: WideString read Get_From write Set_From;
    property FromName: WideString read Get_FromName write Set_FromName;
    property Body: WideString read Get_Body write Set_Body;
    property Subject: WideString read Get_Subject write Set_Subject;
    property IsHTML: Integer read Get_IsHTML write Set_IsHTML;
    property AltBody: WideString read Get_AltBody write Set_AltBody;
    property Helo: WideString read Get_Helo write Set_Helo;
    property ContentTransferEncoding: WideString read Get_ContentTransferEncoding write Set_ContentTransferEncoding;
    property Priority: Integer read Get_Priority write Set_Priority;
    property CharSet: WideString read Get_CharSet write Set_CharSet;
    property Timestamp: TDateTime read Get_Timestamp write Set_Timestamp;
    property Username: WideString read Get_Username write Set_Username;
    property Password: WideString read Get_Password write Set_Password;
    property Timeout: Integer read Get_Timeout write Set_Timeout;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TMailSenderProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TMailSender
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TMailSenderProperties = class(TPersistent)
  private
    FServer:    TMailSender;
    function    GetDefaultInterface: IMailSender;
    constructor Create(AServer: TMailSender);
  protected
    function  Get_IncludeErrorCode: Integer;
    procedure Set_IncludeErrorCode(pVal: Integer);
    function  Get_Host: WideString;
    procedure Set_Host(const pVal: WideString);
    function  Get_Port: Integer;
    procedure Set_Port(pVal: Integer);
    function  Get_From: WideString;
    procedure Set_From(const pVal: WideString);
    function  Get_FromName: WideString;
    procedure Set_FromName(const pVal: WideString);
    function  Get_Body: WideString;
    procedure Set_Body(const pVal: WideString);
    function  Get_Subject: WideString;
    procedure Set_Subject(const pVal: WideString);
    function  Get_IsHTML: Integer;
    procedure Set_IsHTML(pVal: Integer);
    function  Get_AltBody: WideString;
    procedure Set_AltBody(const pVal: WideString);
    function  Get_Helo: WideString;
    procedure Set_Helo(const pVal: WideString);
    function  Get_ContentTransferEncoding: WideString;
    procedure Set_ContentTransferEncoding(const pVal: WideString);
    function  Get_Priority: Integer;
    procedure Set_Priority(pVal: Integer);
    function  Get_Expires: TDateTime;
    function  Get_CharSet: WideString;
    procedure Set_CharSet(const pVal: WideString);
    function  Get_Timestamp: TDateTime;
    procedure Set_Timestamp(pVal: TDateTime);
    function  Get_Username: WideString;
    procedure Set_Username(const pVal: WideString);
    function  Get_Password: WideString;
    procedure Set_Password(const pVal: WideString);
    function  Get_Timeout: Integer;
    procedure Set_Timeout(pVal: Integer);
  public
    property DefaultInterface: IMailSender read GetDefaultInterface;
  published
    property IncludeErrorCode: Integer read Get_IncludeErrorCode write Set_IncludeErrorCode;
    property Host: WideString read Get_Host write Set_Host;
    property Port: Integer read Get_Port write Set_Port;
    property From: WideString read Get_From write Set_From;
    property FromName: WideString read Get_FromName write Set_FromName;
    property Body: WideString read Get_Body write Set_Body;
    property Subject: WideString read Get_Subject write Set_Subject;
    property IsHTML: Integer read Get_IsHTML write Set_IsHTML;
    property AltBody: WideString read Get_AltBody write Set_AltBody;
    property Helo: WideString read Get_Helo write Set_Helo;
    property ContentTransferEncoding: WideString read Get_ContentTransferEncoding write Set_ContentTransferEncoding;
    property Priority: Integer read Get_Priority write Set_Priority;
    property CharSet: WideString read Get_CharSet write Set_CharSet;
    property Timestamp: TDateTime read Get_Timestamp write Set_Timestamp;
    property Username: WideString read Get_Username write Set_Username;
    property Password: WideString read Get_Password write Set_Password;
    property Timeout: Integer read Get_Timeout write Set_Timeout;
  end;
{$ENDIF}


procedure Register;

implementation

uses ComObj;

class function CoMailSender.Create: IMailSender;
begin
  Result := CreateComObject(CLASS_MailSender) as IMailSender;
end;

class function CoMailSender.CreateRemote(const MachineName: string): IMailSender;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_MailSender) as IMailSender;
end;

procedure TMailSender.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{794D671E-F0F0-11D2-BEB0-009027438003}';
    IntfIID:   '{794D671D-F0F0-11D2-BEB0-009027438003}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TMailSender.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IMailSender;
  end;
end;

procedure TMailSender.ConnectTo(svrIntf: IMailSender);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TMailSender.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TMailSender.GetDefaultInterface: IMailSender;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TMailSender.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TMailSenderProperties.Create(Self);
{$ENDIF}
end;

destructor TMailSender.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TMailSender.GetServerProperties: TMailSenderProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function  TMailSender.Get_IncludeErrorCode: Integer;
begin
  Result := DefaultInterface.Get_IncludeErrorCode;
end;

procedure TMailSender.Set_IncludeErrorCode(pVal: Integer);
begin
  DefaultInterface.Set_IncludeErrorCode(pVal);
end;

function  TMailSender.Get_Host: WideString;
begin
  Result := DefaultInterface.Get_Host;
end;

procedure TMailSender.Set_Host(const pVal: WideString);
begin
  DefaultInterface.Set_Host(pVal);
end;

function  TMailSender.Get_Port: Integer;
begin
  Result := DefaultInterface.Get_Port;
end;

procedure TMailSender.Set_Port(pVal: Integer);
begin
  DefaultInterface.Set_Port(pVal);
end;

function  TMailSender.Get_From: WideString;
begin
  Result := DefaultInterface.Get_From;
end;

procedure TMailSender.Set_From(const pVal: WideString);
begin
  DefaultInterface.Set_From(pVal);
end;

function  TMailSender.Get_FromName: WideString;
begin
  Result := DefaultInterface.Get_FromName;
end;

procedure TMailSender.Set_FromName(const pVal: WideString);
begin
  DefaultInterface.Set_FromName(pVal);
end;

function  TMailSender.Get_Body: WideString;
begin
  Result := DefaultInterface.Get_Body;
end;

procedure TMailSender.Set_Body(const pVal: WideString);
begin
  DefaultInterface.Set_Body(pVal);
end;

function  TMailSender.Get_Subject: WideString;
begin
  Result := DefaultInterface.Get_Subject;
end;

procedure TMailSender.Set_Subject(const pVal: WideString);
begin
  DefaultInterface.Set_Subject(pVal);
end;

function  TMailSender.Get_IsHTML: Integer;
begin
  Result := DefaultInterface.Get_IsHTML;
end;

procedure TMailSender.Set_IsHTML(pVal: Integer);
begin
  DefaultInterface.Set_IsHTML(pVal);
end;

function  TMailSender.Get_AltBody: WideString;
begin
  Result := DefaultInterface.Get_AltBody;
end;

procedure TMailSender.Set_AltBody(const pVal: WideString);
begin
  DefaultInterface.Set_AltBody(pVal);
end;

function  TMailSender.Get_Helo: WideString;
begin
  Result := DefaultInterface.Get_Helo;
end;

procedure TMailSender.Set_Helo(const pVal: WideString);
begin
  DefaultInterface.Set_Helo(pVal);
end;

function  TMailSender.Get_ContentTransferEncoding: WideString;
begin
  Result := DefaultInterface.Get_ContentTransferEncoding;
end;

procedure TMailSender.Set_ContentTransferEncoding(const pVal: WideString);
begin
  DefaultInterface.Set_ContentTransferEncoding(pVal);
end;

function  TMailSender.Get_Priority: Integer;
begin
  Result := DefaultInterface.Get_Priority;
end;

procedure TMailSender.Set_Priority(pVal: Integer);
begin
  DefaultInterface.Set_Priority(pVal);
end;

function  TMailSender.Get_Expires: TDateTime;
begin
  Result := DefaultInterface.Get_Expires;
end;

function  TMailSender.Get_CharSet: WideString;
begin
  Result := DefaultInterface.Get_CharSet;
end;

procedure TMailSender.Set_CharSet(const pVal: WideString);
begin
  DefaultInterface.Set_CharSet(pVal);
end;

function  TMailSender.Get_Timestamp: TDateTime;
begin
  Result := DefaultInterface.Get_Timestamp;
end;

procedure TMailSender.Set_Timestamp(pVal: TDateTime);
begin
  DefaultInterface.Set_Timestamp(pVal);
end;

function  TMailSender.Get_Username: WideString;
begin
  Result := DefaultInterface.Get_Username;
end;

procedure TMailSender.Set_Username(const pVal: WideString);
begin
  DefaultInterface.Set_Username(pVal);
end;

function  TMailSender.Get_Password: WideString;
begin
  Result := DefaultInterface.Get_Password;
end;

procedure TMailSender.Set_Password(const pVal: WideString);
begin
  DefaultInterface.Set_Password(pVal);
end;

function  TMailSender.Get_Timeout: Integer;
begin
  Result := DefaultInterface.Get_Timeout;
end;

procedure TMailSender.Set_Timeout(pVal: Integer);
begin
  DefaultInterface.Set_Timeout(pVal);
end;

function  TMailSender.Send: WordBool;
begin
  Result := DefaultInterface.Send;
end;

procedure TMailSender.AddAddress(const Address: WideString);
begin
  DefaultInterface.AddAddress(Address, EmptyParam);
end;

procedure TMailSender.AddAddress(const Address: WideString; Name: OleVariant);
begin
  DefaultInterface.AddAddress(Address, Name);
end;

procedure TMailSender.AddReplyTo(const Address: WideString);
begin
  DefaultInterface.AddReplyTo(Address, EmptyParam);
end;

procedure TMailSender.AddReplyTo(const Address: WideString; Name: OleVariant);
begin
  DefaultInterface.AddReplyTo(Address, Name);
end;

procedure TMailSender.AddCC(const Address: WideString);
begin
  DefaultInterface.AddCC(Address, EmptyParam);
end;

procedure TMailSender.AddCC(const Address: WideString; Name: OleVariant);
begin
  DefaultInterface.AddCC(Address, Name);
end;

procedure TMailSender.AddBcc(const Address: WideString);
begin
  DefaultInterface.AddBcc(Address, EmptyParam);
end;

procedure TMailSender.AddBcc(const Address: WideString; Name: OleVariant);
begin
  DefaultInterface.AddBcc(Address, Name);
end;

procedure TMailSender.AddAttachment(const Path: WideString);
begin
  DefaultInterface.AddAttachment(Path);
end;

procedure TMailSender.Reset;
begin
  DefaultInterface.Reset;
end;

procedure TMailSender.AddEmbeddedImage(const Path: WideString; const ContentID: WideString);
begin
  DefaultInterface.AddEmbeddedImage(Path, ContentID);
end;

procedure TMailSender.AppendBodyFromFile(const Path: WideString);
begin
  DefaultInterface.AppendBodyFromFile(Path);
end;

procedure TMailSender.ResetAll;
begin
  DefaultInterface.ResetAll;
end;

procedure TMailSender.SendEncrypted(const pUnk: IUnknown);
begin
  DefaultInterface.SendEncrypted(pUnk);
end;

procedure TMailSender.AddCustomHeader(const Header: WideString);
begin
  DefaultInterface.AddCustomHeader(Header);
end;

procedure TMailSender.SendSigned(const pUnk: IUnknown);
begin
  DefaultInterface.SendSigned(pUnk);
end;

procedure TMailSender.SendSignedAndEncrypted(const pSignMsg: IUnknown; const pEncryptMsg: IUnknown);
begin
  DefaultInterface.SendSignedAndEncrypted(pSignMsg, pEncryptMsg);
end;

procedure TMailSender.SendToQueue;
begin
  DefaultInterface.SendToQueue(EmptyParam);
end;

procedure TMailSender.SendToQueue(Path: OleVariant);
begin
  DefaultInterface.SendToQueue(Path);
end;

procedure TMailSender.LogonUser(const Domain: WideString; const UserID: WideString; 
                                const Password: WideString);
begin
  DefaultInterface.LogonUser(Domain, UserID, Password, EmptyParam);
end;

procedure TMailSender.LogonUser(const Domain: WideString; const UserID: WideString; 
                                const Password: WideString; Flags: OleVariant);
begin
  DefaultInterface.LogonUser(Domain, UserID, Password, Flags);
end;

procedure TMailSender.RevertToSelf;
begin
  DefaultInterface.RevertToSelf;
end;

function  TMailSender.EncodeHeader(const Header: WideString): WideString;
begin
  Result := DefaultInterface.EncodeHeader(Header);
end;

procedure TMailSender.SendToNewsgroup(const Newsgroup: WideString);
begin
  DefaultInterface.SendToNewsgroup(Newsgroup);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TMailSenderProperties.Create(AServer: TMailSender);
begin
  inherited Create;
  FServer := AServer;
end;

function TMailSenderProperties.GetDefaultInterface: IMailSender;
begin
  Result := FServer.DefaultInterface;
end;

function  TMailSenderProperties.Get_IncludeErrorCode: Integer;
begin
  Result := DefaultInterface.Get_IncludeErrorCode;
end;

procedure TMailSenderProperties.Set_IncludeErrorCode(pVal: Integer);
begin
  DefaultInterface.Set_IncludeErrorCode(pVal);
end;

function  TMailSenderProperties.Get_Host: WideString;
begin
  Result := DefaultInterface.Get_Host;
end;

procedure TMailSenderProperties.Set_Host(const pVal: WideString);
begin
  DefaultInterface.Set_Host(pVal);
end;

function  TMailSenderProperties.Get_Port: Integer;
begin
  Result := DefaultInterface.Get_Port;
end;

procedure TMailSenderProperties.Set_Port(pVal: Integer);
begin
  DefaultInterface.Set_Port(pVal);
end;

function  TMailSenderProperties.Get_From: WideString;
begin
  Result := DefaultInterface.Get_From;
end;

procedure TMailSenderProperties.Set_From(const pVal: WideString);
begin
  DefaultInterface.Set_From(pVal);
end;

function  TMailSenderProperties.Get_FromName: WideString;
begin
  Result := DefaultInterface.Get_FromName;
end;

procedure TMailSenderProperties.Set_FromName(const pVal: WideString);
begin
  DefaultInterface.Set_FromName(pVal);
end;

function  TMailSenderProperties.Get_Body: WideString;
begin
  Result := DefaultInterface.Get_Body;
end;

procedure TMailSenderProperties.Set_Body(const pVal: WideString);
begin
  DefaultInterface.Set_Body(pVal);
end;

function  TMailSenderProperties.Get_Subject: WideString;
begin
  Result := DefaultInterface.Get_Subject;
end;

procedure TMailSenderProperties.Set_Subject(const pVal: WideString);
begin
  DefaultInterface.Set_Subject(pVal);
end;

function  TMailSenderProperties.Get_IsHTML: Integer;
begin
  Result := DefaultInterface.Get_IsHTML;
end;

procedure TMailSenderProperties.Set_IsHTML(pVal: Integer);
begin
  DefaultInterface.Set_IsHTML(pVal);
end;

function  TMailSenderProperties.Get_AltBody: WideString;
begin
  Result := DefaultInterface.Get_AltBody;
end;

procedure TMailSenderProperties.Set_AltBody(const pVal: WideString);
begin
  DefaultInterface.Set_AltBody(pVal);
end;

function  TMailSenderProperties.Get_Helo: WideString;
begin
  Result := DefaultInterface.Get_Helo;
end;

procedure TMailSenderProperties.Set_Helo(const pVal: WideString);
begin
  DefaultInterface.Set_Helo(pVal);
end;

function  TMailSenderProperties.Get_ContentTransferEncoding: WideString;
begin
  Result := DefaultInterface.Get_ContentTransferEncoding;
end;

procedure TMailSenderProperties.Set_ContentTransferEncoding(const pVal: WideString);
begin
  DefaultInterface.Set_ContentTransferEncoding(pVal);
end;

function  TMailSenderProperties.Get_Priority: Integer;
begin
  Result := DefaultInterface.Get_Priority;
end;

procedure TMailSenderProperties.Set_Priority(pVal: Integer);
begin
  DefaultInterface.Set_Priority(pVal);
end;

function  TMailSenderProperties.Get_Expires: TDateTime;
begin
  Result := DefaultInterface.Get_Expires;
end;

function  TMailSenderProperties.Get_CharSet: WideString;
begin
  Result := DefaultInterface.Get_CharSet;
end;

procedure TMailSenderProperties.Set_CharSet(const pVal: WideString);
begin
  DefaultInterface.Set_CharSet(pVal);
end;

function  TMailSenderProperties.Get_Timestamp: TDateTime;
begin
  Result := DefaultInterface.Get_Timestamp;
end;

procedure TMailSenderProperties.Set_Timestamp(pVal: TDateTime);
begin
  DefaultInterface.Set_Timestamp(pVal);
end;

function  TMailSenderProperties.Get_Username: WideString;
begin
  Result := DefaultInterface.Get_Username;
end;

procedure TMailSenderProperties.Set_Username(const pVal: WideString);
begin
  DefaultInterface.Set_Username(pVal);
end;

function  TMailSenderProperties.Get_Password: WideString;
begin
  Result := DefaultInterface.Get_Password;
end;

procedure TMailSenderProperties.Set_Password(const pVal: WideString);
begin
  DefaultInterface.Set_Password(pVal);
end;

function  TMailSenderProperties.Get_Timeout: Integer;
begin
  Result := DefaultInterface.Get_Timeout;
end;

procedure TMailSenderProperties.Set_Timeout(pVal: Integer);
begin
  DefaultInterface.Set_Timeout(pVal);
end;

{$ENDIF}

procedure Register;
begin
  RegisterComponents('ActiveX',[TMailSender]);
end;

end.
