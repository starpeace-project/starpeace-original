unit ASPENCRYPTLib_TLB;

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
// File generated on 12/5/01 2:18:16 PM from Type Library described below.

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
// Type Lib: D:\Program Files\Persits Software\AspEncrypt\Bin\AspEncrypt.dll (1)
// IID\LCID: {B72DF063-28A4-11D3-BF19-009027438003}\0
// Helpfile: 
// DepndLst: 
//   (1) v2.0 stdole, (D:\WINNT\System32\STDOLE2.TLB)
//   (2) v4.0 StdVCL, (D:\WINNT\System32\STDVCL40.DLL)
// Errors:
//   Hint: Parameter 'File' of ICryptoMessage.SignFile changed to 'File_'
//   Error creating palette bitmap of (TCryptoManager) : Invalid GUID format
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
  ASPENCRYPTLibMajorVersion = 1;
  ASPENCRYPTLibMinorVersion = 0;

  LIBID_ASPENCRYPTLib: TGUID = '{B72DF063-28A4-11D3-BF19-009027438003}';

  IID_ICryptoManager: TGUID = '{B72DF06F-28A4-11D3-BF19-009027438003}';
  CLASS_CryptoManager: TGUID = '{B72DF070-28A4-11D3-BF19-009027438003}';
  IID_ICryptoStore: TGUID = '{935A7A43-2A3F-11D3-BF1E-009027438003}';
  IID_ICryptoCerts: TGUID = '{935A7A4C-2A3F-11D3-BF1E-009027438003}';
  IID_ICryptoCert: TGUID = '{935A7A4A-2A3F-11D3-BF1E-009027438003}';
  IID_ICryptoName: TGUID = '{A58BC1F3-2B19-11D3-BF1F-009027438003}';
  IID_ICryptoContext: TGUID = '{2F612C16-2FBE-11D3-BF25-009027438003}';
  IID_ICryptoKey: TGUID = '{B72DF075-28A4-11D3-BF19-009027438003}';
  IID_ICryptoBlob: TGUID = '{B24C5CC6-4FF8-11D3-BF60-009027438003}';
  IID_ICryptoHash: TGUID = '{F837B983-2975-11D3-BF1A-009027438003}';
  IID_ICryptoMessage: TGUID = '{8F481223-33A9-11D3-BF2A-009027438003}';
  IID_ICryptoCrl: TGUID = '{2A6866E6-63A2-11D3-BF89-009027438003}';
  IID_ICryptoCRLs: TGUID = '{2A6866E8-63A2-11D3-BF89-009027438003}';
  IID_IXEncrypt: TGUID = '{103B19CF-496F-4A3C-B975-129A56BA0646}';
  CLASS_XEncrypt: TGUID = '{F9463571-87CB-4A90-A1AC-2284B7F5AF4E}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum CryptoBlobTypes
type
  CryptoBlobTypes = TOleEnum;
const
  cbtSimpleBlob = $00000001;
  cbtPublicKeyBlob = $00000006;
  cbtPrivateKeyBlob = $00000007;
  cbtPlainTextBlob = $00000008;

// Constants for enum CryptoAlgorithms
type
  CryptoAlgorithms = TOleEnum;
const
  catKeyExchange = $00000001;
  catSignature = $00000002;
  calgRC2 = $00006602;
  calgRC4 = $00006801;
  calgMD2 = $00008001;
  calgMD4 = $00008002;
  calgMD5 = $00008003;
  calgSHA = $00008004;
  calgDES = $00006601;
  calg3DES = $00006603;
  calg3DES2 = $00006609;

// Constants for enum CryptoCipherModes
type
  CryptoCipherModes = TOleEnum;
const
  ccmCBC = $00000001;
  ccmECB = $00000002;
  ccmOFB = $00000003;
  ccmCFB = $00000004;
  ccmCTS = $00000005;

// Constants for enum CryptoCipherPadding
type
  CryptoCipherPadding = TOleEnum;
const
  ccpPKCS5 = $00000001;
  ccpRandom = $00000002;
  ccpZero = $00000003;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  ICryptoManager = interface;
  ICryptoManagerDisp = dispinterface;
  ICryptoStore = interface;
  ICryptoStoreDisp = dispinterface;
  ICryptoCerts = interface;
  ICryptoCertsDisp = dispinterface;
  ICryptoCert = interface;
  ICryptoCertDisp = dispinterface;
  ICryptoName = interface;
  ICryptoNameDisp = dispinterface;
  ICryptoContext = interface;
  ICryptoContextDisp = dispinterface;
  ICryptoKey = interface;
  ICryptoKeyDisp = dispinterface;
  ICryptoBlob = interface;
  ICryptoBlobDisp = dispinterface;
  ICryptoHash = interface;
  ICryptoHashDisp = dispinterface;
  ICryptoMessage = interface;
  ICryptoMessageDisp = dispinterface;
  ICryptoCrl = interface;
  ICryptoCrlDisp = dispinterface;
  ICryptoCRLs = interface;
  ICryptoCRLsDisp = dispinterface;
  IXEncrypt = interface;
  IXEncryptDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  CryptoManager = ICryptoManager;
  XEncrypt = IXEncrypt;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  PPUserType1 = ^ICryptoKey; {*}


// *********************************************************************//
// Interface: ICryptoManager
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B72DF06F-28A4-11D3-BF19-009027438003}
// *********************************************************************//
  ICryptoManager = interface(IDispatch)
    ['{B72DF06F-28A4-11D3-BF19-009027438003}']
    function  Get_IncludeErrorCode: Integer; safecall;
    procedure Set_IncludeErrorCode(pVal: Integer); safecall;
    function  OpenStore(const Name: WideString; LocalMachine: OleVariant): ICryptoStore; safecall;
    procedure LogonUser(const Domain: WideString; const Username: WideString; 
                        const Password: WideString; Flag: OleVariant); safecall;
    procedure RevertToSelf; safecall;
    function  OpenContext(const Container: WideString; bMachineKey: Integer; CreateNew: OleVariant): ICryptoContext; safecall;
    function  ImportCertFromFile(const Path: WideString): ICryptoCert; safecall;
    procedure DeleteKeySet(const Container: WideString; bMachineKeyset: Integer); safecall;
    function  ImportStoreFromFile(const Path: WideString): ICryptoStore; safecall;
    procedure SetDefaultProvider(const Name: WideString); safecall;
    function  CreateBlob: ICryptoBlob; safecall;
    function  Get_Expires: TDateTime; safecall;
    function  ImportCrlFromFile(const Path: WideString): ICryptoCrl; safecall;
    function  ImportCertFromBlob(const piBlob: ICryptoBlob): ICryptoCert; safecall;
    function  OpenStoreFromPFX(const Path: WideString; const Password: WideString): ICryptoStore; safecall;
    function  OpenContextEx(const Provider: WideString; const Container: WideString; 
                            bMachineKey: Integer; CreateNew: OleVariant): ICryptoContext; safecall;
    function  CreateMemoryStore: ICryptoStore; safecall;
    property IncludeErrorCode: Integer read Get_IncludeErrorCode write Set_IncludeErrorCode;
    property Expires: TDateTime read Get_Expires;
  end;

// *********************************************************************//
// DispIntf:  ICryptoManagerDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B72DF06F-28A4-11D3-BF19-009027438003}
// *********************************************************************//
  ICryptoManagerDisp = dispinterface
    ['{B72DF06F-28A4-11D3-BF19-009027438003}']
    property IncludeErrorCode: Integer dispid 2;
    function  OpenStore(const Name: WideString; LocalMachine: OleVariant): ICryptoStore; dispid 9;
    procedure LogonUser(const Domain: WideString; const Username: WideString; 
                        const Password: WideString; Flag: OleVariant); dispid 10;
    procedure RevertToSelf; dispid 11;
    function  OpenContext(const Container: WideString; bMachineKey: Integer; CreateNew: OleVariant): ICryptoContext; dispid 13;
    function  ImportCertFromFile(const Path: WideString): ICryptoCert; dispid 14;
    procedure DeleteKeySet(const Container: WideString; bMachineKeyset: Integer); dispid 15;
    function  ImportStoreFromFile(const Path: WideString): ICryptoStore; dispid 16;
    procedure SetDefaultProvider(const Name: WideString); dispid 17;
    function  CreateBlob: ICryptoBlob; dispid 18;
    property Expires: TDateTime readonly dispid 19;
    function  ImportCrlFromFile(const Path: WideString): ICryptoCrl; dispid 20;
    function  ImportCertFromBlob(const piBlob: ICryptoBlob): ICryptoCert; dispid 21;
    function  OpenStoreFromPFX(const Path: WideString; const Password: WideString): ICryptoStore; dispid 22;
    function  OpenContextEx(const Provider: WideString; const Container: WideString; 
                            bMachineKey: Integer; CreateNew: OleVariant): ICryptoContext; dispid 23;
    function  CreateMemoryStore: ICryptoStore; dispid 24;
  end;

// *********************************************************************//
// Interface: ICryptoStore
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {935A7A43-2A3F-11D3-BF1E-009027438003}
// *********************************************************************//
  ICryptoStore = interface(IDispatch)
    ['{935A7A43-2A3F-11D3-BF1E-009027438003}']
    function  Get_Certificates: ICryptoCerts; safecall;
    function  Get_Name: WideString; safecall;
    procedure DeleteCertificate(const SerialNumber: WideString); safecall;
    procedure AddCertificate(const pCert: ICryptoCert; Disposition: OleVariant); safecall;
    procedure AddCrl(const piCrl: ICryptoCrl; Disposition: OleVariant); safecall;
    function  Get_CRLs: ICryptoCRLs; safecall;
    procedure DeleteCrl(nIndex: Integer); safecall;
    procedure ExportToPFX(const Password: WideString; const Path: WideString); safecall;
    property Certificates: ICryptoCerts read Get_Certificates;
    property Name: WideString read Get_Name;
    property CRLs: ICryptoCRLs read Get_CRLs;
  end;

// *********************************************************************//
// DispIntf:  ICryptoStoreDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {935A7A43-2A3F-11D3-BF1E-009027438003}
// *********************************************************************//
  ICryptoStoreDisp = dispinterface
    ['{935A7A43-2A3F-11D3-BF1E-009027438003}']
    property Certificates: ICryptoCerts readonly dispid 1;
    property Name: WideString readonly dispid 2;
    procedure DeleteCertificate(const SerialNumber: WideString); dispid 3;
    procedure AddCertificate(const pCert: ICryptoCert; Disposition: OleVariant); dispid 4;
    procedure AddCrl(const piCrl: ICryptoCrl; Disposition: OleVariant); dispid 5;
    property CRLs: ICryptoCRLs readonly dispid 6;
    procedure DeleteCrl(nIndex: Integer); dispid 7;
    procedure ExportToPFX(const Password: WideString; const Path: WideString); dispid 8;
  end;

// *********************************************************************//
// Interface: ICryptoCerts
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {935A7A4C-2A3F-11D3-BF1E-009027438003}
// *********************************************************************//
  ICryptoCerts = interface(IDispatch)
    ['{935A7A4C-2A3F-11D3-BF1E-009027438003}']
    function  Get_Count: Integer; safecall;
    function  Get__NewEnum: IUnknown; safecall;
    function  Item(Index: OleVariant): ICryptoCert; safecall;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  ICryptoCertsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {935A7A4C-2A3F-11D3-BF1E-009027438003}
// *********************************************************************//
  ICryptoCertsDisp = dispinterface
    ['{935A7A4C-2A3F-11D3-BF1E-009027438003}']
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
    function  Item(Index: OleVariant): ICryptoCert; dispid 0;
  end;

// *********************************************************************//
// Interface: ICryptoCert
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {935A7A4A-2A3F-11D3-BF1E-009027438003}
// *********************************************************************//
  ICryptoCert = interface(IDispatch)
    ['{935A7A4A-2A3F-11D3-BF1E-009027438003}']
    function  Get_SerialNumber: WideString; safecall;
    function  Get_Version: Integer; safecall;
    function  Get_SignatureAlgorithm: WideString; safecall;
    function  Get_SignatureAlgID: CryptoAlgorithms; safecall;
    function  Get_Subject: ICryptoName; safecall;
    function  Get_Issuer: ICryptoName; safecall;
    function  Get_NotBefore: TDateTime; safecall;
    function  Get_NotAfter: TDateTime; safecall;
    procedure ExportToFile(const Path: WideString; bBase64: Integer); safecall;
    procedure ExportToFilePKCS7(const Path: WideString; bIncludePath: Integer); safecall;
    function  Get_IssuerCert: ICryptoCert; safecall;
    function  Get_StoreName: WideString; safecall;
    function  Get_PrivateKeyExists: Integer; safecall;
    function  Get_PrivateKeyContext: ICryptoContext; safecall;
    function  Get_PublicKeyLength: Integer; safecall;
    procedure SetEnhancedKeyUsage(const ObjectID: WideString); safecall;
    procedure TransferToLocalMachine(const Store: WideString); safecall;
    procedure SetPrivateKeyContext(const piContext: ICryptoContext); safecall;
    function  Get_PublicKey: ICryptoBlob; safecall;
    function  VerifySignature(const piCert: ICryptoCert): WordBool; safecall;
    function  Get_Sha1Hash: ICryptoBlob; safecall;
    function  Get_KeyUsage: Smallint; safecall;
    function  Get_BasicConstraints: Smallint; safecall;
    procedure ExportToPFX(const Path: WideString; const Password: WideString); safecall;
    property SerialNumber: WideString read Get_SerialNumber;
    property Version: Integer read Get_Version;
    property SignatureAlgorithm: WideString read Get_SignatureAlgorithm;
    property SignatureAlgID: CryptoAlgorithms read Get_SignatureAlgID;
    property Subject: ICryptoName read Get_Subject;
    property Issuer: ICryptoName read Get_Issuer;
    property NotBefore: TDateTime read Get_NotBefore;
    property NotAfter: TDateTime read Get_NotAfter;
    property IssuerCert: ICryptoCert read Get_IssuerCert;
    property StoreName: WideString read Get_StoreName;
    property PrivateKeyExists: Integer read Get_PrivateKeyExists;
    property PrivateKeyContext: ICryptoContext read Get_PrivateKeyContext;
    property PublicKeyLength: Integer read Get_PublicKeyLength;
    property PublicKey: ICryptoBlob read Get_PublicKey;
    property Sha1Hash: ICryptoBlob read Get_Sha1Hash;
    property KeyUsage: Smallint read Get_KeyUsage;
    property BasicConstraints: Smallint read Get_BasicConstraints;
  end;

// *********************************************************************//
// DispIntf:  ICryptoCertDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {935A7A4A-2A3F-11D3-BF1E-009027438003}
// *********************************************************************//
  ICryptoCertDisp = dispinterface
    ['{935A7A4A-2A3F-11D3-BF1E-009027438003}']
    property SerialNumber: WideString readonly dispid 1;
    property Version: Integer readonly dispid 2;
    property SignatureAlgorithm: WideString readonly dispid 3;
    property SignatureAlgID: CryptoAlgorithms readonly dispid 4;
    property Subject: ICryptoName readonly dispid 5;
    property Issuer: ICryptoName readonly dispid 6;
    property NotBefore: TDateTime readonly dispid 7;
    property NotAfter: TDateTime readonly dispid 8;
    procedure ExportToFile(const Path: WideString; bBase64: Integer); dispid 10;
    procedure ExportToFilePKCS7(const Path: WideString; bIncludePath: Integer); dispid 11;
    property IssuerCert: ICryptoCert readonly dispid 12;
    property StoreName: WideString readonly dispid 13;
    property PrivateKeyExists: Integer readonly dispid 14;
    property PrivateKeyContext: ICryptoContext readonly dispid 15;
    property PublicKeyLength: Integer readonly dispid 16;
    procedure SetEnhancedKeyUsage(const ObjectID: WideString); dispid 17;
    procedure TransferToLocalMachine(const Store: WideString); dispid 18;
    procedure SetPrivateKeyContext(const piContext: ICryptoContext); dispid 19;
    property PublicKey: ICryptoBlob readonly dispid 20;
    function  VerifySignature(const piCert: ICryptoCert): WordBool; dispid 21;
    property Sha1Hash: ICryptoBlob readonly dispid 22;
    property KeyUsage: Smallint readonly dispid 24;
    property BasicConstraints: Smallint readonly dispid 25;
    procedure ExportToPFX(const Path: WideString; const Password: WideString); dispid 26;
  end;

// *********************************************************************//
// Interface: ICryptoName
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A58BC1F3-2B19-11D3-BF1F-009027438003}
// *********************************************************************//
  ICryptoName = interface(IDispatch)
    ['{A58BC1F3-2B19-11D3-BF1F-009027438003}']
    function  Item(Item: OleVariant): WideString; safecall;
    function  Get_Name: WideString; safecall;
    property Name: WideString read Get_Name;
  end;

// *********************************************************************//
// DispIntf:  ICryptoNameDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A58BC1F3-2B19-11D3-BF1F-009027438003}
// *********************************************************************//
  ICryptoNameDisp = dispinterface
    ['{A58BC1F3-2B19-11D3-BF1F-009027438003}']
    function  Item(Item: OleVariant): WideString; dispid 0;
    property Name: WideString readonly dispid 1;
  end;

// *********************************************************************//
// Interface: ICryptoContext
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2F612C16-2FBE-11D3-BF25-009027438003}
// *********************************************************************//
  ICryptoContext = interface(IDispatch)
    ['{2F612C16-2FBE-11D3-BF25-009027438003}']
    function  GetUserKey(bKeyExchange: Integer): ICryptoKey; safecall;
    procedure CreateKeyPair(bKeyExchange: Integer); safecall;
    function  GenerateKey(Algorithm: OleVariant; BitSize: OleVariant): ICryptoKey; safecall;
    function  CreateHash(Algorithm: OleVariant): ICryptoHash; safecall;
    function  GenerateKeyFromPassword(const Password: WideString; HashAlgorithm: OleVariant; 
                                      KeyAlgorithm: OleVariant; BitSize: OleVariant): ICryptoKey; safecall;
    function  ImportKeyFromFile(const pExchangeKey: ICryptoKey; const Path: WideString; 
                                BlobType: CryptoBlobTypes): ICryptoKey; safecall;
    function  CreateCertificate(const pSignerContext: ICryptoContext; const Subject: WideString; 
                                NotBefore: TDateTime; NotAfter: TDateTime; 
                                bIncludePrivateKey: Integer): ICryptoCert; safecall;
    function  CreateMessage(TripleDes: OleVariant): ICryptoMessage; safecall;
    function  ImportKeyFromCert(const piCert: ICryptoCert): ICryptoKey; safecall;
    function  CreateEmptyKey(Algorithm: OleVariant; BitSize: OleVariant): ICryptoKey; safecall;
    procedure GetDefaultUserKey(var ppKey: ICryptoKey); safecall;
    function  GenerateCertificateRequest(const Subject: WideString): WideString; safecall;
    function  Get_KeySpec: Integer; safecall;
    function  CreateCertificateFromRequest(const pSignerContext: ICryptoContext; 
                                           const Request: WideString; NotBefore: TDateTime; 
                                           NotAfter: TDateTime): ICryptoCert; safecall;
    function  Get_ProviderName: WideString; safecall;
    function  ImportKeyFromBlob(const piExchangeKey: ICryptoKey; const piBlob: ICryptoBlob; 
                                BlobType: CryptoBlobTypes): ICryptoKey; safecall;
    function  Get_ContainerName: WideString; safecall;
    function  CreateCRL(const pIssuerContext: ICryptoContext; ThisUpdate: TDateTime; 
                        NextUpdate: TDateTime): ICryptoCrl; safecall;
    function  CreateExponentOneKey: ICryptoKey; safecall;
    function  ImportRawKey(const piBlob: ICryptoBlob; Alg: Integer): ICryptoKey; safecall;
    property KeySpec: Integer read Get_KeySpec;
    property ProviderName: WideString read Get_ProviderName;
    property ContainerName: WideString read Get_ContainerName;
  end;

// *********************************************************************//
// DispIntf:  ICryptoContextDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2F612C16-2FBE-11D3-BF25-009027438003}
// *********************************************************************//
  ICryptoContextDisp = dispinterface
    ['{2F612C16-2FBE-11D3-BF25-009027438003}']
    function  GetUserKey(bKeyExchange: Integer): ICryptoKey; dispid 1;
    procedure CreateKeyPair(bKeyExchange: Integer); dispid 2;
    function  GenerateKey(Algorithm: OleVariant; BitSize: OleVariant): ICryptoKey; dispid 3;
    function  CreateHash(Algorithm: OleVariant): ICryptoHash; dispid 4;
    function  GenerateKeyFromPassword(const Password: WideString; HashAlgorithm: OleVariant; 
                                      KeyAlgorithm: OleVariant; BitSize: OleVariant): ICryptoKey; dispid 5;
    function  ImportKeyFromFile(const pExchangeKey: ICryptoKey; const Path: WideString; 
                                BlobType: CryptoBlobTypes): ICryptoKey; dispid 6;
    function  CreateCertificate(const pSignerContext: ICryptoContext; const Subject: WideString; 
                                NotBefore: TDateTime; NotAfter: TDateTime; 
                                bIncludePrivateKey: Integer): ICryptoCert; dispid 7;
    function  CreateMessage(TripleDes: OleVariant): ICryptoMessage; dispid 8;
    function  ImportKeyFromCert(const piCert: ICryptoCert): ICryptoKey; dispid 9;
    function  CreateEmptyKey(Algorithm: OleVariant; BitSize: OleVariant): ICryptoKey; dispid 10;
    procedure GetDefaultUserKey(var ppKey: ICryptoKey); dispid 11;
    function  GenerateCertificateRequest(const Subject: WideString): WideString; dispid 12;
    property KeySpec: Integer readonly dispid 13;
    function  CreateCertificateFromRequest(const pSignerContext: ICryptoContext; 
                                           const Request: WideString; NotBefore: TDateTime; 
                                           NotAfter: TDateTime): ICryptoCert; dispid 14;
    property ProviderName: WideString readonly dispid 15;
    function  ImportKeyFromBlob(const piExchangeKey: ICryptoKey; const piBlob: ICryptoBlob; 
                                BlobType: CryptoBlobTypes): ICryptoKey; dispid 16;
    property ContainerName: WideString readonly dispid 17;
    function  CreateCRL(const pIssuerContext: ICryptoContext; ThisUpdate: TDateTime; 
                        NextUpdate: TDateTime): ICryptoCrl; dispid 18;
    function  CreateExponentOneKey: ICryptoKey; dispid 19;
    function  ImportRawKey(const piBlob: ICryptoBlob; Alg: Integer): ICryptoKey; dispid 20;
  end;

// *********************************************************************//
// Interface: ICryptoKey
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B72DF075-28A4-11D3-BF19-009027438003}
// *********************************************************************//
  ICryptoKey = interface(IDispatch)
    ['{B72DF075-28A4-11D3-BF19-009027438003}']
    procedure EncryptFile(const InputPath: WideString; const OutputPath: WideString); safecall;
    procedure DecryptFile(const InputPath: WideString; const OutputPath: WideString); safecall;
    procedure ExportToFile(const pExchangeKey: ICryptoKey; const Path: WideString; 
                           BlobType: CryptoBlobTypes); safecall;
    function  EncryptBlob(BufferToEncrypt: OleVariant; bFinal: Integer): OleVariant; safecall;
    procedure DeriveFromPassword(const Password: WideString); safecall;
    function  DecryptBlob(BufferToDecrypt: OleVariant; bFinal: Integer): OleVariant; safecall;
    function  Get_Length: Integer; safecall;
    function  EncryptText(const Text: WideString): ICryptoBlob; safecall;
    function  DecryptText(const pBlob: ICryptoBlob): WideString; safecall;
    function  ExportToBlob(const piExchangeKey: ICryptoKey; BlobType: CryptoBlobTypes): ICryptoBlob; safecall;
    function  Get_Mode: CryptoCipherModes; safecall;
    procedure Set_Mode(pVal: CryptoCipherModes); safecall;
    function  Get_Padding: CryptoCipherPadding; safecall;
    procedure Set_Padding(pVal: CryptoCipherPadding); safecall;
    function  EncryptBinary(const piBlob: ICryptoBlob): ICryptoBlob; safecall;
    function  DecryptBinary(const piBlob: ICryptoBlob): ICryptoBlob; safecall;
    property Length: Integer read Get_Length;
    property Mode: CryptoCipherModes read Get_Mode write Set_Mode;
    property Padding: CryptoCipherPadding read Get_Padding write Set_Padding;
  end;

// *********************************************************************//
// DispIntf:  ICryptoKeyDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B72DF075-28A4-11D3-BF19-009027438003}
// *********************************************************************//
  ICryptoKeyDisp = dispinterface
    ['{B72DF075-28A4-11D3-BF19-009027438003}']
    procedure EncryptFile(const InputPath: WideString; const OutputPath: WideString); dispid 1;
    procedure DecryptFile(const InputPath: WideString; const OutputPath: WideString); dispid 2;
    procedure ExportToFile(const pExchangeKey: ICryptoKey; const Path: WideString; 
                           BlobType: CryptoBlobTypes); dispid 3;
    function  EncryptBlob(BufferToEncrypt: OleVariant; bFinal: Integer): OleVariant; dispid 4;
    procedure DeriveFromPassword(const Password: WideString); dispid 5;
    function  DecryptBlob(BufferToDecrypt: OleVariant; bFinal: Integer): OleVariant; dispid 6;
    property Length: Integer readonly dispid 7;
    function  EncryptText(const Text: WideString): ICryptoBlob; dispid 8;
    function  DecryptText(const pBlob: ICryptoBlob): WideString; dispid 9;
    function  ExportToBlob(const piExchangeKey: ICryptoKey; BlobType: CryptoBlobTypes): ICryptoBlob; dispid 10;
    property Mode: CryptoCipherModes dispid 11;
    property Padding: CryptoCipherPadding dispid 12;
    function  EncryptBinary(const piBlob: ICryptoBlob): ICryptoBlob; dispid 13;
    function  DecryptBinary(const piBlob: ICryptoBlob): ICryptoBlob; dispid 14;
  end;

// *********************************************************************//
// Interface: ICryptoBlob
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B24C5CC6-4FF8-11D3-BF60-009027438003}
// *********************************************************************//
  ICryptoBlob = interface(IDispatch)
    ['{B24C5CC6-4FF8-11D3-BF60-009027438003}']
    function  Get_Hex: WideString; safecall;
    procedure Set_Hex(const pVal: WideString); safecall;
    function  Get_Base64: WideString; safecall;
    procedure Set_Base64(const pVal: WideString); safecall;
    function  Get_Binary: OleVariant; safecall;
    procedure Set_Binary(pVal: OleVariant); safecall;
    procedure DumpToFile(const Path: WideString); safecall;
    procedure LoadFromFile(const Path: WideString); safecall;
    procedure DumpToRegistry(Hive: Integer; const Key: WideString; const Value: WideString); safecall;
    procedure LoadFromRegistry(Hive: Integer; const Key: WideString; const Value: WideString); safecall;
    property Hex: WideString read Get_Hex write Set_Hex;
    property Base64: WideString read Get_Base64 write Set_Base64;
    property Binary: OleVariant read Get_Binary write Set_Binary;
  end;

// *********************************************************************//
// DispIntf:  ICryptoBlobDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B24C5CC6-4FF8-11D3-BF60-009027438003}
// *********************************************************************//
  ICryptoBlobDisp = dispinterface
    ['{B24C5CC6-4FF8-11D3-BF60-009027438003}']
    property Hex: WideString dispid 0;
    property Base64: WideString dispid 2;
    property Binary: OleVariant dispid 3;
    procedure DumpToFile(const Path: WideString); dispid 4;
    procedure LoadFromFile(const Path: WideString); dispid 5;
    procedure DumpToRegistry(Hive: Integer; const Key: WideString; const Value: WideString); dispid 6;
    procedure LoadFromRegistry(Hive: Integer; const Key: WideString; const Value: WideString); dispid 7;
  end;

// *********************************************************************//
// Interface: ICryptoHash
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F837B983-2975-11D3-BF1A-009027438003}
// *********************************************************************//
  ICryptoHash = interface(IDispatch)
    ['{F837B983-2975-11D3-BF1A-009027438003}']
    function  Get_Value: ICryptoBlob; safecall;
    procedure AddText(const Text: WideString); safecall;
    function  DeriveKey(Algorithm: OleVariant; BitSize: OleVariant): ICryptoKey; safecall;
    procedure Reset; safecall;
    procedure AddFile(const Path: WideString); safecall;
    function  Sign(KeyExchange: Integer): ICryptoBlob; safecall;
    function  VerifySignature(const piBlob: ICryptoBlob; const piKey: ICryptoKey): WordBool; safecall;
    procedure AddBinary(const piBlob: ICryptoBlob); safecall;
    property Value: ICryptoBlob read Get_Value;
  end;

// *********************************************************************//
// DispIntf:  ICryptoHashDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F837B983-2975-11D3-BF1A-009027438003}
// *********************************************************************//
  ICryptoHashDisp = dispinterface
    ['{F837B983-2975-11D3-BF1A-009027438003}']
    property Value: ICryptoBlob readonly dispid 0;
    procedure AddText(const Text: WideString); dispid 1;
    function  DeriveKey(Algorithm: OleVariant; BitSize: OleVariant): ICryptoKey; dispid 4;
    procedure Reset; dispid 5;
    procedure AddFile(const Path: WideString); dispid 6;
    function  Sign(KeyExchange: Integer): ICryptoBlob; dispid 7;
    function  VerifySignature(const piBlob: ICryptoBlob; const piKey: ICryptoKey): WordBool; dispid 8;
    procedure AddBinary(const piBlob: ICryptoBlob); dispid 9;
  end;

// *********************************************************************//
// Interface: ICryptoMessage
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8F481223-33A9-11D3-BF2A-009027438003}
// *********************************************************************//
  ICryptoMessage = interface(IDispatch)
    ['{8F481223-33A9-11D3-BF2A-009027438003}']
    procedure AddRecipientCert(const piCert: ICryptoCert); safecall;
    procedure OpenToEncode(Size: Integer); safecall;
    function  Update(const pbData: WideString; cbData: Integer; bFinal: Integer): WideString; safecall;
    procedure SetSignerCert(const piCert: ICryptoCert); safecall;
    procedure OpenToSign(Size: Integer); safecall;
    procedure Close; safecall;
    function  EncryptText(const Text: WideString): WideString; safecall;
    function  SignText(const Text: WideString): WideString; safecall;
    function  DecryptText(const Text: WideString; const Context: WideString): WideString; safecall;
    function  VerifySignature(const Signature: WideString; const piHash: ICryptoHash; 
                              const piCert: ICryptoCert): WordBool; safecall;
    function  SignFile(const File_: WideString): WideString; safecall;
  end;

// *********************************************************************//
// DispIntf:  ICryptoMessageDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8F481223-33A9-11D3-BF2A-009027438003}
// *********************************************************************//
  ICryptoMessageDisp = dispinterface
    ['{8F481223-33A9-11D3-BF2A-009027438003}']
    procedure AddRecipientCert(const piCert: ICryptoCert); dispid 1;
    procedure OpenToEncode(Size: Integer); dispid 2;
    function  Update(const pbData: WideString; cbData: Integer; bFinal: Integer): WideString; dispid 3;
    procedure SetSignerCert(const piCert: ICryptoCert); dispid 4;
    procedure OpenToSign(Size: Integer); dispid 5;
    procedure Close; dispid 6;
    function  EncryptText(const Text: WideString): WideString; dispid 7;
    function  SignText(const Text: WideString): WideString; dispid 8;
    function  DecryptText(const Text: WideString; const Context: WideString): WideString; dispid 9;
    function  VerifySignature(const Signature: WideString; const piHash: ICryptoHash; 
                              const piCert: ICryptoCert): WordBool; dispid 10;
    function  SignFile(const File_: WideString): WideString; dispid 11;
  end;

// *********************************************************************//
// Interface: ICryptoCrl
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2A6866E6-63A2-11D3-BF89-009027438003}
// *********************************************************************//
  ICryptoCrl = interface(IDispatch)
    ['{2A6866E6-63A2-11D3-BF89-009027438003}']
    procedure AddRevocation(const SerialNumber: WideString; RevocationDate: TDateTime); safecall;
    procedure ExportToFile(const Path: WideString); safecall;
    function  Get_Issuer: ICryptoName; safecall;
    function  Get_EffectiveDate: TDateTime; safecall;
    function  Get_NextUpdate: TDateTime; safecall;
    function  Get_StoreName: WideString; safecall;
    function  Get_SerialNumbers: OleVariant; safecall;
    function  Get_Dates: OleVariant; safecall;
    property Issuer: ICryptoName read Get_Issuer;
    property EffectiveDate: TDateTime read Get_EffectiveDate;
    property NextUpdate: TDateTime read Get_NextUpdate;
    property StoreName: WideString read Get_StoreName;
    property SerialNumbers: OleVariant read Get_SerialNumbers;
    property Dates: OleVariant read Get_Dates;
  end;

// *********************************************************************//
// DispIntf:  ICryptoCrlDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2A6866E6-63A2-11D3-BF89-009027438003}
// *********************************************************************//
  ICryptoCrlDisp = dispinterface
    ['{2A6866E6-63A2-11D3-BF89-009027438003}']
    procedure AddRevocation(const SerialNumber: WideString; RevocationDate: TDateTime); dispid 1;
    procedure ExportToFile(const Path: WideString); dispid 2;
    property Issuer: ICryptoName readonly dispid 3;
    property EffectiveDate: TDateTime readonly dispid 4;
    property NextUpdate: TDateTime readonly dispid 5;
    property StoreName: WideString readonly dispid 6;
    property SerialNumbers: OleVariant readonly dispid 7;
    property Dates: OleVariant readonly dispid 8;
  end;

// *********************************************************************//
// Interface: ICryptoCRLs
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2A6866E8-63A2-11D3-BF89-009027438003}
// *********************************************************************//
  ICryptoCRLs = interface(IDispatch)
    ['{2A6866E8-63A2-11D3-BF89-009027438003}']
    function  Get_Count: Integer; safecall;
    function  Get__NewEnum: IUnknown; safecall;
    function  Item(Index: Integer): ICryptoCrl; safecall;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  ICryptoCRLsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2A6866E8-63A2-11D3-BF89-009027438003}
// *********************************************************************//
  ICryptoCRLsDisp = dispinterface
    ['{2A6866E8-63A2-11D3-BF89-009027438003}']
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
    function  Item(Index: Integer): ICryptoCrl; dispid 0;
  end;

// *********************************************************************//
// Interface: IXEncrypt
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {103B19CF-496F-4A3C-B975-129A56BA0646}
// *********************************************************************//
  IXEncrypt = interface(IDispatch)
    ['{103B19CF-496F-4A3C-B975-129A56BA0646}']
    function  OpenStore(const Name: WideString; LocalMachine: OleVariant): ICryptoStore; safecall;
    function  CreateBlob: ICryptoBlob; safecall;
    procedure DeleteKeySet(const Container: WideString; bMachineKeyset: Integer); safecall;
    function  OpenContext(const Container: WideString; bMachineKey: Integer; CreateNew: OleVariant): ICryptoContext; safecall;
    procedure SetDefaultProvider(const Name: WideString); safecall;
    function  ImportCertFromFile(const Path: WideString): ICryptoCert; safecall;
    function  ImportStoreFromFile(const Path: WideString): ICryptoStore; safecall;
    function  ImportCertFromBlob(const piBlob: ICryptoBlob): ICryptoCert; safecall;
    function  OpenStoreFromPFX(const Path: WideString; const Password: WideString): ICryptoStore; safecall;
    function  ImportCrlFromFile(const Path: WideString): ICryptoCrl; safecall;
    function  OpenContextEx(const Provider: WideString; const Container: WideString; 
                            bMachineKey: Integer; CreateNew: OleVariant): ICryptoContext; safecall;
    function  PickCertificate(const piStore: ICryptoStore; Flags: Integer; 
                              const Caption: WideString; const Message: WideString): ICryptoCert; safecall;
    function  Get_IncludeErrorCode: Integer; safecall;
    procedure Set_IncludeErrorCode(pVal: Integer); safecall;
    property IncludeErrorCode: Integer read Get_IncludeErrorCode write Set_IncludeErrorCode;
  end;

// *********************************************************************//
// DispIntf:  IXEncryptDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {103B19CF-496F-4A3C-B975-129A56BA0646}
// *********************************************************************//
  IXEncryptDisp = dispinterface
    ['{103B19CF-496F-4A3C-B975-129A56BA0646}']
    function  OpenStore(const Name: WideString; LocalMachine: OleVariant): ICryptoStore; dispid 1;
    function  CreateBlob: ICryptoBlob; dispid 2;
    procedure DeleteKeySet(const Container: WideString; bMachineKeyset: Integer); dispid 3;
    function  OpenContext(const Container: WideString; bMachineKey: Integer; CreateNew: OleVariant): ICryptoContext; dispid 4;
    procedure SetDefaultProvider(const Name: WideString); dispid 5;
    function  ImportCertFromFile(const Path: WideString): ICryptoCert; dispid 6;
    function  ImportStoreFromFile(const Path: WideString): ICryptoStore; dispid 7;
    function  ImportCertFromBlob(const piBlob: ICryptoBlob): ICryptoCert; dispid 8;
    function  OpenStoreFromPFX(const Path: WideString; const Password: WideString): ICryptoStore; dispid 9;
    function  ImportCrlFromFile(const Path: WideString): ICryptoCrl; dispid 10;
    function  OpenContextEx(const Provider: WideString; const Container: WideString; 
                            bMachineKey: Integer; CreateNew: OleVariant): ICryptoContext; dispid 11;
    function  PickCertificate(const piStore: ICryptoStore; Flags: Integer; 
                              const Caption: WideString; const Message: WideString): ICryptoCert; dispid 12;
    property IncludeErrorCode: Integer dispid 13;
  end;

// *********************************************************************//
// The Class CoCryptoManager provides a Create and CreateRemote method to          
// create instances of the default interface ICryptoManager exposed by              
// the CoClass CryptoManager. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCryptoManager = class
    class function Create: ICryptoManager;
    class function CreateRemote(const MachineName: string): ICryptoManager;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TCryptoManager
// Help String      : CryptoManager Class
// Default Interface: ICryptoManager
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TCryptoManagerProperties= class;
{$ENDIF}
  TCryptoManager = class(TOleServer)
  private
    FIntf:        ICryptoManager;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TCryptoManagerProperties;
    function      GetServerProperties: TCryptoManagerProperties;
{$ENDIF}
    function      GetDefaultInterface: ICryptoManager;
  protected
    procedure InitServerData; override;
    function  Get_IncludeErrorCode: Integer;
    procedure Set_IncludeErrorCode(pVal: Integer);
    function  Get_Expires: TDateTime;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: ICryptoManager);
    procedure Disconnect; override;
    function  OpenStore(const Name: WideString): ICryptoStore; overload;
    function  OpenStore(const Name: WideString; LocalMachine: OleVariant): ICryptoStore; overload;
    procedure LogonUser(const Domain: WideString; const Username: WideString; 
                        const Password: WideString); overload;
    procedure LogonUser(const Domain: WideString; const Username: WideString; 
                        const Password: WideString; Flag: OleVariant); overload;
    procedure RevertToSelf;
    function  OpenContext(const Container: WideString; bMachineKey: Integer): ICryptoContext; overload;
    function  OpenContext(const Container: WideString; bMachineKey: Integer; CreateNew: OleVariant): ICryptoContext; overload;
    function  ImportCertFromFile(const Path: WideString): ICryptoCert;
    procedure DeleteKeySet(const Container: WideString; bMachineKeyset: Integer);
    function  ImportStoreFromFile(const Path: WideString): ICryptoStore;
    procedure SetDefaultProvider(const Name: WideString);
    function  CreateBlob: ICryptoBlob;
    function  ImportCrlFromFile(const Path: WideString): ICryptoCrl;
    function  ImportCertFromBlob(const piBlob: ICryptoBlob): ICryptoCert;
    function  OpenStoreFromPFX(const Path: WideString; const Password: WideString): ICryptoStore;
    function  OpenContextEx(const Provider: WideString; const Container: WideString; 
                            bMachineKey: Integer): ICryptoContext; overload;
    function  OpenContextEx(const Provider: WideString; const Container: WideString; 
                            bMachineKey: Integer; CreateNew: OleVariant): ICryptoContext; overload;
    function  CreateMemoryStore: ICryptoStore;
    property  DefaultInterface: ICryptoManager read GetDefaultInterface;
    property Expires: TDateTime read Get_Expires;
    property IncludeErrorCode: Integer read Get_IncludeErrorCode write Set_IncludeErrorCode;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TCryptoManagerProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TCryptoManager
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TCryptoManagerProperties = class(TPersistent)
  private
    FServer:    TCryptoManager;
    function    GetDefaultInterface: ICryptoManager;
    constructor Create(AServer: TCryptoManager);
  protected
    function  Get_IncludeErrorCode: Integer;
    procedure Set_IncludeErrorCode(pVal: Integer);
    function  Get_Expires: TDateTime;
  public
    property DefaultInterface: ICryptoManager read GetDefaultInterface;
  published
    property IncludeErrorCode: Integer read Get_IncludeErrorCode write Set_IncludeErrorCode;
  end;
{$ENDIF}



// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TXEncrypt
// Help String      : Persits Software XEncrypt
// Default Interface: IXEncrypt
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
  TXEncrypt = class(TOleControl)
  private
    FIntf: IXEncrypt;
    function  GetControlInterface: IXEncrypt;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
  public
    function  OpenStore(const Name: WideString): ICryptoStore; overload;
    function  OpenStore(const Name: WideString; LocalMachine: OleVariant): ICryptoStore; overload;
    function  CreateBlob: ICryptoBlob;
    procedure DeleteKeySet(const Container: WideString; bMachineKeyset: Integer);
    function  OpenContext(const Container: WideString; bMachineKey: Integer): ICryptoContext; overload;
    function  OpenContext(const Container: WideString; bMachineKey: Integer; CreateNew: OleVariant): ICryptoContext; overload;
    procedure SetDefaultProvider(const Name: WideString);
    function  ImportCertFromFile(const Path: WideString): ICryptoCert;
    function  ImportStoreFromFile(const Path: WideString): ICryptoStore;
    function  ImportCertFromBlob(const piBlob: ICryptoBlob): ICryptoCert;
    function  OpenStoreFromPFX(const Path: WideString; const Password: WideString): ICryptoStore;
    function  ImportCrlFromFile(const Path: WideString): ICryptoCrl;
    function  OpenContextEx(const Provider: WideString; const Container: WideString; 
                            bMachineKey: Integer): ICryptoContext; overload;
    function  OpenContextEx(const Provider: WideString; const Container: WideString; 
                            bMachineKey: Integer; CreateNew: OleVariant): ICryptoContext; overload;
    function  PickCertificate(const piStore: ICryptoStore; Flags: Integer; 
                              const Caption: WideString; const Message: WideString): ICryptoCert;
    property  ControlInterface: IXEncrypt read GetControlInterface;
    property  DefaultInterface: IXEncrypt read GetControlInterface;
  published
    property IncludeErrorCode: Integer index 13 read GetIntegerProp write SetIntegerProp stored False;
  end;

procedure Register;

implementation

uses ComObj;

class function CoCryptoManager.Create: ICryptoManager;
begin
  Result := CreateComObject(CLASS_CryptoManager) as ICryptoManager;
end;

class function CoCryptoManager.CreateRemote(const MachineName: string): ICryptoManager;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CryptoManager) as ICryptoManager;
end;

procedure TCryptoManager.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{B72DF070-28A4-11D3-BF19-009027438003}';
    IntfIID:   '{B72DF06F-28A4-11D3-BF19-009027438003}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TCryptoManager.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as ICryptoManager;
  end;
end;

procedure TCryptoManager.ConnectTo(svrIntf: ICryptoManager);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TCryptoManager.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TCryptoManager.GetDefaultInterface: ICryptoManager;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TCryptoManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TCryptoManagerProperties.Create(Self);
{$ENDIF}
end;

destructor TCryptoManager.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TCryptoManager.GetServerProperties: TCryptoManagerProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function  TCryptoManager.Get_IncludeErrorCode: Integer;
begin
  Result := DefaultInterface.Get_IncludeErrorCode;
end;

procedure TCryptoManager.Set_IncludeErrorCode(pVal: Integer);
begin
  DefaultInterface.Set_IncludeErrorCode(pVal);
end;

function  TCryptoManager.Get_Expires: TDateTime;
begin
  Result := DefaultInterface.Get_Expires;
end;

function  TCryptoManager.OpenStore(const Name: WideString): ICryptoStore;
begin
  Result := DefaultInterface.OpenStore(Name, EmptyParam);
end;

function  TCryptoManager.OpenStore(const Name: WideString; LocalMachine: OleVariant): ICryptoStore;
begin
  Result := DefaultInterface.OpenStore(Name, LocalMachine);
end;

procedure TCryptoManager.LogonUser(const Domain: WideString; const Username: WideString; 
                                   const Password: WideString);
begin
  DefaultInterface.LogonUser(Domain, Username, Password, EmptyParam);
end;

procedure TCryptoManager.LogonUser(const Domain: WideString; const Username: WideString; 
                                   const Password: WideString; Flag: OleVariant);
begin
  DefaultInterface.LogonUser(Domain, Username, Password, Flag);
end;

procedure TCryptoManager.RevertToSelf;
begin
  DefaultInterface.RevertToSelf;
end;

function  TCryptoManager.OpenContext(const Container: WideString; bMachineKey: Integer): ICryptoContext;
begin
  Result := DefaultInterface.OpenContext(Container, bMachineKey, EmptyParam);
end;

function  TCryptoManager.OpenContext(const Container: WideString; bMachineKey: Integer; 
                                     CreateNew: OleVariant): ICryptoContext;
begin
  Result := DefaultInterface.OpenContext(Container, bMachineKey, CreateNew);
end;

function  TCryptoManager.ImportCertFromFile(const Path: WideString): ICryptoCert;
begin
  Result := DefaultInterface.ImportCertFromFile(Path);
end;

procedure TCryptoManager.DeleteKeySet(const Container: WideString; bMachineKeyset: Integer);
begin
  DefaultInterface.DeleteKeySet(Container, bMachineKeyset);
end;

function  TCryptoManager.ImportStoreFromFile(const Path: WideString): ICryptoStore;
begin
  Result := DefaultInterface.ImportStoreFromFile(Path);
end;

procedure TCryptoManager.SetDefaultProvider(const Name: WideString);
begin
  DefaultInterface.SetDefaultProvider(Name);
end;

function  TCryptoManager.CreateBlob: ICryptoBlob;
begin
  Result := DefaultInterface.CreateBlob;
end;

function  TCryptoManager.ImportCrlFromFile(const Path: WideString): ICryptoCrl;
begin
  Result := DefaultInterface.ImportCrlFromFile(Path);
end;

function  TCryptoManager.ImportCertFromBlob(const piBlob: ICryptoBlob): ICryptoCert;
begin
  Result := DefaultInterface.ImportCertFromBlob(piBlob);
end;

function  TCryptoManager.OpenStoreFromPFX(const Path: WideString; const Password: WideString): ICryptoStore;
begin
  Result := DefaultInterface.OpenStoreFromPFX(Path, Password);
end;

function  TCryptoManager.OpenContextEx(const Provider: WideString; const Container: WideString; 
                                       bMachineKey: Integer): ICryptoContext;
begin
  Result := DefaultInterface.OpenContextEx(Provider, Container, bMachineKey, EmptyParam);
end;

function  TCryptoManager.OpenContextEx(const Provider: WideString; const Container: WideString; 
                                       bMachineKey: Integer; CreateNew: OleVariant): ICryptoContext;
begin
  Result := DefaultInterface.OpenContextEx(Provider, Container, bMachineKey, CreateNew);
end;

function  TCryptoManager.CreateMemoryStore: ICryptoStore;
begin
  Result := DefaultInterface.CreateMemoryStore;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TCryptoManagerProperties.Create(AServer: TCryptoManager);
begin
  inherited Create;
  FServer := AServer;
end;

function TCryptoManagerProperties.GetDefaultInterface: ICryptoManager;
begin
  Result := FServer.DefaultInterface;
end;

function  TCryptoManagerProperties.Get_IncludeErrorCode: Integer;
begin
  Result := DefaultInterface.Get_IncludeErrorCode;
end;

procedure TCryptoManagerProperties.Set_IncludeErrorCode(pVal: Integer);
begin
  DefaultInterface.Set_IncludeErrorCode(pVal);
end;

function  TCryptoManagerProperties.Get_Expires: TDateTime;
begin
  Result := DefaultInterface.Get_Expires;
end;

{$ENDIF}

procedure TXEncrypt.InitControlData;
const
  CControlData: TControlData2 = (
    ClassID: '{F9463571-87CB-4A90-A1AC-2284B7F5AF4E}';
    EventIID: '';
    EventCount: 0;
    EventDispIDs: nil;
    LicenseKey: nil (*HR:$80040154*);
    Flags: $00000000;
    Version: 401);
begin
  ControlData := @CControlData;
end;

procedure TXEncrypt.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as IXEncrypt;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TXEncrypt.GetControlInterface: IXEncrypt;
begin
  CreateControl;
  Result := FIntf;
end;

function  TXEncrypt.OpenStore(const Name: WideString): ICryptoStore;
begin
  Result := DefaultInterface.OpenStore(Name, EmptyParam);
end;

function  TXEncrypt.OpenStore(const Name: WideString; LocalMachine: OleVariant): ICryptoStore;
begin
  Result := DefaultInterface.OpenStore(Name, LocalMachine);
end;

function  TXEncrypt.CreateBlob: ICryptoBlob;
begin
  Result := DefaultInterface.CreateBlob;
end;

procedure TXEncrypt.DeleteKeySet(const Container: WideString; bMachineKeyset: Integer);
begin
  DefaultInterface.DeleteKeySet(Container, bMachineKeyset);
end;

function  TXEncrypt.OpenContext(const Container: WideString; bMachineKey: Integer): ICryptoContext;
begin
  Result := DefaultInterface.OpenContext(Container, bMachineKey, EmptyParam);
end;

function  TXEncrypt.OpenContext(const Container: WideString; bMachineKey: Integer; 
                                CreateNew: OleVariant): ICryptoContext;
begin
  Result := DefaultInterface.OpenContext(Container, bMachineKey, CreateNew);
end;

procedure TXEncrypt.SetDefaultProvider(const Name: WideString);
begin
  DefaultInterface.SetDefaultProvider(Name);
end;

function  TXEncrypt.ImportCertFromFile(const Path: WideString): ICryptoCert;
begin
  Result := DefaultInterface.ImportCertFromFile(Path);
end;

function  TXEncrypt.ImportStoreFromFile(const Path: WideString): ICryptoStore;
begin
  Result := DefaultInterface.ImportStoreFromFile(Path);
end;

function  TXEncrypt.ImportCertFromBlob(const piBlob: ICryptoBlob): ICryptoCert;
begin
  Result := DefaultInterface.ImportCertFromBlob(piBlob);
end;

function  TXEncrypt.OpenStoreFromPFX(const Path: WideString; const Password: WideString): ICryptoStore;
begin
  Result := DefaultInterface.OpenStoreFromPFX(Path, Password);
end;

function  TXEncrypt.ImportCrlFromFile(const Path: WideString): ICryptoCrl;
begin
  Result := DefaultInterface.ImportCrlFromFile(Path);
end;

function  TXEncrypt.OpenContextEx(const Provider: WideString; const Container: WideString; 
                                  bMachineKey: Integer): ICryptoContext;
begin
  Result := DefaultInterface.OpenContextEx(Provider, Container, bMachineKey, EmptyParam);
end;

function  TXEncrypt.OpenContextEx(const Provider: WideString; const Container: WideString; 
                                  bMachineKey: Integer; CreateNew: OleVariant): ICryptoContext;
begin
  Result := DefaultInterface.OpenContextEx(Provider, Container, bMachineKey, CreateNew);
end;

function  TXEncrypt.PickCertificate(const piStore: ICryptoStore; Flags: Integer; 
                                    const Caption: WideString; const Message: WideString): ICryptoCert;
begin
  Result := DefaultInterface.PickCertificate(piStore, Flags, Caption, Message);
end;

procedure Register;
begin
  RegisterComponents('ActiveX',[TXEncrypt]);
  RegisterComponents('ActiveX',[TCryptoManager]);
end;

end.
