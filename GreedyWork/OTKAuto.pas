unit OTKAuto;

interface

uses
  ComObj, ActiveX, GreedApi_TLB, StdVcl, SyncObjs, rc4;

type
  TOTK = class(TAutoObject, IOTK)
  protected
    function Cypher(const text: WideString): WideString; safecall;
    function Get_ErrorCode: Integer; safecall;
    { Protected declarations }
  end;

implementation

  uses
    Windows, SysUtils, Classes, ComServ, RDOInterfaces, RDOObjectProxy, WinSockRDOConnection,
    Registry;

  // Global vars

  const
    ERROR_NONE    = 0;
    ERROR_REGFAIL = 1;
    ERROR_CYPHR   = 2;
    ERROR_DSFAIL  = 3;
    ERROR_UNKNOWN = 4;

  var
    theLock   : TCriticalSection = nil;
    theCypher : TRC4             = nil;
    theKeyId  : integer          = -1;
    errorCode : integer          = ERROR_NONE;


  // Global methods

  function GetDSProxy(addr : string; port, timeout : integer) : OleVariant;
    var
      DSCnnt  : IRDOConnectionInit;
      DSProxy : OleVariant;
      sId     : integer;
    begin
      result := Unassigned;
      DSCnnt := TWinSockRDOConnection.Create('Main');
      DSCnnt.Server := addr;
      DSCnnt.Port := port;
      if DSCnnt.Connect(timeout)
        then
          begin
            DSProxy := TRDOObjectProxy.Create as IDispatch;
            DSProxy.SetConnection(DSCnnt);
            DSProxy.TimeOut := timeout;
            if DSProxy.BindTo('DirectoryServer')
              then
                begin
                  sId := DSProxy.RDOOpenSession;
                  if (sId <> 0) and DSProxy.BindTo(sId)
                    then result := DSProxy;
                end;
          end;
    end;

  function CypherSimKey(key : string; var cypKey : string) : boolean;
    var
      Reg : TRegistry;
      CM  : OleVariant;
      Ctx : OleVariant;
      Msg : OleVariant;
      pth : string;
    begin
      try
        Reg := TRegistry.Create;
        try
          Reg.RootKey := HKEY_LOCAL_MACHINE;
          if Reg.OpenKey('\Software\Wow6432Node\Starpeace\Five\Common', false)
            then
              begin
                pth := Reg.ReadString('PBK_CERT');
                CM  := CreateOleObject('Persits.CryptoManager');
                Ctx := CM.OpenContextEx('Microsoft Enhanced Cryptographic Provider v1.0', '', true);
                Msg := Ctx.CreateMessage(true);
                Msg.AddRecipientCert(CM.ImportCertFromFile(pth));
                cypKey := Msg.EncryptText(key);
                result := true;
              end
            else
              begin
                result := false;
                errorCode := ERROR_REGFAIL;
              end;
          finally
            Reg.Free;
          end;
      except
        errorCode := ERROR_CYPHR;
        result := false;
      end;
    end;

  procedure InitCypher;
    var
      Reg     : TRegistry;
      DSProxy : OleVariant;
      simKey  : string;
      cyphKey : string;
      addr    : WideString;
      port    : Integer;
    begin
      try
        theLock.Enter;
        try
          Reg := TRegistry.Create;
          try
            Reg.RootKey := HKEY_LOCAL_MACHINE;
            if Reg.OpenKey('\Software\Wow6432Node\Starpeace\Five\Common', false)
              then
                begin
                  addr := Reg.ReadString('DSAddr');
                  port := Reg.ReadInteger('DSPort');
                end
              else
                begin
                  addr := '';
                  port := 0;
                  errorCode := ERROR_REGFAIL;
                end;
          finally
            Reg.Free;
          end;

          if (addr <> '') and (port <> 0)
            then DSProxy := GetDSProxy(addr, port, 20000)
            else DSProxy := Unassigned;

          if not VarIsEmpty(DSProxy)
            then
              begin
                // Create the cypher
                theCypher := TRC4.Create;

                // generate the OTK
                simKey := theCypher.genKey(32);

                // cypher the key
                if CypherSimKey(simKey, cyphKey)
                  then
                    begin
                      // store the key
                      theKeyId := DSProxy.RDOStoreKey(cyphKey);
                      if theKeyId >= 0
                        then
                          begin
                            theCypher := TRC4.Create;
                            theCypher.Key := simKey;
                          end
                        else
                          begin
                            theCypher.Free;
                            theCypher := nil;
                            errorCode := ERROR_DSFAIL;
                          end;
                    end
                  else
                    begin
                      theCypher.Free;
                      theCypher := nil;
                    end;
              end;
        finally
          theLock.Leave;
        end;
      except
        theCypher.Free;
        theCypher := nil;
        errorCode := ERROR_UNKNOWN;
      end;
    end;

  function DecorateText(text : string) : string;
    var
      txtLen : integer;
      decLen : integer;
      i      : integer;
    begin
      result := text;
      txtLen := length(text);
      decLen := length(DecChars);
      for i := 1 to txtLen do
        insert(DecChars[1 + random(decLen)], result, random(1 + length(result)));
    end;


  // TOTK

  function TOTK.Cypher(const text: WideString): WideString;
    var
      aux : string;
    begin
      theLock.Enter;
      try
        if theCypher = nil
          then InitCypher;
        if theCypher <> nil
          then
            begin
              aux    := DecorateText(text);
              result := theCypher.toHex(IntToStr(theKeyId) + '|') + theCypher.toHex(theCypher.Apply(aux));
            end
          else result := '';
      finally
        theLock.Leave;
      end;
    end;

  function TOTK.Get_ErrorCode: Integer;
    begin
      result := errorCode;
    end;

initialization

  theLock := TCriticalSection.Create;

  TAutoObjectFactory.Create(ComServer, TOTK, Class_OTK, ciMultiInstance, tmApartment);

finalization

  theLock.Free;

end.
