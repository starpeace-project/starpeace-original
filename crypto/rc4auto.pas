unit rc4auto;

interface

uses
  ComObj, ActiveX, spcrypt_TLB, StdVcl, rc4;

type
  TRC4Auto = class(TAutoObject, IRC4)
  protected
    procedure Apply(const text: WideString; inHex, outHex: Integer); safecall;
    function  Get_Key: WideString; safecall;
    procedure Set_Key(const Value: WideString); safecall;
    function  Get_Result: WideString; safecall;
    { Protected declarations }
  public
    procedure Initialize; override;
  private
    fRC4 : TRC4;
    fResult : string;
  end;

implementation

uses ComServ;

procedure TRC4Auto.Initialize;
  begin
    inherited;
    fRC4 := TRC4.Create;
  end;

function TRC4Auto.Get_Key: widestring;
  begin
    result := '';
  end;

procedure TRC4Auto.Apply(const text: WideString; inHex, outHex: Integer);
  var
    aux : string;
  begin
    if fRC4 <> nil
      then
        begin
          if inHex <> 0
            then aux := fRC4.toBin(text)
            else aux := text;
          aux := fRC4.Apply(aux);
          if outHex <> 0
            then fResult := fRC4.toHex(aux)
            else fResult := aux;
        end
      else fResult := '';
  end;

procedure TRC4Auto.Set_Key(const Value: widestring);
  begin
    if fRC4 <> nil
      then fRC4.Key := Value;
  end;

function TRC4Auto.Get_Result: WideString;
  begin
    result := fResult;
  end;

initialization

  TAutoObjectFactory.Create(ComServer, TRC4Auto, Class_RC4, ciMultiInstance, tmApartment);

end.
