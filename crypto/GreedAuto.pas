unit GreedAuto;

interface

uses
  ComObj, ActiveX, Greed_TLB, StdVcl;

type
  TOTK = class(TAutoObject, IOTK)
  protected
    function Connect(const addr: WideString; port: Integer): WordBool;
      safecall;
    function Cypher(const text: WideString): WideString; safecall;
    { Protected declarations }
  end;

implementation

uses ComServ;

function TOTK.Connect(const addr: WideString; port: Integer): WordBool;
  begin
    result := false;
  end;

function TOTK.Cypher(const text: WideString): WideString;
  begin
    result := text;
  end;

initialization
  TAutoObjectFactory.Create(ComServer, TOTK, Class_OTK,
    ciMultiInstance, tmBoth);
end.
