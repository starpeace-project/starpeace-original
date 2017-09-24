unit xx;

interface

uses
  ComObj, xx1_TLB;

type
  TPepito = class(TAutoObject, IPepito)
  protected
    function GetName: WideString; safecall;
  end;

implementation

uses ComServ;

function TPepito.GetName: WideString;
  begin
    result := 'The bull shit...'
  end;

initialization
  TAutoObjectFactory.Create(ComServer, TPepito, Class_Pepito, ciMultiInstance);
end.
