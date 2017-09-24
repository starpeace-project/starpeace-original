unit Unit1;

interface

uses
  ComObj, ActiveX, Mailo_TLB, StdVcl;

type
  TMuyMilo = class(TAutoObject, IMuyMilo)
  protected
    function Get_Name: WideString; safecall;
    { Protected declarations }
  end;

implementation

uses ComServ;

function TMuyMilo.Get_Name: WideString;
  begin
    result := 'La peste';
  end;

initialization
  TAutoObjectFactory.Create(ComServer, TMuyMilo, Class_MuyMilo,
    ciMultiInstance, tmApartment);
end.
