unit CrackAuto;

interface

uses
  ComObj, Crack_TLB;

type
  TTestCrack = class(TAutoObject, ITestCrack)
  protected
    function GetCrack: WideString; safecall;
  end;

implementation

  uses ComServ;

  function TTestCrack.GetCrack: WideString;
    begin
      result := 'Kaka de kuka';
    end;

initialization

  TAutoObjectFactory.Create(ComServer, TTestCrack, Class_TestCrack, ciMultiInstance);

end.
