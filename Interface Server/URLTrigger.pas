unit URLTrigger;

interface

uses
  ComObj, FiveWebUtils_TLB;

type
  TTURLTrigger = class(TAutoObject, ITURLTrigger)
  protected
    function Trig(const URL: WideString): WordBool; safecall;
  end;

implementation

uses
  ComServ, WinInet;

function TTURLTrigger.Trig(const URL: WideString): WordBool;
  var
    Handle    : HINTERNET;
    ReqHandle : HINTERNET;
  begin
    Handle := InternetOpen( 'URLTrigger', INTERNET_OPEN_TYPE_DIRECT , nil, nil, 0 );
    if Handle <> nil
      then
        begin
          ReqHandle := InternetOpenUrlW( Handle, PWideChar(URL), nil, 0, INTERNET_FLAG_DONT_CACHE or INTERNET_FLAG_RELOAD, 0 );
          if ReqHandle <> nil
            then
              begin
                InternetCloseHandle( ReqHandle );
                result := true
              end
            else result := false;
          InternetCloseHandle( Handle );
        end
      else result := false;
  end;

initialization
  TAutoObjectFactory.Create(ComServer, TTURLTrigger, Class_TURLTrigger, ciMultiInstance);
end.
