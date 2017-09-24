
unit FileToolsObj;

interface

uses
  ComObj, NewsFileUtils_TLB;

type
  TTools = class(TAutoObject, ITools)
  protected
    function DecodeNewsDate(const date: WideString): WideString; safecall;
    function EncodeNewsDate(const date: WideString): WideString; safecall;
  end;

implementation

uses ComServ, SysUtils;

function FixDate( date : string ) : string;
  var
    i : integer;
  begin
    result := date;
    for i := 0 to pred(length(date)) do
      if result[i] = '-'
        then result[i] := '/';
  end;

function TTools.DecodeNewsDate(const date: WideString): WideString;
  var
    dt      : TDateTime;
    y, m, d : word;
  begin
    try
      result := date;
    except
      result := date;
    end;
  end;

function TTools.EncodeNewsDate(const date: WideString): WideString;
  begin
    try
      result := date;
    except
      result := date;
    end;
  end;


initialization
  TAutoObjectFactory.Create(ComServer, TTools, Class_Tools, ciMultiInstance);
end.
