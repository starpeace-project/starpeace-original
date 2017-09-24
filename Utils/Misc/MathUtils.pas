unit MathUtils;

interface

  function Dist( x1, y1, x2, y2 : integer ) : integer;
  function min( a, b : integer ) : integer;
  function max( a, b : integer ) : integer;
  function minI64( a, b : int64) : int64;
  function maxI64( a, b : int64 ) : int64;
  function realmin( a, b : single ) : single;
  function realmax( a, b : single ) : single;
  function currmin( a, b : currency ) : currency;
  function currmax( a, b : currency ) : currency;
  function FormatMoney( money : currency ) : string;
  function FormatMoneyStr( str : string ) : string;
  function FormattedStrToMoney( str : string ) : currency;
  function intcond (cond : boolean; exp1, exp2 : integer) : integer;
  function realcond(cond : boolean; exp1, exp2 : single ) : single;
  function SmartRound(value : single) : integer;

implementation

  uses
    SysUtils;

  function Dist( x1, y1, x2, y2 : integer ) : integer;
    begin
      result := abs(x1 - x2) + abs(y1 - y2)
    end;

  function min( a, b : integer ) : integer;
    begin
      if a < b
        then result := a
        else result := b
    end;

  function max( a, b : integer ) : integer;
    begin
      if a > b
        then result := a
        else result := b
    end;

  function minI64(a, b : int64) : int64;
    begin
      if a < b
        then result := a
        else result := b
    end;

  function maxI64(a, b : int64) : int64;
    begin
      if a > b
        then result := a
        else result := b
    end;

  function realmin( a, b : single ) : single;
    begin
      if a < b
        then result := a
        else result := b
    end;

  function realmax( a, b : single ) : single;
    begin
      if a > b
        then result := a
        else result := b
    end;

  function currmin( a, b : currency ) : currency;
    begin
      if a < b
        then result := a
        else result := b
    end;

  function currmax( a, b : currency ) : currency;
    begin
      if a > b
        then result := a
        else result := b
    end;

  function FormatMoney( money : currency ) : string;
    var
      cents : currency;
    begin
      if money = 0
        then result := '$0'
        else
          if abs(money) > 1
            then
              begin
                result := Format('%.0n', [int(money)]);
                if money >= 0
                  then insert('$', result, 1)
                  else insert('$', result, 2);
              end
            else
              begin
                cents := int(100*money);
                if cents = 0
                  then result := '$0'
                  else result := Format('%.0n¢', [cents]);
              end;
    end;

  function FormatMoneyStr( str : string ) : string;
    begin
      if str <> ''
        then
          try
            result := FormatMoney(StrToCurr(str));
          except
            result := FormatMoney(0);
          end
        else result := FormatMoney(0);
    end;

  function FormattedStrToMoney( str : string ) : currency;
    var                                               
      i : integer;
    begin
      for i := length(str) downto 1 do
        if str[i] in ['$', ',', ' ', 'A'..'Z']
          then system.delete( str, i, 1 );
      result := StrToCurr( str );
    end;

  function intcond(cond : boolean; exp1, exp2 : integer) : integer;
    begin
      if cond
        then result := exp1
        else result := exp2;
    end;

  function realcond(cond : boolean; exp1, exp2 : single ) : single;
    begin
      if cond
        then result := exp1
        else result := exp2;
    end;

  function SmartRound(value : single) : integer;
    begin
      if value > 0
        then result := max(1, round(value))
        else result := 0;
    end;

end.
