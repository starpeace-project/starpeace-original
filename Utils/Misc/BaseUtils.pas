unit BaseUtils;

interface

  // ABC is the numerical base which figures are A,B,C,...,Z

  function IntToAbc(value, digits : integer) : string;
  function DateToAbc(date : TDateTime) : string;
  function TimeToAbc(date : TDateTime) : string;
  function DateTimeToAbc(date : TDateTime) : string;


implementation

  uses
    SysUtils;

  const
    abcBase = ord('Z') - ord('A') + 1;
    abcZero = 'A';

  function IntToAbc(value, digits : integer) : string;
    var
      m : integer;
      a : array[0..32] of char;
      i : integer;
    begin
      fillchar(a, sizeof(a), 0);
      i := 0;
      repeat
        m     := value mod abcBase;
        value := value div abcBase;
        a[i]  := char(ord('A') + m);
        inc(i);
      until value = 0;
      if i < digits
        then
          begin
            move(a[0], a[digits-i], i);
            fillchar(a, digits-i, abcZero);
          end;
      result := a;
    end;

  function DateToAbc(date : TDateTime) : string;
    var
      Year, Month, Day : Word;
    begin
      DecodeDate(Date, Year, Month, Day);
      result := IntToHex(Year, 4) + IntToHex(Month, 1) + IntToHex(Day, 2);
    end;

  function TimeToAbc(date : TDateTime) : string;
    var
      Hour, Min, Sec, MSec : Word;
    begin
      DecodeTime(Date, Hour, Min, Sec, MSec);
      result := IntToHex(Hour, 2)  + IntToHex(Min, 2) + IntToHex(Sec, 2) + IntToHex(MSec, 2);
    end;

  function DateTimeToAbc(date : TDateTime) : string;
    begin
      result := DateToAbc(date) + TimeToAbc(date);
    end;

end.
