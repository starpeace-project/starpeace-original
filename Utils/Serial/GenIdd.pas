unit GenIdd;

interface

  const
    HighDigits      = 8;
    LowDigits       = 7;
    CheckDigits     = 1;
    NumberOfDigits  = HighDigits + LowDigits + CheckDigits;
    BlockSize       = 4;
    Separetor       = '-';
    BlueMagicNumber = 1;
    RedMagicNumber  = 11; // old - 7
    Epsilon         = 0.000001;
    MaxIddNumber    = 10000000;
    MaxClassNumber  = 0.5;
    KeySize         = NumberOfDigits + (NumberOfDigits div BlockSize - 1);

  type
    TCompressedKEY = array[0..Pred(NumberOfDigits div 2)] of byte;

  function GenUniqueIdd(const aCounter : integer; const aClassNumber : extended): string;
  function LighIddCheck(const aIdd: string): boolean;
  function HeavyIddCheck(const aIdd: string; const aClassNumber : extended): boolean;

  function TrimKEY(const aIdd: string) : TCompressedKEY;
  function ExpandKey(const aIdd: TCompressedKEY; out IsTrial : boolean) : string;

implementation

  uses SysUtils, Math{, AutoCheck};

  const
    MaxGarbage = 10;
    Garbage    : array[0..MaxGarbage - 1] of extended = ( 0.12786045554, 0.2225785038,
                                                          0.34015415325, 0.4119533409,
                                                          0.51205369088, 0.6156257852,
                                                          0.75768370009, 0.8823171039,
                                                          0.98521254468, 0.0696543522);

  function TransformIndex(const aIndex : integer): integer;
    begin
      result := round(power(10, HighDigits - 1) / (2 * pi)) + BlueMagicNumber * aIndex;
    end;

  function GenCheckStr(const aPattern : string): string;
    var
      aux, idx, old : integer;
    begin
      result := '';
      repeat
        old := RedMagicNumber;
        for idx := 1 to length(aPattern) do
          begin
            if aPattern[idx] in ['0'..'9']
              then aux := strtoint(aPattern[idx])
              else aux := byte(aPattern[idx]);
            old := (old + 3 * aux) mod 10;
          end;
        result := result + inttostr(old);
      until (length(result) >= CheckDigits);
    end;

  function FormatIdd(const aNumber : extended): string;
    var
      idx : integer;
      aux : string;
    begin
      try
        str(aNumber:HighDigits:LowDigits, aux);
        aux := concat(copy(aux, 1, HighDigits), copy(aux, HighDigits + 2, LowDigits));
        aux := aux + GenCheckStr(aux);
        result := '';
        for idx := 0 to (length(aux) div BlockSize) + 1 do
          result := result + copy(aux, idx * BlockSize + 1, BlockSize) + Separetor;
        idx := length(result);
        while (result[idx] = Separetor) do dec(idx);
        setlength(result, idx);
      except
        result := '';
      end;
    end;

  function GenSeed(const aCounter : integer; const aClassNumber : extended): extended;
    var
      key : extended;
    begin
      try
        key    := arcsin(aClassNumber - round(aClassNumber)) + Garbage[random(MaxGarbage)];
        result := TransformIndex(aCounter) * 2 * pi + key;
      except
        result := 0;
      end;
    end;

  function GenUniqueIdd(const aCounter : integer; const aClassNumber : extended): string;
    begin
      result := FormatIdd(GenSeed(aCounter, aClassNumber));
    end;

  function GetSeedFromIdd(const aIdd : string): extended;
    var
      cif, idx : integer;
      aux      : string;
      error    : integer;
    begin
      try
        cif := 0;
        for idx := 1 to length(aIdd) do
          begin
            if aIdd[idx] in ['0'..'9']
              then
                begin
                  aux := aux + aIdd[idx];
                  inc(cif);
                  if cif = HighDigits
                    then aux := aux + '.';
                end;
          end;
        val(aux, result, error);
      except
        result := 0;
      end;
    end;

  function LighIddCheck(const aIdd: string): boolean;
    var
      idx : integer;
      tst, aux : string;
    begin
      try
        aux := '';
        for idx := 1 to (length(aIdd) - CheckDigits) do
          if aIdd[idx] in ['0'..'9']
            then aux := aux + aIdd[idx];
        tst := GenCheckStr(aux);
        aux := copy(aIdd, length(aIdd) - CheckDigits + 1, CheckDigits);
        result := (tst = aux) and (Length(aIdd) = KeySize);
      except
        result := false;
      end;
    end;

  function _glass(const x: extended): extended;
    var
      aux : extended;
    begin
      result := abs(x);
      aux    := trunc(result / (2* pi));
      result := result - aux * 2 * pi;
      if x < 0
        then result := -result;
    end;

  function _stone(const x: extended): extended;
    var
      value, pow  : extended;
      fact, delta : extended;
      count : longint;
      sign  : integer;
    begin
      value  := _glass(x);
      result := value;
      pow    := value;
      fact   := 1;
      sign   := 1;
      count  := 1;
      repeat
        sign   := -sign;
        pow    := pow * value * value;
        count  := count + 2;
        fact   := fact * (count - 1) * count;
        delta  := sign * pow / fact;
        result := result + delta;
      until abs(delta) < epsilon;
    end;

  function HeavyIddCheck(const aIdd: string; const aClassNumber : extended): boolean;
    var
      x : extended;
      count : integer;
    begin
      count := 0;
      repeat
        try
          x := GetSeedFromIdd(aIdd) - Garbage[count];
          x := _stone(x);
          result := abs(x - aClassNumber) < epsilon;
          inc(count);
        except
          result := false;
        end;
      until result or (count = MaxGarbage);
      //Zero := Zero + 10*succ(random(12))*integer(not result);
    end;

  function TrimKEY(const aIdd: string) : TCompressedKEY;
    var
      i, Id : cardinal;
    begin
      if aIdd <> ''
        then
          begin
            ID := 1;
            for i := 0 to Pred(NumberOfDigits div 2) do
              begin
                Result[i] := byte(StrToInt(Copy(aIdd, ID, 2)));
                if Odd(i)
                  then ID := ID + 3
                  else ID := ID + 2
              end;
          end
        else FillChar(Result, SizeOf(Result), 0);
    end;

  function ExpandKey(const aIdd: TCompressedKEY; out IsTrial : boolean) : string;
    var
      aux : string;
      i, sum : cardinal;
    begin
      SetLength(Result, KeySize);
      Sum := 0;
      for i := 0 to Pred(NumberOfDigits div 2) do
        Sum := Sum + aIdd[i];
      Result := '';
      if Sum > 0
        then
          begin
            IsTrial := false;
            for i := 0 to Pred(NumberOfDigits div 2) do
              begin
                aux := IntToStr(aIdd[i]);
                if Length(aux) = 1
                  then aux := '0' + aux;
                Result := Result + Aux;
                if Odd(i) and (i < Pred(NumberOfDigits div 2))
                  then Result := Result + Separetor;
              end;
          end
        else IsTrial := true;
    end;

end.
