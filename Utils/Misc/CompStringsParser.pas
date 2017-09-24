unit CompStringsParser;

interface

  uses
    SysUtils;

  type
    TCharSet = set of char;

  const
    Spaces  : TCharSet = [#0, #9, ' ', #10, #13];
    LineEnd : TCharSet = [#13, #10];
                                                         
    // Compose string functions

    function SkipChar(const str : string; var pos : integer; ch : char) : boolean;
    function SkipBackChar(const str : string; var pos : integer; ch : char) : boolean;
    function SkipChars(const str : string; var pos : integer; chars : TCharSet) : boolean;
    function SkipBackChars(const str : string; var pos : integer; chars : TCharSet) : boolean;

    function GetNextString(const str : string; var pos : integer; sep : TCharSet) : string;
    function GetNextStringUpTo(const str : string; var pos : integer; sep : char) : string;
    function GetPrevStringUpTo(const str : string; var pos : integer; sep : char) : string;
    function GetNextQuotedString(const str : string; var pos : integer; QuoteChar : char) : string;

    function IsDigit (c : char) : boolean;
    function IsLetter(c : char) : boolean;

    function CreateStringOf(c : char; count : integer) : string;

implementation

  function SkipChars(const str : string; var pos : integer; chars : TCharSet) : boolean;
    var
      len : integer;
    begin
      len := length(str);
      while (pos < len) and (str[pos] in chars) do
        inc(pos);
      result := pos < len;
    end;

  function SkipBackChars(const str : string; var pos : integer; chars : TCharSet) : boolean;
    begin
      while (pos > 0) and (str[pos] in chars) do
        dec(pos);
      result := pos > 0;
    end;

  function SkipChar(const str : string; var pos : integer; ch : char) : boolean;
    var
      len : integer;
    begin
      len := length(str);
      while (pos < len) and (str[pos] <> ch) do
        inc(pos);
      result := pos < len;
    end;

  function SkipBackChar(const str : string; var pos : integer; ch : char) : boolean;
    begin
      while (pos > 0) and (str[pos] <> ch) do
        dec(pos);
      result := pos > 0;
    end;

  function GetNextString(const str : string; var pos : integer; sep : TCharSet) : string;
    var
      OldPos : integer;
      count  : integer;
      len    : integer;
    begin
      len := length(str);

      while (pos <= len) and (str[pos] in sep) do
        inc(pos);

      OldPos := pos;
      while (pos <= len) and not (str[pos] in sep) do
        inc(pos);

      count := pos - OldPos;
      if count > 0
        then
          begin
            SetLength(result, count);
            Move(str[OldPos], result[1], count);
          end
        else result := '';
    end;

  function GetNextStringUpTo(const str : string; var pos : integer; sep : char) : string;
    var
      OldPos : integer;
      count  : integer;
      len    : integer;
    begin
      len := length(str);

      if (pos <= len) and (str[pos] = sep)
        then inc(pos);

      OldPos := pos;
      while (pos <= len) and (str[pos] <> sep) do
        inc(pos);

      count := pos - OldPos;
      if count > 0
        then
          begin
            SetLength(result, count);
            Move(str[OldPos], result[1], count);
          end
        else result := '';
    end;

  function GetPrevStringUpTo(const str : string; var pos : integer; sep : char) : string;
    var
      OldPos : integer;
      count  : integer;
    begin
      if (pos > 0) and (str[pos] = sep)
        then dec(pos);

      OldPos := pos;
      while (pos > 0) and (str[pos] <> sep) do
        dec(pos);

      count := OldPos - pos;
      if count > 0
        then
          begin
            SetLength(result, count);
            Move(str[pos+1], result[1], count);
          end
        else result := '';
    end;

  function GetNextQuotedString(const str : string; var pos : integer; QuoteChar : char) : string;
    var
      len    : integer;
      OldPos : integer;
      count  : integer;
    begin
      len := length(str);
      while (pos < len) and (str[pos] <> QuoteChar) do
        inc(pos);
      if pos < len
        then
          begin
            inc(pos);
            OldPos := pos;
            while (pos < len) and (str[pos] <> QuoteChar) do
              inc(pos);
            count  :=  pos - OldPos;
            result := copy(str, OldPos, count);
          end
        else
          result := '';
    end;

  function IsDigit(c : char) : boolean;
    begin
      result := (byte(c) >= byte('0')) and (byte(c) <= byte('9'))
    end;

  function IsLetter(c : char) : boolean;
    begin
      result := ((byte(c) >= byte('A')) and (byte(c) <= byte('Z'))) or ((byte(c) >= byte('a')) and (byte(c) <= byte('z')));
    end;

  function CreateStringOf(c : char; count : integer) : string;
    begin
      if count > 0
        then
          begin
            SetLength(result, count);
            FillChar(result[1], count, c);
          end
        else
          result := '';
    end;

end.

