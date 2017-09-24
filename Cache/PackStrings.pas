unit PackStrings;

interface

  function IsPacked(const str : WideString) : boolean;
  function PackStr(const str : string) : WideString;
  function UnpackStr(const str : WideString) : string;

implementation

  function IsPacked(const str : WideString) : boolean;
    begin
      result := (length(str) > 0) and (pchar(str)[1] <> #0);
    end;

  function PackStr(const str : string) : WideString;
    var
      aux : pchar absolute result;
      len : integer;
      i   : integer;
    begin
      len := length(str);
      if len <> 0
        then
          begin
            if len and 1 <> 0
              then SetLength(result, len div 2 + 1)
              else SetLength(result, len div 2);
            for i := 0 to pred(length(str)) do
              aux[i] := pchar(str)[i];
            aux[length(str)] := #0;
          end
        else result := '';
    end;

  function UnpackStr(const str : WideString) : string;
    begin
      result := pchar(str);
    end;


end.
