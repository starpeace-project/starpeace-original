unit SrvObject;

interface

  {$M+}
  type
    TXObject =
      class
        published
          function GetStr(size : integer) : OleVariant;
          function GetStrLen(str : widestring) : OleVariant;
          function GetStrPos(str1, str2 : widestring) : OleVariant;
      end;
  {$M-}

implementation

  // TXObject

  function TXObject.GetStr(size : integer) : OleVariant;
    var
      str : string;
    begin
      if size > 0
        then
          begin
            SetLength(str, size);
            FillChar(str[1], size, 65);
          end
        else str := '';
      result := str;
    end;

  function TXObject.GetStrLen(str : widestring) : OleVariant;
    begin
      result := Length(str);
    end;

  function TXObject.GetStrPos(str1, str2 : widestring) : OleVariant;
    begin
      result := pos(str1, str2);
    end;

end.
