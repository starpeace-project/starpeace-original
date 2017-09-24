unit CacheNameUtils;

interface

  procedure TranslateChars(var str : string; fCh, tCh : char);
  function  GetObjectFolder(const World : string; X, Y : integer) : string;
  function  GetCoordWildcards(X, Y : Integer) : string;

implementation

  uses
    CacheObjects, SysUtils, SpecialChars;

  const
    ClearMask = integer(not((1 shl 6) - 1));

  procedure TranslateChars(var str : string; fCh, tCh : char);
    var
      i : integer;
    begin
      for i := 0 to pred(length(str)) do
        if pchar(str)[i] = fCh
          then pchar(str)[i] := tCh;
    end;

  function GetObjectFolder(const World : string; X, Y : integer) : string;
    begin
      result := GetCacheRootPath + 'Worlds\' + World + '\Map\' + IntToStr(X and ClearMask) + BackSlashChar + IntToStr(Y and ClearMask) + '\';
    end;

  function GetCoordWildcards(X, Y: Integer) : string;
    begin
      result := IntToStr(X) + BackSlashChar + IntToStr(Y) + BackSlashChar + '*';
    end;

end.
