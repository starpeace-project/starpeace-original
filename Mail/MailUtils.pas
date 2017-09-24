unit MailUtils;

interface

  uses
    SysUtils;

  const
    csInvalidMailAddrChars = ['\', '/', ':', '*', '?', '"', '<', '>', '|'];

  function  RemoveFullPath(const Path : string) : boolean;
  function  GenMessageId(Date : TDateTime) : string;
  procedure TranslateChars(var str : string; fCh, tCh : char);
  function  GetAccountPath(const World, Account : string) : string;
  function  ValidMailAddress(const Addr : string) : boolean;
  function  ExtractAddressAccount(const Addr : string) : string;
  function  ExistsAccountPath(const World, Addr : string) : boolean;

implementation

  uses
    Windows, FileCtrl, IniFiles, MailData, ShellAPI, CompStringsParser,
    BaseUtils;

  // Remove a full path

  function RemoveFullPath(const Path : string) : boolean;
    var
      FileOp : TSHFileOpStruct;
      tmp    : array[0..MAX_PATH] of char;
    begin
      fillchar(tmp, sizeof(tmp), 0);
      strpcopy(tmp, Path);
      // If Path is a folder the last '\' must be removed.
      if Path[length(Path)] = '\'
        then tmp[length(Path)-1] := #0;
      with FileOp do
        begin
          wFunc  := FO_DELETE;
          Wnd    := 0;
          pFrom  := tmp;
          pTo    := nil;
          fFlags := FOF_NOCONFIRMATION or FOF_SILENT;
          hNameMappings := nil;
        end;
      try
        if DirectoryExists(Path)
          then result := SHFileOperation( FileOp ) = 0
          else result := true;
      except
        result := false;
      end;
    end;

  function GenMessageId(Date : TDateTime) : string;
    begin
      result := DateTimeToAbc(EncodeDate(9999, 12, 31) - Date) + IntToAbc(random(25), 1);
    end;

  procedure TranslateChars(var str : string; fCh, tCh : char);
    var
      i : integer;
    begin
      for i := 1 to length(str) do
        if str[i] = fCh
          then str[i] := tCh;
    end;

  function GetAccountPath(const World, Account : string) : string;
    var
      aux : string;
    begin
      aux := Account;
      if pos('@', aux) = 0
        then result := GetMailRoot + 'Worlds\' + World + '\' + aux + '.' + World + '.net\'
        else
          begin
            TranslateChars(aux, '@', '.');
            result := GetMailRoot + 'Worlds\' + World + '\' + aux + '\';
          end;
    end;

  function ValidMailAddress(const Addr : string) : boolean;
    var
      len : integer;
      i   : integer;
    begin
      len := length(Addr);
      i   := 1;
      while (i <= len) and not (Addr[i] in csInvalidMailAddrChars) do
        inc(i);
      result := i > len;
    end;

  function ExtractAddressAccount(const Addr : string) : string;
    var
      p : integer;
    begin
      p := 1;
      result := GetNextStringUpTo(Addr, p, '@');
    end;

  function ExistsAccountPath(const World, Addr : string) : boolean;
    begin
      result := FileCtrl.DirectoryExists(GetAccountPath(World, Addr));
    end;

end.
