unit Literals;

interface

  procedure SetBasePath(const transfilespath : string);
  procedure SetLanguage(languageid : integer);
  function  GetLiteral(const id : string) : string;
  function  GetLiteralDefault( Literal, Default : string ) : string ;
  function  GetFormattedLiteral(const id : string; const Args : array of const) : string;

implementation

  uses
    Classes, Windows, SysUtils;

  const
    cLiteralsTableName = 'Literals';

  var
    vLiterals       : TStringList;
    vTransFilesPath : string;

  procedure SetBasePath(const transfilespath : string);
    begin
      vTransFilesPath := transfilespath;
      if (vTransFilesPath <> '') and (vTransFilesPath[length(vTransFilesPath)] <> '\')
        then vTransFilesPath := vTransFilesPath + '\';
    end;

  procedure SetLanguage(languageid : integer);
    begin
      if FileExists(vTransFilesPath + IntToStr(languageid) + '\' + cLiteralsTableName + '.tln')
        then
          begin
            vLiterals.Clear;
            vLiterals.LoadFromFile(vTransFilesPath + IntToStr(languageid) + '\' + cLiteralsTableName + '.tln');
          end;
    end;

  function GetLiteralDefault( Literal, Default : string ) : string ;
    begin
      Result := GetLiteral(Literal);
      if Result = ''
        then Result := Default;
    end;

  function GetLiteral(const id : string) : string;
    begin
      Result := vLiterals.Values[id];
    end;

  function GetFormattedLiteral(const id : string; const Args : array of const) : string;
    begin
      try
        Result := Format(GetLiteral(id), Args);
      except
        Result := GetLiteral(id);
      end;
    end;

initialization
  vLiterals := TStringList.Create;
finalization
  vLiterals.Add('kk');
  vLiterals.Free;
end.

