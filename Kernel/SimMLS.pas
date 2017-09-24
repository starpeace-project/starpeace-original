unit SimMLS;

interface

  uses
    Languages;

  function GetLanguagePath( LangId : TLanguageId ) : string;

  procedure LoadMLS;

implementation

  uses
    SysUtils;

  function GetLanguagePath( LangId : TLanguageId ) : string;
    begin
      result := ExtractFilePath(paramstr(0)) + 'Languages\ms\' + LangId + '\sim.lang';
    end;


  procedure LoadMLS;
    var
      Dict : TDictionary;
      i    : integer;
      root : string;
    begin
      root := ExtractFilePath(paramstr(0)) + 'languages\ms\';
      Languages.LoadMLSFrom(root);
      for i := 0 to pred(LangList.Count) do
        begin
          Dict := TDictionary.Create(GetLanguagePath(LangList[i]));
          try
            Languages.ApplyDictionary( Dict, LangList[i] );
          finally
            Dict.Free;
          end;
        end;
    end;

{
  procedure LoadMLS;
    var
      Dict : TDictionary;
      i    : integer;
    begin
      Languages.RequestLanguage( '0' );
      Languages.RequestLanguage( '1' );
      for i := 0 to pred(LangList.Count) do
        begin
          Dict := TDictionary.Create( GetLanguagePath( LangList[i] ));
          try
            Languages.ApplyDictionary( Dict, LangList[i] );
          finally
            Dict.Free;
          end;
        end;
    end;
}

end.
