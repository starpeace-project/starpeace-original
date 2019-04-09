unit Languages;

interface

  uses
    Classes, CacheAgent, MapStringToObject, Variants;

  type
    TLanguageId = string;

  type
    TMultiString =
      class( TStringList )
      end;

    TRegMultiString =
      class( TMultiString )
        public
          constructor Create( anId, aDefValue : string );
          destructor  Destroy; override;
        private
          fId           : string;
          fDefaultValue : string;
        public
          property Id           : string read fId;
          property DefaultValue : string read fDefaultValue;
      end;

    TDictionary =
      class
        public
          constructor Create( filename : string );
        private
          fLangId : TLanguageId;
          fValues : TStringList;
          fIndex  : TStringList;
        private
          function  GetValue( Id : string ) : string;
          procedure SetValue( Id, value : string );
        public
          property LangId : TLanguageId read fLangId write fLangId;
          property Values[Id : string] : string read GetValue write SetValue;
        public
          procedure Store( filename : string );
      end;

  const
    langDefault = '0';

  procedure InitMLS;
  procedure DoneMLS;

  const
    LangList   : TStringList = nil;
    NullString : TRegMultiString = nil;

  procedure RequestLanguage( Id : string );
  function  CreateDictionary : TDictionary;
  procedure ApplyDictionary( Dict : TDictionary; LangId : TLanguageId );

  function GetMultiString( Id : string ) : TRegMultiString;
  function InstantiateMultiString( Source : TRegMultiString; Data : array of const ) : TMultiString;
  function CloneMultiString( Source : TMultiString ) : TMultiString;

  procedure StoreMultiStringToCache( Name : string; MS : TMultiString; Cache : TObjectCache );

  procedure RegisterBackup;
  procedure LoadMLSFrom(path : string);

implementation

  uses
    SysUtils, Collection, VCLBackup, Logs;

  const
    MLSRegistry : TStringList = nil;


  // RegMultiString
                                                                         
  constructor TRegMultiString.Create( anId, aDefValue : string );
    begin
      inherited Create;
      fId := anId;
      fDefaultValue := aDefValue;
      Values[langDefault] := fDefaultValue;
      MLSRegistry.AddObject( fId, self );
    end;                                                 

  destructor TRegMultiString.Destroy;       
    begin
      try
        Logs.Log( 'Demolition', TimeToStr(Now) + ' - ' + ClassName );
      except
      end;
      inherited;
    end;


  // TDictionary

  constructor TDictionary.Create( filename : string );
    var
      i    : integer;
      p    : integer;
      aux  : string;
      name : string;
    begin
      fValues := TStringList.Create;
      if filename <> ''
        then
          begin
            fValues.LoadFromFile( filename );
            fLangId := fValues.Values['LangId'];
            fIndex  := TStringList.Create;
            fIndex.Sorted := true;
            fIndex.Duplicates := dupIgnore;
            for i := 0 to pred(fValues.Count) do
              begin
                aux := fValues[i];
                p   := system.pos('=', aux);
                if p > 0
                  then
                    begin
                      name := copy(aux, 1, p - 1);
                      fIndex.AddObject(name, TObject(i));
                    end;
              end;
          end
        else fLangId := '0';
    end;

  function TDictionary.GetValue( Id : string ) : string;
    var
      idx : integer;
      aux : string;
      p   : integer;
    begin
      if fIndex = nil
        then result := fValues.Values[Id]
        else
          begin
            idx := fIndex.IndexOf(Id);
            if idx >= 0
              then
                begin
                  idx := integer(fIndex.Objects[idx]);
                  aux := fValues[idx];
                end
              else aux := '';
            p := system.pos('=', aux);
            if p > 0
              then result := system.copy(aux, p + 1, length(aux) - p)
              else result := '';
          end;
    end;

  procedure TDictionary.SetValue( Id, value : string );
    begin
      fValues.Values[Id] := value;
    end;

  procedure TDictionary.Store( filename : string );
    begin
      fValues.SaveToFile( filename );
    end;

  procedure InitMLS;
    begin
      MLSRegistry := TStringList.Create;
      LangList    := TStringList.Create;
      NullString  := TRegMultiString.Create( 'NULL', '+++STRING NOT DEFINED+++' );
    end;

  procedure DoneMLS;
    begin
      MLSRegistry.Free;
    end;

  procedure RequestLanguage( Id : string );
    begin
      LangList.Add( Id );
    end;

  function CreateDictionary : TDictionary;
    var
      i  : integer;
      MS : TRegMultiString;
    begin
      result := TDictionary.Create( '' );
      for i := 0 to pred(MLSRegistry.Count) do
        begin
          MS := TRegMultiString(MLSRegistry.Objects[i]);
          if MS <> NullString
            then result.Values[MS.fId] := MS.fDefaultValue;
        end;
    end;

  procedure ApplyDictionary( Dict : TDictionary; LangId : TLanguageId );
    var
      i  : integer;
      MS : TRegMultiString;
    begin
      for i := 0 to pred(MLSRegistry.Count) do
        begin
          MS := TRegMultiString(MLSRegistry.Objects[i]);
          if MS <> NullString
            then MS.Values[LangId] := Dict.Values[MS.fId];
        end;
    end;

  function GetMultiString( Id : string ) : TRegMultiString;
    var
      idx : integer;
    begin
      idx := MLSRegistry.IndexOf( id );
      if idx <> -1
        then result := TRegMultiString(MLSRegistry.Objects[idx])     
        else result := NullString;
    end;

  function InstantiateMultiString( Source : TRegMultiString; Data : array of const ) : TMultiString;
    var
      i : integer;
    begin
      result := TMultiString.Create;
      for i := 0 to pred(Source.Count) do                  
        try
          result.Add( Format( Source[i], Data ) );
        except
        end;
    end;

  function CloneMultiString( Source : TMultiString ) : TMultiString;
    var
      i : integer;
    begin
      result := TMultiString.Create;
      for i := 0 to pred(Source.Count) do
        result.Add( Source[i] );
    end;

  procedure StoreMultiStringToCache( Name : string; MS : TMultiString; Cache : TObjectCache );
    var
      i : integer;
    begin
      for i := 0 to pred(LangList.Count) do
        Cache.WriteString( Name + IntToStr(i), MS.Values[LangList[i]] );
    end;

  type
    TMultiStringBackupAgent =
      class( TStringListBackupAgent )
      end;

  procedure RegisterBackup;
    begin
      TMultiStringBackupAgent.Register( [TMultiString] );
    end;

  procedure LoadMLSFrom(path : string);
    var
      Srch : TSearchRec;
    begin
      if FindFirst(path + '\*.*', faDirectory, Srch) = 0
        then
          repeat
            if (Srch.Attr and faDirectory <> 0) and (Srch.Attr and faArchive = 0) and (Srch.Name <> '..') and (Srch.Name <> '.')
              then RequestLanguage(Srch.Name);
          until FindNext(Srch) <> 0;
    end;

initialization

  InitMLS;

finalization

  DoneMLS;

end.
