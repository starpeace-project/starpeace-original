unit CacheProperties;
//falta manejar cuando no es un par nombre=valor
//crearlo sin dicc   funciones para activar y desactivar el diccionario
interface

  uses classes, containers;

  type

    TStringItem = class(TItem)
    private
      fString : string;
    public
      property StringData : string read fString write fString;
    end;

    TCacheProperties = class(TObject)//TPersistent
    private
      fDictionary : TDictionary;
      fStringList : TStrings;
      function  Get(Index: Integer): string;
      function  GetName(Index: Integer): string;
      function  GetValue(const Name: string): string;
      procedure Put(Index: Integer; const Value: string);
      procedure SetValue(const Name, Value: string);
      procedure PopulateDictionary;
    public
      constructor Create; //virtual;//overload;
//      constructor Create(AStringList: TStringList); overload;
      destructor  Destroy; override;
      procedure   Clear; virtual; //abstract;
      function    IndexOfName(const Name: string): Integer; virtual;
      procedure   SaveToStream(Stream: TStream); virtual;
      procedure   LoadFromStream(Stream: TStream); virtual;
      function    Add(const S: string): Integer; virtual;
      function    ActivateDictionary : boolean;
      function    DeactivateDictionary : boolean;
      procedure   AssignProperties(AStringList: TStrings); virtual;
    public
      property StringList : TStrings read fStringList;
      property Names[Index: Integer]: string read GetName;
      property Values[const Name: string]: string read GetValue write SetValue;
      property Strings[Index: Integer]: string read Get write Put; default;
    end;

implementation

  uses
    sysutils;
{ TCacheProperties }

  procedure TCacheProperties.AssignProperties(AStringList: TStrings);
    begin
      if fStringList <> nil
        then
          begin
            fStringList.Free;
            fStringList := nil;
          end;
      fStringList := AStringList;
      DeactivateDictionary;
    end;

  function TCacheProperties.ActivateDictionary: boolean;
    begin
      if fDictionary = nil
        then
          begin
            fDictionary := TDictionary.Create(nil,true);
            PopulateDictionary;
            Result := true;
          end
        else
          Result := false;
    end;

  function TCacheProperties.DeactivateDictionary: boolean;
    begin
      if fDictionary <> nil
        then
          begin
            fDictionary.Clear;
            fDictionary.Free;
            fDictionary := nil;
            Result := true;
          end
        else
          Result := false;
    end;

  procedure TCacheProperties.PopulateDictionary;
    var
      StrItem : TStringItem;
      i : integer;
    begin
      if fDictionary <> nil
        then
          begin
            fDictionary.Clear;
            for i:= 0 to fStringList.Count-1 do
              begin
                StrItem := TStringItem.Create;
                StrItem.StringData := fStringList[i];
                fDictionary.Insert(fStringList.Names[i], StrItem);
              end;
          end;
    end;

  function TCacheProperties.Add(const S: string): Integer;
    var
      StrItem : TStringItem;
      P : integer;
      Name : string;
    begin
      Result := -1;
      if fStringList <> nil
        then Result := fStringList.Add(S);
      if fDictionary <> nil
        then
          begin
            StrItem := TStringItem.Create;
            StrItem.StringData := S;
            P := AnsiPos('=',S);
            if P <> 0
              then
                begin
                  Name := copy(S,1,P-1);
                  fDictionary.Insert(Name, StrItem);
                end
              else
                StrItem.Free;
          end;
    end;

  procedure TCacheProperties.Clear;
    begin
      if fStringList <> nil
        then fStringList.Clear;
      if fDictionary <> nil
        then fDictionary.Clear;
    end;

  constructor TCacheProperties.Create;
    begin
//      inherited Create;
      fDictionary := nil;//TDictionary.Create(nil, true);
      fStringList := TStringList.Create;//TStringList.Create;
    end;

{  constructor TCacheProperties.Create(AStringList: TStringList);
    begin
      inherited Create;
      fDictionary := nil;//TDictionary.Create(nil, true);
      fStringList := AStringList;
    end;}


  destructor TCacheProperties.Destroy;
    begin
      if fDictionary <> nil
        then
          begin
            fDictionary.Clear;
            fDictionary.Free;
            fDictionary := nil;
          end;
      if fStringList <> nil
        then
          begin
            fStringList.Clear;
            fStringList.Free;
            fStringList := nil;
          end;
      inherited Destroy;
    end;


  {  ojojojo
     en TCachedObject
        cambiar///////
              property  Values : TStrings read fProperties;
        por//////////
              property  Values : TStrings read fProperties.StringList;


     en UpdateProperties cambiar
     procedure TCachedObject.UpdateProperties(Props : TStrings);
      var
        i     : integer;
        index : integer;
        Name  : string;
        s     : string;
      begin
        for i := 0 to pred(Props.Count) do

        cambiar esto///////////
          begin
            Name  := Props.Names[i];
            index := fProperties.IndexOfName(Name);
            if index <> NoIndex
              then fProperties[index] := Props[i]
              else fProperties.Add(Props[i]);
          end;
        por esto///////////////
          begin
            Name  := Props.Names[i];
            if fProperties.Values[Name] <> ''
              then
                begin
                  P := AnsiPos('=',Props[i]);
                  fProperties.Values[Name]:= copy(Props[i],P+1,MaxInt);
                end
              else
                begin
                  fProperties.Add(Props[i]);
                end;
          end;
      end;}


  function TCacheProperties.Get(Index: Integer): string;
    begin
      if fStringList <> nil
        then Result := fStringList[Index]
        else Result := '';
    end;

  function TCacheProperties.GetName(Index: Integer): string;
    begin
      if fStringList <> nil
        then Result := fStringList.Names[Index]
        else Result := '';
    end;

  function TCacheProperties.GetValue(const Name: string): string;
    var
      StrItem : TStringItem;
      P : integer;
    begin
      if fDictionary <> nil
        then
          begin
            StrItem := TStringItem(fDictionary[Name]);
            if StrItem <> nil
              then
                begin
                  Result := StrItem.StringData;//Get(Index);
                  P := AnsiPos('=', Result);
                  if P <> 0
                    then
                      Result := copy(Result, P+1, MaxInt)
                    else
                      SetLength(Result, 0);
                end
              else
                Result := '';
          end
        else
          begin
            if fStringList <> nil
              then Result := fStringList.Values[Name]
              else Result := '';
          end;
    end;

  function TCacheProperties.IndexOfName(const Name: string): Integer;
    begin
      if fStringList <> nil
        then Result := fStringList.IndexOfName(Name)
        else Result := -1;
    end;

  procedure TCacheProperties.LoadFromStream(Stream: TStream);
    begin
      if fStringList <> nil
        then fStringList.LoadFromStream(Stream);
      //PopulateDictionary;
    end;

  procedure TCacheProperties.Put(Index: Integer; const Value: string);
    var
      StrItem : TStringItem;
    begin
      //StrItem := fDictionary.Items[fStringList.Names[Index]];
      if fDictionary <> nil
        then
          begin
            fDictionary.Remove(fStringList.Names[Index]);
            fStringList[Index] := Value;
            StrItem := TStringItem.Create;
            StrItem.StringData := Value;
            fDictionary.Insert(fStringList.Names[Index],StrItem);
          end
        else
          begin
            if fStringList <> nil
              then fStringList[Index] := Value;
          end;
    end;

  procedure TCacheProperties.SaveToStream(Stream: TStream);
    begin
      if fStringList <> nil
        then fStringList.SaveToStream(Stream);
    end;

  procedure TCacheProperties.SetValue(const Name, Value: string);
    var
      StrItem : TStringItem;
    begin
      if fDictionary <> nil
        then
          begin
            StrItem := TStringItem(fDictionary[Name]);
            StrItem.StringData := Name + '=' + Value;
          end
        else
          begin
            if fStringList <> nil
              then fStringList.Values[Name] := Value;
          end;
    end;


end.
