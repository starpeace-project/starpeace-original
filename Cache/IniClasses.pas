unit IniClasses;

interface

  uses
    SysUtils, Classes;

  const
    iniBOOLTRUE = '1';

  const
    tidGeneral  = 'General';
    tidInherits = 'inherits';
    tidName     = 'Name';
    tidId       = 'Id';

  type
    TIniClass =
      class
        public
          constructor Create(const aPath : string; aId : integer);
          constructor Open(const aPath : string);
          destructor  Destroy; override;
        private
          fClassName  : string;
          fPath       : string;
          fSections   : TStringList;
          fParents    : TStringList;
          fCurSection : string;
        private
          procedure ReadAnsestors(const aClassName : string; Properties : TStringList);
          procedure ReadClassProperties(ClassName : string; Properties : TStringList);
          procedure DoGetSection(Name : string; Properties : TStringList);
        public
          function  ReadString(const Section, Name : string; DefValue : string ) : string;
          function  ReadInteger(const Section, Name : string; DefValue : integer) : integer;
          function  ReadBool(const Section, Name : string; DefValue : boolean) : boolean;
          procedure ReadAllSections;
        private
          function  GetSection(const Section : string) : TStringList;
        public
          property  Sections[const Section : string] : TStringList read GetSection;
          property  SectionList : TStringList read fSections;
      end;

implementation

  uses
    IniFiles, CompStringsParser;

  function GetActualClassName(const aPath, aClassId : string) : string;
    var
      Search : TSearchRec;
    begin
      try
        if FindFirst(aPath + '*.' + aClassId + '.*.ini', faArchive, Search) = 0
          then result := Search.Name
          else result := aClassId;
      finally
        FindClose(Search);
      end;
    end;

  constructor TIniClass.Create(const aPath : string; aId : integer);
    begin
      inherited Create;
      fPath      := aPath;
      fClassName := GetActualClassName(aPath, IntToStr(aId));
      fSections  := TStringList.Create;
      fSections.Sorted := true;
      fSections.Duplicates := dupIgnore;
    end;

  constructor TIniClass.Open(const aPath : string);
    begin
      inherited Create;
      fPath      := ExtractFilePath(aPath);
      fClassName := ExtractFileName(aPath);
      fSections  := TStringList.Create;
      fSections.Sorted := true;
      fSections.Duplicates := dupIgnore;
    end;

  destructor TIniClass.Destroy;
    var
      i : integer;
    begin
      for i := 0 to pred(fSections.count) do
        fSections.Objects[i].Free;
      fSections.Free;
      fParents.Free;
      inherited;
    end;

  procedure TIniClass.ReadAnsestors(const aClassName : string; Properties : TStringList);
    var
      IniFile : TIniFile;
      inh     : string;
      aux     : string;
      p       : integer;
      auxSect : TStringList;
    begin
      if fParents.IndexOf(aClassName) = -1
        then fParents.Add(aClassName);
      IniFile := TIniFile.Create(fPath + aClassName);
      try
        try
          inh := IniFile.ReadString(tidGeneral, tidInherits, '');
          auxSect := TStringList.Create;
          IniFile.ReadSections(auxSect);
          fSections.AddStrings(auxSect);
          auxSect.Free;
        finally
          IniFile.Free;
        end;
        p := length(inh);
        if p > 0
          then
            begin
              aux := GetPrevStringUpTo(inh, p, ',');
              while aux <> '' do
                begin
                  ReadAnsestors(trim(aux) + '.ini', Properties);
                  dec(p);
                  aux := GetPrevStringUpTo(inh, p, ',');
                end;
            end;
      except
      end;
    end;

  procedure TIniClass.ReadClassProperties(ClassName : string; Properties : TStringList);
    var
      IniFile : TIniFile;
      i       : integer;
      NewSect : TStringList;
    begin
      IniFile := TIniFile.Create(fPath + ClassName);
      try
        try
          if fCurSection <> ''
            then IniFile.ReadSectionValues(fCurSection, Properties)
            else
              for i := 0 to pred(fSections.Count) do
                if fSections.Objects[i] <> nil
                  then IniFile.ReadSectionValues(fSections[i], TStringList(fSections.Objects[i]))
                  else
                    begin
                      NewSect := TStringList.Create;
                      IniFile.ReadSectionValues(fSections[i], NewSect);
                      fSections.Objects[i] := NewSect;
                    end;
        finally
          IniFile.Free;
        end;
      except
      end;
    end;

  procedure TIniClass.DoGetSection(Name : string; Properties : TStringList);
    var
      i : integer;
    begin
      fCurSection := Name;
      if fParents = nil
        then
          begin
            fParents := TStringList.Create;
            ReadAnsestors(fClassName, Properties);
          end;
      for i := pred(fParents.Count) downto 0 do
        ReadClassProperties(fParents[i], Properties);
      fCurSection := '';
    end;

  function TIniClass.ReadString(const Section, Name : string; DefValue : string) : string;
    var
      aux : string;
    begin
      aux := GetSection(lowercase(Section)).Values[Name];
      if aux <> ''
        then result := aux
        else result := DefValue;
    end;

  function TIniClass.ReadInteger(const Section, Name : string; DefValue : integer) : integer;
    var
      aux : string;
    begin
      aux := GetSection(lowercase(Section)).Values[Name];
      if aux = ''
        then result := DefValue
        else
          try
            result := StrToInt(aux);
          except
            result := DefValue;
          end;
    end;

  function TIniClass.ReadBool(const Section, Name : string; DefValue : boolean) : boolean;
    var
      aux : string;
    begin
      aux := GetSection(lowercase(Section)).Values[Name];
      if aux = ''
        then result := DefValue
        else result := aux = iniBOOLTRUE;
    end;

  procedure TIniClass.ReadAllSections;
    begin
      DoGetSection('', nil);
    end;

  function TIniClass.GetSection(const Section : string) : TStringList;
    var
      Sect  : TStringList;
      index : integer;
    begin
      index := fSections.IndexOf(lowercase(Section));
      if index <> -1
        then
          begin
            Sect := TStringList(fSections.Objects[index]);
            if Sect = nil
              then
                begin
                  Sect := TStringlist.Create;
                  fSections.Objects[index] := Sect;
                  DoGetSection(Section, Sect);
                end
          end
        else
          begin
            Sect := TStringlist.Create;
            fSections.AddObject(lowercase(Section), Sect);
            DoGetSection(Section, Sect);
          end;
       result := Sect;
    end;

end.

