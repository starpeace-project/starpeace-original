unit VisualClassManager;

interface

  uses
    IniClasses, Collection, Classes, SysUtils;

  type
    TCollection = Collection.TCollection;

  type
    TClassManager = class;
    TVisualClass  = class;
    TSection      = class;

    TClassManager =            
      class
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fClasses : TCollection;
        public
          procedure AddClass(aClass : TIniClass);
        private
          function GetCount : integer;
          function GetClass(index : integer) : TVisualClass;
          function GetClassById(id : integer) : TVisualClass;
        public
          property Count : integer read GetCount;
          property Classes[index : integer] : TVisualClass read GetClass;
          property ClassById[id : integer] : TVisualClass read GetClassById;
        private
          function CompareItems(Item1, Item2 : TObject) : integer;
        public
          constructor Load(Stream : TStream);
          procedure   Store(Stream : TStream);
      end;

    TVisualClass =
      class
        public
          constructor Create(aIniClass : TIniClass);
          constructor Load(Stream : TStream; Strs : TStrings);
          constructor CreateFromINI(path : string);
          procedure   Store(Stream : TStream);
          procedure   StoreToINI(path : string);
          destructor  Destroy; override;
        private
          fId       : word;
          fSections : TCollection;
        private
          function GetCount : integer;
          function GetSection(index : integer) : TSection;
        public
          property Id : word read fId write fId;
          property Count : integer read GetCount;
          property Sections[index : integer] : TSection read GetSection;
        public
          function  ReadString(const Section, Name, DefVal : string) : string;
          function  ReadInteger(const Section, Name : string; DefVal : integer) : integer;
          function  ReadBool(const Section, Name : string; DefVal : boolean) : boolean;
          procedure WriteString(const Section, Name, Value : string);
          //procedure WriteInteger(const Section, Name, Value : string);
          //procedure WriteBool(const Section, Name, Value : string);
      end;

    TSection =
      class
        public
          constructor Create(aName : string; Properties : TStringList);
          constructor Load(Stream : TStream; Strs : TStrings);
          procedure   Store(Stream : TStream);
          destructor  Destroy; override;
        private
          fNameIndex  : word;
          fName       : string;
          fProperties : TStringList;
        public
          property Name : string read fName;
          property Properties : TStringList read fProperties;
      end;

implementation

  uses
    DelphiStreamUtils, IniFiles;

  // TClassManager

  constructor TClassManager.Create;
    begin
      inherited Create;
      fClasses := TSortedCollection.Create(0, rkBelonguer, CompareItems);
    end;

  constructor TClassManager.Load(Stream : TStream);
    var
      List  : TStringList;
      i     : integer;
      cnt   : word;
      aux   : string;
    begin
      inherited Create;
      fClasses := TSortedCollection.Create(0, rkBelonguer, CompareItems);
      List := TStringList.Create;
      Stream.Read(cnt, sizeof(cnt));
      for i := 0 to pred(cnt) do
        if ReadLine(Stream, aux)
          then List.Add(aux);
      Stream.Read(cnt, sizeof(cnt));
      for i := 0 to pred(cnt) do
        fClasses.Insert(TVisualClass.Load(Stream, List));
      List.Free;
    end;

  procedure TClassManager.Store(Stream : TStream);
    var
      List  : TStringList;
      vcIdx : integer;
      scIdx : integer;
      ppIdx : integer;
      VCls  : TVisualClass;
      idx   : integer;
      Sctn  : TSection;
      aux   : string;
      w     : word;
    begin
      List := TStringList.Create;
      List.Duplicates := dupIgnore;
      // Get the string table
      for vcIdx := 0 to pred(Count) do
        begin
          VCls := Classes[vcIdx];
          for scIdx := 0 to pred(VCls.Count) do
            begin
              Sctn := VCls.Sections[scIdx];
              idx  := List.IndexOf(Sctn.Name);
              if idx = -1
                then idx := List.Add(Sctn.Name);
              Sctn.fNameIndex := idx;
              for ppIdx := 0 to pred(Sctn.Properties.Count) do
                begin
                  aux := Sctn.Properties[ppIdx];
                  idx  := List.IndexOf(aux);
                  if idx = -1
                    then idx := List.Add(aux);
                  Sctn.Properties.Objects[ppIdx] := TObject(idx);
                end;
            end;
        end;
      // Save the string table
      w := List.Count;
      Stream.Write(w, sizeof(w));
      for ppIdx := 0 to pred(w) do
        WriteLine(Stream, List[ppIdx]);
      List.Free;
      // Save the indexes
      w := Count;
      Stream.Write(w, sizeof(w));
      for vcIdx := 0 to pred(Count) do
        begin
          VCls := Classes[vcIdx];
          VCls.Store(Stream);
        end;
    end;

  destructor TClassManager.Destroy;
    begin
      fClasses.Free;
      inherited;
    end;

  procedure TClassManager.AddClass(aClass : TIniClass);
    var
      NewClass : TVisualClass;
    begin
      NewClass := TVisualClass.Create(aClass);
      if NewClass.Id <> 0
        then fClasses.Insert(NewClass)
        else raise Exception.Create('');
    end;

  function TClassManager.GetCount : integer;
    begin
      result := fClasses.Count;
    end;

  function TClassManager.GetClass(index : integer) : TVisualClass;
    begin
      result := TVisualClass(fClasses[index]);
    end;

  function TClassManager.GetClassById(id : integer) : TVisualClass;
    var
      i   : integer;
      cnt : integer;
    begin
      i   := 0;
      cnt := fClasses.Count;
      while (i < cnt) and (Classes[i].Id <> id) do
        inc(i);
      if i < cnt
        then result := Classes[i]
        else result := nil;
    end;

  function TClassManager.CompareItems(Item1, Item2 : TObject) : integer;
    begin
      result := TVisualClass(Item1).fId - TVisualClass(Item2).fId;
    end;

  // TVisualClass

  constructor TVisualClass.Create(aIniClass : TIniClass);
    var
      i : integer;
    begin
      inherited Create;
      fSections := TCollection.Create(0, rkBelonguer);
      for i := 0 to pred(aIniClass.SectionList.Count) do
        fSections.Insert(TSection.Create(aIniClass.SectionList[i], TStringList(aIniClass.SectionList.Objects[i])));
      fId := aIniClass.ReadInteger(tidGeneral, 'Id', 0);
    end;

  constructor TVisualClass.Load(Stream : TStream; Strs : TStrings);
    var
      sn : byte;
      i  : integer;
    begin
      inherited Create;
      fSections := TCollection.Create(0, rkBelonguer);
      // Save the Id
      Stream.Read(fId, sizeof(fId));
      Stream.Read(sn, sizeof(sn));
      for i := 0 to pred(sn) do
        fSections.Insert(TSection.Load(Stream, Strs));
    end;

  constructor TVisualClass.CreateFromINI(path : string);
    var
      Ini   : TIniFile;
      Secs  : TStringList;
      i, j  : integer;
      Names : TStringList;
      Props : TStringList;
      Sec   : TSection;
    begin
      inherited Create;
      fSections := TCollection.Create(0, rkBelonguer);
      Ini       := TIniFile.Create(path);
      Secs      := TStringList.Create;
      Props     := TStringList.Create;
      Names     := TStringList.Create;
      Ini.ReadSections(Secs);
      for i := 0 to pred(Secs.Count) do
        begin
          if lowercase(Secs[i]) = 'general'
            then fId := Ini.ReadInteger('general', 'Id', 0);
          Ini.ReadSection(Secs[i], Names);
          for j := 0 to pred(Names.Count) do
            Props.Values[Names[j]] := Ini.ReadString(Secs[i], Names[j], '');
          Sec := TSection.Create(Secs[i], Props);
          fSections.Insert(Sec);
          Props.Clear;
          Names.Clear;
        end;
      Names.Free;
      Props.Free;
      Secs.Free;
      Ini.Free;
    end;

  procedure TVisualClass.Store(Stream : TStream);
    var
      sn : byte;
      i  : integer;
    begin
      // Save the Id
      Stream.Write(fId, sizeof(fId));
      // Save sections
      sn := Count;
      Stream.Write(sn, sizeof(sn));
      for i := 0 to pred(Count) do
        Sections[i].Store(Stream);
    end;

  procedure TVisualClass.StoreToINI(path : string);
    var
      Ini   : TIniFile;
      i, j  : integer;
      Sec   : TSection;
      aux   : string;
    begin
      inherited Create;
      Ini := TIniFile.Create(path);
      for i := 0 to pred(fSections.Count) do
        begin
          Sec := TSection(fSections[i]);
          for j := 0 to pred(Sec.Properties.Count) do
            begin
              aux := Sec.Properties.Names[j];
              if j <> pred(Sec.Properties.Count)
                then Ini.WriteString(Sec.Name, #9 + aux, Sec.Properties.Values[aux])
                else Ini.WriteString(Sec.Name, #9 + aux, Sec.Properties.Values[aux] + ^M^J);
            end;
        end;
      if fId <> 0
        then Ini.WriteInteger('General', #9 + 'Id', fId);
      Ini.Free;
    end;

  destructor TVisualClass.Destroy;
    begin
      fSections.Free;
      inherited;
    end;

  function TVisualClass.GetCount : integer;
    begin
      result := fSections.Count;
    end;

  function TVisualClass.GetSection(index : integer) : TSection;
    begin
      result := TSection(fSections[index]);
    end;

  function TVisualClass.ReadString(const Section, Name, DefVal : string) : string;
    var
      index : integer;
      cnt   : integer;
    begin
      cnt   := Count;
      index := 0;
      while (index < cnt) and (Sections[index].Name <> Section) do
        inc(index);
      if index < cnt
        then
          begin
            result := Sections[index].Properties.Values[Name];
            if result = ''
              then result := DefVal;
          end
        else result := DefVal;
    end;

  function TVisualClass.ReadInteger(const Section, Name : string; DefVal : integer) : integer;
    var
      aux : string;
    begin
      aux := ReadString(Section, Name, '');
      if aux <> ''
        then
          try
            result := StrToInt(aux);
          except
            result := DefVal;
          end
        else result := DefVal;
    end;

  function TVisualClass.ReadBool(const Section, Name : string; DefVal : boolean) : boolean;
    var
      aux : string;
    begin
      aux := ReadString(Section, Name, '');
      if aux <> ''
        then result := aux = '1'
        else result := DefVal;
    end;

  procedure TVisualClass.WriteString(const Section, Name, Value : string);
    var
      index : integer;
      cnt   : integer;
    begin
      cnt   := Count;
      index := 0;
      while (index < cnt) and (Sections[index].Name <> Section) do
        inc(index);
      if index < cnt
        then Sections[index].Properties.Values[Name] := Value;
    end;


  // TSection

  constructor TSection.Create(aName : string; Properties : TStringList);
    begin
      inherited Create;
      fName := aName;
      fProperties := TStringList.Create;
      fProperties.AddStrings(Properties);
      {$IFNDEF KEEP_GENERAL}
      if lowercase(fName) = 'general'
        then
          begin
            fProperties.Values[tidName] := '';
            fProperties.Values[tidInherits] := '';
            fProperties.Values[tidId] := '';
          end;
      {$ENDIF}
    end;

  constructor TSection.Load(Stream : TStream; Strs : TStrings);
    var
      idx : word;
      ppc : byte;
      i   : integer;
    begin
      inherited Create;
      Stream.Read(idx, sizeof(idx));
      fName := Strs[idx];
      Stream.Read(ppc, sizeof(ppc));
      fProperties := TStringList.Create;
      for i := 0 to pred(ppc) do
        begin
          Stream.Read(idx, sizeof(idx));
          fProperties.Add(Strs[idx]);
        end;
    end;

  procedure TSection.Store(Stream : TStream);
    var
      pn : byte;
      i  : integer;
      w  : word;
    begin
      // Save the index
      Stream.Write(fNameIndex, sizeof(fNameIndex));
      // Save properties
      pn := fProperties.Count;
      Stream.Write(pn, sizeof(pn));
      for i := 0 to pred(pn) do
        begin
          w := integer(fProperties.Objects[i]);
          Stream.Write(w, sizeof(w));
        end;
    end;

  destructor TSection.Destroy;
    begin
      fProperties.Free;
      inherited;
    end;

end.
