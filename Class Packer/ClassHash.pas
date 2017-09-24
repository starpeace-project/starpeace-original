unit ClassHash;

interface

  uses
    Collection, Classes;

  type
    TCollection = Collection.TCollection;

  type
    TClassIdEntry =
      class
        public
          constructor Create(anId : integer; aName : string);
          destructor  Destroy; override;
        private
          fId    : integer;
          fName  : string;
          fLarge : TStringList;
          fSmall : TStringList;
        public
          property Id    : integer     read fId    write fId;
          property Name  : string      read fName  write fName;
          property Large : TStringList read fLarge;
          property Small : TStringList read fSmall;
      end;

    TClassIdHash =
      class
        public
          constructor Create;
          destructor  Destroy; override;
          function    Compare(Item1, Item2 : TObject) : integer;
        private
          fEntries : TCollection;
        public
          procedure AddEntry(Name : string);
        private
          function GetEntryById(id : integer) : TClassIdEntry;
          function GetEntries(index : integer) : TClassIdEntry;
          function GetCount : integer;
        public
          property Count : integer read GetCount;
          property EntryById[id : integer] : TClassIdEntry read GetEntryById;
          property Entries[index : integer] : TClassIdEntry read GetEntries; default;
      end;

  var
    ClassIdHash : TClassIdHash;

  function CreateClusterHash(Path, Cluster : string) : boolean;
  function RenumerateCluster(Cluster, sourcePath, destPath : string; FirstNum : integer) : boolean;

implementation

  uses
    SysUtils, CompStringsParser, DelphiStreamUtils, VisualClassManager;

  // TClassIdEntry

  constructor TClassIdEntry.Create(anId : integer; aName : string);
    begin
      inherited Create;
      fId    := anId;
      fName  := aName;
      fLarge := TStringList.Create;
      fSmall := TStringList.Create;
    end;

  destructor TClassIdEntry.Destroy;
    begin
      fLarge.Free;
      fSmall.Free;
      inherited;
    end;

  // TClassIdHash

  constructor TClassIdHash.Create;
    begin
      inherited Create;
      fEntries := TSortedCollection.Create(0, rkBelonguer, Compare);
    end;

  destructor TClassIdHash.Destroy;
    begin
      fEntries.Free;
      inherited;
    end;

  function TClassIdHash.Compare(Item1, Item2 : TObject) : integer;
    begin
      result := TClassIdEntry(Item1).fId - TClassIdEntry(Item2).fId;
    end;

  procedure TClassIdHash.AddEntry(Name : string);
    var
      aux    : string;
      id     : integer;
      p      : integer;
      Entry  : TClassIdEntry;
    begin
      // Dis.2115.smallFarm.Construction.final.ini
      p   := 1;
      aux := GetNextStringUpTo(Name, p, '.');
      aux := GetNextStringUpTo(Name, p, '.');
      id  := StrToInt(aux);
      aux := GetNextStringUpTo(Name, p, '.');
      Entry := EntryById[id];
      if Entry = nil
        then
          begin
            Entry := TClassIdEntry.Create(id div 10, aux);
            fEntries.Insert(Entry);
          end;
      aux := lowercase(aux);
      if pos('small', aux) <> 0
        then Entry.fSmall.Add(copy(Name, 1, length(Name) - 4))
        else Entry.fLarge.Add(copy(Name, 1, length(Name) - 4));
    end;

  function TClassIdHash.GetEntryById(id : integer) : TClassIdEntry;
    var
      i : integer;
    begin
      i := 0;
      while (i < fEntries.Count) and (TClassIdEntry(fEntries[i]).fId <> id div 10) do
        inc(i);
      if i < fEntries.Count
        then result := TClassIdEntry(fEntries[i])
        else result := nil;
    end;

  function TClassIdHash.GetEntries(index : integer) : TClassIdEntry;
    begin
      result := TClassIdEntry(fEntries[index]);
    end;

  function TClassIdHash.GetCount : integer;
    begin
      result := fEntries.Count;
    end;


  // CreateClusterHash

  function CreateClusterHash(Path, Cluster : string) : boolean;
    var
      Search : TSearchRec;
      found  : integer;
      //Stream : TStream;
      //i      : integer;
    begin
      ClassIdHash := TClassIdHash.Create;
      found := FindFirst(Path + Cluster + '*.ini', faArchive, Search);
      while found = 0 do
        begin
          ClassIdHash.AddEntry(Search.Name);
          found := FindNext(Search);
        end;
      result := true;

      {// Remove latter!!!!!!!!!!!
      Stream := TFileStream.Create(Path + 'hash.txt', fmCreate);
      for i := 0 to pred(ClassIdHash.fEntries.Count) do
        begin
          DelphiStreamUtils.WriteLine(Stream, 'Class: ' + TClassIdEntry(ClassIdHash.fEntries[i]).Name);
          if TClassIdEntry(ClassIdHash.fEntries[i]).fLarge.Count > 0
            then
              begin
                DelphiStreamUtils.WriteLine(Stream, '');
                TClassIdEntry(ClassIdHash.fEntries[i]).fLarge.SaveToStream(Stream);
              end;
          if TClassIdEntry(ClassIdHash.fEntries[i]).fSmall.Count > 0
            then
              begin
                DelphiStreamUtils.WriteLine(Stream, '');
                TClassIdEntry(ClassIdHash.fEntries[i]).fSmall.SaveToStream(Stream);
              end;
          DelphiStreamUtils.WriteLine(Stream, '');
        end;
      Stream.Free;
      }
    end;

  function ReplaceId(Name : string; NewId : integer) : string;
    var
      p       : integer;
      cluster : string;
      aux     : string;
    begin
      p := 1;
      cluster := GetNextStringUpTo(Name, p, '.');
      inc(p);
      aux := GetNextStringUpTo(Name, p, '.');
      inc(p);
      aux := copy(Name, p, length(Name) - p + 1);
      result := cluster + '.' + IntToStr(NewId) + '.' + aux;
    end;

  function ExtractId(Name : string) : integer;
    var
      p       : integer;
      aux     : string;
    begin
      try
        p := 1;
        aux := GetNextStringUpTo(Name, p, '.');
        aux := GetNextStringUpTo(Name, p, '.');
        result := StrToInt(aux);
      except
        result := 0;
      end;
    end;

  procedure RenumerateFile(srcPath, dstPath, Name : string; oldNames, newNames : TStringList; newId : integer);

    var
      VsCls  : TVisualClass;
      inh    : string;
      p      : integer;
      aux    : string;
      newInh : string;

      function GetNewName(name : string) : string;
        var
          idx : integer;
        begin
          idx := oldNames.IndexOf(name);
          if idx <> -1
            then result := newNames[idx]
            else result := name;
        end;

    begin
      VsCls := TVisualClass.CreateFromINI(srcPath);
      if VsCls.Id <> 0
        then VsCls.Id := newId;
      inh := VsCls.ReadString('General', 'Inherits', '');
      if inh <> ''
        then
          begin
            p := 0;
            newInh := '';
            aux := trim(GetNextStringUpTo(inh, p, ','));
            while aux <> '' do
              begin
                if newInh = ''
                  then newInh := GetNewName(aux)
                  else newInh := newInh + ', ' + GetNewName(aux);
                inc(p);
                aux := trim(GetNextStringUpTo(inh, p, ','));
              end;
            VsCls.WriteString('General', 'Inherits', newInh);
          end;
      VsCls.WriteString('General', 'Name', Name);
      VsCls.StoreToINI(dstPath);
    end;

  function RenumerateCluster(Cluster, sourcePath, destPath : string; FirstNum : integer) : boolean;
    var
      i, j    : integer;
      Entry   : TClassIdEntry;
      oldNames : TStringList;
      newNames : TStringList;
      aux      : string;
      currNum  : integer;
      oldId    : integer;
      oldName  : string;
      idFile   : Text;
    begin
      currNum  := FirstNum;
      oldNames := TStringList.Create;
      newNames := TStringList.Create;

      CreateClusterHash(sourcePath, Cluster);
      Assign(idFile, destPath + Cluster + '.txt');
      Rewrite(idFile);
      for i := 0 to pred(ClassIdHash.Count) do
        begin
          Entry := ClassIdHash[i];

          // Renumerate Facility

          if Entry.Small.Count > 0
            then
              begin
                WriteLn(idFile, 'Small ' + Entry.Name + ' = ' + IntToStr(currNum + 1));
                for j := 0 to pred(Entry.Small.Count) do
                  begin
                    oldName := Entry.Small[j];
                    oldId   := ExtractId(oldName);
                    oldNames.Add(oldName);
                    if oldId mod 10 = 0
                      then aux := ReplaceId(oldName, currNum)
                      else aux := ReplaceId(oldName, currNum + j);
                    newNames.Add(aux);
                  end;
                for j := 0 to pred(Entry.Small.Count) do
                  begin
                    oldName := Entry.Small[j];
                    oldId   := ExtractId(oldName);
                    if oldId mod 10 = 0
                      then aux := ReplaceId(oldName, currNum)
                      else aux := ReplaceId(oldName, currNum + j);
                    if oldId mod 10 = 0
                      then RenumerateFile(sourcePath + oldName + '.ini', destPath + aux + '.ini', aux, oldNames, newNames, currNum)
                      else RenumerateFile(sourcePath + oldName + '.ini', destPath + aux + '.ini', aux, oldNames, newNames, currNum + j);
                  end;
                inc(currNum, 20);
              end;

          if Entry.Large.Count > 0
            then
              begin
                WriteLn(idFile, Entry.Name + ' = ' + IntToStr(currNum + 1));
                for j := 0 to pred(Entry.Large.Count) do
                  begin
                    oldName := Entry.Large[j];
                    oldId   := ExtractId(oldName);
                    oldNames.Add(oldName);
                    if oldId mod 10 = 0
                      then aux := ReplaceId(oldName, currNum)
                      else aux := ReplaceId(oldName, currNum + j);
                    newNames.Add(aux);
                  end;
                for j := 0 to pred(Entry.Large.Count) do
                  begin
                    oldName := Entry.Large[j];
                    oldId   := ExtractId(oldName);
                    if oldId mod 10 = 0
                      then aux := ReplaceId(oldName, currNum)
                      else aux := ReplaceId(oldName, currNum + j);
                    if oldId mod 10 = 0
                      then RenumerateFile(sourcePath + oldName + '.ini', destPath + aux + '.ini', aux, oldNames, newNames, currNum)
                      else RenumerateFile(sourcePath + oldName + '.ini', destPath + aux + '.ini', aux, oldNames, newNames, currNum + j);
                  end;
                inc(currNum, 20);
              end;

        end;
      CloseFile(idFile);
      result := true;
    end;

end.
