unit SyncIndex;

interface

  uses
    Windows, Collection, Classes, SysUtils, CabUtils;

  const
    IgnoreDate : TFileTime = (dwLowDateTime : 0; dwHighDateTime : 0);

  type
    TSynchObjectKind = (sokArchive, sokFolder, sokSyncFolder);

    TCollection = Collection.TCollection;

  type
    TFolderIndexEntry = class;
    TFolderIndex      = class;

    TFolderIndexEntry =
      class
        public
          constructor Create(aName : string; aDate : TFileTime; aSize : integer; aPckt : string; aKind : TSynchObjectKind);
        private
          fName : string;
          fDate : TFileTime;
          fSize : integer;
          fPckt : string;
          fKind : TSynchObjectKind;
          fFlag : boolean;
        public
          property Name : string read fName;
          property Date : TFileTime read fDate;
          property Size : integer read fSize;
          property Pckt : string read fPckt;
          property Kind : TSynchObjectKind read fKind;
          property Flag : boolean read fFlag;
      end;

    TFolderIndex =
      class
        public
          constructor Create;
          constructor Load(Stream : TStream);
          procedure   Store(Stream : TStream);
          destructor  Destroy; override;
        private
          fItems : TCollection;
        public
          procedure Add(Entry : TFolderIndexEntry);
          procedure AddCab(path : string; Info : TSearchRec);
          function  FindFile(name : string; var Entry : TFolderIndexEntry) : boolean;
        private
          function  Compare(Item1, Item2 : TObject) : integer;
          procedure AddCabInfo(cabfile : pchar; Info : PFileInCabinetInfo);
          function  GetCount : integer;
          function  GetEntry(index : integer) : TFolderIndexEntry;
        public
          property Count : integer read GetCount;
          property Entries[index : integer] : TFolderIndexEntry read GetEntry; default;
      end;

    TMasterIndex =
      class
        public
          constructor Create(aPath, aName : string; Synch : boolean);
          constructor Load(Stream : TStream);
          procedure   Store(Stream : TStream);
          destructor  Destroy; override;
        private
          fName         : string;
          fFolderIndex  : TFolderIndex;
          fSubFolders   : TCollection;
        private
          function GetCount : integer;
          function GetItem(index : integer) : TMasterIndex;
        public
          property Name         : string       read fName;
          property FolderIndex  : TFolderIndex read fFolderIndex;
          property Count        : integer      read GetCount;
          property Items[index : integer] : TMasterIndex read GetItem; default;
      end;

  const
    syncFolderFile      = 'folders.sync';
    syncIndexFile       = 'index.sync';
    syncMasterIndexFile = 'index.midx';

  function  CompareFolder(indexName, Folder : string; NewFiles, OldFiles, NewDirs, SyncDirs, OldDirs : TStringList; var count : integer; recurse : boolean) : TObject; // Returns a TFolderIndex or a TMasterIndex, just make sure you to release it yourself
  procedure CreateIndex(Folder, indexName : string; rec : boolean);
  procedure CreateMasterIndex(Folder, indexName : string);
  function  ChangePath(path : string; fCh, tCh : char) : string;
  procedure ChangePaths(List : TStringList; fCh, tCh : char);

implementation

  uses
    DelphiStreamUtils;


  function FileTimeToDateTime(FileTime : TFileTime) : TDateTime;
    var
      SysTime : TSystemTime;
    begin
      FileTimeToSystemTime(FileTime, SysTime);
      result := SystemTimeToDateTime(SysTime);
    end;

  function FileTimeToStr(FileTime : TFileTime) : string;
    begin
      result := DateTimeToStr(FileTimeToDateTime(FileTime));
    end;

  // TFolderIndexEntry

  constructor TFolderIndexEntry.Create(aName : string; aDate : TFileTime; aSize : integer; aPckt : string; aKind : TSynchObjectKind);
    begin
      fName := aName;
      fDate := aDate;
      fSize := aSize;
      fPckt := aPckt;
      fKind := aKind;
    end;

  // TFolderIndex

  constructor TFolderIndex.Create;
    begin
      inherited Create;
      fItems := Collection.TSortedCollection.Create(0, rkBelonguer, Compare);
    end;

  constructor TFolderIndex.Load(Stream : TStream);
    var
      count : integer;
      i     : integer;
      Entry : TFolderIndexEntry;
      name  : string;
      date  : TFileTime;
      size  : integer;
      pck   : string;
      kind  : TSynchObjectKind;
    begin
      inherited Create;
      fItems := Collection.TCollection.Create(0, rkBelonguer);
      Stream.Read(count, sizeof(count));
      for i := 0 to pred(count) do
        begin
          ReadString(Stream, name);
          Stream.Read(date, sizeof(date));
          Stream.Read(size, sizeof(size));
          ReadString(Stream, pck);
          Stream.Read(kind, sizeof(kind));
          Entry := TFolderIndexEntry.Create(name, date, size, pck, kind);
          fItems.Insert(Entry);
        end;
    end;

  procedure TFolderIndex.Store(Stream : TStream);
    var
      count : integer;
      i     : integer;
    begin
      count := fItems.Count;
      Stream.Write(count, sizeof(count));
      for i := 0 to pred(count) do
        with TFolderIndexEntry(fItems[i]) do
          begin
            WriteString(Stream, fName);
            Stream.Write(fDate, sizeof(fDate));
            Stream.Write(fSize, sizeof(fSize));
            WriteString(Stream, fPckt);
            Stream.Write(fKind, sizeof(fKind));
          end;
    end;

  destructor TFolderIndex.Destroy;
    begin
      fItems.Free;
      inherited;
    end;

  procedure TFolderIndex.Add(Entry : TFolderIndexEntry);
    var
      tmpEntry : TFolderIndexEntry;
    begin
      Entry.fName := lowercase(Entry.fName);
      if FindFile(Entry.fName, tmpEntry)
        then Entry.Free
        else fItems.Insert(Entry);
    end;

  procedure TFolderIndex.AddCabInfo(cabfile : pchar; Info : PFileInCabinetInfo);
    var
      name   : string;
      Entry  : TFolderIndexEntry;
      fltm   : TFileTime;
      utcft  : TFileTime;
      cbName : string;
    begin
      name   := lowercase(Info.NameInCabinet);
      cbName := ExtractFileName(cabfile);
      DosDateTimeToFileTime(Info.DosDate, Info.DosTime, fltm);
      LocalFileTimeToFileTime(fltm, utcft);
      if FindFile(name, Entry)
        then
          begin
            Entry.fPckt := cbName;
            //Entry.fDate := utcft;
          end
        else
          begin
            Entry := TFolderIndexEntry.Create(name, utcft, Info.FileSize, cbName, sokArchive);
            fItems.Insert(Entry);
          end;
    end;

  procedure TFolderIndex.AddCab(path : string; Info : TSearchRec);
    var
      i : integer;
    begin
      ListCabFile(path, AddCabInfo);
      for i := 0 to pred(Count) do
        if Entries[i].Pckt = Info.Name
          then Entries[i].fSize := Info.Size;
    end;

  function TFolderIndex.FindFile(name : string; var Entry : TFolderIndexEntry) : boolean;
    var
      i : integer;
    begin
      i := 0;
      name := lowercase(name);
      while (i < Count) and (Entries[i].fName <> name) do
        inc(i);
      result := i < Count;
      if result
        then Entry := Entries[i]
        else Entry := nil;
    end;

  function TFolderIndex.Compare(Item1, Item2 : TObject) : integer;
    begin
      result := TFolderIndexEntry(Item1).fSize - TFolderIndexEntry(Item2).fSize;
    end;

  function TFolderIndex.GetCount : integer;
    begin
      result := fItems.Count;
    end;

  function TFolderIndex.GetEntry(index : integer) : TFolderIndexEntry;
    begin
      result := TFolderIndexEntry(fItems[index]);
    end;


  // TMasterIndex

  constructor TMasterIndex.Create(aPath, aName : string; Synch : boolean);
    var
      Stream  : TStream;
      IdxName : string;
      i       : integer;
      MstrIdx : TMasterIndex;
    begin
      inherited Create;
      fName         := aName;
      fSubFolders   := TCollection.Create(0, rkBelonguer);
      if aName <> ''
        then IdxName := aPath + aName + '\' + syncIndexFile
        else IdxName := aPath + syncIndexFile;
      if FileExists(IdxName)
        then
          begin
            Stream := TFileStream.Create(IdxName, fmOpenRead);
            fFolderIndex := TFolderIndex.Load(Stream);
            i := 0;
            while i < fFolderIndex.Count do
              begin
                if fFolderIndex[i].Kind = sokSyncFolder
                  then
                    begin
                      MstrIdx := TMasterIndex.Create(ExtractFilePath(IdxName), fFolderIndex[i].fName, true);
                      fSubFolders.Insert(MstrIdx);
                    end;
                inc(i);
              end;
          end
        else IdxName := '';
    end;

  constructor TMasterIndex.Load(Stream : TStream);
    var
      i : integer;
      c : integer;
      b : boolean;
    begin
      inherited Create;
      Stream.Read(b, sizeof(b));
      if b
        then fFolderIndex := TFolderIndex.Load(Stream)
        else fFolderIndex := nil;
      ReadString(Stream, fName);
      Stream.Read(c, sizeof(c));
      fSubFolders := TCollection.Create(0, rkBelonguer);
      for i := 0 to pred(c) do
        fSubFolders.Insert(TMasterIndex.Load(Stream));
    end;

  procedure TMasterIndex.Store(Stream : TStream);
    var
      i : integer;
      c
      : integer;
      b : boolean;
    begin
      b  := fFolderIndex <> nil;
      Stream.Write(b, sizeof(b));
      if b
        then fFolderIndex.Store(Stream);
      WriteString(Stream, fName);
      c := Count;
      Stream.Write(c, sizeof(c));
      for i := 0 to pred(c) do
        Items[i].Store(Stream);
    end;

  destructor TMasterIndex.Destroy;
    begin
      fFolderIndex.Free;
      fSubFolders.Free;
      inherited;
    end;

  function TMasterIndex.GetCount : integer;
    begin
      if fSubFolders <> nil
        then result := fSubFolders.Count
        else result := 0;
    end;

  function TMasterIndex.GetItem(index : integer) : TMasterIndex;
    begin
      if fSubFolders <> nil
        then result := TMasterIndex(fSubFolders[index])
        else result := nil;
    end;

  // CompareFolders

  procedure BuildTree(Path : string; Dirs : TStringList);
    var
      Search : TSearchRec;
    begin
      Dirs.Add(Path);
      if FindFirst(Path + '*.*', faDirectory, Search) = 0
        then
          repeat
            if (Search.Attr and faDirectory <> 0) and (Search.Name <> '.') and (Search.Name <> '..')
              then BuildTree(Path + Search.Name + '\' , Dirs);
          until FindNext(Search) <> 0;
      FindClose(Search);
    end;

  function DoCompareFolder(Dest, Prefix : string; Index : TFolderIndex; NewFiles, OldFiles, NewDirs, SyncDirs, OldDirs : TStringList; recurse : boolean) : integer;
    var
      Search  : TSearchRec;
      Entry   : TFolderIndexEntry;
      Tree    : TStringList;
      i       : integer;
      idxDate : string;
      curDate : string;
      newFile : string;
      CurPath : string;
      RelPath : string;
      DestLen : integer;
    begin
      result := 0;
      try
        DestLen := length(Dest);
        Tree := TStringList.Create;
        if recurse
          then BuildTree(Dest, Tree)
          else Tree.Add(Dest);
        while Tree.Count > 0 do
          begin
            CurPath := Tree[0];
            Tree.Delete(0);
            //if FindFirst(Dest + '*.*', faArchive + faDirectory, Search) = 0
            if FindFirst(CurPath + '*.*', faArchive + faDirectory, Search) = 0
              then
                begin
                  repeat
                    if (Search.Name <> '.') and (Search.Name <> '..')
                      then
                        begin
                          RelPath := copy(CurPath, DestLen + 1, length(CurPath) - DestLen);
                          if Index.FindFile(RelPath + Search.Name, Entry)
                            then
                              begin
                                Entry.fFlag := true;
                                if Search.Attr and faDirectory <> 0
                                  then
                                    case Entry.fKind of
                                      sokArchive :
                                        begin
                                          OldDirs.Add(Prefix + Search.Name);
                                          if Entry.fPckt <> ''
                                            then newFile := Prefix + Entry.fPckt
                                            else newFile := Prefix + Search.Name;
                                          if NewFiles.IndexOf(newFile) = -1
                                            then
                                              begin
                                                NewFiles.AddObject(newFile, Index);
                                                inc(result, Entry.Size);
                                              end;
                                        end;
                                      sokSyncFolder :
                                        SyncDirs.Add(Prefix + Search.Name);
                                    end
                                  else
                                    begin
                                      if Entry.fKind = sokArchive
                                        then
                                          begin
                                            idxDate := FileTimeToStr(Entry.fDate);
                                            curDate := FileTimeToStr(Search.FindData.ftCreationTime) + '' + FileTimeToStr(Search.FindData.ftLastWriteTime);
                                            // >> ¿...?
                                            if CompareFileTime(Entry.fDate, Search.FindData.ftLastWriteTime) > 0
                                              then
                                                begin
                                                  if Entry.fPckt <> ''
                                                    then newFile := Prefix + Entry.fPckt
                                                    else newFile := Prefix + Entry.fName;
                                                  if NewFiles.IndexOf(newFile) = -1
                                                    then
                                                      begin
                                                        NewFiles.AddObject(newFile, Index);
                                                        inc(result, Entry.Size);
                                                      end;
                                                end
                                          end
                                        else OldFiles.Add(Prefix + Entry.fName);
                                    end
                              end
                            else
                              if Search.Attr and faArchive <> 0
                                then OldFiles.Add(Prefix + Search.Name)
                                else
                                  if Search.Attr and faDirectory <> 0
                                    then OldDirs.Add(Prefix + Search.Name);
                        end;
                  until FindNext(Search) <> 0;
                end;
            FindClose(Search);
          end;
      // Check for new files and folders
      for i := 0 to pred(Index.Count) do
        begin
          Entry := Index.Entries[i];
          if not Entry.fFlag
            then
              case Entry.fKind of
                sokArchive :
                  begin
                    if Entry.fPckt <> ''
                      then newFile := Prefix + Entry.fPckt
                      else newFile := Prefix + Entry.fName;
                    if NewFiles.IndexOf(newFile) = -1
                      then
                        begin
                          NewFiles.AddObject(newFile, Index);
                          inc(result, Entry.Size);
                        end;
                  end;
                sokSyncFolder :
                  begin
                    SyncDirs.Add(Prefix + Entry.fName);
                    NewDirs.Add(Prefix + Entry.fName);
                  end;
                sokFolder :
                  NewDirs.Add(Prefix + Entry.fName);
              end;
        end;
      finally
        //FindClose(Search);
      end;
    end;

  function CompareFolder(indexName, Folder : string; NewFiles, OldFiles, NewDirs, SyncDirs, OldDirs : TStringList; var count : integer; recurse : boolean) : TObject; // Returns a TFolderIndex or a TMasterIndex, just make sure you to release it yourself

    procedure CompareMasterIndex(Prefix : string; MasterIndex : TMasterIndex);
      var
        i      : integer;
        NewPrf : string;
      begin
        count := count + DoCompareFolder(Folder + Prefix, Prefix, MasterIndex.FolderIndex, NewFiles, OldFiles, NewDirs, SyncDirs, OldDirs, recurse);
        for i := 0 to pred(MasterIndex.Count) do
          begin
            if Prefix <> ''
              then
                if MasterIndex[i].Name <> ''
                  then NewPrf := Prefix + MasterIndex[i].Name + '\'
                  else NewPrf := Prefix
              else
                if MasterIndex[i].Name <> ''
                  then NewPrf := MasterIndex[i].Name + '\'
                  else NewPrf := '';
            CompareMasterIndex(NewPrf, MasterIndex[i]);
          end;
      end;

    var
      Index       : TFolderIndex;
      Stream      : TStream;
      MasterIndex : TMasterIndex;
    begin
      count := 0;
      if FileExists(Folder + indexName)
        then
          begin
            Stream := TFileStream.Create(Folder + indexName, fmOpenRead);
            try
              try
                if lowercase(ExtractFileExt(indexName)) = '.sync'
                  then
                    begin
                      Index  := TFolderIndex.Load(Stream);
                      count  := count + DoCompareFolder(Folder, '', Index, NewFiles, OldFiles, NewDirs, SyncDirs, OldDirs, recurse);
                      result := Index;
                    end
                  else
                    if lowercase(ExtractFileExt(indexName)) = '.midx'
                      then
                        begin
                          MasterIndex := TMasterIndex.Load(Stream);
                          CompareMasterIndex('', MasterIndex);
                          result := MasterIndex;
                        end
                      else result := nil;
              finally
                Stream.Free;
              end;
            except
              result := nil;
            end;
          end
        else result := nil;
    end;

  procedure CreateIndex(Folder, indexName : string; rec : boolean);
    var
      SyncFolder : TStringList;
      Search     : TSearchRec;
      Index      : TFolderIndex;
      Stream     : TStream;
      Entry      : TFolderIndexEntry;
      FileExt    : string;
      i          : integer;
    begin
      try
        SyncFolder := TStringList.Create;
        if FileExists(Folder + syncFolderFile)
          then
            begin
              SyncFolder.LoadFromFile(Folder + syncFolderFile);
              for i := 0 to pred(SyncFolder.Count) do
                SyncFolder[i] := lowercase(SyncFolder[i]);
            end;
        Index := TFolderIndex.Create;
        try
          if FindFirst(Folder + '*.*', faArchive + faDirectory, Search) = 0
            then
              repeat
                if Search.Attr and faArchive <> 0
                  then
                    begin
                      FileExt := UpperCase(ExtractFileExt(Search.Name));
                      if FileExt = '.CAB'
                        then Index.AddCab(Folder + Search.Name, Search)
                        else
                          if (FileExt <> '.SYNC') and (FileExt <> '.BAT') and (FileExt <> '.MIDX')
                            then
                              if not Index.FindFile(Search.Name, Entry)
                                then
                                  begin
                                    Entry :=
                                      TFolderIndexEntry.Create(
                                        Search.Name,
                                        Search.FindData.ftLastWriteTime, // >> ¿...?
                                        Search.Size,
                                        '',
                                        sokArchive);
                                    Entry.fFlag := true;
                                    Index.Add(Entry);
                                  end
                                else
                                  begin
                                    Entry.fDate := Search.FindData.ftLastWriteTime;
                                    Entry.fFlag := true;
                                  end;
                    end
                  else
                    if (Search.Attr and faDirectory <> 0) and (Search.Name <> '.') and (Search.Name <> '..')
                      then
                        begin
                          if SyncFolder.IndexOf(lowercase(Search.Name)) <> noIndex
                            then Index.Add(TFolderIndexEntry.Create(Search.Name, IgnoreDate, 0, '', sokSyncFolder))
                            else Index.Add(TFolderIndexEntry.Create(Search.Name, IgnoreDate, 0, '', sokFolder));
                          if rec
                            then CreateIndex(Folder + Search.Name + '\', indexName, true);
                        end;
              until FindNext(Search) <> 0;
          Stream := TFileStream.Create(Folder + indexName, fmCreate);
          try
            Index.Store(Stream);
          finally
            Stream.Free;
          end;
        finally
          FindClose(Search);
          Index.Free;
          SyncFolder.Free;
        end;
      except
      end;
    end;

  procedure CreateMasterIndex(Folder, indexName : string);
    var
      MasterIndex : TMasterIndex;
      Stream      : TStream;
    begin
      if (Folder <> '') and (Folder[length(Folder)] <> '\')
        then Folder := Folder + '\';
      MasterIndex := TMasterIndex.Create(Folder, '', true);
      Stream      := TFileStream.Create(Folder + IndexName, fmCreate);
      try
        MasterIndex.Store(Stream);
      finally
        Stream.Free;
      end;
    end;

  function ChangePath(path : string; fCh, tCh : char) : string;
    var
      i : integer;
    begin
      if path <> ''
        then
          begin
            SetLength(result, length(path));
            for i := 1 to length(path) do
              if path[i] = fCh
                then result[i] := tCh
                else result[i] := path[i];
          end
        else result := '';
    end;

  procedure ChangePaths(List : TStringList; fCh, tCh : char);
    var
      i : integer;
    begin
      for i := 0 to pred(List.Count) do
        List[i] := ChangePath(List[i], fCh, tCh);
    end;

end.
