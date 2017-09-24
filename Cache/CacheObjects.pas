unit CacheObjects;

interface

  uses
    Windows, Classes, SysUtils, FileCtrl, SpecialChars, CacheProperties;

  const
    UnassignedRoot = '\';

  // Cache root path

  function  GetCacheRootPath : string;
  procedure SetCacheRootPath(const aPath : string);
  procedure ForceFolder(const Path : string);

  const
    DefaultObjectName = 'object.five';
    FiveExt           = '.five';
    AllFiveObjects    = '*.five';
    LinkMarkPos       = 2;
    LinkMark          = 'LNK';
    ArchiveLinkMark   = 'ALNK';
    FolderLinkMark    = 'FLNK';

  const
    onArchives        = faArchive;
    onFolders         = faDirectory;
    onBoth            = onArchives or onFolders;

  type
    TCacheIOResult   = set of (ioFileDoesntExist, ioTimeOutRead, ioTimeOutWrite, ioExpiredTTL);
    TIteratorOptions = integer;
    TCachedObject    = class; // Image of an object stored in a file or folder.
    TFolderIterator  = class; // Iterator on a folder

    TCachedObject =
      class
        public
          constructor Create(const aPath : string; TheProperties : TStrings);
          constructor Open(const aPath : string; Locked : boolean);
          constructor Init(const aPath : string);
          destructor  Destroy; override;
          procedure   Delete;
        private
          procedure   RemoveFolder(const ThePath : string);
        private
          fRelPath    : string;         // Relative path
          fProperties : TCacheProperties;// TStrings;       // List containing the pairs Prop=Value
          fLocked     : boolean;        // Determines if the files is Locked
          fStream     : TStream;        // File stream, nil if not locked
          fIOResult   : TCacheIOResult; // Result of the las file operation
          fRecLinks   : boolean;        // Indicates whether or not this object will record the links to allow undo
        public
          procedure SetProperty(const Name, Value : string);
          function  GetProperty(const Name : string) : string;
          function  GetName : string;
          function  GetPath : string;
          procedure SetPath(const aPath : string);
          function  GetAbsolutePath : string;
          function  GetErrorCode : integer;
          function  GetValues : TStrings;
        public
          property  Values : TStrings read GetValues;//fProperties;
          property  Properties[const name : string] : string read GetProperty write SetProperty; default;
          property  Name         : string         read GetName;
          property  Path         : string         read GetPath write SetPath;
          property  RelPath      : string         read fRelPath;
          property  AbsolutePath : string         read GetAbsolutePath;
          property  IOResult     : TCacheIOResult read fIOResult write fIOResult;
          property  Locked       : boolean        read fLocked;
          property  RecLinks     : boolean        read fRecLinks write fRecLinks;
          property  ErrorCode    : integer        read GetErrorCode;
        public
          function  IsArchive : boolean;
          function  IsFolder  : boolean;
          function  CreateFolder(const FolderName : string) : boolean;
          function  ContainsFolder(const FolderName : string) : boolean;
          function  GetFolderIterator(const FolderName : string) : TFolderIterator;
          procedure Flush;
          procedure Lock;
          procedure Unlock;
          procedure UpdateProperties(Props : TStrings);
          procedure ActivateDictionary(Activate: WordBool);
        private
          function  CreateLink(const aPath : string) : boolean;
          function  DeleteLink(const aPath : string) : boolean;
        public
          procedure CreateLinks;
          procedure DeleteLinks;
          procedure DeleteFolders;
          procedure ClearProperties;
        protected
          function  GetTargetPath(const aPath : string) : string;
          function  GetObjectLinkName : string;
          procedure InitProperties;
          function  LoadProperties : boolean;
          function  SaveProperties : boolean;
      end;

    TFolderIterator =
      class
        public
          constructor Create(const aPath, TheWildCards : string; TheOptions : integer);
          destructor  Destroy; override;
        private
          fPath       : string;            // Absolute path
          fWildCards  : string;            // *.?.* etc.
          fCurrent    : string;            // Name of the current file or folder
          fOptions    : TIteratorOptions;  // Kind of files to iterate on
          fSearchRec  : TSearchRec;        // Search struct
          fEmpty      : boolean;           // True if no file was found
        public
          procedure Reset;
          function  Next : boolean;
          function  IsFolder : boolean;
        private
          procedure SetOptions(Options : TIteratorOptions);
          function  GetFullPath : string;
        public
          property Options  : TIteratorOptions read fOptions write SetOptions;
          property Empty    : boolean read fEmpty;
          property Current  : string  read fCurrent;
          property FullPath : string  read GetFullPath;
      end;

  function RemoveFullPath(const Path : string) : boolean;

implementation

  uses
    ShellAPI, CacheNameUtils, SyncObjs, CompStringsParser, CacheCommon,
    CacheRegistryData, CacheLinks;

  const
    WriteFailTimeOut   = 25; // 5 seconds
    ReadFailTimeOut    = 25; // 5 seconds
    DeleteFailTimeOut  = 15; // 5 seconds
    NoIndex            = -1;

  var
    CacheRootPath : string = UnassignedRoot;

  function GetCacheRootPath : string;
    begin
      if CacheRootPath = UnassignedRoot
        then SetCacheRootPath(CacheRegistryData.ReadCacheValue('', 'RootPath'));
      result := CacheRootPath;
    end;

  procedure SetCacheRootPath(const aPath : string);
    begin
      if aPath[length(aPath)] = '\'
        then CacheRootPath := aPath
        else CacheRootPath := aPath + '\';
    end;

  procedure ForceFolder(const Path : string);
    begin
      if not DirectoryExists(Path)
        then ForceDirectories(Path);
    end;

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
          fFlags := FOF_NOCONFIRMATION or FOF_SILENT or FOF_NOERRORUI;
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

  procedure CloseSearch(var Search : TSearchRec);
    begin
      if Search.FindHandle <> INVALID_HANDLE_VALUE
        then
          begin
            FindClose(Search);
            Search.FindHandle := INVALID_HANDLE_VALUE
          end;
    end;

  // TCachedObject

  constructor TCachedObject.Create(const aPath : string; TheProperties : TStrings);
    var
      AbsPath : string;
      refLnks : boolean;
    begin
      inherited Create;
      fRelPath := aPath;
      if FileExists(Path)
        then
          begin
            fProperties := TCacheProperties.Create;//TStringList.Create;
            InitProperties;
            refLnks := fProperties.Values[ppRefLinks] <> '';
            if (TheProperties = nil) or (refLnks and (CompareText(fProperties.Values[LinkName], TheProperties.Values[LinkName]) <> 0))
              then DeleteLinks;
            fProperties.Free;
          end
        else
          begin
            AbsPath := AbsolutePath;
            ForceFolder(ExtractFilePath(AbsPath));
          end;
      if TheProperties <> nil
        then
          begin
            fProperties := TCacheProperties.Create;
            fProperties.AssignProperties(TheProperties) // := TheProperties
          end
        else fProperties := TCacheProperties.Create;//TStringList.Create;
      {if TheProperties <> nil
        then fProperties := TheProperties
        else fProperties := TStringList.Create;}
    end;

  constructor TCachedObject.Open(const aPath : string; Locked : boolean);
    begin
      inherited Create;
      fLocked     := Locked;
      fProperties := TCacheProperties.Create;//TStringList.Create;
      SetPath(aPath);
    end;

  constructor TCachedObject.Init(const aPath : string);
    begin
      inherited Create;
      fProperties := TCacheProperties.Create;//TStringList.Create;
      fRelPath := GetTargetPath(aPath);
    end;

  destructor TCachedObject.Destroy;
    begin
      fProperties.Free;
      fStream.Free;
      inherited;
    end;

  procedure TCachedObject.Delete;
    var
      DelFails : integer;
      FullPath : string;
    begin
      try
        FullPath := AbsolutePath;
        if IsFolder
          then
            begin
              if DirectoryExists(FullPath)
                then RemoveFolder(FullPath)
            end
          else
            if FileExists(FullPath)
              then
                begin
                  DelFails := 0;
                  while (DelFails < DeleteFailTimeOut) and not DeleteFile(FullPath) do
                    begin
                      inc(DelFails);
                      Sleep(10); // Sleep 0.01 seconds. This might not occur
                    end;
                end;
        DeleteLinks;
        DeleteFolders;
      finally
        Destroy;
      end;
    end;

  procedure TCachedObject.RemoveFolder(const ThePath : string);
    var
      DelFails : integer;
    begin
      DelFails := 0;
      while (DelFails < DeleteFailTimeOut) and not RemoveFullPath(ThePath) do
        begin
          inc(DelFails);
          Sleep(10); // Sleep 0.01 seconds. This might not occur
        end;
    end;

  procedure TCachedObject.CreateLinks;
    var
      Links : string;
      p     : integer;
      s     : string;
      link  : string;
    begin
      if fProperties <> nil
        then
          begin
            Links := fProperties.Values[LinkName];
            if Links <> ''
              then
                begin
                  p    := 1;
                  s    := GetNextStringUpTo(Links, p, LinkSep);
                  link := '';
                  while s <> '' do
                    begin
                      if s[length(s)] = '\'
                        then
                          begin
                            if link = ''
                              then link := GetObjectLinkName;
                            CreateLink(s + link);
                          end
                        else CreateLink(s);
                      s := GetNextStringUpTo(Links, p, LinkSep);
                    end;
                end;
          end;
    end;

  procedure TCachedObject.DeleteLinks;
    var
      Links : string;
      p     : integer;
      s     : string;
    begin
      if fProperties <> nil
        then
          begin
            Links := fProperties.Values[LinkName];
            if Links <> ''
              then
                begin
                  p := 1;
                  s := GetNextStringUpTo(Links, p, LinkSep);
                  while s <> '' do                                     
                    begin
                      DeleteLink(s);
                      s := GetNextStringUpTo(Links, p, LinkSep);
                    end;
                end;
          end;
    end;

  procedure TCachedObject.DeleteFolders;
    var
      Folders : string;
      p       : integer;
      s       : string;
    begin
      if fProperties <> nil
        then
          begin
            Folders := fProperties.Values[DelPName];
            if Folders <> ''
              then
                begin
                  p := 1;
                  s := GetNextStringUpTo(Folders, p, LinkSep);
                  while s <> '' do
                    begin
                      RemoveFullPath(CacheRootPath + s);
                      s := GetNextStringUpTo(Folders, p, LinkSep);
                    end;
                end;
          end;
    end;

  function TCachedObject.CreateLink(const aPath : string) : boolean;
    var
      ActPath : string;
      FPath   : string;
      flName  : string;
      flInfo  : string;
      i       : integer;
      cnt     : integer;
      len     : integer;
    begin
      ActPath := CacheRootPath + aPath;
      FPath   := ExtractFilePath(ActPath);
      flName  := ExtractFileName(ActPath);
      flInfo  := '';
      if UpperCase(copy(flName, 1, 3)) = 'MAP'
        then
          begin
            i   := 1;
            cnt := 0;
            len := length(flName);
            while (i <= len) and (cnt < 2) do
              begin
                if flName[i] = BackSlashChar
                  then inc(cnt);
                inc(i);
              end;
            if cnt = 2
              then
                begin
                  flInfo := copy(flName, i, len - i + 1);
                  flName := copy(flName, 4, i - 4);
                end;
          end;
      ForceFolder(FPath);
      FPath := FPath + flName;
      if CacheLinks.CreateLink(FPath, flInfo, 5) and fRecLinks
        then
          begin
            CacheLinks.RecordLinks(FPath, false);
            result := true;
          end
        else result := false;
    end;

  function TCachedObject.DeleteLink(const aPath : string) : boolean;
    var
      link : string;
      ActPath : string;
      FPath   : string;
      flName  : string;
      i       : integer;
      cnt     : integer;
      len     : integer;
    begin
      if aPath[length(aPath)] <> '\'     //Worlds\Zyrane\Map\256}320\MAP315}364}Companies}Gaba Inc.five}Chemical Plant 7{315{364.five}
        then
          begin
            ActPath := CacheRootPath + aPath;
            FPath   := ExtractFilePath(ActPath); //Worlds\Zyrane\Map\256}320\
            flName  := ExtractFileName(ActPath); //MAP315}364}Companies}Gaba Inc.five}Chemical Plant 7{315{364.five}
            if UpperCase(copy(flName, 1, 3)) = 'MAP'
              then
                begin
                  i   := 1;
                  cnt := 0;
                  len := length(flName);
                  while (i <= len) and (cnt < 2) do
                    begin
                      if flName[i] = BackSlashChar
                        then inc(cnt);
                      inc(i);
                    end;
                  if cnt = 2
                    then flName := copy(flName, 4, i - 4);
                  link := FPath + flName;
                end
              else link := ActPath;
          end
        else link := CacheRootPath + aPath + GetObjectLinkName;
      result := CacheLinks.DeleteLink(link, 5);
    end;

  procedure TCachedObject.ClearProperties;
    begin
      fProperties.Clear;
    end;

  function TCachedObject.GetName : string;
    var
      len : integer;
      aux : pchar;
      tmp : string;
      eps : integer;
    begin
      len := length(Path);
      if len > 0
        then
          begin
            aux := pchar(pchar(Path) + len - 1);
            if aux[0] = '\'
              then dec(aux);
            while (aux >= pchar(Path)) and (aux[0] <> '\') do
              dec(aux);
            tmp := pchar(aux + 1);
            eps := pos(FiveExt, tmp);
            if eps = 0
              then result := tmp
              else result := copy(tmp, 1, eps - 1);
          end
        else result := '';
    end;

  function TCachedObject.GetPath : string;
    begin
      if IsFolder
        then result := AbsolutePath + DefaultObjectName
        else result := AbsolutePath;
    end;

  procedure TCachedObject.SetPath(const aPath : string);
    begin
      fRelPath := GetTargetPath(aPath);
      if not IsFolder and DirectoryExists(AbsolutePath)
        then fRelPath := fRelPath + '\';
      fProperties.Clear;
      fStream.Free;
      fStream   := nil;
      fIOResult := [];
      InitProperties;
    end;

  function TCachedObject.GetAbsolutePath : string;
    begin
      result := GetCacheRootPath + fRelPath;
    end;

  function TCachedObject.GetErrorCode : integer;
    begin
      result := 0;
      if ioFileDoesntExist in fIOResult
        then result := result or 1;
      if ioTimeOutRead in fIOResult
        then result := result or 2;
      if ioTimeOutWrite in fIOResult
        then result := result or 4;
      if ioExpiredTTL in fIOResult
        then result := result or 8;
    end;


  function TCachedObject.IsArchive : boolean;
    begin
      result := (fRelPath <> '') and (fRelPath[length(fRelPath)] <> '\');
    end;

  function TCachedObject.IsFolder : boolean;
    begin
      result := (fRelPath <> '') and (fRelPath[length(fRelPath)] = '\');
    end;

  function TCachedObject.CreateFolder(const FolderName : string) : boolean;
    var
      FullPath : string;
    begin
      if IsFolder
        then
          begin
            FullPath := AbsolutePath + FolderName;
            ForceFolder(FullPath);
            result := DirectoryExists(FullPath);
          end
        else result := false;
    end;

  function TCachedObject.ContainsFolder(const FolderName : string) : boolean;
    begin
      result := IsFolder and DirectoryExists(AbsolutePath + FolderName);
    end;

  function TCachedObject.GetFolderIterator(const FolderName : string) : TFolderIterator;
    begin
      if ContainsFolder(FolderName)
        then result := TFolderIterator.Create(AbsolutePath + FolderName, AllFiveObjects, onBoth)
        else result := nil;
    end;

  function TCachedObject.GetValues: TStrings;
    begin
      Result := fProperties.StringList;
    end;

  procedure TCachedObject.SetProperty(const Name, Value : string);
    begin
      fProperties.Values[Name] := Value;
    end;

  function TCachedObject.GetProperty(const Name : string) : string;
    begin
      result := fProperties.Values[Name];
    end;

  procedure TCachedObject.Flush;
    var
      WriteFails : integer;
    begin
      WriteFails := 0;
      while (WriteFails < WriteFailTimeOut) and not SaveProperties do
        begin
          Sleep(10); // Sleep 0.01 seconds
          inc(WriteFails);
        end;
      if WriteFails = WriteFailTimeOut
        then fIOResult := fIOResult + [ioTimeOutWrite];
      CreateLinks;
    end;

  procedure TCachedObject.Lock;
    begin
      fLocked := true;
      try
        if fStream = nil
          then fStream := TFileStream.Create(Path, fmOpenReadWrite or fmShareExclusive);
      except
      end;
    end;

  procedure TCachedObject.Unlock;
    begin
      fLocked := false;
      fStream.Free;
      fStream := nil;
    end;

  procedure TCachedObject.ActivateDictionary(Activate: WordBool);
    begin
      if Activate
        then
          fProperties.ActivateDictionary
        else
          fProperties.DeactivateDictionary
    end;

  procedure TCachedObject.UpdateProperties(Props : TStrings);
    var
      i     : integer;
      //index : integer;
      Name  : string;
      P     : integer;
    begin
      for i := 0 to pred(Props.Count) do
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
          {Name  := Props.Names[i];
          index := fProperties.IndexOfName(Name);
          if index <> NoIndex
            then fProperties[index] := Props[i]
            else fProperties.Add(Props[i]);}
        end;
    end;

  function TCachedObject.GetTargetPath(const aPath : string) : string;
    var
      FileName : string;
      Link     : string;
    begin
      FileName := ExtractFileName(aPath);
      if pos(LinkMark, FileName) = LinkMarkPos
        then
          begin
            Link := copy(FileName, length(FolderLinkMark) + 1, length(FileName) - length(LinkMark));
            TranslateChars(Link, BackSlashChar, '\');
            if (FileName[1] = 'F') or (FileName[1] = 'f')
              then result := Link + '\'
              else result := Link;
          end
        else result := aPath;
    end;

  function TCachedObject.GetObjectLinkName : string;
    begin
      if IsFolder
        then result := FolderLinkMark + copy(fRelPath, 1, length(fRelPath)-1)
        else result := ArchiveLinkMark + fRelPath;
      TranslateChars(result, '\', BackSlashChar);
    end;

  procedure TCachedObject.InitProperties;
    var
      ReadFails : integer;
    begin
      if FileExists(Path)
        then
          begin
            ReadFails := 0;
            while (ReadFails < ReadFailTimeOut) and not LoadProperties do
              begin
                inc(ReadFails);
                Sleep(100); // Sleep 0.05 seconds
              end;
            if ReadFails = ReadFailTimeOut
              then fIOResult := fIOResult + [ioTimeOutRead];
          end
        else fIOResult := fIOResult + [ioFileDoesntExist];
    end;

  function TCachedObject.LoadProperties : boolean;
    var
      TmpPath : string;
    begin
      TmpPath := Path;
      try
        if fLocked
          then fStream := TFileStream.Create(TmpPath, fmOpenReadWrite or fmShareExclusive)
          else fStream := TFileStream.Create(TmpPath, fmOpenRead);
        try
          fProperties.Clear;
          fProperties.LoadFromStream(fStream);
          result := true;
        finally
          if not fLocked
            then
              begin
                fStream.Free;
                fStream := nil;
              end;
        end;
      except
        result  := false;
      end
    end;

  function TCachedObject.SaveProperties : boolean;
    var
      aux : string;
    begin
      try
        aux := Path;
        if fStream <> nil
          then fStream.Seek(soFromBeginning, 0)
          else
            begin
              ForceFolder(ExtractFilePath(aux));
              fStream := TFileStream.Create(aux, fmCreate or fmShareExclusive);
            end;
        try
          fProperties.SaveToStream(fStream);
          result := true;
        finally
          fStream.Free;
          fStream := nil;
        end;
      except
        result := false;
      end;
    end;

  // TFolderIterator

  constructor TFolderIterator.Create(const aPath, TheWildCards : string; TheOptions : integer);
    begin
      inherited Create;
      fWildCards := TheWildCards;
      if aPath[length(aPath)] = '\'
        then fPath := aPath
        else fPath := aPath + '\';
      fOptions := TheOptions;
      fSearchRec.FindHandle := INVALID_HANDLE_VALUE; // Uuuf!!
      Reset;
    end;

  destructor TFolderIterator.Destroy;
    begin
      CloseSearch(fSearchRec);
      inherited;
    end;

  procedure TFolderIterator.Reset;
    begin
      CloseSearch(fSearchRec);
      fEmpty := FindFirst(fPath + fWildCards, fOptions, fSearchRec) <> 0;
      if not fEmpty
        then
          begin
            fCurrent := fSearchRec.Name;
            if (fSearchRec.Attr and fOptions = 0) or ((Lowercase(fCurrent) = DefaultObjectName) or (fCurrent = '.') or (fCurrent = '..'))
              then fEmpty := not Next;
          end
        else
          begin
            fCurrent := '';
            CloseSearch(fSearchRec);
          end;
    end;

  function TFolderIterator.Next : boolean;
    begin
      if FindNext(fSearchRec) = 0
        then
          begin
            fCurrent := fSearchRec.Name;
            if (fSearchRec.Attr and fOptions = 0) or ((Lowercase(fCurrent) = DefaultObjectName) or (fCurrent = '.') or (fCurrent = '..'))
              then result := Next
              else result := true;
          end
        else
          begin
            fCurrent := '';
            result := false;
          end;
      if not result
        then CloseSearch(fSearchRec);
    end;

  function TFolderIterator.IsFolder : boolean;
    begin
      result := fSearchRec.Attr = onFolders;
    end;

  procedure TFolderIterator.SetOptions(Options : TIteratorOptions);
    begin
      fOptions := Options;
      Reset;
    end;

  function TFolderIterator.GetFullPath : string;
    begin
      if fSearchRec.Attr = onFolders
        then result := fPath + fCurrent + '\'
        else result := fPath + fCurrent;
    end;

end.
