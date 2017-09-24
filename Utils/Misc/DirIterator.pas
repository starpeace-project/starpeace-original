unit DirIterator;

interface

  uses
    Windows, Classes, SysUtils;

  const
    onArchives = faArchive;
    onFolders  = faDirectory;
    onBoth     = onArchives or onFolders;

  type
    TIteratorOptions = integer;
    TFolderIterator  =
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
        private
          procedure SetOptions(Options : TIteratorOptions);
          function  GetFullPath : string;
        public
          property Options  : TIteratorOptions read fOptions write SetOptions;
          property Empty    : boolean read fEmpty;
          property Current  : string  read fCurrent;
          property FullPath : string  read GetFullPath;
      end;

implementation

  procedure CloseSearch(var Search : TSearchRec);
    begin
      if Search.FindHandle <> INVALID_HANDLE_VALUE
        then
          begin
            FindClose(Search);
            Search.FindHandle := INVALID_HANDLE_VALUE
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
            if (fCurrent = '.') or (fCurrent = '..')
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
            if (fCurrent = '.') or (fCurrent = '..')
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
