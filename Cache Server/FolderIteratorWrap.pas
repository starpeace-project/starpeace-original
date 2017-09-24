unit FolderIteratorWrap;

interface

  uses
    CacheObjects;

  type
    TFolderIteratorWrap =
      class
        public
          constructor Create(const aPath : string; Options : integer);
          constructor Assign(anIterator : TFolderIterator);
          destructor  Destroy; override;
        published
          function  Empty(useless : integer) : OleVariant;
          function  CurrentPath(useless : integer) : OleVariant;
          function  Next(useless : integer) : OleVariant;
          procedure Reset;
          function  SetFolder(const aPath : WideString; Options : Integer) : OleVariant;
        private
          fIterator : TFolderIterator;
        end;

implementation

  uses
    CacheServerReportForm;

  constructor TFolderIteratorWrap.Create(const aPath : string; Options : integer);
    begin
      inherited Create;
      SetFolder(aPath, Options);
      inc(CacheServerReportForm.RDOCacheItrCount);
    end;

  constructor TFolderIteratorWrap.Assign(anIterator : TFolderIterator);
    begin
      inherited Create;
      fIterator := anIterator;
      inc(CacheServerReportForm.RDOCacheItrCount);
    end;

  destructor TFolderIteratorWrap.Destroy;
    begin
      dec(CacheServerReportForm.RDOCacheItrCount);
      fIterator.Free;
      inherited;
    end;

  function TFolderIteratorWrap.Empty(useless : integer) : OleVariant;
    begin
      result := (fIterator = nil) or fIterator.Empty;
    end;

  function TFolderIteratorWrap.CurrentPath(useless : integer) : OleVariant;
    begin
      try
        if fIterator <> nil
          then result := fIterator.Current
          else result := '';
      except
        result := '';
      end;
    end;

  function TFolderIteratorWrap.Next(useless : integer) : OleVariant;
    begin
      try
        result := (fIterator <> nil) and fIterator.Next;
      except
        result := false;
      end;
    end;

  procedure TFolderIteratorWrap.Reset;
    begin
      try
        if fIterator <> nil
          then fIterator.Reset;
      except
      end;
    end;

  function TFolderIteratorWrap.SetFolder(const aPath: WideString; Options : integer) : OleVariant;
    begin
      try
        fIterator.Free;
        fIterator := TFolderIterator.Create( GetCacheRootPath + aPath, AllFiveObjects, Options );
        result := true;
      except
        result := false;
      end;
    end;

end.
