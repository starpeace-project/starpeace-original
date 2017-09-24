unit DirectoryIterator;

interface

uses
  ComObj, FolderIterator_TLB, CacheObjects, Registry, StdVcl;

type
  TDirectoryIterator = class(TAutoObject, IDirectoryIterator)
  protected
    function Empty: WordBool; safecall;
    function Next: WordBool; safecall;
    procedure Reset; safecall;
    procedure SetFolder(const aPath, aWildcard: WideString; Options: Integer); safecall;
    procedure SetOptions(Options: Integer); safecall;
    function Get_Current: WideString; safecall;
    function Get_FullPath: WideString; safecall;
    function Get_IsFolder: WordBool; safecall;
  private
    fFolderIterator : TFolderIterator;
    fFolderEmpty    : boolean;
    fCacheRoot      : string;
  public
    destructor Destroy; override;
    procedure  Initialize; override;
  end;

implementation

  uses
    Windows, ComServ, CacheRegistryKeys;

  function TDirectoryIterator.Empty: WordBool;
    begin
      try
        result := (fFolderIterator = nil) or fFolderEmpty;
      except
        result := true;
      end;
    end;

  function TDirectoryIterator.Next: WordBool;
    begin
      try
        if fFolderIterator <> nil
          then result := fFolderIterator.Next
          else result := false;
      except
        result := false;
      end;
    end;

  procedure TDirectoryIterator.Reset;
    begin
      try
        if fFolderIterator <> nil
          then fFolderIterator.Reset;
      except
      end;
    end;

  procedure TDirectoryIterator.SetFolder(const aPath, aWildcard: WideString; Options: Integer);
    begin
      try
        fFolderIterator.Free;
        if aPath[length( aPath )] <> '\'
          then fFolderIterator := TFolderIterator.Create( fCacheRoot + aPath + '\', aWildCard, Options )
          else fFolderIterator := TFolderIterator.Create( fCacheRoot + aPath, aWildCard, Options );
        fFolderEmpty := fFolderIterator.Empty;
      except
      end;
    end;

  procedure TDirectoryIterator.SetOptions(Options: Integer);
    begin
      try
        if fFolderIterator <> nil
          then fFolderIterator.Options := Options;
      except
      end;
    end;

  function TDirectoryIterator.Get_Current: WideString;
    begin
      try
        if fFolderIterator <> nil
          then result := fFolderIterator.Current
          else result := '';
      except
        result := '';
      end;
    end;

  function TDirectoryIterator.Get_FullPath: WideString;
    begin
      try
        if fFolderIterator <> nil
          then result := fFolderIterator.FullPath
          else result := '';
      except
        result := '';
      end;
    end;

  destructor TDirectoryIterator.Destroy;
    begin
      fFolderIterator.Free;
      inherited;
    end;

  procedure TDirectoryIterator.Initialize;
    begin
      inherited;
      fCacheRoot := CacheObjects.GetCacheRootPath;
    end;

  function TDirectoryIterator.Get_IsFolder: WordBool;
    begin
      try
        result := (fFolderIterator <> nil) and fFolderIterator.IsFolder;
      except
        result := false;
      end;
    end;

initialization

  TAutoObjectFactory.Create(ComServer, TDirectoryIterator, Class_DirectoryIterator, ciMultiInstance, tmApartment);

end.
