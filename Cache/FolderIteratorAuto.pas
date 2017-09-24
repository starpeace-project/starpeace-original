unit FolderIteratorAuto;

interface

uses
  ComObj, CacheManager_TLB, CacheObjects;

type
  TFolderIteratorAuto = class(TAutoObject, IFolderIteratorAuto)
  protected
    function Empty: WordBool; safecall;
    function Get_Current: WideString; safecall;
    function Get_FullPath: WideString; safecall;
    function Next: WordBool; safecall;
    procedure Reset; safecall;
    procedure SetFolder(const aPath: WideString; Options: Integer); safecall;
    procedure SetOptions(Options: Integer); safecall;
  public
    constructor Create(const aPath : string);
    constructor Assign(anIterator : TFolderIterator);
    destructor Destroy; override;
  private
    fIterator : TFolderIterator;
  end;

implementation

  uses ComServ;

  function TFolderIteratorAuto.Empty : WordBool;
    begin
      result := (fIterator = nil) or fIterator.Empty;
    end;

  function TFolderIteratorAuto.Get_Current : WideString;
    begin
      if fIterator <> nil
        then result := fIterator.Current
        else result := '';
    end;


  function TFolderIteratorAuto.Get_FullPath: WideString;
    begin
      if fIterator <> nil
        then result := fIterator.FullPath
        else result := '';
    end;

  function TFolderIteratorAuto.Next : WordBool;
    begin
      result := (fIterator <> nil) and fIterator.Next;
    end;

  procedure TFolderIteratorAuto.Reset;
    begin
      if fIterator <> nil
        then fIterator.Reset;
    end;


  procedure TFolderIteratorAuto.SetFolder(const aPath: WideString; Options: Integer);
    begin
      fIterator.Free;
      fIterator := TFolderIterator.Create( aPath, AllFiveObjects, Options );
    end;

  procedure TFolderIteratorAuto.SetOptions(Options : Integer);
    begin
      if fIterator <> nil
        then fIterator.Options := Options;
    end;

  constructor TFolderIteratorAuto.Create(const aPath : string);
    begin
      inherited Create;
      SetFolder(aPath, onBoth);
    end;

  constructor TFolderIteratorAuto.Assign(anIterator : TFolderIterator);
    begin
      inherited Create;
      fIterator := anIterator;
    end;

  destructor TFolderIteratorAuto.Destroy;
    begin
      fIterator.Free;
      inherited;
    end;

initialization

  TAutoObjectFactory.Create( ComServer, TFolderIteratorAuto, Class_FolderIteratorAuto, ciMultiInstance );

end.
