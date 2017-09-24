unit FileUtilsObj;

interface

uses
  ComObj, NewsFileUtils_TLB, SysUtils;

type
  TFolderIterator = class(TAutoObject, IFolderIterator)
  public
    destructor Destroy; override;
  protected
    function FindFirst(const aPath: WideString): WideString; safecall;
    function FindNext: WideString; safecall;
    procedure FindClose; safecall;
    function Get_LangId: WideString; safecall;
    procedure Set_LangId(const Value: WideString); safecall;
  private
    fSearchRec : TSearchRec;
    fRecValid  : boolean;
    fLangId    : string;
  protected
    property LangId : widestring read Get_LangId;
  end;

implementation

uses ComServ, NewsRegistry, Registry, Windows;

destructor TFolderIterator.Destroy;
  begin
    FindClose;
    inherited;
  end;

function TFolderIterator.FindFirst(const aPath: WideString): WideString;

  function GlobalizePath( path : string ) : string;
    var
      Reg : TRegistry;
    begin
      if (pos( ':', path ) = 0) and (path[1] <> '\')
        then
          try
            Reg := TRegistry.Create;
            try
              Reg.RootKey := HKEY_LOCAL_MACHINE;
              if Reg.OpenKey( tidRegKey_News, false )
                then result := Format( Reg.ReadString( 'Path' ), [LangId] ) + path
                else result := path;
            finally
              Reg.Free;
            end;
          except
            result := path;
          end
        else result := path;
    end;

  var
    path : string;
  begin
    FindClose;
    path := GlobalizePath( aPath );
    if SysUtils.FindFirst( path, faAnyFile, fSearchRec ) = 0
      then
        begin
          result := fSearchRec.Name;
          fRecValid := true;
        end
      else
        begin
          result := '';
          FindClose;
        end;
  end;

function TFolderIterator.FindNext: WideString;
  begin
    if SysUtils.FindNext( fSearchRec ) = 0
      then result := fSearchRec.Name
      else
        begin
          result := '';
          FindClose;
        end;
  end;

procedure TFolderIterator.FindClose;
  begin
    if fRecValid
      then
        begin
          SysUtils.FindClose( fSearchRec );
          fRecValid := false;
        end;
  end;

function TFolderIterator.Get_LangId: WideString;
  begin
    if fLangId <> ''
      then result := fLangId
      else result := '0';
  end;
                                                                   
procedure TFolderIterator.Set_LangId(const Value: WideString);
  begin
    fLangId := Value;
  end;

initialization
  TAutoObjectFactory.Create(ComServer, TFolderIterator, Class_FolderIterator, ciMultiInstance);
end.
