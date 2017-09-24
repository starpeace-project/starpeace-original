unit CacheManagerAuto;

interface

uses
  Windows, ActiveX, ComObj, CacheManager_TLB, CacheObjects, CacheManagerRDO;

type
  TCacheManagerAuto = class(TAutoObject, ICacheManagerAuto)
  protected
    function ContainsFolder(const Name: WideString): WordBool; safecall;
    function GetFolderIterator(const Folder: WideString): OleVariant; safecall;
    function GetPath: WideString; safecall;
    function SetClass(const Name: WideString): WordBool; safecall;
    function SetObject(X, Y: Integer): WordBool; safecall;
    function SetObjectOfWorld(X, Y: Integer;
      const World: WideString): WordBool; safecall;
    function SetPath(const aPath: WideString): WordBool; safecall;
    function SetWorld(const Name: WideString): WordBool; safecall;
    procedure CreateFolder(const Name: WideString); safecall;
    procedure CreateObject(const aPath: WideString); safecall;
    procedure Flush; safecall;
  private
    fWorldName    : string;
    fProxy        : TWorldRDO;
    fPropertyName : string;
    fCachedObject : TCachedObject;
    fPath         : string;
  private
    function  GetIDsOfNames(const IID : TGUID; Names : Pointer; NameCount, LocaleID : Integer; DispIDs : Pointer) : HResult; override; stdcall;
    function  Invoke(DispID : Integer; const IID : TGUID; LocaleID : Integer; Flags : Word; var Params; VarResult, ExcepInfo, ArgErr : Pointer) : HResult; override; stdcall;
    function  SetToObject(const aPath : string): WordBool; safecall;
    function  RequestCache(Id : string) : string;
    function  UpdateCache (Id : string) : boolean;
  public
    destructor Destroy; override;
  end;

implementation

  uses
    Classes, SysUtils, ComServ, FolderIteratorAuto, CacheCommon, RegistryData,
    CacheNameUtils;

  const
    NormalDispId = $100;

  // TCacheProxyAuto

  function TCacheManagerAuto.GetIDsOfNames(const IID : TGUID; Names : Pointer; NameCount, LocaleID : Integer; DispIDs : Pointer) : HResult;
    begin
      if inherited GetIDsOfNames( IID, Names, NameCount, LocaleID, DispIDs ) = S_OK
        then result := S_OK
        else
          if fCachedObject <> nil
            then
              begin
                fPropertyName      := POLEStrList(Names)^[0];
                PInteger(DispIds)^ := NormalDispId;
                result := S_OK;
              end
            else result := DISP_E_UNKNOWNNAME;
    end;

  function TCacheManagerAuto.Invoke(DispID : Integer; const IID : TGUID; LocaleID : Integer; Flags : Word; var Params; VarResult, ExcepInfo, ArgErr : Pointer) : HResult;
    var
      Parameters : TDispParams;
      ParamValue : OleVariant;
      PropValue  : string;
      IndexValue : OleVariant;
    begin
      if DispId = NormalDispId
        then
          try
            Parameters := TDispParams(Params);
            if (Flags and DISPATCH_PROPERTYGET <> 0) or (Flags and DISPATCH_METHOD <> 0)
              then
                if VarResult <> nil
                  then
                    case Parameters.cArgs of
                      0 :
                        begin
                          PropValue := fCachedObject.Properties[fPropertyName];
                          PVariant(VarResult)^ := PropValue;
                          result := S_OK;
                        end;
                      1 :
                        begin
                          VariantInit( ParamValue );
                          VariantChangeType( ParamValue, OleVariant(Parameters.rgvarg[0]), 0, VT_BSTR );
                          PropValue := fCachedObject.Properties[fPropertyName + ParamValue];
                          PVariant(VarResult)^ := PropValue;
                          result := S_OK;
                        end;
                      else
                        result := DISP_E_BADPARAMCOUNT;
                    end
                  else result := DISP_E_MEMBERNOTFOUND
              else
                if (Parameters.cNamedArgs = 1) and (Parameters.rgdispidNamedArgs[0] = DISPID_PROPERTYPUT)
                  then
                    case Parameters.cArgs of
                      1 :
                        begin
                          VariantInit( ParamValue );
                          VariantChangeType( ParamValue, OleVariant(Parameters.rgvarg[0]), 0, VT_BSTR );
                          fCachedObject.Properties[fPropertyName] := ParamValue;
                          result := S_OK;
                        end;
                      2 :
                        begin
                          VariantInit( ParamValue );
                          VariantChangeType( ParamValue, OleVariant(Parameters.rgvarg[0]), 0, VT_BSTR );
                          VariantInit(IndexValue);
                          VariantChangeType( IndexValue, OleVariant(Parameters.rgvarg[1]), 0, VT_BSTR );
                          fCachedObject.Properties[fPropertyName + IndexValue] := ParamValue;
                          result := S_OK;
                        end;
                      else
                        result := DISP_E_BADPARAMCOUNT
                    end
                  else Result := DISP_E_PARAMNOTOPTIONAL
          except
            result := DISP_E_EXCEPTION;
          end
        else result := inherited Invoke( DispID, IID, LocaleID, Flags, Params, VarResult, ExcepInfo, ArgErr );
    end;

  function TCacheManagerAuto.SetToObject(const aPath : string): WordBool;

    procedure ReturnToPrevPath;
      begin
        if fPath <> ''
          then fCachedObject.SetPath( fPath );
      end;

    var
      TTL     : string;
      LastMod : string;
      AuxPath : string;

    begin
      AuxPath := aPath;
      if fCachedObject = nil
        then fCachedObject := TCachedObject.Open( AuxPath, true )
        else fCachedObject.SetPath( AuxPath );
      if not (ioFileDoesntExist in fCachedObject.IOResult) and not (ioTimeOutRead in fCachedObject.IOResult)
        then
          begin
            TTL     := fCachedObject[ppTTL];
            LastMod := fCachedObject[ppLastMod];
            try
              if (TTL <> TTLNever) and (LastMod <> '') and (Now - StrToDateTime( LastMod ) > StrToDateTime( TTL ))
                then
                  if UpdateCache( fCachedObject[ppObjId] )
                    then
                      begin
                        result := true;
                        fPath  := AuxPath;
                      end
                    else
                      begin
                        result := false;
                        ReturnToPrevPath;
                      end
                else
                  result := true;
              fCachedObject.Unlock;
            except
              result := false;
              ReturnToPrevPath;
            end;
          end
        else
          begin
            result := false;
            ReturnToPrevPath;
          end;
    end;

  function TCacheManagerAuto.GetFolderIterator(const Folder: WideString): OleVariant;
    begin
      if fCachedObject <> nil
        then result := TFolderIteratorAuto.Assign( fCachedObject.GetFolderIterator( Folder )) as IDispatch
        else result := TFolderIteratorAuto.Assign( nil ) as IDispatch;
    end;

  function TCacheManagerAuto.GetPath : WideString;
    begin
      if fCachedObject <> nil
        then result := fCachedObject.Path
        else result := '';
    end;

  function TCacheManagerAuto.SetPath(const aPath: WideString): WordBool;
    begin
      result := SetToObject( fWorldName + '\' + aPath );
    end;

  function TCacheManagerAuto.ContainsFolder(const Name: WideString): WordBool;
    begin
      result := (fCachedObject <> nil) and fCachedObject.ContainsFolder( Name );
    end;

  function TCacheManagerAuto.UpdateCache(Id : string) : boolean;
    var
      Cache : TStringList;
      Data  : string;
    begin
      Data := RequestCache( Id );
      if (Data <> '') or (Data <> resError)
        then
          begin
            Cache := TStringList.Create;
            Cache.Text := Data;
            fCachedObject.UpdateProperties( Cache );
            fCachedObject.Properties[ppLastMod] := DateTimeToStr( Now );
            result := true;
          end
        else result := true;
    end;

  procedure TCacheManagerAuto.CreateObject(const aPath: WideString);
    begin
      fCachedObject.Free;
      fCachedObject := TCachedObject.Create( aPath, nil );
    end;

  procedure TCacheManagerAuto.Flush;
    begin
      if fCachedObject <> nil
        then fCachedObject.Flush;
    end;

  procedure TCacheManagerAuto.CreateFolder(const Name: WideString);
    begin
      if fCachedObject <> nil
        then fCachedObject.CreateFolder( Name );
    end;

  function TCacheManagerAuto.RequestCache(Id : string) : string;
    begin
      if fProxy <> nil
        then result := fProxy.GetCache( StrToInt( Id ) )
        else result := '';
    end;

  function TCacheManagerAuto.SetClass(const Name: WideString) : WordBool;
    begin
      result := SetToObject( GetClassPath( Name ) );
    end;

  function TCacheManagerAuto.SetObject(X, Y: Integer) : WordBool;
    var
      Path      : string;
      WildCards : string;
      SearchRec : TSearchRec;
    begin
      try
        if fWorldName <> ''
          then
            begin
              Path := GetObjectFolder( fWorldName, X, Y );
              WildCards := GetCoordWildCards( X, Y );
              try
                if FindFirst( Path + WildCards, faArchive, SearchRec ) = 0
                  then
                    begin
                      Path := Copy( SearchRec.Name, length( WildCards ), length( SearchRec.Name ) - length( WildCards ) + 1 );
                      TranslateChars( Path, '×', '\' );
                      result := SetToObject( Path );
                    end
                  else result := false;
              finally
                FindClose( SearchRec );
              end;
            end
          else result := false;
      except
        result := false;
      end;
    end;

  function TCacheManagerAuto.SetWorld(const Name: WideString) : WordBool;
    begin
      try
        fProxy     := GetWorldRDO( Name );
        fWorldName := Name;
        result     := true;
        fCachedObject.Free;
        fCachedObject := nil;
      except
        fProxy := nil;
        result := false;
      end;
    end;

  function TCacheManagerAuto.SetObjectOfWorld(X, Y: Integer; const World: WideString) : WordBool;
    begin
      result := SetWorld( World ) and SetObject( X, Y );
    end;

  destructor TCacheManagerAuto.Destroy;
    begin
      fCachedObject.Free;
      fProxy.Free;
      inherited;
    end;

initialization

  TAutoObjectFactory.Create( ComServer, TCacheManagerAuto, Class_CacheManagerAuto, ciMultiInstance );

end.
