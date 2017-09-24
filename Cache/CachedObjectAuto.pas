unit CachedObjectAuto;

interface

uses
  Windows, ActiveX, ComObj, CacheManager_TLB, CacheObjects, CacheManagerRDO,
  CacheObjectSpool;

type
  TCachedObjectAuto = class(TAutoObject, ICachedObject)
  public
    function ContainsFolder(const Name: WideString): WordBool; safecall;
    function GetFolderIterator(const Folder: WideString): OleVariant; safecall;
    function GetPath: WideString; safecall;
    function SetClass(const Name: WideString): WordBool; safecall;
    function SetObject(X, Y: Integer): WordBool; safecall;
    function SetObjectOfWorld(X, Y: Integer; const World: WideString): WordBool; safecall;
    function SetPath(const aPath: WideString): WordBool; safecall;
    function SetWorld(const Name: WideString): WordBool; safecall;
    function Get_Recache: WordBool; safecall;
    procedure Set_Recache(Value: WordBool); safecall;
    function Properties(const Name: WideString): WideString; safecall;
    function Get_Path: WideString; safecall;
    procedure Set_Path(const Value: WideString); safecall;
    function  Get_ErrorCode: Integer; safecall;
    procedure ActivateDictionary(Activate: WordBool); safecall;
  private
    fCacheSpool   : TWorldCacheSpool;
    fCachedObject : TCachedObject;
    fWorldName    : string;
    fPropertyName : string;
    fRecache      : boolean;
    fErrorCode    : integer;
  private
    procedure ReleaseObject;
  public
    function  GetIDsOfNames(const IID : TGUID; Names : Pointer; NameCount, LocaleID : Integer; DispIDs : Pointer) : HResult; override; stdcall;
    function  Invoke(DispID : Integer; const IID : TGUID; LocaleID : Integer; Flags : Word; var Params; VarResult, ExcepInfo, ArgErr : Pointer) : HResult; override; stdcall;
  public
    procedure  Initialize; override;
    destructor Destroy;    override;
  end;

implementation

  uses
    Classes, SysUtils, ComServ, CacheCommon, CacheRegistryData, SpecialChars,
    CacheNameUtils, Registry;

  const
    NormalDispId = $100;

  // TCachedObjectAuto

  function TCachedObjectAuto.GetIDsOfNames(const IID : TGUID; Names : Pointer; NameCount, LocaleID : Integer; DispIDs : Pointer) : HResult;
    begin
      if inherited GetIDsOfNames(IID, Names, NameCount, LocaleID, DispIDs) = S_OK
        then result := S_OK
        else
          begin
            fPropertyName      := POLEStrList(Names)^[0];
            PInteger(DispIds)^ := NormalDispId;
            result := S_OK;
          end;
    end;

  function TCachedObjectAuto.Invoke(DispID : Integer; const IID : TGUID; LocaleID : Integer; Flags : Word; var Params; VarResult, ExcepInfo, ArgErr : Pointer) : HResult;
    var
      Parameters  : TDispParams;
      ParamValue  : OleVariant;
      ParamValue2 : OleVariant;
      PropValue   : WideString;
      IndexValue  : OleVariant;
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
                          if fCachedObject <> nil
                            then PropValue := fCachedObject.Properties[fPropertyName]
                            else PropValue := '';
                          PVariant(VarResult)^ := PropValue;
                          result := S_OK;
                        end;
                      1 :
                        begin
                          VariantInit(ParamValue);
                          VariantChangeType(ParamValue, OleVariant(Parameters.rgvarg[0]), 0, VT_BSTR);
                          if fCachedObject <> nil
                            then PropValue := fCachedObject.Properties[fPropertyName + ParamValue]
                            else PropValue := '';
                          PVariant(VarResult)^ := PropValue;
                          result := S_OK;
                        end;
                      2 :
                        begin
                          VariantInit(ParamValue);
                          VariantInit(ParamValue2);
                          VariantChangeType(ParamValue, OleVariant(Parameters.rgvarg[0]), 0, VT_BSTR);
                          VariantChangeType(ParamValue2, OleVariant(Parameters.rgvarg[1]), 0, VT_BSTR);
                          if fCachedObject <> nil
                            then PropValue := fCachedObject.Properties[fPropertyName + ParamValue + '.' + ParamValue2]
                            else PropValue := '';
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
                          VariantInit(ParamValue);
                          VariantChangeType(ParamValue, OleVariant(Parameters.rgvarg[0]), 0, VT_BSTR);
                          if fCachedObject <> nil
                            then fCachedObject.Properties[fPropertyName] := ParamValue;
                          result := S_OK;
                        end;
                      2 :
                        begin
                          VariantInit(ParamValue);
                          VariantChangeType(ParamValue, OleVariant(Parameters.rgvarg[0]), 0, VT_BSTR);
                          VariantInit(IndexValue);
                          VariantChangeType(IndexValue, OleVariant(Parameters.rgvarg[1]), 0, VT_BSTR);
                          if fCachedObject <> nil
                            then fCachedObject.Properties[fPropertyName + IndexValue] := ParamValue;
                          result := S_OK;
                        end;
                      else
                        result := DISP_E_BADPARAMCOUNT
                    end
                  else Result := DISP_E_PARAMNOTOPTIONAL
          except
            result := E_FAIL;
          end
        else result := inherited Invoke(DispID, IID, LocaleID, Flags, Params, VarResult, ExcepInfo, ArgErr);
    end;

  function TCachedObjectAuto.GetFolderIterator(const Folder: WideString): OleVariant;
    begin
      try
        if fCachedObject.IsFolder
          then
            try
              result := CreateOleObject('FolderIterator.DirectoryIterator');
              result.SetFolder(fCachedObject.RelPath + Folder, AllFiveObjects, onBoth);
            except
              result := NULL;
            end
          else result := NULL;
      except
        result := NULL;
      end;
    end;

  function TCachedObjectAuto.GetPath : WideString;
    var
      p   : integer;
      aux : string;
    begin
      try
        if fCachedObject <> nil             
          then
            begin
              aux := fCachedObject.RelPath;
              p   := System.Pos(uppercase(fWorldName), uppercase(aux));
              if p <> 0
                then
                  begin
                    p := p + Length(fWorldName) + 1;
                    result := System.Copy(aux, p, Length(aux) - p + 1);
                  end
                else result := aux;
            end
          else result := '';
      except
        result := '';
      end;
    end;

  function TCachedObjectAuto.SetPath(const aPath: WideString): WordBool;
    begin
      try
        ReleaseObject;
        if fCacheSpool <> nil
          then fCachedObject := fCacheSpool.GetObjectByPath(aPath, fRecache)
          else fCachedObject := nil;
        if fCachedObject <> nil
          then
            begin
              result := fCachedObject.IOResult = [];
              fErrorCode := fCachedObject.ErrorCode;
            end
          else
            begin
              result := false;
              fErrorCode := -1;
            end;
      except
        fErrorCode := -2;
        result := false;
      end;
    end;

  function TCachedObjectAuto.ContainsFolder(const Name: WideString): WordBool;
    begin
      try
        result := (fCachedObject <> nil) and fCachedObject.ContainsFolder(Name);
      except
        result := false;
      end;
    end;

  function TCachedObjectAuto.SetClass(const Name: WideString) : WordBool;
    begin
      try
        ReleaseObject;
        fCacheSpool   := CacheSpools.Classes;
        fCachedObject := fCacheSpool.GetClass(Name);
        result        := fCachedObject <> nil;
      except
        result := false;
      end;
    end;

  function TCachedObjectAuto.SetObject(X, Y: Integer) : WordBool;
    begin
      try
        ReleaseObject;
        if fCacheSpool <> nil
          then fCachedObject := fCacheSpool.GetObjectByCoord(X, Y, fRecache)
          else fCachedObject := nil;
        result := fCachedObject <> nil;
      except
        result := false;
      end;
    end;

  function TCachedObjectAuto.SetWorld(const Name : WideString) : WordBool;
    begin
      try
        ReleaseObject;
        fWorldName  := Name;
        fCacheSpool := CacheSpools.GetWorldSpool(Name);
        result      := true;
      except
        result := false;
      end;
    end;

  function TCachedObjectAuto.SetObjectOfWorld(X, Y: Integer; const World: WideString) : WordBool;
    begin
      try
        result := SetWorld(World) and SetObject(X, Y);
      except
        result := false;
      end;
    end;

  procedure TCachedObjectAuto.Initialize;
    begin
      inherited;
      fRecache := true;
    end;

  destructor TCachedObjectAuto.Destroy;
    begin
      ReleaseObject;
      inherited;
    end;

  function TCachedObjectAuto.Get_Recache: WordBool;
    begin
      result := fRecache;
    end;

  procedure TCachedObjectAuto.Set_Recache(Value: WordBool);
    begin
      fRecache := Value;
    end;

  function TCachedObjectAuto.Properties(const Name: WideString): WideString;
    begin
      try
        if fCachedObject <> nil
          then result := fCachedObject.Properties[Name]
          else result := '';
      except
        result := '';
      end;
    end;

  procedure TCachedObjectAuto.ReleaseObject;
    begin
      {
      if (fCacheSpool <> nil) and (fCachedObject <> nil)
        then fCacheSpool.DeleteObject(fCachedObject);
      }
      fCachedObject.Free;
      fCachedObject := nil;
    end;

  function TCachedObjectAuto.Get_Path: WideString; safecall;
    begin
      result := GetPath;
    end;

  procedure TCachedObjectAuto.Set_Path(const Value: WideString);
    begin
      SetPath(Value);
    end;

  function TCachedObjectAuto.Get_ErrorCode: Integer;
    begin
      result := fErrorCode;
    end;

  procedure TCachedObjectAuto.ActivateDictionary(Activate: WordBool);
    begin
      if fCachedObject <> nil
        then fCachedObject.ActivateDictionary(Activate);
    end;

initialization

  TAutoObjectFactory.Create(ComServer, TCachedObjectAuto, Class_CachedObject, ciMultiInstance, tmApartment);

end.
