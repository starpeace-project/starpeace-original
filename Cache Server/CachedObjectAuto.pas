unit CachedObjectAuto;

interface

uses
  Windows, ActiveX, ComObj, FIVECacheServer_TLB, CacheObjects, CachedObjectWrap;

type
  TCachedObject = class(TAutoObject, ICachedObject)
  public
    function ContainsFolder(const Name: WideString): WordBool; safecall;
    function GetFolderIterator(const Folder: WideString): OleVariant; safecall;
    function GetPath: WideString; safecall;
    function SetClass(const Name: WideString): WordBool; safecall;
    function SetObject(X, Y: Integer): WordBool; safecall;
    function SetObjectOfWorld(X, Y: Integer; const World: WideString): WordBool; safecall;
    function SetPath(const aPath: WideString): WordBool; safecall;
    function SetWorld(const Name: WideString): WordBool; safecall;
    procedure CreateFolder(const Name: WideString); safecall;
    procedure CreateObject(const aPath: WideString); safecall;
    procedure Flush; safecall;
    function Get_Recache: WordBool; safecall;
    procedure Set_Recache(Value: WordBool); safecall;
    function Properties(const Name: WideString): WideString; safecall;
    procedure KeepAlive; safecall;
  private
    fPropertyName : string;
    fCachedObject : TCachedObjectWrap;
  private
    function  GetIDsOfNames(const IID : TGUID; Names : Pointer; NameCount, LocaleID : Integer; DispIDs : Pointer) : HResult; override; stdcall;
    function  Invoke(DispID : Integer; const IID : TGUID; LocaleID : Integer; Flags : Word; var Params; VarResult, ExcepInfo, ArgErr : Pointer) : HResult; override; stdcall;
  public
    procedure  Initialize; override;
    destructor Destroy;    override;
  protected
  end;

implementation

  uses
    Classes, SysUtils, ComServ, CacheCommon, CacheRegistryData, SpecialChars,
    CacheNameUtils;

  var
    Logs : boolean = false;

  const
    NormalDispId = $100;

  // TCachedObject

  function TCachedObject.GetIDsOfNames(const IID : TGUID; Names : Pointer; NameCount, LocaleID : Integer; DispIDs : Pointer) : HResult;
    begin
      if inherited GetIDsOfNames(IID, Names, NameCount, LocaleID, DispIDs) = S_OK
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

  function TCachedObject.Invoke(DispID : Integer; const IID : TGUID; LocaleID : Integer; Flags : Word; var Params; VarResult, ExcepInfo, ArgErr : Pointer) : HResult;
    var
      Parameters : TDispParams;
      ParamValue : OleVariant;
      PropValue  : WideString;
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
                          PropValue := fCachedObject.CachedObject.Properties[fPropertyName];
                          PVariant(VarResult)^ := PropValue;
                          result := S_OK;
                        end;
                      1 :
                        begin
                          VariantInit(ParamValue);
                          VariantChangeType(ParamValue, OleVariant(Parameters.rgvarg[0]), 0, VT_BSTR);
                          PropValue := fCachedObject.CachedObject.Properties[fPropertyName + ParamValue];
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
                          fCachedObject.CachedObject.Properties[fPropertyName] := ParamValue;
                          result := S_OK;
                        end;
                      2 :
                        begin
                          VariantInit(ParamValue);
                          VariantChangeType(ParamValue, OleVariant(Parameters.rgvarg[0]), 0, VT_BSTR);
                          VariantInit(IndexValue);
                          VariantChangeType(IndexValue, OleVariant(Parameters.rgvarg[1]), 0, VT_BSTR);
                          fCachedObject.CachedObject.Properties[fPropertyName + IndexValue] := ParamValue;
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

  function TCachedObject.GetFolderIterator(const Folder: WideString): OleVariant;
    begin
      try
        if fCachedObject.CachedObject.IsFolder
          then
            try
              result := CreateOleObject('FolderIterator.DirectoryIterator');
              result.SetFolder(fCachedObject.CachedObject.RelPath + Folder, AllFiveObjects, onBoth);
            except
              result := NULL;
            end
          else result := NULL;
      except
        result := NULL;
      end;
    end;

  function TCachedObject.GetPath : WideString;
    begin
      result := fCachedObject.CachedObject.Path;
    end;

  function TCachedObject.SetPath(const aPath: WideString): WordBool;
    begin
      result := fCachedObject.SetPath(aPath);
    end;

  function TCachedObject.ContainsFolder(const Name: WideString): WordBool;
    begin
      try
        result := fCachedObject. ContainsFolder(Name);
      except
        result := false;
      end;
    end;

  procedure TCachedObject.Flush;
    begin
      // >>
    end;

  procedure TCachedObject.CreateFolder(const Name: WideString);
    begin
      // >>
    end;

  procedure TCachedObject.CreateObject(const aPath: WideString);
    begin
      // >>
    end;

  function TCachedObject.SetClass(const Name: WideString) : WordBool;
    begin
      result := fCachedObject.SetClass(Name);
    end;

  function TCachedObject.SetObject(X, Y: Integer) : WordBool;
    begin
      result := fCachedObject.SetObject(X, Y);
    end;

  function TCachedObject.SetWorld(const Name : WideString) : WordBool;
    begin
      result := fCachedObject.SetWorld(Name);
    end;

  function TCachedObject.SetObjectOfWorld(X, Y: Integer; const World: WideString) : WordBool;
    begin
      result := fCachedObject.SetObjectOfWorld(X, Y, World);
    end;

  procedure TCachedObject.Initialize;
    begin
      fCachedObject := TCachedObjectWrap.Create;
    end;

  destructor TCachedObject.Destroy;
    begin
      fCachedObject.Free;
      inherited;
    end;

  function TCachedObject.Get_Recache: WordBool;
    begin
      result := fCachedObject.Recache;
    end;

  procedure TCachedObject.Set_Recache(Value: WordBool);
    begin
      fCachedObject.Recache := Value;
    end;

  function TCachedObject.Properties(const Name: WideString): WideString;
    begin
      try
        if fCachedObject.CachedObject <> nil
          then result := fCachedObject.CachedObject.Properties[Name]
          else result := '';
      except
        result := '';
      end;
    end;

  procedure TCachedObject.KeepAlive;
    begin
      fCachedObject.KeepAlive;
    end;

initialization

  TAutoObjectFactory.Create(ComServer, TCachedObject, Class_CachedObject, ciMultiInstance);

end.
