unit RegistryData;

interface

  uses
    Classes, SysUtils;

  // GetClassPath returns the path of the given Class

  function GetClassPath( Name : WideString ) : WideString;

  type
    ECacheRegistryError = class(Exception);

implementation

  uses
    Windows, ActiveX, ComObj, Registry, CacheRegistryKeys, CacheObjects, CacheCommon;

  type
    POleVariant = ^OleVariant;

  const
    NoIndex = -1;

  function GetClassPath( Name : WideString ) : WideString;
    begin
      result := GetCacheRootPath + 'Classes\' + Name + '\' + FiveExt;
    end;

  procedure ReadCacheRootFromRegistry;
    var
      Root : string;
      Reg  : TRegistry;
    begin
      Reg := TRegistry.Create;
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      try
        if Reg.OpenKey( CacheKey, false )
          then
            begin
              Root := Reg.ReadString( 'RootPath' );
              CacheObjects.SetCacheRootPath( Root );
            end
          else raise ECacheRegistryError.Create( 'Cannot read the root path from registry' );
      finally
        Reg.Free;
      end;
    end;


initialization

  ReadCacheRootFromRegistry;

end.
