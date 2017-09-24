unit CacheRegistryData;

interface

  uses
    Classes, SysUtils;

  function ReadCacheValue(const Path, Name : string) : string;

  type
    ECacheRegistryError = class(Exception);

implementation

  uses
    Windows, ActiveX, ComObj, Registry, CacheRegistryKeys, CacheObjects, CacheCommon;

  const
    NoIndex = -1;

  function ReadCacheValue(const Path, Name : string) : string;
    var
      Reg  : TRegistry;
      s    : string;
      //l    : longint;
    begin
      Reg := TRegistry.Create( KEY_READ	or KEY_WRITE );
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      try
        if Reg.OpenKey(CacheKey + Path, false)
          then result := Reg.ReadString(Name)
          else
            begin
              //l := GetLastError;
              RaiseLastWin32Error;
              s := 'Cannot read the value ' + Path + '--' + Name;
              raise ECacheRegistryError.Create(s);
            end;
      finally
        Reg.Free;
      end;
    end;

end.
