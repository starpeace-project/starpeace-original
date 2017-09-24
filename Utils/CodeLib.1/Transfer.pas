unit Transfer;

interface

uses
  CoreTypes;

type
  IEnumNames = IEnumStrings;

type
  IPropertyStorage =
    interface
      function GetClass : string;
      function GetName : string;
      function SetName(const which : string) : boolean;
      function GetProperty(const name : string) : string;
      function SetProperty(const name, value : string) : boolean;
      function CreateProperty(const name, value : string) : boolean;
      function EnumProperties : IEnumNames;
      function OpenStorage(const name : string) : IPropertyStorage;
      function CreateStorage(const theClass, name : string) : IPropertyStorage;
      function EnumChildren : IEnumNames;
    end;


procedure TransferData(const src, dst : IPropertyStorage);


implementation


procedure TransferData(const src, dst : IPropertyStorage);
  var
    name : string;
    Enum : IEnumNames;
    s, d : IPropertyStorage;
  begin
    Enum := src.EnumProperties;
    if Enum <> nil
      then
        while Enum.Next(name) > 0 do
          dst.CreateProperty(name, src.GetProperty(name));
    Enum := src.EnumChildren;
    if Enum <> nil
      then
        while Enum.Next(name) > 0 do
          begin
            s := src.OpenStorage(name);
            assert(s <> nil);
            d := dst.CreateStorage(s.GetClass, name);
            if d <> nil
              then TransferData(s, d);
          end;
  end;

end.

