program LandClassInfoGenerator;

uses
  Windows,
  Land in 'Land.pas',
  LandClasses in 'LandClasses.pas',
  Collection in '..\Kernel\Collection.pas';

var
  LandClassInfo : TLandClassInfo;

begin
  if ParamCount > 0
    then
      begin
        LandClassInfo := TLandClassInfo.Create(ParamStr(1));
        try
          LandClassInfo.Run;
        finally
          LandClassInfo.Free;
        end;
      end;
end.
