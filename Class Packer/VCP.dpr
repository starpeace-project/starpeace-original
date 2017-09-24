program VCP;

uses
  Forms,
  SysUtils,
  Classes,
  VCPMainFrm in 'VCPMainFrm.pas' {VisualClassPackerForm},
  IniClasses in '..\Cache\IniClasses.pas',
  VisualClassManager in 'VisualClassManager.pas',
  CompStringsParser in '..\Utils\Misc\CompStringsParser.pas',
  DelphiStreamUtils in '..\Utils\Misc\DelphiStreamUtils.pas';

{$R *.RES}

procedure CreateBinaryFile(FullPath : string);
  var
    Search   : TSearchRec;
    Stream   : TStream;
    ClassMan : TClassManager;
    IniClss  : TIniClass;
    Path     : string;
  begin
    Path := ExtractFilePath(FullPath);
    ClassMan := TClassManager.Create;
    try
      if FindFirst(Path + '*.final.ini', faArchive, Search) = 0
        then
          repeat
            try
              IniClss := TIniClass.Open(Path + Search.Name);
              try
                IniClss.ReadAllSections;
                ClassMan.AddClass(IniClss);
              finally
                IniClss.Free;
              end;
            except
            end;
          until FindNext(Search) <> 0;
      Stream := TFileStream.Create(FullPath, fmCreate);
      try
        ClassMan.Store(Stream);
      finally
        Stream.Free;
      end;
    finally
      ClassMan.Free;
    end;
  end;

begin
  if ParamCount = 0
    then
      begin
        Application.Initialize;
        Application.CreateForm(TVisualClassPackerForm, VisualClassPackerForm);
        Application.Run;
      end
    else CreateBinaryFile(ParamStr(1));
end.
