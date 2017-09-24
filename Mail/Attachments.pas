unit Attachments;

interface

  uses
    SysUtils, Classes;

  const
    tidClass      = 'Class';
    tidProperties = 'Properties';

  const
    propExecuted = 'Executed';

  type
    TAttachment         = class;
    TAttachmentExecuter = class;

    TAttachment =
      class
        public
          constructor Create(const aText : string);
          constructor Load(const path : string);
          destructor  Destroy; override;
        private
          fProperties : TStringList;
        private
          function GetKind : string;
        public
          property Kind       : string      read GetKind;
          property Properties : TStringList read fProperties;
        public
          procedure Store(const path : string);
      end;

    TAttachmentExecuteResult = (erOk, erError, erDelete, erIgnoredAttachment);

    TAttachmentExecuter =
      class
        public
          constructor Create(aProxy : OleVariant);
        protected
          fProxy : OleVariant;
        public
          function Execute(TAttach : TAttachment) : TAttachmentExecuteResult; virtual; abstract;
      end;

  procedure InitAttachmentExecuters;
  procedure DoneAttachmentExecuters;
  procedure AddAttachmentExecuters(const ClassName : string; Exec : TAttachmentExecuter);
  function  ExcecuteAttachment(Attach : TAttachment) : TAttachmentExecuteResult;

implementation

  uses
    IniFiles;

  var
    Executers : TStringList = nil;

  procedure InitAttachmentExecuters;
    begin
      Executers := TStringList.Create;
    end;

  procedure DoneAttachmentExecuters;
    var
      i : integer;
    begin
      for i := 0 to pred(Executers.Count) do
        Executers.Objects[i].Free;
      Executers.Free;
    end;

  procedure AddAttachmentExecuters(const ClassName : string; Exec : TAttachmentExecuter);
    begin
      Executers.AddObject(ClassName, Exec);
    end;

  function ExcecuteAttachment(Attach : TAttachment) : TAttachmentExecuteResult;
    var
      index : integer;
      Exec  : TAttachmentExecuter;
    begin
      index := Executers.IndexOf(Attach.Kind);
      if index <> -1
        then
          begin
            Exec := TAttachmentExecuter(Executers.Objects[index]);
            if Exec <> nil
              then result := Exec.Execute(Attach)
              else result := erIgnoredAttachment;
          end
        else result := erIgnoredAttachment;
    end;

  // TAttachment

  constructor TAttachment.Create(const aText : string);
    begin
      inherited Create;
      fProperties := TStringList.Create;
      fProperties.Text := aText;
    end;

  constructor TAttachment.Load(const path : string);
    var
      IniFile : TIniFile;
    begin
      inherited Create;
      IniFile := TIniFile.Create(path);
      try
        fProperties := TStringList.Create;
        IniFile.ReadSection(tidProperties, fProperties);
      finally
        IniFile.Free;
      end;
    end;

  destructor TAttachment.Destroy;
    begin
      fProperties.Free;
      inherited;
    end;

  function TAttachment.GetKind : string;
    begin
      result := fProperties.Values[tidClass];
    end;

  procedure TAttachment.Store(const path : string);
    var
      i       : integer;
      name    : string;
      IniFile : TIniFile;
    begin
      IniFile := TIniFile.Create(path);
      try
        for i := 0 to pred(fProperties.Count) do
          begin
            name := fProperties.Names[i];
            IniFile.WriteString(tidProperties, name, fProperties.Values[name]);
          end;
      finally
        IniFile.Free;
      end;
    end;

  // TAttachmentExecuter

  constructor TAttachmentExecuter.Create(aProxy : OleVariant);
    begin
      inherited Create;
      fProxy := aProxy;
    end;

end.
