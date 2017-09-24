unit xBackupObjects;

interface

  uses
    SysUtils, Classes, ObjectIndex;

  const
    LineBreak = #13#10;
    IndentInc = '  ';
    EqualSign = ' = ';
    BeginMark = '{';
    EndMark   = '}';
    NilValue  = 'nil';

  const
    chkMatch   = 0;
    chkNoMatch = 2;
    chkFail    = 3;

  type
    TBackupObject = class;
    TBackupWriter = class;
    TBackupReader = class;

    TBackupObject =
      class
        public
          constructor Create(aStream : TStream);
          destructor  Destroy; override;
        protected
          fStream : TStream;
          fIndent : string;
        public
          procedure AddIndentation; virtual;
          procedure DecIndentation; virtual;
      end;

    TBackupWriter =
      class(TBackupObject)
        private
          fCurLine : string;
        public
          procedure WriteString(Name : string; Value : string);
          procedure WriteChar(Name : string; Value : char);
          procedure WriteBoolean(Name : string; Value : boolean);
          procedure WriteByte(Name : string; Value : byte);
          procedure WriteWord(Name : string; Value : word);
          procedure WriteInteger(Name : string; Value : integer);
          procedure WriteSingle(Name : string; Value : single);
          procedure WriteDouble(Name : string; Value : double);
          procedure WriteCurrency(Name : string; Value : double);
        private
          procedure _WriteObject(Name : string; Value : TObject; Loose : boolean);
        public
          procedure WriteObject(Name : string; Value : TObject);
          procedure WriteObjectRef(Name : string; Value : TObject);
          procedure WriteLooseObject(Name : string; Value : TObject);
          procedure WriteMethod(Name : string; Method : TMethod);
        public
          procedure AddIndentation; override;
          procedure DecIndentation; override;
        private
          procedure WriteLine(line : string);
      end;

    TBackupReader =
      class(TBackupObject)
        public
          constructor Create(aStream : TStream);
          destructor  Destroy; override;
        private
          fFixups : TObjectIndex;
        public
          function  ReadString(Name : string) : string;
          function  ReadChar(Name : string) : char;
          function  ReadBoolean(Name : string) : boolean;
          function  ReadByte(Name : string) : byte;
          function  ReadWord(Name : string) : word;
          function  ReadInteger(Name : string) : integer;
          function  ReadSingle(Name : string) : single;
          function  ReadDouble(Name : string) : double;
          function  ReadCurrency(Name : string) : double;
          function  ReadObject(Name : string; var O) : string; // Returns the class name
          procedure ReadMethod(Name : string; var Method : TMethod);
        public
          procedure AddIndentation; override;
          procedure DecIndentation; override;
        private
          function  ReadProp(Name : string) : string;
      end;

    EInternalBackupError = class(Exception);

    CBackupAgent = class of TBackupAgent;
    TBackupAgent =
      class
        public
          class procedure Register(const Classes : array of TClass);
        protected
          class function  CreateObject(aClass : TClass) : TObject;      virtual;
          class procedure Write(Stream : TBackupWriter; Obj : TObject); virtual; abstract;
          class procedure Read (Stream : TBackupReader; Obj : TObject); virtual; abstract;
      end;

  procedure RegisterClass(aClass : TClass);
  function  CreateBackup(Path : string) : TBackupWriter;
  function  OpenBackup(Path : string) : TBackupReader;
  function  GetBackupAgent(ClassName : string; var Agent : CBackupAgent; var ClassType : TClass) : boolean;

implementation

  uses
    DelphiStreamUtils, CompStringsParser; //, BackupAgentRegistry;

  type
    PObject = ^TObject;

  const
    NoIndex = -1;

  const
    TypeMissmathCode = 'Type missmatch error';


  // External routines

  procedure RegisterBackupAgent(aClass, anAgent : TClass); external 'BackupRegistry.dll';
  procedure GetClassAgent(const ClassName : string; var TheAgent, TheClass : TClass); external 'BackupRegistry.dll';

  // TBackupObject

  constructor TBackupObject.Create(aStream : TStream);
    begin
      inherited Create;
      fStream := aStream;
    end;

  destructor TBackupObject.Destroy;
    begin
      fStream.Free;
      inherited;
    end;

  procedure TBackupObject.AddIndentation;
    begin
      fIndent := fIndent + IndentInc;
    end;

  procedure TBackupObject.DecIndentation;
    begin
      if fIndent <> ''
        then SetLength(fIndent, Length(fIndent) - 2);
    end;

  // TBackupWriter

  procedure TBackupWriter.WriteString(Name : string; Value : string);
    begin
      // WriteLine(Name + EqualSign + IntToStr(length(Value)) + ' ' + Value);
      WriteLine(Name + EqualSign + Value);
    end;

  procedure TBackupWriter.WriteChar(Name : string; Value : char);
    begin
      WriteLine(Name + EqualSign + IntToStr(length(Value)) + ' ' + Value);
    end;

  procedure TBackupWriter.WriteBoolean(Name : string; Value : boolean);
    begin
      if Value
        then WriteLine(Name + EqualSign + 'T')
        else WriteLine(Name + EqualSign + 'F');
    end;

  procedure TBackupWriter.WriteByte(Name : string; Value : byte);
    begin
      WriteLine(Name + EqualSign + IntToStr(Value));
    end;

  procedure TBackupWriter.WriteWord(Name : string; Value : word);
    begin
      WriteLine(Name + EqualSign + IntToStr(Value));
    end;

  procedure TBackupWriter.WriteInteger(Name : string; Value : integer);
    begin
      WriteLine(Name + EqualSign + IntToStr(Value));
    end;

  procedure TBackupWriter.WriteSingle(Name : string; Value : single);
    begin
      WriteLine(Name + EqualSign + FloatToStr(Value));
    end;

  procedure TBackupWriter.WriteDouble(Name : string; Value : double);
    begin
      WriteLine(Name + EqualSign + FloatToStr(Value));
    end;

  procedure TBackupWriter.WriteCurrency(Name : string; Value : double);
    begin
      WriteLine(Name + EqualSign + FloatToStr(Value));
    end;

  procedure TBackupWriter._WriteObject(Name : string; Value : TObject; Loose : boolean);
    var
      BackupAgent : CBackupAgent;
      TheClass    : TClass;
    begin
      if Value = nil
        then
          WriteLine(Name + EqualSign + NilValue)
        else
          begin
            if Loose
              then WriteLine(Name + EqualSign + Value.ClassName)
              else WriteLine(Name + EqualSign + Value.ClassName + '(' + IntToStr(integer(Value)) + ')');
            AddIndentation;
            try
              if GetBackupAgent(Value.ClassName, BackupAgent, TheClass)
                then BackupAgent.Write(Self, Value)
                else raise EInternalBackupError.Create('Error class ' + Value.ClassName + ' not registered');
            finally
              DecIndentation;
            end;
          end;
    end;

  procedure TBackupWriter.WriteObject(Name : string; Value : TObject);
    begin
      _WriteObject(Name, Value, false);
    end;

  procedure TBackupWriter.WriteObjectRef(Name : string; Value : TObject);
    begin
      if Value <> nil
        then WriteLine(Name + EqualSign + Value.ClassName + '(' + IntToStr(integer(Value)) + ')!')
        else WriteLine(Name + EqualSign + NilValue);
    end;

  procedure TBackupWriter.WriteLooseObject(Name : string; Value : TObject);
    begin
      _WriteObject(Name, Value, true);
    end;

  procedure TBackupWriter.WriteMethod(Name : string; Method : TMethod);
    begin
      if (Method.Code <> nil) and (Method.Data <> nil)
        then
          with Method do
            begin
              WriteLine(Name + EqualSign + 'Method');
              AddIndentation;
              WriteObjectRef('Object', Data);
              WriteLine('Code' + EqualSign + TObject(Data).MethodName(Code));
              DecIndentation;
            end
          else WriteLine(Name + EqualSign + NilValue);
    end;

  procedure TBackupWriter.AddIndentation;
    begin
      WriteLine(BeginMark);
      inherited AddIndentation;
    end;

  procedure TBackupWriter.DecIndentation;
    begin
      inherited DecIndentation;
      WriteLine(EndMark);
    end;

  procedure TBackupWriter.WriteLine(line : string);
    begin
      if fIndent <> ''
        then fStream.Write(fIndent[1], Length(fIndent));
      if line <> ''
        then fStream.Write(line[1], Length(line));
      fStream.Write(LineBreak, 2);
      fCurLine := '';
    end;

  // TBackupReader

  constructor TBackupReader.Create(aStream : TStream);
    begin
      inherited;
      fFixups := TObjectIndex.Create(1024); // >>
    end;

  destructor TBackupReader.Destroy;
    begin
      fFixups.Free;
      inherited;
    end;

  function TBackupReader.ReadString(Name : string) : string;
    begin
      result := ReadProp(Name);
    end;

  function TBackupReader.ReadChar(Name : string) : char;
    var
      aux : string;
    begin
      aux := ReadProp(Name);
      if length(aux) <> 1
        then result := aux[1]
        else raise EInternalBackupError.Create(TypeMissmathCode);
    end;

  function TBackupReader.ReadBoolean(Name : string) : boolean;
    var
      aux : string;
    begin
      aux := ReadProp(Name);
      if length(aux) = 1
        then result := aux[1] = 'T'
        else raise EInternalBackupError.Create(TypeMissmathCode);
    end;

  function TBackupReader.ReadByte(Name : string) : byte;
    var
      aux : string;
    begin
      aux := ReadProp(Name);
      try
        result := StrToInt(aux);
      except
        raise EInternalBackupError.Create(TypeMissmathCode);
      end;
    end;

  function TBackupReader.ReadWord(Name : string) : word;
    var
      aux : string;
    begin
      aux := ReadProp(Name);
      try
        result := StrToInt(aux);
      except
        raise EInternalBackupError.Create(TypeMissmathCode);
      end;
    end;

  function TBackupReader.ReadInteger(Name : string) : integer;
    var
      aux : string;
    begin
      aux := ReadProp(Name);
      try
        result := StrToInt(aux);
      except
        raise EInternalBackupError.Create(TypeMissmathCode);
      end;
    end;

  function TBackupReader.ReadSingle(Name : string) : single;
    var
      aux : string;
    begin
      aux := ReadProp(Name);
      try
        result := StrToFloat(aux);
      except
        raise EInternalBackupError.Create(TypeMissmathCode);
      end;
    end;

  function TBackupReader.ReadDouble(Name : string) : double;
    var
      aux : string;
    begin
      aux := ReadProp(Name);
      try
        result := StrToFloat(aux);
      except
        raise EInternalBackupError.Create(TypeMissmathCode);
      end;
    end;

  function TBackupReader.ReadCurrency(Name : string) : double;
    var
      aux : string;
    begin
      aux := ReadProp(Name);
      try
        result := StrToCurr(aux);
      except
        raise EInternalBackupError.Create(TypeMissmathCode);
      end;
    end;

  procedure TBackupReader.ReadMethod(Name : string; var Method : TMethod);
    var
      aux         : string;
      BackupAgent : CBackupAgent;
      ClassType   : TClass;
      ClsName     : string;
    begin
      aux := ReadProp(Name);
      if aux <> NilValue
        then
          begin
            AddIndentation;
            ClsName := ReadObject('Object', TObject(Method.Data));
            if GetBackupAgent(ClsName, BackupAgent, ClassType)
              then Method.Code := ClassType.MethodAddress(ReadString('Code'))
              else raise EInternalBackupError.Create('Error class "' + ClsName + '" not registered');
            DecIndentation;
          end
        else
          begin
            Method.Data := nil;
            Method.Code := nil;
          end;
    end;

  function TBackupReader.ReadObject(Name : string; var O) : string;
    var
      Obj        : TObject absolute O;
      aux        : string;
      cls        : string;
      ref        : integer;
      p          : integer;
      IdxEntry   : TIndexEntry;
      Index      : integer;

    procedure UpdateFixups;
      var
        AuxPtr : PObject;
        Index  : integer;
      begin
        Index := fFixups.IdIndex[ref];
        if Index = NoIndex
          then
            fFixups.AddEntry(IndexEntry(ref, Obj, true))
          else
            begin
              // Fix anything in the fixup
              IdxEntry := fFixups.Entries[Index];
              AuxPtr   := PObject(IdxEntry.Obj);
              while AuxPtr <> nil do
                begin
                  IdxEntry.Obj := TObject(PObject(IdxEntry.Obj)^);
                  AuxPtr^      := Obj;
                  AuxPtr       := PObject(IdxEntry.Obj);
                end;
              IdxEntry.Obj := Obj;
              IdxEntry.Flg := true;
              fFixups.Entries[Index] := IdxEntry;
            end;
      end;

    procedure DoReadObject(ClassName : string; UpdateFixup : boolean);
      var
        BackupAgent : CBackupAgent;
        TheClass    : TClass;
      begin
        AddIndentation;
        if GetBackupAgent(ClassName, BackupAgent, TheClass)
          then
            begin
              Obj := BackupAgent.CreateObject(TheClass);
              if Obj <> nil
                then
                  try
                    if UpdateFixup
                      then UpdateFixups;
                    BackupAgent.Read(Self, Obj)
                  except
                    Obj.Free;
                    raise;
                  end
                else raise EInternalBackupError.Create('Error unspected incongruity')
            end
          else raise EInternalBackupError.Create('Error class not registered: ' + ClassName);
        DecIndentation;
      end;

    begin
      aux := ReadProp(Name);
      if aux = NilValue
        then
          begin
            Obj    := nil;
            result := '';
          end
        else
          begin
            p := 1;
            // Read the class
            cls := GetNextStringUpTo(aux, p, '(');
            if cls <> ''
              then
                if p > length(aux)
                  then // No fixup is needed
                    DoReadObject(cls, false)
                  else // This object must update the fixups
                    begin
                      inc(p);
                      try
                        ref := StrToInt(GetNextStringUpTo(aux, p, ')'));
                        inc(p);
                        if p = length(aux)
                          then // This an object reference X=TX(121212)!
                            begin
                              Index := fFixups.IdIndex[ref];
                              if Index <> NoIndex
                                then
                                  begin
                                    IdxEntry := fFixups.Entries[Index];
                                    if IdxEntry.Flg
                                      then
                                        Obj := IdxEntry.Obj
                                      else
                                        begin
                                          // Insert the reference in the fixups
                                          IdxEntry.Obj := TObject(@Obj);
                                          IdxEntry.Flg := false;
                                          Obj := IdxEntry.Obj;
                                          fFixups.Entries[Index] := IdxEntry;
                                        end;
                                  end
                                else
                                  begin
                                    // Create the fixup for this object
                                    Obj := nil;
                                    fFixups.AddObject(ref, TObject(@Obj));
                                  end
                            end
                          else DoReadObject(cls, true); // >> This an object, lets read it
                      except
                        raise;
                      end;
                    end
              else raise EInternalBackupError.Create('No class found');
            result := cls;
          end;
    end;

  procedure TBackupReader.AddIndentation;
    var
      bm : string;
    begin
      fStream.Seek(length(fIndent), soFromCurrent);
      if DelphiStreamUtils.ReadLine(fStream, bm) and (bm = BeginMark)
        then inherited AddIndentation
        else raise EInternalBackupError.Create('');
    end;

  procedure TBackupReader.DecIndentation;
    var
      em : string;
    begin
      inherited DecIndentation;
      fStream.Seek(length(fIndent), soFromCurrent);
      if DelphiStreamUtils.ReadLine(fStream, em) and (em = EndMark)
        then else raise EInternalBackupError.Create('Remaining data to read');
    end;

  function TBackupReader.ReadProp(Name : string) : string;
    var
      aux     : string;
      AuxName : string;
      p       : integer;
    begin
      result := '';
      fStream.Seek(length(fIndent), soFromCurrent);
      if DelphiStreamUtils.ReadLine(fStream, aux)
        then
          begin
            p := pos(EqualSign, aux);
            if p <> 0
              then
                 begin
                   AuxName := copy(aux, 1, p - 1);
                   if (Name = '') or (AuxName = Name)
                     then result := copy(aux, p + length(EqualSign), length(aux))
                     else raise EInternalBackupError.Create('');
                 end
              else raise EInternalBackupError.Create('');
          end
        else raise EInternalBackupError.Create('');
    end;


  // TBackupAgent

  class procedure TBackupAgent.Register;
    var
      i : integer;
    begin
      try
        for i := low(Classes) to high(Classes) do
          RegisterBackupAgent(Classes[i], Self);
      except
        raise EInternalBackupError.Create('Class already registered');
      end;
    end;


  class function TBackupAgent.CreateObject(aClass : TClass) : TObject;
    begin
      result := aClass.Create;
    end;

  // Utility functions

  procedure RegisterClass(aClass : TClass);
    var
      Agent : CBackupAgent;
      aux   : TClass;
      Cls   : TClass;
    begin
      Agent := nil;
      aux   := aClass;
      while (aux <> nil) and (Agent = nil) do
        begin
          GetBackupAgent(aux.ClassName, Agent, Cls);
          aux := aux.ClassParent;
        end;
      if Agent <> nil
        then RegisterBackupAgent(aClass, Agent)
        else raise EInternalBackupError.Create('No class for "' + aClass.ClassName + '" parent has been registered yet');
    end;

  function CreateBackup(Path : string) : TBackupWriter;
    begin
      result := TBackupWriter.Create(TFileStream.Create(Path, fmCreate));
    end;

  function OpenBackup(Path : string) : TBackupReader;
    begin
      result := TBackupReader.Create(TFileStream.Create(Path, fmOpenRead));
    end;

  function GetBackupAgent(ClassName : string; var Agent : CBackupAgent; var ClassType : TClass) : boolean;
    begin
      GetClassAgent(ClassName, TClass(Agent), ClassType);
      result := (Agent <> nil) and (ClassType <> nil);
    end;


end.
