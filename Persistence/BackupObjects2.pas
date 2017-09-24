unit BackupObjects;

{$DEFINE USELogs}

interface

  uses
    SysUtils, Classes, SyncObjs, BackupInterfaces, ObjectIndex, BackupConsts,
    BlockLevels;

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

  const
    bckBaseIndex        = $ffff;
    bckDefaultIndexSize = $ffff;

  type
    TBackupObject        = class;
    TBackupWriter        = class;
    TVerboseBackupWriter = class;
    TBinaryBackupWriter  = class;
    TBackupReader        = class;
    TVerboseBackupReader = class;
    TBinaryBackupReader  = class;

    TBackupObject =
      class(TInterfacedObject)
        public
          constructor Create(aStream : TStream; OwnsStream : boolean; Section : TCriticalSection);
          destructor  Destroy; override;
        protected
          fStream     : TStream;
          fOwnsStream : boolean;
          fSection    : TCriticalSection;
          fDebugStack : TStringList;
        private
          procedure Lock;
          procedure Unlock;
          function  GetCallStack : string;
        public
          property DebugStack : TStringList read fDebugStack;
          property CallStack   : string      read GetCallStack;
      end;

    TBackupWriter =
      class(TBackupObject, IBackupWriter)
        public
          procedure WriteString(const Name : string; Value : string);       virtual; stdcall; abstract;
          procedure WriteClass(const Name : string; Value : string);        virtual; stdcall; abstract;
          procedure WriteChar(const Name : string; Value : char);           virtual; stdcall; abstract;
          procedure WriteBoolean(const Name : string; Value : boolean);     virtual; stdcall; abstract;
          procedure WriteByte(const Name : string; Value : byte);           virtual; stdcall; abstract;
          procedure WriteWord(const Name : string; Value : word);           virtual; stdcall; abstract;
          procedure WriteInteger(const Name : string; Value : integer);     virtual; stdcall; abstract;
          procedure WriteSingle(const Name : string; Value : single);       virtual; stdcall; abstract;
          procedure WriteDouble(const Name : string; Value : double);       virtual; stdcall; abstract;
          procedure WriteCurrency(const Name : string; Value : currency);   virtual; stdcall; abstract;
          procedure WriteObject(const Name : string; Value : TObject);      virtual; stdcall; abstract;
          procedure WriteObjectRef(const Name : string; Value : TObject);   virtual; stdcall; abstract;
          procedure WriteLooseObject(const Name : string; Value : TObject); virtual; stdcall; abstract;
          procedure WriteMethod(const Name : string; Method : TMethod);     virtual; stdcall; abstract;
          procedure WriteBuffer(const Name : string; var buffer; size : integer); virtual; stdcall; abstract;
      end;

    TVerboseBackupWriter =
      class(TBackupWriter)
        private
          fCurLine : string;
          fIndent  : string;
        public
          procedure WriteString(const Name : string; Value : string);       override;
          procedure WriteClass(const Name : string; Value : string);       override;
          procedure WriteChar(const Name : string; Value : char);           override;
          procedure WriteBoolean(const Name : string; Value : boolean);     override;
          procedure WriteByte(const Name : string; Value : byte);           override;
          procedure WriteWord(const Name : string; Value : word);           override;
          procedure WriteInteger(const Name : string; Value : integer);     override;
          procedure WriteSingle(const Name : string; Value : single);       override;
          procedure WriteDouble(const Name : string; Value : double);       override;
          procedure WriteCurrency(const Name : string; Value : currency);   override;
          procedure WriteObject(const Name : string; Value : TObject);      override;
          procedure WriteObjectRef(const Name : string; Value : TObject);   override;
          procedure WriteLooseObject(const Name : string; Value : TObject); override;
          procedure WriteMethod(const Name : string; Method : TMethod);     override;
          procedure WriteBuffer(const Name : string; var buffer; size : integer); override;
        private
          procedure _WriteObject(const Name : string; Value : TObject; Loose : boolean);
        public
          procedure AddIndentation;
          procedure DecIndentation;
        private
          procedure WriteLine(line : string);
      end;

    TBinaryBackupWriter =
      class(TBackupWriter)
        public
          constructor Create(aStream : TStream; OwnsStream : boolean; Section : TCriticalSection);
          destructor  Destroy; override;
        private
          fClasses : TStringList;
        public
          procedure WriteString(const Name : string; Value : string);       override;
          procedure WriteClass(const Name : string; Value : string);        override;
          procedure WriteChar(const Name : string; Value : char);           override;
          procedure WriteBoolean(const Name : string; Value : boolean);     override;
          procedure WriteByte(const Name : string; Value : byte);           override;
          procedure WriteWord(const Name : string; Value : word);           override;
          procedure WriteInteger(const Name : string; Value : integer);     override;
          procedure WriteSingle(const Name : string; Value : single);       override;
          procedure WriteDouble(const Name : string; Value : double);       override;
          procedure WriteCurrency(const Name : string; Value : currency);   override;
          procedure WriteObject(const Name : string; Value : TObject);      override;
          procedure WriteObjectRef(const Name : string; Value : TObject);   override;
          procedure WriteLooseObject(const Name : string; Value : TObject); override;
          procedure WriteMethod(const Name : string; Method : TMethod);     override;
          procedure WriteBuffer(const Name : string; var buffer; size : integer); override;
      end;

    TBackupReaderNotify = procedure(Comment : string; Percent : byte) of object;

    TBackupReader =
      class(TBackupObject, IBackupReader)
        public
          constructor Create(aStream : TStream; OwnsStream : boolean);
          destructor  Destroy; override;
        protected
          procedure   FixEntry(var Entry : TIndexEntry; anObj : TObject);
        private
          fFixups  : TObjectIndex;
          fSize    : integer;
          fNotify  : TBackupReaderNotify;
        public
          property  Notify : TBackupReaderNotify read fNotify write fNotify;
        public
          function  ReadString(const Name : string; DefVal : string) : string;                virtual; stdcall; abstract;
          function  ReadClass(const Name : string; DefVal : string) : string;                 virtual; stdcall; abstract;
          function  ReadChar(const Name : string; DefVal : char) : char;                      virtual; stdcall; abstract;
          function  ReadBoolean(const Name : string; DefVal : boolean) : boolean;             virtual; stdcall; abstract;
          function  ReadByte(const Name : string; DefVal : byte) : byte;                      virtual; stdcall; abstract;
          function  ReadWord(const Name : string; DefVal : word) : word;                      virtual; stdcall; abstract;
          function  ReadInteger(const Name : string; DefVal : integer) : integer;             virtual; stdcall; abstract;
          function  ReadSingle(const Name : string; DefVal : single) : single;                virtual; stdcall; abstract;
          function  ReadDouble(const Name : string; DefVal : double) : double;                virtual; stdcall; abstract;
          function  ReadCurrency(const Name : string; DefVal : currency) : currency;          virtual; stdcall; abstract;
          function  ReadObject(const Name : string; var O; DefVal : TObject) : string;        virtual; stdcall; abstract;
          function  ReadLooseObject(const Name : string; var O; DefVal : TObject) : string;   virtual; stdcall; abstract;
          procedure ReadMethod(const Name : string; var Method : TMethod; DefVal : TMethod);  virtual; stdcall; abstract;
          procedure ReadBuffer(const Name : string; var buffer; DefVal :  pointer; size : integer);    virtual; stdcall; abstract;
        protected
          procedure DoReadObject(var anObj : TObject; anObjId : integer; const aClassName : string; Fixups : boolean);
          procedure FixObjectRef(var anObj : TObject; anObjId : integer);
          procedure Update; virtual;
      end;

    TVerboseBackupReader =
      class(TBackupReader)
        public
          constructor Create(aStream : TStream; aPath : string);
          destructor  Destroy; override;
        private
          fLevels : TMultiLevelStream;
          fLog    : TStream;
        public
          function  ReadString(const Name : string; DefVal : string) : string;               override;
          function  ReadClass(const Name : string; DefVal : string) : string;                override;
          function  ReadChar(const Name : string; DefVal : char) : char;                     override;
          function  ReadBoolean(const Name : string; DefVal : boolean) : boolean;            override;
          function  ReadByte(const Name : string; DefVal : byte) : byte;                     override;
          function  ReadWord(const Name : string; DefVal : word) : word;                     override;
          function  ReadInteger(const Name : string; DefVal : integer) : integer;            override;
          function  ReadSingle(const Name : string; DefVal : single) : single;               override;
          function  ReadDouble(const Name : string; DefVal : double) : double;               override;
          function  ReadCurrency(const Name : string; DefVal : currency) : currency;         override;
          function  ReadObject(const Name : string; var O; DefVal : TObject) : string;       override;
          function  ReadLooseObject(const Name : string; var O; DefVal : TObject) : string;  override;
          procedure ReadMethod(const Name : string; var Method : TMethod; DefVal : TMethod); override;
          procedure ReadBuffer(const Name : string; var buffer; DefVal :  pointer; size : integer); override;
        public
          procedure AddIndentation;
          procedure DecIndentation;
        private
          function  ReadProp(const Name, DefVal : string) : string;
        protected
          procedure Update; override;
      end;

    TBinaryBackupReader =
      class(TBackupReader)
        public
          constructor Create(aStream : TStream; OwnsStream : boolean);
          destructor  Destroy; override;
        private
          fClasses : TStringList;
        public
          function  ReadString(const Name : string; DefVal : string) : string;               override;
          function  ReadClass(const Name : string; DefVal : string) : string;                override;
          function  ReadChar(const Name : string; DefVal : char) : char;                     override;
          function  ReadBoolean(const Name : string; DefVal : boolean) : boolean;            override;
          function  ReadByte(const Name : string; DefVal : byte) : byte;                     override;
          function  ReadWord(const Name : string; DefVal : word) : word;                     override;
          function  ReadInteger(const Name : string; DefVal : integer) : integer;            override;
          function  ReadSingle(const Name : string; DefVal : single) : single;               override;
          function  ReadDouble(const Name : string; DefVal : double) : double;               override;
          function  ReadCurrency(const Name : string; DefVal : currency) : currency;         override;
          function  ReadObject(const Name : string; var O; DefVal : TObject) : string;       override;
          function  ReadLooseObject(const Name : string; var O; DefVal : TObject) : string;  override;
          procedure ReadMethod(const Name : string; var Method : TMethod; DefVal : TMethod); override;
          procedure ReadBuffer(const Name : string; var buffer; DefVal :  pointer; size : integer); override;
      end;

  // Backup utility functions

  function CreateBackup(Path : string; Verbose : boolean; Section : TCriticalSection) : TBackupWriter;
  function OpenBackup(Path : string; aNotProc : TBackupReaderNotify) : TBackupReader;
  function CreateBinaryBackup(Obj : TObject; Section : TCriticalSection) : TStream;
  function CreateVerboseBackup(Obj : TObject; Section : TCriticalSection) : TStream;

implementation

  uses
  {$IFNDEF USELogs}
    DelphiStreamUtils, CompStringsParser;
  {$ELSE}
    DelphiStreamUtils, CompStringsParser, Logs;
  {$ENDIF}


  type
    PObject  = ^TObject;
    TRelKind = (rkUseShared, rkBelongsShared, rkBelongs);

  const
    NoIndex    = -1;
    NoObjectID = 0;

  const
    TypeMissmathCode = 'Type missmatch error';


  procedure LogError(Exec : Exception; text, CallStack : string);
    begin
      Logs.Log('Backup', DateTimeToStr(Now) + ': ' + Exec.Message + ' ' + text + ' Stack: ' + CallStack);
      raise Exec;
    end;
                                                                              
  // TBackupObject

  constructor TBackupObject.Create(aStream : TStream; OwnsStream : boolean; Section : TCriticalSection);
    begin
      inherited Create;
      fStream := aStream;
      fOwnsStream := OwnsStream;
      fSection := Section;
      fDebugStack := TStringList.Create;
    end;

  destructor TBackupObject.Destroy;
    begin
      if fOwnsStream
        then fStream.Free;
      fDebugStack.Free;
      inherited;
    end;

  procedure TBackupObject.Lock;
    begin
      if fSection <> nil
        then fSection.Enter;
    end;

  procedure TBackupObject.Unlock;
    begin
      if fSection <> nil
        then fSection.Leave;
    end;

  function TBackupObject.GetCallStack : string;
    var
      i : integer;
    begin
      if fDebugStack.Count > 0
        then
          begin
            result := fDebugStack[0];
            for i := 1 to pred(fDebugStack.Count) do
              result := fDebugStack[i] + '->' + result;
          end
        else result := '[EMPTY]';
    end;

  // TVerboseBackupWriter

  procedure TVerboseBackupWriter.WriteString(const Name : string; Value : string);
    begin
      WriteLine(Name + EqualSign + Value);
    end;

  procedure TVerboseBackupWriter.WriteClass(const Name : string; Value : string);
    begin
      WriteLine(Name + EqualSign + Value);
    end;

  procedure TVerboseBackupWriter.WriteChar(const Name : string; Value : char);
    begin
      WriteLine(Name + EqualSign + IntToStr(length(Value)) + ' ' + Value);
    end;

  procedure TVerboseBackupWriter.WriteBoolean(const Name : string; Value : boolean);
    begin
      if Value
        then WriteLine(Name + EqualSign + 'T')
        else WriteLine(Name + EqualSign + 'F');
    end;

  procedure TVerboseBackupWriter.WriteByte(const Name : string; Value : byte);
    begin
      WriteLine(Name + EqualSign + IntToStr(Value));
    end;

  procedure TVerboseBackupWriter.WriteWord(const Name : string; Value : word);
    begin
      WriteLine(Name + EqualSign + IntToStr(Value));
    end;

  procedure TVerboseBackupWriter.WriteInteger(const Name : string; Value : integer);
    begin
      WriteLine(Name + EqualSign + IntToStr(Value));
    end;

  procedure TVerboseBackupWriter.WriteSingle(const Name : string; Value : single);
    begin
      WriteLine(Name + EqualSign + FloatToStr(Value));
    end;

  procedure TVerboseBackupWriter.WriteDouble(const Name : string; Value : double);
    begin
      WriteLine(Name + EqualSign + FloatToStr(Value));
    end;

  procedure TVerboseBackupWriter.WriteCurrency(const Name : string; Value : currency);
    begin
      WriteLine(Name + EqualSign + FloatToStr(Value));
    end;

  procedure TVerboseBackupWriter._WriteObject(const Name : string; Value : TObject; Loose : boolean);
    var
      BackupAgent : CBackupAgent;
      TheClass    : TClass;
      clsName     : string;
    begin
      if Value = nil
        then
          WriteLine(Name + EqualSign + NilValue)
        else
          begin
            try
              clsName := Value.ClassName;
            except
              on E : Exception do
                begin
                  clsName := 'Unknown';
                  LogError(E, 'Corrupt class writing object.', CallStack);
                end;
            end;
            if Loose
              then WriteLine(Name + EqualSign + clsName)
              else WriteLine(Name + EqualSign + clsName + '(' + IntToStr(integer(Value)) + ')');
            AddIndentation;
            try
              if GetBackupAgent(Value.ClassName, BackupAgent, TheClass)
                then
                  try
                    BackupAgent.Write(Self, Value)
                  except
                    on E : Exception do
                      LogError(E, 'Internal Error writing object of class ' + clsName, CallStack);
                  end
                else LogError(EInternalBackupError.Create('Error class not registered'), '', CallStack);
            finally
              DecIndentation;
            end;
          end;
    end;

  procedure TVerboseBackupWriter.WriteObject(const Name : string; Value : TObject);
    begin
      Lock;
      try
        fDebugStack.Insert(0, Name);
        _WriteObject(Name, Value, false);
      finally
        Unlock;
        fDebugStack.Delete(0);
      end;
    end;

  procedure TVerboseBackupWriter.WriteObjectRef(const Name : string; Value : TObject);
    begin
      try
        fDebugStack.Insert(0, Name + '.ref');
        if Value <> nil
          then
            try
              WriteLine(Name + EqualSign + Value.ClassName + '(' + IntToStr(integer(Value)) + ')!')
            except
              on E : Exception do
                LogError(E, 'Corrupt object class writing object reference.', CallStack);
            end
          else WriteLine(Name + EqualSign + NilValue);
      finally
        fDebugStack.Delete(0);
      end;
    end;

  procedure TVerboseBackupWriter.WriteLooseObject(const Name : string; Value : TObject);
    begin
      Lock;
      try
        fDebugStack.Insert(0, Name);
        _WriteObject(Name, Value, true);
      finally
        Unlock;
        fDebugStack.Delete(0);
      end;
    end;

  procedure TVerboseBackupWriter.WriteMethod(const Name : string; Method : TMethod);
    begin
      try
        fDebugStack.Insert(0, Name + '.Method');
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
      finally
        fDebugStack.Delete(0);
      end;
    end;

  procedure TVerboseBackupWriter.WriteBuffer(const Name : string; var buffer; size : integer);
    var
      aux : string;
      i   : integer;
      buf : TByteArray absolute buffer;
    begin
      SetLength(aux, 2 * size);
      for i := 1 to size do
        begin
          aux[2*i-1] := char($41 + byte(buf[i-1]) and $0F);
          aux[2*i]   := char($41 + ((byte(buf[i-1]) and $F0) shr 4));
        end;
      WriteString(Name, aux);
    end;

  procedure TVerboseBackupWriter.AddIndentation;
    begin
      WriteLine(BeginMark);
      fIndent := fIndent + IndentInc;
    end;

  procedure TVerboseBackupWriter.DecIndentation;
    begin
      if fIndent <> ''
        then SetLength(fIndent, Length(fIndent) - 2);
      WriteLine(EndMark);
    end;

  procedure TVerboseBackupWriter.WriteLine(line : string);
    begin
      if fIndent <> ''
        then fStream.Write(fIndent[1], Length(fIndent));
      if line <> ''
        then fStream.Write(line[1], Length(line));
      fStream.Write(LineBreak, 2);
      fCurLine := '';
    end;

  // TBackupReader

  constructor TBackupReader.Create(aStream : TStream; OwnsStream : boolean);
    begin
      inherited Create(aStream, OwnsStream, nil);
      fFixups := TObjectIndex.Create(bckDefaultIndexSize);
      fSize   := aStream.Size;
    end;

  destructor TBackupReader.Destroy;
    var
      i     : integer;
      Entry : TIndexEntry;
    begin
      for i := 0 to pred(fFixups.Count) do
        if not fFixups.Entries[i].Flg
          then
            begin
              Entry := fFixups.Entries[i];
              FixEntry(Entry, nil);
              fFixups.Entries[i] := Entry;
            end;
      fFixups.Free;
      inherited;
    end;

  procedure TBackupReader.FixEntry(var Entry : TIndexEntry; anObj : TObject);
    var
      AuxPtr : PObject;
    begin
      AuxPtr := PObject(Entry.Obj);
      while AuxPtr <> nil do
        begin
          Entry.Obj := TObject(PObject(Entry.Obj)^);
          AuxPtr^   := anObj;
          AuxPtr    := PObject(Entry.Obj);
        end;
      Entry.Obj := anObj;
      Entry.Flg := true;
    end;

  procedure TBackupReader.DoReadObject(var anObj : TObject; anObjId : integer; const aClassName : string; Fixups : boolean);

    var
      BackupAgent : CBackupAgent;
      TheClass    : TClass;

    procedure UpdateFixups;
      var
        IdxEntry : TIndexEntry;
        AuxPtr   : PObject;
        Index    : integer;
      begin
        Index := fFixups.IdIndex[anObjId];
        if Index = NoIndex
          then
            fFixups.AddEntry(IndexEntry(anObjId, anObj, true))
          else
            begin
              // Fix anything in the fixup
              IdxEntry := fFixups.Entries[Index];
              AuxPtr   := PObject(IdxEntry.Obj);
              while AuxPtr <> nil do
                begin
                  IdxEntry.Obj := TObject(PObject(IdxEntry.Obj)^);
                  AuxPtr^      := anObj;
                  AuxPtr       := PObject(IdxEntry.Obj);
                end;
              IdxEntry.Obj := anObj;
              IdxEntry.Flg := true;
              fFixups.Entries[Index] := IdxEntry;
            end;
      end;

    begin
      if GetBackupAgent(aClassName, BackupAgent, TheClass)
        then
          begin
            anObj := BackupAgent.CreateObject(TheClass);
            if anObj <> nil
              then
                try
                  if (anObjId <> NoObjectID) and Fixups
                    then UpdateFixups;
                  BackupAgent.Read(Self, anObj)
                except
                  raise EInternalBackupError.Create('Error an object of class: ' + aClassName);
                end
              else raise EInternalBackupError.Create('Error unspected incongruity')
          end
        else raise EInternalBackupError.Create('Error class ' + aClassName + ' not registered');
      Update;
    end;

  procedure TBackupReader.FixObjectRef(var anObj : TObject; anObjId : integer);
    var
      Index    : integer;
      IdxEntry : TIndexEntry;
    begin
      Index := fFixups.IdIndex[anObjId];
      if Index <> NoIndex
        then
          begin
            IdxEntry := fFixups.Entries[Index];
            if IdxEntry.Flg
              then
                anObj := IdxEntry.Obj
              else
                begin
                  // Insert the reference in the fixups
                  anObj        := IdxEntry.Obj;
                  IdxEntry.Obj := TObject(@anObj);
                  IdxEntry.Flg := false;
                  fFixups.Entries[Index] := IdxEntry;
                end;
          end
        else
          begin
            // Create the fixup for this object
            anObj := nil;
            fFixups.AddObject(anObjId, TObject(@anObj));
          end
    end;

  procedure TBackupReader.Update;
    var
      pos : double;
    begin
      if Assigned(fNotify)
        then
          begin
            pos := fStream.Position;
            fNotify('Reading...', round(100*pos/fSize));
          end;
    end;

  // TVerboseBackupReader

  constructor TVerboseBackupReader.Create(aStream : TStream; aPath : string);
    begin
      inherited Create(aStream, false);
      try
        if Assigned(fNotify)
          then fNotify('Creating index...', 0);
        fLevels := TMultiLevelStream.Create(aStream, aPath);
        fLog := TFileStream.Create(ExtractFilePath(aPath) + '\Differences.log', fmCreate);
        fLevels.Log := fLog;
      finally
        fStream.Free;
        fStream := nil;
      end
    end;

  destructor TVerboseBackupReader.Destroy;
    begin
      fLevels.Free;
      fLog.Free;
      inherited;
    end;

  function TVerboseBackupReader.ReadString(const Name : string; DefVal : string) : string;
    begin
      result := ReadProp(Name, DefVal);
    end;

  function TVerboseBackupReader.ReadClass(const Name : string; DefVal : string) : string;
    begin
      result := ReadProp(Name, DefVal);
    end;

  function TVerboseBackupReader.ReadChar(const Name : string; DefVal : char) : char;
    var
      aux : string;
    begin
      aux := ReadProp(Name, '');
      if length(aux) > 0
        then result := aux[1]
        else result := DefVal;
    end;

  function TVerboseBackupReader.ReadBoolean(const Name : string; DefVal : boolean) : boolean;
    var
      aux : string;
    begin
      aux := ReadProp(Name, '');
      if length(aux) > 0
        then result := aux[1] = 'T'
        else result := DefVal;
    end;

  function TVerboseBackupReader.ReadByte(const Name : string; DefVal : byte) : byte;
    var
      aux : string;
    begin
      aux := ReadProp(Name, '');
      if aux <> ''
        then
          try
            result := StrToInt(aux);
          except
            raise EInternalBackupError.Create(TypeMissmathCode);
          end
        else result := DefVal;
    end;

  function TVerboseBackupReader.ReadWord(const Name : string; DefVal : word) : word;
    var
      aux : string;
    begin
      aux := ReadProp(Name, '');
      if aux <> ''
        then
          try
            result := StrToInt(aux);
          except
            raise EInternalBackupError.Create(TypeMissmathCode);
          end
        else result := DefVal;
    end;

  function TVerboseBackupReader.ReadInteger(const Name : string; DefVal : integer) : integer;
    var
      aux : string;
    begin
      aux := ReadProp(Name, '');
      if aux <> ''
        then
          try
            result := StrToInt(aux);
          except
            raise EInternalBackupError.Create(TypeMissmathCode);
          end
        else result := DefVal;
    end;

  function TVerboseBackupReader.ReadSingle(const Name : string; DefVal : single) : single;
    var
      aux : string;
    begin
      aux := ReadProp(Name, '');
      if aux <> ''
        then
          try
            result := StrToFloat(aux);
          except
            raise EInternalBackupError.Create(TypeMissmathCode);
          end
        else result := DefVal;
    end;

  function TVerboseBackupReader.ReadDouble(const Name : string; DefVal : double) : double;
    var
      aux : string;
    begin
      aux := ReadProp(Name, '');
      if aux <> ''
        then
          try
            result := StrToFloat(aux);
          except
            raise EInternalBackupError.Create(TypeMissmathCode);
          end
        else result := DefVal;
    end;

  function TVerboseBackupReader.ReadCurrency(const Name : string; DefVal : currency) : currency;
    var
      aux : string;
    begin
      aux := ReadProp(Name, '');
      if aux <> ''
        then
          try
            result := StrToCurr(aux);
          except
            raise EInternalBackupError.Create(TypeMissmathCode);
          end
        else result := DefVal;
    end;

  procedure TVerboseBackupReader.ReadMethod(const Name : string; var Method : TMethod; DefVal : TMethod);
    var
      aux         : string;
      BackupAgent : CBackupAgent;
      ClassType   : TClass;
    begin
      aux := fLevels.ReadLevel(Name, NilValue);
      if aux <> NilValue
        then
          begin
            AddIndentation;
            if GetBackupAgent(ReadObject('Object', TObject(Method.Data), nil), BackupAgent, ClassType)
              then Method.Code := ClassType.MethodAddress(ReadClass('Code', ''))
              else raise EInternalBackupError.Create('Error class not registered');
            DecIndentation;
          end
        else
          begin
            Method.Data := nil;
            Method.Code := nil;
          end;
    end;

  procedure TVerboseBackupReader.ReadBuffer(const Name : string; var buffer; DefVal :  pointer; size : integer);
    var
      aux : string;
      i   : integer;
      buf : TByteArray absolute buffer;
    begin
      aux := ReadString(Name, '');
      if size <> IgnoredBufferSize
        then
          begin           
            if aux <> ''
              then
                begin
                  if length(aux) = 2*size
                    then
                      for i := 1 to size do
                        buf[i-1] := (byte(aux[2*i-1]) - $41) or ((byte(aux[2*i]) - $41) shl 4)
                    else
                      if DefVal <> nil
                        then move(DefVal^, buffer, size);
                    //else raise EInternalBackupError.Create('Wrong buffer size')
                end
              else
                if DefVal <> nil
                  then move(DefVal^, buffer, size);
          end;
    end;

  function TVerboseBackupReader.ReadObject(const Name : string; var O; DefVal : TObject) : string;
    var
      Obj : TObject absolute O;
      aux : string;
      cls : string;
      Id  : integer;
      p   : integer;
    begin
      aux := fLevels.ReadLevel(Name, NilValue);
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
                    begin
                      AddIndentation;
                      DoReadObject(Obj, NoObjectID, cls, true);
                      DecIndentation;
                    end
                  else // This object must update the fixups
                    begin
                      inc(p);
                      try
                        Id := StrToInt(GetNextStringUpTo(aux, p, ')'));
                        inc(p);
                        if p = length(aux)
                          then // This an object reference, lets fix it
                            FixObjectRef(Obj, Id)
                          else // This an object instance, lets read it
                            begin
                              AddIndentation;
                              DoReadObject(Obj, Id, cls, true);
                              DecIndentation;
                            end;
                      except
                        raise;
                      end;
                    end
              else raise EInternalBackupError.Create('No class found');
            result := cls;
          end;
    end;

  function TVerboseBackupReader.ReadLooseObject(const Name : string; var O; DefVal : TObject) : string;
    begin
      result := ReadObject(Name, O, DefVal);
    end;

  procedure TVerboseBackupReader.AddIndentation;
    begin
      fLevels.EnterLevel;
    end;

  procedure TVerboseBackupReader.DecIndentation;
    begin
      fLevels.LeaveLevel;
    end;

  function TVerboseBackupReader.ReadProp(const Name, DefVal : string) : string;
    begin
      result := fLevels.ReadString(Name, DefVal);
    end;

  procedure TVerboseBackupReader.Update;
    begin
      if Assigned(fNotify)
        then fNotify('Reading verbose...', round(100 * fLevels.Position / fLevels.Size));
    end;

  // TBinaryBackupWriter

  constructor TBinaryBackupWriter.Create(aStream : TStream; OwnsStream : boolean; Section : TCriticalSection);
    begin
      inherited Create(aStream, OwnsStream, Section);
      fClasses := TStringList.Create;
      fClasses.Duplicates := dupIgnore;
      fClasses.Add('');
    end;

  destructor TBinaryBackupWriter.Destroy;
    begin
      fClasses.Free;
      inherited;
    end;

  procedure TBinaryBackupWriter.WriteString(const Name : string; Value : string);
    var
      len : integer;
    begin
      len := length(Value);
      fStream.Write(len, sizeof(len));
      if len > 0
        then fStream.Write(Value[1], len);
    end;

  procedure TBinaryBackupWriter.WriteClass(const Name : string; Value : string);
    var
      index : integer;
    begin
      index := fClasses.IndexOf(Value);
      if index <> -1
        then WriteInteger('', bckBaseIndex + index)
        else
          begin
            WriteString('', Value);
            fClasses.Add(Value);
          end;
    end;

  procedure TBinaryBackupWriter.WriteChar(const Name : string; Value : char);
    begin
      fStream.Write(Value, sizeof(Value));
    end;

  procedure TBinaryBackupWriter.WriteBoolean(const Name : string; Value : boolean);
    begin
      fStream.Write(Value, sizeof(Value));
    end;

  procedure TBinaryBackupWriter.WriteByte(const Name : string; Value : byte);
    begin
      fStream.Write(Value, sizeof(Value));
    end;

  procedure TBinaryBackupWriter.WriteWord(const Name : string; Value : word);
    begin
      fStream.Write(Value, sizeof(Value));
    end;

  procedure TBinaryBackupWriter.WriteInteger(const Name : string; Value : integer);
    begin
      fStream.Write(Value, sizeof(Value));
    end;

  procedure TBinaryBackupWriter.WriteSingle(const Name : string; Value : single);
    begin
      fStream.Write(Value, sizeof(Value));
    end;

  procedure TBinaryBackupWriter.WriteDouble(const Name : string; Value : double);
    begin
      fStream.Write(Value, sizeof(Value));
    end;

  procedure TBinaryBackupWriter.WriteCurrency(const Name : string; Value : currency);
    begin
      fStream.Write(Value, sizeof(Value));
    end;

  procedure TBinaryBackupWriter.WriteObject(const Name : string; Value : TObject);
    var
      BackupAgent : CBackupAgent;
      TheClass    : TClass;
      clsName     : string;
    begin
      Lock;
      try
        fDebugStack.Insert(0, Name);
        if Value <> nil
          then
            try
              try
                clsName := Value.ClassName;
              except
                on E : Exception do
                  LogError(E, 'Corrupt class writing object.', CallStack);
              end;
              if GetBackupAgent(clsName, BackupAgent, TheClass)
                then
                  begin
                    WriteClass('', clsName);
                    WriteByte('', ord(rkBelongsShared));
                    WriteInteger('', integer(Value));
                    try
                      BackupAgent.Write(Self, Value);
                    except
                      on E : Exception do
                        LogError(E, 'Internal error writing object of class ' + clsName, CallStack);
                    end
                  end
                else LogError(EInternalBackupError.Create('Error class "' + Value.ClassName + '" not registered'), '', CallStack);
            except
              on E : Exception do
                LogError(E, 'Corrupt Object Class', CallStack);
            end
          else WriteString('', '');
      finally
        Unlock;
        fDebugStack.Delete(0);
      end;
    end;

  procedure TBinaryBackupWriter.WriteObjectRef(const Name : string; Value : TObject);
    var
      clsName : string;
    begin
      try
        fDebugStack.Insert(0, Name + '.ref');
        if Value <> nil
          then
            begin
              try
                clsName := Value.ClassName;
              except
                on E : Exception do
                  LogError(E, 'Corupt class writing object reference.', CallStack);
              end;
              if clsName <> ''
                then
                  begin
                    WriteClass('', clsName);
                    WriteByte('', ord(rkUseShared));
                    WriteInteger('', integer(Value));
                  end
                else WriteInteger('', 0);
            end
          else WriteInteger('', 0);
      finally
        fDebugStack.Delete(0);
      end;
    end;

  procedure TBinaryBackupWriter.WriteLooseObject(const Name : string; Value : TObject);
    var
      BackupAgent : CBackupAgent;
      TheClass    : TClass;
      clsName     : string;
    begin
      Lock;
      try
        fDebugStack.Insert(0, Name);
        if Value <> nil
          then
            begin
              try
                clsName := Value.ClassName;
              except
                on E : Exception do
                  LogError(E, 'Corrupt class writing loose object.', CallStack);
              end;
              if GetBackupAgent(clsName, BackupAgent, TheClass)
                then
                  begin
                    WriteClass('', Value.ClassName);
                    WriteByte('', ord(rkBelongs));
                    try
                      BackupAgent.Write(Self, Value);
                    except
                      on E : Exception do
                        LogError(E, 'Internal error writing loose object of class ' + clsName, CallStack);
                    end;
                  end
                else LogError(EInternalBackupError.Create('Error class "' + Value.ClassName + '" not registered'), '', CallStack);
            end
          else WriteString('', '');
      finally
        Unlock;
        fDebugStack.Delete(0);
      end;
    end;

  procedure TBinaryBackupWriter.WriteMethod(const Name : string; Method : TMethod);
    begin
      try
        fDebugStack.Insert(0, Name + '.Method');
        if (Method.Code <> nil) and (Method.Data <> nil)
          then
            with Method do
              begin
                WriteObjectRef('', Data);
                WriteClass('', TObject(Data).MethodName(Code));
              end
            else WriteInteger('', 0);
      finally
        fDebugStack.Delete(0);
      end;
    end;

  procedure TBinaryBackupWriter.WriteBuffer(const Name : string; var buffer; size : integer);
    begin
      fStream.Write(size, sizeof(size));
      fStream.Write(buffer, size);
    end;

  // TBinaryBackupReader

  constructor TBinaryBackupReader.Create(aStream : TStream; OwnsStream : boolean);
    begin
      inherited Create(aStream, OwnsStream);
      fClasses := TStringList.Create;
      fClasses.Duplicates := dupIgnore;
      fClasses.Add('');
    end;

  destructor TBinaryBackupReader.Destroy;
    begin
      fClasses.Free;
      inherited;
    end;

  function TBinaryBackupReader.ReadString(const Name : string; DefVal : string) : string;
    var
      len : integer;
    begin
      fStream.Read(len, sizeof(len));
      if len > 0
        then
          begin
            SetLength(result, len);
            fStream.Read(result[1], len);
          end
        else result := '';
    end;

  function TBinaryBackupReader.ReadClass(const Name : string; DefVal : string) : string;
    var
      token : integer;
    begin
      token := ReadInteger('', 0);
      if token > 0
        then
          if token < bckBaseIndex
            then
              begin
                fStream.Seek(fStream.Position - sizeof(token), soFromBeginning);
                result := ReadString('', '');
                fClasses.Add(result);
              end
            else
              if token - bckBaseIndex < fClasses.Count
                then result := fClasses[token - bckBaseIndex]
                else raise Exception.Create('Invalid index reading class.')
        else result := DefVal;
    end;

  function TBinaryBackupReader.ReadChar(const Name : string; DefVal : char) : char;
    begin
      fStream.Read(result, sizeof(result));
    end;

  function TBinaryBackupReader.ReadBoolean(const Name : string; DefVal : boolean) : boolean;
    begin
      fStream.Read(result, sizeof(result));
    end;

  function TBinaryBackupReader.ReadByte(const Name : string; DefVal : byte) : byte;
    begin
      fStream.Read(result, sizeof(result));
    end;

  function TBinaryBackupReader.ReadWord(const Name : string; DefVal : word) : word;
    begin
      fStream.Read(result, sizeof(result));
    end;

  function TBinaryBackupReader.ReadInteger(const Name : string; DefVal : integer) : integer;
    begin
      fStream.Read(result, sizeof(result));
    end;

  function TBinaryBackupReader.ReadSingle(const Name : string; DefVal : single) : single;
    begin
      fStream.Read(result, sizeof(result));
    end;

  function TBinaryBackupReader.ReadDouble(const Name : string; DefVal : double) : double;
    begin
      fStream.Read(result, sizeof(result));
    end;

  function TBinaryBackupReader.ReadCurrency(const Name : string; DefVal : currency) : currency;
    begin
      fStream.Read(result, sizeof(result));
    end;

  function TBinaryBackupReader.ReadObject(const Name : string; var O; DefVal : TObject) : string;
    var
      Obj     : TObject absolute O;
      cls     : string;
      RelKind : TRelKind;
      Id     : integer;
    begin
      cls := ReadClass('', '');
      if cls <> ''
        then
          begin
            RelKind := TRelKind(ReadByte('', 0));
            case RelKind of
              rkBelongs :
                DoReadObject(Obj, NoObjectId, cls, true);
              rkUseShared :
                begin
                  Id := ReadInteger('', 0);
                  FixObjectRef(Obj, Id);
                end;
              rkBelongsShared :
                begin
                  Id := ReadInteger('', 0);
                  DoReadObject(Obj, Id, cls, true);
                end;
              else Obj := nil;
            end;
          end
        else Obj := nil;
      result := cls;
    end;

  function TBinaryBackupReader.ReadLooseObject(const Name : string; var O; DefVal : TObject) : string;
    var
      Obj     : TObject absolute O;
      cls     : string;
      RelKind : TRelKind;
      Id     : integer;
    begin
      cls := ReadClass('', '');
      if cls <> ''
        then
          begin
            RelKind := TRelKind(ReadByte('', 0));
            case RelKind of
              rkBelongs :
                DoReadObject(Obj, NoObjectId, cls, false);
              rkUseShared :
                begin
                  ReadInteger('', 0);
                  //Id := ReadInteger('', 0);
                  //FixObjectRef(Obj, Id);
                end;
              rkBelongsShared :
                begin
                  Id := ReadInteger('', 0);
                  DoReadObject(Obj, Id, cls, false);
                end;
              else Obj := nil;
            end;
          end
        else Obj := nil;
      result := cls;
    end;

  procedure TBinaryBackupReader.ReadMethod(const Name : string; var Method : TMethod; DefVal : TMethod);
    var
      cls         : string;
      BackupAgent : CBackupAgent;
      ClassType   : TClass;
    begin
      cls := ReadObject('', Method.Data, nil);
      if cls <> ''
        then
          if GetBackupAgent(cls, BackupAgent, ClassType)
            then Method.Code := ClassType.MethodAddress(ReadClass('', ''))
            else raise EInternalBackupError.Create('Error class not registered ' + cls)
        else Method.Code := nil;
    end;

  procedure TBinaryBackupReader.ReadBuffer(const Name : string; var buffer; DefVal :  pointer; size : integer);
    begin
      if size <> IgnoredBufferSize
        then
          begin
            if ReadInteger('', 0) = size
              then fStream.Read(buffer, size)
              else raise EInternalBackupError.Create('Buffer sizes doesn''t match')
          end
        else
          begin
            size := ReadInteger('', 0);
            fStream.Seek(size, soFromCurrent);
          end;
    end;

  // Utility functions

  function CreateBackup(Path : string; Verbose : boolean; Section : TCriticalSection) : TBackupWriter;
    var
      Stream : TStream;
      aux    : string;
    begin
      Stream := TFileStream.Create(Path, fmCreate);
      if Verbose
        then
          begin
            aux := 'VER'^M^J;
            Stream.Write(aux[1], length(aux));
            result := TVerboseBackupWriter.Create(Stream, true, Section);
          end
        else
          begin
            aux := 'BIN'^M^J;
            Stream.Write(aux[1], length(aux));
            result := TBinaryBackupWriter.Create(Stream, true, Section);
          end;
    end;

  function OpenBackup(Path : string; aNotProc : TBackupReaderNotify) : TBackupReader;
    var
      Stream : TStream;
      aux    : string;
    begin
      Stream := TFileStream.Create(Path, fmOpenRead);
      try
        aux    := 'BIN'^M^J;
        Stream.Read(aux[1], length(aux));
        if aux = 'VER'^M^J
          then result := TVerboseBackupReader.Create(Stream, Path)
          else
            if aux = 'BIN'^M^J
              then result := TBinaryBackupReader.Create(Stream, true)
              else raise EInternalBackupError.Create('Invalid file structure');
        result.Notify := aNotProc;
      except
        Stream.Free;
        raise;
      end;
    end;

  function CreateBinaryBackup(Obj : TObject; Section : TCriticalSection) : TStream;
    var
      aux    : string;
      Backup : IBackupWriter;
    begin
      result := TMemoryStream.Create;
      aux    := 'BIN'^M^J;
      result.Write(aux[1], length(aux));
      Backup := TBinaryBackupWriter.Create(result, false, Section);
      try
        Backup.WriteObject('', Obj);
      finally
        result.Position := 0;
      end;
    end;

  function CreateVerboseBackup(Obj : TObject; Section : TCriticalSection) : TStream;
    var
      aux    : string;
      Backup : IBackupWriter;
    begin
      result := TMemoryStream.Create;
      aux    := 'VER'^M^J;
      result.Write(aux[1], length(aux));
      Backup := TVerboseBackupWriter.Create(result, false, Section);
      try
        Backup.WriteObject('World', Obj);
      finally
        result.Position := 0;
      end;
    end;

end.


