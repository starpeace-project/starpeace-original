// Streams.pas - Persistence for all objects
// Copyright (C) 1996, Merchise group /\/>

unit Streams;

interface

{$LONGSTRINGS    ON}                   // Merchise' style
{$WRITEABLECONST OFF}
{$TYPEDADDRESS   ON}

uses
  Classes, SysUtils;


type
  EInvalidProperty = class(EFilerError);

type
  TPipe =
    class
      private
        fStream : TStream;
      public
        property Stream : TStream read fStream;
        constructor Create(aStream : TStream);
        destructor  Destroy;                                                         override;
      public
        procedure RegisterPointers(const which : array of pointer);
        function  RegisterPointer(which : pointer) : integer;
      private
        Pointers : TList;
        Classes  : TList;
    end;

type
  TInput =
    class(TPipe)
      public
        constructor Create(aStream : TStream);
        destructor  Destroy;                                                                     override;
      public
        procedure Read(var buf; size : integer);
        function  ReadString : string;
        function  ReadPChar(which : pchar) : pchar;
        procedure ReadBuffer(var buf; size : longint);
        procedure ReadPointer(var which { : pointer or TObject});
	function  ReadObject : TObject;
        function  ReadMethod : TMethod;
        function  ReadClass : TClass;
        procedure ReadObjectPublishedData(obj : TObject);
      protected
        function  GetPointer(id : integer) : pointer;
        procedure FixPointer(id : integer; which : pointer);
      private
        Fixups   : TList;
    end;

type
  TOutput =
    class(TPipe)
      public
        procedure Write(const Buf; size : longint);
        procedure WriteString(which : string);
        procedure WritePChar(which : pchar);
        procedure WriteBuffer(const buf; size : longint);
        procedure WritePointer(ptr : pointer);
	procedure WriteObject(which : TObject);
        procedure WriteMethod(const method : TMethod);
        procedure WriteClass(which : TClass);
        procedure WriteObjectPublishedData(obj : TObject);
      protected
        function  PutPointer(which : pointer) : integer;
    end;

type
  CListable = class of TListable;
  TListable =
    class
      public
        class procedure Register;
    end;

type
  CStreamable = class of TStreamable;
  TStreamable =
    class(TListable)
      public
        constructor Read(Gate : TInput);                                                          virtual;
        procedure   Write(Gate : TOutput);                                                        virtual;
    end;

type
  CStreamableManager = class of TStreamableManager;
  TStreamableManager =
    class(TListable)
      public
        class function  Read(Gate : TInput) : TObject;                                  virtual;
        class procedure Write(Gate : TOutput; obj : TObject);                           virtual; 
        class function  ClassManaged : TClass;                                          virtual; abstract;
    end;

procedure RegisterClass(which : TClass);
procedure RegisterClasses(const which : array of TClass);
procedure UnRegisterClass(which : TClass);
procedure UnRegisterClasses(const which : array of TClass);
function  FindClass(const name : string): TClass;
function  GetClass(const name : string): TClass;
function  GetManager(which : TObject) : CStreamableManager;


implementation


uses
  TypInfo, Assertion;


var
  Listables : TList;


const
  vNilIndex = 0;


// -- TPipe

constructor TPipe.Create(aStream : TStream);
  begin
    fStream  := aStream;
    Pointers := TList.Create;
    Classes  := TList.Create;
    Pointers.Add(nil);             // Used as a valid reference in any model
  end;

destructor TPipe.Destroy;
  begin
    Classes.Free;
    Pointers.Free;
    fStream.Free;
    inherited;
  end;

procedure TPipe.RegisterPointers(const which : array of pointer);
  var
    i : integer;
  begin
    for i := low(which) to high(which) do
      RegisterPointer(which[i]);
  end;

function TPipe.RegisterPointer(which : pointer) : integer;
  begin
    Result := Pointers.IndexOf(which);
    if Result < 0
      then Result := Pointers.Add(which);
  end;


{ -- TInput -- }

constructor TInput.Create(aStream : TStream);
  begin
    inherited;
    Fixups := TList.Create;
  end;

destructor TInput.Destroy;
  begin
    {$ifndef _NODEBUG}
    Assert((Fixups.Count = 0), '(Fixups.Count = 0) at STREAM.PAS::TPipe.Destroy');
    {$endif}
    Fixups.Free;
    inherited;
  end;

procedure TInput.Read(var buf; size : integer);
  begin
    Stream.ReadBuffer(buf, size);
  end;

function TInput.ReadString : string;
  var
    len : integer;
  begin
    Read(len, sizeof(len));
    SetLength(Result, len);
    if len > 0
      then Read(Result[1], len);
  end;

function TInput.ReadPChar(which : pchar) : pchar;
  var
    len : integer;
  begin
    Stream.Read(len, sizeof(len));
    if which = nil
      then which := StrAlloc(len);
    Stream.ReadBuffer(which^, len);
    which[len] := #0;
    Result := which;
  end;

procedure TInput.ReadBuffer(var buf; size : longint);
  var
    id : integer;
  begin
    Read(id, sizeof(id));
    FixPointer(id, @buf);
    Stream.ReadBuffer(buf, size);
  end;

procedure TInput.ReadPointer(var which);
  var
    i, id  : integer;
    aux    : pointer absolute which;
  begin
    Read(id, sizeof(integer));
    aux := GetPointer(id);
    if aux = nil
      then
        begin
          aux := pointer(id);
          Fixups.Add(@aux);
          i := RegisterPointer(aux);
          {$ifndef _NODEBUG}
          Assert(i = id, '(i = id) at STREAM.PAS::TInput.ReadPointer');
          {$endif}
        end;
  end;

function TInput.ReadObject : TObject;
  var
    aux  : TClass;
    id   : integer;
  begin
    Read(id, sizeof(id));
    Result := GetPointer(id);
    if Result = nil
      then
        begin
          aux := GetClass(ReadString);
          if aux <> nil
            then
              if aux.InheritsFrom(TStreamable)
                then Result := CStreamable(aux).Read(Self)
                else
                  if aux.InheritsFrom(TStreamableManager)
                    then Result := CStreamableManager(aux).Read(Self);
          {$ifndef _NODEBUG}
          Assert(Result <> nil, '(Result <> nil) at STREAM.PAS::TInput.ReadObject');
          {$endif}
        end;
  end;

function TInput.ReadMethod : TMethod;
  begin
    with Result do
      begin
        data := ReadObject;
        if data <> nil
          then
            begin
              code := TObject(data).MethodAddress(ReadString);
              {$ifndef _NODEBUG}
              Assert(code <> nil, '(code <> nil) at STREAM.PAS::TInput.ReadMethod');
              {$endif}
            end
          else code := nil;
      end;
  end;

function TInput.ReadClass : TClass;
  begin
    Result := GetClass(ReadString);
  end;

procedure TInput.ReadObjectPublishedData(obj : TObject);
  var
    name   : string;
    cinfo  : PTypeInfo;
    info   : PPropInfo;
    aux    : TMethod;
  begin
    RegisterPointer(obj);
    cinfo := obj.ClassInfo;
    repeat
      name := ReadString;
      if name <> ''
        then
          begin
            info := GetPropInfo(cinfo, name);
            if info <> nil
              then
                with info^, GetTypeData(info^.PropType)^ do
                  case PropType^.Kind of
                    tkChar, tkInteger, tkEnumeration, tkSet :
                      case OrdType of
                        otSByte, otUByte : SetOrdProp(obj, info, ReadByte);
                        otSWord, otUWord : SetOrdProp(obj, info, ReadWord);
                        otSLong          : SetOrdProp(obj, info, ReadLongint);
                      end;
                    tkFloat :
                      case FloatType of
                        ftSingle :
                          SetFloatProp(obj, info, ReadSingle);
                        ftDouble :
                          SetFloatProp(obj, info, ReadDouble);
                        ftExtended :
                          SetFloatProp(obj, info, ReadExtended);
                        ftComp :
                          SetFloatProp(obj, info, ReadComp);
                      end;
                    tkString :
                      SetStrProp(obj, info, ReadString);
                    tkClass :
                      SetOrdProp(obj, info, longint(ReadObject));
                    tkMethod :
                      begin
                        ReadMethod(aux);
                        SetMethodProp(obj, info, aux);
                      end;
                    else raise EInvalidProperty.Create(name);
                  end;
          end;
    until name = '';
  end;

function TInput.GetPointer(id : integer) : pointer;
  begin
    if id < Pointers.Count
      then Result := Pointers[id]
      else Result := nil;
  end;

procedure TInput.FixPointer(id : integer; which : pointer);
  type
    TFixup = ^pointer;
  var
    i   : integer;
    aux : pointer;
  begin
    aux := pointer(id);
    for i := 0 to Fixups.Count - 1 do
      if aux = TFixup(Fixups[i])^
        then
          begin
            TFixup(Fixups[i])^ := which;
            Fixups.Delete(i);
          end;
    Fixups.Pack;
    i := Pointers.IndexOf(aux);
    if i < 0
      then i := RegisterPointer(which)  // Not found
      else Pointers[i] := which;
    {$ifndef _NODEBUG}
    Assert(i = id, '(i = id) at STREAM.PAS::TPipe.FixPointer');
    {$endif}
  end;


 { -- TOutput -- }

procedure TOutput.Write(const Buf; size : longint);
  begin
    Stream.WriteBuffer(Buf, Size);
  end;

procedure TOutput.WriteString(which : string);
  {$ifdef Win32}
  var
    len : integer;
  begin
    len := length(which);
    Stream.Write(len, sizeof(len));
    if len > 0
      then Stream.Write(which[1], len);
  end;
  {$else}
  begin
    Stream.WriteBuffer(which, sizeof(which[0]) + byte(which[0]));
  end;
  {$endif}

procedure TOutput.WritePChar(which : pchar);
  var
    len : integer;
  begin
    if which = nil
      then len := 0
      else len := StrLen(which);
    Stream.Write(len, sizeof(len));
    if len > 0
      then Stream.WriteBuffer(which^, len);
  end;

procedure TOutput.WriteBuffer(const buf; size : longint);
  var
    id : integer;
  begin
    id := RegisterPointer(@buf);
    Write(id, sizeof(id));
    Stream.WriteBuffer(buf, size);
  end;

procedure TOutput.WritePointer(ptr : pointer);
  var
    id : integer;
  begin
    id := RegisterPointer(ptr);
    Write(id, sizeof(id));
  end;

procedure TOutput.WriteObject(which : TObject);
  var
    mngr : CStreamableManager;
    save : boolean;
    id   : integer;
  begin
    save := Pointers.IndexOf(which) < 0;       // Not Registered yet.
    id   := RegisterPointer(which);
    Write(id, sizeof(id));
    if save
      then
        begin
          if which is TStreamable
            then
              begin
                WriteString(which.ClassName);
                TStreamable(which).Write(Self);
              end
            else
              begin
                mngr := GetManager(which);
                {$ifndef _NODEBUG}
                Assert(mngr <> nil, '(mngr <> nil) at STREAM.PAS::TOutput.WriteObject');
                {$endif}
                WriteString(mngr.ClassName);
                mngr.Write(Self, which);
              end;
        end;
  end;

procedure TOutput.WriteMethod(const method : TMethod);
  begin
    with method do
      begin
        WriteObject(data);
        if data <> nil
          then WriteString(TObject(data).MethodName(code));
      end;
  end;

procedure TOutput.WriteClass(which : TClass);
  begin
    if which = nil
      then WriteString('')
      else WriteString(which.ClassName);
  end;


procedure TOutput.WriteObjectPublishedData(obj : TObject);
  var
    cinfo    : PTypeInfo;
    data     : PTypeData;
    PropList : PPropList;
    i        : integer;
    ordv     : longint;
  begin
    cinfo := obj.ClassInfo;
    data  := GetTypeData(cinfo);
    getmem(PropList, data^.PropCount*sizeof(PropList^[0]));
    GetPropInfos(cinfo, PropList);
    for i := 0 to data^.PropCount - 1 do
      with PropList^[i]^ do                                        // info ~ PropList^[i] 
        if IsStoredProp(obj, PropList^[i])
          then
            begin
              WriteString(name);
              with GetTypeData(PropType)^ do
                case PropType^.Kind of
                  tkChar, tkInteger, tkEnumeration, tkSet :
                    begin
                      ordv := GetOrdProp(obj, PropList^[i]);
                      if ordv <> Default
                        then
                          case OrdType of
                            otSByte, otUByte :
                              WriteByte(ordv);
                            otSWord, otUWord :
                              WriteWord(ordv);
                            otSLong :
                              WriteLongint(ordv);
                          end;
                    end;
                  tkFloat :
                    case FloatType of
                      ftSingle :
                        WriteSingle(GetFloatProp(obj, PropList^[i]));
                      ftDouble :
                        WriteDouble(GetFloatProp(obj, PropList^[i]));
                      ftExtended :
                        WriteExtended(GetFloatProp(obj, PropList^[i]));
                      ftComp :
                        WriteComp(GetFloatProp(obj, PropList^[i]));
                    end;
                  tkString  :
                    WriteString(GetStrProp(obj, PropList^[i]));
                  tkClass :
                    WriteObject(TObject(GetOrdProp(obj, PropList^[i])));
                  tkMethod :
                    WriteMethod(GetMethodProp(obj, PropList^[i]));
                  else raise EInvalidProperty.Create(name);
                end;
            end;
    WriteString('');
  end;

function TOutput.PutPointer(which : pointer) : integer;
  begin
    Result := Pointers.IndexOf(which);
  end;


{ -- TListable -- }

class procedure TListable.Register;
  begin
    RegisterClass(Self);
  end;

class procedure TListable.Unregister;
  begin
    UnregisterClass(Self);
  end;


{ -- TStreamable -- }

constructor TStreamable.Read(Gate : TInput);
  begin
    Gate.ReadObjectPublishedData(Self);
  end;

procedure TStreamable.Write(Gate : TOutput);
  begin
    Gate.WriteObjectPublishedData(Self);
  end;


{ -- Registration -- }

procedure RegisterClass(which : TClass);
  begin
    {$ifndef _NODEBUG}
    Assert(Listables.IndexOf(which) < 0, '(Listables.IndexOf(which) < 0) at STREAM.PAS::RegisterClass');
    {$endif}
    Listables.Add(which);
  end;

procedure RegisterClasses(const which : array of TClass);
  var
    i : integer;
  begin
    for i := low(which) to high(which) do
      RegisterClass(which[i]);
  end;

procedure UnRegisterClass(which : TClass);
  begin
    Listables.Remove(which);
    Listables.Pack;
  end;

procedure UnRegisterClasses(const which : array of TClass);
  var
    i : integer;
  begin
    for i := low(which) to high(which) do
      UnregisterClass(which[i]);
  end;

function FindClass(const name : string): TClass;
  begin
    Result := GetClass(name);
    {$ifndef _NODEBUG}
    Assert(Result <> nil, '(Result <> nil) at STREAM.PAS::FindClass');
    {$endif}
  end;

function GetClass(const name : string): TClass;
  var
    i : integer;
  begin
    i := 0;
    while (i < Listables.Count) and (TClass(Listables[i]).ClassName <> name) do
      inc(i);
    if i < Listables.Count
      then Result := Listables.Items[i]
      else Result := nil;
  end;

function GetManager(which : TObject) : CStreamableManager;
  var
    i : integer;
  begin
    with Listables do
      begin
        i := 0;
        while (i < Count) and not (TClass(Items[i]).InheritsFrom(TStreamableManager) and 
                                   (which is CStreamableManager(Items[i]).ClassManaged)) do
          inc(i);
      if i < Count
        then Result := Items[i]
        else Result := nil;
    end;
  end;


initialization
  Listables := TList.Create;
finalization
  Listables.Free;
end.

function pointer(id : integer) : pointer;
  begin
    Result := pointer(id);
  end;

function ValidReference(ref : pointer) : boolean;
  begin
    {$ifdef Win32}
    Result := integer(ref) >= $10000;
    {$else}
    Result := PtrRec(ref).seg <> vInvalidSegment;
    {$endif}
  end;


