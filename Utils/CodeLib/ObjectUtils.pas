unit ObjectUtils;

interface

uses
  SysUtils, TypInfo, CoreTypes, Transfer;

type
  EInvalidProperty = class(Exception);

type
  IEnumFieldNames    = IEnumNames;
  IEnumPropertyNames = IEnumNames;
  IEnumMethodNames   = IEnumStrings;

type
  TEnumFieldNames =
    class(TInterfacedObject, IEnumFieldNames)
      public
        constructor Create(aClass : TClass);
      private // IEnumFields
        function  Next(out which : array of string) : integer;
        function  Skip(count : integer) : integer;
        procedure Reset;
      private
        fClass   : TClass;
        fCurrent : TClass;
        fCount   : integer;
        fOffset  : ^shortstring;
        procedure UpdateNewClass;
    end;

type
  TEnumPropertyNames =
    class(TInterfacedObject, IEnumPropertyNames)
      public
        constructor Create(aClass : TClass);
        destructor  Destroy;   override;
      private // IEnumFields
        function  Next(out which : array of string) : integer;
        function  Skip(count : integer) : integer;
        procedure Reset;
      private
        fList  : PPropList;
        fCount : integer;
        fIndex : integer;
    end;

type
  TEnumMethodNames =
    class(TInterfacedObject, IEnumMethodNames)
      public
        constructor Create(aClass : TClass);
      private // IEnumFields
        function  Next(out which : array of string) : integer;
        function  Skip(count : integer) : integer;
        procedure Reset;
      private
        fClass   : TClass;
        fCurrent : TClass;
        fCount   : integer;
        fOffset  : pchar;
        procedure UpdateNewClass;
    end;

type
  TObjectPropertyStorage =
    class(TInterfacedObject, IPropertyStorage)
      public
        constructor Create(obj : TObject; build : boolean);
      private // IPropertyStorage
        function GetClass : string;
        function GetName : string;
        function SetName(const which : string) : boolean;
        function GetProperty(const name : string) : string;
        function IPropertyStorage.SetProperty = CreateProperty;
        function CreateProperty(const name, value : string) : boolean;
        function EnumProperties : IEnumNames;
        function OpenStorage(const name : string) : IPropertyStorage;
        function CreateStorage(const theClass, name : string) : IPropertyStorage;
        function EnumChildren : IEnumNames;
      private
        fObject : TObject;
        fName   : string;
        fBuild  : boolean;
        constructor InternalCreate(obj : TObject; build : boolean; const name : string);
        function  GetMethodString(Info : PPropInfo) : string;
        procedure SetMethodString(Info : PPropInfo; const which : string);
        function  ExtractMethodAddr(const from : string) : pointer;
        function  FindFieldClass(const name : string) : TClass;
    end;


implementation


type
  PFieldClassTable = ^TFieldClassTable;
  TFieldClassTable =
    packed record
      Count   : smallint;
      Classes : array[word] of ^TClass;
    end;

function GetFieldClassTable(aClass : TClass) : PFieldClassTable;
  var
    aux : pointer;
  begin
    aux := pointer(pointer(pchar(aClass) + vmtFieldTable)^);
    if aux <> nil
      then Result := pointer(pointer(pchar(aux) + 2)^)
      else Result := nil;
  end;


// TEnumFieldNames

constructor TEnumFieldNames.Create(aClass : TClass);
  begin
    inherited Create;
    fClass := aClass;
    Reset;
  end;

function TEnumFieldNames.Next(out which : array of string) : integer;
  begin
    Result := 0;
    while (Result <= high(which)) and (fCurrent <> nil) do
      begin
        which[Result] := fOffset^;
        inc(Result);
        dec(fCount);
        if fCount > 0
          then inc(pchar(fOffset), length(fOffset^) + 7)
          else
            begin
              fCurrent := fCurrent.ClassParent;
              if fCurrent <> nil
                then UpdateNewClass;
            end;
      end;
  end;

function TEnumFieldNames.Skip(count : integer) : integer;
  begin
    Result := 0;
    while (Result < count) and (fCurrent <> nil) do
      begin
        inc(Result);
        dec(fCount);
        if fCount > 0
          then inc(pchar(fOffset), length(fOffset^) + 7)
          else
            begin
              fCurrent := fCurrent.ClassParent;
              if fCurrent <> nil
                then UpdateNewClass;
            end;
      end;
  end;

procedure TEnumFieldNames.Reset;
  begin
    fCurrent := fClass;
    UpdateNewClass;
  end;

procedure TEnumFieldNames.UpdateNewClass;
  var
    aux : pointer;
  begin
    aux := pointer(pointer(pchar(fCurrent) + vmtFieldTable)^);
    while (aux = nil) and (fCurrent <> nil) do
      begin
        fCurrent := fCurrent.ClassParent;
        if fCurrent <> nil
          then aux := pointer(pointer(pchar(fCurrent) + vmtFieldTable)^);
      end;
    if aux <> nil
      then
        begin
          fCount  := word(aux^);
          fOffset := @shortstring(pointer(pchar(aux) + 12)^);
        end;
  end;


// TEnumPropertyNames

constructor TEnumPropertyNames.Create(aClass : TClass);
  var
    TypeInfo : PTypeInfo;
  begin
    inherited Create;
    TypeInfo := aClass.ClassInfo;
    if TypeInfo <> nil
      then
        begin
          fCount := GetTypeData(TypeInfo).PropCount;
          getmem(fList, fCount*sizeof(fList[0]));
          GetPropInfos(TypeInfo, fList);
        end;
  end;

destructor TEnumPropertyNames.Destroy;
  begin
    freemem(fList);
    inherited;
  end;

function TEnumPropertyNames.Next(out which : array of string) : integer;
  begin
    Result := 0;
    while (Result <= high(which)) and (fIndex < fCount) do
      begin
        which[Result] := fList[fIndex].Name;
        inc(Result);
        inc(fIndex);
      end;
  end;

function TEnumPropertyNames.Skip(count : integer) : integer;
  begin
    Result := 0;
    while (Result < count) and (fIndex < fCount) do
      begin
        inc(Result);
        inc(fIndex);
      end;
  end;

procedure TEnumPropertyNames.Reset;
  begin
    fIndex := 0;
  end;


// TEnumMethodNames

constructor TEnumMethodNames.Create(aClass : TClass);
  begin
    inherited Create;
    fClass := aClass;
    Reset;
  end;

function TEnumMethodNames.Next(out which : array of string) : integer;
  type
    pstr = ^shortstring;
  begin
    Result := 0;
    while (Result <= high(which)) and (fCurrent <> nil) do
      begin
        which[Result] := pstr(fOffset + 6)^;
        inc(Result);
        dec(fCount);
        if fCount > 0
          then inc(fOffset, word(pointer(fOffset)^))
          else
            begin
              fCurrent := fCurrent.ClassParent;
              if fCurrent <> nil
                then UpdateNewClass;
            end;
      end;
  end;

function TEnumMethodNames.Skip(count : integer) : integer;
  begin
    Result := 0;
    while (Result < count) and (fCurrent <> nil) do
      begin
        inc(Result);
        dec(fCount);
        if fCount > 0
          then inc(fOffset, word(pointer(fOffset)^))
          else
            begin
              fCurrent := fCurrent.ClassParent;
              if fCurrent <> nil
                then UpdateNewClass;
            end;
      end;
  end;

procedure TEnumMethodNames.Reset;
  begin
    fCurrent := fClass;
    UpdateNewClass;
  end;

procedure TEnumMethodNames.UpdateNewClass;
  var
    aux : pointer;
  begin
    aux := pointer(pointer(pchar(fCurrent) + vmtMethodTable)^);
    while (aux = nil) and (fCurrent <> nil) do
      begin
        fCurrent := fCurrent.ClassParent;
        if fCurrent <> nil
          then aux := pointer(pointer(pchar(fCurrent) + vmtMethodTable)^);
      end;
    if aux <> nil
      then
        begin
          fCount  := word(aux^);
          fOffset := pchar(aux) + sizeof(word);
        end;
  end;


// TObjectPropertyStorage

constructor TObjectPropertyStorage.Create(obj : TObject; build : boolean);
  begin
    inherited Create;
    assert((obj <> nil) and (obj.ClassInfo <> nil));
    fObject := obj;
    fBuild  := build;
  end;

function TObjectPropertyStorage.GetClass : string;
  begin
    Result := fObject.ClassName;
  end;

function TObjectPropertyStorage.GetName : string;
  begin
    Result := fName;
  end;

function TObjectPropertyStorage.SetName(const which : string) : boolean;
  begin
    Result := false;
  end;

function TObjectPropertyStorage.GetProperty(const name : string) : string;
  var
    info : PPropInfo;
  begin
    info := GetPropInfo(fObject.ClassInfo, name);
    if info <> nil
      then
        case info.PropType^.Kind of
          tkChar, tkInteger, tkEnumeration, tkSet, tkWChar :
            Result := IntToStr(GetOrdProp(fObject, info));
          tkFloat :
            Result := FloatToStr(GetFloatProp(fObject, info));
          tkString, tkLString, tkWString :
             Result := GetStrProp(fObject, info);
          tkVariant :
            Result := GetVariantProp(fObject, info);
          tkClass :
            Result := '[' + GetTypeData(info.PropType^).ClassType.ClassName + ']';
          tkMethod :
            Result := GetMethodString(info);
          else raise EInvalidProperty.Create(info.Name);
        end
      else Result := '';
  end;

function TObjectPropertyStorage.CreateProperty(const name, value : string) : boolean;
  var
    info : PPropInfo;
  begin
    info := GetPropInfo(fObject.ClassInfo, name);
    Result := info <> nil;
    if Result
      then
        case info.PropType^.Kind of
          tkChar, tkInteger, tkEnumeration, tkSet, tkWChar :
            SetOrdProp(fObject, info, StrToInt(value));
          tkFloat :
            SetFloatProp(fObject, info, StrToFloat(value));
          tkString, tkLString, tkWString :
             SetStrProp(fObject, info, value);
          tkVariant :
            SetVariantProp(fObject, info, value);
          tkClass :
            ; // assert(which = '[' + TypeData.ClassType.ClassName + ']');
          tkMethod :
            SetMethodString(info, value);
          else raise EInvalidProperty.Create(info.Name);
        end;
  end;

function TObjectPropertyStorage.EnumProperties : IEnumNames;
  begin
    Result := TEnumPropertyNames.Create(fObject.ClassType);
  end;

function TObjectPropertyStorage.OpenStorage(const name : string) : IPropertyStorage;
  var
    aux : ^TObject;
  begin
    aux := fObject.FieldAddress(name);
    if (aux <> nil) and (aux^ <> nil)
      then Result := TObjectPropertyStorage.InternalCreate(aux^, fBuild, name)
      else Result := nil;
  end;

function TObjectPropertyStorage.CreateStorage(const theClass, name : string) : IPropertyStorage;
  var
    aux : ^TObject;
  begin
    aux := fObject.FieldAddress(name);
    if aux <> nil
      then
        begin
          if (aux^ = nil) and fBuild and (theClass <> '')
            then aux^ := FindFieldClass(theClass).Create;
          if aux^ <> nil
            then Result := TObjectPropertyStorage.InternalCreate(aux^, fBuild, name)
            else Result := nil;
        end
      else Result := nil;
  end;

function TObjectPropertyStorage.EnumChildren : IEnumNames;
  begin
    Result := TEnumFieldNames.Create(fObject.ClassType);
  end;

constructor TObjectPropertyStorage.InternalCreate(obj : TObject; build : boolean; const name : string);
  begin
    Create(obj, build);
    fName := name;
  end;

function TObjectPropertyStorage.GetMethodString(Info : PPropInfo) : string;
  type
    TParam =
      packed record
        Flags     : TParamFlags;
        ParamName : ShortString;
        TypeName  : ShortString;
      end;
    TParamList = array[1..1024] of TParam;
  var
    TypeData : PTypeData;
    Method   : TMethod;
    i        : integer;
  begin
    Method := GetMethodProp(fObject, Info);
    assert(fObject = Method.Data);
    TypeData := GetTypeData(Info.PropType^);
    if (TypeData.MethodKind = mkFunction) or (TypeData.MethodKind = mkSafeFunction)
      then Result := 'function '
      else Result := 'procedure ';
    Result := Result + fObject.ClassName + '.' + fObject.MethodName(Method.Code) + '(';
    for i := 1 to TypeData.ParamCount do
      begin
        with TParamList((@TypeData.ParamList)^)[i] do
          begin
            if pfVar in Flags
              then Result := Result + 'var '
              else
                if pfConst in Flags
                  then Result := Result + 'const '
                  else
                    if pfOut in Flags
                      then Result := Result + 'out ';
            Result := Result + ParamName + ' : ';
            if pfArray in Flags
              then Result := Result + 'array of ';
            if pfAddress in Flags
              then Result := Result + '^';
            if pfReference in Flags
              then Result := Result + '&';
            Result := Result + TypeName;
          end;
        if i < ParamCount
          then Result := Result + ', ';
      end;
    Result := Result + ')';
    if (TypeData.MethodKind = mkFunction) or (TypeData.MethodKind = mkSafeFunction)
      then Result := Result + ShortString((@TParamList((@TypeData.ParamList)^)[ParamCount + 1])^);  // ResultType
  end;

procedure TObjectPropertyStorage.SetMethodString(Info : PPropInfo; const which : string);
  var
    Method : TMethod;
  begin
    Method.Data := fObject;
    Method.Code := ExtractMethodAddr(which);
    SetMethodProp(fObject, Info, Method);
  end;

function TObjectPropertyStorage.ExtractMethodAddr(const from : string) : pointer;
  var
    aux    : pchar;
    buffer : array[byte] of char;
    len    : integer;
  begin
    aux := StrScan(pchar(from), '.') + 1;
    assert((aux <> nil) and (aux[-1] = '.'));
    len := StrScan(aux, '(') - aux;
    assert(aux[len] = '(');
    StrLCopy(buffer, aux, len);
    Result := fObject.MethodAddress(string(buffer));
  end;

function TObjectPropertyStorage.FindFieldClass(const name : string) : TClass;
  var
    i          : integer;
    ClassTable : PFieldClassTable;
    ClassType  : TClass;
  begin
    Result := nil;
    ClassType := fObject.ClassType;
    while (ClassType <> nil) and (Result = nil) do
      begin
        ClassTable := GetFieldClassTable(ClassType);
        if ClassTable <> nil
          then
            begin
              i := 0;
              repeat
                if CompareText(ClassTable.Classes[i].ClassName, name) = 0
                  then Result := ClassTable.Classes[i]^;
                inc(i);
              until (i >= ClassTable.Count) or (Result <> nil);
            end;
        ClassType := ClassType.ClassParent;
      end;
    assert(Result <> nil);
  end;



end.

