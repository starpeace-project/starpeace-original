unit chCtoPAS;

interface

  function ChangeCHeaderToPas(CHeader : string) : string;
  function ChangeCDefineToPasConst(CDef : string) : string;

implementation

uses Col;

{Auxiliar data types}

type
  TParameter =
    class
      public
        VarName : string;
        VarType : string;
      public
        constructor Create(aVarName, aVarType : string);
    end;

type
  TParameterList =
    class
      private
        fParams : TCollection;
      private
        function GetParam(index : integer) : TParameter;
        function GetCount : integer;
      public
        constructor Create;
        destructor  Destroy; override;
        procedure   Add(VarName, VarType : string);
      public
        property Params[index : integer] : TParameter read GetParam; default;
        property Count : integer read GetCount;
    end;

{TParameter ...}

constructor TParameter.Create;
  begin
    inherited Create;
    VarName := aVarName;
    VarType := aVarType;
  end;

{TParameterList ...}

function TParameterList.GetParam(index : integer) : TParameter;
  begin
    Result := TParameter(fParams[index]);
  end;

function TParameterList.GetCount : integer;
  begin
    Result := fParams.Count;
  end;

constructor TParameterList.Create;
  begin
    inherited Create;
    fParams := TCollection.Create(5, 5, rkBelonguer);
  end;

destructor TParameterList.Destroy;
  begin
    fParams.Free;
    inherited Destroy;
  end;

procedure TParameterList.Add(VarName, VarType : string);
  begin
    fParams.Insert(TParameter.Create(VarName, VarType));
  end;

{Auxilar variables}

var
  FunctionName : string;
  ReturnType   : string;
  ParamList    : TParameterList;
  Sentence     : string;
  p            : integer;

{Auxiliar subroutines}

procedure SkipSpaces;
  begin
    while (p < length(Sentence)) and (Sentence[p] in [#0..#$20]) do inc(p);
  end;

function ReadIdentifier : string;
  begin
    Result := '';
    while (p < length(Sentence)) and (Sentence[p] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
      begin
        Result := Result + Sentence[p];
        inc(p);
      end;
  end;

function ReadBlock : string;
  begin
    Result := '';
    while (p < length(Sentence)) and not (Sentence[p] in [#0..#$20]) do
      begin
        Result := Result + Sentence[p];
        inc(p);
      end;
  end;

procedure ReadParameter;
  var
    vn : string;
    vt : string;
  begin
    SkipSpaces;
    vt := ReadIdentifier;
    SkipSpaces;
    vn := ReadIdentifier;
    ParamList.Add(vn, vt);
  end;

procedure ReadParameters;
  var
    NextChar : char;
  begin
    SkipSpaces;
    if (p <= length(Sentence)) and (Sentence[p] <> ')')
      then
        begin
          ReadParameter;
          SkipSpaces;
          if p <= length(Sentence)
            then NextChar := Sentence[p]
            else NextChar := #0;
          while NextChar = ',' do
            begin
              inc(p); // Avoid COMA
              ReadParameter;
              SkipSpaces;
              if p <= length(Sentence)
                then NextChar := Sentence[p]
                else NextChar := #0;
            end;
        end;
  end;

function BuildParams : string;
  var
    i : integer;
  begin
    Result := '';
    if ParamList.Count > 0
      then
        begin
          Result := ParamList[0].VarName + ' : ' + ParamList[0].VarType;
          i := 1;
          while i < ParamList.Count do
            begin
              Result := Result + '; ' + ParamList[i].VarName + ' : ' + ParamList[i].VarType;
              inc(i);
            end;
          Result := '(' + Result + ')';
        end;
  end;

function BuildPascalHeader : string;
  begin
    if ReturnType = 'void'
      then Result := 'procedure '
      else Result := 'function ';
    Result := Result + FunctionName + BuildParams;
    if ReturnType <> 'void'
      then Result := Result + ' : ' + ReturnType + ';'
      else Result := Result + ';';
  end;

{Main routine}

function ChangeCHeaderToPas(CHeader : string) : string;
  begin
    {Initialize}
    ParamList    := TParameterList.Create;
    Sentence     := CHeader;
    p            := 1;
    {Read tokens}
    SkipSpaces;
    ReturnType := ReadIdentifier;
    SkipSpaces;
    FunctionName := ReadIdentifier;
    SkipSpaces;
    inc(p); // avoid '('
    ReadParameters;
    inc(p); // avoid ')'
    if FunctionName = '' // Consider: FunctName(ParamList)
      then
        begin
          FunctionName := ReturnType;
          ReturnType   := 'integer';
        end;
    {Build PASCAL sentence}
    Result := BuildPascalHeader;
    {Get rid of the list}
    ParamList.Free;
  end;

function ChangeCDefineToPasConst(CDef : string) : string;
  var
    decl, value : string;
  begin
    Sentence := CDef;
    p        := 0;
    SkipSpaces;
    ReadBlock;
    SkipSpaces;
    decl  := ReadBlock;
    SkipSpaces;
    value := ReadBlock;
    SkipSpaces;
    result := '    ' + decl + ' := ' + value + ' ' + ReadBlock;
  end;

end.
