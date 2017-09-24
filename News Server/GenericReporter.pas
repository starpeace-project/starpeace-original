unit GenericReporter;

interface

  uses
    News, Classes, Collection, SysUtils, Windows;

  type
    IContext =
      interface
        function SolveIdentifier( Id : string ) : string;
      end;

  type
    TDefinition =
      class
        public
          constructor Create( filename : string );
          destructor  Destroy; override;
        private
          fRules : TCollection;
        public
          function Evaluate( Context : IContext ) : single;
      end;

    TGenericReporter =
      class( TMetaReporter, IContext )
        public
          constructor Create( aName, filename : string; aLog : ILog ); override;
          destructor  Destroy; override;
        private
          fDefinition : TDefinition;
        protected
          function StoryStrength( aNewspaper : TNewspaper ) : integer; override;
        // IContext
        private
          fNewspaper : TNewspaper;
        private
          function SolveIdentifier( Id : string ) : string;
        // IUknown
        private
          function QueryInterface(const IID: TGUID; out Obj): hresult; stdcall;
          function _AddRef  : integer; stdcall;
          function _Release : integer; stdcall;
      end;

  type
    EExpressionError =
      class( Exception )
        public
          constructor Create( const Msg : string; pos : integer );
        private
          fPos : integer;
        public
          property Position : integer read fPos;
      end;

    ERuleError = class( Exception );

  procedure RegisterReporters;
  
implementation

  uses
    CompStringsParser;


  // EExpressionError

  constructor EExpressionError.Create( const Msg : string; pos : integer );
    begin
      inherited Create( Msg );
      fPos := pos;
    end;

  type
    TSymbol =
      class
        private
          fPos : integer;
        public
          property Position : integer read fPos;
      end;


  // Identifiers

  type
    CIdentifier = class of TIdentifier;
    TIdentifier =
      class( TSymbol )
      end;

    TVariable =
      class( TIdentifier )
        public
          constructor Create( anId : string );
        private
          fId : string;
        public
          property Id : string read fId;
        public
          function Evaluate( Context : IContext ) : string;
      end;

    constructor TVariable.Create( anId : string );
      begin
        inherited Create;
        fId := uppercase(anId);
      end;

    function TVariable.Evaluate( Context : IContext ) : string;
      begin
        result := Context.SolveIdentifier( fId );
      end;

  type
    TNumericConstant =
      class( TIdentifier )
        public
          constructor Create( aValue : single );
        private
          fValue : single;
        public
          property Value : single read fValue;
      end;

    constructor TNumericConstant.Create( aValue : single );
      begin
        inherited Create;
        fValue := aValue;
      end;

  type
    TStringConstant =
      class( TIdentifier )
        public
          constructor Create( aValue : string );
        private
          fValue : string;
        public
          property Value : string read fValue;
      end;

    constructor TStringConstant.Create( aValue : string );
      begin
        inherited Create;
        fValue := aValue;
        if fValue <> ''
          then
            begin
              if fValue[1] = ''''
                then delete( fValue, 1, 1 );
              if fValue[length(fValue)] = ''''
                then delete( fValue, length(fValue), 1 );
            end;
      end;

  type
    TBooleanConstant =
      class( TIdentifier )
        public
          constructor Create( aValue : boolean );
        private
          fValue : boolean;
        public
          property Value : boolean read fValue;
      end;

    constructor TBooleanConstant.Create( aValue : boolean );
      begin
        inherited Create;
        fValue := aValue;
      end;

  function GetRealValOfIdentifier( Identifier : TIdentifier; Context : IContext ) : single;
    var
      tmpIdentifier : TStringConstant;
    begin
      if Identifier is TNumericConstant
        then result := TNumericConstant(Identifier).Value
        else
          if Identifier is TBooleanConstant
            then result := ord(TBooleanConstant(Identifier).Value)
            else
              if Identifier is TVariable
                then
                  if TVariable(Identifier).Id = 'RND'
                    then result := random
                    else
                      begin
                        tmpIdentifier := TStringConstant.Create( TVariable(Identifier).Evaluate( Context ) );
                        try
                          result := GetRealValOfIdentifier( tmpIdentifier, Context );
                        finally
                          tmpIdentifier.Free;
                        end;
                      end
                else
                  if Identifier is TStringConstant
                    then
                      try
                        result := StrToFloat( TStringConstant(Identifier).Value );
                      except
                        raise EExpressionError.Create( 'Numeric value expected', Identifier.Position );
                      end
                    else result := 0;
    end;

  function GetStringValOfIdentifier( Identifier : TIdentifier; Context : IContext ) : string;
    begin
      if Identifier is TStringConstant
        then result := TStringConstant(Identifier).Value
        else
          if Identifier is TVariable
            then result := TVariable(Identifier).Evaluate( Context )
            else
              if Identifier is TNumericConstant
                then result := FloatToStr(TNumericConstant(Identifier).Value)
                else
                  if Identifier is TBooleanConstant
                    then
                      if TBooleanConstant(Identifier).Value
                        then result := 'TRUE'
                        else result := 'FALSE'
                    else result := '';
    end;

  function GetBooleanValOfIdentifier( Identifier : TIdentifier; Context : IContext ) : boolean;
    var
      tmpIdentifier : TNumericConstant;
    begin
      if Identifier is TBooleanConstant
        then result := TBooleanConstant(Identifier).Value
        else
          if Identifier is TNumericConstant
            then result := TNumericConstant(Identifier).Value <> 0
            else
              if Identifier is TVariable
                then
                  if TVariable(Identifier).Id = 'TRUE'
                    then result := true
                    else
                      if TVariable(Identifier).Id = 'FALSE'
                        then result := false
                        else
                          try
                            tmpIdentifier := TNumericConstant.Create( StrToFloat( TVariable(Identifier).Evaluate( Context )));
                            try
                              result := GetBooleanValOfIdentifier( tmpIdentifier, Context );
                            finally
                              tmpIdentifier.Free;
                            end
                          except
                            result := false;
                          end
                else result := false;
    end;
    

  // Operators

  type
    COperator = class of TOperator;
    TOperator =
      class( TSymbol )
        public
          function Combine( IdentifierA, IdentifierB : TIdentifier; Context : IContext ) : TIdentifier; virtual; abstract;
      end;

  type
    TNumericOperator =
      class( TOperator )
        public
          function Combine( IdentifierA, IdentifierB : TIdentifier; Context : IContext ) : TIdentifier; override;
        protected
          function CombineRealValues( valA, valB : single ) : single; virtual; abstract;
      end;

    function TNumericOperator.Combine( IdentifierA, IdentifierB : TIdentifier; Context : IContext ) : TIdentifier;
      var
        valA     : single;
        valB     : single;
        opResult : single;
      begin
        valA     := GetRealValOfIdentifier( IdentifierA, Context );
        valB     := GetRealValOfIdentifier( IdentifierB, Context );
        opResult := CombineRealValues( valA, valB );
        result := TNumericConstant.Create( opResult );
        result.fPos := Position;
      end;

  type
    TAditionOperator =
      class( TNumericOperator )
        public
          function Combine( IdentifierA, IdentifierB : TIdentifier; Context : IContext ) : TIdentifier; override;
        protected
          function CombineRealValues( valA, valB : single ) : single; override;
      end;

    function TAditionOperator.Combine( IdentifierA, IdentifierB : TIdentifier; Context : IContext ) : TIdentifier;
      var
        valA : string;
        valB : string;
      begin
        try
          result := inherited Combine( IdentifierA, IdentifierB, Context );
          result.fPos := Position;
        except
          valA := GetStringValOfIdentifier( IdentifierA, Context );
          valB := GetStringValOfIdentifier( IdentifierB, Context );
          result := TStringConstant.Create( valA + valB );
          result.fPos := Position;
        end;
      end;

    function TAditionOperator.CombineRealValues( valA, valB : single ) : single;
      begin
        result := valA + valB
      end;

  type
    TSubstractionOperator =
      class( TNumericOperator )
        protected
          function CombineRealValues( valA, valB : single ) : single; override;
      end;

    function TSubstractionOperator.CombineRealValues( valA, valB : single ) : single;
      begin
        result := valA - valB;
      end;
      

  type
    TMultOperator =
      class( TNumericOperator )
        protected
          function CombineRealValues( valA, valB : single ) : single; override;
      end;

    function TMultOperator.CombineRealValues( valA, valB : single ) : single;
      begin
        result := valA*valB;
      end;


  type
    TDivOperator =
      class( TNumericOperator )
        protected
          function CombineRealValues( valA, valB : single ) : single; override;
      end;

    function TDivOperator.CombineRealValues( valA, valB : single ) : single;
      begin
        if valB <> 0
          then result := valA/valB
          else result := 0;
      end;


  type
    TComparisonOperator =
      class( TOperator )
        public
          function Combine( IdentifierA, IdentifierB : TIdentifier; Context : IContext ) : TIdentifier; override;
        protected
          function CombineRealValues  ( valA, valB : single ) : boolean; virtual; abstract;
          function CombineStringValues( valA, valB : string ) : boolean; virtual; abstract;
      end;

    function TComparisonOperator.Combine( IdentifierA, IdentifierB : TIdentifier; Context : IContext ) : TIdentifier;
      var
        realA, realB : single;
        strA, strB   : string;
        opResult     : boolean;
      begin
        if (IdentifierA is TStringConstant) or (IdentifierB is TStringConstant)
          then
            begin
              strA     := GetStringValOfIdentifier( IdentifierA, Context );
              strB     := GetStringValOfIdentifier( IdentifierB, Context );
              opResult := CombineStringValues( strA, strB );
            end
          else
            begin
              realA    := GetRealValOfIdentifier( IdentifierA, Context );
              realB    := GetRealValOfIdentifier( IdentifierB, Context );
              opResult := CombineRealValues( realA, realB );
            end;
        result := TBooleanConstant.Create( opResult );
        result.fPos := Position;
      end;

  type
    TEqualOperator =
      class( TComparisonOperator )
        protected
          function CombineRealValues  ( valA, valB : single ) : boolean; override;
          function CombineStringValues( valA, valB : string ) : boolean; override;
      end;

    function TEqualOperator.CombineRealValues( valA, valB : single ) : boolean;
      begin
        result := valA = valB
      end;

    function TEqualOperator.CombineStringValues( valA, valB : string ) : boolean;
      begin
        result := valA = valB
      end;


  type
    TNotEqualOperator =
      class( TComparisonOperator )
        protected
          function CombineRealValues  ( valA, valB : single ) : boolean; override;
          function CombineStringValues( valA, valB : string ) : boolean; override;
      end;

    function TNotEqualOperator.CombineRealValues( valA, valB : single ) : boolean;
      begin
        result := valA <> valB
      end;

    function TNotEqualOperator.CombineStringValues( valA, valB : string ) : boolean;
      begin
        result := valA <> valB
      end;


  type
    TGreaterOperator =
      class( TComparisonOperator )
        protected
          function CombineRealValues  ( valA, valB : single ) : boolean; override;
          function CombineStringValues( valA, valB : string ) : boolean; override;
      end;

    function TGreaterOperator.CombineRealValues( valA, valB : single ) : boolean;
      begin
        result := valA > valB
      end;

    function TGreaterOperator.CombineStringValues( valA, valB : string ) : boolean;
      begin
        result := valA > valB
      end;


  type
    TGreaterEqualOperator =
      class( TComparisonOperator )
        protected
          function CombineRealValues  ( valA, valB : single ) : boolean; override;
          function CombineStringValues( valA, valB : string ) : boolean; override;
      end;

    function TGreaterEqualOperator.CombineRealValues( valA, valB : single ) : boolean;
      begin
        result := valA >= valB
      end;

    function TGreaterEqualOperator.CombineStringValues( valA, valB : string ) : boolean;
      begin
        result := valA >= valB
      end;


  type
    TLesserOperator =
      class( TComparisonOperator )
        protected
          function CombineRealValues  ( valA, valB : single ) : boolean; override;
          function CombineStringValues( valA, valB : string ) : boolean; override;
      end;

    function TLesserOperator.CombineRealValues( valA, valB : single ) : boolean;
      begin
        result := valA < valB
      end;

    function TLesserOperator.CombineStringValues( valA, valB : string ) : boolean;
      begin
        result := valA < valB
      end;


  type
    TLesserEqualOperator =
      class( TComparisonOperator )
        protected
          function CombineRealValues  ( valA, valB : single ) : boolean; override;
          function CombineStringValues( valA, valB : string ) : boolean; override;
      end;

    function TLesserEqualOperator.CombineRealValues( valA, valB : single ) : boolean;
      begin
        result := valA <= valB
      end;

    function TLesserEqualOperator.CombineStringValues( valA, valB : string ) : boolean;
      begin
        result := valA <= valB
      end;


  type
    TBooleanOperator =
      class( TOperator )
        public
          function Combine( IdentifierA, IdentifierB : TIdentifier; Context : IContext ) : TIdentifier; override;
        protected
          function CombineBooleanValues( valA, valB : boolean ) : boolean; virtual; abstract;
      end;

    function TBooleanOperator.Combine( IdentifierA, IdentifierB : TIdentifier; Context : IContext ) : TIdentifier;
      var
        valA     : boolean;
        valB     : boolean;
        opResult : boolean;
      begin
        valA     := GetBooleanValOfIdentifier( IdentifierA, Context );
        valB     := GetBooleanValOfIdentifier( IdentifierB, Context );
        opResult := CombineBooleanValues( valA, valB );
        result   := TBooleanConstant.Create( opResult );
        result.fPos := Position;
      end;


  type
    TBoolOrOperator =
      class( TBooleanOperator )
        protected
          function CombineBooleanValues( valA, valB : boolean ) : boolean; override;
      end;

    function TBoolOrOperator.CombineBooleanValues( valA, valB : boolean ) : boolean;
      begin
        result := valA or valB;
      end;


  type
    TBoolAndOperator =
      class( TBooleanOperator )
        protected
          function CombineBooleanValues( valA, valB : boolean ) : boolean; override;
      end;

    function TBoolAndOperator.CombineBooleanValues( valA, valB : boolean ) : boolean;
      begin
        result := valA and valB;
      end;

  const
    RuleUndefined = -1;

  type
    TRuleOperator =
      class( TOperator )
        public
          function Combine( IdentifierA, IdentifierB : TIdentifier; Context : IContext ) : TIdentifier; override;
      end;

    function TRuleOperator.Combine( IdentifierA, IdentifierB : TIdentifier; Context : IContext ) : TIdentifier;
      var
        condition : boolean;
        value     : single;
        opResult  : single;
      begin
        condition := GetBooleanValOfIdentifier( IdentifierA, Context );
        value     := GetRealValOfIdentifier( IdentifierB, Context );
        if condition
          then opResult := value
          else opResult := RuleUndefined;
        result := TNumericConstant.Create( opResult );
        result.fPos := Position;
      end;


  var
   OperatorTypes : TStringList = nil;

  procedure InitOperatorTypes;
    begin
      OperatorTypes := TStringList.Create;
      with OperatorTypes do
        begin
          AddObject( '+',  TObject(TAditionOperator) );
          AddObject( '-',  TObject(TSubstractionOperator) );
          AddObject( '*',  TObject(TMultOperator) );
          AddObject( '/',  TObject(TDivOperator) );
          AddObject( '=',  TObject(TEqualOperator) );
          AddObject( '<>', TObject(TNotEqualOperator) );
          AddObject( '>',  TObject(TGreaterOperator) );
          AddObject( '>=', TObject(TGreaterEqualOperator) );
          AddObject( '<',  TObject(TLesserOperator) );
          AddObject( '<=', TObject(TLesserEqualOperator) );
          AddObject( '|',  TObject(TBoolOrOperator) );
          AddObject( '&',  TObject(TBoolAndOperator) );
          AddObject( ':',  TObject(TRuleOperator) );
        end;
    end;

  procedure DoneOperatorTypes;
    begin
      OperatorTypes.Free;
    end;

  function GetOperatorType( token : string ) : COperator;
    var
      idx : integer;
    begin
      idx := OperatorTypes.IndexOf( token );
      if idx <> NoIndex
        then result := COperator(OperatorTypes.Objects[idx])
        else result := nil;
    end;


  // Operation

  type
    TOperation =
      class
        public
          constructor Create( expression : string );
          destructor Destroy; override;
        private
          fItems : TCollection;
        public
          function Evaluate( Context : IContext ) : TIdentifier;
        private
          class function CreateIdentifier( token : string; pos : integer ) : TIdentifier;
          class function CreateOperator( token : string; pos : integer ) : TOperator;
      end;

    constructor TOperation.Create( expression : string );

      function GetSubExpression( expression : string; var pos : integer ) : string;
        var
          parcount : integer;
        begin
          parcount := 0;
          result   := '';
          repeat
            case expression[pos] of
              '(' : inc( parcount );
              ')' : dec( parcount );
            end;
            result := result + expression[pos];
            inc( pos );
          until (parcount = 0) or (pos > length(expression));
          if (result[1] = '(') and (result[length(result)] = ')') and (parcount = 0)
            then result := copy( result, 2, length(result) - 2 )
            else raise EExpressionError.Create( 'Error in parethesis', pos );
        end;

      procedure RenderToPolish( expression : string; Items : TCollection );
        type
          TParsingState = (stSeekLeftIdentifier, stSeekOperator, stSeekRightIdentifier);
        const
          BlankChars = [' ', #9, #10, #13];
          Operators  = ['+', '-', '*', '/', '&', '|', '=', ':', '<', '>'];
        var
          p     : integer;
          state : TParsingState;
          token : string;
          Left  : TIdentifier;
          Right : TIdentifier;
          Op    : TOperator;
        begin
          state := stSeekLeftIdentifier;
          p     := 1;
          Op    := nil;
          while (p <= length(expression)) and (expression[p] in BlankChars) do
            inc( p );
          while p <= length(expression) do
            begin
              case state of
                stSeekLeftIdentifier, stSeekRightIdentifier :
                  begin
                    case expression[p] of
                      '(' :
                        begin
                          token := GetSubExpression( expression, p );
                          RenderToPolish( token, Items );
                          if state = stSeekRightIdentifier
                            then Items.Insert( Op );
                        end;
                      '+', '*', '/', '&', '|', ':' :
                        raise EExpressionError.Create( 'Identifier expected', p );
                      else
                        begin
                          if expression[p] = ''''
                            then
                              begin
                                token := '''' + GetNextString( expression, p, [''''] ) + '''';
                                inc( p );
                              end
                            else token := GetNextString( expression, p, BlankChars + Operators );
                          if state = stSeekLeftIdentifier
                            then
                              begin
                                Left  := CreateIdentifier( token, p );
                                Items.Insert( Left );
                              end
                            else
                              begin
                                Right := CreateIdentifier( token, p );
                                Items.Insert( Right );
                                Items.Insert( Op );
                              end;
                        end
                    end;
                    state := stSeekOperator;
                  end;
                stSeekOperator :
                  if expression[p] in Operators
                    then
                      begin
                        token := '';
                        while (expression[p] in Operators) and (p <= length(expression)) do
                          begin
                            token := token + expression[p];
                            inc( p );
                          end;
                        Op := CreateOperator( token, p );
                        state := stSeekRightIdentifier;
                      end
                    else raise EExpressionError.Create( 'Operator expected', p );
              end;
              while (p <= length(expression)) and (expression[p] in BlankChars) do
                inc( p );
            end;
        end;

      begin
        inherited Create;
        fItems := TCollection.Create( 0, rkBelonguer );
        RenderToPolish( expression, fItems );
      end;

    destructor TOperation.Destroy;
      begin
        fItems.Free;
        inherited;
      end;

    function TOperation.Evaluate( Context : IContext ) : TIdentifier;
      var
        EvaluationStack  : TCollection;
        Left, Right, Res : TIdentifier;
        i, j             : integer;
      begin
        EvaluationStack := TCollection.Create( 0, rkUse );
        try
          EvaluationStack.InsertColl( fItems );
          i := 0;
          repeat
            if EvaluationStack[i] is TOperator
              then
                begin
                  Left  := TIdentifier(EvaluationStack[i - 2]);
                  Right := TIdentifier(EvaluationStack[i - 1]);
                  Res   := TOperator(EvaluationStack[i]).Combine( Left, Right, Context );
                  for j := 0 to 2 do
                    begin
                      if fItems.IndexOf( EvaluationStack[i - 2] ) = NoIndex
                        then EvaluationStack[i - 2].Free;
                      EvaluationStack.AtDelete( i - 2 );
                    end;
                  EvaluationStack.AtInsert( i - 2, Res );
                  i := i - 1;
                end
              else inc( i );
          until EvaluationStack.Count = 1;
          result := TIdentifier(EvaluationStack[0]);
        finally
          EvaluationStack.Free;
        end;
      end;

    class function TOperation.CreateIdentifier( token : string; pos : integer ) : TIdentifier;

      function IsNumeric( token : string ) : boolean;
        const
          NumChars = ['-', '0'..'9', '.'];
        var
          i : integer;
        begin
          result := true;
          i      := 1;
          while (i <= length(token)) and result do
            begin
              result := token[i] in NumChars;
              inc( i );
            end;
        end;

      begin
        if token[1] = ''''
          then result := TStringConstant.Create( token )
          else
            try
              if IsNumeric( token ) // to avoid annoying exception 
                then result := TNumericConstant.Create( StrToFloat(token) )
                else result := TVariable.Create( token );
            except
              result := TVariable.Create( token );
            end;
        result.fPos := pos;
      end;

    class function TOperation.CreateOperator( token : string; pos : integer ) : TOperator;
      var
        OperatorType : COperator;
      begin
        OperatorType := GetOperatorType( token );
        if OperatorType <> nil
          then
            begin
              result := OperatorType.Create;
              result.fPos := pos;
            end
          else raise EExpressionError.Create( 'Unknown operator "' + token + '"', pos );
      end;

  // Rule
      
  type
    TRule =
      class
        public
          constructor Create( expression : string; pos : integer );
        private
          fBody : TOperation;
          fPos  : integer;
        public
          property Position : integer read fPos;
        public
          function Evaluate( Context : IContext ) : single;
      end;

    constructor TRule.Create( expression : string; pos : integer );
      begin
        inherited Create;
        fPos  := pos;
        try
          fBody := TOperation.Create( expression );
        except
          on E : EExpressionError do
            raise ERuleError.Create( E.Message + ' (' + IntToStr(Position) + ',' + IntToStr(E.Position) + ')' );
          on E : Exception do
            raise ERuleError.Create( 'Unknown error.' );
        end;
      end;

    function TRule.Evaluate( Context : IContext ) : single;
      var
        opResult : TIdentifier;
      begin
        try
          opResult := fBody.Evaluate( Context );
          if opResult is TNumericConstant
            then result := TNumericConstant(opResult).Value
            else raise ERuleError.Create( 'Rule must evaluate to numeric value (line ' + IntToStr(Position) + ')' );
        except
          on E : EExpressionError do
            raise ERuleError.Create( E.Message + ' (line ' + IntToStr(Position) + ', char ' + IntToStr(E.Position) + ')' );
          on E : Exception do
            raise ERuleError.Create( 'Unknown error ' + ' (line ' + IntToStr(Position) + ')' );
        end;
      end;
      

  // Definition

  constructor TDefinition.Create( filename : string );
    var
      Lines : TStringList;
      i     : integer;
      Rule  : TRule;
    begin
      inherited Create;
      Lines := TStringList.Create;
      try
        Lines.LoadFromFile( filename );
        fRules := TCollection.Create( 0, rkBelonguer );
        for i := 0 to pred(Lines.Count) do
          begin
            Rule := TRule.Create( Lines[i], i + 1 );
            fRules.Insert( Rule );
          end;
      finally
        Lines.Free;
      end;
    end;

  destructor TDefinition.Destroy;
    begin
      fRules.Free;
      inherited;
    end;

  function TDefinition.Evaluate( Context : IContext ) : single;
    var
      i : integer;
    begin
      i := 0;
      repeat
        result := TRule(fRules[i]).Evaluate( Context );
        inc( i );
      until (i = fRules.Count) or (result <> RuleUndefined);
      if result = RuleUndefined
        then result := 0;
    end;


  // Generic Reporter

  constructor TGenericReporter.Create( aName, filename : string; aLog : ILog );
    begin
      inherited;
      try
        fDefinition := TDefinition.Create( filename + 'reporter.def' );
      except
        on E : Exception do
          Log.LogThis( '    (' + name + ') ' + E.Message );
      end;
    end;

  destructor TGenericReporter.Destroy;
    begin
      fDefinition.Free;
      inherited;
    end;
    
  function TGenericReporter.StoryStrength( aNewspaper : TNewspaper ) : integer;
    begin
      fNewspaper := aNewspaper;
      try
        if fDefinition <> nil
          then result := round(fDefinition.Evaluate( self ))
          else result := 0;
      except
        on E : Exception do
          begin
            Log.LogThis( '    (' + name + ') ' + E.Message );
            result := 0;
          end
      end;
    end;

  function TGenericReporter.SolveIdentifier( Id : string ) : string;
    begin
      result := SolveSymbol( Id, '0', fNewspaper );
    end;

  function TGenericReporter.QueryInterface(const IID: TGUID; out Obj): hresult; stdcall;
    begin
      pointer(Obj) := nil;
      result := E_FAIL;
    end;

  function TGenericReporter._AddRef  : integer; stdcall;
    begin
      result := 1;
    end;

  function TGenericReporter._Release : integer; stdcall;
    begin
      result := 1;
    end;


  // RegisterReporters

  procedure RegisterReporters;
    begin
      RegisterMetaReporterType( 'Auto', TGenericReporter );
    end;

  
initialization

  InitOperatorTypes;

finalization

  DoneOperatorTypes;
  
end.
