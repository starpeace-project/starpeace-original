unit RDOUtils;

interface

  uses
    Windows;

  const
    UnknownPriority = MAXLONG;

  // String scanning utilities

  function  KeyWordPos( KeyWord : string; Text : string ) : integer;
  procedure SkipSpaces( StringText : string; var ScanPos : integer );
  function  ReadIdent( StringText : string; var ScanPos : integer )   : string;
  function  ReadNumber( StringText : string; var ScanPos : integer )  : string;
  function  ReadLiteral( StringText : string; var ScanPos : integer ) : string;
  function  EndOfStringText( ScanPos, StringLen : integer ) : boolean;

  // RDO string encoding utilities

  function RDOStrEncode( str : string ) : string;
  function RDOStrDecode( str : string ) : string;

  function RDOWideStrEncode( str : widestring ) : widestring;
  function RDOWideStrDecode( str : widestring ) : widestring;

  function WideStrToStr( widestr : widestring ) : string;
  function StrToWideStr( str : string ) : widestring;

  // Conversion utilities

  function GetVariantFromStr( VarAsStr : string ) : variant;
  function GetStrFromVariant( aVariant : variant; out IllegalVType : boolean ) : string;
  function GetTypeIdFromVariant( aVariant : variant; out IllegalVType : boolean ) : string;

  function PriorityIdToPriority( PriorityId : char )  : integer;
  function PriorityToPriorityId( Priority : integer ) : char;

  // Query text manipulation

  function GetQueryText( var RawText : string ) : string;

implementation

  uses
    SysUtils, RDOProtocol
    {$IFDEF VER140}
      ,Variants
    {$ENDIF}
    ;

  // Support routines

  function IsDigit( aChar : char ) : boolean;
    begin
      Result := aChar in [ '0' .. '9' ]
    end;

  function IsHexaDigit( aChar : char ) : boolean;
    begin
      Result := ( aChar in [ '0' .. '9' ] ) or ( aChar in [ 'A' .. 'F' ] ) or ( aChar in [ 'a' .. 'f' ] )
    end;

  function IsLetter( aChar : char ) : boolean;
    begin
      Result := ( aChar in [ 'A' .. 'Z' ] ) or ( aChar in [ 'a' .. 'z' ] )
    end;

  function IsValidFirstIdentChar( aChar : char ) : boolean;
    begin
      Result := IsLetter( aChar ) or ( aChar = '_' )
    end;

  function IsValidIdentChar( aChar : char ) : boolean;
    begin
      Result := IsLetter( aChar ) or IsDigit( aChar ) or ( aChar = '_' )
    end;

  // String scanning utilities

  function KeyWordPos( KeyWord : string; Text : string ) : integer;
    var
      TextIdx      : integer;
      TextLen      : integer;
      KeyWordIdx   : integer;
      KeyWordLen   : integer;
      KeyWrdPos    : integer;
      EndOfLiteral : boolean;
    begin
      KeyWrdPos := 0;
      TextIdx := 1;
      TextLen := Length( Text );
      KeyWordLen := Length( KeyWord );
      while ( TextIdx <= TextLen ) and ( KeyWrdPos = 0 ) do
        if UpCase( Text[ TextIdx ] ) = UpCase( KeyWord[ 1 ] )
          then
            begin
              KeyWordIdx := 2;
              while ( KeyWordIdx <= KeyWordLen ) and ( UpCase( Text[ TextIdx + KeyWordIdx - 1 ] ) = UpCase( KeyWord[ KeyWordIdx ] ) ) do
                inc( KeyWordIdx );
              if KeyWordIdx > KeyWordLen
                then
                  KeyWrdPos := TextIdx
                else
                  inc( TextIdx )
            end
          else
            if Text[ TextIdx ] = LiteralDelim
              then
                repeat
                  inc( TextIdx );
                  while ( TextIdx <= TextLen ) and ( Text[ TextIdx ] <> LiteralDelim ) do
                    inc( TextIdx );
                  inc( TextIdx );
                  if ( TextIdx <= TextLen ) and ( Text[ TextIdx ] = LiteralDelim )
                    then
                      EndOfLiteral := false
                    else
                      EndOfLiteral := true;
                until EndOfLiteral
              else
                inc( TextIdx );
      Result := KeyWrdPos
    end;

  function ReadIdent( StringText : string; var ScanPos : integer ) : string;
    var
      StringLen : integer;
    begin
      Result := '';
      StringLen := Length( StringText );
      if not EndOfStringText( ScanPos, StringLen ) and IsValidFirstIdentChar( StringText[ ScanPos ] )
        then
          begin
            Result := StringText[ ScanPos ];
            inc( ScanPos );
            while not EndOfStringText( ScanPos, StringLen ) and IsValidIdentChar( StringText[ ScanPos ] ) do
              begin
                Result := Result + StringText[ ScanPos ];
                inc( ScanPos )
              end
          end
    end;

  function ReadNumber( StringText : string; var ScanPos : integer ) : string;
    var
      StringLen : integer;
    begin
      Result := '';
      StringLen := Length( StringText );
      if not EndOfStringText( ScanPos, StringLen )
        then
          if IsDigit( StringText[ ScanPos ] )
            then
              begin
                Result := StringText[ ScanPos ];
                inc( ScanPos );
                while not EndOfStringText( ScanPos, StringLen ) and IsDigit( StringText[ ScanPos ] ) do
                  begin
                    Result := Result + StringText[ ScanPos ];
                    inc( ScanPos )
                  end
              end
            else
              if StringText[ ScanPos ] = '$'
                then
                  begin
                    inc( ScanPos );
                    while not EndOfStringText( ScanPos, StringLen ) and IsHexaDigit( StringText[ ScanPos ] ) do
                      begin
                        Result := Result + StringText[ ScanPos ];
                        inc( ScanPos )
                      end
                  end
    end;

  function ReadLiteral( StringText : string; var ScanPos : integer ) : string;
    var
      Delimiter    : char;
      StringLen     : integer;
      EndOfLiteral : boolean;
    begin
      Result := '';
      StringLen := Length( StringText );
      if not EndOfStringText( ScanPos, StringLen )
        then
          begin
            Delimiter := StringText[ ScanPos ];
            if ( Delimiter = Quote ) or ( Delimiter = LiteralDelim )
              then
                begin
                  if not EndOfStringText( ScanPos, StringLen )
                    then
                      begin
                        inc( ScanPos );
                        repeat
                          while not EndOfStringText( ScanPos, StringLen ) and ( StringText[ ScanPos ] <> Delimiter ) do
                            begin
                              Result := Result + StringText[ ScanPos ];
                              inc( ScanPos )
                            end;
                          EndOfLiteral := true;
                          if not EndOfStringText( ScanPos, StringLen )
                            then
                              begin
                                inc( ScanPos );
                                if not EndOfStringText( ScanPos, StringLen ) and ( StringText[ ScanPos ] = Delimiter )
                                  then
                                    begin
                                      EndOfLiteral := false;
                                      inc( ScanPos );
                                      Result := Result + Delimiter
                                    end
                              end;
                        until EndOfLiteral;
                      end
                end
              else
                if IsDigit( Delimiter )
                  then
                    Result := ReadNumber( StringText, ScanPos )
                  else
                    if IsValidFirstIdentChar( Delimiter )
                      then
                        Result := ReadIdent( StringText, ScanPos )
          end
    end;

  procedure SkipSpaces( StringText : string; var ScanPos : integer );
    var
      StringLen : integer;
    begin
      StringLen := Length( StringText );
      while not EndOfStringText( ScanPos, StringLen ) and ( StringText[ ScanPos ] in WhiteSpace ) do
        inc( ScanPos )
    end;

  function EndOfStringText( ScanPos, StringLen : integer ) : boolean;
    begin
      Result := ScanPos > StringLen
    end;

  // RDO string encoding utilities

  function RDOStrEncode( str : string ) : string;
    var
      i : integer;
    begin
      for i := length(str) downto 1 do
        if str[i] = LiteralDelim
          then insert( LiteralDelim, str, i );
      result := str;
    end;

  function RDOStrDecode( str : string ) : string;
    var
      i : integer;
    begin
      for i := length(str) downto 2 do
        if (str[i] = LiteralDelim) and (str[pred(i)] = LiteralDelim)
          then delete( str, i, 1 );
      result := str;
    end;

  function WideStrToStr( widestr : widestring ) : string;
    begin
      result := widestr;
    end;

  function xWideStrToStr( widestr : widestring ) : string;
    var
      i : integer;
    begin
      setlength( result, 2*length(widestr) );
      for i := 1 to length(widestr) do
        begin
          result[2*pred(i) + 1] := char(hi(word(widestr[i])));
          result[2*pred(i) + 2] := char(lo(word(widestr[i])));
        end;
    end;

  function StrToWideStr( str : string ) : widestring;
    begin
      result := str;
    end;

  function xStrToWideStr( str : string ) : widestring;
    var
      i : integer;
    begin
      setlength( result, length(str) div 2 + length(str) mod 2 );
      for i := 1 to length(result) do
        word(result[i]) := (word(str[2*pred(i) + 1]) shl 8 or byte(str[2*pred(i) + 2]));
    end;

  function RDOWideStrEncode( str : widestring ) : widestring;
    var
      i : integer;
    begin
      for i := length( str ) downto 1 do
        if str[ i ] = LiteralDelim
          then insert( LiteralDelim, str, i );
      result := str;
    end;

  function RDOWideStrDecode( Str : widestring ) : widestring;
    var
      i : integer;
    begin
      for i := length( Str ) downto 2 do
        if ( str[ i ] = LiteralDelim ) and ( str[ pred( i ) ] = LiteralDelim )
          then delete( str, i, 1 );
      result := str;
    end;

  // Variant to and from text conversion utilities

  function GetVariantFromStr( VarAsStr : string ) : variant;
    var
      TypeId : char;
      tmp    : variant;
    begin
      if VarAsStr <> ''
        then
          begin
            TypeId := VarAsStr[ 1 ];
            Delete( VarAsStr, 1, 1 );
            if Length( VarAsStr ) <> 0
              then tmp := VarAsStr
              else tmp := '';
            case TypeId of
              OrdinalId:
                if tmp <> ''
                  then VarCast( Result, tmp, varInteger )
                  else Result := 0;
              SingleId:
                if tmp <> ''
                  then VarCast( Result, tmp, varSingle )
                  else Result := 0;
              DoubleId:
                if tmp <> ''
                  then VarCast( Result, tmp, varDouble )
                  else Result := 0;
              StringId:
                Result := RDOStrDecode( tmp );
              OLEStringId:
                Result := StrToWideStr( RDOStrDecode( tmp ) );
              VariantId:
                TVarData( Result ).VType := varVariant;
              VoidId:
                Result := UnAssigned
              else
                raise Exception.Create( '' )
            end
          end
        else Result := UnAssigned
    end;

  function GetStrFromVariant( aVariant : variant; out IllegalVType : boolean ) : string;
    var
      d : double;
    begin
      IllegalVType := false;
      try
        case TVarData( aVariant ).VType and varTypeMask of
          varSmallint, varInteger, varError, varBoolean, varByte:
            Result := OrdinalId + VarAsType( aVariant, varString );
          varSingle:
            Result := SingleId + VarAsType( aVariant, varString );
          varDouble, varDate, varCurrency:
            begin
              d := aVariant;
              Result := DoubleId + VarAsType( d{aVariant}, varString );
            end;
          varString:
            Result := StringId + RDOStrEncode( aVariant );
          varOleStr:
            Result := OLEStringId + RDOStrEncode( WideStrToStr( aVariant ) );
          varVariant:
            Result := VariantId + VarAsType( aVariant, varString );
          varEmpty:
            Result := VoidId
          else
            begin
              Result := #0;
              IllegalVType := true
            end
        end
      except
        IllegalVType := true
      end
    end;

  function GetTypeIdFromVariant( aVariant : variant; out IllegalVType : boolean ) : string;
    begin
      IllegalVType := false;
      try
        case TVarData( aVariant ).VType and varTypeMask of
          varSmallint, varInteger, varError, varBoolean, varByte:
            Result := OrdinalId;
          varSingle:
            Result := SingleId;
          varDouble, varDate, varCurrency:
            Result := DoubleId;
          varOleStr, varString:
            Result := StringId;
          varVariant:
            Result := VariantId;
          varEmpty:
            Result := VoidId
          else
            begin
              Result := #0;
              IllegalVType := true
            end
        end
      except
        IllegalVType := true
      end
    end;

  function PriorityIdToPriority( PriorityId : char ) : integer;
    begin
      case PriorityId of
        NormPrio:
          Result := THREAD_PRIORITY_NORMAL;
        AboveNormPrio:
          Result := THREAD_PRIORITY_ABOVE_NORMAL;
        BelowNormPrio:
          Result := THREAD_PRIORITY_BELOW_NORMAL;
        HighestPrio:
          Result := THREAD_PRIORITY_HIGHEST;
        IdlePrio:
          Result := THREAD_PRIORITY_IDLE;
        LowestPrio:
          Result := THREAD_PRIORITY_LOWEST;
        TimeCritPrio:
          Result := THREAD_PRIORITY_TIME_CRITICAL
        else
          Result := MAXLONG
      end
    end;

  function PriorityToPriorityId( Priority : integer ) : char;
    begin
      case Priority of
        THREAD_PRIORITY_LOWEST:
          Result := LowestPrio;
        THREAD_PRIORITY_BELOW_NORMAL:
          Result := BelowNormPrio;
        THREAD_PRIORITY_NORMAL:
          Result := NormPrio;
        THREAD_PRIORITY_HIGHEST:
          Result := HighestPrio;
        THREAD_PRIORITY_ABOVE_NORMAL:
          Result := AboveNormPrio;
        THREAD_PRIORITY_TIME_CRITICAL:
          Result := TimeCritPrio;
        THREAD_PRIORITY_IDLE:
          Result := IdlePrio
        else
          Result := NormPrio
      end
    end;

  // Query text manipulation

  function GetQueryText( var RawText : string  ) : string;
    var
      SemiColIdx : integer;
    begin
      SemiColIdx := KeyWordPos( QueryTerm, RawText );
      if SemiColIdx <> 0
        then
          begin
            Result := Copy( RawText, 1, SemiColIdx );
            Delete( RawText, 1, SemiColIdx )
          end
        else
          Result := ''
    end;

end.
