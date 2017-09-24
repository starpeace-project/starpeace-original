unit StreamToStr;

interface

  uses
    Classes;

  function  StreamToString( Stream : TStream ) : string;
  procedure StringToStream( str : string; Stream : TStream );

implementation

  uses
    SysUtils, CompStringsParser;

  const
    ByteSep = ':';

  function StreamToString( Stream : TStream ) : string;
    var
      b : byte;
    begin
      result := '';
      Stream.Position := 0;
      while Stream.Position < Stream.Size do
        begin
          Stream.ReadBuffer( b, sizeof(b) );
          result := result + IntToStr(b) + ByteSep;
        end;
    end;

  procedure StringToStream( str : string; Stream : TStream );
    var
      token : string;
      p     : integer;
      b     : byte;
    begin
      try
        p := 1;
        repeat
          token := GetNextStringUpTo( str, p, ByteSep );
          if token <> ''
            then
              begin
                b := StrToInt( token );
                Stream.WriteBuffer( b, sizeof(b) );
              end;
        until token = '';
      except
      end;
    end;

end.
