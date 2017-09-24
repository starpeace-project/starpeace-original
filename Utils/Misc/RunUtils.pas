unit RunUtils;

interface

  function GetCommandApp( const CommandLine : string ) : string;
  function GetCommandParams( const CommandLine : string ) : string;
  
implementation

  uses
    StrUtils;
    
  function GetCommandApp( const CommandLine : string ) : string;
    var
      SpacePos : integer;
    begin
      SpacePos := System.Pos( ' ', CommandLine );
      if SpacePos <> 0
        then Result := LeftStr( CommandLine, SpacePos -1 )
        else Result := '';
    end;

  function GetCommandParams( const CommandLine : string ) : string;
    var
      SpacePos : integer;
    begin
      SpacePos := System.Pos( ' ', CommandLine );
      if SpacePos <> 0
        then Result := RightStr( CommandLine, length( CommandLine ) - SpacePos + 1 )
        else Result := '';
    end;

end.

