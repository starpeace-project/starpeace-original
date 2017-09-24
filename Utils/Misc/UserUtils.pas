unit UserUtils;

interface

  function FormatUserName( const aUser : string ) : string;

implementation

  uses
    SysUtils;

  function FormatUserName( const aUser : string ) : string;
    var
      UpStr, LoStr : string;
    begin
      UpStr := UpperCase( aUser );
      LoStr := LowerCase( aUser );
      if ( aUser = LoStr ) or ( aUser = UpStr )
        then
          if length( aUser ) > 3
            then Result := UpStr[1] + copy( LoStr, 2, length( LoStr ) )
            else Result := UpStr
        else Result := aUser;
    end;

end.
