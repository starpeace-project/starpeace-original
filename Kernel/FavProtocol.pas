unit FavProtocol;

interface

  const
    fvkFolder = 0;
    fvkLink   = 1;

  const
    chrPathSeparator = '/';
    chrPropSeparator = #1;
    chrItemSeparator = #2;

  function  SerializeItemProps( Id, Kind : integer; Name, Info : string; SubItemCount : integer ) : string;
  procedure UnSerializeItemProps( ItemProps : string; out Id, Kind : integer; out Name, Info : string; out SubItemCount : integer );
  function  ExtractParentLocation( Location : string ) : string;

implementation

  uses
    CompStringsParser, SysUtils;

  function SerializeItemProps( Id, Kind : integer; Name, Info : string; SubItemCount : integer ) : string;
    begin
      result :=
        IntToStr(Id) + chrPropSeparator +
        IntToStr(Kind) + chrPropSeparator +
        Name + chrPropSeparator +
        Info + chrPropSeparator +
        IntToStr(SubItemCount) + chrPropSeparator;
    end;

  procedure UnSerializeItemProps( ItemProps : string; out Id, Kind : integer; out Name, Info : string; out SubItemCount : integer );
    var
      p : integer;
    begin
      p := 1;
      Id := StrToInt(GetNextStringUpTo( ItemProps, p, chrPropSeparator ));
      Kind := StrToInt(GetNextStringUpTo( ItemProps, p, chrPropSeparator ));
      Name := GetNextStringUpTo( ItemProps, p, chrPropSeparator );
      Info := GetNextStringUpTo( ItemProps, p, chrPropSeparator );
      SubItemCount := StrToInt(GetNextStringUpTo( ItemProps, p, chrPropSeparator ));
    end;

  function ExtractParentLocation( Location : string ) : string;
    var
      idStr : string;
      oldpos, pos : integer;
    begin
      if Location <> ''
        then
          begin
            pos := 1;
            repeat
              oldpos := pos;
              idStr  := GetNextStringUpTo( Location, pos, chrPathSeparator );
              if idStr <> ''
                then
                  begin
                    result := system.copy( Location, 1, oldpos - 1 );
                  end;
            until idStr = '';
          end
        else result := '';
    end;

end.
