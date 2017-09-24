unit Icons;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  uses
    Windows, SysUtils, ShellAPI;

  function GetIconFromPathIndx( const PathIndx : string ) : HICON;

  function GetSmallIconFromFile( const FileName : string ) : HICON;
  function GetSmallIconFromResource( const ResName : string ) : HICON;
  function GetLargeIconFromFile( const FileName : string ) : HICON;
  function GetLargeIconFromResource( const ResName : string ) : HICON;

  //

  procedure GetSmallIconSize( var Width, Height : integer );
  procedure GetLargeIconSize( var Width, Height : integer );

implementation

  uses
    StrUtils;

  procedure GetSmallIconSize( var Width, Height : integer );
    begin
      Width  := GetSystemMetrics( SM_CXSMICON );
      Height := GetSystemMetrics( SM_CYSMICON );
    end;

  procedure GetLargeIconSize( var Width, Height : integer );
    begin
      Width  := GetSystemMetrics( SM_CXICON );
      Height := GetSystemMetrics( SM_CYICON );
    end;

  //

  function GetIconFromPathIndx( const PathIndx : string ) : HICON;
    var
      Pos   : integer;
      Indx  : integer;
      hInst : integer;
    begin
      if (PathIndx <> '%1') and (PathIndx <> '')
        then
          try
            Pos := BackPos( ',', PathIndx, 0 );
            if Pos <> 0
              then
                begin
                  try
                    Indx := StrToInt( RightStr( PathIndx, Length(PathIndx) - Pos ));
                  except
                    Indx := 0;
                  end;
                  if Indx < 0
                    then
                      begin
                        hInst := LoadLibrary( pchar(copy( PathIndx, 1, Pos - 1 )) );
                        if hInst <> 0
                          then
                            begin
                              Result := LoadIcon( hInst, MAKEINTRESOURCE( -Indx ) );
                              FreeLibrary( hInst );
                            end
                          else Result := 0;
                      end
                    else Result := ExtractIcon( hInstance, pchar(copy( PathIndx, 1, Pos - 1 )), Indx );
                end
              else Result := ExtractIcon( hInstance, pchar(PathIndx), 0 );
          except
            Result := 0;
          end
        else Result := 0;
    end;

  //
    
  function GetSmallIconFromFile( const FileName : string ) : HICON;
    var
      Width, Height : integer;
    begin
      GetSmallIconSize( Width, Height );
      Result := LoadImage( hInstance, pchar(FileName), IMAGE_ICON, Width, Height, LR_DEFAULTCOLOR or LR_LOADFROMFILE );
    end;

  function GetSmallIconFromResource( const ResName : string ) : HICON;
    var
      Width, Height : integer;
    begin
      GetSmallIconSize( Width, Height );
      Result := LoadImage( hInstance, pchar(ResName), IMAGE_ICON, Width, Height, LR_DEFAULTCOLOR );
    end;

  function GetLargeIconFromFile( const FileName : string ) : HICON;
    var
      Width, Height : integer;
    begin
      GetLargeIconSize( Width, Height );
      Result := LoadImage( hInstance, pchar(FileName), IMAGE_ICON, Width, Height, LR_DEFAULTCOLOR or LR_LOADFROMFILE);
    end;

  function GetLargeIconFromResource( const ResName : string ) : HICON;
    var
      Width, Height : integer;
    begin
      GetLargeIconSize( Width, Height );
      Result := LoadImage( hInstance, pchar(ResName), IMAGE_ICON, Width, Height, LR_DEFAULTCOLOR );
    end;

end.

