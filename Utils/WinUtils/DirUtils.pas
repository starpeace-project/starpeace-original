unit DirUtils;

// Copyright (c) 1997 JRG & Yeti, Merchise

interface

  function RemoveEmptyDirs( const Path : string ) : boolean;

  // Directory travelling

  type
    TNotifyProc = procedure ( const ItemFound : string; Attr : integer; Data : integer );

  // Travel flags
  const
    tfNotifyHidden         = $01;
    tfNotifySystem         = $02;
    tfNotifyAll            = tfNotifyHidden or tfNotifySystem;
    tfNotifyFiles          = $10;
    tfNotifySubdirectories = $20;
    tfNotifyBoth           = tfNotifyFiles or tfNotifySubdirectories;

  procedure Travel( const DirName : string; Flags : integer; Data : integer; Notification : TNotifyProc );

implementation

  uses
    Windows, SysUtils;

  function RemoveEmptyDirs( const Path : string ) : boolean;
    var
      SearchRec  : TSearchRec;
      FindResult : integer;
    begin
      FindResult := FindFirst( Path + '*.*', faDirectory, SearchRec );
      while FindResult = 0 do
        begin
          if ( SearchRec.Name <> '.' ) and ( SearchRec.Name <> '..' )
            then RemoveEmptyDirs( Path + SearchRec.Name + '\' );
          FindResult := FindNext( SearchRec );
        end;
      FindClose( SearchRec );
      Result := RemoveDirectory( pchar( Copy( Path, 1, pred( Length( Path ) ) ) ) );
    end;

  // Directory travelling

  procedure Travel( const DirName : string; Flags : integer; Data : integer; Notification : TNotifyProc );

    procedure TravelSubdirectories( const DirName : string );
      var
        SearchRec   : TSearchRec;
        FoundResult : integer;
      begin
        FoundResult := FindFirst( DirName + '\*.*', faAnyFile, SearchRec );
        try
          while FoundResult = 0 do
            with SearchRec do
              begin
                if ( Name <> '.' ) and ( Name <> '..' )
                  then
                    begin
                      if ( ( Flags and tfNotifyHidden <> 0 ) or ( Attr and faHidden = 0 ) ) and
                         ( ( Flags and tfNotifySystem <> 0 ) or ( Attr and faSysFile = 0 ) ) and
                         ( ( Flags and tfNotifySubdirectories <> 0 ) or ( Attr and faDirectory = 0 ) )
                        then Notification( DirName + '\' + Name, Attr, Data );
                      if Attr and faDirectory <> 0
                        then TravelSubdirectories( DirName + '\' + Name );
                    end;
                FoundResult := FindNext( SearchRec );
              end;
        finally
          FindClose( SearchRec );
        end;
      end;

    begin
      TravelSubdirectories( DirName );
    end;

end.
