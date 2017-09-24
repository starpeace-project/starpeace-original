unit Resources;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  uses
    Windows, Icons;

  // Module mapping:

  function  LoadResourceModule( const FileName : string ) : HMODULE;
  procedure FreeResourceModule( ResourceModule : HMODULE );

  // Icon resources

  type
    PGroupIconDirEntry = ^TGroupIconDirEntry;
    TGroupIconDirEntry =
      packed record
        bWidth       : byte;        // Width, in pixels, of the image
        bHeight      : byte;        // Height, in pixels, of the image
        bColorCount  : byte;        // Number of colors in image (0 if >=8bpp)
        bReserved    : byte;        // Reserved
        wPlanes      : word;        // Color Planes
        wBitCount    : word;        // Bits per pixel
        dwBytesInRes : dword;       // how many bytes in this resource?
        nID          : word;        // the ID
      end;

    PGroupIconDir = ^TGroupIconDir;
    TGroupIconDir =
      packed record
        idReserved : word;                              // Reserved (must be 0)
        idType     : word;                              // Resource type (1 for icons)
        idCount    : word;                              // How many images?
        idEntries  : array[0..0] of TGroupIconDirEntry; // The entries for each image
      end;

  type
    PIconGroupList = ^TIconGroupList;
    TIconGroupList = array[0..0] of PGroupIconDir;

    TIconGroups =
      record
        Count : integer;
        Items : PIconGroupList;
      end;

  type
    PIconResource = pointer;                       // !!! <- Refine this

  type
    PIconResourceList = ^TIconResourceList;
    TIconResourceList = array[0..0] of PIconResource;

    TIconResources =
      record
        Count : integer;
        Items : PIconResourceList;
      end;

  type
    PResNameList = ^TResNameList;
    TResNameList = array[0..0] of pchar;

    TResNames =
      record
        Count : integer;
        Items : PResNameList;
      end;

  // Resource loading:

  function GroupSize( GroupIcon : PGroupIconDir ) : integer;

  function GetIconResourceFromIndx( Module : HMODULE; IconIndx : integer; Size : integer ) : PIconResource;
  function GetIconGroupFromIndx( Module : HMODULE; IconIndx : integer ) : PGroupIconDir;
  function GetIconGroupFromName( Module : HMODULE; IconName : PAnsiChar ) : PGroupIconDir;

  // Resource enumeration:

  function GetIconNames( Module : HMODULE; var Names : TResNames ) : integer;
  function GetIconGroups( Module : HMODULE; var IconGroups : TIconGroups ) : integer;
  function GetIconResources( Module : HMODULE; IconGroup : PGroupIconDir; var IconResources : TIconResources ) : integer;

  function GetIconResourceCount( Module : HMODULE ) : integer;
  function GetGroupIconCount( Module : HMODULE ) : integer;
  function GetLastIconResource( Module : HMODULE ) : integer;
  function GetLastGroupIcon( Module : HMODULE ) : integer;

  // Icon data

  type
    TIconData =
      record
        Group     : PGroupIconDir;
        Resources : TIconResources;
      end;

  function GetIconDataFromIndx( Module : HMODULE; IconIndx : integer; var Data : TIconData ) : integer;
  function GetIconDataFromName( Module : HMODULE; IconName : PAnsiChar; var Data : TIconData ) : integer;

  procedure FreeIconData( var Data : TIconData );

  // Shell stuff:

  function GetIconHandleFromIndx( Data : TIconData; IconIndx : integer ) : HICON;

  function BestIconIndx( IconGroup : PGroupIconDir; Width, Height : integer ) : integer;
  function GetSmallIconIndx( IconGroup : PGroupIconDir ) : integer;
  function GetLargeIconIndx( IconGroup : PGroupIconDir ) : integer;

  // Resource updating:

  type
    HUPDATE = THandle;

  function  CreateResources( const FileName : string ) : HUPDATE;
  function  UpdateResources( const FileName : string ) : HUPDATE;
  procedure EndUpdateResources( Module : HUPDATE; Discard : boolean );

  function AddIconData( Module : HUPDATE; ResName : PAnsiChar; LastIcon : integer; Data : TIconData ) : integer;
  function UpdateIconData( Module : HUPDATE; ResName : PAnsiChar; LastIcon : integer; OldGroup : PGroupIconDir; Data : TIconData ) : integer;

implementation

  uses
    SysUtils, StrUtils;

  // Module mapping:

  function LoadResourceModule( const FileName : string ) : HMODULE;
    begin
      Result := LoadLibraryEx( pchar(FileName), 0, LOAD_LIBRARY_AS_DATAFILE );
      if Debugging
        then assert( Result <> 0, 'Could not load Module ''' + FileName + ''' in Resources.LoadResourceModule' );
    end;

  procedure FreeResourceModule( ResourceModule : HMODULE );
    begin
      if Debugging
        then assert( FreeLibrary( ResourceModule ), 'Could not free Module in Resources.FreeResourceModule' )
        else FreeLibrary( ResourceModule );
    end;

  // Resource loading:

  function GroupSize( GroupIcon : PGroupIconDir ) : integer;
    begin
      Result := sizeof( TGroupIconDir ) + pred( GroupIcon.idCount ) * sizeof( TGroupIconDirEntry );
    end;

  type
    PResEnumFromIndxData = ^TResEnumFromIndxData;
    TResEnumFromIndxData =
      record
        ResType : PAnsiChar;
        ResName : pchar;
        Indx    : integer;
        Count   : integer;
      end;

  function CallbackFindFromIndx( Module : HMODULE; EnumResType, EnumResName : PAnsiChar; Param : integer ) : boolean; stdcall;
    var
      EnumFromIndxData : PResEnumFromIndxData absolute Param;
    begin
      Result := true;
      with EnumFromIndxData^ do
        if EnumResType = ResType
          then
            if succ( Indx ) < Count
              then inc( Indx )
              else
                begin
                  ResName := StrNew( EnumResName );
                  Result := false;
                end;
    end;

  function GetIconGroupFromIndx( Module : HMODULE; IconIndx : integer ) : PGroupIconDir;
    var
      EnumFromIndxData : TResEnumFromIndxData;
    begin
      with EnumFromIndxData do
        begin
          ResType := RT_GROUP_ICON;
          Count   := succ( IconIndx );
          EnumResourceNames( Module, ResType, @CallbackFindFromIndx, longint(@EnumFromIndxData) );
          Result := GetIconGroupFromName( Module, ResName );
        end;
    end;

  function GetIconGroupFromName( Module : HMODULE; IconName : PAnsiChar ) : PGroupIconDir;
    var
      ResHandle     : HRSRC;
      GlobalHandle  : HGLOBAL;
      GroupResource : PGroupIconDir;
    begin
      Result := nil;
      ResHandle := FindResource( Module, IconName, RT_GROUP_ICON );
      if ResHandle <> 0
        then
          begin
            GlobalHandle := LoadResource( Module, ResHandle );
            if (GlobalHandle <> 0)
              then
                begin
                  GroupResource := LockResource( GlobalHandle );
                  New( Result );
                  Result^ := GroupResource^;
                  // Do we need to free GlobalHandle in Win95? There are conflicts in Win32 SDK
                end;
          end;
    end;

  function GetIconResourceFromIndx( Module : HMODULE; IconIndx : integer; Size : integer ) : PIconResource;
    var
      ResHandle    : HRSRC;
      GlobalHandle : HGLOBAL;
      IconResource : PIconResource;
    begin
      Result := nil;
      ResHandle := FindResource( Module, MakeIntResource(IconIndx), RT_ICON );
      if ResHandle <> 0
        then
          begin
            GlobalHandle := LoadResource( Module, ResHandle );
            if (GlobalHandle <> 0)
              then
                begin
                  IconResource := LockResource( GlobalHandle );
                  GetMem( Result, Size );
                  Move( IconResource^, Result^, Size );
                end;
          end;
    end;

  // Resource enumeration:

  type
    PResEnumData = ^TResEnumData;
    TResEnumData =
      record
        ResType  : PAnsiChar;
        Count    : integer;
        ResNames : PResNameList;
      end;

  const
    ResAllocGranularity = 15;

  function CallbackEnumerator( Module : HMODULE; EnumResType, EnumResName : PAnsiChar; Param : integer ) : boolean; stdcall;
    var
      EnumData : PResEnumData absolute Param;
    begin
      with EnumData^ do
        if EnumResType = ResType
          then
            begin
              if Count mod ResAllocGranularity = 0
                then ReallocMem( ResNames, (Count + ResAllocGranularity) * sizeof( ResNames[0] ));
              ResNames[Count] := StrNew( EnumResName );
              inc( Count );
            end;
      Result := true;
    end;

  function GetIconNames( Module : HMODULE; var Names : TResNames ) : integer;
    var
      EnumData : TResEnumData;
    begin
      with EnumData do
        begin
          ResType  := RT_GROUP_ICON;
          Count    := 0;
          ResNames := nil;
          EnumResourceNames( Module, ResType, @CallbackEnumerator, longint(@EnumData) );
          Result := Count;
        end;
    end;

  function GetIconGroups( Module : HMODULE; var IconGroups : TIconGroups ) : integer;
    var
      Names : TResNames;
      i     : integer;
    begin
      with IconGroups do
        begin
          Count := GetIconNames( Module, Names );
          GetMem( Items, Count * sizeof(Items[0]) );
          for i := 0 to pred(Count) do
            Items[i] := GetIconGroupFromName( Module, Names.Items[i] );
          Result := Count;
        end;
    end;

  function GetIconResources( Module : HMODULE; IconGroup : PGroupIconDir; var IconResources : TIconResources ) : integer;
    var
      i : integer;
    begin
      with IconGroup^, IconResources do
        begin
          Count := idCount;
          GetMem( Items, Count * sizeof(Items[0]) );
          for i := 0 to pred(Count) do
            with idEntries[i] do
              Items[i] := GetIconResourceFromIndx( Module, nId, dwBytesInRes );
          Result := Count;    
        end;
    end;

  function GetIconResourceCount( Module : HMODULE ) : integer;
    var
      EnumFromIndxData : TResEnumFromIndxData;
    begin
      with EnumFromIndxData do
        begin
          ResType := RT_ICON;
          Count   := MaxInt;
          EnumResourceNames( Module, ResType, @CallbackFindFromIndx, longint(@EnumFromIndxData) );
          Result  := Count;
        end;
     end;

  function GetLastIconResource( Module : HMODULE ) : integer;
    var
      EnumFromIndxData : TResEnumFromIndxData;
    begin
      with EnumFromIndxData do
        begin
          ResType := RT_ICON;
          Count   := MaxInt;
          EnumResourceNames( Module, ResType, @CallbackFindFromIndx, longint(@EnumFromIndxData) );
          Result  := integer( ResName );
        end;
     end;

  function GetGroupIconCount( Module : HMODULE ) : integer;
    var
      EnumFromIndxData : TResEnumFromIndxData;
    begin
      with EnumFromIndxData do
        begin
          ResType := RT_GROUP_ICON;
          Count   := MaxInt;
          EnumResourceNames( Module, ResType, @CallbackFindFromIndx, longint(@EnumFromIndxData) );
          Result  := Count;
        end;
     end;

  function GetLastGroupIcon( Module : HMODULE ) : integer;
    var
      EnumFromIndxData : TResEnumFromIndxData;
    begin
      with EnumFromIndxData do
        begin
          ResType := RT_GROUP_ICON;
          Count   := MaxInt;
          EnumResourceNames( Module, ResType, @CallbackFindFromIndx, longint(@EnumFromIndxData) );
          Result  := integer( ResName );
        end;
     end;

  // Icon data

  function GetIconDataFromPathIndx( const PathIndx : string; var Data : TIconData ) : integer;
    var
      Pos    : integer;
      Indx   : integer;
      Path   : string;
      Module : HMODULE;
    begin
      if PathIndx <> '%1'
        then
          begin
            Pos := BackPos( ',', PathIndx, Length(PathIndx) );
            if Pos <> 0
              then
                begin
                  try
                    Indx := StrToInt( RightStr( PathIndx, Length(PathIndx) - Pos ));
                  except
                    Indx := 0;
                  end;
                  Path := copy( PathIndx, 1, Pos - 1 );
                end
              else
                begin
                  Indx := 0;
                  Path := PathIndx;
                end;
            Module := LoadResourceModule( Path );
            Result := GetIconDataFromIndx( Module, Indx, Data );
            FreeResourceModule( Module );
          end
        else
          begin
            FillChar( Data, sizeof(Data), 0);
            Result := 0;
          end;
    end;

  function GetIconDataFromIndx( Module : HMODULE; IconIndx : integer; var Data : TIconData ) : integer;
    begin
      with Data do
        begin
          Group  := GetIconGroupFromIndx( Module, IconIndx );
          Result := GetIconResources( Module, Group, Resources );
        end;
    end;

  function GetIconDataFromName( Module : HMODULE; IconName : PAnsiChar; var Data : TIconData ) : integer;
    begin
      with Data do
        begin
          Group  := GetIconGroupFromName( Module, IconName );
          Result := GetIconResources( Module, Group, Resources );
        end;
    end;

  procedure FreeIconData( var Data : TIconData );
    var
      i : integer;
    begin
      with Data, Resources do
        begin
          FreeMem( Group );
          for i := 0 to pred( Count ) do
            FreeMem( Items[i] );
          FreeMem( Items );
        end;
    end;

  const
    IconWin2x = $20000;
    IconWin3x = $30000;

  function GetIconHandleFromIndx( Data : TIconData; IconIndx : integer ) : HICON;
    begin
      with Data do
        Result := CreateIconFromResourceEx( Resources.Items[IconIndx], Group.idEntries[IconIndx].dwBytesInRes, true, IconWin3x, 0, 0, LR_LOADREALSIZE );
    end;

  // Resource updating:

  function CreateResources( const FileName : string ) : HUPDATE;
    begin
      Result := BeginUpdateResource( pchar(FileName), true );
    end;

  function UpdateResources( const FileName : string ) : HUPDATE;
    begin
      Result := BeginUpdateResource( pchar(FileName), false );
    end;

  procedure EndUpdateResources( Module : HUPDATE; Discard : boolean );
    begin
      EndUpdateResource( Module, Discard );
    end;

  function AddIconData( Module : HUPDATE; ResName : PAnsiChar; LastIcon : integer; Data : TIconData ) : integer;
    var
      i : integer;
    begin
      with Data do
        begin
          with Group^, Resources do
            for i := 0 to pred( Count ) do
              begin
                inc( LastIcon );
                UpdateResource( Module, RT_ICON, MakeIntResource(LastIcon), LANG_SYSTEM_DEFAULT, Items[i], idEntries[i].dwBytesInRes );
                idEntries[i].nId := LastIcon;
              end;
          UpdateResource( Module, RT_GROUP_ICON, ResName, LANG_SYSTEM_DEFAULT, Group, GroupSize(Group) );
        end;
      Result := LastIcon;
    end;

  function UpdateIconData( Module : HUPDATE; ResName : PAnsiChar; LastIcon : integer; OldGroup : PGroupIconDir; Data : TIconData ) : integer;
    var
      i : integer;
    begin
      // Erase old resource
      with OldGroup^ do
        begin
          for i := 0 to pred( idCount ) do
            UpdateResource( Module, RT_ICON, MakeIntResource( idEntries[i].nId ), LANG_SYSTEM_DEFAULT, nil, 0 );
          UpdateResource( Module, RT_GROUP_ICON, ResName, LANG_SYSTEM_DEFAULT, nil, 0 );
        end;

      // Add new resource
      with Data do
        begin
          with Group^, Resources do
            for i := 0 to pred( Count ) do
              begin
                inc( LastIcon );
                UpdateResource( Module, RT_ICON, MakeIntResource(LastIcon), LANG_SYSTEM_DEFAULT, Items[i], idEntries[i].dwBytesInRes );
                idEntries[i].nId := LastIcon;
              end;
          UpdateResource( Module, RT_GROUP_ICON, ResName, LANG_SYSTEM_DEFAULT, Group, GroupSize(Group) );
        end;
      Result := LastIcon;
    end;

  // Shell stuff:

  function BestIconIndx( IconGroup : PGroupIconDir; Width, Height : integer ) : integer;
    begin
      Result := LookupIconIdFromDirectoryEx( pointer(IconGroup), true, Width, Height, LR_DEFAULTCOLOR );
    end;

  function GetSmallIconIndx( IconGroup : PGroupIconDir ) : integer;
    var
      Width, Height : integer;
    begin
      GetSmallIconSize( Width, Height );
      Result := LookupIconIdFromDirectoryEx( pointer(IconGroup), true, Width, Height, LR_DEFAULTCOLOR );
    end;

  function GetLargeIconIndx( IconGroup : PGroupIconDir ) : integer;
    var
      Width, Height : integer;
    begin
      GetLargeIconSize( Width, Height );
      Result := LookupIconIdFromDirectoryEx( pointer(IconGroup), true, Width, Height, LR_DEFAULTCOLOR );
    end;

end.
