unit ShellUtils;

interface

  uses
    StrUtils, RegUtils, SysUtils, Windows, ShellAPI , classes;

  type
    TRegExtensionInfo =
      record
        Caption     : string;
        Description : string;
        IconPath    : string;
        IconIndex   : integer;
        NeedsIcon   : boolean;
        DefaultIcon : THandle;
        Actions     : TStringList;
        Commands    : TStringList;
      end;

  procedure GetRegExtensionInfo( const Ext : string; var Info : TRegExtensionInfo );

  procedure SetAsociateIcon( const Ext : string; const Value : string; Indx :integer);

  procedure SetDescription( const Ext, value : string );

  procedure SetAction( const Ext, Action, Command : string);

  procedure SetExtension( const Ext, value : string);

  procedure DeleteAction( const Ext, Action : string);

  function  ExtensionExist( const Ext : string ) : boolean;

  var
    RegistredExtensions : TStringList;

implementation

  const
    UnkIconFile     = 'SHELL32.DLL';
    UnkIconFileIndx = 0;

 var
    UnkIcon : HICON;

  procedure GetRegExtensionInfo( const Ext : string; var Info : TRegExtensionInfo );
    var
      IconHandlerKey : HKEY;
      ShellKey       : HKEY;
      FileTypeKey    : HKEY;
      ActionKey      : HKEY;
      FileName       : string;
      Indx           : integer;
      Pos            : integer;
      Size           : word;
      Count          : integer;
      A              : array[0..MAX_PATH] of char;

    procedure SetDefaultIcon( NeedsIcon : boolean);
      begin
        Info.DefaultIcon := UnkIcon;
        Info.NeedsIcon   := NeedsIcon;
      end;

    begin
      Info.Caption  := Ext;
      Info.NeedsIcon := false;
      FileName := GetRegValue( HKEY_CLASSES_ROOT, ext);
      RegOpenKey( HKEY_CLASSES_ROOT, pchar(FileName), FileTypeKey);
      RegOpenKey( FileTypeKey, 'shell', ShellKey);
      Info.Description := GetRegValue( HKEY_CLASSES_ROOT, FileName);
      FileName := GetRegValue( FileTypeKey, 'DefaultIcon');
      if RegOpenKey( FileTypeKey, 'Shellex\IconHandler', IconHandlerKey) =  ERROR_SUCCESS
        then
          begin
            Info.NeedsIcon := true;
            RegCloseKey( IconHandlerKey);
          end
        else
          if FileName = '%1'
            then SetDefaultIcon( true )
            else Info.NeedsIcon := false;

      if FileName = ''
        then Info.DefaultIcon := UnkIcon
        else
          if FileName <> '%1'
            then
              begin
                Pos := BackPos( ',', FileName, 0);
                if pos <> 0
                  then
                    begin
                      Indx := StrToInt( RightStr( FileName, Length( FileName) - pos));
                      Delete( FileName, Pos, MaxInt);
                    end
                  else Indx := 0;
                Info.IconPath  := FileName;
                Info.IconIndex := indx;
                Info.DefaultIcon := ExtractIcon( hInstance, pchar(FileName), Indx);
                if Info.DefaultIcon = 0
                  then Info.DefaultIcon := UnkIcon;
              end;
      Count := 0;
      Info.Actions  := TStringList.Create;
      Info.Commands := TStringList.Create;
      Size := MAX_PATH + 1;
      while RegEnumKey( ShellKey, Count, A, Size ) = NO_ERROR do
        begin
          Info.Actions.Add( string( A ));
          if RegOpenKey( ShellKey, A, ActionKey) = ERROR_SUCCESS
            then FileName := GetRegValue( ActionKey, 'command' );
          RegCloseKey( ActionKey);
          Info.Commands.Add( FileName );
          inc( Count );
          Size := MAX_PATH + 1;
        end;
      RegCloseKey( ShellKey);
      RegCloseKey( FileTypeKey);
    end;

  procedure SetAsociateIcon( const Ext : string; const Value : string; Indx :integer);
    var
      FileTypeKey  : HKEY;
      FileName : string;
    begin
      FileName := GetRegValue( HKEY_CLASSES_ROOT, ext);
      RegOpenKey( HKEY_CLASSES_ROOT, pchar(FileName), FileTypeKey);
      SetRegValue( FileTypeKey, 'DefaultIcon', value + ', ' + IntToStr(Indx));
    end;

  procedure SetDescription( const Ext, value : string );
    var
      FileName : string;
    begin
      FileName := GetRegValue( HKEY_CLASSES_ROOT, ext);
      SetRegValue( HKEY_CLASSES_ROOT, FileName, value);
    end;

  procedure SetAction( const Ext, Action, Command : string);
    var
      FileName : string;
      Key      : HKEY;
    begin
      FileName := GetRegValue( HKEY_CLASSES_ROOT, ext);
      RegOpenKey( HKEY_CLASSES_ROOT, pchar(FileName), Key);
      SetRegValue( Key, 'shell\' + Action + '\Command', command);
    end;

  procedure SetExtension( const Ext, value : string);
    begin
      SetRegValue( HKEY_CLASSES_ROOT, Ext, value);
    end;

  procedure DeleteAction( const Ext, Action : string);
    var
      FileName : string;
      ShellKey : HKEY;
    begin
      FileName := GetRegValue( HKEY_CLASSES_ROOT, ext);
      RegOpenKey( HKEY_CLASSES_ROOT, pchar(FileName + '\Shell'), ShellKey);
      RegDeleteKey( ShellKey, pchar(Action));
    end;

  function ExtensionExist( const Ext : string ) : boolean;
    var
      Key : HKey;
    begin
      Result := RegistredExtensions.IndexOf( Ext ) <> -1;
    end;

var
  Count : integer;
  Size  : integer;
  Ext   : string;

initialization
  RegistredExtensions := TStringList.Create;
  Count := 0;
  Size  := MAX_PATH;
  SetLength( Ext, MAX_PATH );
  while RegEnumKey( HKEY_CLASSES_ROOT, Count, pchar(Ext), Size ) = NO_ERROR do
    begin
      SetLength( Ext, strlen( pchar(Ext) ));
      if Ext[1] = '.'
        then RegistredExtensions.Add( Ext );
      inc( Count );
      SetLength( Ext, MAX_PATH );
    end;
  RegistredExtensions.Sort;
  UnkIcon := ExtractIcon( hInstance, UnkIconFile, UnkIconFileIndx);

finalization
  RegistredExtensions.Free;
end.
