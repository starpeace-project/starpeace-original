unit TextShortcuts;

interface

  uses
    Classes, Menus, Collection;

  type
    TShortcutInfo =
      class
        public
          constructor Create( aDesc : string; aShortcut : TShortcut; aText : string );
        private
          fDesc     : string;
          fShortcut : TShortcut;
          fText     : string;
        public
          property Desc     : string    read fDesc     write fDesc;
          property Shortcut : TShortcut read fShortcut write fShortcut;
          property Text     : string    read fText     write fText;
      end;

    TTextShortcutMger =
      class
        public
          constructor Create;
          destructor  Destroy; override;
        public
          procedure LoadShortcuts( const FileName : string );
          procedure SaveShortcuts( const FileName : string );
          function  AddShortcutText( Shortcut : TShortcut; Desc, Text : string ) : TShortcutInfo;
          procedure DeleteShortcutText( Shortcut : TShortcut );
          function  getShortcutText( ShiftState : TShiftState; Key : word; out Text : string ) : boolean;
          function  getShortcutByDesc( aDesc : string ) : TShortcutInfo;
          function  getShortcutByKey( aKey : TShortcut ) : TShortcutInfo;
        private
          fShortcuts : TCollection;
          function getCount : integer;
          function getShortcut( idx : integer) : TShortcutInfo;
        public
          property ShortcutCount : integer read getCount;
          property Shortcuts[idx : integer] : TShortcutInfo read getShortcut;
      end;

implementation

  uses
    IniFiles, SysUtils;

  // TShorcutInfo

  constructor TShortcutInfo.Create( aDesc : string; aShortcut : TShortcut; aText : string );
    begin
      inherited Create;
      fDesc     := aDesc;
      fShortcut := aShortcut;
      fText     := aText;
    end;

  // TTextShortcutMger

  constructor TTextShortcutMger.Create;
    begin
      inherited;
      fShortcuts := TCollection.Create( 20, rkBelonguer );
    end;

  destructor TTextShortcutMger.Destroy;
    begin
      fShortcuts.Free;
      inherited;
    end;

  const
    DESC_ID     = 'Desc';
    SHORTCUT_ID = 'Shortcut';
    TEXT_ID     = 'Text';

  procedure TTextShortcutMger.LoadShortcuts( const FileName : string );
    var
      ini      : TIniFile;
      sections : TStringList;
      Desc     : string;
      Shortc   : word;
      Text     : string;
      i        : integer;
    begin
      ini := TIniFile.Create( FileName );
      sections := TStringList.Create;
      try
        ini.ReadSections( sections );
        for i := 0 to pred(sections.Count) do
          begin
            Desc   := ini.ReadString( sections[i], DESC_ID, '' );
            Shortc := ini.ReadInteger( sections[i], SHORTCUT_ID, 0 );
            Text   := ini.ReadString( sections[i], TEXT_ID, '' );
            AddShortcutText( Shortc, Desc, Text );
          end;
      finally
        ini.Free;
        sections.Free;
      end;
    end;

  procedure TTextShortcutMger.SaveShortcuts( const FileName : string );
    var
      ini : TIniFile;
      i   : integer;
      StrLst : TStringList;
    begin
      ini := TIniFile.Create( FileName );
      try
        StrLst := TStringList.Create;
        ini.ReadSections( StrLst );
        for i := 0 to pred(StrLst.Count) do
          ini.EraseSection( StrLst[i] );
        for i := 0 to pred(fShortcuts.Count) do
          begin
            ini.WriteString( 'Key.' + IntToStr(i), DESC_ID, TShortcutInfo(fShortcuts[i]).Desc );
            ini.WriteInteger( 'Key.' + IntToStr(i), SHORTCUT_ID, TShortcutInfo(fShortcuts[i]).Shortcut );
            ini.WriteString( 'Key.' + IntToStr(i), TEXT_ID, TShortcutInfo(fShortcuts[i]).Text );
          end;
      finally
        ini.Free;
      end;
    end;

  function TTextShortcutMger.AddShortcutText( Shortcut : TShortcut; Desc, Text : string ) : TShortcutInfo;
    begin
      result := TShortcutInfo.Create( Desc, Shortcut, Text );
      fShortcuts.Insert( result );
    end;

  procedure TTextShortcutMger.DeleteShortcutText( Shortcut : TShortcut );
    var
      info : TShortcutInfo;
    begin
      info := getShortcutByKey( Shortcut );
      if info <> nil
        then fShortcuts.Delete( info );
    end;

  function TTextShortcutMger.getShortcutByDesc( aDesc : string ) : TShortcutInfo;
    var
      found : boolean;
      i     : integer;
    begin
      found := false;
      i     := 0;
      while (i < fShortcuts.Count) and not found do
        begin
          found := TShortcutInfo(fShortcuts[i]).Desc = aDesc;
          inc( i );
        end;
      if found
        then result := TShortcutInfo(fShortcuts[pred(i)])
        else result := nil;
    end;

  function TTextShortcutMger.getShortcutByKey( aKey : TShortcut ) : TShortcutInfo;
    var
      found : boolean;
      i     : integer;
    begin
      found := false;
      i     := 0;
      while (i < fShortcuts.Count) and not found do
        begin
          found := TShortcutInfo(fShortcuts[i]).Shortcut = aKey;
          inc( i );
        end;
      if found
        then result := TShortcutInfo(fShortcuts[pred(i)])
        else result := nil;
    end;

  function TTextShortcutMger.getShortcutText( ShiftState : TShiftState; Key : word; out Text : string ) : boolean;
    var
      TheKey : TShortcut;
      info   : TShortcutInfo;
    begin
      TheKey := Shortcut( Key, ShiftState );
      info   := getShortcutByKey( TheKey );
      result := info <> nil;
      if result
        then Text := info.Text;
    end;

  function TTextShortcutMger.getCount : integer;
    begin
      result := fShortcuts.Count;
    end;

  function TTextShortcutMger.getShortcut( idx : integer) : TShortcutInfo;
    begin
      result := TShortcutInfo(fShortcuts[idx]);
    end;

end.
