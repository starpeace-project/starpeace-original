unit DirectoryManager;

interface

  uses DBTables;

  // Node Type

  const
    ntKey       = 0;
    ntBoolean   = 1;
    ntInteger   = 2;
    ntFloat     = 3;
    ntString    = 4;
    ntDate      = 5;
    ntCurrency  = 6;
    ntBigString = 7;

  type
    TDirectoryManager =
      class
        public
          constructor Create( aTableName : widestring; SetSecurity : wordbool );
          destructor  Destroy; override;

          function GetCurrentKey : olevariant;                             safecall;
          function SetCurrentKey( FullPathKey : widestring ) : olevariant; safecall;

          function CreateFullPathKey( FullPathKey : widestring; ForcePath : wordbool ) : olevariant; safecall;
          function CreateKey        ( KeyName     : widestring ) : olevariant;                       safecall;

          function FullPathKeyExists( FullPathKey : widestring ) : olevariant; safecall;
          function KeyExists        ( KeyName     : widestring ) : olevariant; safecall;

          function KeysCount   : olevariant; safecall;
          function ValuesCount : olevariant; safecall;

          function GetKeyNames   : olevariant; safecall;
          function GetValueNames : olevariant; safecall;

          function WriteBoolean    ( Name : widestring; Value : wordbool ) : olevariant;  safecall;
          function WriteInteger    ( Name : widestring; Value : integer ) : olevariant;   safecall;
          function WriteFloat      ( Name : widestring; Value : double ) : olevariant;    safecall;
          function WriteString     ( Name, Value : widestring ) : olevariant;             safecall;
          function WriteDate       ( Name : wideString; Value : TDateTime ) : olevariant; safecall;
          function WriteDateFromStr( Name, Value : widestring ) : olevariant;             safecall;
          function WriteCurrency   ( Name : widestring; Value : currency ) : olevariant;  safecall;

          function ReadBoolean  ( Name : widestring ) : olevariant; safecall;
          function ReadInteger  ( Name : widestring ) : olevariant; safecall;
          function ReadFloat    ( Name : widestring ) : olevariant; safecall;
          function ReadString   ( Name : widestring ) : olevariant; safecall;
          function ReadDate     ( Name : widestring ) : olevariant; safecall;
          function ReadDateAsStr( Name : widestring ) : olevariant; safecall;
          function ReadCurrency ( Name : widestring ) : olevariant; safecall;

          function FullPathValueExists( FullPathName : widestring ) : olevariant; safecall;
          function ValueExists        ( Name         : widestring ) : olevariant; safecall;

          function DeleteFullPathNode( FullPathNode  : widestring ) : olevariant; safecall;
          function DeleteNode        ( NodeName      : widestring ) : olevariant; safecall;

          function FullQuery( aQuery : widestring; Kind : integer ) : olevariant; safecall;
          function Query    ( aQuery : widestring; Kind : integer ) : olevariant; safecall;

          function IsSecureKey     ( FullKeyName : widestring                      ) : olevariant; safecall;
          function SetSecurityOfKey( FullKeyName : widestring; Security : wordbool ) : olevariant; safecall;

          function TypeOf( FullPathNode : widestring ) : olevariant; safecall;

          function IntegrateValues( RelValuePath : widestring ) : olevariant; safecall;

          function QueryKey ( FullKeyName,   ValueNameList : widestring ) : oleVariant; safecall;
          function SearchKey( SearchPattern, ValueNameList : widestring ) : oleVariant; safecall;
          function EditKey  ( FullPathKey, newName : widestring; Security : integer ) : olevariant; safecall;
        private
          function pFullPathKeyExists( FullPathKey : string ) : boolean;

          function GetFullPath(    Name : string ) : string;
          function GetParentID( KeyName : string ) : integer;

          function WriteValue( aName, Value : widestring; aType : integer ) : boolean;
          function ReadValue ( aName : widestring ) : string;

          function WriteBigString( Name, Value : widestring ) : boolean;
          function ReadBigString( Name : widestring ) : string;
          function IsBigString( Name : widestring ) : boolean;

          function pIsSecureKey( FullKeyName : string ) : boolean;
          function IsOrHaveSecureKey( FullKeyName : string ) : boolean;
        private
          fTableName    : string;
          fCurrentKey   : widestring;
          fDirSecurity  : wordbool;
          fQuerySession : TQuery;
      end;

  var
    fDataBase : TDataBase;

implementation

  uses
    Classes, SysUtils, ComObj, DB, Logs;

  const
    MaxStringLength = 32760;
    DriverName      = 'MSSQL1';


////////////////////////////////////////////////////////////////////////////////
// TDirectoryManager

// Generals procedures

  procedure Log( Msg : string );
    begin
      Logs.Log('queries', DateTimeToStr(Now) + ' ' + Msg);
    end;{ TDirectoryManager.LogThis }

  function TDirectoryManager.GetCurrentKey : olevariant;
    begin
      result := fCurrentKey;
    end;{ TDirectoryManager.GetCurrentKey }

  function TDirectoryManager.GetFullPath( Name : string ) : string;
    begin
      if GetCurrentKey = ''
        then result := Name
        else result := GetCurrentKey + '/' + Name;
    end; { TDirectoryManager.GetFullPath }

  function TDirectoryManager.Query( aQuery : widestring; Kind : integer ) : olevariant;
    begin
      result := FullQuery( GetFullPath( aQuery ), Kind );
    end;{ TDirectoryManager.Query }

  function TDirectoryManager.KeyExists( KeyName : widestring ) : olevariant;
    begin
      result := FullPathKeyExists( GetFullPath( KeyName ));
    end;{ TDirectoryManager.KeyExists }

  function TDirectoryManager.ValueExists( Name : widestring ) : olevariant;
    begin
      result := FullPathValueExists( GetFullPath( Name ));
    end;{ TDirectoryManager.ValueExists }

  function TDirectoryManager.SetCurrentKey( FullPathKey : widestring ) : olevariant;
    begin
      if FullPathKey = ''
        then result := true
        else
          begin
            try
              result := pFullPathKeyExists( FullPathKey ) and ((not fDirSecurity) or (not pIsSecureKey( FullPathKey )));
            except
              on e : EOleException do
                begin
                  Log( 'ERROR: ' + e.Message + ' @ SetCurrentKey(' + FullPathKey + ')' );
                  result := false;
                end;
            end;
          end;
      if result
        then fCurrentKey := FullPathKey;
    end;{ TDirectoryManager.SetCurrentKey }

  function TDirectoryManager.DeleteNode( NodeName : widestring ) : olevariant;
    begin
      result := DeleteFullPathNode( ( NodeName ));
    end;{ TDirectoryManager.DeleteNode }

  function TDirectoryManager.CreateKey( KeyName : widestring ) : olevariant;
    begin
      result := CreateFullPathKey( GetFullPath( KeyName ), true );
    end;{ TDirectoryManager.SetCurrentKey }

  function TDirectoryManager.WriteBoolean( Name : widestring; Value : wordbool ) : olevariant;
    begin
      if Value
        then result := WriteValue( Name, 'true' , ntBoolean )
        else result := WriteValue( Name, 'false', ntBoolean );
    end;{ TDirectoryManager.WriteBoolean }

  function TDirectoryManager.WriteInteger( Name : widestring; Value : Integer ) : olevariant;
    begin
      result := WriteValue( Name, IntToStr( Value ), ntInteger );
    end;{ TDirectoryManager.WriteInteger }

  function TDirectoryManager.WriteFloat( Name : widestring; Value : Double ) : olevariant;
    begin
      result := WriteValue( Name, FloatToStr( Value ), ntFloat );
    end;{ TDirectoryManager.WriteFloat }

  function TDirectoryManager.WriteString( Name, Value : widestring ) : olevariant;
    begin
      if( Length( Value ) > 250 )
        then result := WriteBigString( Name, Value )
        else result := WriteValue( Name, Value, ntString );
    end;{ TDirectoryManager.WriteString }

  function TDirectoryManager.WriteDate( Name : widestring; Value : TDateTime ) : olevariant;
    begin
      result := WriteValue( Name, FloatToStr( Value ), ntDate );
    end;{ TDirectoryManager.WriteDate }

  function TDirectoryManager.WriteDateFromStr( Name, Value : widestring ) : olevariant;
    begin
      result := WriteDate( Name, StrToDate( Value ));
    end;{ TDirectoryManager.WriteDateFromStr }

  function TDirectoryManager.WriteCurrency( Name : widestring; Value : Currency ) : olevariant;
    begin
      result := WriteValue( Name, FloatToStr( Value ), ntCurrency );
    end;{ TDirectoryManager.WriteCurrency }

  function TDirectoryManager.ReadBoolean( Name : widestring ) : olevariant;
    var
      aux : string;
    begin
      aux := ReadValue( Name );
      result := (aux = 'true') or (aux = '1');
    end;{ TDirectoryManager.ReadBoolean }

  function TDirectoryManager.ReadInteger( Name : widestring ) : olevariant;
    begin
      try
        result := StrToInt( ReadValue( Name ));
      except
        result := 0;
      end;
    end;{ TDirectoryManager.ReadInteger }

  function TDirectoryManager.ReadFloat( Name : widestring ) : olevariant;
    begin
      try
        result := StrToFloat( ReadValue( Name ));
      except
        result := 0.0;
      end;
    end;{ TDirectoryManager.ReadFloat }

  function TDirectoryManager.ReadString( Name : widestring ) : olevariant;
    begin
      if IsBigString( Name )
        then result := ReadBigString( Name )
        else result := ReadValue( Name );
    end;{ TDirectoryManager.ReadString }

  function TDirectoryManager.ReadDate( Name : widestring ) : olevariant;
    begin
      try
        result := StrToFloat( ReadValue( Name ));
      except
        result := 0;
      end;
    end;{ TDirectoryManager.ReadDate }

  function TDirectoryManager.ReadDateAsStr( Name : widestring ) : olevariant;
    begin
      try
        result := DateToStr( ReadDate( Name ));
      except
        result := '';
      end;
    end;{ TDirectoryManager.ReadDateAsStr }

  function TDirectoryManager.ReadCurrency( Name : widestring ) : olevariant;
    begin
      try
        result := StrToFloat( ReadValue( Name ));
      except
        result := 0.0;
      end;
    end;{ TDirectoryManager.ReadCurrency }

  // Specifics procedures

  constructor TDirectoryManager.Create( aTableName : widestring; SetSecurity : wordbool );
    begin
      try
        inherited Create;
        fCurrentKey   := '';
        fTableName    := aTableName;
        fDirSecurity  := SetSecurity;

        fQuerySession := TQuery.Create(nil);
        fQuerySession.DataBaseName := DriverName;
      except
        on e : Exception do
          begin
            Log('ERROR: Create ' + ' ' + e.Message);
            raise;
          end;
      end;
    end;{ TDirectoryManager.Create }

  destructor TDirectoryManager.Destroy;
    begin
      try
        if fQuerySession.Active then fQuerySession.Close;
        fQuerySession.Destroy;
        inherited;
      except
        on e : Exception do
          begin
            Log('ERROR: Destroy ' + ' ' + e.Message);
            raise;
          end;
      end;
    end;{ TDirectoryManager.Destroy }

  function TDirectoryManager.FullPathKeyExists( FullPathKey : widestring ) : olevariant;
    begin
      try
        if pFullPathKeyExists( FullPathKey )
          then
            if fDirSecurity
              then result := not pIsSecureKey( FullPathKey )
              else result := true
          else result := false;
      except
        on e : Exception do
          begin
            result := false;
            Log('ERROR: FullPathKeyExists ' + e.Message + ' Key: ' + FullPathKey );
          end;
      end;
    end;{ TDirectoryManager.FullPathKeyExists }

  function TDirectoryManager.FullPathValueExists( FullPathName : widestring ) : olevariant;
    var
      lsPos : integer;
    begin
      try
        try
          if fQuerySession.Active then fQuerySession.Close;
          fQuerySession.SQL.Clear;
          fQuerySession.SQL.Add('SELECT Entry');
          fQuerySession.SQL.Add('FROM ' + fTableName );
          fQuerySession.SQL.Add('WHERE  Entry LIKE "' + FullPathName + '"' + ' and Kind <> ' + IntToStr(ntKey) );
          fQuerySession.Open;

          if fQuerySession.FindFirst
            then
              begin
                if not fDirSecurity
                  then
                    begin
                      lsPos := LastDelimiter( '/', FullPathName );
                      if lsPos > 0
                         then result := not pIsSecureKey( Copy( FullPathName, 0, lsPos - 1))
                         else result := false;
                    end
                  else result := false;
              end
            else result := false;
        finally
          fQuerySession.Close;
        end;
      except
        on e : Exception do
          begin
            result := false;
            Log('ERROR: FullPathValueExists ' + ' ' + e.Message );
          end;
      end;
    end;{ TDirectoryManager.FullPathValueExists }


  function TDirectoryManager.CreateFullPathKey( FullPathKey : widestring; ForcePath : wordbool ) : olevariant;

    function recCreateFullPathKey( keyPath, keyName : string ) : boolean;
    var
      lsPos       : integer;
      ParentID    : integer;
      newKeyPath  : string;
      newKeyName  : string;
    begin
      if ( keyPath <> '' ) and not pFullPathKeyExists( keyPath )
        then
          begin
            lsPos    := LastDelimiter('/', keyPath );
            if lsPos > 0
              then
                begin
                  newKeyPath := Copy( keyPath, 0, lsPos - 1 );
                  newKeyName := Copy( keyPath, lsPos + 1, Length( keyPath));
                end
              else
                begin
                  newKeyPath := '';
                  newKeyName := keyPath;
                end;
            result := recCreateFullPathKey( newKeyPath, newKeyName );
          end
        else result := true;
      if result
        then
          begin
            if KeyPath = ''
              then
                begin
                  ParentID   := 0;
                  newKeyName := KeyName;
                end
              else
                begin
                  ParentID   := GetParentID(keyPath);
                  newKeyName := keyPath + '/' + KeyName;
                end;

            try
              try
                if fQuerySession.Active then fQuerySession.Close;
                fQuerySession.SQL.Clear;
                fQuerySession.SQL.Add('INSERT INTO ' + fTableName + '(Entry, ParentID, Kind)' );
                fQuerySession.SQL.Add('VALUES (' + '"' + newKeyName + '"' + ',' + IntToStr(ParentID) + ',' + IntToStr(ntKey) + ')' );
                fQuerySession.ExecSQL;
              finally
                fQuerySession.Close;
              end;
            except
              on e : Exception do
                begin
                  if ForcePath
                    then Log('ERROR: recCreateFullPathKey (true ) ' + e.Message )
                    else Log('ERROR: recCreateFullPathKey (false) ' + e.Message );
                  result := false;
                end;
            end;
          end;
    end; { recCreateFullPathKey }

    var
      keyPath : string;
      keyName : string;
      lsPos   : integer;
  begin
    try
      if pFullPathKeyExists( FullPathKey )
        then
          if fDirSecurity
            then result := not pIsSecureKey( FullPathKey )
            else result := true
        else
          begin
            lsPos := LastDelimiter('/', FullPathKey );
            if lsPos > 0
              then
                begin
                  KeyPath := Copy( FullPathKey, 0, lsPos - 1 );
                  KeyName := Copy( FullPathKey, lsPos + 1, Length( FullPathKey ));
                end
              else
                begin
                  KeyPath := '';
                  KeyName := FullPathKey;
                end;
            if ForcePath or (KeyPath = '') or FullPathKeyExists( keyPath )
              then result := recCreateFullPathKey( keyPath, keyName )
              else result := false;
          end;
    except
      on e : Exception do
        begin
          if ForcePath
            then Log('ERROR: CreateFullPathKey (true) ' + e.Message )
            else Log('ERROR: CreateFullPathKey (false) ' + e.Message );
          result := false;
        end;
   end;
  end;{ TDirectoryManager.CreateFullPathKey }


  function TDirectoryManager.KeysCount : olevariant;
    var
      pID     : integer;
      currKey : string;
    begin
      try
        currKey  := GetCurrentKey;
        pID      := GetParentID(currKey);
        try
          if fQuerySession.Active then fQuerySession.Close;
          fQuerySession.SQL.Clear;
          fQuerySession.SQL.Add('SELECT count(*) Counter');
          fQuerySession.SQL.Add('FROM ' + fTableName );
          fQuerySession.SQL.Add('WHERE ParentID = ' + IntToStr(pID) + ' and Kind = ' + IntToStr(ntKey) );
          fQuerySession.Open;

          if fQuerySession.FindFirst
            then result := fQuerySession.FieldByName('Counter').AsInteger
            else result := 0;
        finally
          fQuerySession.Close;
        end;
      except
        on e : Exception do
          begin
            Log('ERROR: KeysCount ' + e.message );
            result := -1;
          end;
      end;
    end;{ TDirectoryManager.KeysCount }


  function TDirectoryManager.ValuesCount : olevariant;
    var
      pID     : integer;
      currKey : string;
    begin
      try
        currKey  := GetCurrentKey;
        pID      := GetParentID(currKey);
        try
          if fQuerySession.Active then fQuerySession.Close;
          fQuerySession.SQL.Clear;
          fQuerySession.SQL.Add('SELECT count(*) Counter');
          fQuerySession.SQL.Add('FROM ' + fTableName );
          fQuerySession.SQL.Add('WHERE ParentID = ' + IntToStr(pID) + ' and Kind <> ' + IntToStr(ntKey) );
          fQuerySession.Open;

          if fQuerySession.FindFirst
            then result := fQuerySession.FieldByName('Counter').AsInteger
            else result := 0;
        finally
          fQuerySession.Close;
        end;
      except
        on e : Exception do
          begin
            result := -1;
            Log('ERROR: ValuesCount '  + e.Message );
          end;
      end;
    end;{ TDirectoryManager.ValuesCount }

  function TDirectoryManager.GetKeyNames : olevariant;
    var
      currKey  : string;
      KeyName  : string;
      keyNames : TStringList;
      pID      : integer;
    begin
      result   := '';
      keyNames := TStringList.Create;
      try
        currKey  := GetCurrentKey;
        try
          if currKey <> ''
            then
              begin
                pID := GetParentID(currKey);

                if fQuerySession.Active then fQuerySession.Close;
                fQuerySession.SQL.Clear;
                fQuerySession.SQL.Add('SELECT Entry');
                fQuerySession.SQL.Add('FROM ' + fTableName );
                fQuerySession.SQL.Add('WHERE ParentID = ' + IntToStr(pID) + ' and Kind = ' + IntToStr(ntKey) );
                fQuerySession.Open;

                if fQuerySession.FindFirst
                  then
                    begin
                      while not fQuerySession.EOF do
                        begin
                          KeyName := fQuerySession.FieldByName('Entry').AsString;
                          KeyName := Copy( KeyName, length(currKey) + 2, length(KeyName) );
                          KeyNames.Add(KeyName);
                          fQuerySession.Next;
                        end;
                      result := KeyNames.Text;
                    end;
              end
            else
              begin
                if fQuerySession.Active then fQuerySession.Close;
                fQuerySession.SQL.Clear;
                fQuerySession.SQL.Add('SELECT Entry');
                fQuerySession.SQL.Add('FROM ' + fTableName );
                fQuerySession.SQL.Add('WHERE ParentID = 0' );
                fQuerySession.Open;

                if fQuerySession.FindFirst
                  then
                    begin
                      while not fQuerySession.EOF do
                        begin
                          KeyName := fQuerySession.FieldByName('Entry').AsString;
                          KeyNames.Add(KeyName);
                          fQuerySession.Next;
                        end;
                      result := KeyNames.Text;
                    end;
              end;
        finally
          KeyNames.free;
          fQuerySession.Close;
        end;
      except
        on e : Exception do
          begin
            result := '';
            Log('ERROR: GetKeyNames ' + e.message );
          end;
      end;
    end;{ TDirectoryManager.GetKeyNames }

  function TDirectoryManager.GetValueNames : olevariant;
    var
      currKey    : string;
      valueName  : string;
      valueNames : TStringList;
      pID        : integer;
    begin
      result     := '';
      valueNames := TStringList.Create;
      try
        currKey  := GetCurrentKey;
        pID      := GetParentID(currKey);
        try
          if fQuerySession.Active then fQuerySession.Close;
          fQuerySession.SQL.Clear;
          fQuerySession.SQL.Add('SELECT Entry');
          fQuerySession.SQL.Add('FROM ' + fTableName );
          fQuerySession.SQL.Add('WHERE ParentID = ' + IntToStr(pID) + ' and Kind <> ' + IntToStr(ntKey) );
          fQuerySession.Open;

          if fQuerySession.FindFirst
            then
              begin
                while not fQuerySession.EOF do
                  begin
                    valueName := fQuerySession.FieldByName('Entry').AsString;
                    valueName := Copy( valueName, length(currKey) + 2, length(valueName) );
                    valueNames.Add(valueName);
                    fQuerySession.Next;
                  end;
                result := valueNames.Text;
              end;
        finally
          fQuerySession.Close;
          valueNames.Free;
        end;
      except
        on e : Exception do
          begin
            result := '';
            Log('ERROR: GetValueNames ' + e.Message );
          end;
      end;
    end;{ TDirectoryManager.GetValueNames }

  function TDirectoryManager.DeleteFullPathNode( FullPathNode : widestring ) : olevariant;
    begin
      if fDirSecurity and FullPathKeyExists( FullPathNode ) and IsOrHaveSecureKey( FullPathNode )
        then result := false
        else
          begin
            try
              try
                if fQuerySession.Active then fQuerySession.Close;
                fQuerySession.SQL.Add('DELETE FROM ' + fTableName );
                fQuerySession.SQL.Add('WHERE  Entry LIKE ' + '"' + FullPathNode + '/%"'); //SubNodes
                fQuerySession.ExecSQL;
              finally
                fQuerySession.Close;
              end;

              try
                if fQuerySession.Active then fQuerySession.Close;
                fQuerySession.SQL.Clear;
                fQuerySession.SQL.Add('DELETE FROM ' + fTableName );
                fQuerySession.SQL.Add('WHERE  Entry LIKE ' + '"' + FullPathNode + '"'); //Node
                fQuerySession.ExecSQL;
                result := true;
              finally
                fQuerySession.Close;
              end;
            except
              on e : Exception do
                begin
                  Log('ERROR: DeleteFullPathNode ' + e.Message );
                  result := false;
                end;
            end;
          end;
    end;{ TDirectoryManager.DeleteFullPathNode }

  function TDirectoryManager.SetSecurityOfKey( FullKeyName : widestring; Security : wordbool ) : olevariant;
    begin
      if not fDirSecurity and FullPathKeyExists( FullKeyName )
        then
          begin
            try
              try
                if fQuerySession.Active then fQuerySession.Close;
                fQuerySession.SQL.Clear;
                fQuerySession.SQL.Add('UPDATE ' + fTableName);
                if Security
                  then fQuerySession.SQL.Add('SET Security = 1')
                  else fQuerySession.SQL.Add('SET Security = 0');
                fQuerySession.SQL.Add('WHERE  Entry LIKE ' + '"' + FullKeyName + '"');
                fQuerySession.ExecSQL;
              finally
                fQuerySession.Close;
              end;
            except
              on e : Exception do
                begin
                  Log('ERROR: SetSecurityOfKey ' + e.message );
                  result := false;
                end;
            end;
            result := true;
          end
        else result := false;
    end;{ TDirectoryManager.SetSecurityOfKey }

  function TDirectoryManager.IsSecureKey( FullKeyName : widestring ) : olevariant;
    begin
      if fDirSecurity
        then result := true
        else
          try
            result := pIsSecureKey( FullKeyName );
          except
            on e : Exception do
              begin
                result := true;
                Log('ERROR: IsSecureKey ' + e.message );
              end;
          end;
    end;{ TDirectoryManager.IsSecureKey }

  function TDirectoryManager.IsOrHaveSecureKey( FullKeyName : string ) : boolean;
    begin
      try
        result := true;
        try
          if fQuerySession.Active then fQuerySession.Close;
          fQuerySession.SQL.Clear;
          fQuerySession.SQL.Add('SELECT Security');
          fQuerySession.SQL.Add('FROM ' + fTableName );
          fQuerySession.SQL.Add('WHERE  Entry LIKE "' + FullKeyName + '"' + ' and Kind = ' + IntToStr(ntKey) );
          fQuerySession.Open;

          if fQuerySession.FindFirst
            then
              while result and not fQuerySession.Eof do
                begin
                  result := result and fQuerySession.FieldByName('Security').AsBoolean;
                  fQuerySession.Next;
                end
            else result := false;
        finally
          fQuerySession.Close;
        end;
      except
        on e : Exception do
          begin
            result := false;
            Log('ERROR: IsOrHaveSecureKey ' + e.message );
          end;
      end;
    end;{ TDirectoryManager.IsOrHaveSecureKey }

  function TDirectoryManager.WriteValue( aName, Value: widestring; aType : integer ) : boolean;
    var
      Find      : boolean;
      lsPos     : integer;
      pID       : integer;
      keyName   : string;
      valueName : string;
      sec       : boolean;
    begin
      try
        valueName := GetFullPath(aName);
        try
          if fQuerySession.Active then fQuerySession.Close;
          fQuerySession.SQL.Clear;
          fQuerySession.SQL.Add('SELECT Kind ');
          fQuerySession.SQL.Add('FROM ' + fTableName );
          fQuerySession.SQL.Add('WHERE  Entry LIKE "' + valueName + '"' + ' and Kind <> ' + IntToStr(ntKey) );
          fQuerySession.SQL.Add('Group By Kind');
          fQuerySession.Open;

          Find := fQuerySession.FindFirst;

          if Find and (fQuerySession.FieldByName('Kind').AsInteger = 7)
            then
              begin
                DeleteFullPathNode( valueName );
                Find := false;
              end;

        finally
          fQuerySession.Close;
        end;

        if Find
          then
            begin
              try
                if fQuerySession.Active then fQuerySession.Close;
                fQuerySession.SQL.Clear;
                fQuerySession.SQL.Add('UPDATE ' + fTableName);
                fQuerySession.SQL.Add('SET Value = ' + '"' + Value + '",' + 'Kind = ' + IntToStr(aType) );
                fQuerySession.SQL.Add('WHERE  Entry LIKE ' + '"' + valueName + '"');
                fQuerySession.ExecSQL;
                result := true;
              finally
                fQuerySession.Close;
              end;
            end
          else
            begin
              try
                lsPos     := LastDelimiter('/', valueName);
                keyName   := copy(valueName, 0, lsPos - 1);

                sec := pIsSecureKey(keyName);
                pID := GetParentID (keyName);

                if fQuerySession.Active then fQuerySession.Close;
                fQuerySession.SQL.Clear;
                fQuerySession.SQL.Add('INSERT INTO ' + fTableName + '(Entry, ParentID, Kind, Value, Security, ValueSize)'   );
                fQuerySession.SQL.Add('VALUES (' + '"' + valueName + '"' + ',' + IntToStr(pID) + ','                        );
                fQuerySession.SQL.Add( IntToStr(aType) + ',"' + Value + '",' + IntToStr( Ord(sec) ) + ',' + '0' + ')'       );
                fQuerySession.ExecSQL;
                result := true;
              finally
                fQuerySession.Close;
              end;
            end;
      except
        on e : Exception do
          begin
            result := false;
            Log('ERROR: WriteValue ' + e.message  );
          end;
      end;
    end;{ TDirectoryManager.WriteValue }


  function TDirectoryManager.ReadValue( aName : widestring ) : string;
    var
      Trap : integer;
    begin
      try
        try
          Trap := 1;
          if fQuerySession.Active
             then
               begin
                 Trap := 1;
                 fQuerySession.Close;
                 Trap := 2;
               end;
          Trap := 3;
          fQuerySession.SQL.Clear;
          Trap := 4;
          fQuerySession.SQL.Add('SELECT Value');
          Trap := 5;
          fQuerySession.SQL.Add('FROM ' + fTableName );
          Trap := 6;
          fQuerySession.SQL.Add('WHERE Entry LIKE "' + GetFullPath(aName) + '"' );
          Trap := 7;
          fQuerySession.Open;
          Trap := 8;

          if fQuerySession.FindFirst
            then
              begin
                Trap := 9;
                result := fQuerySession.FieldByName('Value').AsString;
                Trap := 10;
              end
            else result := '';
           Trap := 11;
        finally
          fQuerySession.Close;
        end;
      except
        on e : Exception do
          begin
            result := '';
            Log('ERROR: ReadValue ' + e.message );
          end;
      end;
    end;{ TDirectoryManager.ReadValue }

  function TDirectoryManager.TypeOf( FullPathNode : widestring ) : olevariant; safecall;
    begin
      try
        try
          if fQuerySession.Active then fQuerySession.Close;
          fQuerySession.SQL.Clear;
          fQuerySession.SQL.Add('SELECT Kind');
          fQuerySession.SQL.Add('FROM ' + fTableName );
          fQuerySession.SQL.Add('WHERE  Entry LIKE "' + FullPathNode + '"');
          fQuerySession.SQL.Add('GROUP BY Kind');
          fQuerySession.Open;

          if fQuerySession.FindFirst
            then result := fQuerySession.FieldByName('Kind').AsInteger
            else result := -1;
        finally
          fQuerySession.Close;
        end;
      except
        on e : Exception do
          begin
            result := false;
            Log('ERROR: TypeOf ' + e.message );
          end;
      end;
    end;{ TDirectoryManager.TypeOf }


  function TDirectoryManager.IntegrateValues( RelValuePath : widestring ) : olevariant;
    var
      str       : string;
      value     : string;
      ValueName : string;
      intSum    : integer;
      floatSum  : double;
      ntType    : integer;
    begin
      intSum   := 0;
      floatSum := 0;
      try
        try
          if fQuerySession.Active then fQuerySession.Close;
          fQuerySession.SQL.Clear;
          fQuerySession.SQL.Add('SELECT *');
          fQuerySession.SQL.Add('FROM ' + fTableName );
          fQuerySession.SQL.Add('WHERE  Entry LIKE ' + '"' + RelValuePath + '"' + '/%');
          fQuerySession.Open;

          if fQuerySession.FindFirst
            then
              while not fQuerySession.Eof do
                begin
                  ValueName := fQuerySession.FieldByName('Entry').AsString;
                  str       := Copy( ValueName, length(GetCurrentKey) + 2, length(ValueName) );
                  if (LastDelimiter( '/', str ) = 0)
                    then
                      begin
                        value := fQuerySession.FieldByName('Value').AsString;
                        if (fQuerySession.FieldByName('Kind').AsInteger = ntInteger )
                          then ntType := ntInteger
                          else ntType := ntFloat;
                        case ntType of
                          ntInteger   : intSum   := intSum   + StrToInt  (value);
                          ntFloat     : floatSum := floatSum + StrToFloat(value);
                          else result := 0;
                        end;
                      end;
                  fQuerySession.Next;
                end;
        finally
          fQuerySession.Close;
        end;
        result := floatSum + intSum;
      except
        on e : Exception do
          begin
            result := 0;
            Log('ERROR: IntegrateValues ' + e.message );
          end;
      end;
    end;{ TDirectoryManager.IntegrateValues }

  function TDirectoryManager.SearchKey( SearchPattern, ValueNameList : widestring ) : oleVariant; safecall;
    var
      resultList  : TStringList;
      valueNames  : TStringList;
      count       : integer;
      i, idx      : integer;
      subElement  : string;
      element     : string;
      str         : string;
      value       : string;
      FullKeyName : string;
      bool        : oleVariant;

      function GetKeyName( KeyName : string ) : string;
        var
          lsPos : integer;
        begin
          result := copy(KeyName, length(FullKeyName) + 2, length(KeyName) );
          lsPos  := System.Pos( '/', result );
          result := copy(result, 0, lsPos - 1 );
        end;{ GetKeyName }

      function FormatQuery( str : string ) : string;
        var
          i : integer;
        begin
          for i := 1 to length(str) do
            if str[i] = '*'
              then str[i] := '%';
          result := str;
        end;{ FormatQuery }

    begin
      try
        valueNames  := TStringList.Create;
        try
          valueNames.Text := string(ValueNameList);
          if valueNames.Count > 0
            then
              begin
                FullKeyName := GetCurrentKey;
                str := '''' + FullKeyName + '/' + FormatQuery(SearchPattern) + '''' + ' and (';
                for i := 0 to ValueNames.Count - 1 do
                  begin
                    str := str + '(';
                    str := str + 'Entry LIKE ' + '''' + '%/' + ValueNames[i] + '''' + ' and Kind <> ' + IntToStr(ntKey);
                    str := str + ')';
                    if i < (ValueNames.Count - 1)
                      then str := str + ' or '
                      else str := str + ')';
                  end;
                  try
                    if fQuerySession.Active then fQuerySession.Close;
                    fQuerySession.SQL.Clear;
                    fQuerySession.SQL.Add('SELECT *');
                    fQuerySession.SQL.Add('FROM ' + fTableName );
                    fQuerySession.SQL.Add('WHERE  Entry LIKE '  + str );
                    fQuerySession.Open;

                    if fQuerySession.FindFirst
                      then
                        begin
                          count := 1;
                          element := GetKeyName( fQuerySession.FieldByName('Entry').AsString );
                          while not fQuerySession.EOF do
                            begin
                              subElement  := GetKeyName( fQuerySession.FieldByName('Entry').AsString );
                              if subElement <> element
                                then
                                  begin
                                    element := subElement;
                                    inc(count);
                                  end;
                              fQuerySession.Next;
                            end;
                        end
                      else count := 0;

                    if count > 0
                      then
                        begin
                          resultList := TStringList.Create;
                          try
                            resultList.Values['Count'] := IntToStr( count );
                            idx     := 0;
                            fQuerySession.First;
                            while not fQuerySession.Eof do
                              begin
                                element := GetKeyName( fQuerySession.FieldByName('Entry').AsString );
                                resultList.Values['Key' + IntToStr(idx)] := element;
                                for i := 0 to ValueNames.Count - 1 do
                                  begin
                                    subElement := fQuerySession.FieldByName('Entry').AsString;
                                    while not fQuerySession.EOF and (fQuerySession.FieldByName('Entry').AsString <> subElement ) do
                                      fQuerySession.Next;
                                    if not fQuerySession.EOF
                                      then
                                        begin
                                          bool  := true;
                                          value := fQuerySession.FieldByName('Value').AsString
                                        end
                                      else bool := false;
                                    if bool
                                      then resultList.Values[valueNames[i] + IntTostr(idx)] := value;
                                    fQuerySession.Next;
                                  end;
                                inc(idx);
                              end;
                            result := resultList.Text;
                          finally
                            resultList.Free;
                          end;
                        end
                      else result := '';
                  finally
                    fQuerySession.Close;
                  end;
              end;
        finally
          valueNames.free;
        end;
      except
        on e : Exception do
          begin
            Log('ERROR: SearchKey ' + e.message );
            result := '';
          end;
      end;
    end;{ TDirectoryManager.SearchKey }


  function TDirectoryManager.QueryKey( FullKeyName, ValueNameList : widestring ) : olevariant;
    var
      resultList  : TStringList;
      valueNames  : TStringList;
      count       : integer;
      i, idx      : integer;
      Pos         : integer;
      subElement  : string;
      element     : string;
      str         : string;
      value       : string;
      bool        : oleVariant;
      Trap        : integer;

      function GetKeyName( KeyName : string ) : string;
        var
          lsPos : integer;
        begin
          result := copy(KeyName, length(FullKeyName) + 2, length(KeyName) );
          lsPos  := System.Pos( '/', result );
          result := copy(result, 0, lsPos - 1 );
        end;{ GetKeyName }

      function FindByName( EntryName : string ) : string;
        var
          lsPos : integer;
        begin
          result := copy(EntryName, length(FullKeyName) + 2, length(EntryName) );
          lsPos  := System.Pos( '/', result );
          result := copy(result, lsPos + 1, length(result) );
        end;{ FindByName }

      function GetPosInResultList( Entry : string ) : integer;
        var
          i    : integer;
          Find : boolean;
        begin
          i := 0;
          Find := false;
          while not Find and (i < valueNames.Count) do
            begin
              if system.Pos( valueNames[i], Entry ) > 0
                then Find := true
                else inc(i);
            end;
          if Find
            then result :=  i
            else result := -1;
        end;{ GetPosInResultList }

    begin
      try
        valueNames  := TStringList.Create;
        try
          valueNames.Text := string(ValueNameList);
          if valueNames.Count > 0
            then
              begin
                 str := '''' + FullKeyName + '/%' + '''' + ' and (';
                 for i := 0 to ValueNames.Count - 1 do
                   begin
                     str := str + '(';
                     str := str + 'Entry LIKE ' + '''' + '%/' + ValueNames[i] + '''' + ' and Kind <> ' + IntToStr(ntKey);
                     str := str + ')';
                     if i < (ValueNames.Count - 1)
                       then str := str + ' or '
                       else str := str + ')';
                   end;

                   try
                     Trap := 1;
                     if fQuerySession.Active
                       then
                         begin
                           Trap := 2;
                           fQuerySession.Close;
                           Trap := 3;
                         end;
                     fQuerySession.SQL.Clear;
                     Trap := 4;
                     fQuerySession.SQL.Add('SELECT *');
                     Trap := 5;
                     fQuerySession.SQL.Add('FROM ' + fTableName );
                     Trap := 6;
                     fQuerySession.SQL.Add('WHERE  Entry LIKE '  + str );
                     Trap := 7;
                     fQuerySession.Open;
                     Trap := 8;

                     if fQuerySession.FindFirst
                       then
                         begin
                           count := 1;
                           Trap := 8;
                           element := GetKeyName( fQuerySession.FieldByName('Entry').AsString );
                           Trap := 9;
                           while not fQuerySession.EOF do
                             begin
                               Trap := 9;
                               subElement  := GetKeyName( fQuerySession.FieldByName('Entry').AsString );
                               Trap := 10;
                               if subElement <> element
                                 then
                                   begin
                                     element := subElement;
                                     inc(count);
                                   end;
                               Trap := 8;
                               fQuerySession.Next;
                               Trap := 9;
                             end;
                         end
                       else count := 0;

                     resultList := TStringList.Create;
                     resultList.Values['Count'] := IntToStr( count );

                     if count > 0
                       then
                         begin
                            try
                              idx  := 0;
                              Trap := 10;
                              fQuerySession.First;
                              Trap := 11;
                              while not fQuerySession.Eof do
                                begin
                                  Trap := 12;
                                  element := GetKeyName( fQuerySession.FieldByName('Entry').AsString );
                                  Trap := 13;
                                  resultList.Values['Key' + IntToStr(idx)] := element;
                                  for i := 0 to ValueNames.Count - 1 do
                                    begin
                                      Trap := 14;
                                      subElement := fQuerySession.FieldByName('Entry').AsString;
                                      Trap := 15;
                                      while not fQuerySession.EOF and (fQuerySession.FieldByName('Entry').AsString <> subElement ) do
                                        fQuerySession.Next;
                                      Trap := 16;
                                      if not fQuerySession.EOF
                                        then
                                          begin
                                            Trap := 17;
                                            bool  := true;
                                            value := fQuerySession.FieldByName('Value').AsString;
                                            Trap := 18;
                                          end
                                        else bool := false;
                                      if bool
                                        then
                                          begin
                                            Pos := GetPosInResultList(subElement);
                                            if Pos >= 0
                                              then resultList.Values[valueNames[Pos] + IntTostr(idx)] := value;
                                          end;
                                      Trap := 18;
                                      fQuerySession.Next;
                                    end;
                                  inc(idx);
                                end;
                              result := resultList.Text;
                            finally
                              resultList.Free;
                            end;
                         end;
                   finally
                     fQuerySession.Close;
                   end;
              end;
        finally
          valueNames.free;
        end;
      except
        on e : Exception do
          begin
            result := '';
            Log('ERROR: QueryKey ' + IntToStr(Trap) + ' ' + e.message );
          end;
      end;
    end;{ TDirectoryManager.QueryKey }

  function TDirectoryManager.EditKey( FullPathKey, newName : widestring; Security : integer ) : olevariant;
    var
      EntryKey     : string;
      curKeyName   : string;
      lsPos        : integer;
    begin
      lsPos := LastDelimiter('/', FullPathKey );
      if lsPos > 0
        then curKeyName := copy( FullPathKey, lsPos + 1, length(FullPathKey) )
        else curKeyName := FullPathKey;
      try
        try
          if fQuerySession.Active then fQuerySession.Close;
          fQuerySession.RequestLive := true;
          fQuerySession.SQL.Clear;
          fQuerySession.SQL.Add('SELECT Entry, Security');
          fQuerySession.SQL.Add('FROM ' + fTableName );
          fQuerySession.SQL.Add('WHERE  Entry LIKE '  + '"'  + string(FullPathKey) + '/%' + '"' );
          fQuerySession.Open;
          fQuerySession.RequestLive := false;

          if fQuerySession.FindFirst
            then
              while not fQuerySession.Eof do
                begin
                  EntryKey  := fQuerySession.FieldByName('Entry').AsString;
                  if length(EntryKey) > length(curKeyName)
                    then lsPos  := Pos( curKeyName + '/', EntryKey )
                    else lsPos  := 0;

                  System.Delete(EntryKey, lsPos, length(curKeyName) );
                  System.Insert(NewName, EntryKey, lsPos );

                  fQuerySession.Edit;
                  fQuerySession.FieldByName('Entry').AsString     := EntryKey;
                  fQuerySession.FieldByName('Security').AsBoolean := (Security <> 0);
                  fQuerySession.Post;

                  fQuerySession.Next;
                end;
        finally
          fQuerySession.Close;
        end;

        try
          fQuerySession.RequestLive := true;
          if fQuerySession.Active then fQuerySession.Close;
          fQuerySession.SQL.Clear;
          fQuerySession.SQL.Add('SELECT Entry, Security');
          fQuerySession.SQL.Add('FROM ' + fTableName );
          fQuerySession.SQL.Add('WHERE  Entry LIKE '  + '"'  + string(FullPathKey) + '"' );
          fQuerySession.Open;
          fQuerySession.RequestLive := false;

          if fQuerySession.FindFirst
            then
              begin
                fQuerySession.Edit;
                EntryKey := fQuerySession.FieldByName('Entry').AsString;
                lsPos    := Pos( curKeyName, EntryKey );

                System.Delete(EntryKey, lsPos, length(curKeyName) );
                System.Insert(NewName, EntryKey, lsPos );

                fQuerySession.Edit;
                fQuerySession.FieldByName('Entry').AsString     := EntryKey;
                fQuerySession.FieldByName('Security').AsBoolean := ( Security <> 0);
                fQuerySession.Post;

                result := true;
              end
            else result := false;  
        finally
          fQuerySession.Close;
        end;
      except
        on e : Exception do
          begin
            result := false;
            Log('ERROR: EditKey ' + e.message );
          end;
      end;
    end;{ TDirectoryManager..FullQuery }

  function TDirectoryManager.FullQuery( aQuery : widestring; Kind : integer ) : olevariant;
    var
      elementNames : TStringList;
      Aux1, Aux2   : string;
      value        : string;
      lsPos        : integer;
      pID          : integer;

    function ClearQuery : string;
      const
        CharQuerySet = [ '%', '_' ];
      var
        s : string;
        i : integer;
      begin
        result := '';
        s := string(aQuery);
        for i := 1 to length(s) do
          if not (s[i] in CharQuerySet)
            then result := result + s[i];
        if result[length(result)] = '/'
          then delete(result, length(result), 1);
      end;

    begin
      Aux1 := string(aQuery);
      if Kind >= 0
        then Aux2 := ' and Kind = ' + IntToStr(Kind)
        else Aux2 := '';

      pID          := GetParentID(Aux1);
      elementNames := TStringList.Create;
      try
        try
          try
            if fQuerySession.Active then fQuerySession.Close;
            fQuerySession.SQL.Clear;
            fQuerySession.SQL.Add('SELECT Entry, Value, Kind');
            fQuerySession.SQL.Add('FROM ' + fTableName );
            fQuerySession.SQL.Add('WHERE  Entry LIKE '  + '"' + Aux1 + '"' + Aux2 + ' and ParentID = ' + IntToStr(pID) );
            fQuerySession.SQL.Add('Group By Entry');
            fQuerySession.Open;

            Aux2 := ClearQuery;
            if fQuerySession.FindFirst
              then
                while not fQuerySession.Eof do
                  begin
                    Aux1  := fQuerySession.FieldByName('Entry').AsString;
                    Aux1  := Copy( Aux1, length(Aux2) + 2, length(Aux1) );
                    lsPos := System.Pos( '/', Aux1 );
                    if lsPos = 0
                      then
                        begin
                          if fQuerySession.FieldByName('Kind').AsInteger = 0
                            then elementNames.Add(Aux1)
                            else
                              begin
                                value := fQuerySession.FieldByName('Value').AsString;
                                elementNames.Values['ValueName'] :=  Aux1;
                                elementNames.Values['value']     := Value;
                              end;
                        end;
                    fQuerySession.Next;
                  end;
            result := elementNames.Text;
          finally
            fQuerySession.Close;
          end;
        finally
          elementNames.Free;
        end;
      except
        on e : Exception do
          begin
            result := '';
            Log('ERROR: FullQuery ' + e.message );
          end;
      end;
    end;{ TDirectoryManager.FullQuery }

  function TDirectoryManager.pFullPathKeyExists( FullPathKey : string ) : boolean;
    var
      Trap : integer;
    begin
      try
        try
          Trap := 1;
          if fQuerySession.Active
            then
              begin
                Trap := 2;
                fQuerySession.Close;
                Trap := 3;
              end;
          Trap := 4;
          fQuerySession.SQL.Clear;
          Trap := 5;
          fQuerySession.SQL.Add('SELECT Entry');
          Trap := 6;
          fQuerySession.SQL.Add('FROM ' + fTableName );
          Trap := 7;
          fQuerySession.SQL.Add('WHERE  Entry LIKE "' + FullPathKey + '"' + ' and Kind = ' + IntToStr(ntKey) );
          Trap := 8;
          fQuerySession.Open;
          Trap := 9;

          result := fQuerySession.FindFirst;
          Trap := 10;
        finally
          fQuerySession.Close;
        end;
      except
        on e : Exception do
          begin
            result := false;
            Log('ERROR: pFullPathKeyExists ' + IntToStr(Trap) + '  ' +  e.message);
          end;
      end
    end;{ TDirectoryManager.pFullPathKeyExists }

  function TDirectoryManager.pIsSecureKey( FullKeyName : string ) : boolean;
    begin
      try
        try
          if fQuerySession.Active then fQuerySession.Close;
          fQuerySession.SQL.Clear;
          fQuerySession.SQL.Add('SELECT Entry');
          fQuerySession.SQL.Add('FROM ' + fTableName );
          fQuerySession.SQL.Add('WHERE  Entry LIKE "' + FullKeyName + '"' + ' and Security <> 0');
          fQuerySession.Open;

          result := fQuerySession.FindFirst
        finally
          fQuerySession.Close;
        end;
      except
        on e : Exception do
          begin
            result := false;
            Log('ERROR: pIsSecureKey ' + e.message );
          end;
      end;
    end;{ TDirectoryManager.IsSecureKey }

  function TDirectoryManager.GetParentID( keyName : string ) : integer;
    begin
      result := 0;
      try
        if KeyName <> ''
          then
            try
              if fQuerySession.Active then fQuerySession.Close;
              fQuerySession.SQL.Clear;
              fQuerySession.SQL.Add('SELECT ID');
              fQuerySession.SQL.Add('FROM ' + fTableName );
              fQuerySession.SQL.Add('WHERE  Entry LIKE ' + '"' + keyName + '"');
              fQuerySession.Open;

              if fQuerySession.FindFirst
                then result := fQuerySession.FieldByName('ID').AsInteger
                else result := -1;
            finally
              fQuerySession.Close;
            end;
      except
        on e : Exception do
          begin
            result := -1;
            Log('ERROR: GetParentID ' + e.message );
          end;
      end;
    end;{ TDirectoryManager.GetParentID }

  function TDirectoryManager.IsBigString( Name : widestring ) : boolean;
    var
      trap : integer;
    begin
      try
        try
          trap := 1;
          if fQuerySession.Active then fQuerySession.Close;
          trap := 2;
          fQuerySession.SQL.Clear;
          trap := 3;
          fQuerySession.SQL.Add('SELECT Kind');
          trap := 4;
          fQuerySession.SQL.Add('FROM ' + fTableName );
          trap := 5;
          fQuerySession.SQL.Add('WHERE Entry LIKE ' + '"' + GetFullPath(Name) + '"');
          trap := 6;
          fQuerySession.Open;
          trap := 7;
          if fQuerySession.FindFirst
            then
              begin
                trap := 8;
                result := (fQuerySession.FieldByName('Kind').AsInteger = ntBigString);
                trap := 9;
              end
            else result := false;
          trap := 10;
        finally
          fQuerySession.Close;
        end;
      except
        on e : exception do
          begin
            Log('IsBigString --> ERROR: ( ' + IntToStr(Trap) + '  ' + e.Message + ' key: ' + GetFullPath(Name) );
            result := false;
          end;
      end;
    end;{ TDirectoryManager.IsBigString }

  function TDirectoryManager.ReadBigString( Name : widestring ) : string;
    var
      FullPathName : string;
      ValueName    : array[0..20] of string;
      ValueSize    : array[0..20] of integer;
      Count, i     : integer;
    begin
      try
        try
          FullPathName := GetFullPath(Name);

          if fQuerySession.Active then fQuerySession.Close;
          fQuerySession.SQL.Clear;
          fQuerySession.SQL.Add('SELECT Entry, ValueSize');
          fQuerySession.SQL.Add('FROM ' + fTableName );
          fQuerySession.SQL.Add('WHERE  Entry LIKE '  + '"' + FullPathName + '%"' );
          fQuerySession.Open;

          Count := 0;
          if fQuerySession.FindFirst
            then
              while not fQuerySession.EOF do
                begin
                  ValueName[Count] := fQuerySession.FieldByName('Entry').AsString;
                  ValueSize[Count] := fQuerySession.FieldByName('ValueSize').AsInteger;
                  inc(Count);
                  fQuerySession.Next;
                end;
        finally
          fQuerySession.Close;
        end;

        try
          for i := 0 to Count - 1do
            begin
              if fQuerySession.Active then fQuerySession.Close;
              fQuerySession.SQL.Clear;
              fQuerySession.SQL.Add('sp_dboption FiveDirManager, "select into/bulkcopy", true');
              fQuerySession.SQL.Add('DECLARE @@ptrval varbinary(16)');
              fQuerySession.SQL.Add('SET TEXTSIZE ' + IntToStr(ValueSize[i]) );
              fQuerySession.SQL.Add('SELECT @@ptrval = TEXTPTR(Value)');
              fQuerySession.SQL.Add('FROM ' + fTableName);
              fQuerySession.SQL.Add('WHERE Entry Like ' + '"' + ValueName[i] + '"' );
              fQuerySession.SQL.Add('READTEXT ' + fTableName + '.Value @@ptrval 0 ' + IntToStr(ValueSize[i]) );
              fQuerySession.Open;

              result := result + fQuerySession.FieldByName('Value').AsString;
            end;
        finally
          fQuerySession.Close;
        end;

        try
          if fQuerySession.Active then fQuerySession.Close;
          fQuerySession.SQL.Clear;
          fQuerySession.SQL.Add('sp_dboption FiveDirManager, "select into/bulkcopy", false');
          fQuerySession.ExecSQL;
        finally
          fQuerySession.Close;
        end;
      except
        on e : Exception do
          begin
            Log('ERROR: ReadBigString ' + e.message );
            result := '';
          end;
      end;
    end;{ TDirectoryManager.ReadBigString }

  function TDirectoryManager.WriteBigString( Name, Value : widestring ) : boolean;
    const
      MaxTextSize = 32700;
    var
      Find        : boolean;
      KeyName     : string;
      theName     : string;
      saveValue   : string;
      ValueSize   : integer;
      lsPos, i    : integer;
      pID         : integer;
      sec         : boolean;
    begin
      theName := GetFullPath(Name);
      try

        try
          if fQuerySession.Active then fQuerySession.Close;
          fQuerySession.SQL.Clear;
          fQuerySession.SQL.Add('SELECT Entry');
          fQuerySession.SQL.Add('FROM ' + fTableName );
          fQuerySession.SQL.Add('WHERE  Entry LIKE "' + theName + '"' + ' and Kind <> ' + IntToStr(ntKey) );
          fQuerySession.Open;

          Find := fQuerySession.FindFirst;
        finally
          fQuerySession.Close;
        end;

        if Find then DeleteFullPathNode( theName );

        lsPos   := LastDelimiter('/', theName);
        keyName := copy(theName, 0, lsPos - 1);
        sec     := pIsSecureKey(keyName);
        pID     := GetParentID (keyName);

        for i := 0 to Length( Value ) div MaxTextSize do
          begin
            saveValue := Copy( Value, i*MaxTextSize + 1, MaxTextSize );
            ValueSize := length(saveValue);

            try
              if fQuerySession.Active then fQuerySession.Close;
              fQuerySession.SQL.Clear;
              fQuerySession.SQL.Add('INSERT INTO ' + fTableName + '(Entry, ParentID, Kind, Value, Security, ValueSize)' );
              fQuerySession.SQL.Add('VALUES (' + '"' + theName + '"' + ',' + IntToStr(pID) + ','                        );
              fQuerySession.SQL.Add( '7' + ',"' + ' ' + '",' + IntToStr( Ord(sec) ) + ',' + IntToStr(valueSize) + ')'   );
              fQuerySession.ExecSQL;
            finally
              fQuerySession.Close;
            end;

            try
              if fQuerySession.Active then fQuerySession.Close;
              fQuerySession.SQL.Clear;
              fQuerySession.SQL.Add('sp_dboption FiveDirManager, "select into/bulkcopy", true');
              fQuerySession.SQL.Add('DECLARE @@ptrval varbinary(16)');
              fQuerySession.SQL.Add('SET TEXTSIZE ' + IntToStr(ValueSize) );
              fQuerySession.SQL.Add('SELECT @@ptrval = TEXTPTR(Value)');
              fQuerySession.SQL.Add('FROM ' + fTableName);
              fQuerySession.SQL.Add('WHERE Entry Like ' + '"' + theName + '"' );
              fQuerySession.SQL.Add('WRITETEXT ' + fTableName + '.Value @@ptrval ' + '"' + saveValue + '"');
              fQuerySession.ExecSQL;
            finally
              fQuerySession.Close;
            end;

            try
              if fQuerySession.Active then fQuerySession.Close;
              fQuerySession.SQL.Clear;
              fQuerySession.SQL.Add('sp_dboption FiveDirManager, "select into/bulkcopy", false');
              fQuerySession.ExecSQL;
            finally
              fQuerySession.Close;
            end;

            theName := theName + '/V' + IntToStr(i);
          end;
        result := true;
      except
        on e : Exception do
          begin
            result := false;
            Log('ERROR: WriteString ' + e.message );
          end;
      end;
    end;{ TDirectoryManager.WriteBigString }

initialization

  Log( 'Restarting ........................ ' + DateTimeToStr(Now) );

  fDataBase := TDataBase.Create(nil);
  fDataBase.DataBaseName := DriverName;
  fDataBase.LoginPrompt  := False;
  fDataBase.Params.Values['USER NAME'] := 'sa';
  fDataBase.Params.Values['PASSWORD']  := 'elmanicero';


finalization

  Log( 'Closing ........................ ' + DateTimeToStr(Now) );

  fDataBase.Close;
  fDataBase.Free;

end.
