unit DirectoryManager;

interface

  uses ADOInt;

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
          constructor Create( aDBName : widestring; SetSecurity : wordbool );
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

          function WriteBoolean    ( Name : widestring; Value : wordbool ) : olevariant;   safecall;
          function WriteInteger    ( Name : widestring; Value : integer  ) : olevariant;   safecall;
          function WriteFloat      ( Name : widestring; Value : double   ) : olevariant;   safecall;
          function WriteString     ( Name, Value : widestring ) : olevariant;              safecall;
          function WriteDate       ( Name : wideString; Value : TDateTime  ) : olevariant; safecall;
          function WriteDateFromStr( Name, Value : widestring ) : olevariant;              safecall;
          function WriteCurrency   ( Name : widestring; Value : currency ) : olevariant;   safecall;

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

          function IsSecureKey       ( FullKeyName : widestring                      ) : olevariant; safecall;
          function SetSecurityOfKey  ( FullKeyName : widestring; Security : wordbool ) : olevariant; safecall;

          function IsSecureValue     ( FullPathName : widestring                      ) : olevariant; safecall;
          function SetSecurityOfValue( FullPathName : widestring; Security : wordbool ) : olevariant; safecall;
          function TypeOf( FullPathNode : widestring ) : olevariant; safecall;

          function IntegrateValues( FullPathName : widestring ) : olevariant; safecall;

          function QueryKey ( FullKeyName,   ValueNameList : widestring ) : oleVariant; safecall;
          function SearchKey( SearchPattern, ValueNameList : widestring ) : oleVariant; safecall;
          function EditKey  ( FullPathKey, newName, oldName : widestring; Security : byte ) : olevariant; safecall;
        private
          function pFullPathKeyExists  ( FullPathKey  : string; var secured : integer ) : boolean;
          function pFullPathValueExists( FullPathName : string; var secured : integer ) : boolean;

          function DeleteValue( FullPathName : string ) : boolean;

          function GetFullPath(    Name : string ) : string;
          function GetNodeID  ( KeyName : string ) : string;

          function WriteValue( aName, aValue : widestring; aType : integer  ) : boolean;
          function ReadValue ( aName : widestring ) : string;

          function WriteBigString( aName, aValue : widestring ) : boolean;
          function ReadBigString ( aName : widestring         ) : string;
          function IsBigString   ( Name  : widestring         ) : boolean;

          function pIsSecureKey     ( FullKeyName  : string ) : boolean;
          function pIsSecureValue   ( FullPathName : string ) : boolean;
//        function IsOrHaveSecureKey( FullKeyName  : string ) : boolean;

          function ExecQuery( FullKeyName, SearchPattern, ValueNameList : string ) : string;
        private
          procedure EndConnection;
          function  InitConnection : boolean;

//        function qFindByField              ( query : String ) : boolean;
          function qModifyFields             ( query : String ) : boolean;
          function qGetValueOfField          ( query : String ) : string;
          function qSelectRowsByField        ( query : String; nfields : integer ) : String;
          function qSelectRowsIncludingValues( query : String; nfields : integer ) : String;
        private
          fCurrentKey   : widestring;
          fDirSecurity  : wordbool;
          fDBName       : string;
          Conn          : Connection;
      end;

  var
    dbUser     : string = 'sa';
    dbPassword : string = '';

implementation

  uses
    Classes, SysUtils, Logs, MainWindow, ComObj;

  const
    MaxVarCharLength = 127;

// Generals procedures

  procedure Log( Msg : string );
    begin
      Logs.Log('queries', DateTimeToStr(Now) + ' ' + Msg);
    end;{ TDirectoryManager.LogThis }

  procedure EncodeString( var str : widestring );
    var
      i : integer;
    begin
      for i := 1 to length(str) do
        if str[i] = ''''
          then str[i] := #7;
    end;{ EncodeString }

  procedure UnEncodeString( var str : string );
    var
      i : integer;
    begin
      for i := 1 to length(str) do
        if str[i] = #7
          then str[i] := '''';
    end;{ UnEncodeString }

  function LinkString( SplitStr : string ) : string;
    var
      p : pchar;
    begin
      p := pchar(SplitStr);
      while p[0] <> #0 do
        begin
          if p[0] = #13
            then p[0] := #8
            else
              if p[0] = #10
                then p[0] := #9;
          inc(p);
        end;
      result := SplitStr;
    end;{ LinkString }

  function SplitString( JointStr : string ) : string;
    var
      p : pchar;
    begin
      p := pchar(JointStr);
      while p[0] <> #0 do
        begin
          if p[0] = #8
            then p[0] := #13
            else
              if p[0] = #9
                then p[0] := #10;
          inc(p);
        end;
      result := JointStr;
    end;{ SplitString }


////////////////////////////////////////////////////////////////////////////////
// TDirectoryManager

  function TDirectoryManager.GetCurrentKey : olevariant;
    begin
      if fCurrentKey <> ''
        then result := fCurrentKey
        else result := '';
    end;{ TDirectoryManager.GetCurrentKey }

  function TDirectoryManager.GetFullPath( Name : string ) : string;
    begin
      if GetCurrentKey = ''
        then result := Name
        else result := GetCurrentKey + '/' + Name;
    end; { TDirectoryManager.GetFullPath }

  function TDirectoryManager.KeyExists( KeyName : widestring ) : olevariant;
    begin
      result := FullPathKeyExists( GetFullPath( KeyName ));
    end;{ TDirectoryManager.KeyExists }

  function TDirectoryManager.ValueExists( Name : widestring ) : olevariant;
    begin
      result := FullPathValueExists( GetFullPath( Name ));
    end;{ TDirectoryManager.ValueExists }

  function TDirectoryManager.SetCurrentKey( FullPathKey : widestring ) : olevariant;
    var
      bool    : boolean;
      secured : integer;
    begin
      if FullPathKey = ''
        then result := true
        else
          begin
            try
              bool   := pFullPathKeyExists( FullPathKey, secured );
              result := bool and ( (secured = 0) or not fDirSecurity  )
            except
              on e : Exception do
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
      EncodeString(Value);
      if( Length( Value ) <= MaxVarCharLength )
        then result := WriteValue( Name, Value, ntString )
        else result := WriteBigString( Name, Value )
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
      if aux <> ''
        then result := (aux = 'true') or (aux = '1')
        else result := false;
    end;{ TDirectoryManager.ReadBoolean }

  function TDirectoryManager.ReadInteger( Name : widestring ) : olevariant;
    var
      aux : string;
    begin
      try
        aux    := ReadValue( Name );
        if aux <> ''
          then result := StrToInt( aux )
          else result := 0;
      except
        result := 0;
      end;
    end;{ TDirectoryManager.ReadInteger }

  function TDirectoryManager.ReadFloat( Name : widestring ) : olevariant;
    var
      aux : string;
    begin
      try
        aux    := ReadValue( Name );
        if aux <> ''
          then result := StrToFloat( aux )
          else result := 0.0;
      except
        result := 0.0;
      end;
    end;{ TDirectoryManager.ReadFloat }

  function TDirectoryManager.ReadString( Name : widestring ) : olevariant;
    var
      s, n : string;
    begin
      n := string(Name);
      if IsBigString( n )
        then s := ReadBigString( n )
        else s := ReadValue( n );
      UnencodeString(s);
      result := s;
    end;{ TDirectoryManager.ReadString }

  function TDirectoryManager.ReadDate( Name : widestring ) : olevariant;
    var
      aux : string;
    begin
      try
        aux := ReadValue( Name );
        if aux <> ''
          then result := StrToFloat( aux )
          else result := 0;
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
    var
      aux : string;
    begin
      try
        aux    := ReadValue( Name );
        if aux <> ''
          then result := StrToFloat( aux )
          else result := 0.0;
      except
        result := 0.0;
      end;
    end;{ TDirectoryManager.ReadCurrency }

  // Specifics procedures

  constructor TDirectoryManager.Create( aDBName : widestring; SetSecurity : wordbool );
    begin
      try
        inherited Create;
        fCurrentKey  := '';
        fDBName      := aDBName;
        fDirSecurity := SetSecurity;
        Conn         := CoConnection.Create;
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
    var
      secured : integer;
    begin
      try
        if pFullPathKeyExists( FullPathKey, secured )
          then
            if fDirSecurity
              then result := true
              else result := (secured = 0)
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
      secured : integer;
    begin
      try
        if pFullPathValueExists( FullPathName, secured )
          then
            if fDirSecurity
              then result := true
              else result := secured = 0
          else result := false;
      except
        on e : Exception do
          begin
            result := false;
            Log('ERROR: FullPathValueExists ' + e.Message + ' Value: ' + FullPathName );
          end;
      end;
    end;{ TDirectoryManager.FullPathValueExists }


  function TDirectoryManager.CreateFullPathKey( FullPathKey : widestring; ForcePath : wordbool ) : olevariant;
  var
    query   : TStringList;
    secured : integer;
  begin
    try
      query := TStringList.Create;
      if pFullPathKeyExists( FullPathKey, secured )
        then
          if fDirSecurity
            then result := true
            else result := secured = 0
        else
          begin
            query.Add('exec proc_InsertKey ' + '''' + FullPathKey + '''');
            result := qModifyFields(query.Text);
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
      query : TStringList;
    begin
      query := TStringList.Create;
      try
        try
          query.Add('SELECT count(*) Counter');
          query.Add('FROM tbl_Keys');
          query.Add('WHERE parent_key = ( ');
          query.Add('SELECT key_id FROM tbl_KeyPaths');

          if fDirSecurity
            then query.Add('WHERE full_path = ' + '''' + GetCurrentKey + '''' + ')' )
            else query.Add('WHERE full_path = ' + '''' + GetCurrentKey + '''' + ' and is_Secured = 0 )' );

          result := StrToInt( qGetValueOfField( query.Text ) );
        finally
          query.Free;
        end;
      except
        on e : Exception do
          begin
            result := -1;
            Log('ERROR: KeysCount ' + e.message );
          end;
      end;
    end;{ TDirectoryManager.KeysCount }


  function TDirectoryManager.ValuesCount : olevariant;
    var
      query : TStringList;
    begin
      query := TStringList.Create;
      try
        try
          query.Add('SELECT count(*) Counter');
          query.Add('FROM tbl_Values');
          query.Add('WHERE parent_key_id = ( ');
          query.Add('SELECT key_id FROM tbl_KeyPaths');

          if fDirSecurity
            then query.Add('WHERE full_path = ' + '''' + GetCurrentKey + '''' + ')' )
            else query.Add('WHERE full_path = ' + '''' + GetCurrentKey + '''' + ' and is_Secured = 0 )' );

          result := StrToInt( qGetValueOfField( query.Text ) )
        finally
          query.Free;
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
      query   : TStringList;
      currKey : string;
    begin
      query := TStringList.Create;
      try
        try
          query.Add('SELECT rel_name');
          query.Add('FROM tbl_Keys' );
          query.Add('WHERE parent_key = (');
          query.Add('SELECT key_id FROM tbl_KeyPaths');

          currKey := GetCurrentKey;
          if fDirSecurity
            then
              begin
                if currKey <> ''
                  then query.Add('WHERE full_Path = ' + '''' + currKey + '''' + ' and key_id <> id )' )
                  else query.Add('WHERE id = key_id )' );
              end
            else
              begin
                if currKey <> ''
                  then query.Add('WHERE full_Path = ' + '''' + currKey + '''' + ' and key_id <> id and is_secured = 0 )' )
                  else query.Add('WHERE id = key_id and is_secured = 0 )' );
              end;

           result := qSelectRowsByField( query.Text, 1 );
        finally
          query.free;
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
      query : TStringList;
    begin
      query := TStringList.Create;
      try
        try
          query.Add('SELECT Name');
          query.Add('FROM tbl_Values' );
          query.Add('WHERE parent_key_id = (');
          query.Add('SELECT key_id FROM tbl_KeyPaths');

          if fDirSecurity
            then query.Add('WHERE full_Path = ' + '''' + GetCurrentKey + '''' + ')' )
            else query.Add('WHERE full_Path = ' + '''' + GetCurrentKey + '''' + 'and is_secured = 0 )' );

          result := qSelectRowsByField( query.Text, 1 );
        finally
          query.free;
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
    var
      query   : TStringList;
      secured : integer;
    begin
      try
        if fDirSecurity
          then
            begin
              if pFullPathKeyExists( FullPathNode, secured )
                then
                  begin
                    query := TStringList.Create;
                    try
                      query.Add('exec proc_DeleteKey ' + '''' + FullPathNode + '''' );
                      result := qModifyFields(query.Text);
                    finally
                      query.Free;
                    end;
                  end
                else result := DeleteValue( FullPathNode )
            end
          else result := false;
      except
        on e : Exception do
          begin
            Log('ERROR: DeleteFullPathNode ' + e.Message );
            result := false;
          end;
      end;
    end;{ TDirectoryManager.DeleteFullPathNode }

  function TDirectoryManager.SetSecurityOfKey( FullKeyName : widestring; Security : wordbool ) : olevariant;
    var
      query   : TStringList;
      secured : integer;
    begin
      try
        if fDirSecurity
          then
            begin
              if pFullPathKeyExists( FullKeyName, secured )
                then
                  begin
                    query := TStringList.Create;
                    try
                      query.Add('UPDATE tbl_Keys');
                      case Security of
                        true  : query.Add('SET is_Secured = 1');
                        false : query.Add('SET is_Secured = 0');
                      end;
                      query.Add('WHERE id = ( ');
                      query.Add('SELECT Key_id FROM tbl_KeyPaths');
                      query.Add('WHERE full_path = ' + '''' + string(FullKeyName) + '''' + ')' );

                      result := qModifyFields(query.Text);
                    finally
                      query.Free;
                    end;
                  end
                else result := false;
            end
          else result := false;
      except
        on e : Exception do
          begin
            Log('ERROR: SetSecurityOfKey ' + e.message );
            result := false;
          end;
      end;
    end;{ TDirectoryManager.SetSecurityOfKey }

  function TDirectoryManager.SetSecurityOfValue( FullPathName : widestring; Security : wordbool ) : olevariant;
    var
      query     : TStringList;
      secured   : integer;
      lsPos     : integer;
      currKey   : string;
      valueName : string;
    begin
      try
        if fDirSecurity
          then
            begin
              if pFullPathValueExists( FullPathName, secured )
                then
                  begin
                    query := TStringList.Create;
                    lsPos := LastDelimiter('/', FullPathName);
                    currKey   := copy(FullPathName, 0, lsPos - 1 );
                    valueNAme := copy(FullPathName, lsPos + 1, length(FullPathName) );
                    try
                      query.Add('UPDATE tbl_Values');
                      if Security
                        then query.Add('SET is_Secured = 1')
                        else query.Add('SET is_Secured = 0');

                      query.Add('WHERE parent_key_id = ( ');
                      query.Add('SELECT Key_id FROM tbl_KeyPaths');
                      query.Add('WHERE full_path = ' + '''' + currKey + '''' + ' and Name = ' + '''' + valueName + '''' +  ' )' );

                      result := qModifyFields(query.Text);
                    finally
                      query.Free;
                    end;
                  end
                else result := false;
            end
          else result := false;
      except
        on e : Exception do
          begin
            Log('ERROR: SetSecurityOfValue ' + e.message );
            result := false;
          end;
      end;
    end;{ TDirectoryManager.SetSecurityOfValue }

  function TDirectoryManager.IsSecureKey( FullKeyName : widestring ) : olevariant;
    begin
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

  function TDirectoryManager.IsSecureValue( FullPathName : widestring ) : olevariant;
    begin
      try
        if pIsSecureValue( FullPathName )
          then result := 1
          else result := 0;
       except
         on e : Exception do
           begin
             result := true;
             Log('ERROR: IsSecureValue ' + e.message );
           end;
       end;
    end;{ TDirectoryManager.IsSecureValue }

{
  function TDirectoryManager.IsOrHaveSecureKey( FullKeyName : string ) : boolean;
    var
      query : TStringList;
    begin
      query := TStringList.Create;
      try
        try
          query.Add('SELECT count(*) Counter');
          query.Add('FROM tbl_KeyAncestors a, tbl_KeyPaths p, tbl_Keys k');
          query.Add('WHERE a.ancestor_key = p.key_id and');
          query.Add('p.full_path = ' + '''' + FullKeyName + '''' + ' and ');
          query.Add('k.id = a.key_id and k.is_secured = 1');

          result := StrToInt( qGetValueOfField( query.Text ) ) > 0;
        finally
          query.free;
        end;
      except
        on e : Exception do
          begin
            result := false;
            Log('ERROR: IsOrHaveSecureKey ' + e.message );
          end;
      end;
    end;
}

  function TDirectoryManager.WriteValue( aName, aValue: widestring; aType : integer ) : boolean;
    var
      UID, id: string;
      Name   : string;
      Value  : string;
      query  : TStringList;
      fields : TStringList;
    begin
      try
        if fDirSecurity
          then
            begin
              query  := TStringList.Create;
              fields := TStringList.Create;
              try
                Name  := string(aName);
                Value := string(aValue);

                query.Add('SELECT v.id, v.Kind');
                query.Add('FROM tbl_values v, tbl_KeyPaths p');
                query.Add('WHERE p.key_id = v.parent_key_id and');
                query.Add('v.name = ' + '''' + Name + '''' + ' and p.full_path = ' + '''' + GetCurrentKey + '''');

                fields.text := qSelectRowsByField( query.Text, 2 );

                if fields.Count > 0
                  then
                    begin
                      id := fields.Strings[0];
                      if fields.Strings[1] = '7'  // Kind
                        then
                          begin
                            query.Add('DELETE FROM tbl_Values');
                            query.Add('WHERE id = ' + '''' + id + '''' );

                            result := qModifyFields( query.Text );

                            if result
                              then begin
                                id := GetNodeID(GetCurrentKey);
                                if id <> ''
                                  then begin
                                    UID := CreateClassID;

                                    query.Clear;
                                    query.Add('INSERT INTO tbl_Values ( id, parent_key_id, Name, Kind, Value ) ');
                                    query.Add('VALUES(' + '''' + UID + '''' + ',' + '''' + id + '''' + ','      );
                                    query.Add('''' + Name + '''' + ',' + IntToStr(aType) + ',' + '''' + Value + '''' + ')' );

                                    result := qModifyFields( query.Text );
                                  end
                                else result := false;
                              end;
                          end
                        else
                          begin
                            query.Add('UPDATE tbl_Values');
                            query.Add('SET value = ' + '''' + Value + '''' + ',' + ' Kind = ' + IntToStr(aType) );
                            query.Add('WHERE id = ' + '''' + id + '''');

                            result := qModifyFields( query.Text );
                          end;
                    end
                  else
                    begin
                      id := GetNodeID(GetCurrentKey);
                      if id <> ''
                        then begin
                          UID := CreateClassID;

                          query.Clear;
                          query.Add('INSERT INTO tbl_Values ( id, parent_key_id, Name, Kind, Value ) ');
                          query.Add('VALUES(' + '''' + UID + '''' + ',' + '''' + id + '''' + ','      );
                          query.Add('''' + Name + '''' + ',' + IntToStr(aType) + ',' + '''' + Value + '''' + ')' );

                          result := qModifyFields( query.Text );
                        end
                      else result := false;
                    end;
              finally
                query.Free;
                fields.Create;
              end;
            end
          else result := false;
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
      query : TStringList;
      Name  : string;
    begin
      query := TStringList.Create;
      try
        try
          Name := string(aName);

          query.Add('SELECT Value');
          query.Add('FROM tbl_Values');
          query.Add('WHERE parent_key_id = (');
          query.Add('SELECT key_id FROM tbl_KeyPaths');

          if fDirSecurity
            then query.Add('WHERE full_path = ' + '''' + GetCurrentKey + '''' + ' and Name = ' + '''' + Name + '''' + ')' )
            else query.Add('WHERE full_path = ' + '''' + GetCurrentKey + '''' + ' and Name = ' + '''' + Name + '''' + 'and is_secured = 0 )' );

          result := qGetValueOfField( query.Text );
        finally
          query.Free;
        end;
      except
        on e : Exception do
          begin
            result := '';
            Log('ERROR: ReadValue ' + e.message );
          end;
      end;
    end;{ TDirectoryManager.ReadValue }

  function TDirectoryManager.ReadBigString( aName : widestring ) : string;
    var
      query  : TStringList;
      fields : TStringList;
      Name   : string;
      bool   : boolean;
    begin
      if fDirSecurity
        then
          begin
            query  := TStringList.Create;
            fields := TStringList.Create;
            try
              try
                Name := string(aName);

                query.Add('SELECT v.id, size');
                query.Add('FROM tbl_values v, tbl_KeyPaths p');
                query.Add('WHERE p.key_id = v.parent_key_id and');
                query.Add('v.name = ' + '''' + Name + '''' + ' and p.full_path = ' + '''' + GetCurrentKey + '''');

                fields.Text := qSelectRowsByField( query.Text, 2 );

                if fields.Count > 0
                  then
                    begin
                      query.Clear;
                      query.Add('sp_dboption ' + fDBName + ',' + '''' +  'select into/bulkcopy' + '''' + ', true');
                      bool := qModifyFields( query.Text );

                      if bool
                        then
                          begin
                            query.Clear;
                            query.Add('DECLARE @@ptrval varbinary(16)');
                            query.Add('SET TEXTSIZE ' + fields.Strings[1]);
                            query.Add('SELECT @@ptrval = TEXTPTR(bigText)');
                            query.Add('FROM tbl_values v');
                            query.Add('WHERE v.id = ' + '''' + fields.Strings[0] + '''' );
                            query.Add('READTEXT tbl_values.bigText @@ptrval 0 ' + fields.Strings[1] );
                            result := qGetValueOfField(query.Text);

                            query.Clear;
                            query.Add('sp_dboption ' + fDBName + ',' + '''' +  'select into/bulkcopy' + '''' + ', false');
                            bool := qModifyFields( query.Text );
                          end
                        else result := '';  
                    end
                  else result := ''
              finally
                query.free;
              end;
            except
              on e : Exception do
                begin
                  Log('ERROR: ReadBigString in ' + GetFullPath(Name) +  '  ' + e.message );
                  result := '';
                end;
            end;
          end
        else result := '';
    end;{ TDirectoryManager.ReadBigString }

  function TDirectoryManager.WriteBigString( aName, aValue : widestring ) : boolean;
    var
      query  : TStringList;
      fields : TStringList;
      vID    : string;
      kID    : string;
      UID    : string;
      Name   : string;
      vSize  : string;
      Value  : string;
      bool   : boolean;
    begin
      try
        result := false;
        if fDirSecurity
          then
            begin
              query  := TStringList.Create;
              fields := TStringList.Create;
              try
                Name  := string(aName);
                Value := string(aValue);
                vSize := IntToStr( Length(Value) );

                query.Add('SELECT v.id, v.Kind');
                query.Add('FROM tbl_values v, tbl_KeyPaths p');
                query.Add('WHERE p.key_id = v.parent_key_id and');
                query.Add('v.name = ' + '''' + Name + '''' + ' and p.full_path = ' + '''' + GetCurrentKey + '''');

                fields.text := qSelectRowsByField( query.Text, 2 );

                if fields.Count > 0
                  then
                    begin
                      vid := fields.Strings[0];

                      if fields.Strings[1] = '4'  // Kind
                        then
                          begin
                            query.Add('UPDATE tbl_Values');
                            query.Add('SET value = ' + '''' + '' + '''' + ',' + ' Kind = 7' + ',' + ' bigText = ' + '''' + '' + '''');
                            query.Add('WHERE id = ' + '''' + vid + '''');
                            bool := qModifyFields( query.Text );
                          end;

                      query.Clear;
                      query.Add('sp_dboption ' + fDBName + ',' + '''' +  'select into/bulkcopy' + '''' + ', true');
                      bool := qModifyFields( query.Text );

                      if bool
                        then
                          begin
                            query.Clear;
                            query.Add('DECLARE @@ptrval varbinary(16)');
                            query.Add('SET TEXTSIZE ' + vSize );
                            query.Add('SELECT @@ptrval = TEXTPTR(bigText)');
                            query.Add('FROM tbl_values v');
                            query.Add('WHERE v.id = ' + '''' + vID + '''' );
                            query.Add('WRITETEXT tbl_values.bigText @@ptrval ' + '''' + Value + '''');
                            result := qModifyFields(query.Text);

                            if result
                              then
                                begin
                                  query.Clear;
                                  query.Add('UPDATE tbl_Values');
                                  query.Add('SET size = ' + vSize );
                                  query.Add('WHERE id = ' + '''' + vID + '''' );
                                  result := qModifyFields(query.Text);
                                end;

                            query.Clear;
                            query.Add('sp_dboption ' + fDBName + ',' + '''' +  'select into/bulkcopy' + '''' + ', false');
                            bool := qModifyFields( query.Text );
                          end
                        else result := false;
                    end
                  else
                    begin
                      kID := GetNodeID(GetCurrentKey);
                      if kID <> ''
                        then
                          begin
                            UID := CreateClassID;

                            query.Clear;
                            query.Add('sp_dboption ' + fDBName + ',' +  '''' +  'select into/bulkcopy' + '''' + ', true');
                            bool := qModifyFields( query.Text );

                            if bool
                              then
                                begin
                                  query.Clear;
                                  query.Add('INSERT INTO tbl_Values ( id, parent_key_id, Name, Kind, Value, bigText, size )'         );
                                  query.Add('VALUES(' + '''' + UID + '''' + ',' + '''' + kID + '''' + ',' + '''' + Name + '''' + ',' );
                                  query.Add( '7' + ',' + '''' + '' + '''' + ',' + '''' + '' + '''' + ',' + vSize + ')'               );
                                  result := qModifyFields( query.Text );

                                  if result
                                    then
                                      begin
                                        query.Clear;
                                        query.Add('DECLARE @@ptrval varbinary(16)');
                                        query.Add('SET TEXTSIZE ' + vSize );
                                        query.Add('SELECT @@ptrval = TEXTPTR(bigText)');
                                        query.Add('FROM tbl_values');
                                        query.Add('WHERE id = ' + '''' + UID + '''');
                                        query.Add('WRITETEXT tbl_values.bigText @@ptrval ' + '''' + Value + '''');

                                        result := qModifyFields(query.Text);
                                      end;

                                  query.Clear;
                                  query.Add('sp_dboption ' + fDBName + ',' + '''' +  'select into/bulkcopy' + '''' + ', false');
                                  bool := qModifyFields( query.Text );
                                end
                              else result := false;
                          end
                        else result := false
                    end;
              finally
                query.Free;
                fields.Create;
              end;
            end
          else result := false;
      except
        on e : Exception do
          begin
            result := false;
            Log('ERROR: WriteValue in ' + GetFullPath(Name) + '  ' + e.message  );
          end;
      end;
    end;{ TDirectoryManager.WriteBigString }

  function TDirectoryManager.TypeOf( FullPathNode : widestring ) : olevariant; safecall;
    var
      query     : TStringList;
      lsPos     : integer;
      ValueName : string;
      KeyName   : string;
      StrKind   : string;
    begin
      query := TStringList.Create;
      try
        try
          lsPos     := LastDelimiter( '/', FullPathNode );
          keyName   := System.Copy( FullPathNode, 0, lsPos -1 );
          ValueName := System.Copy( FullPathNode, lsPos + 1, length(FullPathNode) );

          query.Add('SELECT Kind');
          query.Add('FROM tbl_Values');
          query.Add('WHERE parent_key_id = (');
          query.Add('SELECT key_id FROM tbl_KeyPaths');
          query.Add('WHERE full_path = ' + '''' + keyName + '''' + ' and Name = ' + '''' + ValueName + '''' +  ')' );

          StrKind := qGetValueOfField( query.Text );

          if length( StrKind ) > 0
            then result := StrToInt( StrKind )
            else result := 0;
        finally
          query.free;
        end;
      except
        on e : Exception do
          begin
            result := -1;
            Log('ERROR: TypeOf ' + e.message + '--->  KeyName = ' + KeyName + ' ValueName = ' + ValueName  );
          end;
      end;
    end;{ TDirectoryManager.TypeOf }

  function TDirectoryManager.pIsSecureKey( FullKeyName : string ) : boolean;
    var
      query : TStringList;
    begin
      query := TStringList.Create;
      try
        try
          query.Add('SELECT count(*) Counter');
          query.Add('FROM tbl_Keys' );
          query.Add('WHERE is_Secured = 1 and id = ( ');
          query.Add('SELECT key_id');
          query.Add('FROM tbl_KeyPaths');
          query.Add('WHERE full_path = ' + '''' + FullKeyName + '''' + ')' );

          result := StrToInt( qGetValueOfField( query.Text ) ) > 0;
        finally
          query.free;
        end;
      except
        on e : Exception do
          begin
            result := false;
            Log('ERROR: pIsSecureKey ' + e.message );
          end;
      end;
    end;{ TDirectoryManager.IsSecureKey }

  function TDirectoryManager.pIsSecureValue( FullPathName : string ) : boolean;
    var
      query     : TStringList;
      lsPos     : integer;
      keyName   : string;
      ValueName : string;
      aux       : string;
    begin
      query := TStringList.Create;
      try
        try
          lsPos     := LastDelimiter( '/', FullPathName );
          keyName   := System.Copy( FullPathName, 0, lsPos -1 );
          ValueName := System.Copy( FullPathName, lsPos + 1, length(FullPathName) );

          query.Add('SELECT is_Secured');
          query.Add('FROM tbl_Values');
          query.Add('WHERE parent_key_id = (');
          query.Add('SELECT key_id');
          query.Add('FROM tbl_KeyPaths');
          query.Add('WHERE full_path = ' + '''' + keyName + '''' + ' and Name = ' + '''' + ValueName + '''' +  ')' );

          aux := qGetValueOfField( query.Text );

          if aux <> ''
            then result := StrToInt( aux ) <> 0
            else result := false;

        finally
          query.free;
        end;
      except
        on e : Exception do
          begin
            result := false;
            Log('ERROR: pIsSecureValue ' + e.message );
          end;
      end;
    end;{ TDirectoryManager.IsSecureValue }

  function TDirectoryManager.DeleteValue( FullPathName : string ) : boolean;
    var
      query     : TStringList;
      lsPos     : integer;
      keyName   : string;
      ValueName : string;
    begin
      if fDirSecurity
        then
          begin
            query := TStringList.Create;
            try
              try
                lsPos     := LastDelimiter( '/', FullPathName );
                keyName   := System.Copy( FullPathName, 0, lsPos -1 );
                ValueName := System.Copy( FullPathName, lsPos + 1, length(FullPathName) );

                query.Add('DELETE');
                query.Add('FROM tbl_Values');
                query.Add('WHERE parent_key_id = (');
                query.Add('SELECT key_id FROM tbl_KeyPaths');
                query.Add('WHERE full_path = ' + '''' + keyName + '''' + ' and Name = ' + '''' + ValueName + '''' +  ')' );

                result := qModifyFields( query.Text );
              finally
                query.free;
              end;
            except
              on e : Exception do
                begin
                  result := false;
                  Log('ERROR: pIsSecureValue ' + e.message );
                end;
            end;
          end
        else result := false;
    end; { TDirectoryManager.DeleteValue }

  function TDirectoryManager.GetNodeID( keyName : string ) : string;
    var
      query : TStringList;
    begin
      query := TStringList.Create;
      try
        if KeyName <> ''
          then
            try
              query.Add('SELECT key_id');
              query.Add('FROM tbl_KeyPaths' );
              query.Add('WHERE full_path = ' + '''' + keyName + '''' );

              result := qGetValueOfField( query.Text );
            finally
              query.Free;
            end
          else result := '';
      except
        on e : Exception do
          begin
            result := '';
            Log('ERROR: GetNodeID ' + e.message );
          end;
      end;
    end;{ TDirectoryManager.GetNodeID}

  function TDirectoryManager.IsBigString( Name : widestring ) : boolean;
    begin
      try
        result := TypeOf( GetFullPath(Name) ) = ntBigString;
      except
        on e : exception do
          begin
            Log('IsBigString --> ERROR: ( ' + e.Message + ' key: ' + GetFullPath(Name) );
            result := false;
          end;
      end;
    end;{ TDirectoryManager.IsBigString }

  function TDirectoryManager.pFullPathKeyExists( FullPathKey : string; var secured : integer ) : boolean;
    var
      query : TStringList;
      aux   : string;
    begin
      query  := TStringList.Create;
      try
        try
          query.Add('SELECT is_secured');
          query.Add('FROM tbl_Keys');
          query.Add('WHERE id = (');
          query.Add('SELECT key_id FROM tbl_KeyPaths WHERE full_path = ' + '''' + FullPathKey + '''' + ')' );

          aux := qGetValueOfField( query.Text );

          if aux <> ''
            then
              begin
                result  := true;
                secured := StrToInt(aux);
              end
            else
              begin
                result  := false;
                secured := -1;
              end;
        finally
          query.Free;
        end;
      except
        on e : Exception do
          begin
            result := false;
            Log('ERROR: pFullPathKeyExists ' +  e.message);
          end;
      end
    end;{ TDirectoryManager.pFullPathKeyExists }

  function TDirectoryManager.pFullPathValueExists( FullPathName : string; var secured : integer ) : boolean;
    var
      query     : TStringList;
      lsPos     : integer;
      keyName   : string;
      ValueName : string;
      aux       : string;
    begin
      query := TStringList.Create;
      try
        try
          lsPos     := LastDelimiter( '/', FullPathName );
          keyName   := System.Copy( FullPathName, 0, lsPos -1 );
          ValueName := System.Copy( FullPathName, lsPos + 1, length(FullPathName) );

          query.Add('SELECT is_secured');
          query.Add('FROM tbl_Values');
          query.Add('WHERE parent_key_id = (');
          query.Add('SELECT key_id');
          query.Add('FROM tbl_KeyPaths');
          query.Add('WHERE full_path = ' + '''' + keyName + '''' + ' and Name = ' + '''' + ValueName + '''' +  ' )' );

          aux := qGetValueOfField( query.Text );

          if aux <> ''
            then
              begin
                result  := true;
                secured := StrToInt(aux);
              end
            else
              begin
                result  := false;
                secured := -1;
              end;
        finally
          query.free;
        end;
      except
        on e : Exception do
          begin
            result := false;
            Log('ERROR: pIsSecureValue ' + e.message );
          end;
      end;
    end;{ TDirectoryManager.pFullPathValueExists }

  function TDirectoryManager.IntegrateValues( FullPathName : widestring ) : olevariant;
    var
      value      : string;
      intSum     : integer;
      floatSum   : double;
      ntKind     : integer;
      row        : integer;
      query      : TStringList;
      RowsFields : TStringList;
    begin
      query      := TStringList.Create;
      RowsFields := TStringList.Create;
      try
        intSum     := 0;
        floatSum   := 0;
        try
          query.Add('SELECT Value, Kind');
          query.Add('FROM tbl_Values JOIN tbl_KeyPaths');
          query.Add('ON ( key_id = parent_key_id and (Kind = 2 or Kind = 3) ) ');
          query.Add('WHERE full_path LIKE ' + '''' + string(FullPathName) + '/%' + '''' );

          RowsFields.Text := qSelectRowsIncludingValues( query.Text, 2 );

          for row := 0 to (RowsFields.Count div 2) - 1 do
            begin
              value := SplitString( RowsFields.Strings[2 * row] );   // Value
              if StrToInt( RowsFields.Strings[2 * row + 1] ) = ntInteger // Kind
                then ntKind := ntInteger
                else ntKind := ntFloat;
              case ntKind of
                ntInteger   : intSum   := intSum   + StrToInt  (value);
                ntFloat     : floatSum := floatSum + StrToFloat(value);
                else result := 0;
              end;
            end;
        finally
          query.free;
          RowsFields.free;
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
      FullPathKey : string;
    begin
      try
        FullPathKey := GetCurrentKey;
        result      := ExecQuery(FullPathKey, SearchPattern, ValueNameList );
      except
        on e : Exception do
          begin
            Log('ERROR: SearchKey ' + e.message );
            result := '';
          end;
      end;
    end;{ TDirectoryManager.SearchKey }

  function TDirectoryManager.QueryKey( FullKeyName, ValueNameList : widestring ) : olevariant;
    begin
      try
        result := ExecQuery(FullKeyName, '', ValueNameList );
      except
        on e : Exception do
          begin
            result := '';
            Log('ERROR: QueryKey ' + e.message );
          end;
      end;
    end;{ TDirectoryManager.QueryKey }

  function TDirectoryManager.ExecQuery( FullKeyName, SearchPattern, ValueNameList : string ) : string;
    var
      query       : TStringList;
      RowsFields  : TStringList;
      resultList  : TStringList;
      valueNames  : TStringList;
      i, j, Count : integer;
      idx, row, p : integer;
      value       : string;
      Element     : string;
      subElement  : string;

      function queryFormat( str : string ) : string;
        var
          i : integer;
        begin
          for i := 1 to length(str) do
            if str[i] = '*'
              then str[i] := '%';
          result := str;
        end;{ queryFormat }

      function GetPosition( Entry : string ) : integer;
        var
          i    : integer;
          Find : boolean;
        begin
          i := 0;
          Find := false;
          while not Find and (i < valueNames.Count ) do
            begin
              if System.Pos( UpperCase(valueNames[i]), UpperCase(Entry) ) > 0
                then Find := true
                else inc(i);
            end;
          if Find
            then result :=  i
            else result := -1;
        end;{ GetPosition }

      function TruncateKey( KeyName, FullKeyName : string ) : string;
        var
          lsPos : integer;
        begin
          result := System.copy(KeyName, length(FullKeyName) + 2, length(KeyName) );
          lsPos  := System.Pos( '/', result );
          if lsPos > 0
            then result := System.copy(result, 0, lsPos - 1 );
        end;{ TruncateKey }

      function queryValuesName : string;
        var
          lsPos, i  : integer;
          valueName : string;
        begin
          result := 'v.Name = ';
          for i := 0 to valueNames.Count - 1 do
            begin
              lsPos := LastDelimiter( '/', valueNames.Strings[i] );
              if lsPos > 0
                then valueName := System.copy(valueNames.Strings[i], lsPos + 1, length(valueNames.Strings[i]) )
                else valueName := valueNames.Strings[i];
              result := result + '''' + valueName + '''';
              if i < valueNames.Count - 1
                then result := result + ' or v.Name = '
            end;
        end;{ queryValuesName }

      function queryKeysName : string;
        var
          lsPos, i : integer;
          keyName  : string;
        begin
          result := '';
          for i := 0 to valueNames.Count - 1 do
            begin
              lsPos := LastDelimiter( '/', valueNames.Strings[i] );
              if lsPos > 0
                then keyName := System.copy(valueNames.Strings[i], 0, lsPos - 1)
                else keyName := valueNames.Strings[i];
              keyName := '''' + '%/' + keyName + '''';
              if Pos(keyName, result ) = 0
                then
                  begin
                    if i <> 0
                      then result := result + ' or ';
                    result := result + 'full_path LIKE ' + keyName;
                  end;
            end;
        end;{ queryKeysName }

    begin
      query      := TStringList.Create;
      valueNames := TStringList.Create;
      resultList := TStringList.Create;
      RowsFields := TStringList.Create;
      try
        valueNames.Text := string( Lowercase(ValueNameList) );
        if valueNames.Count > 0
          then
            begin
              try
                query.Add('SELECT p.full_path, v.Name, v.Value');
                query.Add('FROM tbl_KeyPaths p, tbl_Values v');

                if SearchPattern = ''
                  then
                    begin
                      query.Add('WHERE full_path LIKE ' + '''' + FullKeyName + '/%' + '''' );
                      query.Add('and ( ' + queryKeysName + ' ) and p.key_id = v.parent_key_id and ( ' + queryValuesName + ')' );
                    end
                  else
                    begin
                      query.Add('WHERE full_path LIKE ' + '''' + FullKeyName + '/'  + queryFormat(SearchPattern) + '''' );
                      query.Add('and p.key_id = v.parent_key_id and ( ' + queryValuesName + ')' );
                    end;

                RowsFields.Text := qSelectRowsIncludingValues( query.Text, 3 );

                Count := RowsFields.Count div 3;
                if Count > 0
                  then
                    begin
                      idx := 1;
                      Element := RowsFields.Strings[0];
                      for row := 0 to Count - 1 do
                        begin
                          subElement := RowsFields.Strings[3 * row];
                          if subElement <> Element
                            then
                              begin
                                Element := subElement;
                                inc(idx);
                              end
                        end;
                    end
                  else idx := 0;

                resultList.Values['Count'] := '0';

                if count > 0
                  then
                    begin
                      row := 0;
                      idx := 0;
                      while row <= Count - 1 do
                        begin
                          Element := TruncateKey( RowsFields.Strings[3 * row], FullKeyName );
                          resultList.Values['Key' + IntToStr(idx)] := Element;
                          j := 0;
                          for i := row to row + ValueNames.Count - 1 do
                            begin
                              if i <= Count - 1
                                then
                                  begin
                                    subElement := TruncateKey( RowsFields.Strings[i * 3], FullKeyName );
                                    if subElement = Element
                                      then
                                        begin
                                          SubElement := RowsFields.Strings[i * 3] + '/' + RowsFields.Strings[i * 3 + 1];
                                          p := GetPosition(SubElement);
                                          if p >= 0
                                            then
                                              begin
                                                value := SplitString( RowsFields.Strings[i * 3 + 2] );
                                                resultList.Values[ valueNames[p] + IntToStr(idx) ] := value;
                                             end;
                                          inc(j);
                                        end;
                                  end;
                            end;
                          inc(idx);
                          inc(row, j);
                        end;
                      resultList.Values['Count'] := IntToStr(idx);
                    end;
                result := resultList.Text;
              finally
                RowsFields.free;
                resultList.Free;
                valueNames.free;
                query.Free;
              end;
            end;
      except
        on e : Exception do
          begin
            result := '';
            Log('ERROR: QueryKey ' + e.message );
          end;
      end;
    end;{ TDirectoryManager.ExcQuery }


  function TDirectoryManager.EditKey( FullPathKey, newName, oldName : widestring; Security : byte ) : olevariant;
    var
      query : TStringList;
    begin
      query := TStringList.Create;
      try
        try
          query.Add('exec proc_RenameKey ' + '''' + FullPathKey + '''' + ',' + '''' + newName+ '''' + ',' + ''''+ oldName + '''' + ',' + IntToStr(Security) );
          result := qModifyFields(query.Text);
        finally
          query.Free;
        end;
      except
        on e : Exception do
          begin
            result := false;
            Log('ERROR: EditKey ' + e.message );
          end;
      end;
    end;{ TDirectoryManager.EditKey }


  function TDirectoryManager.InitConnection : boolean;
    var
      Conn_Str : string;
    begin
      try
        Conn_Str := 'Provider=SQLOLEDB.1; Initial Catalog=' + fDBName + '; Data Source=' + DirectoryWin.IPAddress.Text;
        Conn.Open(Conn_Str, dbUser, dbPassword, 0);
        Conn.Set_CommandTimeout(0);
        result := true;
      except
        raise;
        result := false;
      end;
    end;{ TDirectoryManager.QueryFind }

  procedure TDirectoryManager.EndConnection;
    begin
      try
        Conn.Close;
      except
        raise;
      end;
    end;{ TDirectoryManager.EndConnection }

{
  function TDirectoryManager.qFindByField( query : String ) : boolean;
    var
      rs        : _RecordsetDisp;
      rec_Count : olevariant;
    begin
      if InitConnection
        then begin
          try
            try
              rs  := Conn.Execute(query, rec_count, 0) as _RecordsetDisp;
              result := not rs.EOF;
            finally
              EndConnection;
            end;
          except
            raise;
            result := false;
          end
        end
      else result := false;
    end;
}

  function TDirectoryManager.qGetValueOfField( query : String ) : string;
    var
      rs        : _RecordsetDisp;
      rec_Count : olevariant;
    begin
      if InitConnection
        then begin
          try
            try
              rs  := Conn.Execute(query, rec_count, 0) as _RecordsetDisp;
              if not rs.EOF
                then result := rs.Fields.Item[0].Value
                else result := '';
            finally
              EndConnection;
            end;
          except
            result := '';
            raise;
          end;
        end
      else result := '';
    end;{ TDirectoryManager.qGetValueOfField }

  function TDirectoryManager.qModifyFields( query : String ) : boolean;
    var
      rs        : _RecordsetDisp;
      rec_Count : olevariant;
    begin
      if InitConnection
        then begin

          try
            try
              rs := Conn.Execute(query, rec_count, 0) as _RecordsetDisp;
              result := true;
            finally
              EndConnection;
            end;
          except
            raise;
            result := false;
          end;
        end
      else result := false;
    end;{ TDirectoryManager.qModifyFields }

  function TDirectoryManager.qSelectRowsByField( query : String; nfields : integer ) : String;
    var
      rs        : _RecordsetDisp;
      rec_Count : olevariant;
      FieldList : TStringList;
      i         : integer;
    begin
      if InitConnection
        then begin
          try
            FieldList := TStringList.Create;
            try
              rs  := Conn.Execute(query, rec_count, 0) as _RecordsetDisp;
              while not rs.EOF do
                begin
                  for i := 0 to nFields - 1 do
                    FieldList.Add(rs.Fields.Item[i].Value);
                  rs.MoveNext;
                end;
              result := FieldList.Text;
            finally
              FieldList.Free;
              EndConnection;
            end;
          except
            result := '';
            raise;
          end
        end
      else result := '';
    end;{ TDirectoryManager.qSelectRowsByField }

  function TDirectoryManager.qSelectRowsIncludingValues( query : String; nfields : integer ) : String;
    var
      rs        : _RecordsetDisp;
      rec_Count : olevariant;
      FieldList : TStringList;
      i         : integer;
      aux       : string;
    begin
      if InitConnection
        then begin
          try
            FieldList := TStringList.Create;
            try
              rs  := Conn.Execute(query, rec_count, 0) as _RecordsetDisp;
              while not rs.EOF do
                begin
                  for i := 0 to nFields - 1 do
                    begin
                      aux := LinkString(rs.Fields.Item[i].Value);
                      FieldList.Add(aux);
                    end;
                  rs.MoveNext;
                end;
              result := FieldList.Text;
            finally
              FieldList.Free;
              EndConnection;
            end;
          except
            result := '';
            raise;
          end
        end
      else result := '';
    end;{ TDirectoryManager.qSelectRowsIncludingValues }


initialization

  Log( 'Restarting .......................');

finalization

  Log( 'Closing ..........................');

end.
