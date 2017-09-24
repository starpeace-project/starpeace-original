unit DirServerSession;

interface

  type
    IDirServerSession =
      interface
        procedure EndSession;

        function GetCurrentKey : widestring ;
        function SetCurrentKey( FullPathKey : widestring ) : olevariant;

        function CreateFullPathKey( FullPathKey : widestring; ForcePath : wordbool ) : olevariant;
        function CreateKey ( KeyName : widestring ) : olevariant;

        function FullPathKeyExists( FullPathKey : widestring ) : olevariant;
        function KeyExists( KeyName : widestring ) : olevariant;

        function KeysCount   : olevariant;
        function ValuesCount : olevariant;

        function GetKeyNames   : olevariant;
        function GetValueNames : olevariant;

        procedure WriteBoolean( Name : widestring; Value : wordbool );
        procedure WriteInteger( Name : widestring; Value : integer );
        procedure WriteFloat( Name : widestring; Value : double );
        procedure WriteString( Name, Value : widestring );
        procedure WriteDate( Name : widestring; Value : TDateTime );
        procedure WriteDateFromStr( Name, Value : widestring );
        procedure WriteCurrency( Name : widestring; Value : currency );

        function ReadBoolean( Name : widestring ) : olevariant;
        function ReadInteger( Name : widestring ) : olevariant;
        function ReadFloat( Name : widestring ) : olevariant;
        function ReadString( Name : widestring ) : olevariant;
        function ReadDate( Name : widestring ) : olevariant;
        function ReadDateAsStr( Name : widestring ) : olevariant;
        function ReadCurrency( Name : widestring ) : olevariant;

        function FullPathValueExists( FullPathName : widestring ) : olevariant;
        function ValueExists( Name : widestring ) : olevariant;

        function DeleteFullPathNode( FullPathNode : widestring ) : olevariant;
        function DeleteNode( NodeName : widestring ) : olevariant;

        function FullQuery( aQuery : widestring ) : olevariant;
        function Query( aQuery : widestring ) : olevariant;

        function IntegrateValues( RelValuePath : widestring ) : olevariant;

        function QueryKey( FullKeyName, ValueNameList : widestring ) : olevariant;
      end;  // >> the session interface is not complete

    type
      TDirServerSession =
        class(TInterfacedObject, IDirServerSession)
          public
            constructor Create(SessionProxy : variant);
            destructor  Destroy; override;
          private // IDirServerSession
            procedure EndSession;

            function GetCurrentKey : widestring;
            function SetCurrentKey( FullPathKey : widestring ) : olevariant;

            function CreateFullPathKey( FullPathKey : widestring; ForcePath : wordbool ) : olevariant;
            function CreateKey( KeyName : widestring ) : olevariant;

            function FullPathKeyExists( FullPathKey : widestring ) : olevariant;
            function KeyExists( KeyName : widestring ) : olevariant;

            function KeysCount   : olevariant;
            function ValuesCount : olevariant;

            function GetKeyNames   : olevariant;
            function GetValueNames : olevariant;

            procedure WriteBoolean( Name : widestring; Value : wordbool );
            procedure WriteInteger( Name : widestring; Value : integer );
            procedure WriteFloat( Name : widestring; Value : double );
            procedure WriteString( Name, Value : widestring );
            procedure WriteDate( Name : widestring; Value : TDateTime );
            procedure WriteDateFromStr( Name, Value : widestring );
            procedure WriteCurrency( Name : widestring; Value : currency );

            function ReadBoolean( Name : widestring ) : olevariant;
            function ReadInteger( Name : widestring ) : olevariant;
            function ReadFloat( Name : widestring ) : olevariant;
            function ReadString( Name : widestring ) : olevariant;
            function ReadDate( Name : widestring ) : olevariant;
            function ReadDateAsStr( Name : widestring ) : olevariant;
            function ReadCurrency( Name : widestring ) : olevariant;

            function FullPathValueExists( FullPathName : widestring ) : olevariant;
            function ValueExists( Name : widestring ) : olevariant;

            function DeleteFullPathNode( FullPathNode : widestring ) : olevariant;
            function DeleteNode( NodeName : widestring ) : olevariant;

            function FullQuery( aQuery : widestring ) : olevariant;
            function Query( aQuery : widestring ) : olevariant;

            function IntegrateValues( RelValuePath : widestring ) : olevariant;

            function QueryKey( FullKeyName, ValueNameList : widestring ) : olevariant;
          private
            fSessionProxy : variant;
        end;

implementation

  // TDirServerSession

  constructor TDirServerSession.Create(SessionProxy : variant);
    begin
      inherited Create;
      fSessionProxy := SessionProxy;
    end;

  destructor TDirServerSession.Destroy;
    begin
      EndSession;
      fSessionProxy := Null;
      inherited;
    end;

  procedure TDirServerSession.EndSession;
    begin
      fSessionProxy.RDOEndSession;
    end;

  function TDirServerSession.GetCurrentKey : widestring;
    begin
      Result := fSessionProxy.RDOGetCurrentKey;
    end;

  function TDirServerSession.SetCurrentKey( FullPathKey : widestring ) : olevariant;
    begin
      Result := fSessionProxy.RDOSetCurrentKey(FullPathKey);
    end;

  function TDirServerSession.CreateFullPathKey( FullPathKey : widestring; ForcePath : wordbool ) : olevariant;
    begin
      Result := fSessionProxy.RDOCreateFullPathKey(FullPathKey, ForcePath);
    end;

  function TDirServerSession.CreateKey( KeyName : widestring ) : olevariant;
    begin
      Result := fSessionProxy.RDOCreateKey(KeyName);
    end;

  function TDirServerSession.FullPathKeyExists( FullPathKey : widestring ) : olevariant;
    begin
      Result := fSessionProxy.RDOFullPathKeyExists(FullPathKey);
    end;

  function TDirServerSession.KeyExists( KeyName : widestring ) : olevariant;
    begin
      Result := fSessionProxy.RDOKeyExists(KeyName);
    end;

  function TDirServerSession.KeysCount : olevariant;
    begin
      Result := fSessionProxy.RDOKeysCount;
    end;

  function TDirServerSession.ValuesCount : olevariant;
    begin
      Result := fSessionProxy.RDOValuesCount;
    end;

  function TDirServerSession.GetKeyNames : olevariant;
    begin
      Result := fSessionProxy.RDOGetKeyNames;
    end;

  function TDirServerSession.GetValueNames : olevariant;
    begin
      Result := fSessionProxy.RDOGetValueNames;
    end;

  procedure TDirServerSession.WriteBoolean( Name : widestring; Value : wordbool );
    begin
      fSessionProxy.RDOWriteBoolean(Name, Value);
    end;

  procedure TDirServerSession.WriteInteger( Name : widestring; Value : integer );
    begin
      fSessionProxy.RDOWriteInteger(Name, Value);
    end;

  procedure TDirServerSession.WriteFloat( Name : widestring; Value : double );
    begin
      fSessionProxy.RDOWriteFloat(Name, Value);
    end;

  procedure TDirServerSession.WriteString( Name, Value : widestring );
    begin
      fSessionProxy.RDOWriteString(Name, Value);
    end;

  procedure TDirServerSession.WriteDate( Name : widestring; Value : TDateTime );
    begin
      fSessionProxy.RDOWriteDate(Name, Value);
    end;

  procedure TDirServerSession.WriteDateFromStr( Name, Value : widestring );
    begin
      fSessionProxy.RDOWriteDateFromStr(Name, Value);
    end;

  procedure TDirServerSession.WriteCurrency( Name : widestring; Value : currency );
    begin
      fSessionProxy.RDOWriteCurrency(Name, Value);
    end;

  function TDirServerSession.ReadBoolean( Name : widestring ) : olevariant;
    begin
      Result := fSessionProxy.RDOReadBoolean(Name);
    end;

  function TDirServerSession.ReadInteger( Name : widestring ) : olevariant;
    begin
      Result := fSessionProxy.RDOReadInteger(Name);
    end;

  function TDirServerSession.ReadFloat( Name : widestring ) : olevariant;
    begin
      Result := fSessionProxy.RDOReadFloat(Name);
    end;

  function TDirServerSession.ReadString( Name : widestring ) : olevariant;
    begin
      Result := fSessionProxy.RDOReadString(Name);
    end;

  function TDirServerSession.ReadDate( Name : widestring ) : olevariant;
    begin
      Result := fSessionProxy.RDOReadDate(Name);
    end;

  function TDirServerSession.ReadDateAsStr( Name : widestring ) : olevariant;
    begin
      Result := fSessionProxy.RDOReadDateFromStr(Name);
    end;

  function TDirServerSession.ReadCurrency( Name : widestring ) : olevariant;
    begin
      Result := fSessionProxy.RDOReadCurrency(Name);
    end;

  function TDirServerSession.FullPathValueExists( FullPathName : widestring ) : olevariant;
    begin
      Result := fSessionProxy.RDOFullPathValueExists(FullPathName);
    end;

  function TDirServerSession.ValueExists( Name : widestring ) : olevariant;
    begin
      Result := fSessionProxy.RDOValueExists(Name);
    end;

  function TDirServerSession.DeleteFullPathNode( FullPathNode : widestring ) : olevariant;
    begin
      Result := fSessionProxy.RDODeleteFullPathNode(FullPathNode);
    end;

  function TDirServerSession.DeleteNode( NodeName : widestring ) : olevariant;
    begin
      Result := fSessionProxy.RDODeleteNode(NodeName);
    end;

  function TDirServerSession.FullQuery( aQuery : widestring ) : olevariant;
    begin
      Result := fSessionProxy.RDOFullQuery(aQuery);
    end;

  function TDirServerSession.Query( aQuery : widestring ) : olevariant;
    begin
      Result := fSessionProxy.RDOQuery(aQuery);
    end;

  function TDirServerSession.IntegrateValues( RelValuePath : widestring ) : olevariant;
    begin
      Result := fSessionProxy.RDOIntegrateValues(RelValuePath);
    end;

  function TDirServerSession.QueryKey( FullKeyName, ValueNameList : widestring ) : olevariant;
    begin
      Result := fSessionProxy.RDOQueryKey(FullKeyName, ValueNameList);
    end;

end.
