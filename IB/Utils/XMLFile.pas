unit XMLFile;

interface

  uses
    classes;

  type
    TXMLNode =
      class
        public
          constructor Create( anXMLNode : OleVariant );
        public
          function  getName : string;
          function  CreateChild( NodeName : string ) : TXMLNode;
          procedure DeleteChildByName( NodeName : string );
          procedure DeleteChild( Child : TXMLNode );
          function  getChildByName( Name : string ) : TXMLNode;
        public
          function  ValueAsString : string;
          function  ValueAsBoolean : boolean;
          function  ValueAsInteger : integer;
          function  ValueAsFloat : single;
          procedure setValueAsString( value : string );
          procedure setValueAsBoolean( value : boolean );
          procedure setValueAsInteger( value : integer );
          procedure setValueAsFloat( value : single );
        public
          function ReadString( PropertyName : string; defaultValue : string ) : string;
          function ReadBoolean( PropertyName : string; defaultValue : boolean ) : boolean;
          function ReadInteger( PropertyName : string; defaultValue : integer ) : integer;
          function ReadFloat( PropertyName : string; defaultValue : single ) : single;
        public
          procedure WriteString( PropertyName : string; Value : string );
          procedure WriteBoolean( PropertyName : string; Value : boolean );
          procedure WriteInteger( PropertyName : string; Value : integer );
          procedure WriteFloat( PropertyName : string; Value : single );
        public
          procedure getChilds( storage : TList );
          procedure queryChilds( query : string; storage : TList );
        private
          fXMLNode : OleVariant;
      end;

    TXMLFile =
      class
        public
          constructor Create;
          destructor Destroy; override;
        public
          function  Load( filename : string ): boolean;
          procedure Save( filename : string );
        public
          function  CreateNode( Path : string; NodeName : string ) : TXMLNode;
          procedure DeleteNodeByName( Path : string; NodeName : string );
          procedure DeleteNode( Child : TXMLNode );
          function  getNodeByName( Path : string ) : TXMLNode;
        public
          function ReadString( Path : string; PropertyName : string; defaultValue : string ) : string;
          function ReadBoolean( Path : string; PropertyName : string; defaultValue : boolean ) : boolean;
          function ReadInteger( Path : string; PropertyName : string; defaultValue : integer ) : integer;
          function ReadFloat( Path : string; PropertyName : string; defaultValue : single ) : single;
        public
          procedure WriteString( Path : string; PropertyName : string; Value : string );
          procedure WriteBoolean( Path : string; PropertyName : string; Value : boolean );
          procedure WriteInteger( Path : string; PropertyName : string; Value : integer );
          procedure WriteFloat( Path : string; PropertyName : string; Value : single );
        public
          procedure getAllNodes( Path : string; storage : TList );
          procedure queryNodes( Path : string; query : string; storage : TList );
        private
          fXML      : OleVariant;
          fRootNode : TXMLNode;
          fFilename : string;
          function getNode( Path : string ) : TXMLNode;
        public
          property RootNode : TXMLNode read fRootNode;
      end;

implementation

  uses
    SysUtils, ComObj;

  // TXMLNode

  constructor TXMLNode.Create( anXMLNode : OleVariant );
    begin
      inherited Create;
      fXMLNode := anXMLNode;
    end;

  function TXMLNode.getName : string;
    begin
      result := fXMLNode.nodeName;
    end;

  function TXMLNode.CreateChild( NodeName : string ) : TXMLNode;
    var
      newNode : OleVariant;
    begin
      newNode := fXMLNode.ownerDocument.createNode(1, NodeName, '');
      fXMLNode.appendChild( newNode );

      result := TXMLNode.Create( newNode );
    end;

  procedure TXMLNode.DeleteChildByName( NodeName : string );
    var
      newNode : OleVariant;
    begin
      newNode := fXMLNode.selectSingleNode( NodeName );
      fXMLNode.removeChild( newNode );
    end;

  procedure TXMLNode.DeleteChild( Child : TXMLNode );
    begin
      fXMLNode.removeChild( Child.fXMLNode );
    end;

  function TXMLNode.getChildByName( Name : string ) : TXMLNode;
    var
      node : OleVariant;
    begin
      node := fXMLNode.selectSingleNode( Name );
      if not VarIsEmpty( node )
        then result := TXMLNode.Create( node )
        else result := nil;
    end;

  function TXMLNode.ValueAsString : string;
    begin
       result := ReadString( 'value', '' );
    end;

  function TXMLNode.ValueAsBoolean : boolean;
    begin
       result := ReadBoolean( 'value', false );
    end;

  function TXMLNode.ValueAsInteger : integer;
    begin
       result := ReadInteger( 'value', 0 );
    end;

  function TXMLNode.ValueAsFloat : single;
    begin
       result := ReadFloat( 'value', 0 );
    end;

  procedure TXMLNode.setValueAsString( value : string );
    begin
      WriteString( 'value', value );
    end;

  procedure TXMLNode.setValueAsBoolean( value : boolean );
    begin
      WriteBoolean( 'value', value );
    end;

  procedure TXMLNode.setValueAsInteger( value : integer );
    begin
      WriteInteger( 'value', value );
    end;

  procedure TXMLNode.setValueAsFloat( value : single );
    begin
      WriteFloat( 'value', value );
    end;

  function TXMLNode.ReadString( PropertyName : string; defaultValue : string ) : string;
    var
      attr : OleVariant;
    begin
      attr := fXMLNode.attributes.getNamedItem( PropertyName );

      if not VarIsEmpty(attr)
        then result := attr.NodeValue
        else result := defaultValue;
    end;

  function TXMLNode.ReadBoolean( PropertyName : string; defaultValue : boolean ) : boolean;
    var
      attr : OleVariant;
    begin
      attr := fXMLNode.attributes.getNamedItem( PropertyName );

      if not VarIsEmpty(attr)
        then
          try
            result := boolean(StrToInt(attr.NodeValue));
          except
            result := defaultValue;
          end
        else result := defaultValue;
    end;

  function TXMLNode.ReadInteger( PropertyName : string; defaultValue : integer ) : integer;
    var
      attr : OleVariant;
    begin
      attr := fXMLNode.attributes.getNamedItem( PropertyName );

      if not VarIsEmpty(attr)
        then
          try
            result := StrToInt(attr.NodeValue);
          except
            result := defaultValue;
          end
        else result := defaultValue;
    end;

  function TXMLNode.ReadFloat( PropertyName : string; defaultValue : single ) : single;
    var
      attr : OleVariant;
    begin
      attr := fXMLNode.attributes.getNamedItem( PropertyName );

      if not VarIsEmpty(attr)
        then
          try
            result := StrToFloat(attr.NodeValue);
          except
            result := defaultValue;
          end
        else result := defaultValue;
    end;

  procedure TXMLNode.WriteString( PropertyName : string; Value : string );
    var
      attr : OleVariant;
    begin
      attr := fXMLNode.attributes.getNamedItem( PropertyName );

      if VarIsEmpty(attr)
        then
          begin
            attr           := fXMLNode.ownerDocument.createAttribute( PropertyName );
            attr.nodeValue := Value;
            fXMLNode.attributes.setNamedItem( attr );
          end
        else
          begin
            attr.nodeValue := Value;
          end;
    end;

  procedure TXMLNode.WriteBoolean( PropertyName : string; Value : boolean );
    var
      attr : OleVariant;
    begin
      attr := fXMLNode.attributes.getNamedItem( PropertyName );

      if VarIsEmpty(attr)
        then
          begin
            attr           := fXMLNode.ownerDocument.createAttribute( PropertyName );
            attr.nodeValue := IntToStr(integer(Value));
            fXMLNode.attributes.setNamedItem( attr );
          end
        else
          begin
            attr.nodeValue := IntToStr(integer(Value));
          end;
    end;

  procedure TXMLNode.WriteInteger( PropertyName : string; Value : integer );
    var
      attr : OleVariant;
    begin
      attr := fXMLNode.attributes.getNamedItem( PropertyName );

      if VarIsEmpty(attr)
        then
          begin
            attr           := fXMLNode.ownerDocument.createAttribute( PropertyName );
            attr.nodeValue := IntToStr(Value);
            fXMLNode.attributes.setNamedItem( attr );
          end
        else
          begin
            attr.nodeValue := IntToStr(Value);
          end;
    end;

  procedure TXMLNode.WriteFloat( PropertyName : string; Value : single );
    var
      attr : OleVariant;
    begin
      attr := fXMLNode.attributes.getNamedItem( PropertyName );

      if VarIsEmpty(attr)
        then
          begin
            attr           := fXMLNode.ownerDocument.createAttribute( PropertyName );
            attr.nodeValue := FloatToStr( Value );
            fXMLNode.attributes.setNamedItem( attr );
          end
        else
          begin
            attr.nodeValue := FloatToStr( Value );
          end;
    end;

  procedure TXMLNode.getChilds( storage : TList );
    var
      i : integer;
    begin
      for i := 0 to pred(integer(fXMLNode.childNodes.length)) do
        storage.Add( TXMLNode.Create( fXMLNode.childNodes.item(i) ));
    end;

  procedure TXMLNode.queryChilds( query : string; storage : TList );
    var
      nodes : OleVariant;
      i     : integer;
    begin
      nodes := fXMLNode.selectNodes( query );
      for i := 0 to pred(integer(nodes.length)) do
        storage.Add( TXMLNode.Create( nodes.item(i) ));
    end;

  // TXMLFile

  constructor TXMLFile.Create;
    var
      node : OleVariant;
    begin
      inherited;
      fXML                 := CreateOleObject( 'Msxml2.DOMDocument.4.0' ); //Msxml2.DOMDocument
      node                 := fXML.createElement( 'root' );
      fXML.documentElement := node;
      fRootNode            := TXMLNode.Create( fXML.documentElement );
    end;

  destructor TXMLFile.Destroy;
    begin
      fRootNode.Free;
      inherited;
    end;

  function TXMLFile.Load( filename : string ): boolean;
    begin
      try
        fFilename := filename;
        fXML.load( filename );
        if fRootNode <> nil
          then fRootNode.Free;
        fRootNode := TXMLNode.Create( fXML.documentElement );
        result := true;
      except
        result := false;
      end;
    end;

  procedure TXMLFile.Save( filename : string );
    begin
      try
        if filename <> ''
          then fFilename := filename;
          
        fXML.save( fFilename );
      except
      end;
    end;

  function TXMLFile.CreateNode( Path : string; NodeName : string ) : TXMLNode;
    var
      parent : TXMLNode;
    begin
      if Path = ''
        then parent := fRootNode
        else parent := getNode( Path );

      if parent <> nil
        then result := parent.CreateChild( NodeName )
        else result := nil;

      if Path <> ''
        then parent.Free;
    end;

  procedure TXMLFile.DeleteNodeByName( Path : string; NodeName : string );
    var
      parent : TXMLNode;
    begin
      if Path = ''
        then parent := fRootNode
        else parent := getNode( Path );

      if parent <> nil
        then parent.DeleteChildByName( NodeName );

      if Path <> ''
        then parent.Free;
    end;

  procedure TXMLFile.DeleteNode( Child : TXMLNode );
    var
      parent : TXMLNode;
    begin
      parent := nil;
      if not VarIsEmpty( Child.fXMLNode.parentNode )
        then parent := TXMLNode.Create( Child.fXMLNode.parentNode );

      if parent <> nil
        then parent.DeleteChild( Child );

      parent.Free;
    end;

  function TXMLFile.getNodeByName( Path : string ) : TXMLNode;
    begin
      result := getNode( Path );
    end;

  function TXMLFile.ReadString( Path : string; PropertyName : string; defaultValue : string ) : string;
    var
      node : TXMLNode;
    begin
      try
        if Path = ''
          then node := fRootNode
          else node := getNode( Path );

        if node <> nil
          then
            begin
              if PropertyName <> ''
                then result := node.ReadString( PropertyName, defaultValue )
                else result := node.ValueAsString;
            end
          else result := defaultValue;

        if Path <> ''
          then node.Free;
      except
        result := defaultValue;
      end;
    end;

  function TXMLFile.ReadBoolean( Path : string; PropertyName : string; defaultValue : boolean ) : boolean;
    var
      node : TXMLNode;
    begin
      try
        if Path = ''
          then node := fRootNode
          else node := getNode( Path );

        if node <> nil
          then
            begin
              if PropertyName <> ''
                then result := node.ReadBoolean( PropertyName, defaultValue )
                else result := node.ValueAsBoolean;
            end
          else result := defaultValue;

        if Path <> ''
          then node.Free;
      except
        result := defaultValue;
      end;
    end;

  function TXMLFile.ReadInteger( Path : string; PropertyName : string; defaultValue : integer ) : integer;
    var
      node : TXMLNode;
    begin
      try
        if Path = ''
          then node := fRootNode
          else node := getNode( Path );

        if node <> nil
          then
            begin
              if PropertyName <> ''
                then result := node.ReadInteger( PropertyName, defaultValue )
                else result := node.ValueAsInteger;
            end
          else result := defaultValue;

        if Path <> ''
          then node.Free;
      except
        result := defaultValue;
      end;
    end;

  function TXMLFile.ReadFloat( Path : string; PropertyName : string; defaultValue : single ) : single;
    var
      node : TXMLNode;
    begin
      try
        if Path = ''
          then node := fRootNode
          else node := getNode( Path );

        if node <> nil
          then
            begin
              if PropertyName <> ''
                then result := node.ReadFloat( PropertyName, defaultValue )
                else result := node.ValueAsFloat;
            end
          else result := defaultValue;

        if Path <> ''
          then node.Free;
      except
        result := defaultValue;
      end;
    end;

  procedure TXMLFile.WriteString( Path : string; PropertyName : string; Value : string );
    var
      node : TXMLNode;
    begin
      try
        if Path = ''
          then node := fRootNode
          else node := getNode( Path );

        if node <> nil
          then
            begin
              if PropertyName <> ''
                then node.WriteString( PropertyName, Value )
                else node.setValueAsString( Value );
            end;

        if Path <> ''
          then node.Free;
      except
      end;
    end;

  procedure TXMLFile.WriteBoolean( Path : string; PropertyName : string; Value : boolean );
    var
      node : TXMLNode;
    begin
      try
        if Path = ''
          then node := fRootNode
          else node := getNode( Path );

        if node <> nil
          then
            begin
              if PropertyName <> ''
                then node.WriteBoolean( PropertyName, Value )
                else node.setValueAsBoolean( Value );
            end;

        if Path <> ''
          then node.Free;
      except
      end;
    end;

  procedure TXMLFile.WriteInteger( Path : string; PropertyName : string; Value : integer );
    var
      node : TXMLNode;
    begin
      try
        if Path = ''
          then node := fRootNode
          else node := getNode( Path );

        if node <> nil
          then
            begin
              if PropertyName <> ''
                then node.WriteInteger( PropertyName, Value )
                else node.setValueAsInteger( Value );
            end;

        if Path <> ''
          then node.Free;
      except
      end;
    end;

  procedure TXMLFile.WriteFloat( Path : string; PropertyName : string; Value : single );
    var
      node : TXMLNode;
    begin
      try
        if Path = ''
          then node := fRootNode
          else node := getNode( Path );

        if node <> nil
          then
            begin
              if PropertyName <> ''
                then node.WriteFloat( PropertyName, Value )
                else node.setValueAsFloat( Value );
            end;

        if Path <> ''
          then node.Free;
      except
      end;
    end;

  procedure TXMLFile.getAllNodes( Path : string; storage : TList );
    var
      node : TXMLNode;
    begin
      try
        if Path = ''
          then node := fRootNode
          else node := getNode( Path );

        if node <> nil
          then node.getChilds( storage );

        if Path <> ''
          then node.Free;
      except
      end;
    end;

  procedure TXMLFile.queryNodes( Path : string; query : string; storage : TList );
    var
      node : TXMLNode;
    begin
      try
        if Path = ''
          then node := fRootNode
          else node := getNode( Path );

        if node <> nil
          then node.queryChilds( query, storage );

        if Path <> ''
          then node.Free;
      except
      end;
    end;

  function TXMLFile.getNode( Path : string ) : TXMLNode;
    var
      node : OleVariant;
    begin
      node := fXML.selectSingleNode( Path );
      if not VarIsEmpty( node )
        then result := TXMLNode.Create( node )
        else result := nil;
    end;

end.
