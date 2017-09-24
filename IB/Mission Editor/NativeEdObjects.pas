unit NativeEdObjects;

interface

  uses
    XMLFile, EditableObjects;

  type
    TNativeMetaEditableObject =
      class(TMetaEditableObject)
        public
          function  Instantiate( aName, aValue : string; const data ) : TEditableObject; override;
          procedure Clone( aMetaEditableObject : TMetaEditableObject ); override;
        private
          fInstancesEditor  : TObjectEditor;
          fInstancesOptions : integer;
        public
          property InstancesEditor : TObjectEditor read fInstancesEditor write fInstancesEditor;
          property InstancesOptions : integer      read fInstancesOptions write fInstancesOptions;
      end;

    TNativeMetaObjectPool =
      class(TMetaObjectPool)
        public
          procedure Init( const data ); override;
          procedure Save( filename : string );
        private
          procedure ReadMetaInstances( filename : string );
          procedure SaveMetaInstance( where : TXMLNode; Instance : TNativeMetaEditableObject );
          procedure SaveProperty( where : TXMLNode; prop : TNativeMetaEditableObject );
      end;

  var
    TheMetaObjectPool : TNativeMetaObjectPool;

  procedure CreateMetaObjectPool;
  procedure InitMetaObjectPool( defFile : string );
  procedure DestroyMetaObjectPool;

implementation

  uses
    classes, sysutils, stringUtils;

  // TNativeMetaEditableObject

  function TNativeMetaEditableObject.Instantiate( aName, aValue : string; const data ) : TEditableObject;
    var
      i        : integer;
      newProp : TEditableObject;
    begin
      result              := TEditableObject.Create;
      result.ObjectEditor := fInstancesEditor;
      result.MetaClass    := self;
      result.Name         := aName;
      result.Value        := aValue;
      result.Options      := fInstancesOptions;

      for i := 0 to pred(fProperties.Count) do
        with TMetaEditableObject(fProperties[i]) do
          begin
            newProp := TMetaEditableObject(fProperties[i]).Instantiate( TEditableObject(fProperties[i]).Name, TEditableObject(fProperties[i]).Value, data );
            result.Properties.Insert( newProp );
          end;
    end;

  procedure TNativeMetaEditableObject.Clone( aMetaEditableObject : TMetaEditableObject );
    var
      NativeMetaObject : TNativeMetaEditableObject;
      prop             : TNativeMetaEditableObject;
      i                : integer;
    begin
      NativeMetaObject := TNativeMetaEditableObject(aMetaEditableObject);
      NativeMetaObject.ObjectEditor      := ObjectEditor;
      NativeMetaObject.InstancesEditor   := InstancesEditor;
      NativeMetaObject.Options           := NativeMetaObject.Options or Options;
      NativeMetaObject.fInstancesOptions := fInstancesOptions;
      NativeMetaObject.MetaClass         := MetaClass;
      for i := 0 to pred(fProperties.Count) do
        begin
          prop       := TNativeMetaEditableObject.Create;
          prop.Name  := TNativeMetaEditableObject(fProperties[i]).Name;
          prop.Value := TNativeMetaEditableObject(fProperties[i]).Value;
          TNativeMetaEditableObject(fProperties[i]).Clone( prop );
          NativeMetaObject.Properties.Insert( prop );
        end;
    end;

  // TNativeMetaObjectPool

  procedure TNativeMetaObjectPool.Init( const data );
    var
      filename : string absolute data;
    begin
      ReadMetaInstances( filename );
    end;

  procedure TNativeMetaObjectPool.Save( filename : string );
    var
      XMLFile : TXMLFile;

    procedure _Save( Instance : TNativeMetaEditableObject );
      var
        i    : integer;
        node : TXMLNode;
      begin
        node := XMLFile.CreateNode( '/root/Metaclasses', 'Metaclass' );
        try
          SaveMetaInstance( node, Instance );
        finally
          node.Free;
        end;

        for i := 0 to pred(Instance.Children.Count) do
          _Save( TNativeMetaEditableObject(Instance.Children[i]) );
      end;
    var
      i : integer;
    begin
      XMLFile := TXMLFile.Create;
      try
        XMLFile.CreateNode( '', 'Metaclasses' );
        for i := 0 to pred(fMetaObjects.Count) do
          _Save( TNativeMetaEditableObject(fMetaObjects[i]) );
        XMLFile.Save( filename );
      finally
        XMLFile.Free;
      end;
    end;

  procedure TNativeMetaObjectPool.ReadMetaInstances( filename : string );
    function createMetaObject( ancestor : TNativeMetaEditableObject; cname : string ): TNativeMetaEditableObject;
      var
        i : integer;
      begin
        result              := TNativeMetaEditableObject.Create;
        result.Name         := cName;
        result.ObjectEditor := getEditor( cname );

        if ancestor <> nil
          then
            begin
              if ancestor.getChildren( cname ) = nil
                then
                  begin
                    result.fInstancesEditor := ancestor.fInstancesEditor;
                    ancestor.Children.Insert( result );
                    result.Parent := ancestor;
                    for i := 0 to pred(ancestor.Properties.Count) do
                      result.Properties.Insert( ancestor.Properties[i] );
                  end;
            end
          else
            begin
              if get( cname ) = nil
                then fMetaObjects.Insert( result );
            end;
      end;

    procedure ReadProperty( obj : TNativeMetaEditableObject; node : TXMLNode );
      var
        prop         : TNativeMetaEditableObject;
        prop1        : TNativeMetaEditableObject;
        MetaClass    : TNativeMetaEditableObject;
        i            : integer;
        newProp      : boolean;
        propName     : string;
        cName        : string;
        aux          : integer;
        proplist     : TList;
        propNode     : TXMLNode;
      begin
        try
          propName := node.ReadString( 'name', '' );
          if propName <> ''
            then
              begin
                prop    := TNativeMetaEditableObject(obj.getProperty( propName ));
                newProp := prop = nil;

                if newProp
                  then
                    begin
                      prop      := TNativeMetaEditableObject.Create;
                      prop.Name := propName;
                    end;


                cName := node.ReadString( 'class', '' );
                if cName <> ''
                  then
                    begin
                      if newProp
                        then
                          begin
                            MetaClass := TNativeMetaEditableObject(get( cName ));
                            if MetaClass <> nil
                              then
                                begin
                                  MetaClass.Clone( prop );
                                  prop.MetaClass := MetaClass;
                                end;
                          end;
                    end;

                prop.Value            := node.ValueAsString;
                aux := node.ReadInteger( 'options', 0 );
                if aux <> 0
                  then prop.Options := aux;

                aux := node.ReadInteger( 'i_options', 0 );
                if aux <> 0
                  then prop.InstancesOptions := aux;

                proplist := TList.Create;

                try
                  node.queryChilds( 'property', proplist );
                  for i := 0 to pred(proplist.Count) do
                    begin
                      propNode := TXMLNode(proplist[i]);

                      prop1 := TNativeMetaEditableObject(prop.getProperty( propNode.ReadString( 'name', '' )));
                      if prop1 = nil
                        then
                          begin
                            prop1       := TNativeMetaEditableObject.Create;
                            prop1.Name  := propNode.ReadString( 'name', '' );
                            prop1.Value := propNode.ValueAsString;
                            prop.Properties.Insert( prop1 );
                          end
                        else
                          begin
                            prop1.Value := propNode.ValueAsString;
                          end;

                      propNode.Free;
                    end;

                finally
                  proplist.Free;
                end;

                if newProp
                  then obj.Properties.Insert( prop );
              end
        finally
          node.Free;
        end;
      end;

    var
      XMLFile   : TXMLFile;
      currClass : TXMLNode;
      cName     : string;
      i, j      : integer;
      anc       : TNativeMetaEditableObject;
      obj       : TNativeMetaEditableObject;
      ancestor  : string;
      classlist : TList;
      propList  : TList;
    begin
      XMLFile   := TXMLFile.Create;
      classlist := TList.Create;
      propList  := TList.Create;
      try
        XMLFile.Load( filename );

        XMLFile.queryNodes( '/root/Metaclasses', 'Metaclass', classlist );
        for i := 0 to pred(classlist.Count) do
          begin
            currClass := TXMLNode(classlist[i]);
            cName     := currClass.ReadString( 'name', '' );
            if cName <> ''
              then
                begin
                  ancestor  := currClass.ReadString( 'ancestor', '' );
                  anc       := TNativeMetaEditableObject(get( ancestor ));
                  obj       := createMetaObject( anc, cName );

                  obj.fInstancesEditor := getEditor( currClass.ReadString( 'i_editor', '' ));
                  obj.Value            := currClass.ValueAsString;
                  obj.InstancesOptions := currClass.ReadInteger( 'i_options', 0 );
                  obj.Options          := currClass.ReadInteger( 'options', 0 );
                  obj.ObjectEditor     := getEditor( currClass.ReadString( 'editor', '' ));

                  propList.Clear;
                  currClass.queryChilds( 'property', propList );

                  for j := 0 to pred(propList.Count) do
                    ReadProperty( obj, TXMLNode(propList[j]) );

                  if (obj.InstancesEditor = nil) and (anc <> nil)
                    then obj.InstancesEditor := anc.InstancesEditor;
                end;

            currClass.Free;
          end;
      finally
        XMLFile.Free;
        classlist.Free;
        propList.Free;
      end;
    end;

  procedure TNativeMetaObjectPool.SaveMetaInstance( where : TXMLNode; Instance : TNativeMetaEditableObject );
    var
      i        : integer;
      propNode : TXMLNode;
    begin
      with where do
        begin
          if Instance.Parent <> nil
            then WriteString( 'ancestor', Instance.Parent.getFullName );

          WriteString( 'name', Instance.Name );
          if Instance.InstancesEditor <> nil
            then WriteString( 'i_editor', Instance.InstancesEditor.Name );

          setValueAsString( Instance.Value );
          WriteInteger( 'i_options', Instance.InstancesOptions );
          WriteInteger( 'options', Instance.Options );

          if Instance.InstancesEditor <> nil
            then WriteString( 'i_editor', Instance.InstancesEditor.Name );

          if Instance.ObjectEditor <> nil
            then WriteString( 'editor', Instance.ObjectEditor.Name );

          for i := 0 to pred(Instance.Properties.Count) do
            begin
              propNode := where.CreateChild( 'property' );
              SaveProperty( propNode, TNativeMetaEditableObject(Instance.Properties[i]) );
              propNode.Free;
            end;
        end;
    end;

  procedure TNativeMetaObjectPool.SaveProperty( where : TXMLNode; prop : TNativeMetaEditableObject );
    var
      i    : integer;
      node : TXMLNode;
    begin
      where.WriteString( 'name', prop.Name );

      if prop.MetaClass <> nil
        then where.WriteString( 'class', prop.MetaClass.getFullName );

      where.setValueAsString( prop.Value );
      where.WriteInteger( 'options', prop.Options );
      where.WriteInteger( 'i_options', prop.InstancesOptions );

      for i := 0 to pred( prop.Properties.Count) do
        begin
          node := where.CreateChild( 'property' );
          try
            node.WriteString( 'name', TNativeMetaEditableObject(prop.Properties[i]).Name );
            node.WriteString( 'value', TNativeMetaEditableObject(prop.Properties[i]).Value );
          finally
            node.Free;
          end;
        end;
    end;

  procedure CreateMetaObjectPool;
    begin
      TheMetaObjectPool := TNativeMetaObjectPool.Create;
    end;

  procedure InitMetaObjectPool( defFile : string );
    begin
      TheMetaObjectPool.Init( defFile );
    end;

  procedure DestroyMetaObjectPool;
    begin
      TheMetaObjectPool.Free;
    end;

end.
