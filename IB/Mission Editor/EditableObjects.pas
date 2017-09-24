unit EditableObjects;

interface

  uses
    classes, Collection;

  const // options
    OPT_ALLOWNEW  = 1;
    OPT_EVENT     = 2;
    OPT_NUMERABLE = 4;

  const // visual options
    VOP_SHOWINMAINCONTROL    = 1;
    VOP_EDITCHILDREN         = 2;
    VOP_ALLOWPROPERTYEDITING = 4;
    VOP_DONOTSHOWEVENTS      = 8;

  const // msgs
    MSG_GETSELOBJECT = 1;

  type
    TEditableObject     = class;
    CEditableObject     = class of TEditableObject;
    TObjectEditor       = class;
    TMetaEditableObject = class;

    TObjectEditor =
      class
        public
          function  SendMessage( control : TObject; msg : integer; var data ) : boolean; virtual; abstract;
          procedure HideEditor( const visualcontext; visualEditor : TObject ); virtual; abstract;
          procedure ShowEditor( const visualcontext; visualEditor : TObject ); virtual; abstract;
          function  CreateVisualEditor( options : integer; const visualcontext; obj : TEditableObject ) : TObject; virtual; abstract;
          procedure UpdateObject( VisualEditor : TObject; obj : TObject ); virtual; abstract;
          function  CreateNewObject : TEditableObject; virtual; abstract;
          procedure DestroyEditor( editor : TObject ); virtual; abstract;
        protected
          fName : string;
        public
          property Name : string read fName;
      end;

    TEditableObject =
      class
        public
          constructor Create;
        public
          function Edit( options : integer; const visualcontext ) : TObject;
        public
          function getChildren( name : string ) : TEditableObject;
          function getProperty( propName : string ) : TEditableObject;
        protected
          fName         : string;
          fValue        : string;
          fChildren     : TCollection;
          fOptions      : integer;
          fProperties   : TCollection;
          fObjectEditor : TObjectEditor;
          fMetaClass    : TMetaEditableObject;
        public
          property Name         : string read fName write fName;
          property Value        : string read fValue write fValue;
          property Options      : integer read fOptions write fOptions;
          property Properties   : TCollection read fProperties;
          property Children     : TCollection read fChildren;
          property ObjectEditor : TObjectEditor read fObjectEditor write fObjectEditor;
          property MetaClass    : TMetaEditableObject read fMetaClass write fMetaClass;
      end;

    TMetaEditableObject =
      class(TEditableObject)
        public
          function  Instantiate( aName, aValue : string; const data ) : TEditableObject; virtual; abstract;
          procedure Clone( aMetaEditableObject : TMetaEditableObject ); virtual;
          function  getFullName : string;
        private
          fParent : TMetaEditableObject;
        public
          property Parent : TMetaEditableObject read fParent write fParent;
      end;

    TMetaObjectPool =
      class
        public
          constructor Create;
          destructor  Destroy; override;
        public
          procedure Init( const data ); virtual; abstract;
          function  get( cname : string ) : TMetaEditableObject;
          procedure registerEditor( ObjectEditor : TObjectEditor );
          function  getEditor( name : string ): TObjectEditor;
        protected
          fMetaObjects : TCollection;
          fEditors     : TCollection;
      end;

implementation

  uses
    stringUtils, sysUtils;

  // TEditableObject

  constructor TEditableObject.Create;
    begin
      inherited Create;
      fChildren     := TCollection.Create( 1, rkBelonguer );
      fProperties   := TCollection.Create( 10, rkBelonguer );
    end;

  function TEditableObject.Edit( options : integer; const visualcontext ) : TObject;
    begin
      result := nil;
      if fObjectEditor <> nil
        then result := fObjectEditor.CreateVisualEditor( options, visualcontext, self );
    end;

  function TEditableObject.getChildren( name : string ) : TEditableObject;
    var
      i : integer;
    begin
      i := 0;
      while (i < fChildren.Count) and (CompareText(TEditableObject(fChildren[i]).Name, name) <> 0) do
        inc( i );
      if i < fChildren.Count
        then result := TEditableObject(fChildren[i])
        else result := nil;
    end;

  function TEditableObject.getProperty( propName : string ) : TEditableObject;
    var
      i : integer;
    begin
      i := 0;
      while (i < fProperties.Count) and (TEditableObject(fProperties[i]).Name <> propName) do
        inc( i );
      if i < fProperties.Count
        then result := TEditableObject(fProperties[i])
        else result := nil;
    end;

  // TMetaEditableObject

  procedure TMetaEditableObject.Clone( aMetaEditableObject : TMetaEditableObject );
    begin
    end;

 function TMetaEditableObject.getFullName : string;
   var
     aux   : TMetaEditableObject;
     names : TStringList;
     i     : integer;
   begin
     names := TStringList.Create;
     try
       aux   := self;
       while aux <> nil do
         begin
           names.Add( aux.Name );
           aux := aux.Parent;
         end;

       result := names[pred(names.Count)];
       for i := names.Count -2 downto 0 do
         result := result + '.' + names[i];
     finally
       names.Free;
     end;
   end;
   
  // TMetaObjectPool

  constructor TMetaObjectPool.Create;
    begin
      inherited;
      fMetaObjects := TCollection.Create( 20, rkBelonguer );
      fEditors     := TCollection.Create( 20, rkBelonguer );
    end;

  destructor TMetaObjectPool.Destroy;
    begin
      fMetaObjects.Free;
      fEditors.Free;
      inherited;
    end;

  function TMetaObjectPool.get( cname : string ) : TMetaEditableObject;
    function getRootClass( name : string ) : TMetaEditableObject;
      var
        i : integer;
      begin
        i := 0;
        while (i < fMetaObjects.Count) and (TEditableObject(fMetaObjects[i]).Name <> name) do
          inc( i );
        if i < fMetaObjects.Count
          then result := TMetaEditableObject(fMetaObjects[i])
          else result := nil;
      end;

    var
      i         : integer;
      classInfo : TStringList;
      c         : TMetaEditableObject;
    begin
      classInfo := TStringList.Create;

      try
        SymbolSeparated( cname, '.', classinfo );

        c := nil;
        if classinfo.Count > 0
          then
            begin
              c := getRootClass( classinfo[0] );
              for i := 1 to pred(classinfo.Count) do
                c := TMetaEditableObject(c.getChildren( classinfo[i] ));
            end;
        result := c;
      finally
        classInfo.Free;
      end;
    end;

  procedure TMetaObjectPool.registerEditor( ObjectEditor : TObjectEditor );
    begin
      fEditors.Insert( ObjectEditor );
    end;

  function TMetaObjectPool.getEditor( name : string ): TObjectEditor;
    var
      i : integer;
    begin
      i := 0;
      while (i < fEditors.Count) and (CompareText(TObjectEditor(fEditors[i]).Name, name) <> 0) do
        inc( i );
      if i < fEditors.Count
        then result := TObjectEditor(fEditors[i])
        else result := nil;
    end;

end.
