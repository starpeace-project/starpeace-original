unit MetaInstances;

interface

  uses
    ClassStorageInt, Classes, Languages, Variants
    {$IFNDEF CLIENT}
    , CacheAgent;
    {$ELSE}
    ;
    {$ENDIF}

  type
    // MetaIntances are objects to be collected in the ClassStorage. They act as
    // classes of other objects. Important features:
    //   Id : string
    //     Class Identifier (must be unique withing one class family).
    //   Register( ClassFamily )
    //     Registers the MetaInstance in the class storage as [ClassFamily, Id]

    TMetaInstance =
      class
        public
          constructor Create( anId : string );
        private
          fId        : string;
          fFamily    : string;
          fIndex     : integer;
          fCacheable : boolean;
        published
          property Id     : string     read fId;
          property Family : string     read fFamily;
          property Index  : integer    read fIndex;
          property Cacheable : boolean read fCacheable write fCacheable;
        public
          procedure RetrieveTexts( Container : TDictionary ); virtual;
          procedure StoreTexts   ( Container : TDictionary ); virtual;
          procedure EvaluateTexts; virtual;
          procedure CloneTexts; virtual;
        public
          procedure Register( ClassFamily : TClassFamilyId );
      end;

  function ObjectIs( ClassName : string; O : TObject ) : boolean;
  function ClassIs( ClassName : string; C : TClass ) : boolean;
  {$IFNDEF CLIENT}
  procedure CacheFamily( Family : string );
  procedure CloneFamily( Family : string );
  {$ENDIF}
  procedure RetrieveTexts( filename : string );
  procedure StoreTexts( Family : string; filename : string );
  procedure StoreMissingTexts( Family : string; filename, LangId : string );
  procedure EvaluateTexts( Family : string );

implementation

  uses
    ClassStorage
    {$IFNDEF CLIENT}
    , ModelServerCache;
    {$ELSE}
    ;
    {$ENDIF}

  // TMetaInstance

  constructor TMetaInstance.Create( anId : string );
    begin
      inherited Create;
      fId        := anId;
      fCacheable := false;
    end;

  procedure TMetaInstance.RetrieveTexts( Container : TDictionary );   
    begin
    end;

  procedure TMetaInstance.StoreTexts( Container : TDictionary );
    begin
    end;

  procedure TMetaInstance.EvaluateTexts;
    begin
    end;

  procedure TMetaInstance.CloneTexts;
    begin
    end;

  procedure TMetaInstance.Register( ClassFamily : TClassFamilyId );
    begin
      fFamily := ClassFamily;
      TheClassStorage.RegisterClass( ClassFamily, fId, self );
      fIndex := TheClassStorage.ClassCount[ClassFamily];
      {
      if Cacheable
        then CacheMetaObject( self, noKind, noInfo );
      }
    end;


  // ObjectIs

  function ObjectIs( ClassName : string; O : TObject ) : boolean;
    var
      SuperClass : TClass;
    begin
      try
        SuperClass := O.ClassType;
        while (SuperClass <> nil) and (SuperClass.ClassName <> ClassName) do
          SuperClass := SuperClass.ClassParent;
        result := (SuperClass <> nil);
      except
        result := false;
      end;
    end;                        


  // ClassIs

  function ClassIs( ClassName : string; C : TClass ) : boolean;
    var
      SuperClass : TClass;
    begin
      try
        SuperClass := C;
        while (SuperClass <> nil) and (SuperClass.ClassName <> ClassName) do
          SuperClass := SuperClass.ClassParent;
        result := (SuperClass <> nil);
      except
        result := false;
      end;
    end;


  {$IFNDEF CLIENT}
  // CacheFamily

  procedure CacheFamily( Family : string );
    var
      count : integer;
      i     : integer;
    begin
      count := TheClassStorage.ClassCount[Family];
      for i := 0 to pred(count) do
        try
          if TMetaInstance(TheClassStorage.ClassByIdx[Family, i]).Cacheable
            then CacheMetaObject( TheClassStorage.ClassByIdx[Family, i], noKind, noInfo );
        except
        end;
    end;

  //CloneFamily
  procedure CloneFamily( Family : string );
    var
      count : integer;
      i     : integer;
    begin
      count := TheClassStorage.ClassCount[Family];
      for i := 0 to pred(count) do
        try
          TMetaInstance(TheClassStorage.ClassByIdx[Family, i]).CloneTexts;
        except
        end;
    end;
  {$ENDIF}

  procedure RetrieveTexts( filename : string );
    var
      Dict   : TDictionary;
      Family : string;
      count  : integer;
      i      : integer;
    begin
      Dict := TDictionary.Create( filename );
      try
        Family := Dict.Values['Target'];
        if Family <> ''
          then                                      
            begin
              count := TheClassStorage.ClassCount[Family];
              for i := 0 to pred(count) do
                try
                  TMetaInstance(TheClassStorage.ClassByIdx[Family, i]).RetrieveTexts( Dict );
                except
                end;
            end;
      finally
        Dict.Free;
      end;
    end;

  procedure StoreTexts( Family : string; filename : string );  
    var
      Dict  : TDictionary;
      count : integer;
      i     : integer;
    begin
      Dict := TDictionary.Create( '' );
      try
        count := TheClassStorage.ClassCount[Family];
        for i := 0 to pred(count) do
          try
            TMetaInstance(TheClassStorage.ClassByIdx[Family, i]).StoreTexts( Dict );
          except
          end;
        Dict.Store( filename );           
      finally
        Dict.Free;
      end;
    end;

  procedure StoreMissingTexts( Family : string; filename, LangId : string );
    var
      Dict  : TDictionary;
      count : integer;
      i     : integer;
    begin
      Dict := TDictionary.Create( '' );
      Dict.LangId := LangId;
      try
        count := TheClassStorage.ClassCount[Family];
        for i := 0 to pred(count) do
          try
            TMetaInstance(TheClassStorage.ClassByIdx[Family, i]).StoreTexts( Dict );
          except
          end;
        Dict.Store( filename );           
      finally
        Dict.Free;
      end;
    end;

  procedure EvaluateTexts( Family : string );
    var
      count : integer;
      i     : integer;
    begin
      count := TheClassStorage.ClassCount[Family];
      for i := 0 to pred(count) do
        try
          TMetaInstance(TheClassStorage.ClassByIdx[Family, i]).EvaluateTexts;
        except
        end;
    end;

end.



