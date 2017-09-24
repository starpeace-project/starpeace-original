unit RDOObjectRegistry;

interface

  uses
    Classes;

  const
    NoId = 0;

  type
    TRegistryEntries = TList;

  type
    TRDOObjectsRegistry = class
      private
        fRegistryEntries : TRegistryEntries;
        procedure CleanUpRegistryEntries;
      public
        constructor Create;
        destructor  Destroy; override;
      public
        procedure RegisterObject( ObjectName : string; ObjectId : integer );
        function  GetObjectId( ObjectName : string ) : integer;
    end;

implementation

  type
    TRegistryEntry =
      record
        ObjectName : string;
        ObjectId   : integer;
      end;

    PRegistryEntry = ^TRegistryEntry;

  constructor TRDOObjectsRegistry.Create;
    begin
      inherited Create;
      fRegistryEntries := TRegistryEntries.Create;
    end;

  destructor TRDOObjectsRegistry.Destroy;
    begin
      CleanUpRegistryEntries;
      fRegistryEntries.Free;
      inherited Destroy
    end;

  procedure TRDOObjectsRegistry.RegisterObject( ObjectName : string; ObjectId : integer );
    var
      NewRegEntry : PRegistryEntry;
    begin
      New( NewRegEntry );
      NewRegEntry.ObjectName := ObjectName;
      NewRegEntry.ObjectId := ObjectId;
      fRegistryEntries.Add( NewRegEntry )
    end;

  function TRDOObjectsRegistry.GetObjectId( ObjectName : string ) : integer;
    var
      RegEntryIdx : integer;
    begin
      RegEntryIdx := 0;
      with fRegistryEntries do
        begin
          while ( RegEntryIdx < Count ) and ( ObjectName <> PRegistryEntry( Items[ RegEntryIdx ] ).ObjectName ) do
            inc( RegEntryIdx );
          if RegEntryIdx < Count
            then
              Result := PRegistryEntry( Items[ RegEntryIdx ] ).ObjectId
            else
              Result := NoId
        end
    end;

  procedure TRDOObjectsRegistry.CleanUpRegistryEntries;
    var
      RegEntryIdx : integer;
    begin
      with fRegistryEntries do
        for RegEntryIdx := 0 to Count - 1 do
          Dispose( PRegistryEntry( Items[ RegEntryIdx ] ) );
    end;

end.



