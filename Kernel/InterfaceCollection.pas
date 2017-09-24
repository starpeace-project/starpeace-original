unit InterfaceCollection;

interface

  uses
    Windows, Collection;

  type
    TInterfaceCollection =
      class(TInterfacedObject)
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fInterfaces : TLockableCollection;
        private
          procedure ReleaseItem(index : integer);
        public
          procedure lock;
          procedure unlock;
          procedure Insert(Item : IUnknown);
          procedure Delete(Item : IUnknown);
          function  IndexOf(Item : IUnknown) : integer;
        private
          function  GetCount : integer;
          function  GetInterface(index : integer) : IUnknown;
          procedure SetInterface(index : integer; Item : IUnknown);
        public
          property Count : integer read GetCount;
          property Interfaces[index : integer] : IUnknown read GetInterface write SetInterface; default;
      end;

implementation

  // TInterfaceCollection

  constructor TInterfaceCollection.Create;
    begin
      inherited;
      fInterfaces := TLockableCollection.Create(0, rkUse);
    end;

  destructor TInterfaceCollection.Destroy;
    var
      i : integer;
    begin
      for i := 0 to pred(Count) do
        ReleaseItem(i);
      inherited;
    end;

  procedure TInterfaceCollection.ReleaseItem(index : integer);
    var
      p    : TObject;
      Item : IUnknown absolute p;
    begin
      p := fInterfaces[index];
      if p <> nil
        then Item._Release;
    end;

  procedure TInterfaceCollection.lock;
    begin
      fInterfaces.lock;
    end;

  procedure TInterfaceCollection.unlock;
    begin
      fInterfaces.unlock;
    end;

  procedure TInterfaceCollection.Insert(Item : IUnknown);
    var
      p : TObject absolute Item;
    begin
      lock;
      try
        fInterfaces.Insert(p);
        Item._AddRef;
      finally
        unlock;
      end;
    end;

  procedure TInterfaceCollection.Delete(Item : IUnknown);
    var
      index : integer;
    begin
      lock;
      try
        index := IndexOf(Item);
        if index <> -1
          then
            begin
              ReleaseItem(index);
              fInterfaces.AtDelete(index);
            end;
      finally
        unlock;
      end;
    end;

  function TInterfaceCollection.IndexOf(Item : IUnknown) : integer;
    var
      p : TObject absolute Item;
    begin
      lock;
      try
        result := fInterfaces.IndexOf(p);
      finally
        unlock;
      end;
    end;

  function  TInterfaceCollection.GetCount : integer;
    begin
      result := fInterfaces.Count;
    end;

  function TInterfaceCollection.GetInterface(index : integer) : IUnknown;
    var
      p : TObject absolute result;
    begin
      lock;
      try
        p := fInterfaces[index];
        result._AddRef;
      finally
        unlock;
      end;
    end;

  procedure TInterfaceCollection.SetInterface(index : integer; Item : IUnknown);
    var
      p : TObject absolute Item;
    begin
      lock;
      try
        ReleaseItem(index);
        fInterfaces[index] := p;
        Item._AddRef;
      finally
        unlock;
      end;
    end;


end.
