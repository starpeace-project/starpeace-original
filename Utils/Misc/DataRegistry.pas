unit DataRegistry;

interface

  uses
    Classes, MapStringToObject;

  type
    TDataRegistry = class;

    TDataRegistry =
      class
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fRegistry : TMapStringToObject; // TStringList;
        public
          procedure Add(const aKey : string; Obj : TObject);
          procedure Remove(const  aKey : string);
        private
          function  GetObject(const aKey : string) : TObject;
        public
          property Objects[const aKey : string] : TObject read GetObject; default;
      end;


implementation

  // TDataRegistry

  constructor TDataRegistry.Create;
    begin
      inherited Create;
      fRegistry := TMapStringToObject.Create(mmOwn); //TStringList.Create;
      {
      with fRegistry do
        begin
          Sorted := true;
          Duplicates := dupError;
        end;
      }
    end;

  destructor TDataRegistry.Destroy;
    {var
      i : integer;}
    begin
      {for i := 0 to pred(fRegistry.Count) do
        fRegistry.Objects[i].Free;}
      fRegistry.Free;
      inherited;
    end;

  procedure TDataRegistry.Add(const aKey : string; Obj : TObject);
    begin
      try
        fRegistry[aKey] := Obj; //fRegistry.AddObject(aKey, Obj);
      except
        Obj.Free;
        raise;
      end;
    end;

  procedure TDataRegistry.Remove(const aKey : string);
    {var
      index : integer;}
    begin
      {index := fRegistry.IndexOf(aKey);
      if index <> -1
        then
          begin
            fRegistry.Objects[index].Free;
            fRegistry.Delete(index);
          end;}
      fRegistry.Remove(aKey);
    end;

  function TDataRegistry.GetObject(const aKey : string) : TObject;
    {var
      index : integer;}
    begin
      result := fRegistry[aKey];
      {index := fRegistry.IndexOf(aKey);
      if index <> -1
        then result := fRegistry.Objects[index]
        else result := nil;}
    end;

end.
