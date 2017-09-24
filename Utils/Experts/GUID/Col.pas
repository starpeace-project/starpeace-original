unit Col;

interface

  uses
    Classes;

  const
    NoIndex = -1;

  type
    TRelationshipKind = (rkUse, rkBelonguer);

  type
    CCollection = class of TCollection;
    PCollection = ^TCollection;
    TCollection =
      class
        public
          constructor Create( aLimit, aDelta : integer; aRelKind : TRelationshipKind );
          destructor  Destroy; override;
        public
          procedure Insert ( Item : TObject );                    virtual;
          procedure Delete ( Item : TObject );                    virtual;
          procedure Extract( Item : TObject );                    virtual;
          procedure AtInsert ( Index : integer; Item : TObject ); virtual;
          procedure AtDelete ( Index : integer );                 virtual;
          procedure AtExtract( Index : integer );                 virtual;
          function  IndexOf( Item : TObject ) : integer;          virtual;
          procedure InsertColl( Coll : TCollection );
          procedure DeleteAll;                                    virtual;
          procedure ExtractAll;                                   virtual;
        protected
          function  GetItem( index : integer ) : TObject;         virtual;
          procedure SetItem( index : integer; Item : TObject );   virtual;
          function  GetCount : integer;                           virtual;
        public
          procedure Sort( Compare : TListSortCompare );           virtual;
        public
          property Items[index : integer] : TObject read GetItem write SetItem; default;
          property Count : integer read GetCount;
        private
          fList    : TList;
          fRelKind : TRelationshipKind;
      end;

implementation


  { TCollection }

  constructor TCollection.Create( aLimit, aDelta : integer; aRelKind : TRelationshipKind );
    begin
      inherited Create;
      fList    := TList.Create;
      fRelKind := aRelKind;
    end;

  destructor TCollection.Destroy;
    var
      i : integer;
    begin
      if fRelKind = rkBelonguer
        then
          for i := 0 to pred(fList.Count) do
            Items[i].Free;
      fList.Free;
      inherited Destroy;
    end;

  procedure TCollection.Insert( Item : TObject );
    begin
      AtInsert( fList.Count, Item );
    end;

  procedure TCollection.Delete( Item : TObject );
    var
      Idx : integer;
    begin
      Idx := IndexOf( Item );
      if Idx >= 0
        then AtDelete( Idx );
    end;

  procedure TCollection.Extract( Item : TObject );
    var
      Idx : integer;
    begin
      Idx := IndexOf( Item );
      if Idx >= 0
        then AtExtract( Idx );
    end;

  procedure TCollection.AtInsert( Index : integer; Item : TObject );
    begin
      if fList.Capacity = fList.Count
        then fList.Expand;
      if Index < fList.Count
        then fList.Insert( Index, Item )
        else fList.Add( Item );
    end;

  procedure TCollection.AtDelete( Index : integer );
    begin
      if fRelKind = rkBelonguer
        then TObject(fList[Index]).Free;
      fList.Delete( Index );
      fList.Pack;
    end;

  procedure TCollection.AtExtract( Index : integer );
    begin
      fList.Delete( Index );
      fList.Pack;
    end;

  function TCollection.IndexOf( Item : TObject ) : integer;
    begin
      result := fList.IndexOf( Item );
    end;

  procedure TCollection.InsertColl( Coll : TCollection );
    var
      i : integer;
    begin
      for i := 0 to pred(Coll.Count) do
        Insert( Coll[i] );
    end;

  procedure TCollection.DeleteAll;
    begin
      if fRelKind = rkBelonguer
        then
          while fList.Count > 0 do
            AtDelete( 0 )
        else fList.Clear;
    end;

  procedure TCollection.ExtractAll;
    begin
      fList.Clear;
    end;

  procedure TCollection.Sort( Compare : TListSortCompare );
    begin
      fList.Sort( Compare );
    end;

  function TCollection.GetItem( index : integer ) : TObject;
    begin
      result := fList.Items[index]
    end;

  procedure TCollection.SetItem( index : integer; Item : TObject );
    begin
      fList.Items[index] := Item;
    end;

  function TCollection.GetCount : integer;
    begin
      result := fList.Count;
    end;

end.
