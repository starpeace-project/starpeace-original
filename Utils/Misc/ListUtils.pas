unit ListUtils;

// Copyright (c) 1998 Jorge Romero Gomez, Merchise.

interface

  uses
    SysUtils, MemUtils;

  type
    PPointerArray = ^TPointerArray;
    TPointerArray = array[0..65535] of pointer;

  type
    TUnknownList =
      class
        protected
          fList       : PPointerArray;
          fCount      : integer;
          fCapacity   : integer;
          fHoles      : integer;
          fItemsOwned : boolean;
          fAutoPack   : boolean;

          procedure SetCapacity( Value : integer );                                  virtual;
          procedure ReallocItems( OldCount, NewCount : integer );                    virtual;
          procedure MoveItems( OldIndx, NewIndx, Count : integer );                  virtual;

        public
          constructor Create;                                                        virtual;
          destructor  Destroy;                                                       override;

          procedure Delete( Indx : integer );                                        virtual; abstract;
          procedure Move( OldIndx, NewIndx : integer );                              virtual; abstract;
          procedure Exchange( Indx1, Indx2 : integer );                              virtual;
          procedure Clear;
          procedure Pack;                                                            virtual;
          procedure Grow;
          procedure AssertCapacity( Value : integer );
          function  FindFrom( Indx : integer; Value : pointer ) : integer;
          function  SkipFrom( Indx : integer; Value : pointer ) : integer;

          property Capacity : integer    read fCapacity    write SetCapacity;
          property Count : integer       read fCount;
          property List : PPointerArray  read fList;
          property AutoPack : boolean    read fAutoPack    write fAutoPack   default false;
          property ItemsOwned : boolean  read fItemsOwned  write fItemsOwned default true;
      end;

  type
    TIntegerList =
      class( TUnknownList )
        protected
          function  GetItem( Indx : integer ) : integer;
          procedure SetItem( Indx : integer; Value : integer );

        public
          property Items[ Indx : integer ] : integer read GetItem write SetItem; default;

          procedure Pack;                                                            override;
          procedure Move( OldIndx, NewIndx : integer );                              override;
          procedure Delete( Indx : integer );                                        override;
          procedure Remove( Value : integer );
          function  Add( Value : integer ) : integer;
          procedure Insert( Indx : integer; Value : integer );
          function  IndexOf( Value : integer ) : integer;
      end;

  type
    TPointerList =
      class( TUnknownList )
        protected
          function  GetItem( Indx : integer ) : pointer;
          procedure SetItem( Indx : integer; Value : pointer );

        public
          property Items[ Indx : integer ] : pointer read GetItem write SetItem; default;

          procedure Move( OldIndx, NewIndx : integer );                              override;
          procedure Delete( Indx : integer );                                        override;
          procedure Remove( Value : pointer );
          function  Add( Value : pointer ) : integer;
          procedure Insert( Indx : integer; Value : pointer );
          function  IndexOf( Value : pointer ) : integer;
      end;

  type
    TObjectList =
      class( TUnknownList )
        protected
          function  GetItem( Indx : integer ) : TObject;
          procedure SetItem( Indx : integer; Value : TObject );

        public
          property Items[ Indx : integer ] : TObject read GetItem write SetItem; default;

          procedure Move( OldIndx, NewIndx : integer );                              override;
          procedure Delete( Indx : integer );                                        override;
          procedure Remove( Value : TObject );
          function  Add( Value : TObject ) : integer;
          procedure Insert( Indx : integer; Value : TObject );
          function  IndexOf( Value : TObject ) : integer;
      end;

  type
    TInterfaceList =
      class( TUnknownList )
        protected
          function  GetItem( Indx : integer ) : IUnknown;
          procedure SetItem( Indx : integer; const Value : IUnknown );

        public
          property Items[ Indx : integer ] : IUnknown read GetItem write SetItem; default;

          procedure Move( OldIndx, NewIndx : integer );                              override;
          procedure Delete( Indx : integer );                                        override;
          procedure Remove( const Value : IUnknown );
          function  Add( const Value : IUnknown ) : integer;
          procedure Insert( Indx : integer; const Value : IUnknown );
          function  IndexOf( const Value : IUnknown ) : integer;
      end;

  type
    TStrList =
      class( TUnknownList )
        protected
          function  GetItem( Indx : integer ) : string;
          procedure SetItem( Indx : integer; const Value : string );

        public
          property Items[ Indx : integer ] : string read GetItem write SetItem; default;

          procedure Move( OldIndx, NewIndx : integer );                              override;
          procedure Delete( Indx : integer );                                        override;
          procedure Remove( const Value : string );
          function  Add( const Value : string ) : integer;
          procedure Insert( Indx : integer; const Value : string );
          function  IndexOf( const Value : string ) : integer;
      end;

  // Hashed Lists

  type
    THashedList =
      class( TUnknownList )
        protected
          fValues : PPointerArray;

          procedure ReallocItems( OldCount, NewCount : integer );                    override;
          procedure MoveItems( OldIndx, NewIndx, Count : integer );                  override;

          function  GetName( Indx : integer ) : string;
          procedure SetName( Indx : integer; const Value : string );

        public
          property  Names[ Indx : integer ] : string read GetName write SetName;
          procedure Move( OldIndx, NewIndx : integer );                              override;

          constructor Create;                                                        override;
          destructor  Destroy;                                                       override;

          function  IndexOf( const Name : string ) : integer;
          procedure Remove( const Name : string );                                   virtual;
      end;

    TPointerHash =
      class( THashedList )
        protected
          function  GetItem( const Name : string ) : pointer;
          procedure SetItem( const Name : string; Value : pointer );
          function  GetValue( Indx : integer ) : pointer;
          procedure SetValue( Indx : integer; Value : pointer );

        public
          property  Items[ const Name : string ] : pointer   read GetItem  write SetItem; default;
          property  Values[ Indx : integer ] : pointer       read GetValue write SetValue;

          procedure Delete( Indx : integer );                                        override;

          procedure RemoveValue( Value : pointer );
          function  Add( const Name : string; Value : pointer ) : integer;
      end;

    TObjectHash =
      class( THashedList )
        protected
          function  GetItem( const Name : string ) : TObject;
          procedure SetItem( const Name : string; Value : TObject );
          function  GetValue( Indx : integer ) : TObject;
          procedure SetValue( Indx : integer; Value : TObject );

        public
          property  Items[ const Name : string ] : TObject   read GetItem  write SetItem; default;
          property  Values[ Indx : integer ] : TObject       read GetValue write SetValue;

          procedure Delete( Indx : integer );                                        override;

          procedure RemoveValue( Value : TObject );
          function  Add( const Name : string; Value : TObject ) : integer;
      end;

implementation

  // TUnknownList

  procedure TUnknownList.AssertCapacity( Value : integer );
    var
      NewCap : integer;
    begin
      if Value > Capacity
        then
          begin
            if AutoPack and ( fHoles > Capacity div 4 ) and ( fHoles > 10 )
              then Pack;
            if Capacity > 64
              then NewCap := Capacity + Capacity div 4
              else
                if Capacity > 8
                  then NewCap := Capacity + 16
                  else NewCap := Capacity + 4;
            if NewCap < Value
              then SetCapacity( Value )
              else SetCapacity( NewCap );
          end;
    end;

  procedure TUnknownList.ReallocItems( OldCount, NewCount : integer );
    var
      i       : integer;
      NewSize : integer;
    begin
      NewSize := NewCount * sizeof( fList[0] );
      if OldCount = 0
        then
          begin
            GetMem( fList, NewSize );
            fillchar( fList^, NewSize, 0 );
          end
        else
          if NewCount > OldCount
            then
              begin
                ReallocMem( fList, NewSize );
                fillchar( fList[OldCount], NewSize - sizeof( fList[0] ) * OldCount, 0 );
              end
            else
              begin
                for i := OldCount downto NewCount do
                  Delete( i );
                ReallocMem( fList, NewSize );
              end;
    end;

  procedure TUnknownList.SetCapacity( Value : integer );
    begin
      if Value <> Capacity
        then
          begin
            if fList = nil
              then ReallocItems( 0, Value )
              else ReallocItems( Capacity, Value );
            fCapacity := Value;
          end;
    end;

  procedure TUnknownList.Grow;
    begin
      AssertCapacity( Capacity + 1 );
    end;

  function TUnknownList.FindFrom( Indx : integer; Value : pointer ) : integer;
    begin
      Result := Indx;
      while ( fList[Result] <> Value ) and ( Result < Count ) do
        inc( Result );
    end;

  function TUnknownList.SkipFrom( Indx : integer; Value : pointer ) : integer;
    begin
      Result := Indx;
      while ( fList[Result] = Value ) and ( Result < Count ) do
        inc( Result );
    end;

  procedure TUnknownList.Exchange( Indx1, Indx2 : integer );
    var
      Tmp : pointer;
    begin
      Tmp          := fList[Indx1];
      fList[Indx1] := fList[Indx2];
      fList[Indx2] := Tmp;
    end;

  procedure TUnknownList.Clear;
    var
      i       : integer;
      PackBak : boolean;
    begin
      PackBak  := AutoPack;
      AutoPack := false;
      for i := Count - 1 downto 0 do
        Delete( i );
      AutoPack := PackBak;
    end;

  constructor TUnknownList.Create;
    begin
      inherited;

      fItemsOwned := true;
    end;

  destructor TUnknownList.Destroy;
    begin
      Clear;
      FreePtr( fList );

      inherited;
    end;

  procedure TUnknownList.MoveItems( OldIndx, NewIndx, Count : integer );
    begin
      System.Move( fList[OldIndx], fList[NewIndx], Count * sizeof( fList[0] ) );
    end;

  procedure TUnknownList.Pack;
    var
      First, Last, Next, ItemCount : integer;
    begin
      if fHoles <> 0
        then
          begin // Do something
            if fHoles < fCount
              then
                begin
                  Next  := 0;
                  First := FindFrom( 0, nil );
                  repeat
                    Next := FindFrom( Next, nil );
                    Last := SkipFrom( Next, nil );
                    if Last < Count
                      then
                        begin
                          Next      := FindFrom( Last, nil ) - 1;
                          ItemCount := Next - Last + 1;
                          MoveItems( Last, First, ItemCount );
                          inc( First, ItemCount );
                        end
                      else Next := Last;
                  until Next >= Count - 1;
                  fCount := First;
                end
              else fCount := 0;
            fHoles := 0;
          end;
    end;

  // TPointerList

  procedure TPointerList.Move( OldIndx, NewIndx : integer );
    begin
      if Assigned( fList[NewIndx] ) and ( NewIndx < OldIndx ) // Adjust the shift that occurs if item is
        then inc( OldIndx );                                  // inserted before the
      Insert( NewIndx, Items[OldIndx] );
      Delete( OldIndx );
    end;

  function TPointerList.Add( Value : pointer ) : integer;
    begin
      AssertCapacity( Count + 1 );
      Result       := Count;
      fList[Count] := Value;
      inc( fCount );
    end;

  procedure TPointerList.Delete( Indx : integer );
    begin
      assert( (Indx >= 0) and (Indx < Count), 'Invalid index in TPointerList.Delete' );
      if ItemsOwned
        then FreePtr( fList[Indx] );
      if Indx < fCount - 1
        then inc( fHoles )
        else dec( fCount );
    end;

  function TPointerList.GetItem( Indx : integer ) : pointer;
    begin
      assert( (Indx >= 0) and (Indx < Capacity), 'Indx out of range in ListUtils.TPointerList.GetItem' );
      Result := fList[Indx];
    end;

  procedure TPointerList.SetItem( Indx : integer; Value : pointer );
    begin
      assert( (Indx >= 0) and (Indx < Capacity), 'Indx out of range in ListUtils.TPointerList.SetItem' );
      if Count < Indx
        then fCount := Indx;
      fList[Indx] := Value;
    end;

  procedure TPointerList.Remove( Value : pointer );
    begin
      Delete( IndexOf( Value ) );
    end;

  procedure TPointerList.Insert( Indx : integer; Value : pointer );
    begin
      assert( (Indx >= 0) and (Indx <= Count), 'Invalid index in TPointerList.Insert' );
      if fList[Indx] = nil
        then Items[Indx] := Value
        else
          begin
            AssertCapacity( Count + 1 ); // Make sure there's space for one more
            System.Move( fList[Indx + 1], fList[Indx], ( Count - Indx + 1 ) * sizeof( fList[0] ) );
          end;
    end;

  function TPointerList.IndexOf( Value : pointer ) : integer;
    var
      i : integer;
    begin
      i := 0;
      while ( i < Count ) and (Items[i] <> Value) do
        inc( i );
      if i < Count
        then Result := i
        else Result := -1;
    end;

  // TObjectList

  procedure TObjectList.Move( OldIndx, NewIndx : integer );
    begin
      if Assigned( fList[NewIndx] ) and ( NewIndx < OldIndx ) // Adjust the shift that occurs if item is
        then inc( OldIndx );                                  // inserted before the
      Insert( NewIndx, Items[OldIndx] );
      Delete( OldIndx );
    end;

  function TObjectList.Add( Value : TObject ) : integer;
    begin
      AssertCapacity( Count + 1 );
      Result       := Count;
      fList[Count] := Value;
      inc( fCount );
    end;

  procedure TObjectList.Delete( Indx : integer );
    begin
      assert( (Indx >= 0) and (Indx < Count), 'Invalid index in TObjectList.Delete' );
      if ItemsOwned
        then TObject( fList[Indx] ).Free;
      if Indx < fCount - 1
        then inc( fHoles )
        else dec( fCount );
    end;

  function TObjectList.GetItem( Indx : integer ) : TObject;
    begin
      assert( (Indx >= 0) and (Indx < Capacity), 'Indx out of range in ListUtils.TObjectList.GetItem' );
      Result := fList[Indx];
    end;

  procedure TObjectList.SetItem( Indx : integer; Value : TObject );
    begin
      assert( (Indx >= 0) and (Indx < Capacity), 'Indx out of range in ListUtils.TObjectList.SetItem' );
      if Count < Indx
        then fCount := Indx;
      fList[Indx] := Value;
    end;

  procedure TObjectList.Remove( Value : TObject );
    begin
      Delete( IndexOf( Value ) );
    end;

  procedure TObjectList.Insert( Indx : integer; Value : TObject );
    begin
      assert( (Indx >= 0) and (Indx <= Count), 'Invalid index in TObjectList.Insert' );
      if fList[Indx] = nil
        then Items[Indx] := Value
        else
          begin
            AssertCapacity( Count + 1 ); // Make sure there's space for one more
            System.Move( fList[Indx + 1], fList[Indx], ( Count - Indx + 1 ) * sizeof( fList[0] ) );
          end;
    end;

  function TObjectList.IndexOf( Value : TObject ) : integer;
    var
      i : integer;
    begin
      i := 0;
      while ( i < Count ) and (Items[i] <> Value) do
        inc( i );
      if i < Count
        then Result := i
        else Result := -1;
    end;

  // TStrList

  procedure TStrList.Move( OldIndx, NewIndx : integer );
    begin
      if Assigned( fList[NewIndx] ) and ( NewIndx < OldIndx ) // Adjust the shift that occurs if item is
        then inc( OldIndx );                                  // inserted before the
      Insert( NewIndx, Items[OldIndx] );
      Delete( OldIndx );
    end;

  function TStrList.Add( const Value : string ) : integer;
    begin
      AssertCapacity( Count + 1 );
      Result                 := Count;
      string( fList[Count] ) := Value;
      inc( fCount );
    end;

  procedure TStrList.Delete( Indx : integer );
    begin
      assert( (Indx >= 0) and (Indx < Count), 'Invalid index in TStrList.Delete' );
      if ItemsOwned
        then string( fList[Indx] ) := '';
      if Indx < fCount - 1
        then inc( fHoles )
        else dec( fCount );
    end;

  function TStrList.GetItem( Indx : integer ) : string;
    begin
      assert( (Indx >= 0) and (Indx < Capacity), 'Indx out of range in ListUtils.TStrList.GetItem' );
      Result := string( fList[Indx] );
    end;

  procedure TStrList.SetItem( Indx : integer; const Value : string );
    begin
      assert( (Indx >= 0) and (Indx < Capacity), 'Indx out of range in ListUtils.TStrList.SetItem' );
      if Count <= Indx
        then fCount := Indx + 1;
      string( fList[Indx] ) := Value;
    end;

  procedure TStrList.Remove( const Value : string );
    begin
      Delete( IndexOf( Value ) );
    end;

  procedure TStrList.Insert( Indx : integer; const Value : string );
    begin
      assert( (Indx >= 0) and (Indx <= Count), 'Invalid index in TStrList.Insert' );
      if fList[Indx] = nil
        then Items[Indx] := Value
        else
          begin
            AssertCapacity( Count + 1 ); // Make sure there's space for one more
            System.Move( fList[Indx + 1], fList[Indx], ( Count - Indx + 1 ) * sizeof( fList[0] ) );
          end;
    end;

  function TStrList.IndexOf( const Value : string ) : integer;
    var
      i : integer;
    begin
      i := 0;
      while ( i < Count ) and (Items[i] <> Value) do
        inc( i );
      if i < Count
        then Result := i
        else Result := -1;
    end;

  // TInterfaceList

  procedure TInterfaceList.Move( OldIndx, NewIndx : integer );
    begin
      if Assigned( fList[NewIndx] ) and ( NewIndx < OldIndx ) // Adjust the shift that occurs if item is
        then inc( OldIndx );                                  // inserted before the
      Insert( NewIndx, Items[OldIndx] );
      Delete( OldIndx );
    end;

  function TInterfaceList.Add( const Value : IUnknown ) : integer;
    begin
      AssertCapacity( Count + 1 );
      Result                    := Count;
      IUnknown( fList[Count]  ) := Value;
      inc( fCount );
    end;

  procedure TInterfaceList.Delete( Indx : integer );
    begin
      assert( (Indx >= 0) and (Indx < Count), 'Invalid index in TInterfaceList.Delete' );
      if ItemsOwned
        then IUnknown( fList[Indx] ) := nil;
      if Indx < fCount - 1
        then inc( fHoles )
        else dec( fCount );
    end;

  function TInterfaceList.GetItem( Indx : integer ) : IUnknown;
    begin
      assert( (Indx >= 0) and (Indx < Capacity), 'Indx out of range in ListUtils.TInterfaceList.GetItem' );
      Result := IUnknown( fList[Indx] );
    end;

  procedure TInterfaceList.SetItem( Indx : integer; const Value : IUnknown );
    begin
      assert( (Indx >= 0) and (Indx < Capacity), 'Indx out of range in ListUtils.TInterfaceList.SetItem' );
      if Count < Indx
        then fCount := Indx;
      IUnknown( fList[Indx] ) := Value;
    end;

  procedure TInterfaceList.Remove( const Value : IUnknown );
    begin
      Delete( IndexOf( Value ) );
    end;

  procedure TInterfaceList.Insert( Indx : integer; const Value : IUnknown );
    begin
      assert( (Indx >= 0) and (Indx <= Count), 'Invalid index in TInterfaceList.Insert' );
      if fList[Indx] = nil
        then Items[Indx] := Value
        else
          begin
            AssertCapacity( Count + 1 ); // Make sure there's space for one more
            System.Move( fList[Indx + 1], fList[Indx], ( Count - Indx + 1 ) * sizeof( fList[0] ) );
          end;
    end;

  function TInterfaceList.IndexOf( const Value : IUnknown ) : integer;
    var
      i : integer;
    begin
      i := 0;
      while ( i < Count ) and (Items[i] <> Value) do
        inc( i );
      if i < Count
        then Result := i
        else Result := -1;
    end;

  // TIntegerList

  procedure TIntegerList.Move( OldIndx, NewIndx : integer );
    begin
      if Assigned( fList[NewIndx] ) and ( NewIndx < OldIndx ) // Adjust the shift that occurs if item is
        then inc( OldIndx );                                  // inserted before the
      Insert( NewIndx, Items[OldIndx] );
      Delete( OldIndx );
    end;

  procedure TIntegerList.Pack;
    begin
      // Do nothing
    end;
    
  function TIntegerList.Add( Value : integer ) : integer;
    begin
      AssertCapacity( Count + 1 );
      Result       := Count;
      fList[Count] := pointer( Value );
      inc( fCount );
    end;

  procedure TIntegerList.Delete( Indx : integer );
    begin
      assert( (Indx >= 0) and (Indx < Count), 'Invalid index in TIntegerList.Delete' );
      if Indx < fCount - 1
        then System.Move( fList[Indx + 1], fList[Indx], ( Count - Indx - 1 ) * sizeof( fList[0] ) );
      dec( fCount );
    end;

  function TIntegerList.GetItem( Indx : integer ) : integer;
    begin
      assert( (Indx >= 0) and (Indx < Capacity), 'Indx out of range in ListUtils.TIntegerList.GetItem' );
      Result := integer( fList[Indx] );
    end;

  procedure TIntegerList.SetItem( Indx : integer; Value : integer );
    begin
      assert( (Indx >= 0) and (Indx < Capacity), 'Indx out of range in ListUtils.TIntegerList.SetItem' );
      if Count < Indx
        then fCount := Indx;
      fList[Indx] := pointer( Value );
    end;

  procedure TIntegerList.Remove( Value : integer );
    begin
      Delete( IndexOf( Value ) );
    end;

  procedure TIntegerList.Insert( Indx : integer; Value : integer );
    begin
      assert( (Indx >= 0) and (Indx <= Count), 'Invalid index in TIntegerList.Insert' );
      if fList[Indx] = nil
        then Items[Indx] := Value
        else
          begin
            AssertCapacity( Count + 1 ); // Make sure there's space for one more
            System.Move( fList[Indx + 1], fList[Indx], ( Count - Indx + 1 ) * sizeof( fList[0] ) );
          end;
    end;

  function TIntegerList.IndexOf( Value : integer ) : integer;
    var
      i : integer;
    begin
      i := 0;
      while ( i < Count ) and (Items[i] <> Value) do
        inc( i );
      if i < Count
        then Result := i
        else Result := -1;
    end;

  // THashList

  procedure THashedList.Move( OldIndx, NewIndx : integer );
    begin
      raise Exception.Create( 'I know this is a patch, but can''t call THashedList.Move!!' );
    end;

  constructor THashedList.Create;
    begin

    end;

  destructor THashedList.Destroy;
    begin

    end;

  procedure THashedList.ReallocItems( OldCount, NewCount : integer );
    begin

    end;

  procedure THashedList.MoveItems( OldIndx, NewIndx, Count : integer );
    begin
      System.Move( fList[OldIndx], fList[NewIndx], Count * sizeof( fList[0] ) );
      System.Move( fList[OldIndx], fList[NewIndx], Count * sizeof( fList[0] ) );
    end;

  function THashedList.GetName( Indx : integer ) : string;
    begin
      assert( (Indx >= 0) and (Indx < Capacity), 'Indx out of range in ListUtils.THashList.GetName' );
      Result := string( fList[Indx] );
    end;

  procedure THashedList.SetName( Indx : integer; const Value : string );
    begin
      assert( (Indx >= 0) and (Indx < Capacity), 'Indx out of range in ListUtils.THashList.SetName' );
      if Count <= Indx
        then fCount := Indx + 1;
      string( fList[Indx] ) := Value;
    end;

  procedure THashedList.Remove( const Name : string );
    begin
      Delete( IndexOf( Name ) );
    end;

  function THashedList.IndexOf( const Name : string ) : integer;
    var
      i : integer;
    begin
      i := 0;
      while ( i < Count ) and (Name[i] <> Name) do
        inc( i );
      if i < Count
        then Result := i
        else Result := -1;
    end;

  // TPointerHash

  function TPointerHash.GetValue( Indx : integer ) : pointer;
    begin
      Result := nil;
    end;

  procedure TPointerHash.SetValue( Indx : integer; Value : pointer );
    begin

    end;

  function TPointerHash.GetItem( const Name : string ) : pointer;
    begin
      Result := nil;
    end;

  procedure TPointerHash.SetItem( const Name : string; Value : pointer );
    begin

    end;

  procedure TPointerHash.RemoveValue( Value : pointer );
    begin

    end;

  function TPointerHash.Add( const Name : string; Value : pointer ) : integer;
    begin
      AssertCapacity( Count + 1 );
      Result                 := Count;
      string( fList[Count] ) := Name;
      fValues[Count]         := Value;
      inc( fCount );
    end;

  procedure TPointerHash.Delete( Indx : integer );
    begin
      assert( (Indx >= 0) and (Indx < Count), 'Invalid index in TPointerHash.Delete' );
      if ItemsOwned
        then string( fList[Indx] ) := '';
      if Indx < fCount - 1
        then inc( fHoles )
        else dec( fCount );
    end;

  // TObjectHash

  function TObjectHash.GetValue( Indx : integer ) : TObject;
    begin
      Result := nil;
    end;

  procedure TObjectHash.SetValue( Indx : integer; Value : TObject );
    begin

    end;

  function TObjectHash.GetItem( const Name : string ) : TObject;
    begin
      Result := nil;
    end;

  procedure TObjectHash.SetItem( const Name : string; Value : TObject );
    begin

    end;

  procedure TObjectHash.RemoveValue( Value : TObject );
    begin

    end;

  function TObjectHash.Add( const Name : string; Value : TObject ) : integer;
    begin
      AssertCapacity( Count + 1 );
      Result                 := Count;
      string( fList[Count] ) := Name;
      fValues[Count]         := Value;
      inc( fCount );
    end;

  procedure TObjectHash.Delete( Indx : integer );
    begin
      assert( (Indx >= 0) and (Indx < Count), 'Invalid index in ListUtils.TObjectHash.Delete' );
      if ItemsOwned
        then string( fList[Indx] ) := '';
      if Indx < fCount - 1
        then inc( fHoles )
        else dec( fCount );
    end;

end.

