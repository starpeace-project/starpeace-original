unit ColorTableMgr;

// Color Translation Tables Manager. Copyright (c) 1998 Jorge Romero Gomez, Merchise.

interface

  uses
    Windows, SysUtils,
    Gdi, ColorTrans, MemUtils, ListUtils;

  // TPaletteInfo

  type
    TTransTableStates = ( tsMixMatrixValid, tsHiColorTableValid, tsHiColorUnpackTableValid, tsTrueColorTableValid );
    TTransTableState  = set of TTransTableStates;

  type
    TPaletteInfo = class;

    FColorFilter = function( Palette : TPaletteInfo; Data : array of const ) : PRgbPalette;

    TPaletteInfo =
      class
        protected
          fCount              : integer;
          fRgbPalette         : PRgbPalette;
          fMixMatrix          : PColorMixMatrix;
          fHiColorTable       : PColorTransTableHi;
          fHiColorUnpackTable : PColorTransTableTrue;
          fTrueColorTable     : PColorTransTableTrue;
          fOwned              : boolean;
          fState              : TTransTableState;

          procedure Release;

        public
          property Count              : integer               read fCount;
          property RgbPalette         : PRgbPalette           read fRgbPalette;
          property Owned              : boolean               read fOwned      write fOwned default false;
          property State              : TTransTableState      read fState;
          property MixMatrix          : PColorMixMatrix       read fMixMatrix;
          property HiColorTable       : PColorTransTableHi    read fHiColorTable;
          property HiColorUnpackTable : PColorTransTableTrue  read fHiColorUnpackTable;
          property TrueColorTable     : PColorTransTableTrue  read fTrueColorTable;

          constructor Create;
          destructor  Destroy;                                                                                                 override;

          procedure AttachPalette( const aRgbPalette : PRgbPalette; aCount : integer );
          procedure RequiredState( const ReqState : TTransTableState );
          procedure UpdateTables;

        public
          procedure FilterPalette( Filter : FColorFilter; Data : array of const );
          function  FilteredPalette( Filter : FColorFilter; Data : array of const ) : TPaletteInfo;
      end;

  // TFilterManager

  type
    TFilterManager =
      class
        protected
          fFilters : TPointerHash;

          function GetFilter( const Name : string ) : FColorFilter;
          function GetFilterIndex( const Name : string ) : integer;
          function GetFilterByIndex( Indx : integer ) : FColorFilter;

        public
          constructor Create;                                                        virtual;
          destructor  Destroy;                                                       override;

          procedure Register( const Name : string; aFilter : FColorFilter );

        public
          property Filter[ const Name : string ] : FColorFilter  read GetFilter; default;
          property FilterIndex[ const Name : string ] : integer  read GetFilterIndex;
          property FilterByIndex[ Indx : integer ] : FColorFilter read GetFilterByIndex;
      end;

  // TPaletteInfoManager

  type
    TPaletteInfoManager =
      class
        protected
          fPaletteData : TObjectList;

        public
          property Items : TObjectList read fPaletteData;

          constructor Create;
          destructor  Destroy;                                                                                                 override;

          function FindByPalette( const Value : PRgbPalette ) : TPaletteInfo;
          function AddPalette( const Value : PRgbPalette; aCount : integer ) : TPaletteInfo;
      end;

implementation

  // TFilterManager

  constructor TFilterManager.Create;
    begin
      inherited;

      fFilters := TPointerHash.Create;
    end;

  destructor TFilterManager.Destroy;
    begin
      fFilters.Free;

      inherited;
    end;

  function TFilterManager.GetFilterIndex( const Name : string ) : integer;
    begin
      Result := fFilters.IndexOf( Name );
      assert( Result >= 0, 'Filter ''' + Name + ''' has not been registered in ColorTableMgr.TFilterManager.GetFilterIndex!!' );
    end;

  function TFilterManager.GetFilterByIndex( Indx : integer ) : FColorFilter;
    begin
      Result := fFilters.Values[Indx];
      assert( Assigned( Result ), 'Filter ' + IntToStr( Indx ) + ' has not been registered in ColorTableMgr.TFilterManager.GetFilter!!' );
    end;

  function TFilterManager.GetFilter( const Name : string ) : FColorFilter;
    begin
      Result := fFilters[Name];
      assert( Assigned( Result ), 'Filter ''' + Name + ''' has not been registered in ColorTableMgr.TFilterManager.GetFilter!!' );
    end;

  procedure TFilterManager.Register( const Name : string; aFilter : FColorFilter );
    begin
      fFilters[ Name ] := pointer( @aFilter );
    end;

  // TPaletteInfo

  procedure TPaletteInfo.FilterPalette( Filter : FColorFilter; Data : array of const );
    var
      tRgbPalette : PRgbPalette;
      bakOwned    : boolean;
    begin
      tRgbPalette := Filter( Self, Data );
      try
        bakOwned := Owned;
        Owned    := false;
        Release;
        Move( tRgbPalette^, fRgbPalette^, Count * sizeof( fRgbPalette[0] ) );
        Owned    := bakOwned;
      finally
        freemem( tRgbPalette );
      end;
    end;

  function TPaletteInfo.FilteredPalette( Filter : FColorFilter; Data : array of const ) : TPaletteInfo;
    var
      tRgbPalette : PRgbPalette;
    begin
      tRgbPalette := Filter( Self, Data );

      Result := TPaletteInfo.Create;
      Result.AttachPalette( tRgbPalette, Count );
      Result.Owned := true;
    end;

  procedure TPaletteInfo.Release;
    begin
      FreePtr( fMixMatrix );
      FreePtr( fHiColorTable );
      FreePtr( fHiColorUnpackTable );
      FreePtr( fTrueColorTable );
      if Owned
        then FreePtr( fRgbPalette );

      fState := [];
    end;

  constructor TPaletteInfo.Create;
    begin
      inherited;
    end;

  destructor TPaletteInfo.Destroy;
    begin
      Release;

      inherited;
    end;

  procedure TPaletteInfo.AttachPalette( const aRgbPalette : PRgbPalette; aCount : integer );
    begin
      Release;
      fRgbPalette := aRgbPalette;
      fCount      := aCount;
    end;

  procedure TPaletteInfo.RequiredState( const ReqState : TTransTableState );
    var
      NeededState : TTransTableState;
    begin
      NeededState := ReqState - State;
      if NeededState <> []
        then
          begin
            assert( Assigned( fRgbPalette ), 'Palette not assigned yet in ColorTableMgr.TPaletteInfo.RequiredState!!' );
            if (tsHiColorUnpackTableValid in NeededState) and (fHiColorUnpackTable = nil)
              then fHiColorUnpackTable := ColorTrans.Tab8toHiColorUnpacked( RgbPalette^, Count, 0, 0, 0 );
            if (tsHiColorTableValid in NeededState) and (fHiColorTable = nil)
              then fHiColorTable := ColorTrans.Tab8toHiColor( RgbPalette^, Count, 0, 0, 0 );
            if (tsTrueColorTableValid in NeededState) and (fTrueColorTable = nil)
              then fTrueColorTable := ColorTrans.Tab8toTrueColor( RgbPalette^, Count, 0, 0, 0 );
            fState := fState + NeededState;
          end;
    end;

  procedure TPaletteInfo.UpdateTables;
    begin
      if (tsHiColorUnpackTableValid in fState) and (fHiColorUnpackTable <> nil)
        then ColorTrans.Tab8toHiColorUnpackedInPlace( RgbPalette^, Count, fHiColorUnpackTable^, 0, 0, 0 );
      if (tsHiColorTableValid in fState) and (fHiColorTable <> nil)
        then ColorTrans.Tab8toHiColorInPlace( RgbPalette^, Count, fHiColorTable^, 0, 0, 0 );
      if (tsTrueColorTableValid in fState) and (fTrueColorTable <> nil)
        then ColorTrans.Tab8toTrueColorInPlace( RgbPalette^, Count, fTrueColorTable^, 0, 0, 0 );
    end;

  // TPaletteInfoManager

  constructor TPaletteInfoManager.Create;
    begin
      inherited;

      fPaletteData := TObjectList.Create;
    end;

  destructor TPaletteInfoManager.Destroy;
    begin
      fPaletteData.Free;

      inherited;
    end;

  function TPaletteInfoManager.FindByPalette( const Value : PRgbPalette ) : TPaletteInfo;
    var
      i : integer;
    begin
      with Items do
        begin
          i := 0;
          while ( i < Count ) and ( TPaletteInfo( Items[i] ).RgbPalette <> Value ) do
            inc( i );
          if i < Count
            then Result := TPaletteInfo( Items[i] )
            else Result := nil;
        end;
    end;

  function TPaletteInfoManager.AddPalette( const Value : PRgbPalette; aCount : integer ) : TPaletteInfo;
    begin
      Result := FindByPalette( Value );
      if not Assigned( Result )
        then
          begin
            Result := TPaletteInfo.Create;
            Result.AttachPalette( Value, aCount );
            Items.Add( Result );
          end;

    end;

(*
  procedure TBufferPalette.CreateMixTable( Alpha : integer );
    var
      RgbEntries : TRgbPalette;
    begin
      with Palette do
        begin
          LogToRgbEntries( 0, NumberOfEntries, Entries, RgbEntries );
          MixTable       := MixColors( RgbEntries, NumberOfEntries, Alpha );
          fMixTableOwned := true;
        end;
    end;
*)

end.
