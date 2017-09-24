unit Palettes;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

// Check NumberOfEntries, so tasks don't work on non-used entries

interface

  uses
    SysUtils, Classes, Windows,
    MemUtils, GDI, ColorTrans;

  type
    TBufferPaletteTask = class;

    // TBufferPalette

    CBufferPalette = class of TBufferPalette;
    TBufferPalette =
      class( TPersistent )
        protected
          fPalette       : T256LogPalette;
          fOriginal      : T256PalEntries;
          fHandle        : HPALETTE;

          function  GetHandle : HPALETTE;                                                       virtual;
          procedure SetHandle( aHandle : HPALETTE );                                            virtual;

        public
          property Palette  : T256LogPalette read fPalette;
          property Original : T256PalEntries read fOriginal;
          property Handle   : HPALETTE       read GetHandle      write SetHandle;

        public
          destructor  Destroy;                                                                  override;
          constructor Create;                                                                   virtual;

          procedure   Assign( Source : TPersistent );                                           override;

          procedure   Release;
          procedure   HandleNeeded;
          function    ReleaseHandle : HPALETTE;

          constructor CreateFromHandle( Handle : HPALETTE );                                    virtual;
          constructor CreateFromLogPalette( const LogEntries; Count : integer );                virtual;
          constructor CreateFromRgbPalette( const RgbQuads; Count : integer );                  virtual;

          procedure   ForceIdentity;                                                            virtual;
          procedure   ReserveEntriesForAnimation( Indx, Count : integer; Reserved : boolean );  virtual;

          procedure   AssignLogEntries( Indx, Count : integer; const LogEntries );              virtual;
          procedure   AssignRgbEntries( Indx, Count : integer; const RgbQuads );              virtual;

        public
          procedure   DoStep;                                                                   virtual;

        public
          procedure   SaveToFile( const FileName : string );                                    virtual;
          procedure   LoadFromFile( const FileName : string );                                  virtual;
          procedure   SaveToStream( Stream : TStream );                                         virtual;
          procedure   LoadFromStream( Stream : TStream );                                       virtual;

          // F/Xs

          procedure   ChangeLogEntries( Indx, Count : integer; const LogEntries );              virtual;
          procedure   ChangeRgbEntries( Indx, Count : integer; const RgbQuads );              virtual;

          procedure   Restore;                                                                  virtual;
          procedure   Update;                                                                   virtual;

          function    Animating : boolean;
          procedure   Convert( const GoalLogPalette; Indx, Count : integer; Steps : integer );  virtual;
          procedure   Cycle( Reversed : boolean; Indx, Count : integer;
                             CyclingStep, Steps : integer );                                    virtual;
          procedure   FadeIn( Indx, Count : integer; Steps : integer );                         virtual;
          procedure   FadeOut( Indx, Count : integer; Steps : integer );                        virtual;
          procedure   Blackout;                                                                 virtual;

        public
          function    NearestIndex( Color : TColorRef ) : integer;                              virtual;

        protected
          Tasks : TList;

          function NewTask( TaskType : integer;
                            Indx, Count, Steps, Tag : integer; Info : pointer ) : TBufferPaletteTask;    virtual;
      end;

    // Palette F/X Tasks

    TPaletteDelta =
      record
        Red   : integer;
        Green : integer;
        Blue  : integer;
      end;

    TPaletteDeltas = array[0..255] of TPaletteDelta;

    TBufferPaletteTask =
      class
        protected
          fOwner           : TBufferPalette;
          fIndx, fCount    : integer;
          fSteps           : integer;
          fTag             : integer;

          fTaskType        : integer;
          fCurrentStep     : integer;
          fCurrent, fDelta : TPaletteDeltas;

        protected
          constructor Create( Indx, Count, Steps, Tag : integer; Owner : TBufferPalette );
          procedure   DoStep;                                                         virtual;

          procedure   Cycle;
          procedure   Convert( const GoalLogPalette );
      end;

  // Helper functions

  function SetCanvasPalette( Handle : HDC; Palette : TBufferPalette ) : HPALETTE;

implementation

  uses
    StreamUtils;

  // Helper functions

  function SetCanvasPalette( Handle : HDC; Palette : TBufferPalette ) : HPALETTE;
    begin
      Palette.Update;
      Result := SelectPalette( Handle, Palette.Handle, false );
    end;

  // Palette F/X Task Types

  const
    ttNone     =  0;
    ttCycle    = 10;
    ttConvert  = 20;

  // TBufferPalette

  procedure TBufferPalette.Release;
    var
      indx : integer;
    begin
      for indx := Tasks.Count - 1 downto 0 do
        TBufferPaletteTask( Tasks[indx] ).Free;
      Tasks.Clear;

      if fHandle <> 0
        then
          begin
            DeleteObject( fHandle );
            fHandle := 0;
          end;
    end;

  destructor TBufferPalette.Destroy;
    begin
      Release;
      Tasks.Free;

      inherited;
    end;

  constructor TBufferPalette.Create;
    begin
      inherited Create;

      with Palette do
        begin
          Version         := LogPaletteVersion;
          NumberOfEntries := 256;
        end;
      Tasks := TList.Create;
    end;

  constructor TBufferPalette.CreateFromLogPalette( const LogEntries; Count : integer );
    begin
      Create;
      with fPalette do
        begin
          assert( Count <= 256, 'Count too big in Palettes.TBufferPalette.CreateFromLogPalette!!' );

          NumberOfEntries := Count;
          Move( LogEntries, Entries, sizeof( Entries[0] )* Count );
          Move( Entries, fOriginal, sizeof( fOriginal ) );
        end;
    end;

  constructor TBufferPalette.CreateFromRgbPalette( const RgbQuads; Count : integer );
    var
      RgbPalette : TRgbPalette absolute RgbQuads;
    begin
      Create;
      with fPalette do
        begin
          NumberOfEntries := Count;
          RgbToLogEntries( 0, Count, RgbPalette, Entries );
          MarkNoCollapse( 0, Count, Entries );
          Move( Entries, fOriginal, sizeof( fOriginal ) );
        end;
    end;

  constructor TBufferPalette.CreateFromHandle( Handle : HPALETTE );
    begin
      Create;
      SetHandle( Handle );
    end;

  procedure TBufferPalette.Assign( Source : TPersistent );
    begin
      if Source is TBufferPalette
        then
          with TBufferPalette( Source ) do
            begin
              Self.Release;
              Move( fPalette, Self.fPalette, sizeof( fPalette ) + sizeof( fOriginal ) );
            end
        else inherited;
    end;

  procedure TBufferPalette.HandleNeeded;
    begin
      if fHandle <> 0
        then GetHandle;
    end;

  function TBufferPalette.ReleaseHandle : HPALETTE;
    begin
      Result  := Handle;
      fHandle := 0;
    end;

  function TBufferPalette.GetHandle : HPALETTE;
    begin
      if fHandle = 0
        then fHandle := PaletteHandle( Palette );
      Result := fHandle;
    end;

  procedure TBufferPalette.SetHandle( aHandle : HPALETTE );
    var
      First, Last, Count : integer;
    begin
      Release;
      with fPalette do
        begin
          fHandle := aHandle;

          Update;
          GetColorAvailInfo( First, Last, Count );
          MarkNoCollapse( First, Count, fOriginal );
        end;
    end;

  procedure TBufferPalette.Update;
    begin
      if fHandle <> 0
        then
          begin
            GetLogPalette( fHandle, 0, 256, fOriginal, true );
            with fPalette do
              Move( fOriginal, Entries, sizeof( Entries ) );
          end;
    end;

  procedure TBufferPalette.Restore;
    begin
      ChangeLogEntries( 0, 256, Original );
    end;

  procedure TBufferPalette.ReserveEntriesForAnimation( Indx, Count : integer; Reserved : boolean );
    begin
      if Reserved
        then MarkReserved( Indx, Count, fOriginal )
        else MarkNoCollapse( Indx, Count, fOriginal );
      Update;
      if fHandle <> 0
        then SetPaletteEntries( Handle, 0, 256, fOriginal );
    end;

  procedure TBufferPalette.ForceIdentity;
    var
      ScreenDC : HDC;
    begin
      if AvailableColors < 256
        then
          begin
            with fPalette do
              begin
                ScreenDC := GetDC( 0 );
                GetSystemPaletteEntries( ScreenDC, 0, FirstAvailColor, Entries );
                GetSystemPaletteEntries( ScreenDC, succ( LastAvailColor ), FirstAvailColor, Entries );
                ReleaseDC( 0, ScreenDC );
                Move( Entries, fOriginal, sizeof( fOriginal ) );
              end;
            if fHandle <> 0
              then SetPaletteEntries( Handle, 0, 256, fPalette.Entries );
          end;
    end;

  procedure TBufferPalette.AssignLogEntries( Indx, Count : integer; const LogEntries );
    begin
      ChangeLogEntries( Indx, Count, LogEntries );
      if @LogEntries <> @fOriginal
        then Move( LogEntries, fOriginal[Indx], Count * sizeof( fOriginal[0] ) );
    end;

  procedure TBufferPalette.AssignRgbEntries( Indx, Count : integer; const RgbQuads );
    begin
      ChangeRgbEntries( Indx, Count, RgbQuads );
      if @RgbQuads <> @fPalette.Entries
        then Move( fPalette.Entries[Indx], fOriginal[Indx], Count * sizeof( fPalette.Entries[0] ) );
    end;

  procedure TBufferPalette.ChangeRgbEntries( Indx, Count : integer; const RgbQuads );
    var
      Rgb : TRgbPalette absolute RgbQuads;
      i   : integer;
    begin           
      assert( Count <= 256, 'Count too big in Palettes.TBufferPalette.ChangeRgbEntries!!' );

      for i := Indx to Indx + pred( Count ) do
        with fPalette.Entries[i], Rgb[i] do
          begin
            peRed   := rgbRed;
            peGreen := rgbGreen;
            peBlue  := rgbBlue;
          end;
      if fHandle <> 0
        then SetPaletteEntries( fHandle, 0, 256, fPalette.Entries );
    end;

  procedure TBufferPalette.ChangeLogEntries( Indx, Count : integer; const LogEntries );
    begin
      assert( Count <= 256, 'Count too big in Palettes.TBufferPalette.ChangeLogEntries!!' );

      if @LogEntries <> @fPalette.Entries
        then Move( LogEntries, fPalette.Entries[Indx], Count * sizeof( fPalette.Entries[0] ) );
      if fHandle <> 0
        then SetPaletteEntries( fHandle, 0, 256, fPalette.Entries );
    end;

   function TBufferPalette.NewTask( TaskType : integer; Indx, Count, Steps, Tag : integer; Info : pointer ) : TBufferPaletteTask;
    begin
      Result := TBufferPaletteTask.Create( Indx, Count, Steps, Tag, Self );
      case TaskType of
        ttCycle :
          Result.Cycle;
        ttConvert :
          Result.Convert( Info^ );
        else FreeObject( Result );
      end;
    end;

  procedure TBufferPalette.DoStep;
    var
      Indx     : integer;
      CurrTask : TBufferPaletteTask;
    begin
      for Indx := Tasks.Count - 1 downto 0 do
        begin
          CurrTask := TBufferPaletteTask( Tasks[ Indx ] );
          with CurrTask do
            begin
              DoStep;
              if fSteps <> -1
                then
                  begin
                    inc( fCurrentStep );
                    if fCurrentStep >= fSteps
                      then
                        begin
                          Tasks.Remove( CurrTask );
                          CurrTask.Free;
                        end;
                  end;
            end;
        end;
      Tasks.Pack;
    end;

  function TBufferPalette.Animating : boolean;
    begin
      Result := ( Tasks.Count > 0 );
    end;

  procedure TBufferPalette.Convert( const GoalLogPalette; Indx, Count, Steps : integer );
    begin
      assert( Count <= 256, 'Count too big in Palettes.TBufferPalette.Convert!!' );
      assert( Steps > 0, 'Invalid step count in Palettes.TBufferPalette.Convert!!' );

      Tasks.Add( NewTask( ttConvert, Indx, Count, Steps, 0, @GoalLogPalette ) );
    end;

  procedure TBufferPalette.Cycle( Reversed : boolean; Indx, Count, CyclingStep, Steps : integer );
    begin
      assert( Count <= 256, 'Count too big in Palettes.TBufferPalette.Cycle!!' );
      assert( (Steps > 0) or (Steps = -1), 'Invalid step count in Palettes.TBufferPalette.Cycle!!' );
      assert( (CyclingStep > 0), 'Invalid cycling step in Palettes.TBufferPalette.Cycle!!' );

      if Reversed
        then CyclingStep := -CyclingStep;
      Tasks.Add( NewTask( ttCycle, Indx, Count, Steps, CyclingStep, nil ) )
    end;

  procedure TBufferPalette.FadeIn( Indx, Count, Steps : integer );
    begin
      assert( Count <= 256, 'Count too big in Palettes.TBufferPalette.FadeIn!!' );
      assert( Steps > 0, 'Invalid step count in Palettes.TBufferPalette.FadeIn!!' );
      
      Convert( Original, Indx, Count, Steps );
    end;

  procedure TBufferPalette.FadeOut( Indx, Count, Steps : integer );
    begin
      assert( Count <= 256, 'Count too big in Palettes.TBufferPalette.FadeOut!!' );
      assert( Steps > 0, 'Invalid step count in Palettes.TBufferPalette.FadeOut!!' );

      Convert( LogBlackPalette.Entries, Indx, Count, Steps );
    end;

  procedure TBufferPalette.Blackout;
    begin
      ChangeLogEntries( 0, 256, LogBlackPalette.Entries );
    end;

  function TBufferPalette.NearestIndex( Color : TColorRef ) : integer;
    begin
      Result := GetNearestPaletteIndex( Handle, Color );
    end;

  procedure TBufferPalette.LoadFromStream( Stream : TStream );
    begin
      Release;
      Stream.ReadBuffer( fPalette, sizeof( fPalette ) );
      AssignLogEntries( 0, 256, fPalette.Entries );
    end;

  procedure TBufferPalette.SaveToStream( Stream : TStream );
    begin
      Stream.WriteBuffer( fPalette, sizeof( fPalette ) );
    end;

 procedure TBufferPalette.LoadFromFile( const FileName : string );
    begin
      StreamObject( TFileStream.Create( FileName, fmOpenRead or fmShareDenyWrite ), LoadFromStream );
    end;

  procedure TBufferPalette.SaveToFile( const FileName : string );
    begin
      StreamObject( TFileStream.Create( FileName, fmCreate ), SaveToStream );
    end;

  // -----------------
  // Palette F/X Tasks
  // -----------------

  constructor TBufferPaletteTask.Create( Indx, Count, Steps, Tag : integer; Owner : TBufferPalette );
    begin
      inherited Create;

      fOwner := Owner;
      fIndx  := Indx;
      fCount := Count;
      fSteps := Steps;
      fTag   := Tag;
    end;

  procedure CycleElements( Step, Count : integer; var Elements; ElementSize : integer );
    var
      Buffer : array[0..2048] of char;
    begin
      assert( ElementSize < sizeof(Buffer), 'ElementSize too big in Palettes.CycleElements!!' );
      assert( ElementSize > 0, 'ElementSize invalid in Palettes.CycleElements!!' );
      assert( Step <> 0, 'Step invalid in Palettes.CycleElements!!' );

      if Step > 0
        then
          begin
            Move( Elements, Buffer, Step * ElementSize );
            Move( ( pchar(@Elements) + Step * ElementSize )^, Elements, ( Count - Step ) * ElementSize );
            Move( Buffer, ( pchar(@Elements) + ( Count - Step ) * ElementSize )^, Step * ElementSize );
          end
        else
          begin
            Move( ( pchar(@Elements) + ( Count + Step ) * ElementSize )^, Buffer, -Step * ElementSize );
            Move( Elements, ( pchar(@Elements) - Step * ElementSize )^, ( Count + Step ) * ElementSize );
            Move( Buffer, Elements, -Step * ElementSize );
          end;
    end;

  procedure TBufferPaletteTask.DoStep;
    var
      i     : integer;
      cTask : TBufferPaletteTask;
      cFlag : boolean;
    begin
      case fTaskType of
        ttCycle :
          with fOwner, fPalette do
            begin
              cFlag := false; // Set if we are cycling and converting the same entries

              for i := 0 to Tasks.Count - 1 do
                begin
                  cTask := Tasks[i];
                  if ( cTask.fTaskType = ttConvert )
                    then
                      begin
                        assert( ( (fIndx >= cTask.fIndx) and (fIndx  + fCount <= cTask.fIndx + cTask.fCount) )
                                or ( fIndx + fCount < cTask.fIndx )
                                or ( fIndx > cTask.fIndx + cTask.fCount ),
                                'Entries are being cycled and converted in an unsupported way in Palettes.TBufferPalette.DoStep!!' );
                        if (fIndx >= cTask.fIndx) and (fIndx  + fCount <= cTask.fIndx + cTask.fCount)
                          then
                            begin
                              cFlag := true;
                              CycleElements( fTag, fCount, cTask.fDelta[fIndx], sizeof( fDelta[0] ) );
                              CycleElements( fTag, fCount, cTask.fCurrent[fIndx], sizeof( fCurrent[0] ) );
                            end;
                      end;
                end;
              if not cFlag
                then
                  begin
                    CycleElements( fTag, fCount, Entries[fIndx], sizeof( Entries[0] ) );
                    AnimatePalette( Handle, fIndx, fCount, @Entries[fIndx] );
                  end;
            end;

        ttConvert :
          with fOwner, fPalette do
            begin
              for i := fIndx to fIndx + fCount - 1 do
                with Entries[i], fCurrent[i] do
                  begin
                    inc( Red, fDelta[i].Red );
                    inc( Green, fDelta[i].Green );
                    inc( Blue, fDelta[i].Blue );

                    peRed   := Red div 1024;
                    peGreen := Green div 1024;
                    peBlue  := Blue div 1024;
                  end;
              AnimatePalette( Handle, fIndx, fCount, @Entries[fIndx] );
            end;
      end;
    end;

  procedure TBufferPaletteTask.Cycle;
    begin
      fTaskType := ttCycle;
    end;

  procedure TBufferPaletteTask.Convert( const GoalLogPalette );
    var
      Goal : T256PalEntries absolute GoalLogPalette;
      i    : integer;
    begin
      assert( ( fSteps > 0 ), 'fSteps <= 0 in Palettes.TBufferPaletteTask.Convert!!' );
      
      fTaskType := ttConvert;
      for i := fIndx to fIndx + fCount - 1 do
        begin
          with fCurrent[i], fOwner.Palette.Entries[i] do
            begin
              Red   := 1024 * peRed;
              Green := 1024 * peGreen;
              Blue  := 1024 * peBlue;
            end;
          with fDelta[i], Goal[i], fOwner.Palette do
            begin
              Red   := ( peRed   - Entries[i].peRed   ) * 1024 div fSteps;
              Green := ( peGreen - Entries[i].peGreen ) * 1024 div fSteps;
              Blue  := ( peBlue  - Entries[i].peBlue )  * 1024 div fSteps;
            end;
        end;
    end;

end.
