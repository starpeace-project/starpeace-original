unit SpeedImage;

// Copyright (c) 1997 Jorge Romero Gomez, Merchise.

interface

  uses
    Windows, Messages, Graphics, Controls, Forms, Classes,
    BufferColors, CanvasBmp, SpeedBmp;

  type
    TSpeedImage =
      class( TGraphicControl )
        private
          fPicture            : TSpeedBitmap;
          fOnProgress         : TProgressEvent;
          fAutoSize           : boolean;
          fCenter             : boolean;
          fIncrementalDisplay : boolean;
          fDrawing            : boolean;
          fStretch            : boolean;
          fTile               : boolean;
          fColorTint          : TColor;
          fColorBase          : TColor;

          function  GetCanvas : TBufferCanvas;
          procedure SetAutoSize( Value : boolean );
          procedure SetCenter( Value : boolean );
          procedure SetPicture( Value : TSpeedBitmap );
          procedure SetStretch( Value : boolean );
          procedure SetColorTint( Value : TColor );
          procedure SetColorBase( Value : TColor );
          procedure SetTile( Value : boolean );
          procedure PictureChanged( Sender : TObject );

        protected
          function  DestRect : TRect;
          function  GetPalette : HPALETTE;                                                                     override;
          procedure Paint;                                                                                     override;
          procedure Tint;

          function  DoPaletteChange : boolean;
          procedure Progress( Sender : TObject; Stage : TProgressStage;
                              PercentDone : byte; RedrawNow : boolean; const R : TRect; const Msg : string );  dynamic;

          procedure Loaded;                                                                                    override;

        public
          constructor Create( AOwner : TComponent );                                                           override;
          destructor  Destroy;                                                                                 override;

          procedure SetBounds( aLeft, aTop, aWidth, aHeight : integer );                                       override;

          property Canvas : TBufferCanvas read GetCanvas;

        published
          property IncrementalDisplay : boolean read fIncrementalDisplay write fIncrementalDisplay default false;
          property AutoSize : boolean           read fAutoSize           write SetAutoSize         default false;
          property Center : boolean             read fCenter             write SetCenter           default true;
          property Stretch : boolean            read fStretch            write SetStretch          default false;
          property Tile : boolean               read fTile               write SetTile             default false;
          property Picture : TSpeedBitmap       read fPicture            write SetPicture;
          property OnProgress : TProgressEvent  read fOnProgress         write fOnProgress;
          property ColorTint : TColor           read fColorTint          write SetColorTint        default clNone;
          property ColorBase : TColor           read fColorBase          write SetColorBase        default clNone;

        published
          property DragCursor;
          property Align;
          property DragMode;
          property Enabled;
          property ParentShowHint;
          property PopupMenu;
          property ShowHint;
          property Visible;
          property OnClick;
          property OnDblClick;
          property OnDragDrop;
          property OnDragOver;
          property OnEndDrag;
          property OnMouseDown;
          property OnMouseMove;
          property OnMouseUp;
          property OnStartDrag;
      end;

  procedure Register;

implementation

  {$R *.DCR}

  // SpeedImage

  constructor TSpeedImage.Create( AOwner : TComponent );
    begin
      inherited;

      fPicture            := TSpeedBitmap.Create;
      fPicture.OnChange   := PictureChanged;
      fPicture.OnProgress := Progress;
      fCenter             := true;
      fColorTint          := clNone;
      fColorBase          := clNone;
      Height              := 32;
      Width               := 32;
      ControlStyle        := ControlStyle + [csReplicatable];
    end;

  destructor TSpeedImage.Destroy;
    begin
      fPicture.Free;

      inherited;
    end;

  procedure TSpeedImage.Loaded;
    begin
      inherited;

      if (ColorTint <> clNone) and Assigned( Picture ) and not Picture.Empty and
         not (csDesigning in ComponentState)
        then Tint;
    end;

  procedure TSpeedImage.Tint;
    begin
      BitmapTint( Picture, ColorTint, ColorBase );
      PictureChanged( Self );
    end;

  procedure TSpeedImage.SetColorTint( Value : TColor );
    begin
      // Just change tint color, later we may add more code:
      fColorTint := Value;
    end;

  procedure TSpeedImage.SetColorBase( Value : TColor );
    begin
      // Just change base color, later we may add more code:
      fColorBase := Value;
    end;

  procedure TSpeedImage.SetTile( Value : boolean );
    begin
      if Value <> fTile
        then
          begin
            fTile := Value;
            PictureChanged( Self );
          end;
    end;

  function TSpeedImage.GetPalette : HPALETTE;
    begin
      if Assigned( fPicture )
        then Result := fPicture.Palette
        else Result := 0;
    end;

  function TSpeedImage.DestRect : TRect;
    begin
      if Stretch or Tile
        then Result := ClientRect
        else
          if Center
            then Result := Bounds( ( Width - Picture.Width ) div 2, ( Height - Picture.Height ) div 2,
                                   Picture.Width, Picture.Height )
            else Result := Rect( 0, 0, Picture.Width, Picture.Height );
    end;

  procedure TSpeedImage.Paint;
    var
      Save : boolean;
    begin
      if csDesigning in ComponentState
        then
          with inherited Canvas do
            begin
              Pen.Style   := psDash;
              Brush.Style := bsClear;
              Rectangle( 0, 0, Width, Height );
            end;
      Save     := fDrawing;
      fDrawing := true;
      try
        if not Picture.Empty
          then
            if not Tile
              then Picture.StretchDraw( inherited Canvas, DestRect )
              else Picture.Tile( inherited Canvas, 0, 0, DestRect );
      finally
        fDrawing := Save;
      end;
    end;

  function TSpeedImage.DoPaletteChange : boolean;
    var
      ParentForm : TCustomForm;
    begin
      Result := false;
      if Visible and ( not ( csLoading in ComponentState ) )
         and Assigned( Picture ) and ( Picture.PaletteModified )
        then
          begin
            if ( Picture.Palette = 0 )
              then Picture.PaletteModified := false
              else
                begin
                  ParentForm := GetParentForm( Self );
                  if Assigned( ParentForm ) and ParentForm.Active and Parentform.HandleAllocated
                    then
                      begin
                        if fDrawing
                          then ParentForm.Perform( WM_QueryNewPalette, 0, 0 )
                          else PostMessage( ParentForm.Handle, WM_QueryNewPalette, 0, 0 );
                        Picture.PaletteModified := false;
                        Result := true;
                      end;
                end;
          end;
    end;

  procedure TSpeedImage.Progress( Sender : TObject; Stage : TProgressStage;
                                  PercentDone : byte; RedrawNow : boolean; const R : TRect; const Msg : string );
    begin
      if fIncrementalDisplay and RedrawNow
        then
          if DoPaletteChange
            then Update
            else Paint;
      if Assigned( fOnProgress )
        then fOnProgress( Sender, Stage, PercentDone, RedrawNow, R, Msg );
    end;

  function TSpeedImage.GetCanvas : TBufferCanvas;
    begin
      Result := Picture.Canvas;
    end;

  procedure TSpeedImage.SetAutoSize( Value : boolean );
    begin
      fAutoSize := Value;
      PictureChanged( Self );
    end;

  procedure TSpeedImage.SetCenter( Value : boolean );
    begin
      if fCenter <> Value
        then
          begin
            fCenter := Value;
            PictureChanged( Self );
          end;
    end;

  procedure TSpeedImage.SetPicture( Value : TSpeedBitmap );
    begin
      if not Assigned( Picture )
        then fPicture := TSpeedBitmap.Create;
      fPicture.Assign( Value );
    end;

  procedure TSpeedImage.SetStretch( Value : boolean );
    begin
      if Value <> fStretch
        then
          begin
            fStretch := Value;
            PictureChanged( Self );
          end;
    end;

  procedure TSpeedImage.SetBounds( aLeft, aTop, aWidth, aHeight : integer );
    begin
      if (aWidth = Width) or (aHeight = Height)
        then inherited
        else
          begin
            inherited;
            fAutoSize := false;
            PictureChanged( Self );
          end;
    end;

  procedure TSpeedImage.PictureChanged( Sender : TObject );
    begin
      if AutoSize and not Picture.Empty 
        then inherited SetBounds( Left, Top, Picture.Width, Picture.Height );
      if Assigned( Picture ) and ( not Picture.Empty ) 
        then
          begin
            if ( not Picture.Transparent ) and ( Stretch or ( Picture.Width >= Width )
              and ( Picture.Height >= Height ) )
              then ControlStyle := ControlStyle + [csOpaque]
              else ControlStyle := ControlStyle - [csOpaque];
            if DoPaletteChange and fDrawing
              then Update;
          end
        else
          ControlStyle := ControlStyle - [csOpaque];
      if not fDrawing
        then Invalidate;
    end;

  procedure Register;
    begin
      RegisterComponents( 'Merchise', [TSpeedImage] );
    end;

end.
