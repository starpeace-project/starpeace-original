unit Sprites;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  uses
    Windows, SpriteImages, Buffer;

  type
    TShownState = (stNormal, stGlass, stHidden ); // !! Esto está mal

  type
    TSprite =
      class
        public
          constructor Create;
          destructor  Destroy;                                                      override;

        public
          procedure NextFrame;                                                      virtual;
          procedure GotoFrame( Value : integer );                                   virtual;

          procedure Draw( const ClipArea : TRect; Buffer : TBuffer );               virtual; // Draw current frame
          procedure DrawNextFrame( const ClipArea : TRect; Buffer : TBuffer );      virtual; // Advance frame then draw

          procedure Glass;
          procedure Hide;
          procedure Show;

          procedure MoveTo( const Value : TPoint );

        protected
          fImage        : TSpriteImage;
          fCurrentFrame : integer;
          fOrigin       : TPoint;
          fZOrder       : integer;
          fShown        : TShownState;
          fPrevious     : TShownState;
          fStartFrame   : integer;
          fLastFrame    : integer;

          procedure SetStartFrame( Value : integer );
          procedure SetLastFrame( Value : integer );
          procedure SetZOrder( Value : integer );

        public
          property Image        : TSpriteImage read fImage;
          property Shown        : TShownState  read fShown;
          property CurrentFrame : integer      read fCurrentFrame;
          property Origin       : TPoint       read fOrigin       write MoveTo;
          property ZOrder       : integer      read fZOrder       write SetZOrder;
          property StartFrame   : integer      read fStartFrame   write SetStartFrame;
          property LastFrame    : integer      read fLastFrame    write SetLastFrame;

          procedure SetFrameRange( aStartFrame, aLastFrame : integer );
      end;

implementation

  // TSprite

  constructor TSprite.Create;
    begin
      inherited;

      fStartFrame   := 0;
      fLastFrame    := Image.FrameCount - 1;
      fCurrentFrame := StartFrame;
    end;

  destructor TSprite.Destroy;
    begin
      inherited;
    end;

  procedure TSprite.NextFrame;
    var
      Indx : integer;
    begin
      if fCurrentFrame < LastFrame
        then Indx := CurrentFrame + 1
        else Indx := StartFrame;
      GotoFrame( Indx );
    end;

  procedure TSprite.SetStartFrame( Value : integer );
    begin
      if Value >= 0
        then
          if Value < Image.FrameCount
            then fStartFrame := Value
            else fStartFrame := Image.FrameCount - 1
        else fStartFrame := 0;
    end;

  procedure TSprite.SetLastFrame( Value : integer );
    begin
      if Value >= 0
        then
          if Value < Image.FrameCount
            then fLastFrame := Value
            else fLastFrame := Image.FrameCount - 1
        else fLastFrame := 0;
    end;

  procedure TSprite.SetZOrder( Value : integer );
    begin
      fZOrder := Value;
    end;

  procedure TSprite.SetFrameRange( aStartFrame, aLastFrame : integer );
    begin
      StartFrame := aStartFrame;
      LastFrame  := aLastFrame;
    end;

  procedure TSprite.GotoFrame( Value : integer );
    begin
      if Value >= StartFrame
        then
          if Value <= LastFrame
            then fCurrentFrame := Value
            else fCurrentFrame := LastFrame
        else fCurrentFrame := StartFrame;
    end;

  procedure TSprite.Draw( const ClipArea : TRect; Buffer : TBuffer );
    begin (*
      with Origin do
        case fShown of
          stNormal :
            fImage.Draw( x, y, fCurrentFrame, ClipArea, Buffer );
          stGlass :
            fImage.DrawGlassed( x, y, fCurrentFrame, ClipArea, Buffer );
        end; *)
    end;

  procedure TSprite.Glass;
    begin
      fShown := stGlass;
    end;

  procedure TSprite.Hide;
    begin
      fPrevious := fShown;
      fShown := stHidden;
    end;

  procedure TSprite.Show;
    begin
      fShown := fPrevious;
    end;

  procedure TSprite.MoveTo( const Value : TPoint );
    begin
      Origin := Value;
    end;

  procedure TSprite.DrawNextFrame( const ClipArea : TRect; Buffer : TBuffer );
    begin
      NextFrame;
      Draw( ClipArea, Buffer );
    end;

end.
