unit PlayerGif;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
    SpeedBmp, PlayerAnim, CodingGif;

  type
    TGifPlayer =
      class( TAnimPlayer )
        private
          fGifDecoder  : TGifDecoder;
          fTickCounter : integer;

        protected
          procedure ResetPalette;                                                           override;

          procedure InitPlayer;                                                             override;
          procedure DonePlayer;                                                             override;

          procedure SetPaintFlags( Value : TPaintFlags );                                   override;
          procedure SetStartingFrame( aStartingFrame : integer );                           override;
          procedure SetEndingFrame( aEndingFrame : integer );                               override;
          function  GetStartingFrame : integer;                                             override;
          function  GetEndingFrame : integer;                                               override;
          function  GetKeyframed : boolean;                                                 override;
          function  GetAnimFrame : integer;                                                 override;
          function  GetAnimFrameCount : integer;                                            override;

        public
          property GifDecoder : TGifDecoder read fGifDecoder;

          constructor Create( AOwner : TComponent );                                        override;

          procedure TimerTick;                                                              override;
          procedure LoadFromStream( aStream : TStream );                                    override;
          procedure SeekFrame( Index : integer );                                           override;
          procedure Next;                                                                   override;
          function  Empty : boolean;                                                        override;
          class function TimerResolution : integer;                                         override;

        published
          property OnFramePlayed;
          property OnFinished;
          property OnFrameCountPlayed;
          property OnFatalException;

          property AutoSize;
          property Paused;
          property Center;
          property Stretch;
          property Loop;
          property Filename;
          property StartingFrame;
          property EndingFrame;
          property ShareTimer;
          property PaintFlags;

        published
          property Align;
          property Visible;
          property Enabled;
          property ParentShowHint;
          property ShowHint;
          property PopupMenu;

          property OnClick;
          property OnDblClick;
          property OnMouseDown;
          property OnMouseMove;
          property OnMouseUp;

          property DragMode;
          property DragCursor;
          property OnDragDrop;
          property OnDragOver;
          property OnEndDrag;
          property OnStartDrag;
        end;

  // VCL Registration

  procedure Register;

implementation

  uses
    WinUtils, GDI, ColorSpaces, Dibs, FlicPlayback,
    MemUtils, NumUtils, ViewUtils;

  {$R *.DCR}

  // TGifPlayer

  procedure TGifPlayer.TimerTick;
    begin
      inc( fTickCounter, fTimerInterval );
      if fTickCounter >= fGifDecoder.FrameDelay[AnimFrame]
        then
          begin
            fTickCounter := 0;
            inherited;
          end;
    end;

  constructor TGifPlayer.Create( AOwner : TComponent );
    begin
      inherited;

      PlayFrom := pfAuto;
    end;

  function TGifPlayer.Empty : boolean;
    begin
      Result := ( GifDecoder = nil );
    end;

  const
    LoadInMemoryThreshold = 512 * 1024;

  procedure TGifPlayer.InitPlayer;
    var
      i, newInterval : integer;
    begin
      inherited;

      with fAnimRect do
        begin
          Left := 0;
          Top  := 0;
          Right  := GifDecoder.Width;
          Bottom := GifDecoder.Height;
        end;

      GifDecoder.OnFinished := Finished;
      newInterval := GifDecoder.FrameDelay[1];
      for i := 2 to GifDecoder.FrameCount do
        NewInterval := mcd( NewInterval, GifDecoder.FrameDelay[i] );
      if newInterval > 0
        then TimerInterval := newInterval;

      fAnimBuffer := TSpeedBitmap.CreateSized( AnimWidth, -AnimHeight, 8 );
      with AnimBuffer do
        begin
          GifDecoder.AttachToDib( 0, 0, DibHeader, ScanLines );
          ChangePaletteEntries( 0, GifDecoder.fDibHeader.biClrUsed, DibColors( GifDecoder.fDibHeader )^ );
          if pfIgnorePalette in PaintFlags
            then IgnorePalette := true;
          if pfUseIdentityPalette in PaintFlags
            then ForcePaletteIdentity( true, true );
        end;

      Changed;
      Next;
    end;

 procedure TGifPlayer.DonePlayer;
   begin
     inherited;
     FreeObject( fGifDecoder );
   end;

  class function TGifPlayer.TimerResolution : integer;
    begin
      Result := 15;
    end;

  procedure TGifPlayer.SetPaintFlags( Value : TPaintFlags );
    var
      FullUpdate : boolean;
    begin
      inherited;

      FullUpdate := pfFullUpdate in Value;
      if FullUpdate <> (pfFullUpdate in PaintFlags)
        then
          begin
            if FullUpdate
              then Include( fPaintFlags, pfFullUpdate )
              else Exclude( fPaintFlags, pfFullUpdate );
          end;
    end;

  procedure TGifPlayer.LoadFromStream( aStream : TStream );
    begin
      DonePlayer;

      fGifDecoder := TGifDecoder.Create;
      try
        GifDecoder.LoadFromStream( aStream );
        InitPlayer;
      except
        DonePlayer;
      end;
    end;

  procedure TGifPlayer.Next;
    begin
      try
        with GifDecoder do
          begin
            ProcessFrame;
            inherited;
            NextFrame;
          end;

      except
        DonePlayer;
        FatalException;
      end;
    end;

  procedure TGifPlayer.SetStartingFrame( aStartingFrame : integer );
    begin
      if Assigned( GifDecoder )
        then GifDecoder.StartingFrame := aStartingFrame;
      inherited;
    end;

  procedure TGifPlayer.SetEndingFrame( aEndingFrame : integer );
    begin
      if Assigned( GifDecoder )
        then GifDecoder.EndingFrame := aEndingFrame;
      inherited;
    end;

  procedure TGifPlayer.SeekFrame( Index : integer );
    begin
      if Assigned( GifDecoder )
        then GifDecoder.SeekFrame( Index );
      inherited;
    end;

  function TGifPlayer.GetAnimFrame : integer;
    begin
      if Assigned( GifDecoder )
        then Result := GifDecoder.CurrentFrameIndx
        else Result := 0;
    end;

  function TGifPlayer.GetAnimFrameCount : integer;
    begin
      if Assigned( GifDecoder )
        then Result := GifDecoder.FrameCount
        else Result := 0;
    end;

  function  TGifPlayer.GetKeyframed : boolean;
    begin
      Result := false;
    end;

  function  TGifPlayer.GetStartingFrame : integer;
    begin
      if Assigned( GifDecoder )
        then Result := GifDecoder.StartingFrame
        else Result := 1;
    end;

  function  TGifPlayer.GetEndingFrame : integer;
    begin
      if Assigned( GifDecoder )
        then Result := GifDecoder.EndingFrame
        else Result := -1;
    end;

  procedure TGifPlayer.ResetPalette;
    var
      bakCurrentFrame : integer;
    begin
      bakCurrentFrame := AnimFrame;
      SeekFrame( 1 );                    // Make sure we replay the palette chunk
      SeekFrame( bakCurrentFrame );
    end;

  // Registration

  procedure Register;
    begin
      RegisterComponents( 'Merchise', [TGifPlayer] );
    end;

end.
