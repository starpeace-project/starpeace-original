unit PlayerFlic;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
    SpeedBmp, PlayerAnim, Flics, CodingFlic;

  type
    TFlicPlayer =
      class( TAnimPlayer )
        private
          fFlicDecoder : TFlicDecoder;

        protected
          procedure ResetPalette;                                                           override;

          procedure InitPlayer;                                                             override;
          procedure DonePlayer;                                                             override;

          procedure SetPaintFlags( Value : TPaintFlags );                                   override;
          procedure SetAutoDelay( UseAutoDelay : boolean );                                 override;
          procedure SetStartingFrame( aStartingFrame : integer );                           override;
          procedure SetEndingFrame( aEndingFrame : integer );                               override;
          function  GetStartingFrame : integer;                                             override;
          function  GetEndingFrame : integer;                                               override;
          function  GetKeyframed : boolean;                                                 override;
          function  GetAnimFrame : integer;                                                 override;
          function  GetAnimFrameCount : integer;                                            override;

        public
          property FlicDecoder : TFlicDecoder read fFlicDecoder;

          constructor Create( AOwner : TComponent );                                        override;

          procedure LoadFromStream( aStream : TStream );                                    override;
          procedure SeekFrame( Index : integer );                                           override;
          procedure Next;                                                                   override;
          function  Empty : boolean;                                                        override;
          class function TimerResolution : integer;                                         override;

        published
          property TimerInterval;
          property AutoDelay;

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
//        property Keyframed;
          property PlayFrom;
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
    WinUtils, GDI, ColorSpaces, Dibs, DibDraw, FlicPlayback,
    MemUtils, NumUtils, ViewUtils;

  {$R *.DCR}

  // Flic palette manipulation

  threadvar
    fCurrentPlayer : TFlicPlayer;

  function PlayColorChunks( Chunk : pointer; const FliSize : TPoint; Buffer : pointer; BufferWidth : integer ) : pointer;
    var
      ChunkColor   : ^TFliChunkColor absolute Chunk;
      ColorPacket  : ^TColorPacket;
    var
      i, j         : integer;
      ColorCount   : integer;
      CurrentColor : integer;
      FirstChanged : integer;
      LastChanged  : integer;
    begin
      Result := pchar(Chunk) + PFliChunk(Chunk).Size;
      if (ChunkColor.Magic = idColor) or (ChunkColor.Magic = idColor256)
        then
          with fCurrentPlayer, AnimBuffer do
            begin
              FirstChanged := MaxInt;
              LastChanged  := 0;

              ColorPacket  := @ChunkColor.Packets;
              CurrentColor := 0;
              for i := 0 to ChunkColor.Count - 1 do
                begin
                  with ColorPacket^ do
                    begin
                      inc( CurrentColor, Skip );

                      if Count = 0 // Count = 0 means all colors..
                        then ColorCount := 256
                        else ColorCount := Count;

                      if FirstChanged > CurrentColor
                        then FirstChanged := CurrentColor;
                      if LastChanged < CurrentColor + ColorCount - 1
                        then LastChanged := CurrentColor + ColorCount - 1;

                      for j := 0 to ColorCount - 1 do
                        if not ( (CurrentColor + j) in LockedPalEntries )
                          then
                            with RgbEntries[CurrentColor + j], Rgb[j] do
                              begin
                                if ChunkColor.Magic = idColor
                                  then
                                    begin
                                      rgbRed   := r * 4;
                                      rgbGreen := g * 4;
                                      rgbBlue  := b * 4;
                                    end
                                  else
                                    begin
                                      rgbRed   := r;
                                      rgbGreen := g;
                                      rgbBlue  := b;
                                    end;
                              end;
                    end;
                  Inc( CurrentColor, ColorCount );
                  Inc( pchar(ColorPacket), sizeof(ColorPacket^) + sizeof( ColorPacket.Rgb[0] ) * ( ColorCount - 1 ) );
                end;

              if FirstChanged <= LastChanged
                then ChangePaletteEntries( FirstChanged, LastChanged - FirstChanged + 1, RgbEntries^ );
            end;
    end;

  // TFlicPlayer

  constructor TFlicPlayer.Create( AOwner : TComponent );
    begin
      inherited;

      PlayFrom := pfAuto;
    end;

  function TFlicPlayer.Empty : boolean;
    begin
      Result := FlicDecoder = nil;
    end;

  const
    LoadInMemoryThreshold = 512 * 1024;

  procedure TFlicPlayer.InitPlayer;
    var
      bakAutoDelay : boolean;
      newInterval  : integer;
    begin
      inherited;

      with fAnimRect do
        begin
          Left   := 0;
          Top    := 0;
          Right  := FlicDecoder.Width;
          Bottom := FlicDecoder.Height;
        end;

      FlicDecoder.UnkChunkPlayer := PlayColorChunks;
      FlicDecoder.OnFinished     := Finished;

      if ( PlayFrom = pfMemory )
         or ( (PlayFrom = pfAuto) and (FlicDecoder.DiskSize < LoadInMemoryThreshold) )
        then FlicDecoder.Stream.LoadInMemory; // Load the whole flic to memory

      fAnimBuffer := TSpeedBitmap.CreateSized( AnimWidth, -AnimHeight, 8 );
      with AnimBuffer do
        begin
          FlicDecoder.AttachToDib( 0, 0, DibHeader, ScanLines );
          if pfIgnorePalette in PaintFlags
            then IgnorePalette := true;
          if pfUseIdentityPalette in PaintFlags
            then ForcePaletteIdentity( true, true );
        end;

      bakAutoDelay := AutoDelay;
      if AutoDelay
        then newInterval := FlicDecoder.FrameDelay[0] // All frames in Flic play at the same speed
        else newInterval := TimerInterval;
      TimerInterval := newInterval;                   // SetTimerInterval disables AutoDelay,
      fAutoDelay    := bakAutoDelay;                  //   so we must restore it..

      Changed;
      Next;
    end;

 procedure TFlicPlayer.DonePlayer;
   begin
     inherited;
     FreeObject( fFlicDecoder );
   end;

  class function TFlicPlayer.TimerResolution : integer;
    begin
      Result := 15;
    end;

  procedure TFlicPlayer.SetPaintFlags( Value : TPaintFlags );
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

  procedure TFlicPlayer.LoadFromStream( aStream : TStream );
    begin
      DonePlayer;

      fFlicDecoder := TFlicDecoder.Create;
      try
        FlicDecoder.LoadFromStream( aStream );
        InitPlayer;
      except
        DonePlayer;
      end;
    end;

  procedure TFlicPlayer.SetAutoDelay( UseAutoDelay : boolean );
    begin
      if UseAutoDelay <> AutoDelay
        then
          begin
            if UseAutoDelay and Assigned( FlicDecoder )
              then TimerInterval := FlicDecoder.FrameDelay[0];
            fAutoDelay := UseAutoDelay;
          end;
    end;

  procedure TFlicPlayer.Next;
    var
      bakDontUpdateScreen : boolean;
    begin
      try
        fCurrentPlayer := Self;
        with FlicDecoder do
          begin
            if PFliFrame( CurrentFrame ).Chunks > 0
              then
                begin
                  ProcessFrame;
                  inherited;
                end
              else     // If no chunks, then we don't have to repaint
                begin
                  bakDontUpdateScreen := DontUpdateScreen;
                  DontUpdateScreen    := true;
                  inherited;
                  DontUpdateScreen    := bakDontUpdateScreen;
                end;

            NextFrame;
          end;

      except
        DonePlayer; // !!!
        FatalException;
      end;
    end;

  procedure TFlicPlayer.SeekFrame( Index : integer );
    begin
      if Assigned( FlicDecoder )
        then FlicDecoder.SeekFrame( Index );
      inherited;
    end;

  function TFlicPlayer.GetAnimFrame : integer;
    begin
      if Assigned( FlicDecoder )
        then Result := FlicDecoder.CurrentFrameIndx
        else Result := 0;
    end;

  function TFlicPlayer.GetAnimFrameCount : integer;
    begin
      if Assigned( FlicDecoder )
        then Result := FlicDecoder.FrameCount
        else Result := 0;
    end;

  function  TFlicPlayer.GetKeyframed : boolean;
    begin
      Result := false;
    end;

  procedure TFlicPlayer.SetStartingFrame( aStartingFrame : integer );
    begin
      if Assigned( FlicDecoder )
        then FlicDecoder.StartingFrame := aStartingFrame;
      inherited;
    end;

  procedure TFlicPlayer.SetEndingFrame( aEndingFrame : integer );
    begin
      if Assigned( FlicDecoder )
        then FlicDecoder.EndingFrame := aEndingFrame;
      inherited;
    end;

  function  TFlicPlayer.GetStartingFrame : integer;
    begin
      if Assigned( FlicDecoder )
        then Result := FlicDecoder.StartingFrame
        else Result := 1;
    end;

  function  TFlicPlayer.GetEndingFrame : integer;
    begin
      if Assigned( FlicDecoder )
        then Result := FlicDecoder.EndingFrame
        else Result := -1;
    end;

  procedure TFlicPlayer.ResetPalette;
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
      RegisterComponents( 'Merchise', [TFlicPlayer] );
    end;

end.
