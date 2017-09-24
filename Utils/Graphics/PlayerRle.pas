unit PlayerRle;

// Copyright (c) 1997 Jorge Romero Gomez, Merchise.

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
    SpeedBmp, PlayerAnim, Dibs, DibDraw,
    CodingRle, Flics;

  type
    TRlePlayer =
      class( TAnimPlayer )
        private
          fRleHeader : PDib;
          fRlePlayer : TRleDecoder;

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
          procedure UpdateFrame;                                                            override;

        public
          property RlePlayer : TRleDecoder read fRlePlayer;

          constructor Create( AOwner : TComponent );                                        override;

          procedure LoadFromStream( aStream : TStream );                                    override;

          function  Empty : boolean;                                                        override;
          procedure SeekFrame( Index : integer );                                           override;
          procedure Next;                                                                   override;

        published
          property TimerInterval;
          property AutoDelay;
        end;

  // VCL Registration

  procedure Register;

implementation

  uses
    WinUtils, GDI, ColorSpaces, DibRle,
    MemUtils, NumUtils, ViewUtils;

  {$R *.DCR}

  // TRlePlayer

  threadvar
    fCurrentPlayer : TRlePlayer;

  function PlayColorChunks( Chunk : pointer; const FliSize : TPoint; Buffer : pointer; BufferWidth : integer ) : pointer;
    var
      ChunkColor   : PFliChunkColor256 absolute Chunk;
      ColorPacket  : PColorPacket;
    var
      i, j         : integer;
      ColorCount   : integer;
      CurrentColor : integer;
      FirstChanged : integer;
      LastChanged  : integer;
    begin
      Result := pchar(Chunk) + PRleChunk(Chunk).Size;
      if ChunkColor.Magic = idColor256
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
                                rgbRed   := r;
                                rgbGreen := g;
                                rgbBlue  := b;
                              end;
                    end;
                  Inc( CurrentColor, ColorCount );
                  Inc( pchar(ColorPacket), sizeof(ColorPacket^) + sizeof( ColorPacket.Rgb[0] ) * ( ColorCount - 1 ) );
                end;

              if FirstChanged <= LastChanged
                then ChangePaletteEntries( FirstChanged, LastChanged - FirstChanged + 1, RgbEntries^ );
            end;
    end;

  constructor TRlePlayer.Create( AOwner : TComponent );
    begin
      inherited;

      fPlayFrom := pfAuto;
    end;

  function TRlePlayer.Empty : boolean;
    begin
      Result := RlePlayer = nil;
    end;

  const
    LoadInMemoryThreshold = 512 * 1024;

  procedure TRlePlayer.InitPlayer;
{    var
      bakAutoDelay  : boolean;
      newFrameDelay : integer;}
    begin
      inherited;

      with fAnimRect do
        begin
          Left   := 0;
          Top    := 0;
          Right  := RlePlayer.Width;
          Bottom := RlePlayer.Height;
        end;

      RlePlayer.UnkChunkPlayer := PlayColorChunks;
      RlePlayer.OnFinished     := Finished;

      if ( PlayFrom = pfMemory )
         or ( (PlayFrom = pfAuto) and (RlePlayer.DiskSize < LoadInMemoryThreshold) )
        then RlePlayer.Stream.LoadInMemory;
      fAnimBuffer := TSpeedBitmap.CreateSized( AnimWidth, -AnimHeight, 8 );

      // Alloc RLE DibHeader
      fRleHeader := DibNewHeader( AnimWidth, AnimHeight, 8 );
      fRleHeader.biCompression := BI_RLE8;
      DibSetUsage( fRleHeader, 0, DIB_PAL_COLORS );

      with AnimBuffer do
        begin
          RlePlayer.AttachToDib( 0, 0, DibHeader, ScanLines );
          if pfUseIdentityPalette in PaintFlags
            then ForcePaletteIdentity( true, true );
        end;
(*
      bakAutoDelay := AutoDelay;
      if AutoDelay
        then newFrameDelay := RlePlayer.FrameDelay[0]
        else newFrameDelay := FrameDelay;
      FrameDelay := newFrameDelay;                // SetFrameDelay disables AutoDelay,
      fAutoDelay := bakAutoDelay;                 //   so we must restore it..
*)
      Changed;
      Next;
   end;

  procedure TRlePlayer.DonePlayer;
    begin
      inherited;

      // Free RLE DibHeader
      if Assigned( fRleHeader )
        then
          begin
            DibFree( fRleHeader );
            fRleHeader := nil;
          end;
      FreeObject( fRlePlayer );
    end;

  procedure TRlePlayer.LoadFromStream( aStream : TStream );
    begin
      DonePlayer;

      fRlePlayer := TRleDecoder.Create;
      try
        RlePlayer.LoadFromStream( aStream );
        InitPlayer;
      except
        DonePlayer;
      end;
    end;

  procedure TRlePlayer.Next;
    begin
      try
        fCurrentPlayer := Self;

        with RlePlayer do
          begin
            ProcessFrame;
            inherited;
            NextFrame;
          end;

      except
        DonePlayer; // !!!
        FatalException;
      end;
    end;

  procedure TRlePlayer.UpdateFrame;
    var
      dc         : HDC;
      OldPalette : HPALETTE;
    begin
      dc := Canvas.Handle;
      OldPalette := SelectPalette( dc, AnimBuffer.Palette, false );
      RealizePalette( dc );

      with DestRect do
        //StretchDibBlt( dc, Left, Top, Width, Height, fRleHeader, RlePlayer.CurrentFrame,
        //               0, 0, AnimWidth, AnimHeight, Canvas.CopyMode, DIB_PAL_COLORS );

      SelectPalette( dc, OldPalette, false );
    end;

  procedure TRlePlayer.SeekFrame( Index : integer );
    begin
      if Assigned( RlePlayer )
       then RlePlayer.SeekFrame( Index );
    end;

  function  TRlePlayer.GetKeyframed : boolean;
    begin
      Result := false;
    end;

  function TRlePlayer.GetAnimFrame : integer;
    begin
      if Assigned( RlePlayer )
        then Result := RlePlayer.CurrentFrameIndx
        else Result := 0;
    end;

  function TRlePlayer.GetAnimFrameCount : integer;
    begin
      if Assigned( RlePlayer )
        then Result := RlePlayer.FrameCount
        else Result := 0;
    end;

  procedure TRlePlayer.SetPaintFlags( Value : TPaintFlags );
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

  procedure TRlePlayer.SetAutoDelay( UseAutoDelay : boolean );
    begin
      if UseAutoDelay <> AutoDelay
        then
          begin
(*
            if UseAutoDelay and Assigned( RlePlayer )
              then FrameDelay := RlePlayer.FrameDelay;
            fAutoDelay := UseAutoDelay;
*)            
          end;
    end;

  procedure TRlePlayer.SetStartingFrame( aStartingFrame : integer );
    begin
      if Assigned( RlePlayer )
        then RlePlayer.StartingFrame := aStartingFrame;
    end;

  procedure TRlePlayer.SetEndingFrame( aEndingFrame : integer );
    begin
      if Assigned( RlePlayer )
        then RlePlayer.EndingFrame := aEndingFrame;
    end;

  function TRlePlayer.GetStartingFrame : integer;
    begin
      if Assigned( RlePlayer )
        then Result := RlePlayer.StartingFrame
        else Result := 1;
    end;

  function TRlePlayer.GetEndingFrame : integer;
    begin
      if Assigned( RlePlayer )
        then Result := RlePlayer.EndingFrame
        else Result := -1;
    end;

  procedure TRlePlayer.ResetPalette;
    begin
    end;

  // Registration

  procedure Register;
    begin
      RegisterComponents( 'Merchise', [TRlePlayer] );
    end;

end.
