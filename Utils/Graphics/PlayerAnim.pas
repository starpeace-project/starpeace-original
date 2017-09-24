unit PlayerAnim;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

// Things left:
// - Implement & make use of Keyframed 
//

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
    SpeedBmp, TimerUtils, SharedTimer;

  type
    TByteSet = set of byte;

  type
    TPlayFromWhatMedia = ( pfStream, pfMemory, pfAuto );

  type
    TPaintFlag  = ( pfFullUpdate, pfUseIdentityPalette, pfIgnorePalette );
    TPaintFlags = set of TPaintFlag;

  type
    TAnimPlayer =
      class( TGraphicControl, ITickeable )
        public
          constructor Create( AOwner : TComponent );                                        override;
          destructor  Destroy;                                                              override;

          procedure SetBounds( aLeft, aTop, aWidth, aHeight : integer );                    override;
          function  GetPalette : HPALETTE;                                                  override;
          procedure Paint;                                                                  override;
          procedure Loaded;                                                                 override;

        protected
          FrameCounter : integer;

          DestRect     : TRect;

          QueueCount          : integer;
          DontUpdateScreen    : boolean;
          fLockedPalEntries   : TByteSet;           // These entries will not be changed by fli palette chunks

          fPlayFrom           : TPlayFromWhatMedia; // This has effect only in the loading process...
          fAnimRect           : TRect;
          fAnimBuffer         : TSpeedBitmap;

          fPaused             : boolean;
          fAutoSize           : boolean;
          fCenter             : boolean;
          fStretch            : boolean;

          fOnFrameCountPlayed : TNotifyEvent;
          fOnFramePlayed      : TNotifyEvent;
          fOnFinished         : TNotifyEvent;
          fOnFatalException   : TNotifyEvent;

          fFilename           : string;
          fPaintFlags         : TPaintFlags;
          fLoop               : boolean;

          fShareTimer         : boolean;
          fTicker             : TTicker;

          fTimerInterval      : integer;
          fAutoDelay          : boolean;

          procedure LockPalEntries( Indx, Count : integer );
          procedure UnlockPalEntries( Indx, Count : integer );
          procedure ResetPalette;                                                           virtual;

          procedure SetPaused( Value : boolean );                                           virtual;
          procedure SetAutoSize( Value : boolean );                                         virtual;
          procedure SetCenter( Value : boolean );                                           virtual;
          procedure SetStretch( Value : boolean );                                          virtual;
          procedure SetPaintFlags( Value : TPaintFlags );                                   virtual;

          procedure Finished( Sender : TObject );                                           virtual;
          procedure Changed;                                                                virtual;
          procedure FatalException;                                                         virtual;

          procedure SetStartingFrame( aStartingFrame : integer );                           virtual;
          procedure SetEndingFrame( aEndingFrame : integer );                               virtual;
          function  GetKeyframed : boolean;                                                 virtual; abstract;
          function  GetStartingFrame : integer;                                             virtual; abstract;
          function  GetEndingFrame : integer;                                               virtual; abstract;

          procedure SetShareTimer( Shared : boolean );
          procedure ReserveTimer;                                                           virtual;
          procedure ReleaseTimer;                                                           virtual;
          procedure UpdateTimer( IntervalChanged : boolean );                               virtual;

          procedure PlayFrames( Count : integer );                                          virtual;
          procedure SeekFrame( Index : integer );                                           virtual;
          procedure FrameChanged;                                                           virtual;                                                                           

          procedure SetAutoDelay( UseAutoDelay : boolean );                                 virtual; abstract;
          procedure SetTimerInterval( Interval : integer);                                  virtual;

          procedure InitPlayer;                                                             virtual;
          procedure DonePlayer;                                                             virtual;

          function  GetAnimFrame : integer;                                                 virtual; abstract;
          function  GetAnimFrameCount : integer;                                            virtual; abstract;

          function  GetUpdateRect : PRect;                                                  virtual;
          procedure UpdateFrame;                                                            virtual;

        public
          // IUnknown for ITickeable
          fRefCount : integer;

          function QueryInterface( const iid : TGUID; out obj ) : integer;                  stdcall;
          function _AddRef : integer;                                                       stdcall;
          function _Release : integer;                                                      stdcall;

          // ITickeable
          function  ITickeable.Enabled = Active;
          procedure ITickeable.Tick = TimerTick;
          function  Interval : integer;

        public
          procedure SetFrameRange( aStartingFrame, aEndingFrame : integer );                virtual;
          procedure ResetFrameRange;                                                        virtual;

          class function TimerResolution : integer;                                         virtual;

          procedure LoadFromFile( aFilename : string );
          procedure LoadFromResourceName( Instance : THandle; const ResourceName : string );
          procedure LoadFromResourceID( Instance : THandle; ResourceId : Integer );
          procedure LoadFromStream( aStream : TStream );                                    virtual; abstract;

          function  Active : boolean;                                                       virtual;
          function  Empty : boolean;                                                        virtual;
          function  GetColor( indx : integer ) : TColor;                                    virtual;
          procedure SetColor( indx : integer; aColor : TColor);                             virtual;

          procedure ResetTints;
          procedure TintColors( Indx, Count : integer; TintColor, BaseColor : TColor );     virtual;

          procedure TimerTick;                                                              virtual;
          procedure Pause;                                                                  virtual;
          procedure Resume;                                                                 virtual;
          procedure Next;                                                                   virtual;

          property  Color[indx : integer] : TColor read GetColor write SetColor;

          property  AnimBuffer : TSpeedBitmap      read fAnimBuffer;
          property  AnimRect : TRect               read fAnimRect;
          property  AnimSize : TPoint              read fAnimRect.BottomRight;
          property  AnimWidth : integer            read fAnimRect.Right;
          property  AnimHeight : integer           read fAnimRect.Bottom;
          property  AnimFrame : integer            read GetAnimFrame          write SeekFrame;
          property  AnimFrameCount : integer       read GetAnimFrameCount;

          property  LockedPalEntries : TByteSet    read fLockedPalEntries;
          property  UpdateRect : PRect             read GetUpdateRect;

        public
          property OnFramePlayed : TNotifyEvent      read fOnFramePlayed      write fOnFramePlayed;
          property OnFinished : TNotifyEvent         read fOnFinished         write fOnFinished;
          property OnFrameCountPlayed : TNotifyEvent read fOnFrameCountPlayed write fOnFrameCountPlayed;
          property OnFatalException : TNotifyEvent   read fOnFatalException   write fOnFatalException;

          property AutoSize : boolean                read fAutoSize           write SetAutoSize           default true;
          property Paused : boolean                  read fPaused             write SetPaused             default false;
          property Center : boolean                  read fCenter             write SetCenter             default false;
          property Stretch : boolean                 read fStretch            write SetStretch            default false;
          property Loop : boolean                    read fLoop               write fLoop                 default true;
          property Filename : string                 read fFilename           write LoadFromFile;
          property StartingFrame : integer           read GetStartingFrame    write SetStartingFrame      default 1;
          property EndingFrame   : integer           read GetEndingFrame      write SetEndingFrame        default -1;
          property Keyframed : boolean               read GetKeyFramed                                    default false;
          property PlayFrom : TPlayFromWhatMedia     read fPlayFrom           write fPlayFrom             default pfAuto;
          property ShareTimer : boolean              read fShareTimer         write SetShareTimer         default true;
          property PaintFlags : TPaintFlags          read fPaintFlags         write SetPaintFlags         default [];

          property TimerInterval : integer           read fTimerInterval      write SetTimerInterval      default 0;
          property AutoDelay : boolean               read fAutoDelay          write SetAutoDelay          default true;
      end;

  var
    fSharedTimer : TSharedTimer = nil;

implementation

  uses
    WinUtils, GDI, ColorSpaces, Dibs, MemUtils, ViewUtils;

  // TSharedAnimTimer

  // TAnimPlayer

  constructor TAnimPlayer.Create( AOwner : TComponent );
    begin
      inherited;

      ControlStyle := ControlStyle + [csReplicatable];

      Height := 60;
      Width  := 60;

      fPaused              := false;
      fLoop                := true;
      fAutoSize            := true;
      fAutoDelay           := true;
      fShareTimer          := true;
    end;

  destructor TAnimPlayer.Destroy;
    begin
      DonePlayer;

      inherited;
    end;

  procedure TAnimPlayer.Loaded;
    begin
      inherited;
      if not (csDesigning in ComponentState)
        then ReserveTimer;
    end;

 procedure TAnimPlayer.InitPlayer;
   begin
     if not ( csLoading in ComponentState )
       then ResetTints;

     if csDesigning in ComponentState
       then DontUpdateScreen := true;
   end;

 procedure TAnimPlayer.DonePlayer;
   begin
     fFilename := '';
     if Assigned( AnimBuffer )
       then
         begin
           if not (csDesigning in ComponentState)
             then ReleaseTimer;
           FreeObject( fAnimBuffer );
         end;
   end;

  function TAnimPlayer.GetPalette : HPALETTE;
    begin
      if Assigned( AnimBuffer )
        then Result := AnimBuffer.Palette
        else Result := 0;
    end;

  function TAnimPlayer.GetUpdateRect : PRect;
    begin
      Result := nil; // Update the whole rect...
    end;

  procedure TAnimPlayer.UpdateFrame;
    var
      RectPtr : PRect;
      Rect    : TRect;
    begin
      if Assigned( AnimBuffer )
        then
          begin
            RectPtr := UpdateRect;
            if RectPtr = nil
              then AnimBuffer.StretchDraw( Canvas, DestRect )
              else
                begin
                  Rect := RectPtr^;
                  with Rect do
                    begin
                      // Since the clipping rectangle is supposed to use the same coordinates as DestRect
                      // we must first convert the UpdateRect
                      Left  := Left  * DestRect.Right div AnimWidth;
                      Right := Right * DestRect.Right div AnimWidth;
                      Top    := Top    * DestRect.Bottom div AnimHeight;
                      Bottom := Bottom * DestRect.Bottom div AnimHeight;

                      AnimBuffer.ClipStretchDraw( Canvas, DestRect, Rect );
                    end;
                end;
          end;
    end;

  procedure TAnimPlayer.Paint;
    begin
      if Assigned( AnimBuffer )
        then AnimBuffer.StretchDraw( inherited Canvas, DestRect )
        else
          if csDesigning in ComponentState
            then
              with inherited Canvas do
                begin
                  Pen.Style   := psDash;
                  Brush.Style := bsClear;
                  Rectangle( 0, 0, Width, Height );
                end;
    end;

  const
    QueueThreshold = 1;

  // IUnknown for ITickeable

  function TAnimPlayer.QueryInterface( const iid : TGUID; out obj ) : integer;
    const
      E_NOINTERFACE = $80004002;
    begin
      if GetInterface( iid, Obj )
        then Result := 0
        else Result := E_NOINTERFACE;
    end;

  function TAnimPlayer._AddRef : integer;
    begin
      inc( fRefCount );
      Result := fRefCount;
    end;

  function TAnimPlayer._Release : integer;
    begin
      dec( fRefCount );
      Result := fRefCount;
    end;

  // ITickeable

  function TAnimPlayer.Active : boolean;
    begin
      Result := (not Empty) and (not Paused);
    end;

  function TAnimPlayer.Interval : integer;
    begin
      Result := fTimerInterval;
    end;

  procedure TAnimPlayer.TimerTick;
    var
      bakDontUpdateScreen : boolean;
    begin
      if ThreadHasTheseMessagesWaiting( QS_IMPORTANT )
        then
          begin
            inc( QueueCount );
            if QueueCount = QueueThreshold
              then
                begin
                  QueueCount          := 0;
                  bakDontUpdateScreen := DontUpdateScreen;
                  DontUpdateScreen    := true;
                  Next;
                  DontUpdateScreen := bakDontUpdateScreen;
                end
              else Next;
          end
        else
          begin
            QueueCount := 0;
            Next;
          end;
    end;

  // TAnimPlayer (cont)
  
  procedure TAnimPlayer.SetShareTimer( Shared : boolean );
    begin
      if (Shared <> ShareTimer)
        then
          if ( csDesigning in ComponentState ) or (csLoading in ComponentState )
            then fShareTimer := Shared
            else
              begin
                ReleaseTimer;
                fShareTimer := Shared;
                ReserveTimer;
              end;
    end;

  class function TAnimPlayer.TimerResolution : integer;
    begin
      Result := 10;
    end;

  type
    TProcOfObject =
      record
        Addr : pointer;
        Self : pointer;
      end;

  procedure TAnimPlayer.ReserveTimer;
    begin
      if ShareTimer
        then
          begin
            if not Assigned( fSharedTimer )
              then fSharedTimer := TSharedTimer.Create( TimerResolution );

            fSharedTimer.InsertClient( Self );
          end
        else
          begin
            fTicker := TSimpleTimer.Create;//TEnhancedTimer.Create; //TEnhancedTimer
            with fTicker do
              begin
                PostTicks  := true;
                OnTimer    := TimerTick;
                Resolution := TimerResolution div 2;
                Interval   := TimerInterval;
                Enabled    := Active;
              end;
          end;
    end;

  procedure TAnimPlayer.ReleaseTimer;
    begin
      if ShareTimer
        then
          begin
            if Assigned( fSharedTimer ) and ( not Paused )
              then fSharedTimer.DeleteClient( Self );
          end
        else FreeObject( fTicker );
    end;

  procedure TAnimPlayer.UpdateTimer( IntervalChanged : boolean );
    begin
      if not (csDesigning in ComponentState)
        then
          if ShareTimer and Assigned( fSharedTimer )
            then
              with fSharedTimer do
                if IntervalChanged
                  then RefreshClient( Self )
                  else
                    if Paused
                      then DeleteClient( Self )
                      else InsertClient( Self )
            else
              if Assigned( fTicker )
                then
                  begin
                    fTicker.Enabled := not Paused;
                    if IntervalChanged
                      then fTicker.Interval := TimerInterval;
                  end;
    end;

  procedure TAnimPlayer.Pause;
    begin
      Paused := true;
    end;

  procedure TAnimPlayer.Resume;
    begin
      Paused := false;
    end;

  procedure TAnimPlayer.SetPaused( Value : boolean );
    begin
      if Paused <> Value //
        then
          begin
            fPaused := Value;
            UpdateTimer( false );
          end;
    end;

  procedure TAnimPlayer.PlayFrames( Count : integer );
    begin
      FrameCounter := Count;
    end;

  procedure TAnimPlayer.SeekFrame( Index : integer );
    begin
      if Assigned( AnimBuffer )
        then FrameChanged;
    end;

  procedure TAnimPlayer.SetFrameRange( aStartingFrame, aEndingFrame : integer );
    begin
      StartingFrame := aStartingFrame;
      EndingFrame   := aEndingFrame;
    end;

  procedure TAnimPlayer.ResetFrameRange;
    begin
      StartingFrame := 1;
      EndingFrame   := AnimFrameCount;
    end;

  procedure TAnimPlayer.SetStartingFrame( aStartingFrame : integer );
    begin
      if AnimFrame < aStartingFrame
        then AnimFrame := aStartingFrame;
    end;

  procedure TAnimPlayer.SetEndingFrame( aEndingFrame : integer );
    begin
      if AnimFrame > aEndingFrame
        then AnimFrame := StartingFrame;
    end;

  procedure TAnimPlayer.FrameChanged;
    begin
      with AnimBuffer do
        if PaletteModified
          then
            begin
              if not IgnorePalette
                then PaletteChanged( false );
              PaletteModified := false;
            end;
      if not DontUpdateScreen and Visible
        then UpdateFrame;

      if Assigned( fOnFramePlayed )
        then fOnFramePlayed( Self );
    end;

  procedure TAnimPlayer.Next;
    begin
      FrameChanged;
      if FrameCounter > 0
        then
          begin
            dec( FrameCounter );
            if (FrameCounter = 0) and Assigned( fOnFrameCountPlayed )
              then fOnFrameCountPlayed( Self );
          end;
    end;

  procedure TAnimPlayer.SetAutoSize( Value : boolean );
    begin
      fAutoSize := Value;
      if fAutoSize
        then Align := alNone;
      Changed;
    end;

  procedure TAnimPlayer.SetCenter( Value : boolean );
    begin
      if fCenter <> Value
        then
          begin
            fCenter := Value;
            Changed;
          end;
    end;

  procedure TAnimPlayer.SetStretch( Value : boolean );
    begin
      if Value <> fStretch
        then
          begin
            fStretch := Value;
            Changed;
          end;
    end;

  procedure TAnimPlayer.SetBounds( aLeft, aTop, aWidth, aHeight : integer );
    begin
      if (aWidth = Width) or (aHeight = Height)
        then inherited
        else
          begin
            inherited;
            fAutoSize := false;
            Changed;
          end;
    end;

  procedure TAnimPlayer.Changed;
    begin
      if AutoSize and not Empty
        then inherited SetBounds( Left, Top, AnimWidth, AnimHeight );
      if Assigned( AnimBuffer )
        then
          begin
            if Stretch
              then DestRect := ClientRect
              else
                if Center
                  then DestRect := Bounds( (Width - AnimBuffer.Width) div 2, (Height - AnimBuffer.Height) div 2,
                                           AnimBuffer.Width, AnimBuffer.Height )
                  else DestRect := Rect( 0, 0, AnimBuffer.Width, AnimBuffer.Height );

            with DestRect do
              if (Right - Left >= Width) and (Bottom - Top >= Height)
                then ControlStyle := ControlStyle + [csOpaque]
                else ControlStyle := ControlStyle - [csOpaque];
          end
        else
          ControlStyle := ControlStyle - [csOpaque];
      Invalidate;
    end;

  function TAnimPlayer.Empty : boolean;
    begin
      Result := AnimBuffer = nil;
    end;

  procedure TAnimPlayer.LoadFromFile( aFilename : string );
     begin
       if aFilename <> ''
         then
           begin
             LoadFromStream( TFileStream.Create( aFilename, fmOpenRead or fmShareDenyWrite) );
             if not Empty
               then fFilename := aFilename;
           end
         else DonePlayer;
       Changed;  
     end;

  procedure TAnimPlayer.LoadFromResourceName( Instance : THandle; const ResourceName : string );
    begin
      LoadFromStream( TResourceStream.Create( Instance, ResourceName, RT_BITMAP ) )
    end;

  procedure TAnimPlayer.LoadFromResourceID( Instance : THandle; ResourceId : Integer );
    begin
      LoadFromStream( TResourceStream.CreateFromID( Instance, ResourceId, RT_BITMAP ) )
    end;

   procedure TAnimPlayer.Finished( Sender : TObject );
     begin
       if not Loop
         then Pause;
       if Assigned( fOnFinished )
         then fOnFinished( Self );
     end;

   procedure TAnimPlayer.FatalException;
     begin
       if Assigned( fOnFatalException )
         then fOnFatalException( Self );
     end;

  function TAnimPlayer.GetColor( indx : integer ) : TColor;
    begin
      Result := TColor( AnimBuffer.RgbEntries[indx] )
    end;

  procedure TAnimPlayer.LockPalEntries( Indx, Count : integer );
    begin
      fLockedPalEntries := fLockedPalEntries + [Indx..Indx + Count - 1];
    end;

  procedure TAnimPlayer.UnlockPalEntries( Indx, Count : integer );
    begin
      fLockedPalEntries := fLockedPalEntries - [Indx..Indx + Count];
    end;

  procedure TAnimPlayer.SetColor( Indx : integer; aColor : TColor);
    begin
      if Assigned( AnimBuffer )
        then
          with AnimBuffer do
            begin
              LockPalEntries( Indx, 1 );

              longint( RgbEntries[indx] ) := ColorToRGB( aColor );
              ChangePaletteEntries( 0, DibHeader.biBitCount, RgbEntries^ );
            end;
    end;

  procedure TAnimPlayer.ResetTints;
    begin
      UnlockPalEntries( low(byte), high(byte) );
      if not Empty
        then ResetPalette;
    end;

  procedure TAnimPlayer.ResetPalette;
    begin
      // Do nothing...
    end;

  procedure TAnimPlayer.TintColors( Indx, Count : integer; TintColor, BaseColor : TColor );
    begin
      assert( Assigned( AnimBuffer ), 'Empty buffer in AnimPlayer.TAnimPlayer.TintColors!!' );

      with AnimBuffer do
        begin
          LockPalEntries( Indx, Count );
          TintRgbEntries( Indx, Count, RgbEntries^, ColorToRgb( TintColor ), ColorToRgb( BaseColor ) );
          ChangePaletteEntries( 0, DibHeader.biClrUsed, RgbEntries^ );
          if not IgnorePalette
            then PaletteChanged( true );
        end;
    end;

  procedure TAnimPlayer.SetPaintFlags( Value : TPaintFlags );
    var
      UseIdentity   : boolean;
      IgnorePalette : boolean;
    begin
      UseIdentity := pfUseIdentityPalette in Value;
      if UseIdentity <> (pfUseIdentityPalette in PaintFlags)
        then
          begin
            if UseIdentity
              then Include( fPaintFlags, pfUseIdentityPalette )
              else Exclude( fPaintFlags, pfUseIdentityPalette );
            if Assigned( AnimBuffer )
              then AnimBuffer.ForcePaletteIdentity( UseIdentity, true );
          end;

      IgnorePalette := pfIgnorePalette in Value;
      if IgnorePalette <> (pfIgnorePalette in PaintFlags)
        then
          begin
            if IgnorePalette
              then Include( fPaintFlags, pfIgnorePalette )
              else Exclude( fPaintFlags, pfIgnorePalette );
            if Assigned( AnimBuffer )
              then AnimBuffer.IgnorePalette := IgnorePalette;
          end;
    end;

  procedure TAnimPlayer.SetTimerInterval( Interval : integer );
    begin
      assert( Interval >= 0, 'Invalid interval specified in TAnimPlayer.SetTimerInterval!!' );
      if not (csReading in ComponentState)
        then fAutoDelay := false;
      if Interval <> TimerInterval
        then
          begin
            fTimerInterval := Interval;
            if not (csDesigning in ComponentState)
              then UpdateTimer( true );
          end;
    end;

initialization

finalization
  fSharedTimer.Free;

end.
