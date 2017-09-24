unit BlockTicker;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

  type
    TBlockTicker =
      class( TCustomControl )
        public
          constructor Create(AOwner: TComponent); override;
          destructor  Destroy;                  override;
        private
          fFont       : TFont;
          fBuffer     : TBitmap;
          fCaption    : TCaption;
          fIndex      : integer;
          fForeColor  : TColor;
          fBackColor  : TColor;
          fLitColor   : TColor;
          fCompleted  : boolean;
          fHorzMargin : integer;
          fVertMargin : integer;
          fHovering   : boolean;
          fShowBack   : boolean;
        private
          procedure SetCaption( aCaption : TCaption );
        published
          property Font       : TFont    read fFont       write fFont;
          property Caption    : TCaption read fCaption    write SetCaption;
          property ForeColor  : TColor   read fForeColor  write fForeColor;
          property BackColor  : TColor   read fBackColor  write fBackColor;
          property LitColor   : TColor   read fLitColor   write fLitColor;
          property HorzMargin : integer  read fHorzMargin write fHorzMargin;
          property VertMargin : integer  read fVertMargin write fVertMargin;
          property Hovering   : boolean  read fHovering   write fHovering;
          property ShowBack   : boolean  read fShowBack   write fShowBack;
        published
          property Align;
          property OnClick;
        public
          procedure Tick;
        protected
          procedure SetBounds( ALeft, ATop, AWidth, AHeight : integer); override;
          procedure MouseMove( Shift : TShiftState; X, Y : integer); override;
          procedure Paint; override;
        private
          procedure UpdateImage;
        private
          procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
      end;

  procedure Register;

implementation


  // TBlockTicker

  constructor TBlockTicker.Create(AOwner: TComponent);
    begin
      inherited;
      fFont := TFont.Create;
    end;

  destructor TBlockTicker.Destroy;
    begin
      fFont.free;
      fBuffer.Free;
      inherited;
    end;

  procedure TBlockTicker.SetCaption( aCaption : TCaption );
    begin
      fCaption   := aCaption;
      fIndex     := 1;
      fCompleted := false;
    end;

  procedure TBlockTicker.Tick;
    begin
      if not fCompleted
        then
          begin
            if fIndex < length(Caption)
              then inc(fIndex)
              else fCompleted := true;
            UpdateImage;
            Invalidate;
          end;
    end;

  procedure TBlockTicker.SetBounds( ALeft, ATop, AWidth, AHeight : integer);
    begin
      inherited;
      fBuffer.Free;
      fBuffer := TBitmap.Create;
      fBuffer.Width  := AWidth;
      fBuffer.Height := AHeight;
      UpdateImage;
    end;

  procedure TBlockTicker.MouseMove( Shift : TShiftState; X, Y : integer);
    var
      MouseWasCaptured : boolean;
    begin
      inherited;
      if Hovering
        then
          begin
            Cursor := crHandPoint;
            MouseWasCaptured := MouseCapture;
            MouseCapture := (x >= 0) and (y >= 0) and (x < Width) and (y < Height);
            if MouseCapture xor MouseWasCaptured
              then
                begin                                                 
                  UpdateImage;
                  Invalidate;
                end;
          end
        else Cursor := crDefault;
    end;
    
  procedure TBlockTicker.Paint;
    begin
      Canvas.Draw( 0, 0, fBuffer );
    end;

  procedure TBlockTicker.UpdateImage;
    type
      TRGB =
        packed record
          x, b, g, r : byte;
        end;
    var
      R      : TRect;
      subStr : string;
      c1, c2 : TRGB;
      midlit : TColor;
      middrk : TColor;
      i      : integer;
    begin
      R := Rect( 0, 0, Width, Height );
      InflateRect( R, -HorzMargin, -VertMargin );
      fBuffer.Canvas.Font.Assign( Font );
      fBuffer.Canvas.Brush.Color := BackColor;
      c1 := TRGB(LitColor);
      c2 := TRGB(ForeColor);
      c1.r := (c1.r + c2.r) div 2;
      c1.g := (c1.g + c2.g) div 2;
      c1.b := (c1.b + c2.b) div 2;
      c1.x := (c1.x + c2.x) div 2;
      midlit := TColor(c1);
      c1 := TRGB(BackColor);
      c2 := TRGB(ForeColor);
      c1.r := (2*c1.r + c2.r) div 3;
      c1.g := (2*c1.g + c2.g) div 3;
      c1.b := (2*c1.b + c2.b) div 3;
      c1.x := (2*c1.x + c2.x) div 3;
      middrk := TColor(c1);
      with fBuffer.Canvas do
        begin
          Brush.Style := bsSolid;
          Pen.Style := psSolid;
          Pen.Color := BackColor;
          Rectangle( 0, 0, Width + 1, Height + 1 );
          if fShowBack
            then
              begin
                Pen.Color := middrk;
                for i := 0 to pred(Height div 2) do
                  begin
                    MoveTo( 0, 2*i );
                    LineTo( Width + 1, 2*i );
                  end;
              end;
          Brush.Style := bsClear;
          if not fCompleted
            then
              begin
                subStr := copy( Caption, 1, fIndex );
                if MouseCapture and Hovering
                  then Font.Style := Font.Style + [fsUnderline]
                  else Font.Style := Font.Style - [fsUnderline];
                Font.Color := LitColor;
                DrawText( Handle, pchar(subStr + '|'), fIndex + 1, R, DT_WORDBREAK or DT_NOPREFIX );
                Font.Color := midlit;
                DrawText( Handle, pchar(subStr), fIndex - 1, R, DT_WORDBREAK or DT_NOPREFIX );
                Font.Color := ForeColor;
                DrawText( Handle, pchar(subStr), fIndex - 2, R, DT_WORDBREAK or DT_NOPREFIX );
              end
            else
              begin
                Font.Color := ForeColor;
                if MouseCapture and Hovering
                  then Font.Style := Font.Style + [fsUnderline]
                  else Font.Style := Font.Style - [fsUnderline];
                DrawText( Handle, pchar(Caption), -1, R, DT_WORDBREAK or DT_NOPREFIX );
              end;
        end;
    end;

  procedure TBlockTicker.WMEraseBkgnd(var Message: TMessage); 
    begin
      Message.Result := 1;
    end;

  // Register

  procedure Register;
    begin
      RegisterComponents('Five', [TBlockTicker]);
    end;




end.

