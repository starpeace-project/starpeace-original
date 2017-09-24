unit PercentEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  stdctrls;

  type
    TPercentEdit =
      class(TGraphicControl)
        public
          constructor Create(AOwner : TComponent); override;
          destructor  Destroy; override;
        private
          fTopColor       : TColor;
          fBottomColor    : TColor;
          fOutNibColor    : TColor;
          fInNibColor     : TColor;
          fOutBorderColor : TColor;
          fInBorderColor  : TColor;
          fMinPerc        : integer;
          fMaxPerc        : integer;
          fValue          : integer;
          fMidValue       : integer;
          fBarHeight      : integer;
          fTopMargin      : integer;
          fOnMoveBar      : TNotifyEvent;
          fOnChange       : TNotifyEvent;
          fLabel          : TLabel;
          fLineColor      : TColor;
          // Range
          fRanged         : boolean;
          fRangeFromColor : TColor;
          fRangeToColor   : TColor;
        private
          fBitmap         : TBitmap;
          fNibOffset      : word;
          fMidOffset      : word;
          fRetValue       : boolean;
          fOldValue       : integer;
        private
          procedure RenderToBmp;
          procedure DrawAll;
          procedure DrawNibble(R : TRect);
          procedure DrawLines(R : TRect; midLine : boolean);
          procedure DrawNumber(var R : TRect; number : string; flag : boolean);
          function  DrawNumbers(R : TRect) : boolean;
          procedure SetNibble(x : integer);
        protected
          procedure Paint; override;
          procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
          procedure Loaded; override;
        protected
          procedure CMTextChange(var Message: TMessage); message CM_TEXTCHANGED;
          procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
          procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
          procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
          procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
          procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
          procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
          procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
        private
          procedure SetTopColor(aColor : TColor);
          procedure SetBottomColor(aColor : TColor);
          procedure SetOutNibColor(aColor : TColor);
          procedure SetInNibColor(aColor : TColor);
          procedure SetOutBorderColor(aColor : TColor);
          procedure SetInBorderColor(aColor : TColor);
          procedure SetMinPerc(aValue : integer);
          procedure SetMaxPerc(aValue : integer);
          procedure SetValue(aValue : integer);
          procedure SetMidValue(aValue : integer);
          procedure SetBarHeight(aHeight : integer);
          procedure SetTopMargin(aMargin : integer);
          //procedure SetBorderMargin(aMargin : integer);
          procedure SetLabel(aLabel : TLabel);
          procedure SetLineColor(aColor : TColor);

          procedure SetRanged(value : boolean);
          procedure SetRangeFromColor(aColor : TColor);
          procedure SetRangeToColor(aColor : TColor);
        public
          procedure ReturnValue;
        published
          property TopColor       : TColor read fTopColor write SetTopColor;
          property BottomColor    : TColor read fBottomColor write SetBottomColor;
          property OutNibColor    : TColor read fOutNibColor write SetOutNibColor;
          property InNibColor     : TColor read fInNibColor write SetInNibColor;
          property OutBorderColor : TColor read fOutBorderColor write SetOutBorderColor;
          property InBorderColor  : TColor read fInBorderColor write SetInBorderColor;
          property MinPerc        : integer read fMinPerc write SetMinPerc;
          property MaxPerc        : integer read fMaxPerc write SetMaxPerc;
          property Value          : integer read fValue write SetValue;
          property MidValue       : integer read fMidValue write SetMidValue;
          property BarHeight      : integer read fBarHeight write SetBarHeight;
          property TopMargin      : integer read fTopMargin write SetTopMargin;
          property OnMoveBar      : TNotifyEvent read fOnMoveBar write fOnMoveBar;
          property OnChange       : TNotifyEvent read fOnChange write fOnChange;
          property ValueLabel     : TLabel read fLabel write SetLabel;
          property LineColor      : TColor read fLineColor write SetLineColor;
          //property BorderMargin   : integer read fBorderMargin write SetBorderMargin;

          // Range
          property  Ranged         : boolean read fRanged         write SetRanged;
          property  RangeFromColor : TColor  read fRangeFromColor write SetRangeFromColor;
          property  RangeToColor   : TColor  read fRangeToColor   write SetRangeToColor;

          property  Align;
          property  DragCursor;
          property  DragMode;
          property  Enabled;
          property  Font;
          property  ParentFont;
          //property  ParentShowHint;
          property  PopupMenu;
          property  ShowHint;
          property  Visible;
          property  OnClick;
          property  OnDragDrop;
          property  OnDragOver;
          property  OnEndDrag;
          property  OnMouseDown;
          property  OnMouseMove;
          property  OnMouseUp;
          property  OnStartDrag;
      end;

  procedure Register;

implementation

  uses
    GradientUtils, Literals;

  // TPercentEdit

  constructor TPercentEdit.Create(AOwner : TComponent);
    begin
      inherited;
      fTopColor       := $080808;
      fBottomColor    := clBlack;
      fOutNibColor    := $050607;
      fInNibColor     := $090709;
      fOutBorderColor := $283934;
      fInBorderColor  := $979797;
      fMinPerc        := 0;
      fMaxPerc        := 100;
      fValue          := 50;
      fBarHeight      := 12;
      fBitmap         := TBitmap.Create;
      fTopMargin      := 8;
      //fBorderMargin   := 5;
    end;

  destructor TPercentEdit.Destroy;
    begin
      fBitmap.Free;
      inherited;
    end;


  procedure TPercentEdit.RenderToBmp;
    begin
      GradientUtils.Gradient(fBitmap.Canvas, fTopColor, fBottomColor, false, Rect(0, 0, pred(fBitmap.Width), pred(fBitmap.Height)));
      if fRanged and (fMidOffset > fBarHeight)
        then GradientUtils.Gradient(fBitmap.Canvas, fRangeFromColor, fRangeToColor, false, Rect(0, 0, fMidOffset - fBarHeight + 1, pred(fBitmap.Height)));
    end;

  procedure TPercentEdit.DrawAll;
    begin
      RenderToBmp;
      Refresh;
    end;

  procedure TPercentEdit.DrawLines(R : TRect; midLine : boolean);
    begin
      Canvas.Pen.Color := Font.Color;
      Canvas.MoveTo(R.Left + 1, R.Bottom);
      Canvas.LineTo(R.Left + 1, R.Bottom + fBarHeight div 2);
      Canvas.MoveTo(R.Right - 1, R.Bottom);
      Canvas.LineTo(R.Right - 1, R.Bottom + fBarHeight  div 2);
      if midLine
        then
          if not fRanged
            then
              begin
                Canvas.MoveTo(fMidOffset, R.Bottom);
                Canvas.LineTo(fMidOffset, R.Bottom + fBarHeight  div 2);
              end
            else
              if fMidOffset > 1 + Canvas.TextWidth(IntToStr(fMidValue)) div 2
                then
                  begin
                    Canvas.Pen.Color := fRangeToColor;
                    Canvas.MoveTo(fMidOffset, R.Bottom);
                    Canvas.LineTo(fMidOffset, R.Bottom + fBarHeight  div 2);
                  end;
    end;

  procedure TPercentEdit.DrawNibble(R : TRect);
    var
      Points : array[0..3] of TPoint;
    begin
      Points[0].x := fNibOffset - fBarHeight;
      Points[0].y := R.Top + fBarHeight div 2;
      Points[1].x := Points[0].x + fBarHeight;
      Points[1].y := Points[0].y - fBarHeight;
      Points[2].x := Points[1].x + fBarHeight;
      Points[2].y := Points[1].y + fBarHeight;
      Points[3].x := Points[2].x - fBarHeight;
      Points[3].y := Points[2].y + fBarHeight;
      Canvas.Pen.Color   := fOutBorderColor;
      Canvas.Brush.Color := fOutNibColor;
      Canvas.Polygon(Points);
      inc(Points[0].x, fBarHeight div 2);
      inc(Points[1].y, fBarHeight div 2);
      dec(Points[2].x, fBarHeight div 2);
      dec(Points[3].y, fBarHeight div 2);
      Canvas.Pen.Color   := fInBorderColor;
      Canvas.Brush.Color := fInNibColor;
      Canvas.Polygon(Points);
    end;

  procedure TPercentEdit.DrawNumber(var R : TRect; number : string; flag : boolean);
    begin
      Canvas.Font := Font;
      if flag
        then Canvas.Font.Color := fRangeToColor;
      Windows.DrawText(Canvas.Handle, pchar(number), -1, R, DT_CALCRECT + DT_SINGLELINE + DT_CENTER);
      Windows.DrawText(Canvas.Handle, pchar(number), -1, R, DT_SINGLELINE + DT_CENTER);
    end;

  function TPercentEdit.DrawNumbers(R : TRect) : boolean;
    var
      aux  : string;
      C    : TRect;
      fnum : integer;
      lnum : integer;
      nwth : integer;
    begin
      Canvas.Font.Color  := Font.Color;
      Canvas.Brush.Style := bsClear;
      aux := IntToStr(fMinPerc);
      C.Top  := R.Bottom + fBarHeight;
      C.Left := fBarHeight div 2;
      DrawNumber(C, aux, false);
      fnum   := c.Right;
      aux    := IntToStr(fMaxPerc);
      C.Left := Width - Canvas.TextWidth(aux) - fBarHeight;
      lnum   := C.Left;
      DrawNumber(C, aux, false);
      aux    := IntToStr(fMidValue);
      nwth   := Canvas.TextWidth(aux);
      C.Left := fMidOffset - nwth div 2;
      result := (fnum < C.Left - 1) and (C.Left + nwth + 1 < lnum);
      if result
        then DrawNumber(C, aux, fRanged);
    end;

  procedure TPercentEdit.SetNibble(x : integer);
    var
      perc : single;
      rgth : integer;
    begin
      rgth := Width - fBarHeight;
      if x > rgth
        then x := rgth
        else
          if x < fBarHeight
            then x := fBarHeight;
      perc   := (x - fBarHeight)/(rgth - fBarHeight);
      SetValue(round(perc*(fMaxPerc - fMinPerc)));
    end;
{
  function TPercentEdit.GetMidOffset : integer;
    var
      perc    : single;
      barSize : integer;
    begin
      barSize := pred(Width) - 2*fBarHeight;
      perc    := (fMidValue - fMinPerc)/(fMaxPerc - fMinPerc);
      result  := fBarHeight + round(perc*barSize);
    end;
 }

  procedure TPercentEdit.Paint;
    var
      S : TRect;
      R : TRect;
      m : boolean;
    begin
      R := Rect(0, 0, pred(fBitmap.Width), pred(fBitmap.Height));
      S := R;
      OffsetRect(R, fBarHeight, fTopMargin);
      Canvas.CopyRect(R, fBitmap.Canvas, S);
      DrawNibble(R);
      m := DrawNumbers(R);
      DrawLines(R, m);
    end;

  procedure TPercentEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
    begin
      inherited;
      fBitmap.Width := Width - 2*fBarHeight;
      SetValue(fValue);
    end;

  procedure TPercentEdit.Loaded;
    begin
      inherited;
      fBitmap.Height := fBarHeight;
    end;

  procedure TPercentEdit.CMTextChange(var Message: TMessage);
    begin
      try
        if (Text <> '') and (Text <> GetLiteral('Literal418'))
          then Value := StrToInt(Text)
          else Value := fMinPerc;
      except
      end;
    end;

  procedure TPercentEdit.CMMouseEnter(var Message: TMessage);
    begin
    end;

  procedure TPercentEdit.CMMouseLeave(var Message: TMessage);
    begin
    end;

  procedure TPercentEdit.CMFontChanged(var Message: TMessage);
    begin
      Refresh;
    end;

  procedure TPercentEdit.WMEraseBkgnd(var Message: TMessage);
    begin
      Message.Result := 1;
    end;

  procedure TPercentEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
      inherited;
      if Enabled
        then SetNibble(X);
    end;

  procedure TPercentEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
    begin
      inherited;
      if Enabled and (ssLeft in Shift)
        then SetNibble(X);
    end;

  procedure TPercentEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
      if Enabled and Assigned(fOnChange)
        then
          begin
            fOnChange(self);
            if fRetValue
              then SetValue(fOldValue);
          end;
    end;

  procedure TPercentEdit.SetTopColor(aColor : TColor);
    begin
      if fTopColor <> aColor
        then
          begin
            fTopColor := aColor;
            SetValue(fValue);
          end;
    end;

  procedure TPercentEdit.SetBottomColor(aColor : TColor);
    begin
      if fBottomColor <> aColor
        then
          begin
            fBottomColor := aColor;
            SetValue(fValue);
          end;
    end;

  procedure TPercentEdit.SetOutNibColor(aColor : TColor);
    begin
      if fOutNibColor <> aColor
        then
          begin
            fOutNibColor := aColor;
            SetValue(fValue);
          end;
    end;

  procedure TPercentEdit.SetInNibColor(aColor : TColor);
    begin
      if fInNibColor <> aColor
        then
          begin
            fInNibColor := aColor;
            SetValue(fValue);
          end;
    end;

  procedure TPercentEdit.SetOutBorderColor(aColor : TColor);
    begin
      if fOutBorderColor <> aColor
        then
          begin
            fOutBorderColor := aColor;
            SetValue(fValue);
          end;
    end;

  procedure TPercentEdit.SetInBorderColor(aColor : TColor);
    begin
      if fInBorderColor <> aColor
        then
          begin
            fInBorderColor := aColor;
            SetValue(fValue);
          end;
    end;

  procedure TPercentEdit.SetMinPerc(aValue : integer);
    begin
      if (aValue < fMaxPerc) and (fMinPerc <> aValue)
        then
          begin
            fMinPerc := aValue;
            SetValue(fValue);
          end;
    end;

  procedure TPercentEdit.SetMaxPerc(aValue : integer);
    begin
      if (aValue > fMinPerc) and (fMaxPerc <> aValue)
        then
          begin
            fMaxPerc := aValue;
            SetValue(fValue);
          end;
    end;

  procedure TPercentEdit.SetValue(aValue : integer);
    var
      barSize : integer;
      perc    : single;
      strVal  : string;
    begin
      fOldValue := fValue;
      if aValue >= fMaxPerc
        then
          begin
            fValue := fMaxPerc;
            fNibOffset := fBarHeight + fBitmap.Width - fBarHeight;
          end
        else
          if aValue <= fMinPerc
            then
              begin
                fValue := fMinPerc;
                fNibOffset := fBarHeight + fBarHeight;
              end
            else fValue := aValue;
      barSize    := pred(Width) - 2*fBarHeight;
      perc       := (fValue - fMinPerc)/(fMaxPerc - fMinPerc);
      fNibOffset := fBarHeight + round(perc*barSize);
      perc       := (fMidValue - fMinPerc)/(fMaxPerc - fMinPerc);
      fMidOffset := fBarHeight + round(perc*barSize);
      strVal     := IntToStr(fValue) + '%';
      Hint       := strVal;
      DrawAll;
      if fLabel <> nil
        then fLabel.Caption := strVal;
      if Assigned(fOnMoveBar)
        then fOnMoveBar(self);
      fRetValue := false;
    end;

  procedure TPercentEdit.SetMidValue(aValue : integer);
    begin
      if fMidValue <> aValue
        then
          begin
            if (aValue < fMinPerc) or (aValue > fMaxPerc)
              then fMidValue := (fMaxPerc - fMinPerc) div 2
              else fMidValue := aValue;
            SetValue(fValue);
          end;
    end;

  procedure TPercentEdit.SetBarHeight(aHeight : integer);
    begin
      if aHeight <> fBarHeight
        then
          begin
            fBitmap.Height := aHeight;
            fBitmap.Width  := Width - 2*aHeight;
            fBarHeight := aHeight;
            SetValue(fValue);
          end;
    end;

  procedure TPercentEdit.SetTopMargin(aMargin : integer);
    begin
      if fTopMargin <> aMargin
        then
          begin
            fTopMargin := aMargin;
            SetValue(fValue);
          end;
    end;

{
  procedure TPercentEdit.SetBorderMargin(aMargin : integer);
    begin
      if fBarHeight <> aMargin
        then
          begin
            fBarHeight := aMargin;
            fBitmap.Width := Width - 2*fBarHeight;
            SetValue(fValue);
          end;
    end;
}

  procedure TPercentEdit.SetLabel(aLabel : TLabel);
    begin
      fLabel := aLabel;
      if fLabel <> nil
        then fLabel.Caption := IntToStr(fValue) + '%';
    end;

  procedure TPercentEdit.SetLineColor(aColor : TColor);
    begin
      if fLineColor <> aColor
        then
          begin
            fLineColor := aColor;
            SetValue(fValue);
          end;
    end;

  procedure TPercentEdit.SetRanged(value : boolean);
    begin
      if fRanged <> value
        then
          begin
            fRanged := value;
            SetValue(fValue);
          end;
    end;

  procedure TPercentEdit.SetRangeFromColor(aColor : TColor);
    begin
      if aColor <> fRangeFromColor
        then
          begin
            fRangeFromColor := aColor;
            SetValue(fValue);
          end;
    end;

  procedure TPercentEdit.SetRangeToColor(aColor : TColor);
    begin
      if aColor <> fRangeToColor
        then
          begin
            fRangeToColor := aColor;
            SetValue(fValue);
          end;
    end;

  procedure TPercentEdit.ReturnValue;
    begin
      fRetValue := true;
    end;


  procedure Register;
    begin
      RegisterComponents('Five', [TPercentEdit]);
    end;

end.
